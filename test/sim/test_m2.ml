(* The tests of the library layers on the simulator (design section 2.14,
   M2). *)

open Windtrap
open Weft
module Sim = Weft_sim
module Op = Private.Op
module S = Private.Sched

(* Each test of the fiber layer runs with 200 seeds on 4 virtual domains. *)
let seeds_200 =
  let n = ref 0 in
  { Portable.name= "200 seeds"
  ; domains= 4
  ; run=
      (fun f ->
        let rec go seed =
          let v = Sim.run ~seed ~domains:4 (fun _ -> f ()) in
          if seed = 200 then v else go (seed + 1)
        in
        go 1 )
  ; listen=
      (fun () ->
        incr n ;
        let a = Net.Tcp ("m2", !n) in
        (Net.listen a, a) )
  ; udp=
      (fun () ->
        incr n ;
        let a = ("m2", !n) in
        (Net.udp a, a) )
  ; tmp=
      (let n = ref 0 in
       fun () ->
         incr n ;
         let d = Printf.sprintf "/tmp%d" !n in
         Dir.mkdir d ; d ) }

module Sched = Private.Sched

let ms_since t0 =
  Int64.to_int
    (Int64.div (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) 1_000_000L)

(* With a time cost, each job takes 0 to 10 ms of virtual time. *)
let cost seed =
  Sim.run ~seed (fun sim ->
      Sim.set_blocking_cost sim (Span.ms 10) ;
      let t0 = Time.now Mono in
      Dir.mkdir "/d" ;
      let f = File.openfile [Write; Create] "/d/f" in
      File.write f "x" ; File.close f ; ms_since t0 )

(* A cancel during a job: the fiber detaches, and an fd that the job opened
   closes. A read that a cancel ends holds the fd until the job completes, so
   the close of the file waits for it (the simulator checks it). *)
let detach seed =
  Sim.run ~seed ~domains:4 (fun sim ->
      let f = File.openfile [Write; Create] "/f" in
      File.write f (String.make 100 'x') ;
      File.close f ;
      let before = Sim.open_fds sim in
      Sim.set_blocking_cost sim (Span.ms 10) ;
      let timeouts = ref 0 in
      Fiber.scope (fun s ->
          for _ = 1 to 10 do
            Fiber.fork s (fun () ->
                match
                  Fiber.with_timeout Mono (Span.ms 5) (fun () ->
                      let f = File.openfile [Read] "/f" in
                      Fun.protect
                        ~finally:(fun () -> File.close f)
                        (fun () -> File.read f (Bytes.create 10) 0 10) )
                with
                | n ->
                    equal int 10 n
                | exception Fiber.Timeout ->
                    incr timeouts )
          done ) ;
      (* The detached jobs complete. *)
      Time.sleep Mono (Span.ms 20) ;
      equal int before (Sim.open_fds sim) ;
      !timeouts )

(* A DNS server that never answers: [with_timeout] ends at the deadline. *)
let dns_timeout () =
  Sim.run (fun sim ->
      Sim.set_dns sim "slow.example" Sim.No_answer ;
      let t0 = Time.now Mono in
      raises Fiber.Timeout (fun () ->
          Fiber.with_timeout Mono (Span.s 2) (fun () ->
              Dns.resolve "slow.example" ) ) ;
      equal int 2000 (ms_since t0) )

let dns () =
  Sim.run (fun sim ->
      Sim.set_dns sim "server" (Sim.Addresses ["10.0.0.1"; "10.0.0.2"]) ;
      Sim.set_dns sim "alias" (Sim.Addresses ["10.0.0.2"]) ;
      equal (list string) ["10.0.0.1"; "10.0.0.2"] (Dns.resolve "server") ;
      equal string "alias" (Dns.reverse "10.0.0.2") ;
      equal string "server" (Dns.reverse "10.0.0.1") ;
      let err = Portable.error_of in
      equal string "ENOENT" (err (fun () -> Dns.resolve "nobody")) ;
      equal string "ENOENT" (err (fun () -> Dns.reverse "10.9.9.9")) )

(* F4: a read that a cancel detached is still in the pool when main returns. The
   domain waits for it at the end of run, so the fd closes. *)
let detached_at_end seed =
  let saved = ref None in
  let timed_out =
    Sim.run ~seed (fun sim ->
        saved := Some sim ;
        let f = File.openfile [Read; Write; Create] "/f" in
        File.write f "data" ;
        Sim.set_blocking_cost sim (Span.ms 50) ;
        let r =
          match
            Fiber.with_timeout Mono (Span.ms 1) (fun () ->
                File.pread f ~file_offset:0 (Bytes.create 4) 0 4 )
          with
          | _ ->
              false
          | exception Fiber.Timeout ->
              true
        in
        File.close f ; r )
  in
  equal int 0 (Sim.open_fds (Option.get !saved)) ;
  timed_out

(* A write that a cancel detached still occurs at its completion. It gives
   [true] if the cancel came before the completion. *)
let detached_write seed =
  Sim.run ~seed (fun sim ->
      let f = File.openfile [Read; Write; Create] "/f" in
      Sim.set_blocking_cost sim (Span.ms 10) ;
      let timed_out =
        match
          Fiber.with_timeout Mono (Span.ms 1) (fun () -> File.write f "data")
        with
        | () ->
            false
        | exception Fiber.Timeout ->
            true
      in
      Time.sleep Mono (Span.ms 20) ;
      equal int 4 (File.fstat f).size ;
      File.close f ;
      timed_out )

(* F4 on 2 domains: domain 1 starts a read, and a cancel detaches it. Domain 0
   ends, and domain 1 waits for the completion, so the fd closes. It gives
   [true] if the read was on domain 1 and the cancel came before the
   completion. *)
let detached_on_domain_1 seed =
  let saved = ref None in
  let on_1 = ref false in
  Sim.run ~seed ~domains:2 (fun sim ->
      saved := Some sim ;
      let f = File.openfile [Read; Write; Create] "/f" in
      File.write f "data" ;
      Sim.set_blocking_cost sim (Span.ms 50) ;
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              let d = Helpers.dom () in
              match
                Fiber.with_timeout Mono (Span.ms 1) (fun () ->
                    File.pread f ~file_offset:0 (Bytes.create 4) 0 4 )
              with
              | _ ->
                  ()
              | exception Fiber.Timeout ->
                  on_1 := d = 1 ) ) ;
      File.close f ) ;
  equal int 0 (Sim.open_fds (Option.get !saved)) ;
  !on_1

(* A completion that a fiber waited for leaves no record. *)
let no_external_left () =
  Sim.run (fun sim ->
      for _ = 1 to 10 do
        ignore (File.stat "/")
      done ;
      equal int 0 (Hashtbl.length (Sim.sched sim).S.externals) )

(* A cancel from another domain can come between the start of a job and the
   start of its wait. Then the fiber never waits, and it detaches. Its
   completion leaves no record. *)
let detach_before_wait seed =
  Sim.run ~seed ~domains:4 (fun sim ->
      let inner = ref None in
      Fiber.scope (fun outer ->
          Fiber.fork outer (fun () ->
              Fiber.scope (fun s ->
                  inner := Some s ;
                  Fiber.fork s (fun () ->
                      while true do
                        ignore (File.stat "/")
                      done ) ) ) ;
          while Option.is_none !inner do
            Fiber.yield ()
          done ;
          Helpers.spin (seed mod 40) ;
          Fiber.cancel (Option.get !inner) Exit ) ;
      Time.sleep Mono (Span.ms 1) ;
      equal
        ~msg:(Printf.sprintf "seed %d" seed)
        int 0
        (Hashtbl.length (Sim.sched sim).S.externals) )

exception Boom

(* A fiber in a cancelled context does not start a job: the directory does not
   come. *)
let cancelled_job () =
  Sim.run (fun _ ->
      let r =
        Fiber.scope (fun s ->
            Fiber.cancel s Boom ;
            match Dir.mkdir "/cancelled" with
            | () ->
                "no cancel"
            | exception Fiber.Cancelled Boom ->
                "cancelled" )
      in
      equal string "cancelled" r ;
      Time.sleep Mono (Span.ms 1) ;
      equal string "ENOENT"
        (Portable.error_of (fun () -> File.stat "/cancelled")) )

let jobs =
  group "jobs"
    [ test "F4: a detached job at the end of run releases its fd" (fun () ->
          is_true (Helpers.some_seed 20 detached_at_end) )
    ; test "a job takes a time cost from the seed" (fun () ->
          let costs = List.init 50 (fun s -> cost (s + 1)) in
          List.iter (fun c -> at_most int ~than:40 c) costs ;
          greater int ~than:1 (List.length (List.sort_uniq compare costs)) ;
          equal (list int) costs (List.init 50 (fun s -> cost (s + 1))) )
    ; test "with no cost, a job takes no time" (fun () ->
          Sim.run (fun sim ->
              Sim.set_blocking_cost sim Span.zero ;
              raises
                (Invalid_argument "Sim.set_blocking_cost: the span is negative")
                (fun () -> Sim.set_blocking_cost sim (Span.ms (-1)) ) ;
              let t0 = Time.now Mono in
              Dir.mkdir "/x" ;
              equal int 0 (ms_since t0) ) )
    ; test "a cancel detaches from the job, and no fd stays open" (fun () ->
          let total = ref 0 in
          for seed = 1 to 200 do
            total := !total + detach seed
          done ;
          (* Some jobs end in time, and some do not. *)
          greater int ~than:0 !total ;
          less int ~than:2000 !total )
    ; test "with_timeout around a DNS that never answers ends at the deadline"
        dns_timeout
    ; test "the name table of Dns" dns
    ; test "a completion leaves no record" no_external_left
    ; test "a detach before the wait leaves no record, with 100 seeds"
        (fun () -> Helpers.seeds 100 detach_before_wait )
    ; test "a detach after the completion of an open closes its fd" (fun () ->
          Sim.run (fun sim ->
              let n0 = Sim.open_fds sim in
              let id =
                Helpers.ok
                  (Helpers.submit
                     (Op.Start (0, Op.Open ("/f", [O_read; O_create], 0))) )
              in
              Helpers.ok (Helpers.submit (Op.Detach id)) ;
              equal int n0 (Sim.open_fds sim) ) )
    ; test "a Finish after a detach gives Model_error" (fun () ->
          contains ~sub:"Finish of job"
            (Helpers.model_error (fun _ ->
                 let j = Op.Stat "/" in
                 let id = Helpers.ok (Helpers.submit (Op.Start (0, j))) in
                 Helpers.ok (Helpers.submit (Op.Detach id)) ;
                 ignore (Helpers.submit (Op.Finish (id, j))) ) ) )
    ; test "a second Finish of a job gives Model_error" (fun () ->
          contains ~sub:"Finish of job"
            (Helpers.model_error (fun _ ->
                 let j = Op.Stat "/" in
                 let id = Helpers.ok (Helpers.submit (Op.Start (0, j))) in
                 ignore (Helpers.ok (Helpers.submit (Op.Finish (id, j)))) ;
                 ignore (Helpers.submit (Op.Finish (id, j))) ) ) )
    ; test "a Finish with another operation gives Model_error" (fun () ->
          contains ~sub:"not the operation of Start"
            (Helpers.model_error (fun _ ->
                 let id =
                   Helpers.ok (Helpers.submit (Op.Start (0, Op.Stat "/")))
                 in
                 ignore (Helpers.submit (Op.Finish (id, Op.Mkdir ("/x", 0)))) )
            ) )
    ; test "a Detach of a job that does not exist gives Model_error" (fun () ->
          contains ~sub:"which does not exist"
            (Helpers.model_error (fun _ ->
                 ignore (Helpers.submit (Op.Detach 1_000_000)) ) ) )
    ; test "a write that a cancel detaches occurs, with 20 seeds" (fun () ->
          is_true (Helpers.some_seed 20 detached_write) )
    ; test "F4 on 2 domains: a detached job of domain 1 releases its fd"
        (fun () -> is_true (Helpers.some_seed 50 detached_on_domain_1) )
    ; test "a fiber in a cancelled context does not start a job" cancelled_job
    ]

(* Processes and signals (milestone M2). *)

let pipe_all = Portable.drain Pipe.read

let status_text = function
  | Process.Exited n ->
      Printf.sprintf "exited %d" n
  | Process.Signaled SIGINT ->
      "SIGINT"
  | Process.Signaled SIGTERM ->
      "SIGTERM"
  | Process.Signaled SIGKILL ->
      "SIGKILL"
  | Process.Signaled _ ->
      "other signal"

let processes =
  group "processes"
    [ test "a fake program writes its output and exits at its time" (fun () ->
          Sim.run (fun sim ->
              Sim.add_program sim "greet" (fun args ->
                  Sim.program ~runs:(Span.ms 30) ~code:3
                    ~output:(String.concat " " ("hello" :: args))
                    () ) ;
              let t0 = Time.now Mono in
              let p = Process.spawn ~stdout:Piped "greet" ["you"] in
              let out = Option.get (Process.stdout p) in
              equal string "hello you" (pipe_all out) ;
              Pipe.close out ;
              equal string "exited 3" (status_text (Process.wait p)) ;
              equal int 30 (ms_since t0) ;
              (* A second wait gives the same status. *)
              equal string "exited 3" (status_text (Process.wait p)) ) )
    ; test "a signal ends a fake program, but not one that it ignores"
        (fun () ->
          Sim.run (fun sim ->
              Sim.add_program sim "server" (fun _ ->
                  Sim.program ~ignores:[SIGHUP] () ) ;
              let p = Process.spawn "server" [] in
              Process.kill p SIGHUP ;
              Time.sleep Mono (Span.ms 1) ;
              is_true
                (sync
                   (choose
                      [wrap (Process.exited p) (fun _ -> false); always true] ) ) ;
              Process.kill p SIGINT ;
              equal string "SIGINT" (status_text (Process.wait p)) ;
              (* A kill after the exit does nothing. *)
              Process.kill p SIGKILL ) )
    ; test "two fibers wait for one child" (fun () ->
          Sim.run ~domains:4 (fun sim ->
              Sim.add_program sim "short" (fun _ ->
                  Sim.program ~runs:(Span.ms 5) () ) ;
              let p = Process.spawn "short" [] in
              Fiber.scope (fun s ->
                  let a = Fiber.fork_promise s (fun () -> Process.wait p) in
                  let b = Fiber.fork_promise s (fun () -> Process.wait p) in
                  equal string "exited 0" (status_text (Promise.await a)) ;
                  equal string "exited 0" (status_text (Promise.await b)) ) ;
              (* The pidfd closes when each domain took its close notice. *)
              Time.sleep Mono (Span.ms 1) ;
              equal int 0 (Sim.open_fds sim) ) )
    ; test "C3: a waiter goes on when the reaper leaves, with 500 seeds"
        (fun () ->
          for seed = 1 to 500 do
            Sim.run ~seed ~domains:4 (fun sim ->
                Sim.net_faults sim {Sim.no_faults with extra_edges= true} ;
                Sim.add_program sim "long" (fun _ ->
                    Sim.program ~runs:(Span.ms 100) () ) ;
                let p = Process.spawn "long" [] in
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        try
                          ignore
                            (Fiber.with_timeout Mono (Span.ms 20) (fun () ->
                                 Process.wait p ) )
                        with Fiber.Timeout -> () ) ;
                    equal string "exited 0" (status_text (Process.wait p)) ) )
          done )
    ; test
        "C11: a kill on another domain at the time of the reap does not raise, \
         with 500 seeds" (fun () ->
          Helpers.seeds 500 (fun seed ->
              Sim.run ~seed ~domains:2 (fun sim ->
                  Sim.add_program sim "p" (fun _ ->
                      Sim.program ~runs:(Span.ms 1) () ) ;
                  let p = Process.spawn "p" [] in
                  Fiber.scope (fun s ->
                      Fiber.fork s (fun () -> ignore (Process.wait p)) ;
                      Fiber.fork s (fun () ->
                          Time.sleep Mono (Span.ms 1) ;
                          Process.kill p SIGTERM ) ) ) ) )
    ; test "a write to the stdin of a child that exited gives EPIPE" (fun () ->
          Sim.run (fun sim ->
              Sim.add_program sim "quick" (fun _ ->
                  Sim.program ~runs:(Span.ms 1) () ) ;
              let p = Process.spawn ~stdin:Piped "quick" [] in
              ignore (Process.wait p) ;
              let w = Option.get (Process.stdin p) in
              raises
                (Pipe.Error (Broken_pipe, "Pipe.write"))
                (fun () -> Pipe.write w "x") ;
              Pipe.close w ) )
    ; test "C10: a pipe write of PIPE_BUF bytes or less is atomic" (fun () ->
          for seed = 1 to 50 do
            Sim.run ~seed ~domains:4 (fun sim ->
                Sim.net_faults sim {Sim.no_faults with short_io= true} ;
                let r, w = Pipe.create () in
                let got =
                  Fiber.scope (fun s ->
                      (* The reader starts late, so the pipe becomes full. *)
                      let got =
                        Fiber.fork_promise s (fun () ->
                            Time.sleep Mono (Span.ms 1) ;
                            pipe_all r )
                      in
                      Fiber.scope (fun writers ->
                          (* 100 bytes, and exactly PIPE_BUF bytes with an
                             uppercase letter. *)
                          List.iter
                            (fun c ->
                              Fiber.fork writers (fun () ->
                                  for _ = 1 to 20 do
                                    Pipe.write w (String.make 100 c)
                                  done ;
                                  for _ = 1 to 8 do
                                    Pipe.write w
                                      (String.make 4096 (Char.uppercase_ascii c))
                                  done ) )
                            ['a'; 'b'] ) ;
                      Pipe.close w ;
                      Promise.await got )
                in
                Pipe.close r ;
                (* Each run of one letter is whole messages: a multiple of 100
                   bytes, or exactly 4096 bytes. *)
                let rec check off =
                  if off < String.length got then (
                    let c = got.[off] in
                    let rec stop i =
                      if i < String.length got && got.[i] = c then stop (i + 1)
                      else i
                    in
                    let n = stop off - off in
                    is_true
                      ( if c = 'A' || c = 'B' then n mod 4096 = 0
                        else n mod 100 = 0 ) ;
                    check (off + n) )
                in
                check 0 ;
                equal int (4000 + (16 * 4096)) (String.length got) )
          done )
    ; test "short_io splits a small write of a socket" (fun () ->
          let short = ref false in
          for seed = 1 to 20 do
            Sim.run ~seed (fun sim ->
                Sim.net_faults sim {Sim.no_faults with short_io= true} ;
                let a = Sim.addr sim "s:1" in
                let l = Net.listen a in
                Fiber.scope (fun s ->
                    Fiber.fork s (fun () ->
                        let c, _ = Net.accept l in
                        ignore (Portable.read_n c 100) ;
                        Net.close c ) ;
                    let c = Net.connect a in
                    let n = Net.write_sub c (Bytes.make 100 'x') 0 100 in
                    if n < 100 then short := true ;
                    Net.write c (String.make (100 - n) 'x') ;
                    Net.close c ) ;
                Net.close l )
          done ;
          is_true !short )
    ; test "an unknown program gives ENOENT" (fun () ->
          Sim.run (fun _ ->
              raises
                (Process.Error (No_entry, "Process.spawn"))
                (fun () -> ignore (Process.spawn "nothing" [])) ) )
    ; test "a count of signals goes to one fiber" (fun () ->
          Sim.run ~domains:4 ~signals:[SIGUSR1] (fun sim ->
              Sim.signal sim SIGUSR1 ;
              Sim.signal sim SIGUSR1 ;
              Time.sleep Mono (Span.ms 1) ;
              equal int 2 (sync (Signal.received SIGUSR1)) ;
              let got = Atomic.make 0 in
              Fiber.scope (fun s ->
                  for _ = 1 to 2 do
                    Fiber.fork s (fun () ->
                        ignore
                          (Atomic.fetch_and_add got
                             (sync (Signal.received SIGUSR1)) ) )
                  done ;
                  Time.sleep Mono (Span.ms 1) ;
                  Sim.signal sim SIGUSR1 ;
                  Time.sleep Mono (Span.ms 1) ;
                  equal int 1 (Atomic.get got) ;
                  Sim.signal sim SIGUSR1 ) ;
              equal int 2 (Atomic.get got) ) )
    ; test "C8: the simulator watches only the signals of run ~signals"
        (fun () ->
          (* A signal that the program does not watch gets its default action:
             SIGWINCH does nothing, and SIGTERM ends the run. *)
          raises (Sim.Killed SIGTERM) (fun () ->
              Sim.run (fun sim ->
                  raises
                    (Invalid_argument
                       "Signal.received: the world does not watch this signal"
                    ) (fun () -> sync (Signal.received SIGTERM) ) ;
                  Sim.signal sim SIGWINCH ;
                  Sim.signal sim SIGTERM ) ) )
    ; test "C9: SIGTSTP and signal 0 do not end a fake program" (fun () ->
          Sim.run (fun sim ->
              Sim.add_program sim "p" (fun _ -> Sim.program ()) ;
              let p = Process.spawn "p" [] in
              Process.kill p SIGTSTP ;
              Process.kill p (Other_signal 0) ;
              (* SIGTTOU on Linux. *)
              Process.kill p (Other_signal 22) ;
              Time.sleep Mono (Span.ms 1) ;
              is_true
                (sync
                   (choose
                      [wrap (Process.exited p) (fun _ -> false); always true] ) ) ;
              Process.kill p SIGTERM ;
              equal string "SIGTERM" (status_text (Process.wait p)) ) )
    ; test "SIGKILL cannot be an event" (fun () ->
          Sim.run (fun _ ->
              raises
                (Invalid_argument
                   "Signal.received: this signal cannot be an event" )
                (fun () -> sync (Signal.received SIGKILL) ) ) )
    ; test "a wait of a child that the parent reaped gives ECHILD" (fun () ->
          Sim.run (fun sim ->
              Sim.add_program sim "p" (fun _ ->
                  Sim.program ~runs:(Span.ms 1) () ) ;
              let r =
                Helpers.ok
                  (Helpers.submit
                     (Op.Spawn
                        { prog= "p"
                        ; argv= ["p"]
                        ; env= None
                        ; cwd= None
                        ; stdin= Inherit
                        ; stdout= Inherit
                        ; stderr= Inherit } ) )
              in
              Time.sleep Mono (Span.ms 2) ;
              is_true
                ( Helpers.ok (Helpers.submit (Op.Wait_child r.pidfd))
                = Some (Exited 0) ) ;
              equal string "ECHILD"
                (Helpers.op_error (Helpers.submit (Op.Wait_child r.pidfd))) ;
              Helpers.ok (Helpers.submit (Op.Close r.pidfd)) ) )
    ; test "a child that other code reaped gives ESRCH and ECHILD" (fun () ->
          Sim.run (fun sim ->
              Sim.add_program sim "p" (fun _ ->
                  Sim.program ~runs:(Span.ms 1) () ) ;
              let p = Process.spawn "p" [] in
              Time.sleep Mono (Span.ms 2) ;
              (* Other code reaps the child, as waitpid (-1) does. The fds of
                 the simulator start at 3. *)
              let reaps =
                List.filter
                  (fun fd ->
                    Helpers.submit (Op.Wait_child fd) = Ok (Some (Exited 0)) )
                  (List.init 8 Fun.id)
              in
              equal int 1 (List.length reaps) ;
              raises
                (Process.Error (Other "ESRCH", "Process.kill"))
                (fun () -> Process.kill p SIGTERM) ;
              (* Each wait tries the reap again, and gets the error again. *)
              for _ = 1 to 2 do
                raises
                  (Process.Error (Other "ECHILD", "Process.wait"))
                  (fun () -> Process.wait p)
              done ) ) ]

(* A graceful shutdown: at SIGTERM, the server stops to accept, and each
   connection ends its request. *)
let shutdown seed =
  Sim.run ~seed ~domains:4 ~signals:[SIGTERM] (fun sim ->
      let a = Sim.addr sim "server:80" in
      let l = Net.listen a in
      let served = Atomic.make 0 in
      Fiber.scope (fun s ->
          (* The server. *)
          Fiber.fork s (fun () ->
              Fiber.scope (fun conns ->
                  Fiber.scope (fun acceptor ->
                      Fiber.fork acceptor (fun () ->
                          while true do
                            let c, _ = Net.accept l in
                            Fiber.fork conns (fun () ->
                                let m = Portable.read_n c 4 in
                                Time.sleep Mono (Span.ms 20) ;
                                Net.write c m ;
                                Net.close c ;
                                Atomic.incr served )
                          done ) ;
                      ignore (sync (Signal.received SIGTERM)) ;
                      Fiber.cancel acceptor Exit ) ) ;
              Net.close l ) ;
          (* Three clients, and SIGTERM while they wait. *)
          for _ = 1 to 3 do
            Fiber.fork s (fun () ->
                let c = Net.connect a in
                Net.write c "ping" ;
                equal string "ping" (Portable.read_n c 4) ;
                Net.close c )
          done ;
          Time.sleep Mono (Span.ms 10) ;
          Sim.signal sim SIGTERM ) ;
      equal int 3 (Atomic.get served) ;
      Time.now Mono )

let shutdown_test () =
  for seed = 1 to 100 do
    equal int64 20_000_000L (Clock.to_ns Mono (shutdown seed))
  done

(* The tests of the public API with all faults, 20 seeds each, on 4 virtual
   domains. *)
let faults_world =
  let n = ref 0 in
  { Portable.name= "faults"
  ; domains= 4
  ; run=
      (fun f ->
        let rec go seed =
          let v =
            Sim.run ~seed ~domains:4 (fun sim ->
                Sim.net_faults sim
                  { jitter= Span.ms 2
                  ; short_io= true
                  ; extra_edges= true
                  ; udp_loss= true
                  ; (* A reset ends the exchange of a test: test_net.ml has the
                       tests of this fault. *)
                    resets= false } ;
                Sim.set_blocking_cost sim (Span.ms 1) ;
                f () )
          in
          if seed = 20 then v else go (seed + 1)
        in
        go 1 )
  ; listen=
      (fun () ->
        incr n ;
        let a = Net.Tcp ("faults", !n) in
        (Net.listen a, a) )
  ; udp=
      (fun () ->
        incr n ;
        let a = ("faults", !n) in
        (Net.udp a, a) )
  ; tmp=
      (let n = ref 0 in
       fun () ->
         incr n ;
         let d = Printf.sprintf "/tmp%d" !n in
         Dir.mkdir d ; d ) }

(* The end of a watch (issue 6). *)

let not_watched = "Signal.received: the world does not watch this signal"

(* [f] must end the run with [Killed sg]. *)
let killed sg f =
  match f () with
  | () ->
      failwith "the run did not end"
  | exception Sim.Killed s ->
      is_true (s = sg)

(* caravan: the first SIGINT stops the server gracefully, and the second aborts
   it. The third gets the default action, and the process ends. *)
let three_sigints seed =
  let at = ref 0L in
  killed SIGINT (fun () ->
      Sim.run ~seed ~domains:4 ~signals:[SIGINT] (fun sim ->
          Fiber.scope (fun s ->
              Fiber.fork s (fun () ->
                  equal int 1 (sync (Signal.received SIGINT)) ;
                  equal int 1 (sync (Signal.received SIGINT)) ;
                  Signal.unwatch SIGINT ) ;
              for _ = 1 to 3 do
                Time.sleep Mono (Span.ms 1) ;
                at := Clock.to_ns Mono (Time.now Mono) ;
                Sim.signal sim SIGINT
              done ;
              Time.sleep Mono (Span.ms 1) ) ) ) ;
  equal int64 3_000_000L !at

(* The guard of the second branch stops the watch after the guard of [received]
   started it, and before the scan. The waiter raises. *)
let unwatch_in_guard () =
  Sim.run ~signals:[SIGUSR1] (fun _ ->
      raises (Invalid_argument not_watched) (fun () ->
          sync
            (choose
               [ Signal.received SIGUSR1
               ; guard (fun () -> Signal.unwatch SIGUSR1 ; never) ] ) ) )

(* A waiter that took another branch stays in the list of waiters. The unwatch
   does not resume it, and the watch ends. *)
let unwatch_after_other_branch () =
  Sim.run ~signals:[SIGUSR1] (fun _ ->
      equal string "timeout"
        (sync
           (choose
              [ wrap (Signal.received SIGUSR1) (fun _ -> "signal")
              ; wrap (Time.after Mono (Span.ms 1)) (fun () -> "timeout") ] ) ) ;
      Signal.unwatch SIGUSR1 ;
      raises (Invalid_argument not_watched) (fun () ->
          sync (Signal.received SIGUSR1) ) )

let unwatch =
  group "unwatch"
    [ test "the third SIGINT ends the process, with 100 seeds" (fun () ->
          for seed = 1 to 100 do
            three_sigints seed
          done )
    ; test "a signal in the count gets its action at the unwatch" (fun () ->
          killed SIGINT (fun () ->
              Sim.run ~signals:[SIGINT] (fun sim ->
                  Sim.signal sim SIGINT ;
                  Time.sleep Mono (Span.ms 1) ;
                  Signal.unwatch SIGINT ) ) )
    ; test "a signal that domain 0 did not take gets its action" (fun () ->
          killed SIGINT (fun () ->
              Sim.run ~signals:[SIGINT] (fun sim ->
                  Sim.signal sim SIGINT ;
                  Signal.unwatch SIGINT ;
                  Time.sleep Mono (Span.ms 1) ) ) ;
          for seed = 1 to 50 do
            killed SIGTERM (fun () ->
                Sim.run ~seed ~domains:4 ~signals:[SIGTERM] (fun sim ->
                    Sim.signal sim SIGTERM ;
                    Signal.unwatch SIGTERM ;
                    Time.sleep Mono (Span.ms 1) ) )
          done )
    ; test "a waiter at the unwatch raises, with 50 seeds" (fun () ->
          for seed = 1 to 50 do
            Sim.run ~seed ~domains:4 ~signals:[SIGINT; SIGTERM] (fun sim ->
                Fiber.scope (fun s ->
                    let p =
                      Fiber.fork_promise s (fun () ->
                          match
                            sync
                              (choose
                                 [ Signal.received SIGINT
                                 ; Signal.received SIGTERM ] )
                          with
                          | _ ->
                              "no error"
                          | exception Invalid_argument m ->
                              m )
                    in
                    Time.sleep Mono (Span.ms 1) ;
                    Signal.unwatch SIGINT ;
                    equal string not_watched (Promise.await p) ) ;
                raises (Invalid_argument not_watched) (fun () ->
                    sync (Signal.received SIGINT) ) ;
                raises
                  (Invalid_argument
                     "Signal.unwatch: the world does not watch this signal" )
                  (fun () -> Signal.unwatch SIGINT ) ;
                (* SIGTERM stays an event. *)
                Sim.signal sim SIGTERM ;
                equal int 1 (sync (Signal.received SIGTERM)) )
          done )
    ; test "SIGWINCH, SIGPIPE, SIGTTIN and SIGURG do nothing" (fun () ->
          Sim.run ~signals:[SIGWINCH; SIGPIPE] (fun sim ->
              Sim.signal sim SIGWINCH ;
              Time.sleep Mono (Span.ms 1) ;
              Signal.unwatch SIGWINCH ;
              Signal.unwatch SIGPIPE ;
              Sim.signal sim SIGWINCH ;
              Sim.signal sim SIGPIPE ;
              (* SIGTTIN and SIGURG on Linux. *)
              Sim.signal sim (Other_signal 21) ;
              Sim.signal sim (Other_signal 23) ;
              Time.sleep Mono (Span.ms 1) ) )
    ; test "a signal number is the signal with its Linux name" (fun () ->
          (* SIGPIPE, SIGCHLD and SIGWINCH do not end the run. *)
          Sim.run (fun sim ->
              Sim.signal sim (Other_signal 13) ;
              Sim.signal sim (Other_signal 17) ;
              Sim.signal sim (Other_signal 28) ;
              Time.sleep Mono (Span.ms 1) ) ;
          (* SIGUSR1 is 10, for a watch, an event and an unwatch. *)
          Sim.run ~signals:[SIGUSR1; Other_signal 12] (fun sim ->
              Sim.signal sim (Other_signal 10) ;
              equal int 1 (sync (Signal.received SIGUSR1)) ;
              Sim.signal sim SIGUSR2 ;
              equal int 1 (sync (Signal.received (Other_signal 12))) ;
              Signal.unwatch (Other_signal 10) ;
              Signal.unwatch SIGUSR2 ) ;
          (* The signal of the end of the run has its name. *)
          raises (Sim.Killed SIGTERM) (fun () ->
              Sim.run (fun sim -> Sim.signal sim (Other_signal 15)) ) ;
          (* A fake program that ignores SIGTERM also ignores signal 15, and
             signal 9 ends it as SIGKILL. *)
          Sim.run (fun sim ->
              Sim.add_program sim "p" (fun _ ->
                  Sim.program ~ignores:[Other_signal 15] () ) ;
              let p = Process.spawn "p" [] in
              Process.kill p SIGTERM ;
              Process.kill p (Other_signal 17) ;
              Time.sleep Mono (Span.ms 1) ;
              is_true
                (sync
                   (choose
                      [wrap (Process.exited p) (fun _ -> false); always true] ) ) ;
              Process.kill p (Other_signal 9) ;
              equal string "SIGKILL" (status_text (Process.wait p)) ) )
    ; test "an unwatch of a signal that is not watched raises" (fun () ->
          Sim.run (fun _ ->
              raises
                (Invalid_argument
                   "Signal.unwatch: the world does not watch this signal" )
                (fun () -> Signal.unwatch SIGINT ) ;
              raises
                (Invalid_argument
                   "Signal.unwatch: this signal cannot be an event" ) (fun () ->
                  Signal.unwatch SIGKILL ) ) )
    ; test "an unwatch in a guard of the same sync makes the waiter raise"
        unwatch_in_guard
    ; test "an unwatch does not resume a waiter that took another branch"
        unwatch_after_other_branch ]

let tests =
  [ Windtrap.group "200 seeds"
      [ Portable.layers seeds_200
      ; Portable.semaphores seeds_200
      ; Portable.vars seeds_200
      ; Portable.fiber_ids seeds_200
      ; Portable.files seeds_200 ]
  ; jobs
  ; processes
  ; group "with faults" (Portable.tests faults_world)
  ; group "signals"
      [ test "SIGTERM gives a graceful shutdown with 100 seeds" shutdown_test
      ; unwatch ] ]
