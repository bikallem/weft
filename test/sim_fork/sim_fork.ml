(* The fork rule (issue 8, design section 2.8): [Weft_sim.run] starts no OS
   domain and no OS thread, for each value of [~domains]. A run that uses each
   part of the simulator comes first: the virtual domains, the jobs of [File],
   [Dir] and [Dns] with a time cost, [Process.spawn], the timers, the network,
   [Sim.socket_pair], the faults, [Sim.fault] and a watched signal. Then the
   process forks. OCaml 5 refuses [Unix.fork] after a [Domain.spawn], so the
   fork tells that no domain started. On Linux, the test also counts the threads
   of the process. The last step is the control: after a [Domain.spawn], the
   fork must fail. It prints "ok" at the end. *)

open Weft
module Sim = Weft_sim

(* The threads of the process, on a system with [/proc/self/task]. *)
let threads () =
  match Sys.readdir "/proc/self/task" with
  | a ->
      Some (Array.length a)
  | exception Sys_error _ ->
      None

let check name b = if not b then failwith name

(* A run with each part of the simulator. *)
let busy domains =
  Sim.run ~seed:domains ~domains ~signals:[Signal.SIGUSR1] (fun sim ->
      Sim.set_latency sim (Span.ms 1) ;
      (* Each fault, with no [no_faults]: a new fault breaks the build, and it
         must come in this run. *)
      Sim.net_faults sim
        { Sim.jitter= Span.ms 1
        ; short_io= true
        ; extra_edges= true
        ; udp_loss= true
        ; resets= true } ;
      Sim.set_blocking_cost sim (Span.ms 2) ;
      Sim.set_dns sim "db" (Sim.Addresses ["10.0.0.1"]) ;
      Sim.add_program sim "worker" (fun _ ->
          Sim.program ~runs:(Span.ms 5) ~output:"done" () ) ;
      let a = Sim.addr sim "server:80" in
      let l = Net.listen a in
      Fiber.scope (fun s ->
          let got =
            Fiber.fork_promise s (fun () -> sync (Signal.received SIGUSR1))
          in
          Fiber.fork s (fun () ->
              let x, y = Sim.socket_pair sim in
              Net.write x "pair" ;
              Sim.fault sim y Net.Reset ;
              ( try ignore (Net.read x (Bytes.create 8) 0 8)
                with Net.Error _ -> () ) ;
              Net.close x ; Net.close y ) ;
          Fiber.fork s (fun () ->
              Time.sleep Mono (Span.ms 1) ;
              Sim.signal sim SIGUSR1 ;
              check "signal" (Promise.await got >= 1) ) ;
          Fiber.fork_daemon s (fun () ->
              while true do
                let c, _ = Net.accept l in
                ( try ignore (Net.read c (Bytes.create 16) 0 16)
                  with Net.Error _ -> () ) ;
                Net.close c
              done ) ;
          for i = 1 to 2 * domains do
            Fiber.fork s (fun () ->
                let dir = Printf.sprintf "/d%d" i in
                Dir.mkdir dir ;
                let f = File.openfile [Write; Create] (dir ^ "/f") in
                File.write f "data" ;
                File.close f ;
                check "readdir" (Dir.readdir dir = ["f"]) ;
                check "resolve" (Dns.resolve "db" = ["10.0.0.1"]) ;
                let p = Process.spawn ~stdout:Piped "worker" [] in
                check "exit" (Process.wait p = Process.Exited 0) ;
                Option.iter Pipe.close (Process.stdout p) ;
                Time.sleep Mono (Span.ms i) ;
                let c = Net.connect a in
                (try Net.write c "hello" with Net.Error _ -> ()) ;
                Net.close c ;
                let u = Net.udp ("client", 0) in
                Net.sendto u (Bytes.of_string "x") 0 1 ("server", 53) ;
                Net.close u )
          done ) ;
      Net.close l )

(* A fork, and a child that exits at once. It gives [Error] with the message of
   the refusal. *)
let fork () =
  match Unix.fork () with
  | 0 ->
      Unix._exit 0
  | pid -> (
    match Unix.waitpid [] pid with
    | _, Unix.WEXITED 0 ->
        Ok ()
    | _ ->
        failwith "the child did not exit with 0" )
  | exception Failure m ->
      Error m

let () =
  let before = threads () in
  List.iter
    (fun domains ->
      busy domains ;
      check
        (Printf.sprintf "the threads after a run with %d domains" domains)
        (threads () = before) ;
      match fork () with
      | Ok () ->
          ()
      | Error m ->
          failwith (Printf.sprintf "fork after %d domains: %s" domains m) )
    [1; 2; 4; 8; 16] ;
  check "one thread" (match before with Some n -> n = 1 | None -> true) ;
  (* The control: the fork must fail after a domain started. *)
  Domain.join (Domain.spawn ignore) ;
  check "the control" (Result.is_error (fork ())) ;
  print_endline "ok"
