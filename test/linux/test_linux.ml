(* The tests on the Linux world (design section 2.7, and the "done" list of
   milestone M0). *)

open Windtrap
open Weft

(* A new directory for each test that needs one. The directories go at the
   exit. *)
let tmp =
  let base = Filename.temp_dir "weft-test" "" and n = ref 0 in
  at_exit (fun () -> ignore (Sys.command ("rm -rf " ^ Filename.quote base))) ;
  fun () ->
    incr n ;
    let d = Filename.concat base (string_of_int !n) in
    Sys.mkdir d 0o755 ; d

let world =
  { Portable.name= "linux"
  ; domains= 1
  ; run= (fun f -> Weft_linux.run ~domains:1 f)
  ; listen=
      (fun () ->
        let l = Net.listen (Net.Tcp ("127.0.0.1", 0)) in
        (l, Net.Tcp ("127.0.0.1", Weft_linux.local_port l)) )
  ; tmp }

let elapsed_ns f =
  Weft_linux.run ~domains:1 (fun () ->
      let t0 = Time.now Mono in
      f () ;
      Int64.to_int (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) )

(* Without load, a sleep ends between the deadline and the deadline plus 2 ms
   (the slack of 1 ms included). *)
let sleep_10ms () =
  for _ = 1 to 20 do
    let ns = elapsed_ns (fun () -> Time.sleep Mono (Span.ms 10)) in
    at_least int ~than:10_000_000 ns ;
    less int ~than:12_000_000 ns
  done

let sleep_10s () =
  if Sys.getenv_opt "WEFT_LONG" = None then
    skip ~reason:"set WEFT_LONG=1 to run it" () ;
  let ns = elapsed_ns (fun () -> Time.sleep Mono (Span.s 10)) in
  at_least int ~than:10_000_000_000 ns ;
  less int ~than:10_002_000_000 ns

(* The echo server answers a client in another process. *)
let other_process () =
  let client =
    match Sys.getenv_opt "WEFT_ECHO_CLIENT" with
    | Some c ->
        (* create_process looks for a relative name in PATH. *)
        if Filename.is_relative c then Filename.concat (Sys.getcwd ()) c else c
    | None ->
        skip ~reason:"WEFT_ECHO_CLIENT is not set" ()
  in
  let out_r, out_w = Unix.pipe ~cloexec:true () in
  let pid =
    Weft_linux.run ~domains:1 (fun () ->
        let l = Net.listen (Net.Tcp ("127.0.0.1", 0)) in
        let port = Weft_linux.local_port l in
        let pid =
          Unix.create_process client
            [|client; string_of_int port|]
            Unix.stdin out_w Unix.stderr
        in
        let c, _ = Net.accept l in
        let buf = Bytes.create 5 in
        let rec loop off =
          if off < 5 then
            match Net.read c buf off (5 - off) with
            | 0 ->
                ()
            | n ->
                loop (off + n)
        in
        loop 0 ;
        Net.write c (Bytes.to_string buf) ;
        Net.close c ;
        Net.close l ;
        pid )
  in
  Unix.close out_w ;
  let _, status = Unix.waitpid [] pid in
  let got = In_channel.input_all (Unix.in_channel_of_descr out_r) in
  Unix.close out_r ;
  is_true (status = Unix.WEXITED 0) ;
  equal string "hello" got

let open_fds () = Array.length (Sys.readdir "/proc/self/fd")

(* 10,000 connections open and close, and no fd stays open. *)
let no_fd_leak () =
  let before = open_fds () in
  Weft_linux.run ~domains:1 (fun () ->
      let l, a = world.listen () in
      Fiber.scope (fun s ->
          Fiber.fork_daemon s (fun () -> Portable.echo_server l) ;
          for _ = 1 to 10_000 do
            let c = Net.connect a in
            Net.write c "x" ;
            equal string "x" (Portable.read_n c 1) ;
            Net.close c
          done ) ;
      Net.close l ) ;
  equal int before (open_fds ())

let unix_socket () =
  let path = Filename.temp_file "weft" ".sock" in
  Sys.remove path ;
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      Weft_linux.run ~domains:1 (fun () ->
          let l = Net.listen (Net.Unix path) in
          Fiber.scope (fun s ->
              Fiber.fork_daemon s (fun () -> Portable.echo_server l) ;
              let c = Net.connect (Net.Unix path) in
              Net.write c "unix" ;
              equal string "unix" (Portable.read_n c 4) ;
              Net.close c ) ;
          Net.close l ) )

let posix_time () =
  let p =
    Weft_linux.run ~domains:1 (fun () -> Time.posix_to_unix_s (Time.now Posix))
  in
  less (float 1e-9) ~than:1.0 (Float.abs (p -. Unix.gettimeofday ()))

let invalid_address () =
  Weft_linux.run ~domains:1 (fun () ->
      match Net.connect (Net.Tcp ("999.1.1.1", 80)) with
      | _ ->
          failwith "no error"
      | exception Net.Error (Net.Other _, "Net.connect") ->
          () )

(* C3: a Register that fails gives its error to the waiter. Two records for one
   fd make the second EPOLL_CTL_ADD fail with EEXIST. *)
let register_failure () =
  Weft_linux.run ~domains:1 (fun () ->
      let l : Private.Net.t = Net.listen (Net.Tcp ("127.0.0.1", 0)) in
      ignore (sync (choose [Net.readable l; Time.after Mono (Span.ms 1)])) ;
      let clone = Private.Sched.Fdtab.alloc l.Private.Sched.os in
      ( match Net.accept clone with
      | _ ->
          failwith "no error"
      | exception Net.Error (Net.Exists, _) ->
          () ) ;
      Net.close l )

(* L2: the peer of a Unix socket has a Unix address. *)
let unix_peer_addr () =
  let path = Filename.temp_file "weft" ".sock" in
  Sys.remove path ;
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      Weft_linux.run ~domains:1 (fun () ->
          let l = Net.listen (Net.Unix path) in
          let c = Net.connect (Net.Unix path) in
          let s, peer = Net.accept l in
          is_true (peer = Net.Unix "") ;
          (* A Unix socket has no port. *)
          equal int 0 (Weft_linux.local_port l) ;
          Net.close s ;
          Net.close c ;
          Net.close l ) )

(* L3: a port out of range is an error, not a truncated port. *)
let port_range () =
  Weft_linux.run ~domains:1 (fun () ->
      match Net.listen (Net.Tcp ("127.0.0.1", 70000)) with
      | _ ->
          failwith "no error"
      | exception Net.Error (Net.Other _, _) ->
          () )

(* The thread pool (milestone M2). *)

let dns_localhost () =
  Weft_linux.run ~domains:1 (fun () ->
      let addrs = Dns.resolve "localhost" in
      is_true (List.mem "127.0.0.1" addrs || List.mem "::1" addrs) ;
      (* With no DNS server, the error can be another error than ENOENT. *)
      match Dns.resolve "no-such-host.invalid" with
      | _ ->
          failwith "no error"
      | exception Dns.Error (_, "Dns.resolve") ->
          () )

(* The pool closes the fds after the end of run: wait up to 1 s for it. *)
let fds_back_to before =
  let rec wait n =
    if open_fds () <> before && n > 0 then (
      Unix.sleepf 0.001 ;
      wait (n - 1) )
  in
  wait 1000

(* An open of a FIFO blocks in the pool until a writer comes. The fiber detaches
   at the deadline. Then a writer comes, the job completes, and the pool closes
   the fd that the job opened. *)
let detach_from_open () =
  let fifo = Filename.concat (tmp ()) "fifo" in
  Unix.mkfifo fifo 0o600 ;
  let before = open_fds () in
  let t =
    Weft_linux.run ~domains:1 (fun () ->
        let t0 = Time.now Mono in
        raises Fiber.Timeout (fun () ->
            Fiber.with_timeout Mono (Span.ms 20) (fun () ->
                File.openfile [Read] fifo ) ) ;
        let t = Span.to_ns (Time.diff Mono (Time.now Mono) t0) in
        let w = File.openfile [Write] fifo in
        File.close w ; t )
  in
  at_least int64 ~than:20_000_000L t ;
  less int64 ~than:100_000_000L t ;
  (* The pool closes the fds after the end of run. *)
  fds_back_to before ;
  equal int before (open_fds ())

(* C4: a job that blocks does not stop the next job of the pool. *)
let blocked_job () =
  let fifo = Filename.concat (tmp ()) "fifo" in
  Unix.mkfifo fifo 0o600 ;
  Weft_linux.run ~domains:1 (fun () ->
      (* After this job, a thread of the pool is idle. *)
      ignore (File.stat "/") ;
      Fiber.scope (fun s ->
          (* The two jobs come at the same time to the idle thread. *)
          let r = Fiber.fork_promise s (fun () -> File.openfile [Read] fifo) in
          let t0 = Time.now Mono in
          let st =
            Fiber.fork_promise s (fun () ->
                ignore (File.stat "/") ;
                Time.diff Mono (Time.now Mono) t0 )
          in
          let ns =
            match
              Fiber.with_timeout Mono (Span.ms 500) (fun () -> Promise.await st)
            with
            | d ->
                Span.to_ns d
            | exception Fiber.Timeout ->
                Int64.max_int
          in
          let w = File.openfile [Write] fifo in
          File.close w ;
          File.close (Promise.await r) ;
          less int64 ~than:100_000_000L ns ) )

(* 1,000 files open and close, and the pool closes each fd. *)
let no_file_leak () =
  let d = tmp () in
  let before = open_fds () in
  Weft_linux.run ~domains:1 (fun () ->
      Fiber.scope (fun s ->
          for i = 1 to 10 do
            Fiber.fork s (fun () ->
                for j = 1 to 100 do
                  let f =
                    File.openfile [Write; Create]
                      (Filename.concat d (Printf.sprintf "%d-%d" i j))
                  in
                  File.write f "x" ; File.close f
                done )
          done ) ) ;
  fds_back_to before ;
  equal int before (open_fds ()) ;
  equal int 1000 (Array.length (Sys.readdir d))

(* Processes and signals (milestone M2). *)

let read_pipe = Portable.drain Pipe.read

(* The child gets no blocked signals, also when run blocks SIGINT. *)
let child_sigint () =
  let before = open_fds () in
  Weft_linux.run ~domains:1 ~signals:[SIGINT] (fun () ->
      let p = Process.spawn "sleep" ["10"] in
      Time.sleep Mono (Span.ms 20) ;
      Process.kill p SIGINT ;
      is_true (Process.wait p = Process.Signaled SIGINT) ) ;
  equal int before (open_fds ())

let child_output () =
  Weft_linux.run ~domains:1 (fun () ->
      let p =
        Process.spawn ~stdout:Piped ~stderr:Null "sh"
          ["-c"; "echo hi; echo err >&2; exit 3"]
      in
      let out = Option.get (Process.stdout p) in
      equal string "hi\n" (read_pipe out) ;
      Pipe.close out ;
      is_true (Process.wait p = Process.Exited 3) ;
      let p = Process.spawn ~stdin:Piped ~stdout:Piped ~cwd:"/" "cat" [] in
      let i = Option.get (Process.stdin p)
      and o = Option.get (Process.stdout p) in
      Pipe.write i "through cat" ;
      Pipe.close i ;
      equal string "through cat" (read_pipe o) ;
      Pipe.close o ;
      is_true (Process.wait p = Process.Exited 0) ;
      match Process.spawn "no-such-program-weft" [] with
      | _ ->
          failwith "no error"
      | exception Process.Error (Net.No_entry, "Process.spawn") ->
          () )

(* SIGTERM to the process: a fiber that waits gets it, and run ends. *)
let sigterm () =
  let n =
    Weft_linux.run ~domains:1 ~signals:[SIGTERM] (fun () ->
        Fiber.scope (fun s ->
            let got =
              Fiber.fork_promise s (fun () -> sync (Signal.received SIGTERM))
            in
            Time.sleep Mono (Span.ms 10) ;
            Unix.kill (Unix.getpid ()) Sys.sigterm ;
            Promise.await got ) )
  in
  at_least int ~than:1 n

(* C7: a signal by its number is the same event as by its name. *)
let signal_number () =
  let n =
    Weft_linux.run ~domains:1 ~signals:[SIGUSR1] (fun () ->
        Fiber.scope (fun s ->
            let got =
              Fiber.fork_promise s (fun () ->
                  sync (Signal.received (Other_signal 10)) )
            in
            Time.sleep Mono (Span.ms 10) ;
            Unix.kill (Unix.getpid ()) Sys.sigusr1 ;
            Promise.await got ) )
  in
  at_least int ~than:1 n

(* The check of the threads: out of run, the calling thread does not block
   SIGUSR2. In run ~signals, each thread of the process blocks it, and no thread
   blocks SIGHUP. *)
let blocked_threads () =
  let self = string_of_int (Unix.getpid ()) in
  is_true (List.mem self (Weft_linux.unblocked_threads [SIGUSR2])) ;
  Weft_linux.run ~domains:1 ~signals:[SIGUSR2] (fun () ->
      equal (list string) [] (Weft_linux.unblocked_threads [SIGUSR2]) ;
      (* No thread blocks SIGHUP. *)
      is_true (Weft_linux.unblocked_threads [SIGUSR2; SIGHUP] <> []) )

(* M3: a finaliser fills an Ivar from outside a fiber, and the fiber that waits
   wakes. *)
let finaliser_fill () =
  Weft_linux.run ~domains:1 (fun () ->
      let iv = Ivar.create () in
      let () =
        let x = ref 1 in
        Gc.finalise (fun _ -> ignore (Ivar.fill_from_outside iv "gc")) x
      in
      Fiber.scope (fun s ->
          let p = Fiber.fork_promise s (fun () -> sync (Ivar.read iv)) in
          Fiber.yield () ;
          Gc.full_major () ;
          equal string "gc" (Promise.await p) ) )

let unwatched () =
  Weft_linux.run ~domains:1 (fun () ->
      raises
        (Invalid_argument
           "Signal.received: the world does not watch this signal" ) (fun () ->
          sync (Signal.received SIGUSR1) ) )

let linux =
  group "linux"
    [ test "a Unix domain socket echoes" unix_socket
    ; test ~timeout:5. "C3: a Register that fails raises in the waiter"
        register_failure
    ; test "L2: the peer of a Unix socket has a Unix address" unix_peer_addr
    ; test "L3: a port out of range gives an error" port_range
    ; test "Time.now Posix is the time of the system" posix_time
    ; test "an address that is not an IP address gives an error" invalid_address
    ; test "a 10 ms sleep ends between 10 ms and 12 ms" sleep_10ms
    ; slow "a 10 s sleep ends between 10 s and 10.002 s" sleep_10s
    ; test "the echo server answers a client in another process" other_process
    ; slow "10,000 connections open and close with no fd leak" no_fd_leak
    ; test "Dns.resolve finds localhost" dns_localhost
    ; test "a fiber detaches from an open that blocks" detach_from_open
    ; test "1,000 files open and close with no fd leak" no_file_leak
    ; test "C4: a job that blocks does not stop the next job" blocked_job
    ; test "a child stops at SIGINT" child_sigint
    ; test "the pipes of a child" child_output
    ; test "SIGTERM to the process is an event" sigterm
    ; test "C7: Other_signal 10 is SIGUSR1" signal_number
    ; test "each thread of run ~signals blocks the signals" blocked_threads
    ; test "a finaliser fills an Ivar from outside a fiber" finaliser_fill
    ; test "a signal that run does not watch is not an event" unwatched ]

let () = exit (Windtrap.run "linux" (Portable.tests world @ [linux]))
