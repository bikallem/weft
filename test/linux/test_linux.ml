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
        (l, Net.local_addr l) )
  ; udp=
      (fun () ->
        let s = Net.udp ("127.0.0.1", 0) in
        (s, Portable.host_port s) )
  ; tmp }

let elapsed_ns f =
  Weft_linux.run ~domains:1 (fun () ->
      let t0 = Time.now Mono in
      f () ;
      Int64.to_int (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) )

(* A sleep never ends before its deadline. A shared machine can delay one sleep
   for a long time. Thus the median of the 20 sleeps checks the precision (less
   than 12 ms), and the limit of 500 ms for each sleep finds a timer that fires
   much too late. *)
let sleep_10ms () =
  let all =
    List.init 20 (fun _ ->
        let ns = elapsed_ns (fun () -> Time.sleep Mono (Span.ms 10)) in
        at_least int ~than:10_000_000 ns ;
        less int ~than:500_000_000 ns ;
        ns )
  in
  less int ~than:12_000_000 (List.nth (List.sort compare all) 10)

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
        let _, port = Portable.host_port l in
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

(* 10,000 connections open and close, and no fd stays open. The client closes
   first, and the server resets: thus no side keeps a TIME_WAIT socket. A
   TIME_WAIT socket keeps its entry in the conntrack table of the host for 120
   s, and after a reset the entry stays for 10 s only. Parallel runs with
   TIME_WAIT sockets filled this table. Then the host dropped each SYN, and the
   test did not end. *)
let no_fd_leak () =
  let before = open_fds () in
  Weft_linux.run ~domains:1 (fun () ->
      let l, a = world.listen () in
      Fiber.scope (fun s ->
          Fiber.fork_daemon s (fun () -> Portable.echo_server ~reset:true l) ;
          for _ = 1 to 10_000 do
            let c = Portable.connect_fast a in
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
          is_true (Net.local_addr l = Net.Unix path) ;
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

(* The pool closes the fds of the files after the end of run: wait up to 10 s
   for it. A test that closes a file must wait for the close before it ends. If
   not, a later test can count the fd before its close and not after it. *)
let fds_back_to before =
  let rec wait n =
    if open_fds () <> before && n > 0 then (
      Unix.sleepf 0.001 ;
      wait (n - 1) )
  in
  wait 10_000

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
  let before = open_fds () in
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
          less int64 ~than:100_000_000L ns ) ) ;
  fds_back_to before ;
  equal int before (open_fds ())

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

(* The limit of detached jobs (design section 2.3). The file pool has 16 threads
   for the live jobs, and 8 more for the detached jobs that run. An open of a
   FIFO holds a thread until a writer comes. *)
let pool_max = 16

let pool_limit = 8

(* A writer opens the FIFO, and each open that blocks on it ends. *)
let release fifo = Unix.close (Unix.openfile fifo [O_WRONLY; O_NONBLOCK] 0)

(* [n] fibers that each hold a thread of the pool. *)
let hold s fifo n =
  let l =
    List.init n (fun _ ->
        Fiber.fork_promise s (fun () -> File.openfile [Read] fifo) )
  in
  Time.sleep Mono (Span.ms 50) ;
  l

let new_fifo () =
  let fifo = Filename.concat (tmp ()) "fifo" in
  Unix.mkfifo fifo 0o600 ; fifo

(* A detach of a job in the queue removes it: the open never creates the
   file. *)
let detach_queued () =
  let fifo = new_fifo () in
  let file = Filename.concat (Filename.dirname fifo) "never" in
  let before = open_fds () in
  Weft_linux.run ~domains:1 (fun () ->
      Fiber.scope (fun s ->
          let held = hold s fifo pool_max in
          raises Fiber.Timeout (fun () ->
              Fiber.with_timeout Mono (Span.ms 50) (fun () ->
                  File.openfile [Write; Create] file ) ) ;
          release fifo ;
          List.iter (fun p -> File.close (Promise.await p)) held ;
          (* The pool has threads again: the job would run now. *)
          ignore (File.stat "/") ;
          Time.sleep Mono (Span.ms 20) ) ) ;
  is_false (Sys.file_exists file) ;
  fds_back_to before ;
  equal int before (open_fds ())

(* Each detached job that runs gives one more thread, up to the limit. *)
let detached_threads () =
  let fifo = new_fifo () in
  let before = open_fds () in
  Weft_linux.run ~domains:1 (fun () ->
      Fiber.scope (fun s ->
          for _ = 1 to pool_max do
            Fiber.fork s (fun () ->
                raises Fiber.Timeout (fun () ->
                    Fiber.with_timeout Mono (Span.ms 30) (fun () ->
                        File.openfile [Read] fifo ) ) )
          done ) ;
      (* All threads of the live jobs hold a detached job. *)
      ignore (Fiber.with_timeout Mono (Span.s 1) (fun () -> File.stat "/")) ;
      Fiber.scope (fun s ->
          let held = hold s fifo (pool_limit - 1) in
          (* The last thread of the limit. *)
          ignore (Fiber.with_timeout Mono (Span.s 1) (fun () -> File.stat "/")) ;
          let held = held @ hold s fifo 1 in
          raises Fiber.Timeout (fun () ->
              Fiber.with_timeout Mono (Span.ms 50) (fun () -> File.stat "/") ) ;
          release fifo ;
          List.iter (fun p -> File.close (Promise.await p)) held ) ) ;
  (* The pool closes the fds of the detached opens. *)
  fds_back_to before ;
  equal int before (open_fds ())

(* A pool that is full for more than 1 s writes one warning to stderr. *)
let full_warning () =
  let fifo = new_fifo () in
  let log = Filename.concat (Filename.dirname fifo) "stderr" in
  let before = open_fds () in
  let fd = Unix.openfile log [O_WRONLY; O_CREAT] 0o600 in
  let saved = Unix.dup Unix.stderr in
  let w0 = Weft_linux.pool_warnings () in
  Unix.dup2 fd Unix.stderr ;
  Fun.protect
    ~finally:(fun () ->
      Unix.dup2 saved Unix.stderr ;
      Unix.close saved )
    (fun () ->
      Weft_linux.run ~domains:1 (fun () ->
          Fiber.scope (fun s ->
              let held = hold s fifo pool_max in
              raises Fiber.Timeout (fun () ->
                  Fiber.with_timeout Mono (Span.ms 1300) (fun () ->
                      File.stat "/" ) ) ;
              release fifo ;
              List.iter (fun p -> File.close (Promise.await p)) held ) ) ) ;
  Unix.close fd ;
  fds_back_to before ;
  equal int before (open_fds ()) ;
  equal int (w0 + 1) (Weft_linux.pool_warnings ()) ;
  let text = In_channel.with_open_text log In_channel.input_all in
  equal string
    "weft: the file pool was full for 1 s: queued jobs 1, threads 16, detached \
     jobs that run 0\n"
    text

(* UDP on Linux: IPv6, the size limit, and the sizes of the buffers, which the
   simulator copies. *)
let udp_ipv6 () =
  Weft_linux.run ~domains:1 (fun () ->
      let a = Net.udp ("::1", 0) and b = Net.udp ("::1", 0) in
      let a_addr = Portable.host_port a in
      Net.sendto b (Bytes.of_string "hi") 0 2 a_addr ;
      let buf = Bytes.create 8 in
      let k, from = Net.recvfrom a buf 0 8 in
      equal string "hi" (Bytes.sub_string buf 0 k) ;
      is_true (from = Portable.host_port b) ;
      Net.close a ;
      Net.close b )

(* The local address on Linux: the text of an IPv6 address, and the address of
   "localhost". *)
let local_addr_text () =
  Weft_linux.run ~domains:1 (fun () ->
      let l6 = Net.listen (Net.Tcp ("::1", 0)) in
      let a6 = Net.local_addr l6 in
      let c = Net.connect a6 in
      let s, peer = Net.accept l6 in
      ( match (a6, Net.local_addr c) with
      | Net.Tcp ("::1", p), Net.Tcp ("::1", q) ->
          is_true (p > 0 && q > 0)
      | _ ->
          fail "not ::1" ) ;
      is_true (Net.local_addr c = peer) ;
      is_true (Net.local_addr s = a6) ;
      let l4 = Net.listen (Net.Tcp ("localhost", 0)) in
      ( match Net.local_addr l4 with
      | Net.Tcp ("127.0.0.1", p) ->
          is_true (p > 0)
      | _ ->
          fail "not 127.0.0.1" ) ;
      List.iter Net.close [s; c; l6; l4] )

let udp_size_limit () =
  Weft_linux.run ~domains:1 (fun () ->
      let a = Net.udp ("127.0.0.1", 0) in
      let a_addr = Portable.host_port a in
      Net.set_option a Recv_buffer 100_000 ;
      Net.sendto a (Bytes.make 65507 'x') 0 65507 a_addr ;
      let buf = Bytes.create 70_000 in
      equal int 65507 (fst (Net.recvfrom a buf 0 70_000)) ;
      Net.close a )

let option_sizes () =
  Weft_linux.run ~domains:1 (fun () ->
      let u = Net.udp ("127.0.0.1", 0) in
      Net.set_option u Recv_buffer 1000 ;
      equal int 2304 (Net.get_option u Recv_buffer) ;
      Net.set_option u Recv_buffer 100_000 ;
      equal int 200_000 (Net.get_option u Recv_buffer) ;
      Net.set_option u Send_buffer 1 ;
      equal int 4608 (Net.get_option u Send_buffer) ;
      Net.close u )

(* A pool that cannot start a thread completes its jobs with EAGAIN. A detach
   after that completion must not crash (the review of the limit of detached
   jobs). The helper runs with a limit of one process. *)
let no_threads () =
  let helper =
    match Sys.getenv_opt "WEFT_NO_THREADS" with
    | Some c ->
        if Filename.is_relative c then Filename.concat (Sys.getcwd ()) c else c
    | None ->
        skip ~reason:"WEFT_NO_THREADS is not set" ()
  in
  if Sys.command "command -v prlimit > /dev/null" <> 0 then
    skip ~reason:"prlimit is not installed" () ;
  if Unix.getuid () = 0 then skip ~reason:"root has no process limit" () ;
  let out = Filename.concat (tmp ()) "out" in
  let code =
    Sys.command
      (Printf.sprintf "prlimit --nproc=1:1 %s > %s 2>&1" (Filename.quote helper)
         (Filename.quote out) )
  in
  equal int 0 code ;
  equal string "ok\n" (In_channel.with_open_text out In_channel.input_all)

(* The resource errors (issue 3). The helper runs with a limit of 64 fds
   (prlimit --nofile=64:64). *)
let few_fds () =
  let helper =
    match Sys.getenv_opt "WEFT_FEW_FDS" with
    | Some c ->
        if Filename.is_relative c then Filename.concat (Sys.getcwd ()) c else c
    | None ->
        skip ~reason:"WEFT_FEW_FDS is not set" ()
  in
  if Sys.command "command -v prlimit > /dev/null" <> 0 then
    skip ~reason:"prlimit is not installed" () ;
  let out = Filename.concat (tmp ()) "out" in
  let code =
    Sys.command
      (Printf.sprintf "prlimit --nofile=64:64 %s > %s 2>&1"
         (Filename.quote helper) (Filename.quote out) )
  in
  equal string "ok\n" (In_channel.with_open_text out In_channel.input_all) ;
  equal int 0 code

(* Processes and signals (milestone M2). *)

let read_pipe = Portable.drain Pipe.read

(* A shell starts a background job with SIGINT ignored, and a child inherits
   this action. Thus the test gives SIGINT its default action for the time of
   the spawn. *)
let with_default_sigint f =
  let old = Sys.signal Sys.sigint Sys.Signal_default in
  Fun.protect ~finally:(fun () -> Sys.set_signal Sys.sigint old) f

(* The child gets no blocked signals, also when run blocks SIGINT. *)
let child_sigint () =
  let before = open_fds () in
  with_default_sigint (fun () ->
      Weft_linux.run ~domains:1 ~signals:[SIGINT] (fun () ->
          let p = Process.spawn "sleep" ["10"] in
          Time.sleep Mono (Span.ms 20) ;
          Process.kill p SIGINT ;
          is_true (Process.wait p = Process.Signaled SIGINT) ) ) ;
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

(* The end of a watch (issue 6). The helper must end by SIGINT in each mode. *)
let unwatch_helper () =
  match Sys.getenv_opt "WEFT_UNWATCH" with
  | Some c ->
      if Filename.is_relative c then Filename.concat (Sys.getcwd ()) c else c
  | None ->
      skip ~reason:"WEFT_UNWATCH is not set" ()

let unwatch_modes () =
  let helper = unwatch_helper () in
  with_default_sigint (fun () ->
      Weft_linux.run ~domains:1 (fun () ->
          List.iter
            (fun mode ->
              let p = Process.spawn helper [mode] in
              let st =
                Fiber.with_timeout Mono (Span.s 10) (fun () -> Process.wait p)
              in
              if st <> Process.Signaled SIGINT then failwith mode )
            ["taken"; "count"; "pending"] ) )

(* After the unwatch, a SIGINT ends the process also when its only domain
   computes with no switch. *)
let unwatch_busy () =
  let helper = unwatch_helper () in
  with_default_sigint (fun () ->
      Weft_linux.run ~domains:1 (fun () ->
          let p = Process.spawn ~stdout:Piped helper ["busy"] in
          let out = Option.get (Process.stdout p) in
          let buf = Bytes.create 6 in
          equal int 6 (Pipe.read out buf 0 6) ;
          equal string "ready\n" (Bytes.to_string buf) ;
          Process.kill p SIGINT ;
          let st =
            Fiber.with_timeout Mono (Span.s 10) (fun () -> Process.wait p)
          in
          Pipe.close out ;
          is_true (st = Process.Signaled SIGINT) ) )

(* Run [f] in a fiber on domain 1. If the caller runs on domain 0, it computes,
   so domain 1 takes the new fiber. The caller can also run on domain 1, for
   example after a sleep. *)
let on_domain_1 f =
  let dom () = Private.Sched.prog "test" Private.Sched.Dom_index in
  let rec go tries =
    if tries = 0 then failwith "no fiber ran on domain 1" ;
    let ran = Atomic.make None in
    Fiber.scope (fun s ->
        Fiber.fork s (fun () ->
            let d = dom () in
            if d = 1 then f () ;
            Atomic.set ran (Some d) ) ;
        let t0 = Unix.gettimeofday () in
        while Atomic.get ran = None && Unix.gettimeofday () -. t0 < 0.1 do
          ()
        done ) ;
    if Atomic.get ran <> Some 1 then go (tries - 1)
  in
  if dom () = 1 then f () else go 20

(* In the process of the test: the waiter raises, a later received and a second
   unwatch raise, and SIGTERM stays an event. The thread of the unwatch and the
   thread of run do not block SIGUSR2 any more. *)
let unwatch_waiter () =
  let not_watched = "Signal.received: the world does not watch this signal" in
  Weft_linux.run ~domains:2 ~signals:[SIGUSR2; SIGTERM] (fun () ->
      equal (list string) [] (Weft_linux.unblocked_threads [SIGUSR2]) ;
      Fiber.scope (fun s ->
          let p =
            Fiber.fork_promise s (fun () ->
                match sync (Signal.received SIGUSR2) with
                | _ ->
                    "no error"
                | exception Invalid_argument m ->
                    m )
          in
          Time.sleep Mono (Span.ms 10) ;
          on_domain_1 (fun () -> Signal.unwatch SIGUSR2) ;
          equal string not_watched (Promise.await p) ) ;
      (* The thread of domain 0, the thread of run, unblocks SIGUSR2 at its next
         wait. The unwatch ran on domain 1. *)
      Time.sleep Mono (Span.ms 10) ;
      let unblocked = Weft_linux.unblocked_threads [SIGUSR2] in
      is_true (List.mem (string_of_int (Unix.getpid ())) unblocked) ;
      less int ~than:3 (List.length unblocked) ;
      raises (Invalid_argument not_watched) (fun () ->
          sync (Signal.received SIGUSR2) ) ;
      raises
        (Invalid_argument "Signal.unwatch: the world does not watch this signal")
        (fun () -> Signal.unwatch SIGUSR2 ) ;
      Unix.kill (Unix.getpid ()) Sys.sigterm ;
      at_least int ~than:1 (sync (Signal.received SIGTERM)) ) ;
  (* The run restores the mask of the calling thread. *)
  is_true
    (List.mem
       (string_of_int (Unix.getpid ()))
       (Weft_linux.unblocked_threads [SIGUSR2]) )

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

exception Lost_exn

(* M3: the finaliser of a failed promise that no fiber awaited gives the
   report. *)
let promise_report () =
  let got = ref [] in
  let old = Atomic.get Private.promise_report in
  Atomic.set Private.promise_report (fun _ e -> got := e :: !got) ;
  Fun.protect
    ~finally:(fun () -> Atomic.set Private.promise_report old)
    (fun () ->
      Weft_linux.run ~domains:1 (fun () ->
          Fiber.scope (fun s ->
              ignore (Fiber.fork_promise s (fun () -> raise Lost_exn)) ) ) ;
      Gc.full_major () ;
      Gc.full_major () ;
      is_true (List.mem Lost_exn !got) )

(* M3: the watchdog reports a fiber that computes for 200 ms with no switch, one
   time. A fiber that sleeps or yields gives no report. The watchdog measures
   the time of the wall clock, and on a busy host the OS can stop a thread for
   more than 10 ms. Thus the check of the fiber that yields uses a limit of 100
   ms. The fiber computes 200 ms in all, so the watchdog must count its
   yields. *)
let watchdog () =
  let busy_ms ms =
    let t0 = Unix.gettimeofday () in
    while Unix.gettimeofday () -. t0 < Float.of_int ms /. 1000. do
      ()
    done
  in
  let before = Weft_linux.watchdog_reports () in
  Weft_linux.run ~domains:1
    ~watchdog:(Some (Span.ms 100))
    (fun () ->
      Time.sleep Mono (Span.ms 50) ;
      for _ = 1 to 200 do
        busy_ms 1 ; Fiber.yield ()
      done ) ;
  equal int before (Weft_linux.watchdog_reports ()) ;
  Weft_linux.run ~domains:1 (fun () -> busy_ms 200) ;
  equal int (before + 1) (Weft_linux.watchdog_reports ()) ;
  Weft_linux.run ~domains:1 ~watchdog:None (fun () -> busy_ms 50) ;
  equal int (before + 1) (Weft_linux.watchdog_reports ())

(* M3: a Posix sleep ends after its deadline on the wall clock. A sleep on Mono
   also passes this test: a step of the clock needs root, so only the simulator
   tests the step. A busy host can delay the end of a sleep by more than 10 ms.
   Thus, as in the test of the 10 ms sleep, the limit of 500 ms finds only a
   timer that fires much too late. *)
let posix_sleep () =
  Weft_linux.run ~domains:1 (fun () ->
      let p0 = Time.now Posix and t0 = Time.now Mono in
      Time.sleep Posix (Span.ms 20) ;
      at_least int64 ~than:20_000_000L
        (Span.to_ns (Time.diff Posix (Time.now Posix) p0)) ;
      less int64 ~than:500_000_000L
        (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) )

(* M3: a reader whose reads always find data yields after 64 KiB, so a timer on
   the same domain is not late by much. The reader of a full pipe works 20 us on
   each chunk, and a fiber sleeps 1 ms 20 times. *)
let busy_reader () =
  let worst =
    Weft_linux.run ~domains:1 ~watchdog:None (fun () ->
        let p = Process.spawn ~stdout:Piped "cat" ["/dev/zero"] in
        let out = Option.get (Process.stdout p) in
        let stop = Atomic.make false and worst = ref 0L in
        Fiber.scope (fun s ->
            Fiber.fork s (fun () ->
                let buf = Bytes.create 65536 in
                while not (Atomic.get stop) do
                  ignore (Pipe.read out buf 0 65536) ;
                  let t1 = Unix.gettimeofday () in
                  while Unix.gettimeofday () -. t1 < 20e-6 do
                    ()
                  done
                done ) ;
            Fiber.fork s (fun () ->
                for _ = 1 to 20 do
                  let t = Time.now Mono in
                  Time.sleep Mono (Span.ms 1) ;
                  worst :=
                    Int64.max !worst
                      (Span.to_ns (Time.diff Mono (Time.now Mono) t))
                done ;
                Atomic.set stop true ) ) ;
        Process.kill p SIGKILL ;
        ignore (Process.wait p) ;
        Pipe.close out ;
        !worst )
  in
  less int64 ~than:20_000_000L worst

(* The review of M3, A: the expire inside wait resumes a pinned fiber on domain
   0. The domain must poll, not sleep. *)
let pinned_sleeps () =
  Weft_linux.run ~domains:1 ~slack:Span.zero (fun () ->
      Fiber.scope (fun s ->
          Fiber.fork ~pinned:true s (fun () ->
              for i = 1 to 20_000 do
                Time.sleep Mono (Span.ns (i mod 4000))
              done ) ) )

(* The review of M3, B: a pinned fiber that yields does not starve the other
   fibers of domain 0. *)
let pinned_yield () =
  Weft_linux.run ~domains:1 (fun () ->
      let flag = Atomic.make false in
      Fiber.scope (fun s ->
          Fiber.fork ~pinned:true s (fun () ->
              while not (Atomic.get flag) do
                Fiber.yield ()
              done ) ;
          Fiber.fork s (fun () -> Atomic.set flag true) ) )

let unwatched () =
  Weft_linux.run ~domains:1 (fun () ->
      raises
        (Invalid_argument
           "Signal.received: the world does not watch this signal" ) (fun () ->
          sync (Signal.received SIGUSR1) ) )

(* [Time.to_utc] and [Time.of_utc] agree with [Unix.gmtime] for random times in
   the full range of [Clock.posix], and for the days near the ends of the months
   and the years. *)
let utc_against_gmtime () =
  let st = Random.State.make [|7|] in
  let limit = 9_223_372_036 in
  let check secs =
    let t = Clock.of_ns Posix (Int64.mul (Int64.of_int secs) 1_000_000_000L) in
    let u = Time.to_utc t and g = Unix.gmtime (float_of_int secs) in
    if
      not
        ( u.year = g.tm_year + 1900
        && u.month = g.tm_mon + 1
        && u.day = g.tm_mday && u.hour = g.tm_hour && u.minute = g.tm_min
        && u.second = g.tm_sec && u.weekday = g.tm_wday )
    then fail (Printf.sprintf "to_utc differs from gmtime at %d s" secs) ;
    match Time.of_utc u with
    | Some t' when t' = t ->
        ()
    | _ ->
        fail (Printf.sprintf "of_utc is not the inverse of to_utc at %d s" secs)
  in
  for _ = 1 to 200_000 do
    check
      (Int64.to_int (Random.State.int64 st (Int64.of_int (2 * limit))) - limit)
  done ;
  (* Narrow ranges: near 1970, and near 2000 to 2100. *)
  for _ = 1 to 100_000 do
    check (Random.State.int st 400_000_000 - 200_000_000) ;
    check (946_684_800 + Random.State.int st 1_000_000_000)
  done ;
  (* Every day boundary from 1900 to 2100, and one second before it. *)
  for d = -25_567 to 47_482 do
    check (d * 86400) ;
    check ((d * 86400) - 1)
  done

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
    ; test "a detach after the completion of a pool with no thread" no_threads
    ; test "with no free fd, each operation that makes an fd gives EMFILE"
        few_fds
    ; test "UDP on IPv6" udp_ipv6
    ; test "local_addr gives IPv6 text, and 127.0.0.1 for localhost"
        local_addr_text
    ; test "a datagram of 65507 bytes goes through" udp_size_limit
    ; test "the sizes of the buffers, as in the simulator" option_sizes
    ; test "a detach of a job in the queue removes it" detach_queued
    ; test "each detached job that runs gives one more thread, up to the limit"
        detached_threads
    ; test "a pool that is full for more than 1 s writes a warning" full_warning
    ; test "a child stops at SIGINT" child_sigint
    ; test "the pipes of a child" child_output
    ; test "SIGTERM to the process is an event" sigterm
    ; test "C7: Other_signal 10 is SIGUSR1" signal_number
    ; test "each thread of run ~signals blocks the signals" blocked_threads
    ; test ~timeout:60. "after Signal.unwatch, SIGINT ends the process"
        unwatch_modes
    ; test ~timeout:30. "after Signal.unwatch, SIGINT ends a busy process"
        unwatch_busy
    ; test "a waiter at Signal.unwatch raises" unwatch_waiter
    ; test "a finaliser fills an Ivar from outside a fiber" finaliser_fill
    ; test "a failed promise that nobody awaits gives a report" promise_report
    ; test "the watchdog reports a fiber that computes 200 ms" watchdog
    ; test "a Posix sleep ends after its deadline" posix_sleep
    ; test "a reader that never waits yields after 64 KiB" busy_reader
    ; test ~timeout:30. "A: a pinned fiber with many short sleeps ends"
        pinned_sleeps
    ; test ~timeout:30. "B: a pinned fiber that yields does not starve"
        pinned_yield
    ; test "a signal that run does not watch is not an event" unwatched
    ; test "Time.to_utc agrees with Unix.gmtime" utc_against_gmtime ]

let () =
  exit
    (Windtrap.run "linux"
       ( Portable.tests world
       @ [ Portable.reset_details world
         ; group "linux on 4 domains"
             [ Portable.fiber_ids
                 { world with
                   name= "linux on 4 domains"
                 ; domains= 4
                 ; run= (fun f -> Weft_linux.run ~domains:4 f) } ]
         ; linux ] ) )
