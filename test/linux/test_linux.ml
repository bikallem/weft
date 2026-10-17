(* The tests on the Linux world (design section 2.7, and the "done" list of
   milestone M0). *)

open Windtrap
open Weft

let world =
  Portable.loopback_world ~name:"linux" ~domains:1
    {Portable.call= (fun f -> Weft_linux.run ~domains:1 f)}

let elapsed_ns = Portable.elapsed_ns world

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

(* The pool closes the fds of the files after the end of the operation (section
   2.7 of the design). Thus the count of fds first waits up to 10 s until the
   pool has no job ([pool_idle]). If not, a test can count the fd of a file that
   an earlier test closed, before the close. A wait for the correct count is not
   sufficient: the count can be correct for a short time, while a detached open
   of the pool did not get its fd yet. If the pool has jobs after 10 s, the test
   gives a failure. [sleep] waits 1 ms. In a run, it must be [in_run]: a wait
   with no switch for more than 10 ms gives a warning of the watchdog. *)
let pool_idle sleep =
  let rec wait n =
    if Weft_linux.pool_busy () > 0 then
      if n = 0 then fail "the pool has jobs after 10 s"
      else (
        sleep () ;
        wait (n - 1) )
  in
  wait 10_000

let in_run () = Time.sleep Mono (Span.ms 1)

let open_fds () =
  pool_idle (fun () -> Unix.sleepf 0.001) ;
  Array.length (Sys.readdir "/proc/self/fd")

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

(* An open of a FIFO blocks in the pool until a writer comes. The fiber detaches
   at the deadline. Then a writer comes, the job completes, and the pool closes
   the fd that the job opened. The writer stays open until the pool has no job,
   as in [release]: on a busy host, the detach can remove the open from the
   queue before it starts, and then a writer that waits for a reader blocks. *)
let detach_from_open () =
  let fifo = Filename.concat (Portable.tmp ()) "fifo" in
  Unix.mkfifo fifo 0o600 ;
  let before = open_fds () in
  let t =
    Weft_linux.run ~domains:1 (fun () ->
        let t0 = Time.now Mono in
        raises Fiber.Timeout (fun () ->
            Fiber.with_timeout Mono (Span.ms 20) (fun () ->
                File.openfile [Read] fifo ) ) ;
        let t = Span.to_ns (Time.diff Mono (Time.now Mono) t0) in
        let w = Unix.openfile fifo [O_RDWR] 0 in
        pool_idle in_run ; Unix.close w ; t )
  in
  at_least int64 ~than:20_000_000L t ;
  less int64 ~than:500_000_000L t ;
  equal int before (open_fds ())

(* C4: a job that blocks does not stop the next job of the pool. *)
let blocked_job () =
  let fifo = Filename.concat (Portable.tmp ()) "fifo" in
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
          (* A stat that waits for the open ends only after the writer, thus
             after the timeout. *)
          let ns =
            match
              Fiber.with_timeout Mono (Span.s 1) (fun () -> Promise.await st)
            with
            | d ->
                Span.to_ns d
            | exception Fiber.Timeout ->
                Int64.max_int
          in
          let w = File.openfile [Write] fifo in
          File.close w ;
          File.close (Promise.await r) ;
          less int64 ~than:500_000_000L ns ) ) ;
  equal int before (open_fds ())

(* 1,000 files open and close, and the pool closes each fd. *)
let no_file_leak () =
  let d = Portable.tmp () in
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
  equal int before (open_fds ()) ;
  equal int 1000 (Array.length (Sys.readdir d))

(* The limit of detached jobs (design section 2.3). The file pool has 16 threads
   for the live jobs, and 8 more for the detached jobs that run. An open of a
   FIFO holds a thread until a writer comes. *)
let pool_max = 16

let pool_limit = 8

(* A writer opens the FIFO, and each open that blocks on it ends. Then the
   fibers close the files of [held]. The writer stays open until the pool has no
   job, so that the detached opens also end: on a busy host, an open can start
   after the open of the writer. A writer that closed at once gave an open that
   never ended. On Linux, an open of a FIFO with O_RDWR does not block, also
   with no reader. *)
let release fifo held =
  let w = Unix.openfile fifo [O_RDWR] 0 in
  Fun.protect
    ~finally:(fun () -> Unix.close w)
    (fun () ->
      List.iter (fun p -> File.close (Promise.await p)) held ;
      pool_idle in_run )

(* [n] fibers that each hold a thread of the pool. *)
let hold s fifo n =
  let l =
    List.init n (fun _ ->
        Fiber.fork_promise s (fun () -> File.openfile [Read] fifo) )
  in
  Time.sleep Mono (Span.ms 50) ;
  l

let new_fifo () =
  let fifo = Filename.concat (Portable.tmp ()) "fifo" in
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
          release fifo held ;
          (* The pool has threads again: the job would run now. *)
          ignore (File.stat "/") ;
          Time.sleep Mono (Span.ms 20) ) ) ;
  is_false (Sys.file_exists file) ;
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
          release fifo held ) ) ;
  equal int before (open_fds ())

(* Run [f] with stderr in the file [log]. *)
let with_stderr log f =
  let fd = Unix.openfile log [O_WRONLY; O_CREAT] 0o600 in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      let saved = Unix.dup Unix.stderr in
      Fun.protect
        ~finally:(fun () ->
          Unix.dup2 saved Unix.stderr ;
          Unix.close saved )
        (fun () -> Unix.dup2 fd Unix.stderr ; f ()) )

(* A pool that is full for more than 1 s writes one warning to stderr. *)
let full_warning () =
  let fifo = new_fifo () in
  let log = Filename.concat (Filename.dirname fifo) "stderr" in
  let before = open_fds () in
  let w0 = Weft_linux.pool_warnings () in
  with_stderr log (fun () ->
      Weft_linux.run ~domains:1 (fun () ->
          Fiber.scope (fun s ->
              let held = hold s fifo pool_max in
              raises Fiber.Timeout (fun () ->
                  Fiber.with_timeout Mono (Span.ms 1300) (fun () ->
                      File.stat "/" ) ) ;
              release fifo held ) ) ) ;
  equal int before (open_fds ()) ;
  equal int (w0 + 1) (Weft_linux.pool_warnings ()) ;
  let text = In_channel.with_open_text log In_channel.input_all in
  equal string
    "weft: the file pool was full for 1 s: queued jobs 1, threads 16, detached \
     jobs that run 0\n"
    text

(* The target of the fd with the number [n] (a text), or [None] if the fd is not
   open. *)
let fd_target n =
  match Unix.readlink ("/proc/self/fd/" ^ n) with
  | l ->
      Some l
  | exception Unix.Unix_error _ ->
      None

let is_open n = Option.is_some (fd_target n)

(* The numbers of the open fds with the target [l]. *)
let fds_of l =
  List.filter
    (fun n -> fd_target n = Some l)
    (Array.to_list (Sys.readdir "/proc/self/fd"))

(* The Unix fd with the number [n]. On Unix, an fd is its number. *)
let fd_of_int : int -> Unix.file_descr = Obj.magic

(* The pool closes a file, because the close of a file can block (design section
   2.7). The world closes another fd at once, also if the fd has the number of a
   file that closed before. The fibers hold all threads of the pool, so a close
   in the pool waits in the queue. *)
let pool_closes_files () =
  let fifo = new_fifo () in
  let name = "pool-closes-this-file" in
  let path = Filename.concat (Filename.dirname fifo) name in
  let before = open_fds () in
  Weft_linux.run ~domains:1 (fun () ->
      let f = File.openfile [Write; Create] path in
      let n =
        match
          List.find_opt
            (fun n ->
              Option.fold ~none:false
                ~some:(String.ends_with ~suffix:("/" ^ name))
                (fd_target n) )
            (Array.to_list (Sys.readdir "/proc/self/fd"))
        with
        | Some n ->
            n
        | None ->
            fail "the file has no fd"
      in
      Fiber.scope (fun s ->
          let held = hold s fifo pool_max in
          File.close f ;
          is_true (is_open n) ;
          release fifo held ) ;
      is_false (is_open n) ;
      (* A socket gets the number of the file, in a maximum of 64 tries. *)
      let rec socket tries others =
        if tries = 0 then (
          List.iter Net.close others ;
          fail "no socket got the number of the file" ) ;
        let u = Net.udp ("127.0.0.1", 0) in
        if is_open n then (u, others) else socket (tries - 1) (u :: others)
      in
      let u, others = socket 64 [] in
      Fiber.scope (fun s ->
          let held = hold s fifo pool_max in
          Net.close u ;
          is_false (is_open n) ;
          release fifo held ) ;
      List.iter Net.close others ) ;
  equal int before (open_fds ())

(* A job that a run detaches can end after the close of the world. The close
   closes the mailboxes of the pool, so the worker then writes no eventfd. If
   not, the worker writes 8 bytes to the old number of the eventfd. Here, the
   write end of a pipe gets that number after the run. *)
let detached_after_close () =
  let fifo = new_fifo () in
  let n =
    Weft_linux.run ~domains:1 (fun () ->
        let n =
          match fds_of "anon_inode:[eventfd]" with
          | [n] ->
              fd_of_int (int_of_string n)
          | _ ->
              fail "the world does not have one eventfd"
        in
        raises Fiber.Timeout (fun () ->
            Fiber.with_timeout Mono (Span.ms 50) (fun () ->
                File.openfile [Read] fifo ) ) ;
        n )
  in
  (* Each fd in [opened] has its own number. *)
  let opened = ref [] in
  let keep fd =
    opened := fd :: !opened ;
    fd
  in
  Fun.protect
    ~finally:(fun () ->
      List.iter Unix.close !opened ;
      Sys.remove fifo )
    (fun () ->
      let r0, w = Unix.pipe ~cloexec:true () in
      let r0 = keep r0 and w = keep w in
      (* If the read end has the number, dup2 replaces it. *)
      let r = if r0 = n then keep (Unix.dup ~cloexec:true r0) else r0 in
      if w <> n then (
        Unix.dup2 ~cloexec:true w n ;
        if r0 <> n then ignore (keep n) ) ;
      ignore (keep (Unix.openfile fifo [O_RDWR] 0)) ;
      pool_idle (fun () -> Unix.sleepf 0.001) ;
      Unix.set_nonblock r ;
      let got =
        match Unix.read r (Bytes.create 8) 0 8 with
        | k ->
            k
        | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
            0
      in
      equal int 0 got )

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
  let out = Filename.concat (Portable.tmp ()) "out" in
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
  let out = Filename.concat (Portable.tmp ()) "out" in
  let code =
    Sys.command
      (Printf.sprintf "prlimit --nofile=64:64 %s > %s 2>&1"
         (Filename.quote helper) (Filename.quote out) )
  in
  equal string "ok\n" (In_channel.with_open_text out In_channel.input_all) ;
  equal int 0 code

(* Processes and signals (milestone M2). *)

let read_pipe = Portable.drain Pipe.read

(* The child gets no blocked signals, also when run blocks SIGINT. *)
let child_sigint () =
  let before = open_fds () in
  Portable.with_default_sigint (fun () ->
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

(* With stdin closed, the end of the parent for [~stdout:Piped] gets fd 0. The
   test closes stdin in the run, after the world opened its fds. *)
let piped_fd_zero () =
  Weft_linux.run ~domains:1 (fun () ->
      let saved = Unix.dup ~cloexec:true Unix.stdin in
      Unix.close Unix.stdin ;
      Fun.protect
        ~finally:(fun () ->
          Unix.dup2 ~cloexec:false saved Unix.stdin ;
          Unix.close saved )
        (fun () ->
          let p = Process.spawn ~stdout:Piped "sh" ["-c"; "echo hi"] in
          is_true
            (Option.fold ~none:false
               ~some:(String.starts_with ~prefix:"pipe:")
               (fd_target "0") ) ;
          let out = Option.get (Process.stdout p) in
          equal string "hi\n" (read_pipe out) ;
          Pipe.close out ;
          is_true (Process.wait p = Process.Exited 0) ) )

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
   blocks SIGHUP. After the run, the calling thread does not block SIGUSR2
   again. *)
let blocked_threads () =
  let self = string_of_int (Unix.getpid ()) in
  is_true (List.mem self (Weft_linux.unblocked_threads [SIGUSR2])) ;
  Weft_linux.run ~domains:1 ~signals:[SIGUSR2] (fun () ->
      equal (list string) [] (Weft_linux.unblocked_threads [SIGUSR2]) ;
      (* No thread blocks SIGHUP. *)
      is_true (Weft_linux.unblocked_threads [SIGUSR2; SIGHUP] <> []) ) ;
  (* The close of the world restores the mask of the calling thread. *)
  is_true (List.mem self (Weft_linux.unblocked_threads [SIGUSR2]))

(* The id of the calling thread, from /proc/thread-self ("pid/task/tid"). *)
let thread_id () = Filename.basename (Unix.readlink "/proc/thread-self")

(* The index of the domain of the calling fiber. *)
let dom () = Private.Sched.prog "test" Private.Sched.Dom_index

(* Compute for [s] seconds, or until [stop] gives [true]. *)
let spin ?(stop = fun () -> false) s =
  let t0 = Unix.gettimeofday () in
  while (not (stop ())) && Unix.gettimeofday () -. t0 < s do
    ()
  done

(* Run [f] in a fiber on domain 1, and give its result. The caller forks a fiber
   and computes for a maximum of 100 ms, so domain 1 takes the fiber. If the
   fiber runs on domain 0, the caller forks again, 20 times at most. The caller
   computes until [f] ends, and then [busy] seconds more. With [~sleep:true], it
   computes only until the fiber starts, and then it waits: thus domain 0 can
   sleep while [f] runs. *)
let fork_on_domain_1 ?(sleep = false) ?(busy = 0.) f =
  let rec go tries =
    if tries = 0 then failwith "no fiber ran on domain 1" ;
    let ran = Atomic.make None and ended = Atomic.make false in
    let r =
      Fiber.scope (fun s ->
          let p =
            Fiber.fork_promise s (fun () ->
                let d = dom () in
                Atomic.set ran (Some d) ;
                let r = if d = 1 then Some (f ()) else None in
                Atomic.set ended true ; r )
          in
          spin
            ~stop:(fun () ->
              if sleep then Atomic.get ran <> None else Atomic.get ended )
            0.1 ;
          if Atomic.get ran = Some 1 then spin busy ;
          Promise.await p )
    in
    match r with Some x -> x | None -> go (tries - 1)
  in
  go 20

(* Run [f] in a fiber on domain 1. The caller can also run on domain 1, for
   example after a sleep. *)
let on_domain_1 f = if dom () = 1 then f () else fork_on_domain_1 f

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

(* An unwatch on domain 1 leaves the unblock of the thread of domain 0 to that
   thread. Here, domain 1 waits first: after the unwatch, a pinned fiber
   computes on domain 0 for 50 ms. The wait of domain 1 must not take the
   unblock, and the thread of run unblocks the signal at its next wait. *)
let unwatch_unblock0 () =
  let self = string_of_int (Unix.getpid ()) in
  Weft_linux.run ~domains:2 ~watchdog:None ~signals:[SIGUSR2] (fun () ->
      Fiber.scope (fun s ->
          Fiber.fork ~pinned:true s (fun () ->
              fork_on_domain_1 ~busy:0.05 (fun () -> Signal.unwatch SIGUSR2) ;
              Time.sleep Mono (Span.ms 10) ;
              is_true (List.mem self (Weft_linux.unblocked_threads [SIGUSR2])) ) ) )

(* An unwatch on domain 1 unblocks the signal in its own thread at once. It also
   wakes domain 0, so that the thread of domain 0 unblocks the signal when
   domain 0 sleeps with no deadline. A fiber on domain 1 blocks its thread for
   50 ms, and domain 0 sleeps in this time. *)
let unwatch_wakes_domain0 () =
  let self = string_of_int (Unix.getpid ()) in
  let unblocked t = List.mem t (Weft_linux.unblocked_threads [SIGUSR2]) in
  Weft_linux.run ~domains:2 ~watchdog:None ~signals:[SIGUSR2] (fun () ->
      Fiber.scope (fun s ->
          Fiber.fork ~pinned:true s (fun () ->
              let own, zero =
                fork_on_domain_1 ~sleep:true (fun () ->
                    Unix.sleepf 0.05 ;
                    Signal.unwatch SIGUSR2 ;
                    let own = unblocked (thread_id ()) in
                    let t0 = Unix.gettimeofday () in
                    while
                      (not (unblocked self)) && Unix.gettimeofday () -. t0 < 0.5
                    do
                      Unix.sleepf 0.001
                    done ;
                    (own, unblocked self) )
              in
              is_true own ; is_true zero ) ) )

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

(* A new world can watch signals: no open world holds the claim. *)
let signals_free () = Weft_linux.close (Weft_linux.create ~signals:[SIGUSR1] ())

(* A limit of the watchdog that is not positive raises. The run closes the world
   first, so a later world can watch signals. *)
let watchdog_limit () =
  List.iter
    (fun ns ->
      raises
        (Invalid_argument
           "Weft_linux.run: the limit of the watchdog is not positive" )
        (fun () ->
          Weft_linux.run ~domains:1 ~signals:[SIGUSR1]
            ~watchdog:(Some (Span.ns ns))
            ignore ) ;
      signals_free () )
    [0; -1]

(* M3: a Posix sleep ends after its deadline on the wall clock. A sleep on Mono
   also passes this test: a step of the clock needs root, so only the simulator
   tests the step. A busy host can delay the end of one sleep by more than 10
   ms. Thus, as in the test of the 10 ms sleep, the median of 7 sleeps of 20 ms
   checks the precision (less than 30 ms), and the limit of 500 ms for each
   sleep finds a timer that fires much too late. *)
let posix_sleep () =
  let all =
    Weft_linux.run ~domains:1 (fun () ->
        List.init 7 (fun _ ->
            let p0 = Time.now Posix and t0 = Time.now Mono in
            Time.sleep Posix (Span.ms 20) ;
            at_least int64 ~than:20_000_000L
              (Span.to_ns (Time.diff Posix (Time.now Posix) p0)) ;
            let ns = Span.to_ns (Time.diff Mono (Time.now Mono) t0) in
            less int64 ~than:500_000_000L ns ;
            ns ) )
  in
  less int64 ~than:30_000_000L (List.nth (List.sort compare all) 3)

(* The world sets the alarm of a clock only for a deadline on that clock. In a
   new world, one sleep sets the alarm of its clock one time: after the alarm,
   the timerfd is not armed, and no deadline stays. *)
let alarm_sets () =
  let sets sleep =
    let n = Weft_linux.alarm_sets () in
    Weft_linux.run ~domains:1 ~watchdog:None sleep ;
    Weft_linux.alarm_sets () - n
  in
  equal int 1 (sets (fun () -> Time.sleep Mono (Span.ms 20))) ;
  equal int 1 (sets (fun () -> Time.sleep Posix (Span.ms 20)))

(* For each fd of the inode type [inode] (for example "timerfd"), the value of
   the line that starts with [field] (for example "ticks:") in its fdinfo. *)
let fdinfo inode field =
  let k = String.length field in
  List.filter_map
    (fun n ->
      List.find_map
        (fun l ->
          if String.starts_with ~prefix:field l then
            Some (String.trim (String.sub l k (String.length l - k)))
          else None )
        (In_channel.with_open_text ("/proc/self/fdinfo/" ^ n)
           In_channel.input_lines ) )
    (fds_of ("anon_inode:[" ^ inode ^ "]"))

(* The world reads the eventfd after a wake, and each timerfd after its alarm.
   The counts are then 0. A world on one domain has one eventfd and two
   timerfds. *)
let drained () =
  Weft_linux.run ~domains:1 ~watchdog:None (fun () ->
      (* A job of the pool writes the eventfd at its end. *)
      ignore (File.stat "/") ;
      equal (list string) ["0"] (fdinfo "eventfd" "eventfd-count:") ;
      Time.sleep Mono (Span.ms 20) ;
      equal (list string) ["0"; "0"] (fdinfo "timerfd" "ticks:") ;
      Time.sleep Posix (Span.ms 20) ;
      equal (list string) ["0"; "0"] (fdinfo "timerfd" "ticks:") )

(* M3: a reader whose reads always find data yields after 64 KiB, so a timer on
   the same domain is not late by much. The reader of a full pipe works 20 us on
   each chunk, and a fiber sleeps 1 ms 20 times. The OS can stop the thread for
   more than 10 ms on a busy host, and on a shared CPU a wake can come 5 ms
   late. Thus the median of the 20 sleeps is less than 10 ms, and each sleep
   less than 500 ms. *)
let busy_reader () =
  let all =
    Weft_linux.run ~domains:1 ~watchdog:None (fun () ->
        let p = Process.spawn ~stdout:Piped "cat" ["/dev/zero"] in
        let out = Option.get (Process.stdout p) in
        let stop = Atomic.make false and all = ref [] in
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
                  all := Span.to_ns (Time.diff Mono (Time.now Mono) t) :: !all
                done ;
                Atomic.set stop true ) ) ;
        Process.kill p SIGKILL ;
        ignore (Process.wait p) ;
        Pipe.close out ;
        !all )
  in
  List.iter (fun ns -> less int64 ~than:500_000_000L ns) all ;
  less int64 ~than:10_000_000L (List.nth (List.sort compare all) 10)

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

(* A Unix path of 107 bytes fits in sun_path with its NUL byte, and a path of
   108 bytes gives ENAMETOOLONG. The simulator has the same limit. *)
let unix_path_limit () =
  let dir = Portable.tmp () in
  let path n =
    Filename.concat dir (String.make (n - String.length dir - 1) 'p')
  in
  Weft_linux.run ~domains:1 (fun () ->
      let too_long f = equal string "ENAMETOOLONG" (Portable.error_of f) in
      too_long (fun () -> Net.listen (Net.Unix (path 108))) ;
      too_long (fun () -> Net.connect (Net.Unix (path 108))) ;
      let l = Net.listen (Net.Unix (path 107)) in
      let c = Net.connect (Net.Unix (path 107)) in
      List.iter Net.close [c; l] )

(* Only one world at a time can watch signals, as on kqueue. For a world with no
   signals, the claim is not necessary. A refused create does not release the
   claim of the open world, so a second try also raises. *)
let one_signal_world () =
  let another =
    Invalid_argument "Weft_linux.create: another world watches signals"
  in
  Weft_linux.run ~domains:1 ~signals:[SIGUSR1] (fun () ->
      raises another (fun () ->
          ignore (Weft_linux.create ~signals:[SIGUSR2] ()) ) ;
      raises another (fun () ->
          ignore (Weft_linux.create ~signals:[SIGUSR2] ()) ) ;
      Weft_linux.close (Weft_linux.create ()) ) ;
  (* After the close, a new world can watch signals. *)
  Weft_linux.close (Weft_linux.create ~signals:[SIGUSR2] ()) ;
  (* A second close of a world does nothing: it does not release the claim of a
     later world. *)
  let a = Weft_linux.create ~signals:[SIGUSR1] () in
  Weft_linux.close a ;
  let b = Weft_linux.create ~signals:[SIGUSR2] () in
  Weft_linux.close a ;
  raises another (fun () -> ignore (Weft_linux.create ~signals:[SIGUSR1] ())) ;
  Weft_linux.close b ;
  (* A number of domains out of the range raises before the claim. *)
  List.iter
    (fun domains ->
      raises
        (Invalid_argument
           "Weft_linux.create: the number of domains is not in the range 1 to \
            62" ) (fun () ->
          ignore (Weft_linux.create ~domains ~signals:[SIGUSR1] ()) ) )
    [0; 63] ;
  signals_free ()

(* create ~signals writes a warning for each thread that does not block the
   signals. A domain that starts before the create does not block them. *)
let unblocked_warning () =
  let log = Filename.concat (Portable.tmp ()) "stderr" in
  let tid = Atomic.make None and stop = Atomic.make false in
  let d =
    Domain.spawn (fun () ->
        Atomic.set tid (Some (thread_id ())) ;
        while not (Atomic.get stop) do
          Unix.sleepf 0.001
        done )
  in
  Fun.protect
    ~finally:(fun () -> Atomic.set stop true ; Domain.join d)
    (fun () ->
      spin ~stop:(fun () -> Atomic.get tid <> None) 10. ;
      let tid =
        match Atomic.get tid with
        | Some t ->
            t
        | None ->
            fail "the domain did not start in 10 s"
      in
      with_stderr log (fun () ->
          Weft_linux.close (Weft_linux.create ~signals:[SIGUSR1] ()) ) ;
      contains
        ~sub:
          ( "weft: thread " ^ tid
          ^ " does not block the signals of run ~signals: a signal can go to it\n"
          )
        (In_channel.with_open_text log In_channel.input_all) )

(* With stdin closed, the signalfd of the world is fd 0. The world gets a signal
   on it, an unwatch works, and the close of the world closes it. A world that
   does not get the signal leaves it pending, and the handler then takes it
   after the close. *)
let signalfd_zero () =
  let old = Sys.signal Sys.sigusr1 (Sys.Signal_handle ignore) in
  let saved = Unix.dup ~cloexec:true Unix.stdin in
  Unix.close Unix.stdin ;
  Fun.protect
    ~finally:(fun () ->
      Unix.dup2 ~cloexec:false saved Unix.stdin ;
      Unix.close saved ;
      Sys.set_signal Sys.sigusr1 old )
    (fun () ->
      Weft_linux.run ~domains:1 ~signals:[SIGUSR1; SIGUSR2] (fun () ->
          equal string "anon_inode:[signalfd]" (Unix.readlink "/proc/self/fd/0") ;
          Unix.kill (Unix.getpid ()) Sys.sigusr1 ;
          at_least int ~than:1
            (Fiber.with_timeout Mono (Span.ms 500) (fun () ->
                 sync (Signal.received SIGUSR1) ) ) ;
          Signal.unwatch SIGUSR2 ) ;
      is_false (is_open "0") )

(* The soft limit of open fds of this process, from /proc/self/limits. *)
let fd_soft_limit () =
  let l =
    List.find
      (String.starts_with ~prefix:"Max open files")
      (In_channel.with_open_text "/proc/self/limits" In_channel.input_lines)
  in
  List.nth (List.filter (( <> ) "") (String.split_on_char ' ' l)) 3

(* Run [f] with a soft limit of [n] open fds. prlimit changes the limit of this
   process. *)
let with_fd_limit n f =
  let pid = Unix.getpid () and old = fd_soft_limit () in
  let set soft =
    if
      Sys.command (Printf.sprintf "prlimit --pid %d --nofile=%s:" pid soft) <> 0
    then fail "prlimit cannot change the limit of open fds"
  in
  set (string_of_int n) ;
  Fun.protect ~finally:(fun () -> set old) f

(* Open fds until EMFILE. *)
let fill_fds () =
  let rec go acc =
    match Unix.dup ~cloexec:true Unix.stderr with
    | fd ->
        go (fd :: acc)
    | exception Unix.Unix_error (EMFILE, _, _) ->
        acc
  in
  go []

(* A create with signals that fails at an fd closes the fds that it made, and it
   restores the mask of the calling thread. The program few_fds.exe examines
   this in a process of its own, but a mutation run arms a mutant only in the
   process of the suite. Thus this test lowers the limit of open fds of the
   process of the suite. With [k] free fds, the create fails in the call
   [name]. *)
let failed_create () =
  if Sys.command "command -v prlimit > /dev/null" <> 0 then
    skip ~reason:"prlimit is not installed" () ;
  let self = string_of_int (Unix.getpid ()) in
  let before = open_fds () in
  let top =
    Array.fold_left
      (fun m n -> Int.max m (int_of_string n))
      0
      (Sys.readdir "/proc/self/fd")
  in
  with_fd_limit (top + 16) (fun () ->
      let spare = ref [] in
      let free k =
        for _ = 1 to k do
          match !spare with
          | fd :: l ->
              Unix.close fd ;
              spare := l
          | [] ->
              fail "no fd"
        done
      in
      Fun.protect
        ~finally:(fun () -> List.iter Unix.close !spare)
        (fun () ->
          spare := fill_fds () ;
          (* The two rows of timerfd_create are the timerfd of Mono, then the
             timerfd of Posix. *)
          List.iter
            (fun (k, name) ->
              free k ;
              ( match Weft_linux.create ~signals:[SIGUSR1] () with
              | w ->
                  Weft_linux.close w ;
                  fail (name ^ ": no error")
              | exception Failure m ->
                  equal string (name ^ ": Too many open files") m ) ;
              let again = fill_fds () in
              spare := again @ !spare ;
              equal int k (List.length again) ;
              (* The check of the mask reads /proc with one fd. *)
              free 1 ;
              is_true (List.mem self (Weft_linux.unblocked_threads [SIGUSR1])) ;
              spare := fill_fds () @ !spare )
            [ (0, "signalfd")
            ; (1, "epoll_create1")
            ; (2, "eventfd")
            ; (3, "timerfd_create")
            ; (4, "timerfd_create") ] ) ) ;
  signals_free () ;
  equal int before (open_fds ())

let linux =
  group "linux"
    [ test "a Unix domain socket echoes" unix_socket
    ; test ~timeout:5. "C3: a Register that fails raises in the waiter"
        register_failure
    ; test "L2: the peer of a Unix socket has a Unix address" unix_peer_addr
    ; test "L3: a port out of range gives an error" port_range
    ; test "a Unix path of 108 bytes gives ENAMETOOLONG" unix_path_limit
    ; test "Time.now Posix is the time of the system" posix_time
    ; test "an address that is not an IP address gives an error" invalid_address
    ; test "a 10 ms sleep ends between 10 ms and 12 ms"
        (Portable.sleep_10ms ~median:12_000_000 world)
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
    ; test "the pool closes a file, and only a file" pool_closes_files
    ; test "a job that ends after the close of the world writes no fd"
        detached_after_close
    ; test "a child stops at SIGINT" child_sigint
    ; test "the pipes of a child" child_output
    ; test "with stdin closed, the pipe of a child can be fd 0" piped_fd_zero
    ; test "SIGTERM to the process is an event" sigterm
    ; test "C7: Other_signal 10 is SIGUSR1" signal_number
    ; test "each thread of run ~signals blocks the signals" blocked_threads
    ; test ~timeout:60. "after Signal.unwatch, SIGINT ends the process"
        (Portable.unwatch_modes world)
    ; test ~timeout:30. "after Signal.unwatch, SIGINT ends a busy process"
        (Portable.unwatch_busy world)
    ; test "a waiter at Signal.unwatch raises" unwatch_waiter
    ; test
        "after an unwatch and a wait on domain 1, the thread of run unblocks \
         the signal"
        unwatch_unblock0
    ; test "an unwatch on domain 1 wakes domain 0" unwatch_wakes_domain0
    ; test "only one world at a time watches signals" one_signal_world
    ; test "create warns for a thread that does not block the signals"
        unblocked_warning
    ; test "with stdin closed, the signalfd is fd 0" signalfd_zero
    ; test "a create that fails closes its fds and restores the mask"
        failed_create
    ; test "a finaliser fills an Ivar from outside a fiber" finaliser_fill
    ; test "a failed promise that nobody awaits gives a report" promise_report
    ; test "the watchdog reports a fiber that computes 200 ms" watchdog
    ; test "a limit of the watchdog that is not positive raises" watchdog_limit
    ; test "a Posix sleep ends after its deadline" posix_sleep
    ; test "a sleep sets the alarm of its clock one time" alarm_sets
    ; test "the world reads its eventfd and its timerfds after an event" drained
    ; test "a reader that never waits yields after 64 KiB" busy_reader
    ; test ~timeout:30. "A: a pinned fiber with many short sleeps ends"
        pinned_sleeps
    ; test ~timeout:30. "B: a pinned fiber that yields does not starve"
        pinned_yield
    ; test "a signal that run does not watch is not an event" unwatched
    ; test "Time.to_utc agrees with Unix.gmtime" utc_against_gmtime ]

(* The review of 2026-10-16 (design section 3.12). *)

(* C3: the names of a large directory take linear time on the domain thread. The
   old code took 6.9 s for 40,000 names. [Resolve] keeps the first copy of each
   address, in order. *)
let many_names () =
  let n = 40_000 in
  let data =
    String.concat "" (List.init n (fun i -> Printf.sprintf "file-%06d\000" i))
  in
  let t0 = Sys.time () in
  equal int n (List.length (Weft_posix.names data)) ;
  equal int n (List.length (Weft_posix.addresses data)) ;
  less float_exact ~than:1.0 (Sys.time () -. t0) ;
  equal (list string) ["b"; "a"] (Weft_posix.addresses "b\000a\000b\000a\000")

(* C4: on Linux, accept(2) can give a pending network error of the new
   connection, and the world retries it. A test cannot cause these errors, so
   this test examines the set of errors. An accept on a UDP socket gives
   EOPNOTSUPP, and the world does not retry it: the accept raises. *)
let accept_retry () =
  equal (list string)
    [ "ECONNABORTED"
    ; "EPROTO"
    ; "ENETDOWN"
    ; "EHOSTUNREACH"
    ; "ENETUNREACH"
    ; "ENOPROTOOPT"
    ; "EHOSTDOWN"
    ; "ENONET" ]
    (List.map Weft_posix.errname (Array.to_list Weft_posix.accept_retry)) ;
  Weft_linux.run ~domains:1 (fun () ->
      let s = Net.udp ("127.0.0.1", 0) in
      raises_match
        (function Net.Error (Net.Other "EOPNOTSUPP", _) -> true | _ -> false)
        (fun () -> Net.accept s) ;
      Net.close s )

(* P4: a read and a write of 1 MiB copy between the buffer of the user and the C
   buffer of the job, with no OCaml string between them. Thus 10 reads and 10
   writes allocate much less than one buffer (131,072 words) on the major heap.
   The old code allocated a string for each read and for each write. The data at
   the offsets of the buffers is correct. *)
let large_file_io () =
  let size = 1 lsl 20 in
  let path = Filename.concat (Portable.tmp ()) "big" in
  Weft_linux.run ~domains:1 (fun () ->
      let f = File.openfile [File.Read; File.Write; File.Create] path in
      let src = Bytes.init (size + 3) (fun i -> Char.chr (i land 0xff)) in
      let dst = Bytes.make (size + 5) '.' in
      let major () = (Gc.quick_stat ()).Gc.major_words in
      let w0 = major () in
      for _ = 1 to 10 do
        equal int size (File.pwrite f ~file_offset:0 src 3 size) ;
        equal int size (File.pread f ~file_offset:0 dst 5 size)
      done ;
      less float_exact ~than:131072. (major () -. w0) ;
      equal string (Bytes.sub_string src 3 size) (Bytes.sub_string dst 5 size) ;
      equal string "....." (Bytes.sub_string dst 0 5) ;
      File.close f )

(* P6: a worker writes the wake fd only when it adds an id to an empty mailbox.
   The wake fd is the write end of a pipe here, so the bytes in the pipe count
   the writes (8 bytes for each). 10 jobs complete with no take: one write.
   After the take, the next job writes again. The old code wrote one time for
   each job. *)
let one_wake_write () =
  let module Op = Private.Op in
  let r, w = Weft_posix.pipe () in
  is_true (r >= 0) ;
  let p = Weft_posix.create_pool [|w|] in
  let start () =
    match Weft_posix.start_job p 0 (Op.Stat "/") with
    | Ok id ->
        id
    | _ ->
        fail "the job did not start"
  in
  let finish id =
    match Weft_posix.finish_job p id (Op.Stat "/") with
    | Ok _ ->
        ()
    | _ ->
        fail "the job did not finish"
  in
  let buf = Bytes.create 1024 in
  let written () =
    pool_idle (fun () -> Unix.sleepf 0.001) ;
    Int.max 0 (Weft_posix.fd_read r buf 0 1024)
  in
  let ids = List.init 10 (fun _ -> start ()) in
  equal int 8 (written ()) ;
  let taken = Weft_posix.completions p 0 [] in
  equal int 10 (List.length taken) ;
  let id = start () in
  equal int 8 (written ()) ;
  equal int 1 (List.length (Weft_posix.completions p 0 [])) ;
  List.iter finish (id :: ids) ;
  Weft_posix.close_pool p ;
  ignore (Weft_posix.close_fd r) ;
  ignore (Weft_posix.close_fd w)

(* K6: [random] of the world reads 8 bytes of the OS for each call. Each value
   is in the range 0 to [n - 1]. 100 values in the range 0 to [2^30 - 1] are
   almost all different. *)
let world_random () =
  let w = Weft_linux.create () in
  Fun.protect
    ~finally:(fun () -> Weft_linux.close w)
    (fun () ->
      List.iter
        (fun n ->
          for _ = 1 to 1000 do
            let x = Weft_linux.World.random w n in
            is_true (x >= 0 && x < n)
          done )
        [1; 2; 7; 1 lsl 30; max_int] ;
      let l = List.init 100 (fun _ -> Weft_linux.World.random w (1 lsl 30)) in
      at_least int ~than:90 (List.length (List.sort_uniq compare l)) )

(* K5: [run] checks the limit of the watchdog before it makes the world. Thus a
   bad limit makes no fd, and it raises its own error also while another world
   holds the claim of the signals. The old code made the world first, and then
   [create] raised the error of the claim. *)
let limit_before_create () =
  let a = Weft_linux.create ~signals:[SIGUSR1] () in
  Fun.protect
    ~finally:(fun () -> Weft_linux.close a)
    (fun () ->
      let before = open_fds () in
      raises
        (Invalid_argument
           "Weft_linux.run: the limit of the watchdog is not positive" )
        (fun () ->
          Weft_linux.run ~domains:1 ~signals:[SIGUSR2]
            ~watchdog:(Some (Span.ns 0))
            ignore ) ;
      equal int before (open_fds ()) )

let review_2026 =
  group "review of 2026-10-16"
    [ test "C3: the names of 40,000 entries take linear time" many_names
    ; test "C4: accept retries the pending network errors on Linux" accept_retry
    ; test "P4: a read and a write of 1 MiB allocate no OCaml string"
        large_file_io
    ; test "P6: a worker writes the wake fd only for an empty mailbox"
        one_wake_write
    ; test "K5: a bad limit of the watchdog makes no world" limit_before_create
    ; test "K6: the random of the world is in the range 0 to n - 1" world_random
    ]

let () =
  exit
    (Windtrap.run "linux"
       ( Portable.tests world
       @ [ Portable.reset_details world
         ; Portable.zero_writes world
         ; group "linux on 4 domains"
             [ Portable.fiber_ids
                 { world with
                   name= "linux on 4 domains"
                 ; domains= 4
                 ; run= (fun f -> Weft_linux.run ~domains:4 f) } ]
         ; linux
         ; review_2026 ] ) )
