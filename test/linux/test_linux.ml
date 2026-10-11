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
      (* With no DNS server, the error is not ENOENT. *)
      match Dns.resolve "no-such-host.invalid" with
      | _ ->
          failwith "no error"
      | exception Dns.Error (_, "Dns.resolve") ->
          () )

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
  let rec wait n =
    if open_fds () <> before && n > 0 then (
      Unix.sleepf 0.001 ;
      wait (n - 1) )
  in
  wait 1000 ;
  equal int before (open_fds ())

(* 1,000 files open and close, and the pool closes each fd. *)
let no_file_leak () =
  let d = tmp () in
  let before = open_fds () in
  Weft_linux.run ~domains:2 (fun () ->
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
  let rec wait n =
    if open_fds () <> before && n > 0 then (
      Unix.sleepf 0.001 ;
      wait (n - 1) )
  in
  wait 1000 ;
  equal int before (open_fds ()) ;
  equal int 1000 (Array.length (Sys.readdir d))

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
    ; test "1,000 files open and close with no fd leak" no_file_leak ]

let () = exit (Windtrap.run "linux" (Portable.tests world @ [linux]))
