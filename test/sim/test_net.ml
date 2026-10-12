(* The tests of Net on the simulator (design sections 2.7 and 2.8, and the
   "done" list of milestone M0). *)

open Windtrap
open Weft
module Sim = Weft_sim

let ms_since t0 =
  Int64.to_int
    (Int64.div (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) 1_000_000L)

(* Read until [n] bytes or the end of the stream. *)
let read_n c n =
  let buf = Bytes.create n in
  let rec loop off =
    if off = n then off
    else match Net.read c buf off (n - off) with 0 -> off | k -> loop (off + k)
  in
  let got = loop 0 in
  Bytes.sub_string buf 0 got

let read_all c =
  let b = Buffer.create 64 and buf = Bytes.create 4096 in
  let rec loop () =
    match Net.read c buf 0 4096 with
    | 0 ->
        Buffer.contents b
    | k ->
        Buffer.add_subbytes b buf 0 k ;
        loop ()
  in
  loop ()

(* An echo server: each connection sends back what it reads. *)
let echo_server l =
  Fiber.scope (fun s ->
      while true do
        let c, _ = Net.accept l in
        Fiber.fork ~name:"echo" s (fun () ->
            let buf = Bytes.create 1024 in
            let rec loop () =
              match Net.read c buf 0 1024 with
              | 0 ->
                  Net.close c
              | k ->
                  ignore (Net.write_sub c buf 0 k) ;
                  loop ()
            in
            loop () )
      done )

let echo_once ?(domains = 1) seed =
  Sim.run ~seed ~domains (fun sim ->
      let addr = Sim.addr sim "server:7" in
      let l = Net.listen addr in
      Fiber.scope (fun s ->
          Fiber.fork_daemon ~name:"server" s (fun () -> echo_server l) ;
          let c = Net.connect addr in
          Net.write c "hello" ;
          let r = read_n c 5 in
          Net.close c ; r ) )

let echo_1000_seeds () =
  for seed = 0 to 999 do
    equal ~msg:(Printf.sprintf "seed %d" seed) string "hello" (echo_once seed)
  done

(* The 408 test of section 2.8: a head that is not complete gives 408 after 10
   s. *)
let rec contains_crlf2 s i =
  i + 3 < String.length s
  && (String.sub s i 4 = "\r\n\r\n" || contains_crlf2 s (i + 1))

let http_server l =
  Fiber.scope (fun s ->
      while true do
        let c, _ = Net.accept l in
        Fiber.fork ~name:"handler" s (fun () ->
            let head = Buffer.create 64 and buf = Bytes.create 256 in
            let rec read_head () =
              if not (contains_crlf2 (Buffer.contents head) 0) then
                match Net.read c buf 0 256 with
                | 0 ->
                    ()
                | k ->
                    Buffer.add_subbytes head buf 0 k ;
                    read_head ()
            in
            ( match Fiber.with_timeout Mono (Span.s 10) read_head with
            | () ->
                Net.write c "HTTP/1.1 200 OK\r\n\r\n"
            | exception Fiber.Timeout ->
                Net.write c "HTTP/1.1 408 Request Timeout\r\n\r\n" ) ;
            Net.close c )
      done )

let status_of response = String.sub response 9 3

let t408 ?(domains = 1) seed =
  Sim.run ~seed ~domains (fun sim ->
      Fiber.scope (fun s ->
          let addr = Sim.addr sim "server:80" in
          let l = Net.listen addr in
          Fiber.fork_daemon ~name:"server" s (fun () -> http_server l) ;
          let c = Net.connect addr in
          Net.write c "GET / HT" ;
          let t0 = Time.now Mono in
          let r = read_all c in
          (status_of r, ms_since t0) ) )

let t408_1000_seeds () =
  for seed = 0 to 999 do
    equal
      ~msg:(Printf.sprintf "seed %d" seed)
      (pair string int) ("408", 10_000) (t408 seed)
  done

let t200_full_head () =
  Sim.run (fun sim ->
      Fiber.scope (fun s ->
          let addr = Sim.addr sim "server:80" in
          let l = Net.listen addr in
          Fiber.fork_daemon s (fun () -> http_server l) ;
          let c = Net.connect addr in
          Net.write c "GET / HTTP/1.1\r\n\r\n" ;
          equal string "200" (status_of (read_all c)) ) )

let error_of f =
  match f () with
  | _ ->
      "no error"
  | exception Net.Error (e, _) ->
      Format.asprintf "%a" Private.Op.pp_error e

let refused_before_listen () =
  Sim.run (fun sim ->
      equal string "ECONNREFUSED"
        (error_of (fun () -> Net.connect (Sim.addr sim "server:1"))) )

let listen_twice () =
  Sim.run (fun sim ->
      let a = Sim.addr sim "server:2" in
      let _l = Net.listen a in
      equal string "EADDRINUSE" (error_of (fun () -> Net.listen a)) )

let eof_and_epipe () =
  Sim.run (fun sim ->
      let a = Sim.addr sim "server:3" in
      let l = Net.listen a in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              let c, _ = Net.accept l in
              Net.write c "bye" ; Net.close c ) ;
          let c = Net.connect a in
          equal string "bye" (read_all c) ;
          (* The peer closed: the stream ended, and a write gives EPIPE. *)
          equal string "EPIPE" (error_of (fun () -> Net.write c "x")) ) )

let shutdown_gives_eof () =
  Sim.run (fun sim ->
      let a = Sim.addr sim "server:4" in
      let l = Net.listen a in
      Fiber.scope (fun s ->
          let p =
            Fiber.fork_promise s (fun () ->
                let c, _ = Net.accept l in
                let r = read_all c in
                Net.write c (String.uppercase_ascii r) ;
                Net.close c ;
                r )
          in
          let c = Net.connect a in
          Net.write c "ping" ;
          Net.shutdown c ;
          equal string "PING" (read_all c) ;
          equal string "ping" (Promise.await p) ) )

(* A fiber that waits on a socket gets EBADF when another fiber closes it. *)
let close_wakes_waiter () =
  Sim.run (fun sim ->
      let a = Sim.addr sim "server:5" in
      let l = Net.listen a in
      Fiber.scope (fun s ->
          let p =
            Fiber.fork_promise s (fun () -> error_of (fun () -> Net.accept l))
          in
          Fiber.yield () ;
          Net.close l ;
          equal string "EBADF" (Promise.await p) ) )

(* A late write after close gives EBADF and closes nothing. *)
let late_write_after_close () =
  Sim.run (fun sim ->
      let a = Sim.addr sim "server:6" in
      let l = Net.listen a in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              let c, _ = Net.accept l in
              equal string "abc" (read_all c) ) ;
          let c = Net.connect a in
          let other = Net.connect (Sim.addr sim "server:6") in
          Net.write c "abc" ;
          Net.close c ;
          equal string "EBADF" (error_of (fun () -> Net.write c "late")) ;
          Net.close c ;
          (* The other socket still works. *)
          Net.close other ) )

let big_transfer () =
  Sim.run (fun sim ->
      let a = Sim.addr sim "server:8" in
      let l = Net.listen a in
      let data = String.init (1 lsl 20) (fun i -> Char.chr (i land 0xff)) in
      Fiber.scope (fun s ->
          Fiber.fork_daemon s (fun () -> echo_server l) ;
          let c = Net.connect a in
          let got =
            Fiber.scope (fun s2 ->
                let r =
                  Fiber.fork_promise s2 (fun () ->
                      read_n c (String.length data) )
                in
                Net.write c data ; Promise.await r )
          in
          is_true (String.equal data got) ) )

let latency_round_trip () =
  Sim.run (fun sim ->
      Sim.set_latency sim (Span.ms 5) ;
      let a = Sim.addr sim "server:9" in
      let l = Net.listen a in
      Fiber.scope (fun s ->
          Fiber.fork_daemon s (fun () -> echo_server l) ;
          let t0 = Time.now Mono in
          let c = Net.connect a in
          equal ~msg:"connect" int 5 (ms_since t0) ;
          let t1 = Time.now Mono in
          Net.write c "hi" ;
          equal string "hi" (read_n c 2) ;
          equal ~msg:"echo" int 10 (ms_since t1) ) )

let settle_runs_others () =
  Sim.run (fun sim ->
      let n = ref 0 in
      Fiber.scope (fun s ->
          for _ = 1 to 3 do
            Fiber.fork s (fun () -> Fiber.yield () ; incr n)
          done ;
          Sim.settle sim ;
          equal int 3 !n ) )

let manual_mode () =
  Sim.run ~auto:false (fun sim ->
      let t0 = Time.now Mono in
      let woke = ref [] in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              Time.sleep Mono (Span.s 1) ;
              woke := ms_since t0 :: !woke ) ;
          Fiber.fork s (fun () ->
              Time.sleep Mono (Span.s 3) ;
              woke := ms_since t0 :: !woke ) ;
          ( match Time.add Mono t0 (Span.s 2) with
          | Some at ->
              Sim.advance_until sim at
          | None ->
              assert false ) ;
          equal (list int) [1000] !woke ;
          equal int 2000 (ms_since t0) ;
          match Time.add Mono t0 (Span.s 5) with
          | Some at ->
              Sim.advance_until sim at
          | None ->
              assert false ) ;
      equal (list int) [3000; 1000] !woke )

let manual_mode_deadlock () =
  match Sim.run ~auto:false (fun _ -> Time.sleep Mono (Span.s 1)) with
  | () ->
      failwith "no deadlock"
  | exception Sim.Deadlock (first :: _) ->
      equal string "manual mode: only advance_until moves the time" first
  | exception Sim.Deadlock [] ->
      failwith "no message"

let advance_in_auto () =
  Sim.run (fun sim ->
      raises_match
        (function Invalid_argument _ -> true | _ -> false)
        (fun () -> Sim.advance_until sim (Time.now Mono)) )

let read_deadlock () =
  match
    Sim.run (fun sim ->
        let a = Sim.addr sim "server:10" in
        let l = Net.listen a in
        Fiber.scope (fun s ->
            Fiber.fork ~name:"reader" s (fun () ->
                let c, _ = Net.accept l in
                ignore (read_all c) ) ;
            let _c = Net.connect a in
            Time.sleep Mono (Span.s 1) ) )
  with
  | () ->
      failwith "no deadlock"
  | exception Sim.Deadlock names ->
      equal (list string) ["main"; "reader"] names

(* The order of the OS: close(2) of a registered fd is a model error. *)
let close_before_deregister () =
  match
    Sim.run (fun sim ->
        let a = Sim.addr sim "server:11" in
        let l : Private.Net.t = Net.listen a in
        Fiber.scope (fun s ->
            Fiber.fork s (fun () -> ignore (Net.accept l)) ;
            Fiber.yield () ;
            ignore
              (Private.World.perform "test"
                 (Private.World.Submit (Private.Op.Close l.Private.Sched.os)) ) ) )
  with
  | () ->
      failwith "no model error"
  | exception Sim.Model_error m ->
      contains ~sub:"before the Deregister" m

(* 10,000 connections open and close, and the slots of the fd table are used
   again. *)
let many_connections () =
  Sim.run (fun sim ->
      let a = Sim.addr sim "server:12" in
      let l = Net.listen a in
      Fiber.scope (fun s ->
          Fiber.fork_daemon s (fun () -> echo_server l) ;
          let before = !Private.Sched.Fdtab.next in
          for _ = 1 to 10_000 do
            let c = Net.connect a in
            Net.write c "x" ;
            equal string "x" (read_n c 1) ;
            Net.close c
          done ;
          Fiber.yield () ;
          less int ~than:(before + 16) !Private.Sched.Fdtab.next ) )

let addr_parse () =
  Sim.run (fun sim ->
      is_true (Sim.addr sim "server:80" = Net.Tcp ("server", 80)) ;
      is_true (Sim.addr sim "unix:/run/s" = Net.Unix "/run/s") ;
      raises_match
        (function Invalid_argument _ -> true | _ -> false)
        (fun () -> Sim.addr sim "server") ;
      raises_match
        (function Invalid_argument _ -> true | _ -> false)
        (fun () -> Sim.addr sim "server:x") )

let invalid = function Invalid_argument _ -> true | _ -> false

let bad_ranges () =
  Sim.run (fun sim ->
      let a = Sim.addr sim "server:13" in
      let _l = Net.listen a in
      let c = Net.connect a in
      let b = Bytes.create 4 in
      List.iter
        (fun (off, len) ->
          raises_match invalid (fun () -> Net.read c b off len) ;
          raises_match invalid (fun () -> Net.write_sub c b off len) )
        [(-1, 1); (0, -1); (0, 5); (4, 1); (5, 0); (3, 2)] ;
      (* A read needs at least one byte. A write of 0 bytes gives 0. *)
      raises_match invalid (fun () -> Net.read c b 0 0) ;
      raises_match invalid (fun () -> Net.read c b 4 0) ;
      equal int 0 (Net.write_sub c b 4 0) ;
      equal int 4 (Net.write_sub c b 0 4) )

let partial_read () =
  Sim.run (fun sim ->
      let a = Sim.addr sim "server:14" in
      let l = Net.listen a in
      let c = Net.connect a in
      let s, _ = Net.accept l in
      Net.write c "hello" ;
      let b = Bytes.create 5 in
      equal int 2 (Net.read s b 0 2) ;
      (* Ask for more than what is left: the read gives the rest. *)
      let rest = Bytes.create 5 in
      equal int 3 (Net.read s rest 0 5) ;
      Bytes.blit rest 0 b 2 3 ;
      equal string "hello" (Bytes.to_string b) ;
      Net.write c "!" ;
      equal int 1 (Net.read s b 0 5) ;
      equal char '!' (Bytes.get b 0) )

let backlog_limit () =
  Sim.run (fun sim ->
      let a = Sim.addr sim "server:15" in
      let _l = Net.listen ~backlog:1 a in
      let _c1 = Net.connect a in
      equal string "ECONNREFUSED" (error_of (fun () -> Net.connect a)) )

let client_ports_differ () =
  Sim.run (fun sim ->
      let a = Sim.addr sim "server:16" in
      let l = Net.listen a in
      let _c1 = Net.connect a and _c2 = Net.connect a in
      let _, p1 = Net.accept l in
      let _, p2 = Net.accept l in
      is_true (p1 <> p2) )

(* The bytes that the latency holds count against the buffer of the peer. *)
let inflight_backpressure () =
  Sim.run (fun sim ->
      Sim.set_latency sim (Span.ms 5) ;
      let a = Sim.addr sim "server:17" in
      let l = Net.listen a in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () -> ignore (Net.accept l)) ;
          let c = Net.connect a in
          let b = Bytes.create (200 * 1024) in
          equal int 65536 (Net.write_sub c b 0 (Bytes.length b)) ;
          raises Fiber.Timeout (fun () ->
              Fiber.with_timeout Mono (Span.ms 1) (fun () ->
                  ignore (Net.write_sub c b 0 (Bytes.length b)) ) ) ) )

(* A Register of an fd that is ready gives one edge at once. *)
let register_ready_fd () =
  Sim.run (fun sim ->
      let a = Sim.addr sim "server:18" in
      let l = Net.listen a in
      let c = Net.connect a in
      sync (Net.readable l) ;
      let _s, _ = Net.accept l in
      (* A new connection is writable. *)
      sync (Net.writable c) )

(* A listener that closes resets the connections that it did not accept. *)
let reset_by_listener () =
  Sim.run (fun sim ->
      let a = Sim.addr sim "server:21" in
      let l = Net.listen a in
      let c = Net.connect a in
      Net.close l ;
      sync (Net.readable c) ;
      sync (Net.writable c) ;
      equal string "ECONNRESET"
        (error_of (fun () -> Net.read c (Bytes.create 1) 0 1)) )

let refused_while_connecting () =
  Sim.run (fun sim ->
      Sim.set_latency sim (Span.ms 5) ;
      let a = Sim.addr sim "server:19" in
      let l = Net.listen a in
      Fiber.scope (fun s ->
          let p =
            Fiber.fork_promise s (fun () -> error_of (fun () -> Net.connect a))
          in
          Time.sleep Mono (Span.ms 1) ;
          Net.close l ;
          equal string "ECONNREFUSED" (Promise.await p) ) )

(* A planted lost wakeup on an fd: a parked fiber is a waiter of a ready fd, but
   it is not in a queue. *)
let planted_fd_lost_wakeup () =
  match
    Sim.run (fun sim ->
        let a = Sim.addr sim "server:20" in
        let l : Private.Net.t = Net.listen a in
        Fiber.scope (fun s ->
            Fiber.fork ~name:"victim" s (fun () ->
                sync (Ivar.read (Ivar.create ())) ) ;
            (* Register the listener: wait on it with a timeout. *)
            ignore (sync (choose [Net.readable l; Time.after Mono (Span.ms 1)])) ;
            let _c = Net.connect a in
            (* The poll takes the edge of the connect before the plant. *)
            Time.sleep Mono (Span.ms 1) ;
            let sched = Sim.sched sim in
            let victim =
              List.assoc "victim" (Private.Sched_debug.parked sched)
            in
            let reg = Hashtbl.find sched.doms.(0).regs l.slot in
            reg.rwait <- Private.Sched.Waiters.add victim 0 reg.rwait ;
            reg.rflag <- false ;
            Time.sleep Mono (Span.s 1) ) )
  with
  | () ->
      failwith "no lost wakeup"
  | exception Sim.Lost_wakeup problems ->
      is_true
        (List.exists
           (fun p -> String.starts_with ~prefix:"victim waits to read fd" p)
           problems )

(* L5: a cancel of advance_until ends the advance. *)
let advance_cancelled () =
  Sim.run ~auto:false (fun sim ->
      let t0 = Time.now Mono in
      let at n = Option.get (Time.add Mono t0 (Span.s n)) in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () -> Sim.advance_until sim (at 5)) ;
          Fiber.yield () ;
          Fiber.cancel s Exit ) ;
      Sim.advance_until sim (at 1) ;
      equal int 1000 (ms_since t0) )

(* L6: the connections in flight count against the backlog. *)
let backlog_counts_in_flight () =
  Sim.run (fun sim ->
      Sim.set_latency sim (Span.ms 5) ;
      let a = Sim.addr sim "server:22" in
      let _l = Net.listen ~backlog:1 a in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () -> ignore (Net.connect a)) ;
          Fiber.yield () ;
          equal string "ECONNREFUSED" (error_of (fun () -> Net.connect a)) ) )

(* UDP in the simulator. *)

let send u dst msg =
  Net.sendto u (Bytes.of_string msg) 0 (String.length msg) dst

(* The datagrams that came, after a wait of 10 ms. *)
let received u =
  Time.sleep Mono (Span.ms 10) ;
  let buf = Bytes.create 70_000 in
  let rec loop acc =
    match
      Fiber.with_timeout Mono (Span.ms 1) (fun () ->
          let k, _ = Net.recvfrom u buf 0 (Bytes.length buf) in
          Bytes.sub_string buf 0 k )
    with
    | d ->
        loop (d :: acc)
    | exception Fiber.Timeout ->
        List.rev acc
  in
  loop []

let udp_pair () = (Net.udp ("a", 1), ("a", 1), Net.udp ("b", 1))

let udp_loss () =
  Sim.run ~seed:7 (fun sim ->
      Sim.net_faults sim {Sim.no_faults with udp_loss= true} ;
      let a, a_addr, b = udp_pair () in
      (* 8 groups of 100: the queue takes about 160 small datagrams. *)
      let n = ref 0 in
      for g = 0 to 7 do
        for i = 1 to 100 do
          send b a_addr (string_of_int ((100 * g) + i))
        done ;
        n := !n + List.length (received a)
      done ;
      (* 700 on average. *)
      at_least int ~than:600 !n ;
      less int ~than:780 !n )

let udp_order () =
  let order faults =
    Sim.run ~seed:3 (fun sim ->
        Sim.net_faults sim faults ;
        let a, a_addr, b = udp_pair () in
        for i = 1 to 50 do
          send b a_addr (string_of_int i)
        done ;
        List.map int_of_string (received a) )
  in
  let sent = List.init 50 (fun i -> i + 1) in
  equal (list int) sent (order Sim.no_faults) ;
  let late = order {Sim.no_faults with jitter= Span.ms 2} in
  equal (list int) sent (List.sort compare late) ;
  is_false (late = sent)

(* A full queue loses the datagrams that do not fit. The counts are near the
   counts of Linux: 92, 86 and 1 (see [dgram_cost] in the simulator). *)
let udp_full_queue () =
  let kept rcvbuf =
    Sim.run (fun _ ->
        let a, a_addr, b = udp_pair () in
        Option.iter (Net.set_option a Recv_buffer) rcvbuf ;
        for _ = 1 to 300 do
          send b a_addr (String.make 1000 'x')
        done ;
        List.length (received a) )
  in
  equal int 93 (kept None) ;
  equal int 87 (kept (Some 100_000)) ;
  equal int 1 (kept (Some 1000)) ;
  (* A read frees the cost of its datagram. *)
  Sim.run (fun _ ->
      let a, a_addr, b = udp_pair () in
      Net.set_option a Recv_buffer 1000 ;
      for i = 1 to 3 do
        send b a_addr (string_of_int i) ;
        equal (list string) [string_of_int i] (received a)
      done )

let udp_size () =
  Sim.run (fun _ ->
      let a, a_addr, b = udp_pair () in
      send b a_addr (String.make 65507 'x') ;
      equal (list int) [65507] (List.map String.length (received a)) ;
      raises_match
        (function Net.Error (Other "EMSGSIZE", _) -> true | _ -> false)
        (fun () -> send b a_addr (String.make 65508 'x')) )

let udp_latency () =
  Sim.run (fun sim ->
      Sim.set_latency sim (Span.ms 5) ;
      let a, a_addr, b = udp_pair () in
      let t0 = Time.now Mono in
      send b a_addr "x" ;
      let buf = Bytes.create 1 in
      let k, from = Net.recvfrom a buf 0 1 in
      equal int 1 k ;
      is_true (from = ("b", 1)) ;
      equal int 5 (ms_since t0) )

let udp_free_ports () =
  Sim.run (fun _ ->
      let a = Net.udp ("h", 0) and b = Net.udp ("h", 0) in
      let buf = Bytes.create 1 in
      send a ("h", 40000) "x" ;
      let _, from = Net.recvfrom a buf 0 1 in
      (* [a] sent to itself, at the first free port. *)
      is_true (from = ("h", 40000)) ;
      send b ("h", 40000) "y" ;
      let _, from = Net.recvfrom a buf 0 1 in
      is_true (from = ("h", 40001)) )

(* The sizes follow Linux with its default limits. *)
let option_sizes () =
  Sim.run (fun _ ->
      let u = Net.udp ("h", 1) in
      equal int 212992 (Net.get_option u Recv_buffer) ;
      Net.set_option u Recv_buffer 1000 ;
      equal int 2304 (Net.get_option u Recv_buffer) ;
      Net.set_option u Recv_buffer 100_000 ;
      equal int 200_000 (Net.get_option u Recv_buffer) ;
      Net.set_option u Send_buffer 1_000_000 ;
      equal int 425984 (Net.get_option u Send_buffer) ;
      Net.set_option u Send_buffer 1 ;
      equal int 4608 (Net.get_option u Send_buffer) ;
      equal string "ENOPROTOOPT"
        (Portable.error_of (fun () -> Net.set_option u No_delay true)) ;
      let l = Net.listen (Net.Unix "/s") in
      equal string "EOPNOTSUPP"
        (Portable.error_of (fun () -> Net.set_option l No_delay true)) ;
      (* The defaults of a TCP socket. *)
      let t = Net.listen (Net.Tcp ("h", 1)) in
      equal int 131072 (Net.get_option t Recv_buffer) ;
      equal int 16384 (Net.get_option t Send_buffer) )

(* The port 0 goes around after 65535, from 40000 again. *)
let udp_ports_go_around () =
  Sim.run (fun _ ->
      for _ = 40000 to 65535 + 1 do
        Net.close (Net.udp ("h", 0))
      done )

let udp_port_range () =
  Sim.run (fun _ ->
      let einval = Some (Net.Other "EINVAL") in
      let error f =
        match f () with _ -> None | exception Net.Error (e, _) -> Some e
      in
      is_true (error (fun () -> Net.udp ("h", -1)) = einval) ;
      is_true (error (fun () -> Net.udp ("h", 65536)) = einval) ;
      Net.close (Net.udp ("h", 65535)) ;
      Net.close (Net.udp ("h", 1)) )

(* A loop of [readable] and [read_now] on a quiet connection. With
   [extra_edges], an edge with no data comes: [read_now] gives None and clears
   the flag, so the next [readable] waits for a new edge. *)
let read_now_no_spin () =
  for seed = 1 to 50 do
    Sim.run ~seed ~domains:2 (fun sim ->
        Sim.net_faults sim {Sim.no_faults with extra_edges= true} ;
        let a = Net.Tcp ("now", 1) in
        let l = Net.listen a in
        Fiber.scope (fun s ->
            Fiber.fork s (fun () ->
                let c, _ = Net.accept l in
                Time.sleep Mono (Span.ms 100) ;
                Net.write c "late" ;
                Net.close c ) ;
            let c = Net.connect a in
            let buf = Bytes.create 16 and stale = ref 0 in
            let rec loop acc =
              sync (Net.readable c) ;
              match Net.read_now c buf 0 16 with
              | None ->
                  incr stale ;
                  if !stale > 1000 then failwith "the loop spins" ;
                  loop acc
              | Some 0 ->
                  acc
              | Some k ->
                  loop (acc ^ Bytes.sub_string buf 0 k)
            in
            equal string "late" (loop "") ;
            less int ~than:20 !stale ;
            Net.close c ) ;
        Net.close l )
  done

(* The reset comes to the peer after the latency, as the data does. *)
let reset_latency () =
  Sim.run (fun sim ->
      Sim.set_latency sim (Span.ms 5) ;
      let a = Sim.addr sim "server:9" in
      let l = Net.listen a in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              let c, _ = Net.accept l in
              Time.sleep Mono (Span.ms 20) ;
              Net.reset c ) ;
          let c = Net.connect a in
          let t0 = Time.now Mono in
          raises
            (Net.Error (Reset, "Net.read"))
            (fun () -> ignore (Net.read c (Bytes.create 1) 0 1)) ;
          (* The server accepted at 5 ms and reset at 25 ms, and the reset came
             5 ms later. *)
          equal int 25 (ms_since t0) ;
          Net.close c ) ;
      Net.close l ;
      equal int 0 (Sim.open_fds sim) )

(* With [resets], some connections end with ECONNRESET, and the others echo all
   of their data. *)
let random_resets () =
  let resets = ref 0 and complete = ref 0 in
  for seed = 1 to 20 do
    Sim.run ~seed (fun sim ->
        Sim.net_faults sim {Sim.no_faults with resets= true} ;
        let a = Net.Tcp ("resets", 1) in
        let l = Net.listen a in
        Fiber.scope (fun s ->
            Fiber.fork_daemon s (fun () ->
                Fiber.scope (fun conns ->
                    while true do
                      let c, _ = Net.accept l in
                      Fiber.fork conns (fun () ->
                          let buf = Bytes.create 4 in
                          ( try
                              while true do
                                let k = Net.read c buf 0 4 in
                                if k = 0 then raise Exit ;
                                Net.write c (Bytes.sub_string buf 0 k)
                              done
                            with
                            | Exit | Net.Error ((Reset | Broken_pipe), _) ->
                              () ) ;
                          Net.close c )
                    done ) ) ;
            for _ = 1 to 10 do
              let c = Net.connect a in
              ( match
                  for _ = 1 to 20 do
                    Net.write c "ping" ;
                    equal string "ping" (read_n c 4)
                  done
                with
              | () ->
                  incr complete
              | exception Net.Error (Reset, _) ->
                  incr resets ) ;
              Net.close c
            done ) ;
        Net.close l )
  done ;
  (* Each connection has 40 writes: about half of them get a reset. *)
  at_least int ~than:20 !resets ;
  at_least int ~than:20 !complete

let tests =
  [ group "net"
      [ slow "echo: the client gets hello back, with 1000 seeds" echo_1000_seeds
      ; slow "408 after exactly 10 s, with 1000 seeds" t408_1000_seeds
      ; test "a full head gives 200" t200_full_head
      ; test "a connect before the listen gives ECONNREFUSED"
          refused_before_listen
      ; test "a second listen on one address gives EADDRINUSE" listen_twice
      ; test "after the peer closes: the end of the stream, then EPIPE"
          eof_and_epipe
      ; test "shutdown gives the end of the stream to the peer"
          shutdown_gives_eof
      ; test "close gives EBADF to a fiber that waits" close_wakes_waiter
      ; test "a late write after close gives EBADF" late_write_after_close
      ; test "1 MiB goes through the 64 KiB buffers" big_transfer
      ; test "the latency delays the connect and the data" latency_round_trip
      ; test "Sim.addr" addr_parse
      ; test "read and write_sub check the range" bad_ranges
      ; test "a partial read keeps the rest" partial_read
      ; test "a full backlog gives ECONNREFUSED" backlog_limit
      ; test "two clients get different ports" client_ports_differ
      ; test "the bytes in flight count against the buffer"
          inflight_backpressure
      ; test "a Register of a ready fd gives an edge at once" register_ready_fd
      ; test "a listener that closes resets the connections in its backlog"
          reset_by_listener
      ; test "a listener that closes refuses a connect in flight"
          refused_while_connecting
      ; test "a loop of readable and read_now does not spin with extra edges"
          read_now_no_spin
      ; test "a reset comes to the peer after the latency" reset_latency
      ; test "the fault resets ends some connections with ECONNRESET"
          random_resets
      ; slow "10,000 connections use the fd slots again" many_connections ]
  ; group "udp"
      [ test "udp_loss loses about one datagram in 8" udp_loss
      ; test "the jitter can reorder datagrams" udp_order
      ; test "a full queue loses the datagrams that do not fit" udp_full_queue
      ; test "a datagram of more than 65507 bytes gives EMSGSIZE" udp_size
      ; test "the latency delays a datagram" udp_latency
      ; test "the port 0 gives a free port" udp_free_ports
      ; test "the sizes of the buffers follow Linux" option_sizes
      ; test "a UDP port out of range gives EINVAL" udp_port_range
      ; test "the port 0 goes around after 65535" udp_ports_go_around ]
  ; group "sim"
      [ test "settle waits until the others are quiescent" settle_runs_others
      ; test "manual mode: advance_until moves the time" manual_mode
      ; test
          "manual mode: Deadlock tells that only advance_until moves the time"
          manual_mode_deadlock
      ; test "advance_until in auto mode raises Invalid_argument"
          advance_in_auto
      ; test "a read that never ends gives Deadlock" read_deadlock
      ; test "close(2) before Deregister gives Model_error"
          close_before_deregister
      ; test "L5: a cancel ends advance_until" advance_cancelled
      ; test "L6: the connections in flight count against the backlog"
          backlog_counts_in_flight
      ; test "a planted lost wakeup on an fd gives Lost_wakeup"
          planted_fd_lost_wakeup ] ]
