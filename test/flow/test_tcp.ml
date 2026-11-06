(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The tests of Tcp on the simulator, from test/test_tcp.ml of caravan. Each
   test runs in Helpers: 1 seed and 1 domain, except a test whose name starts
   with "race:" (100 seeds and 4 domains). *)

open Windtrap
open Weft
open Weft_flow
open Helpers

(* [write_big flow] writes [big] bytes to [flow]. It waits when the peer does
   not read. *)
let write_big flow = Flow.write flow (Bytes.make big 'x') 0 big

(* [waiting_read sim s flow] starts a read of 1 byte of [flow] in [s], and makes
   sure that the read waits after a settle. *)
let waiting_read sim s flow =
  let reader = Helpers.call ~name:"reader" s (fun () -> read_one flow) in
  is_false ~msg:"the read waits" (Helpers.ended sim reader) ;
  reader

(* [waiting_write sim s flow] starts a write of [big] bytes to [flow] in [s],
   and makes sure that the write waits after a settle. *)
let waiting_write sim s flow =
  let writer = Helpers.call ~name:"writer" s (fun () -> write_big flow) in
  is_false ~msg:"the write waits" (Helpers.ended sim writer) ;
  writer

(* [byte_before_deadline sim ~client flow] makes a read of [flow] wait until a
   byte of [client] comes 1 ns before the read deadline. Then a second read ends
   at the deadline. *)
let byte_before_deadline sim ~client flow =
  let t0 = Time.now Mono in
  let l = Span.s 11 in
  Flow.set_read_deadline flow (Some (t0 +: l)) ;
  Fiber.scope
  @@ fun s ->
  let first = waiting_read sim s flow in
  advance_to sim (just_before (t0 +: l)) ;
  is_false ~msg:"no end before the byte" (ended sim first) ;
  Flow.write_string client "x" ;
  is_true ~msg:"the byte ends the read" (ended sim first) ;
  equal int 1 (await first) ;
  let second = call s (fun () -> read_one flow) in
  raises Flow.Timeout (fun () -> ends_at sim ~t0 l second)

(* [each_way sim] sends 4 MiB from each side to the other at the same time. *)
let each_way sim =
  with_flows sim
  @@ fun ~server ~client ->
  let data = String.init (4 * 1024 * 1024) (fun i -> Char.chr (i land 0xff)) in
  Fiber.scope
  @@ fun s ->
  let send flow =
    call s (fun () ->
        Flow.write_string flow data ;
        Flow.shutdown_send flow )
  in
  let s1 = send client and s2 = send server in
  let r1 = call s (fun () -> read_all server) in
  let r2 = call s (fun () -> read_all client) in
  await s1 ;
  await s2 ;
  is_true ~msg:"the server got it" (String.equal data (await r1)) ;
  is_true ~msg:"the client got it" (String.equal data (await r2))

let tcp_tests =
  group "Tcp"
    [ test "a request and its echo" (fun () ->
          run
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client ->
          Flow.write_string client "ping" ;
          equal string "ping" (read_exactly server 4) ;
          Flow.write_string server "pong" ;
          equal string "pong" (read_exactly client 4) )
    ; test "read_now does not wait for input" (fun () ->
          run
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client ->
          let buf = Bytes.create 8 in
          let now () = Flow.read_now server buf 0 8 in
          (* An event can come with no input. *)
          let rec next () =
            sync (Flow.readable server) ;
            match now () with None -> next () | r -> r
          in
          let before = now () in
          Flow.write_string client "hi" ;
          let data = next () in
          Flow.shutdown_send client ;
          let last = next () in
          equal (list (option int)) [None; Some 2; Some 0] [before; data; last] )
    ; test "half-close: the peer reads the end and can still answer" (fun () ->
          run
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client ->
          Flow.write_string client "request" ;
          Flow.shutdown_send client ;
          equal string "request" (read_all server) ;
          Flow.write_string server "response" ;
          Flow.shutdown_send server ;
          equal string "response" (read_all client) )
    ; test "a read deadline ends a wait with Timeout" (fun () ->
          manual
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client:_ ->
          let t0 = Time.now Mono in
          Flow.set_read_deadline server (Some (t0 +: Span.s 5)) ;
          Fiber.scope (fun s ->
              let reader = call s (fun () -> read_one server) in
              raises Flow.Timeout (fun () -> ends_at sim ~t0 (Span.s 5) reader) ) ;
          (* A deadline at the time now has passed. *)
          Flow.set_read_deadline server (Some (Time.now Mono)) ;
          raises Flow.Timeout (fun () -> read_one server) )
    ; test "a byte 1 ns before the deadline, then a Timeout at it" (fun () ->
          manual
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client -> byte_before_deadline sim ~client server )
    ; test "without a deadline a read waits for data" (fun () ->
          manual
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client ->
          let t0 = Time.now Mono in
          Flow.set_read_deadline server (Some (t0 +: Span.s 5)) ;
          Flow.set_read_deadline server None ;
          Fiber.scope
          @@ fun s ->
          let reader = call s (fun () -> read_exactly server 4) in
          (* The time passes the removed deadline. *)
          advance_to sim (t0 +: Span.s 10) ;
          is_false ~msg:"no end" (ended sim reader) ;
          Flow.write_string client "late" ;
          equal string "late" (await reader) )
    ; test "a write deadline ends a blocked write with Timeout" (fun () ->
          manual
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client:_ ->
          let t0 = Time.now Mono in
          Flow.set_write_deadline server (Some (t0 +: Span.s 5)) ;
          Fiber.scope
          @@ fun s ->
          (* The client does not read: the write waits with the deadline. *)
          let writer = call s (fun () -> write_big server) in
          raises Flow.Timeout (fun () -> ends_at sim ~t0 (Span.s 5) writer) )
    ; (* A deadline from a different fiber gets to a read or a write that waits
         already. The wait starts with d1. Then the test sets an earlier d2,
         then no deadline, then d3. *)
      test "a new deadline ends a read that waits" (fun () ->
          manual
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client:_ ->
          let t0 = Time.now Mono in
          Flow.set_read_deadline server (Some (t0 +: Span.s 10)) ;
          Fiber.scope
          @@ fun s ->
          let reader = waiting_read sim s server in
          Flow.set_read_deadline server (Some (t0 +: Span.s 5)) ;
          advance_to sim (just_before (t0 +: Span.s 5)) ;
          is_false ~msg:"no end before d2" (ended sim reader) ;
          Flow.set_read_deadline server None ;
          advance_to sim (t0 +: Span.s 11) ;
          is_false ~msg:"no end with no deadline" (ended sim reader) ;
          let t1 = Time.now Mono in
          Flow.set_read_deadline server (Some (t1 +: Span.s 2)) ;
          raises Flow.Timeout (fun () -> ends_at sim ~t0:t1 (Span.s 2) reader) )
    ; test "a new deadline ends a write that waits" (fun () ->
          manual
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client:_ ->
          let t0 = Time.now Mono in
          Flow.set_write_deadline server (Some (t0 +: Span.s 10)) ;
          Fiber.scope
          @@ fun s ->
          let writer = waiting_write sim s server in
          Flow.set_write_deadline server (Some (t0 +: Span.s 5)) ;
          advance_to sim (just_before (t0 +: Span.s 5)) ;
          is_false ~msg:"no end before d2" (ended sim writer) ;
          Flow.set_write_deadline server None ;
          advance_to sim (t0 +: Span.s 11) ;
          is_false ~msg:"no end with no deadline" (ended sim writer) ;
          let t1 = Time.now Mono in
          Flow.set_write_deadline server (Some (t1 +: Span.s 2)) ;
          raises Flow.Timeout (fun () -> ends_at sim ~t0:t1 (Span.s 2) writer) )
    ; test "writing to a closed peer raises Closed" (fun () ->
          run
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client ->
          Flow.close server ;
          Weft_sim.settle sim ;
          raises Flow.Closed (fun () -> Flow.write_string client "x") )
    ; test "a reset connection raises Closed, shutdown_send then does nothing"
        (fun () ->
          run
          @@ fun sim ->
          with_pair sim
          @@ fun client server ->
          let server = Tcp.flow server in
          Net.reset client ;
          (* The reset makes the socket readable. *)
          sync (Flow.readable server) ;
          raises Flow.Closed (fun () -> read_one server) ;
          Flow.shutdown_send server ;
          Flow.close server )
    ; test "reset makes the peer see a reset, not the end of the stream"
        (fun () ->
          run
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client ->
          Flow.write_string server "cut" ;
          Flow.reset server ;
          equal string "cut" (read_exactly client 3) ;
          raises Flow.Closed (fun () -> read_one client) )
    ; test "cancelling a blocked read" (fun () ->
          run
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client:_ ->
          Fiber.scope (fun s ->
              let reader = waiting_read sim s server in
              cancel reader ;
              raises Promise.Cancelled (fun () -> await reader) ) ;
          Flow.close server ;
          raises Flow.Closed (fun () -> read_one server) )
    ; test "a close ends a read that waits with Closed" (fun () ->
          run
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client:_ ->
          Fiber.scope
          @@ fun s ->
          let reader = waiting_read sim s server in
          Flow.close server ;
          raises Flow.Closed (fun () -> await reader) )
    ; (* This test covers the check of a passed deadline in the wait of Tcp. The
         read waits with d1. With no sync between, the test sets a deadline that
         has passed and closes the flow. The set wakes the read first. Then the
         check gives Timeout. Without the check, the choose of the wait has two
         ready branches (the readiness after the close, and the passed
         deadline), and some seeds give Closed. *)
      test "race: a passed deadline wins over a close" (fun () ->
          race
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client:_ ->
          Flow.set_read_deadline server (Some (Time.now Mono +: Span.s 100)) ;
          Fiber.scope
          @@ fun s ->
          let reader = waiting_read sim s server in
          Flow.set_read_deadline server (Some (Time.now Mono)) ;
          Flow.close server ;
          raises Flow.Timeout (fun () -> await reader) )
    ; test "a close ends a write that waits with Closed" (fun () ->
          run
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client:_ ->
          Fiber.scope
          @@ fun s ->
          let writer = waiting_write sim s server in
          Flow.close server ;
          raises Flow.Closed (fun () -> await writer) )
    ; (* Each Var.set wakes the waiters, also for an equal value. The flow sets
         only a new value. Each yield lets the read run if a set woke it. *)
      test "an equal deadline does not wake a read that waits" (fun () ->
          run
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client ->
          let d = Some (Time.now Mono +: Span.s 1) in
          Flow.set_read_deadline server d ;
          Fiber.scope
          @@ fun s ->
          let reader = waiting_read sim s server in
          let before = resumes sim in
          for _ = 1 to 10 do
            Flow.set_read_deadline server d ;
            Fiber.yield ()
          done ;
          equal int ~msg:"the resumes after the sets" 0 (resumes sim - before) ;
          Flow.write_string client "x" ;
          equal int 1 (await reader) )
    ; test "a connect to a closed port raises Refused and leaves no fd"
        (fun () ->
          run
          @@ fun sim ->
          no_fd_leak sim
          @@ fun () ->
          raises_match
            (function Net.Error (Refused, _) -> true | _ -> false)
            (fun () -> Tcp.connect (Weft_sim.addr sim "server:81")) )
    ; test "a cancel of a connect that waits leaves no fd" (fun () ->
          run
          @@ fun sim ->
          Weft_sim.set_latency sim (Span.ms 10) ;
          let l = Tcp.Listener.listen (Weft_sim.addr sim "server:82") in
          no_fd_leak sim (fun () ->
              Fiber.scope (fun s ->
                  let c =
                    call s (fun () -> Tcp.connect (Tcp.Listener.addr l))
                  in
                  is_false ~msg:"the connect waits" (ended sim c) ;
                  cancel c ;
                  raises Promise.Cancelled (fun () -> await c) ) ) ;
          Tcp.Listener.close l ) ]

(* [timed at f] runs [f ()], then puts the time of its end in [at], also when
   [f] raises. *)
let timed at f = Fun.protect ~finally:(fun () -> at := Some (Time.now Mono)) f

(* [ended_after at ~t0] is the time from [t0] to the end of a call, after
   [timed]. *)
let ended_after at ~t0 =
  match !at with
  | Some t ->
      Time.diff Mono t t0
  | None ->
      fail "the call recorded no end"

let cancel_tests =
  group "Tcp cancels, faults and passed deadlines"
    [ test "4 MiB each way at once, with short reads and writes" (fun () ->
          run
          @@ fun sim ->
          Weft_sim.net_faults sim {Weft_sim.no_faults with short_io= true} ;
          each_way sim )
    ; test "a read with data in a cancelled fiber raises Cancelled" (fun () ->
          run
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client ->
          Flow.write_string client "x" ;
          Weft_sim.settle sim ;
          raises (Fiber.Cancelled Exit) (fun () ->
              Fiber.scope (fun s -> Fiber.cancel s Exit ; read_one server) ) ;
          equal (option int) ~msg:"the read took no byte" (Some 1)
            (Flow.read_now server (Bytes.create 1) 0 1) )
    ; test "a write in a cancelled fiber raises Cancelled and sends nothing"
        (fun () ->
          run
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client ->
          raises (Fiber.Cancelled Exit) (fun () ->
              Fiber.scope (fun s ->
                  Fiber.cancel s Exit ;
                  Flow.write_string server "x" ) ) ;
          Weft_sim.settle sim ;
          equal (option int) ~msg:"the client got no byte" None
            (Flow.read_now client (Bytes.create 1) 0 1) )
    ; test "a passed deadline does not stop a read that has data" (fun () ->
          run
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client ->
          Flow.write_string client "x" ;
          Weft_sim.settle sim ;
          Flow.set_read_deadline server (Some (Time.now Mono)) ;
          equal int ~msg:"the byte" 1 (read_one server) ;
          raises Flow.Timeout (fun () -> read_one server) )
    ; (* An edge with no data wakes the wait. The read then reads with no wait
         and waits again, with the same deadline. *)
      test "race: a stale readiness does not end a read before its deadline"
        (fun () ->
          race
          @@ fun sim ->
          Weft_sim.net_faults sim {Weft_sim.no_faults with extra_edges= true} ;
          with_flows sim
          @@ fun ~server ~client:_ ->
          let t0 = Time.now Mono in
          Flow.set_read_deadline server (Some (t0 +: Span.s 5)) ;
          let at = ref None in
          Fiber.scope (fun s ->
              let reader =
                call s (fun () -> timed at (fun () -> read_one server))
              in
              raises Flow.Timeout (fun () -> await reader) ) ;
          equal span ~msg:"the end of the read after t0" (Span.s 5)
            (ended_after at ~t0) ) ]

let other_fiber_tests =
  group "Tcp events from another fiber"
    [ test "race: a deadline set on another domain ends the wait at the deadline"
        (fun () ->
          race
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client:_ ->
          let t0 = Time.now Mono in
          let at = ref None in
          Fiber.scope
          @@ fun s ->
          let reader =
            call ~name:"reader" s (fun () ->
                timed at (fun () -> read_one server) )
          in
          is_false ~msg:"the read waits" (ended sim reader) ;
          let setter =
            call ~name:"setter" s (fun () ->
                Flow.set_read_deadline server (Some (Time.now Mono +: Span.s 1)) )
          in
          await setter ;
          raises Flow.Timeout (fun () -> await reader) ;
          equal span ~msg:"the end of the read after t0" (Span.s 1)
            (ended_after at ~t0) )
    ; test "race: a close on another domain ends a read that waits" (fun () ->
          race
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client:_ ->
          equal int ~msg:"the open fds before" 2 (Weft_sim.open_fds sim) ;
          Fiber.scope (fun s ->
              let reader = waiting_read sim s server in
              let closer =
                call ~name:"closer" s (fun () -> Flow.close server)
              in
              await closer ;
              raises Flow.Closed (fun () -> await reader) ) ;
          Weft_sim.settle sim ;
          equal int ~msg:"the open fds after" 1 (Weft_sim.open_fds sim) )
    ; (* The H1 engine waits for this event, and then reads with no wait. *)
      test "readable is ready after a close from another fiber" (fun () ->
          run
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client:_ ->
          Fiber.scope
          @@ fun s ->
          let waiter =
            call s (fun () ->
                sync (Flow.readable server) ;
                Flow.read_now server (Bytes.create 1) 0 1 )
          in
          is_false ~msg:"the fiber waits for readable" (ended sim waiter) ;
          Flow.close server ;
          raises Flow.Closed (fun () -> await waiter) ) ]

(* [read_ends_at ~deadline ~timeout ~ends] does a read with the read deadline
   [t0 + deadline] and the read timeout [timeout]. The read must end with
   Timeout at [t0 + ends]. *)
let read_ends_at ~deadline ~timeout ~ends =
  manual
  @@ fun sim ->
  with_flows sim
  @@ fun ~server ~client:_ ->
  let t0 = Time.now Mono in
  Flow.set_read_deadline server (Some (t0 +: deadline)) ;
  Flow.set_read_timeout server (Some timeout) ;
  Fiber.scope
  @@ fun s ->
  let reader = call s (fun () -> read_one server) in
  raises Flow.Timeout (fun () -> ends_at sim ~t0 ends reader)

let timeout_tests =
  group "Tcp timeouts"
    [ test
        "a read timeout ends a read that waits, at the timeout after its first \
         wait" (fun () ->
          manual
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client:_ ->
          Flow.set_read_timeout server (Some (Span.s 5)) ;
          (* The timeout starts at the wait, not at the set. *)
          advance sim (Span.s 1) ;
          let t0 = Time.now Mono in
          Fiber.scope
          @@ fun s ->
          let reader = call s (fun () -> read_one server) in
          raises Flow.Timeout (fun () -> ends_at sim ~t0 (Span.s 5) reader) )
    ; (* The byte makes an edge, but the raw read of the test takes the byte
         first. Thus the read wakes, finds no data and waits again. *)
      test "a stale wake keeps the read timeout of the call" (fun () ->
          manual
          @@ fun sim ->
          with_pair sim
          @@ fun client sock ->
          let server = Tcp.flow sock in
          Flow.set_read_timeout server (Some (Span.s 5)) ;
          let t0 = Time.now Mono in
          Fiber.scope
          @@ fun s ->
          let reader = waiting_read sim s server in
          advance_to sim (t0 +: Span.s 3) ;
          ignore (Net.write_now client (Bytes.of_string "x") 0 1) ;
          equal (option int) ~msg:"the raw read takes the byte" (Some 1)
            (Net.read_now sock (Bytes.create 1) 0 1) ;
          raises Flow.Timeout (fun () -> ends_at sim ~t0 (Span.s 5) reader) )
    ; test "a write timeout holds for the whole write call" (fun () ->
          manual
          @@ fun sim ->
          with_flows sim
          @@ fun ~server ~client ->
          let t0 = Time.now Mono in
          Flow.set_write_timeout server (Some (Span.s 5)) ;
          Fiber.scope
          @@ fun s ->
          let writer = waiting_write sim s server in
          advance_to sim (t0 +: Span.s 3) ;
          (* The write continues, then waits again. *)
          ignore (read_exactly client (64 * 1024)) ;
          is_false ~msg:"the write waits again" (ended sim writer) ;
          raises Flow.Timeout (fun () -> ends_at sim ~t0 (Span.s 5) writer) )
    ; test "the earlier of the read deadline and the read timeout ends the wait"
        (fun () ->
          read_ends_at ~deadline:(Span.s 3) ~timeout:(Span.s 10)
            ~ends:(Span.s 3) ;
          read_ends_at ~deadline:(Span.s 10) ~timeout:(Span.s 3)
            ~ends:(Span.s 3) ) ]

(* [is_error e x] is [true] if [x] is [Net.Error (e, _)]. *)
let is_error e = function Net.Error (e', _) -> e' = e | _ -> false

let listener_tests =
  group "Tcp listener"
    [ (* The simulator refuses a connect when the backlog is full. Thus the
         clients that the backlog takes give its length. *)
      test "listen gives ~backlog to the socket" (fun () ->
          run
          @@ fun sim ->
          let l =
            Tcp.Listener.listen ~backlog:2 (Weft_sim.addr sim "server:80")
          in
          let a = Tcp.Listener.addr l in
          let clients = List.init 2 (fun _ -> Tcp.connect a) in
          raises_match (is_error Refused) (fun () -> Tcp.connect a) ;
          List.iter Flow.close clients ;
          Tcp.Listener.close l )
    ; test "addr gives the port of a listen on port 0, also after close"
        (fun () ->
          run
          @@ fun sim ->
          let l = Tcp.Listener.listen (Weft_sim.addr sim "server:0") in
          let a = Tcp.Listener.addr l in
          ( match a with
          | Net.Tcp ("server", p) ->
              is_true ~msg:"a port" (p > 0)
          | _ ->
              fail "not the address of the listener" ) ;
          Fiber.scope (fun s ->
              let c = call s (fun () -> Tcp.connect a) in
              let sock, _ = Tcp.Listener.accept l in
              Net.close sock ;
              Flow.close (await c) ) ;
          Tcp.Listener.close l ;
          is_true ~msg:"the same address after close" (Tcp.Listener.addr l = a) )
    ; test
        "accept on a closed listener raises Bad_fd, also an accept that waits"
        (fun () ->
          run
          @@ fun sim ->
          let l = Tcp.Listener.listen (Weft_sim.addr sim "server:80") in
          Fiber.scope (fun s ->
              let acceptor = call s (fun () -> Tcp.Listener.accept l) in
              is_false ~msg:"the accept waits" (ended sim acceptor) ;
              Tcp.Listener.close l ;
              raises_match (is_error Bad_fd) (fun () -> await acceptor) ) ;
          raises_match (is_error Bad_fd) (fun () -> Tcp.Listener.accept l) ;
          (* A second close does nothing. *)
          Tcp.Listener.close l ) ]

(* [with_connection sim name connect f] listens on [name], connects to it with
   [connect], and calls [f ~accepted ~peer]: the socket of the accept, and the
   address of the client. Then it closes the sockets and the listener. *)
let with_connection sim name connect f =
  let l = Tcp.Listener.listen (Weft_sim.addr sim name) in
  Fun.protect ~finally:(fun () -> Tcp.Listener.close l)
  @@ fun () ->
  Fiber.scope
  @@ fun s ->
  let client = call s (fun () -> connect (Tcp.Listener.addr l)) in
  let accepted, peer = Tcp.Listener.accept l in
  let client = await client in
  Fun.protect
    ~finally:(fun () -> Net.close accepted ; Flow.close client)
    (fun () -> f ~accepted ~peer)

let option_tests =
  group "Tcp options"
    [ test "accept and connect set TCP_NODELAY, and a Unix socket gives no error"
        (fun () ->
          run
          @@ fun sim ->
          with_connection sim "server:80" Tcp.connect (fun ~accepted ~peer ->
              is_true ~msg:"the accepted socket"
                (Net.get_option accepted No_delay) ;
              is_true ~msg:"the connected socket" (option_of ~peer Net.No_delay) ) ;
          (* A Unix socket refuses the option. The accept and the connect give
             no error. *)
          with_connection sim "unix:/run/s" Tcp.connect
            (fun ~accepted:_ ~peer:_ -> () ) ) ]

(* [read_after_fault sim e] gives the result of a read of a Tcp flow whose
   socket got the error [e]. *)
let read_after_fault sim e =
  Helpers.with_pair sim
  @@ fun _client sock ->
  let flow = Tcp.flow sock in
  Weft_sim.fault sim sock e ;
  match read_one flow with
  | n ->
      Printf.sprintf "read %d" n
  | exception Flow.Closed ->
      "Closed"
  | exception Net.Error (Other m, _) ->
      "Net.Error " ^ m
  | exception Net.Error (_, _) ->
      "Net.Error"

let error_tests =
  group "Tcp errors"
    [ test "an error that ends the connection raises Closed, another error not"
        (fun () ->
          run
          @@ fun sim ->
          equal (list string)
            ["Closed"; "Closed"; "Closed"; "Closed"; "Net.Error EIO"]
            (List.map (read_after_fault sim)
               [ Net.Reset
               ; Broken_pipe
               ; Other "ENOTCONN"
               ; Other "ECONNABORTED"
               ; Other "EIO" ] ) )
    ; (* A different fiber can close the socket after the check of Flow. *)
      test "shutdown_send on a closed socket does nothing" (fun () ->
          run
          @@ fun sim ->
          with_pair sim
          @@ fun _client sock ->
          let flow = Tcp.flow sock in
          Net.close sock ; Flow.shutdown_send flow ) ]

let tests =
  [ tcp_tests
  ; cancel_tests
  ; other_fiber_tests
  ; timeout_tests
  ; listener_tests
  ; option_tests
  ; error_tests ]
