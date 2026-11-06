(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The tests of weft.flow on the Linux world (design section 2.18), from
   test/linux/test_linux.ml of caravan. They need real sockets: on Linux, a
   shutdown after a reset of the peer gives ENOTCONN. *)

open Windtrap
open Weft
open Weft_flow

(* [in_world ~domains f] runs [f] in a new Linux world with [domains] domains.
   After the failure limit of 10 s, the world cancels [f], and the test fails.
   The limit stops a test that hangs. It is not a wait for an event. *)
let in_world ~domains f =
  Weft_linux.run ~domains (fun () ->
      match Fiber.with_timeout Mono (Span.s 10) f with
      | v ->
          v
      | exception Fiber.Timeout ->
          fail "the test did not end before the failure limit of 10 s" )

(* [with_tcp_pair f] calls [f ~client ~server] with the two sockets of a TCP
   connection on the loopback interface. *)
let with_tcp_pair f =
  let l = Tcp.Listener.listen (Net.Tcp ("127.0.0.1", 0)) in
  Fun.protect ~finally:(fun () -> Tcp.Listener.close l)
  @@ fun () ->
  let client = Net.connect (Tcp.Listener.addr l) in
  Fun.protect ~finally:(fun () -> Net.close client)
  @@ fun () ->
  let server, _ = Tcp.Listener.accept l in
  Fun.protect ~finally:(fun () -> Net.close server)
  @@ fun () -> f ~client ~server

let shutdown_after_reset ~domains () =
  in_world ~domains
  @@ fun () ->
  with_tcp_pair
  @@ fun ~client ~server:sock ->
  let server = Tcp.flow sock in
  Net.reset client ;
  (* The read waits for the reset itself. *)
  raises Flow.Closed (fun () -> Flow.read server (Bytes.create 1) 0 1) ;
  (* The error that Flow.shutdown_send must ignore. *)
  raises_match
    (function Net.Error (Other "ENOTCONN", _) -> true | _ -> false)
    (fun () -> Net.shutdown sock) ;
  Flow.shutdown_send server ;
  Flow.close server

let on ~domains =
  group
    (if domains = 1 then "1 domain" else Printf.sprintf "%d domains" domains)
    [ test "shutdown_send after a peer reset does nothing (ENOTCONN)"
        (shutdown_after_reset ~domains) ]

let tests = group "weft.flow" [on ~domains:1; on ~domains:4]
