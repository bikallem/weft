(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Weft

(* [is_closed e] is [true] if [e] tells that the connection or the socket
   ended. *)
let is_closed : Net.error -> bool = function
  | Bad_fd | Reset | Broken_pipe | Other ("ENOTCONN" | "ECONNABORTED") ->
      true
  | _ ->
      false

(* [read_now] and [write_now] are the calls of [Net] with no wait. They raise
   [Flow.Closed] for an error that ends the connection. *)
let read_now s buf off len =
  try Net.read_now s buf off len
  with Net.Error (e, _) when is_closed e -> raise Flow.Closed

let write_now s buf off len =
  try Net.write_now s buf off len
  with Net.Error (e, _) when is_closed e -> raise Flow.Closed

(* [wait ready ~call_deadline seen] waits for [ready], an event that gives
   [`Ready]. [call_deadline] is the deadline of the call, fixed at its first
   wait. [seen] is the view of the deadline of the flow. A passed deadline wins
   over the other events. A new deadline of the flow from a different fiber
   applies also to a wait that started before it. A close from a different fiber
   makes [ready] ready, and the next operation raises Flow.Closed. *)
let rec wait ready ~call_deadline seen =
  let d = Deadline.earlier call_deadline (Var.value seen) in
  if Deadline.has_passed d then raise Flow.Timeout ;
  match
    sync
      (choose
         [ ready
         ; wrap (Var.changed seen) (fun v -> `Changed v)
         ; wrap (Deadline.at d) (fun () -> `Timeout) ] )
  with
  | `Ready ->
      ()
  | `Changed v ->
      wait ready ~call_deadline v
  | `Timeout ->
      raise Flow.Timeout

(* The engines set the same deadline again before many reads. Each Var.set wakes
   the waiters, also for an equal value. A [Clock.mono] is an int64, thus
   structural equality is exact. *)
let set_deadline v d = if Var.get v <> d then Var.set v d

(* [call_deadline timeout] gives the deadline of a call at its first wait: the
   time now plus the timeout of the flow. Only a call that waits reads the
   clock. *)
let call_deadline timeout = Option.bind (Atomic.get timeout) Deadline.from_now

(* A write call is [Unset] until its first wait. Then it is [Fixed] with the
   deadline of the call. *)
type write_call = Unset | Fixed of Clock.mono option

let flow s =
  let read_deadline = Var.create None and write_deadline = Var.create None in
  let read_timeout = Atomic.make None and write_timeout = Atomic.make None in
  (* The events of the waits, made one time for each flow. *)
  let readable = wrap (Net.readable s) (fun () -> `Ready)
  and writable = wrap (Net.writable s) (fun () -> `Ready) in
  (* Each call starts with Fiber.check_cancel, because only a wait does a sync
     (a cancellation point). *)
  let read buf off len =
    Fiber.check_cancel () ;
    match read_now s buf off len with
    | Some n ->
        n
    | None ->
        let d = call_deadline read_timeout in
        let rec again () =
          wait readable ~call_deadline:d (Var.view read_deadline) ;
          match read_now s buf off len with Some n -> n | None -> again ()
        in
        again ()
  in
  let rec write_from call buf off len =
    if len > 0 then
      match write_now s buf off len with
      | Some n ->
          write_from call buf (off + n) (len - n)
      | None ->
          let d =
            match call with
            | Fixed d ->
                d
            | Unset ->
                call_deadline write_timeout
          in
          wait writable ~call_deadline:d (Var.view write_deadline) ;
          write_from (Fixed d) buf off len
  in
  let write buf off len =
    Fiber.check_cancel () ;
    write_from Unset buf off len
  in
  let shutdown_send () =
    try Net.shutdown s with Net.Error (e, _) when is_closed e -> ()
  in
  Flow.make ~read ~read_now:(read_now s) ~write ~readable:(Net.readable s)
    ~shutdown_send
    ~close:(fun () -> Net.close s)
    ~reset:(fun () -> Net.reset s)
    ~set_read_deadline:(set_deadline read_deadline)
    ~set_write_deadline:(set_deadline write_deadline)
    ~set_read_timeout:(Atomic.set read_timeout)
    ~set_write_timeout:(Atomic.set write_timeout) ()

(* weft sets no TCP_NODELAY. A Unix socket refuses the option, and the catch
   ignores that error. The catch also prevents a leak of the socket of [accept]
   and [connect] on an error. A failure on a TCP socket is not important: the
   next read of a broken connection gives the error. *)
let set_no_delay s = try Net.set_option s No_delay true with Net.Error _ -> ()

(* [ended e] is [true] if the error [e] of a set of an option tells that the
   connection ended. Windows gives WSAENOTCONN, thus ENOTCONN. I think that
   macOS gives EINVAL for a set on a connection that ended (design section 2.7,
   Net.reset), but I did not verify it. EINVAL counts on all worlds: with
   constant arguments, no other world gives it. *)
let ended = function Net.Other "EINVAL" -> true | e -> is_closed e

(* SO_KEEPALIVE has an effect only on TCP, and an OS can refuse it on a Unix
   socket. If the connection ended before the set, [connect] gives the flow, as
   for [set_no_delay], and the next operation gives Flow.Closed. Each other
   exception closes the socket, so that no socket stays open. *)
let set_keep_alive s = function
  | Net.Tcp _ -> (
    match Net.set_option s Keep_alive true with
    | () ->
        ()
    | exception Net.Error (e, _) when ended e ->
        ()
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        Net.close s ;
        Printexc.raise_with_backtrace e bt )
  | Net.Unix _ ->
      ()

(* [Net.connect] closes the socket on an error and on a cancel. No sync occurs
   after it: [set_no_delay] and [set_keep_alive] are world operations, not
   cancellation points. *)
let connect ?(keep_alive = false) addr =
  let s = Net.connect addr in
  set_no_delay s ;
  if keep_alive then set_keep_alive s addr ;
  flow s

module Listener = struct
  type t = {sock: Net.t; addr: Net.addr}

  (* No handler closes [sock]: Net.local_addr does not wait, thus a cancel
     cannot stop it. We know of no failure of getsockname(2) on a new
     listener. *)
  let listen ?backlog addr =
    let sock = Net.listen ?backlog addr in
    {sock; addr= Net.local_addr sock}

  let addr l = l.addr

  let accept l =
    let s, peer = Net.accept l.sock in
    set_no_delay s ; (s, peer)

  let close l = Net.close l.sock
end
