(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The harness of the tests of weft.flow on the simulator. It is a part of
   test/sim_support.ml and test/server_support.ml of caravan. No helper waits in
   real time or polls. The time moves only at a quiescence (auto mode) or with
   [advance_to] (manual mode). *)

open Weft
open Weft_flow

(* A check of the harness failed. The text describes the failure. *)
exception Failed of string

let failf fmt = Printf.ksprintf (fun m -> raise (Failed m)) fmt

(* [is_invalid_arg e] tells if [e] is an [Invalid_argument], for
   [Windtrap.raises_match]. *)
let is_invalid_arg = function Invalid_argument _ -> true | _ -> false

(* The text of a span in the largest unit that gives a whole number, for example
   "2 s" or "1 ns". *)
let span_text d =
  let ns = Span.to_ns d in
  let unit_of k = Int64.rem ns k = 0L in
  if ns = 0L then "0 s"
  else if unit_of 1_000_000_000L then
    Printf.sprintf "%Ld s" (Int64.div ns 1_000_000_000L)
  else if unit_of 1_000_000L then
    Printf.sprintf "%Ld ms" (Int64.div ns 1_000_000L)
  else if unit_of 1_000L then Printf.sprintf "%Ld us" (Int64.div ns 1_000L)
  else Printf.sprintf "%Ld ns" ns

(* Exact time: the operations on the mono line *)

(* [t +: d] is the time [d] after [t]. *)
let ( +: ) t d =
  match Time.add Mono t d with
  | Some t' ->
      t'
  | None ->
      failf "%s after the time is out of the range of the mono line"
        (span_text d)

(* [just_before t] is the time 1 ns before [t]. *)
let just_before t = t +: Span.ns (-1)

(* [not_after fn ~limit what] raises [Failed] if the time now is after [limit].
   [what] names [limit] in the text. *)
let not_after fn ~limit what =
  let late = Time.diff Mono (Time.now Mono) limit in
  if Span.compare late Span.zero > 0 then
    failf "%s: the time now is %s after %s" fn (span_text late) what

(* Runs *)

(* [run f] runs [f sim] in auto mode, with seed 0 and 1 virtual domain by
   default. *)
let run ?(seed = 0) ?(domains = 1) f = Weft_sim.run ~seed ~domains f

(* [race f] runs [f] as [run] does, one time for each seed from 1 to [seeds]
   (100), on [domains] (4) virtual domains. The first failure gives the seed N.
   To replay it, run the body with [run ~seed:N ~domains:4]. *)
let race ?(seeds = 100) ?(domains = 4) f =
  for seed = 1 to seeds do
    match run ~seed ~domains f with
    | () ->
        ()
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        failf "seed %d, %d domains: %s\n%s" seed domains (Printexc.to_string e)
          (Printexc.raw_backtrace_to_string bt)
  done

(* [manual f] runs [f sim] in manual mode: only [advance_to] and [advance] move
   the time. *)
let manual ?(seed = 0) ?(domains = 1) f =
  Weft_sim.run ~seed ~domains ~auto:false f

(* Calls: a function that runs in a fiber of its own. [ended] gets a value when
   the function ends. *)
type 'a call = {promise: 'a Promise.t; ended: unit Ivar.t}

let call ?name s f =
  let ended = Ivar.create () in
  let body () = Fun.protect ~finally:(fun () -> Ivar.fill ended ()) f in
  {promise= Fiber.fork_promise ?name s body; ended}

(* [ended sim c] settles, then tells if [c] ended. *)
let ended sim c =
  Weft_sim.settle sim ;
  Option.is_some (Ivar.peek c.ended)

let await c = Promise.await c.promise

let cancel c = Promise.cancel c.promise

(* Exact time: the steps, for manual mode *)

let advance_to sim at =
  not_after "advance_to" ~limit:at "the target" ;
  Weft_sim.advance_until sim at ;
  Weft_sim.settle sim

let advance sim d = advance_to sim (Time.now Mono +: d)

(* [ends_at sim ~t0 d c] makes sure that [c] did not end at the time now and at
   [t0 + d - 1 ns], and that [c] ended at [t0 + d]. Then it gives the result of
   [c]. *)
let ends_at sim ~t0 d c =
  if Span.compare d (Span.ns 1) < 0 then
    failf "ends_at: the span %s is less than 1 ns" (span_text d) ;
  let target = t0 +: d in
  let last = just_before target in
  not_after "ends_at" ~limit:last "t0 + d - 1 ns" ;
  if ended sim c then failf "ends_at: the call ended at the start" ;
  advance_to sim last ;
  if ended sim c then failf "ends_at: the call ended at t0 + d - 1 ns" ;
  advance_to sim target ;
  if not (ended sim c) then failf "ends_at: the call did not end at t0 + d" ;
  await c

(* Connected pairs and fds *)

(* [with_pair sim f] gives [f] the client and the server side of
   [Weft_sim.socket_pair sim], and closes them when [f] ends. *)
let with_pair sim f =
  let client, server = Weft_sim.socket_pair sim in
  Fun.protect
    ~finally:(fun () -> Net.close client ; Net.close server)
    (fun () -> f client server)

(* [no_fd_leak sim f] runs [f], settles, and raises [Failed] if the number of
   open fds changed. *)
let no_fd_leak sim f =
  let before = Weft_sim.open_fds sim in
  let v = f () in
  Weft_sim.settle sim ;
  let after = Weft_sim.open_fds sim in
  if after <> before then
    failf "no_fd_leak: %d fds open before the body, %d after" before after ;
  v

(* The flow helpers *)

(* More than the capacity of a socket of the simulator (64 KiB). Thus a write of
   [big] bytes that the peer does not read waits. *)
let big = 256 * 1024

(* [with_flows sim f] gives [f] the two sides of a TCP connection of the
   simulator as flows of [Tcp.flow]. The two sockets close when [f] ends. *)
let with_flows sim f =
  with_pair sim (fun client server ->
      f ~server:(Tcp.flow server) ~client:(Tcp.flow client) )

(* [read_all ~size flow] reads [flow] to the end, [size] bytes at a time
   (default 65536), and gives the bytes. *)
let read_all ?(size = 65536) flow =
  let buf = Bytes.create size and out = Buffer.create size in
  let rec loop () =
    match Flow.read flow buf 0 size with
    | 0 ->
        Buffer.contents out
    | n ->
        Buffer.add_subbytes out buf 0 n ;
        loop ()
  in
  loop ()

(* [read_exactly flow n] reads [n] bytes of [flow] and gives them. An end of the
   stream before the [n] bytes raises [Failure]. *)
let read_exactly flow n =
  let buf = Bytes.create n in
  let rec loop got =
    if got < n then
      match Flow.read flow buf got (n - got) with
      | 0 ->
          failwith "read_exactly: the end of the stream before the bytes"
      | k ->
          loop (got + k)
  in
  loop 0 ; Bytes.to_string buf

(* [read_one flow] reads at most 1 byte of [flow]. It gives 1, or 0 at the end
   of the stream. *)
let read_one flow = Flow.read flow (Bytes.create 1) 0 1

(* [span] compares two spans in a check, for example an end time. *)
let span = Windtrap.Testable.make ~pp:Span.pp ~equal:Span.equal

(* The flow of Tcp.connect hides its socket. Thus a test asks the world about
   the socket by its fd, with Weft.Private. *)
let submit op =
  match
    Weft.Private.World.perform "test_flow" (Weft.Private.World.Submit op)
  with
  | Ok v ->
      v
  | Error _ ->
      failf "the world gave an error"

(* [client_fd ~peer] is the fd whose local address is [peer]: the socket of the
   client. The test asks the world about each fd from 0 to 1023, so it does not
   depend on the order of the fds. *)
let client_fd ~peer =
  let local fd =
    Weft.Private.World.perform "test_flow"
      (Weft.Private.World.Submit (Weft.Private.Op.Local_addr fd))
  in
  let rec find fd =
    if fd >= 1024 then failf "no fd has the address of the peer"
    else if local fd = Ok peer then fd
    else find (fd + 1)
  in
  find 0

(* [option_of ~peer opt] is the value of [opt] on the client socket whose local
   address is [peer]. *)
let option_of ~peer opt =
  submit (Weft.Private.Op.Get_option (client_fd ~peer, opt))

(* [resumes sim] is the number of fibers that a commit resumed, on all the
   domains of [sim]. *)
let resumes sim =
  Array.fold_left
    (fun n d -> n + d.Weft.Private.Sched.stats.resumes)
    0 (Weft_sim.sched sim).Weft.Private.Sched.doms
