(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* The harness of the tests of weft-tls, in the simulator. It came from
   test/test_tls.ml of caravan. Each test sets a new Fortuna generator with a
   fixed seed first, thus a run is the same for a seed. The simulator runs on
   one OS thread, thus Fortuna is correct here. *)

open Windtrap
open Weft
open Weft_flow

let seed () =
  Mirage_crypto_rng.set_default_generator
    (Mirage_crypto_rng.create ~seed:"weft tls tests"
       (module Mirage_crypto_rng.Fortuna) )

(* The time of the simulator in the tests: 2025-06-15. The certificates are
   valid from 2020 to 2040. *)
let t0 = 1_750_000_000.

let posix_now () = Ptime.of_float_s (Time.posix_to_unix_s (Time.now Posix))

(* [setup ?peer_name ()] seeds the generator and gives a server and a client
   configuration. *)
let setup ?peer_name () =
  seed () ;
  Tls_support.configs ?peer_name ~time:posix_now ()

let raises_closed ?msg f =
  raises_match ?msg (function Flow.Closed -> true | _ -> false) f

let raises_timeout ?msg f =
  raises_match ?msg (function Flow.Timeout -> true | _ -> false) f

(* The longest run of a test, in virtual time. A test that waits longer is
   failed: the limit makes the run raise in place of a wait with no end. *)
let run_limit = Span.s 30

(* [run ?seed ?domains f] runs [f sim] in the simulator at the time [t0], with
   the limit. *)
let run ?(seed = 0) ?(domains = 1) f =
  Weft_sim.run ~seed ~domains (fun sim ->
      Weft_sim.set_posix sim (Time.posix_of_unix_s t0) ;
      Fiber.with_timeout Mono run_limit (fun () -> f sim) )

(* {1 A raw peer of ocaml-tls} *)

(* [raw_handshake ?with_finished tcp ccfg] does the handshake of a client with
   Tls.Engine on [tcp], and gives the state. [with_finished tls] gives the state
   and the records to send in the same TCP write as the Finished message. *)
let raw_handshake ?with_finished tcp ccfg =
  let buf = Bytes.create 65536 in
  let tls, hello = Tls.Engine.client ccfg in
  Flow.write_string tcp hello ;
  let rec go tls =
    if not (Tls.Engine.handshake_in_progress tls) then tls
    else
      let n = Flow.read tcp buf 0 65536 in
      match Tls.Engine.handle_tls tls (Bytes.sub_string buf 0 n) with
      | Ok (tls, _, `Response r, _) -> (
        match (r, with_finished) with
        | Some fin, Some f when not (Tls.Engine.handshake_in_progress tls) ->
            let tls, more = f tls in
            Flow.write_string tcp (fin ^ more) ;
            tls
        | _ ->
            Option.iter (Flow.write_string tcp) r ;
            go tls )
      | Error (f, _) ->
          fail (Tls.Engine.string_of_failure f)
  in
  go tls

(* [raw_read ?on_data tcp tls] reads [tcp] with Tls.Engine to the end, and gives
   the plaintext. [on_data tls n] runs after each data, with the count of the
   bytes so far, and gives the next state. *)
let raw_read ?(on_data = fun tls _ -> tls) tcp tls =
  let buf = Bytes.create 65536 and out = Buffer.create 4096 in
  let rec go tls =
    match Flow.read tcp buf 0 65536 with
    | 0 ->
        ()
    | n -> (
      match Tls.Engine.handle_tls tls (Bytes.sub_string buf 0 n) with
      | Ok (tls, eof, `Response r, `Data d) ->
          Option.iter (Flow.write_string tcp) r ;
          Option.iter (Buffer.add_string out) d ;
          let tls = on_data tls (Buffer.length out) in
          if eof = None then go tls
      | Error (f, _) ->
          fail (Tls.Engine.string_of_failure f) )
  in
  go tls ; Buffer.contents out

(* [encrypt tls data] is the state after [data] and the records of [data]. *)
let encrypt tls data = Option.get (Tls.Engine.send_application_data tls [data])

let send tcp tls data =
  let tls, out = encrypt tls data in
  Flow.write_string tcp out ; tls

(* {1 Flows and connections} *)

(* [fake_tcp ?read ?readable ?write tcp] is [tcp] with a [read], a [readable]
   and a [write] of the test. *)
let fake_tcp ?read ?readable ?write tcp =
  Flow.make
    ~read:(Option.value read ~default:(Flow.read tcp))
    ~read_now:(Flow.read_now tcp)
    ~write:(Option.value write ~default:(Flow.write tcp))
    ~readable:(Option.value readable ~default:(Flow.readable tcp))
    ~shutdown_send:(fun () -> Flow.shutdown_send tcp)
    ~close:(fun () -> Flow.close tcp)
    ~set_read_deadline:(Flow.set_read_deadline tcp)
    ~set_write_deadline:(Flow.set_write_deadline tcp)
    ()

let listen () = Net.listen (Net.Tcp ("server", 443))

let connect () = Tcp.connect (Net.Tcp ("server", 443))

(* [accept_one s scfg l] runs [Weft_tls.server] on the next connection of [l],
   and gives the text of its error, or ["closed"] for a reset of the peer. *)
let accept_one ?(pause = Span.zero) s scfg l =
  Fiber.fork_promise s (fun () ->
      let c, _ = Net.accept l in
      let tcp = Tcp.flow c in
      Time.sleep Mono pause ;
      Fun.protect ~finally:(fun () -> Flow.close tcp)
      @@ fun () ->
      match Weft_tls.server scfg tcp with
      | _ ->
          "a session"
      | exception Weft_tls.Handshake_error m ->
          m
      | exception Flow.Closed ->
          "closed" )

(* [with_raw ?seed ?domains f] runs [f server tcp tls]: [server] is the TLS flow
   of the server side of a connection, and [tcp] is the TCP flow of the client
   side, after a handshake of a raw client of ocaml-tls with the state [tls]. *)
let with_raw ?seed ?domains f =
  let scfg, ccfg = setup () in
  run ?seed ?domains (fun _ ->
      let l = listen () in
      Fiber.scope (fun s ->
          let server =
            Fiber.fork_promise s (fun () ->
                let c, _ = Net.accept l in
                fst (Weft_tls.server scfg (Tcp.flow c)) )
          in
          let tcp = connect () in
          let tls = raw_handshake tcp ccfg in
          let server = Promise.await server in
          Fun.protect
            ~finally:(fun () -> Flow.close tcp ; Flow.close server ; Net.close l)
            (fun () -> f server tcp tls) ) )

(* [pair ?domains ?wrap ?swrap f] runs [f server client client_tcp] with the TLS
   flows of one connection in the simulator. [wrap] and [swrap] change the TCP
   flows of the client and of the server. *)
let pair ?seed ?domains ?(wrap = Fun.id) ?(swrap = Fun.id) f =
  let scfg, ccfg = setup () in
  run ?seed ?domains (fun _ ->
      let l = listen () in
      Fiber.scope (fun s ->
          let server =
            Fiber.fork_promise s (fun () ->
                let c, _ = Net.accept l in
                fst (Weft_tls.server scfg (swrap (Tcp.flow c))) )
          in
          let tcp = wrap (connect ()) in
          let client, _ = Weft_tls.client ccfg tcp in
          let server = Promise.await server in
          Fun.protect
            ~finally:(fun () ->
              Flow.close client ; Flow.close server ; Net.close l )
            (fun () -> f server client tcp) ) )

let read_string flow n =
  let buf = Bytes.create n in
  Bytes.sub_string buf 0 (Flow.read flow buf 0 n)

(* An application data record with a bad MAC. *)
let bad_record = "\x17\x03\x03\x00\x20" ^ String.make 32 'x'

let ready flow =
  sync
    (choose
       [ wrap (Flow.readable flow) (fun () -> true)
       ; wrap (Time.after Mono (Span.s 1)) (fun () -> false) ] )

(* [read_n flow n] reads [n] bytes of [flow]. *)
let read_n flow size =
  let b = Buffer.create size and buf = Bytes.create 65536 in
  while Buffer.length b < size do
    let n = Flow.read flow buf 0 65536 in
    if n = 0 then fail "an early end" ;
    Buffer.add_subbytes b buf 0 n
  done ;
  Buffer.contents b

(* [read_all flow] reads [flow] to its end. [Flow.Closed] is also the end: for a
   TCP flow that the peer resets. *)
let read_all flow =
  let b = Buffer.create 4096 and buf = Bytes.create 4096 in
  let rec go () =
    match Flow.read flow buf 0 4096 with
    | 0 ->
        Buffer.contents b
    | n ->
        Buffer.add_subbytes b buf 0 n ;
        go ()
    | exception Flow.Closed ->
        Buffer.contents b
  in
  go ()

let elapsed start = Span.to_ns (Time.diff Mono (Time.now Mono) start)

(* [with_flows sim f] gives [f] the TCP flows of the client and of the server
   side of [Weft_sim.socket_pair sim], and closes them when [f] ends. *)
let with_flows sim f =
  let a, b = Weft_sim.socket_pair sim in
  let client = Tcp.flow a and server = Tcp.flow b in
  Fun.protect
    ~finally:(fun () -> Flow.close client ; Flow.close server)
    (fun () -> f client server)
