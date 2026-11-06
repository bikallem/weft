(* The handshake of weft-tls: the deadlines of the base flow, a cancel, a peer
   that keeps the socket full, and the check of the random generator. *)

open Windtrap
open Weft
open Weft_flow
open Support

(* [result f] is the text of the result of the handshake [f ()]. *)
let result f =
  match f () with
  | _ ->
      "a session"
  | exception Weft_tls.Handshake_error m ->
      m
  | exception Flow.Timeout ->
      "Flow.Timeout"
  | exception Flow.Closed ->
      "Flow.Closed"
  | exception Fiber.Timeout ->
      "Fiber.Timeout"

(* [handshake ?deadline ~peer scfg] runs [Weft_tls.server] on the server side of
   a connection, with the read deadline [deadline] after the start, while [peer
   tcp] runs on the client side. It gives the result of the server and the time
   that it took, in ms. *)
let handshake ?deadline ~peer scfg =
  run (fun _ ->
      let l = listen () in
      Fiber.scope (fun s ->
          let start = Time.now Mono in
          let server =
            Fiber.fork_promise s (fun () ->
                let c, _ = Net.accept l in
                let tcp = Tcp.flow c in
                Fun.protect ~finally:(fun () -> Flow.close tcp)
                @@ fun () ->
                Option.iter
                  (fun d ->
                    Flow.set_read_deadline tcp (Time.add Mono (Time.now Mono) d) )
                  deadline ;
                let r = result (fun () -> Weft_tls.server scfg tcp) in
                (r, Int64.to_int (Int64.div (elapsed start) 1_000_000L)) )
          in
          let tcp = connect () in
          Fiber.fork s (fun () ->
              match peer tcp with () | (exception Flow.Closed) -> () ) ;
          let r = Promise.await server in
          Flow.close tcp ; Net.close l ; r ) )

let result_and_time = Testable.pair string int

(* ChangeCipherSpec records: ocaml-tls accepts each one in the handshake, thus
   only the limit of 64 KiB ends it. *)
let ccs = String.concat "" (List.init 2000 (fun _ -> "\x14\x03\x03\x00\x01\x01"))

let tests =
  group "The handshake"
    [ test "a silent peer: the read deadline ends the handshake" (fun () ->
          let scfg, _ = setup () in
          equal result_and_time ("Flow.Timeout", 1000)
            (handshake ~deadline:(Span.s 1) scfg ~peer:(fun _ ->
                 Time.sleep Mono (Span.s 5) ) ) )
    ; test "a slow peer: the read deadline is not extended" (fun () ->
          (* One byte of the ClientHello each 100 ms. *)
          let scfg, ccfg = setup () in
          let _, hello = Tls.Engine.client ccfg in
          equal result_and_time ("Flow.Timeout", 1000)
            (handshake ~deadline:(Span.s 1) scfg ~peer:(fun tcp ->
                 String.iter
                   (fun c ->
                     Flow.write_string tcp (String.make 1 c) ;
                     Time.sleep Mono (Span.ms 100) )
                   hello ) ) )
    ; test "a peer that keeps the socket full ends at 64 KiB" (fun () ->
          let scfg, ccfg = setup () in
          let _, hello = Tls.Engine.client ccfg in
          equal result_and_time
            ("the handshake is too large", 0)
            (handshake ~deadline:(Span.s 1) scfg ~peer:(fun tcp ->
                 Flow.write_string tcp hello ;
                 for _ = 1 to 10 do
                   Flow.write_string tcp ccs
                 done ) ) )
    ; test "a cancel ends the handshake" (fun () ->
          let scfg, _ = setup () in
          run (fun sim ->
              with_flows sim (fun _ tcp ->
                  equal string "Fiber.Timeout"
                    (result (fun () ->
                         Fiber.with_timeout Mono (Span.s 1) (fun () ->
                             Weft_tls.server scfg tcp ) ) ) ) ) )
    ; test "server and client refuse to start with no generator" (fun () ->
          let scfg, ccfg = setup () in
          let refused name f =
            match f () with
            | _ ->
                fail "a handshake with no generator"
            | exception Invalid_argument m ->
                equal string
                  ( "Weft_tls." ^ name
                  ^ ": no default generator: call \
                     Mirage_crypto_rng_unix.use_default () first" )
                  m
          in
          let _, hello = Tls.Engine.client ccfg in
          run (fun sim ->
              with_flows sim (fun client server ->
                  Flow.write_string client hello ;
                  Mirage_crypto_rng.unset_default_generator () ;
                  Fun.protect ~finally:seed
                  @@ fun () ->
                  refused "client" (fun () -> Weft_tls.client ccfg client) ;
                  refused "server" (fun () -> Weft_tls.server scfg server) ;
                  (* The server read no byte, and the client wrote none. *)
                  equal string hello (read_n server (String.length hello)) ;
                  equal (option int) None
                    (Flow.read_now server (Bytes.create 1) 0 1) ) ) ) ]
