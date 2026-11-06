(* TLS in the middle of a protocol: bytes in clear text, then a handshake on the
   same flows. The MySQL case: the client sends an SSL request packet in clear
   text, then the two sides start TLS. *)

open Windtrap
open Weft
open Weft_flow
open Support

(* [handshakes scfg ccfg ~server ~client] does the two handshakes on the two TCP
   flows, and gives the TLS flows of the server and of the client. *)
let handshakes scfg ccfg ~server ~client =
  Fiber.scope (fun s ->
      let p =
        Fiber.fork_promise s (fun () -> fst (Weft_tls.server scfg server))
      in
      let client, _ = Weft_tls.client ccfg client in
      (Promise.await p, client) )

(* [packet w seq payload] writes a MySQL packet to [w], and flushes [w]. A
   packet has a length of 3 bytes, a sequence id and the payload. *)
let packet w seq payload =
  Writer.write_uint24_le w (String.length payload) ;
  Writer.write_uint8 w seq ;
  Writer.write_string w payload ;
  Writer.flush w

(* [read_packet r] reads a MySQL packet, and gives its sequence id and its
   payload. *)
let read_packet r =
  Reader.ensure r 4 ;
  let len = Reader.get_uint24_le r 0 and seq = Reader.get_uint8 r 3 in
  Reader.ensure r (4 + len) ;
  let payload = Reader.sub_string r 4 len in
  Reader.consume r (4 + len) ;
  (seq, payload)

(* The payload of a greeting of the server and of an SSL request of the client
   (32 bytes: the capability flags with CLIENT_SSL, the size, the character set
   and 23 zero bytes). The content does not matter for TLS. *)
let greeting = "\x0a8.0.0-test\x00\x01\x00\x00\x00abcdefgh\x00"

let ssl_request =
  "\x00\x0a\x00\x00" ^ "\x00\x00\x00\x01" ^ "\x21" ^ String.make 23 '\x00'

let packet_t = Testable.pair int string

let tests =
  group "TLS in the middle of a protocol"
    [ test "clear text, then a handshake on the same flows" (fun () ->
          let scfg, ccfg = setup () in
          run (fun sim ->
              with_flows sim (fun client server ->
                  Flow.write_string client "STARTTLS\r\n" ;
                  equal string "STARTTLS\r\n" (read_n server 10) ;
                  Flow.write_string server "OK\r\n" ;
                  equal string "OK\r\n" (read_n client 4) ;
                  let server, client = handshakes scfg ccfg ~server ~client in
                  Flow.write_string client "over TLS" ;
                  equal string "over TLS" (read_n server 8) ;
                  Flow.write_string server "back" ;
                  equal string "back" (read_n client 4) ;
                  Flow.close client ;
                  Flow.close server ) ) )
    ; test "MySQL: an SSL request, then the handshake" (fun () ->
          let scfg, ccfg = setup () in
          run (fun sim ->
              with_flows sim (fun client server ->
                  let sr = Reader.create server and sw = Writer.create server in
                  let cr = Reader.create client and cw = Writer.create client in
                  packet sw 0 greeting ;
                  equal packet_t (0, greeting) (read_packet cr) ;
                  (* No byte of the server waits in the reader of the client. *)
                  equal int 0 (Reader.buffered cr) ;
                  (* The client sends the SSL request and the ClientHello with
                     no wait, in one TCP write when the sizes permit. Thus the
                     reader of the server can hold the start of the handshake:
                     the server gives these bytes back with Flow.prepend. *)
                  Fiber.scope (fun s ->
                      let c =
                        Fiber.fork_promise s (fun () ->
                            packet cw 1 ssl_request ;
                            fst (Weft_tls.client ccfg client) )
                      in
                      Time.sleep Mono (Span.ms 1) ;
                      equal packet_t (1, ssl_request) (read_packet sr) ;
                      let held = Reader.sub_string sr 0 (Reader.buffered sr) in
                      is_true ~msg:"the ClientHello is in the reader"
                        (String.length held > 0) ;
                      let server, _ =
                        Weft_tls.server scfg (Flow.prepend held server)
                      in
                      let client = Promise.await c in
                      (* The packets continue over TLS, with the next sequence
                         id. *)
                      packet (Writer.create client) 2 "the auth response" ;
                      equal packet_t (2, "the auth response")
                        (read_packet (Reader.create server)) ;
                      Flow.close client ;
                      Flow.close server ) ) ) )
    ; test "MySQL: the reader shows a byte after the greeting" (fun () ->
          (* An attacker adds a byte in clear text after the greeting of the
             server. The client sees it in its reader before the upgrade, and
             must close the connection. *)
          run (fun sim ->
              with_flows sim (fun client server ->
                  let sw = Writer.create server in
                  Writer.write_uint24_le sw (String.length greeting) ;
                  Writer.write_uint8 sw 0 ;
                  Writer.write_string sw greeting ;
                  Writer.write_uint8 sw 0x17 ;
                  Writer.flush sw ;
                  let cr = Reader.create client in
                  equal packet_t (0, greeting) (read_packet cr) ;
                  equal int 1 (Reader.buffered cr) ) ) ) ]
