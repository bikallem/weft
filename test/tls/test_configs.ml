(* The configurations of a client that Cistern uses for each ssl-mode of MySQL:
   REQUIRED, VERIFY_CA, VERIFY_IDENTITY with a host name or an IP address, and
   ssl-fingerprint. *)

open Windtrap
open Weft
open Weft_flow
open Support

let ok = function Ok v -> v | Error (`Msg m) -> failwith m

let host s = Domain_name.(host_exn (of_string_exn s))

let localhost_ip = Ipaddr.of_string_exn "127.0.0.1"

(* [server_config leaf key] is the configuration of a server with [leaf]. *)
let server_config leaf key =
  ok (Tls.Config.server ~certificates:(`Single ([leaf], key)) ())

(* [client_config ?peer ?ip authenticator] is the configuration of a client with
   the peer name [peer] or the address [ip]. *)
let client_config ?peer ?ip authenticator =
  let c = ok (Tls.Config.client ~authenticator ?peer_name:peer ()) in
  match ip with Some a -> Tls.Config.ip c a | None -> c

(* [handshake_result scfg ccfg] does a handshake of [ccfg] with a server of
   [scfg], and gives the result of the client: "a session" or the text of the
   error. *)
let handshake_result scfg ccfg =
  run (fun sim ->
      with_flows sim (fun client server ->
          Fiber.scope (fun s ->
              let p =
                Fiber.fork_promise s (fun () ->
                    match Weft_tls.server scfg server with
                    | _ ->
                        ()
                    | exception (Weft_tls.Handshake_error _ | Flow.Closed) ->
                        () )
              in
              let r =
                match Weft_tls.client ccfg client with
                | flow, _ ->
                    Flow.close flow ; "a session"
                | exception Weft_tls.Handshake_error m ->
                    Flow.close client ; m
              in
              Promise.await p ; r ) ) )

(* An authenticator that accepts each certificate: ssl-mode=REQUIRED. *)
let accept_all ?ip:_ ~host:_ _ = Ok None

let ca_of ca = X509.Authenticator.chain_of_trust ~time:posix_now [ca]

let session = "a session"

(* [refused ~cause r] checks that [r] is a failed authentication, and that its
   text has [cause]. *)
let refused ~cause r =
  let has s sub =
    let n = String.length sub in
    let rec at i =
      i + n <= String.length s && (String.sub s i n = sub || at (i + 1))
    in
    at 0
  in
  is_true ~msg:r
    (String.starts_with ~prefix:"authentication failure: " r && has r cause)

let tests =
  group "The configurations of Cistern"
    [ test "REQUIRED: any certificate" (fun () ->
          seed () ;
          let _, leaf, key = Tls_support.pki ~names:["db.internal"] () in
          equal string session
            (handshake_result (server_config leaf key)
               (client_config accept_all) ) )
    ; test "VERIFY_CA: the CA, and no name" (fun () ->
          seed () ;
          let ca, leaf, key = Tls_support.pki ~names:["db.internal"] () in
          let other, _, _ = Tls_support.pki () in
          let scfg = server_config leaf key in
          equal string session
            (handshake_result scfg (client_config (ca_of ca))) ;
          refused ~cause:"invalid certificate chain"
            (handshake_result scfg (client_config (ca_of other))) )
    ; test "VERIFY_IDENTITY: a host name" (fun () ->
          seed () ;
          let ca, leaf, key = Tls_support.pki ~names:["db.example"] () in
          let scfg = server_config leaf key in
          equal string session
            (handshake_result scfg
               (client_config ~peer:(host "db.example") (ca_of ca)) ) ;
          refused ~cause:"does not contain the name other.example"
            (handshake_result scfg
               (client_config ~peer:(host "other.example") (ca_of ca)) ) )
    ; test "VERIFY_IDENTITY: an IP address" (fun () ->
          seed () ;
          let ca, leaf, key =
            Tls_support.pki ~names:["db.example"] ~ips:[localhost_ip] ()
          in
          let scfg = server_config leaf key in
          equal string session
            (handshake_result scfg (client_config ~ip:localhost_ip (ca_of ca))) ;
          refused ~cause:"does not contain the IP 10.0.0.1"
            (handshake_result scfg
               (client_config ~ip:(Ipaddr.of_string_exn "10.0.0.1") (ca_of ca)) ) )
    ; test "ssl-fingerprint: the SHA-256 of the certificate" (fun () ->
          seed () ;
          let _, leaf, key = Tls_support.pki () in
          let _, other, _ = Tls_support.pki () in
          let by_fingerprint cert =
            X509.Authenticator.cert_fingerprint ~time:posix_now ~hash:`SHA256
              ~fingerprint:(X509.Certificate.fingerprint `SHA256 cert)
          in
          let scfg = server_config leaf key in
          equal string session
            (handshake_result scfg (client_config (by_fingerprint leaf))) ;
          refused ~cause:"does not match"
            (handshake_result scfg (client_config (by_fingerprint other))) ) ]
