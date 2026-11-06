(* The shutdown of a TLS flow in a cancelled fiber (the review of 2026-11-06).
   The write of close_notify is a cancellation point. In a cleanup, a shield
   with a timeout sends it. *)

open Windtrap
open Weft
open Weft_flow
open Support

(* [peer server] is the result of a read of the server: "end" for the end of the
   stream (close_notify), or "truncation" for Flow.Closed. *)
let peer server =
  let buf = Bytes.create 16 in
  match Flow.read server buf 0 16 with
  | 0 ->
      "end"
  | n ->
      Printf.sprintf "%d bytes" n
  | exception Flow.Closed ->
      "truncation"

(* [cleanup shutdown] runs a fiber that sleeps, cancels it, and calls
   [shutdown] in the cleanup of the fiber. It gives the text of the result of
   the shutdown. *)
let cleanup shutdown =
  let r = ref "no cleanup" in
  ( try
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              Fun.protect
                ~finally:(fun () ->
                  match shutdown () with
                  | () ->
                      r := "ok"
                  | exception Fiber.Cancelled _ ->
                      r := "Cancelled" )
                (fun () -> Time.sleep Mono (Span.s 10)) ) ;
          Time.sleep Mono (Span.ms 1) ;
          Fiber.cancel s Exit )
    with Fiber.Cancelled _ -> () ) ;
  !r

let tests =
  group "The shutdown"
    [ test "in a cancelled fiber: Cancelled, and a truncation for the peer"
        (fun () ->
          pair (fun server client _ ->
              equal Testable.string "Cancelled"
                (cleanup (fun () -> Flow.shutdown_send client)) ;
              equal Testable.string "truncation" (peer server) ) )
    ; test "in a shield with a timeout: the peer reads the end" (fun () ->
          pair (fun server client _ ->
              equal Testable.string "ok"
                (cleanup (fun () ->
                     Fiber.shield (fun () ->
                         Fiber.with_timeout Mono (Span.s 1) (fun () ->
                             Flow.shutdown_send client ) ) ) ) ;
              equal Testable.string "end" (peer server) ) ) ]
