(* The end of a watch on macOS and FreeBSD (issue 6), as test/linux/unwatch.ml.
   The test runs this program with a mode, and the program must end by SIGINT,
   with the default action.

   "taken": a fiber takes one SIGINT. Then the program unwatches SIGINT and
   sends SIGINT to itself.

   "count": SIGINT comes, domain 0 reads it, and no fiber takes it. The unwatch
   gives it its action.

   "pending": SIGINT comes just before the unwatch. It is usually in the count
   of the handler, because domain 0 runs the fiber and did not take the count.
   The unwatch sends it again.

   "busy": after the unwatch, the program writes "ready", and a fiber computes
   for 5 s with no switch. The test sends SIGINT.

   If the program does not end, it exits with 3 before the close of the
   world. *)

open Weft

let sigint () = Unix.kill (Unix.getpid ()) Sys.sigint

let alive () =
  Time.sleep Mono (Span.s 2) ;
  Unix._exit 3

let () =
  (* A background job of a shell ignores SIGINT. *)
  Sys.set_signal Sys.sigint Sys.Signal_default ;
  let mode = Sys.argv.(1) in
  let domains = if mode = "busy" then 1 else 2 in
  Weft_kqueue.run ~domains ~watchdog:None ~signals:[SIGINT] (fun () ->
      match mode with
      | "taken" ->
          sigint () ;
          ignore (sync (Signal.received SIGINT)) ;
          Signal.unwatch SIGINT ;
          sigint () ;
          alive ()
      | "count" ->
          sigint () ;
          Time.sleep Mono (Span.ms 50) ;
          Signal.unwatch SIGINT ;
          alive ()
      | "pending" ->
          sigint () ; Signal.unwatch SIGINT ; alive ()
      | "busy" ->
          Signal.unwatch SIGINT ;
          print_endline "ready" ;
          let t0 = Unix.gettimeofday () in
          while Unix.gettimeofday () -. t0 < 5. do
            ()
          done ;
          Unix._exit 3
      | _ ->
          failwith "unknown mode" )
