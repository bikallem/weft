(* The supervisor: dune exec examples/supervisor/supervisor_main.exe -- prog
   [args...]. For example: -- sh -c 'echo up; sleep 1; exit 1'. SIGTERM or
   Ctrl-C stops the child and the supervisor. *)

open Weft

let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "usage: supervisor_main.exe prog [args...]" ;
    exit 2 ) ;
  let prog = Sys.argv.(1) in
  let args = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  Weft.run ~signals:[SIGTERM; SIGINT]
    (module Weft_os)
    (fun () ->
      let t0 = Time.now Mono in
      let log e =
        let ms =
          Int64.div (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) 1_000_000L
        in
        let text =
          match (e : Supervisor.event) with
          | Started pid ->
              Printf.sprintf "started, pid %d" pid
          | Output line ->
              "| " ^ line
          | Exited (Exited n) ->
              Printf.sprintf "exited with the code %d" n
          | Exited (Signaled _) ->
              "ended by a signal"
          | Restart_in b ->
              Printf.sprintf "restart in %Ld ms"
                (Int64.div (Span.to_ns b) 1_000_000L)
          | Stopped ->
              "stopped"
        in
        Printf.printf "%6Ld ms  %s\n%!" ms text
      in
      let stop =
        choose
          [ wrap (Signal.received SIGTERM) ignore
          ; wrap (Signal.received SIGINT) ignore ]
      in
      Supervisor.supervise ~stop ~log prog args )
