(* A daily job: dune exec examples/cron/cron_main.exe -- HH:MM. It prints a line
   each day at HH:MM UTC. *)

open Weft

let () =
  let hour, minute =
    if Array.length Sys.argv > 1 then
      Scanf.sscanf Sys.argv.(1) "%d:%d" (fun h m -> (h, m))
    else (0, 0)
  in
  Weft.run
    (module Weft_os)
    (fun () ->
      let now = Time.now Posix in
      Printf.printf "now %s UTC, next run at %s UTC\n%!" (Cron.show now)
        (Cron.show (Cron.next ~hour ~minute now)) ;
      Cron.every_day ~hour ~minute (fun t ->
          Printf.printf "the job of %s UTC runs at %s UTC\n%!" (Cron.show t)
            (Cron.show (Time.now Posix)) ) )
