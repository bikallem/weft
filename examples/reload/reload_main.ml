(* The reload on the OS: dune exec examples/reload/reload_main.exe [file]. The
   file has lines "period_ms=500" and "message=tick". Change the file, then send
   SIGHUP to the process: kill -HUP <pid>. Windows has no SIGHUP, thus there the
   program never reads the file again. *)

open Weft

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "app.conf" in
  Weft.run ~signals:[SIGHUP]
    (module Weft_os)
    (fun () ->
      match Reload.load path with
      | Error e ->
          prerr_endline e
      | Ok c ->
          let config = Var.create c in
          Fiber.scope (fun s ->
              Fiber.fork s (fun () ->
                  Reload.watch path config ~on_error:(fun e ->
                      prerr_endline ("reload: " ^ e ^ ", the old config stays") ) ) ;
              Reload.ticker config ~emit:print_endline ) )
