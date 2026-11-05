(* The disk use of a directory: dune exec examples/du/du_main.exe -- [dir]
   [limit]. *)

open Weft

let () =
  let root = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
  let limit =
    if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 16
  in
  Weft.run
    (module Weft_os)
    (fun () ->
      let t0 = Time.now Mono in
      let s = Du.du ~limit root in
      let ms =
        Int64.div (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) 1_000_000L
      in
      Printf.printf
        "%s: %d bytes in %d files and %d directories, %d errors\n\
         %Ld ms, at most %d operations at the same time\n"
        root s.bytes s.files s.dirs s.errors ms s.peak )
