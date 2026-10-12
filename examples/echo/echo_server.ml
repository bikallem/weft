(* The echo server on Linux: dune exec examples/echo/echo_server.exe [port]. For
   example, try it with: echo hello | nc -N 127.0.0.1 7000 *)

open Weft

let () =
  let port =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 7000
  in
  Weft_linux.run (fun () ->
      let l = Net.listen (Net.Tcp ("127.0.0.1", port)) in
      Printf.printf "echo: on 127.0.0.1:%d\n%!" port ;
      Echo.serve l )
