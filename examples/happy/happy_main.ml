(* Connect to a name: dune exec examples/happy/happy_main.exe -- [host] [port].
   For example, start the echo server, then connect to localhost 7000. *)

open Weft

let show = function
  | Net.Tcp (h, p) when String.contains h ':' ->
      Printf.sprintf "[%s]:%d" h p
  | Net.Tcp (h, p) ->
      Printf.sprintf "%s:%d" h p
  | Net.Unix p ->
      p

let () =
  let host = if Array.length Sys.argv > 1 then Sys.argv.(1) else "localhost" in
  let port =
    if Array.length Sys.argv > 2 then int_of_string Sys.argv.(2) else 7000
  in
  Weft.run
    (module Weft_os)
    (fun () ->
      match Happy.resolve host port with
      | exception (Dns.Error _ as e) ->
          prerr_endline (Printexc.to_string e)
      | addrs -> (
          List.iter (fun a -> print_endline ("address: " ^ show a)) addrs ;
          let t0 = Time.now Mono in
          match Happy.connect addrs with
          | c, a ->
              let us =
                Int64.div (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) 1000L
              in
              Printf.printf "connected to %s in %Ld us\n" (show a) us ;
              Net.close c
          | exception (Net.Error _ as e) ->
              prerr_endline (Printexc.to_string e) ) )
