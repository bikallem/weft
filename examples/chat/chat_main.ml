(* The chat server: dune exec examples/chat/chat_main.exe [port]. Connect two
   clients with nc 127.0.0.1 7001, and send a name, then lines. The first Ctrl-C
   stops the server gracefully. The second Ctrl-C ends it at once. *)

open Weft

let () =
  let port =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 7001
  in
  Weft.run ~signals:[SIGINT]
    (module Weft_os)
    (fun () ->
      let l = Net.listen (Net.Tcp ("127.0.0.1", port)) in
      Printf.printf "chat: on 127.0.0.1:%d\n%!" port ;
      let stop =
        wrap (Signal.received SIGINT) (fun _ ->
            print_endline "chat: stop. Press Ctrl-C again to end at once." ;
            Signal.unwatch SIGINT )
      in
      Chat.serve ~stop l ;
      print_endline "chat: stopped" )
