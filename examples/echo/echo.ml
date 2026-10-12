(* An echo server and its client. The code uses only the API of weft, so it runs
   in each world. *)

open Weft

(* Send back what the client sends. A client that sends nothing for 10 s loses
   its connection. *)
let handle c =
  let buf = Bytes.create 4096 in
  let rec loop () =
    match
      Fiber.with_timeout Mono (Span.s 10) (fun () -> Net.read c buf 0 4096)
    with
    | 0 | (exception Fiber.Timeout) ->
        ()
    | n ->
        Net.write c (Bytes.sub_string buf 0 n) ;
        loop ()
  in
  Fun.protect ~finally:(fun () -> Net.close c) loop

(* Each connection runs in a fiber of its own. An error in one connection does
   not stop the others. *)
let serve l =
  Fiber.scope (fun s ->
      while true do
        let c, _ = Net.accept l in
        Fiber.fork_isolated s
          ~on_error:(fun e _ -> prerr_endline ("echo: " ^ Printexc.to_string e))
          (fun () -> handle c)
      done )

(* Send [msg], end the send side, and read the answer to its end. *)
let ask addr msg =
  let c = Net.connect addr in
  Fun.protect
    ~finally:(fun () -> Net.close c)
    (fun () ->
      Net.write c msg ;
      Net.shutdown c ;
      let b = Buffer.create 64 and buf = Bytes.create 4096 in
      let rec loop () =
        match Net.read c buf 0 4096 with
        | 0 ->
            Buffer.contents b
        | n ->
            Buffer.add_subbytes b buf 0 n ;
            loop ()
      in
      loop () )
