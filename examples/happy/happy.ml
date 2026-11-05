(* A race of connects to the addresses of a name, as in RFC 8305 ("happy
   eyeballs"). The code uses only the API of weft, so it runs in each world. *)

open Weft

(* The addresses of [host]. A DNS server that does not answer gives
   [Fiber.Timeout] after [timeout]. *)
let resolve ?(timeout = Span.s 5) host port =
  Fiber.with_timeout Mono timeout (fun () -> Dns.resolve host)
  |> List.map (fun ip -> Net.Tcp (ip, port))

(* One connect. The promise holds the result, not an exception, thus a failure
   does not stop the race. *)
let attempt s addr =
  Fiber.fork_promise s (fun () ->
      match Net.connect addr with
      | c ->
          Ok (c, addr)
      | exception (Net.Error _ as e) ->
          Error e )

(* Close the socket of an attempt that lost. A cancel can come after the connect
   ended, thus the promise can hold a socket. *)
let discard p =
  match Promise.await p with
  | Ok (c, _) ->
      Net.close c
  | Error _ | (exception Promise.Cancelled) ->
      ()

(* Connect to the first address. If it does not connect in [delay], start the
   next address too. If an attempt fails, start the next address at once. The
   first attempt that connects wins, and the race cancels the others. If all
   attempts fail, raise the last error. *)
let connect ?(delay = Span.ms 250) addrs =
  Fiber.scope (fun s ->
      let rec race pending todo last_error =
        let finished =
          List.map
            (fun p -> wrap (Promise.ready p) (fun r -> `Finished (p, r)))
            pending
        in
        let next =
          if todo = [] then []
          else [wrap (Time.after Mono delay) (fun () -> `Next)]
        in
        if pending = [] && todo = [] then raise last_error ;
        match sync (choose (next @ finished)) with
        | `Finished (p, Ok won) ->
            let others = List.filter (fun q -> q != p) pending in
            List.iter Promise.cancel others ;
            List.iter discard others ;
            won
        | `Finished (p, Error e) ->
            let pending = List.filter (fun q -> q != p) pending in
            if todo = [] then race pending [] e else start pending todo e
        | `Next ->
            start pending todo last_error
      and start pending todo last_error =
        match todo with
        | [] ->
            race pending [] last_error
        | a :: todo ->
            race (pending @ [attempt s a]) todo last_error
      in
      start [] addrs (Net.Error (No_entry, "Happy.connect")) )
