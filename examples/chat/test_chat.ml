(* The chat server in the simulator, on 2 virtual domains with a latency of 5
   ms, for 30 seeds. *)

open Weft
module Sim = Weft_sim

(* Connect, send the name, and wait for the welcome of the server. *)
let connect a name =
  let c = Net.connect a in
  Net.write c (name ^ "\n") ;
  let lines = Chat.Lines.create c in
  ( match Chat.Lines.read lines with
  | Some w ->
      assert (String.starts_with ~prefix:("welcome " ^ name) w)
  | None ->
      assert false ) ;
  (c, lines)

(* Read lines until [line]. The lines of the joins can come before it. *)
let rec read_until lines line =
  match Chat.Lines.read lines with
  | Some l when l = line ->
      ()
  | Some _ ->
      read_until lines line
  | None ->
      failwith ("no line: " ^ line)

let () =
  for seed = 1 to 30 do
    Sim.run ~seed ~domains:2 ~signals:[SIGINT] (fun sim ->
        Sim.set_latency sim (Span.ms 5) ;
        let a = Sim.addr sim "chat:1" in
        let l = Net.listen a in
        let errors = Atomic.make 0 in
        Fiber.scope (fun s ->
            Fiber.fork ~name:"server" s (fun () ->
                Chat.serve
                  ~stop:(wrap (Signal.received SIGINT) ignore)
                  ~on_error:(fun _ _ -> Atomic.incr errors)
                  l ;
                Net.close l ) ;
            let alice, alice_in = connect a "alice" in
            let bob, bob_in = connect a "bob" in
            let carol, carol_in = connect a "carol" in
            (* A line of alice comes to bob and carol. *)
            Net.write alice "hi\n" ;
            read_until bob_in "alice: hi" ;
            read_until carol_in "alice: hi" ;
            (* A reset of the connection of carol stops only her handler. *)
            Sim.fault sim carol Reset ;
            read_until alice_in "carol left" ;
            Net.close carol ;
            assert (Atomic.get errors = 1) ;
            Net.write bob "still here\n" ;
            read_until alice_in "bob: still here" ;
            (* SIGINT stops the server. Each client gets "bye", then the end of
               the stream. *)
            Sim.signal sim SIGINT ;
            List.iter
              (fun (c, lines) ->
                read_until lines "server: bye" ;
                assert (Chat.Lines.read lines = None) ;
                Net.close c )
              [(alice, alice_in); (bob, bob_in)] ) )
  done
