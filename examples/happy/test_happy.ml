(* The race of connects in the simulator, on 2 virtual domains, for 50 seeds.
   The name table of the simulator gives the addresses of the name "svc". *)

open Weft
module Sim = Weft_sim

let ms_since t0 =
  Int64.to_int
    (Int64.div (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) 1_000_000L)

let check ~seed ~latency ~listen ~winner ~ms =
  Sim.run ~seed ~domains:2 (fun sim ->
      Sim.set_latency sim (Span.ms latency) ;
      Sim.set_dns sim "svc" (Sim.Addresses ["10.0.0.1"; "10.0.0.2"]) ;
      let ls = List.map (fun ip -> Net.listen (Net.Tcp (ip, 80))) listen in
      let t0 = Time.now Mono in
      let addrs = Happy.resolve "svc" 80 in
      ( match Happy.connect addrs with
      | c, a ->
          assert (Some a = Option.map (fun ip -> Net.Tcp (ip, 80)) winner) ;
          (* The socket of the winner is open. *)
          Net.write c "x" ;
          Net.close c
      | exception Net.Error (Refused, _) ->
          assert (winner = None) ) ;
      assert (ms_since t0 = ms) ;
      List.iter Net.close ls )

let () =
  for seed = 1 to 50 do
    (* The two addresses listen. The first connects in 5 ms, before the delay of
       250 ms. *)
    check ~seed ~latency:5 ~listen:["10.0.0.1"; "10.0.0.2"]
      ~winner:(Some "10.0.0.1") ~ms:5 ;
    (* The first address refuses: the second starts at once, not after 250
       ms. *)
    check ~seed ~latency:5 ~listen:["10.0.0.2"] ~winner:(Some "10.0.0.2") ~ms:5 ;
    (* A slow network: the second connect starts at 250 ms, but the first
       connects at 300 ms and wins. The race cancels the second connect. *)
    check ~seed ~latency:300 ~listen:["10.0.0.1"; "10.0.0.2"]
      ~winner:(Some "10.0.0.1") ~ms:300 ;
    (* No address listens: the race raises the last error. *)
    check ~seed ~latency:5 ~listen:[] ~winner:None ~ms:0 ;
    (* A DNS server that does not answer: the resolve stops after 5 s. *)
    Sim.run ~seed (fun sim ->
        Sim.set_dns sim "svc" Sim.No_answer ;
        let t0 = Time.now Mono in
        ( match Happy.resolve "svc" 80 with
        | _ ->
            assert false
        | exception Fiber.Timeout ->
            () ) ;
        assert (ms_since t0 = 5000) )
  done
