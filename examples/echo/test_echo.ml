(* The echo server in the simulator, on 2 virtual domains with a latency of 5
   ms, for 100 seeds. Each seed selects one order of the domains, and the same
   seed always gives the same order. The idle timeout of 10 s takes no real
   time: the virtual clock moves to the next deadline. *)

open Weft
module Sim = Weft_sim

let () =
  for seed = 1 to 100 do
    Sim.run ~seed ~domains:2 (fun sim ->
        Sim.set_latency sim (Span.ms 5) ;
        let a = Sim.addr sim "server:7" in
        let l = Net.listen a in
        Fiber.scope (fun s ->
            Fiber.fork_daemon s (fun () -> Echo.serve l) ;
            assert (Echo.ask a "hello" = "hello") ;
            (* A client that sends nothing: the server closes it after 10 s. *)
            let c = Net.connect a in
            let t0 = Time.now Mono in
            assert (Net.read c (Bytes.create 1) 0 1 = 0) ;
            let waited = Span.to_ns (Time.diff Mono (Time.now Mono) t0) in
            assert (waited >= 10_000_000_000L && waited < 10_100_000_000L) ;
            Net.close c ) )
  done
