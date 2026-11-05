(* The pool in the simulator, on 2 virtual domains, for 50 seeds. *)

open Weft
module Sim = Weft_sim

let ms_since t0 =
  Int64.to_int
    (Int64.div (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) 1_000_000L)

let () =
  for seed = 1 to 50 do
    Sim.run ~seed ~domains:2 (fun _ ->
        (* The jobs count the jobs that run. A job that a nack stopped fills
           [stopped]. *)
        let running = Atomic.make 0
        and peak = Atomic.make 0
        and stopped = Ivar.create () in
        let job d =
          let n = Atomic.fetch_and_add running 1 + 1 in
          let rec raise_peak () =
            let p = Atomic.get peak in
            if n > p && not (Atomic.compare_and_set peak p n) then raise_peak ()
          in
          raise_peak () ;
          Fun.protect
            ~finally:(fun () -> Atomic.decr running)
            (fun () ->
              try
                Time.sleep Mono (Span.ms d) ;
                d
              with Fiber.Cancelled _ as e -> Ivar.fill stopped d ; raise e )
        in
        Fiber.scope (fun s ->
            let pool = Pool.create s ~workers:3 job in
            (* A call that ends in time gives its value. *)
            assert (Pool.call pool 10 = 10) ;
            (* A timeout: the nack makes the worker stop the job at once, not
               after 1000 ms. *)
            let t0 = Time.now Mono in
            assert (Pool.call_timeout pool (Span.ms 50) 1000 = None) ;
            assert (sync (Ivar.read stopped) = 1000) ;
            assert (ms_since t0 = 50) ;
            (* 20 calls at the same time: the pool runs 3 jobs at most. *)
            Fiber.scope (fun calls ->
                for _ = 1 to 20 do
                  Fiber.fork calls (fun () -> assert (Pool.call pool 10 = 10))
                done ) ;
            assert (Atomic.get peak = 3) ;
            assert (Atomic.get running = 0) ;
            (* The rate limit: 50 tokens at 10 each second take 5 s. *)
            let rate = Pool.Rate.create s ~per_s:10 ~burst:1 in
            let t0 = Time.now Mono in
            for _ = 1 to 50 do
              Pool.Rate.take rate
            done ;
            assert (ms_since t0 = 5000) ) )
  done
