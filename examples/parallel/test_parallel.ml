(* The examples of Parallel in the simulator, on 4 virtual domains. The sleeps
   take no real time: the virtual clock moves to the next deadline. *)

open Weft
module Sim = Weft_sim

exception Boom

(* The virtual time since [t0], in ms. *)
let ms_since t0 =
  Int64.to_int
    (Int64.div (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) 1_000_000L)

let () =
  for seed = 1 to 20 do
    Sim.run ~seed ~domains:4 (fun _ ->
        (* Each seed gives a different order of the fibers, but the same
           result. *)
        assert (Parallel.primes_below ~tasks:8 10_000 = 1229) ;
        (* A failure: the call for 3 raises after 1 ms. The scope cancels the 7
           other calls, which wait for 10 s. *)
        let cancelled = Atomic.make 0 in
        let t0 = Time.now Mono in
        ( match
            Parallel.map
              (fun i ->
                if i = 3 then (
                  Time.sleep Mono (Span.ms 1) ;
                  raise Boom ) ;
                try Time.sleep Mono (Span.s 10)
                with Fiber.Cancelled _ as e -> Atomic.incr cancelled ; raise e )
              [1; 2; 3; 4; 5; 6; 7; 8]
          with
        | _ ->
            assert false
        | exception Boom ->
            () ) ;
        assert (Atomic.get cancelled = 7) ;
        assert (ms_since t0 = 1) ;
        (* A race: the fast function wins, and the race cancels the slow
           function. *)
        let t0 = Time.now Mono in
        let slow () =
          Time.sleep Mono (Span.s 2) ;
          "slow"
        in
        let fast () =
          Time.sleep Mono (Span.s 1) ;
          "fast"
        in
        assert (Parallel.race [slow; fast] = "fast") ;
        assert (ms_since t0 = 1000) ;
        (* A timeout: the cancel stops the sleep of 10 s at 1 s, and the flush
           of 5 ms runs in its shield. *)
        let flushed = ref false in
        let t0 = Time.now Mono in
        ( match
            Fiber.with_timeout Mono (Span.s 1) (fun () ->
                Parallel.with_flush
                  ~flush:(fun () ->
                    Time.sleep Mono (Span.ms 5) ;
                    flushed := true )
                  (fun () -> Time.sleep Mono (Span.s 10)) )
          with
        | () ->
            assert false
        | exception Fiber.Timeout ->
            () ) ;
        assert !flushed ;
        assert (ms_since t0 = 1005) ;
        (* A timeout around [map]: the scope of [map] ends with no result, and
           [map] raises the cancel, which [with_timeout] gives as [Timeout]. *)
        match
          Fiber.with_timeout Mono (Span.s 1) (fun () ->
              Parallel.map (fun () -> Time.sleep Mono (Span.s 10)) [(); ()] )
        with
        | _ ->
            assert false
        | exception Fiber.Timeout ->
            () )
  done
