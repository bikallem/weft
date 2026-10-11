(* The measurements of open questions 5 and 7 (design section 2.13), on the
   Linux world. Run: dune exec bench/m3.exe *)

open Weft
module S = Private.Sched

let now_s = Unix.gettimeofday

(* Q5: two fibers send to each other N times through two channels, and a third
   fiber yields at the same time. It gives the round trips per second, and the
   yields of the third fiber for each round trip: with no limit, the next slot
   can starve the local queue. *)
let ping_pong ~domains ~n =
  Weft_linux.run ~domains (fun () ->
      let a = Chan.create () and b = Chan.create () in
      let stop = Atomic.make false and yields = Atomic.make 0 in
      let t0 = now_s () in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              while not (Atomic.get stop) do
                Atomic.incr yields ; Fiber.yield ()
              done ) ;
          Fiber.fork s (fun () ->
              for i = 1 to n do
                sync (Chan.send a i) ;
                ignore (sync (Chan.recv b))
              done ;
              Atomic.set stop true ) ;
          Fiber.fork s (fun () ->
              for _ = 1 to n do
                let x = sync (Chan.recv a) in
                sync (Chan.send b x)
              done ) ) ;
      let dt = now_s () -. t0 in
      (float n /. dt, float (Atomic.get yields) /. float n) )

let q5 () =
  print_endline
    "Q5: the limit of the next slot (ping-pong of 200,000 round trips, and a \
     fiber that yields)" ;
  Printf.printf "  %6s %8s %14s %18s\n" "limit" "domains" "trips/s"
    "yields/trip" ;
  List.iter
    (fun limit ->
      S.next_limit := limit ;
      List.iter
        (fun domains ->
          let trips, yields = ping_pong ~domains ~n:200_000 in
          Printf.printf "  %6d %8d %14.0f %18.3f\n%!" limit domains trips yields )
        [1; 4] )
    [0; 1; 3; 16; 1_000_000] ;
  S.next_limit := 3

(* The steal of the next slot: F1 wakes F2 into its next slot and then computes
   for 50 ms with no switch. With no steal of the next slot, F2 waits for the
   end of the computation, also with idle domains. *)
let next_latency ~domains =
  Weft_linux.run ~domains (fun () ->
      let c = Chan.create () in
      Fiber.scope (fun s ->
          let got = Fiber.fork_promise s (fun () -> sync (Chan.recv c)) in
          Fiber.yield () ;
          Fiber.fork s (fun () ->
              let t0 = now_s () in
              sync (Chan.send c t0) ;
              while now_s () -. t0 < 0.05 do
                ()
              done ) ;
          let t0 = Promise.await got in
          (now_s () -. t0) *. 1000. ) )

let () =
  Printf.printf "CPUs: %d\n" (Domain.recommended_domain_count ()) ;
  q5 () ;
  print_endline "Q5 with no wake at a push to the next slot" ;
  S.wake_on_next := false ;
  q5 () ;
  S.wake_on_next := true ;
  print_endline
    "The steal of the next slot: F2 waits for F1 that computes 50 ms" ;
  List.iter
    (fun domains ->
      Printf.printf "  %d domains: F2 runs after %.1f ms\n%!" domains
        (next_latency ~domains) )
    [1; 4]
