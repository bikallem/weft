(* The measurements of open questions 3, 4, 6 and 8 (design section 2.13), on
   the Linux world. Run: dune exec bench/m1.exe *)

open Weft
module S = Private.Sched

let now_s = Unix.gettimeofday

(* The counters of all domains of a run. *)
let stats_of (t : S.t) =
  Array.fold_left
    (fun (w, e, ie, bp) d ->
      let s = d.S.stats in
      (w + s.S.wakes, e + s.S.edges, ie + s.S.idle_edges, bp + s.S.busy_polls) )
    (0, 0, 0, 0) t.S.doms

let run_with_stats ~domains main =
  let sched = ref None in
  let w = Weft_linux.create ~domains () in
  let r =
    Fun.protect
      ~finally:(fun () -> Weft_linux.close w)
      (fun () ->
        Private.run_world ~domains
          ~on_create:(fun s -> sched := Some s)
          (module Weft_linux.World)
          w main )
  in
  (r, stats_of (Option.get !sched))

let percentile p l =
  let a = Array.of_list (List.sort compare l) in
  a.(Int.min (Array.length a - 1) (int_of_float (p *. float (Array.length a))))

(* Q3: a busy domain polls each K switches. One domain runs 200 fibers that
   yield, and a timer fiber sleeps 1 ms 300 times. *)
let q3 () =
  print_endline
    "Q3: the value of K (1 domain, 200 fibers that yield, a 1 ms timer)" ;
  Printf.printf "  %6s %14s %14s %14s %12s\n" "K" "late mean us" "late p99 us"
    "late max us" "yields/s" ;
  List.iter
    (fun k ->
      S.k_poll := k ;
      let (lates, yields, secs), _ =
        run_with_stats ~domains:1 (fun () ->
            let stop = Atomic.make false and yields = Atomic.make 0 in
            let lates = ref [] in
            let t0 = now_s () in
            Fiber.scope (fun s ->
                for _ = 1 to 200 do
                  Fiber.fork s (fun () ->
                      while not (Atomic.get stop) do
                        Atomic.incr yields ; Fiber.yield ()
                      done )
                done ;
                for _ = 1 to 300 do
                  let a = Time.now Mono in
                  Time.sleep Mono (Span.ms 1) ;
                  let late =
                    Int64.sub
                      (Span.to_ns (Time.diff Mono (Time.now Mono) a))
                      1_000_000L
                  in
                  lates := (Int64.to_float late /. 1e3) :: !lates
                done ;
                Atomic.set stop true ) ;
            (!lates, Atomic.get yields, now_s () -. t0) )
      in
      let mean = List.fold_left ( +. ) 0. lates /. float (List.length lates) in
      Printf.printf "  %6d %14.1f %14.1f %14.1f %12.0f\n%!" k mean
        (percentile 0.99 lates)
        (List.fold_left Float.max 0. lates)
        (float yields /. secs) )
    [1; 8; 61; 256; 1024] ;
  S.k_poll := 61

(* Q6, Q4 and Q8 on an echo server: C clients each do R round trips of 64
   bytes. *)
let echo ~domains ~clients ~rounds =
  let (secs, _), stats =
    run_with_stats ~domains (fun () ->
        let l = Net.listen (Net.Tcp ("127.0.0.1", 0)) in
        let a = Net.local_addr l in
        let t0 = now_s () in
        Fiber.scope (fun s ->
            Fiber.fork_daemon s (fun () ->
                Fiber.scope (fun conns ->
                    while true do
                      let c, _ = Net.accept l in
                      Fiber.fork conns (fun () ->
                          let buf = Bytes.create 64 in
                          let rec loop () =
                            match Net.read c buf 0 64 with
                            | 0 ->
                                Net.close c
                            | k ->
                                ignore (Net.write_sub c buf 0 k) ;
                                loop ()
                          in
                          loop () )
                    done ) ) ;
            Fiber.scope (fun cs ->
                for _ = 1 to clients do
                  Fiber.fork cs (fun () ->
                      let c = Net.connect a in
                      let msg = String.make 64 'x' and buf = Bytes.create 64 in
                      for _ = 1 to rounds do
                        Net.write c msg ;
                        let rec got n =
                          if n < 64 then got (n + Net.read c buf n (64 - n))
                        in
                        got 0
                      done ;
                      Net.close c )
                done ) ) ;
        Net.close l ;
        (now_s () -. t0, ()) )
  in
  (float (clients * rounds) /. secs, stats)

let q6_q4_q8 () =
  print_endline
    "Q6, Q4, Q8: echo, 64 clients, 2000 round trips each (64 bytes), on \
     127.0.0.1" ;
  Printf.printf "  %8s %14s %16s %18s\n" "domains" "trips/s" "wakes/trip"
    "idle edges %" ;
  List.iter
    (fun domains ->
      let trips, (wakes, edges, idle, _) =
        echo ~domains ~clients:64 ~rounds:2000
      in
      Printf.printf "  %8d %14.0f %16.3f %18.1f\n%!" domains trips
        (float wakes /. (64. *. 2000.))
        (if edges = 0 then 0. else 100. *. float idle /. float edges) )
    [1; 2; 4; 8]

(* Q8: many short fibers. *)
let q8 () =
  print_endline "Q8: 100,000 short fibers (each one adds to a counter)" ;
  Printf.printf "  %8s %14s %16s\n" "domains" "fibers/s" "wakes/fiber" ;
  List.iter
    (fun domains ->
      let secs, (wakes, _, _, _) =
        run_with_stats ~domains (fun () ->
            let n = Atomic.make 0 in
            let t0 = now_s () in
            Fiber.scope (fun s ->
                for _ = 1 to 100_000 do
                  Fiber.fork s (fun () -> Atomic.incr n)
                done ) ;
            now_s () -. t0 )
      in
      Printf.printf "  %8d %14.0f %16.3f\n%!" domains (100_000. /. secs)
        (float wakes /. 100_000.) )
    [1; 2; 4; 8]

let () =
  Printf.printf "CPUs: %d\n" (Domain.recommended_domain_count ()) ;
  q3 () ;
  q6_q4_q8 () ;
  q8 ()
