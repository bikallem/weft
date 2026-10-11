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

(* The cost of with_timeout around a call that ends at once. *)
let timeouts () =
  Weft_linux.run ~domains:1 (fun () ->
      let n = 200_000 in
      let t0 = now_s () in
      for _ = 1 to n do
        Fiber.with_timeout Mono (Span.s 10) ignore
      done ;
      float n /. (now_s () -. t0) )

(* Q7, the slack: 1,000 fibers sleep 10 times each, a random time of 1 to 20 ms.
   It gives the mean and p99 lateness and the calls of timerfd_settime. *)
let slack_run slack =
  let late = Atomic.make [] in
  let rec add x =
    let l = Atomic.get late in
    if not (Atomic.compare_and_set late l (x :: l)) then add x
  in
  let sets0 = Weft_linux.alarm_sets () in
  Weft_linux.run ~slack ~domains:4 (fun () ->
      Fiber.scope (fun s ->
          for i = 1 to 1000 do
            Fiber.fork s (fun () ->
                for j = 1 to 10 do
                  let ms = 1 + (((i * 7) + (j * 13)) mod 20) in
                  let t0 = Time.now Mono in
                  Time.sleep Mono (Span.ms ms) ;
                  let d = Span.to_ns (Time.diff Mono (Time.now Mono) t0) in
                  add ((Int64.to_float d /. 1e3) -. float (ms * 1000))
                done )
          done ) ) ;
  let l = List.sort compare (Atomic.get late) in
  let a = Array.of_list l in
  let mean = List.fold_left ( +. ) 0. l /. float (Array.length a) in
  (mean, a.(Array.length a * 99 / 100), Weft_linux.alarm_sets () - sets0)

let q7 () =
  print_endline
    "Q7, the slack (4 domains, 1,000 fibers, 10 sleeps of 1 to 20 ms each)" ;
  Printf.printf "  %8s %14s %14s %16s\n" "slack" "late mean us" "late p99 us"
    "timerfd sets" ;
  List.iter
    (fun us ->
      let mean, p99, sets = slack_run (Span.us us) in
      Printf.printf "  %6dus %14.1f %14.1f %16d\n%!" us mean p99 sets )
    [0; 100; 1000; 10_000] ;
  print_endline "Q7, the watchdog (10 ms): the reports on normal loads" ;
  let r0 = Weft_linux.watchdog_reports () in
  ignore (slack_run (Span.ms 1)) ;
  Weft_linux.run ~domains:8 (fun () ->
      Fiber.scope (fun s ->
          for _ = 1 to 100_000 do
            Fiber.fork s ignore
          done ) ) ;
  for _ = 1 to 5 do
    ignore (ping_pong ~domains:4 ~n:100_000)
  done ;
  Printf.printf "  reports: %d\n%!" (Weft_linux.watchdog_reports () - r0)

(* The yield in the I/O modules: a reader of a pipe that is always full never
   gets EAGAIN, so it never switches. A timer fiber on the same domain sleeps 1
   ms 50 times. It gives the mean and the max lateness, and the MiB/s. *)
let busy_reader () =
  Weft_linux.run ~domains:1 (fun () ->
      let p = Process.spawn ~stdout:Piped "cat" ["/dev/zero"] in
      let out = Option.get (Process.stdout p) in
      let stop = Atomic.make false and bytes = ref 0 in
      let late = ref [] in
      let t0 = now_s () in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              let buf = Bytes.create 65536 in
              while (not (Atomic.get stop)) && now_s () -. t0 < 1.0 do
                let n = Pipe.read out buf 0 65536 in
                bytes := !bytes + n ;
                (* The work on each chunk: about 20 us. *)
                let t1 = now_s () in
                while now_s () -. t1 < 20e-6 do
                  ()
                done
              done ) ;
          Fiber.fork s (fun () ->
              for _ = 1 to 50 do
                if now_s () -. t0 > 1.0 then Atomic.set stop true ;
                let t = Time.now Mono in
                Time.sleep Mono (Span.ms 1) ;
                late :=
                  Int64.to_float (Span.to_ns (Time.diff Mono (Time.now Mono) t))
                  /. 1e3
                  -. 1000.
                  :: !late
              done ;
              Atomic.set stop true ) ) ;
      let dt = now_s () -. t0 in
      Process.kill p SIGKILL ;
      ignore (Process.wait p) ;
      Pipe.close out ;
      let l = !late in
      ( List.fold_left ( +. ) 0. l /. float (List.length l)
      , List.fold_left Float.max 0. l
      , float !bytes /. dt /. 1048576. ) )

(* The words that one sync allocates, and the syncs per second: an event that is
   ready, and an Ivar that is full. *)
let sync_cost () =
  Weft_linux.run ~domains:1 (fun () ->
      let iv = Ivar.create () in
      Ivar.fill iv 1 ;
      let measure name ev =
        let n = 200_000 in
        let w0 = Gc.minor_words () and t0 = now_s () in
        for _ = 1 to n do
          ignore (sync ev)
        done ;
        let dt = now_s () -. t0 and w = Gc.minor_words () -. w0 in
        Printf.printf "  %-14s %8.1f words  %10.0f syncs/s\n%!" name
          (w /. float n)
          (float n /. dt)
      in
      measure "always" (always ()) ;
      measure "a full Ivar" (Ivar.read iv) ;
      measure "a choose of 2" (choose [Ivar.read iv; never]) )

let () =
  Printf.printf "CPUs: %d\n" (Domain.recommended_domain_count ()) ;
  print_endline "The cost of one sync" ;
  sync_cost () ;
  print_endline
    "The yield of a read: a reader of a full pipe that works 20 us on each 64 \
     KiB, and a 1 ms timer, on 1 domain" ;
  Printf.printf "  %10s %14s %14s %10s\n" "yield KiB" "late mean us"
    "late max us" "MiB/s" ;
  List.iter
    (fun kib ->
      Private.Net.yield_bytes := kib * 1024 ;
      let mean, max, mibs = busy_reader () in
      Printf.printf "  %10d %14.0f %14.0f %10.0f\n%!" kib mean max mibs )
    [64; 256; 1024; 4096; max_int / 1024] ;
  Private.Net.yield_bytes := 64 * 1024 ;
  q7 () ;
  Printf.printf "with_timeout around a call that ends at once: %.0f/s\n%!"
    (timeouts ()) ;
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
