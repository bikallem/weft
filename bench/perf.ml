(* The measurements of the scheduler, one mode and one domain count for each
   run, on the Linux world. Thus a script can run two builds back to back. Run:
   dune exec bench/perf.exe -- MODE DOMAINS [ARGS]

   yield D N One fiber yields N times. pingpong D N Two fibers send to each
   other N times on two channels. par D F N Each of F fibers yields N times. q8
   D 100,000 short fibers (open question 8). cpus D F R N R rounds: a fiber
   forks F fibers that each compute N steps with no yield, and joins them. It
   gives the time for each round: the spread of work to the idle domains. *)

open Weft
module S = Private.Sched

let now_s = Unix.gettimeofday

let wakes (t : S.t) =
  Array.fold_left (fun w d -> w + d.S.stats.S.wakes) 0 t.S.doms

(* Run [main] on [domains], and give its result and the wakes of the run. *)
let run ~domains main =
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
  (r, wakes (Option.get !sched))

let timed f () =
  let t0 = now_s () in
  f () ;
  now_s () -. t0

let yields ~domains n =
  let secs, w =
    run ~domains
      (timed (fun () ->
           for _ = 1 to n do
             Fiber.yield ()
           done ) )
  in
  Printf.printf "yield d=%d %.2f M yields/s wakes/yield=%.3f\n%!" domains
    (float n /. secs /. 1e6)
    (float w /. float n)

let pingpong ~domains n =
  let secs, w =
    run ~domains
      (timed (fun () ->
           let a = Chan.create () and b = Chan.create () in
           Fiber.scope (fun s ->
               Fiber.fork s (fun () ->
                   for _ = 1 to n do
                     sync (Chan.send b (sync (Chan.recv a)))
                   done ) ;
               for i = 1 to n do
                 sync (Chan.send a i) ;
                 ignore (sync (Chan.recv b))
               done ) ) )
  in
  Printf.printf "pingpong d=%d %.3f M trips/s wakes/trip=%.3f\n%!" domains
    (float n /. secs /. 1e6)
    (float w /. float n)

let par ~domains ~fibers n =
  let secs, w =
    run ~domains
      (timed (fun () ->
           Fiber.scope (fun s ->
               for _ = 1 to fibers do
                 Fiber.fork s (fun () ->
                     for _ = 1 to n do
                       Fiber.yield ()
                     done )
               done ) ) )
  in
  Printf.printf "par d=%d fibers=%d %.2f M yields/s wakes=%d\n%!" domains fibers
    (float (fibers * n) /. secs /. 1e6)
    w

let q8 ~domains =
  let n = 100_000 in
  let secs, w =
    run ~domains
      (timed (fun () ->
           let c = Atomic.make 0 in
           Fiber.scope (fun s ->
               for _ = 1 to n do
                 Fiber.fork s (fun () -> Atomic.incr c)
               done ) ) )
  in
  Printf.printf "q8 d=%d %.0f fibers/s wakes/fiber=%.3f\n%!" domains
    (float n /. secs)
    (float w /. float n)

(* A computation of [n] steps with no effect. *)
let spin n =
  let x = ref 0 in
  for i = 1 to n do
    x := !x + (i land 7)
  done ;
  Sys.opaque_identity !x

let cpus ~domains ~fibers ~rounds n =
  let secs, w =
    run ~domains
      (timed (fun () ->
           for _ = 1 to rounds do
             Fiber.scope (fun s ->
                 for _ = 1 to fibers do
                   Fiber.fork s (fun () -> ignore (spin n))
                 done )
           done ) )
  in
  Printf.printf "cpus d=%d fibers=%d %.2f ms/round wakes=%d\n%!" domains fibers
    (secs /. float rounds *. 1e3)
    w

let () =
  let i = int_of_string in
  match List.tl (Array.to_list Sys.argv) with
  | ["yield"; d; n] ->
      yields ~domains:(i d) (i n)
  | ["pingpong"; d; n] ->
      pingpong ~domains:(i d) (i n)
  | ["par"; d; f; n] ->
      par ~domains:(i d) ~fibers:(i f) (i n)
  | ["q8"; d] ->
      q8 ~domains:(i d)
  | ["cpus"; d; f; r; n] ->
      cpus ~domains:(i d) ~fibers:(i f) ~rounds:(i r) (i n)
  | _ ->
      prerr_endline
        "usage: perf.exe MODE DOMAINS [ARGS] (the comment of perf.ml)" ;
      exit 2
