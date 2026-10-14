(* The helpers of the tests on the simulator. *)

open Weft
module Sim = Weft_sim
module S = Private.Sched

(* [run f] runs [f] on the simulator. *)
let run ?seed f = Sim.run ?seed (fun _ -> f ())

(* A log of lines. [add] adds a line from a format, and [get] gives the lines in
   the order of the adds. *)
let log () =
  let l = ref [] in
  let add fmt = Printf.ksprintf (fun s -> l := s :: !l) fmt in
  let get () = List.rev !l in
  (add, get)

(* [seeds n f] runs [f seed] for each seed from 1 to [n]. *)
let seeds n f =
  for seed = 1 to n do
    f seed
  done

(* [some_seed n f] runs [f seed] for each seed from 1 to [n]. It gives [true] if
   one of the runs gives [true]. *)
let some_seed n f = List.exists Fun.id (List.init n (fun s -> f (s + 1)))

(* [spin_until flag] spins until [flag] is set, or for 10,000 switch points. It
   gives [true] if the flag came. Only another domain can set the flag. *)
let spin_until flag =
  let n = ref 0 in
  while (not (Atomic.get flag)) && !n < 10_000 do
    incr n ;
    ignore (Time.now Mono)
  done ;
  Atomic.get flag

(* The sum of a counter of the measurements over the domains of [sim]. *)
let sum_stats sim f =
  Array.fold_left (fun n d -> n + f d.S.stats) 0 (Sim.sched sim).S.doms
