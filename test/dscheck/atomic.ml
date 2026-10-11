(* The traced atomics of dscheck, in place of Stdlib.Atomic. A write is
   progress: it resets the count of spins of Domain.cpu_relax. *)
include Dscheck.TracedAtomic

let progress () = Domain.spins := 0

let set r v = set r v ; progress ()

let exchange r v =
  let x = exchange r v in
  progress () ; x

let compare_and_set r seen v =
  let ok = compare_and_set r seen v in
  if ok then progress () ;
  ok

let fetch_and_add r n =
  let x = fetch_and_add r n in
  progress () ; x

let incr r = ignore (fetch_and_add r 1)

let decr r = ignore (fetch_and_add r (-1))
