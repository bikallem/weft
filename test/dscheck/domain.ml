(* Domain, with a cpu_relax that ends a trace after [!limit] spins with no write
   (Atomic resets the count at each write). A spin loop waits for another
   thread, and dscheck can schedule the spinner again and again, so such a trace
   has no end. After [limit] spins in one trace, cpu_relax raises [Pruned], and
   the test drops the trace. The race between the read of the spinner and the
   write of the other thread makes dscheck run the orders in which the other
   thread goes on. *)
include Stdlib.Domain

exception Pruned

let limit = ref 8

let spins = ref 0

let cpu_relax () =
  incr spins ;
  if !spins > !limit then raise Pruned
