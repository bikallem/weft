(** The two time lines (design section 2.6). *)

(** A point on the monotonic line: for durations and timeouts. *)
type mono

(** A point on the wall clock: for calendar points. *)
type posix

type 'time line = Mono : mono line | Posix : posix line

(**/**)

(* For the worlds and the scheduler only. *)
val to_ns : 'time line -> 'time -> int64

val of_ns : 'time line -> int64 -> 'time
