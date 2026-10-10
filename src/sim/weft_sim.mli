(** The simulator world (design section 2.8). It runs the program on one OS
    thread, with a virtual clock and a PRNG from the seed. *)

(** The state of one simulation. *)
type t

val run : ?seed:int -> ?auto:bool -> (t -> 'a) -> 'a
(** [run main] runs [main] in the simulator. [~seed] is 0 by default. With
    [~auto:true] (the default), the time moves to the next deadline at each
    quiescence.

    @raise Deadlock
      if the program is quiescent, no deadline exists, and no event can come.
    @raise Lost_wakeup
      if a check at quiescence finds a fiber or a timer that the scheduler did
      not run. *)

val seed : t -> int

(** The fibers that wait. *)
exception Deadlock of string list

(** The problems that the check found. *)
exception Lost_wakeup of string list

(**/**)

(* For the tests of the checks. *)
val sched : t -> Weft.Private.Sched.t
