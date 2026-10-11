(** The simulator world (design section 2.8). It runs the program on one OS
    thread, with a virtual clock, an in-memory network and a PRNG from the seed.
*)

(** The state of one simulation. *)
type t

val run : ?seed:int -> ?domains:int -> ?auto:bool -> (t -> 'a) -> 'a
(** [run main] runs [main] in the simulator. [~seed] is 0 by default. [~domains]
    is the number of virtual domains, 1 by default: the seed selects the domain
    that continues at each world effect. With [~auto:true] (the default), the
    time moves to the next deadline at each quiescence. With [~auto:false], only
    {!advance_until} moves the time.

    @raise Deadlock
      if the program is quiescent, no deadline exists, and no event can come.
    @raise Lost_wakeup
      if a check at quiescence finds a fiber, a timer or an fd waiter that the
      scheduler did not run.
    @raise Model_error
      if the program breaks the order of the OS, for example a close(2) before a
      Deregister. *)

val settle : t -> unit
(** [settle t] waits until the program is quiescent. *)

val advance_until : t -> Weft.Clock.mono -> unit
(** [advance_until t at] moves the time one deadline at a time up to [at], and
    settles at each one. Only in manual mode.

    @raise Invalid_argument in auto mode, or if an advance is active. *)

val addr : t -> string -> Weft.Net.addr
(** [addr t "server:80"] is the address of a name and a port in the in-memory
    network. [addr t "unix:/run/s"] is a Unix socket. *)

val set_latency : t -> Weft.Span.t -> unit
(** The latency of each packet. It is 0 by default. *)

(** The fibers that wait. *)
exception Deadlock of string list

(** The problems that the check found. *)
exception Lost_wakeup of string list

exception Model_error of string

(**/**)

(* For the tests of the checks. *)
val sched : t -> Weft.Private.Sched.t
