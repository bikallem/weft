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

val set_blocking_cost : t -> Weft.Span.t -> unit
(** [set_blocking_cost t limit]: each [Blocking] operation (a [File], [Dir] or
    [Dns] operation) takes a time cost from the seed, in the range 0 to [limit].
    The operation occurs at the end of its cost. With a limit of 0 (the
    default), it occurs at once, and its fiber continues at a later switch point
    (open question 2). *)

(** The answer of the name table for a host name. A name that is not in the
    table gives [No_entry]. *)
type dns_answer =
  | Addresses of string list
  | No_answer  (** The DNS server never answers. *)

val set_dns : t -> string -> dns_answer -> unit
(** [set_dns t name answer] adds [name] to the name table of [Dns].
    [Dns.reverse a] gives the first name, in the sort order, whose addresses
    hold [a]. *)

(** The fibers that wait. *)
exception Deadlock of string list

(** The problems that the check found. *)
exception Lost_wakeup of string list

exception Model_error of string

(**/**)

(* For the tests of the checks. *)
val sched : t -> Weft.Private.Sched.t

(* The number of open fds of the simulator. *)
val open_fds : t -> int
