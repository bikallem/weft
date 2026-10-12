(** The simulator world (design section 2.8). It runs the program on one OS
    thread, with a virtual clock, an in-memory network and a PRNG from the seed.
*)

(** The state of one simulation. *)
type t

val run :
     ?seed:int
  -> ?domains:int
  -> ?auto:bool
  -> ?signals:Weft.Signal.t list
  -> ?outside_fills:bool
  -> ?livelock:int
  -> (t -> 'a)
  -> 'a
(** [run main] runs [main] in the simulator. [~seed] is 0 by default. [~domains]
    is the number of virtual domains, 1 by default: the seed selects the domain
    that continues at each world effect. With [~auto:true] (the default), the
    time moves to the next deadline at each quiescence. With [~auto:false], only
    {!advance_until} moves the time. [~signals] are the signals that the program
    watches, as for [Weft_linux.run ~signals]: none by default.

    [~outside_fills:true] accepts [Ivar.fill_from_outside]. The simulator gives
    each such fill at the next quiescence. The GC selects when a finaliser runs,
    so a test with such a fill gives the same run only in the same binary with
    the same history. With [~outside_fills:false] (the default), such a fill is
    a [Model_error] at the next quiescence.

    [~livelock] (10{^ 7} by default) is the limit of the switch points with no
    progress: only [Now], [Switch], [Random], or a poll that gives nothing. A
    loop that yields until a time comes never ends in the simulator, because the
    time moves only at a quiescence.

    @raise Livelock
      with the fibers that are not parked. A long stream of data is progress, so
      it is not a livelock.

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

val set_posix : t -> Weft.Clock.posix -> unit
(** [set_posix t p] makes the wall clock jump to [p]. Only [Now Posix] changes.
    A [Posix] timer follows the step: it ends when the new time reaches its
    deadline, as with [TFD_TIMER_CANCEL_ON_SET] on Linux. A [Mono] timer does
    not move. *)

(** The faults of the network. They come from the seed, and they are off by
    default. A TCP stream keeps its order and loses no data, so the loss and the
    reordering of packets show only as delays: [jitter]. Only a reset ([resets])
    ends a stream before its end. A UDP datagram can come in a different order
    ([jitter]) or not at all ([udp_loss]). *)
type net_faults =
  { jitter: Weft.Span.t
        (** Each delivery takes a random delay more, from 0 to [jitter]. The
            data of a stream keeps its order. *)
  ; short_io: bool
        (** A read or a write of a socket or a pipe moves a random part of the
            bytes, at least one. *)
  ; extra_edges: bool
        (** A wait of a domain gets an edge with no change, one time in 8, for
            one of its registrations. *)
  ; udp_loss: bool  (** One UDP datagram in 8 is lost. *)
  ; resets: bool
        (** One write of a TCP socket in 64 resets the connection. The write
            gives [Error (Reset, _)], and the peer gets the reset after the
            delay, as from [Net.reset]. *) }

val no_faults : net_faults
(** All faults off. Make the record with [{Sim.no_faults with ...}], so that a
    new fault does not break the test. *)

val net_faults : t -> net_faults -> unit

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

(** A fake program for [Process.spawn]. *)
type program

val program :
     ?runs:Weft.Span.t
  -> ?code:int
  -> ?output:string
  -> ?ignores:Weft.Signal.t list
  -> unit
  -> program
(** [program ~runs ~code ~output ()] writes [output] on its stdout, and it exits
    with [code] after [runs]. With no [~runs], it runs until a signal. A signal
    that is not in [~ignores] ends it, except [SIGSTOP], [SIGTSTP], [SIGCONT],
    [SIGCHLD], [SIGWINCH] and [Other_signal 0]. [SIGKILL] always ends it. *)

val add_program : t -> string -> (string list -> program) -> unit
(** [add_program t name f]: [Process.spawn name args] starts [f args]. A name
    that is not in the table gives [No_entry]. *)

val signal : t -> Weft.Signal.t -> unit
(** [signal t s] sends [s] to the program. Domain 0 gets it at its next switch
    point.

    @raise Model_error if [run ~signals] does not hold [s]. *)

(** The fibers that wait. *)
exception Deadlock of string list

(** The problems that the check found. *)
exception Lost_wakeup of string list

exception Model_error of string

(** The fibers that are not parked. *)
exception Livelock of string list

(** The exceptions of the promises of the run that failed and that no fiber
    awaited. [run] raises it at its end, after a full GC. *)
exception Unawaited of string list

(**/**)

(* For the tests of the checks. *)
val sched : t -> Weft.Private.Sched.t

(* The number of open fds of the simulator. *)
val open_fds : t -> int
