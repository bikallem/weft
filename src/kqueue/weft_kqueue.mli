(** The kqueue world, for macOS and FreeBSD (design section 2.7): one kqueue for
    each domain, with edge-triggered filters ([EV_CLEAR]), the timeout of
    [kevent] as the alarm, and a pipe for [wake]. *)

(** The state of the world: the poller of each domain. *)
type t

module World : Weft.WORLD with type t = t

val create : ?domains:int -> ?signals:Weft.Signal.t list -> unit -> t
(** [create ~domains ()] makes the poller of each domain, 1 by default.

    [~signals] (none by default) are the signals that [Signal.received] can wait
    for. Each of them gets a handler of the world, which counts it and wakes
    domain 0, so the process does not end. [Signal.unwatch] and [close] restore
    the actions. SIGPIPE is ignored, so a write with no reader gives EPIPE.

    A [Posix] timer can end up to 1 s late after a step of the wall clock: the
    world examines the clock again at least each second.

    @raise Invalid_argument
      if [~domains] is not in the range 1 to 62. [create] with [~signals] also
      raises [Invalid_argument] if another world of the process holds signals.
      Only one world at a time can watch signals. *)

val close : t -> unit
(** [close t] stops the thread pools, closes the fds of the world, also the exit
    pipes of the children, and restores the signal actions of [create]. A second
    close does nothing. *)

val run :
     ?slack:Weft.Span.t
  -> ?domains:int
  -> ?signals:Weft.Signal.t list
  -> ?watchdog:Weft.Span.t option
  -> (unit -> 'a)
  -> 'a
(** [run main] runs [main] with a new kqueue world, and closes the world at the
    end. [~domains] is [min 4 (Domain.recommended_domain_count ())] by default
    (open question 6). [~slack] is 100 us by default (open question 7).

    [~watchdog] is the limit of the watchdog, 10 ms by default: if a fiber runs
    on a domain for more than the limit with no switch, a C thread writes a
    warning to [stderr] with the index of the domain. [None] stops the watchdog.
*)

(**/**)

val watchdog_reports : unit -> int
(** The number of warnings of all watchdogs, for the tests. *)

val watchdog_checks : unit -> int
(** The number of checks of all watchdogs, for the tests. *)

val pool_warnings : unit -> int
(** The number of warnings of the thread pools, for the tests. *)

val pool_busy : unit -> int
(** For the tests: the jobs of the thread pools that wait in the queue, plus the
    threads that are not idle. A thread that is not idle runs a job, or it
    starts or ends. At 0, the pools closed each fd of their jobs, also of the
    detached jobs and of the closes of files. *)
