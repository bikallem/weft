(** The kqueue world, for macOS and FreeBSD (design section 2.7): one kqueue for
    each domain, with edge-triggered filters ([EV_CLEAR]), the timeout of
    [kevent] as the alarm, and a pipe for [wake]. *)

(** The state of the world: the poller of each domain. *)
type t

module World : Weft.WORLD with type t = t

val create : ?domains:int -> ?signals:Weft.Signal.t list -> unit -> t
(** [create ~domains ()] makes the poller of each domain, 1 by default.

    [~signals] (none by default) are the signals that [Signal.received] can wait
    for. Domain 0 gets them with [EVFILT_SIGNAL]. Each of them except SIGCHLD
    gets the action "ignore", so the process does not end: [close] restores the
    actions. SIGPIPE is ignored, so a write with no reader gives EPIPE.

    A [Posix] timer can end up to 1 s late after a step of the wall clock: the
    world examines the clock again at least each second. *)

val close : t -> unit
(** [close t] closes the fds of the pollers. *)

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

val local_port : Weft.Net.t -> int
(** The port of a TCP or UDP socket, for example after a listen on port 0. *)

(**/**)

val watchdog_reports : unit -> int
(** The number of warnings of all watchdogs, for the tests. *)

val pool_warnings : unit -> int
(** The number of warnings of the thread pools, for the tests. *)
