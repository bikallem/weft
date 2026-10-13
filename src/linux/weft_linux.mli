(** The Linux world (design section 2.7): one epoll set for each domain,
    edge-triggered, with a [timerfd] alarm on [CLOCK_MONOTONIC], a second
    [timerfd] on [CLOCK_REALTIME] for the [Posix] deadlines, and an [eventfd]
    for [wake]. *)

(** The state of the world: the poller of each domain. *)
type t

module World : Weft.WORLD with type t = t

val create : ?domains:int -> ?signals:Weft.Signal.t list -> unit -> t
(** [create ~domains ()] makes the poller of each domain, 1 by default.

    [~signals] (none by default) are the signals that [Signal.received] can wait
    for. [create] blocks them in the calling thread, and the threads of the
    world copy this mask. A signalfd in the poller of domain 0 gets them.
    [create] writes a warning to [stderr] for each thread that does not block
    them, because the OS can give a signal to that thread. [close] restores the
    mask. [Signal.unwatch] removes a signal from the signalfd and unblocks it in
    the thread of the domain that calls it. SIGPIPE is ignored, so a write with
    no reader gives EPIPE. *)

val close : t -> unit
(** [close t] stops the thread pools, closes the fds of the world and restores
    the signal mask of [create]. *)

val run :
     ?slack:Weft.Span.t
  -> ?domains:int
  -> ?signals:Weft.Signal.t list
  -> ?watchdog:Weft.Span.t option
  -> (unit -> 'a)
  -> 'a
(** [run main] runs [main] with a new Linux world, and closes the world at the
    end. [~domains] is [min 4 (Domain.recommended_domain_count ())] by default
    (open question 6). [~slack] is 100 us by default (open question 7).

    [~watchdog] is the limit of the watchdog, 10 ms by default (open question
    7): if a fiber runs on a domain for more than the limit with no switch, a C
    thread writes a warning to [stderr] with the index of the domain. [None]
    stops the watchdog. *)

(**/**)

val alarm_sets : unit -> int
(** The calls of [timerfd_settime], for the measurements. *)

val watchdog_reports : unit -> int
(** The number of warnings of all watchdogs, for the tests. *)

val pool_warnings : unit -> int
(** The number of warnings of the thread pools that were full for more than 1 s,
    for the tests. *)

val pool_busy : unit -> int
(** For the tests: the jobs of the thread pools that wait in the queue, plus the
    threads that are not idle. A thread that is not idle runs a job, or it
    starts or ends. At 0, the pools closed each fd of their jobs, also of the
    detached jobs and of the closes of files. *)

val unblocked_threads : Weft.Signal.t list -> string list
(** The threads of the process that do not block each of these signals. For the
    tests. *)
