(** The Windows world (design section 2.7, Windows): one I/O completion port
    (IOCP) for each domain, a high-resolution timer as the alarm, the edges of
    the sockets with [AFD_POLL], and pipes with overlapped I/O. *)

(** The state of the world: the port of each domain. *)
type t

module World : Weft.WORLD with type t = t

val create : ?domains:int -> ?signals:Weft.Signal.t list -> unit -> t
(** [create ~domains ()] makes the port of each domain, 1 by default.

    [~signals] (none by default) are the signals that [Signal.received] can wait
    for. Windows has no signals: the events of the console come as [SIGINT]
    (Ctrl-C), [Other_signal 21] (Ctrl-Break, [SIGBREAK] of the C runtime) and
    [SIGHUP] (the close of the console, a logoff or a shutdown). The other
    signals never come. [Signal.unwatch] and [close] give the events their
    default action again.

    A [Posix] timer can end up to 1 s late after a step of the wall clock: the
    world examines the clock again at least each second.

    The fds of the world, for example a pipe, do not go to a later world.

    @raise Invalid_argument
      if [~domains] is not in the range 1 to 62. [create] with [~signals] also
      raises [Invalid_argument] if another world of the process watches signals.
      Only one world at a time can watch signals. *)

val close : t -> unit
(** [close t] stops the thread pools, stops the I/O of the world, and closes the
    ports. A second close does nothing. *)

val run :
     ?slack:Weft.Span.t
  -> ?domains:int
  -> ?signals:Weft.Signal.t list
  -> ?watchdog:Weft.Span.t option
  -> (unit -> 'a)
  -> 'a
(** [run main] runs [main] with a new Windows world, and closes the world at the
    end. [~domains] is [min 4 (Domain.recommended_domain_count ())] by default
    (open question 6). [~slack] is 100 us by default (open question 7).

    [~watchdog] is the limit of the watchdog, 10 ms by default: if a fiber runs
    on a domain for more than the limit with no switch, a C thread writes a
    warning to [stderr] with the index of the domain. [None] stops the watchdog.

    [Process.kill] ends a child with each signal, and the child gets the exit
    code 1: [Process.wait] gives [Exited 1], never [Signaled]. *)

(**/**)

val watchdog_reports : unit -> int
(** The number of warnings of all watchdogs, for the tests. *)

val watchdog_checks : unit -> int
(** The number of checks of all watchdogs, for the tests. *)

val pool_warnings : unit -> int
(** The number of warnings of the thread pools, for the tests. *)

val pool_busy : unit -> int
(** For the tests: the jobs of the thread pools that wait in the queue, plus the
    threads that are not idle. *)
