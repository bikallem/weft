(** The Linux world (design section 2.7): one epoll set for each domain,
    edge-triggered, with a [timerfd] alarm on [CLOCK_MONOTONIC] and an [eventfd]
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
    mask. SIGPIPE is ignored, so a write with no reader gives EPIPE. *)

val close : t -> unit
(** [close t] closes the fds of the pollers. *)

val run :
     ?slack:Weft.Span.t
  -> ?domains:int
  -> ?signals:Weft.Signal.t list
  -> (unit -> 'a)
  -> 'a
(** [run main] runs [main] with a new Linux world, and closes the world at the
    end. [~domains] is [min 4 (Domain.recommended_domain_count ())] by default
    (open question 6). *)

val local_port : Weft.Net.t -> int
(** The port of a TCP socket, for example after a listen on port 0. *)

(**/**)

val unblocked_threads : Weft.Signal.t list -> string list
(** The threads of the process that do not block each of these signals. For the
    tests. *)
