(** The Linux world (design section 2.7): one epoll set for each domain,
    edge-triggered, with a [timerfd] alarm on [CLOCK_MONOTONIC] and an [eventfd]
    for [wake]. *)

(** The state of the world: the poller of each domain. *)
type t

module World : Weft.WORLD with type t = t

val create : unit -> t
(** [create ()] makes the poller of domain 0. *)

val close : t -> unit
(** [close t] closes the fds of the pollers. *)

val run : ?slack:Weft.Span.t -> (unit -> 'a) -> 'a
(** [run main] runs [main] with a new Linux world, and closes the world at the
    end. *)

val local_port : Weft.Net.t -> int
(** The port of a TCP socket, for example after a listen on port 0. *)
