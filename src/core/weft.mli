(** Weft: fibers, CML events and pluggable worlds. *)

val version : string
(** The version of the library. *)

module Span = Span
module Clock = Clock

(** {1 Events} (design section 2.4) *)

(** An event: a value that a fiber can wait for. *)
type 'a event

val sync : 'a event -> 'a
(** [sync ev] waits until one branch of [ev] commits, then gives its value. It
    is a cancellation point: if the context of the fiber is cancelled, it raises
    [Fiber.Cancelled]. *)

val choose : 'a event list -> 'a event
(** [choose evs] commits to one branch. If more than one branch is ready, a
    random value from the world selects it. *)

val wrap : 'a event -> ('a -> 'b) -> 'b event
(** [wrap ev f] applies [f] to the value, in the fiber that synced. *)

val guard : (unit -> 'a event) -> 'a event
(** [guard g] runs [g] at sync time, in the fiber. *)

val with_nack : (unit event -> 'a event) -> 'a event
(** [with_nack f] gives [f] a nack event. The nack is ready if the sync commits
    to a branch that is not in the event of [f]. *)

val always : 'a -> 'a event
(** [always v] is ready at once. *)

val never : 'a event
(** [never] is never ready. *)

(** {1 Time} (design section 2.6) *)

module Time : sig
  val now : 'time Clock.line -> 'time

  val add : 'time Clock.line -> 'time -> Span.t -> 'time option
  (** [None] on overflow. *)

  val diff : 'time Clock.line -> 'time -> 'time -> Span.t
  (** [diff line a b] is [a - b]. It saturates on overflow. *)

  val at : Clock.mono Clock.line -> Clock.mono -> unit event

  val after : Clock.mono Clock.line -> Span.t -> unit event
  (** If the deadline overflows, the event is [never]. *)

  val sleep : Clock.mono Clock.line -> Span.t -> unit

  val posix_of_unix_s : float -> Clock.posix

  val posix_to_unix_s : Clock.posix -> float
end

(** {1 Fibers} (design section 2.5) *)

module Promise : sig
  type 'a t

  val await : 'a t -> 'a
  (** Gives the value, or raises the exception of the fiber again, with its
      backtrace. *)

  val ready : 'a t -> 'a event
  (** The event form of [await]. *)
end

module Fiber : sig
  type scope

  val scope : (scope -> 'a) -> 'a
  (** [scope f] runs [f] in a new scope, then waits until all fibers of the
      scope end. The wait ignores cancels. If a member failed, it raises the
      first exception, or [Multiple]. *)

  val fork : ?name:string -> scope -> (unit -> unit) -> unit
  (** [fork scope f] runs [f] in a new fiber of [scope]. If [f] raises, the
      scope fails.

      @raise Scope_closed if the scope ended or a context of it is cancelled. *)

  val fork_daemon : ?name:string -> scope -> (unit -> unit) -> unit
  (** The scope cancels a daemon fiber when all other members end. *)

  val fork_promise : ?name:string -> scope -> (unit -> 'a) -> 'a Promise.t
  (** If the fiber raises, the promise holds the exception, and the scope does
      not fail. *)

  val cancel : scope -> exn -> unit
  (** [cancel scope e] cancels the scope and its nested scopes, but not the code
      inside a shield. Each [sync] there raises [Cancelled e]. *)

  val yield : unit -> unit
  (** Puts the fiber at the tail of the queue. It is a cancellation point. *)

  val shield : (unit -> 'a) -> 'a
  (** In [f], a [sync] does not see the cancels of the outer scopes. *)

  val with_timeout : Clock.mono Clock.line -> Span.t -> (unit -> 'a) -> 'a
  (** Runs [f] in a new scope. If [f] does not end in time, the scope is
      cancelled, and the call raises [Timeout]. *)

  exception Cancelled of exn

  exception Multiple of (exn * Printexc.raw_backtrace) list

  exception Timeout

  exception Not_in_fiber of string

  exception Internal_error of exn * Printexc.raw_backtrace

  exception Scope_closed
end

module Ivar : sig
  type 'a t

  val create : unit -> 'a t

  val fill : 'a t -> 'a -> unit
  (** @raise Invalid_argument if the Ivar is full. *)

  val try_fill : 'a t -> 'a -> bool

  val read : 'a t -> 'a event

  val peek : 'a t -> 'a option
end

(** {1 Worlds} (design section 2.3) *)

module type WORLD = World.WORLD

val run :
  ?slack:Span.t -> (module WORLD with type t = 'w) -> 'w -> (unit -> 'a) -> 'a
(** [run (module W) w main] runs [main] as the first fiber, with the world [W].
    When [main] returns, the root scope cancels the other fibers and waits for
    them. *)

(** The parts inside the library. They are for the tests and for the worlds of
    the library, not for programs. They can change in each version. *)
module Private : sig
  val run_world :
       ?slack:Span.t
    -> ?on_create:(Sched.t -> unit)
    -> (module WORLD with type t = 'w)
    -> 'w
    -> (unit -> 'a)
    -> 'a
  (** [run] with a function that gets the scheduler when it starts. The
      simulator uses it. *)

  val scope_ctx : Fiber.scope -> Sched.ctx
  (** The node of a scope, for the tests. *)

  val ivar_state : 'a Ivar.t -> 'a Sched.ivar_state
  (** The state of an Ivar, for the tests. *)

  module Commit = Commit
  module World = World
  module Op = Op
  module Sched = Sched
  module Sched_debug = Sched_debug
  module Event = Event
  module Net = Net
  module Chan = Chan
end

module Chan : sig
  (** A synchronous channel: a [send] and a [recv] commit together. *)
  type 'a t = 'a Private.Chan.t

  val create : unit -> 'a t

  val send : 'a t -> 'a -> unit event

  val recv : 'a t -> 'a event
end

(** {1 Sockets} (design sections 2.3 and 2.7) *)

module Net : sig
  type addr = Private.Net.addr =
    | Tcp of string * int
    | Unix of string
        (** In the simulator, the host of [Tcp] is a name, for example
            ["server"]. *)

  (** A socket. *)
  type t = Private.Net.t

  type error = Private.Op.error =
    | Would_block
    | Bad_fd  (** [EBADF]: the socket is closed. *)
    | Refused
    | Reset
    | Broken_pipe
    | In_use
    | Other of string

  (** The error, and the name of the function. It is [Private.Net.Error]. *)
  exception Error of error * string

  val listen : ?backlog:int -> addr -> t

  val accept : t -> t * addr

  val connect : addr -> t

  val read : t -> bytes -> int -> int -> int
  (** [read t buf off len] reads at least one byte, or gives 0 at the end of the
      stream.

      @raise Invalid_argument if [len < 1] or the range is not in [buf]. *)

  val write : t -> string -> unit
  (** Writes all of the string. *)

  val write_sub : t -> bytes -> int -> int -> int
  (** Writes at least one byte, and gives the number. *)

  val shutdown : t -> unit
  (** Ends the send side. The peer reads the end of the stream. *)

  val close : t -> unit
  (** Closes the socket. A fiber that waits on it, and each later operation,
      gets [Error (Bad_fd, _)]. A second [close] does nothing. [close] never
      blocks, so a cancelled fiber can call it. *)

  val readable : t -> unit event
  (** Ready when the socket can be read. It is also ready after a [close]: the
      next operation then raises [Error (Bad_fd, _)]. *)

  val writable : t -> unit event
  (** As [readable], for a write. *)
end
