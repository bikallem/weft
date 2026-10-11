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

  val cancel : 'a t -> unit
  (** [cancel p] cancels only the fiber of [p]: it runs in a scope of its own. A
      second cancel, or a cancel after the end, does nothing. *)

  (** [await] raises it if the fiber of the promise was cancelled, by [cancel]
      or by a cancel of an outer scope. Thus the caller can tell it from a
      cancel of its own context. *)
  exception Cancelled
end

module Fiber : sig
  type scope

  (** A handler of the user, for example of a user effect (design section 2.5,
      Effect handlers of a scope). It must be safe for parallel use, and its
      [effc] must give [None] for each effect that it does not know. *)
  type handler = {handle: 'a. (unit -> 'a) -> 'a}

  val scope : ?handler:handler -> (scope -> 'a) -> 'a
  (** [scope f] runs [f] in a new scope, then waits until all fibers of the
      scope end. The wait ignores cancels. If a member failed, it raises the
      first exception, or [Multiple].

      With [~handler], [f] and each fiber of the scope and of its nested scopes
      run inside [handler.handle]. A nested scope runs its fibers as
      [outer.handle (fun () -> inner.handle g)]. *)

  val fork : ?name:string -> scope -> (unit -> unit) -> unit
  (** [fork scope f] runs [f] in a new fiber of [scope]. If [f] raises, the
      scope fails.

      @raise Scope_closed if the scope ended or a context of it is cancelled. *)

  val fork_daemon : ?name:string -> scope -> (unit -> unit) -> unit
  (** The scope cancels a daemon fiber when all other members end. *)

  val fork_isolated :
       ?name:string
    -> scope
    -> on_error:(exn -> Printexc.raw_backtrace -> unit)
    -> (unit -> unit)
    -> unit
  (** As [fork], but if [f] raises an exception that is not [Cancelled],
      [on_error] gets it with its backtrace, and the scope does not fail. A
      server forks each connection in this way. If [on_error] raises, the scope
      fails. *)

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

  (** A mutex for fiber code: [lock] suspends the fiber, not the domain (design
      section 2.5, Locks for fiber code). *)
  module Mutex : sig
    type t

    val create : unit -> t

    val use_rw : protect:bool -> t -> (unit -> 'a) -> 'a
    (** [use_rw ~protect m f] locks [m], runs [f] and unlocks [m]. If [f]
        raises, the protected data can be incorrect, so [m] becomes poisoned. A
        [Cancelled] from [Condition.wait] does not poison. With [~protect:true],
        [f] runs in a shield, but [Condition.wait] in [f] sees the cancels from
        outside.

        @raise Poisoned if [m] is poisoned. *)

    val use_ro : t -> (unit -> 'a) -> 'a
    (** As [use_rw ~protect:false], but [m] never becomes poisoned. For code
        that only reads the protected data. *)

    val lock : t -> unit
    (** A cancellation point.

        @raise Poisoned if [m] is poisoned. *)

    val unlock : t -> unit
    (** @raise Invalid_argument if [m] is not locked. *)

    exception Poisoned
  end

  (** A condition variable for fiber code. *)
  module Condition : sig
    type t

    val create : unit -> t

    val wait : t -> Mutex.t -> unit
    (** [wait c m] unlocks [m], waits for a signal, and locks [m] again, also
        when the fiber is cancelled. It is a cancellation point. *)

    val signal : t -> unit
    (** Wakes one waiter, if one. *)

    val broadcast : t -> unit
    (** Wakes all waiters. *)
  end
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
     ?slack:Span.t
  -> ?domains:int
  -> (module WORLD with type t = 'w)
  -> 'w
  -> (unit -> 'a)
  -> 'a
(** [run (module W) w main] runs [main] as the first fiber, with the world [W].
    When [main] returns, the root scope cancels the other fibers and waits for
    them. [~domains] (1 by default) is the number of domains: domain 0 runs on
    the calling thread, and each other domain on an OCaml domain of its own. The
    world must have a poller for each domain.

    @raise Invalid_argument if [domains] is not in the range 1 to 62. *)

(** The parts inside the library. They are for the tests and for the worlds of
    the library, not for programs. They can change in each version. *)
module Private : sig
  val run_world :
       ?slack:Span.t
    -> ?domains:int
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
  module Ring = Ring
  module Job = Job
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
    | No_entry  (** [ENOENT], or a name that DNS does not know. *)
    | Exists  (** [EEXIST]. *)
    | Not_dir  (** [ENOTDIR]. *)
    | Is_dir  (** [EISDIR]. *)
    | Not_empty  (** [ENOTEMPTY]. *)
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

(** {1 Files and directories} (design section 2.3, The I/O modules)

    Each operation runs on the thread pool in production, and the simulator
    has an in-memory file system. Each operation is a cancellation point: on a
    cancel, the fiber does not wait for the operation, and the pool completes
    it alone (design section 2.5, Blocking operations). *)

module File : sig
  (** An open file. *)
  type t

  (** The same exception as [Net.Error]: the error, and the name of the
      function. *)
  exception Error of Net.error * string

  (** [Read] and [Write] together open the file for read and write. *)
  type flag = Read | Write | Create | Exclusive | Truncate | Append

  type kind = Private.Op.kind = Regular | Directory | Symlink | Special

  type stat = {kind: kind; size: int; perm: int; mtime: Clock.posix}

  val openfile : ?perm:int -> flag list -> string -> t
  (** [openfile flags path]. [~perm] is [0o644] by default.

      @raise Error
        with [No_entry], [Exists] (with [Create] and [Exclusive]), [Not_dir] or
        [Is_dir] (with [Write]). *)

  val read : t -> bytes -> int -> int -> int
  (** [read t buf off len] reads at the offset of [t], and moves the offset. It
      gives 0 at the end of the file.

      @raise Invalid_argument if [len < 1] or the range is not in [buf]. *)

  val pread : t -> file_offset:int -> bytes -> int -> int -> int
  (** As [read], at [file_offset]. The offset of [t] does not move. *)

  val write : t -> string -> unit
  (** Writes all of the string at the offset of [t]. *)

  val write_sub : t -> bytes -> int -> int -> int
  (** Writes at least one byte, and gives the number. *)

  val pwrite : t -> file_offset:int -> bytes -> int -> int -> int

  val fsync : t -> unit

  val fstat : t -> stat

  val stat : string -> stat
  (** @raise Error with [No_entry] or [Not_dir]. *)

  val truncate : t -> int -> unit

  val close : t -> unit
  (** As [Net.close]: it never blocks, and the pool does the close(2). *)
end

module Dir : sig
  (** The same exception as [Net.Error]: the error, and the name of the
      function. *)
  exception Error of Net.error * string

  val mkdir : ?perm:int -> string -> unit
  (** @raise Error with [Exists], [No_entry] or [Not_dir]. *)

  val readdir : string -> string list
  (** The names in the directory, sorted, with no ["."] and [".."].

      @raise Error with [No_entry] or [Not_dir]. *)

  val rename : string -> string -> unit
  (** @raise Error
        with [No_entry], [Is_dir] (a file on a directory), [Not_dir] or
        [Not_empty]. *)

  val unlink : string -> unit
  (** @raise Error with [No_entry] or [Is_dir]. *)

  val rmdir : string -> unit
  (** @raise Error with [No_entry], [Not_dir] or [Not_empty]. *)
end

(** {1 Pipes} *)

module Pipe : sig
  (** The same exception as [Net.Error]. *)
  exception Error of Net.error * string

  (** One end of a pipe. *)
  type t

  val create : unit -> t * t
  (** [create ()] gives the read end and the write end. *)

  val read : t -> bytes -> int -> int -> int
  (** As [Net.read]. It gives 0 when the write end is closed and the pipe is
      empty. *)

  val write : t -> string -> unit
  (** Writes all of the string.

      @raise Error with [Broken_pipe] if the read end is closed. *)

  val write_sub : t -> bytes -> int -> int -> int

  val close : t -> unit
  (** As [Net.close]. *)
end

(** {1 Names} *)

module Dns : sig
  (** The same exception as [Net.Error]: the error, and the name of the
      function. *)
  exception Error of Net.error * string

  val resolve : string -> string list
  (** The addresses of a host name, as text, for example ["127.0.0.1"]. It is a
      cancellation point, so [Fiber.with_timeout] around it ends at the
      deadline, also if the DNS server does not answer.

      @raise Error with [No_entry] if the name is not known. *)

  val reverse : string -> string
  (** The name of an address.

      @raise Error with [No_entry] if the address has no name. *)
end
