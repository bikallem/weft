(** Weft: fibers, CML events and pluggable worlds. *)

val version : string
(** The version of the library. *)

module Span = Span
module Clock = Clock

(** {1 Events}

    Design section 2.4. *)

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

(** {1 Time}

    Design section 2.6. *)

module Time : sig
  val now : 'time Clock.line -> 'time

  val add : 'time Clock.line -> 'time -> Span.t -> 'time option
  (** [None] on overflow. *)

  val diff : 'time Clock.line -> 'time -> 'time -> Span.t
  (** [diff line a b] is [a - b]. It saturates on overflow. *)

  val at : 'time Clock.line -> 'time -> unit event
  (** [at line t] is ready when the time of [line] is [t] or later. A [Posix]
      deadline follows a step of the wall clock: the wait ends when the clock
      reaches [t], also after a step. *)

  val after : 'time Clock.line -> Span.t -> unit event
  (** If the deadline overflows, the event is [never]. *)

  val sleep : 'time Clock.line -> Span.t -> unit

  val posix_of_unix_s : float -> Clock.posix

  val posix_to_unix_s : Clock.posix -> float

  (** A date and a time of day in UTC (design section 2.6). *)
  type utc =
    { year: int
    ; month: int  (** 1 to 12. *)
    ; day: int  (** 1 to 31. *)
    ; hour: int  (** 0 to 23. *)
    ; minute: int  (** 0 to 59. *)
    ; second: int  (** 0 to 59. *)
    ; weekday: int  (** 0 is Sunday, 6 is Saturday. *) }

  val to_utc : Clock.posix -> utc
  (** The date of a time in the proleptic Gregorian calendar. A time before 1970
      gives a date before 1970. [to_utc] removes the fraction of a second: the
      result is the second at or before the time. POSIX time has no leap second,
      so [second] is never 60, and a day always has 86400 seconds. The result
      covers the full range of [Clock.posix], the years 1677 to 2262. The
      smallest [Clock.posix] is 1677-09-21 00:12:43.145224192, so for the first
      0.85 s of the range [to_utc] gives 00:12:43. *)

  val of_utc : utc -> Clock.posix option
  (** The inverse of [to_utc]. It ignores [weekday]. It gives [None] if a field
      is out of range (for example, a second of 60), or if the time is outside
      the range of [Clock.posix]. The first whole second of the range is
      00:12:44, so [of_utc] gives [None] for 1677-09-21 00:12:43. *)
end

(** {1 Fibers}

    Design section 2.5. *)

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

  val fork : ?name:string -> ?pinned:bool -> scope -> (unit -> unit) -> unit
  (** [fork scope f] runs [f] in a new fiber of [scope]. If [f] raises, the
      scope fails. With [~pinned:true], the fiber runs only on domain 0, the
      domain of the thread that called [run]: for example for a C library that
      is not thread-safe.

      @raise Scope_closed if the scope ended or a context of it is cancelled. *)

  val fork_daemon : ?name:string -> scope -> (unit -> unit) -> unit
  (** The scope cancels a daemon fiber when all other members end. *)

  val fork_isolated :
       ?name:string
    -> ?pinned:bool
    -> scope
    -> on_error:(exn -> Printexc.raw_backtrace -> unit)
    -> (unit -> unit)
    -> unit
  (** As [fork], but if [f] raises an exception that is not [Cancelled],
      [on_error] gets it with its backtrace, and the scope does not fail. A
      server forks each connection in this way. If [on_error] raises, the scope
      fails. *)

  val fork_promise :
    ?name:string -> ?pinned:bool -> scope -> (unit -> 'a) -> 'a Promise.t
  (** If the fiber raises, the promise holds the exception, and the scope does
      not fail. *)

  val cancel : scope -> exn -> unit
  (** [cancel scope e] cancels the scope and its nested scopes, but not the code
      inside a shield. Each [sync] there raises [Cancelled e]. *)

  val yield : unit -> unit
  (** Puts the fiber at the tail of the queue. It is a cancellation point. *)

  (** The id of a fiber (design section 2.5, The id of a fiber). Two fibers of
      one run have different ids. A fiber keeps its id when it moves to a
      different domain. *)
  module Id : sig
    type t

    val equal : t -> t -> bool

    val compare : t -> t -> int
    (** In one run, the order is the order of the forks. Thus [Map.Make] and
        [Set.Make] take [Id]. *)

    val hash : t -> int

    val to_int : t -> int
    (** The number of the fiber in its run: [main] has 0, and each new fiber
        gets the next number. The reports of the simulator name a fiber with no
        name ["fiber-n"], with the same number. In the simulator, a seed gives
        the same numbers. The ids of two runs can be equal. *)
  end

  val self : unit -> Id.t
  (** The id of the fiber that calls it. The body of [scope], [shield] and
      [with_timeout] runs in the fiber of the caller, so it has the id of the
      caller. Each [fork] makes a new fiber with a new id, also [fork_promise].
      For example, a writer fiber can keep its id. A function can then refuse a
      call from that fiber that waits for the end of the writer.

      @raise Not_in_fiber outside a fiber. *)

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

  (** A counting semaphore for fiber code. The acquire is an event, so a
      [choose] can wait for a permit and for other events (design section 2.5,
      Locks for fiber code). A lock is [create 1]. *)
  module Semaphore : sig
    type t

    val create : ?max:int -> int -> t
    (** [create ?max n] is a semaphore with [n] free permits. A release never
        makes more than [max] permits free. The default [max] is [n]. A count
        with no limit, as [Semaphore.Counting.make 0] of the stdlib, is
        [create ~max:max_int 0].

        @raise Invalid_argument
          if [n < 0], [max < n] or [max = 0]. Thus [create 0] raises: a
          semaphore that can never have a permit is a bug. Give a [~max] of 1 or
          more. *)

    val acquire : t -> unit event
    (** An event that takes one permit. The sync gets the permit only if it
        commits to this branch. If the [choose] commits to another branch, or a
        cancel ends the sync, the permit goes to the next waiter. The waiters
        get the permits in the order in which they started to wait (FIFO). *)

    val try_acquire : t -> bool
    (** Takes a free permit and gives [true], or gives [false] if no permit is
        free. It does not wait, and it is not a cancellation point. A [choose]
        of [acquire] and [always] is not a try: if a permit is free, the
        [choose] selects one of the two ready branches at random. *)

    val release : t -> unit
    (** Gives one permit back to the semaphore: to the oldest waiter, or to the
        free permits.

        @raise Invalid_argument
          if the free permits are already at the [max] of [create]. *)

    val try_release : t -> bool
    (** As [release], but it gives [false] and changes nothing if the free
        permits are already at the [max] of [create]. For example, the refill of
        a rate limit with [create ~max:m 0] uses it, because a refill of a full
        bucket is normal. *)

    val use : t -> (unit -> 'a) -> 'a
    (** [use t f] acquires a permit, runs [f] and releases the permit, also if
        [f] raises. The acquire is a cancellation point. *)
  end
end

module Ivar : sig
  type 'a t

  val create : unit -> 'a t

  val fill : 'a t -> 'a -> unit
  (** @raise Invalid_argument if the Ivar is full. *)

  val try_fill : 'a t -> 'a -> bool

  val fill_from_outside : 'a t -> 'a -> bool
  (** The fill for code that does not run in a fiber: a finaliser, a
      [Sys.signal] handler, a thread of [Thread.create], or a C callback. It
      touches no sync word and no lock: a domain does the fill at a safe point
      (design section 2.5, Fiber code with no handler). If the Ivar is full at
      that time, the fill does nothing. It gives [false] if no [run] is active.
      One [run] at a time takes these fills. An Ivar belongs to the [run] that
      created it or that last added a waiter to it: a fill of an Ivar of a [run]
      that ended does nothing and gives [false]. An Ivar that code made outside
      a [run], with no waiter, accepts the fill of any [run]. The simulator
      accepts fills only with [Sim.run ~outside_fills:true]. *)

  val read : 'a t -> 'a event

  val peek : 'a t -> 'a option
end

(** A variable: a value that can change, with an event for its change (design
    section 2.4, A variable). *)
module Var : sig
  type 'a t

  (** A view: a variable and the state of it that a fiber saw. Each set makes a
      new state. *)
  type 'a view

  val create : 'a -> 'a t

  val get : 'a t -> 'a
  (** [get t] is [value (view t)]. *)

  val view : 'a t -> 'a view

  val value : 'a view -> 'a

  val set : 'a t -> 'a -> unit
  (** [set t v] gives [t] a new state with the value [v], and it wakes the
      fibers that wait in [changed]. Each set is a change, also a set of an
      equal value. A program that wants no wake for an equal value compares the
      values before the set.

      @raise Fiber.Not_in_fiber outside a fiber. Then [t] does not change. *)

  val changed : 'a view -> 'a view event
  (** [changed seen] is ready when the state of the variable of [seen] is not
      the state of [seen]. The event gives a view of the same variable, with its
      state when the fiber continues. That state is never the state of [seen].
      Thus a set between [view] and the sync is not lost: the event is ready at
      once. Only the last value counts: a fiber can miss the values between two
      of its syncs. The value can change again before the fiber uses the result.
      Then the next [changed] is ready at once. For example, a wait with a
      deadline that can change:

      {[
      let rec wait seen =
        match
          sync
            (choose
               [ wrap (Var.changed seen) (fun v -> `Changed v)
               ; wrap (Time.at Mono (Var.value seen)) (fun () -> `Timeout) ] )
        with
        | `Changed v ->
            wait v
        | `Timeout ->
            ()
      in
      wait (Var.view d)
      ]} *)
end

(** {1 Worlds}

    A world is everything outside the program: the clock, the I/O, the waits,
    the randomness and the order in which the domains run. In production, the
    world is the OS ([Weft_linux], [Weft_kqueue]). In tests, the world is the
    simulator ([Weft_sim]). The simulator models the OS, and it also controls
    the order of the domains and the randomness. Design section 2.3. *)

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

    [~slack] (100 us by default) rounds the alarm of a domain up to a multiple
    of the slack. Thus near deadlines share one alarm. A timer never fires
    before its deadline (open question 7).

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

  val promise_report : (int option -> exn -> unit) Atomic.t
  (** The report of a promise that failed and that no fiber awaited, with the id
      of the run of the promise ([Sched.t.run_id]). By default, it writes the
      exception to [stderr]. It runs in a finaliser: it must not perform an
      effect or raise. *)

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
  module Process = Process
end

module Chan : sig
  (** A synchronous channel: a [send] and a [recv] commit together. *)
  type 'a t = 'a Private.Chan.t

  val create : unit -> 'a t

  val send : 'a t -> 'a -> unit event

  val recv : 'a t -> 'a event
end

(** {1 Sockets}

    Design sections 2.3 and 2.7. *)

module Net : sig
  (** A port out of the range 0 to 65535 gives [Error (Other "EINVAL", _)]. A
      NUL byte in a host or a path gives
      [Error (Other "EINVAL: a NUL byte in a string", _)], as for each path,
      name and argument of [File], [Dir], [Dns] and [Process.spawn]. *)
  type addr = Private.Net.addr =
    | Tcp of string * int
        (** A host and a port. [local_addr] also gives the address of a UDP
            socket as [Tcp]. In the simulator, the host is a name, for example
            ["server"]. *)
    | Unix of string
        (** The path of a Unix domain socket. A path of 108 bytes or more on
            Linux and in the simulator (104 on macOS and FreeBSD) gives
            [Error (Other "ENAMETOOLONG", _)]. *)

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
    | No_resources of string
        (** The OS has no resource for the operation at this time: [EMFILE],
            [ENFILE], [ENOBUFS] or [ENOMEM]. A spawn gives [EAGAIN] at the limit
            of processes. A [File], [Dir] or [Dns] operation gives [EAGAIN] when
            the thread pool cannot start a thread. The string is the name of the
            errno, for example ["EMFILE"]. A program can pause, then try again.
            The simulator gives ["EMFILE"] at the limit of
            [Weft_sim.set_fd_limit]. *)
    | Other of string

  (** The error, and the name of the function. It is [Private.Net.Error]. *)
  exception Error of error * string

  val listen : ?backlog:int -> addr -> t

  val accept : t -> t * addr
  (** Waits for a connection, and gives its socket and the address of the
      client. A connection that the client reset before the accept gives no
      error. On FreeBSD, the OS gives [ECONNABORTED], and [accept] takes the
      next connection. Linux, macOS and the simulator give the connection. On
      Linux and in the simulator, its first read gets [Error (Reset, _)]. On
      macOS, its address is [Tcp ("", 0)].

      @raise Error
        with [No_resources] if the process or the system has no free fd or no
        memory. On Linux, FreeBSD and the simulator, the connection stays in the
        backlog, and the next [accept] can get it. macOS closes it. Thus an
        accept loop pauses, then calls [accept] again. The facts about macOS and
        the backlog of FreeBSD come from a read of their source, not from a
        test. *)

  val connect : addr -> t

  val local_addr : t -> addr
  (** The local address of a socket, for example the port that the OS selected
      for a listen on port 0. A connected socket has a port that the OS
      selected. An accepted socket has the address that its client connected to.
      A Unix socket with no path, for example a client, gives [Unix ""]. A UDP
      socket gives [Tcp (host, port)]. In the production worlds, the host is an
      IP address, for example ["127.0.0.1"] after a listen on ["localhost"]. In
      the simulator, the host is the name of the listener or of the UDP socket.
      A client has the host ["client"]. The simulator selects the port of a
      client, and a port for [listen] and [udp] on port 0, from a counter that
      starts at 40000.

      @raise Error with [Bad_fd] if the socket is closed. *)

  val read : t -> bytes -> int -> int -> int
  (** [read t buf off len] reads at least one byte, or gives 0 at the end of the
      stream.

      @raise Invalid_argument if [len < 1] or the range is not in [buf]. *)

  val write : t -> string -> unit
  (** Writes all of the string. *)

  val write_sub : t -> bytes -> int -> int -> int
  (** Writes at least one byte, and gives the number. *)

  val read_now : t -> bytes -> int -> int -> int option
  (** [read_now t buf off len] does one read with no wait. It gives [None] if no
      byte is ready, [Some 0] at the end of the stream, and [Some n] if it read
      [n] bytes. On a UDP socket, [Some 0] is an empty datagram, as for [read]
      (see [udp]). After [None], the next [readable] waits for a new event of
      the OS. An event can be stale, so read again, and on [None] wait again.
      Use it after [readable] in a [choose], so that a stale event does not
      block.

      @raise Invalid_argument if [len < 1] or the range is not in [buf]. *)

  val write_now : t -> bytes -> int -> int -> int option
  (** [write_now t buf off len] does one write with no wait. It gives [None] if
      the socket cannot take a byte, and [Some n] if it wrote [n] bytes. After
      [None], the next [writable] waits for a new event of the OS. An event can
      be stale, as for [read_now].

      @raise Invalid_argument if the range is not in [buf]. *)

  val shutdown : t -> unit
  (** Ends the send side. The peer reads the end of the stream. *)

  val close : t -> unit
  (** Closes the socket. A fiber that waits on it, and each later operation,
      gets [Error (Bad_fd, _)]. A second [close] does nothing. [close] never
      blocks, so a cancelled fiber can call it. On Linux, the close of a TCP or
      Unix stream socket with unread data sends a reset: the next read of the
      peer gets [Error (Reset, _)]. *)

  val reset : t -> unit
  (** Closes a TCP socket with a reset (RST, [SO_LINGER] with a time of 0). The
      peer reads the data that came before the reset. Then its next read or
      write gets [Error (Reset, _)], and later reads give the end of the stream.
      On macOS and FreeBSD, the write can get [Broken_pipe]. The OS drops the
      data that the peer did not get. [reset] follows the protocol of [close],
      and it never blocks.

      A reset after [shutdown] gives the peer the end of the stream and
      [Error (Broken_pipe, _)], as on Linux. If the peer already closed, or if
      [t] is closed, a listener, a Unix socket or a UDP socket, [reset] is the
      same as [close]. Thus a Unix socket with unread data also gives a reset.
  *)

  val readable : t -> unit event
  (** Ready when the socket can be read. It is also ready after a [close]: the
      next operation then raises [Error (Bad_fd, _)]. The event can be stale:
      another fiber read the data, or the OS gave an event with no data. Thus a
      fiber that must not block uses [read_now] after it. *)

  val writable : t -> unit event
  (** As [readable], for a write. *)

  (** {2 UDP} *)

  val udp : string * int -> t
  (** [udp (host, port)] gives a UDP socket, bound to the host and the port. The
      port 0 gives a free port, and [local_addr] gives it as [Tcp (host, port)].
      In the simulator, the host is a name, for example ["client"]. [close]
      closes the socket. [read] takes one datagram with no address, and 0 is
      then an empty datagram, not the end of a stream. [write] raises
      [Error (Other "EDESTADDRREQ", _)]. *)

  val sendto : t -> bytes -> int -> int -> string * int -> unit
  (** [sendto t buf off len (host, port)] sends one datagram with the bytes of
      the range. A datagram to an address with no socket is lost, with no error.
      A datagram of more than 65507 bytes raises [Error (Other "EMSGSIZE", _)].

      @raise Invalid_argument
        if the range is not in [buf], or if [t] is not a UDP socket. *)

  val recvfrom : t -> bytes -> int -> int -> int * (string * int)
  (** [recvfrom t buf off len] waits for one datagram. It gives the number of
      bytes in [buf], and the host and the port of the sender. If the datagram
      has more than [len] bytes, the other bytes are lost.

      @raise Invalid_argument
        if [len < 1], if the range is not in [buf], or if [t] is not a UDP
        socket. *)

  (** {2 Socket options} *)

  type 'a sockopt = 'a Private.Op.sockopt =
    | No_delay : bool sockopt  (** [TCP_NODELAY]. Only for TCP. *)
    | Keep_alive : bool sockopt  (** [SO_KEEPALIVE]. *)
    | Broadcast : bool sockopt  (** [SO_BROADCAST]. *)
    | Recv_buffer : int sockopt  (** [SO_RCVBUF], in bytes. *)
    | Send_buffer : int sockopt  (** [SO_SNDBUF], in bytes. *)

  val set_option : t -> 'a sockopt -> 'a -> unit
  (** [set_option t opt v] sets an option of the socket. The OS can change a
      size: Linux keeps two times the value. Thus [get_option] can give a
      different value.

      @raise Invalid_argument if a size is not positive. *)

  val get_option : t -> 'a sockopt -> 'a
  (** [get_option t opt] gives the value of an option of the socket. An accepted
      socket has the options of its listener. *)
end

(** {1 Files and directories}

    Design section 2.3, The I/O modules.

    Each operation runs on the thread pool in production, and the simulator has
    an in-memory file system. Each operation is a cancellation point: on a
    cancel, the fiber does not wait for the operation, and the pool completes it
    alone (design section 2.5, Blocking operations). *)

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

(** {1 Signals and processes}

    Design section 2.7, Signals. *)

module Signal : sig
  type t = Private.Op.signal =
    | SIGHUP
    | SIGINT
    | SIGQUIT
    | SIGILL
    | SIGABRT
    | SIGFPE
    | SIGKILL
    | SIGSEGV
    | SIGBUS
    | SIGPIPE
    | SIGALRM
    | SIGTERM
    | SIGUSR1
    | SIGUSR2
    | SIGCHLD
    | SIGCONT
    | SIGSTOP
    | SIGTSTP
    | SIGWINCH
    | Other_signal of int  (** A signal with its number on this OS. *)

  val received : t -> int event
  (** [received s] gives the number of times that [s] came since the last commit
      of a [received s] event. The number can be 1 for many signals, because the
      OS does not queue them. One fiber gets each signal: a signal that comes
      when no fiber waits stays until the next commit. The world must watch [s],
      for example with [Weft_linux.run ~signals].

      @raise Invalid_argument
        at [sync], if the world does not watch [s] (also after [unwatch s]), or
        if [s] cannot be an event: [SIGILL], [SIGFPE], [SIGSEGV], [SIGBUS],
        [SIGKILL] and [SIGSTOP]. *)

  val unwatch : t -> unit
  (** [unwatch s] stops the watch of [s] for the remaining time of the run. The
      signal then gets the action that it had before [run], usually the default
      action: for [SIGINT], the end of the process. A signal that came before
      the unwatch and that no fiber took also gets the action, so the process
      can end in [unwatch]. A fiber that waits on [received s] at the unwatch
      raises [Invalid_argument] at its [sync], as a later [received s] does.
      [unwatch] does not block. A server that stops gracefully at the first
      [SIGINT] can call it, so that the next [SIGINT] ends the process.

      @raise Invalid_argument
        if the world does not watch [s], also after an earlier [unwatch s], or
        if [s] cannot be an event.
      @raise Net.Error
        if the OS gives an error, for example [ENOMEM] of [signalfd]. Then the
        world still watches [s]. *)
end

module Process : sig
  (** The same exception as [Net.Error]. *)
  exception Error of Net.error * string

  (** A child process. *)
  type t = Private.Process.t

  (** How the child gets its fd 0, 1 or 2. *)
  type stdio =
    | Inherit  (** The fd of the parent. *)
    | Null  (** [/dev/null]. *)
    | Piped  (** A pipe: the parent gets the other end. *)

  type status = Private.Op.status = Exited of int | Signaled of Signal.t

  val spawn :
       ?env:string list
    -> ?cwd:string
    -> ?stdin:stdio
    -> ?stdout:stdio
    -> ?stderr:stdio
    -> string
    -> string list
    -> t
  (** [spawn prog args] starts [prog] with the arguments [args]. A [prog] with
      no [/] is found in [PATH]. The child gets the default action for each
      signal, and no blocked signals. Each stdio is [Inherit] by default.

      @raise Error
        with [No_entry] if the program does not exist, or [No_resources] if the
        OS cannot make the process or its pipes. *)

  val pid : t -> int

  val stdin : t -> Pipe.t option
  (** The write end of the pipe of [~stdin:Piped]. *)

  val stdout : t -> Pipe.t option

  val stderr : t -> Pipe.t option

  val exited : t -> status event
  (** Ready when the child exited. *)

  val wait : t -> status
  (** [sync (exited t)]. A program must wait for each child: the wait ends the
      zombie and closes the fd of the child.

      @raise Error
        with [Other "ECHILD"] if other code reaped the child, for example with
        [waitid] or with [SIGCHLD] set to [SIG_IGN]. *)

  val kill : t -> Signal.t -> unit
  (** Sends a signal to the child. After the exit, it does nothing.

      @raise Error
        with [Other "ESRCH"] if other code reaped the child, as on Linux. *)
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
