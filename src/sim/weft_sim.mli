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

    The fork rule: [run] starts no OS domain and no OS thread, for each value of
    [~domains]. The virtual domains run on the thread that calls [run]. The jobs
    of [File], [Dir] and [Dns], the fake processes of [Process.spawn], the
    timers and the faults also run on this thread. Thus a process can call
    [Unix.fork] before, between and after runs, for example a tool that forks
    one process for each mutant. OCaml 5 refuses [Unix.fork] after a
    [Domain.spawn], also after its [Domain.join], so other code of the process
    must not start a domain, for example [Weft_linux.run] with more than one
    domain.

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
      Deregister.
    @raise Killed
      if the program gets a signal with an action that ends the process (see
      {!signal}). *)

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

val open_fds : t -> int
(** The number of open fds of the simulator: the sockets, the ends of the pipes,
    the files and the children. The simulator has no fds 0, 1 and 2. *)

val set_fd_limit : t -> int option -> unit
(** [set_fd_limit t (Some n)] limits the open fds to [n], as [RLIMIT_NOFILE]. An
    operation that makes [k] fds raises [Error (No_resources "EMFILE", _)] if
    {!open_fds} plus [k] is more than [n]: [Net.listen], [Net.connect],
    [Net.accept], [Net.udp], [File.openfile], [Dir.readdir] (1 fd),
    [Pipe.create] (2 fds) and [Process.spawn] (1 fd, and 2 fds for each
    [Piped]). [Dir.readdir] closes its fd at the end, as [opendir] on Linux. As
    on Linux, [accept] gives the error also with an empty backlog, it leaves the
    connection in the backlog, and the listener gets no new edge. [None] (the
    default) removes the limit. A limit below {!open_fds} closes no fd. To make
    the next [accept] fail, set the limit to {!open_fds}.

    @raise Invalid_argument if [n] is negative. *)

val socket_pair : t -> Weft.Net.t * Weft.Net.t
(** [socket_pair t] gives the two sides of a TCP connection, as a connect and an
    accept give them, but with no listener. The connection is established at
    once. It has the latency and the faults of each TCP connection. [Net.reset]
    and {!fault} act on it as on a connection of [connect]. The first socket has
    the address [Tcp ("client", p)], and the second [Tcp ("server", q)], with
    [p] and [q] from the counter of ports (as for [Net.local_addr]). The peer of
    each socket is the address of the other socket.

    @raise Weft.Net.Error
      with [No_resources "EMFILE"] if the two fds are more than the limit of
      {!set_fd_limit}. *)

val fault : t -> Weft.Net.t -> Weft.Net.error -> unit
(** [fault t s e] breaks the connection of [s] at the side of [s]. The next
    operation of [s] raises [Weft.Net.Error (e, _)] one time: the next write, or
    the next read after the data that [s] already has. This is the order of the
    error of a socket on Linux. Then [s] has the end of the stream and no peer:
    later reads give 0, and writes give [Broken_pipe]. A fiber that waits on [s]
    wakes. After the fault, no data, no end of the stream and no reset of the
    peer comes to [s].

    The socket has one slot for its error, as on Linux. Thus a second fault
    replaces an error that [s] did not give, also the reset of a peer.

    With [e = Reset], the fault is a reset of the two sides, as from a
    middlebox. The peer gets the reset after the delay, after the data that [s]
    sent before, as from [Net.reset]. On a Unix socket, the writes of [s] give
    [Broken_pipe] and its next read gives [Reset], as after a reset of a Unix
    socket. With another error, the link is lost. The peer gets nothing, and its
    data stays in flight, so its writes fill the window and then wait. The peer
    can thus wait until its own timeout.

    The fault occurs at the call, and the delay of a reset comes from the seed,
    as each delay. Thus a seed gives the same run.

    @raise Invalid_argument
      if [e] is not [Reset], [Broken_pipe], [Refused], [No_resources _] or
      [Other _], or if [s] is not a socket of a TCP or Unix stream connection.
    @raise Weft.Net.Error with [Bad_fd] if [s] is closed. *)

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
    [SIGCHLD], [SIGWINCH], and [Other_signal] 0, 21, 22 and 23 ([SIGTTIN],
    [SIGTTOU] and [SIGURG] on Linux). [SIGKILL] always ends it. *)

val add_program : t -> string -> (string list -> program) -> unit
(** [add_program t name f]: [Process.spawn name args] starts [f args]. A name
    that is not in the table gives [No_entry]. *)

val signal : t -> Weft.Signal.t -> unit
(** [signal t s] sends [s] to the program. If the program watches [s], domain 0
    gets it at its next switch point. If not, for example after
    [Signal.unwatch s], [s] gets its default action at once. The action ends the
    run with [Killed s], except for the signals that do not end a fake program
    (see {!program}), which do nothing. The world ignores [SIGPIPE], so it also
    does nothing. *)

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

(** The program got a signal with an action that ends the process, for example
    [SIGINT] after [Signal.unwatch SIGINT]. [run] raises it at once, as the OS
    ends the process. *)
exception Killed of Weft.Signal.t

(**/**)

(* For the tests of the checks. *)
val sched : t -> Weft.Private.Sched.t
