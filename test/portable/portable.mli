(** The tests that use only the public API, for each world. *)

type world =
  { name: string
  ; domains: int  (** The number of domains of [run]. *)
  ; run: 'a. (unit -> 'a) -> 'a  (** Runs a program in the world. *)
  ; listen: unit -> Weft.Net.t * Weft.Net.addr
        (** A new listener, and the address that a client connects to. *)
  ; udp: unit -> Weft.Net.t * (string * int)
        (** A new UDP socket, and the address that a peer sends to. *)
  ; tmp: unit -> string
        (** A new empty directory, in fiber code: [run] calls it. *) }

val tests : world -> Windtrap.test list

val layers : world -> Windtrap.test
(** The tests of the fiber layer (milestone M2), also in [tests]. *)

val semaphores : world -> Windtrap.test
(** The tests of [Fiber.Semaphore] (issue 4), also in [tests]. *)

val vars : world -> Windtrap.test
(** The tests of [Var] (issue 5), also in [tests]. *)

val fiber_ids : world -> Windtrap.test
(** The tests of [Fiber.self] and [Fiber.Id] (issue 9), also in [tests]. *)

val files : world -> Windtrap.test
(** The tests of [File] and [Dir] (milestone M2), also in [tests]. *)

val reset_details : world -> Windtrap.test
(** The details of [Net.reset] that follow Linux. The simulator and the Linux
    world run them. They are not in [tests]. *)

val unwatch_modes : world -> unit -> unit
(** The end of a watch in the program of [WEFT_UNWATCH] (issue 6), in the modes
    "taken", "count" and "pending". The production worlds run it with one
    domain. It is not in [tests]. *)

val unwatch_busy : world -> unit -> unit
(** As [unwatch_modes], in the mode "busy": a SIGINT ends a process whose only
    domain computes with no switch. It is not in [tests]. *)

(**/**)

val echo_server : ?reset:bool -> Weft.Net.t -> unit
(** [echo_server l] echoes the bytes of each client of [l]. At the end of the
    stream, it closes the connection. With [~reset:true], it closes the
    connection with [Net.reset]: then no side of the connection keeps a
    TIME_WAIT socket. *)

val connect_fast : Weft.Net.addr -> Weft.Net.t
(** [connect_fast a] is [Net.connect a], but if the connect takes more than 10
    s, the test gives a failure with a clear message. When the conntrack table
    of the host is full, the host drops each SYN, and a connect waits for 127 s.
    A lost wakeup of the connect in weft also gives this failure. *)

val read_n : Weft.Net.t -> int -> string

val drain : ('a -> bytes -> int -> int -> int) -> 'a -> string
(** [drain read r] reads until the end of the stream. *)

val error_of : (unit -> 'a) -> string
(** The text of the error that [f] raises, or ["no error"]. *)

val host_port : Weft.Net.t -> string * int
(** The local host and port of a TCP or UDP socket. *)
