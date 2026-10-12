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

(**/**)

val echo_server : Weft.Net.t -> unit

val read_n : Weft.Net.t -> int -> string

val drain : ('a -> bytes -> int -> int -> int) -> 'a -> string
(** [drain read r] reads until the end of the stream. *)

val error_of : (unit -> 'a) -> string
(** The text of the error that [f] raises, or ["no error"]. *)

val host_port : Weft.Net.t -> string * int
(** The local host and port of a TCP or UDP socket. *)
