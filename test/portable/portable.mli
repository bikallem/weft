(** The tests that use only the public API, for each world. *)

type world =
  { name: string
  ; domains: int  (** The number of domains of [run]. *)
  ; run: 'a. (unit -> 'a) -> 'a  (** Runs a program in the world. *)
  ; listen: unit -> Weft.Net.t * Weft.Net.addr
        (** A new listener, and the address that a client connects to. *) }

val tests : world -> Windtrap.test list

(**/**)

val echo_server : Weft.Net.t -> unit

val read_n : Weft.Net.t -> int -> string
