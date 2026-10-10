(** The operations that a world implements (design section 2.3). Each operation
    is one attempt, and it never blocks. *)

(** An fd of the world: an OS fd in production, a number of the simulator in the
    simulator. *)
type fd = int

type addr =
  | Tcp of string * int
      (** A host (an IP address, or a name of the simulator) and a port. *)
  | Unix of string  (** The path of a Unix domain socket. *)

type error =
  | Would_block  (** The fd is not ready. *)
  | Bad_fd  (** [EBADF]: the fd is closed. *)
  | Refused  (** [ECONNREFUSED]. *)
  | Reset  (** [ECONNRESET]. *)
  | Broken_pipe  (** [EPIPE]. *)
  | In_use  (** [EADDRINUSE]. *)
  | Other of string

type _ t =
  | Listen : addr * int -> fd t
      (** A stream socket that listens, with a backlog. *)
  | Accept : fd -> (fd * addr) t
  | Connect : addr -> fd t
      (** Starts a connect. The fd is writable when the connect ends. *)
  | Connect_result : fd -> unit t
      (** The result of a connect: [Would_block] while it continues. *)
  | Read : fd * bytes * int * int -> int t  (** 0 at the end of the stream. *)
  | Write : fd * bytes * int * int -> int t
  | Shutdown : fd -> unit t  (** Ends the send side. *)
  | Close : fd -> unit t
  | Register : int * fd * int64 -> unit t
      (** [Register (domain, fd, token)] adds the fd to the poller of the
          domain, edge-triggered, for read and write. *)
  | Deregister : int * fd -> unit t

val pp_addr : Format.formatter -> addr -> unit

val pp_error : Format.formatter -> error -> unit
