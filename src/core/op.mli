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
  | No_entry  (** [ENOENT], or a name that DNS does not know. *)
  | Exists  (** [EEXIST]. *)
  | Not_dir  (** [ENOTDIR]. *)
  | Is_dir  (** [EISDIR]. *)
  | Not_empty  (** [ENOTEMPTY]. *)
  | Other of string

(** The flags of [Open]. [O_read] and [O_write] together give [O_RDWR]. Each
    open has [O_CLOEXEC]. *)
type open_flag =
  | O_read
  | O_write
  | O_create
  | O_exclusive
  | O_truncate
  | O_append

type kind = Regular | Directory | Symlink | Special

type stat =
  {kind: kind; size: int; perm: int; mtime: int64  (** [Posix], in ns. *)}

(** An operation with no readiness model. The production worlds run it on a
    thread pool, and the simulator puts its completion in its agenda (design
    section 2.3). *)
type _ blocking =
  | Open : string * open_flag list * int -> fd blocking
      (** The path, the flags and the permissions. *)
  | Pread : fd * bytes * int * int * int -> int blocking
      (** The fd, the buffer, its range, and the file offset. The offset [-1]
          reads at the offset of the fd. 0 at the end of the file. The world
          copies the data into the buffer only at [Finish]. *)
  | Pwrite : fd * string * int -> int blocking
      (** The fd, the data and the file offset, or [-1]. *)
  | Fsync : fd -> unit blocking
  | Fstat : fd -> stat blocking
  | Stat : string -> stat blocking
  | Ftruncate : fd * int -> unit blocking
  | Mkdir : string * int -> unit blocking
  | Readdir : string -> string list blocking
      (** The names, with no [.] and [..], in any order. *)
  | Rename : string * string -> unit blocking
  | Unlink : string -> unit blocking
  | Rmdir : string -> unit blocking
  | Resolve : string -> string list blocking
      (** [getaddrinfo]: the addresses of a host name, as text. *)
  | Reverse : string -> string blocking
      (** [getnameinfo]: the name of an address. *)

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
  | Pipe : (fd * fd) t
      (** A pipe: the read end and the write end. The two are non-blocking. *)
  | Fd_read : fd * bytes * int * int -> int t
      (** read(2) on a non-blocking fd that the library made, for example a
          pipe. 0 at the end of the stream. *)
  | Fd_write : fd * bytes * int * int -> int t  (** write(2), as [Fd_read]. *)
  | Close : fd -> unit t
  | Register : int * fd * int -> unit t
      (** [Register (domain, fd, token)] adds the fd to the poller of the
          domain, edge-triggered, for read and write. *)
  | Deregister : int * fd -> unit t
  | Start : int * 'a blocking -> int t
      (** [Start (domain, op)] gives [op] to the pool and gives the id of the
          job. When the job completes, the [Wait] of the domain gives
          [Ready.External id]. *)
  | Finish : int * 'a blocking -> 'a t
      (** [Finish (id, op)] gives the result of a job that completed. [op] is
          the operation of [Start]. *)
  | Detach : int -> unit t
      (** The fiber does not wait for the job any more. The world drops its
          result, and closes the fd that it opened. The completion still comes.
      *)

val pp_addr : Format.formatter -> addr -> unit

val pp_error : Format.formatter -> error -> unit
