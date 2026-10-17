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
  | No_resources of string
      (** The OS has no resource for the operation at this time: [EMFILE],
          [ENFILE], [ENOBUFS] or [ENOMEM], or [EAGAIN] of a spawn or of a pool
          with no thread. The string is the name of the errno. *)
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

(** A signal. [Other_signal n] is a signal with no name here, with its number on
    this OS. *)
type signal =
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
  | Other_signal of int

(** How the child gets one of its fds 0, 1 and 2. *)
type stdio =
  | Inherit  (** The fd of the parent. *)
  | Null  (** [/dev/null]. *)
  | Piped  (** A pipe. The parent gets the other end. *)

type spawn =
  { prog: string  (** The program. A name with no [/] is found in [PATH]. *)
  ; argv: string list  (** The arguments, with the name of the program first. *)
  ; env: string list option  (** [None]: the environment of the parent. *)
  ; cwd: string option
  ; stdin: stdio
  ; stdout: stdio
  ; stderr: stdio }

(** A child process: its pid, the fd that tells its exit (a [pidfd] on Linux),
    and the ends of its pipes. *)
type spawned =
  { pid: int
  ; pidfd: fd
  ; to_stdin: fd option
  ; from_stdout: fd option
  ; from_stderr: fd option }

type status = Exited of int | Signaled of signal

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
  | Pwrite : fd * bytes * int * int * int -> int blocking
      (** The fd, the buffer, its range, and the file offset, or [-1]. The world
          copies the data from the buffer at [Start]. *)
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

(** A socket option. *)
type _ sockopt =
  | No_delay : bool sockopt  (** [TCP_NODELAY]. Only for TCP. *)
  | Keep_alive : bool sockopt  (** [SO_KEEPALIVE]. *)
  | Broadcast : bool sockopt  (** [SO_BROADCAST]. *)
  | Recv_buffer : int sockopt  (** [SO_RCVBUF], in bytes. *)
  | Send_buffer : int sockopt  (** [SO_SNDBUF], in bytes. *)

type _ t =
  | Listen : addr * int -> fd t
      (** A stream socket that listens, with a backlog. *)
  | Accept : fd -> (fd * addr) t
  | Connect : addr -> fd t
      (** Starts a connect. The fd is writable when the connect ends. *)
  | Connect_result : fd -> unit t
      (** The result of a connect: [Would_block] while it continues. *)
  | Local_addr : fd -> addr t
      (** [getsockname]: the local address of a socket. A Unix socket with no
          path gives [Unix ""]. *)
  | Read : fd * bytes * int * int -> int t  (** 0 at the end of the stream. *)
  | Write : fd * bytes * int * int -> int t
  | Shutdown : fd -> unit t  (** Ends the send side. *)
  | Reset_on_close : fd -> unit t
      (** Sets [SO_LINGER] to on with a time of 0. Then the close(2) of the fd
          sends a reset (RST), not the end of the stream, and it does not block.
      *)
  | Udp : string * int -> fd t
      (** A UDP socket, bound to a host and a port. The port 0 gives a free
          port. *)
  | Sendto : fd * bytes * int * int * string * int -> int t
      (** Sends one datagram from the range of the buffer to a host and a port.
      *)
  | Recvfrom : fd * bytes * int * int -> (int * string * int) t
      (** Takes one datagram: its length in the buffer, and the host and the
          port of the sender. The world drops the bytes that do not fit. *)
  | Set_option : fd * 'a sockopt * 'a -> unit t
  | Get_option : fd * 'a sockopt -> 'a t
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
  | Spawn : spawn -> spawned t
      (** Starts a child process. The pidfd is readable when the child exits. *)
  | Wait_child : fd -> status option t
      (** The status of a child that exited, and the end of the zombie. [None]
          while the child runs. *)
  | Kill : fd * signal -> unit t  (** Sends a signal to a child. *)
  | Watch : signal -> signal option t
      (** [Ok (Some s)] if the world gives this signal as [Ready.Signal s]: [s]
          is the form of the signal in [Ready.Signal], for example [SIGUSR1] for
          [Other_signal 10] on Linux. [Ok None] if the world does not watch the
          signal. *)
  | Unwatch : signal -> signal option t
      (** Stops the watch of a signal: the signal gets its action again, the
          action that it had before [run]. A signal that came and that the world
          did not give yet also gets the action. [Ok (Some s)] gives the form of
          the signal, as [Watch]. [Ok None] if the world does not watch the
          signal. [Error] is an error of the OS, and the world then continues to
          watch the signal. *)
  | Resend : signal -> unit t
      (** Sends the signal to the process itself, after an [Unwatch]: the signal
          gets its action. *)
  | Detach : int -> unit t
      (** The fiber does not wait for the job any more. The world drops its
          result, and closes the fd that it opened. The completion still comes.
      *)

val pp_addr : Format.formatter -> addr -> unit

val pp_error : Format.formatter -> error -> unit
