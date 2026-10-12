type fd = int

type addr = Tcp of string * int | Unix of string

type error =
  | Would_block
  | Bad_fd
  | Refused
  | Reset
  | Broken_pipe
  | In_use
  | No_entry
  | Exists
  | Not_dir
  | Is_dir
  | Not_empty
  | No_resources of string
  | Other of string

type open_flag =
  | O_read
  | O_write
  | O_create
  | O_exclusive
  | O_truncate
  | O_append

type kind = Regular | Directory | Symlink | Special

type stat = {kind: kind; size: int; perm: int; mtime: int64}

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

type stdio = Inherit | Null | Piped

type spawn =
  { prog: string
  ; argv: string list
  ; env: string list option
  ; cwd: string option
  ; stdin: stdio
  ; stdout: stdio
  ; stderr: stdio }

type spawned =
  { pid: int
  ; pidfd: fd
  ; to_stdin: fd option
  ; from_stdout: fd option
  ; from_stderr: fd option }

type status = Exited of int | Signaled of signal

type _ blocking =
  | Open : string * open_flag list * int -> fd blocking
  | Pread : fd * bytes * int * int * int -> int blocking
  | Pwrite : fd * string * int -> int blocking
  | Fsync : fd -> unit blocking
  | Fstat : fd -> stat blocking
  | Stat : string -> stat blocking
  | Ftruncate : fd * int -> unit blocking
  | Mkdir : string * int -> unit blocking
  | Readdir : string -> string list blocking
  | Rename : string * string -> unit blocking
  | Unlink : string -> unit blocking
  | Rmdir : string -> unit blocking
  | Resolve : string -> string list blocking
  | Reverse : string -> string blocking

type _ sockopt =
  | No_delay : bool sockopt
  | Keep_alive : bool sockopt
  | Broadcast : bool sockopt
  | Recv_buffer : int sockopt
  | Send_buffer : int sockopt

type _ t =
  | Listen : addr * int -> fd t
  | Accept : fd -> (fd * addr) t
  | Connect : addr -> fd t
  | Connect_result : fd -> unit t
  | Local_addr : fd -> addr t
  | Read : fd * bytes * int * int -> int t
  | Write : fd * bytes * int * int -> int t
  | Shutdown : fd -> unit t
  | Reset_on_close : fd -> unit t
  | Udp : string * int -> fd t
  | Sendto : fd * bytes * int * int * string * int -> int t
  | Recvfrom : fd * bytes * int * int -> (int * string * int) t
  | Set_option : fd * 'a sockopt * 'a -> unit t
  | Get_option : fd * 'a sockopt -> 'a t
  | Pipe : (fd * fd) t
  | Fd_read : fd * bytes * int * int -> int t
  | Fd_write : fd * bytes * int * int -> int t
  | Close : fd -> unit t
  | Register : int * fd * int -> unit t
  | Deregister : int * fd -> unit t
  | Start : int * 'a blocking -> int t
  | Finish : int * 'a blocking -> 'a t
  | Spawn : spawn -> spawned t
  | Wait_child : fd -> status option t
  | Kill : fd * signal -> unit t
  | Watch : signal -> signal t
  | Unwatch : signal -> signal t
  | Resend : signal -> unit t
  | Detach : int -> unit t

let pp_addr ppf = function
  | Tcp (h, p) ->
      Format.fprintf ppf "%s:%d" h p
  | Unix p ->
      Format.fprintf ppf "unix:%s" p

let pp_error ppf = function
  | Would_block ->
      Format.pp_print_string ppf "EAGAIN"
  | Bad_fd ->
      Format.pp_print_string ppf "EBADF"
  | Refused ->
      Format.pp_print_string ppf "ECONNREFUSED"
  | Reset ->
      Format.pp_print_string ppf "ECONNRESET"
  | Broken_pipe ->
      Format.pp_print_string ppf "EPIPE"
  | In_use ->
      Format.pp_print_string ppf "EADDRINUSE"
  | No_entry ->
      Format.pp_print_string ppf "ENOENT"
  | Exists ->
      Format.pp_print_string ppf "EEXIST"
  | Not_dir ->
      Format.pp_print_string ppf "ENOTDIR"
  | Is_dir ->
      Format.pp_print_string ppf "EISDIR"
  | Not_empty ->
      Format.pp_print_string ppf "ENOTEMPTY"
  | No_resources s ->
      Format.pp_print_string ppf s
  | Other s ->
      Format.pp_print_string ppf s
