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

type _ t =
  | Listen : addr * int -> fd t
  | Accept : fd -> (fd * addr) t
  | Connect : addr -> fd t
  | Connect_result : fd -> unit t
  | Read : fd * bytes * int * int -> int t
  | Write : fd * bytes * int * int -> int t
  | Shutdown : fd -> unit t
  | Pipe : (fd * fd) t
  | Fd_read : fd * bytes * int * int -> int t
  | Fd_write : fd * bytes * int * int -> int t
  | Close : fd -> unit t
  | Register : int * fd * int -> unit t
  | Deregister : int * fd -> unit t
  | Start : int * 'a blocking -> int t
  | Finish : int * 'a blocking -> 'a t
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
  | Other s ->
      Format.pp_print_string ppf s
