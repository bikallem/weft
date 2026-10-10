type fd = int

type addr = Tcp of string * int | Unix of string

type error =
  | Would_block
  | Bad_fd
  | Refused
  | Reset
  | Broken_pipe
  | In_use
  | Other of string

type _ t =
  | Listen : addr * int -> fd t
  | Accept : fd -> (fd * addr) t
  | Connect : addr -> fd t
  | Connect_result : fd -> unit t
  | Read : fd * bytes * int * int -> int t
  | Write : fd * bytes * int * int -> int t
  | Shutdown : fd -> unit t
  | Close : fd -> unit t
  | Register : int * fd * int -> unit t
  | Deregister : int * fd -> unit t

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
  | Other s ->
      Format.pp_print_string ppf s
