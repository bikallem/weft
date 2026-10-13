(* The parts that the POSIX worlds share (design section 2.7): the stubs of the
   sockets, the pipes and the clocks, the errors, the signal numbers, the thread
   pools and the watchdog. A world adds its poller, its alarm, its wake, its
   signals and its child processes. *)

open Weft.Private

external mono_ns : unit -> int64 = "weft_mono_ns"

external posix_ns : unit -> int64 = "weft_posix_ns"

external listen : int -> string -> int -> int -> int = "weft_listen"

external local_addr : int -> int * string * int * bool = "weft_local_addr"

external connect : int -> string -> int -> int = "weft_connect"

external connect_result : int -> int = "weft_connect_result"

external accept : int -> int * string * int * bool = "weft_accept"

external recv : int -> bytes -> int -> int -> int = "weft_recv"

external send : int -> bytes -> int -> int -> int = "weft_send"

external shutdown_send : int -> int = "weft_shutdown_send"

external reset_on_close : int -> int = "weft_reset_on_close"

external udp : string -> int -> int = "weft_udp"

external sendto : int -> bytes -> int -> int -> string * int -> int
  = "weft_sendto"

external recvfrom : int -> bytes -> int -> int -> int * string * int
  = "weft_recvfrom"

external setsockopt : int -> int -> int -> int = "weft_setsockopt"

external getsockopt : int -> int -> int = "weft_getsockopt"

external pipe : unit -> int * int = "weft_pipe"

external fd_read : int -> bytes -> int -> int -> int = "weft_read"

external fd_write : int -> bytes -> int -> int -> int = "weft_write"

external close_fd : int -> int = "weft_close"

external getrandom : bytes -> int = "weft_getrandom"

external errno_codes : unit -> int array = "weft_errno_codes"

external strerror : int -> string = "weft_strerror"

external errname : int -> string = "weft_errname"

external signal_numbers : unit -> int array = "weft_signal_numbers"

external resend : int -> int = "weft_resend"

(* The thread pools (weft_pool.c). *)

(* The fields of a job, in the order of [weft_job_start]. Only the C side reads
   them. *)
type[@warning "-69"] job_args =
  { id: int
  ; dom: int
  ; kind: int
  ; s1: string
  ; s2: string
  ; fd: int
  ; a: int
  ; b: int
  ; data: string }

external mbx_create : int array -> nativeint = "weft_mbx_create"

external mbx_close : nativeint -> unit = "weft_mbx_close"

external mbx_take : nativeint -> int -> int array = "weft_mbx_take"

external job_start : nativeint -> job_args -> nativeint = "weft_job_start"

external job_result : nativeint -> int * int * string = "weft_job_result"

external job_stat : nativeint -> int * int * int * int64 = "weft_job_stat"

external job_free : nativeint -> unit = "weft_job_free"

external job_detach : nativeint -> unit = "weft_job_detach"

external close_async : int -> unit = "weft_close_async"

external pool_warnings : unit -> int = "weft_pool_warnings"

external pool_busy : unit -> int = "weft_pool_busy"

(* The watchdog (weft_watchdog.c). *)

external watchdog_start :
     (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> int64
  -> nativeint = "weft_watchdog_start"

external watchdog_stop : nativeint -> unit = "weft_watchdog_stop"

external watchdog_reports : unit -> int = "weft_watchdog_reports"

(* Signals. The number of each constructor of [Op.signal], in order. *)
let numbers = signal_numbers ()

let named =
  Op.
    [| SIGHUP
     ; SIGINT
     ; SIGQUIT
     ; SIGILL
     ; SIGABRT
     ; SIGFPE
     ; SIGKILL
     ; SIGSEGV
     ; SIGBUS
     ; SIGPIPE
     ; SIGALRM
     ; SIGTERM
     ; SIGUSR1
     ; SIGUSR2
     ; SIGCHLD
     ; SIGCONT
     ; SIGSTOP
     ; SIGTSTP
     ; SIGWINCH |]

let number_of = function
  | Op.Other_signal n ->
      n
  | s ->
      let rec find i = if named.(i) = s then numbers.(i) else find (i + 1) in
      find 0

let signal_of n =
  let rec find i =
    if i = Array.length numbers then Op.Other_signal n
    else if numbers.(i) = n then named.(i)
    else find (i + 1)
  in
  find 0

(* Errors *)

let codes = errno_codes ()

let eagain, ebadf, econnrefused, econnreset, epipe, eaddrinuse, ewouldblock =
  (codes.(0), codes.(1), codes.(2), codes.(3), codes.(4), codes.(5), codes.(6))

let enoent, eexist, enotdir, eisdir, enotempty =
  (codes.(7), codes.(8), codes.(9), codes.(10), codes.(11))

let emfile, enfile, enobufs, enomem, econnaborted =
  (codes.(12), codes.(13), codes.(14), codes.(15), codes.(16))

let error_of e =
  if
    ( e = eagain || e = ewouldblock )
    [@mutate off "EAGAIN = EWOULDBLOCK on Linux"]
  then Op.Would_block
  else if e = ebadf then Op.Bad_fd
  else if e = econnrefused then Op.Refused
  else if e = econnreset then Op.Reset
  else if e = epipe then Op.Broken_pipe
  else if e = eaddrinuse then Op.In_use
  else if e = enoent then Op.No_entry
  else if e = eexist then Op.Exists
  else if e = enotdir then Op.Not_dir
  else if e = eisdir then Op.Is_dir
  else if e = enotempty then Op.Not_empty
  else if
    e = emfile
    || ( e = enfile || e = enobufs || e = enomem )
       [@mutate off "a test can give only EMFILE"]
  then Op.No_resources (errname e)
  else Op.Other (errname e)

(* [EAGAIN] of an operation that makes a process or a thread: the OS has no
   resource for it. It is not an fd that is not ready. *)
let eagain_resource = function
  | Op.Would_block ->
      Op.No_resources "EAGAIN"
  | e ->
      e

let result r = if r >= 0 then Ok r else Error (error_of (-r))

let unit r = Result.map ignore (result r)

let check name r = if r < 0 then failwith (name ^ ": " ^ strerror (-r)) else r

(* The PRNG of a world: SplitMix64, on 63 bits. Each domain can call [random],
   so the state is one atomic counter. *)
let new_rng () =
  let seed = Bytes.create 8 in
  ignore (check "getrandom" (getrandom seed)) ;
  Atomic.make (Int64.to_int (Bytes.get_int64_le seed 0))

let next rng =
  let z =
    ( Atomic.fetch_and_add rng 0x1E3779B97F4A7C15
    + 0x1E3779B97F4A7C15 )
    [@mutate off "a constant of the PRNG"]
  in
  let z = z lxor (z lsr 30) * 0x3F58476D1CE4E5B9 in
  let z = z lxor (z lsr 27) * 0x14D049BB133111EB in
  z lxor (z lsr 31) land max_int

(* The jobs (weft_pool.c). [jobs] and [files] are under [lock]: each domain can
   start, finish and detach a job. *)
type pool =
  { mbx: nativeint  (** The mailboxes of the domains. *)
  ; lock: Mutex.t
  ; jobs: (int, nativeint) Hashtbl.t  (** The jobs that are not finished. *)
  ; next_job: int Atomic.t
  ; files: (int, unit) Hashtbl.t
        (** The fds of [Open]: the pool does their close(2). *) }

(* [wake_fds] has the fd of each domain that a worker writes after it puts a
   completion in the mailbox of that domain. *)
let create_pool wake_fds =
  { mbx= mbx_create wake_fds
  ; lock= Mutex.create ()
  ; jobs= Hashtbl.create ~random:false 16
  ; next_job= Atomic.make 0
  ; files= Hashtbl.create ~random:false 16 }

(* After [mbx_close], no worker writes a wake fd of the world. *)
let close_pool p = mbx_close p.mbx

(* The completions of the jobs of a domain, before the other events [acc]. A
   worker puts the id in the mailbox before it writes the wake fd, so a take
   after the drain of the wake fd gets each id of this wake. *)
let completions p domain acc =
  Array.fold_right
    (fun id acc -> World.Ready.External id :: acc)
    (mbx_take p.mbx domain) acc

let kind_of : type a. a Op.blocking -> int = function
  | Op.Open _ ->
      0
  | Op.Pread _ ->
      1
  | Op.Pwrite _ ->
      2
  | Op.Fsync _ ->
      3
  | Op.Fstat _ ->
      4
  | Op.Stat _ ->
      5
  | Op.Ftruncate _ ->
      6
  | Op.Mkdir _ ->
      7
  | Op.Readdir _ ->
      8
  | Op.Rename _ ->
      9
  | Op.Unlink _ ->
      10
  | Op.Rmdir _ ->
      11
  | Op.Resolve _ ->
      12
  | Op.Reverse _ ->
      13

let flag_bits flags =
  List.fold_left
    (fun acc f ->
      acc
      lor
      match f with
      | Op.O_read ->
          1
      | Op.O_write ->
          2
      | Op.O_create ->
          4
      | Op.O_exclusive ->
          8
      | Op.O_truncate ->
          16
      | Op.O_append ->
          32 )
    0 flags

let args_of : type a. int -> int -> a Op.blocking -> job_args =
 fun id dom op ->
  let base =
    {id; dom; kind= kind_of op; s1= ""; s2= ""; fd= -1; a= 0; b= -1; data= ""}
  in
  match op with
  | Op.Open (path, flags, perm) ->
      {base with s1= path; a= flag_bits flags; b= perm}
  | Op.Pread (fd, _, _, len, at) ->
      {base with fd; a= len; b= at}
  | Op.Pwrite (fd, data, at) ->
      {base with fd; data; b= at}
  | Op.Fsync fd | Op.Fstat fd ->
      {base with fd}
  | Op.Stat path | Op.Readdir path | Op.Unlink path | Op.Rmdir path ->
      {base with s1= path}
  | Op.Ftruncate (fd, size) ->
      {base with fd; a= size}
  | Op.Mkdir (path, perm) ->
      {base with s1= path; b= perm}
  | Op.Rename (src, dst) ->
      {base with s1= src; s2= dst}
  | Op.Resolve host | Op.Reverse host ->
      {base with s1= host}

let kind_of_int = function
  | 0 ->
      Op.Regular
  | 1 ->
      Op.Directory
  | 2 ->
      Op.Symlink
  | _ ->
      Op.Special

(* The names of [Readdir] or the addresses of [Resolve]: each one ends with a
   NUL byte. [Resolve] can give one address more than one time. *)
let names data =
  List.fold_left
    (fun acc x -> if x = "" || List.mem x acc then acc else x :: acc)
    []
    (String.split_on_char '\000' data)
  |> List.rev

let stat_of j =
  let kind, size, perm, mtime = job_stat j in
  {Op.kind= kind_of_int kind; size; perm; mtime}

let finish : type a. nativeint -> a Op.blocking -> (a, Op.error) result =
 fun j op ->
  let res, err, data = job_result j in
  let r : (a, Op.error) result =
    if err = -1 then Error (Op.Other data)
    else if err <> 0 then Error (eagain_resource (error_of err))
    else
      match op with
      | Op.Open _ ->
          Ok res
      | Op.Pread (_, buf, off, _, _) ->
          Bytes.blit_string data 0 buf off res ;
          Ok res
      | Op.Pwrite _ ->
          Ok res
      | Op.Fsync _ ->
          Ok ()
      | Op.Ftruncate _ ->
          Ok ()
      | Op.Mkdir _ ->
          Ok ()
      | Op.Rename _ ->
          Ok ()
      | Op.Unlink _ ->
          Ok ()
      | Op.Rmdir _ ->
          Ok ()
      | Op.Fstat _ ->
          Ok (stat_of j)
      | Op.Stat _ ->
          Ok (stat_of j)
      | Op.Readdir _ ->
          Ok (names data)
      | Op.Resolve _ ->
          Ok (names data)
      | Op.Reverse _ ->
          Ok data
  in
  job_free j ; r

let take_job p id =
  Mutex.protect p.lock (fun () ->
      let j = Hashtbl.find_opt p.jobs id in
      Hashtbl.remove p.jobs id ; j )

(* Socket options. The code of an option for the stubs: the order of
   [Op.sockopt]. *)
let option_code : type a. a Op.sockopt -> int = function
  | Op.No_delay ->
      0
  | Op.Keep_alive ->
      1
  | Op.Broadcast ->
      2
  | Op.Recv_buffer ->
      3
  | Op.Send_buffer ->
      4

let option_int : type a. a Op.sockopt -> a -> int =
 fun opt v ->
  match opt with
  | Op.No_delay ->
      Bool.to_int v
  | Op.Keep_alive ->
      Bool.to_int v
  | Op.Broadcast ->
      Bool.to_int v
  | Op.Recv_buffer ->
      v
  | Op.Send_buffer ->
      v

let option_value : type a. a Op.sockopt -> int -> a =
 fun opt x ->
  match opt with
  | Op.No_delay ->
      x <> 0
  | Op.Keep_alive ->
      x <> 0
  | Op.Broadcast ->
      x <> 0
  | Op.Recv_buffer ->
      x
  | Op.Send_buffer ->
      x

let host_port = function
  | Op.Tcp (h, p) ->
      (0, h, p)
  | Op.Unix path ->
      (1, path, 0)

(* [spawn_child args defaults] (weft_spawn.c). [defaults] are the signals that
   get the default action in the child. *)
external spawn_child :
     string * string array * string array * bool * string * int array
  -> int array
  -> int * int * int * int * int = "weft_spawn"

(* The arguments of [spawn_child]: (prog, argv, env, has_env, cwd, stdio), with
   a kind for the fds 0, 1 and 2. *)
let spawn_args (sp : Op.spawn) =
  let kind = function Op.Inherit -> 0 | Op.Null -> 1 | Op.Piped -> 2 in
  ( sp.prog
  , Array.of_list sp.argv
  , Array.of_list (Option.value ~default:[] sp.env)
  , Option.is_some sp.env
  , Option.value ~default:"" sp.cwd
  , [|kind sp.stdin; kind sp.stdout; kind sp.stderr|] )

(* The result of [spawn_child]: (pid or -errno, the pidfd, stdin, stdout,
   stderr), with -1 for no pipe. *)
let spawned_of (pid, pidfd, i, o, e) =
  let fd x =
    if (x < 0) [@mutate off "fd 0 is the stdin of the process"] then None
    else Some x
  in
  Result.map
    (fun pid ->
      {Op.pid; pidfd; to_stdin= fd i; from_stdout= fd o; from_stderr= fd e} )
    (Result.map_error eagain_resource (result pid))

(* The result of a wait stub: (kind, code). The kind is 0 while the child runs,
   1 if it exited with the code, 2 if the signal in code ended it, or -errno. *)
let status_of (kind, code) =
  match kind with
  | 0 ->
      Ok None
  | 1 ->
      Ok (Some (Op.Exited code))
  | 2 ->
      Ok (Some (Op.Signaled (signal_of code)))
  | e ->
      Error (error_of (-e))

(* The operations that do not depend on the poller. [None] for the others:
   [Register], [Deregister], [Spawn], [Wait_child], [Kill], [Watch] and
   [Unwatch]. *)
let submit : type a. pool -> a Op.t -> (a, Op.error) result option =
 fun p op ->
  match op with
  | Op.Listen (addr, backlog) ->
      let kind, h, port = host_port addr in
      Some (result (listen kind h port backlog))
  | Op.Accept fd ->
      (* FreeBSD gives ECONNABORTED for a connection that the client reset
         before the accept. Linux gives the connection, and a read of the source
         of macOS shows that macOS also gives it. Thus the world takes the next
         connection, as Go and libuv do. Each ECONNABORTED removes one
         connection from the backlog, so the loop ends. *)
      let rec take () =
        let ((r, _, _, _) as a) = accept fd in
        if r = -econnaborted then take () else a
      in
      let r, host, port, unix = take () in
      Some
        (Result.map
           (fun fd -> (fd, if unix then Op.Unix "" else Op.Tcp (host, port)))
           (result r) )
  | Op.Connect addr ->
      let kind, h, port = host_port addr in
      let r = connect kind h port in
      (* A Unix socket gives EAGAIN when the backlog of the listener is full.
         This is not a connect that continues. *)
      if r = -eagain then Some (Error (Op.Other "EAGAIN: the backlog is full"))
      else Some (result r)
  | Op.Connect_result fd ->
      Some (unit (connect_result fd))
  | Op.Local_addr fd ->
      let r, host, port, unix = local_addr fd in
      Some
        (Result.map
           (fun () -> if unix then Op.Unix host else Op.Tcp (host, port))
           (unit r) )
  | Op.Read (fd, buf, off, len) ->
      Some (result (recv fd buf off len))
  | Op.Write (fd, buf, off, len) ->
      Some (result (send fd buf off len))
  | Op.Shutdown fd ->
      Some (unit (shutdown_send fd))
  | Op.Reset_on_close fd ->
      Some (unit (reset_on_close fd))
  | Op.Udp (host, port) ->
      Some (result (udp host port))
  | Op.Sendto (fd, buf, off, len, host, port) ->
      Some (result (sendto fd buf off len (host, port)))
  | Op.Recvfrom (fd, buf, off, len) ->
      let n, host, port = recvfrom fd buf off len in
      Some (Result.map (fun n -> (n, host, port)) (result n))
  | Op.Set_option (fd, opt, v) ->
      Some (unit (setsockopt fd (option_code opt) (option_int opt v)))
  | Op.Get_option (fd, opt) ->
      Some
        (Result.map (option_value opt)
           (result (getsockopt fd (option_code opt))) )
  | Op.Pipe ->
      let r, w = pipe () in
      Some (Result.map (fun r -> (r, w)) (result r))
  | Op.Fd_read (fd, buf, off, len) ->
      Some (result (fd_read fd buf off len))
  | Op.Fd_write (fd, buf, off, len) ->
      Some (result (fd_write fd buf off len))
  | Op.Close fd ->
      (* close(2) of a file can block: the pool does it (design section 2.7). *)
      if
        Mutex.protect p.lock (fun () ->
            let file = Hashtbl.mem p.files fd in
            Hashtbl.remove p.files fd ; file )
      then Some (Ok (close_async fd))
      else Some (unit (close_fd fd))
  | Op.Start (dom, op) ->
      let id = Atomic.fetch_and_add p.next_job 1 in
      let args = args_of id dom op in
      (* The lock covers the start, so the job is in the table before its
         completion can come. *)
      Some
        (Mutex.protect p.lock (fun () ->
             Hashtbl.replace p.jobs id (job_start p.mbx args) ;
             Ok id ) )
  | Op.Finish (id, op) -> (
    match take_job p id with
    | Some j ->
        let r = finish j op in
        ( match (op, r) with
        | Op.Open _, Ok fd ->
            Mutex.protect p.lock (fun () -> Hashtbl.replace p.files fd ())
        | _ ->
            () ) ;
        Some r
    | None ->
        Some (Error Op.Bad_fd) )
  | Op.Detach id -> (
    match take_job p id with
    | Some j ->
        Some (Ok (job_detach j))
    | None ->
        Some (Error Op.Bad_fd) )
  | Op.Register _
  | Op.Deregister _
  | Op.Spawn _
  | Op.Wait_child _
  | Op.Kill _
  | Op.Watch _
  | Op.Unwatch _ ->
      None
  | Op.Resend s ->
      Some (unit (resend (number_of s)))

let now (type time) (line : time Weft.Clock.line) : time =
  Weft.Clock.of_ns line
    (match line with Mono -> mono_ns () | Posix -> posix_ns ())

let default_domains () = Int.min 4 (Domain.recommended_domain_count ())

(* Run [main] in the world [w], with the watchdog, and [close] the world at the
   end. The watchdog reads the beats of the scheduler. The pair keeps the
   Bigarray alive until the stop. *)
let run (type w) ~name (module W : Weft.WORLD with type t = w) (w : w) ~close
    ?slack ~domains ~watchdog main =
  ( match watchdog with
  | Some l when Int64.compare (Weft.Span.to_ns l) 0L <= 0 ->
      close w ;
      invalid_arg (name ^ ": the limit of the watchdog is not positive")
  | _ ->
      () ) ;
  let dog = ref None in
  let start (s : Sched.t) =
    Option.iter
      (fun limit ->
        dog := Some (watchdog_start s.beats (Weft.Span.to_ns limit), s.beats) )
      watchdog
  in
  Fun.protect
    ~finally:(fun () ->
      Option.iter (fun (d, _) -> watchdog_stop d) !dog ;
      close w )
    (fun () ->
      Weft.Private.run_world ?slack ~domains ~on_create:start (module W) w main )
