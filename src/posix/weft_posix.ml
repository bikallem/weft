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

external accept_retry_codes : unit -> int array = "weft_accept_retry_codes"

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
  ; data: bytes
  ; off: int }

external mbx_create : int array -> nativeint = "weft_mbx_create"

external mbx_close : nativeint -> unit = "weft_mbx_close"

external mbx_take : nativeint -> int -> int array = "weft_mbx_take"

external job_start : nativeint -> job_args -> nativeint = "weft_job_start"

external job_result : nativeint -> int * int = "weft_job_result"

external job_data : nativeint -> string = "weft_job_data"

external job_read : nativeint -> bytes -> int -> int = "weft_job_read"

external job_stat : nativeint -> int * int * int * int64 = "weft_job_stat"

external job_free : nativeint -> unit = "weft_job_free"

external job_detach : nativeint -> unit = "weft_job_detach"

external close_async : int -> unit = "weft_close_async"

external pool_warnings : unit -> int = "weft_pool_warnings"

external pool_busy : unit -> int = "weft_pool_busy"

(* The watchdog (weft_watchdog.c). *)

external watchdog_start :
     (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> int
  -> int64
  -> nativeint = "weft_watchdog_start"

external watchdog_stop : nativeint -> unit = "weft_watchdog_stop"

external watchdog_sleep : nativeint -> unit = "weft_watchdog_sleep" [@@noalloc]

external watchdog_awake : nativeint -> unit = "weft_watchdog_awake" [@@noalloc]

external watchdog_reports : unit -> int = "weft_watchdog_reports"

external watchdog_checks : unit -> int = "weft_watchdog_checks"

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

(* The form of a signal in [Ready.Signal], for example [SIGUSR1] for
   [Other_signal 10] on Linux. *)
let normal s = signal_of (number_of s)

(* Errors *)

let codes = errno_codes ()

let eagain, ebadf, econnrefused, econnreset, epipe, eaddrinuse, ewouldblock =
  (codes.(0), codes.(1), codes.(2), codes.(3), codes.(4), codes.(5), codes.(6))

let enoent, eexist, enotdir, eisdir, enotempty =
  (codes.(7), codes.(8), codes.(9), codes.(10), codes.(11))

let emfile, enfile, enobufs, enomem =
  (codes.(12), codes.(13), codes.(14), codes.(15))

(* The errors of accept(2) that the world retries (weft_posix_stubs.c). *)
let accept_retry = accept_retry_codes ()

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

(* A random int in the range 0 to [n - 1], from 8 bytes of the OS. The value of
   the bytes has 62 bits, so [mod] gives a bias of at most [n / 2^62]. The
   scheduler calls it with [n = 2^30], a divisor of [2^62]: no bias. Each domain
   calls it one time at the start of a run, so the cost of a system call is
   small. *)
let random n =
  let b = Bytes.make 8 '\000' in
  ignore (check "getrandom" (getrandom b)) ;
  let x = Int64.to_int (Bytes.get_int64_le b 0) land max_int in
  x mod n

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
   worker puts the id in the mailbox before it writes the wake fd, and it writes
   the fd only when the mailbox was empty. Thus the world must take after each
   drain of the wake fd: the take gets each id of this wake and of the adds that
   did not write (weft_pool.c, post). *)
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
    { id
    ; dom
    ; kind= kind_of op
    ; s1= ""
    ; s2= ""
    ; fd= -1
    ; a= 0
    ; b= -1
    ; data= Bytes.empty
    ; off= 0 }
  in
  match op with
  | Op.Open (path, flags, perm) ->
      {base with s1= path; a= flag_bits flags; b= perm}
  | Op.Pread (fd, _, _, len, at) ->
      {base with fd; a= len; b= at}
  | Op.Pwrite (fd, data, off, len, at) ->
      {base with fd; data; off; a= len; b= at}
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
   NUL byte. readdir(3) gives each entry of a directory one time. *)
let names data =
  List.filter (fun x -> x <> "") (String.split_on_char '\000' data)

(* [Resolve] can give one address more than one time. The world keeps the first
   one, in the order of getaddrinfo(3). A table keeps the time linear. *)
let addresses data =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun x -> (not (Hashtbl.mem seen x)) && (Hashtbl.replace seen x () ; true))
    (names data)

let stat_of j =
  let kind, size, perm, mtime = job_stat j in
  {Op.kind= kind_of_int kind; size; perm; mtime}

let finish : type a. nativeint -> a Op.blocking -> (a, Op.error) result =
 fun j op ->
  let res, err = job_result j in
  let r : (a, Op.error) result =
    if err = -1 then Error (Op.Other (job_data j))
    else if err <> 0 then Error (eagain_resource (error_of err))
    else
      match op with
      | Op.Open _ ->
          Ok res
      | Op.Pread (_, buf, off, _, _) ->
          Ok (job_read j buf off)
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
          Ok (names (job_data j))
      | Op.Resolve _ ->
          Ok (addresses (job_data j))
      | Op.Reverse _ ->
          Ok (job_data j)
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
  let fd x = if x < 0 then None else Some x in
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

(* The pool operations. The lock covers the start, so the job is in the table
   before its completion can come. *)
let start_job p dom op =
  let id = Atomic.fetch_and_add p.next_job 1 in
  let args = args_of id dom op in
  Mutex.protect p.lock (fun () ->
      Hashtbl.replace p.jobs id (job_start p.mbx args) ;
      Ok id )

let finish_job : type a. pool -> int -> a Op.blocking -> (a, Op.error) result =
 fun p id op ->
  match take_job p id with
  | Some j ->
      let r = finish j op in
      ( match (op, r) with
      | Op.Open _, Ok fd ->
          Mutex.protect p.lock (fun () -> Hashtbl.replace p.files fd ())
      | _ ->
          () ) ;
      r
  | None ->
      Error Op.Bad_fd

let detach_job p id =
  match take_job p id with
  | Some j ->
      Ok (job_detach j)
  | None ->
      Error Op.Bad_fd

(* The close of an fd. close(2) of a file can block: the pool does it (design
   section 2.7). *)
let close_op p fd =
  if
    Mutex.protect p.lock (fun () ->
        let file = Hashtbl.mem p.files fd in
        Hashtbl.remove p.files fd ; file )
  then Ok (close_async fd)
  else unit (close_fd fd)

let now (type time) (line : time Weft.Clock.line) : time =
  Weft.Clock.of_ns line
    (match line with Mono -> mono_ns () | Posix -> posix_ns ())

let default_domains () = Int.min 4 (Domain.recommended_domain_count ())

(* The parts of a world that depend on its poller, its signals and its child
   processes. *)
module type POLLER = sig
  type t

  val name : string
  (** The name of the module of the world, for the errors. *)

  val create : ?domains:int -> ?signals:Op.signal list -> unit -> t

  val close : t -> unit

  val pool : t -> pool

  val watched : t -> Op.signal list Atomic.t
  (** The signals of [run ~signals], with no signal of an unwatch, in their
      [normal] form. *)

  val unwatch_lock : t -> Mutex.t

  val unwatch : t -> Op.signal -> rest:Op.signal list -> (unit, Op.error) result
  (** [unwatch t s ~rest] stops the watch of [s], under [unwatch_lock]. [rest]
      are the other watched signals. After [Ok], [submit] removes [s] from
      [watched]. *)

  val register : t -> int -> Op.fd -> int -> (unit, Op.error) result
  (** [register t domain fd token] *)

  val deregister : t -> int -> Op.fd -> (unit, Op.error) result

  val spawn : t -> Op.spawn -> (Op.spawned, Op.error) result

  val wait_child : t -> Op.fd -> (Op.status option, Op.error) result

  val kill : t -> Op.fd -> Op.signal -> (unit, Op.error) result

  val close_fd : t -> Op.fd -> (unit, Op.error) result
  (** The close of an fd, with [close_op] for each fd that is not of the poller.
  *)

  val wait : t -> domain:int -> World.Wait.t -> World.Ready.t list

  val wake : t -> domain:int -> unit

  val watchdog : t -> nativeint Atomic.t
  (** The watchdog of [run], or [0n]. [Make.World.wait] tells it each sleep. *)
end

(* The world and its [run], from the parts of [P]. *)
module Make (P : POLLER) = struct
  module World = struct
    type t = P.t

    let now _ line = now line

    let submit : type a. t -> a Op.t -> (a, Op.error) result =
     fun t op ->
      match op with
      | Op.Listen (addr, backlog) ->
          let kind, h, port = host_port addr in
          result (listen kind h port backlog)
      | Op.Accept fd ->
          (* FreeBSD gives ECONNABORTED for a connection that the client reset
             before the accept. Linux gives the connection, and a read of the
             source of macOS shows that macOS also gives it. Linux can give a
             pending network error of the new connection, for example EPROTO.
             For these errors, the world takes the next connection (design
             section 2.7, An aborted connection). Each one removes one
             connection from the backlog, so the loop ends. *)
          let rec take () =
            let ((r, _, _, _) as a) = accept fd in
            if Array.mem (-r) accept_retry then take () else a
          in
          let r, host, port, unix = take () in
          Result.map
            (fun fd -> (fd, if unix then Op.Unix "" else Op.Tcp (host, port)))
            (result r)
      | Op.Connect addr ->
          let kind, h, port = host_port addr in
          let r = connect kind h port in
          (* A Unix socket gives EAGAIN when the backlog of the listener is
             full. This is not a connect that continues. *)
          if r = -eagain then Error (Op.Other "EAGAIN: the backlog is full")
          else result r
      | Op.Connect_result fd ->
          unit (connect_result fd)
      | Op.Local_addr fd ->
          let r, host, port, unix = local_addr fd in
          Result.map
            (fun () -> if unix then Op.Unix host else Op.Tcp (host, port))
            (unit r)
      | Op.Read (fd, buf, off, len) ->
          result (recv fd buf off len)
      | Op.Write (fd, buf, off, len) ->
          result (send fd buf off len)
      | Op.Shutdown fd ->
          unit (shutdown_send fd)
      | Op.Reset_on_close fd ->
          unit (reset_on_close fd)
      | Op.Udp (host, port) ->
          result (udp host port)
      | Op.Sendto (fd, buf, off, len, host, port) ->
          result (sendto fd buf off len (host, port))
      | Op.Recvfrom (fd, buf, off, len) ->
          let n, host, port = recvfrom fd buf off len in
          Result.map (fun n -> (n, host, port)) (result n)
      | Op.Set_option (fd, opt, v) ->
          unit (setsockopt fd (option_code opt) (option_int opt v))
      | Op.Get_option (fd, opt) ->
          Result.map (option_value opt)
            (result (getsockopt fd (option_code opt)))
      | Op.Pipe ->
          let r, w = pipe () in
          Result.map (fun r -> (r, w)) (result r)
      | Op.Fd_read (fd, buf, off, len) ->
          result (fd_read fd buf off len)
      | Op.Fd_write (fd, buf, off, len) ->
          result (fd_write fd buf off len)
      | Op.Close fd ->
          P.close_fd t fd
      | Op.Register (d, fd, token) ->
          P.register t d fd token
      | Op.Deregister (d, fd) ->
          P.deregister t d fd
      | Op.Start (dom, op) ->
          start_job (P.pool t) dom op
      | Op.Finish (id, op) ->
          finish_job (P.pool t) id op
      | Op.Detach id ->
          detach_job (P.pool t) id
      | Op.Spawn sp ->
          P.spawn t sp
      | Op.Wait_child fd ->
          P.wait_child t fd
      | Op.Kill (fd, s) ->
          P.kill t fd s
      | Op.Watch s ->
          let s = normal s in
          Ok (if List.mem s (Atomic.get (P.watched t)) then Some s else None)
      | Op.Unwatch s ->
          let s = normal s in
          Mutex.protect (P.unwatch_lock t) (fun () ->
              let watched = Atomic.get (P.watched t) in
              if not (List.mem s watched) then Ok None
              else
                let rest = List.filter (fun x -> x <> s) watched in
                Result.map
                  (fun () ->
                    Atomic.set (P.watched t) rest ;
                    Some s )
                  (P.unwatch t s ~rest) )
      | Op.Resend s ->
          unit (resend (number_of s))

    (* A sleep with no timeout tells the watchdog, so that it can park while all
       domains sleep (weft_watchdog.c). A poll does not count. The wait reads
       the field one time, so a sleep and its wake tell one watchdog. [run] sets
       the field before the domains start, and it stops the watchdog after the
       domains end. *)
    let wait t ~domain (w : World.Wait.t) =
      match w.timeout with
      | Poll ->
          P.wait t ~domain w
      | Sleep ->
          let dog = Atomic.get (P.watchdog t) in
          if dog = 0n then P.wait t ~domain w
          else (
            watchdog_sleep dog ;
            Fun.protect
              ~finally:(fun () -> watchdog_awake dog)
              (fun () -> P.wait t ~domain w) )

    let wake = P.wake

    let random _ n = random n

    let random_bytes _ b = ignore (getrandom b)
  end

  (* The default number of domains is a start value (open question 6). *)
  let default_domains = default_domains

  let watchdog_reports = watchdog_reports

  let watchdog_checks = watchdog_checks

  let pool_warnings = pool_warnings

  let pool_busy = pool_busy

  (* Run [main] in a new world, with the watchdog, and close the world at the
     end. The check of the limit comes before [create], so a bad limit makes no
     world. The watchdog reads the beats of the scheduler. The pair keeps the
     Bigarray alive until the stop. *)
  let run ?slack ?domains ?signals ?(watchdog = Some (Weft.Span.ms 10)) main =
    ( match watchdog with
    | Some l when Int64.compare (Weft.Span.to_ns l) 0L <= 0 ->
        invalid_arg (P.name ^ ".run: the limit of the watchdog is not positive")
    | _ ->
        () ) ;
    let domains =
      match domains with Some n -> n | None -> default_domains ()
    in
    let w = P.create ~domains ?signals () in
    let dog = ref None in
    let start (s : Sched.t) =
      Option.iter
        (fun limit ->
          let d =
            watchdog_start s.beats Sched.beat_stride (Weft.Span.to_ns limit)
          in
          dog := Some (d, s.beats) ;
          Atomic.set (P.watchdog w) d )
        watchdog
    in
    Fun.protect
      ~finally:(fun () ->
        Option.iter (fun (d, _) -> watchdog_stop d) !dog ;
        P.close w )
      (fun () ->
        Weft.Private.run_world ?slack ~domains ~on_create:start
          (module World)
          w main )
end
