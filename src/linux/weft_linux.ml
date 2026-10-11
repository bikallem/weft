open Weft.Private

external mono_ns : unit -> int64 = "weft_mono_ns"

external posix_ns : unit -> int64 = "weft_posix_ns"

external epoll_create : unit -> int = "weft_epoll_create"

external eventfd : unit -> int = "weft_eventfd"

external timerfd : unit -> int = "weft_timerfd"

external timerfd_set : int -> int64 -> int = "weft_timerfd_set"

external drain : int -> int = "weft_drain"

external eventfd_signal : int -> int = "weft_eventfd_signal"

external epoll_add : int -> int -> int -> int = "weft_epoll_add"

external epoll_add_in : int -> int -> int -> int = "weft_epoll_add_in"

external epoll_del : int -> int -> int = "weft_epoll_del"

external epoll_wait : int -> bool -> (int * bool * bool) array
  = "weft_epoll_wait"

external listen : int -> string -> int -> int -> int = "weft_listen"

external local_port_of : int -> int = "weft_local_port"

external connect : int -> string -> int -> int = "weft_connect"

external connect_result : int -> int = "weft_connect_result"

external accept : int -> int * string * int * bool = "weft_accept"

external recv : int -> bytes -> int -> int -> int = "weft_recv"

external send : int -> bytes -> int -> int -> int = "weft_send"

external shutdown_send : int -> int = "weft_shutdown_send"

external pipe : unit -> int * int = "weft_pipe"

external fd_read : int -> bytes -> int -> int -> int = "weft_read"

external fd_write : int -> bytes -> int -> int -> int = "weft_write"

external close_fd : int -> int = "weft_close"

external getrandom : bytes -> int = "weft_getrandom"

external errno_codes : unit -> int array = "weft_errno_codes"

external strerror : int -> string = "weft_strerror"

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

(* Child processes and signals (weft_proc.c). *)

external signal_numbers : unit -> int array = "weft_signal_numbers"

external spawn :
     string * string array * string array * bool * string * int array
  -> int * int * int * int * int = "weft_spawn"

external wait_child : int -> int * int = "weft_wait_child"

external pidfd_kill : int -> int -> int = "weft_pidfd_kill"

external sigmask_block : int array -> bytes = "weft_sigmask_block"

external sigmask_restore : bytes -> unit = "weft_sigmask_restore"

external signalfd : int array -> int = "weft_signalfd"

external signalfd_read : int -> int array = "weft_signalfd_read"

(* The number of each constructor of [Op.signal], in order. *)
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

let codes = errno_codes ()

let eagain, ebadf, econnrefused, econnreset, epipe, eaddrinuse, ewouldblock =
  (codes.(0), codes.(1), codes.(2), codes.(3), codes.(4), codes.(5), codes.(6))

let enoent, eexist, enotdir, eisdir, enotempty =
  (codes.(7), codes.(8), codes.(9), codes.(10), codes.(11))

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
  else Op.Other (strerror e)

let result r = if r >= 0 then Ok r else Error (error_of (-r))

let unit r = Result.map ignore (result r)

(* The tokens of the alarm and of the wake fd. A slot of an fd is never so
   large. *)
let timer_token = -1

let wake_token = -2

let signal_token = -3

type dom =
  { epfd: int
  ; evfd: int
  ; tfd: int
  ; mutable armed: int64  (** The time of the alarm, or 0. *) }

(* [rng] is the state of SplitMix64. Each domain can call [random], so the state
   is one atomic counter. [jobs] and [files] are under [lock]: each domain can
   start, finish and detach a job. *)
type t =
  { doms: dom array
  ; rng: int Atomic.t
  ; mbx: nativeint  (** The mailboxes of the domains. *)
  ; lock: Mutex.t
  ; jobs: (int, nativeint) Hashtbl.t  (** The jobs that are not finished. *)
  ; next_job: int Atomic.t
  ; files: (int, unit) Hashtbl.t
        (** The fds of [Open]: the pool does their close(2). *)
  ; watched: Op.signal list  (** The signals of [run ~signals]. *)
  ; sigfd: int  (** The signalfd in the poller of domain 0, or [-1]. *)
  ; old_mask: bytes option  (** The signal mask before [create]. *) }

let check name r = if r < 0 then failwith (name ^ ": " ^ strerror (-r)) else r

let make_dom () =
  let epfd = check "epoll_create1" (epoll_create ()) in
  let evfd = check "eventfd" (eventfd ()) in
  let tfd = check "timerfd_create" (timerfd ()) in
  ignore (check "epoll_ctl" (epoll_add_in epfd evfd wake_token)) ;
  ignore (check "epoll_ctl" (epoll_add_in epfd tfd timer_token)) ;
  {epfd; evfd; tfd; armed= 0L}

(* The threads that do not block each signal of [sigs], from the [SigBlk] line
   of /proc/self/task/*/status (design section 2.7, Not sure). *)
let unblocked_numbers sigs =
  let blocks dir =
    match
      In_channel.with_open_text (dir ^ "/status") In_channel.input_lines
    with
    | lines -> (
      match
        List.find_map
          (fun l ->
            if String.starts_with ~prefix:"SigBlk:" l then
              Int64.of_string_opt
                ("0x" ^ String.trim (String.sub l 7 (String.length l - 7)))
            else None )
          lines
      with
      | Some mask ->
          List.for_all
            (fun n -> Int64.logand mask (Int64.shift_left 1L (n - 1)) <> 0L)
            sigs
      | None ->
          true )
    | exception Sys_error _ ->
        (* The thread ended. *)
        true
  in
  match Sys.readdir "/proc/self/task" with
  | tasks ->
      List.filter
        (fun task -> not (blocks ("/proc/self/task/" ^ task)))
        (Array.to_list tasks)
  | exception Sys_error _ ->
      []

let unblocked_threads sigs = unblocked_numbers (List.map number_of sigs)

(* A write to a pipe with no reader then gives EPIPE (design section 2.7,
   Signals). The signals of [~signals] are blocked in the calling thread before
   any other thread of the world starts, so each thread copies the mask. *)
let create ?(domains = 1) ?(signals = []) () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
  let nums = Array.of_list (List.map number_of signals) in
  let old_mask, sigfd =
    if signals = [] then (None, -1)
    else
      let old = sigmask_block nums in
      let fd = signalfd nums in
      if (fd < 0) [@mutate off "fd 0 is the stdin of the process"] then (
        sigmask_restore old ;
        failwith ("signalfd: " ^ strerror (-fd)) ) ;
      List.iter
        (fun task ->
          Printf.eprintf
            "weft: thread %s does not block the signals of run ~signals: a \
             signal can go to it\n\
             %!"
            task )
        (unblocked_numbers (Array.to_list nums)) ;
      (Some old, fd)
  in
  let seed = Bytes.create 8 in
  ignore (check "getrandom" (getrandom seed)) ;
  let doms = Array.init domains (fun _ -> make_dom ()) in
  if sigfd >= 0 then
    ignore (check "epoll_ctl" (epoll_add_in doms.(0).epfd sigfd signal_token)) ;
  { doms
  ; rng= Atomic.make (Int64.to_int (Bytes.get_int64_le seed 0))
  ; mbx= mbx_create (Array.map (fun d -> d.evfd) doms)
  ; lock= Mutex.create ()
  ; jobs= Hashtbl.create ~random:false 16
  ; next_job= Atomic.make 0
  ; files= Hashtbl.create ~random:false 16
  ; watched= List.map (fun s -> signal_of (number_of s)) signals
  ; sigfd
  ; old_mask }

(* SplitMix64, on 63 bits. *)
let next t =
  let z =
    ( Atomic.fetch_and_add t.rng 0x1E3779B97F4A7C15
    + 0x1E3779B97F4A7C15 )
    [@mutate off "a constant of the PRNG"]
  in
  let z = z lxor (z lsr 30) * 0x3F58476D1CE4E5B9 in
  let z = z lxor (z lsr 27) * 0x14D049BB133111EB in
  z lxor (z lsr 31) land max_int

(* After [mbx_close], no worker writes an eventfd of the world. *)
let close t =
  mbx_close t.mbx ;
  if t.sigfd >= 0 then ignore (close_fd t.sigfd) ;
  Option.iter sigmask_restore t.old_mask ;
  Array.iter
    (fun d ->
      ignore (close_fd d.tfd) ;
      ignore (close_fd d.evfd) ;
      ignore (close_fd d.epfd) )
    t.doms

(* The jobs (weft_pool.c). *)

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

(* The C side reads each string up to its first NUL byte. *)
let has_nul = List.exists (fun s -> String.contains s '\000')

let nul_error = Error (Op.Other "EINVAL: a NUL byte in a string")

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
    else if err <> 0 then Error (error_of err)
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

let take_job t id =
  Mutex.protect t.lock (fun () ->
      let j = Hashtbl.find_opt t.jobs id in
      Hashtbl.remove t.jobs id ; j )

(* The calls of timerfd_settime, for the measurements. *)
let alarm_sets = Atomic.make 0

module World = struct
  type nonrec t = t

  let now (type time) _ (line : time Weft.Clock.line) : time =
    Weft.Clock.of_ns line
      (match line with Mono -> mono_ns () | Posix -> posix_ns ())

  let host_port = function
    | Op.Tcp (h, p) ->
        (0, h, p)
    | Op.Unix path ->
        (1, path, 0)

  let submit : type a. t -> a Op.t -> (a, Op.error) result =
   fun t op ->
    match op with
    | Op.Listen (addr, backlog) ->
        let kind, h, p = host_port addr in
        result (listen kind h p backlog)
    | Op.Accept fd ->
        let r, host, port, unix = accept fd in
        Result.map
          (fun fd -> (fd, if unix then Op.Unix "" else Op.Tcp (host, port)))
          (result r)
    | Op.Connect addr ->
        let kind, h, p = host_port addr in
        let r = connect kind h p in
        (* A Unix socket gives EAGAIN when the backlog of the listener is full.
           This is not a connect that continues. *)
        if r = -eagain then Error (Op.Other "EAGAIN: the backlog is full")
        else result r
    | Op.Connect_result fd ->
        unit (connect_result fd)
    | Op.Read (fd, buf, off, len) ->
        result (recv fd buf off len)
    | Op.Write (fd, buf, off, len) ->
        result (send fd buf off len)
    | Op.Shutdown fd ->
        unit (shutdown_send fd)
    | Op.Pipe ->
        let r, w = pipe () in
        Result.map (fun r -> (r, w)) (result r)
    | Op.Fd_read (fd, buf, off, len) ->
        result (fd_read fd buf off len)
    | Op.Fd_write (fd, buf, off, len) ->
        result (fd_write fd buf off len)
    | Op.Close fd ->
        (* close(2) of a file can block: the pool does it (design section
           2.7). *)
        if
          Mutex.protect t.lock (fun () ->
              let file = Hashtbl.mem t.files fd in
              Hashtbl.remove t.files fd ; file )
        then Ok (close_async fd)
        else unit (close_fd fd)
    | Op.Start (dom, op) ->
        let id = Atomic.fetch_and_add t.next_job 1 in
        let args = args_of id dom op in
        if has_nul [args.s1; args.s2] then nul_error
        else
          (* The lock covers the start, so the job is in the table before its
             completion can come. *)
          Mutex.protect t.lock (fun () ->
              Hashtbl.replace t.jobs id (job_start t.mbx args) ;
              Ok id )
    | Op.Finish (id, op) -> (
      match take_job t id with
      | Some j ->
          let r = finish j op in
          ( match (op, r) with
          | Op.Open _, Ok fd ->
              Mutex.protect t.lock (fun () -> Hashtbl.replace t.files fd ())
          | _ ->
              () ) ;
          r
      | None ->
          Error Op.Bad_fd )
    | Op.Spawn sp ->
        if
          has_nul
            ( (sp.prog :: sp.argv)
            @ Option.value ~default:[] sp.env
            @ Option.to_list sp.cwd )
        then nul_error
        else
          let kind = function
            | Op.Inherit ->
                0
            | Op.Null ->
                1
            | Op.Piped ->
                2
          in
          let pid, pidfd, i, o, e =
            spawn
              ( sp.prog
              , Array.of_list sp.argv
              , Array.of_list (Option.value ~default:[] sp.env)
              , Option.is_some sp.env
              , Option.value ~default:"" sp.cwd
              , [|kind sp.stdin; kind sp.stdout; kind sp.stderr|] )
          in
          let fd x =
            if (x < 0) [@mutate off "fd 0 is the stdin of the process"] then
              None
            else Some x
          in
          Result.map
            (fun pid ->
              { Op.pid
              ; pidfd
              ; to_stdin= fd i
              ; from_stdout= fd o
              ; from_stderr= fd e } )
            (result pid)
    | Op.Wait_child fd -> (
        let kind, code = wait_child fd in
        match kind with
        | 0 ->
            Ok None
        | 1 ->
            Ok (Some (Op.Exited code))
        | 2 ->
            Ok (Some (Op.Signaled (signal_of code)))
        | e ->
            Error (error_of (-e)) )
    | Op.Kill (fd, s) ->
        unit (pidfd_kill fd (number_of s))
    | Op.Watch s ->
        let s = signal_of (number_of s) in
        if List.mem s t.watched then Ok s
        else Error (Op.Other "the world does not watch this signal")
    | Op.Detach id -> (
      match take_job t id with
      | Some j ->
          Ok (job_detach j)
      | None ->
          Error Op.Bad_fd )
    | Op.Register (d, fd, token) ->
        unit (epoll_add t.doms.(d).epfd fd token)
    | Op.Deregister (d, fd) ->
        unit (epoll_del t.doms.(d).epfd fd)

  (* Set the alarm only when the earliest deadline changed. *)
  let arm d (deadline : Weft.Clock.mono option) =
    let target =
      match deadline with
      | None ->
          0L
      | Some m ->
          Int64.max 1L (Weft.Clock.to_ns Mono m)
    in
    if not (Int64.equal target d.armed) then (
      Atomic.incr alarm_sets ;
      ignore (timerfd_set d.tfd target) ;
      d.armed <- target )

  let wait t ~domain (w : World.Wait.t) =
    let d = t.doms.(domain) in
    arm d w.deadline ;
    let events = epoll_wait d.epfd (w.timeout = World.Wait.Sleep) in
    Array.fold_right
      (fun (token, read, write) acc ->
        if token = timer_token then (
          ignore (drain d.tfd) ;
          (* A timerfd that fired is not armed. *)
          d.armed <- 0L ;
          World.Ready.Alarm :: acc )
        else if token = signal_token then (
          let counts = signalfd_read t.sigfd in
          let acc = ref acc in
          for n = Array.length counts - 1 downto 0 do
            if (counts.(n) > 0) [@mutate off "a count of 0 commits no waiter"]
            then acc := World.Ready.Signal (signal_of n, counts.(n)) :: !acc
          done ;
          !acc )
        else if token = wake_token then (
          ignore (drain d.evfd) ;
          (* A worker puts the id in the mailbox before it writes the eventfd,
             so the take after the drain gets each id of this wake. *)
          World.Ready.Woken
          :: Array.fold_right
               (fun id acc -> World.Ready.External id :: acc)
               (mbx_take t.mbx domain) acc )
        else World.Ready.Fd {token; read; write} :: acc )
      events []

  let wake t ~domain = ignore (eventfd_signal t.doms.(domain).evfd)

  let random t n = next t mod n

  let random_bytes _ b = ignore (getrandom b)
end

(* The default number of domains is a start value (open question 6). *)
let default_domains () = Int.min 4 (Domain.recommended_domain_count ())

external watchdog_start :
     (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
  -> int64
  -> nativeint = "weft_watchdog_start"

external watchdog_stop : nativeint -> unit = "weft_watchdog_stop"

external watchdog_reports : unit -> int = "weft_watchdog_reports"

(* The watchdog reads the beats of the scheduler. The pair keeps the Bigarray
   alive until the stop. *)
let run ?slack ?domains ?signals ?(watchdog = Some (Weft.Span.ms 10)) main =
  let domains = match domains with Some n -> n | None -> default_domains () in
  let t = create ~domains ?signals () in
  let dog = ref None in
  let start (s : Sched.t) =
    Option.iter
      (fun limit ->
        dog := Some (watchdog_start s.beats (Weft.Span.to_ns limit), s.beats) )
      watchdog
  in
  Fun.protect
    ~finally:(fun () ->
      Option.iter (fun (w, _) -> watchdog_stop w) !dog ;
      close t )
    (fun () ->
      Weft.Private.run_world ?slack ~domains ~on_create:start
        (module World)
        t main )

let local_port (fd : Weft.Net.t) =
  Net.acquire fd "Weft_linux.local_port" ;
  let p = local_port_of fd.Sched.os in
  Net.release fd ;
  if p < 0 then raise (Net.Error (error_of (-p), "Weft_linux.local_port")) ;
  p

let alarm_sets () = Atomic.get alarm_sets
