open Weft.Private

(* SplitMix64: a fixed PRNG, so that a seed gives the same run with each version
   of OCaml (design section 2.8). *)
module Prng = struct
  type t = {mutable s: int64}

  let make seed = {s= Int64.of_int seed}

  let next t =
    t.s <- Int64.add t.s 0x9E3779B97F4A7C15L ;
    let z = t.s in
    let z =
      Int64.mul
        (Int64.logxor z (Int64.shift_right_logical z 30))
        0xBF58476D1CE4E5B9L
    in
    let z =
      Int64.mul
        (Int64.logxor z (Int64.shift_right_logical z 27))
        0x94D049BB133111EBL
    in
    Int64.logxor z (Int64.shift_right_logical z 31)

  (* A value in the range 0 to [n - 1], with no bias. *)
  let int t n =
    if
      (n <= 0) [@mutate off "each caller asks for n >= 1; Int64.rem fails at 0"]
    then invalid_arg "random" ;
    let n64 = Int64.of_int n in
    let rec draw () =
      let x = Int64.shift_right_logical (next t) 2 in
      let r = Int64.rem x n64 in
      if
        ( Int64.compare (Int64.sub x r) (Int64.sub (Int64.shift_left 1L 62) n64)
        > 0 )
        [@mutate off "the two limits differ in one draw of 2^62"]
      then draw ()
      else Int64.to_int r
    in
    draw ()

  let bytes t b =
    for i = 0 to Bytes.length b - 1 do
      Bytes.set b i (Char.chr (Int64.to_int (Int64.logand (next t) 0xffL)))
    done
end

exception Deadlock of string list

exception Lost_wakeup of string list

exception Model_error of string

(* The in-memory network (design section 2.8). *)

(* The capacity of the receive buffer of a connection. *)
let capacity = 65536

(* A socket, or one end of a pipe. *)
type role = Sock | Rd_end | Wr_end

type conn =
  { role: role
  ; inbox: Buffer.t  (** The bytes to read, from [rpos]. *)
  ; mutable rpos: int
  ; mutable inflight: int  (** Bytes that the latency still holds. *)
  ; mutable peer: conn option  (** [None] after the peer closed. *)
  ; mutable eof: bool  (** The peer ended its send side. *)
  ; mutable shut: bool  (** This side ended its send side. *)
  ; mutable established: bool
  ; mutable refused: bool
  ; mutable cfd: int  (** [-1] until the server side is accepted. *)
  ; remote: Op.addr }

(* [pending] counts the connections that the latency still holds, so that they
   count against the limit of the backlog. A full backlog refuses the connect at
   once. On Linux, a TCP connect retries the SYN instead, and a Unix connect
   gives EAGAIN. *)
type listener =
  {key: string; backlog: conn Queue.t; limit: int; mutable pending: int}

(* A fake program: the test scripts it (design section 2.8). *)
type program =
  { runs: int64 option  (** The time to the exit, or [None]: until a signal. *)
  ; code: int
  ; output: string  (** The data on its stdout. *)
  ; ignores: Op.signal list }

(* A fake process. The parent holds its pidfd. *)
type proc =
  { prog: program
  ; mutable status: Op.status option
  ; mutable reaped: bool
  ; mutable child_ends: conn list  (** The ends of its pipes. *) }

type sfd = Lsn of listener | Conn of conn | Fil of Sim_fs.handle | Pfd of proc

type net =
  { fds: (int, sfd) Hashtbl.t
  ; mutable next_fd: int
  ; names: (string, int) Hashtbl.t  (** The listeners, by address. *)
  ; regs: (int, (int * int) list) Hashtbl.t
        (** The registrations of each fd: the domain and the token. *)
  ; mutable edges: (int * World.Ready.t) list
        (** The edges to give, by domain, the newest first. *)
  ; mutable latency: int64
  ; mutable next_port: int }

(* The result of a job, before [Finish] gives it with its type. *)
type raw =
  | R_unit
  | R_int of int
  | R_read of string
  | R_stat of Op.stat
  | R_names of string list
  | R_name of string

type job =
  { jfd: int option  (** The fd that the job uses. *)
  ; mutable result: (raw, Op.error) result option
  ; mutable detached: bool
  ; on_drop: raw -> unit
        (** The cleanup of a result that nobody gets: the close of the fd that
            the job opened. *) }

(* The answer of the name table for a host name. *)
type dns_answer = Addresses of string list | No_answer

type t =
  { prng: Prng.t
  ; auto: bool
  ; mutable now: int64  (** The virtual [Mono] time, in nanoseconds. *)
  ; posix0: int64  (** [Posix] at [Mono] 0. *)
  ; woken: bool array  (** A [wake] of each domain that it did not take. *)
  ; mutable sched: Sched.t option
  ; net: net
  ; mutable agenda: (int64 * (unit -> unit)) list
        (** The timed events of the simulator, in order. An event comes after
            the events with the same time that came before it. *)
  ; mutable next_id: int
  ; mutable settles: int list
  ; mutable advance: (int64 * int) option
  ; fs: Sim_fs.t
  ; jobs: (int, job) Hashtbl.t
  ; mutable blocking_cost: int64
        (** The limit of the time cost of a job (open question 2). *)
  ; dns: (string, dns_answer) Hashtbl.t
  ; programs: (string, string list -> program) Hashtbl.t
  ; mutable next_pid: int }

let sched t = Option.get t.sched

let open_fds t = Hashtbl.length t.net.fds

let key addr = Format.asprintf "%a" Op.pp_addr addr

let avail c = Buffer.length c.inbox - c.rpos

let readable n fd =
  match Hashtbl.find_opt n.fds fd with
  | Some (Lsn l) ->
      not (Queue.is_empty l.backlog)
  | Some (Conn c) ->
      avail c > 0 || c.eof || c.refused
  | Some (Pfd p) ->
      Option.is_some p.status
  | Some (Fil _) | None ->
      false

let writable n fd =
  match Hashtbl.find_opt n.fds fd with
  | Some (Conn {role= Rd_end; _}) ->
      false
  | Some (Conn c) -> (
      c.refused
      ||
      match c.peer with
      | None ->
          true
      | Some p ->
          c.established
          && ( avail p + p.inflight
             < capacity )
             [@mutate
               off
                 "a wrong result here gives a spurious edge: the write gets \
                  EAGAIN and waits again"] )
  | Some (Lsn _ | Fil _ | Pfd _) | None ->
      false

(* An edge for each registration of [fd]. *)
let edge n fd ~read ~write =
  List.iter
    (fun (d, token) ->
      n.edges <- (d, World.Ready.Fd {token; read; write}) :: n.edges )
    (Option.value ~default:[] (Hashtbl.find_opt n.regs fd))

let schedule t delay f =
  let at = Int64.add t.now delay in
  let e = (at, f) in
  let rec insert = function
    | [] ->
        [e]
    | ((a, _) as x) :: rest ->
        if Int64.compare at a < 0 then e :: x :: rest else x :: insert rest
  in
  t.agenda <- insert t.agenda

(* Run the events of the agenda that are due. *)
let run_agenda t =
  let rec loop () =
    match t.agenda with
    | (at, f) :: rest when Int64.compare at t.now <= 0 ->
        t.agenda <- rest ;
        f () ;
        loop ()
    | _ ->
        ()
  in
  loop ()

(* After the latency, or at once. *)
let later t f =
  if Int64.equal t.net.latency 0L then f () else schedule t t.net.latency f

let new_fd n =
  let fd = n.next_fd in
  n.next_fd <- (fd + 1) [@mutate off "a counter that goes down is unique too"] ;
  fd

let model_error fmt = Printf.ksprintf (fun s -> raise (Model_error s)) fmt

let fresh t =
  let id = t.next_id in
  t.next_id <- (id + 1) [@mutate off "a counter that goes down is unique too"] ;
  id

(* The jobs of the pool (design section 2.8, The wait steps): the operation
   occurs at the completion. *)

let handle_of n fd =
  match Hashtbl.find_opt n.fds fd with
  | Some (Fil h) ->
      Ok h
  | Some (Lsn _ | Conn _ | Pfd _) ->
      Error (Op.Other "ESPIPE")
  | None ->
      Error Op.Bad_fd

(* The result of a job, or [None] if no answer comes. *)
let perform : type a. t -> a Op.blocking -> (raw, Op.error) result option =
 fun t op ->
  let n = t.net and fs = t.fs and now = Int64.add t.posix0 t.now in
  let ( let* ) = Result.bind in
  let unit r = Some (Result.map (fun () -> R_unit) r) in
  match op with
  | Op.Open (path, flags, perm) ->
      Some
        (let* h = Sim_fs.open_ fs path flags perm ~now in
         let fd = new_fd n in
         Hashtbl.replace n.fds fd (Fil h) ;
         Ok (R_int fd) )
  | Op.Pread (fd, _, _, len, at) ->
      Some
        (let* h = handle_of n fd in
         Result.map (fun s -> R_read s) (Sim_fs.pread h len at) )
  | Op.Pwrite (fd, data, at) ->
      Some
        (let* h = handle_of n fd in
         Result.map (fun k -> R_int k) (Sim_fs.pwrite h data at ~now) )
  | Op.Fsync fd ->
      Some (Result.map (fun _ -> R_unit) (handle_of n fd))
  | Op.Fstat fd ->
      Some
        (Result.map
           (fun h -> R_stat (Sim_fs.stat_of h.Sim_fs.node))
           (handle_of n fd) )
  | Op.Stat path ->
      Some (Result.map (fun s -> R_stat s) (Sim_fs.stat fs path))
  | Op.Ftruncate (fd, size) ->
      unit
        (let* h = handle_of n fd in
         Sim_fs.ftruncate h size ~now )
  | Op.Mkdir (path, perm) ->
      unit (Sim_fs.mkdir fs path perm ~now)
  | Op.Readdir path ->
      Some (Result.map (fun l -> R_names l) (Sim_fs.readdir fs path))
  | Op.Rename (src, dst) ->
      unit (Sim_fs.rename fs src dst)
  | Op.Unlink path ->
      unit (Sim_fs.unlink fs path)
  | Op.Rmdir path ->
      unit (Sim_fs.rmdir fs path)
  | Op.Resolve host -> (
    match Hashtbl.find_opt t.dns host with
    | Some (Addresses (_ :: _ as l)) ->
        Some (Ok (R_names l))
    | Some (Addresses []) | None ->
        Some (Error Op.No_entry)
    | Some No_answer ->
        None )
  | Op.Reverse addr -> (
      let names =
        Hashtbl.fold
          (fun name a acc ->
            match a with
            | Addresses l when List.mem addr l ->
                name :: acc
            | _ ->
                acc )
          t.dns []
      in
      match List.sort String.compare names with
      | name :: _ ->
          Some (Ok (R_name name))
      | [] ->
          Some (Error Op.No_entry) )

(* The result of a job with the type of its operation. *)
let finish : type a. a Op.blocking -> raw -> a =
 fun op raw ->
  let bad () =
    model_error "Finish with an operation that is not the operation of Start"
  in
  match (op, raw) with
  | Op.Open _, R_int fd ->
      fd
  | Op.Pread (_, buf, off, _, _), R_read s ->
      Bytes.blit_string s 0 buf off (String.length s) ;
      String.length s
  | Op.Pwrite _, R_int k ->
      k
  | Op.Fsync _, R_unit ->
      ()
  | Op.Fstat _, R_stat s ->
      s
  | Op.Stat _, R_stat s ->
      s
  | Op.Ftruncate _, R_unit ->
      ()
  | Op.Mkdir _, R_unit ->
      ()
  | Op.Readdir _, R_names l ->
      l
  | Op.Rename _, R_unit ->
      ()
  | Op.Unlink _, R_unit ->
      ()
  | Op.Rmdir _, R_unit ->
      ()
  | Op.Resolve _, R_names l ->
      l
  | Op.Reverse _, R_name s ->
      s
  | _ ->
      bad ()

(* A job completes after a time cost from the seed, in the range 0 to the limit,
   or at once with no limit (open question 2). Its completion goes to the domain
   that started it, also after a detach. *)
let start_job : type a. t -> int -> a Op.blocking -> int =
 fun t d op ->
  let id = fresh t in
  let on_drop = function
    | R_int fd -> (
      match op with Op.Open _ -> Hashtbl.remove t.net.fds fd | _ -> () )
    | _ ->
        ()
  in
  let jfd =
    match op with
    | Op.Pread (fd, _, _, _, _)
    | Op.Pwrite (fd, _, _)
    | Op.Fsync fd
    | Op.Fstat fd
    | Op.Ftruncate (fd, _) ->
        Some fd
    | _ ->
        None
  in
  let j = {jfd; result= None; detached= false; on_drop} in
  Hashtbl.replace t.jobs id j ;
  let complete () =
    match perform t op with
    | None ->
        ()
    | Some r ->
        if j.detached then (Result.iter on_drop r ; Hashtbl.remove t.jobs id)
        else j.result <- Some r ;
        t.net.edges <- (d, World.Ready.External id) :: t.net.edges
  in
  ( if Int64.equal t.blocking_cost 0L then complete ()
    else
      let limit = Int64.to_int t.blocking_cost in
      schedule t (Int64.of_int (Prng.int t.prng (limit + 1))) complete ) ;
  id

(* A read of a socket or of the read end of a pipe. *)
let read_conn n c buf off len =
  let a = avail c in
  if a > 0 then (
    let k = Int.min a len in
    Buffer.blit c.inbox c.rpos buf off k ;
    c.rpos <- c.rpos + k ;
    if c.rpos = Buffer.length c.inbox then (
      Buffer.clear c.inbox ;
      c.rpos <- 0 ) ;
    (* The space is free again: the peer can write. *)
    ( match c.peer with
    | Some p ->
        edge n p.cfd ~read:false ~write:true
    | None ->
        () ) ;
    Ok k )
  else if c.eof then Ok 0
  else if c.refused then Error Op.Reset
  else Error Op.Would_block

(* A write of a socket or of the write end of a pipe. The data of a pipe arrives
   at once, and the data of a socket after the latency. *)
let write_conn t c buf off len =
  let n = t.net in
  if c.shut then Error Op.Broken_pipe
  else
    match c.peer with
    | None ->
        Error Op.Broken_pipe
    | Some p ->
        let space = capacity - avail p - p.inflight in
        if space <= 0 then Error Op.Would_block
        else
          let k = Int.min len space in
          let data = Bytes.sub_string buf off k in
          p.inflight <- p.inflight + k ;
          (if c.role = Sock then later t else fun f -> f ()) (fun () ->
              p.inflight <- p.inflight - k ;
              Buffer.add_string p.inbox data ;
              edge n p.cfd ~read:true ~write:false ) ;
          Ok k

(* The close of a socket or of an end of a pipe: the peer reads the end of the
   stream, and a write of the peer gives EPIPE. *)
let close_conn t c =
  match c.peer with
  | Some p ->
      c.peer <- None ;
      (if c.role = Sock then later t else fun f -> f ()) (fun () ->
          p.eof <- true ;
          p.peer <- None ;
          edge t.net p.cfd ~read:true ~write:true )
  | None ->
      ()

let new_pipe n =
  let mk role =
    { role
    ; inbox= Buffer.create 64
    ; rpos= 0
    ; inflight= 0
    ; peer= None
    ; eof= false
    ; shut= false
    ; established= true
    ; refused= false
    ; cfd= new_fd n
    ; remote= Op.Unix "" }
  in
  let r = mk Rd_end and w = mk Wr_end in
  r.peer <- Some w ;
  w.peer <- Some r ;
  (r, w)

(* The exit of a fake process: it closes the ends of its pipes, and its pidfd
   becomes readable. *)
let exit_proc n pidfd p status =
  if Option.is_none p.status then (
    p.status <- Some status ;
    List.iter
      (fun c ->
        match c.peer with
        | Some peer ->
            c.peer <- None ;
            peer.eof <- true ;
            peer.peer <- None ;
            edge n peer.cfd ~read:true ~write:true
        | None ->
            () )
      p.child_ends ;
    p.child_ends <- [] ;
    edge n pidfd ~read:true ~write:false )

let spawn t (sp : Op.spawn) =
  let n = t.net in
  match Hashtbl.find_opt t.programs sp.prog with
  | None ->
      Error Op.No_entry
  | Some f ->
      let prog = f (List.tl sp.argv) in
      let p = {prog; status= None; reaped= false; child_ends= []} in
      (* The parent gets one end of each pipe, and the child keeps the other. *)
      let pipe ~parent_reads = function
        | Op.Piped ->
            let r, w = new_pipe n in
            let mine, theirs = if parent_reads then (r, w) else (w, r) in
            Hashtbl.replace n.fds mine.cfd (Conn mine) ;
            p.child_ends <- theirs :: p.child_ends ;
            Some mine
        | Op.Inherit | Op.Null ->
            None
      in
      let to_stdin = pipe ~parent_reads:false sp.stdin in
      let from_stdout = pipe ~parent_reads:true sp.stdout in
      let from_stderr = pipe ~parent_reads:true sp.stderr in
      Option.iter
        (fun c ->
          Buffer.add_string c.inbox prog.output ;
          if prog.output <> "" then edge n c.cfd ~read:true ~write:false )
        from_stdout ;
      let pidfd = new_fd n in
      Hashtbl.replace n.fds pidfd (Pfd p) ;
      Option.iter
        (fun d ->
          schedule t d (fun () -> exit_proc n pidfd p (Op.Exited prog.code)) )
        prog.runs ;
      let pid = t.next_pid in
      t.next_pid <-
        (pid + 1) [@mutate off "a counter that goes down is unique too"] ;
      let fd = Option.map (fun c -> c.cfd) in
      Ok
        { Op.pid
        ; pidfd
        ; to_stdin= fd to_stdin
        ; from_stdout= fd from_stdout
        ; from_stderr= fd from_stderr }

let submit : type a. t -> a Op.t -> (a, Op.error) result =
 fun t op ->
  let n = t.net in
  let conn fd k =
    match Hashtbl.find_opt n.fds fd with
    | Some (Conn {role= Rd_end | Wr_end; _}) ->
        Error (Op.Other "ENOTSOCK")
    | Some (Conn c) ->
        k c
    | Some (Lsn _) ->
        Error (Op.Other "ENOTCONN")
    | Some (Fil _ | Pfd _) ->
        Error (Op.Other "ENOTSOCK")
    | None ->
        Error Op.Bad_fd
  in
  match op with
  | Op.Listen (addr, backlog) ->
      let k = key addr in
      if Hashtbl.mem n.names k then Error Op.In_use
      else
        let fd = new_fd n in
        Hashtbl.replace n.fds fd
          (Lsn {key= k; backlog= Queue.create (); limit= backlog; pending= 0}) ;
        Hashtbl.replace n.names k fd ;
        Ok fd
  | Op.Connect addr -> (
      let refused () = Error Op.Refused in
      match Hashtbl.find_opt n.names (key addr) with
      | None ->
          refused ()
      | Some lfd -> (
        match Hashtbl.find_opt n.fds lfd with
        | Some (Lsn l) when Queue.length l.backlog + l.pending < l.limit ->
            l.pending <- l.pending + 1 ;
            let port = n.next_port in
            n.next_port <-
              (port + 1) [@mutate off "a counter that goes down is unique too"] ;
            let mk remote =
              { role= Sock
              ; inbox= Buffer.create 64
              ; rpos= 0
              ; inflight= 0
              ; peer= None
              ; eof= false
              ; shut= false
              ; established= false
              ; refused= false
              ; cfd= -1
              ; remote }
            in
            let c = mk addr and s = mk (Op.Tcp ("client", port)) in
            c.peer <- Some s ;
            s.peer <- Some c ;
            s.established <- true ;
            c.cfd <- new_fd n ;
            Hashtbl.replace n.fds c.cfd (Conn c) ;
            later t (fun () ->
                l.pending <- l.pending - 1 ;
                match Hashtbl.find_opt n.fds lfd with
                | Some (Lsn l') when l' == l ->
                    Queue.push s l.backlog ;
                    c.established <- true ;
                    edge n lfd ~read:true ~write:false ;
                    edge n c.cfd ~read:false ~write:true
                | _ ->
                    c.refused <- true ;
                    c.peer <- None ;
                    edge n c.cfd ~read:true ~write:true ) ;
            Ok c.cfd
        | _ ->
            refused () ) )
  | Op.Connect_result fd ->
      conn fd (fun c ->
          if c.refused then Error Op.Refused
          else if c.established then Ok ()
          else Error Op.Would_block )
  | Op.Accept lfd -> (
    match Hashtbl.find_opt n.fds lfd with
    | Some (Lsn l) -> (
      match Queue.take_opt l.backlog with
      | Some s ->
          s.cfd <- new_fd n ;
          Hashtbl.replace n.fds s.cfd (Conn s) ;
          Ok (s.cfd, s.remote)
      | None ->
          Error Op.Would_block )
    | Some (Conn _ | Fil _ | Pfd _) ->
        Error (Op.Other "EINVAL")
    | None ->
        Error Op.Bad_fd )
  | Op.Read (fd, buf, off, len) ->
      conn fd (fun c -> read_conn n c buf off len)
  | Op.Write (fd, buf, off, len) ->
      conn fd (fun c -> write_conn t c buf off len)
  | Op.Pipe ->
      let r, w = new_pipe n in
      Hashtbl.replace n.fds r.cfd (Conn r) ;
      Hashtbl.replace n.fds w.cfd (Conn w) ;
      Ok (r.cfd, w.cfd)
  | Op.Spawn sp ->
      spawn t sp
  | Op.Wait_child fd -> (
    match Hashtbl.find_opt n.fds fd with
    | Some (Pfd p) -> (
      match p.status with
      | Some _ when p.reaped ->
          Error (Op.Other "ECHILD")
      | Some s ->
          p.reaped <- true ;
          Ok (Some s)
      | None ->
          Ok None )
    | Some _ ->
        Error (Op.Other "EINVAL")
    | None ->
        Error Op.Bad_fd )
  | Op.Kill (fd, sg) -> (
    match Hashtbl.find_opt n.fds fd with
    | Some (Pfd p) ->
        ( match (p.status, sg) with
        | Some _, _ ->
            ()
        | None, (Op.SIGSTOP | SIGCONT | SIGCHLD | SIGWINCH) ->
            ()
        | None, _ when sg <> Op.SIGKILL && List.mem sg p.prog.ignores ->
            ()
        | None, _ ->
            exit_proc n fd p (Op.Signaled sg) ) ;
        Ok ()
    | Some _ ->
        Error (Op.Other "EINVAL")
    | None ->
        Error Op.Bad_fd )
  | Op.Watch _ ->
      Ok ()
  | Op.Fd_read (fd, buf, off, len) -> (
    match Hashtbl.find_opt n.fds fd with
    | Some (Conn ({role= Rd_end | Sock; _} as c)) ->
        read_conn n c buf off len
    | Some (Conn {role= Wr_end; _}) | None ->
        Error Op.Bad_fd
    | Some (Lsn _ | Fil _ | Pfd _) ->
        Error (Op.Other "EINVAL") )
  | Op.Fd_write (fd, buf, off, len) -> (
    match Hashtbl.find_opt n.fds fd with
    | Some (Conn ({role= Wr_end | Sock; _} as c)) ->
        write_conn t c buf off len
    | Some (Conn {role= Rd_end; _}) | None ->
        Error Op.Bad_fd
    | Some (Lsn _ | Fil _ | Pfd _) ->
        Error (Op.Other "EINVAL") )
  | Op.Shutdown fd ->
      conn fd (fun c ->
          c.shut <- true ;
          ( match c.peer with
          | Some p ->
              later t (fun () ->
                  p.eof <- true ;
                  edge n p.cfd ~read:true ~write:false )
          | None ->
              () ) ;
          Ok () )
  | Op.Close fd -> (
      if Hashtbl.mem n.regs fd then
        model_error "close(2) of fd %d before the Deregister of each domain" fd ;
      Hashtbl.iter
        (fun id j ->
          (* Also after a detach: the job holds a user of the fd. *)
          if j.jfd = Some fd && Option.is_none j.result then
            model_error "close(2) of fd %d while job %d uses it" fd id )
        t.jobs ;
      match Hashtbl.find_opt n.fds fd with
      | None ->
          Error Op.Bad_fd
      | Some sfd ->
          Hashtbl.remove n.fds fd ;
          ( match sfd with
          | Conn c ->
              close_conn t c
          | Lsn l ->
              Hashtbl.remove n.names l.key ;
              Queue.iter
                (fun s ->
                  match s.peer with
                  | Some c ->
                      c.refused <- true ;
                      c.peer <- None ;
                      edge n c.cfd ~read:true ~write:true
                  | None ->
                      () )
                l.backlog
          | Fil _ | Pfd _ ->
              () ) ;
          Ok () )
  | Op.Register (d, fd, token) ->
      if not (Hashtbl.mem n.fds fd) then
        model_error "Register of fd %d, which is closed" fd ;
      let l = Option.value ~default:[] (Hashtbl.find_opt n.regs fd) in
      if List.mem_assoc d l then
        model_error "a second Register of fd %d on domain %d" fd d ;
      Hashtbl.replace n.regs fd ((d, token) :: l) ;
      (* A Register of a ready fd gives one edge at once. *)
      let read = readable n fd and write = writable n fd in
      if read || write then
        n.edges <- (d, World.Ready.Fd {token; read; write}) :: n.edges ;
      Ok ()
  | Op.Start (d, job) ->
      Ok (start_job t d job)
  | Op.Finish (id, job) -> (
    match Hashtbl.find_opt t.jobs id with
    | Some {result= Some r; detached= false; _} ->
        Hashtbl.remove t.jobs id ;
        Result.map (finish job) r
    | _ ->
        model_error "Finish of job %d before its completion" id )
  | Op.Detach id -> (
    match Hashtbl.find_opt t.jobs id with
    | Some j -> (
      match j.result with
      | Some r ->
          Result.iter j.on_drop r ; Hashtbl.remove t.jobs id ; Ok ()
      | None ->
          j.detached <- true ;
          Ok () )
    | None ->
        model_error "Detach of job %d, which does not exist" id )
  | Op.Deregister (d, fd) -> (
    match Hashtbl.find_opt n.regs fd with
    | Some l when List.mem_assoc d l ->
        ( match List.remove_assoc d l with
        | [] ->
            Hashtbl.remove n.regs fd
        | l ->
            Hashtbl.replace n.regs fd l ) ;
        Ok ()
    | _ ->
        model_error
          "Deregister of fd %d on domain %d, which has no registration" fd d )

(* The edges of domain [d], the oldest first. *)
let take_edges t d =
  let mine, others = List.partition (fun (d', _) -> d' = d) t.net.edges in
  t.net.edges <- others ;
  List.rev_map snd mine

(* The check of the waiters of the fds: a live waiter on a ready fd is a lost
   wakeup (design section 2.8, The wait steps). *)
let fd_problems t =
  List.concat_map
    (fun (fd, readers, writers) ->
      let r =
        if readable t.net fd then
          List.map
            (fun w ->
              Printf.sprintf "%s waits to read fd %d, which is ready" w fd )
            readers
        else []
      in
      let w =
        if writable t.net fd then
          List.map
            (fun w ->
              Printf.sprintf "%s waits to write fd %d, which is ready" w fd )
            writers
        else []
      in
      r @ w )
    (Sched_debug.fd_waiters (sched t))

let earliest a b =
  match (a, b) with
  | None, x | x, None ->
      x
  | Some a, Some b ->
      Some (Int64.min a b)

(* The virtual domains (design section 2.8, Virtual domains). Each domain runs
   its loop under a handler that stops at each world effect: each world effect
   is a switch point. One top-level loop keeps the continuation of each domain,
   and the PRNG selects the domain that continues. *)

type dstep =
  | Ended
  | Paused : 'a Effect.t * ('a, dstep) Effect.Deep.continuation -> dstep

type vstate =
  | Start of Sched.dom
  | Pending : 'a Effect.t * ('a, dstep) Effect.Deep.continuation -> vstate
  | Sleeping of
      World.Wait.t * (World.Ready.t list, dstep) Effect.Deep.continuation
  | Ended_dom

let start_dom d =
  Effect.Deep.match_with Sched.run_domain d
    { retc= (fun () -> Ended)
    ; exnc=
        (fun e ->
          Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()) )
    ; effc=
        (fun (type a) (e : a Effect.t) ->
          match e with
          | World.Now _
          | World.Submit _
          | World.Wait _
          | World.Wake _
          | World.Random _
          | World.Random_bytes _
          | World.Switch ->
              Some
                (fun (k : (a, dstep) Effect.Deep.continuation) -> Paused (e, k))
          | _ ->
              None ) }

let now_of (type time) t (line : time Weft.Clock.line) : time =
  Weft.Clock.of_ns line
    (match line with Mono -> t.now | Posix -> Int64.add t.posix0 t.now)

(* What a [Wait] of domain [d] gets now: a wake, and the edges, alarms and
   completions of [d]. *)
let take_ready t d =
  let woken = if t.woken.(d) then [World.Ready.Woken] else [] in
  t.woken.(d) <- false ;
  woken @ take_edges t d

let has_ready t d =
  t.woken.(d) || List.exists (fun (d', _) -> d' = d) t.net.edges

let runnable t st d =
  match st.(d) with
  | Start _ | Pending _ ->
      true
  | Sleeping _ ->
      has_ready t d
  | Ended_dom ->
      false

(* Answer the effect of domain [d], continue it, and keep its next state. *)
let step t st d =
  let next = function Ended -> Ended_dom | Paused (e, k) -> Pending (e, k) in
  match st.(d) with
  | Start dom ->
      st.(d) <- next (start_dom dom)
  | Sleeping (_, k) ->
      st.(d) <- next (Effect.Deep.continue k (take_ready t d))
  | Ended_dom ->
      ()
  | Pending (e, k) -> (
      let go v = st.(d) <- next (Effect.Deep.continue k v) in
      match e with
      | World.Now line ->
          go (now_of t line)
      | World.Submit op ->
          go (submit t op)
      | World.Random n ->
          go (Prng.int t.prng n)
      | World.Random_bytes b ->
          go (Prng.bytes t.prng b)
      | World.Switch ->
          go ()
      | World.Wake d' ->
          t.woken.(d') <- true ;
          go ()
      | World.Wait (_, w) -> (
        (* A domain waits only on its own poller. *)
        match (take_ready t d, w.timeout) with
        | (_ :: _ as ready), _ ->
            go ready
        | [], World.Wait.Poll ->
            go []
        | [], World.Wait.Sleep ->
            st.(d) <- Sleeping (w, k) )
      | _ ->
          assert false (* The handler stops only at world effects. *) )

(* The program is quiescent: each domain sleeps, or ended, and nothing is ready
   (design section 2.8, The wait steps). *)
let quiescent t st =
  let s = sched t in
  ( match
      Sched_debug.lost s ~now:(Weft.Clock.of_ns Mono t.now) @ fd_problems t
    with
  | [] ->
      ()
  | l ->
      raise (Lost_wakeup l) ) ;
  let deadlines =
    List.filter_map
      (fun d ->
        match st.(d) with
        | Sleeping (w, _) ->
            Option.map (fun m -> (d, Weft.Clock.to_ns Mono m)) w.deadline
        | _ ->
            None )
      (List.init (Array.length st) Fun.id)
  in
  let deadline =
    List.fold_left (fun acc (_, at) -> earliest acc (Some at)) None deadlines
  in
  let next_agenda =
    match t.agenda with (at, _) :: _ -> Some at | [] -> None
  in
  (* The first domain that did not end gets the completions. *)
  let first =
    let rec find d =
      if
        ( d >= Array.length st )
        [@mutate off "a domain did not end, so the search stops before"]
      then 0
      else match st.(d) with Ended_dom -> find (d + 1) | _ -> d
    in
    find 0
  in
  let give d r = t.net.edges <- (d, r) :: t.net.edges in
  (* Move the time to [at], run the agenda, and give the alarm to each domain
     whose deadline is due. *)
  let move at =
    t.now <- Int64.max t.now at ;
    run_agenda t ;
    List.iter
      (fun (d, dl) ->
        if Int64.compare dl t.now <= 0 then give d World.Ready.Alarm )
      deadlines
  in
  match t.settles with
  | _ :: _ ->
      List.iter
        (fun id -> give first (World.Ready.External id))
        (List.rev t.settles) ;
      t.settles <- []
  | [] -> (
      if t.auto then
        match earliest deadline next_agenda with
        | Some at ->
            move at
        | None ->
            raise (Deadlock (Sched_debug.blocked s))
      else
        match t.advance with
        | Some (target, id) ->
            let at =
              match earliest deadline next_agenda with
              | Some at ->
                  Int64.min at target
              | None ->
                  target
            in
            move at ;
            if Int64.compare at target >= 0 then (
              t.advance <- None ;
              give first (World.Ready.External id) )
        | None ->
            raise
              (Deadlock
                 ( "manual mode: only advance_until moves the time"
                 :: Sched_debug.blocked s ) ) )

let run ?(seed = 0) ?(domains = 1) ?(auto = true) main =
  let t =
    { prng= Prng.make seed
    ; auto
    ; now= 0L
    ; posix0= 1_700_000_000_000_000_000L
    ; woken= Array.make domains false
    ; sched= None
    ; net=
        { fds= Hashtbl.create ~random:false 16
        ; next_fd= 3
        ; names= Hashtbl.create ~random:false 8
        ; regs= Hashtbl.create ~random:false 16
        ; edges= []
        ; latency= 0L
        ; next_port= 40000 }
    ; agenda= []
    ; next_id= 0
    ; settles= []
    ; advance= None
    ; fs= Sim_fs.create ()
    ; jobs= Hashtbl.create ~random:false 16
    ; blocking_cost= 0L
    ; dns= Hashtbl.create ~random:false 8
    ; programs= Hashtbl.create ~random:false 8
    ; next_pid= 1000 }
  in
  let s = Sched.create ~slack:Weft.Span.zero ~simulated:true ~domains () in
  t.sched <- Some s ;
  let result = Sched.start s (fun () -> main t) in
  let st = Array.map (fun d -> Start d) s.doms in
  let all = List.init domains Fun.id in
  let rec loop () =
    match List.filter (runnable t st) all with
    | [] ->
        if
          List.exists
            (fun d -> match st.(d) with Ended_dom -> false | _ -> true)
            all
        then ( quiescent t st ; loop () )
    | ds ->
        step t st (List.nth ds (Prng.int t.prng (List.length ds))) ;
        loop ()
  in
  loop () ; result ()

let wait_external id =
  Event.sync
    (Event.Base (fun () -> {Event.leaf= Sched.external_leaf id; result= ignore}))

let settle t =
  let id = fresh t in
  t.settles <- id :: t.settles ;
  wait_external id

let advance_until t target =
  if t.auto then invalid_arg "Sim.advance_until: the simulator is in auto mode" ;
  if Option.is_some t.advance then
    invalid_arg "Sim.advance_until: an advance is already active" ;
  let id = fresh t in
  t.advance <- Some (Weft.Clock.to_ns Mono target, id) ;
  (* A cancel ends the advance. *)
  match wait_external id with
  | () ->
      ()
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      ( match t.advance with
      | Some (_, id') when id' = id ->
          t.advance <- None
      | _ ->
          () ) ;
      Printexc.raise_with_backtrace e bt

let set_latency t span = t.net.latency <- Weft.Span.to_ns span

let set_blocking_cost t span =
  if Int64.compare (Weft.Span.to_ns span) 0L < 0 then
    invalid_arg "Sim.set_blocking_cost: the span is negative" ;
  t.blocking_cost <- Weft.Span.to_ns span

let set_dns t name answer = Hashtbl.replace t.dns name answer

let program ?runs ?(code = 0) ?(output = "") ?(ignores = []) () =
  {runs= Option.map Weft.Span.to_ns runs; code; output; ignores}

let add_program t name f = Hashtbl.replace t.programs name f

(* Domain 0 gets the signal at its next [Wait] (design section 2.7). *)
let signal t sg = t.net.edges <- (0, World.Ready.Signal (sg, 1)) :: t.net.edges

let addr _ s =
  match String.index_opt s ':' with
  | Some i when String.sub s 0 i = "unix" ->
      Op.Unix (String.sub s (i + 1) (String.length s - i - 1))
  | Some i -> (
      let host = String.sub s 0 i in
      match
        int_of_string_opt (String.sub s (i + 1) (String.length s - i - 1))
      with
      | Some port ->
          Op.Tcp (host, port)
      | None ->
          invalid_arg ("Sim.addr: " ^ s) )
  | None ->
      invalid_arg ("Sim.addr: " ^ s)
