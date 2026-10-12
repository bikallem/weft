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

exception Livelock of string list

exception Unawaited of string list

exception Killed of Op.signal

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
  ; mutable last_at: int64
        (** The time of the last delivery to this side: a later one never comes
            before it. *)
  ; mutable peer: conn option  (** [None] after the peer closed. *)
  ; mutable eof: bool  (** The peer ended its send side. *)
  ; mutable shut: bool  (** This side ended its send side. *)
  ; mutable reset: bool
        (** A reset came: the next read with no data, or on TCP the next write,
            gives [Reset]. *)
  ; mutable abort: bool  (** [Reset_on_close]: the close sends a reset. *)
  ; mutable err: Op.error option
        (** [Sim.fault]: the next read with no data, or the next write, gives
            this error one time. *)
  ; mutable lost: bool
        (** [Sim.fault] with an error that is not [Reset]: the link is lost. The
            data of the peer stays in flight, so the window of the peer stays
            full. *)
  ; mutable established: bool
  ; mutable refused: bool
  ; mutable cfd: int  (** [-1] until the server side is accepted. *)
  ; remote: Op.addr
  ; local: Op.addr  (** [Local_addr]. *)
  ; tcp: bool  (** A TCP socket, not a Unix socket or a pipe. *) }

(* [pending] counts the connections that the latency still holds, so that they
   count against the limit of the backlog. A full backlog refuses the connect at
   once. On Linux, a TCP connect retries the SYN instead, and a Unix connect
   gives EAGAIN. *)
type listener =
  { key: string
  ; backlog: conn Queue.t
  ; limit: int
  ; mutable pending: int
  ; ltcp: bool
  ; laddr: Op.addr  (** The address, with the port of a listen on port 0. *) }

(* A UDP socket: its address, and the datagrams that came, with their senders.
   [bytes] counts the cost of the queue, against the receive buffer. A datagram
   that does not fit is lost, as on Linux. *)
type dgram =
  { host: string
  ; port: int
  ; dkey: string  (** ["host:port"], in [udp_names]. *)
  ; queue: (string * string * int) Queue.t
  ; mutable bytes: int }

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

type sfd =
  | Lsn of listener
  | Conn of conn
  | Dgm of dgram
  | Fil of Sim_fs.handle
  | Pfd of proc

(* The socket options of an fd. The sizes follow Linux with its default limits:
   a set keeps two times the value, from 2304 to 425984 bytes. *)
type opts =
  { mutable no_delay: bool
  ; mutable keep_alive: bool
  ; mutable broadcast: bool
  ; mutable rcvbuf: int
  ; mutable sndbuf: int }

type net =
  { fds: (int, sfd) Hashtbl.t
  ; mutable next_fd: int
  ; names: (string, int) Hashtbl.t  (** The listeners, by address. *)
  ; udp_names: (string, int) Hashtbl.t  (** The UDP sockets, by address. *)
  ; opts: (int, opts) Hashtbl.t
        (** The options of each socket, after its first [Set_option] or
            [Get_option]. *)
  ; regs: (int, (int * int) list) Hashtbl.t
        (** The registrations of each fd: the domain and the token. *)
  ; mutable edges: (int * World.Ready.t) list
        (** The edges to give, by domain, the newest first. *)
  ; mutable latency: int64
  ; mutable jitter: int64  (** The limit of a random delay more. *)
  ; mutable short_io: bool
  ; mutable extra_edges: bool
  ; mutable udp_loss: bool
  ; mutable resets: bool
  ; mutable next_port: int
  ; mutable fd_limit: int option  (** [Sim.set_fd_limit]. *) }

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
  ; mutable posix0: int64  (** [Posix] at [Mono] 0. [set_posix] changes it. *)
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
  ; mutable watched: Op.signal list
        (** The signals of [run ~signals], with no signal of an unwatch. *)
  ; mutable killed: Op.signal option
        (** A signal with an action that ends the process. The run ends at once
            with [Killed]. *)
  ; fills: (Sched.dom -> unit) list Atomic.t
        (** The fills from outside a fiber that wait for the quiescence, the
            newest first. A finaliser can push, so the list is atomic. *)
  ; refused_fill: bool Atomic.t  (** A fill with no [~outside_fills]. *)
  ; resumes_seen: int array
        (** The resumes of each domain at its last step, for [Livelock]. *)
  ; livelock: int  (** The limit of the switch points with no progress. *)
  ; mutable no_progress: int
        (** The switch points since the last progress or quiescence. *)
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
  | Some (Dgm g) ->
      not (Queue.is_empty g.queue)
  | Some (Pfd p) ->
      Option.is_some p.status
  | Some (Fil _) | None ->
      false

(* PIPE_BUF on Linux. *)
let pipe_buf = 4096

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
          (* A pipe is writable with a free page, PIPE_BUF bytes: a write of at
             most PIPE_BUF bytes can wait for that much space. *)
          c.established
          && ( avail p + p.inflight
             <= capacity - if c.role = Wr_end then pipe_buf else 1 )
             [@mutate
               off
                 "a wrong result here gives a spurious edge: the write gets \
                  EAGAIN and waits again"] )
  | Some (Dgm _) ->
      true
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

(* The latency, and a random delay up to the jitter. *)
let delay t =
  let j = t.net.jitter in
  Int64.add t.net.latency
    ( if
        Int64.equal j 0L
        [@mutate
          off "a draw in 0 to 0 gives 0 too; only the seed sequence changes"]
      then 0L
      else
        Int64.of_int
          (Prng.int t.prng
             (( Int64.to_int j + 1 )
              [@mutate off "the limit is in the range, or 1 ns less"] ) ) )

(* After the delay, or at once. *)
let later t f =
  let d = delay t in
  if Int64.equal d 0L then f () else schedule t d f

(* A delivery to [p] after the delay, but never before the last delivery to [p]:
   a stream keeps its order (TCP). *)
let later_to t p f =
  let at = Int64.max (Int64.add t.now (delay t)) p.last_at in
  p.last_at <- at ;
  if
    ( Int64.compare at t.now <= 0 )
    [@mutate
      off
        "at the time of now, the agenda gives the delivery at the next \
         quiescence of the same time; the order of the stream stays"]
  then f ()
  else schedule t (Int64.sub at t.now) f

(* The data of a socket [c] comes to [p] after the delay, and the data of a pipe
   at once. *)
let deliver t c p f = if c.role = Sock then later_to t p f else f ()

(* With [short_io], a read or a write moves a random part of [k] bytes. *)
let part t k =
  if t.net.short_io && (k > 1) [@mutate off "a part of 1 byte is 1 byte"] then
    1 + Prng.int t.prng k
  else k

(* The next port from 40000 to 65535, then from 40000 again, that [used] does
   not refuse. If all are in use, it gives one in use. *)
let fresh_port n used =
  let rec go tries =
    let p = n.next_port in
    n.next_port <- (if p >= 65535 then 40000 else p + 1) ;
    if
      used p
      && ( tries > 0 )
         [@mutate
           off "the count of tries matters only when all ports are in use"]
    then go ((tries - 1) [@mutate off "the same reason"])
    else p
  in
  (go (65535 - 40000)
   [@mutate off "the count of tries matters only when all ports are in use"] )

(* The limit of open fds, as RLIMIT_NOFILE. An operation that makes [k] fds
   gives EMFILE if the open fds and [k] are more than the limit. As on Linux,
   the check comes before the other checks of the operation. *)
let room n k =
  match n.fd_limit with
  | Some limit when Hashtbl.length n.fds + k > limit ->
      Error (Op.No_resources "EMFILE")
  | _ ->
      Ok ()

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
  | Some (Lsn _ | Conn _ | Dgm _ | Pfd _) ->
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
        (let* () = room n 1 in
         let* h = Sim_fs.open_ fs path flags perm ~now in
         let fd = new_fd n in
         Hashtbl.replace n.fds fd (Fil h) ;
         Ok (R_int fd) )
  | Op.Pread (fd, _, _, len, at) ->
      Some
        (let* h = handle_of n fd in
         Result.map (fun s -> R_read s) (Sim_fs.pread h len at) )
  | Op.Pwrite (fd, data, at) ->
      (* With [short_io], a write of a file can be short too. *)
      let k = part t (String.length data) in
      let data =
        if (k < String.length data) [@mutate off "a copy of the whole string"]
        then String.sub data 0 k
        else data
      in
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
      schedule t
        (Int64.of_int
           (Prng.int t.prng
              (( limit + 1 )
               [@mutate off "the limit is in the range, or 1 ns less"] ) ) )
        complete ) ;
  id

(* A read of a socket or of the read end of a pipe. An error of [Sim.fault]
   comes after the data, as the error of a socket on Linux. *)
let read_conn t c buf off len =
  let n = t.net in
  let a = avail c in
  if a > 0 then (
    let k = part t (Int.min a len) in
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
  else if Option.is_some c.err then (
    let e = Option.get c.err in
    c.err <- None ;
    Error e )
  else if c.reset then (
    c.reset <- false ;
    Error Op.Reset )
  else if c.eof then Ok 0
  else if c.refused then Error Op.Reset
  else Error Op.Would_block

(* The close of a socket or of an end of a pipe: the peer reads the end of the
   stream, and a write of the peer gives EPIPE. *)
let close_conn t c =
  match c.peer with
  | Some p ->
      c.peer <- None ;
      deliver t c p (fun () ->
          p.eof <- true ;
          p.peer <- None ;
          edge t.net p.cfd ~read:true ~write:true )
  | None ->
      ()

(* The reset from [c] comes to [p] after the delay. If [p] has no peer at that
   time, after its own reset or a [Sim.fault], it ignores the reset, as a closed
   TCP socket ignores an RST. *)
let send_reset t c p =
  deliver t c p (fun () ->
      if Option.is_some p.peer then (
        p.reset <- true ;
        p.eof <- true ;
        p.peer <- None ;
        edge t.net p.cfd ~read:true ~write:true ) )

(* A reset from [c] to its peer, as on Linux. The peer reads the data that came
   before the reset. Then its next read with no data gives ECONNRESET, and later
   reads give the end of the stream. On TCP (an RST), the next write also gives
   ECONNRESET, and clears it. After a [Shutdown] of [c], a TCP peer gets the end
   of the stream and EPIPE. A Unix stream socket gives a reset only if [c] has
   unread data, also after a [Shutdown], and a write of the peer gives EPIPE.
   The other cases are a normal close. *)
let reset_conn t c =
  let reset = if c.tcp then not c.shut else c.role = Sock && avail c > 0 in
  match c.peer with
  | Some p when reset ->
      c.peer <- None ;
      send_reset t c p
  | _ ->
      close_conn t c

(* A write of a socket or of the write end of a pipe. The data of a pipe arrives
   at once, and the data of a socket after the latency. Data that arrives after
   the close or the reset of the peer is lost. With [resets], one write of a TCP
   socket in 64 resets the connection: this side gets ECONNRESET at once, and
   the peer gets the reset after the delay. *)
let write_conn t c buf off len =
  let n = t.net in
  if Option.is_some c.err then (
    let e = Option.get c.err in
    c.err <- None ;
    Error e )
  else if c.reset && c.tcp then (
    c.reset <- false ;
    Error Op.Reset )
  else if c.shut then Error Op.Broken_pipe
  else
    match c.peer with
    | None ->
        Error Op.Broken_pipe
    | Some _ when c.tcp && n.resets && Prng.int t.prng 64 = 0 ->
        reset_conn t c ;
        c.eof <- true ;
        (* A fiber that waits on this side reads the end of the stream. *)
        edge n c.cfd ~read:true ~write:true ;
        Error Op.Reset
    | Some p ->
        let space = capacity - avail p - p.inflight in
        (* A write of at most PIPE_BUF bytes to a pipe is atomic. *)
        let atomic = c.role = Wr_end && len <= pipe_buf in
        if space <= 0 then Error Op.Would_block
        else if
          atomic
          && ( space < len )
             [@mutate off "with space = len, the write waits for one more read"]
        then Error Op.Would_block
        else
          let k = if atomic then len else part t (Int.min len space) in
          let data = Bytes.sub_string buf off k in
          p.inflight <- p.inflight + k ;
          (* After a lost link, the data never comes, and the peer gets no space
             again, as when no acknowledgment comes. *)
          deliver t c p (fun () ->
              if not p.lost then p.inflight <- p.inflight - k ;
              if Option.is_some p.peer then (
                Buffer.add_string p.inbox data ;
                edge n p.cfd ~read:true ~write:false ) ) ;
          Ok k

(* The largest datagram of UDP over IPv4. *)
let max_datagram = 65507

(* The receive buffer of a UDP socket with no option, as on Linux. *)
let udp_rcvbuf = 212992

(* The cost of a datagram in the receive buffer: its length and the overhead of
   the kernel. Linux 7.1 on loopback keeps 92 datagrams of 1000 bytes with the
   default buffer, 86 with a buffer of 200000 bytes, and 1 with the smallest
   buffer (2304 bytes). The overhead 1304 gives 93, 87 and 1. *)
let dgram_cost len = len + 1304

(* A datagram goes after the delay, with no order: the jitter can reorder
   datagrams. With [udp_loss], one datagram in 8 is lost. At the arrival, a
   datagram to a closed socket is lost. A queue takes a datagram while its cost
   is less than the receive buffer, as on Linux. *)
let send_dgram t g data host port =
  let n = t.net in
  let len = String.length data in
  if len > max_datagram then Error (Op.Other "EMSGSIZE")
  else (
    if not (n.udp_loss && Prng.int t.prng 8 = 0) then
      later t (fun () ->
          match
            Hashtbl.find_opt n.udp_names (Printf.sprintf "%s:%d" host port)
          with
          | Some fd -> (
              let rcvbuf =
                match Hashtbl.find_opt n.opts fd with
                | Some o ->
                    o.rcvbuf
                | None ->
                    udp_rcvbuf
              in
              match Hashtbl.find_opt n.fds fd with
              | Some (Dgm d) when d.bytes < rcvbuf ->
                  Queue.push (data, g.host, g.port) d.queue ;
                  d.bytes <- d.bytes + dgram_cost len ;
                  edge n fd ~read:true ~write:false
              | _ ->
                  () )
          | None ->
              () ) ;
    Ok len )

(* One datagram: the bytes that do not fit in the range are lost. *)
let recv_dgram g buf off len =
  match Queue.take_opt g.queue with
  | Some (data, host, port) ->
      g.bytes <- g.bytes - dgram_cost (String.length data) ;
      let k = Int.min len (String.length data) in
      Bytes.blit_string data 0 buf off k ;
      Ok (k, host, port)
  | None ->
      Error Op.Would_block

let default_opts tcp =
  { no_delay= false
  ; keep_alive= false
  ; broadcast= false
  ; rcvbuf= (if tcp then 131072 else udp_rcvbuf)
  ; sndbuf= (if tcp then 16384 else udp_rcvbuf) }

let copy_opts o =
  { no_delay= o.no_delay
  ; keep_alive= o.keep_alive
  ; broadcast= o.broadcast
  ; rcvbuf= o.rcvbuf
  ; sndbuf= o.sndbuf }

(* The smallest size: 2304 bytes for a receive buffer, 4608 for a send buffer,
   as on Linux. *)
let size ~least v = Int.max least (Int.min (2 * v) 425984)

let set_opt : type a. opts -> a Op.sockopt -> a -> unit =
 fun o opt v ->
  match opt with
  | Op.No_delay ->
      o.no_delay <- v
  | Op.Keep_alive ->
      o.keep_alive <- v
  | Op.Broadcast ->
      o.broadcast <- v
  | Op.Recv_buffer ->
      o.rcvbuf <- size ~least:2304 v
  | Op.Send_buffer ->
      o.sndbuf <- size ~least:4608 v

let get_opt : type a. opts -> a Op.sockopt -> a =
 fun o opt ->
  match opt with
  | Op.No_delay ->
      o.no_delay
  | Op.Keep_alive ->
      o.keep_alive
  | Op.Broadcast ->
      o.broadcast
  | Op.Recv_buffer ->
      o.rcvbuf
  | Op.Send_buffer ->
      o.sndbuf

(* The options of a socket. [TCP_NODELAY] is only for TCP: Linux gives
   EOPNOTSUPP for a Unix socket and ENOPROTOOPT for UDP. *)
let with_opts : type a b.
    t -> int -> a Op.sockopt -> (opts -> b) -> (b, Op.error) result =
 fun t fd opt f ->
  let n = t.net in
  let socket tcp =
    match (opt, tcp) with
    | Op.No_delay, Some false ->
        Error (Op.Other "EOPNOTSUPP")
    | Op.No_delay, None ->
        Error (Op.Other "ENOPROTOOPT")
    | _ ->
        let o =
          match Hashtbl.find_opt n.opts fd with
          | Some o ->
              o
          | None ->
              let o = default_opts (tcp = Some true) in
              Hashtbl.replace n.opts fd o ;
              o
        in
        Ok (f o)
  in
  match Hashtbl.find_opt n.fds fd with
  | Some (Conn {role= Sock; tcp; _}) ->
      socket (Some tcp)
  | Some (Lsn l) ->
      socket (Some l.ltcp)
  | Some (Dgm _) ->
      socket None
  | Some (Conn _ | Fil _ | Pfd _) ->
      Error (Op.Other "ENOTSOCK")
  | None ->
      Error Op.Bad_fd

let new_pipe n =
  let mk role =
    { role
    ; inbox= Buffer.create 64
    ; rpos= 0
    ; inflight= 0
    ; last_at= 0L
    ; peer= None
    ; eof= false
    ; shut= false
    ; reset= false
    ; abort= false
    ; err= None
    ; lost= false
    ; established= true
    ; refused= false
    ; cfd= new_fd n
    ; remote= Op.Unix ""
    ; local= Op.Unix ""
    ; tcp= false }
  in
  let r = mk Rd_end and w = mk Wr_end in
  r.peer <- Some w ;
  w.peer <- Some r ;
  (r, w)

(* One side of a connection, before the connect completes. *)
let new_sock ~tcp ~local remote =
  { role= Sock
  ; inbox= Buffer.create 64
  ; rpos= 0
  ; inflight= 0
  ; last_at= 0L
  ; peer= None
  ; eof= false
  ; shut= false
  ; reset= false
  ; abort= false
  ; err= None
  ; lost= false
  ; established= false
  ; refused= false
  ; cfd= -1
  ; remote
  ; local
  ; tcp }

(* The exit of a fake process: it closes the ends of its pipes, and its pidfd
   becomes readable. *)
let exit_proc t pidfd p status =
  if Option.is_none p.status then (
    p.status <- Some status ;
    List.iter (close_conn t) p.child_ends ;
    p.child_ends <- [] ;
    edge t.net pidfd ~read:true ~write:false )

(* The fds of a spawn, as on Linux: the two ends of each pipe, then the pidfd.
   They come before the search of the program. *)
let spawn_fds (sp : Op.spawn) =
  List.fold_left
    (fun k s -> if s = Op.Piped then k + 2 else k)
    1
    [sp.stdin; sp.stdout; sp.stderr]

let spawn t (sp : Op.spawn) =
  let n = t.net in
  match (room n (spawn_fds sp), Hashtbl.find_opt t.programs sp.prog) with
  | Error e, _ ->
      Error e
  | Ok (), None ->
      Error Op.No_entry
  | Ok (), Some f ->
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
          (* No edge: the Register of the fd gives one, because it is ready. *)
          Buffer.add_string c.inbox prog.output )
        from_stdout ;
      let pidfd = new_fd n in
      Hashtbl.replace n.fds pidfd (Pfd p) ;
      Option.iter
        (fun d ->
          schedule t d (fun () -> exit_proc t pidfd p (Op.Exited prog.code)) )
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

(* A signal to the program itself (design section 2.8, Signals). Domain 0 gets a
   watched signal at its next [Wait]. Each other signal gets the default action,
   and the world ignores SIGPIPE. Only these signals do not end the process: the
   simulator has no stop of a process. *)
let send_self t sg =
  if List.mem sg t.watched then
    t.net.edges <- (0, World.Ready.Signal (sg, 1)) :: t.net.edges
  else
    match sg with
    | Op.SIGPIPE
    | SIGCHLD
    | SIGWINCH
    | SIGCONT
    | SIGSTOP
    | SIGTSTP
    | Other_signal 0 ->
        ()
    | _ ->
        t.killed <- Some sg

let submit : type a. t -> a Op.t -> (a, Op.error) result =
 fun t op ->
  let n = t.net in
  let conn fd k =
    match Hashtbl.find_opt n.fds fd with
    | Some (Conn {role= Rd_end | Wr_end; _}) ->
        Error (Op.Other "ENOTSOCK")
    | Some (Conn c) ->
        k c
    | Some (Lsn _ | Dgm _) ->
        Error (Op.Other "ENOTCONN")
    | Some (Fil _ | Pfd _) ->
        Error (Op.Other "ENOTSOCK")
    | None ->
        Error Op.Bad_fd
  in
  let ( let* ) = Result.bind in
  match op with
  | Op.Listen (addr, backlog) ->
      let* () = room n 1 in
      (* A listen on port 0 gets a free port, as on Linux. *)
      let addr =
        match addr with
        | Op.Tcp (h, 0) ->
            let used p = Hashtbl.mem n.names (key (Op.Tcp (h, p))) in
            Op.Tcp (h, fresh_port n used)
        | a ->
            a
      in
      let k = key addr in
      if Hashtbl.mem n.names k then Error Op.In_use
      else
        let fd = new_fd n in
        let ltcp = match addr with Op.Tcp _ -> true | Op.Unix _ -> false in
        Hashtbl.replace n.fds fd
          (Lsn
             { key= k
             ; backlog= Queue.create ()
             ; limit= backlog
             ; pending= 0
             ; ltcp
             ; laddr= addr } ) ;
        Hashtbl.replace n.names k fd ;
        Ok fd
  | Op.Connect addr -> (
      let* () = room n 1 in
      let refused () = Error Op.Refused in
      match Hashtbl.find_opt n.names (key addr) with
      | None ->
          refused ()
      | Some lfd -> (
        match Hashtbl.find_opt n.fds lfd with
        | Some (Lsn l) when Queue.length l.backlog + l.pending < l.limit ->
            l.pending <- l.pending + 1 ;
            (* The client has the host "client" and a port from the counter. A
               Unix client has no path. *)
            let client =
              if l.ltcp then Op.Tcp ("client", fresh_port n (fun _ -> false))
              else Op.Unix ""
            in
            let c = new_sock ~tcp:l.ltcp ~local:client addr
            and s = new_sock ~tcp:l.ltcp ~local:l.laddr client in
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
  | Op.Local_addr fd -> (
    match Hashtbl.find_opt n.fds fd with
    | Some (Lsn l) ->
        Ok l.laddr
    | Some (Conn {role= Sock; local; _}) ->
        Ok local
    | Some (Dgm g) ->
        Ok (Op.Tcp (g.host, g.port))
    | Some (Conn _ | Fil _ | Pfd _) ->
        Error (Op.Other "ENOTSOCK")
    | None ->
        Error Op.Bad_fd )
  | Op.Accept lfd -> (
    match Hashtbl.find_opt n.fds lfd with
    | Some (Lsn l) -> (
        (* As on Linux, the check comes before the backlog, and the connection
           stays in the backlog. *)
        let* () = room n 1 in
        match Queue.take_opt l.backlog with
        | Some s ->
            s.cfd <- new_fd n ;
            Hashtbl.replace n.fds s.cfd (Conn s) ;
            (* An accepted socket gets the options of its listener. *)
            Option.iter
              (fun o -> Hashtbl.replace n.opts s.cfd (copy_opts o))
              (Hashtbl.find_opt n.opts lfd) ;
            Ok (s.cfd, s.remote)
        | None ->
            Error Op.Would_block )
    | Some (Dgm _) ->
        Error (Op.Other "EOPNOTSUPP")
    | Some (Conn _ | Fil _ | Pfd _) ->
        Error (Op.Other "EINVAL")
    | None ->
        Error Op.Bad_fd )
  | Op.Read (fd, buf, off, len) -> (
    match Hashtbl.find_opt n.fds fd with
    | Some (Dgm g) ->
        Result.map (fun (k, _, _) -> k) (recv_dgram g buf off len)
    | _ ->
        conn fd (fun c -> read_conn t c buf off len) )
  | Op.Write (fd, buf, off, len) -> (
    match Hashtbl.find_opt n.fds fd with
    | Some (Dgm _) ->
        Error (Op.Other "EDESTADDRREQ")
    | _ ->
        conn fd (fun c -> write_conn t c buf off len) )
  | Op.Udp (host, port) ->
      let* () = room n 1 in
      let key p = Printf.sprintf "%s:%d" host p in
      let port =
        if port = 0 then fresh_port n (fun p -> Hashtbl.mem n.udp_names (key p))
        else port
      in
      let k = key port in
      if
        (port < 0) [@mutate off "the port 0 got a free port before this check"]
        || port > 65535
      then Error (Op.Other "EINVAL")
      else if Hashtbl.mem n.udp_names k then Error Op.In_use
      else
        let fd = new_fd n in
        Hashtbl.replace n.fds fd
          (Dgm {host; port; dkey= k; queue= Queue.create (); bytes= 0}) ;
        Hashtbl.replace n.udp_names k fd ;
        Ok fd
  | Op.Sendto (fd, buf, off, len, host, port) -> (
    match Hashtbl.find_opt n.fds fd with
    | Some (Dgm g) ->
        send_dgram t g (Bytes.sub_string buf off len) host port
    | Some (Conn _ | Lsn _) ->
        Error (Op.Other "EISCONN")
    | Some (Fil _ | Pfd _) ->
        Error (Op.Other "ENOTSOCK")
    | None ->
        Error Op.Bad_fd )
  | Op.Recvfrom (fd, buf, off, len) -> (
    match Hashtbl.find_opt n.fds fd with
    | Some (Dgm g) ->
        recv_dgram g buf off len
    | Some (Conn _ | Lsn _) ->
        Error (Op.Other "EOPNOTSUPP")
    | Some (Fil _ | Pfd _) ->
        Error (Op.Other "ENOTSOCK")
    | None ->
        Error Op.Bad_fd )
  | Op.Set_option (fd, opt, v) ->
      with_opts t fd opt (fun o -> set_opt o opt v)
  | Op.Get_option (fd, opt) ->
      with_opts t fd opt (fun o -> get_opt o opt)
  | Op.Pipe ->
      let* () = room n 2 in
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
        | None, (Op.SIGSTOP | SIGTSTP | SIGCONT | SIGCHLD | SIGWINCH) ->
            (* The default action of these signals does not end the process, and
               signal 0 only tells that the process exists. *)
            ()
        | None, Op.Other_signal 0 ->
            ()
        | None, _ when sg <> Op.SIGKILL && List.mem sg p.prog.ignores ->
            ()
        | None, _ ->
            exit_proc t fd p (Op.Signaled sg) ) ;
        Ok ()
    | Some _ ->
        Error (Op.Other "EINVAL")
    | None ->
        Error Op.Bad_fd )
  | Op.Watch sg ->
      if List.mem sg t.watched then Ok sg
      else Error (Op.Other "the world does not watch this signal")
  | Op.Unwatch sg ->
      if List.mem sg t.watched then (
        t.watched <- List.filter (fun x -> x <> sg) t.watched ;
        Ok sg )
      else Error (Op.Other "the world does not watch this signal")
  | Op.Resend sg ->
      send_self t sg ; Ok ()
  | Op.Fd_read (fd, buf, off, len) -> (
    match Hashtbl.find_opt n.fds fd with
    | Some (Conn ({role= Rd_end | Sock; _} as c)) ->
        read_conn t c buf off len
    | Some (Conn {role= Wr_end; _}) | None ->
        Error Op.Bad_fd
    | Some (Lsn _ | Dgm _ | Fil _ | Pfd _) ->
        Error (Op.Other "EINVAL") )
  | Op.Fd_write (fd, buf, off, len) -> (
    match Hashtbl.find_opt n.fds fd with
    | Some (Conn ({role= Wr_end | Sock; _} as c)) ->
        write_conn t c buf off len
    | Some (Conn {role= Rd_end; _}) | None ->
        Error Op.Bad_fd
    | Some (Lsn _ | Dgm _ | Fil _ | Pfd _) ->
        Error (Op.Other "EINVAL") )
  | Op.Shutdown fd ->
      conn fd (fun c ->
          c.shut <- true ;
          ( match c.peer with
          | Some p ->
              later_to t p (fun () ->
                  p.eof <- true ;
                  edge n p.cfd ~read:true ~write:false )
          | None ->
              () ) ;
          Ok () )
  | Op.Reset_on_close fd -> (
    match Hashtbl.find_opt n.fds fd with
    | Some (Conn {role= Rd_end | Wr_end; _} | Fil _ | Pfd _) ->
        Error (Op.Other "ENOTSOCK")
    | Some (Conn c) ->
        c.abort <- true ;
        Ok ()
    | Some (Lsn _ | Dgm _) ->
        Ok ()
    | None ->
        Error Op.Bad_fd )
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
          Hashtbl.remove n.opts fd ;
          ( match sfd with
          | Conn c ->
              (* Linux also sends a reset at the close of a TCP or Unix stream
                 socket with unread data. *)
              if c.abort || (c.role = Sock && avail c > 0) then reset_conn t c
              else close_conn t c
          | Dgm g ->
              Hashtbl.remove n.udp_names g.dkey
          | Lsn l ->
              Hashtbl.remove n.names l.key ;
              Queue.iter
                (fun s ->
                  match s.peer with
                  | Some c when Option.is_some c.peer ->
                      (* The connection is established: it gets a reset, as from
                         [reset_conn]. *)
                      c.reset <- true ;
                      c.eof <- true ;
                      c.peer <- None ;
                      edge n c.cfd ~read:true ~write:true
                  | _ ->
                      (* The client has no peer after a [Sim.fault]. *)
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

(* With [extra_edges], a wait of a domain gets an edge with no change, for a
   registration of that domain, one time in 8 (design section 2.8, The model of
   the fds). *)
let extra_edge t d =
  if Prng.int t.prng 8 = 0 then
    let mine =
      Hashtbl.fold
        (fun _ l acc ->
          List.filter_map
            (fun (d', token) ->
              if
                ( d' = d )
                [@mutate
                  off
                    "a token of another domain goes to a domain with no \
                     registration, which drops it: only the number of edges \
                     changes"]
              then Some token
              else None )
            l
          @ acc )
        t.net.regs []
    in
    match mine with
    | [] ->
        ()
    | l ->
        let token = List.nth l (Prng.int t.prng (List.length l)) in
        t.net.edges <-
          (d, World.Ready.Fd {token; read= true; write= true}) :: t.net.edges

(* Answer the effect of domain [d], continue it, and keep its next state. *)
(* A switch point with no progress: only [Now], [Switch], [Random], or a [Wait]
   that gives nothing. A loop that yields until a time comes makes no progress,
   because the time moves only at a quiescence (design section 2.8,
   Livelock). *)
let no_progress t =
  t.no_progress <- t.no_progress + 1 ;
  if (t.no_progress > t.livelock) [@mutate off "the limit or one step less"]
  then raise (Livelock (Sched_debug.active (sched t)))

let progress t = t.no_progress <- 0

let step t st d =
  (* A fiber that a commit resumed is progress, for example on a channel. *)
  let resumes = (sched t).doms.(d).stats.resumes in
  ( if resumes <> t.resumes_seen.(d) then (
      t.resumes_seen.(d) <- resumes ;
      progress t )
    else
      match st.(d) with
      | Pending ((World.Now _ | World.Switch | World.Random _), _) ->
          no_progress t
      | Pending (World.Wait _, _) when not (has_ready t d) ->
          no_progress t
      | _ ->
          progress t ) ;
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
      | World.Submit op -> (
          let v = submit t op in
          (* A signal ended the process: no domain continues. *)
          match t.killed with
          | Some sg ->
              raise (Killed sg)
          | None ->
              go v )
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
          if t.net.extra_edges then extra_edge t d ;
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
  progress t ;
  let s = sched t in
  ( match
      Sched_debug.lost s
        ~now:(Weft.Clock.of_ns Mono t.now)
        ~posix:(Weft.Clock.of_ns Posix (Int64.add t.posix0 t.now))
      @ fd_problems t
    with
  | [] ->
      ()
  | l ->
      raise (Lost_wakeup l) ) ;
  (* A [Posix] deadline comes at the [Mono] time where [posix0 + now] reaches
     it. The alarm of each line goes to its domain. *)
  let deadlines =
    List.concat_map
      (fun d ->
        match st.(d) with
        | Sleeping (w, _) ->
            Option.to_list
              (Option.map
                 (fun m -> (d, Weft.Clock.to_ns Mono m, World.Ready.Alarm))
                 w.deadline )
            @ Option.to_list
                (Option.map
                   (fun p ->
                     ( d
                     , Int64.sub (Weft.Clock.to_ns Posix p) t.posix0
                     , World.Ready.Posix_alarm ) )
                   w.posix_deadline )
        | _ ->
            [] )
      (List.init (Array.length st) Fun.id)
  in
  let deadline =
    List.fold_left (fun acc (_, at, _) -> earliest acc (Some at)) None deadlines
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
      (fun (d, dl, alarm) -> if Int64.compare dl t.now <= 0 then give d alarm)
      deadlines
  in
  if Atomic.get t.refused_fill then
    model_error
      "a fill from outside a fiber, with no Sim.run ~outside_fills:true" ;
  match (Atomic.exchange t.fills [], t.settles) with
  | (_ :: _ as fills), _ ->
      (* The fills go to the injection queue, and a domain wakes. *)
      List.iter (Sched.inject s) (List.rev fills) ;
      give first World.Ready.Woken
  | [], _ :: _ ->
      List.iter
        (fun id -> give first (World.Ready.External id))
        (List.rev t.settles) ;
      t.settles <- []
  | [], [] -> (
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

let run ?(seed = 0) ?(domains = 1) ?(auto = true) ?(signals = [])
    ?(outside_fills = false) ?(livelock = 10_000_000) main =
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
        ; udp_names= Hashtbl.create ~random:false 8
        ; opts= Hashtbl.create ~random:false 8
        ; regs= Hashtbl.create ~random:false 16
        ; edges= []
        ; latency= 0L
        ; jitter= 0L
        ; short_io= false
        ; extra_edges= false
        ; udp_loss= false
        ; resets= false
        ; next_port= 40000
        ; fd_limit= None }
    ; agenda= []
    ; next_id= 0
    ; settles= []
    ; advance= None
    ; fs= Sim_fs.create ()
    ; jobs= Hashtbl.create ~random:false 16
    ; blocking_cost= 0L
    ; dns= Hashtbl.create ~random:false 8
    ; programs= Hashtbl.create ~random:false 8
    ; watched= signals
    ; killed= None
    ; fills= Atomic.make []
    ; refused_fill= Atomic.make false
    ; resumes_seen= Array.make domains 0
    ; livelock
    ; no_progress= 0
    ; next_pid= 1000 }
  in
  let s = Sched.create ~slack:Weft.Span.zero ~simulated:true ~domains () in
  t.sched <- Some s ;
  (* A fill from outside a fiber waits for the quiescence. The GC selects when a
     finaliser runs, so a seed gives the same run only in the same binary with
     the same history (design section 2.9). *)
  let slot =
    Some
      ( s
      , fun req ->
          if outside_fills then Sched.push_list t.fills req
          else Atomic.set t.refused_fill true )
  in
  Atomic.set Sched.outside slot ;
  (* The promises of this run that failed and that no fiber awaited. *)
  let unawaited = Atomic.make [] in
  let old_report = Atomic.get Weft.Private.promise_report in
  Atomic.set Weft.Private.promise_report (fun run e ->
      match run with
      | Some r when r = s.run_id ->
          Sched.push_list unawaited
            (try Printexc.to_string e with _ -> "an exception")
      | _ ->
          old_report run e ) ;
  (* Also when the run raises: the next runs must not get these hooks. A run can
     end with fibers that wait (Deadlock, Livelock or an exception). Their syncs
     become [Done], so a fill or a send of a later run finds them stale and
     never resumes a fiber of this run. *)
  let restore () =
    ignore (Atomic.compare_and_set Sched.outside slot None) ;
    Atomic.set Weft.Private.promise_report old_report ;
    Sched.Spin.with_lock s.fibers_lock (fun () ->
        Hashtbl.iter
          (fun _ (f : Sched.fiber) ->
            Option.iter
              (fun (y : Sched.sync) -> ignore (Sched.C.notify y.word (-1)))
              (Atomic.get f.current) )
          s.fibers )
  in
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
  match loop () ; result () with
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      restore () ;
      Printexc.raise_with_backtrace e bt
  | v -> (
      (* The queues are empty, but their old slots keep the closures of the
         fibers that ran, and thus their promises. *)
      Array.iter
        (fun d ->
          Array.iter (fun slot -> Atomic.set slot None) d.Sched.ring.Ring.buf ;
          Atomic.set d.Sched.next None )
        s.doms ;
      (* The run holds no promise at this time: a full GC runs the finalisers of
         its promises. [Gc.finalise_last] needs two cycles. *)
      if Atomic.get s.unawaited > 0 then (Gc.full_major () ; Gc.full_major ()) ;
      restore () ;
      match Atomic.get unawaited with
      | [] ->
          v
      | l ->
          raise (Unawaited (List.rev l)) )

let settle t =
  let id = fresh t in
  t.settles <- id :: t.settles ;
  Job.wait id

let advance_until t target =
  if t.auto then invalid_arg "Sim.advance_until: the simulator is in auto mode" ;
  if Option.is_some t.advance then
    invalid_arg "Sim.advance_until: an advance is already active" ;
  let id = fresh t in
  t.advance <- Some (Weft.Clock.to_ns Mono target, id) ;
  (* A cancel ends the advance. *)
  match Job.wait id with
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

(* A step of the wall clock. As TFD_TIMER_CANCEL_ON_SET does on Linux, the step
   ends the wait of each domain for its [Posix] alarm. The [Mono] line does not
   change. *)
let set_posix t p =
  t.posix0 <- Int64.sub (Weft.Clock.to_ns Posix p) t.now ;
  Array.iteri
    (fun d _ -> t.net.edges <- (d, World.Ready.Posix_alarm) :: t.net.edges)
    (sched t).doms

type net_faults =
  { jitter: Weft.Span.t
  ; short_io: bool
  ; extra_edges: bool
  ; udp_loss: bool
  ; resets: bool }

let no_faults =
  { jitter= Weft.Span.zero
  ; short_io= false
  ; extra_edges= false
  ; udp_loss= false
  ; resets= false }

let net_faults t (f : net_faults) =
  if Int64.compare (Weft.Span.to_ns f.jitter) 0L < 0 then
    invalid_arg "Sim.net_faults: the jitter is negative" ;
  t.net.jitter <- Weft.Span.to_ns f.jitter ;
  t.net.short_io <- f.short_io ;
  t.net.extra_edges <- f.extra_edges ;
  t.net.udp_loss <- f.udp_loss ;
  t.net.resets <- f.resets

let set_fd_limit t limit =
  ( match limit with
  | Some l when l < 0 ->
      invalid_arg "Sim.set_fd_limit: the limit is negative"
  | _ ->
      () ) ;
  t.net.fd_limit <- limit

(* A connection with no listener, as after a connect and an accept (design
   section 2.8, A socket pair). It has no latency for the connect, and it counts
   2 fds against the limit. The pair is not a world effect: the simulator runs
   one virtual domain at a time, so the change of the network has no race. *)
let socket_pair t =
  let n = t.net in
  ( match room n 2 with
  | Error e ->
      raise (Net.Error (e, "Sim.socket_pair"))
  | Ok () ->
      () ) ;
  let client = Op.Tcp ("client", fresh_port n (fun _ -> false)) in
  let server =
    let used p = Hashtbl.mem n.names (key (Op.Tcp ("server", p))) in
    Op.Tcp ("server", fresh_port n used)
  in
  let c = new_sock ~tcp:true ~local:client server
  and s = new_sock ~tcp:true ~local:server client in
  c.peer <- Some s ;
  s.peer <- Some c ;
  List.iter
    (fun x ->
      x.established <- true ;
      x.cfd <- new_fd n ;
      Hashtbl.replace n.fds x.cfd (Conn x) )
    [c; s] ;
  (Sched.Fdtab.alloc c.cfd, Sched.Fdtab.alloc s.cfd)

(* A fault of one connection (design section 2.8, A fault of one connection).
   The connection breaks at the side of [fd]: its next read with no data, or its
   next write, gives [e] one time, as the error of a socket on Linux. Then it
   has the end of the stream and no peer. A [Reset] also goes to the peer after
   the delay, as from a middlebox. After another error, the link is lost: the
   peer gets nothing, and its window stays full. *)
let fault t (fd : Net.t) e =
  ( match e with
  | Op.Reset | Broken_pipe | Refused | No_resources _ | Other _ ->
      ()
  | _ ->
      invalid_arg "Sim.fault: the error is not an error of a connection" ) ;
  if not (Sched.Rc.acquire fd) then raise (Net.Error (Op.Bad_fd, "Sim.fault")) ;
  Fun.protect
    ~finally:(fun () -> Sched.Rc.release fd)
    (fun () ->
      let n = t.net in
      match Hashtbl.find_opt n.fds fd.os with
      | Some (Conn ({role= Sock; _} as c)) ->
          (* The socket has one slot for its error, as on Linux: the fault
             replaces an error that the socket did not give. *)
          ( match e with
          | Op.Reset ->
              c.err <- None ;
              c.reset <- true
          | e ->
              c.reset <- false ;
              c.err <- Some e ;
              c.lost <- true ) ;
          ( match c.peer with
          | Some p ->
              c.peer <- None ;
              if e = Op.Reset then send_reset t c p
          | None ->
              () ) ;
          c.eof <- true ;
          (* A fiber that waits on this side gets the error. *)
          edge n c.cfd ~read:true ~write:true
      | _ ->
          invalid_arg "Sim.fault: the fd is not a connected socket" )

let set_blocking_cost t span =
  if Int64.compare (Weft.Span.to_ns span) 0L < 0 then
    invalid_arg "Sim.set_blocking_cost: the span is negative" ;
  t.blocking_cost <- Weft.Span.to_ns span

let set_dns t name answer = Hashtbl.replace t.dns name answer

let program ?runs ?(code = 0) ?(output = "") ?(ignores = []) () =
  {runs= Option.map Weft.Span.to_ns runs; code; output; ignores}

let add_program t name f = Hashtbl.replace t.programs name f

(* A signal that the program does not watch, or no more, gets its action at
   once: [send_self] sets [killed] in the [Resend] of a world effect, and the
   step that answers the effect raises [Killed]. *)
let signal t sg =
  if List.mem sg t.watched then send_self t sg
  else ignore (World.perform "Sim.signal" (World.Submit (Op.Resend sg)))

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
