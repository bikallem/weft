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
      ( n <= 0 )
      [@mutate off "the scheduler asks only for n >= 2; Int64.rem fails at 0"]
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

type conn =
  { inbox: Buffer.t  (** The bytes to read, from [rpos]. *)
  ; mutable rpos: int
  ; mutable inflight: int  (** Bytes that the latency still holds. *)
  ; mutable peer: conn option  (** [None] after the peer closed. *)
  ; mutable eof: bool  (** The peer ended its send side. *)
  ; mutable shut: bool  (** This side ended its send side. *)
  ; mutable established: bool
  ; mutable refused: bool
  ; mutable cfd: int  (** [-1] until the server side is accepted. *)
  ; remote: Op.addr }

type listener = {key: string; backlog: conn Queue.t; limit: int}

type sfd = Lsn of listener | Conn of conn

type net =
  { fds: (int, sfd) Hashtbl.t
  ; mutable next_fd: int
  ; names: (string, int) Hashtbl.t  (** The listeners, by address. *)
  ; regs: (int, (int * int64) list) Hashtbl.t
        (** The registrations of each fd: the domain and the token. *)
  ; mutable edges: (int * World.Ready.t) list
        (** The edges to give, by domain, the newest first. *)
  ; mutable latency: int64
  ; mutable next_port: int }

type t =
  { seed: int
  ; prng: Prng.t
  ; auto: bool
  ; mutable now: int64  (** The virtual [Mono] time, in nanoseconds. *)
  ; posix0: int64  (** [Posix] at [Mono] 0. *)
  ; mutable woken: bool
  ; mutable sched: Sched.t option
  ; net: net
  ; mutable agenda: (int64 * (unit -> unit)) list
        (** The timed events of the simulator, in order. An event comes after
            the events with the same time that came before it. *)
  ; mutable next_id: int
  ; mutable settles: int list
  ; mutable advance: (int64 * int) option }

let seed t = t.seed

let sched t = Option.get t.sched

let key addr = Format.asprintf "%a" Op.pp_addr addr

let avail c = Buffer.length c.inbox - c.rpos

let readable n fd =
  match Hashtbl.find_opt n.fds fd with
  | Some (Lsn l) ->
      not (Queue.is_empty l.backlog)
  | Some (Conn c) ->
      avail c > 0 || c.eof || c.refused
  | None ->
      false

let writable n fd =
  match Hashtbl.find_opt n.fds fd with
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
  | Some (Lsn _) | None ->
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

let submit : type a. t -> a Op.t -> (a, Op.error) result =
 fun t op ->
  let n = t.net in
  let conn fd k =
    match Hashtbl.find_opt n.fds fd with
    | Some (Conn c) ->
        k c
    | Some (Lsn _) ->
        Error (Op.Other "ENOTCONN")
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
          (Lsn {key= k; backlog= Queue.create (); limit= backlog}) ;
        Hashtbl.replace n.names k fd ;
        Ok fd
  | Op.Connect addr -> (
      let refused () = Error Op.Refused in
      match Hashtbl.find_opt n.names (key addr) with
      | None ->
          refused ()
      | Some lfd -> (
        match Hashtbl.find_opt n.fds lfd with
        | Some (Lsn l) when Queue.length l.backlog < l.limit ->
            let port = n.next_port in
            n.next_port <-
              (port + 1) [@mutate off "a counter that goes down is unique too"] ;
            let mk remote =
              { inbox= Buffer.create 64
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
    | Some (Conn _) ->
        Error (Op.Other "EINVAL")
    | None ->
        Error Op.Bad_fd )
  | Op.Read (fd, buf, off, len) ->
      conn fd (fun c ->
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
          else Error Op.Would_block )
  | Op.Write (fd, buf, off, len) ->
      conn fd (fun c ->
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
                  later t (fun () ->
                      p.inflight <- p.inflight - k ;
                      Buffer.add_string p.inbox data ;
                      edge n p.cfd ~read:true ~write:false ) ;
                  Ok k )
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
      match Hashtbl.find_opt n.fds fd with
      | None ->
          Error Op.Bad_fd
      | Some sfd ->
          Hashtbl.remove n.fds fd ;
          ( match sfd with
          | Conn c -> (
            match c.peer with
            | Some p ->
                c.peer <- None ;
                later t (fun () ->
                    p.eof <- true ;
                    p.peer <- None ;
                    edge n p.cfd ~read:true ~write:true )
            | None ->
                () )
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
                l.backlog ) ;
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

(* The program is quiescent: the domain sleeps, and nothing is ready (design
   section 2.8, The wait steps). *)
let quiescent t (w : World.Wait.t) =
  let s = sched t in
  ( match
      Sched_debug.lost s ~now:(Weft.Clock.of_ns Mono t.now) @ fd_problems t
    with
  | [] ->
      ()
  | l ->
      raise (Lost_wakeup l) ) ;
  let deadline = Option.map (Weft.Clock.to_ns Mono) w.deadline in
  let next_agenda =
    match t.agenda with (at, _) :: _ -> Some at | [] -> None
  in
  (* Move the time to [at] and run the agenda. The scheduler reads the clock to
     expire its timers, so the alarm tells only that the time moved. *)
  let move at extra =
    t.now <- Int64.max t.now at ;
    run_agenda t ;
    (World.Ready.Alarm :: extra) @ take_edges t 0
  in
  match t.settles with
  | _ :: _ ->
      let ids = List.rev t.settles in
      t.settles <- [] ;
      List.map (fun id -> World.Ready.External id) ids
  | [] -> (
      if t.auto then
        match earliest deadline next_agenda with
        | Some at ->
            move at []
        | None ->
            raise (Deadlock (Sched_debug.blocked s))
      else
        match t.advance with
        | Some (target, id) -> (
          match earliest (earliest deadline next_agenda) (Some target) with
          | Some at ->
              let reached = Int64.compare at target >= 0 in
              if reached then t.advance <- None ;
              move at (if reached then [World.Ready.External id] else [])
          | None ->
              assert false )
        | None ->
            raise
              (Deadlock
                 ( "manual mode: only advance_until moves the time"
                 :: Sched_debug.blocked s ) ) )

let run ?(seed = 0) ?(auto = true) main =
  let t =
    { seed
    ; prng= Prng.make seed
    ; auto
    ; now= 0L
    ; posix0= 1_700_000_000_000_000_000L
    ; woken= false
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
    ; advance= None }
  in
  let open Effect.Deep in
  match_with
    (fun () ->
      Sched.run ~slack:Weft.Span.zero
        ~on_create:(fun s -> t.sched <- Some s)
        (fun () -> main t) )
    ()
    { retc= Fun.id
    ; exnc=
        (fun e ->
          Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()) )
    ; effc=
        (fun (type a) (e : a Effect.t) ->
          match e with
          | World.Now line ->
              Some
                (fun (k : (a, _) continuation) ->
                  let ns =
                    match line with
                    | Weft.Clock.Mono ->
                        t.now
                    | Weft.Clock.Posix ->
                        Int64.add t.posix0 t.now
                  in
                  continue k (Weft.Clock.of_ns line ns) )
          | World.Submit op ->
              Some (fun k -> continue k (submit t op))
          | World.Random n ->
              Some (fun k -> continue k (Prng.int t.prng n))
          | World.Random_bytes b ->
              Some (fun k -> continue k (Prng.bytes t.prng b))
          | World.Switch _ ->
              Some (fun k -> continue k ())
          | World.Wake _ ->
              Some
                (fun k ->
                  t.woken <- true ;
                  continue k () )
          | World.Wait (d, w) ->
              Some
                (fun k ->
                  let woken = if t.woken then [World.Ready.Woken] else [] in
                  t.woken <- false ;
                  match (woken @ take_edges t d, w.timeout) with
                  | (_ :: _ as ready), _ ->
                      continue k ready
                  | [], World.Wait.Poll ->
                      continue k []
                  | [], World.Wait.Sleep ->
                      continue k (quiescent t w) )
          | _ ->
              None ) }

let fresh t =
  let id = t.next_id in
  t.next_id <- (id + 1) [@mutate off "a counter that goes down is unique too"] ;
  id

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
  wait_external id

let set_latency t span = t.net.latency <- Weft.Span.to_ns span

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
