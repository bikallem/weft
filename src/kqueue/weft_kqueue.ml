(* The kqueue world: macOS and FreeBSD (design section 2.7). Each domain has a
   kqueue and a wake pipe. The alarm is the timeout of kevent. Domain 0 gets the
   signals (EVFILT_SIGNAL) and the exits of the children (EVFILT_PROC). The
   parts that the POSIX worlds share are in Weft_posix. *)

open Weft.Private
open Weft_posix

external kq_create : unit -> int = "weft_kq_create"

external kq_add : int -> int -> int -> int = "weft_kq_add"

external kq_add_in : int -> int -> int -> int = "weft_kq_add_in"

external kq_del : int -> int -> int = "weft_kq_del"

external kq_signal : int -> int -> int -> int = "weft_kq_signal"

external kq_proc : int -> int -> int -> int = "weft_kq_proc"

external kq_wait : int -> int64 -> (int * bool * bool * int) array
  = "weft_kq_wait"

external kq_spawn :
     string * string array * string array * bool * string * int array
  -> int * int * int * int * int = "weft_kq_spawn"

external kq_wait_child : int -> int * int = "weft_kq_wait_child"

external kq_kill : int -> int -> int = "weft_kq_kill"

(* The tokens of the world. A token of an fd is (slot lsl 32) lor gen, so it is
   never negative. *)
let wake_token = -2

let signal_token n = -100 - n

let is_signal token = token <= -100 && token > -1000

let proc_token pid = -1000 - pid

type dom = {kq: int; wake_r: int; wake_w: int}

(* A child: the pipe that becomes readable at its exit. The world writes the
   pipe when domain 0 gets the exit. *)
type proc = {pid: int; exit_r: int; exit_w: int; mutable reaped: bool}

type t =
  { doms: dom array
  ; rng: int Atomic.t
  ; pool: pool
  ; watched: Op.signal list  (** The signals of [run ~signals]. *)
  ; old_actions: (int * Sys.signal_behavior) list
        (** The actions of the signals before [create]. *)
  ; lock: Mutex.t
  ; procs: (int, proc) Hashtbl.t  (** By the read end of the exit pipe. *)
  ; pids: (int, proc) Hashtbl.t  (** By pid. *) }

let make_dom () =
  let kq = check "kqueue" (kq_create ()) in
  let r, w = pipe () in
  let wake_r = check "pipe" r in
  ignore (check "kevent" (kq_add_in kq wake_r wake_token)) ;
  {kq; wake_r; wake_w= w}

(* A watched signal gets the action "ignore": EVFILT_SIGNAL records it, and the
   process does not end. SIGCHLD keeps its action, because an ignored SIGCHLD
   reaps the children, and EVFILT_SIGNAL does not record it. *)
let create ?(domains = 1) ?(signals = []) () =
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
  let nums = List.sort_uniq compare (List.map number_of signals) in
  let doms = Array.init domains (fun _ -> make_dom ()) in
  let chld = number_of Op.SIGCHLD in
  let old_actions =
    List.filter_map
      (fun n ->
        ignore (check "kevent" (kq_signal doms.(0).kq n (signal_token n))) ;
        if n = chld then None else Some (n, Sys.signal n Sys.Signal_ignore) )
      nums
  in
  { doms
  ; rng= new_rng ()
  ; pool= create_pool (Array.map (fun d -> d.wake_w) doms)
  ; watched= List.map (fun s -> signal_of (number_of s)) signals
  ; old_actions
  ; lock= Mutex.create ()
  ; procs= Hashtbl.create ~random:false 8
  ; pids= Hashtbl.create ~random:false 8 }

let close t =
  close_pool t.pool ;
  List.iter (fun (n, a) -> Sys.set_signal n a) t.old_actions ;
  Hashtbl.iter
    (fun _ p ->
      ignore (close_fd p.exit_r) ;
      ignore (close_fd p.exit_w) )
    t.procs ;
  Array.iter
    (fun d ->
      ignore (close_fd d.wake_r) ;
      ignore (close_fd d.wake_w) ;
      ignore (close_fd d.kq) )
    t.doms

let esrch = Error (Op.Other "ESRCH")

(* The exit fd of a child is a pipe. The tables have the child before domain 0
   can get its exit. If the child already exited, the pipe is readable at
   once. *)
let spawn t sp =
  match spawned_of (kq_spawn (spawn_args sp)) with
  | Error _ as e ->
      e
  | Ok (s : Op.spawned) -> (
      let r, w = pipe () in
      match result r with
      | Error e ->
          Error e
      | Ok r ->
          let p = {pid= s.pid; exit_r= r; exit_w= w; reaped= false} in
          Mutex.protect t.lock (fun () ->
              Hashtbl.replace t.procs r p ;
              Hashtbl.replace t.pids s.pid p ) ;
          if kq_proc t.doms.(0).kq s.pid (proc_token s.pid) < 0 then poke w ;
          Ok {s with pidfd= r} )

let proc t fd = Mutex.protect t.lock (fun () -> Hashtbl.find_opt t.procs fd)

module World = struct
  type nonrec t = t

  let now _ line = now line

  let submit : type a. t -> a Op.t -> (a, Op.error) result =
   fun t op ->
    match op with
    | Op.Spawn sp ->
        if spawn_has_nul sp then nul_error else spawn t sp
    | Op.Wait_child fd -> (
      match proc t fd with
      | Some p when not p.reaped ->
          let r = status_of (kq_wait_child p.pid) in
          (match r with Ok (Some _) -> p.reaped <- true | _ -> ()) ;
          r
      | Some _ ->
          Error (Op.Other "ECHILD")
      | None ->
          Error Op.Bad_fd )
    | Op.Kill (fd, s) -> (
      match proc t fd with
      | Some p when not p.reaped ->
          unit (kq_kill p.pid (number_of s))
      | Some _ ->
          esrch
      | None ->
          Error Op.Bad_fd )
    | Op.Watch s ->
        let s = signal_of (number_of s) in
        if List.mem s t.watched then Ok s
        else Error (Op.Other "the world does not watch this signal")
    | Op.Register (d, fd, token) ->
        unit (kq_add t.doms.(d).kq fd token)
    | Op.Deregister (d, fd) ->
        unit (kq_del t.doms.(d).kq fd)
    | Op.Close fd when Option.is_some (proc t fd) ->
        let p =
          Mutex.protect t.lock (fun () ->
              let p = Hashtbl.find t.procs fd in
              Hashtbl.remove t.procs fd ;
              Hashtbl.remove t.pids p.pid ;
              p )
        in
        ignore (close_fd p.exit_w) ;
        unit (close_fd p.exit_r)
    | op -> (
      match Weft_posix.submit t.pool op with
      | Some r ->
          r
      | None ->
          assert false (* The arms above take each other operation. *) )

  (* The timeout of kevent: the earliest deadline, or none. A step of the wall
     clock does not end a wait, so a wait for a [Posix] deadline ends after 1 s
     at most, and the scheduler examines the clock again. *)
  let timeout_ns (w : World.Wait.t) =
    match w.timeout with
    | World.Wait.Poll ->
        0L
    | World.Wait.Sleep -> (
        let left line d now =
          Int64.max 0L (Int64.sub (Weft.Clock.to_ns line d) now)
        in
        let m = Option.map (fun d -> left Mono d (mono_ns ())) w.deadline in
        let p =
          Option.map
            (fun d -> Int64.min 1_000_000_000L (left Posix d (posix_ns ())))
            w.posix_deadline
        in
        match (m, p) with
        | None, None ->
            -1L
        | Some x, None | None, Some x ->
            x
        | Some x, Some y ->
            Int64.min x y )

  (* The exit of a child: its pipe becomes readable. *)
  let exited t token =
    match
      Mutex.protect t.lock (fun () -> Hashtbl.find_opt t.pids (-1000 - token))
    with
    | Some p ->
        poke p.exit_w
    | None ->
        ()

  let wait t ~domain (w : World.Wait.t) =
    let d = t.doms.(domain) in
    Array.fold_right
      (fun (token, read, write, data) acc ->
        if token = wake_token then (
          drain_all d.wake_r ;
          World.Ready.Woken :: completions t.pool domain acc )
        else if is_signal token then
          if (data > 0) [@mutate off "a count of 0 commits no waiter"] then
            World.Ready.Signal (signal_of (-100 - token), data) :: acc
          else acc
        else if token <= -1000 then (exited t token ; acc)
        else World.Ready.Fd {token; read; write} :: acc )
      (kq_wait d.kq (timeout_ns w))
      []

  let wake t ~domain = poke t.doms.(domain).wake_w

  let random t n = next t.rng mod n

  let random_bytes _ b = ignore (getrandom b)
end

let default_domains = default_domains

let watchdog_reports = watchdog_reports

let pool_warnings = pool_warnings

let run ?slack ?domains ?signals ?(watchdog = Some (Weft.Span.ms 10)) main =
  let domains = match domains with Some n -> n | None -> default_domains () in
  Weft_posix.run ~name:"Weft_kqueue.run"
    (module World)
    (create ~domains ?signals ())
    ~close ?slack ~domains ~watchdog main

let local_port fd = local_port ~name:"Weft_kqueue.local_port" fd
