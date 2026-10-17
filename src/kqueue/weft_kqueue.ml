(* The kqueue world: macOS and FreeBSD (design section 2.7). Each domain has a
   kqueue and a wake pipe. The alarm is the timeout of kevent. Domain 0 gets the
   signals (a handler of the world writes its wake pipe) and the exits of the
   children (EVFILT_PROC). The parts that the POSIX worlds share are in
   Weft_posix. *)

open Weft.Private
open Weft_posix

external kq_create : unit -> int = "weft_kq_create"

external kq_add : int -> int -> int -> int = "weft_kq_add"

external kq_add_in : int -> int -> int -> int = "weft_kq_add_in"

external kq_del : int -> int -> int = "weft_kq_del"

external sig_claim : int -> bool = "weft_kq_sig_claim"

external sig_release : unit -> unit = "weft_kq_sig_release"

external sig_catch : int -> bytes = "weft_kq_sig_catch"

external sig_restore : int -> bytes -> unit = "weft_kq_sig_restore"

external sig_take : int -> int = "weft_kq_sig_take"

external kq_proc : int -> int -> int -> int = "weft_kq_proc"

external kq_wait : int -> int64 -> (int * bool * bool) array = "weft_kq_wait"

external kq_wait_child : int -> int * int = "weft_kq_wait_child"

external kq_zombie : int -> unit = "weft_kq_zombie"

external drain_all : int -> unit = "weft_drain_all"

external poke : int -> unit = "weft_poke"

external kq_kill : int -> int -> int = "weft_kq_kill"

(* The tokens of the world. A token of an fd is (slot lsl 32) lor gen, so it is
   never negative. *)
let wake_token = -2

let proc_token pid = -1000 - pid

let pid_of_token token = -1000 - token

type dom = {kq: int; wake_r: int; wake_w: int}

(* A child: the pipe that becomes readable at its exit. The world writes the
   pipe when domain 0 gets the exit. The kernel can tell of the exit before the
   child is a zombie, so [exit_seen] tells the reap to wait for the zombie.
   [reaped] changes only under the lock of the world, with the reap. *)
type proc =
  { pid: int
  ; exit_r: int
  ; exit_w: int
  ; exit_seen: bool Atomic.t
  ; mutable reaped: bool }

type t =
  { doms: dom array
  ; pool: pool
  ; watched: Op.signal list Atomic.t
        (** The signals of [run ~signals], with no signal of an unwatch. *)
  ; old_actions: (int * bytes) list
        (** The numbers of the signals of [run ~signals], and their actions
            before [create]. *)
  ; lock: Mutex.t
  ; procs: (int, proc) Hashtbl.t  (** By the read end of the exit pipe. *)
  ; pids: (int, proc) Hashtbl.t  (** By pid. *)
  ; closed: bool Atomic.t  (** [close] sets it, one time. *) }

let close_dom d =
  ignore (close_fd d.wake_r) ;
  ignore (close_fd d.wake_w) ;
  ignore (close_fd d.kq)

(* The poller of a domain. Before an error, [make_dom] closes the fds that it
   opened. Thus an error in the middle leaves no fd open. *)
let make_dom () =
  let kq = check "kqueue" (kq_create ()) in
  let r, w = pipe () in
  if r < 0 then ignore (close_fd kq) ;
  let wake_r = check "pipe" r in
  let d = {kq; wake_r; wake_w= w} in
  let added = kq_add_in kq wake_r wake_token in
  if added < 0 then close_dom d ;
  ignore (check "kevent" added) ;
  d

(* A watched signal gets the handler of the world (weft_kqueue_stubs.c), which
   counts it and wakes domain 0. Only one world at a time can hold the handler.
   After an error, [create] closes what it made and restores the actions that it
   changed. *)
let create ?(domains = 1) ?(signals = []) () =
  Sched.check_domains "Weft_kqueue.create" domains ;
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore ;
  let nums = List.sort_uniq compare (List.map number_of signals) in
  let doms = ref [] and old_actions = ref [] and claimed = ref false in
  let undo () =
    List.iter (fun (n, a) -> sig_restore n a) !old_actions ;
    if !claimed then sig_release () ;
    List.iter close_dom !doms
  in
  match
    for _ = 1 to domains do
      doms := make_dom () :: !doms
    done ;
    let doms = Array.of_list (List.rev !doms) in
    if nums <> [] then (
      if not (sig_claim doms.(0).wake_w) then
        invalid_arg "Weft_kqueue.create: another world watches signals" ;
      claimed := true ) ;
    List.iter (fun n -> old_actions := (n, sig_catch n) :: !old_actions) nums ;
    (doms, create_pool (Array.map (fun d -> d.wake_w) doms))
  with
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      undo () ;
      Printexc.raise_with_backtrace e bt
  | doms, pool ->
      { doms
      ; pool
      ; watched=
          Atomic.make (List.map (fun s -> signal_of (number_of s)) signals)
      ; old_actions= !old_actions
      ; lock= Mutex.create ()
      ; procs= Hashtbl.create ~random:false 8
      ; pids= Hashtbl.create ~random:false 8
      ; closed= Atomic.make false }

(* The actions come back first. Then the release waits until no handler can
   write the wake pipe, and the pipe can close. *)
let close_once t =
  let watched = List.map number_of (Atomic.get t.watched) in
  List.iter
    (fun (n, a) -> if List.mem n watched then sig_restore n a)
    t.old_actions ;
  if t.old_actions <> [] then sig_release () ;
  close_pool t.pool ;
  Hashtbl.iter
    (fun _ p ->
      ignore (close_fd p.exit_r) ;
      ignore (close_fd p.exit_w) )
    t.procs ;
  Array.iter close_dom t.doms

(* A second close does nothing. Thus it cannot release the handler of a later
   world. *)
let close t = if Atomic.compare_and_set t.closed false true then close_once t

let esrch = Error (Op.Other "ESRCH")

(* The exit of a child: its pipe becomes readable. *)
let exit_seen p =
  Atomic.set p.exit_seen true ;
  poke p.exit_w

(* The exit fd of a child is a pipe, made before the child, so that a failure of
   the pipe leaves no child. The tables have the child before domain 0 can get
   its exit. If the child already exited, EVFILT_PROC refuses it. The child gets
   the default action for SIGPIPE, which the world ignores. The exec gives the
   default action to each signal with a handler, so to each watched signal. *)
let spawn t sp =
  let r, w = pipe () in
  match result r with
  | Error e ->
      Error e
  | Ok r -> (
    match spawned_of (spawn_child (spawn_args sp) [|number_of Op.SIGPIPE|]) with
    | Error _ as e ->
        ignore (close_fd r) ;
        ignore (close_fd w) ;
        e
    | Ok (s : Op.spawned) ->
        let p =
          { pid= s.pid
          ; exit_r= r
          ; exit_w= w
          ; exit_seen= Atomic.make false
          ; reaped= false }
        in
        Mutex.protect t.lock (fun () ->
            Hashtbl.replace t.procs r p ;
            Hashtbl.replace t.pids s.pid p ) ;
        if kq_proc t.doms.(0).kq s.pid (proc_token s.pid) < 0 then exit_seen p ;
        Ok {s with pidfd= r} )

let proc t fd = Mutex.protect t.lock (fun () -> Hashtbl.find_opt t.procs fd)

let posix t op =
  match Weft_posix.submit t.pool op with
  | Some r ->
      r
  | None ->
      assert false (* The arms of [submit] take each other operation. *)

(* Remove a child from the tables. A later child can have the same pid, so the
   entry of the pid goes only if it is this child. *)
let take_proc t fd =
  Mutex.protect t.lock (fun () ->
      match Hashtbl.find_opt t.procs fd with
      | None ->
          None
      | Some p ->
          Hashtbl.remove t.procs fd ;
          ( match Hashtbl.find_opt t.pids p.pid with
          | Some q when q == p ->
              Hashtbl.remove t.pids p.pid
          | _ ->
              () ) ;
          Some p )

(* Only one fiber reaps a child at a time (Process.reap). The reap and the
   change of [reaped] are under the lock, so a kill never sends a signal to a
   pid that the system gave again. *)
let wait_child t p =
  if Atomic.get p.exit_seen then kq_zombie p.pid ;
  Mutex.protect t.lock (fun () ->
      if p.reaped then Error (Op.Other "ECHILD")
      else
        let r = status_of (kq_wait_child p.pid) in
        (match r with Ok (Some _) -> p.reaped <- true | _ -> ()) ;
        r )

(* The end of a watch (design section 2.7). The old action comes back first:
   after it, the kernel gives each new signal its action. Then the world takes
   the count of the signals that came before and that domain 0 did not take, and
   it sends them again, so they get the action. A handler that started before
   the restore can add to the count later: domain 0 takes it, and the scheduler
   sends it again. *)
let unwatch t s =
  let s = signal_of (number_of s) in
  Mutex.protect t.lock (fun () ->
      let watched = Atomic.get t.watched in
      if not (List.mem s watched) then Ok None
      else
        let n = number_of s in
        Atomic.set t.watched (List.filter (fun x -> x <> s) watched) ;
        sig_restore n (List.assoc n t.old_actions) ;
        if sig_take n > 0 then ignore (resend n) ;
        Ok (Some s) )

(* The signals that came since the last take, for the wait of domain 0. *)
let signals t acc =
  List.fold_left
    (fun acc (n, _) ->
      let c = sig_take n in
      if c > 0 then World.Ready.Signal (signal_of n, c) :: acc else acc )
    acc t.old_actions

let kill t p s =
  Mutex.protect t.lock (fun () ->
      if p.reaped then esrch else unit (kq_kill p.pid (number_of s)) )

module World = struct
  type nonrec t = t

  let now _ line = now line

  let submit : type a. t -> a Op.t -> (a, Op.error) result =
   fun t op ->
    match op with
    | Op.Spawn sp ->
        spawn t sp
    | Op.Wait_child fd -> (
      match proc t fd with Some p -> wait_child t p | None -> Error Op.Bad_fd )
    | Op.Kill (fd, s) -> (
      match proc t fd with Some p -> kill t p s | None -> Error Op.Bad_fd )
    | Op.Watch s ->
        let s = signal_of (number_of s) in
        Ok (if List.mem s (Atomic.get t.watched) then Some s else None)
    | Op.Unwatch s ->
        unwatch t s
    | Op.Register (d, fd, token) ->
        unit (kq_add t.doms.(d).kq fd token)
    | Op.Deregister (d, fd) ->
        unit (kq_del t.doms.(d).kq fd)
    | Op.Close fd -> (
      match take_proc t fd with
      | Some p ->
          ignore (close_fd p.exit_w) ;
          unit (close_fd p.exit_r)
      | None ->
          posix t op )
    | op ->
        posix t op

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

  (* Under the lock: a close removes the child under the same lock before it
     closes [exit_w]. *)
  let exited t token =
    Mutex.protect t.lock (fun () ->
        Option.iter exit_seen (Hashtbl.find_opt t.pids (pid_of_token token)) )

  let wait t ~domain (w : World.Wait.t) =
    let d = t.doms.(domain) in
    Array.fold_right
      (fun (token, read, write) acc ->
        if token = wake_token then (
          (* The drain comes before the take of the counts: a handler adds to a
             count before it writes the pipe. *)
          drain_all d.wake_r ;
          let acc = if domain = 0 then signals t acc else acc in
          World.Ready.Woken :: completions t.pool domain acc )
        else if token <= -1000 then (exited t token ; acc)
        else World.Ready.Fd {token; read; write} :: acc )
      (kq_wait d.kq (timeout_ns w))
      []

  let wake t ~domain = poke t.doms.(domain).wake_w

  let random _ n = random n

  let random_bytes _ b = ignore (getrandom b)
end

let default_domains = default_domains

let watchdog_reports = watchdog_reports

let pool_warnings = pool_warnings

let pool_busy = pool_busy

let run ?slack ?domains ?signals ?(watchdog = Some (Weft.Span.ms 10)) main =
  let domains = match domains with Some n -> n | None -> default_domains () in
  Weft_posix.run ~name:"Weft_kqueue.run"
    (module World)
    (create ~domains ?signals ())
    ~close ?slack ~domains ~watchdog main
