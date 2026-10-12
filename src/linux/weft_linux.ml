(* The Linux world (design section 2.7): epoll, a timerfd alarm, an eventfd
   wake, a signalfd and pidfds. The parts that the POSIX worlds share are in
   Weft_posix. *)

open Weft.Private
open Weft_posix

external epoll_create : unit -> int = "weft_epoll_create"

external eventfd : unit -> int = "weft_eventfd"

external timerfd : bool -> int = "weft_timerfd"
(* [timerfd posix]: on [CLOCK_REALTIME] if [posix], else on
   [CLOCK_MONOTONIC]. *)

external timerfd_set : int -> bool -> int64 -> int = "weft_timerfd_set"

external drain : int -> int = "weft_drain"

external eventfd_signal : int -> int = "weft_eventfd_signal"

external epoll_add : int -> int -> int -> int = "weft_epoll_add"

external epoll_add_in : int -> int -> int -> int = "weft_epoll_add_in"

external epoll_del : int -> int -> int = "weft_epoll_del"

external epoll_wait : int -> bool -> (int * bool * bool) array
  = "weft_epoll_wait"

(* Child processes and signals (weft_proc.c). *)

external wait_child : int -> int * int = "weft_wait_child"

external pidfd_kill : int -> int -> int = "weft_pidfd_kill"

external sigmask_block : int array -> bytes = "weft_sigmask_block"

external sigmask_restore : bytes -> unit = "weft_sigmask_restore"

external signalfd : int array -> int = "weft_signalfd"

external signalfd_read : int -> int array = "weft_signalfd_read"

(* The tokens of the alarm and of the wake fd. A slot of an fd is never so
   large. *)
let timer_token = -1

let wake_token = -2

let signal_token = -3

let posix_token = -4

type dom =
  { epfd: int
  ; evfd: int
  ; tfd: int
  ; mutable armed: int64  (** The time of the alarm, or 0. *)
  ; ptfd: int  (** The [Posix] alarm, on [CLOCK_REALTIME] (M3). *)
  ; mutable parmed: int64 }

type t =
  { doms: dom array
  ; rng: int Atomic.t
  ; pool: pool
  ; watched: Op.signal list  (** The signals of [run ~signals]. *)
  ; sigfd: int  (** The signalfd in the poller of domain 0, or [-1]. *)
  ; old_mask: bytes option  (** The signal mask before [create]. *) }

let make_dom () =
  let epfd = check "epoll_create1" (epoll_create ()) in
  let evfd = check "eventfd" (eventfd ()) in
  let tfd = check "timerfd_create" (timerfd false) in
  let ptfd = check "timerfd_create" (timerfd true) in
  ignore (check "epoll_ctl" (epoll_add_in epfd evfd wake_token)) ;
  ignore (check "epoll_ctl" (epoll_add_in epfd tfd timer_token)) ;
  ignore (check "epoll_ctl" (epoll_add_in epfd ptfd posix_token)) ;
  {epfd; evfd; tfd; armed= 0L; ptfd; parmed= 0L}

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
  let doms = Array.init domains (fun _ -> make_dom ()) in
  if sigfd >= 0 then
    ignore (check "epoll_ctl" (epoll_add_in doms.(0).epfd sigfd signal_token)) ;
  { doms
  ; rng= new_rng ()
  ; pool= create_pool (Array.map (fun d -> d.evfd) doms)
  ; watched= List.map (fun s -> signal_of (number_of s)) signals
  ; sigfd
  ; old_mask }

let close t =
  close_pool t.pool ;
  if t.sigfd >= 0 then ignore (close_fd t.sigfd) ;
  Option.iter sigmask_restore t.old_mask ;
  Array.iter
    (fun d ->
      ignore (close_fd d.tfd) ;
      ignore (close_fd d.ptfd) ;
      ignore (close_fd d.evfd) ;
      ignore (close_fd d.epfd) )
    t.doms

(* The calls of timerfd_settime, for the measurements. *)
let alarm_sets = Atomic.make 0

module World = struct
  type nonrec t = t

  let now _ line = now line

  let submit : type a. t -> a Op.t -> (a, Op.error) result =
   fun t op ->
    match op with
    | Op.Spawn sp ->
        if spawn_has_nul sp then nul_error
        else
          (* The world ignores only SIGPIPE: it blocks the watched signals. *)
          spawned_of (spawn_child (spawn_args sp) [|number_of Op.SIGPIPE|])
    | Op.Wait_child fd ->
        status_of (wait_child fd)
    | Op.Kill (fd, s) ->
        unit (pidfd_kill fd (number_of s))
    | Op.Watch s ->
        let s = signal_of (number_of s) in
        if List.mem s t.watched then Ok s
        else Error (Op.Other "the world does not watch this signal")
    | Op.Register (d, fd, token) ->
        unit (epoll_add t.doms.(d).epfd fd token)
    | Op.Deregister (d, fd) ->
        unit (epoll_del t.doms.(d).epfd fd)
    | op -> (
      match Weft_posix.submit t.pool op with
      | Some r ->
          r
      | None ->
          assert false (* The arms above take each other operation. *) )

  (* Set an alarm only when its earliest deadline changed. *)
  let target to_ns = function None -> 0L | Some x -> Int64.max 1L (to_ns x)

  let arm d (w : World.Wait.t) =
    let t = target (Weft.Clock.to_ns Mono) w.deadline in
    if not (Int64.equal t d.armed) then (
      Atomic.incr alarm_sets ;
      ignore (timerfd_set d.tfd false t) ;
      d.armed <- t ) ;
    let p = target (Weft.Clock.to_ns Posix) w.posix_deadline in
    if not (Int64.equal p d.parmed) then (
      Atomic.incr alarm_sets ;
      ignore (timerfd_set d.ptfd true p) ;
      d.parmed <- p )

  let wait t ~domain (w : World.Wait.t) =
    let d = t.doms.(domain) in
    arm d w ;
    let events = epoll_wait d.epfd (w.timeout = World.Wait.Sleep) in
    Array.fold_right
      (fun (token, read, write) acc ->
        if token = timer_token then (
          ignore (drain d.tfd) ;
          (* A timerfd that fired is not armed. *)
          d.armed <- 0L ;
          World.Ready.Alarm :: acc )
        else if token = posix_token then (
          (* After a step of the clock, the read gives ECANCELED, and the alarm
             must be set again. *)
          ignore (drain d.ptfd) ;
          d.parmed <- 0L ;
          World.Ready.Posix_alarm :: acc )
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
          World.Ready.Woken :: completions t.pool domain acc )
        else World.Ready.Fd {token; read; write} :: acc )
      events []

  let wake t ~domain = ignore (eventfd_signal t.doms.(domain).evfd)

  let random t n = next t.rng mod n

  let random_bytes _ b = ignore (getrandom b)
end

(* The default number of domains is a start value (open question 6). *)
let default_domains = default_domains

let watchdog_reports = watchdog_reports

let pool_warnings = pool_warnings

let run ?slack ?domains ?signals ?(watchdog = Some (Weft.Span.ms 10)) main =
  let domains = match domains with Some n -> n | None -> default_domains () in
  Weft_posix.run ~name:"Weft_linux.run"
    (module World)
    (create ~domains ?signals ())
    ~close ?slack ~domains ~watchdog main

let local_port fd = local_port ~name:"Weft_linux.local_port" fd

let alarm_sets () = Atomic.get alarm_sets
