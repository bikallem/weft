(* The Windows world (design section 2.7, Windows): an I/O completion port and a
   high-resolution timer for each domain, the edges of the sockets with
   AFD_POLL, pipes with overlapped I/O, and Ctrl-C. The C code is in weft.posix
   (weft_win_io.c), with the Windows version of the stubs of the POSIX worlds:
   Weft_posix.Make makes the world, as for Linux and kqueue. *)

open Weft.Private
open Weft_posix

external win_create : int -> nativeint = "weft_win_create"

external win_close : nativeint -> unit = "weft_win_close_world"

external win_iocps : nativeint -> int array = "weft_win_iocps"

external win_register : nativeint -> int -> int -> int -> int
  = "weft_win_register"

external win_deregister : nativeint -> int -> int -> int = "weft_win_deregister"

external win_wait : nativeint -> int -> int64 -> (int * bool * bool) array
  = "weft_win_wait"

external win_wake : nativeint -> int -> unit = "weft_win_wake"

external win_wait_child : int -> int * int = "weft_win_wait_child"

external win_kill : int -> int = "weft_win_kill"

external win_watch : nativeint -> int array -> unit = "weft_win_watch"

external win_unwatch : int -> unit = "weft_win_unwatch"

external win_unwatch_all : unit -> unit = "weft_win_unwatch_all"

external win_signal_take : unit -> (int * int) array = "weft_win_signal_take"

(* The tokens of the alarm and of a wake. A token of an fd is never negative. *)
let timer_token = -1

let wake_token = -2

type t =
  { w: nativeint  (** The ports of the domains (weft_win_io.c). *)
  ; pool: pool
  ; watched: Op.signal list Atomic.t
        (** The signals of [run ~signals], with no signal of an unwatch. *)
  ; lock: Mutex.t
  ; signals: bool  (** The world holds the handler of the console. *)
  ; closed: bool Atomic.t  (** [close] sets it, one time. *)
  ; watchdog: nativeint Atomic.t
        (** The watchdog of [run], or [0n] (weft_watchdog.c). *) }

(* [true] while a world with [~signals] is open: the handler of the console is
   one for the process, as on the other worlds. *)
let signal_world = Atomic.make false

let create ?(domains = 1) ?(signals = []) () =
  Sched.check_domains "Weft_windows.create" domains ;
  if signals <> [] && not (Atomic.compare_and_set signal_world false true) then
    invalid_arg "Weft_windows.create: another world watches signals" ;
  match
    let w = win_create domains in
    match create_pool (win_iocps w) with
    | pool ->
        (w, pool)
    | exception e ->
        win_close w ; raise e
  with
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      if signals <> [] then Atomic.set signal_world false ;
      Printexc.raise_with_backtrace e bt
  | w, pool ->
      if signals <> [] then
        win_watch w (Array.of_list (List.map number_of signals)) ;
      { w
      ; pool
      ; watched= Atomic.make (List.map normal signals)
      ; lock= Mutex.create ()
      ; signals= signals <> []
      ; closed= Atomic.make false
      ; watchdog= Atomic.make 0n }

(* A second close does nothing. Thus it cannot release the handler of a later
   world. *)
let close t =
  if Atomic.compare_and_set t.closed false true then (
    if t.signals then (
      win_unwatch_all () ;
      Atomic.set signal_world false ) ;
    close_pool t.pool ;
    win_close t.w )

(* The end of a watch: the event of the console gets its default action. A count
   that domain 0 did not take stays: the scheduler sends it again. *)
let unwatch _ s ~rest:_ = Ok (win_unwatch (number_of s))

(* The signals that came since the last take, for the wait of domain 0. *)
let signals acc =
  Array.fold_left
    (fun acc (n, c) -> World.Ready.Signal (signal_of n, c) :: acc)
    acc (win_signal_take ())

module Poller = struct
  type nonrec t = t

  let name = "Weft_windows"

  let create = create

  let close = close

  let pool t = t.pool

  let watched t = t.watched

  let watchdog t = t.watchdog

  let unwatch_lock t = t.lock

  let unwatch = unwatch

  let register t d fd token = unit (win_register t.w d fd token)

  let deregister t d fd = unit (win_deregister t.w d fd)

  (* The handle of the process is the pidfd: the table of the fds gives its exit
     as a read edge. *)
  let spawn _ sp = spawned_of (spawn_child (spawn_args sp) [||])

  let wait_child _ fd = status_of (win_wait_child fd)

  let kill _ fd _ = unit (win_kill fd)

  let close_fd t fd = close_op t.pool fd

  let wait t ~domain (w : World.Wait.t) =
    Array.fold_right
      (fun (token, read, write) acc ->
        if token = timer_token then acc
        else if token = wake_token then
          let acc = if domain = 0 && t.signals then signals acc else acc in
          completions t.pool domain acc
        else World.Ready.Fd {token; read; write} :: acc )
      (win_wait t.w domain (timeout_ns w))
      []

  let wake t ~domain = win_wake t.w domain
end

include Weft_posix.Make (Poller)
