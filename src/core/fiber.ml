(* Fibers and scopes (design section 2.5). *)

type scope = Sched.ctx

exception Cancelled = Sched.Cancelled

exception Multiple = Sched.Multiple

exception Timeout = Sched.Timeout

exception Not_in_fiber = World.Not_in_fiber

exception Internal_error = Sched.Internal_error

exception Scope_closed = Sched.Scope_closed

type handler = Sched.handler = {handle: 'a. (unit -> 'a) -> 'a}

let scope ?handler f = Sched.scope ?handler f

let fork ?name scope f = Sched.fork ?name scope f

(* A [Cancelled] is not an error of the fiber: it goes to the scope, which
   ignores it. If [on_error] raises, the scope fails (design section 2.5,
   Errors). *)
let fork_isolated ?name scope ~on_error f =
  fork ?name scope (fun () ->
      match f () with
      | () ->
          ()
      | exception (Cancelled _ as e) ->
          raise e
      | exception e ->
          on_error e (Printexc.get_raw_backtrace ()) )

let fork_daemon ?name scope f = Sched.fork_daemon ?name scope f

let cancel = Sched.cancel

let shield = Sched.shield

let yield = Event.yield

module Mutex = struct
  include Locks.Mutex

  exception Poisoned = Locks.Poisoned
end

module Condition = Locks.Condition

(* A new scope T with a daemon that cancels T at the deadline (design section
   2.5, Timeouts). [Timeout] only for the own deadline. If an outer context is
   cancelled, the call raises its [Cancelled], also when it is cancelled before
   the start. *)
let with_timeout (_ : Clock.mono Clock.line) span f =
  let outer = List.hd (Sched.self ()).ctxs in
  let outer_cancel () =
    match Sched.first_flag outer with
    | Some r ->
        raise (Cancelled r)
    | None ->
        ()
  in
  outer_cancel () ;
  let fired = ref false in
  match
    scope (fun t ->
        fork_daemon ~name:"with_timeout" t (fun () ->
            Time.sleep Mono span ;
            fired := true ;
            cancel t Timeout ) ;
        f () )
  with
  | v ->
      v
  | exception (Cancelled _ as e) ->
      let bt = Printexc.get_raw_backtrace () in
      outer_cancel () ;
      if !fired then raise Timeout else Printexc.raise_with_backtrace e bt
