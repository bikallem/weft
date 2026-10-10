(* Fibers and scopes (design section 2.5). *)

type scope = Sched.ctx

exception Cancelled = Sched.Cancelled

exception Multiple = Sched.Multiple

exception Timeout = Sched.Timeout

exception Not_in_fiber = World.Not_in_fiber

exception Internal_error = Sched.Internal_error

exception Scope_closed = Sched.Scope_closed

let scope = Sched.scope

let fork ?name scope f = Sched.fork ?name scope f

let fork_daemon ?name scope f = Sched.fork_daemon ?name scope f

let cancel = Sched.cancel

let shield = Sched.shield

let yield () = Event.sync Event.yield

let with_timeout (_ : Clock.mono Clock.line) span f =
  let outer = List.hd (Sched.self ()).ctxs in
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
      (* Timeout only for the own deadline, and only if no outer context is
         cancelled. *)
      let bt = Printexc.get_raw_backtrace () in
      if !fired && Option.is_none (Sched.first_flag outer) then raise Timeout
      else Printexc.raise_with_backtrace e bt
