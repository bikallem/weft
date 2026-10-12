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

let fork ?name ?pinned scope f = Sched.fork ?name ?pinned scope f

(* A [Cancelled] is not an error of the fiber: it goes to the scope, which
   ignores it. If [on_error] raises, the scope fails (design section 2.5,
   Errors). *)
let fork_isolated ?name ?pinned scope ~on_error f =
  fork ?name ?pinned scope (fun () ->
      match f () with
      | () ->
          ()
      | exception (Cancelled _ as e) ->
          raise e
      | exception e ->
          on_error e (Printexc.get_raw_backtrace ()) )

let fork_daemon ?name scope f = Sched.fork_daemon ?name scope f

let cancel = Sched.cancel

(* The id of a fiber is the id of its record (design section 2.5, The id of a
   fiber). The record goes with the fiber to each domain. *)
module Id = struct
  type t = int

  let equal = Int.equal

  let compare = Int.compare

  let hash = Hashtbl.hash

  let to_int t = t
end

let self () = (Sched.self ()).fid

let shield = Sched.shield

let yield = Event.yield

module Mutex = struct
  include Locks.Mutex

  exception Poisoned = Locks.Poisoned
end

module Condition = Locks.Condition
module Semaphore = Locks.Semaphore

(* A new scope T and a deadline on T: an entry in the heap of the domain, with
   no fiber (design section 2.5, Timeouts, the deadline form of M3). At the
   deadline, the owner of the heap cancels T. When [f] ends, the caller ends the
   entry with a CAS on its word: if the CAS fails, the deadline came first.
   [Timeout] only for the own deadline. If an outer context is cancelled, the
   call raises its [Cancelled], also when it is cancelled before the start. *)
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
  match Time.add Mono (Time.now Mono) span with
  | None ->
      (* The deadline overflows: it never comes. *)
      scope (fun _ -> f ())
  | Some at -> (
      let fired = ref false in
      match
        scope (fun t ->
            let word =
              Sched.prog "Fiber.with_timeout"
                (Sched.Cancel_at (Clock.to_ns Mono at, t))
            in
            let finish () =
              match Sched.C.self_commit word 1 with
              | Ok () ->
                  ()
              | Error _ ->
                  fired := true
            in
            Fun.protect ~finally:finish f )
      with
      | v ->
          v
      | exception (Cancelled _ as e) ->
          let bt = Printexc.get_raw_backtrace () in
          outer_cancel () ;
          if !fired then raise Timeout else Printexc.raise_with_backtrace e bt )
