let version = "0.0.0"

module Span = Span
module Clock = Clock

type 'a event = 'a Event.t

let sync = Event.sync

let choose = Event.choose

let wrap = Event.wrap

let guard = Event.guard

let with_nack = Event.with_nack

let always = Event.always

let never = Event.never

module Time = Time

module Fiber = struct
  include Fiber

  let fork_promise = Promise.fork
end

module Promise = Promise
module Ivar = Ivar
module Net = Net
module Chan = Chan

module type WORLD = World.WORLD

(* The world handler: it gives each world effect to the module of the world
   (design section 2.3). The simulator uses it too. *)
let run_world (type w) ?slack ?on_create (module W : WORLD with type t = w)
    (w : w) main =
  let open Effect.Deep in
  (* Not [try_with]: it raises the exception again with [raise], and the
     backtrace of the fiber is lost. *)
  match_with
    (fun () -> Sched.run ?slack ?on_create main)
    ()
    { retc= Fun.id
    ; exnc=
        (fun e ->
          Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ()) )
    ; effc=
        (fun (type a) (e : a Effect.t) ->
          match e with
          | World.Now line ->
              Some (fun (k : (a, _) continuation) -> continue k (W.now w line))
          | World.Submit op ->
              Some (fun k -> continue k (W.submit w op))
          | World.Wait (domain, wt) ->
              Some (fun k -> continue k (W.wait w ~domain wt))
          | World.Wake domain ->
              Some (fun k -> continue k (W.wake w ~domain))
          | World.Random n ->
              Some (fun k -> continue k (W.random w n))
          | World.Random_bytes b ->
              Some (fun k -> continue k (W.random_bytes w b))
          | World.Switch _ ->
              Some (fun k -> continue k ())
          | _ ->
              None ) }

let run ?slack m w main = run_world ?slack m w main

module Private = struct
  let run_world = run_world

  let scope_ctx (s : Fiber.scope) : Sched.ctx = s

  let ivar_state (iv : 'a Ivar.t) = Atomic.get iv

  module Commit = Commit
  module World = World
  module Op = Op
  module Sched = Sched
  module Sched_debug = Sched_debug
  module Event = Event
  module Net = Net
  module Chan = Chan
end
