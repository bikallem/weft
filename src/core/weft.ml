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

module Promise = struct
  type 'a t = 'a Promise.t

  let await = Promise.await

  let ready = Promise.ready
end

module Ivar = Ivar
module Net_internal = Net
module Net = Net_internal
module Chan_internal = Chan
module Chan = Chan_internal

module type WORLD = World.WORLD

let run (type w) ?slack (module W : WORLD with type t = w) (w : w) main =
  let open Effect.Deep in
  match_with
    (fun () -> Sched.run ?slack main)
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

module Private = struct
  module Commit = Commit
  module World = World
  module Op = Op
  module Sched = Sched
  module Sched_debug = Sched_debug
  module Event = Event
  module Net = Net_internal
  module Chan = Chan_internal
end
