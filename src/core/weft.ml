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
module File = Fs.File
module Dir = Fs.Dir
module Dns = Fs.Dns
module Pipe = Pipe
module Signal = Signal
module Process = Process

module type WORLD = World.WORLD

(* The world handler: it gives each world effect to the module of the world
   (design section 2.3). Each OCaml domain runs its own instance. *)
let handle (type w) (module W : WORLD with type t = w) (w : w) f =
  let open Effect.Deep in
  (* Not [try_with]: it raises the exception again with [raise], and the
     backtrace of the fiber is lost. *)
  match_with f ()
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
          | World.Switch ->
              Some (fun k -> continue k ())
          | _ ->
              None ) }

(* Domain 0 runs on the calling thread, and each other domain on an OCaml domain
   of its own. If a loop fails, the others stop. *)
let run_world (type w) ?slack ?(domains = 1) ?on_create
    (module W : WORLD with type t = w) (w : w) main =
  let t = Sched.create ?slack ~domains () in
  Option.iter (fun f -> f t) on_create ;
  let result = Sched.start t main in
  let run_dom d () =
    match handle (module W) w (fun () -> Sched.run_domain d) with
    | () ->
        Ok ()
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        Atomic.set t.finished true ;
        Array.iter (fun d -> W.wake w ~domain:d.Sched.index) t.doms ;
        Error (e, bt)
  in
  let stop () =
    Atomic.set t.finished true ;
    Array.iter (fun d -> W.wake w ~domain:d.Sched.index) t.doms
  in
  (* If a spawn fails, the domains that started must stop before the error goes
     on: they hold the fds of the world. *)
  let others =
    List.fold_left
      (fun started d ->
        match Domain.spawn (run_dom d) with
        | dom ->
            dom :: started
        | exception e ->
            let bt = Printexc.get_raw_backtrace () in
            stop () ;
            List.iter (fun d -> ignore (Domain.join d)) started ;
            Printexc.raise_with_backtrace e bt )
      []
      (Array.to_list (Array.sub t.doms 1 (domains - 1)))
    |> List.rev
  in
  let first = run_dom t.doms.(0) () in
  let rest = List.map Domain.join others in
  match List.find_opt Result.is_error (first :: rest) with
  | Some (Error (e, bt)) ->
      Printexc.raise_with_backtrace e bt
  | _ ->
      result ()

let run ?slack ?domains m w main = run_world ?slack ?domains m w main

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
  module Ring = Ring
  module Job = Job
end
