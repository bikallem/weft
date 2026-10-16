(* Promises (design section 2.5, Promises). The fiber of a promise runs in a
   scope of its own, so [cancel] cancels only that fiber. *)

exception Cancelled

(* The reason of the cancel of [cancel]. *)
exception Cancel_request

(* A [cancel] before the start of the fiber leaves [Requested], and the fiber
   cancels its scope when it starts. *)
type state = Not_started | Requested | Running of Sched.ctx

type 'a t =
  { result: ('a, exn * Printexc.raw_backtrace) result Ivar.t
  ; state: state Atomic.t
  ; awaited: bool Atomic.t
        (** A fiber got the exception. A separate cell: the finaliser must not
            hold the promise. *)
  ; mutable unawaited: int Atomic.t option
        (** After a failure: the count of the run (design section 2.5, Errors).
        *)
  ; mutable cell: unit ref option
        (** After a failure: the value of the finaliser. Only the promise holds
            it. *) }

(* The report of a promise that failed and that no fiber awaited (design section
   2.5, Errors). It gets the id of the run of the promise. The simulator puts
   its own report here. *)
let default_report _ e =
  let text = try Printexc.to_string e with _ -> "an exception" in
  prerr_string
    ("weft: a promise failed, and no fiber awaited it: " ^ text ^ "\n")

let report : (int option -> exn -> unit) Atomic.t = Atomic.make default_report

(* A finaliser runs at an allocation point, with no handler: it performs no
   effect, and it never raises. Its closure holds the id of the run, not the
   run, because the scheduler can hold the promise. *)
let finaliser run e awaited () =
  try if not (Atomic.get awaited) then (Atomic.get report) run e with _ -> ()

let rec start p inner =
  match Atomic.get p.state with
  | Not_started ->
      if not (Atomic.compare_and_set p.state Not_started (Running inner)) then
        start p inner
  | Requested ->
      Atomic.set p.state (Running inner)
      [@mutate
        off
          "the start cancelled the scope already, and cancel_node on a \
           cancelled node finds no cancellable sync that is not done"] ;
      Fiber.cancel inner Cancel_request
  | Running _ ->
      assert false (* Only the fiber of the promise starts it. *)

(* Only a promise that failed gets a finaliser. The finaliser goes on a new
   cell, not on the promise: another domain can hold the promise in its minor
   heap. In OCaml 5.5, [Gc.finalise_last] on a young value of another domain
   lets the minor GC read freed memory. *)
let fork ?name ?pinned scope f =
  let p =
    { result= Ivar.create ()
    ; state= Atomic.make Not_started
    ; awaited= Atomic.make false
    ; unawaited= None
    ; cell= None }
  in
  Fiber.fork ?name ?pinned scope (fun () ->
      (* A [Cancelled] is a cancel of the promise only if the chain of its scope
         is cancelled when [f] raises it. If not, it comes from a scope inside
         [f] that [f] cancelled, and it is a failure. The check comes before the
         end of the scope: a failure cancels the scope. *)
      let failure = ref false in
      let body inner =
        start p inner ;
        try f ()
        with
        | Fiber.Cancelled _ as e when Option.is_none (Sched.first_flag inner) ->
          failure := true ;
          Printexc.raise_with_backtrace e (Printexc.get_raw_backtrace ())
      in
      let r =
        match Fiber.scope body with
        | v ->
            Ok v
        | exception Fiber.Cancelled _ when not !failure ->
            Error (Cancelled, Printexc.get_raw_backtrace ())
        | exception e ->
            let bt = Printexc.get_raw_backtrace () in
            let run = Atomic.get Sched.outside in
            p.unawaited <- Option.map (fun (t, _) -> t.Sched.unawaited) run ;
            Option.iter Atomic.incr p.unawaited ;
            let cell = ref () in
            p.cell <- Some cell ;
            Gc.finalise_last
              (finaliser
                 (Option.map (fun (t, _) -> t.Sched.run_id) run)
                 e p.awaited )
              cell ;
            Error (e, bt)
      in
      Ivar.fill p.result r ) ;
  p

(* [Self] first: outside a fiber, [cancel] raises [Not_in_fiber] before it
   changes the promise. *)
let rec cancel p =
  ignore (Sched.self ()) ;
  match Atomic.get p.state with
  | Not_started ->
      if
        not
          (Atomic.compare_and_set p.state Not_started Requested)
        [@mutate
          off
            "the retry runs only when the fiber starts on another domain \
             between the read and the CAS, an order that a test cannot select"]
      then cancel p
  | Requested ->
      ()
  | Running n ->
      Fiber.cancel n Cancel_request

let ready p =
  Event.wrap (Ivar.read p.result) (fun r ->
      match r with
      | Ok v ->
          v
      | Error (e, bt) ->
          (* The first fiber that gets the exception. A cancelled promise has no
             count. *)
          if
            Atomic.compare_and_set p.awaited false true
            [@mutate
              off
                "the count only selects the GC at the end of Sim.run; the \
                 finaliser reads the awaited cell"]
          then Option.iter Atomic.decr p.unawaited ;
          Printexc.raise_with_backtrace e bt )

let await p = Event.sync (ready p)
