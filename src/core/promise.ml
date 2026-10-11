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
        (** A fiber got the result. A separate cell: the finaliser must not hold
            the promise. *)
  ; unawaited: int Atomic.t option  (** The count of the run of the promise. *)
  }

(* The report of a promise that failed and that no fiber awaited (design section
   2.5, Errors). It gets the run of the promise. The simulator puts its own
   report here. *)
let default_report _ e =
  let text = try Printexc.to_string e with _ -> "an exception" in
  prerr_string
    ("weft: a promise failed, and no fiber awaited it: " ^ text ^ "\n")

let report : (int option -> exn -> unit) Atomic.t = Atomic.make default_report

(* A finaliser runs at an allocation point, with no handler: it performs no
   effect, and it never raises. *)
let finaliser run result awaited () =
  try
    match Atomic.get result with
    | Sched.Full (Error (e, _)) when not (Atomic.get awaited) -> (
      match e with Cancelled -> () | e -> (Atomic.get report) run e )
    | _ ->
        ()
  with _ -> ()

let rec start p inner =
  match Atomic.get p.state with
  | Not_started ->
      if not (Atomic.compare_and_set p.state Not_started (Running inner)) then
        start p inner
  | Requested ->
      Atomic.set p.state (Running inner) ;
      Fiber.cancel inner Cancel_request
  | Running _ ->
      assert false (* Only the fiber of the promise starts it. *)

let fork ?name ?pinned scope f =
  let p =
    { result= Ivar.create ()
    ; state= Atomic.make Not_started
    ; awaited= Atomic.make false
    ; unawaited=
        Option.map (fun (t, _) -> t.Sched.unawaited) (Atomic.get Sched.outside)
    }
  in
  (* The closure holds the id of the run, not the run: the scheduler can hold
     the promise. *)
  let run =
    Option.map (fun (t, _) -> t.Sched.run_id) (Atomic.get Sched.outside)
  in
  Gc.finalise_last (finaliser run p.result p.awaited) p ;
  Fiber.fork ?name ?pinned scope (fun () ->
      let r =
        match Fiber.scope (fun inner -> start p inner ; f ()) with
        | v ->
            Ok v
        | exception Fiber.Cancelled _ ->
            Error (Cancelled, Printexc.get_raw_backtrace ())
        | exception e ->
            let bt = Printexc.get_raw_backtrace () in
            Option.iter Atomic.incr p.unawaited ;
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
          Atomic.set p.awaited true ; v
      | Error (e, bt) ->
          (* The first fiber that gets the exception. *)
          if Atomic.compare_and_set p.awaited false true && e != Cancelled then
            Option.iter Atomic.decr p.unawaited ;
          Printexc.raise_with_backtrace e bt )

let await p = Event.sync (ready p)
