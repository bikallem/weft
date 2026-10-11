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
  ; state: state Atomic.t }

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
  let p = {result= Ivar.create (); state= Atomic.make Not_started} in
  Fiber.fork ?name ?pinned scope (fun () ->
      let r =
        match Fiber.scope (fun inner -> start p inner ; f ()) with
        | v ->
            Ok v
        | exception Fiber.Cancelled _ ->
            Error (Cancelled, Printexc.get_raw_backtrace ())
        | exception e ->
            Error (e, Printexc.get_raw_backtrace ())
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
  Event.wrap (Ivar.read p.result) (function
    | Ok v ->
        v
    | Error (e, bt) ->
        Printexc.raise_with_backtrace e bt )

let await p = Event.sync (ready p)
