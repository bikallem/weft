(* Promises (design section 2.5, Promises). The fiber of a promise runs in a
   scope of its own, so [cancel] cancels only that fiber. *)

exception Cancelled

(* The reason of the cancel of [cancel]. *)
exception Cancel_request

type 'a t =
  { result: ('a, exn * Printexc.raw_backtrace) result Ivar.t
  ; node: Sched.ctx option Atomic.t  (** The scope of the fiber. *)
  ; requested: bool Atomic.t  (** [cancel] occurred. *) }

(* [cancel] sets [requested] and then reads [node]. The fiber sets [node] and
   then reads [requested]. Thus at least one of the two cancels the scope, also
   when [cancel] comes before the fiber starts. *)
let fork ?name scope f =
  let p =
    { result= Ivar.create ()
    ; node= Atomic.make None
    ; requested= Atomic.make false }
  in
  Fiber.fork ?name scope (fun () ->
      let r =
        match
          Fiber.scope (fun inner ->
              Atomic.set p.node (Some inner) ;
              if Atomic.get p.requested then Fiber.cancel inner Cancel_request ;
              f () )
        with
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
let cancel p =
  ignore (Sched.self ()) ;
  Atomic.set p.requested true ;
  match Atomic.get p.node with
  | Some n ->
      Fiber.cancel n Cancel_request
  | None ->
      ()

let ready p =
  Event.wrap (Ivar.read p.result) (function
    | Ok v ->
        v
    | Error (e, bt) ->
        Printexc.raise_with_backtrace e bt )

let await p = Event.sync (ready p)
