(* Locks for fiber code: they suspend the fiber, not the domain (design section
   2.5, Locks for fiber code). A short spin lock protects the state of each
   lock. Its holder never performs an effect, so the waiters to resume go to
   [enqueue] after the unlock. *)

open Sched

exception Poisoned

(* A fiber that waits. [gone] tells that it left after a cancel, and an Ivar
   that is full tells that it got the lock or the signal. *)
type waiter = {wiv: unit ivar; mutable gone: bool}

let new_waiter () = {wiv= Iv.make (); gone= false}

let is_full w = match Atomic.get w.wiv with Full _ -> true | Empty _ -> false

(* Fill the Ivar of [w] and give the syncs to resume. *)
let give w = Option.value ~default:[] (Iv.fill w.wiv ())

(* The first waiter that did not leave. *)
let rec first q =
  match Queue.take_opt q with Some w when w.gone -> first q | x -> x

module Mutex = struct
  type t =
    { lock: Spin.t
    ; mutable held: bool
    ; mutable poisoned: bool
    ; waiters: waiter Queue.t
    ; mutable outer: ctx list option
          (** Only the holder uses it: the contexts outside the shield of
              [use_rw ~protect:true], for [Condition.wait]. *)
    ; mutable wait_exn: exn option
          (** Only the holder uses it: the [Cancelled] that [Condition.wait]
              raised. It does not poison. *) }

  let create () =
    { lock= Spin.make ()
    ; held= false
    ; poisoned= false
    ; waiters= Queue.create ()
    ; outer= None
    ; wait_exn= None }

  (* Give the mutex to the first waiter, or make it free. *)
  let unlock m =
    ignore (self ()) ;
    let resumed =
      Spin.with_lock m.lock (fun () ->
          if m.poisoned then []
          else if not m.held then
            invalid_arg "Fiber.Mutex.unlock: the mutex is not locked"
          else
            match first m.waiters with
            | Some w ->
                give w
            | None ->
                m.held <- false ;
                [] )
    in
    enqueue resumed

  (* Each waiter wakes and raises [Poisoned]. *)
  let poison m =
    let resumed =
      Spin.with_lock m.lock (fun () ->
          m.poisoned <- true ;
          m.held <- false ;
          let rec all acc =
            match first m.waiters with
            | Some w ->
                all (give w @ acc)
            | None ->
                acc
          in
          all [] )
    in
    enqueue resumed

  let lock m =
    ignore (self ()) ;
    let w =
      Spin.with_lock m.lock (fun () ->
          if m.poisoned then raise Poisoned ;
          if not m.held then (
            m.held <- true ;
            None )
          else
            let w = new_waiter () in
            Queue.push w m.waiters ; Some w )
    in
    match w with
    | None ->
        ()
    | Some w -> (
      match Event.sync (Event.of_ivar w.wiv) with
      | () ->
          if m.poisoned then raise Poisoned
      | exception (Cancelled _ as e) ->
          let bt = Printexc.get_raw_backtrace () in
          (* The unlock can give the mutex to [w] after the cancel. Then [w]
             owns it and gives it on. *)
          let owned =
            Spin.with_lock m.lock (fun () ->
                w.gone <- true ;
                is_full w )
          in
          if owned then unlock m ;
          Printexc.raise_with_backtrace e bt )

  let use_rw ~protect m f =
    lock m ;
    m.outer <- (if protect then Some (self ()).ctxs else None) ;
    m.wait_exn <- None ;
    match if protect then shield f else f () with
    | v ->
        m.outer <- None ;
        unlock m ;
        v
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        ( match m.wait_exn with
        | Some e' when e' == e ->
            m.outer <- None ;
            unlock m
        | _ ->
            m.outer <- None ;
            poison m ) ;
        Printexc.raise_with_backtrace e bt

  let use_ro m f =
    lock m ;
    match f () with
    | v ->
        unlock m ; v
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        unlock m ;
        Printexc.raise_with_backtrace e bt
end

module Condition = struct
  type t = {clock: Spin.t; cwaiters: waiter Queue.t}

  let create () = {clock= Spin.make (); cwaiters= Queue.create ()}

  let signal c =
    ignore (self ()) ;
    enqueue
      (Spin.with_lock c.clock (fun () ->
           match first c.cwaiters with Some w -> give w | None -> [] ) )

  let broadcast c =
    ignore (self ()) ;
    enqueue
      (Spin.with_lock c.clock (fun () ->
           let rec all acc =
             match first c.cwaiters with
             | Some w ->
                 all (give w @ acc)
             | None ->
                 acc
           in
           all [] ) )

  (* Inside [use_rw ~protect:true], the wait sees the cancels from outside the
     shield: the fiber waits with the contexts of [m.outer]. The lock again is
     in a shield, so the fiber always holds [m] when [wait] returns or
     raises. *)
  let wait c (m : Mutex.t) =
    let fiber = self () in
    let w = new_waiter () in
    Spin.with_lock c.clock (fun () -> Queue.push w c.cwaiters) ;
    let outer = m.outer in
    Mutex.unlock m ;
    let saved = fiber.ctxs in
    Option.iter (fun ctxs -> fiber.ctxs <- ctxs) outer ;
    let r =
      match Event.sync (Event.of_ivar w.wiv) with
      | () ->
          None
      | exception (Cancelled _ as e) ->
          Some (e, Printexc.get_raw_backtrace ())
    in
    fiber.ctxs <- saved ;
    ( match r with
    | Some _ ->
        (* A signal that came after the cancel goes to another waiter. *)
        let signalled =
          Spin.with_lock c.clock (fun () ->
              w.gone <- true ;
              is_full w )
        in
        if signalled then signal c
    | None ->
        () ) ;
    shield (fun () -> Mutex.lock m) ;
    m.outer <- outer ;
    match r with
    | None ->
        ()
    | Some (e, bt) ->
        m.wait_exn <- Some e ;
        Printexc.raise_with_backtrace e bt
end
