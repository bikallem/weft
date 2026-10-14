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

(* Give to each waiter that did not leave. *)
let give_all q =
  let rec all acc =
    match first q with Some w -> all (give w @ acc) | None -> acc
  in
  all []

(* A waiter leaves after a cancel, under the lock of its queue. It gives [true]
   if it got the lock or the signal before. *)
let leave lock w =
  Spin.with_lock lock (fun () ->
      w.gone <- true ;
      is_full w )

module Mutex = struct
  type t =
    { lock: Spin.t
    ; mutable held: bool
    ; mutable poisoned: bool
    ; waiters: waiter Queue.t
    ; mutable shield: ctx option
          (** Only the holder uses it: the shield node of
              [use_rw ~protect:true], which [Condition.wait] opens. It is [None]
              when the mutex is free. *)
    ; mutable wait_exn: exn option
          (** Only the holder uses it: the [Cancelled] that [Condition.wait]
              raised. It does not poison. *) }

  let create () =
    { lock= Spin.make ()
    ; held= false
    ; poisoned= false
    ; waiters= Queue.create ()
    ; shield= None
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
    enqueue
      (Spin.with_lock m.lock (fun () ->
           m.poisoned <- true ;
           m.held <- false ;
           give_all m.waiters ) )

  (* A cancellation point, also when the mutex is free. *)
  let lock m =
    check_cancel () ;
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
          if leave m.lock w then unlock m ;
          Printexc.raise_with_backtrace e bt )

  let use_rw ~protect m f =
    lock m ;
    m.wait_exn <- None ;
    let body () =
      if protect then
        shield_node (fun node ->
            m.shield <- Some node ;
            f () )
      else f ()
    in
    match body () with
    | v ->
        m.shield <- None ;
        unlock m ;
        v
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        m.shield <- None ;
        ( match m.wait_exn with
        | Some e' when e' == e ->
            unlock m
        | _ ->
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
    enqueue (Spin.with_lock c.clock (fun () -> give_all c.cwaiters))

  let set_open node o =
    match node with Some {kind= Shield_node a; _} -> Atomic.set a o | _ -> ()

  (* Inside [use_rw ~protect:true], the wait opens the shield of [use_rw]: it
     sees the cancels from outside and from the scopes inside. The node opens
     before the sync publishes its record, so a cancel walk that sees the sync
     sees the open node. The lock again is in a shield, so the fiber always
     holds [m] when [wait] returns or raises. *)
  let wait c (m : Mutex.t) =
    let w = new_waiter () in
    Spin.with_lock c.clock (fun () -> Queue.push w c.cwaiters) ;
    let node = m.shield in
    m.shield <- None ;
    Mutex.unlock m ;
    set_open node true ;
    let r =
      match Event.sync (Event.of_ivar w.wiv) with
      | () ->
          None
      | exception (Cancelled _ as e) ->
          Some (e, Printexc.get_raw_backtrace ())
    in
    set_open node false ;
    (* A signal that came after the cancel goes to another waiter. *)
    if Option.is_some r && leave c.clock w then signal c ;
    shield (fun () -> Mutex.lock m) ;
    m.shield <- node ;
    match r with
    | None ->
        ()
    | Some (e, bt) ->
        m.wait_exn <- Some e ;
        Printexc.raise_with_backtrace e bt
end

(* A counting semaphore (design section 2.5, Locks for fiber code, and
   spec/tla/Semaphore.tla). The acquire is a base event, as a channel is. The
   short lock of the semaphore makes "take a free permit, else register an
   offer" one atomic step. A release commits the word of the oldest waiter with
   a one-word commit, under the same lock. That commit gives the permit: the
   waiter gets it only if the commit wins. A waiter that lost its choose or got
   a cancel has a word that is already [Done]. Then the permit goes to the next
   waiter. Thus no step leaves a permit with a fiber that does not use it. *)
module Semaphore = struct
  type t =
    { slock: Spin.t
    ; max: int  (** The maximum of the free permits. *)
    ; free: int Atomic.t
          (** The free permits. Only a holder of [slock] changes it. It is an
              atomic location only so that dscheck sees each access. *)
    ; waiters: unit Chan.side
          (** The offers of the acquires, the oldest first. When [free] is more
              than 0, the queue is empty: a release gives a permit to a waiter
              before it makes the permit free, and an acquire adds an offer only
              when no permit is free. *) }

  (* The offers hold no value. *)
  let no_cell : unit option ref = ref None

  let create ?max n =
    let max = Option.value max ~default:n in
    if n < 0 then invalid_arg "Fiber.Semaphore.create: the count is negative" ;
    if max < n then
      invalid_arg "Fiber.Semaphore.create: the maximum is less than the count" ;
    (* With a maximum of 0, no release can ever make a permit free. This occurs
       for [create 0] with no [~max], and for [create ~max:0 0]. *)
    if max = 0 then
      invalid_arg
        "Fiber.Semaphore.create: the maximum is 0 (give a ~max of 1 or more)" ;
    {slock= Spin.make (); max; free= Atomic.make n; waiters= Chan.side ()}

  (* The leaf of [acquire]. A free permit goes to the sync only if the sync
     commits to this branch. *)
  let leaf t : leaf =
   fun _ s i ->
    Spin.with_lock t.slock (fun () ->
        let n = Atomic.get t.free in
        if n > 0 then
          match C.self_commit s.word i with
          | Ok () ->
              Atomic.set t.free (n - 1)
          | Error _ ->
              ()
        else Chan.add t.waiters {Chan.osync= s; obranch= i; cell= no_cell} )

  let acquire t = Event.Base (fun () -> {Event.leaf= leaf t; result= ignore})

  (* When a permit is free, no waiter is in the queue. Thus the try keeps the
     FIFO order. *)
  let try_acquire t =
    Spin.with_lock t.slock (fun () ->
        let n = Atomic.get t.free in
        n > 0
        &&
        ( Atomic.set t.free (n - 1) ;
          true ) )

  (* Give the permit to the oldest waiter that did not leave, or make it free.
     The result is the syncs to resume, or [None] if the free permits are at the
     maximum. *)
  let give t =
    Spin.with_lock t.slock (fun () ->
        let rec first () =
          match Chan.pop t.waiters with
          | None ->
              None
          | Some (o : unit Chan.offer) -> (
            match C.notify o.osync.word o.obranch with
            | C.Resume ->
                Some [o.osync]
            | C.In_scan ->
                Some []
            | C.Lost _ ->
                first () )
        in
        match first () with
        | Some resumed ->
            Some resumed
        | None ->
            (* Each offer was stale. *)
            let n = Atomic.get t.free in
            if n >= t.max then None
            else (
              Atomic.set t.free (n + 1) ;
              Some [] ) )

  (* [Self] first: outside a fiber, the release raises [Not_in_fiber] before it
     gives the permit to a waiter that nobody would then resume. *)
  let try_release t =
    ignore (self ()) ;
    match give t with Some resumed -> enqueue resumed ; true | None -> false

  let release t =
    if not (try_release t) then
      invalid_arg "Fiber.Semaphore.release: the free permits are at the maximum"

  let use t f =
    Event.sync (acquire t) ;
    match f () with
    | v ->
        release t ; v
    | exception e ->
        let bt = Printexc.get_raw_backtrace () in
        release t ;
        Printexc.raise_with_backtrace e bt
end
