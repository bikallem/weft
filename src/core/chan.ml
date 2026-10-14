(* Synchronous channels (design section 2.4, The atomic register, and
   spec/tla/Commit.tla). A channel has a short lock. The fiber holds it only
   while it looks for a partner, claims the two words or registers its offer,
   and never across a world effect. *)

open Sched

(* An offer. The cell of a send holds the value, and the cell of a recv gets
   it. *)
type 'a offer = {osync: sync; obranch: int; cell: 'a option ref}

(* The offers of one side, the oldest first: [front], then [back] in the reverse
   order. An add costs O(1). [len] counts the live offers of the last drop of
   the stale offers, and the adds since then. The side drops the stale offers
   when [len] comes to [drop_at], twice the live offers of the last drop. *)
type 'a side =
  { front: 'a offer list Atomic.t
  ; back: 'a offer list Atomic.t
  ; mutable len: int
  ; mutable drop_at: int }

type 'a t = {lock: Spin.t; senders: 'a side; receivers: 'a side}

(* The lock of the channel guards [front] and [back]. They are atomic locations
   only so that dscheck sees each access. *)
let side () = {front= Atomic.make []; back= Atomic.make []; len= 0; drop_at= 8}

let create () = {lock= Spin.make (); senders= side (); receivers= side ()}

(* All offers, the oldest first. *)
let offers side =
  ( match Atomic.get side.back with
  | [] ->
      ()
  | back ->
      Atomic.set side.front (Atomic.get side.front @ List.rev back) ;
      Atomic.set side.back [] ) ;
  Atomic.get side.front

(* Remove the oldest offer and give it. [back] goes to [front] only when [front]
   is empty. Thus each offer moves one time, and a pop costs O(1) on average. *)
let rec pop side =
  match Atomic.get side.front with
  | o :: rest ->
      Atomic.set side.front rest ; Some o
  | [] -> (
    match Atomic.get side.back with
    | [] ->
        None
    | back ->
        Atomic.set side.front (List.rev back) ;
        Atomic.set side.back [] ;
        pop side )

let add side o =
  Atomic.set side.back (o :: Atomic.get side.back) ;
  side.len <- side.len + 1 ;
  if
    ( side.len >= side.drop_at )
    [@mutate
      off "one more offer before a drop: only the time of the drop changes"]
  then (
    let live = List.filter (fun o -> not (is_done o.osync)) (offers side) in
    Atomic.set side.front live ;
    side.len <- List.length live ;
    side.drop_at <- Int.max 8 (2 * side.len) )

(* Find a partner in [l] and commit with it. An offer of the own sync is
   skipped, and a stale offer is removed. The result is the offers that stay,
   and [true] if the scan of the own sync must stop. *)
let rec meet dom s l ~commit =
  match l with
  | [] ->
      ([], false)
  | o :: rest when o.osync == s ->
      let rest, stop = meet dom s rest ~commit in
      (o :: rest, stop)
  | o :: rest -> (
    match commit o with
    | C.Committed {resume_partner} ->
        if resume_partner then resume ~next:true dom o.osync ;
        (rest, true)
    | C.Stale ->
        meet dom s rest ~commit
    | C.Self_done ->
        (l, true) )

(* The leaf of one side. [move o] moves the value between the own cell and the
   cell of the partner [o]. The scan takes the offers in the order of [offers],
   but it does not copy the queue. It scans [front] first. It scans [back] only
   if no partner in [front] stops the scan. After a scan of [front] that does
   not stop, [rest] holds only the offers of the own sync, so it is short. Thus
   a send or a recv costs O(1) on average, not O(N) in the offers. The lists
   change only after the scans, so an exception in a scan loses no offer.
   [meet], [commit] and [resume] do not change [other]. *)
let leaf c ~mine ~other ~cell ~move : leaf =
 fun dom s i ->
  Spin.with_lock c.lock (fun () ->
      let commit o =
        C.claim s.word i o.osync.word o.obranch ~on_commit:(fun () -> move o)
      in
      let rest, stop = meet dom s (Atomic.get other.front) ~commit in
      let rest, stop =
        match Atomic.get other.back with
        | back when (not stop) && back <> [] ->
            let rest2, stop = meet dom s (List.rev back) ~commit in
            Atomic.set other.back [] ;
            (rest @ rest2, stop)
        | _ ->
            (rest, stop)
      in
      Atomic.set other.front rest ;
      if not stop then add mine {osync= s; obranch= i; cell} )

let send c v =
  Event.Base
    (fun () ->
      let cell = ref (Some v) in
      { Event.leaf=
          leaf c ~mine:c.senders ~other:c.receivers ~cell ~move:(fun o ->
              o.cell := Some v )
      ; result= ignore } )

let recv c =
  Event.Base
    (fun () ->
      let cell = ref None in
      { Event.leaf=
          leaf c ~mine:c.receivers ~other:c.senders ~cell ~move:(fun o ->
              cell := !(o.cell) )
      ; result= (fun () -> match !cell with Some v -> v | None -> assert false)
      } )
