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
  { mutable front: 'a offer list
  ; mutable back: 'a offer list
  ; mutable len: int
  ; mutable drop_at: int }

type 'a t = {lock: Spin.t; senders: 'a side; receivers: 'a side}

let side () = {front= []; back= []; len= 0; drop_at= 8}

let create () = {lock= Spin.make (); senders= side (); receivers= side ()}

(* All offers, the oldest first. *)
let offers side =
  ( match side.back with
  | [] ->
      ()
  | back ->
      side.front <- side.front @ List.rev back ;
      side.back <- [] ) ;
  side.front

let add side o =
  side.back <- o :: side.back ;
  side.len <- side.len + 1 ;
  if
    ( side.len >= side.drop_at )
    [@mutate
      off "one more offer before a drop: only the time of the drop changes"]
  then (
    let live = List.filter (fun o -> not (is_done o.osync)) (offers side) in
    side.front <- live ;
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
        if resume_partner then resume dom o.osync ;
        (rest, true)
    | C.Stale ->
        meet dom s rest ~commit
    | C.Self_done ->
        (l, true) )

(* The leaf of one side. [move o] moves the value between the own cell and the
   cell of the partner [o]. *)
let leaf c ~mine ~other ~cell ~move : leaf =
 fun dom s i ->
  Spin.with_lock c.lock (fun () ->
      let rest, stop =
        meet dom s (offers other) ~commit:(fun o ->
            C.claim s.word i o.osync.word o.obranch ~on_commit:(fun () ->
                move o ) )
      in
      other.front <- rest ;
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
