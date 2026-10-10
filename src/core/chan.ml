(* Synchronous channels (design section 2.4, The atomic register, and
   spec/tla/Commit.tla). A channel has a short lock. The fiber holds it only
   while it looks for a partner, claims the two words or registers its offer,
   and never across a world effect. *)

open Sched

type 'a send_offer = {ssync: sync; sbranch: int; value: 'a}

type 'a recv_offer = {rsync: sync; rbranch: int; slot: 'a option ref}

type 'a t =
  { lock: Spin.t
  ; mutable senders: 'a send_offer list  (** The oldest first. *)
  ; mutable receivers: 'a recv_offer list  (** The oldest first. *) }

let create () = {lock= Spin.make (); senders= []; receivers= []}

(* A register drops the stale offers, so that a loop of [choose] with a timeout
   does not grow the list. *)
let live sync_of offers =
  List.filter (fun o -> not (is_done (sync_of o))) offers

(* Find a partner in [offers] and commit with it. An offer of the own sync is
   skipped, and a stale offer is removed. [commit] claims the two words and
   gives the claim. The result is the offers that stay, and [true] if the scan
   of the own sync must stop. *)
let rec meet dom s offers ~sync_of ~commit =
  match offers with
  | [] ->
      ([], false)
  | o :: rest when sync_of o == s ->
      let rest, stop = meet dom s rest ~sync_of ~commit in
      (o :: rest, stop)
  | o :: rest -> (
    match commit o with
    | C.Committed {resume_partner} ->
        if resume_partner then resume dom (sync_of o) ;
        (rest, true)
    | C.Stale ->
        meet dom s rest ~sync_of ~commit
    | C.Self_done ->
        (offers, true) )

let send c v =
  Event.Base
    (fun () ->
      { Event.leaf=
          { attempt=
              (fun dom s i ->
                Spin.with_lock c.lock (fun () ->
                    let receivers, stop =
                      meet dom s c.receivers
                        ~sync_of:(fun o -> o.rsync)
                        ~commit:(fun o ->
                          C.claim s.word i o.rsync.word o.rbranch
                            ~on_commit:(fun () -> o.slot := Some v ) )
                    in
                    c.receivers <- receivers ;
                    if not stop then
                      c.senders <-
                        live (fun o -> o.ssync) c.senders
                        @ [{ssync= s; sbranch= i; value= v}] ) ) }
      ; result= ignore } )

let recv c =
  Event.Base
    (fun () ->
      let slot = ref None in
      { Event.leaf=
          { attempt=
              (fun dom s i ->
                Spin.with_lock c.lock (fun () ->
                    let senders, stop =
                      meet dom s c.senders
                        ~sync_of:(fun o -> o.ssync)
                        ~commit:(fun o ->
                          C.claim s.word i o.ssync.word o.sbranch
                            ~on_commit:(fun () -> slot := Some o.value ) )
                    in
                    c.senders <- senders ;
                    if not stop then
                      c.receivers <-
                        live (fun o -> o.rsync) c.receivers
                        @ [{rsync= s; rbranch= i; slot}] ) ) }
      ; result= (fun () -> match !slot with Some v -> v | None -> assert false)
      } )
