(* The tests of the fd table and of the wake rule of the scheduler (design
   section 2.5). *)

open Windtrap
module S = Weft.Private.Sched

(* A slot that the table uses again gets a new generation, so the token of the
   old open does not name the new one. *)
let new_generation () =
  let a = S.Fdtab.alloc 100 in
  S.Fdtab.release a ;
  let b = S.Fdtab.alloc 101 in
  equal int a.S.slot b.S.slot ;
  is_true (a.S.gen <> b.S.gen) ;
  is_true (S.Fdtab.token a <> S.Fdtab.token b) ;
  S.Fdtab.release b

(* When the table grows, each slot keeps its generation. *)
let growth_keeps_generations () =
  let size () = Array.length !S.Fdtab.gens in
  let first = S.Fdtab.alloc 100 in
  let before = size () and held = ref [] in
  while size () = before do
    held := S.Fdtab.alloc 100 :: !held
  done ;
  S.Fdtab.release first ;
  let again = S.Fdtab.alloc 100 in
  equal int first.S.slot again.S.slot ;
  greater int ~than:first.S.gen again.S.gen ;
  List.iter S.Fdtab.release (again :: !held)

(* K11 of the review of 2026-10-16: a token has 32 bits for the slot, so the
   table refuses a slot of 2^30 or more. The test gives the table the next slot
   2^30, with no free slot. *)
let slot_limit () =
  let next = !S.Fdtab.next and free = !S.Fdtab.free in
  S.Fdtab.free := [] ;
  S.Fdtab.next := 1 lsl 30 ;
  Fun.protect
    ~finally:(fun () ->
      S.Fdtab.next := next ;
      S.Fdtab.free := free )
    (fun () ->
      raises_match
        (function Assert_failure _ -> true | _ -> false)
        (fun () -> ignore (S.Fdtab.alloc 100)) ;
      equal int (1 lsl 30) !S.Fdtab.next )

(* A push marks a wake only if a domain is idle and no domain searches (the Go
   rules, design section 2.5). *)
let wake_rule () =
  let t = S.create ~domains:2 () in
  let d = t.S.doms.(0) in
  S.mark_wake d ;
  is_false ~msg:"no idle domain" d.S.wake ;
  Atomic.set t.S.idle (S.Idle.bit 1) ;
  Atomic.set t.S.searching 1 ;
  S.mark_wake d ;
  is_false ~msg:"a domain searches" d.S.wake ;
  Atomic.set t.S.searching 0 ;
  S.mark_wake d ;
  is_true ~msg:"an idle domain, and no search" d.S.wake

(* A waker with no other idle domain decreases [searching] again, because no
   domain gets its token. Domain 0 is the waker, and only domain 0 is in
   [idle]. *)
let give_with_no_idle () =
  let t = S.create ~domains:2 () in
  Atomic.set t.S.searching 1 ;
  Atomic.set t.S.idle (S.Idle.bit 0) ;
  is_true ~msg:"no domain to wake"
    (Option.is_none (S.Idle.give_token t ~self:0)) ;
  equal ~msg:"no domain holds the token" int 0 (Atomic.get t.S.searching)

(* The last searching domain that finds work wakes an idle domain before it runs
   the fiber. Domain 1 holds the token of a wake, and it steals the fiber of
   domain 0. Domain 2 is idle. The steal must wake domain 2 at once: a fiber
   that computes gives no effect, so a wake at the next effect can come late.
   The handler records the wakes of the world. *)
let wake_after_search () =
  let t = S.create ~domains:3 () in
  let d1 = t.S.doms.(1) in
  let node = S.new_node (S.Scope_node (S.new_scope_state ())) in
  S.push t.S.doms.(0) (S.new_fiber [node]) (fun () -> S.Finished) ;
  Atomic.set t.S.searching 1 ;
  d1.S.token <- true ;
  Atomic.set t.S.idle (S.Idle.bit 2) ;
  let woken = ref [] in
  let r =
    Effect.Deep.try_with S.steal d1
      { effc=
          (fun (type a) (e : a Effect.t) ->
            match e with
            | Weft.Private.World.Wake d ->
                Some
                  (fun (k : (a, _) Effect.Deep.continuation) ->
                    woken := d :: !woken ;
                    Effect.Deep.continue k () )
            | _ ->
                None ) }
  in
  is_true ~msg:"the steal gets the fiber" (Option.is_some r) ;
  equal ~msg:"the steal wakes domain 2" (list int) [2] !woken ;
  is_false ~msg:"no wake waits" d1.S.wake ;
  equal ~msg:"domain 2 holds the count" int 1 (Atomic.get t.S.searching) ;
  is_true (Atomic.get t.S.doms.(2).S.got_token)

(* Only the first cancel of a node walks it (design section 2.5, How a cancel
   reaches a fiber). A sync that starts after the flag reads the flag, so a
   second walk finds nothing to commit. This test parks a sync after the first
   cancel with no read of the flag: only a second walk can commit it. *)
let second_cancel_no_walk () =
  let node = S.new_node (S.Scope_node (S.new_scope_state ())) in
  equal int 0 (List.length (S.cancel_node node Exit)) ;
  let f = S.new_fiber [node] in
  ignore (S.add_member_to node f) ;
  let s =
    {S.word= S.C.make 0; owner= f; sctx= node; cancellable= true; k= None}
  in
  Atomic.set f.S.current (Some s) ;
  is_true (S.C.park s.S.word = `Parked) ;
  equal int 0 (List.length (S.cancel_node node Exit)) ;
  is_true ~msg:"the sync is still parked" (S.C.state s.S.word = S.C.Parked)

(* The members of a node are a linked list. A member leaves the list in any
   order, and the walk gets the others in the fork order. *)
let member_list () =
  let node = S.new_node (S.Scope_node (S.new_scope_state ())) in
  let fs = List.init 5 (fun _ -> S.new_fiber [node]) in
  let ms = Array.of_list (List.map (S.add_member_to node) fs) in
  List.iter (fun i -> S.remove_member node ms.(i)) [1; 4; 0] ;
  equal int 2 node.S.members.S.count ;
  equal (list int)
    (List.map (fun i -> (List.nth fs i).S.fid) [2; 3])
    (List.map (fun f -> f.S.fid) (S.member_fibers node.S.members)) ;
  List.iter (fun i -> S.remove_member node ms.(i)) [3; 2] ;
  equal int 0 node.S.members.S.count ;
  is_true (Option.is_none node.S.members.S.head)

let tests =
  [ group "fd table"
      [ test "a slot used again gets a new generation" new_generation
      ; test "the fd table refuses the slot 2^30" slot_limit
      ; test "a growth of the fd table keeps the generations"
          growth_keeps_generations ]
  ; group "cancel"
      [ test "a second cancel of a node does not walk it" second_cancel_no_walk
      ; test "a member leaves the list in any order" member_list ]
  ; group "wake rule"
      [ test "a push marks a wake only for an idle domain and no search"
          wake_rule
      ; test "a waker with no other idle domain decreases searching"
          give_with_no_idle
      ; test "the last searching domain wakes before it runs the fiber"
          wake_after_search ] ]
