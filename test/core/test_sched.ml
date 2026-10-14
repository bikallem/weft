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

let tests =
  [ group "fd table"
      [ test "a slot used again gets a new generation" new_generation
      ; test "a growth of the fd table keeps the generations"
          growth_keeps_generations ]
  ; group "wake rule"
      [ test "a push marks a wake only for an idle domain and no search"
          wake_rule ] ]
