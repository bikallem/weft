(* The tests of the details of the scheduler that the mutation run found: the
   heap, the fd table, the wake rule, and the runs on a world with a manual
   clock (design sections 2.5 and 2.7). *)

open Windtrap
open Weft
module S = Private.Sched
module Manual = Test_world.Manual

(* A sync of the heap that committed to another branch: a stale entry. *)
let stale_sync () =
  let s = Test_heap.dummy_sync () in
  ignore (S.C.self_commit s.S.word 1) ;
  s

let top_deadline h =
  Option.map (fun e -> Int64.to_int e.S.deadline) (S.Heap.top h)

(* The stale entries at the head of the heap go. *)
let drop_stale () =
  let h = S.Heap.create () in
  let first = Test_heap.dummy_sync () in
  S.Heap.push h ~deadline:1L first 0 ;
  S.Heap.push h ~deadline:2L (Test_heap.dummy_sync ()) 0 ;
  ignore (S.C.self_commit first.S.word 1) ;
  S.Heap.drop_stale h ;
  equal int 1 h.S.size ;
  equal (option int) (Some 2) (top_deadline h)

(* An add drops the stale entries at the head (design section 2.5, Timers and
   domains). *)
let push_drops_stale () =
  let h = S.Heap.create () in
  S.Heap.push h ~deadline:1L (stale_sync ()) 0 ;
  S.Heap.push h ~deadline:5L (Test_heap.dummy_sync ()) 0 ;
  equal int 1 h.S.size ;
  equal (option int) (Some 5) (top_deadline h)

(* The alarm is the earliest live deadline: a cancelled timeout does not set
   it. *)
let alarm_skips_stale () =
  let t = S.create ~slack:Span.zero ~domains:1 () in
  let d = t.S.doms.(0) in
  let first = Test_heap.dummy_sync () in
  S.Heap.push d.S.heap ~deadline:1L first 0 ;
  S.Heap.push d.S.heap ~deadline:2L (Test_heap.dummy_sync ()) 0 ;
  ignore (S.C.self_commit first.S.word 1) ;
  equal (option int64) (Some 2L) (S.alarm_of d d.S.heap)

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
  equal int (first.S.gen + 1) again.S.gen ;
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

(* The beats of a new scheduler are 0, also in memory that held other values. *)
let beats_start_at_zero () =
  for n = 1 to 8 do
    let junk = Bigarray.Array1.create Bigarray.int Bigarray.c_layout n in
    Bigarray.Array1.fill junk (-1) ;
    ignore (Sys.opaque_identity junk) ;
    Gc.full_major () ;
    let t = S.create ~domains:n () in
    for i = 0 to n - 1 do
      equal int 0 t.S.beats.{i}
    done
  done

let us n = Int64.mul (Int64.of_int n) 1_000L

(* K1 with a pinned fiber: its deadline passes between the poll and the sleep.
   The expire puts it in the pinned queue, and domain 0 must not sleep. *)
let pinned_expire_before_sleep () =
  let w = Manual.make ~tick:(us 400) () in
  Test_world.run ~slack:Span.zero w (fun () ->
      Fiber.scope (fun s ->
          Fiber.fork ~pinned:true s (fun () -> Time.sleep Mono (Span.us 1300)) ) )

(* A Register that the world refuses gives its error to the waiter. *)
let register_error () =
  let w = Manual.make () in
  Test_world.run w (fun () ->
      let fd = S.Fdtab.alloc 7 in
      raises
        (Net.Error (Other "no network", "Net"))
        (fun () -> Private.Event.sync (Private.Net.readable fd)) ;
      S.Fdtab.release fd )

(* A fill from outside reaches its waiter while another fiber yields: the domain
   takes the injection queue at each 61st take. *)
let inject_while_busy () =
  let w = Manual.make () in
  let yields = ref 0 in
  Test_world.run w (fun () ->
      Fiber.scope (fun s ->
          let iv = Ivar.create () and filled = ref false in
          Fiber.fork s (fun () ->
              sync (Ivar.read iv) ;
              filled := true ) ;
          Fiber.fork s (fun () ->
              while (not !filled) && !yields < 1000 do
                incr yields ; Fiber.yield ()
              done ) ;
          Fiber.yield () ;
          is_true (Ivar.fill_from_outside iv ()) ) ) ;
  less int ~than:100 !yields

(* Outside a fiber, a cancel raises [Not_in_fiber]. *)
let cancel_outside () =
  let sc = ref None in
  Test_world.run (Manual.make ()) (fun () ->
      Fiber.scope (fun s -> sc := Some s) ) ;
  raises_match
    (function Fiber.Not_in_fiber _ -> true | _ -> false)
    (fun () -> Fiber.cancel (Option.get !sc) Exit)

let tests =
  [ group "scheduler details"
      [ test "a drop takes the stale entries at the head" drop_stale
      ; test "an add drops the stale entries at the head" push_drops_stale
      ; test "the alarm skips a stale entry" alarm_skips_stale
      ; test "a slot used again gets a new generation" new_generation
      ; test "a growth of the fd table keeps the generations"
          growth_keeps_generations
      ; test "a push marks a wake only for an idle domain and no search"
          wake_rule
      ; test "the beats start at 0" beats_start_at_zero
      ; test ~timeout:5.
          "K1: a pinned deadline between the poll and the sleep fires"
          pinned_expire_before_sleep
      ; test "a Register that fails raises in the waiter" register_error
      ; test "a fill from outside reaches a waiter while a fiber yields"
          inject_while_busy
      ; test "a cancel outside a fiber raises Not_in_fiber" cancel_outside ] ]
