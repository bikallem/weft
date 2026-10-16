(* The tests of the deadline heap and the spin lock of the scheduler. *)

open Windtrap
module S = Weft.Private.Sched

(* A sync that never commits, for the entries of the heap. *)
let dummy_sync () =
  let node = S.new_node (S.Scope_node (S.new_scope_state ())) in
  let fiber = S.new_fiber [node] in
  {S.word= S.C.make 0; owner= fiber; sctx= node; cancellable= true; k= None}

type op = Push of int | Pop

let pp_op ppf = function
  | Push d ->
      Format.fprintf ppf "Push %d" d
  | Pop ->
      Format.pp_print_string ppf "Pop"

let ops =
  Gen.list ~size:(Gen.int_range 0 200)
    (Gen.with_pp pp_op
       (Gen.frequency
          [ (3, Gen.map (fun d -> Push d) (Gen.int_range 0 20))
          ; (2, Gen.constant Pop) ] ) )

(* The heap gives the entries in the order of the deadline, then of the push, as
   a sorted list does. *)
let heap_is_ordered ops =
  let h = S.Heap.create () in
  let model = ref [] and seq = ref 0 in
  let s = dummy_sync () in
  List.iter
    (function
      | Push d ->
          S.Heap.push h ~deadline:(Int64.of_int d) s !seq ;
          model := List.merge compare !model [(d, !seq)] ;
          incr seq
      | Pop -> (
        match (S.Heap.top h, !model) with
        | None, [] ->
            ()
        | Some e, (d, i) :: rest ->
            equal (pair int int) (d, i) (Int64.to_int e.deadline, e.branch) ;
            S.Heap.pop h ;
            model := rest
        | Some _, [] ->
            failwith "the heap has an entry that the model does not have"
        | None, _ :: _ ->
            failwith "the heap is empty, but the model is not" ) )
    ops ;
  equal int (List.length !model) h.size

let top_deadline h =
  Option.map (fun e -> Int64.to_int e.S.deadline) (S.Heap.top h)

(* It pushes entries at the deadlines 1 and 2, and gives a function that makes
   the two entries stale. *)
let two_entries h =
  let a = dummy_sync () and b = dummy_sync () in
  S.Heap.push h ~deadline:1L a 0 ;
  S.Heap.push h ~deadline:2L b 0 ;
  fun () ->
    ignore (S.C.self_commit a.S.word 1) ;
    ignore (S.C.self_commit b.S.word 1)

(* The stale entries at the head of the heap go. *)
let drop_stale () =
  let h = S.Heap.create () in
  let stale = two_entries h in
  S.Heap.push h ~deadline:3L (dummy_sync ()) 0 ;
  stale () ;
  S.Heap.drop_stale h ;
  equal int 1 h.S.size ;
  equal (option int) (Some 3) (top_deadline h)

(* An add drops the stale entries at the head (design section 2.5, Timers and
   domains). *)
let push_drops_stale () =
  let h = S.Heap.create () in
  let stale = two_entries h in
  stale () ;
  S.Heap.push h ~deadline:3L (dummy_sync ()) 0 ;
  equal int 1 h.S.size ;
  equal (option int) (Some 3) (top_deadline h)

(* The alarm is the earliest live deadline: a cancelled timeout does not set
   it. *)
let alarm_skips_stale () =
  let t = S.create ~slack:Weft.Span.zero ~domains:1 () in
  let d = t.S.doms.(0) in
  let first = dummy_sync () in
  S.Heap.push d.S.heap ~deadline:1L first 0 ;
  S.Heap.push d.S.heap ~deadline:2L (dummy_sync ()) 0 ;
  ignore (S.C.self_commit first.S.word 1) ;
  equal (option int64) (Some 2L) (S.alarm_of d d.S.heap)

(* A push at the limit, or the push after it, rebuilds the heap: it removes the
   stale entries, and the live entries keep their order. *)
let rebuild_keeps_live () =
  let h = S.Heap.create () in
  let syncs = List.init 64 (fun _ -> dummy_sync ()) in
  List.iteri
    (fun i s -> S.Heap.push h ~deadline:(Int64.of_int (i + 1)) s 0)
    syncs ;
  (* The entries at the even deadlines become stale. *)
  List.iteri
    (fun i s -> if i mod 2 = 1 then ignore (S.C.self_commit s.S.word 1))
    syncs ;
  (* Two pushes, so that the test does not depend on the exact size that starts
     the rebuild. *)
  S.Heap.push h ~deadline:100L (dummy_sync ()) 0 ;
  S.Heap.push h ~deadline:101L (dummy_sync ()) 0 ;
  equal int 34 h.S.size ;
  let rec all acc =
    match S.Heap.top h with
    | Some e ->
        S.Heap.pop h ;
        all (Int64.to_int e.S.deadline :: acc)
    | None ->
        List.rev acc
  in
  equal (list int) (List.init 32 (fun i -> (2 * i) + 1) @ [100; 101]) (all [])

(* P7: the slots past the size hold no sync of an entry that left the heap, so
   the heap does not keep its fiber alive. The test does pushes that grow the
   array, a rebuild, and pops. The old code kept the entries in the slots. *)
let slots_past_size () =
  let h = S.Heap.create () in
  let syncs = List.init 64 (fun _ -> dummy_sync ()) in
  let check () =
    for i = h.S.size to Array.length h.S.entries - 1 do
      let e = h.S.entries.(i) in
      is_false (List.exists (fun s -> e.S.esync == s) syncs)
    done
  in
  List.iteri
    (fun i s ->
      S.Heap.push h ~deadline:(Int64.of_int (i + 1)) s 0 ;
      check () )
    syncs ;
  List.iteri
    (fun i s -> if i mod 2 = 1 then ignore (S.C.self_commit s.S.word 1))
    syncs ;
  S.Heap.push h ~deadline:100L (dummy_sync ()) 0 ;
  S.Heap.push h ~deadline:101L (dummy_sync ()) 0 ;
  equal int 34 h.S.size ;
  check () ;
  while h.S.size > 2 do
    S.Heap.pop h ; check ()
  done

let heap =
  group "heap"
    [ prop "the heap gives the order of a sorted list"
        (Gen.with_pp (Format.pp_print_list pp_op) ops)
        heap_is_ordered
    ; test "a drop takes the stale entries at the head" drop_stale
    ; test "an add drops the stale entries at the head" push_drops_stale
    ; test "the alarm skips a stale entry" alarm_skips_stale
    ; test "a rebuild keeps only the live entries" rebuild_keeps_live
    ; test "the slots past the size hold no entry that left" slots_past_size ]

(* Two domains increment a counter under the lock. *)
let spin_excludes () =
  (* Mutation testing forks a child for each mutant, and OCaml refuses a fork
     after a domain started. *)
  if Option.is_some (Sys.getenv_opt "WINDTRAP_MUTATE") then
    skip ~reason:"a mutation run cannot start a domain" () ;
  let lock = S.Spin.make () and n = ref 0 in
  let work () =
    for _ = 1 to 100_000 do
      S.Spin.with_lock lock (fun () -> n := !n + 1)
    done
  in
  let d = Domain.spawn work in
  work () ; Domain.join d ; equal int 200_000 !n

let spin_unlocks_on_raise () =
  let lock = S.Spin.make () in
  raises Exit (fun () -> S.Spin.with_lock lock (fun () -> raise Exit)) ;
  equal int 1 (S.Spin.with_lock lock (fun () -> 1))

(* The lock is held while [f] runs, and free after. *)
let spin_held () =
  let lock = S.Spin.make () in
  is_true (S.Spin.with_lock lock (fun () -> Atomic.get lock)) ;
  is_false (Atomic.get lock)

let spin =
  group "spin lock"
    [ test "two domains do not enter the lock at the same time" spin_excludes
    ; test "the lock is free after a raise" spin_unlocks_on_raise
    ; test "the lock is held while f runs" spin_held ]

module Ring = Weft.Private.Ring

(* The ring of a domain, on one thread. *)
let ring_full () =
  let r = Ring.create () in
  for i = 1 to Ring.size do
    is_true (Ring.push r i)
  done ;
  is_false (Ring.push r 0) ;
  equal (option int) (Some 1) (Ring.pop r) ;
  is_true (Ring.push r (Ring.size + 1)) ;
  let rec all acc =
    match Ring.pop r with Some x -> all (x :: acc) | None -> List.rev acc
  in
  equal (list int) (List.init Ring.size (fun i -> i + 2)) (all [])

let ring_steal_full () =
  let r = Ring.create () and into = Ring.create () in
  for i = 1 to Ring.size do
    ignore (Ring.push r i)
  done ;
  (* A thief takes half: it keeps the first and puts the others in its ring. *)
  equal (option int) (Some 1) (Ring.steal ~from:r ~into) ;
  let rec count r n =
    match Ring.pop r with Some _ -> count r (n + 1) | None -> n
  in
  equal int ((Ring.size / 2) - 1) (count into 0) ;
  equal int (Ring.size / 2) (count r 0) ;
  equal (option int) None (Ring.steal ~from:r ~into)

(* The slots of the items that left by a pop or a steal hold them no more when
   the ring is empty, also after the indexes go around the ring. *)
let ring_clears () =
  let r = Ring.create () and into = Ring.create () in
  let held r =
    Array.fold_left
      (fun n slot -> if Option.is_some (Atomic.get slot) then n + 1 else n)
      0 r.Ring.buf
  in
  for round = 1 to 3 do
    for i = 1 to 200 do
      ignore (Ring.push r (round, i))
    done ;
    ignore (Ring.steal ~from:r ~into) ;
    while Option.is_some (Ring.pop r) do
      ()
    done ;
    equal ~msg:"the ring of the owner" int 0 (held r) ;
    while Option.is_some (Ring.pop into) do
      ()
    done ;
    equal ~msg:"the ring of the thief" int 0 (held into)
  done

(* Random pushes, pops and steals between two rings. At each pop on an empty
   ring with head h, the slots of h and h + 1 hold no item. *)
let ring_empty_slots () =
  let st = Random.State.make [|7|] in
  let held = ref 0 and empties = ref 0 in
  for trial = 1 to 200 do
    let a = Ring.create () and b = Ring.create () in
    let pushes = 1 + (trial mod 5) in
    for _ = 1 to 5000 do
      let r, o = if Random.State.bool st then (a, b) else (b, a) in
      match Random.State.int st 8 with
      | k when k < pushes ->
          ignore (Ring.push r 1)
      | k when k < 7 ->
          let h = Atomic.get r.Ring.head in
          if Atomic.get r.Ring.tail = h then (
            incr empties ;
            let slot i = Atomic.get r.Ring.buf.(i land Ring.mask) in
            if Option.is_some (slot h) || Option.is_some (slot (h + 1)) then
              incr held ) ;
          ignore (Ring.pop r)
      | _ ->
          if Ring.is_empty o then ignore (Ring.steal ~from:r ~into:o)
    done
  done ;
  greater int ~than:0 !empties ;
  equal int 0 !held

let ring =
  group "ring"
    [ test "a full ring refuses a push and keeps the FIFO order" ring_full
    ; test "a steal takes half of a full ring" ring_steal_full
    ; test "an empty ring holds no item" ring_clears
    ; test "the slots at the head of an empty ring hold no item"
        ring_empty_slots ]

let domains_range () =
  let invalid = function Invalid_argument _ -> true | _ -> false in
  raises_match invalid (fun () -> S.create ~domains:0 ()) ;
  raises_match invalid (fun () -> S.create ~domains:63 ()) ;
  equal int 1 (Array.length (S.create ~domains:1 ()).S.doms) ;
  equal int 62 (Array.length (S.create ~domains:62 ()).S.doms)

(* The beats of a new scheduler are 0, also in memory that held other values. *)
let beats_start_at_zero () =
  for n = 1 to 8 do
    let junk = Bigarray.Array1.create Bigarray.int Bigarray.c_layout n in
    Bigarray.Array1.fill junk (-1) ;
    ignore (Sys.opaque_identity junk) ;
    Gc.full_major () ;
    let t = S.create ~domains:n () in
    for i = 0 to Bigarray.Array1.dim t.S.beats - 1 do
      equal int 0 t.S.beats.{i}
    done
  done

(* Two domains do not write their beats in one cache line: the beats are 128
   bytes or more apart (review 3.12, P2). *)
let beats_apart () =
  for n = 2 to 8 do
    let t = S.create ~domains:n () in
    let at = Array.make n (-1) in
    Array.iteri
      (fun d dom ->
        S.beat dom ;
        for i = 0 to Bigarray.Array1.dim t.S.beats - 1 do
          if t.S.beats.{i} = 1 && not (Array.mem i at) then at.(d) <- i
        done )
      t.S.doms ;
    Array.iteri
      (fun d i ->
        if d > 0 then
          at_least int ~than:128 ((i - at.(d - 1)) * (Sys.word_size / 8)) )
      at
  done

let tests =
  [ heap
  ; spin
  ; ring
  ; group "domains"
      [ test "the number of domains is 1 to 62" domains_range
      ; test "the beats start at 0" beats_start_at_zero
      ; test "the beats of two domains are 128 bytes apart" beats_apart ] ]
