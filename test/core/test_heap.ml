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

let heap =
  group "heap"
    [ prop "the heap gives the order of a sorted list"
        (Gen.with_pp (Format.pp_print_list pp_op) ops)
        heap_is_ordered ]

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

let tests =
  [ heap
  ; spin
  ; ring
  ; group "domains" [test "the number of domains is 1 to 62" domains_range] ]
