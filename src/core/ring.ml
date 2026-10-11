(* The local queue of a domain (design section 2.5, The state of each domain): a
   bounded FIFO ring, as in Go. The owner pushes at the tail and takes from the
   head. A thief takes half of the queue from the head, with a CAS on the head.
   Each slot is an atomic location, so that dscheck sees each access. *)

let size = 256

let mask = size - 1

type 'a t =
  {head: int Atomic.t; tail: int Atomic.t; buf: 'a option Atomic.t array}

let create () =
  { head= Atomic.make 0
  ; tail= Atomic.make 0
  ; buf= Array.init size (fun _ -> Atomic.make None) }

(* The owner only. It gives [false] if the ring is full. *)
let push r x =
  let t = Atomic.get r.tail in
  if t - Atomic.get r.head >= size then false
  else (
    Atomic.set r.buf.(t land mask) (Some x) ;
    Atomic.set r.tail (t + 1) ;
    true )

(* The owner only. A thief can take the head at the same time, so the take is a
   CAS on the head. *)
let rec pop r =
  let h = Atomic.get r.head in
  if Atomic.get r.tail = h then None
  else
    let x = Atomic.get r.buf.(h land mask) in
    if Atomic.compare_and_set r.head h (h + 1) then x else pop r

let is_empty r = Atomic.get r.tail - Atomic.get r.head <= 0

(* A thief takes half of the queue of [from], and puts all but one of them in
   its own ring [into], which is empty. It gives the one that it keeps. The
   thief reads the slots before its CAS on the head: until the head moves, the
   owner cannot write these slots. *)
let rec steal ~from ~into =
  let h = Atomic.get from.head in
  let t = Atomic.get from.tail in
  let n = t - h in
  let n = n - (n / 2) in
  if
    ( n <= 0 )
    [@mutate
      off "a steal of 0 items: the CAS of the head to itself does nothing"]
  then None
  else if n > size / 2 then steal ~from ~into (* the two reads did not agree *)
  else
    let xs = List.init n (fun i -> Atomic.get from.buf.((h + i) land mask)) in
    if not (Atomic.compare_and_set from.head h (h + n)) then steal ~from ~into
    else
      match xs with
      | first :: rest ->
          List.iter
            (function Some x -> ignore (push into x) | None -> ())
            rest ;
          first
      | [] ->
          None
