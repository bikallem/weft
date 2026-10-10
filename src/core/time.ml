(* Time (design section 2.6). *)

let now line = World.perform "Time.now" (World.Now line)

(* [a + b] or [a - b] overflows when the sign of the result is not the sign that
   the operands give. *)
let add line t span =
  let a = Clock.to_ns line t and b = Span.to_ns span in
  let r = Int64.add a b in
  if Int64.logand (Int64.logxor a r) (Int64.logxor b r) < 0L then None
  else Some (Clock.of_ns line r)

(* Saturate on overflow. *)
let diff line a b =
  let a = Clock.to_ns line a and b = Clock.to_ns line b in
  let r = Int64.sub a b in
  if Int64.logand (Int64.logxor a b) (Int64.logxor a r) >= 0L then Span.of_ns r
  else if a < 0L then Span.of_ns Int64.min_int
  else Span.of_ns Int64.max_int

let at (_ : Clock.mono Clock.line) t =
  let deadline = Clock.to_ns Mono t in
  let leaf : Sched.leaf =
   fun dom s i ->
    if
      ( Int64.compare (Sched.now_ns ()) deadline
      >= 0 )
      [@mutate off "an entry at now expires in the next poll, at the same time"]
    then ignore (Sched.C.self_commit s.word i)
    else Sched.Heap.push dom.heap ~deadline s i
  in
  Event.Base (fun () -> {Event.leaf; result= ignore})

let after (_ : Clock.mono Clock.line) span =
  Event.Guard
    (fun () ->
      match add Mono (now Mono) span with
      | None ->
          Event.never
      | Some t ->
          at Mono t )

let sleep line span = Event.sync (after line span)

let posix_of_unix_s f = Clock.of_ns Posix (Span.to_ns (Span.of_float_s f))

let posix_to_unix_s t = Int64.to_float (Clock.to_ns Posix t) /. 1e9
