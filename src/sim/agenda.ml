(* The timed events of the simulator: a binary min-heap on the time and then the
   sequence number. Thus an event comes after the events with the same time that
   came before it, as in the sorted list that the heap replaced (the review of
   2026-11-06). An insert into that list was linear, so a test with many packets
   in flight was quadratic. *)

type event = {at: int64; seq: int; f: unit -> unit}

type t = {mutable entries: event array; mutable size: int; mutable seq: int}

let dummy = {at= Int64.max_int; seq= max_int; f= ignore}

let create () = {entries= Array.make 64 dummy; size= 0; seq= 0}

let lt a b =
  match Int64.compare a.at b.at with 0 -> a.seq < b.seq | c -> c < 0

let swap h i j =
  let x = h.entries.(i) in
  h.entries.(i) <- h.entries.(j) ;
  h.entries.(j) <- x

let rec up h i =
  if i > 0 then
    let p = (i - 1) / 2 in
    if lt h.entries.(i) h.entries.(p) then (swap h i p ; up h p)

(* The slots past the size hold [dummy], which is last. *)
let rec down h i =
  let l = (2 * i) + 1 in
  let r = l + 1 in
  let m = if l < h.size && lt h.entries.(l) h.entries.(i) then l else i in
  let m = if r < h.size && lt h.entries.(r) h.entries.(m) then r else m in
  if m <> i then (swap h i m ; down h m)

let add h at f =
  if h.size = Array.length h.entries then (
    let a = Array.make (2 * h.size) dummy in
    Array.blit h.entries 0 a 0 h.size ;
    h.entries <- a ) ;
  h.entries.(h.size) <- {at; seq= h.seq; f} ;
  h.seq <- h.seq + 1 ;
  h.size <- h.size + 1 ;
  up h (h.size - 1)

(* The time of the first event. *)
let next h = if h.size = 0 then None else Some h.entries.(0).at

(* The first event, if its time is not after [now]. Its slot holds [dummy] after
   the take, so the heap does not keep the closure. *)
let take_due h now =
  if h.size = 0 || Int64.compare h.entries.(0).at now > 0 then None
  else
    let e = h.entries.(0) in
    h.size <- h.size - 1 ;
    h.entries.(0) <- h.entries.(h.size) ;
    h.entries.(h.size) <- dummy ;
    down h 0 ;
    Some e.f
