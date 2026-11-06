exception Too_large

(* The bytes that the reader holds are [buf] from [pos], [len] bytes. [base] is
   the buffer of [size] bytes from [create]. A larger [buf] is temporary: the
   reader uses [base] again when it holds no byte. [spare] is the last larger
   buffer of at most [keep] bytes, for the next growth: thus a stream of values
   a little larger than [size] allocates no buffer for each value. [eof] is set
   when a read of the flow gave 0. *)
type t =
  { flow: Flow.t
  ; size: int
  ; max_size: int
  ; keep: int
  ; base: bytes
  ; mutable buf: bytes
  ; mutable spare: bytes
  ; mutable pos: int
  ; mutable len: int
  ; mutable eof: bool
  ; mutable consumed: int }

let create ?(size = 4096) flow =
  if size < 1 then invalid_arg "Reader.create: size must be positive" ;
  let base = Bytes.create size in
  { flow
  ; size
  ; max_size= Int.max size (1 lsl 24)
  ; keep= Int.max (16 * size) 65536
  ; base
  ; buf= base
  ; spare= Bytes.empty
  ; pos= 0
  ; len= 0
  ; eof= false
  ; consumed= 0 }

let buffered r = r.len

let consumed r = r.consumed

let buffer r = r.buf

let consume r n =
  if n < 0 || n > r.len then invalid_arg "Reader.consume: invalid count" ;
  r.len <- r.len - n ;
  r.consumed <- r.consumed + n ;
  if r.len = 0 then begin
    (* Each larger buffer is larger than [spare] (see [fill]), thus the reader
       keeps the last one. *)
    if Bytes.length r.buf <= r.keep && r.buf != r.base then r.spare <- r.buf ;
    r.pos <- 0 ;
    r.buf <- r.base
  end
  else r.pos <- r.pos + n

(* [move r dst] puts the bytes of [r] at the start of [dst]. [dst] can be
   [r.buf]: Bytes.blit is correct for an overlap. *)
let move r dst =
  Bytes.blit r.buf r.pos dst 0 r.len ;
  r.buf <- dst ;
  r.pos <- 0

(* [read_more r] reads the flow into the free space at the end of [buf], and
   gives the count. The free space is not empty. A count of 0 sets [eof]. *)
let read_more r =
  let start = r.pos + r.len in
  let n = Flow.read r.flow r.buf start (Bytes.length r.buf - start) in
  if n = 0 then r.eof <- true else r.len <- r.len + n ;
  n

(* [fill r n] reads until [r] holds [n] bytes, with [r.len < n <= max_size]. The
   buffer grows only when it is full. It allocates at most two times its size,
   or it takes the spare, which can be larger. Thus the memory follows the bytes
   that came, not the [n] of the caller. *)
let rec fill r n =
  let cap = Bytes.length r.buf in
  (* A full buffer here has [n > cap], because [r.len < n]. *)
  if n <= cap && r.pos + n > cap then move r r.buf
  else if r.pos + r.len = cap then begin
    let want = Int.min n (2 * cap) in
    (* The spare is never [buf] here: [buf] is full, and [want] is larger. *)
    let s = r.spare in
    move r (if Bytes.length s >= want then s else Bytes.create want)
  end ;
  if read_more r = 0 then raise End_of_file ;
  if r.len < n then fill r n

let ensure r n =
  if n > r.len then begin
    if n > r.max_size then raise Too_large ;
    if r.eof then raise End_of_file ;
    fill r n
  end
  else if n < 0 then invalid_arg "Reader.ensure: negative count"

(* [at r fn off k] is the position in [buf] of the [k] bytes at the offset
   [off], after a check that they are in the bytes of [r]. *)
let at r fn off k =
  if off < 0 || k < 0 || off > r.len - k then
    invalid_arg (fn ^ ": out of the buffered bytes") ;
  r.pos + off

let get_uint8 r off = Bytes.get_uint8 r.buf (at r "Reader.get_uint8" off 1)

let get_uint16_le r off =
  Bytes.get_uint16_le r.buf (at r "Reader.get_uint16_le" off 2)

let get_uint16_be r off =
  Bytes.get_uint16_be r.buf (at r "Reader.get_uint16_be" off 2)

let get_uint24_le r off =
  let i = at r "Reader.get_uint24_le" off 3 in
  Bytes.get_uint16_le r.buf i lor (Bytes.get_uint8 r.buf (i + 2) lsl 16)

let get_int32_le r off =
  Bytes.get_int32_le r.buf (at r "Reader.get_int32_le" off 4)

let get_int64_le r off =
  Bytes.get_int64_le r.buf (at r "Reader.get_int64_le" off 8)

let get_int64_be r off =
  Bytes.get_int64_be r.buf (at r "Reader.get_int64_be" off 8)

let view r off len = at r "Reader.view" off len

let sub_string r off len =
  Bytes.sub_string r.buf (at r "Reader.sub_string" off len) len

let read_into r buf off len =
  if off < 0 || len <= 0 || off > Bytes.length buf - len then
    invalid_arg "Reader.read_into: invalid range" ;
  if r.len = 0 && (not r.eof) && len >= r.size then begin
    (* A large part goes from the flow to [buf] with no copy. *)
    let n = Flow.read r.flow buf off len in
    if n = 0 then r.eof <- true ;
    r.consumed <- r.consumed + n ;
    n
  end
  else begin
    if r.len = 0 && not r.eof then ignore (read_more r) ;
    let n = Int.min len r.len in
    Bytes.blit r.buf r.pos buf off n ;
    consume r n ;
    n
  end
