exception Too_large

(* The bytes of the writer are [buf] from 0. [base] is the buffer of [size]
   bytes from [create]. A larger [buf] holds a large frame, and the writer uses
   [base] again after a flush.

   [spare] is the last larger buffer of at most [keep] bytes, for the next frame
   that grows: thus a stream of frames a little larger than [size] allocates no
   buffer for each frame.

   With no frame, [frame] is -1, and [len] is the number of bytes. In the body
   of a frame, [frame] is the offset of its header, and [len] is the end of the
   body. While the header fills, [filling] is set, [body_end] is the end of the
   body, and [len] is the next byte of the header. After an exception of a write
   of the flow, [broken] is set, [len] is 0 and [frame] is -1.

   [limit] is the largest end of a write that needs no check of the state (see
   [room]). *)
type t =
  { flow: Flow.t
  ; size: int
  ; max_frame: int
  ; keep: int
  ; base: bytes
  ; mutable buf: bytes
  ; mutable spare: bytes
  ; mutable len: int
  ; mutable limit: int
  ; mutable frame: int
  ; mutable header: int
  ; mutable filling: bool
  ; mutable body_end: int
  ; mutable broken: bool }

let broken_text = "Writer: a write of the flow failed before"

(* [set_limit w] sets [limit] for the state of [w]. With no frame, it is the end
   of the buffer. In a body, it is also at most the end of the largest frame.
   While the header fills, it is the end of the header less 1: thus the write
   that fills the header goes to [slow_room], which goes to the end of the body.
   When the writer is broken, it is -1: thus each write goes to [slow_room],
   which raises. *)
let set_limit w =
  w.limit <-
    ( if w.broken then -1
      else if w.frame < 0 then Bytes.length w.buf
      else if not w.filling then
        Int.min (Bytes.length w.buf) (w.frame + w.max_frame)
      else w.frame + w.header - 1 )

let create ?(size = 4096) ?(max_frame = 1 lsl 24) flow =
  if size < 8 then invalid_arg "Writer.create: size is less than 8" ;
  if max_frame < 1 then invalid_arg "Writer.create: max_frame must be positive" ;
  let base = Bytes.create size in
  let w =
    { flow
    ; size
    ; (* No frame is larger than a string. Thus [frame + max_frame] in
         [set_limit] does not overflow. *)
      max_frame= Int.min max_frame Sys.max_string_length
    ; keep= Int.max (16 * size) 65536
    ; base
    ; buf= base
    ; spare= Bytes.empty
    ; len= 0
    ; limit= 0
    ; frame= -1
    ; header= 0
    ; filling= false
    ; body_end= 0
    ; broken= false }
  in
  set_limit w ; w

let buffered w = if w.filling then w.body_end else w.len

(* [to_base w] makes [base] the buffer of [w], and keeps a larger buffer of at
   most [keep] bytes as [spare]. Each larger buffer is larger than [spare] (see
   [slow_room]), thus the writer keeps the last one. The caller moves the
   bytes. *)
let to_base w =
  if Bytes.length w.buf <= w.keep && w.buf != w.base then w.spare <- w.buf ;
  w.buf <- w.base

(* [clear w] drops the bytes and the frame of [w]. *)
let clear w =
  to_base w ;
  w.len <- 0 ;
  w.frame <- -1 ;
  w.filling <- false ;
  set_limit w

(* [send w buf off len] writes the bytes to the flow. On an exception, the
   writer drops all its bytes and is broken, because an unknown part of the
   bytes went. *)
let send w buf off len =
  match Flow.write w.flow buf off len with
  | () ->
      ()
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      w.broken <- true ;
      clear w ;
      Printexc.raise_with_backtrace e bt

(* [send_all w] writes all the bytes of [w], with no frame, to the flow. *)
let send_all w = send w w.buf 0 w.len ; clear w

let drop_frame w =
  if w.frame >= 0 then begin
    w.len <- w.frame ;
    w.frame <- -1 ;
    w.filling <- false ;
    (* With no byte, the writer goes back to [base]. With bytes, it keeps the
       buffer, as after a frame that ended, until the next send. *)
    if w.len = 0 then to_base w ;
    set_limit w
  end

(* [room w k] gives the offset of [k] bytes in [buf] for the next write, and
   counts them as written. The caller then puts the bytes there, with no other
   call of the writer between. *)
let rec room w k =
  let i = w.len in
  if i + k <= w.limit then begin
    w.len <- i + k ;
    i
  end
  else slow_room w k

and slow_room w k =
  if w.broken then invalid_arg broken_text ;
  if w.frame < 0 then begin
    (* The buffer is full: [k] is at most [size], and [len] is not 0. *)
    send_all w ; room w k
  end
  else if w.filling then begin
    (* The header fills: this write fills it, or crosses its end. *)
    let i = w.len in
    if i + k > w.frame + w.header then
      invalid_arg "Writer: the write crosses the end of the header" ;
    w.len <- w.body_end ;
    w.frame <- -1 ;
    w.filling <- false ;
    set_limit w ;
    i
  end
  else if w.len + k - w.frame > w.max_frame then begin
    drop_frame w ; raise Too_large
  end
  else begin
    (* The body needs space. The bytes before the frame are complete: they go to
       the flow, and the frame moves to the start. Then the buffer grows if the
       frame does not fit. *)
    if w.frame > 0 then begin
      send w w.buf 0 w.frame ;
      Bytes.blit w.buf w.frame w.buf 0 (w.len - w.frame) ;
      w.len <- w.len - w.frame ;
      w.frame <- 0
    end ;
    let cap = Bytes.length w.buf in
    if w.len + k > cap then begin
      (* The spare is never [buf] here: [buf] is full. *)
      let s = w.spare in
      let buf =
        if Bytes.length s >= w.len + k then s
        else Bytes.create (Int.min w.max_frame (Int.max (w.len + k) (2 * cap)))
      in
      Bytes.blit w.buf 0 buf 0 w.len ;
      w.buf <- buf
    end ;
    set_limit w ;
    room w k
  end

let check_range fn v hi =
  if v < 0 || v > hi then invalid_arg (fn ^ ": out of range")

let write_uint8 w v =
  check_range "Writer.write_uint8" v 0xff ;
  let i = room w 1 in
  Bytes.set_uint8 w.buf i v

let write_uint16_le w v =
  check_range "Writer.write_uint16_le" v 0xffff ;
  let i = room w 2 in
  Bytes.set_uint16_le w.buf i v

let write_uint16_be w v =
  check_range "Writer.write_uint16_be" v 0xffff ;
  let i = room w 2 in
  Bytes.set_uint16_be w.buf i v

let write_uint24_le w v =
  check_range "Writer.write_uint24_le" v 0xff_ffff ;
  let i = room w 3 in
  Bytes.set_uint16_le w.buf i (v land 0xffff) ;
  Bytes.set_uint8 w.buf (i + 2) (v lsr 16)

let write_int32_le w v =
  let i = room w 4 in
  Bytes.set_int32_le w.buf i v

let write_int64_le w v =
  let i = room w 8 in
  Bytes.set_int64_le w.buf i v

let write_int64_be w v =
  let i = room w 8 in
  Bytes.set_int64_be w.buf i v

let copy w src off len =
  let i = room w len in
  Bytes.blit src off w.buf i len

(* [rest w src off len] writes the bytes to the flow with no copy if they are at
   least [size] bytes. If not, it puts them into the buffer. *)
let rest w src off len =
  if len >= w.size then send w src off len else copy w src off len

(* [large w src off len] writes at least [size] bytes, with no frame, that do
   not fit in the free space. If the writer holds bytes, the start of [src]
   fills the free space, and the full buffer goes to the flow. Then [rest]
   writes the other bytes. *)
let large w src off len =
  if w.len > 0 then begin
    let k = Bytes.length w.buf - w.len in
    Bytes.blit src off w.buf w.len k ;
    w.len <- w.len + k ;
    send_all w ;
    rest w src (off + k) (len - k)
  end
  else rest w src off len

let write_bytes w src off len =
  if off < 0 || len < 0 || off > Bytes.length src - len then
    invalid_arg "Writer.write_bytes: invalid range" ;
  if w.broken then invalid_arg broken_text ;
  if w.frame >= 0 || len < w.size || w.len + len <= w.limit then
    copy w src off len
  else large w src off len

let write_string w s =
  write_bytes w (Bytes.unsafe_of_string s) 0 (String.length s)

let start_frame w ~header =
  if w.broken then invalid_arg broken_text ;
  if header < 1 then invalid_arg "Writer.start_frame: header must be positive" ;
  if w.frame >= 0 then invalid_arg "Writer.start_frame: a frame is open" ;
  w.frame <- w.len ;
  w.header <- header ;
  set_limit w ;
  ignore (room w header)

let end_frame w =
  if w.frame < 0 || w.filling then
    invalid_arg "Writer.end_frame: no body is open" ;
  let n = w.len - w.frame - w.header in
  w.filling <- true ;
  w.body_end <- w.len ;
  w.len <- w.frame ;
  set_limit w ;
  n

let abort_frame = drop_frame

let flush w =
  if w.broken then invalid_arg broken_text ;
  if w.frame >= 0 then invalid_arg "Writer.flush: a frame is open" ;
  if w.len > 0 then send_all w
