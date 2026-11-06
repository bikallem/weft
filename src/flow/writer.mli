(** Buffered binary writes to a {!Flow.t}.

    A writer keeps the bytes in its buffer until {!flush}, or until the buffer
    is full. Then it writes them to the flow with one {!Flow.write}. Thus a
    protocol writes a request in many small writes, and one flush sends it. A
    large write with no frame can give two writes of the flow (see
    {!write_string}).

    A frame is a header and a body, and the header holds the length of the body.
    {!start_frame} keeps space for the header. Then the caller writes the body.
    {!end_frame} gives the length of the body, and the next writes fill the
    header. For example, for a MySQL packet:
    {[
    Writer.start_frame w ~header:4 ;
    Writer.write_uint8 w 0x03 ;
    Writer.write_string w sql ;
    let n = Writer.end_frame w in
    Writer.write_uint24_le w n ;
    Writer.write_uint8 w seq ;
    Writer.flush w
    ]}
    The writer does not write the bytes of an open frame to the flow. If the
    code of the body raises, call {!abort_frame}, so that the next frame is
    correct.

    A writer has one user. Two calls at the same time on one writer are not
    supported. A writer and a {!Reader} on the same flow can run at the same
    time in two fibers. A function needs a weft fiber only when the write of the
    flow needs one. The writer sets no deadline and no timeout. They are those
    of the flow.

    A write of the flow can send a part of its bytes before an exception, for
    example a {!Flow.Timeout} or a cancel. Then the state of the stream is not
    known. Thus, after an exception of a write of the flow, the writer is
    broken: it drops its bytes, and each later {!flush}, write, {!start_frame}
    and {!end_frame} raises [Invalid_argument]. Close the flow. *)

(** The type for writers. *)
type t

(** Raised by a write in a frame or by {!start_frame} when the frame would be
    larger than the maximum frame size of the writer. *)
exception Too_large

val create : ?size:int -> ?max_frame:int -> Flow.t -> t
(** [create ~size ~max_frame flow] is a writer of [flow]. Its buffer has [size]
    bytes (default 4096). Outside a frame, a full buffer goes to the flow. In a
    frame, the buffer grows as the body grows. A frame, with its header, has at
    most [max_frame] bytes (default 16 MiB, [1 lsl 24]). After a flush, the
    writer goes back to its buffer of [size] bytes. It keeps the last larger
    buffer for a later frame, if this buffer has at most the larger of
    [16 * size] and 64 KiB bytes.

    @raise Invalid_argument if [size < 8] or [max_frame < 1]. *)

(** {1:integers Integers}

    [write_* w v] writes the integer [v]. The suffix gives the byte order: [le]
    for little-endian, [be] for big-endian.

    A write can write the buffer to the flow first, see {!flush}. It raises the
    exceptions of {!flush}, and also:
    - [Invalid_argument] if [v] is out of the range of the integer type, or if
      the write crosses the end of a header (see {!end_frame}). The writer does
      not change then.
    - {!Too_large} if it is in the body of a frame, and the frame would be
      larger than the maximum frame size. The writer then drops the frame, as
      {!abort_frame} does. *)

val write_uint8 : t -> int -> unit

val write_uint16_le : t -> int -> unit

val write_uint16_be : t -> int -> unit

val write_uint24_le : t -> int -> unit

val write_int32_le : t -> int32 -> unit

val write_int64_le : t -> int64 -> unit

val write_int64_be : t -> int64 -> unit

(** {1:bytes Bytes} *)

val write_string : t -> string -> unit
(** [write_string w s] writes [s]. When no frame is open, [s] has at least
    [size] bytes, and [s] does not fit in the free space, the writer does not
    copy all of [s]. It puts the start of [s] into its free space, if it holds
    bytes, and writes the full buffer to the flow. Then it writes the remaining
    bytes of [s] to the flow directly, if they are at least [size] bytes, or
    keeps them in its buffer. In a frame, [s] always goes into the buffer.

    The direct write saves a copy, but it gives two writes of the flow. A frame
    copies [s] into a buffer that the writer keeps (see {!create}), and gives
    one write. Thus a write with no frame is better only for a value much larger
    than the limit of the kept buffer. For a smaller value, use a frame or a
    larger [size]. *)

val write_bytes : t -> bytes -> int -> int -> unit
(** [write_bytes w buf off len] writes the [len] bytes of [buf] from [off], as
    {!write_string} does.

    @raise Invalid_argument if [off] and [len] are not a valid range of [buf].
*)

(** {1:frames Frames} *)

val start_frame : t -> header:int -> unit
(** [start_frame w ~header] starts a frame. It keeps [header] bytes for the
    header. The next writes are the body. If the buffer has no space, the writer
    first writes the bytes before the frame to the flow.

    @raise Invalid_argument if [header < 1], or if a frame is open.
    @raise Too_large if [header] is more than the maximum frame size. *)

val end_frame : t -> int
(** [end_frame w] ends the body of the open frame and gives its length: the
    number of bytes after the header. Then the next [header] bytes of writes
    fill the header. After them, the writes continue after the body. Until the
    header is full, the frame stays open.

    @raise Invalid_argument if no frame is open, or if its body ended. *)

val abort_frame : t -> unit
(** [abort_frame w] drops the open frame, also when its header is not full. The
    bytes before the frame stay. With no open frame, or on a broken writer, it
    does nothing. It never raises, thus a handler can call it before it raises
    the exception of the body again. *)

(** {1:flushing Flush} *)

val flush : t -> unit
(** [flush w] writes the bytes of the buffer to the flow with one {!Flow.write}.
    With no byte, it does not write the flow.

    @raise Invalid_argument
      if a frame is open, or if its header is not full, or if the writer is
      broken.
    @raise Flow.Timeout
    @raise Flow.Closed
      and the other exceptions of the write of the flow, for example
      [Weft.Fiber.Cancelled]. The writer is then broken. *)

val buffered : t -> int
(** [buffered w] is the number of bytes in the buffer of [w], with the open
    frame. While the header fills, it counts the frame up to the end of its
    body. Before a change of the protocol of the flow, for example a start of
    TLS on it, make sure that it is [0]. *)
