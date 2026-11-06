(** Buffered binary reads from a {!Flow.t}.

    A reader keeps the bytes that it read from its flow and did not consume.
    {!ensure} reads from the flow until the reader holds a given number of
    bytes. The functions [get_*] decode an integer at an offset in these bytes,
    and {!view} gives their position in {!buffer}, so a decoder can read them
    with no copy. {!consume} removes bytes from the start.

    An offset of these functions starts at the first byte that is not consumed.

    The checks of the offsets guard the bytes that the reader holds, not the
    value that the protocol reads. A read of the flow asks for all the free
    space of the buffer, thus the reader often holds the start of the next
    value. A decoder must check each field against the end of its value.

    For example, a MySQL packet has a length of 3 bytes, little-endian, then a
    sequence id of 1 byte, then the payload:
    {[
    Reader.ensure r 4 ;
    let len = Reader.get_uint24_le r 0 and seq = Reader.get_uint8 r 3 in
    Reader.consume r 4 ;
    Reader.ensure r len ;
    (* Decode the payload with get_* and view, from offset 0 to len. A field
       that ends after len is an error, also when get_* accepts it. *)
    Reader.consume r len
    ]}

    A reader has one user. Two calls at the same time on one reader are not
    supported. A call of the reader and a write of the same flow can run at the
    same time in two fibers. A function needs a weft fiber only when the read of
    the flow needs one.

    The reader sets no deadline and no timeout. They are those of the flow. A
    read timeout of the flow ({!Flow.set_read_timeout}) applies to each read of
    the flow, not to an {!ensure}, and an {!ensure} can do many reads. To limit
    the time of a value, use {!Flow.set_read_deadline}. *)

(** The type for readers. *)
type t

(** Raised by {!ensure} when it must hold more bytes than the maximum size of
    the reader: the larger of its [size] and 16 MiB ([1 lsl 24]). *)
exception Too_large

val create : ?size:int -> Flow.t -> t
(** [create ~size flow] is a reader of [flow]. Its buffer has [size] bytes
    (default 4096). The buffer grows for a larger {!ensure}, as the bytes come,
    to the maximum size at most (see {!Too_large}). When the reader holds no
    byte, it goes back to its buffer of [size] bytes. It keeps the last larger
    buffer for a later growth, if this buffer has at most the larger of
    [16 * size] and 64 KiB bytes.

    @raise Invalid_argument if [size < 1]. *)

val ensure : t -> int -> unit
(** [ensure r n] makes sure that [r] holds at least [n] bytes that are not
    consumed. If it holds fewer, it reads the flow until it holds [n] bytes.
    Each read asks for all the free space of the buffer, thus [r] can get more
    than [n] bytes. If [r] holds [n] bytes, [ensure] does not read the flow.

    After the call, the views and the result of {!buffer} from before the call
    are not valid.

    If [ensure] raises, [r] keeps all the bytes that it read, in their order,
    and it consumed no byte. A later [ensure] continues the same value. This
    applies also to a {!Flow.Timeout} and to a cancel.

    @raise Invalid_argument if [n < 0].
    @raise Too_large
      if [n] is more than the maximum size. [ensure] reads nothing then.
    @raise End_of_file
      if the stream ends before [r] holds [n] bytes. {!buffered} then tells what
      came: [0] is an end between two values, and more is an end in a value. A
      later [ensure] that needs more bytes raises [End_of_file], and does not
      read the flow.
    @raise Flow.Timeout
    @raise Flow.Closed
      and the other exceptions of the read of the flow, for example
      [Weft.Fiber.Cancelled]. *)

val buffered : t -> int
(** [buffered r] is the number of bytes of [r] that are not consumed. Before a
    change of the protocol of the flow, for example a start of TLS on it, make
    sure that it is [0]. Bytes in the reader at that time came before the
    change, and a peer that sends them can make the protocol read them as
    protected data. *)

val consumed : t -> int
(** [consumed r] is the number of bytes that [r] consumed since its creation,
    with {!consume} and {!read_into}. A protocol can compare it before and after
    a read of a value: if it did not change, the read took no byte of the value,
    for example after a cancel before the first byte. *)

val consume : t -> int -> unit
(** [consume r n] removes the first [n] bytes of [r]. It does not read the flow.
    The views from before the call are not valid.

    @raise Invalid_argument if [n < 0] or [n > buffered r]. *)

(** {1:integers Integers}

    [get_* r off] decodes the integer at the offset [off]. It consumes nothing.
    The suffix gives the byte order: [le] for little-endian, [be] for
    big-endian.

    Each one raises [Invalid_argument] if [off < 0], or if the bytes of the
    integer are not all in the [buffered r] bytes. *)

val get_uint8 : t -> int -> int

val get_uint16_le : t -> int -> int

val get_uint16_be : t -> int -> int

val get_uint24_le : t -> int -> int
(** [get_uint24_le r off] is the unsigned integer of 3 bytes at [off],
    little-endian, for example the length of a MySQL packet. *)

val get_int32_le : t -> int -> int32

val get_int64_le : t -> int -> int64

val get_int64_be : t -> int -> int64

(** {1:views Views} *)

val view : t -> int -> int -> int
(** [view r off len] is the position in [buffer r] of the [len] bytes at the
    offset [off]. Read them in [buffer r]. You can change them, for example to
    unmask them, because the reader does not read them again. They stay valid
    until the next {!ensure}, {!consume} or {!read_into} on [r].

    @raise Invalid_argument if [off < 0], [len < 0] or [off + len > buffered r].
*)

val buffer : t -> bytes
(** [buffer r] is the buffer of [r], for {!view}. Get it again after each
    {!ensure}, {!consume} or {!read_into}, because these calls can change the
    buffer. *)

val sub_string : t -> int -> int -> string
(** [sub_string r off len] is a copy of the [len] bytes at the offset [off].

    @raise Invalid_argument as {!view}. *)

(** {1:large Large values} *)

val read_into : t -> bytes -> int -> int -> int
(** [read_into r buf off len] moves at most [len] bytes of the stream into [buf]
    from [off], and gives the count. The count is [0] only at the end of the
    stream. If [r] holds bytes, they go first, and the flow is not read. If [r]
    holds no byte and [len] is at least the [size] of [r], one read of the flow
    puts the bytes into [buf] directly. If [len] is smaller, one read of the
    flow fills the buffer of [r], and the first bytes go to [buf].

    A protocol uses it for a value that it takes in parts, for example a
    WebSocket payload.

    @raise Invalid_argument
      if [len <= 0], or if [off] and [len] are not a valid range of [buf].
    @raise Flow.Timeout
    @raise Flow.Closed
      and the other exceptions of the read of the flow. After an exception, [r]
      consumed nothing. *)
