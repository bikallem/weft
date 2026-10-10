(** Spans: a signed count of nanoseconds (design section 2.6). *)

(** A span. The range is the range of [int64] nanoseconds, about 292 years in
    each direction. *)
type t

val zero : t

val ns : int -> t
(** [ns n] is [n] nanoseconds. *)

val us : int -> t
(** [us n] is [n] microseconds.

    @raise Invalid_argument if the result is out of range. *)

val ms : int -> t
(** [ms n] is [n] milliseconds.

    @raise Invalid_argument if the result is out of range. *)

val s : int -> t
(** [s n] is [n] seconds.

    @raise Invalid_argument if the result is out of range. *)

val of_float_s : float -> t
(** [of_float_s f] is [f] seconds, rounded toward zero to a nanosecond.

    @raise Invalid_argument if [f] is not a number or out of range. *)

val to_ns : t -> int64

val of_ns : int64 -> t

val compare : t -> t -> int

val equal : t -> t -> bool

val pp : Format.formatter -> t -> unit
(** [pp] prints the span in nanoseconds, for example [1500000000ns]. *)
