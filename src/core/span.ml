type t = int64

let zero = 0L

let ns n = Int64.of_int n

let scale name unit n =
  let n = Int64.of_int n in
  if
    Int64.compare n (Int64.div Int64.max_int unit) > 0
    || Int64.compare n (Int64.div Int64.min_int unit) < 0
  then invalid_arg name
  else Int64.mul n unit

let us = scale "Span.us" 1_000L

let ms = scale "Span.ms" 1_000_000L

let s = scale "Span.s" 1_000_000_000L

let of_float_s f =
  let n = f *. 1e9 in
  if Float.is_nan n || n >= 0x1p63 || n < -0x1p63 then
    invalid_arg "Span.of_float_s"
  else Int64.of_float n

let to_ns t = t

let of_ns t = t

let compare = Int64.compare

let equal = Int64.equal

let pp ppf t = Format.fprintf ppf "%Ldns" t
