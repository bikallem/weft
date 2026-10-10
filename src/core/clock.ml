type mono = Mono_ns of int64 [@@unboxed]

type posix = Posix_ns of int64 [@@unboxed]

type 'time line = Mono : mono line | Posix : posix line

let to_ns : type time. time line -> time -> int64 =
 fun line t ->
  match (line, t) with Mono, Mono_ns n -> n | Posix, Posix_ns n -> n

let of_ns : type time. time line -> int64 -> time =
 fun line n -> match line with Mono -> Mono_ns n | Posix -> Posix_ns n
