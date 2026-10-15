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

(* A [Posix] deadline goes to the [Posix] heap of the domain. Its alarm follows
   a step of the clock (M3). *)
let at (type time) (line : time Clock.line) (t : time) =
  let deadline = Clock.to_ns line t in
  let leaf : Sched.leaf =
   fun dom s i ->
    let now, heap =
      match line with
      | Mono ->
          (Sched.now_ns (), dom.heap)
      | Posix ->
          (Sched.posix_ns (), dom.pheap)
    in
    if
      ( Int64.compare now deadline >= 0 )
      [@mutate off "an entry at now expires in the next poll, at the same time"]
    then ignore (Sched.C.self_commit s.word i)
    else Sched.Heap.push heap ~deadline s i
  in
  Event.Base (fun () -> {Event.leaf; result= ignore})

let after line span =
  Event.Guard
    (fun () ->
      match add line (now line) span with
      | None ->
          Event.never
      | Some t ->
          at line t )

let sleep line span = Event.sync (after line span)

let posix_of_unix_s f = Clock.of_ns Posix (Span.to_ns (Span.of_float_s f))

let posix_to_unix_s t = Int64.to_float (Clock.to_ns Posix t) /. 1e9

(* UTC calendar dates (design section 2.6). The code is pure integer code, so
   the core needs no [unix]. [days_from_civil] and [civil_from_days] are the
   algorithms of Howard Hinnant, "chrono-Compatible Low-Level Date Algorithms",
   https://howardhinnant.github.io/date_algorithms.html. They use a year that
   starts on 1 March, so the leap day is the last day of the year. A day number
   is the count of days since 1970-01-01. The OCaml [/] rounds to zero, so the
   code makes the era of a negative number with an offset, as the source
   does. *)

type utc =
  { year: int
  ; month: int
  ; day: int
  ; hour: int
  ; minute: int
  ; second: int
  ; weekday: int }

let days_from_civil y m d =
  let y = if m <= 2 then y - 1 else y in
  let era =
    ( ( if y >= 0 then y else y - 399 )
    [@mutate
      off
        "at y = 0 the two branches give the era 0, and of_utc gives None for \
         each year before 1 with each era"] )
    / 400
  in
  let yoe = y - (era * 400) in
  let doy = (((153 * if m > 2 then m - 3 else m + 9) + 2) / 5) + d - 1 in
  let doe = (yoe * 365) + (yoe / 4) - (yoe / 100) + doy in
  (era * 146097) + doe - 719468

let civil_from_days z =
  let z = z + 719468 in
  let era =
    ( if
        ( z >= 0 )
        [@mutate off "at z = 0, (z - 146096) / 146097 also gives the era 0"]
      then z
      else z - 146096 )
    / 146097
  in
  let doe = z - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let d = doy - (((153 * mp) + 2) / 5) + 1 in
  let m = if mp < 10 then mp + 3 else mp - 9 in
  let y = yoe + (era * 400) in
  ((if m <= 2 then y + 1 else y), m, d)

(* The floor of [a / b], for [b > 0]. *)
let fdiv a b = if a >= 0 then a / b else ((a + 1) / b) - 1

let to_utc t =
  (* The floor of the seconds: a time before 1970 with a fraction goes to the
     second before it. The seconds fit in an [int]. *)
  let ns = Clock.to_ns Posix t in
  let q = Int64.div ns 1_000_000_000L in
  let r = Int64.rem ns 1_000_000_000L in
  let secs = Int64.to_int (if r < 0L then Int64.pred q else q) in
  let days = fdiv secs 86400 in
  let rem = secs - (days * 86400) in
  let year, month, day = civil_from_days days in
  { year
  ; month
  ; day
  ; hour= rem / 3600
  ; minute= rem mod 3600 / 60
  ; second= rem mod 60
  ; weekday= days + 4 - (fdiv (days + 4) 7 * 7) }

let is_leap y = y mod 4 = 0 && (y mod 100 <> 0 || y mod 400 = 0)

let days_in_month y m =
  match m with
  | 2 ->
      if is_leap y then 29 else 28
  | 4 | 6 | 9 | 11 ->
      30
  | _ ->
      31

let of_utc u =
  (* The years of the two ends of [Clock.posix] are 1677 to 2262. A bound on the
     year keeps the integer arithmetic far from overflow. *)
  if
    ( u.year < -10000 )
    [@mutate
      off "at the year -10000, the range check of the seconds gives None"]
    || ( u.year > 10000 )
       [@mutate
         off "at the year 10000, the range check of the seconds gives None"]
    || u.month < 1 || u.month > 12 || u.day < 1
    || u.day > days_in_month u.year u.month
    || u.hour < 0 || u.hour > 23 || u.minute < 0 || u.minute > 59
    || u.second < 0 || u.second > 59
  then None
  else
    let secs =
      (days_from_civil u.year u.month u.day * 86400)
      + (u.hour * 3600) + (u.minute * 60) + u.second
    in
    let limit = Int64.to_int (Int64.div Int64.max_int 1_000_000_000L) in
    if secs > limit || secs < -limit then None
    else
      let ns = Int64.mul (Int64.of_int secs) 1_000_000_000L in
      Some (Clock.of_ns Posix ns)
