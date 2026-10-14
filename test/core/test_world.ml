(* The tests of [Weft.run] with a test world, and of [Span] and [Time] (design
   sections 2.3, 2.5 and 2.6). *)

open Windtrap
open Weft

(* A world with a manual clock. A sleep moves the clock to the alarm. Each read
   of [Mono] can also move the clock by [tick] nanoseconds, as the time moves
   while a domain is busy. *)
module Manual = struct
  type t =
    { mutable now: int64
    ; tick: int64
    ; mutable alarms: int64 list  (** The newest first. *)
    ; mutable polls: int
    ; mutable randoms: int }

  let make ?(tick = 0L) () = {now= 0L; tick; alarms= []; polls= 0; randoms= 0}

  let now (type time) t (line : time Clock.line) : time =
    let n = t.now in
    t.now <- Int64.add t.now t.tick ;
    Clock.of_ns line n

  let submit _ (_ : _ Private.Op.t) = Error (Private.Op.Other "no network")

  let wait t ~domain:_ (w : Private.World.Wait.t) =
    match w.timeout with
    | Poll ->
        t.polls <- t.polls + 1 ;
        []
    | Sleep -> (
      match w.deadline with
      | Some d ->
          let d = Clock.to_ns Mono d in
          t.alarms <- d :: t.alarms ;
          if Int64.compare d t.now > 0 then t.now <- d ;
          [Private.World.Ready.Alarm]
      | None ->
          failwith "the program sleeps forever" )

  let wake _ ~domain:_ = ()

  let random t n =
    t.randoms <- t.randoms + 1 ;
    n - 1

  let random_bytes _ b = Bytes.fill b 0 (Bytes.length b) 'x'
end

let run ?slack w f = Weft.run ?slack (module Manual) w f

let ms n = Int64.mul (Int64.of_int n) 1_000_000L

let us n = Int64.mul (Int64.of_int n) 1_000L

let now_ns () = Clock.to_ns Mono (Time.now Mono)

exception Boom

let run_gives_value () =
  let w = Manual.make () in
  equal int 3 (run w (fun () -> 3))

let run_raises () =
  let w = Manual.make () in
  raises Boom (fun () -> run w (fun () -> raise Boom))

(* The alarm rounds up to the slack: one alarm serves near deadlines. *)
let alarm_rounds_up () =
  let w = Manual.make () in
  let woke =
    run ~slack:(Span.ms 1) w (fun () ->
        Time.sleep Mono (Span.us 1500) ;
        now_ns () )
  in
  equal (list int64) [ms 2] w.alarms ;
  equal int64 (ms 2) woke

let slack_zero_is_exact () =
  let w = Manual.make () in
  let woke =
    run ~slack:Span.zero w (fun () ->
        Time.sleep Mono (Span.us 1500) ;
        now_ns () )
  in
  equal (list int64) [us 1500] w.alarms ;
  equal int64 (us 1500) woke

(* The deadlines in one slot of the slack share one alarm, and no timer fires
   before its deadline. *)
let slack_shares_alarm () =
  let w = Manual.make () in
  let woke =
    run ~slack:(Span.ms 1) w (fun () ->
        Fiber.scope (fun s ->
            let a =
              Fiber.fork_promise s (fun () ->
                  Time.sleep Mono (Span.us 1200) ;
                  now_ns () )
            in
            let b =
              Fiber.fork_promise s (fun () ->
                  Time.sleep Mono (Span.us 1700) ;
                  now_ns () )
            in
            let c =
              Fiber.fork_promise s (fun () ->
                  Time.sleep Mono (Span.us 1000) ;
                  now_ns () )
            in
            (Promise.await a, Promise.await b, Promise.await c) ) )
  in
  equal (triple int64 int64 int64) (ms 2, ms 2, ms 1) woke ;
  equal (list int64) [ms 2; ms 1] w.alarms

let no_early_fire =
  let open Gen in
  let+ slack_us = int_range 0 5000 and+ sleep_us = int_range 0 20000 in
  (slack_us, sleep_us)

let never_early (slack_us, sleep_us) =
  let w = Manual.make () in
  let woke =
    run ~slack:(Span.us slack_us) w (fun () ->
        Time.sleep Mono (Span.us sleep_us) ;
        now_ns () )
  in
  at_least int64 ~than:(us sleep_us) woke ;
  less int64 ~than:(Int64.add (us sleep_us) (Int64.add (us slack_us) 1L)) woke

let default_slack_is_100us () =
  let w = Manual.make () in
  ignore (run w (fun () -> Time.sleep Mono (Span.us 1550))) ;
  equal (list int64) [us 1600] w.alarms

(* A busy domain polls each 61 switches, so a due timer runs before a long loop
   of yields ends (design section 2.5, A busy domain also polls). *)
let busy_domain_polls () =
  let w = Manual.make ~tick:(ms 1) () in
  let order = ref [] in
  run ~slack:Span.zero w (fun () ->
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              Time.sleep Mono (Span.ms 1) ;
              order := "timer" :: !order ) ;
          Fiber.fork s (fun () ->
              for _ = 1 to 200 do
                Fiber.yield ()
              done ;
              order := "loop" :: !order ) ) ) ;
  equal (list string) ["timer"; "loop"] (List.rev !order) ;
  (* About one poll for each 61 switches, not one for each switch. *)
  greater int ~than:0 w.polls ;
  less int ~than:20 w.polls

(* An idle poll that makes a timer ready does not sleep. *)
let idle_poll_finds_timer () =
  let w = Manual.make ~tick:(us 400) () in
  run ~slack:Span.zero w (fun () -> Time.sleep Mono (Span.ms 1)) ;
  equal (list int64) [] w.alarms

(* The scan order comes from the PRNG of the domain. The world gives its seed
   one time, when the domain starts. *)
let one_seed_for_each_domain () =
  let w = Manual.make () in
  ignore
    (run w (fun () ->
         for _ = 1 to 10 do
           ignore (sync (choose [always "a"; always "b"; always "c"]))
         done ) ) ;
  equal int 1 w.randoms

(* K1: a deadline that passes between the poll and the sleep. The domain must
   not sleep with the fiber in its queue. *)
let expire_before_sleep () =
  let w = Manual.make ~tick:(us 400) () in
  run ~slack:Span.zero w (fun () -> Time.sleep Mono (Span.us 1300))

(* C4: the slack rounding of a deadline near the largest time saturates. *)
let alarm_saturates () =
  let w = Manual.make () in
  run ~slack:(Span.ms 1) w (fun () ->
      sync (Time.at Mono (Clock.of_ns Mono (Int64.sub Int64.max_int 10L))) ) ;
  equal (list int64) [Int64.max_int] w.alarms

(* [on_create] gets the scheduler before [main] starts. *)
let on_create_runs_first () =
  let w = Manual.make () and created = ref false in
  is_true
    (Private.run_world
       ~on_create:(fun _ -> created := true)
       (module Manual)
       w
       (fun () -> !created) )

(* A finaliser runs outside a fiber. Its fill from outside reaches the run. *)
let fill_from_outside_in_run () =
  let w = Manual.make () in
  equal string "gc"
    (run w (fun () ->
         let iv = Ivar.create () and filled = ref false in
         let () =
           let x = ref 1 in
           Gc.finalise (fun _ -> filled := Ivar.fill_from_outside iv "gc") x
         in
         Gc.full_major () ;
         is_true !filled ;
         sync (Ivar.read iv) ) )

(* After the end of [run], no run takes a fill from outside. *)
let no_fill_after_run () =
  let w = Manual.make () in
  run w (fun () -> ()) ;
  is_false (Ivar.fill_from_outside (Ivar.create ()) 1)

(* An exception of the world in the loop of domain 0 goes to the caller. *)
let world_error_of_domain_0 () =
  let w = Manual.make () in
  raises_match
    (function Failure m -> m = "the program sleeps forever" | _ -> false)
    (fun () -> run w (fun () -> sync (Ivar.read (Ivar.create ()))))

(* After a run that ended with an error of the world, no run takes a fill from
   outside. *)
let no_fill_after_error () =
  let w = Manual.make () in
  raises_match
    (function Failure _ -> true | _ -> false)
    (fun () -> run w (fun () -> sync (Ivar.read (Ivar.create ())))) ;
  is_false (Ivar.fill_from_outside (Ivar.create ()) 1)

(* The minor words of one sync of [ev], on average. *)
let words_of_sync ev =
  let w = Manual.make () in
  run w (fun () ->
      for _ = 1 to 100 do
        ignore (sync ev)
      done ;
      let w0 = Gc.minor_words () in
      for _ = 1 to 1000 do
        ignore (sync ev)
      done ;
      (Gc.minor_words () -. w0) /. 1000. )

(* A sync of a base event does not flatten the event (M3). Thus it allocates
   much less than a sync of a choose of the same event: 60 words and 118 words
   in OCaml 5.5. *)
let base_needs_no_flatten () =
  let base = words_of_sync (always 1)
  and choice = words_of_sync (choose [always 1]) in
  if not (base +. 25. < choice) then
    fail
      (Printf.sprintf "a base sync allocates %.1f words, a choose %.1f" base
         choice )

let world =
  group "Weft.run"
    [ test "run gives the value of main" run_gives_value
    ; test "run raises the exception of main" run_raises
    ; test "the alarm rounds up to the slack" alarm_rounds_up
    ; test "a slack of zero gives the exact alarm" slack_zero_is_exact
    ; test "the deadlines in one slot share one alarm" slack_shares_alarm
    ; prop "a sleep ends at its deadline or later, by less than the slack"
        (Gen.with_pp
           (fun ppf (a, b) -> Format.fprintf ppf "(slack %dus, sleep %dus)" a b)
           no_early_fire )
        never_early
    ; test "the default slack is 100 us" default_slack_is_100us
    ; test "a busy domain polls its timers" busy_domain_polls
    ; test "an idle poll that makes a timer ready does not sleep"
        idle_poll_finds_timer
    ; test "the world gives one seed for each domain" one_seed_for_each_domain
    ; test ~timeout:5. "K1: a deadline between the poll and the sleep fires"
        expire_before_sleep
    ; test ~timeout:5. "C4: the alarm near the largest time saturates"
        alarm_saturates
    ; test "on_create gets the scheduler before main" on_create_runs_first
    ; test "a sync of a base event does not flatten it" base_needs_no_flatten
    ; test "a fill from outside a fiber reaches the run"
        fill_from_outside_in_run
    ; test "after the end of run, a fill from outside gives false"
        no_fill_after_run
    ; test "after an error of the world, a fill from outside gives false"
        no_fill_after_error
    ; test "an error of the world on domain 0 goes to the caller"
        world_error_of_domain_0 ]

(* Span *)

let span_units () =
  equal int64 5L (Span.to_ns (Span.ns 5)) ;
  equal int64 5_000L (Span.to_ns (Span.us 5)) ;
  equal int64 5_000_000L (Span.to_ns (Span.ms 5)) ;
  equal int64 5_000_000_000L (Span.to_ns (Span.s 5)) ;
  equal int64 (-5_000_000_000L) (Span.to_ns (Span.s (-5))) ;
  equal int64 1_500_000_000L (Span.to_ns (Span.of_float_s 1.5)) ;
  equal int64 0L (Span.to_ns Span.zero) ;
  equal string "7ns" (Format.asprintf "%a" Span.pp (Span.ns 7)) ;
  is_true (Span.equal (Span.ms 1) (Span.us 1000)) ;
  equal int (-1) (Span.compare (Span.ns 1) (Span.ns 2))

let invalid = function Invalid_argument _ -> true | _ -> false

let span_limits () =
  let max_s = Int64.to_int (Int64.div Int64.max_int 1_000_000_000L) in
  let min_s = Int64.to_int (Int64.div Int64.min_int 1_000_000_000L) in
  equal int64
    (Int64.mul (Int64.of_int max_s) 1_000_000_000L)
    (Span.to_ns (Span.s max_s)) ;
  equal int64
    (Int64.mul (Int64.of_int min_s) 1_000_000_000L)
    (Span.to_ns (Span.s min_s)) ;
  raises_match invalid (fun () -> Span.s (max_s + 1)) ;
  raises_match invalid (fun () -> Span.s (min_s - 1)) ;
  raises_match invalid (fun () -> Span.ms max_int) ;
  raises_match invalid (fun () -> Span.us min_int) ;
  raises_match invalid (fun () -> Span.of_float_s Float.nan) ;
  raises_match invalid (fun () -> Span.of_float_s 9.3e9) ;
  raises_match invalid (fun () -> Span.of_float_s (-9.3e9)) ;
  equal int64 (-9_000_000_000_000_000_000L)
    (Span.to_ns (Span.of_float_s (-9e9))) ;
  (* The exact limits: 2^63 ns is out of range, and -2^63 ns is in range. *)
  let exact target =
    let f = target /. 1e9 in
    match
      List.find_opt (fun g -> g *. 1e9 = target) [f; Float.pred f; Float.succ f]
    with
    | Some g ->
        g
    | None ->
        failwith "no float gives the limit exactly"
  in
  raises_match invalid (fun () -> Span.of_float_s (exact 0x1p63)) ;
  equal int64 Int64.min_int (Span.to_ns (Span.of_float_s (exact (-0x1p63))))

(* [ms] multiplies exactly in its range. *)
let ms_range =
  let bound = Int64.to_int (Int64.div Int64.max_int 1_000_000L) in
  Gen.frequency
    [ (6, Gen.int_range (-bound) bound)
    ; (1, Gen.of_list ~pp:Format.pp_print_int [bound; -bound; bound - 1; 0]) ]

let ms_is_exact n =
  equal int64 (Int64.mul (Int64.of_int n) 1_000_000L) (Span.to_ns (Span.ms n))

let span =
  group "Span"
    [ test "the units" span_units
    ; test "the limits raise Invalid_argument" span_limits
    ; prop "ms n is n * 10^6 ns" ms_range ms_is_exact ]

(* Time: [add] and [diff] need no world. *)

let mono n = Clock.of_ns Mono n

let int64_edges =
  Gen.frequency
    [ (4, Gen.int64)
    ; (2, Gen.map Int64.of_int Gen.small_int)
    ; ( 1
      , Gen.of_list
          ~pp:(fun ppf x -> Format.fprintf ppf "%LdL" x)
          [ Int64.max_int
          ; Int64.min_int
          ; Int64.pred Int64.max_int
          ; Int64.succ Int64.min_int
          ; 0L
          ; 1L
          ; -1L ] ) ]

let add_overflows_exactly (a, b) =
  let expect_none =
    (Int64.compare b 0L > 0 && Int64.compare a (Int64.sub Int64.max_int b) > 0)
    || Int64.compare b 0L < 0
       && Int64.compare a (Int64.sub Int64.min_int b) < 0
  in
  match Time.add Mono (mono a) (Span.of_ns b) with
  | None ->
      is_true expect_none
  | Some t ->
      is_false expect_none ;
      equal int64 (Int64.add a b) (Clock.to_ns Mono t) ;
      (* Law: diff (add t d) t = d *)
      equal int64 b (Span.to_ns (Time.diff Mono t (mono a)))

let diff_saturates () =
  equal int64 Int64.max_int
    (Span.to_ns (Time.diff Mono (mono 0L) (mono Int64.min_int))) ;
  equal int64 Int64.max_int
    (Span.to_ns (Time.diff Mono (mono Int64.max_int) (mono (-1L)))) ;
  equal int64 Int64.min_int
    (Span.to_ns (Time.diff Mono (mono Int64.min_int) (mono 1L))) ;
  equal int64 (-3L) (Span.to_ns (Time.diff Mono (mono 2L) (mono 5L)))

let posix_round_trip () =
  equal (float 1e-9) 1.5 (Time.posix_to_unix_s (Time.posix_of_unix_s 1.5)) ;
  equal (float 1e-3) 1_700_000_000.25
    (Time.posix_to_unix_s (Time.posix_of_unix_s 1_700_000_000.25))

(* [posix s] is the time [s] seconds after 1970-01-01 00:00:00 UTC. *)
let posix s = Clock.of_ns Posix (Int64.mul (Int64.of_int s) 1_000_000_000L)

let utc (year, month, day, hour, minute, second, weekday) =
  {Time.year; month; day; hour; minute; second; weekday}

let check_utc secs fields =
  let u = utc fields in
  is_true (Time.to_utc (posix secs) = u) ;
  match Time.of_utc u with
  | None ->
      fail "of_utc gave None"
  | Some t ->
      equal int64 (Clock.to_ns Posix (posix secs)) (Clock.to_ns Posix t)

let utc_known () =
  check_utc 0 (1970, 1, 1, 0, 0, 0, 4) ;
  check_utc (-1) (1969, 12, 31, 23, 59, 59, 3) ;
  check_utc 951_782_400 (2000, 2, 29, 0, 0, 0, 2) ;
  check_utc 1_791_828_000 (2026, 10, 12, 18, 0, 0, 1) ;
  check_utc 1_000_000_000 (2001, 9, 9, 1, 46, 40, 0) ;
  check_utc (-86400) (1969, 12, 31, 0, 0, 0, 3) ;
  (* The first and the last whole second of [Clock.posix]. *)
  check_utc 9_223_372_036 (2262, 4, 11, 23, 47, 16, 5) ;
  check_utc (-9_223_372_036) (1677, 9, 21, 0, 12, 44, 2)

let utc_fraction () =
  (* A fraction cuts to the second at or before the time. *)
  let at ns = Time.to_utc (Clock.of_ns Posix ns) in
  equal int 0 (at 999_999_999L).second ;
  equal int 59 (at (-1L)).second ;
  equal int 1969 (at (-1L)).year ;
  equal int 58 (at (-1_000_000_001L)).second ;
  (* The smallest [Clock.posix] is in the second 00:12:43, which [of_utc]
     rejects. *)
  let lo = at Int64.min_int in
  is_true (lo = utc (1677, 9, 21, 0, 12, 43, 2)) ;
  is_true (Time.of_utc lo = None) ;
  is_true (at Int64.max_int = utc (2262, 4, 11, 23, 47, 16, 5))

(* The first day of each month, in a leap year and in a common year. *)
let utc_months () =
  let first_days y =
    List.init 12 (fun i ->
        let m = i + 1 in
        match Time.of_utc (utc (y, m, 1, 0, 0, 0, 0)) with
        | None ->
            fail "of_utc gave None"
        | Some t ->
            let u = Time.to_utc t in
            (u.year, u.month, u.day, u.weekday) )
  in
  (* 2024 is a leap year. *)
  is_true
    ( first_days 2024
    = [ (2024, 1, 1, 1)
      ; (2024, 2, 1, 4)
      ; (2024, 3, 1, 5)
      ; (2024, 4, 1, 1)
      ; (2024, 5, 1, 3)
      ; (2024, 6, 1, 6)
      ; (2024, 7, 1, 1)
      ; (2024, 8, 1, 4)
      ; (2024, 9, 1, 0)
      ; (2024, 10, 1, 2)
      ; (2024, 11, 1, 5)
      ; (2024, 12, 1, 0) ] ) ;
  (* 2023 is a common year. *)
  is_true
    ( first_days 2023
    = [ (2023, 1, 1, 0)
      ; (2023, 2, 1, 3)
      ; (2023, 3, 1, 3)
      ; (2023, 4, 1, 6)
      ; (2023, 5, 1, 1)
      ; (2023, 6, 1, 4)
      ; (2023, 7, 1, 6)
      ; (2023, 8, 1, 2)
      ; (2023, 9, 1, 5)
      ; (2023, 10, 1, 0)
      ; (2023, 11, 1, 3)
      ; (2023, 12, 1, 5) ] ) ;
  (* The last day of February, and the century rules. *)
  let ok f = Time.of_utc (utc f) <> None in
  is_true (ok (2024, 2, 29, 0, 0, 0, 0)) ;
  is_false (ok (2023, 2, 29, 0, 0, 0, 0)) ;
  is_false (ok (1900, 2, 29, 0, 0, 0, 0)) ;
  is_true (ok (2000, 2, 29, 0, 0, 0, 0))

(* Each day of the range of [Clock.posix], against a slow reference: a walk of
   the calendar one day at a time, from 1677-09-22. The walk crosses each era,
   each century and each leap day of the algorithm. *)
let utc_each_day () =
  let leap y = y mod 4 = 0 && (y mod 100 <> 0 || y mod 400 = 0) in
  let month_days y m =
    match m with
    | 2 ->
        if leap y then 29 else 28
    | 4 | 6 | 9 | 11 ->
        30
    | _ ->
        31
  in
  let next (y, m, d, wd) =
    let wd = (wd + 1) mod 7 in
    if d < month_days y m then (y, m, d + 1, wd)
    else if m < 12 then (y, m + 1, 1, wd)
    else (y + 1, 1, 1, wd)
  in
  let date = ref (1677, 9, 22, 3) in
  for days = -106_751 to 106_751 do
    let y, m, d, wd = !date in
    (* Noon of the day. *)
    let secs = (days * 86400) + 43200 in
    let u = utc (y, m, d, 12, 0, 0, wd) in
    if Time.to_utc (posix secs) <> u then
      fail (Printf.sprintf "to_utc differs from the walk at day %d" days) ;
    ( match Time.of_utc u with
    | Some t when Clock.to_ns Posix t = Clock.to_ns Posix (posix secs) ->
        ()
    | _ ->
        fail (Printf.sprintf "of_utc differs from the walk at day %d" days) ) ;
    if days = 0 then is_true ((y, m, d, wd) = (1970, 1, 1, 4)) ;
    date := next !date
  done ;
  is_true (!date = (2262, 4, 12, 6))

let utc_invalid () =
  let none f = Time.of_utc (utc f) = None in
  is_true (none (2026, 0, 1, 0, 0, 0, 0)) ;
  is_true (none (2026, 13, 1, 0, 0, 0, 0)) ;
  is_true (none (2026, 4, 31, 0, 0, 0, 0)) ;
  is_true (none (2026, 4, 0, 0, 0, 0, 0)) ;
  is_true (none (2026, 4, 1, 24, 0, 0, 0)) ;
  is_true (none (2026, 4, 1, 0, 60, 0, 0)) ;
  (* No leap second in POSIX time. *)
  is_true (none (2016, 12, 31, 23, 59, 60, 0)) ;
  is_true (none (2026, 4, 1, 0, 0, -1, 0)) ;
  (* Outside the range of [Clock.posix]. *)
  is_true (none (2262, 4, 12, 0, 0, 0, 0)) ;
  is_true (none (1677, 9, 20, 0, 0, 0, 0)) ;
  is_true (none (1677, 9, 21, 0, 12, 43, 0)) ;
  is_true (none (9999, 12, 31, 23, 59, 59, 0)) ;
  is_true (none (max_int, 1, 1, 0, 0, 0, 0)) ;
  is_true (none (min_int, 1, 1, 0, 0, 0, 0))

let time_outside_fiber () =
  raises_match
    (function Fiber.Not_in_fiber "Time.now" -> true | _ -> false)
    (fun () -> Time.now Mono)

let time =
  group "Time"
    [ prop "add gives None exactly on overflow"
        (Gen.pair int64_edges int64_edges)
        add_overflows_exactly
    ; test "diff saturates" diff_saturates
    ; test "posix and unix seconds" posix_round_trip
    ; test "to_utc and of_utc: known dates" utc_known
    ; test "to_utc cuts the fraction of a second" utc_fraction
    ; test "to_utc: the months of a leap year and a common year" utc_months
    ; test "of_utc: a date out of range gives None" utc_invalid
    ; test "to_utc and of_utc agree with a walk of each day" utc_each_day
    ; test "Time.now outside run raises Not_in_fiber" time_outside_fiber ]

(* A world that watches each signal and gives the signals of [batches], one
   batch at each wait. It records each signal that the core sends again. *)
module Signals = struct
  type t =
    { mutable batches: Private.World.Ready.t list list
    ; mutable resent: Signal.t list }

  let now (type time) _ (line : time Clock.line) : time = Clock.of_ns line 0L

  let submit : type a. t -> a Private.Op.t -> (a, Private.Op.error) result =
   fun t op ->
    match op with
    | Private.Op.Watch s ->
        Ok s
    | Private.Op.Unwatch s ->
        Ok s
    | Private.Op.Resend s ->
        t.resent <- s :: t.resent ;
        Ok ()
    | _ ->
        Error (Private.Op.Other "no network")

  let wait t ~domain:_ (_ : Private.World.Wait.t) =
    match t.batches with
    | b :: rest ->
        t.batches <- rest ;
        b
    | [] ->
        failwith "the program sleeps forever"

  let wake _ ~domain:_ = ()

  let random _ n = n - 1

  let random_bytes _ b = Bytes.fill b 0 (Bytes.length b) 'x'
end

(* A count of 0 is a signal that did not come. A waiter of a watched signal does
   not get it, and the core does not send an unwatched signal again. *)
let signal_count_zero () =
  let w =
    { Signals.batches=
        [ [ Private.World.Ready.Signal (SIGUSR1, 0)
          ; Private.World.Ready.Signal (SIGUSR2, 0) ]
        ; [Private.World.Ready.Signal (SIGUSR1, 2)] ]
    ; resent= [] }
  in
  let n =
    Weft.run
      (module Signals)
      w
      (fun () ->
        Signal.unwatch SIGUSR2 ;
        sync (Signal.received SIGUSR1) )
  in
  equal int 2 n ;
  equal int 0 (List.length w.resent)

let signals =
  group "signals" [test "a count of 0 is not a signal" signal_count_zero]

let tests = [world; span; time; signals]
