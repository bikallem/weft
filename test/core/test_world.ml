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
        alarm_saturates ]

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
    ; test "Time.now outside run raises Not_in_fiber" time_outside_fiber ]

let tests = [world; span; time]
