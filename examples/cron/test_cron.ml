(* The daily job in the simulator. [Sim.set_posix] sets the wall clock, and it
   makes the steps of the clock. A day of virtual time takes no real time. *)

open Weft
module Sim = Weft_sim

let utc ?(second = 0) year month day hour minute =
  Option.get (Time.of_utc {year; month; day; hour; minute; second; weekday= 0})

let () =
  (* [next] across the end of a day, a month, a year, and a leap day. *)
  let next = Cron.next ~hour:0 ~minute:15 in
  assert (next (utc 2026 11 5 23 30) = utc 2026 11 6 0 15) ;
  assert (next (utc 2026 11 5 0 10) = utc 2026 11 5 0 15) ;
  assert (next (utc 2026 1 31 0 15) = utc 2026 2 1 0 15) ;
  assert (next (utc 2026 12 31 1 0) = utc 2027 1 1 0 15) ;
  assert (next (utc 2028 2 28 1 0) = utc 2028 2 29 0 15) ;
  assert ((Time.to_utc (utc 2028 2 29 0 0)).weekday = 2) ;
  for seed = 1 to 10 do
    Sim.run ~seed ~domains:2 (fun sim ->
        Sim.set_posix sim (utc 2026 11 5 2 59) ;
        let t0 = Time.now Mono in
        (* The virtual time since the start, in s. *)
        let s_since () =
          Int64.to_int
            (Int64.div
               (Span.to_ns (Time.diff Mono (Time.now Mono) t0))
               1_000_000_000L )
        in
        let runs = Chan.create () in
        (* The deadline of the job, the wall clock and the Mono time when the
           job runs. *)
        let expect deadline wall s =
          let t = sync (Chan.recv runs) in
          assert (t = deadline) ;
          assert (Cron.show (Time.now Posix) = Cron.show wall) ;
          assert (s_since () = s)
        in
        Fiber.scope (fun sc ->
            Fiber.fork_daemon sc (fun () ->
                Cron.every_day ~hour:3 ~minute:0 (fun t ->
                    sync (Chan.send runs t) ) ) ;
            (* The first run comes after 60 s, and the next run one day
               later. *)
            let day = 86400 in
            expect (utc 2026 11 5 3 0) (utc 2026 11 5 3 0) 60 ;
            expect (utc 2026 11 6 3 0) (utc 2026 11 6 3 0) (60 + day) ;
            (* One hour later, the clock steps forward past the deadline of
               2026-11-07 03:00: the job runs at once. *)
            Time.sleep Mono (Span.s 3600) ;
            Sim.set_posix sim (utc 2026 11 7 5 0) ;
            expect (utc 2026 11 7 3 0) (utc 2026 11 7 5 0) (3660 + day) ;
            (* 1 s later, when the job waits for 2026-11-08 03:00, the clock
               steps back by 26 hours. The job runs two days later. *)
            Time.sleep Mono (Span.s 1) ;
            Sim.set_posix sim (utc 2026 11 6 3 0) ;
            expect (utc 2026 11 8 3 0) (utc 2026 11 8 3 0) (3661 + (3 * day)) ) )
  done
