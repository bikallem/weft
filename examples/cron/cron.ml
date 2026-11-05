(* A job that runs each day at a time of the wall clock (UTC). The code uses
   only the API of weft, so it runs in each world. *)

open Weft

(* The text of a time, for example "2026-11-05 03:00:00". *)
let show t =
  let u = Time.to_utc t in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" u.year u.month u.day u.hour
    u.minute u.second

(* The first time after [now] at [hour]:[minute] UTC. A POSIX day always has
   86400 s, thus the same time on the next day is 86400 s later. *)
let next ~hour ~minute now =
  if hour < 0 || hour > 23 || minute < 0 || minute > 59 then
    invalid_arg "Cron.next" ;
  let today =
    Option.get (Time.of_utc {(Time.to_utc now) with hour; minute; second= 0})
  in
  if Span.compare (Time.diff Posix today now) Span.zero > 0 then today
  else Option.get (Time.add Posix today (Span.s 86400))

(* Run [job] each day at [hour]:[minute] UTC. [job] gets the time of its
   deadline. The wait is on the Posix line, thus it follows a step of the wall
   clock: after a step forward past the deadline, the job runs at once, and
   after a step back, it runs when the clock reaches the deadline again. A wait
   on the Mono line ([Time.after]) does not follow a step. *)
let every_day ~hour ~minute job =
  let rec loop () =
    let t = next ~hour ~minute (Time.now Posix) in
    sync (Time.at Posix t) ;
    job t ;
    loop ()
  in
  loop ()
