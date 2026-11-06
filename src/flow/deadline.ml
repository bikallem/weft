(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

open Weft

(* [Clock.mono] has no [compare]. [Time.diff] saturates, thus its sign is
   correct also for two times that are far apart. *)
let compare a b = Span.compare (Time.diff Mono a b) Span.zero

let from_now span = Time.add Mono (Time.now Mono) span

let has_passed = function
  | None ->
      false
  | Some d ->
      compare (Time.now Mono) d >= 0

let earlier a b =
  match (a, b) with
  | None, d | d, None ->
      d
  | Some x, Some y ->
      (* At equal times, [a] and [b] hold the same time. *)
      if (compare x y <= 0) [@mutate off "the same time"] then a else b

let at = function None -> never | Some d -> Time.at Mono d
