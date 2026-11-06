(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at https://mozilla.org/MPL/2.0/. *)

(* Internal. Deadlines on the monotonic clock of the world. [None] is no
   deadline. [from_now] and [has_passed] read the clock, so they run in a weft
   fiber. *)

val from_now : Weft.Span.t -> Weft.Clock.mono option
(* [from_now span] is [Time.now Mono] plus [span]. It is [None] if the sum
   overflows: no deadline. *)

val has_passed : Weft.Clock.mono option -> bool
(* [has_passed d] is [true] iff [d] is [Some t] and [Time.now Mono] is [t] or
   later. [has_passed None] is [false] and reads no clock. *)

val earlier :
  Weft.Clock.mono option -> Weft.Clock.mono option -> Weft.Clock.mono option
(* [earlier a b] is the earlier of [a] and [b]. [None] is later than each
   time. *)

val at : Weft.Clock.mono option -> unit Weft.event
(* [at d] is [Time.at Mono d], and [Weft.never] for [None]. *)
