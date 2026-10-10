(** Weft: fibers, CML events and pluggable worlds. *)

val version : string
(** The version of the library. *)

(** The parts inside the library. They are for the tests and for the worlds of
    the library, not for programs. They can change in each version. *)
module Private : sig
  module Commit = Commit
end
