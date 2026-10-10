(** The operations that a world implements (design section 2.3). Milestone M0
    adds the socket operations with [Net]. *)

type error =
  | Would_block
  | Error of string
      (** The error of one attempt. [Would_block] tells that the fd is not
          ready. *)

(** One attempt of an OS operation. *)
type _ t = |
