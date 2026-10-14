(** The world effects and the world interface (design sections 2.2 and 2.3).

    The effect constructors are for the scheduler and the worlds only. Fiber
    code calls functions such as [Time.now Mono]. *)

(** The argument of [Wait]. *)
module Wait : sig
  type timeout =
    | Poll  (** Timeout 0. *)
    | Sleep  (** No timeout: the alarm of the domain ends the wait. *)

  (** [deadline] is the earliest deadline of the domain, for its alarm.
      [posix_deadline] is the earliest [Posix] deadline, for its [Posix] alarm
      (M3): a step of the clock must also end the wait. *)
  type t =
    { timeout: timeout
    ; deadline: Clock.mono option
    ; posix_deadline: Clock.posix option }
end

(** The result of [Wait]. *)
module Ready : sig
  type t =
    | Alarm  (** The alarm of the domain went off. *)
    | Posix_alarm
        (** The [Posix] alarm went off, or the clock made a step (M3). *)
    | Woken  (** A [wake] occurred. *)
    | Fd of {token: int; read: bool; write: bool}
        (** An edge of a registered fd. [token] is the token of [Register]. *)
    | External of int
        (** A completion that the world gives, for example the end of
            [Sim.settle]. *)
    | Signal of Op.signal * int
        (** The signal came [n] times since the last [Wait]. Only domain 0 gets
            it. A world gives [n] of 1 or more. The core ignores a count of 0 or
            less. *)
end

type _ Effect.t +=
  | Now : 'time Clock.line -> 'time Effect.t
  | Submit : 'a Op.t -> ('a, Op.error) result Effect.t
  | Wait : int * Wait.t -> Ready.t list Effect.t
  | Wake : int -> unit Effect.t
  | Random : int -> int Effect.t
  | Random_bytes : bytes -> unit Effect.t
  | Switch : unit Effect.t

module type WORLD = sig
  type t

  val now : t -> 'time Clock.line -> 'time
  (** The time on a line. It never blocks. *)

  val submit : t -> 'a Op.t -> ('a, Op.error) result
  (** One attempt of an OS operation. It never blocks. *)

  val wait : t -> domain:int -> Wait.t -> Ready.t list
  (** The only blocking call. *)

  val wake : t -> domain:int -> unit
  (** Ends the [wait] of domain [domain]. A [wake] before the [wait] is not
      lost. *)

  val random : t -> int -> int
  (** [random w n] is in the range 0 to [n - 1]. *)

  val random_bytes : t -> bytes -> unit
end

val perform : string -> 'a Effect.t -> 'a
(** [perform name e] performs [e]. If no handler is present, it raises
    [Not_in_fiber name]. *)

exception Not_in_fiber of string
