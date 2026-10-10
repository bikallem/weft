module Wait = struct
  type timeout = Poll | Sleep

  type t = {timeout: timeout; deadline: Clock.mono option}
end

module Ready = struct
  type t = Alarm | Woken
end

type _ Effect.t +=
  | Now : 'time Clock.line -> 'time Effect.t
  | Submit : 'a Op.t -> ('a, Op.error) result Effect.t
  | Wait : int * Wait.t -> Ready.t list Effect.t
  | Wake : int -> unit Effect.t
  | Random : int -> int Effect.t
  | Random_bytes : bytes -> unit Effect.t
  | Switch : int -> unit Effect.t

module type WORLD = sig
  type t

  val now : t -> 'time Clock.line -> 'time

  val submit : t -> 'a Op.t -> ('a, Op.error) result

  val wait : t -> domain:int -> Wait.t -> Ready.t list

  val wake : t -> domain:int -> unit

  val random : t -> int -> int

  val random_bytes : t -> bytes -> unit
end

exception Not_in_fiber of string

let perform name e =
  try Effect.perform e with Effect.Unhandled _ -> raise (Not_in_fiber name)
