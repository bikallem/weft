(* Ivars: a value that is set one time (design section 2.4). *)

type 'a t = 'a Sched.ivar

let create () = Sched.Iv.make ()

let try_fill t v =
  match Sched.Iv.fill t v with
  | None ->
      false
  | Some resumed ->
      Sched.enqueue resumed ; true

let fill t v =
  if not (try_fill t v) then invalid_arg "Ivar.fill: the Ivar is full"

let read t = Event.of_ivar t

let peek t =
  match Atomic.get t with Sched.Full v -> Some v | Sched.Empty _ -> None
