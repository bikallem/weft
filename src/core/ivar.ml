(* Ivars: a value that is set one time (design section 2.4). *)

type 'a t = 'a Sched.ivar

let create () = Sched.Iv.make ()

(* [Self] first: outside a fiber, the fill raises [Not_in_fiber] before it
   commits waiters that nobody would then resume. *)
let try_fill t v =
  ignore (Sched.self ()) ;
  Sched.fill_resume t v

let fill t v =
  if not (try_fill t v) then invalid_arg "Ivar.fill: the Ivar is full"

let read t = Event.of_ivar t

let peek t =
  match Atomic.get t with Sched.Full v -> Some v | Sched.Empty _ -> None
