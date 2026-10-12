(* A variable with an event for its next change (design section 2.4, A
   variable). It is a composition of an Atomic and Ivar.read, with no protocol
   of its own. *)

(* A state of a variable, and the view that a fiber saw. The set that replaces
   this state fills [next]. *)
type 'a state = {value: 'a; next: unit Ivar.t}

type 'a view = 'a state

type 'a t = 'a state Atomic.t

let create v = Atomic.make {value= v; next= Ivar.create ()}

let view t = Atomic.get t

let value st = st.value

let get t = (Atomic.get t).value

(* [change t v] puts a new state with an exchange, then fills the Ivar of the
   state that it replaced. It gives the syncs to resume. A state is a new record
   that no set puts again, so each state has one replacer, which fills its Ivar.
   The fill comes after the exchange, so a waiter never continues before [get]
   gives the new value. *)
let change t v =
  let old = Atomic.exchange t {value= v; next= Ivar.create ()} in
  Option.value ~default:[] (Sched.Iv.fill old.next ())

(* [Self] first, as in [Ivar.try_fill]: outside a fiber, the set raises
   [Not_in_fiber] before it changes the state. *)
let set t v =
  ignore (Sched.self ()) ;
  Sched.enqueue (change t v)

(* The Ivar of [seen] is full only after a set replaced [seen], and no set puts
   [seen] again. Thus the state of [t] after the fill is never [seen]. No guard
   is necessary: the fill of a set comes at once after its exchange. *)
let changed t seen = Event.wrap (Ivar.read seen.next) (fun () -> Atomic.get t)
