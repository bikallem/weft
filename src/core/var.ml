(* A variable with an event for its next change (design section 2.4, A
   variable). It is a composition of an Atomic and Ivar.read, with no protocol
   of its own. *)

(* A state of a variable. The set that replaces this state fills [next]. *)
type 'a state = {value: 'a; next: unit Ivar.t}

type 'a t = 'a state Atomic.t

(* The view that a fiber saw: the variable and one of its states. A view holds
   no later state. *)
type 'a view = {var: 'a t; st: 'a state}

let create v = Atomic.make {value= v; next= Ivar.create ()}

let view t = {var= t; st= Atomic.get t}

let value v = v.st.value

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

(* The Ivar of [seen] is full only after a set replaced its state, and no set
   puts that state again. Thus the state of the variable after the fill is never
   the state of [seen]. No guard is necessary: the fill of a set comes at once
   after its exchange. *)
let changed seen = Event.wrap (Ivar.read seen.st.next) (fun () -> view seen.var)
