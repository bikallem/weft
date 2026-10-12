(* A variable with an event for its next change (design section 2.4, A
   variable). It is a composition of an Atomic, Ivar.read and always, with no
   protocol of its own. *)

(* A state of a variable. [next] gets the value of the set that replaces this
   state. *)
type 'a state = {value: 'a; next: 'a Ivar.t}

type 'a t = 'a state Atomic.t

let create v = Atomic.make {value= v; next= Ivar.create ()}

let get t = (Atomic.get t).value

(* [change t v] gives [t] the value [v] with a CAS on its state, if [v] is not
   the value of [t]. Then it fills the Ivar of the state that it replaced, and
   it gives the syncs to resume. Thus two states in sequence never have the same
   value, and each state has one replacer, which fills its Ivar. The fill comes
   after the CAS, so a waiter never gets a value before [get] gives it. *)
let change t v =
  let st = {value= v; next= Ivar.create ()} in
  let rec go () =
    let old = Atomic.get t in
    if old.value == v then []
    else if Atomic.compare_and_set t old st then
      Option.value ~default:[] (Sched.Iv.fill old.next v)
    else go ()
  in
  go ()

(* [Self] first, as in [Ivar.try_fill]: outside a fiber, the set raises
   [Not_in_fiber] before it changes the state. *)
let set t v =
  ignore (Sched.self ()) ;
  Sched.enqueue (change t v)

(* The guard reads the state at sync time. If its value is [seen], the fiber
   waits on the Ivar of that state, and the set that replaces the state fills
   it. *)
let changed t seen =
  Event.guard (fun () ->
      let st = Atomic.get t in
      if st.value != seen then Event.always st.value else Ivar.read st.next )
