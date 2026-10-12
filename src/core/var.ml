(* A variable with an event for its next change (design section 2.4, A
   variable). It is a composition of an Atomic, Ivar.read and always, with no
   protocol of its own. *)

(* A state of a variable. [next] gets the value of the set that replaces this
   state. *)
type 'a state = {value: 'a; next: 'a Ivar.t}

type 'a t = 'a state Atomic.t

let create v = Atomic.make {value= v; next= Ivar.create ()}

let get t = (Atomic.get t).value

(* [replace t st] makes [st] the state of [t] with a CAS, if the value of [t] is
   not the value of [st]. It gives the state that it replaced. Thus two states
   in sequence never have the same value, and each state has one replacer. *)
let rec replace t st =
  let old = Atomic.get t in
  if old.value == st.value then None
  else if Atomic.compare_and_set t old st then Some old
  else replace t st

(* [Self] first, as in [Ivar.try_fill]: outside a fiber, the set raises
   [Not_in_fiber] before it changes the state. The fill comes after the CAS, so
   a fiber that reads the new state never waits on the Ivar of the old state. *)
let set t v =
  ignore (Sched.self ()) ;
  match replace t {value= v; next= Ivar.create ()} with
  | Some old ->
      ignore (Sched.fill_resume old.next v)
  | None ->
      ()

(* The guard reads the state at sync time. If its value is [seen], the fiber
   waits on the Ivar of that state, and the set that replaces the state fills
   it. *)
let changed t seen =
  Event.guard (fun () ->
      let st = Atomic.get t in
      if st.value != seen then Event.always st.value else Ivar.read st.next )
