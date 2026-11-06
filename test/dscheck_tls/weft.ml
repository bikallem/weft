(* A model of the parts of weft that the TLS session uses, for dscheck. A thread
   of dscheck is not a fiber, thus the model has no scheduler. *)

type 'a event = unit -> 'a option

let guard f () = f () ()

let always x () = Some x

module Fiber = struct
  exception Cancelled of exn

  (* The cancel of a test: one cancel, which the next cancellation point takes.
     A thread that retries after Cancelled is thus outside the cancelled
     scope. *)
  let cancel = ref (Atomic.make false)

  let check_cancel () =
    if Atomic.get !cancel && Atomic.exchange !cancel false then
      raise (Cancelled Exit)

  (* A spin lock with the cancellation points of the real mutex
     (src/core/locks.ml): a check before the lock, and a cancel during the wait
     raises with the lock not held. *)
  module Mutex = struct
    type t = bool Atomic.t

    let create () = Atomic.make false

    let lock m =
      check_cancel () ;
      while not (Atomic.compare_and_set m false true) do
        check_cancel () ; Domain.cpu_relax ()
      done

    let unlock m = Atomic.set m false
  end
end
