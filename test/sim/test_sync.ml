(* The tests of the locks and of the promises on the simulator (design section
   2.5, Promises, and Locks for fiber code). *)

open Windtrap
open Weft

let not_in_fiber = function Fiber.Not_in_fiber _ -> true | _ -> false

(* [use_ro] unlocks the mutex when [f] gives a value. *)
let use_ro_unlocks () =
  Helpers.run (fun () ->
      let m = Fiber.Mutex.create () in
      equal int 1 (Fiber.Mutex.use_ro m (fun () -> 1)) ;
      equal int 2 (Fiber.Mutex.use_ro m (fun () -> 2)) )

(* After [Condition.wait], the shield of [use_rw ~protect:true] is closed again.
   Thus a cancel does not stop the sleep after the wait. *)
let shield_closes_after_wait () =
  Helpers.run (fun () ->
      let m = Fiber.Mutex.create () and c = Fiber.Condition.create () in
      let slept = ref false in
      Fiber.scope (fun outer ->
          Fiber.scope (fun s ->
              Fiber.fork s (fun () ->
                  Fiber.Mutex.use_rw ~protect:true m (fun () ->
                      Fiber.Condition.wait c m ;
                      Time.sleep Mono (Span.ms 10) ;
                      slept := true ) ) ;
              Fiber.fork outer (fun () ->
                  Time.sleep Mono (Span.ms 1) ;
                  Fiber.Mutex.use_ro m (fun () -> Fiber.Condition.signal c) ;
                  Time.sleep Mono (Span.ms 1) ;
                  Fiber.cancel s (Failure "stop") ) ) ) ;
      is_true !slept )

(* Outside a fiber, each call raises [Not_in_fiber] before it changes the lock.
   The release of the semaphore does not change its count. *)
let lock_calls_outside () =
  let t = Fiber.Semaphore.create ~max:2 1 in
  List.iter
    (fun (name, f) -> raises_match ~msg:name not_in_fiber f)
    [ ("Mutex.unlock", fun () -> Fiber.Mutex.unlock (Fiber.Mutex.create ()))
    ; ( "Condition.signal"
      , fun () -> Fiber.Condition.signal (Fiber.Condition.create ()) )
    ; ( "Condition.broadcast"
      , fun () -> Fiber.Condition.broadcast (Fiber.Condition.create ()) )
    ; ("Semaphore.release", fun () -> Fiber.Semaphore.release t) ] ;
  is_true (Fiber.Semaphore.try_acquire t) ;
  is_false (Fiber.Semaphore.try_acquire t)

exception Boom

(* The promise holds the cell of its finaliser. Thus a GC before the await does
   not report the failure. *)
let gc_before_await () =
  Helpers.run (fun () ->
      Fiber.scope (fun s ->
          let p = Fiber.fork_promise s (fun () -> raise Boom) in
          Time.sleep Mono (Span.ms 1) ;
          Gc.full_major () ;
          Gc.full_major () ;
          raises Boom (fun () -> Promise.await p) ) )

(* A finaliser runs outside a fiber. Its [Promise.cancel] raises [Not_in_fiber]
   and does not cancel the promise. *)
let cancel_outside () =
  Helpers.run (fun () ->
      Fiber.scope (fun s ->
          let p = Fiber.fork_promise s (fun () -> "done") in
          let r = ref None in
          let () =
            let x = ref 1 in
            Gc.finalise
              (fun _ ->
                r :=
                  Some
                    ( match Promise.cancel p with
                    | () ->
                        "no exception"
                    | exception Fiber.Not_in_fiber _ ->
                        "Not_in_fiber"
                    | exception e ->
                        Printexc.to_string e ) )
              x
          in
          Gc.full_major () ;
          equal (option string) (Some "Not_in_fiber") !r ;
          equal string "done" (Promise.await p) ) )

let tests =
  [ group "locks"
      [ test "use_ro unlocks the mutex when f gives a value" use_ro_unlocks
      ; test "the shield of use_rw closes again after Condition.wait"
          shield_closes_after_wait
      ; test "each lock call outside a fiber raises Not_in_fiber"
          lock_calls_outside ]
  ; group "promises"
      [ test "a GC before the await of a failed promise gives no report"
          gc_before_await
      ; test "Promise.cancel outside a fiber raises Not_in_fiber" cancel_outside
      ] ]
