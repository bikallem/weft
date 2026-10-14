(* The tests that kill the survivors of the mutation run in the channels, the
   locks and the promises (design section 2.17). *)

open Windtrap
open Weft
module Sim = Weft_sim
module C = Private.Chan

let run f = Sim.run (fun _ -> f ())

let not_in_fiber = function Fiber.Not_in_fiber _ -> true | _ -> false

(* All the offers of a side, also the stale offers. [C.offers] moves [back] to
   [front], so the count does not use it. *)
let all_offers side =
  List.length (Atomic.get side.C.front) + List.length (Atomic.get side.C.back)

(* Channels *)

(* One branch of the choose skips the offer of the other branch. The offer stays
   in the queue, and a later partner meets it. The seed selects the order of the
   two branches, so the test has a partner of each type. *)
let skipped_own_offer partner_sends () =
  run (fun () ->
      let c = Chan.create () in
      Fiber.scope (fun s ->
          let p =
            Fiber.fork_promise s (fun () ->
                sync
                  (choose
                     [ wrap (Chan.send c 1) (fun () -> "sent")
                     ; wrap (Chan.recv c) (fun v -> "got " ^ string_of_int v) ] ) )
          in
          Time.sleep Mono (Span.ms 1) ;
          if partner_sends then (
            sync (Chan.send c 2) ;
            equal string "got 2" (Promise.await p) )
          else (
            equal int 1 (sync (Chan.recv c)) ;
            equal string "sent" (Promise.await p) ) ) )

(* A send that meets the oldest offer in [front] does not scan [back]. The stale
   offer in [back] stays there, and the send adds no offer of its own. *)
let send_stops_in_front () =
  run (fun () ->
      let c = Chan.create () in
      let got = ref [] in
      Fiber.scope (fun s ->
          for _ = 1 to 3 do
            Fiber.fork s (fun () ->
                (* Read [!got] after the sync, not before it. *)
                let v = sync (Chan.recv c) in
                got := v :: !got )
          done ;
          Time.sleep Mono (Span.ms 1) ;
          (* The scan of [back] meets the first receiver. The two other offers
             go to [front]. *)
          sync (Chan.send c 1) ;
          equal int 2 (all_offers c.C.receivers) ;
          (* A stale offer in [back]. *)
          equal (option int) None
            (sync
               (choose
                  [ wrap (Chan.recv c) Option.some
                  ; wrap (Time.after Mono (Span.ms 1)) (fun () -> None) ] ) ) ;
          equal int 3 (all_offers c.C.receivers) ;
          sync (Chan.send c 2) ;
          equal int 2 (all_offers c.C.receivers) ;
          equal int 0 (all_offers c.C.senders) ;
          sync (Chan.send c 3) ) ;
      equal (list int) [1; 2; 3] (List.sort compare !got) )

(* Locks *)

(* [use_ro] unlocks the mutex when [f] gives a value. *)
let use_ro_unlocks () =
  run (fun () ->
      let m = Fiber.Mutex.create () in
      equal int 1 (Fiber.Mutex.use_ro m (fun () -> 1)) ;
      equal int 2 (Fiber.Mutex.use_ro m (fun () -> 2)) )

(* After [Condition.wait], the shield of [use_rw ~protect:true] is closed again.
   Thus a cancel does not stop the sleep after the wait. *)
let shield_closes_after_wait () =
  run (fun () ->
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

(* Outside a fiber, a call raises [Not_in_fiber] before it changes the lock. *)
let unlock_outside () =
  raises_match not_in_fiber (fun () ->
      Fiber.Mutex.unlock (Fiber.Mutex.create ()) )

let signal_outside () =
  raises_match not_in_fiber (fun () ->
      Fiber.Condition.signal (Fiber.Condition.create ()) )

let broadcast_outside () =
  raises_match not_in_fiber (fun () ->
      Fiber.Condition.broadcast (Fiber.Condition.create ()) )

let release_outside () =
  let t = Fiber.Semaphore.create ~max:2 1 in
  raises_match not_in_fiber (fun () -> Fiber.Semaphore.release t) ;
  is_true (Fiber.Semaphore.try_acquire t) ;
  is_false (Fiber.Semaphore.try_acquire t)

(* Promises *)

exception Boom

(* The promise holds the cell of its finaliser. Thus a GC before the await does
   not report the failure. *)
let gc_before_await () =
  run (fun () ->
      Fiber.scope (fun s ->
          let p = Fiber.fork_promise s (fun () -> raise Boom) in
          Time.sleep Mono (Span.ms 1) ;
          Gc.full_major () ;
          Gc.full_major () ;
          raises Boom (fun () -> Promise.await p) ) )

(* A finaliser runs outside a fiber. Its [Promise.cancel] raises [Not_in_fiber]
   and does not cancel the promise. *)
let cancel_outside () =
  run (fun () ->
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
  [ group "mutants of sync"
      [ test "a recv meets an offer that its own sync skipped"
          (skipped_own_offer false)
      ; test "a send meets an offer that its own sync skipped"
          (skipped_own_offer true)
      ; test "a send that meets an offer in front does not scan back"
          send_stops_in_front
      ; test "use_ro unlocks the mutex when f gives a value" use_ro_unlocks
      ; test "the shield of use_rw closes again after Condition.wait"
          shield_closes_after_wait
      ; test "Mutex.unlock outside a fiber raises Not_in_fiber" unlock_outside
      ; test "Condition.signal outside a fiber raises Not_in_fiber"
          signal_outside
      ; test "Condition.broadcast outside a fiber raises Not_in_fiber"
          broadcast_outside
      ; test "Semaphore.release outside a fiber raises Not_in_fiber"
          release_outside
      ; test "a GC before the await of a failed promise gives no report"
          gc_before_await
      ; test "Promise.cancel outside a fiber raises Not_in_fiber" cancel_outside
      ] ]
