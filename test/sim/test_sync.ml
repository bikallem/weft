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

(* A [with_timeout] around [Condition.wait] in [use_rw ~protect:true] raises
   [Timeout], the reason of the cancel of the wait. The data is correct at the
   wait, so the mutex stays usable (the review of 2026-11-06). *)
let timeout_of_wait_no_poison () =
  Helpers.run (fun () ->
      let m = Fiber.Mutex.create () and c = Fiber.Condition.create () in
      raises Fiber.Timeout (fun () ->
          Fiber.Mutex.use_rw ~protect:true m (fun () ->
              Fiber.with_timeout Mono (Span.ms 5) (fun () ->
                  Fiber.Condition.wait c m ) ) ) ;
      equal string "free" (Fiber.Mutex.use_ro m (fun () -> "free")) )

(* A [Timeout] that does not come from a wait poisons the mutex. *)
let timeout_outside_wait_poisons () =
  Helpers.run (fun () ->
      let m = Fiber.Mutex.create () in
      raises Fiber.Timeout (fun () ->
          Fiber.Mutex.use_rw ~protect:true m (fun () ->
              Fiber.with_timeout Mono (Span.ms 5) (fun () ->
                  Time.sleep Mono (Span.ms 10) ) ) ) ;
      raises Fiber.Mutex.Poisoned (fun () -> Fiber.Mutex.use_ro m ignore) )

(* A condition and a mutex that two runs share, for example as globals of a test
   that runs many seeds. The first run ends while a fiber waits on the
   condition. The signal of the second run goes to the waiter of the second run,
   not to the dead waiter (the review of 2026-11-06). *)
let condition_of_two_runs () =
  let m = Fiber.Mutex.create () and c = Fiber.Condition.create () in
  let wait () =
    Fiber.Mutex.lock m ; Fiber.Condition.wait c m ; Fiber.Mutex.unlock m
  in
  raises_match
    (function Weft_sim.Deadlock _ -> true | _ -> false)
    (fun () -> Weft_sim.run (fun _ -> wait ())) ;
  Weft_sim.run (fun _ ->
      Fiber.scope (fun s ->
          Fiber.fork s wait ;
          Time.sleep Mono (Span.ms 1) ;
          Fiber.Mutex.use_ro m (fun () -> Fiber.Condition.signal c) ) )

type _ Effect.t += Drop : unit Effect.t

(* A handler that drops the continuation of [Drop]: it raises with no
   discontinue. The end of the nested scope never runs. The scope of the handler
   raises [Invalid_argument], and the contexts of the fiber get their old value
   (the review of 2026-11-06). *)
let dropped_continuation () =
  let drop =
    { Fiber.handle=
        (fun f ->
          Effect.Deep.match_with f ()
            { retc= Fun.id
            ; exnc= raise
            ; effc=
                (fun (type a) (e : a Effect.t) ->
                  match e with
                  | Drop ->
                      Some
                        (fun (_ : (a, _) Effect.Deep.continuation) -> raise Exit)
                  | _ ->
                      None ) } ) }
  in
  Helpers.run (fun () ->
      raises_match
        (function Invalid_argument _ -> true | _ -> false)
        (fun () ->
          Fiber.scope ~handler:drop (fun _ ->
              Fiber.scope (fun inner ->
                  Fiber.fork inner (fun () -> Time.sleep Mono (Span.ms 1)) ;
                  Effect.perform Drop ) ) ) ;
      (* The fiber runs in its own scope again: a cancel check works. *)
      Fiber.check_cancel () ;
      Time.sleep Mono (Span.ms 5) )

(* The link of a nested scope (the review of 2026-11-06, design section 2.5). A
   scope links in the children of its parent at its first fork, and its end
   removes it. *)
let link_removed_at_end () =
  Helpers.run (fun () ->
      Fiber.scope (fun outer ->
          let children () =
            Hashtbl.length (Private.scope_ctx outer).Private.Sched.children
          in
          Fiber.scope (fun _ -> ()) ;
          equal int 0 (children ()) ;
          Fiber.scope (fun inner ->
              Fiber.fork inner ignore ;
              equal int 1 (children ()) ) ;
          equal int 0 (children ()) ) )

(* A fork into a scope S whose parent P is also not linked links P first. Thus a
   cancel of the scope A above P reaches the fiber that the fork added. *)
let link_parents_first () =
  Helpers.run (fun () ->
      let reached = ref false in
      ( try
          Fiber.scope (fun a ->
              Fiber.fork a (fun () ->
                  Time.sleep Mono (Span.ms 1) ;
                  Fiber.cancel a Exit ) ;
              Fiber.scope (fun _p ->
                  Fiber.scope (fun s ->
                      Fiber.fork s (fun () ->
                          try sync (Ivar.read (Ivar.create ()))
                          with Fiber.Cancelled _ as e ->
                            reached := true ;
                            raise e ) ;
                      Fiber.shield (fun () -> Time.sleep Mono (Span.ms 5)) ) ) )
        with Fiber.Cancelled _ -> () ) ;
      is_true !reached )

(* fork_daemon links its scope. Thus a cancel of the outer scope reaches the
   daemon at once, before the body of its scope ends. *)
let daemon_links_scope () =
  let add, get = Helpers.log () in
  Helpers.run (fun () ->
      try
        Fiber.scope (fun a ->
            Fiber.fork a (fun () ->
                Time.sleep Mono (Span.ms 1) ;
                Fiber.cancel a Exit ) ;
            Fiber.scope (fun s ->
                Fiber.fork_daemon s (fun () ->
                    try sync (Ivar.read (Ivar.create ()))
                    with Fiber.Cancelled _ as e -> add "daemon" ; raise e ) ;
                Fiber.shield (fun () -> Time.sleep Mono (Span.ms 5)) ;
                add "body" ) )
      with Fiber.Cancelled _ -> () ) ;
  equal (list string) ["daemon"; "body"] (get ())

(* Outside a fiber, each call raises [Not_in_fiber] before it changes the lock.
   The release and the use of the semaphore do not change its count. *)
let lock_calls_outside () =
  let t = Fiber.Semaphore.create ~max:2 1 in
  List.iter
    (fun (name, f) -> raises_match ~msg:name not_in_fiber f)
    [ ("Mutex.unlock", fun () -> Fiber.Mutex.unlock (Fiber.Mutex.create ()))
    ; ( "Condition.signal"
      , fun () -> Fiber.Condition.signal (Fiber.Condition.create ()) )
    ; ( "Condition.broadcast"
      , fun () -> Fiber.Condition.broadcast (Fiber.Condition.create ()) )
    ; ("Semaphore.release", fun () -> Fiber.Semaphore.release t)
    ; ("Semaphore.use", fun () -> Fiber.Semaphore.use t ignore)
    ; ("Fiber.check_cancel", Fiber.check_cancel) ] ;
  is_true (Fiber.Semaphore.try_acquire t) ;
  is_false (Fiber.Semaphore.try_acquire t)

(* A waiter that a cancel stops, and that gets the mutex from an unlock before
   it runs again: it releases the mutex. Thus the mutex is free after the
   cancel. *)
let lock_after_cancel () =
  Helpers.run (fun () ->
      let m = Fiber.Mutex.create () in
      Fiber.Mutex.lock m ;
      let got = ref false in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              Fiber.Mutex.lock m ;
              got := true ) ;
          Fiber.yield () ;
          Fiber.cancel s Exit ;
          Fiber.Mutex.unlock m ) ;
      is_false !got ;
      equal int 1 (Fiber.Mutex.use_ro m (fun () -> 1)) )

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

(* A second cancel before the start of the fiber does nothing more. The fiber
   stops at the first cancellation point of [f]. *)
let cancel_twice_before_start () =
  Helpers.run (fun () ->
      let ran = ref false in
      Fiber.scope (fun s ->
          let p =
            Fiber.fork_promise s (fun () ->
                Fiber.yield () ;
                ran := true )
          in
          Promise.cancel p ;
          Promise.cancel p ;
          raises Promise.Cancelled (fun () -> Promise.await p) ) ;
      is_false !ran )

(* The report of a run gives a failure of another run, or of no run, to the
   report that was there before the run. *)
let report_of_other_run () =
  let got = ref [] in
  Helpers.with_report (fun () ->
      Atomic.set Private.promise_report (fun run _ ->
          got := (if run = None then "no run" else "other run") :: !got ) ;
      Weft_sim.run (fun sim ->
          let report = Atomic.get Private.promise_report in
          report None Exit ;
          report (Some ((Weft_sim.sched sim).Private.Sched.run_id + 1)) Exit ) ) ;
  equal (list string) ["no run"; "other run"] (List.rev !got)

(* The report of a promise runs in a finaliser. If the report raises, the
   finaliser drops the exception, so the run goes on. *)
let report_raises () =
  let called = ref false in
  Helpers.with_report (fun () ->
      Helpers.run (fun () ->
          Atomic.set Private.promise_report (fun _ _ ->
              called := true ;
              raise Exit ) ;
          Fiber.scope (fun s ->
              ignore (Fiber.fork_promise s (fun () -> raise Boom)) ) ) ;
      is_true !called )

let tests =
  [ group "locks"
      [ test "use_ro unlocks the mutex when f gives a value" use_ro_unlocks
      ; test "the shield of use_rw closes again after Condition.wait"
          shield_closes_after_wait
      ; test "a timeout of Condition.wait in use_rw does not poison"
          timeout_of_wait_no_poison
      ; test "a timeout outside a wait in use_rw poisons"
          timeout_outside_wait_poisons
      ; test "the end of a linked scope removes its link" link_removed_at_end
      ; test "a fork links the parents that are not linked first"
          link_parents_first
      ; test "fork_daemon links its scope" daemon_links_scope
      ; test "a handler that drops a continuation fails its scope"
          dropped_continuation
      ; test "a signal skips the dead waiter of an earlier run"
          condition_of_two_runs
      ; test "each lock call outside a fiber raises Not_in_fiber"
          lock_calls_outside
      ; test "a cancelled waiter that got the mutex releases it"
          lock_after_cancel ]
  ; group "promises"
      [ test "a GC before the await of a failed promise gives no report"
          gc_before_await
      ; test "Promise.cancel outside a fiber raises Not_in_fiber" cancel_outside
      ; test "a second cancel before the start does nothing more"
          cancel_twice_before_start
      ; test "a report that raises does not stop the run" report_raises
      ; test "a failure of another run goes to the old report"
          report_of_other_run ] ]
