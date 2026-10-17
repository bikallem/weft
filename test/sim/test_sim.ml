(* The tests of the scheduler, the events and the scopes on the simulator
   (design sections 2.4 to 2.8, and the "done" list of milestone M0). *)

open Windtrap
open Weft
module Sim = Weft_sim
module C = Private.Chan
module S = Private.Sched

let run = Helpers.run

(* The time since [t0], in milliseconds. *)
let ms_since t0 =
  Int64.to_int
    (Int64.div (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) 1_000_000L)

let log = Helpers.log

exception Boom of string

(* Time *)

let sleep_one_hour () =
  let cpu0 = Sys.time () in
  let elapsed =
    run (fun () ->
        let t0 = Time.now Mono in
        Time.sleep Mono (Span.s 3600) ;
        Time.diff Mono (Time.now Mono) t0 )
  in
  equal int64 3_600_000_000_000L (Span.to_ns elapsed) ;
  less float_exact ~than:1.0 (Sys.time () -. cpu0)

let sleeps_in_order () =
  let add, get = log () in
  run (fun () ->
      let t0 = Time.now Mono in
      Fiber.scope (fun s ->
          List.iter
            (fun (name, ms) ->
              Fiber.fork s (fun () ->
                  Time.sleep Mono (Span.ms ms) ;
                  add "%s at %d ms" name (ms_since t0) ) )
            [("c", 30); ("a", 10); ("b", 20); ("a2", 10)] ) ) ;
  equal (list string)
    ["a at 10 ms"; "a2 at 10 ms"; "b at 20 ms"; "c at 30 ms"]
    (get ())

let choose_first_deadline () =
  run (fun () ->
      let t0 = Time.now Mono in
      let r =
        sync
          (choose
             [ wrap (Time.after Mono (Span.s 2)) (fun () -> "two")
             ; wrap (Time.after Mono (Span.s 1)) (fun () -> "one") ] )
      in
      equal string "one" r ;
      equal int 1000 (ms_since t0) )

let negative_span_is_ready () =
  run (fun () ->
      let t0 = Time.now Mono in
      Time.sleep Mono (Span.ms (-5)) ;
      equal int 0 (ms_since t0) )

let overflow_never () =
  run (fun () ->
      Time.sleep Mono (Span.ms 1) ;
      is_none (Time.add Mono (Time.now Mono) (Span.of_ns Int64.max_int)) ;
      (* An event that overflows is never ready: a later timeout wins. *)
      let r =
        sync
          (choose
             [ wrap
                 (Time.after Mono (Span.of_ns Int64.max_int))
                 (fun () -> "overflow")
             ; wrap (Time.after Mono (Span.s 1)) (fun () -> "timeout") ] )
      in
      equal string "timeout" r )

let posix_follows_mono () =
  run (fun () ->
      let p0 = Time.now Posix in
      Time.sleep Mono (Span.s 5) ;
      equal int64 5_000_000_000L
        (Span.to_ns (Time.diff Posix (Time.now Posix) p0)) )

let time =
  group "time"
    [ test "a sleep of one hour takes exactly 3600 s of virtual time"
        sleep_one_hour
    ; test "sleeps end in the order of their deadlines" sleeps_in_order
    ; test "choose commits to the first deadline" choose_first_deadline
    ; test "a negative span is ready at once" negative_span_is_ready
    ; test "a deadline that overflows is never" overflow_never
    ; test "Posix moves with Mono" posix_follows_mono ]

(* Events *)

let always_and_never () =
  run (fun () ->
      equal int 3 (sync (always 3)) ;
      equal int 4 (sync (choose [never; always 4])) ;
      equal int 5 (sync (wrap (always 2) (fun x -> x + 3))) )

let guard_runs_at_sync () =
  run (fun () ->
      let n = ref 0 in
      let ev = guard (fun () -> incr n ; always !n) in
      equal int 0 !n ;
      equal int 1 (sync ev) ;
      equal int 2 (sync ev) )

let nack_of_the_loser () =
  run (fun () ->
      let nack = ref None in
      let ev =
        choose
          [ with_nack (fun n ->
                nack := Some n ;
                Time.after Mono (Span.s 10) )
          ; wrap (always ()) (fun () -> ()) ]
      in
      sync ev ;
      (* The other branch committed, so the nack is ready. *)
      match !nack with
      | Some n ->
          sync n
      | None ->
          failwith "the guard did not run" )

let nack_of_the_winner () =
  run (fun () ->
      let nack = ref None in
      sync
        (choose
           [ with_nack (fun n ->
                 nack := Some n ;
                 always () )
           ; never ] ) ;
      match !nack with
      | Some n ->
          let r =
            sync
              (choose
                 [ wrap n (fun () -> "nack")
                 ; wrap (Time.after Mono (Span.s 1)) (fun () -> "none") ] )
          in
          equal string "none" r
      | None ->
          failwith "the guard did not run" )

let guard_raises_enables_nacks () =
  run (fun () ->
      let nack = ref None in
      raises (Boom "guard") (fun () ->
          sync
            (choose
               [ with_nack (fun n ->
                     nack := Some n ;
                     never )
               ; guard (fun () -> raise (Boom "guard")) ] ) ) ;
      match !nack with
      | Some n ->
          sync n
      | None ->
          failwith "the first guard did not run" )

let ivar_read_and_fill () =
  run (fun () ->
      let iv = Ivar.create () in
      let r =
        Fiber.scope (fun s ->
            let p = Fiber.fork_promise s (fun () -> sync (Ivar.read iv)) in
            Fiber.fork s (fun () ->
                Time.sleep Mono (Span.ms 5) ;
                Ivar.fill iv 42 ) ;
            Promise.await p )
      in
      equal int 42 r ;
      equal (option int) (Some 42) (Ivar.peek iv) ;
      is_false (Ivar.try_fill iv 1) ;
      raises_match
        (function Invalid_argument _ -> true | _ -> false)
        (fun () -> Ivar.fill iv 1) )

let yields_alternate () =
  let add, get = log () in
  run (fun () ->
      Fiber.scope (fun s ->
          List.iter
            (fun name ->
              Fiber.fork s (fun () ->
                  for i = 1 to 2 do
                    add "%s%d" name i ; Fiber.yield ()
                  done ) )
            ["a"; "b"] ) ) ;
  equal (list string) ["a1"; "b1"; "a2"; "b2"] (get ())

let events =
  group "events"
    [ test "always, never and wrap" always_and_never
    ; test "a guard runs at each sync" guard_runs_at_sync
    ; test "the nack of a branch that lost is ready" nack_of_the_loser
    ; test "the nack of the branch that won is not ready" nack_of_the_winner
    ; test "a guard that raises enables the nacks of the guards before it"
        guard_raises_enables_nacks
    ; test "an Ivar gives its value to a waiter" ivar_read_and_fill
    ; test "two fibers that yield alternate" yields_alternate ]

(* Scopes *)

let child_error_cancels_body () =
  run (fun () ->
      let t0 = Time.now Mono in
      raises (Boom "child") (fun () ->
          Fiber.scope (fun s ->
              Fiber.fork s (fun () ->
                  Time.sleep Mono (Span.s 1) ;
                  raise (Boom "child") ) ;
              Time.sleep Mono (Span.s 10) ) ) ;
      equal int 1000 (ms_since t0) )

let cancel_reaches_nested () =
  run (fun () ->
      let t0 = Time.now Mono in
      let seen = ref "" in
      Fiber.scope (fun outer ->
          Fiber.fork outer (fun () ->
              try Fiber.scope (fun _ -> Time.sleep Mono (Span.s 10))
              with Fiber.Cancelled (Boom m) -> seen := m ) ;
          Time.sleep Mono (Span.s 1) ;
          Fiber.cancel outer (Boom "stop") ) ;
      equal string "stop" !seen ;
      equal int 1000 (ms_since t0) )

(* The cleanup example of section 2.5: the shield lets the cleanup send its
   frame after a cancel. *)
let shield_cleanup () =
  let sent = ref false in
  run (fun () ->
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              Fun.protect
                (fun () -> Time.sleep Mono (Span.s 10))
                ~finally:(fun () ->
                  Fiber.shield (fun () ->
                      Fiber.with_timeout Mono (Span.s 1) (fun () ->
                          Time.sleep Mono (Span.ms 100) ;
                          sent := true ) ) ) ) ;
          Time.sleep Mono (Span.s 1) ;
          Fiber.cancel s (Boom "close") ) ) ;
  is_true !sent

let shield_timeout_still_works () =
  run (fun () ->
      let t0 = Time.now Mono in
      raises Fiber.Timeout (fun () ->
          Fiber.shield (fun () ->
              Fiber.with_timeout Mono (Span.s 1) (fun () ->
                  Time.sleep Mono (Span.s 10) ) ) ) ;
      equal int 1000 (ms_since t0) )

let join_waits_for_all () =
  let add, get = log () in
  run (fun () ->
      raises (Boom "a") (fun () ->
          Fiber.scope (fun s ->
              Fiber.fork s (fun () ->
                  Time.sleep Mono (Span.s 1) ;
                  raise (Boom "a") ) ;
              Fiber.fork s (fun () ->
                  Fiber.shield (fun () -> Time.sleep Mono (Span.s 5)) ;
                  add "slow child ended" ) ) ) ;
      add "scope ended" ) ;
  equal (list string) ["slow child ended"; "scope ended"] (get ())

let multiple_failures () =
  run (fun () ->
      match
        Fiber.scope (fun s ->
            Fiber.fork s (fun () -> raise (Boom "a")) ;
            Fiber.fork s (fun () -> raise (Boom "b")) )
      with
      | () ->
          failwith "no exception"
      | exception Fiber.Multiple l ->
          equal (list string) ["a"; "b"]
            (List.map (function Boom m, _ -> m | _ -> "other") l) )

let with_timeout_exact () =
  run (fun () ->
      let t0 = Time.now Mono in
      raises Fiber.Timeout (fun () ->
          Fiber.with_timeout Mono (Span.ms 250) (fun () ->
              Time.sleep Mono (Span.s 10) ) ) ;
      equal int 250 (ms_since t0) )

let with_timeout_value () =
  run (fun () ->
      let t0 = Time.now Mono in
      equal int 7
        (Fiber.with_timeout Mono (Span.s 1) (fun () ->
             Time.sleep Mono (Span.ms 10) ;
             7 ) ) ;
      equal int 10 (ms_since t0) )

(* A span that makes the deadline overflow: the deadline never comes. Thus
   [with_timeout] adds no timer, and it gives the value of [f]. *)
let with_timeout_overflow () =
  Sim.run (fun sim ->
      Time.sleep Mono (Span.ms 1) ;
      equal int 7
        (Fiber.with_timeout Mono (Span.of_ns Int64.max_int) (fun () ->
             equal int 0 (Private.Sched_debug.live_timers (Sim.sched sim)) ;
             Time.sleep Mono (Span.ms 10) ;
             7 ) ) )

(* A cancel is not an error of an isolated fiber: [on_error] does not get it,
   and the scope ends with no failure. *)
let isolated_cancel () =
  run (fun () ->
      let errors = ref [] in
      Fiber.scope (fun s ->
          Fiber.fork_isolated s
            ~on_error:(fun e _ -> errors := e :: !errors)
            (fun () -> sync (Ivar.read (Ivar.create ()))) ;
          Fiber.yield () ;
          Fiber.cancel s Exit ) ;
      equal int 0 (List.length !errors) )

let with_timeout_outer_cancel () =
  run (fun () ->
      let r = ref "" in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              try
                Fiber.with_timeout Mono (Span.s 1) (fun () ->
                    Time.sleep Mono (Span.s 10) )
              with
              | Fiber.Timeout ->
                  r := "timeout"
              | Fiber.Cancelled (Boom m) ->
                  r := m ) ;
          Time.sleep Mono (Span.ms 500) ;
          Fiber.cancel s (Boom "outer") ) ;
      equal string "outer" !r )

let daemon_cancelled_at_end () =
  let add, get = log () in
  run (fun () ->
      Fiber.scope (fun s ->
          Fiber.fork_daemon s (fun () ->
              try
                while true do
                  Time.sleep Mono (Span.ms 300) ;
                  add "tick"
                done
              with Fiber.Cancelled _ as e -> add "daemon cancelled" ; raise e ) ;
          Time.sleep Mono (Span.s 1) ) ) ;
  equal (list string) ["tick"; "tick"; "tick"; "daemon cancelled"] (get ())

let cancel_is_sticky () =
  run (fun () ->
      let n = ref 0 in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              for _ = 1 to 3 do
                try Time.sleep Mono (Span.s 10)
                with Fiber.Cancelled _ -> incr n
              done ) ;
          Fiber.yield () ;
          Fiber.cancel s (Boom "stop") ) ;
      equal int 3 !n )

let cancel_wins_over_ready () =
  run (fun () ->
      let r = ref "" in
      Fiber.scope (fun s ->
          Fiber.cancel s (Boom "first") ;
          try r := sync (always "ready")
          with Fiber.Cancelled (Boom m) -> r := m ) ;
      equal string "first" !r )

let fork_into_ended_scope () =
  run (fun () ->
      let saved = ref None in
      Fiber.scope (fun s -> saved := Some s) ;
      match !saved with
      | Some s ->
          raises Fiber.Scope_closed (fun () -> Fiber.fork s ignore)
      | None ->
          failwith "no scope" )

let fork_into_cancelled_scope () =
  run (fun () ->
      Fiber.scope (fun s ->
          Fiber.cancel s (Boom "stop") ;
          raises Fiber.Scope_closed (fun () -> Fiber.fork s ignore) ) )

let promise_holds_exception () =
  run (fun () ->
      Fiber.scope (fun s ->
          let p = Fiber.fork_promise s (fun () -> raise (Boom "p")) in
          raises (Boom "p") (fun () -> Promise.await p) ;
          (* The scope did not fail: the next promise works. *)
          equal int 1 (Promise.await (Fiber.fork_promise s (fun () -> 1))) ) )

let promise_ready_in_choose () =
  run (fun () ->
      Fiber.scope (fun s ->
          let p =
            Fiber.fork_promise s (fun () ->
                Time.sleep Mono (Span.s 5) ;
                "late" )
          in
          let r =
            sync
              (choose
                 [ Promise.ready p
                 ; wrap (Time.after Mono (Span.s 1)) (fun () -> "timeout") ] )
          in
          equal string "timeout" r ;
          equal string "late" (Promise.await p) ) )

let rec deep n = if n = 0 then raise (Boom "deep") else 1 + deep (n - 1)

let backtrace_of_child () =
  Printexc.record_backtrace true ;
  let check f =
    match run f with
    | _ ->
        failwith "no exception"
    | exception Boom _ ->
        let bt = Printexc.get_backtrace () in
        contains ~sub:"deep" bt
  in
  check (fun () ->
      Fiber.scope (fun s -> Fiber.fork s (fun () -> ignore (deep 3))) ) ;
  check (fun () ->
      Fiber.scope (fun s ->
          Promise.await (Fiber.fork_promise s (fun () -> deep 3)) ) )

let main_gives_value () = equal string "done" (run (fun () -> "done"))

let no_fibers_after_cancel () =
  Sim.run (fun sim ->
      let base = List.length (Private.Sched_debug.fibers (Sim.sched sim)) in
      Fiber.scope (fun s ->
          for _ = 1 to 5 do
            Fiber.fork s (fun () -> Time.sleep Mono (Span.s 100))
          done ;
          Fiber.fork s (fun () -> sync never) ;
          Time.sleep Mono (Span.s 1) ;
          Fiber.cancel s (Boom "stop") ) ;
      equal int base (List.length (Private.Sched_debug.fibers (Sim.sched sim))) ;
      equal (list string) [] (Private.Sched_debug.blocked (Sim.sched sim)) ;
      equal int 0 (Private.Sched_debug.live_timers (Sim.sched sim)) )

let daemon_end_keeps_daemons () =
  let add, get = log () in
  run (fun () ->
      let t0 = Time.now Mono in
      Fiber.scope (fun s ->
          Fiber.fork_daemon s (fun () -> Time.sleep Mono (Span.ms 100)) ;
          Fiber.fork s (fun () -> Time.sleep Mono (Span.ms 200)) ;
          Fiber.fork_daemon s (fun () ->
              try Time.sleep Mono (Span.s 10)
              with Fiber.Cancelled _ as e ->
                add "%s at %d ms" "daemon cancelled" (ms_since t0) ;
                raise e ) ;
          Time.sleep Mono (Span.s 1) ) ) ;
  (* The end of the first daemon does not cancel the second one: only the end of
     the body does. *)
  equal (list string) ["daemon cancelled at 1000 ms"] (get ())

let with_timeout_inner_cancel () =
  run (fun () ->
      let r =
        try
          Fiber.with_timeout Mono (Span.s 1) (fun () ->
              Fiber.scope (fun s ->
                  Fiber.cancel s (Boom "inner") ;
                  sync (always "value") ) )
        with
        | Fiber.Timeout ->
            "timeout"
        | Fiber.Cancelled (Boom m) ->
            m
      in
      equal string "inner" r )

let live_timer_count () =
  Sim.run (fun sim ->
      Fiber.scope (fun s ->
          Fiber.fork s (fun () -> Time.sleep Mono (Span.s 1)) ;
          Fiber.yield () ;
          equal int 1 (Private.Sched_debug.live_timers (Sim.sched sim)) ) )

(* Regression tests of the review of M0. *)

(* C1: with_timeout in a cancelled context raises the Cancelled of that context,
   not Scope_closed. *)
let with_timeout_in_cancelled_scope () =
  run (fun () ->
      let r = ref "" in
      Fiber.scope (fun s ->
          Fiber.cancel s (Boom "x") ;
          try Fiber.with_timeout Mono (Span.s 1) (fun () -> r := "ran")
          with Fiber.Cancelled (Boom m) -> r := m ) ;
      equal string "x" !r )

(* C1: the failure of one member is not Multiple with a Scope_closed of another
   member that called with_timeout. *)
let with_timeout_after_failure () =
  run (fun () ->
      let c = Chan.create () in
      raises (Boom "b") (fun () ->
          Fiber.scope (fun s ->
              Fiber.fork s (fun () ->
                  while true do
                    Fiber.with_timeout Mono (Span.s 1) (fun () ->
                        ignore (sync (Chan.recv c)) )
                  done ) ;
              Fiber.fork s (fun () ->
                  sync (Chan.send c 1) ;
                  raise (Boom "b") ) ) ) )

(* T5: an outer cancel at the same time as the deadline gives the Cancelled of
   the outer scope. *)
let with_timeout_outer_at_deadline () =
  run (fun () ->
      let r = ref "" in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              try
                Fiber.with_timeout Mono (Span.s 1) (fun () ->
                    Time.sleep Mono (Span.s 10) )
              with
              | Fiber.Timeout ->
                  r := "timeout"
              | Fiber.Cancelled (Boom m) ->
                  r := m ) ;
          Time.sleep Mono (Span.s 1) ;
          Fiber.cancel s (Boom "outer") ) ;
      equal string "outer" !r )

(* C2: a daemon removes its node when it ends. *)
let daemon_nodes_removed () =
  run (fun () ->
      Fiber.scope (fun s ->
          for _ = 1 to 100 do
            Fiber.fork_daemon s ignore
          done ;
          Fiber.yield () ;
          let ctx = Private.scope_ctx s in
          equal int 0 (Hashtbl.length ctx.Private.Sched.children) ;
          match ctx.Private.Sched.kind with
          | Private.Sched.Scope_node st ->
              equal int 0 (Hashtbl.length st.Private.Sched.daemons)
          | _ ->
              failwith "not a scope" ) )

let yield_in_cancelled_scope () =
  run (fun () ->
      let r = ref "" in
      Fiber.scope (fun s ->
          Fiber.cancel s (Boom "y") ;
          try Fiber.yield () with Fiber.Cancelled (Boom m) -> r := m ) ;
      equal string "y" !r )

(* T6: a fill outside a fiber raises Not_in_fiber and changes nothing. *)
let fill_outside_fiber () =
  let iv = Ivar.create () in
  raises_match
    (function Fiber.Not_in_fiber _ -> true | _ -> false)
    (fun () -> Ivar.fill iv 1) ;
  equal (option int) None (Ivar.peek iv)

(* K4: the waiters of an Ivar stay bounded in a loop with a timeout. *)
let ivar_waiters_bounded () =
  run (fun () ->
      let iv : int Ivar.t = Ivar.create () in
      for _ = 1 to 100 do
        ignore
          (sync
             (choose
                [ wrap (Ivar.read iv) Option.some
                ; wrap (Time.after Mono (Span.ms 1)) (fun () -> None) ] ) )
      done ;
      match Private.ivar_state iv with
      | Private.Sched.Empty (_, w) ->
          less int ~than:16 (List.length w.Private.Sched.ws)
      | Private.Sched.Full _ ->
          failwith "full" )

let reviews =
  group "review of M0"
    [ test "C1: with_timeout in a cancelled scope gives its Cancelled"
        with_timeout_in_cancelled_scope
    ; test "C1: with_timeout after a failure does not add Scope_closed"
        with_timeout_after_failure
    ; test "T5: an outer cancel at the deadline is not a Timeout"
        with_timeout_outer_at_deadline
    ; test "C2: a daemon removes its node when it ends" daemon_nodes_removed
    ; test "a yield in a cancelled scope raises Cancelled"
        yield_in_cancelled_scope
    ; test "T6: a fill outside a fiber raises Not_in_fiber and fills nothing"
        fill_outside_fiber
    ; test "K4: the waiters of an Ivar stay bounded" ivar_waiters_bounded ]

(* A fiber that ended is not a member of its scope any more. *)
let ended_fibers_leave () =
  Sim.run (fun _ ->
      Fiber.scope (fun s ->
          for _ = 1 to 10 do
            Fiber.fork s ignore
          done ;
          Fiber.yield () ;
          (* Only the fiber of the body stays. *)
          equal int 1 (Private.scope_ctx s).S.members.S.count ) )

(* The body of a scope that ended is not a member of the scope any more. *)
let body_leaves () =
  Sim.run (fun _ ->
      let sc = ref None in
      Fiber.scope (fun s -> sc := Some s) ;
      equal int 0 (Private.scope_ctx (Option.get !sc)).S.members.S.count )

(* A cancel of a scope reaches its daemons with the reason of the cancel, not
   with the end of the other members. *)
let daemon_gets_reason () =
  Sim.run (fun _ ->
      let got = ref None in
      Fiber.scope (fun s ->
          Fiber.fork_daemon s (fun () ->
              match sync (Ivar.read (Ivar.create ())) with
              | () ->
                  ()
              | exception Fiber.Cancelled r ->
                  got := Some r ) ;
          Fiber.yield () ;
          Fiber.cancel s Exit ;
          (* The daemon runs before the body ends. *)
          try Fiber.yield () with Fiber.Cancelled _ -> () ) ;
      is_true (match !got with Some Exit -> true | _ -> false) )

(* After [f], a cancel of the scope reaches the next wait of the fiber. *)
let cancel_after f =
  Sim.run (fun _ ->
      let got = ref false in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              f () ;
              match sync (Ivar.read (Ivar.create ())) with
              | () ->
                  ()
              | exception Fiber.Cancelled _ ->
                  got := true ) ;
          Fiber.yield () ;
          Fiber.cancel s Exit ) ;
      is_true !got )

(* The root scope (design section 2.5). The tests fork into it with the inside
   API: a program cannot. *)

(* When main returns, the root scope cancels the other fibers. *)
let root_cancels_others () =
  let reason = ref None in
  Sim.run (fun _ ->
      let root = List.hd (S.self ()).S.ctxs in
      S.fork root (fun () ->
          match sync (Ivar.read (Ivar.create ())) with
          | () ->
              ()
          | exception Fiber.Cancelled r ->
              reason := Some r ) ) ;
  is_true (match !reason with Some S.Main_returned -> true | _ -> false)

(* Main is a member of the root scope: a cancel of the root reaches it. *)
let main_in_root () =
  Sim.run (fun _ ->
      let root = List.hd (S.self ()).S.ctxs in
      S.fork root (fun () -> S.cancel root Exit) ;
      match sync (Ivar.read (Ivar.create ())) with
      | () ->
          failwith "the wait ended"
      | exception Fiber.Cancelled Exit ->
          () )

let scopes =
  group "scopes"
    [ test "an error in a child cancels the body" child_error_cancels_body
    ; test "a cancel goes down to a nested scope" cancel_reaches_nested
    ; test "the cleanup in a shield sends its frame" shield_cleanup
    ; test "with_timeout inside a shield still times out"
        shield_timeout_still_works
    ; test "the join waits for all children when a child fails"
        join_waits_for_all
    ; test "two failures give Multiple" multiple_failures
    ; test "with_timeout raises Timeout at exactly the deadline"
        with_timeout_exact
    ; test "with_timeout gives the value of f" with_timeout_value
    ; test "with_timeout with a deadline that overflows adds no timer"
        with_timeout_overflow
    ; test "a cancel of an isolated fiber does not go to on_error"
        isolated_cancel
    ; test "an outer cancel in with_timeout is not a Timeout"
        with_timeout_outer_cancel
    ; test "the scope cancels a daemon when the others end"
        daemon_cancelled_at_end
    ; test "the end of a daemon does not cancel the other daemons"
        daemon_end_keeps_daemons
    ; test "a cancel inside with_timeout is not a Timeout"
        with_timeout_inner_cancel
    ; test "live_timers counts a sleep" live_timer_count
    ; test "a cancel is sticky" cancel_is_sticky
    ; test "a cancel at the start of a sync wins over a ready branch"
        cancel_wins_over_ready
    ; test "a fork into an ended scope raises Scope_closed"
        fork_into_ended_scope
    ; test "a fork into a cancelled scope raises Scope_closed"
        fork_into_cancelled_scope
    ; test "a promise holds the exception, and the scope does not fail"
        promise_holds_exception
    ; test "Promise.ready races a timeout" promise_ready_in_choose
    ; test "the backtrace of a child is present after scope and await"
        backtrace_of_child
    ; test "run gives the value of main" main_gives_value
    ; test "a cancelled scope leaves no fibers and no live timers"
        no_fibers_after_cancel
    ; test "a fiber that ended leaves its scope" ended_fibers_leave
    ; test "the body of a scope that ended leaves it" body_leaves
    ; test "a daemon gets the reason of the cancel" daemon_gets_reason
    ; test "a cancel reaches a fiber after a shield" (fun () ->
          cancel_after (fun () -> Fiber.shield ignore) )
    ; test "a cancel reaches a fiber after a shield that raised" (fun () ->
          cancel_after (fun () ->
              try Fiber.shield (fun () -> raise Exit) with Exit -> () ) )
    ; test "the root scope cancels the others when main returns"
        root_cancels_others
    ; test "a cancel of the root scope reaches main" main_in_root ]

(* The checks of the simulator *)

let deadlock_names_fiber () =
  match
    run (fun () ->
        Fiber.scope (fun s ->
            Fiber.fork ~name:"waiter" s (fun () ->
                sync (Ivar.read (Ivar.create ())) ) ) )
  with
  | () ->
      failwith "no deadlock"
  | exception Sim.Deadlock names ->
      equal (list string) ["main"; "waiter"] names

let planted_lost_wakeup () =
  match
    Sim.run (fun sim ->
        Fiber.scope (fun s ->
            Fiber.fork ~name:"victim" s (fun () ->
                sync (Ivar.read (Ivar.create ())) ) ;
            Fiber.yield () ;
            (* Commit the parked sync of the victim, but do not put it in a
               queue: a lost wakeup. *)
            List.iter
              (fun (name, (s : Private.Sched.sync)) ->
                if name = "victim" then ignore (Private.Sched.C.notify s.word 0) )
              (Private.Sched_debug.parked (Sim.sched sim)) ;
            Time.sleep Mono (Span.s 1) ) )
  with
  | () ->
      failwith "no lost wakeup"
  | exception Sim.Lost_wakeup l ->
      equal (list string) ["victim is committed but not in a queue"] l

type _ Effect.t += No_handler : unit Effect.t

(* The simulator stops only at the world effects. Another effect with no handler
   raises [Effect.Unhandled] in the fiber that performs it. *)
let unhandled_effect () =
  run (fun () ->
      let r =
        match Effect.perform No_handler with
        | () ->
            "a handler"
        | exception Effect.Unhandled No_handler ->
            "Unhandled"
      in
      equal string "Unhandled" r )

let checks =
  group "checks"
    [ test "a fiber that waits forever gives Deadlock with its name"
        deadlock_names_fiber
    ; test "a planted lost wakeup gives Lost_wakeup" planted_lost_wakeup
    ; test "an effect with no handler raises Effect.Unhandled in the fiber"
        unhandled_effect ]

let stale_timers () =
  let add, get = log () in
  Sim.run (fun sim ->
      let t0 = Time.now Mono in
      Fiber.scope (fun s ->
          (* Four live entries: a rebuild of the heap must keep their order, and
             an even count reaches the last child in the heapify. *)
          List.iter
            (fun (name, sec) ->
              Fiber.fork s (fun () ->
                  Time.sleep Mono (Span.s sec) ;
                  add "%s at %d ms" name (ms_since t0) ) )
            [("c", 300); ("a", 100); ("d", 400); ("b", 200)] ;
          (* Let the sleepers register their entries first. *)
          Fiber.yield () ;
          (* Each sync can register a timer that then loses to [always]. The
             stale entries are the earliest, so they are at the top. *)
          for _ = 1 to 300 do
            sync (choose [always (); Time.after Mono (Span.ms 500)])
          done ;
          Time.sleep Mono (Span.s 1) ;
          add "%s at %d ms" "main" (ms_since t0) ) ;
      equal int 0 (Private.Sched_debug.live_timers (Sim.sched sim)) ) ;
  equal (list string)
    [ "main at 1000 ms"
    ; "a at 100000 ms"
    ; "b at 200000 ms"
    ; "c at 300000 ms"
    ; "d at 400000 ms" ]
    (get ())

let two_timers_one_resume () =
  run (fun () ->
      let n = ref 0 in
      sync
        (choose
           [ wrap (Time.after Mono (Span.s 1)) (fun () -> incr n)
           ; wrap (Time.after Mono (Span.s 1)) (fun () -> incr n) ] ) ;
      Time.sleep Mono (Span.s 1) ;
      equal int 1 !n )

let ivar_stale_waiter () =
  run (fun () ->
      let iv = Ivar.create () in
      let r =
        sync
          (choose
             [ wrap (Ivar.read iv) (fun v -> v)
             ; wrap (Time.after Mono (Span.ms 1)) (fun () -> 0) ] )
      in
      equal int 0 r ;
      (* The fill finds the stale waiter, and a new read still works. *)
      Ivar.fill iv 5 ;
      equal int 5 (sync (Ivar.read iv)) )

let timers =
  group "timers"
    [ test "stale timers do not change the live ones" stale_timers
    ; test "two timers of one sync resume the fiber one time"
        two_timers_one_resume
    ; test "a fill skips a waiter that committed to another branch"
        ivar_stale_waiter ]

(* Channels *)

let live_offers c =
  let module C = Private.Chan in
  let live o = not (Private.Sched.is_done o) in
  let count side =
    List.length (List.filter (fun o -> live o.C.osync) (C.offers side))
  in
  count c.C.senders + count c.C.receivers

let send_recv_meet () =
  run (fun () ->
      let c = Chan.create () in
      let got =
        Fiber.scope (fun s ->
            let r = Fiber.fork_promise s (fun () -> sync (Chan.recv c)) in
            Fiber.fork s (fun () -> sync (Chan.send c "hello")) ;
            Promise.await r )
      in
      equal string "hello" got )

(* The cross scenario of spec/tla/Commit.tla: X syncs on [send c1; recv c2] and
   Y on [send c2; recv c1]. Exactly one pair commits for each. *)
let cross seed =
  let x, y =
    run ~seed (fun () ->
        let c1 = Chan.create () and c2 = Chan.create () in
        Fiber.scope (fun s ->
            let x =
              Fiber.fork_promise s (fun () ->
                  sync
                    (choose
                       [ wrap (Chan.send c1 "x") (fun () -> "x sent on c1")
                       ; wrap (Chan.recv c2) (fun v -> "x got " ^ v ^ " on c2")
                       ] ) )
            in
            let y =
              Fiber.fork_promise s (fun () ->
                  sync
                    (choose
                       [ wrap (Chan.send c2 "y") (fun () -> "y sent on c2")
                       ; wrap (Chan.recv c1) (fun v -> "y got " ^ v ^ " on c1")
                       ] ) )
            in
            (Promise.await x, Promise.await y) ) )
  in
  is_true
    ( (x, y) = ("x sent on c1", "y got x on c1")
    || (x, y) = ("x got y on c2", "y sent on c2") )

let own_offer_never_meets () =
  run (fun () ->
      let c = Chan.create () in
      let r =
        sync
          (choose
             [ wrap (Chan.send c 1) (fun () -> "met")
             ; wrap (Chan.recv c) (fun _ -> "met")
             ; wrap (Time.after Mono (Span.s 1)) (fun () -> "timeout") ] )
      in
      equal string "timeout" r )

let stale_offer_skipped () =
  run (fun () ->
      let c = Chan.create () in
      let r =
        sync
          (choose
             [ wrap (Chan.recv c) (fun v -> v)
             ; wrap (Time.after Mono (Span.ms 10)) (fun () -> "timeout") ] )
      in
      equal string "timeout" r ;
      (* The offer of the first recv is stale. The send must wait for the second
         receiver. *)
      let t0 = Time.now Mono in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              Time.sleep Mono (Span.s 1) ;
              equal string "late" (sync (Chan.recv c)) ) ;
          sync (Chan.send c "late") ;
          equal int 1000 (ms_since t0) ) )

let cancel_in_recv () =
  run (fun () ->
      let c = Chan.create () in
      let r = ref "" in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              try ignore (sync (Chan.recv c))
              with Fiber.Cancelled (Boom m) -> r := m ) ;
          Fiber.yield () ;
          Fiber.cancel s (Boom "stop") ) ;
      equal string "stop" !r ;
      equal int 0 (live_offers c) )

let no_offers_after_cancel () =
  run (fun () ->
      let c = Chan.create () in
      Fiber.scope (fun s ->
          for i = 1 to 3 do
            Fiber.fork s (fun () -> sync (Chan.send c i))
          done ;
          Fiber.fork s (fun () -> ignore (sync (Chan.recv (Chan.create ())))) ;
          Fiber.yield () ;
          equal int 3 (live_offers c) ;
          Fiber.cancel s (Boom "stop") ) ;
      equal int 0 (live_offers c) )

let timeout_loop_bounded () =
  run (fun () ->
      let c : int Chan.t = Chan.create () in
      for _ = 1 to 100 do
        ignore
          (sync
             (choose
                [ wrap (Chan.recv c) Option.some
                ; wrap (Time.after Mono (Span.ms 1)) (fun () -> None) ] ) )
      done ;
      let module C = Private.Chan in
      (* The stale offers are dropped when the side doubles its size, from at
         least 8 offers. *)
      less int ~than:8 (List.length (C.offers c.C.receivers)) )

(* Each sent value arrives exactly one time. *)
let values_conserved (seed, n) =
  let got =
    run ~seed (fun () ->
        let c = Chan.create () in
        let got = ref [] in
        Fiber.scope (fun s ->
            for i = 1 to n do
              Fiber.fork s (fun () ->
                  if i mod 2 = 0 then Fiber.yield () ;
                  sync (Chan.send c i) ) ;
              Fiber.fork s (fun () ->
                  if i mod 3 = 0 then Fiber.yield () ;
                  (* Read [!got] after the sync, not before it. *)
                  let v = sync (Chan.recv c) in
                  got := v :: !got )
            done ) ;
        !got )
  in
  equal (list int) (List.init n (fun i -> i + 1)) (List.sort compare got)

(* All the offers of a side, also the stale offers. [C.offers] moves [back] to
   [front], so the count does not use it. *)
let all_offers side =
  List.length (Atomic.get side.C.front) + List.length (Atomic.get side.C.back)

(* One branch of the choose skips the offer of the other branch. The offer stays
   in the queue, and a later partner meets it. The seed selects the order of the
   two branches, so the test has a partner of each type. *)
let skipped_own_offer partner_sends () =
  Helpers.run (fun () ->
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
  Helpers.run (fun () ->
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

(* With an empty [back], [C.offers] gives [front] itself, with no copy. The drop
   of the stale offers calls it, so a copy adds a second pass over the
   offers. *)
let offers_no_copy () =
  Helpers.run (fun () ->
      let c = Chan.create () in
      Fiber.scope (fun s ->
          for _ = 1 to 3 do
            Fiber.fork s (fun () -> ignore (sync (Chan.recv c)))
          done ;
          Time.sleep Mono (Span.ms 1) ;
          sync (Chan.send c 1) ;
          let side = c.C.receivers in
          equal int 0 (List.length (Atomic.get side.C.back)) ;
          let front = Atomic.get side.C.front in
          is_true (C.offers side == front) ;
          sync (Chan.send c 2) ;
          sync (Chan.send c 3) ) )

let chans =
  group "channels"
    [ test "a send and a recv that start at the same time meet" send_recv_meet
    ; prop "cross: each sync commits exactly one pair" Gen.int cross
    ; test "a sync never meets its own offer" own_offer_never_meets
    ; test "a stale offer does not meet a later send" stale_offer_skipped
    ; test "a cancel ends a recv and leaves no offer" cancel_in_recv
    ; test "a cancelled scope leaves no live offers" no_offers_after_cancel
    ; test "a loop of recv with a timeout keeps the offers bounded"
        timeout_loop_bounded
    ; prop "each sent value arrives exactly one time"
        (Gen.pair Gen.int (Gen.int_range 0 12))
        values_conserved
    ; test "a recv meets an offer that its own sync skipped"
        (skipped_own_offer false)
    ; test "a send meets an offer that its own sync skipped"
        (skipped_own_offer true)
    ; test "a send that meets an offer in front does not scan back"
        send_stops_in_front
    ; test "with an empty back, offers does not copy front" offers_no_copy ]

(* Properties *)

let spans = Gen.list ~size:(Gen.int_range 1 8) (Gen.int_range (-3) 5000)

let each_sleep_is_exact ms_list =
  let woke =
    run (fun () ->
        let t0 = Time.now Mono in
        let r = ref [] in
        Fiber.scope (fun s ->
            List.iteri
              (fun i ms ->
                Fiber.fork s (fun () ->
                    Time.sleep Mono (Span.ms ms) ;
                    r := (i, ms_since t0) :: !r ) )
              ms_list ) ;
        !r )
  in
  List.iter (fun (i, at) -> equal int (Int.max 0 (List.nth ms_list i)) at) woke ;
  equal int (List.length ms_list) (List.length woke)

(* A program with random choices: its trace depends only on the seed. *)
let random_program () =
  let add, get = log () in
  Fiber.scope (fun s ->
      for i = 1 to 4 do
        Fiber.fork s (fun () ->
            for _ = 1 to 3 do
              let v = sync (choose [always "x"; always "y"; always "z"]) in
              add "%d%s" i v ; Fiber.yield ()
            done )
      done ) ;
  get ()

let same_seed_same_run seed =
  equal (list string) (run ~seed random_program) (run ~seed random_program)

let seeds_differ () =
  let traces = List.init 8 (fun seed -> run ~seed random_program) in
  greater int ~than:1 (List.length (List.sort_uniq compare traces))

(* The [n] bytes of a [Random_bytes] in a run with [seed]. *)
let random_bytes seed n =
  run ~seed (fun () ->
      let b = Bytes.make n '\000' in
      Private.World.perform "test" (Private.World.Random_bytes b) ;
      Bytes.to_string b )

let random_bytes_from_seed () =
  let a = random_bytes 1 64 in
  equal int 64 (String.length a) ;
  equal string a (random_bytes 1 64) ;
  is_true (a <> random_bytes 2 64) ;
  is_true (a <> String.make 64 '\000') ;
  equal string "" (random_bytes 1 0)

let properties =
  group "properties"
    [ prop "each sleep ends at exactly its deadline" spans each_sleep_is_exact
    ; prop "a seed gives the same run" Gen.int same_seed_same_run
    ; test "different seeds give different runs" seeds_differ
    ; test "a seed gives the same random bytes" random_bytes_from_seed ]

(* The tests of the public API that each world runs. *)
let portable_world =
  let n = ref 0 in
  { Portable.name= "portable"
  ; domains= 1
  ; run= (fun f -> run f)
  ; listen=
      (fun () ->
        incr n ;
        let a = Net.Tcp ("portable", !n) in
        (Net.listen a, a) )
  ; udp=
      (fun () ->
        incr n ;
        let a = ("portable", !n) in
        (Net.udp a, a) )
  ; tmp=
      (let n = ref 0 in
       fun () ->
         incr n ;
         let d = Printf.sprintf "/tmp%d" !n in
         Dir.mkdir d ; d ) }

(* The same tests on four virtual domains, with a seed for each run. *)
let portable_world_4 =
  let n = ref 0 and seed = ref 0 in
  { Portable.name= "portable on 4 domains"
  ; domains= 4
  ; run=
      (fun f ->
        incr seed ;
        Sim.run ~seed:!seed ~domains:4 (fun _ -> f ()) )
  ; listen=
      (fun () ->
        incr n ;
        let a = Net.Tcp ("portable4", !n) in
        (Net.listen a, a) )
  ; udp=
      (fun () ->
        incr n ;
        let a = ("portable4", !n) in
        (Net.udp a, a) )
  ; tmp=
      (let n = ref 0 in
       fun () ->
         incr n ;
         let d = Printf.sprintf "/tmp%d" !n in
         Dir.mkdir d ; d ) }

(* The review of 2026-10-16 (design section 3.12). *)

(* C1: a fiber cancels a scope inside it, and the Cancelled of that scope goes
   out of the fiber. Nobody cancelled the scope of the fiber, thus the Cancelled
   is a failure of that scope. *)
let inner_cancel_fails_fork () =
  run (fun () ->
      match
        Fiber.scope (fun o ->
            Fiber.fork o (fun () ->
                Fiber.scope (fun s ->
                    Fiber.cancel s (Boom "inner") ;
                    Fiber.yield () ) ) )
      with
      | () ->
          failwith "the scope gave no failure"
      | exception Fiber.Cancelled (Boom m) ->
          equal string "inner" m )

(* C1: the body of a scope raises the Cancelled of a scope inside it. The scope
   fails, and it cancels the other fiber at once. *)
let inner_cancel_fails_body () =
  run (fun () ->
      let t0 = Time.now Mono in
      let other = ref "running" in
      ( match
          Fiber.scope (fun o ->
              Fiber.fork o (fun () ->
                  try
                    Time.sleep Mono (Span.s 100) ;
                    other := "ended"
                  with Fiber.Cancelled _ as e ->
                    other := "cancelled" ;
                    raise e ) ;
              Fiber.scope (fun s ->
                  Fiber.fork s (fun () -> Fiber.cancel s (Boom "inner")) ;
                  Time.sleep Mono (Span.s 1) ) )
        with
      | () ->
          failwith "the scope gave no failure"
      | exception Fiber.Cancelled (Boom m) ->
          equal string "inner" m ) ;
      equal string "cancelled" !other ;
      equal int 0 (ms_since t0) )

(* C1: the Cancelled of a scope inside an isolated fiber goes to [on_error]. *)
let inner_cancel_isolated () =
  run (fun () ->
      let errors = ref [] in
      Fiber.scope (fun s ->
          Fiber.fork_isolated s
            ~on_error:(fun e _ -> errors := e :: !errors)
            (fun () ->
              Fiber.scope (fun inner ->
                  Fiber.cancel inner (Boom "inner") ;
                  Fiber.yield () ) ) ) ;
      match !errors with
      | [Fiber.Cancelled (Boom m)] ->
          equal string "inner" m
      | _ ->
          failwith "on_error did not get one Cancelled" )

(* C1: the Cancelled of a scope inside a promise is a failure of the promise,
   not a cancel of it. *)
let inner_cancel_promise () =
  run (fun () ->
      Fiber.scope (fun s ->
          let p =
            Fiber.fork_promise s (fun () ->
                Fiber.scope (fun inner ->
                    Fiber.cancel inner (Boom "inner") ;
                    Fiber.yield () ) )
          in
          match Promise.await p with
          | () ->
              failwith "the promise gave no failure"
          | exception Fiber.Cancelled (Boom m) ->
              equal string "inner" m ) )

(* C2: [f] runs in a finaliser, outside a fiber. It must raise
   [Not_in_fiber]. *)
let outside_fiber f =
  let r = ref None in
  let () =
    let x = ref 1 in
    Gc.finalise
      (fun _ ->
        r :=
          Some
            ( match f () with
            | () ->
                "no exception"
            | exception Fiber.Not_in_fiber _ ->
                "Not_in_fiber"
            | exception e ->
                Printexc.to_string e ) )
      x
  in
  Gc.full_major () ;
  equal (option string) (Some "Not_in_fiber") !r

(* C2: a fork outside a fiber raises [Not_in_fiber] before it changes the scope.
   Thus the scope ends. *)
let fork_outside_fiber () =
  run (fun () ->
      Fiber.scope (fun s -> outside_fiber (fun () -> Fiber.fork s ignore)) ;
      Fiber.scope (fun s ->
          outside_fiber (fun () -> Fiber.fork_daemon s ignore) ;
          equal int 0 (Hashtbl.length (Private.scope_ctx s).S.children) ) )

(* C2: a wait outside a fiber raises [Not_in_fiber] before it adds a waiter.
   Thus the next signal goes to a fiber that waits. *)
let wait_outside_fiber () =
  let c = Fiber.Condition.create () and m = Fiber.Mutex.create () in
  raises_match
    (function Fiber.Not_in_fiber _ -> true | _ -> false)
    (fun () -> Fiber.Condition.wait c m) ;
  run (fun () ->
      let woken = ref false in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              Fiber.Mutex.use_rw ~protect:false m (fun () ->
                  Fiber.Condition.wait c m ;
                  woken := true ) ) ;
          Fiber.yield () ;
          Fiber.Mutex.use_ro m (fun () -> Fiber.Condition.signal c) ) ;
      is_true !woken )

let review_2026 =
  group "review of 2026-10-16"
    [ test "C1: a Cancelled from an inner scope fails a fork"
        inner_cancel_fails_fork
    ; test "C1: a Cancelled from an inner scope fails the body"
        inner_cancel_fails_body
    ; test "C1: a Cancelled from an inner scope goes to on_error"
        inner_cancel_isolated
    ; test "C1: a Cancelled from an inner scope fails a promise"
        inner_cancel_promise
    ; test "C2: a fork outside a fiber does not change the scope"
        fork_outside_fiber
    ; test "C2: a wait outside a fiber adds no waiter" wait_outside_fiber ]

let () =
  exit
    (Windtrap.run "sim"
       ( [ time
         ; timers
         ; events
         ; chans
         ; scopes
         ; reviews
         ; review_2026
         ; checks
         ; properties ]
       @ Test_net.tests @ Test_m1.tests @ Test_m2.tests @ Test_m3.tests
       @ Test_wakes.tests @ Test_io.tests @ Test_sync.tests
       @ Portable.tests portable_world
       @ Portable.tests portable_world_4
       @ [ Portable.reset_details portable_world
         ; Portable.reset_details portable_world_4
         ; Portable.zero_writes portable_world
         ; Portable.zero_writes portable_world_4 ] ) )
