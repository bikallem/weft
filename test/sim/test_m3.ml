(* The tests of the tuning on the simulator (design section 2.14, M3). *)

open Windtrap
open Weft
module Sim = Weft_sim
module S = Private.Sched
module C = S.C

let dom () = S.prog "test" S.Dom_index

(* Pinned fibers run only on domain 0: at the start, after a yield, after a
   sleep, and after a wake by a fiber of another domain. *)
let pinned seed =
  Sim.run ~seed ~domains:4 (fun _ ->
      let off = Atomic.make 0
      and c = Chan.create ()
      and other = Atomic.make 0 in
      let check () = if dom () <> 0 then Atomic.incr off in
      Fiber.scope (fun s ->
          for _ = 1 to 4 do
            Fiber.fork ~pinned:true s (fun () ->
                check () ;
                Fiber.yield () ;
                check () ;
                Time.sleep Mono (Span.ms 1) ;
                check () ;
                sync (Chan.recv c) ;
                check () )
          done ;
          for _ = 1 to 4 do
            Fiber.fork s (fun () ->
                Fiber.yield () ;
                if dom () <> 0 then Atomic.incr other ;
                sync (Chan.send c ()) )
          done ) ;
      (Atomic.get off, Atomic.get other) )

(* A fill from outside a fiber touches no sync word: it gives at once, also
   while the sync word of the waiter is Claimed, for example by the code that a
   finaliser interrupts. A plain fill would spin on Claimed forever: with that
   bug, this test does not end. *)
let fill_while_claimed () =
  Sim.run ~outside_fills:true (fun _ ->
      let iv = Ivar.create () and waiter = ref None in
      Fiber.scope (fun s ->
          let p =
            Fiber.fork_promise s (fun () ->
                waiter := Some (S.self ()) ;
                sync (Ivar.read iv) )
          in
          Fiber.yield () ;
          let w = Option.get !waiter in
          let sy = Option.get (Atomic.get w.S.current) in
          let word = C.word sy.S.word in
          equal string "Parked"
            (Format.asprintf "%a" C.pp_state (Atomic.get word)) ;
          Atomic.set word C.Claimed ;
          is_true (Ivar.fill_from_outside iv 7) ;
          Atomic.set word C.Parked ;
          equal int 7 (Promise.await p) ) )

(* A finaliser fills an Ivar from outside a fiber. *)
let finaliser () =
  Sim.run ~outside_fills:true (fun _ ->
      let iv = Ivar.create () in
      let () =
        let x = ref 1 in
        Gc.finalise (fun _ -> ignore (Ivar.fill_from_outside iv "gc")) x
      in
      Gc.full_major () ;
      equal string "gc" (sync (Ivar.read iv)) )

let refused () =
  raises
    (Sim.Model_error
       "a fill from outside a fiber, with no Sim.run ~outside_fills:true" )
    (fun () ->
      Sim.run (fun _ ->
          let iv = Ivar.create () in
          ignore (Ivar.fill_from_outside iv ()) ;
          sync (Ivar.read iv) ) )

(* An Ivar of a run that ended: a fill from outside in a later run gives false,
   and the later run does not refuse it. *)
let earlier_run () =
  let iv = ref (Ivar.create ()) in
  Sim.run (fun _ -> iv := Ivar.create ()) ;
  Sim.run (fun _ -> is_false (Ivar.fill_from_outside !iv 1)) ;
  is_true (Ivar.peek !iv = None)

(* The waiter of a run that ended in Deadlock is dead: a later run does not
   resume it. *)
let dead_waiter () =
  let iv = ref (Ivar.create ()) in
  raises_match
    (function Sim.Deadlock _ -> true | _ -> false)
    (fun () ->
      Sim.run (fun _ ->
          iv := Ivar.create () ;
          sync (Ivar.read !iv) ) ) ;
  Sim.run ~outside_fills:true (fun _ ->
      is_false (Ivar.fill_from_outside !iv 1) ;
      Fiber.yield () )

(* A run ends in Deadlock with a fiber that waits on [iv]. In a later run, a
   fiber fills [iv], with no waiter (dead_fill) or after a register of its own
   waiter (dead_and_live). The dead waiter never runs again. *)
let dead_run iv resumed =
  raises_match
    (function Sim.Deadlock _ -> true | _ -> false)
    (fun () ->
      Sim.run (fun _ ->
          iv := Ivar.create () ;
          ignore (sync (Ivar.read !iv) : int) ;
          resumed := true ) )

let dead_fill () =
  let iv = ref (Ivar.create ()) and resumed = ref false in
  dead_run iv resumed ;
  Sim.run (fun _ -> Ivar.fill !iv 7 ; Fiber.yield () ; Fiber.yield ()) ;
  is_false !resumed

let dead_and_live () =
  let iv = ref (Ivar.create ()) and resumed = ref false in
  dead_run iv resumed ;
  Sim.run (fun _ ->
      Fiber.scope (fun s ->
          let p = Fiber.fork_promise s (fun () -> sync (Ivar.read !iv)) in
          Fiber.yield () ;
          Ivar.fill !iv 7 ;
          equal int 7 (Promise.await p) ;
          Fiber.yield () ;
          Fiber.yield () ) ) ;
  is_false !resumed

(* An Ivar belongs to the run of its last waiter: a fiber of a later run waits
   for it, and a fill from outside reaches that fiber. *)
let later_waiter () =
  let iv = ref (Ivar.create ()) in
  Sim.run (fun _ -> iv := Ivar.create ()) ;
  Sim.run ~outside_fills:true (fun _ ->
      Fiber.scope (fun s ->
          let p = Fiber.fork_promise s (fun () -> sync (Ivar.read !iv)) in
          Fiber.yield () ;
          is_true (Ivar.fill_from_outside !iv 5) ;
          equal int 5 (Promise.await p) ) )

(* The deadline form of with_timeout: no fiber, and the entries that the caller
   ended go from the heap. The Timeout comes at exactly the deadline. *)
let deadline_form () =
  Sim.run (fun sim ->
      let sched = Sim.sched sim in
      for _ = 1 to 1000 do
        Fiber.with_timeout Mono (Span.s 10) ignore
      done ;
      equal int 1 (Hashtbl.length sched.S.fibers) ;
      at_most int ~than:128 sched.S.doms.(0).S.heap.S.size ;
      let t0 = Time.now Mono in
      raises Fiber.Timeout (fun () ->
          Fiber.with_timeout Mono (Span.ms 7) (fun () ->
              Time.sleep Mono (Span.s 1) ) ) ;
      equal int64 7_000_000L (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) )

(* A loop that yields until a time comes is a livelock in the simulator. *)
let livelock () =
  match
    Sim.run ~livelock:100_000 (fun _ ->
        let t0 = Time.now Mono in
        Fiber.scope (fun s ->
            Fiber.fork_daemon ~name:"parked" s (fun () ->
                sync (Ivar.read (Ivar.create ())) ) ;
            Fiber.fork ~name:"waiter" s (fun () ->
                while
                  Span.to_ns (Time.diff Mono (Time.now Mono) t0) < 1_000_000L
                do
                  Fiber.yield ()
                done ) ) )
  with
  | () ->
      failwith "no livelock"
  | exception Sim.Livelock l ->
      is_true (List.mem "waiter" l) ;
      is_false (List.mem "parked" l)

(* A long stream with no timer is progress, also with a small limit. *)
let stream () =
  Sim.run ~livelock:1000 ~domains:2 (fun sim ->
      let a = Sim.addr sim "stream:1" in
      let l = Net.listen a in
      let data = String.make (4 lsl 20) 'x' in
      Fiber.scope (fun s ->
          Fiber.fork_daemon s (fun () -> Portable.echo_server l) ;
          let c = Net.connect a in
          let got =
            Fiber.fork_promise s (fun () ->
                Portable.read_n c (String.length data) )
          in
          Net.write c data ;
          equal int (String.length data) (String.length (Promise.await got)) ;
          Net.close c ) ;
      Net.close l )

exception Boom

(* The report of a promise that failed and that no fiber awaited. *)
let unawaited () =
  raises
    (Sim.Unawaited [Printexc.to_string Boom])
    (fun () ->
      Sim.run (fun _ ->
          Fiber.scope (fun s ->
              ignore (Fiber.fork_promise s (fun () -> raise Boom)) ) ) ) ;
  (* An awaited failure, and a cancelled promise, give no report. *)
  Sim.run (fun _ ->
      Fiber.scope (fun s ->
          let p = Fiber.fork_promise s (fun () -> raise Boom) in
          raises Boom (fun () -> Promise.await p) ;
          let q = Fiber.fork_promise s (fun () -> Time.sleep Mono (Span.s 1)) in
          Promise.cancel q ) )

let mono_ms t0 =
  Int64.to_int
    (Int64.div (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) 1_000_000L)

(* Real Posix timers: a step of the wall clock moves a Posix deadline, and not a
   Mono deadline. *)
let posix_timers () =
  Sim.run (fun sim ->
      let t0 = Time.now Mono in
      Time.sleep Posix (Span.s 10) ;
      equal int 10_000 (mono_ms t0) ;
      (* A step forward: the Posix deadline is due at once. *)
      let p0 = Time.now Posix in
      let at = Option.get (Time.add Posix p0 (Span.s 10)) in
      Fiber.scope (fun s ->
          let posix =
            Fiber.fork_promise s (fun () ->
                sync (Time.at Posix at) ;
                mono_ms t0 )
          in
          let mono =
            Fiber.fork_promise s (fun () ->
                Time.sleep Mono (Span.s 5) ;
                mono_ms t0 )
          in
          Time.sleep Mono (Span.s 1) ;
          Sim.set_posix sim (Option.get (Time.add Posix p0 (Span.s 20))) ;
          equal int 11_000 (Promise.await posix) ;
          equal int 15_000 (Promise.await mono) ) ;
      (* A step back: the deadline is later in Mono time. *)
      let t1 = Time.now Mono in
      let p1 = Time.now Posix in
      Fiber.scope (fun s ->
          let posix =
            Fiber.fork_promise s (fun () ->
                sync
                  (Time.at Posix (Option.get (Time.add Posix p1 (Span.s 10)))) ;
                mono_ms t1 )
          in
          Time.sleep Mono (Span.s 1) ;
          Sim.set_posix sim (Option.get (Time.add Posix p1 (Span.s (-99)))) ;
          equal int (1_000 + 109_000) (Promise.await posix) ) )

(* The review of M3, B: a pinned fiber that yields does not starve the local
   queue of domain 0. *)
let pinned_yield () =
  Sim.run ~livelock:100_000 (fun _ ->
      let flag = Atomic.make false in
      Fiber.scope (fun s ->
          Fiber.fork ~pinned:true s (fun () ->
              while not (Atomic.get flag) do
                Fiber.yield ()
              done ) ;
          Fiber.fork s (fun () -> Atomic.set flag true) ) )

(* The review of M3: a long exchange on a channel is progress, also with a small
   limit of Livelock. *)
let channel_progress () =
  Sim.run ~livelock:1000 (fun _ ->
      let c = Chan.create () in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              for i = 1 to 5000 do
                sync (Chan.send c i)
              done ) ;
          for _ = 1 to 5000 do
            ignore (sync (Chan.recv c))
          done ) )

(* The review of M3: a run that raises restores the report of the promises. *)
let restore_report () =
  let old = Atomic.get Private.promise_report in
  raises Exit (fun () -> Sim.run (fun _ -> raise Exit)) ;
  is_true (Atomic.get Private.promise_report == old)

(* The report of a run gives a failure of another run, or of no run, to the
   report that was there before the run. *)
let report_of_other_run () =
  let old = Atomic.get Private.promise_report in
  let got = ref [] in
  Atomic.set Private.promise_report (fun run _ ->
      got := (if run = None then "no run" else "other run") :: !got ) ;
  Fun.protect
    ~finally:(fun () -> Atomic.set Private.promise_report old)
    (fun () ->
      Sim.run (fun sim ->
          let report = Atomic.get Private.promise_report in
          report None Exit ;
          report (Some ((Sim.sched sim).S.run_id + 1)) Exit ) ) ;
  equal (list string) ["no run"; "other run"] (List.rev !got)

(* The steal of a next slot: F1 wakes F2 into its next slot, then F1 makes
   switch points with no switch of fiber. Another domain takes F2. *)
let next_steal seed =
  Sim.run ~seed ~domains:4 (fun _ ->
      let c = Chan.create ()
      and ran = Atomic.make false
      and early = Atomic.make false in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              sync (Chan.recv c) ;
              Atomic.set ran true ) ;
          Fiber.yield () ;
          Fiber.fork s (fun () ->
              (* F2 parks first, so the send claims F2. *)
              Time.sleep Mono (Span.ms 1) ;
              sync (Chan.send c ()) ;
              (* F2 ran before the end of the loop of F1. *)
              Atomic.set early (Helpers.spin_until ran) ) ) ;
      Atomic.get early )

(* A pinned fiber that a fiber of another domain wakes while domain 0 sleeps. *)
let pinned_wake seed =
  Sim.run ~seed ~domains:4 (fun _ ->
      let c = Chan.create () in
      Fiber.scope (fun s ->
          Fiber.fork ~pinned:true s (fun () -> sync (Chan.recv c)) ;
          Fiber.fork s (fun () ->
              Time.sleep Mono (Span.ms 1) ;
              (* The send comes from another domain. *)
              while dom () = 0 do
                Fiber.yield ()
              done ;
              sync (Chan.send c ()) ) ) )

(* The check of the queues at quiescence sees a fiber in a next slot. *)
let lost_next () =
  let t = S.create ~domains:1 () in
  let f = S.new_fiber [S.new_node (S.Scope_node (S.new_scope_state ()))] in
  S.push_next t.S.doms.(0) f (fun () -> S.Finished) ;
  is_true
    (List.mem "the queue of domain 0 is not empty"
       (Private.Sched_debug.lost t ~now:(Clock.of_ns Mono 0L)) )

(* One fiber that a fiber wakes goes to the next slot: it runs before the fiber
   that is at the head of the local queue. *)
let woken_goes_next () =
  let add, get = Helpers.log () in
  Sim.run (fun _ ->
      let iv = Ivar.create () in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              sync (Ivar.read iv) ;
              add "woken" ) ;
          Fiber.yield () ;
          Fiber.fork s (fun () -> add "queued") ;
          Ivar.fill iv () ;
          Fiber.yield () ) ) ;
  equal (list string) ["woken"; "queued"] (get ())

(* The next slot runs at most 3 times in sequence (open question 5). Two fibers
   that send to each other fill the next slot, and the third fiber runs after 3
   runs from the next slot. *)
let next_limit () =
  let add, get = Helpers.log () in
  Sim.run (fun _ ->
      let c = Chan.create () in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              for i = 1 to 6 do
                sync (Chan.send c i)
              done ) ;
          Fiber.fork s (fun () ->
              for _ = 1 to 6 do
                add "%s" (string_of_int (sync (Chan.recv c)))
              done ) ;
          Fiber.fork s (fun () -> add "%s" "other") ) ) ;
  equal (list string) ["1"; "2"; "3"; "other"; "4"; "5"; "6"] (get ())

(* Domain 0 takes a pinned fiber that yields at most 3 times in sequence. Then
   it takes the local queue. *)
let pinned_limit () =
  let add, get = Helpers.log () in
  Sim.run (fun _ ->
      Fiber.scope (fun s ->
          Fiber.fork ~pinned:true s (fun () ->
              for _ = 1 to 6 do
                add "p" ; Fiber.yield ()
              done ) ;
          Fiber.fork s (fun () ->
              for _ = 1 to 2 do
                add "n" ; Fiber.yield ()
              done ) ) ) ;
  equal string "pppnpppn" (String.concat "" (get ()))

(* A step of the wall clock wakes a domain that sleeps until a Posix deadline.
   The other domain does not let the program become quiescent. It gives true if
   the waiter slept on domain 1. *)
let posix_step seed =
  Sim.run ~seed ~domains:2 ~livelock:100_000 (fun sim ->
      let fired = Atomic.make false and on = ref (-1) in
      let at = Option.get (Time.add Posix (Time.now Posix) (Span.s 10)) in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              on := dom () ;
              sync (Time.at Posix at) ;
              Atomic.set fired true ) ;
          Fiber.fork ~pinned:true s (fun () ->
              Time.sleep Mono (Span.ms 1) ;
              Sim.set_posix sim (Option.get (Time.add Posix at (Span.s 1))) ;
              while not (Atomic.get fired) do
                Fiber.yield ()
              done ) ) ;
      !on = 1 )

let tests =
  [ group "m3"
      [ test "pinned fibers run only on domain 0, with 200 seeds" (fun () ->
            (* The senders also run on other domains. *)
            let other = ref 0 in
            for seed = 1 to 200 do
              let off, o = pinned seed in
              equal ~msg:(Printf.sprintf "seed %d" seed) int 0 off ;
              other := !other + o
            done ;
            greater int ~than:0 !other )
      ; test "B: a pinned fiber that yields does not starve" pinned_yield
      ; test "a woken fiber goes to the next slot" woken_goes_next
      ; test "the next slot runs at most 3 times in sequence" next_limit
      ; test "a pinned fiber runs at most 3 times in sequence" pinned_limit
      ; test "a thief takes a next slot, with 50 seeds" (fun () ->
            is_true (Helpers.some_seed 50 next_steal) )
      ; test "a pinned fiber wakes while domain 0 sleeps, with 50 seeds"
          (fun () ->
            for seed = 1 to 50 do
              pinned_wake seed
            done )
      ; test "the check of the queues sees a next slot" lost_next
      ; test "a fill from outside gives at once while the word is Claimed"
          fill_while_claimed
      ; test "the deadline form of with_timeout has no fiber" deadline_form
      ; test "a yield loop that waits for the time gives Livelock" livelock
      ; test "a stream of 4 MiB is not a livelock" stream
      ; test "a long exchange on a channel is not a livelock" channel_progress
      ; test "a run that raises restores the report of promises" restore_report
      ; test "a failure of another run goes to the old report"
          report_of_other_run
      ; test "a failed promise that nobody awaits gives Unawaited" unawaited
      ; test "a step of the wall clock moves a Posix timer only" posix_timers
      ; test
          "a step of the wall clock wakes a domain that sleeps, with 20 seeds"
          (fun () -> is_true (Helpers.some_seed 20 posix_step) )
      ; test "a finaliser fills an Ivar" finaliser
      ; test "a fill from outside needs ~outside_fills" refused
      ; test "a fill of an Ivar of a run that ended gives false" earlier_run
      ; test "a later run does not resume a waiter of a run that ended"
          dead_waiter
      ; test "a fill reaches a waiter of a later run" later_waiter
      ; test "a fill of a fiber does not resume a waiter of a run that ended"
          dead_fill
      ; test "a register of a later run drops the waiters of a run that ended"
          dead_and_live
      ; test "no run: fill_from_outside gives false" (fun () ->
            is_false (Ivar.fill_from_outside (Ivar.create ()) ()) )
      ; test "after a run, no run takes a fill from outside" (fun () ->
            Sim.run ~outside_fills:true (fun _ -> ()) ;
            is_false (Ivar.fill_from_outside (Ivar.create ()) ()) ) ] ]
