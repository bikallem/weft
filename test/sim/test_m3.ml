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
      let off = Atomic.make 0 and c = Chan.create () in
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
                sync (Chan.send c ()) )
          done ) ;
      Atomic.get off )

(* A fill from outside a fiber touches no sync word: it gives at once, also
   while the sync word of the waiter is Claimed, for example by the code that a
   finaliser interrupts. A plain fill would spin on Claimed forever. *)
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
  Sim.run ~outside_fills:true (fun sim ->
      let iv = Ivar.create () in
      let () =
        let x = ref 1 in
        Gc.finalise (fun _ -> ignore (Ivar.fill_from_outside iv "gc")) x
      in
      Gc.full_major () ;
      equal string "gc" (sync (Ivar.read iv)) ;
      equal int 1 (List.length (Sim.fill_log sim)) )

let refused () =
  raises
    (Sim.Model_error
       "a fill from outside a fiber, with no Sim.run ~outside_fills:true" )
    (fun () ->
      Sim.run (fun _ ->
          let iv = Ivar.create () in
          ignore (Ivar.fill_from_outside iv ()) ;
          sync (Ivar.read iv) ) )

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
      is_true (List.mem "waiter" l)

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

let tests =
  [ group "m3"
      [ test "pinned fibers run only on domain 0, with 200 seeds" (fun () ->
            for seed = 1 to 200 do
              equal ~msg:(Printf.sprintf "seed %d" seed) int 0 (pinned seed)
            done )
      ; test "a fill from outside gives at once while the word is Claimed"
          fill_while_claimed
      ; test "the deadline form of with_timeout has no fiber" deadline_form
      ; test "a yield loop that waits for the time gives Livelock" livelock
      ; test "a stream of 4 MiB is not a livelock" stream
      ; test "a failed promise that nobody awaits gives Unawaited" unawaited
      ; test "a finaliser fills an Ivar" finaliser
      ; test "a fill from outside needs ~outside_fills" refused
      ; test "no run: fill_from_outside gives false" (fun () ->
            is_false (Ivar.fill_from_outside (Ivar.create ()) ()) ) ] ]
