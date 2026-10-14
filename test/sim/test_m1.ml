(* The tests of many domains on the simulator (design section 2.14, M1). *)

open Windtrap
open Weft
module Sim = Weft_sim
module S = Private.Sched

(* The domain that runs the fiber. *)
let dom () = S.prog "test" S.Dom_index

let ms_since t0 =
  Int64.to_int
    (Int64.div (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) 1_000_000L)

(* Fibers that start on domain 0 run on other domains too. *)
let work_stealing seed =
  Sim.run ~seed ~domains:4 (fun _ ->
      let seen = Hashtbl.create 4 and ends = ref 0 in
      Fiber.scope (fun s ->
          for _ = 1 to 40 do
            Fiber.fork s (fun () ->
                for _ = 1 to 20 do
                  Hashtbl.replace seen (dom ()) () ;
                  Fiber.yield ()
                done ;
                incr ends )
          done ) ;
      equal int 40 !ends ;
      greater int ~than:1 (Hashtbl.length seen) )

(* Chained timers: each sleep of each fiber ends at exactly its deadline. *)
let chained_timers seed =
  Sim.run ~seed ~domains:4 (fun _ ->
      Fiber.scope (fun s ->
          for i = 1 to 8 do
            Fiber.fork s (fun () ->
                let t0 = Time.now Mono in
                for j = 1 to 25 do
                  Time.sleep Mono (Span.ms i) ;
                  equal int (i * j) (ms_since t0)
                done )
          done ) )

(* A fiber that a thief stole uses the state of the thief: a fiber parks on the
   domain [d] that runs it, and another fiber finds its entry in the heap of
   [d]. *)
let entry_on_domain seed =
  Sim.run ~seed ~domains:4 (fun sim ->
      let sched = Sim.sched sim in
      let found = ref 0 and stolen = ref 0 in
      Fiber.scope (fun s ->
          for _ = 1 to 8 do
            Fiber.fork s (fun () ->
                for _ = 1 to 5 do
                  Fiber.yield () ;
                  let d = dom () in
                  if d <> 0 then incr stolen ;
                  let me = S.self () in
                  Fiber.scope (fun inner ->
                      Fiber.fork inner (fun () ->
                          (* The check runs while [me] sleeps. *)
                          Time.sleep Mono (Span.us 500) ;
                          let h = sched.S.doms.(d).S.heap in
                          let mine = ref false in
                          for i = 0 to h.S.size - 1 do
                            let e = h.S.entries.(i) in
                            if
                              e.S.esync.S.owner == me
                              && not (S.is_done e.S.esync)
                            then mine := true
                          done ;
                          if !mine then incr found ) ;
                      Time.sleep Mono (Span.ms 1) )
                done )
          done ) ;
      (!found, !stolen) )

let thief_uses_its_state () =
  let stolen_any = ref false in
  for seed = 1 to 50 do
    let found, stolen = entry_on_domain seed in
    equal ~msg:(Printf.sprintf "seed %d" seed) int 40 found ;
    if stolen > 0 then stolen_any := true
  done ;
  is_true !stolen_any

(* A seed gives the same order of events when it runs again. *)
let trace seed =
  let log = ref [] in
  Sim.run ~seed ~domains:4 (fun _ ->
      Fiber.scope (fun s ->
          for i = 1 to 6 do
            Fiber.fork s (fun () ->
                for j = 1 to 4 do
                  log :=
                    ( i
                    , j
                    , dom ()
                    , Int64.to_int (Clock.to_ns Mono (Time.now Mono)) )
                    :: !log ;
                  ignore
                    (sync (choose [always (); Time.after Mono (Span.ms j)])) ;
                  Fiber.yield ()
                done )
          done ) ) ;
  !log

let replay () =
  let first = List.init 20 (fun s -> trace (s + 1)) in
  let again = List.init 20 (fun s -> trace (s + 1)) in
  is_true (first = again) ;
  greater int ~than:1 (List.length (List.sort_uniq compare first))

(* The ids of the fibers of a run. Each of 6 fibers forks 3 fibers on the domain
   that runs it, so the order of these forks comes from the seed. *)
let id_trace seed =
  let log = ref [] in
  Sim.run ~seed ~domains:4 (fun _ ->
      Fiber.scope (fun s ->
          for i = 1 to 6 do
            Fiber.fork s (fun () ->
                let parent = Fiber.Id.to_int (Fiber.self ()) in
                Fiber.yield () ;
                Fiber.scope (fun s ->
                    for j = 1 to 3 do
                      Fiber.fork s (fun () ->
                          let id = Fiber.Id.to_int (Fiber.self ()) in
                          log := (i, j, parent, id, dom ()) :: !log )
                    done ) )
          done ) ) ;
  !log

(* A fiber keeps its id on each domain that runs it. It gives [true] if a fiber
   moved. *)
let id_moves seed =
  let moved = ref false in
  Sim.run ~seed ~domains:4 (fun _ ->
      Fiber.scope (fun s ->
          for _ = 1 to 20 do
            Fiber.fork s (fun () ->
                let id = Fiber.self () and d = dom () in
                for _ = 1 to 20 do
                  Fiber.yield () ;
                  is_true (Fiber.Id.equal id (Fiber.self ())) ;
                  if dom () <> d then moved := true
                done )
          done ) ) ;
  !moved

(* A seed gives the same ids when it runs again, also after other runs. The ids
   of a run are 0 (main) to 24, and the seed sets their order. *)
let id_replay () =
  let first = List.init 20 (fun s -> id_trace (s + 1)) in
  let again = List.init 20 (fun s -> id_trace (s + 1)) in
  is_true (first = again) ;
  List.iter
    (fun log ->
      let ids = List.sort compare (List.map (fun (_, _, _, id, _) -> id) log) in
      equal (list int) (List.init 18 (fun k -> k + 7)) ids ;
      is_true (List.for_all (fun (i, _, parent, _, _) -> parent = i) log) )
    first ;
  greater int ~than:1
    (List.length
       (List.sort_uniq compare
          (List.map (List.map (fun (i, j, _, id, _) -> (i, j, id))) first) ) )

(* The protocol of with_nack (design section 2.4): a client sends a request with
   its nack, and it waits for the reply or a timeout. The server waits to send
   the reply or for the nack. Each request that timed out gives the server its
   nack. *)
let nack_server seed =
  Sim.run ~seed ~domains:4 (fun _ ->
      let requests = Chan.create () in
      let abandoned = ref 0 and timeouts = ref 0 in
      Fiber.scope (fun s ->
          Fiber.fork_daemon s (fun () ->
              while true do
                let reply, nack, delay = sync (Chan.recv requests) in
                Fiber.fork s (fun () ->
                    Time.sleep Mono (Span.ms delay) ;
                    sync
                      (choose
                         [ Chan.send reply ()
                         ; wrap nack (fun () -> incr abandoned) ] ) )
              done ) ;
          for i = 1 to 10 do
            Fiber.fork s (fun () ->
                let reply = Chan.create () in
                let got =
                  sync
                    (choose
                       [ with_nack (fun nack ->
                             guard (fun () ->
                                 (* The delay of the server: half of the
                                    requests time out. *)
                                 sync
                                   (Chan.send requests
                                      (reply, nack, 2 * (i mod 2) * 5) ) ;
                                 wrap (Chan.recv reply) (fun () -> true) ) )
                       ; wrap (Time.after Mono (Span.ms 5)) (fun () -> false) ] )
                in
                if not got then incr timeouts )
          done ) ;
      equal int 5 !timeouts ;
      equal int !timeouts !abandoned )

(* The counters of the measurements (open questions 3, 4 and 8) count. *)
let counters_of seed =
  Sim.run ~seed ~domains:2 (fun sim ->
      (* Domain 1 is idle when the forks come. *)
      Time.sleep Mono (Span.ms 1) ;
      let a = Sim.addr sim "server:2" in
      let l = Net.listen a in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              let c, _ = Net.accept l in
              Net.write c "x" ; Net.close c ) ;
          for _ = 1 to 20 do
            Fiber.fork s (fun () ->
                for _ = 1 to 200 do
                  Fiber.yield ()
                done )
          done ;
          let c = Net.connect a in
          ignore (Net.read c (Bytes.create 1) 0 1) ;
          Net.close c ) ;
      let sum = Helpers.sum_stats sim in
      ( sum (fun s -> s.S.edges)
      , sum (fun s -> s.S.busy_polls)
      , sum (fun s -> s.S.wakes)
      , sum (fun s -> s.S.idle_edges) ) )

(* Over ten seeds, a domain is idle at some push. *)
let counters () =
  let e, b, w, i =
    List.fold_left
      (fun (e, b, w, i) seed ->
        let e', b', w', i' = counters_of seed in
        (e + e', b + b', w + w', i + i') )
      (0, 0, 0, 0) (List.init 10 succ)
  in
  greater int ~than:0 e ;
  greater int ~than:0 b ;
  greater int ~than:0 w ;
  at_most int ~than:e i ;
  at_least int ~than:0 i

let tests =
  [ group "many domains"
      [ test "a stolen fiber uses the state of the thief" thief_uses_its_state
      ; test "a seed gives the same order of events" replay
      ; test "a seed gives the same fiber ids on 4 domains" id_replay
      ; test "a fiber keeps its id on each domain, with 50 seeds" (fun () ->
            let moves = List.init 50 (fun s -> id_moves (s + 1)) in
            is_true (List.mem true moves) )
      ; test "the counters of the measurements count" counters
      ; test "the server gets the nack of each client that left" (fun () ->
            Helpers.seeds 200 nack_server )
      ; slow "echo with 4 domains and 10,000 seeds" (fun () ->
            Helpers.seeds 10_000 (fun seed ->
                equal
                  ~msg:(Printf.sprintf "seed %d" seed)
                  string "hello"
                  (Test_net.echo_once ~domains:4 seed) ) )
      ; slow "408 with 4 domains and 10,000 seeds" (fun () ->
            Helpers.seeds 10_000 (fun seed ->
                equal
                  ~msg:(Printf.sprintf "seed %d" seed)
                  (pair string int) ("408", 10_000)
                  (Test_net.t408 ~domains:4 seed) ) )
      ; slow "chained timers with 4 domains and 10,000 seeds" (fun () ->
            Helpers.seeds 10_000 chained_timers )
      ; slow "work stealing with 4 domains and 10,000 seeds" (fun () ->
            Helpers.seeds 10_000 work_stealing ) ] ]
