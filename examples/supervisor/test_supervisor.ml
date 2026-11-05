(* The supervisor in the simulator, on 2 virtual domains, for 30 seeds. The
   children are fake programs of the simulator. *)

open Weft
module Sim = Weft_sim

(* Run [f] with a log that keeps each event and its virtual time in ms. *)
let with_log f =
  let t0 = Time.now Mono in
  let events = Atomic.make [] in
  let log e =
    let ms =
      Int64.to_int
        (Int64.div (Span.to_ns (Time.diff Mono (Time.now Mono) t0)) 1_000_000L)
    in
    let e =
      match (e : Supervisor.event) with
      | Started _ ->
          "started"
      | Output l ->
          "| " ^ l
      | Exited (Exited n) ->
          Printf.sprintf "exited %d" n
      | Exited (Signaled _) ->
          "signaled"
      | Restart_in b ->
          Printf.sprintf "restart in %Ld" (Int64.div (Span.to_ns b) 1_000_000L)
      | Stopped ->
          "stopped"
    in
    let rec push () =
      let l = Atomic.get events in
      if not (Atomic.compare_and_set events l ((ms, e) :: l)) then push ()
    in
    push ()
  in
  f log ;
  (* The output of a child and its exit can come in any order. Thus the check
     sorts the events of the same time. *)
  List.sort compare (Atomic.get events)

let () =
  for seed = 1 to 30 do
    Sim.run ~seed ~domains:2 ~signals:[SIGTERM] (fun sim ->
        (* A program that fails two times after 50 ms, then runs until a signal.
           The waits are 100 ms, then 200 ms. SIGTERM at 1000 ms stops the child
           and the supervisor. *)
        let runs = ref 0 in
        Sim.add_program sim "flaky" (fun _ ->
            incr runs ;
            if !runs <= 2 then
              Sim.program ~runs:(Span.ms 50) ~code:1 ~output:"crash\n" ()
            else Sim.program ~output:"up\n" () ) ;
        let events =
          with_log (fun log ->
              Fiber.scope (fun s ->
                  Fiber.fork s (fun () ->
                      Supervisor.supervise
                        ~stop:(wrap (Signal.received SIGTERM) ignore)
                        ~log "flaky" [] ) ;
                  Time.sleep Mono (Span.s 1) ;
                  Sim.signal sim SIGTERM ) )
        in
        assert (
          events
          = List.sort compare
              [ (0, "started")
              ; (0, "| crash")
              ; (50, "exited 1")
              ; (50, "restart in 100")
              ; (150, "started")
              ; (150, "| crash")
              ; (200, "exited 1")
              ; (200, "restart in 200")
              ; (400, "started")
              ; (400, "| up")
              ; (1000, "signaled")
              ; (1000, "stopped") ] ) ;
        (* A program that fails after 6 s: each run is longer than [healthy],
           thus the wait stays at 100 ms. The third run exits with 0, and the
           supervisor ends. *)
        let runs = ref 0 in
        Sim.add_program sim "slow" (fun _ ->
            incr runs ;
            Sim.program ~runs:(Span.s 6) ~code:(if !runs < 3 then 2 else 0) () ) ;
        let events =
          with_log (fun log -> Supervisor.supervise ~log "slow" [])
        in
        assert (
          events
          = [ (0, "started")
            ; (6000, "exited 2")
            ; (6000, "restart in 100")
            ; (6100, "started")
            ; (12100, "exited 2")
            ; (12100, "restart in 100")
            ; (12200, "started")
            ; (18200, "exited 0") ] ) ;
        (* A timeout around the supervisor: the cancel stops the child too. *)
        Sim.add_program sim "forever" (fun _ -> Sim.program ()) ;
        let events =
          with_log (fun log ->
              match
                Fiber.with_timeout Mono (Span.ms 500) (fun () ->
                    Supervisor.supervise ~log "forever" [] )
              with
              | () ->
                  assert false
              | exception Fiber.Timeout ->
                  () )
        in
        assert (events = [(0, "started"); (500, "signaled")]) ;
        (* No fd of the children stays open. The fd of a child closes when each
           domain took its close notice, thus the check waits 1 ms. *)
        Time.sleep Mono (Span.ms 1) ;
        assert (Sim.open_fds sim = 0) )
  done
