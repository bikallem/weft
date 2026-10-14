(* The tests of the idle domains, of their wakes and of the close notices on the
   simulator (design section 2.5, Idle and wake, and section 2.7, An fd in many
   pollers). *)

open Windtrap
open Weft
module Sim = Weft_sim
module S = Private.Sched

(* A domain that has no fiber takes the injection queue at once: it does not
   loop to the 61st take. *)
let inject_at_once () =
  Sim.run ~outside_fills:true ~livelock:50 (fun _ ->
      let iv = Ivar.create () in
      is_true (Ivar.fill_from_outside iv 3) ;
      equal int 3 (sync (Ivar.read iv)) )

(* On 2 domains, a fiber makes another fiber ready, then computes. The other
   domain sleeps, and it must wake and take the fiber before the computation
   ends. *)

(* Main sleeps, so that the other domain sleeps too. Then main makes a fiber
   ready with [push], and computes. *)
let wakes_other ~seed ?signals push =
  Sim.run ~seed ~domains:2 ?signals (fun sim ->
      let ran = Atomic.make false in
      Fiber.scope (fun s ->
          let go = push sim s ran in
          Time.sleep Mono (Span.ms 1) ;
          Helpers.spin 1000 ;
          go () ;
          equal
            ~msg:(Printf.sprintf "seed %d" seed)
            bool true (Helpers.spin_until ran) ) )

(* A fork. *)
let fork_wakes seed =
  wakes_other ~seed (fun _ s ran () ->
      Fiber.fork s (fun () -> Atomic.set ran true) )

(* A send that resumes the receiver, then the next step of the fiber. *)
let send_wakes seed =
  wakes_other ~seed (fun _ s ran ->
      let c = Chan.create () in
      Fiber.fork s (fun () ->
          sync (Chan.recv c) ;
          Atomic.set ran true ) ;
      fun () ->
        sync (Chan.send c ()) ;
        ignore (Fiber.self ()) )

(* A fill of an Ivar with one waiter. *)
let fill_wakes seed =
  wakes_other ~seed (fun _ s ran ->
      let iv = Ivar.create () in
      Fiber.fork s (fun () ->
          sync (Ivar.read iv) ;
          Atomic.set ran true ) ;
      fun () -> Ivar.fill iv () )

(* An unwatch: the waiter raises. *)
let unwatch_wakes seed =
  wakes_other ~seed ~signals:[SIGUSR1] (fun _ s ran ->
      Fiber.fork s (fun () ->
          ( try ignore (sync (Signal.received SIGUSR1))
            with Invalid_argument _ -> () ) ;
          Atomic.set ran true ) ;
      fun () -> Signal.unwatch SIGUSR1 )

(* A close: the reader gets EBADF. If main runs on the domain of the
   registration, the close resumes the reader on this domain. If not, the close
   wakes the domain of the registration with a close notice. *)
let close_wakes seed =
  wakes_other ~seed (fun sim s ran ->
      let a, b = Sim.socket_pair sim in
      Fiber.fork s (fun () ->
          ( try ignore (Net.read a (Bytes.create 1) 0 1)
            with Net.Error (Net.Bad_fd, _) -> () ) ;
          Net.close b ; Atomic.set ran true ) ;
      fun () -> Net.close a )

(* Many forks in sequence: the other domain often looks for work at the fork. It
   checks the queues again after it adds itself to the idle set, and it finds
   the fiber. *)
let forks_in_sequence seed =
  Sim.run ~seed ~domains:2 (fun _ ->
      Time.sleep Mono (Span.ms 1) ;
      Fiber.scope (fun s ->
          for i = 1 to 100 do
            let ran = Atomic.make false in
            Fiber.fork s (fun () -> Atomic.set ran true) ;
            equal
              ~msg:(Printf.sprintf "seed %d, fork %d" seed i)
              bool true (Helpers.spin_until ran)
          done ) )

(* The end of the run. Fibers on the two domains register the fds. Then each
   close of main sends a close notice to the other domain, and that domain can
   see the end of main before its next poll. It takes the notices when it leaves
   its loop, so the fds close. *)
let last_notices seed =
  let sim_of = ref None in
  Sim.run ~seed ~domains:2 (fun sim ->
      sim_of := Some sim ;
      let pairs = List.init 8 (fun _ -> Sim.socket_pair sim) in
      Fiber.scope (fun s ->
          for _ = 1 to 4 do
            Fiber.fork s (fun () ->
                Fiber.yield () ;
                (* One scan registers each fd on the domain of the fiber. *)
                sync
                  (choose
                     ( Time.after Mono (Span.ms 1)
                     :: List.map (fun (a, _) -> Net.readable a) pairs ) ) )
          done ) ;
      List.iter (fun (_, b) -> Net.close b) pairs ;
      List.iter (fun (a, _) -> Net.close a) pairs ) ;
  equal
    ~msg:(Printf.sprintf "seed %d" seed)
    int 0
    (Sim.open_fds (Option.get !sim_of))

(* A Wait resumes a fiber and brings a close notice. The domain takes the notice
   before the fiber runs, so the fiber sees the fd closed. A pinned fiber
   registers [xr] on domain 0 and waits on [pr]. A fiber that can run on domain
   1 closes [xr], then writes to [pr]. It gives 1 if the fd of [xr] was open
   when the reader woke. *)
let notice_before_resume seed =
  Sim.run ~seed ~domains:2 (fun sim ->
      let xr, xw = Pipe.create () in
      let pr, pw = Pipe.create () in
      let registered = Atomic.make false in
      let late = ref 0 in
      Fiber.scope (fun s ->
          Fiber.fork ~pinned:true s (fun () ->
              ignore (Pipe.read xr (Bytes.create 1) 0 1) ;
              Atomic.set registered true ;
              let n0 = Sim.open_fds sim in
              ignore (Pipe.read pr (Bytes.create 1) 0 1) ;
              if Sim.open_fds sim <> n0 - 1 then late := 1 ) ;
          Fiber.fork s (fun () ->
              Time.sleep Mono (Span.ms 1) ;
              Pipe.write xw "a" ;
              while not (Atomic.get registered) do
                Time.sleep Mono (Span.ms 1)
              done ;
              (* The time moves only when the reader waits on [pr]. *)
              Time.sleep Mono (Span.ms 1) ;
              Pipe.close xr ;
              Pipe.write pw "b" ) ) ;
      Pipe.close xw ;
      Pipe.close pr ;
      Pipe.close pw ;
      !late )

(* A close notice in the mailbox before a Wait: the domain takes it before the
   Wait, so a later edge of the closed fd does not count in [edges]. With no
   poll of a busy domain, a pinned fiber registers [xr] on domain 0 and yields
   until a stop. A fiber on domain 1 writes to [xr] and closes it. It gives the
   pair: the fiber ran on domain 1, and the count of edges on domain 0 after the
   first write. *)
let stale_edge seed =
  let k = !S.k_poll in
  S.k_poll := max_int ;
  Fun.protect
    ~finally:(fun () -> S.k_poll := k)
    (fun () ->
      Sim.run ~seed ~domains:2 (fun sim ->
          let xr, xw = Pipe.create () in
          let registered = Atomic.make false and stop = Atomic.make false in
          let before = ref 0 and on1 = ref false in
          let d0 () = (Sim.sched sim).S.doms.(0).S.stats.S.edges in
          Fiber.scope (fun s ->
              Fiber.fork ~pinned:true s (fun () ->
                  ( try ignore (Pipe.read xr (Bytes.create 1) 0 1)
                    with Net.Error _ -> () ) ;
                  Atomic.set registered true ;
                  while not (Atomic.get stop) do
                    Fiber.yield ()
                  done ) ;
              Fiber.fork s (fun () ->
                  Time.sleep Mono (Span.ms 1) ;
                  Pipe.write xw "a" ;
                  let dom () = S.prog "test" S.Dom_index in
                  while (not (Atomic.get registered)) && dom () = 1 do
                    Fiber.yield ()
                  done ;
                  on1 := Atomic.get registered && dom () = 1 ;
                  before := d0 () ;
                  Pipe.write xw "b" ;
                  Pipe.close xr ;
                  Atomic.set stop true ) ) ;
          (* Each domain waits before the count. *)
          Time.sleep Mono (Span.ms 1) ;
          Pipe.close xw ;
          (!on1, d0 () - !before) ) )

let tests =
  [ group "idle and wake"
      [ test "an idle domain takes a fill from outside at once" inject_at_once
      ; test "a fork wakes an idle domain, with 50 seeds" (fun () ->
            Helpers.seeds 50 fork_wakes )
      ; test "a send wakes an idle domain, with 50 seeds" (fun () ->
            Helpers.seeds 50 send_wakes )
      ; test "a fill wakes an idle domain, with 50 seeds" (fun () ->
            Helpers.seeds 50 fill_wakes )
      ; test "an unwatch wakes an idle domain, with 50 seeds" (fun () ->
            Helpers.seeds 50 unwatch_wakes )
      ; test "a close wakes an idle domain, with 50 seeds" (fun () ->
            Helpers.seeds 50 close_wakes )
      ; test "a domain that looks for work finds a fork, with 20 seeds"
          (fun () -> Helpers.seeds 20 forks_in_sequence ) ]
  ; group "close notices"
      [ test "the last close notices close the fds, with 200 seeds" (fun () ->
            Helpers.seeds 200 last_notices )
      ; test "a close notice goes before the fiber of the same Wait, 200 seeds"
          (fun () ->
            let late = ref 0 in
            Helpers.seeds 200 (fun seed ->
                late := !late + notice_before_resume seed ) ;
            equal int 0 !late )
      ; test "an edge after a close notice does not count, with 200 seeds"
          (fun () ->
            let hits = ref 0 and counted = ref 0 in
            Helpers.seeds 200 (fun seed ->
                let on1, d = stale_edge seed in
                if on1 then (
                  incr hits ;
                  counted := !counted + d ) ) ;
            greater int ~than:0 !hits ;
            equal int 0 !counted ) ] ]
