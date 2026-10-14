(* The tests of the details of the scheduler that the mutation run found (design
   sections 2.4 to 2.9). *)

open Windtrap
open Weft
module Sim = Weft_sim
module S = Private.Sched

(* Switch points with no switch of fiber, as a fiber that computes. *)
let spin n =
  for _ = 1 to n do
    ignore (Time.now Mono)
  done

(* [spin_until flag] spins until [flag] is set, or for 10,000 switch points. It
   gives [true] if the flag came. Only another domain can set the flag. *)
let spin_until flag =
  let n = ref 0 in
  while (not (Atomic.get flag)) && !n < 10_000 do
    incr n ;
    ignore (Time.now Mono)
  done ;
  Atomic.get flag

let seeds n f =
  for seed = 1 to n do
    f seed
  done

let log () =
  let l = ref [] in
  ((fun s -> l := s :: !l), fun () -> List.rev !l)

(* Where a woken fiber goes (design section 2.5). *)

(* One fiber that a fiber wakes goes to the next slot: it runs before the fiber
   that is at the head of the local queue. *)
let woken_goes_next () =
  let add, get = log () in
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
  let add, get = log () in
  Sim.run (fun _ ->
      let c = Chan.create () in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              for i = 1 to 6 do
                sync (Chan.send c i)
              done ) ;
          Fiber.fork s (fun () ->
              for _ = 1 to 6 do
                add (string_of_int (sync (Chan.recv c)))
              done ) ;
          Fiber.fork s (fun () -> add "other") ) ) ;
  equal (list string) ["1"; "2"; "3"; "other"; "4"; "5"; "6"] (get ())

(* Domain 0 takes a pinned fiber that yields at most 3 times in sequence. Then
   it takes the local queue. *)
let pinned_limit () =
  let add, get = log () in
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

(* The counters and the beats of a domain. *)

(* Each fiber that a commit resumes counts in [resumes]: the simulator uses it
   to find progress (Livelock). *)
let resumes_count () =
  Sim.run (fun sim ->
      let c = Chan.create () in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              for i = 1 to 10 do
                sync (Chan.send c i)
              done ) ;
          for _ = 1 to 10 do
            ignore (sync (Chan.recv c))
          done ) ;
      at_least int ~than:10 (Sim.sched sim).S.doms.(0).S.stats.S.resumes )

(* The beat of a domain is one more at the start and at the end of each run of a
   fiber. Thus it is odd while a fiber runs. *)
let beats () =
  Sim.run (fun sim ->
      let beat () = (Sim.sched sim).S.beats.{0} in
      let b1 = beat () in
      Fiber.yield () ;
      let b2 = beat () in
      greater int ~than:0 b1 ;
      equal int 1 (b1 land 1) ;
      equal int (b1 + 2) b2 )

(* The completions of the jobs. *)

(* A completion that a fiber waited for leaves no record. *)
let no_external_left () =
  Sim.run (fun sim ->
      for _ = 1 to 10 do
        ignore (File.stat "/")
      done ;
      equal int 0 (Hashtbl.length (Sim.sched sim).S.externals) )

(* A cancel from another domain can come between the start of a job and the
   start of its wait. Then the fiber never waits, and it detaches. Its
   completion leaves no record. *)
let detach_before_wait seed =
  Sim.run ~seed ~domains:4 (fun sim ->
      let inner = ref None in
      Fiber.scope (fun outer ->
          Fiber.fork outer (fun () ->
              Fiber.scope (fun s ->
                  inner := Some s ;
                  Fiber.fork s (fun () ->
                      while true do
                        ignore (File.stat "/")
                      done ) ) ) ;
          while Option.is_none !inner do
            Fiber.yield ()
          done ;
          spin (seed mod 40) ;
          Fiber.cancel (Option.get !inner) Exit ) ;
      Time.sleep Mono (Span.ms 1) ;
      equal
        ~msg:(Printf.sprintf "seed %d" seed)
        int 0
        (Hashtbl.length (Sim.sched sim).S.externals) )

(* A close wakes a fiber that waits to write. *)
let close_wakes_writer () =
  Sim.run (fun sim ->
      let a, b = Sim.socket_pair sim in
      let got = ref None in
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              match Net.write a (String.make (1 lsl 24) 'x') with
              | () ->
                  ()
              | exception Net.Error (e, _) ->
                  got := Some e ) ;
          Time.sleep Mono (Span.ms 1) ;
          Net.close a ) ;
      Net.close b ;
      is_true (!got = Some Net.Bad_fd) )

(* The scopes. *)

(* A fiber that ended is not a member of its scope any more. *)
let ended_fibers_leave () =
  Sim.run (fun _ ->
      Fiber.scope (fun s ->
          for _ = 1 to 10 do
            Fiber.fork s ignore
          done ;
          Fiber.yield () ;
          (* Only the fiber of the body stays. *)
          equal int 1 (Hashtbl.length (Private.scope_ctx s).S.members) ) )

(* The body of a scope that ended is not a member of the scope any more. *)
let body_leaves () =
  Sim.run (fun _ ->
      let sc = ref None in
      Fiber.scope (fun s -> sc := Some s) ;
      equal int 0
        (Hashtbl.length (Private.scope_ctx (Option.get !sc)).S.members) )

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

(* The fills from outside a fiber. *)

(* A domain that has no fiber takes the injection queue at once: it does not
   loop to the 61st take. *)
let inject_at_once () =
  Sim.run ~outside_fills:true ~livelock:50 (fun _ ->
      let iv = Ivar.create () in
      is_true (Ivar.fill_from_outside iv 3) ;
      equal int 3 (sync (Ivar.read iv)) )

(* The wakes (design section 2.5, Idle and wake). On 2 domains, a fiber makes
   another fiber ready, then computes. The other domain sleeps, and it must wake
   and take the fiber before the computation ends. *)

(* Main sleeps, so that the other domain sleeps too. Then main makes a fiber
   ready with [push], and computes. *)
let wakes_other ~seed ?signals push =
  Sim.run ~seed ~domains:2 ?signals (fun sim ->
      let ran = Atomic.make false in
      Fiber.scope (fun s ->
          let go = push sim s ran in
          Time.sleep Mono (Span.ms 1) ;
          spin 1000 ;
          go () ;
          equal ~msg:(Printf.sprintf "seed %d" seed) bool true (spin_until ran) ) )

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
              bool true (spin_until ran)
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

let tests =
  [ group "scheduler details"
      [ test "a woken fiber goes to the next slot" woken_goes_next
      ; test "the next slot runs at most 3 times in sequence" next_limit
      ; test "a pinned fiber runs at most 3 times in sequence" pinned_limit
      ; test "the resumes count" resumes_count
      ; test "the beat is odd while a fiber runs" beats
      ; test "a completion leaves no record" no_external_left
      ; test "a detach before the wait leaves no record, with 100 seeds"
          (fun () -> seeds 100 detach_before_wait )
      ; test "a close wakes a writer" close_wakes_writer
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
      ; test "a cancel of the root scope reaches main" main_in_root
      ; test "an idle domain takes a fill from outside at once" inject_at_once
      ; test "a fork wakes an idle domain, with 50 seeds" (fun () ->
            seeds 50 fork_wakes )
      ; test "a send wakes an idle domain, with 50 seeds" (fun () ->
            seeds 50 send_wakes )
      ; test "a fill wakes an idle domain, with 50 seeds" (fun () ->
            seeds 50 fill_wakes )
      ; test "an unwatch wakes an idle domain, with 50 seeds" (fun () ->
            seeds 50 unwatch_wakes )
      ; test "a close wakes an idle domain, with 50 seeds" (fun () ->
            seeds 50 close_wakes )
      ; test "a domain that looks for work finds a fork, with 20 seeds"
          (fun () -> seeds 20 forks_in_sequence )
      ; test "the last close notices close the fds, with 200 seeds" (fun () ->
            seeds 200 last_notices ) ] ]
