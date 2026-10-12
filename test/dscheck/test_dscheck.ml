(* The dscheck tests (design section 2.14, M1). Each test runs each order of the
   atomic operations of its threads. *)

module C = Sched.C

let failures = ref 0

(* The traces of one test, and the traces that a spin limit dropped. *)
let traces = ref 0

let pruned_traces = ref 0

let pruned = ref false

(* Run a test: [setup] spawns the threads and gives the check of the end. *)
let trace setup =
  Atomic.trace (fun () ->
      Domain.spins := 0 ;
      pruned := false ;
      let check = setup () in
      Atomic.final (fun () ->
          incr traces ;
          if !pruned then incr pruned_traces else Atomic.check check ) )

(* The liveness of a test: random orders with a high limit of spins with no
   write. A correct spinner waits for a few steps of the other thread, so
   100,000 calls of cpu_relax with no write tell a deadlock or a livelock. *)
let live setup =
  Domain.limit := 100_000 ;
  Fun.protect
    ~finally:(fun () -> Domain.limit := 8)
    (fun () ->
      Atomic.trace ~impl:(`Random 200) (fun () ->
          Domain.spins := 0 ;
          pruned := false ;
          let check = setup () in
          Atomic.final (fun () ->
              incr traces ;
              if !pruned then failwith "a deadlock or a livelock" ;
              Atomic.check check ) ) )

(* A thread of a test. A spin limit drops its trace. *)
let spawn f =
  Atomic.spawn (fun () -> try f () with Domain.Pruned -> pruned := true)

(* [test_dscheck.exe text] runs only the tests whose name has [text]. *)
let filter = if Array.length Sys.argv > 1 then Some Sys.argv.(1) else None

let selected name =
  match filter with
  | None ->
      true
  | Some f ->
      let n = String.length f in
      let rec at i =
        i + n <= String.length name && (String.sub name i n = f || at (i + 1))
      in
      at 0

let rec case name f = if selected name then case_ name f

and case_ name f =
  traces := 0 ;
  pruned_traces := 0 ;
  match f () with
  | () when !traces = !pruned_traces ->
      incr failures ;
      Printf.printf "  [FAIL] %s: each trace was pruned\n%!" name
  | () ->
      Printf.printf "  [OK]   %s (%d traces, %d pruned)\n%!" name !traces
        !pruned_traces
  | exception e ->
      incr failures ;
      Printf.printf "  [FAIL] %s: %s\n%!" name (Printexc.to_string e)

(* The commit protocol (design section 2.4, spec/tla/Commit.tla). A sync has a
   resume count. A fiber that ends its scan with [`Done] continues: that is one
   resume. A partner that commits a parked sync resumes it. At the end, each
   sync with Done has exactly one resume, and a parked sync none. *)

type fiber = {sync: C.t; resumes: int Atomic.t}

let fiber id = {sync= C.make id; resumes= Atomic.make 0}

let resume f = Atomic.incr f.resumes

(* The end of the scan of the fiber [f]. *)
let park f = match C.park f.sync with `Done _ -> resume f | `Parked -> ()

(* A one-word partner, for example a cancel or a timer. *)
let notify f i = match C.notify f.sync i with C.Resume -> resume f | _ -> ()

(* A claim by [mine], in its scan, of the offer of [partner]. *)
let claim mine partner =
  match C.claim mine.sync 0 partner.sync 0 ~on_commit:ignore with
  | C.Committed {resume_partner} ->
      if resume_partner then resume partner
  | C.Stale | C.Self_done ->
      ()

let one_resume fibers () =
  List.for_all
    (fun f ->
      match C.state f.sync with
      | C.Done _ ->
          Atomic.get f.resumes = 1
      | C.Parked ->
          Atomic.get f.resumes = 0
      | C.Waiting | C.Claimed ->
          false )
    fibers

(* A partner commits a sync while that sync is in its scan. *)
let commit_during_scan () =
  trace (fun () ->
      let x = fiber 0 and y = fiber 1 in
      spawn (fun () -> claim x y ; park x) ;
      spawn (fun () -> park y) ;
      one_resume [x; y] )

(* A cancel while a claimer holds the word, with the sync in its scan or
   parked. *)
let notify_while_claimed () =
  trace (fun () ->
      let x = fiber 0 and y = fiber 1 in
      spawn (fun () -> claim x y ; park x) ;
      spawn (fun () -> park y) ;
      spawn (fun () -> notify y 1) ;
      one_resume [x; y] )

(* Two partners and one parked sync: a claimer and a cancel. *)
let two_partners () =
  trace (fun () ->
      let x = fiber 0 and p = fiber 1 in
      ignore (C.park p.sync) ;
      spawn (fun () -> claim x p ; park x) ;
      spawn (fun () -> notify p 1) ;
      one_resume [x; p] )

(* Two fibers that sync on each other: each claims the other in its scan. The id
   order prevents a deadlock. *)
let cross_setup () =
  let x = fiber 0 and y = fiber 1 in
  spawn (fun () -> claim x y ; park x) ;
  spawn (fun () -> claim y x ; park y) ;
  one_resume [x; y]

let cross () = trace cross_setup

(* The objects of the scheduler for a test. *)

module S = Sched

(* A world for the threads of a test: it records the effects. *)
type world =
  { closes: int Atomic.t
  ; wakes: int Atomic.t
  ; registers: int Atomic.t
  ; dels: int Atomic.t
  ; early_close: bool Atomic.t  (** A close(2) before the DEL of a Register. *)
  ; resends: int Atomic.t  (** The signals that went to the process again. *) }

let world () =
  { closes= Atomic.make 0
  ; wakes= Atomic.make 0
  ; registers= Atomic.make 0
  ; dels= Atomic.make 0
  ; early_close= Atomic.make false
  ; resends= Atomic.make 0 }

let in_world w f () =
  Effect.Deep.match_with f ()
    { retc= Fun.id
    ; exnc= raise
    ; effc=
        (fun (type a) (e : a Effect.t) ->
          match e with
          | World.Submit (Op.Close _) ->
              Some
                (fun (k : (a, _) Effect.Deep.continuation) ->
                  Atomic.incr w.closes ;
                  Effect.Deep.continue k (Ok ()) )
          | World.Submit (Op.Register _) ->
              Some
                (fun k ->
                  Atomic.incr w.registers ;
                  Effect.Deep.continue k (Ok ()) )
          | World.Submit (Op.Deregister _) ->
              Some
                (fun k ->
                  Atomic.incr w.dels ;
                  Effect.Deep.continue k (Ok ()) )
          | World.Submit (Op.Resend _) ->
              Some
                (fun k ->
                  Atomic.incr w.resends ;
                  Effect.Deep.continue k (Ok ()) )
          | World.Wake _ ->
              Some (fun k -> Atomic.incr w.wakes ; Effect.Deep.continue k ())
          | World.Random _ ->
              Some (fun k -> Effect.Deep.continue k 0)
          | _ ->
              None ) }

let spawn_in w f = spawn (in_world w f)

(* A continuation for a sync record: the handler of the scheduler gives it. *)
let cont () : (int, S.step) Effect.Deep.continuation =
  match
    S.start_fiber (fun () -> ignore (S.sync_req ~nocancel:true [||])) ()
  with
  | S.Suspended (r, k) -> (
    match r with S.Sync _ -> k | _ -> assert false )
  | S.Finished ->
      assert false

let scope_node ?parent () =
  S.new_node ?parent (S.Scope_node (S.new_scope_state ()))

let new_sync ?(id = 0) ctx =
  let f = S.new_fiber [ctx] in
  {S.word= C.make id; owner= f; sctx= ctx; cancellable= true; k= Some (cont ())}

(* The resumes of a sync: the fibers that went to a queue. *)
let queued (t : S.t) s =
  Array.fold_left
    (fun n d ->
      let r = d.S.ring in
      let c = ref 0 in
      for i = Atomic.get r.Ring.head to Atomic.get r.Ring.tail - 1 do
        match Atomic.get r.Ring.buf.(i land Ring.mask) with
        | Some x when x.S.rfiber == s.S.owner ->
            incr c
        | _ ->
            ()
      done ;
      n + !c )
    0 t.S.doms

(* An fd record that the setup makes, with no use of the global table. *)
let new_fdrec () =
  { S.slot= 0
  ; gen= 1
  ; os= 7
  ; rc= Atomic.make (S.Open 0)
  ; doms= Atomic.make 0
  ; streak= 0
  ; fkind= S.Any }

(* The ring: a push by the owner, and a steal by two thieves. Each item is taken
   exactly one time. *)
let ring_steal () =
  trace (fun () ->
      let owner = Ring.create ()
      and t1 = Ring.create ()
      and t2 = Ring.create () in
      let taken = Atomic.make [] in
      let take x =
        let rec add () =
          let l = Atomic.get taken in
          if not (Atomic.compare_and_set taken l (x :: l)) then add ()
        in
        add ()
      in
      spawn (fun () ->
          ignore (Ring.push owner 1) ;
          ignore (Ring.push owner 2) ;
          ignore (Ring.push owner 3) ;
          match Ring.pop owner with Some x -> take x | None -> () ) ;
      List.iter
        (fun into ->
          spawn (fun () ->
              match Ring.steal ~from:owner ~into with
              | Some x ->
                  take x
              | None ->
                  () ) )
        [t1; t2] ;
      fun () ->
        let rest r =
          let rec all acc =
            match Ring.pop r with Some x -> all (x :: acc) | None -> acc
          in
          all []
        in
        let all = Atomic.get taken @ rest owner @ rest t1 @ rest t2 in
        List.sort compare all = [1; 2; 3] )

(* The idle protocol (Wake.tla): domain 0 goes idle while domain 1 pushes work.
   The sleeper sees the work, or the pusher wakes it. *)
let idle_push () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let slept = Atomic.make false in
      let d0 = t.S.doms.(0) and d1 = t.S.doms.(1) in
      let f = S.new_fiber [scope_node ()] in
      spawn_in w (fun () ->
          S.Idle.add t 0 ;
          if not (S.work_for d0) then Atomic.set slept true ) ;
      spawn_in w (fun () ->
          S.push d1 f (fun () -> S.Finished) ;
          S.flush_wake d1 ) ;
      fun () -> (not (Atomic.get slept)) || Atomic.get w.wakes = 1 )

(* The Go rules (M3, Wake.tla): domain 0 searches, finds nothing, decreases
   [searching], goes idle and checks again, while domain 1 pushes. A push wakes
   nobody while a domain searches. Domain 0 finds the fiber, or it sleeps and
   gets one wake. *)
let search_push () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let found = Atomic.make false and slept = Atomic.make false in
      let d0 = t.S.doms.(0) and d1 = t.S.doms.(1) in
      let f = S.new_fiber [scope_node ()] in
      spawn_in w (fun () ->
          if Option.is_some (S.steal d0) then Atomic.set found true
          else (
            S.Idle.add t 0 ;
            if not (S.work_for d0) then Atomic.set slept true ) ) ;
      spawn_in w (fun () ->
          S.push d1 f (fun () -> S.Finished) ;
          S.flush_wake d1 ) ;
      fun () ->
        Atomic.get found || (not (Atomic.get slept)) || Atomic.get w.wakes = 1 )

(* The injection queue (M3, Wake.tla): domain 0 goes idle while a thread outside
   the domains pushes a fill and wakes an idle domain. Domain 0 sees the request
   in its second check, or it sleeps and gets one wake. *)
let inject_idle () =
  trace (fun () ->
      let t = S.create ~domains:1 () in
      let w = world () in
      let slept = Atomic.make false and outside_wakes = Atomic.make 0 in
      let d0 = t.S.doms.(0) in
      spawn_in w (fun () ->
          S.Idle.add t 0 ;
          if not (S.work_for d0) then Atomic.set slept true ) ;
      spawn (fun () ->
          S.inject t (fun _ -> ()) ;
          match S.Idle.take_any t with
          | Some _ ->
              Atomic.incr outside_wakes
          | None ->
              () ) ;
      fun () -> (not (Atomic.get slept)) || Atomic.get outside_wakes = 1 )

(* The pinned queue (M3, Wake.tla): domain 1 pushes a pinned fiber and wakes
   domain 0 only if domain 0 is in [idle]. At the same time, domain 0 goes idle
   and checks again. Domain 0 sees the fiber, or it sleeps and gets one wake. *)
let pinned_push () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let slept = Atomic.make false in
      let d0 = t.S.doms.(0) and d1 = t.S.doms.(1) in
      let f = S.new_fiber ~pinned:true [scope_node ()] in
      spawn_in w (fun () ->
          S.Idle.add t 0 ;
          if not (S.work_for d0) then Atomic.set slept true ) ;
      spawn_in w (fun () ->
          S.push d1 f (fun () -> S.Finished) ;
          S.flush_wake d1 ) ;
      fun () -> (not (Atomic.get slept)) || Atomic.get w.wakes = 1 )

(* The next slot (M3): the owner puts a fiber and takes it, while a thief takes
   the slot with a CAS. The fiber runs exactly one time. *)
let next_steal () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let d0 = t.S.doms.(0) and d1 = t.S.doms.(1) in
      let f = S.new_fiber [scope_node ()] in
      let runs = Atomic.make 0 in
      let run () = Atomic.incr runs ; S.Finished in
      spawn_in w (fun () ->
          S.push_next d0 f run ;
          match S.take_next d0 with Some r -> ignore (r.S.run ()) | None -> () ) ;
      spawn_in w (fun () ->
          match S.steal d1 with Some r -> ignore (r.S.run ()) | None -> () ) ;
      fun () ->
        (* A fiber that nobody took is still in a queue. *)
        ( Atomic.get runs
        + (if Option.is_some (Atomic.get d0.S.next) then 1 else 0)
        + if Ring.is_empty d0.S.ring && Ring.is_empty d1.S.ring then 0 else 1 )
        = 1 )

(* The atomic register of a channel: a send and a recv on an empty channel at
   the same time. They meet. *)
let chan_meet () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let c = Chan.create () in
      let ctx = scope_node () in
      let x = new_sync ~id:0 ctx and y = new_sync ~id:1 ctx in
      let leaf ev =
        match ev with Event.Base mk -> (mk ()).Event.leaf | _ -> assert false
      in
      let run dom s ev () =
        leaf ev dom s 0 ;
        match C.park s.S.word with `Done _ -> () | `Parked -> ()
      in
      spawn_in w (run t.S.doms.(0) x (Chan.send c 1)) ;
      spawn_in w (run t.S.doms.(1) y (Chan.recv c)) ;
      fun () ->
        (* No lost rendezvous: the two did not both park. *)
        C.state x.S.word <> C.Parked || C.state y.S.word <> C.Parked )

(* The atomic register of an Ivar: a fill during a register. The reader gets one
   resume, or it continues itself. *)
let ivar_fill_register () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let iv = S.Iv.make () in
      let r = new_sync (scope_node ()) in
      let continued = Atomic.make 0 in
      spawn_in w (fun () ->
          S.Iv.register iv 0 r 0 ;
          match C.park r.S.word with
          | `Done _ ->
              Atomic.incr continued
          | `Parked ->
              () ) ;
      spawn_in w (fun () ->
          match S.Iv.fill iv 1 with
          | Some l ->
              List.iter (S.resume t.S.doms.(1)) l
          | None ->
              () ) ;
      fun () ->
        C.state r.S.word <> C.Parked && Atomic.get continued + queued t r = 1 )

(* The self-commit: a ready branch (always, a full Ivar, a set fd flag), which
   commits itself, and a cancel at the same time. One of them wins, and the
   fiber continues one time. *)
let self_and_cancel () =
  trace (fun () ->
      let s = fiber 0 in
      let winners = Atomic.make 0 in
      spawn (fun () ->
          ( match C.self_commit s.sync 0 with
          | Ok () ->
              Atomic.incr winners
          | Error _ ->
              () ) ;
          park s ) ;
      spawn (fun () ->
          match C.notify s.sync 1 with
          | C.Resume ->
              Atomic.incr winners ; resume s
          | C.In_scan ->
              Atomic.incr winners
          | C.Lost _ ->
              () ) ;
      fun () -> one_resume [s] () && Atomic.get winners = 1 )

(* The cancel: a cancel walk while a fiber starts a sync (the Dekker pair). The
   sync ends: it is not parked with the flag set. *)
let cancel_and_sync () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let node = scope_node () in
      let f = S.new_fiber [node] in
      S.add_member_to node f ;
      spawn_in w (fun () ->
          S.sync t.S.doms.(0) f {S.leaves= [||]; nocancel= false} (cont ()) ) ;
      spawn_in w (fun () ->
          List.iter (S.resume t.S.doms.(1)) (S.cancel_node node Exit) ) ;
      fun () ->
        match Atomic.get f.S.current with
        | Some s ->
            C.state s.S.word <> C.Parked
        | None ->
            true )

(* The cancel: a walk while the fiber leaves the scope. The cancel never ends a
   sync that the fiber started in the parent. *)
let cancel_and_leave () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let parent = scope_node () in
      let node = scope_node ~parent () in
      let f = S.new_fiber [node; parent] in
      S.add_member_to node f ;
      spawn_in w (fun () ->
          S.remove_member node f ;
          f.S.ctxs <- [parent] ;
          S.sync t.S.doms.(0) f {S.leaves= [||]; nocancel= false} (cont ()) ) ;
      spawn_in w (fun () ->
          List.iter (S.resume t.S.doms.(1)) (S.cancel_node node Exit) ) ;
      fun () ->
        match Atomic.get f.S.current with
        | Some s ->
            C.state s.S.word = C.Parked
        | None ->
            false )

(* The scope: a fork while the last member ends. The fork fails, or the scope
   stays open. *)
let fork_and_end () =
  trace (fun () ->
      let node = scope_node () in
      let st = S.state_of node in
      let forked = Atomic.make false in
      spawn (fun () ->
          match S.add_member node ~daemon:false with
          | () ->
              Atomic.set forked true
          | exception S.Scope_closed ->
              () ) ;
      spawn (fun () -> ignore (S.member_end node ~daemon:false)) ;
      fun () ->
        let c = Atomic.get st.S.counts in
        let closed = c land S.closed_bit <> 0 in
        if Atomic.get forked then (not closed) && S.all_count c = 1 else closed )

(* The fd record: a submit and a close at the same time. close(2) occurs one
   time, and never before the submit releases its user. *)
let submit_and_close () =
  trace (fun () ->
      let w = world () in
      let fdr = new_fdrec () in
      let bad = Atomic.make false in
      spawn_in w (fun () ->
          if S.Rc.acquire fdr then (
            (* The syscall on the fd: close(2) must not have occurred. *)
            if Atomic.get w.closes <> 0 then Atomic.set bad true ;
            S.Rc.release fdr ) ) ;
      spawn_in w (fun () -> ignore (S.Rc.close fdr)) ;
      fun () -> (not (Atomic.get bad)) && Atomic.get w.closes = 1 )

(* The fd record: a Register and a close at the same time (Fd.tla). The Register
   undoes itself, or the close sends a notice that removes it. In the two cases,
   close(2) comes after the DEL. *)
let register_and_close () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let fdr = new_fdrec () in
      let d0 = t.S.doms.(0) in
      (* The thread of domain 0: a Register in a scan, then the mailbox between
         fibers, two times. Only this thread takes the mailbox of domain 0, and
         never during its own Register (busy[d] of Fd.tla). *)
      spawn_in w (fun () ->
          ignore (S.ensure_reg d0 fdr) ;
          S.take_mail d0 ;
          S.take_mail d0 ) ;
      spawn_in w (fun () ->
          if S.Rc.close fdr then S.close_notices t.S.doms.(1) fdr ) ;
      fun () ->
        (* close(2) never comes before a DEL. Each Register has its DEL and
           close(2) occurs one time, or a notice waits in the mailbox: the
           domain takes it at its next turn. *)
        let pending =
          match Atomic.get d0.S.mailbox with [] -> false | _ -> true
        in
        (not (Atomic.get w.early_close))
        && ( pending
           || Atomic.get w.registers = Atomic.get w.dels
              && Atomic.get w.closes = 1 ) )

(* A one-word partner while a claim of its sync is put back: x (id 1) claims p
   (id 0) first, then its own word, which a cancel made Done. x puts p back to
   Parked. A cancel of p at the same time must not be lost (Commit.tla,
   BugDropOnClaimed). *)
let notify_while_put_back () =
  trace (fun () ->
      let p = fiber 0 and x = fiber 1 in
      ignore (C.park p.sync) ;
      spawn (fun () -> claim x p ; park x) ;
      spawn (fun () -> notify x 1) ;
      spawn (fun () -> notify p 2) ;
      fun () ->
        one_resume [x; p] ()
        && match C.state p.sync with C.Done _ -> true | _ -> false )

(* The scope: a fork_daemon while the last member that is not a daemon ends. The
   daemon is not added, or its node gets the cancel. *)
let daemon_and_end () =
  trace (fun () ->
      let w = world () in
      let node = scope_node () in
      let st = S.state_of node in
      spawn_in w (fun () ->
          match S.start_fiber (fun () -> S.fork_daemon node ignore) () with
          | _ ->
              ()
          | exception S.Scope_closed ->
              () ) ;
      spawn_in w (fun () -> ignore (S.member_end node ~daemon:false)) ;
      fun () ->
        List.for_all
          (fun d -> Option.is_some (Atomic.get d.S.flag))
          (S.by_id st.S.daemons) )

(* The completions of the jobs (design section 2.14, the state of M2). Domain 0
   runs the fiber: its leaf, its park, and then the completion of the job, in
   this order, because the completion goes to the domain that started the job. A
   cancel from domain 1 resumes the fiber there, and the fiber drops its wait.
   The release of the fd runs exactly one time, the fiber continues one time,
   and the table is empty at the end. *)
let job_completion () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let d0 = t.S.doms.(0) and d1 = t.S.doms.(1) in
      let id = 5 and released = Atomic.make 0 and continued = Atomic.make 0 in
      Hashtbl.replace t.S.externals id
        { S.ewait= None
        ; arrived= false
        ; dropped= false
        ; on_done= Some (fun () -> Atomic.incr released) } ;
      d0.S.held <- 1 ;
      let s = new_sync (scope_node ()) in
      spawn_in w (fun () ->
          (* The scan does not run the leaf of a sync that is done. *)
          if not (S.is_done s) then S.external_leaf id d0 s 0 ;
          ( match C.park s.S.word with
          | `Done i ->
              Atomic.incr continued ;
              if i < 0 then S.drop_external t id
          | `Parked ->
              () ) ;
          S.on_ready d0 (World.Ready.External id) ) ;
      spawn_in w (fun () ->
          match C.notify s.S.word (-1) with
          | C.Resume ->
              S.resume d1 s ; S.drop_external t id
          | C.In_scan | C.Lost _ ->
              () ) ;
      fun () ->
        Atomic.get released = 1
        && d0.S.held = 0
        && Atomic.get continued + queued t s = 1
        && Hashtbl.length t.S.externals = 0 )

(* The delivery of a signal on domain 0, a waiter on domain 1, and a cancel of
   the waiter from domain 2. The waiter gets the signal (its cell holds 1), or
   the count stays 1. The waiter continues one time. *)
let signal_delivery () =
  trace (fun () ->
      let t = S.create ~domains:3 () in
      let w = world () in
      let s = new_sync (scope_node ()) and cell = ref 0 in
      let continued = Atomic.make 0 in
      spawn_in w (fun () -> S.deliver t.S.doms.(0) Op.SIGUSR1 1) ;
      spawn_in w (fun () ->
          S.signal_leaf Op.SIGUSR1 cell t.S.doms.(1) s 0 ;
          match C.park s.S.word with
          | `Done _ ->
              Atomic.incr continued
          | `Parked ->
              () ) ;
      spawn_in w (fun () ->
          match C.notify s.S.word (-1) with
          | C.Resume ->
              S.resume t.S.doms.(2) s
          | C.In_scan | C.Lost _ ->
              () ) ;
      fun () ->
        let count = (S.sigstate t Op.SIGUSR1).S.count in
        let got = C.state s.S.word = C.Done 0 in
        Atomic.get continued + queued t s = 1
        && if got then !cell = 1 && count = 0 else count = 1 )

(* The end of a watch (design section 2.7, The end of a watch, and
   spec/tla/Signal.tla): a delivery on domain 0, a waiter on domain 1, and the
   mark of the unwatch on domain 2. The waiter gets the signal, or the signal
   goes to the process again exactly one time: from the delivery after the mark,
   or from the count that the mark takes. The waiter continues one time, with
   the signal or with -1. *)
let signal_unwatch () =
  trace (fun () ->
      let t = S.create ~domains:3 () in
      let w = world () in
      let s = new_sync (scope_node ()) and cell = ref 0 in
      let continued = Atomic.make 0 in
      spawn_in w (fun () -> S.deliver t.S.doms.(0) Op.SIGUSR1 1) ;
      spawn_in w (fun () ->
          S.signal_leaf Op.SIGUSR1 cell t.S.doms.(1) s 0 ;
          match C.park s.S.word with
          | `Done _ ->
              Atomic.incr continued
          | `Parked ->
              () ) ;
      spawn_in w (fun () ->
          let n = S.unwatch_signal t.S.doms.(2) Op.SIGUSR1 in
          if n > 0 then Atomic.incr w.resends ) ;
      fun () ->
        let st = S.sigstate t Op.SIGUSR1 in
        let got = if !cell = 1 then 1 else 0 in
        Atomic.get continued + queued t s = 1
        && C.state s.S.word = C.Done 0
        && (!cell = 1 || !cell = -1)
        && got + Atomic.get w.resends = 1
        && st.S.count = 0 && st.S.swait = [] )

(* The semaphore (design section 2.5, Locks for fiber code, and
   spec/tla/Semaphore.tla). [sem free] has an initial count of 2 and [free] free
   permits: the other permits are with holders. *)
module Sem = Locks.Semaphore

let sem free =
  let t = Sem.create 2 in
  Atomic.set t.Sem.free free ; t

let base_leaf ev =
  match ev with Event.Base mk -> (mk ()).Event.leaf | _ -> assert false

(* 1 if the sync committed to branch [i] (it got the permit). *)
let got s i = if C.state s.S.word = C.Done i then 1 else 0

(* The scan of a sync with one acquire branch on [dom], and its end. *)
let acquire_scan dom t s continued () =
  if not (S.is_done s) then base_leaf (Sem.acquire t) dom s 0 ;
  match C.park s.S.word with `Done _ -> Atomic.incr continued | `Parked -> ()

let release dom t () =
  List.iter (S.resume dom) (Option.value ~default:[] (Sem.give t))

(* The resumes of a sync in a next slot: a partner of a channel resumes it
   there. *)
let in_next (t : S.t) s =
  Array.fold_left
    (fun n d ->
      match Atomic.get d.S.next with
      | Some r when r.S.rfiber == s.S.owner ->
          n + 1
      | _ ->
          n )
    0 t.S.doms

(* An acquire with no free permit while a holder releases on another domain. The
   waiter gets the permit: the release finds its offer, or the scan finds the
   free permit (no lost wakeup). *)
let sem_acquire_release () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let p = sem 0 and x = new_sync (scope_node ()) in
      let continued = Atomic.make 0 in
      spawn_in w (acquire_scan t.S.doms.(0) p x continued) ;
      spawn_in w (release t.S.doms.(1) p) ;
      fun () ->
        got x 0 = 1
        && Atomic.get p.Sem.free = 0
        && Atomic.get continued + queued t x = 1 )

(* An acquire with a free permit while a cancel ends its sync. The sync takes
   the permit only if it commits to the acquire. *)
let sem_acquire_cancel () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let p = sem 1 and x = new_sync (scope_node ()) in
      let continued = Atomic.make 0 in
      spawn_in w (acquire_scan t.S.doms.(0) p x continued) ;
      spawn_in w (fun () ->
          match C.notify x.S.word (-1) with
          | C.Resume ->
              S.resume t.S.doms.(1) x
          | C.In_scan | C.Lost _ ->
              () ) ;
      fun () ->
        got x 0 + Atomic.get p.Sem.free = 1
        && Atomic.get continued + queued t x = 1 )

(* Two parked waiters, a release, and a cancel of the first waiter at the same
   time. The permit goes to the first waiter, or to the second one if the cancel
   came first (FIFO). No permit is lost or doubled. *)
let sem_release_cancel () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let d0 = t.S.doms.(0) and d1 = t.S.doms.(1) in
      let p = sem 0 in
      let x = new_sync ~id:0 (scope_node ())
      and y = new_sync ~id:1 (scope_node ()) in
      List.iter
        (fun s ->
          base_leaf (Sem.acquire p) d0 s 0 ;
          ignore (C.park s.S.word) )
        [x; y] ;
      spawn_in w (release d0 p) ;
      spawn_in w (fun () ->
          match C.notify x.S.word (-1) with
          | C.Resume ->
              S.resume d1 x
          | C.In_scan | C.Lost _ ->
              () ) ;
      fun () ->
        got x 0 + got y 0 + Atomic.get p.Sem.free = 1
        && (got y 0 = 0 || C.state x.S.word = C.Done (-1))
        && queued t x = 1
        && queued t y = got y 0 )

(* A parked sync with two branches: an acquire (branch 0) and a recv on a
   channel (branch 1). A send on domain 1 and a release on domain 0 come at the
   same time. The sync commits to one branch, and the permit stays free if the
   recv won. The release can meet the word as Claimed. *)
let sem_lost_choose () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let d0 = t.S.doms.(0) and d1 = t.S.doms.(1) in
      let p = sem 0 and c = Chan.create () in
      let x = new_sync ~id:0 (scope_node ())
      and y = new_sync ~id:1 (scope_node ()) in
      base_leaf (Sem.acquire p) d0 x 0 ;
      base_leaf (Chan.recv c) d0 x 1 ;
      ignore (C.park x.S.word) ;
      let continued = Atomic.make 0 in
      spawn_in w (release d0 p) ;
      spawn_in w (fun () ->
          base_leaf (Chan.send c 7) d1 y 0 ;
          match C.park y.S.word with
          | `Done _ ->
              Atomic.incr continued
          | `Parked ->
              () ) ;
      fun () ->
        got x 0 + Atomic.get p.Sem.free = 1
        && got x 0 + got y 0 = 1
        && queued t x + in_next t x = 1 )

(* Two releases on two domains and one parked waiter. The waiter gets one
   permit, and the other one is free. *)
let sem_two_releases () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let d0 = t.S.doms.(0) and d1 = t.S.doms.(1) in
      let p = sem 0 and x = new_sync (scope_node ()) in
      base_leaf (Sem.acquire p) d0 x 0 ;
      ignore (C.park x.S.word) ;
      spawn_in w (release d0 p) ;
      spawn_in w (release d1 p) ;
      fun () -> got x 0 = 1 && Atomic.get p.Sem.free = 1 && queued t x = 1 )

(* One sync with two acquire branches of one semaphore, in its scan on domain 0,
   and two releases on domain 1. The sync gets one permit. The second release
   meets the stale offer of the other branch, and it makes the permit free. *)
let sem_two_branches () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let d0 = t.S.doms.(0) and d1 = t.S.doms.(1) in
      let p = sem 0 and x = new_sync (scope_node ()) in
      let continued = Atomic.make 0 in
      spawn_in w (fun () ->
          List.iter
            (fun i -> if not (S.is_done x) then base_leaf (Sem.acquire p) d0 x i)
            [0; 1] ;
          match C.park x.S.word with
          | `Done _ ->
              Atomic.incr continued
          | `Parked ->
              () ) ;
      spawn_in w (fun () -> release d1 p () ; release d1 p ()) ;
      fun () ->
        got x 0 + got x 1 = 1
        && Atomic.get p.Sem.free = 1
        && Atomic.get continued + queued t x = 1
        && Chan.offers p.Sem.waiters = [] )

(* The acquire leaf with a free permit meets its own word as Claimed: the sync
   has an offer on a channel, and a send on domain 1 claims it. The sync takes
   the permit only if it commits to the acquire. *)
let sem_self_commit_claimed () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let d0 = t.S.doms.(0) and d1 = t.S.doms.(1) in
      let p = sem 1 and c = Chan.create () in
      let x = new_sync ~id:0 (scope_node ())
      and y = new_sync ~id:1 (scope_node ()) in
      base_leaf (Chan.recv c) d0 x 1 ;
      let continued = Atomic.make 0 in
      spawn_in w (fun () ->
          if not (S.is_done x) then base_leaf (Sem.acquire p) d0 x 0 ;
          match C.park x.S.word with
          | `Done _ ->
              Atomic.incr continued
          | `Parked ->
              () ) ;
      spawn_in w (fun () ->
          base_leaf (Chan.send c 7) d1 y 0 ;
          ignore (C.park y.S.word) ) ;
      fun () ->
        got x 0 + Atomic.get p.Sem.free = 1
        && got x 0 + got y 0 = 1
        && Atomic.get continued + queued t x + in_next t x = 1 )

(* The variable (design section 2.4, A variable). [var_sync dom seen x got] is
   the sync of [x] on [changed seen] on [dom]: the leaf and the end of the scan.
   [seen] gives the view at the start of the thread. [got] gets the result of
   the event. *)
let var_sync dom seen x got continued () =
  let seen = seen () in
  let leaves, results, _ = Event.flatten (Var.changed seen) in
  got := Some (seen, results.(0)) ;
  leaves.(0) dom x 0 ;
  match C.park x.S.word with `Done _ -> Atomic.incr continued | `Parked -> ()

let var_set dom t v () = List.iter (S.resume dom) (Var.change t v)

(* The view of the event, if the sync committed. *)
let var_result x got =
  match (C.state x.S.word, !got) with
  | C.Done 0, Some (_, r) ->
      Some (r ())
  | _ ->
      None

(* The end of a sync of [x] on [changed seen]. If the sync committed, it
   continued one time, and its view is not [seen] and has a value of [values].
   If not, [x] is parked, and the state of [t] is [seen]. Thus each state that a
   fiber waits on has a set that fills its Ivar. *)
let var_end t v x got continued values =
  match (var_result x got, !got) with
  | Some r, Some (seen, _) ->
      r.Var.st != seen.Var.st
      && List.mem (Var.value r) values
      && Atomic.get continued + queued t x = 1
  | None, Some (seen, _) ->
      C.state x.S.word = C.Parked
      && Atomic.get v == seen.Var.st
      && queued t x = 0
  | _, None ->
      false

(* A sync on a view of [t] while a set of 1 runs on another domain. The register
   finds the Ivar of the view full, or the set fills it and commits the sync (no
   lost wake). *)
let var_set_changed () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let v = Var.create 0 and x = new_sync (scope_node ()) in
      let s0 = Var.view v in
      let got = ref None and continued = Atomic.make 0 in
      spawn_in w (var_sync t.S.doms.(0) (fun () -> s0) x got continued) ;
      spawn_in w (var_set t.S.doms.(1) v 1) ;
      fun () -> var_end t v x got continued [1] && C.state x.S.word = C.Done 0 )

(* Two sets on two domains, and a view and a sync on a third domain. The view
   can be the state of the first set. The fiber waits only while its view is the
   state of [t]. *)
let var_two_sets () =
  trace (fun () ->
      let t = S.create ~domains:3 () in
      let w = world () in
      let v = Var.create 0 and x = new_sync (scope_node ()) in
      let got = ref None and continued = Atomic.make 0 in
      spawn_in w (var_sync t.S.doms.(0) (fun () -> Var.view v) x got continued) ;
      spawn_in w (var_set t.S.doms.(1) v 1) ;
      spawn_in w (var_set t.S.doms.(2) v 2) ;
      fun () -> List.mem (Var.get v) [1; 2] && var_end t v x got continued [1; 2] )

(* A set of 1 and a set of 0 again on one domain, while a fiber syncs on its
   first view. The value is 0 again, but the state is new: the sync commits. *)
let var_set_back () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let v = Var.create 0 and x = new_sync (scope_node ()) in
      let s0 = Var.view v in
      let got = ref None and continued = Atomic.make 0 in
      spawn_in w (var_sync t.S.doms.(0) (fun () -> s0) x got continued) ;
      spawn_in w (fun () ->
          var_set t.S.doms.(1) v 1 () ;
          var_set t.S.doms.(1) v 0 () ) ;
      fun () ->
        Var.get v = 0
        && var_end t v x got continued [0; 1]
        && C.state x.S.word = C.Done 0 )

(* A sync on an old view, with no set during the sync, and a cancel at the same
   time. The Ivar of the view is full: the sync commits itself to the event, or
   the cancel wins. *)
let var_old_view_cancel () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let v = Var.create 0 and x = new_sync (scope_node ()) in
      let s0 = Var.view v in
      ignore (Var.change v 1) ;
      let got = ref None and continued = Atomic.make 0 in
      spawn_in w (var_sync t.S.doms.(0) (fun () -> s0) x got continued) ;
      spawn_in w (fun () ->
          match C.notify x.S.word (-1) with
          | C.Resume ->
              S.resume t.S.doms.(1) x
          | C.In_scan | C.Lost _ ->
              () ) ;
      fun () ->
        ( match var_result x got with
          | Some r ->
              r.Var.st == Atomic.get v
          | None ->
              C.state x.S.word = C.Done (-1) )
        && C.state x.S.word <> C.Parked
        && Atomic.get continued + queued t x = 1 )

(* A set while a cancel ends the sync of the waiter. One of them wins, and the
   fiber continues one time. *)
let var_set_cancel () =
  trace (fun () ->
      let t = S.create ~domains:2 () in
      let w = world () in
      let d0 = t.S.doms.(0) and d1 = t.S.doms.(1) in
      let v = Var.create 0 and x = new_sync (scope_node ()) in
      let s0 = Var.view v in
      let got = ref None and continued = Atomic.make 0 in
      var_sync d0 (fun () -> s0) x got continued () ;
      spawn_in w (var_set d0 v 1) ;
      spawn_in w (fun () ->
          match C.notify x.S.word (-1) with
          | C.Resume ->
              S.resume d1 x
          | C.In_scan | C.Lost _ ->
              () ) ;
      fun () ->
        Var.get v = 1
        && ( Option.map Var.value (var_result x got) = Some 1
           || C.state x.S.word = C.Done (-1) )
        && queued t x = 1 )

let () =
  print_endline "dscheck" ;
  case "commit: a commit during the scan" commit_during_scan ;
  case "commit: a cancel while a claimer holds the word" notify_while_claimed ;
  case "commit: two partners and one parked sync" two_partners ;
  case "commit: two fibers that sync on each other" cross ;
  case "ring: a push by the owner and a steal by two thieves" ring_steal ;
  case "idle: a domain goes idle while another domain pushes work" idle_push ;
  case "idle: a searching domain goes idle while another domain pushes"
    search_push ;
  case "next slot: a take by the owner and a steal at the same time" next_steal ;
  case "inject: a fill from outside while a domain goes idle" inject_idle ;
  case "pinned: a pinned push while domain 0 goes idle" pinned_push ;
  case "register: a send and a recv on an empty channel at the same time"
    chan_meet ;
  case "register: a fill of an Ivar during a register" ivar_fill_register ;
  case "self-commit: a ready branch and a cancel at the same time"
    self_and_cancel ;
  case "commit: a cancel while a claim of its sync is put back"
    notify_while_put_back ;
  case "scope: a fork_daemon while the last member ends" daemon_and_end ;
  case "cancel: a walk while a fiber starts a sync" cancel_and_sync ;
  case "cancel: a walk while the fiber leaves the scope" cancel_and_leave ;
  case "scope: a fork while the last member ends" fork_and_end ;
  case "fd: a submit and a close at the same time" submit_and_close ;
  case "fd: a Register and a close at the same time" register_and_close ;
  case "jobs: a completion and a cancel of its waiter" job_completion ;
  case "signals: a delivery and a cancel of the waiter" signal_delivery ;
  case "signals: a delivery, a waiter and an unwatch" signal_unwatch ;
  case "semaphore: an acquire and a release at the same time"
    sem_acquire_release ;
  case "semaphore: an acquire with a free permit and a cancel"
    sem_acquire_cancel ;
  case "semaphore: a release and a cancel of the first waiter"
    sem_release_cancel ;
  case "semaphore: a release while a send commits the other branch"
    sem_lost_choose ;
  case "semaphore: two releases and one waiter" sem_two_releases ;
  case "semaphore: one sync with two acquire branches, and two releases"
    sem_two_branches ;
  case "semaphore: a free permit while a send claims the sync"
    sem_self_commit_claimed ;
  case "variable: a set and a sync on changed at the same time" var_set_changed ;
  case "variable: two sets, and a view and a sync" var_two_sets ;
  case "variable: a set and a set back during a sync" var_set_back ;
  case "variable: a sync on an old view and a cancel" var_old_view_cancel ;
  case "variable: a set and a cancel of the waiter" var_set_cancel ;
  case "commit: two fibers that sync on each other, liveness" (fun () ->
      live cross_setup ) ;
  if !failures > 0 then exit 1
