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
  }

let world () =
  { closes= Atomic.make 0
  ; wakes= Atomic.make 0
  ; registers= Atomic.make 0
  ; dels= Atomic.make 0
  ; early_close= Atomic.make false }

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
  {S.slot= 0; gen= 1; os= 7; rc= Atomic.make (S.Open 0); doms= Atomic.make 0}

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
          S.Iv.register iv r 0 ;
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

let () =
  print_endline "dscheck" ;
  case "commit: a commit during the scan" commit_during_scan ;
  case "commit: a cancel while a claimer holds the word" notify_while_claimed ;
  case "commit: two partners and one parked sync" two_partners ;
  case "commit: two fibers that sync on each other" cross ;
  case "ring: a push by the owner and a steal by two thieves" ring_steal ;
  case "idle: a domain goes idle while another domain pushes work" idle_push ;
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
  case "commit: two fibers that sync on each other, liveness" (fun () ->
      live cross_setup ) ;
  if !failures > 0 then exit 1
