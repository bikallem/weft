(* The scheduler of one domain, the events, the fibers and the scopes (design
   sections 2.4 and 2.5). Milestone M0 has one domain, but each part of the
   shared state uses atomics or a short lock, as milestone M1 needs. *)

module C = Commit.Make (Atomic)

exception Cancelled of exn

exception Multiple of (exn * Printexc.raw_backtrace) list

exception Timeout

exception Internal_error of exn * Printexc.raw_backtrace

exception Scope_closed

(* The reason of the cancel of the root scope when main returns. *)
exception Main_returned

(* The reason of the cancel of the daemons when the other members end. *)
exception Daemons_end

(* A short spin lock. Its holder never performs an effect. *)
module Spin = struct
  type t = bool Atomic.t

  let make () = Atomic.make false

  let rec lock t =
    if
      not
        (Atomic.compare_and_set t false true)
      [@mutate
        off
          "one domain never contends; test_heap.ml examines the lock with two \
           domains, but a mutation run cannot start a domain"]
    then ( Domain.cpu_relax () ; lock t )

  let unlock t = Atomic.set t false

  let with_lock t f =
    lock t ;
    match f () with v -> unlock t ; v | exception e -> unlock t ; raise e
end

type fiber =
  { fid: int
  ; name: string
  ; mutable ctxs: ctx list  (** The cancel contexts, the innermost first. *)
  ; current: sync option Atomic.t  (** The sync of the fiber, if one. *)
  ; mutable queued: bool  (** The fiber is in a queue. For [Lost_wakeup]. *) }

and sync =
  { word: C.t
  ; owner: fiber
  ; sctx: ctx  (** The innermost context when the sync started. *)
  ; cancellable: bool
  ; mutable k: (int, step) Effect.Deep.continuation option }

and ctx =
  { nid: int
  ; kind: kind
  ; parent: ctx option
  ; flag: exn option Atomic.t  (** The cancel of this node. *)
  ; lock: Spin.t
  ; members: (int, fiber) Hashtbl.t  (** By fiber id, under [lock]. *)
  ; children: (int, ctx) Hashtbl.t
        (** The nested nodes that are not shields, by node id, under [lock]. *)
  }

and kind = Scope_node of scope_state | Shield_node | Daemon_node

and scope_state =
  { counts: int Atomic.t
        (** The closed bit, the members that are not daemons, and all members.
        *)
  ; failures: (exn * Printexc.raw_backtrace) list Atomic.t
        (** The newest first. *)
  ; join: unit ivar
  ; daemons: (int, ctx) Hashtbl.t  (** Under the lock of the scope node. *) }

and 'a ivar = 'a ivar_state Atomic.t

and 'a ivar_state = Empty of waiters | Full of 'a

(* A list of waiters, the newest first. It drops the waiters whose sync is Done
   only when it has grown to twice its size after the last drop. Thus an add
   costs O(1) on average, and the list holds at most twice its live waiters
   (design section 2.7, The waiters). *)
and waiters = {ws: (sync * int) list; len: int; drop_at: int}

(* The record of one open fd (design section 2.7, close). All domains share
   it. *)
and fdrec =
  { slot: int
  ; gen: int
  ; os: Op.fd
  ; rc: rc Atomic.t
  ; doms: int Atomic.t
        (** The bit of each domain with a registration. Only the owner of a bit
            changes it, with [fetch_and_add], because the bits of all domains
            share the word. *) }

(* The state machine of Eio's [Rcfd]. *)
and rc = Open of int | Closing of int | Closed

(* The registration of an fd in the poller of one domain. Only that domain uses
   it. *)
and reg =
  { fdr: fdrec
  ; mutable rflag: bool
  ; mutable wflag: bool
  ; mutable rwait: waiters
  ; mutable wwait: waiters }

and step =
  | Finished
  | Suspended : 'a req * ('a, step) Effect.Deep.continuation -> step

and _ req =
  | Sync : sync_req -> int req
  | Yield : bool req  (** Gives [true] if the context is cancelled. *)
  | Fork : fiber * (unit -> unit) -> unit req
  | Self : fiber req
  | Enqueue : sync list -> unit req
  | Close_regs : fdrec -> unit req

and sync_req = {leaves: leaf array; nocancel: bool}

(** The part of a base event that the scheduler runs in the scan, with the
    branch index. It commits the branch if it is ready, or it registers an
    offer. *)
and leaf = dom -> sync -> int -> unit

and dom =
  { index: int
  ; queue: ready Queue.t
  ; heap: heap
  ; ids: Commit.Id.gen
  ; sched: t
  ; mutable switches: int
  ; regs: (int, reg) Hashtbl.t  (** The registrations, by slot. *)
  ; externals: (int, sync * int) Hashtbl.t
        (** The waiters of [Ready.External], by id. *) }

and ready = {rfiber: fiber; run: unit -> step}

and t =
  { mutable doms: dom array
  ; slack: int64
  ; fibers: (int, fiber) Hashtbl.t  (** The live fibers, for the debug. *)
  ; fibers_lock: Spin.t
  ; mutable finished: bool }

(* The heap of deadlines of one domain. Only its owner uses it. *)
and heap =
  { mutable entries: entry array
  ; mutable size: int
  ; mutable next_seq: int
  ; mutable rebuild_at: int }

and entry = {deadline: int64; seq: int; esync: sync; branch: int}

type _ Effect.t += Prog : 'a req -> 'a Effect.t

let prog name r = World.perform name (Prog r)

let is_done s = match C.state s.word with C.Done _ -> true | _ -> false

module Waiters = struct
  let empty = {ws= []; len= 0; drop_at= 8}

  let add s i w =
    let w = {w with ws= (s, i) :: w.ws; len= w.len + 1} in
    if
      ( w.len < w.drop_at )
      [@mutate
        off "one more waiter before a drop: only the time of the drop changes"]
    then w
    else
      let ws = List.filter (fun (s, _) -> not (is_done s)) w.ws in
      let len = List.length ws in
      {ws; len; drop_at= Int.max 8 (2 * len)}

  (* The waiters, the oldest first. *)
  let to_list w = List.rev w.ws
end

(* The deadline heap. The order is the deadline, then the order of the adds, so
   the result does not depend on the heap layout. *)
module Heap = struct
  let create () = {entries= [||]; size= 0; next_seq= 0; rebuild_at= 64}

  let lt a b =
    match Int64.compare a.deadline b.deadline with
    | 0 ->
        a.seq < b.seq
    | c ->
        c < 0

  let swap h i j =
    let x = h.entries.(i) in
    h.entries.(i) <- h.entries.(j) ;
    h.entries.(j) <- x

  let rec up h i =
    if (i > 0) [@mutate off "at 0, the parent is 0 itself, and lt is false"]
    then
      let p = (i - 1) / 2 in
      if lt h.entries.(i) h.entries.(p) then (swap h i p ; up h p)

  let rec down h i =
    let l = (2 * i) + 1 in
    let r = l + 1 in
    let m =
      if
        ( l < h.size )
        [@mutate
          off
            "at l = size, the slot holds the element that moves down, and lt \
             of an element with itself is false"]
        && lt h.entries.(l) h.entries.(i)
      then l
      else i
    in
    let m =
      if
        ( r < h.size )
        [@mutate
          off
            "at r = size, the slot holds the element that moves down, or an \
             entry that the rebuild compacted past the size"]
        && lt h.entries.(r) h.entries.(m)
      then r
      else m
    in
    if m <> i then (swap h i m ; down h m)

  let top h = if h.size = 0 then None else Some h.entries.(0)

  let pop h =
    h.size <- h.size - 1 ;
    if (h.size > 0) [@mutate off "at 0, down has no child to compare"] then (
      h.entries.(0) <- h.entries.(h.size) ;
      down h 0 )

  (* Remove the stale entries in place: their sync committed to another
     branch. *)
  let rebuild h =
    let n = ref 0 in
    for i = 0 to h.size - 1 do
      let e = h.entries.(i) in
      if not (is_done e.esync) then (
        h.entries.(!n) <- e ;
        incr n )
    done ;
    h.size <- !n ;
    for
      i = ((h.size / 2) - 1) [@mutate off "down does nothing on a leaf"]
      downto 0
    do
      down h i
    done ;
    h.rebuild_at <- Int.max 64 (2 * h.size)

  let drop_stale h =
    let rec loop () =
      match top h with
      | Some e when is_done e.esync ->
          pop h ; loop ()
      | _ ->
          ()
    in
    loop ()

  (* An add drops the stale entries at the head, and rebuilds the heap when it
     has grown to twice its size after the last rebuild (design section 2.5,
     Timers and domains). *)
  let push h ~deadline esync branch =
    let e = {deadline; seq= h.next_seq; esync; branch} in
    h.next_seq <- h.next_seq + 1 ;
    drop_stale h ;
    if (h.size >= h.rebuild_at) [@mutate off "one more entry before a rebuild"]
    then rebuild h ;
    if h.size = Array.length h.entries then (
      let a = Array.make (Int.max 16 (2 * h.size)) e in
      Array.blit h.entries 0 a 0 h.size ;
      h.entries <- a ) ;
    h.entries.(h.size) <- e ;
    h.size <- h.size + 1 ;
    up h (h.size - 1)
end

(* The atomic register of an Ivar (design section 2.4, spec/tla/Ivar.tla). *)
module Iv = struct
  let make () : 'a ivar = Atomic.make (Empty Waiters.empty)

  (* The scan of a reader: a poll, then a register with a CAS on Empty. *)
  let rec register (iv : 'a ivar) s i =
    match Atomic.get iv with
    | Full _ ->
        ignore (C.self_commit s.word i)
    | Empty w as old ->
        if not (Atomic.compare_and_set iv old (Empty (Waiters.add s i w))) then
          register iv s i

  (* [fill iv v] gives [Some resumed] with the syncs to resume, or [None] if the
     Ivar is full. *)
  let rec fill (iv : 'a ivar) v =
    match Atomic.get iv with
    | Full _ ->
        None
    | Empty w as old ->
        if Atomic.compare_and_set iv old (Full v) then
          Some
            (List.filter_map
               (fun (s, i) ->
                 match C.notify s.word i with C.Resume -> Some s | _ -> None )
               (Waiters.to_list w) )
        else fill iv v

  let get (iv : 'a ivar) =
    match Atomic.get iv with
    | Full v ->
        v
    | Empty _ ->
        invalid_arg "Ivar: the value is not set"

  let leaf iv : leaf = fun _ s i -> register iv s i
end

(* The table of fd slots. A slot is used again with a new generation, so a token
   of an old open never names the new one. *)
module Fdtab = struct
  let lock = Spin.make ()

  let gens = ref (Array.make 64 0)

  let free = ref []

  let next = ref 0

  let alloc os =
    Spin.with_lock lock (fun () ->
        let slot =
          match !free with
          | s :: rest ->
              free := rest ;
              s
          | [] ->
              let s = !next in
              incr next ;
              if s >= Array.length !gens then (
                let a = Array.make (2 * s) 0 in
                Array.blit !gens 0 a 0 s ;
                gens := a ) ;
              s
        in
        let gen =
          (!gens.(slot) + 1) [@mutate off "any change gives a new generation"]
          land 0xffff_ffff
        in
        !gens.(slot) <- gen ;
        {slot; gen; os; rc= Atomic.make (Open 0); doms= Atomic.make 0} )

  let release r = Spin.with_lock lock (fun () -> free := r.slot :: !free)

  (* The slot and the generation in one [int]. A slot is less than 2^30. *)
  let token r = (r.slot lsl 32) lor r.gen

  let of_token t = (t lsr 32, t land 0xffff_ffff)
end

(* The fibers. *)

let next_fid = Atomic.make 0

let new_fiber ?name ctxs =
  let fid = Atomic.fetch_and_add next_fid 1 in
  let name =
    match name with Some n -> n | None -> "fiber-" ^ string_of_int fid
  in
  {fid; name; ctxs; current= Atomic.make None; queued= false}

let register t f =
  Spin.with_lock t.fibers_lock (fun () -> Hashtbl.replace t.fibers f.fid f)

let unregister t f =
  Spin.with_lock t.fibers_lock (fun () -> Hashtbl.remove t.fibers f.fid)

(* The handler of the program effects of one fiber. It holds no state of a
   domain: it gives a step to the loop that runs the fiber (design section 2.5,
   How a fiber finds its domain). *)
let handler : (unit, step) Effect.Deep.handler =
  { retc= (fun () -> Finished)
  ; exnc= raise
  ; effc=
      (fun (type a) (e : a Effect.t) ->
        match e with
        | Prog r ->
            Some
              (fun (k : (a, step) Effect.Deep.continuation) -> Suspended (r, k))
        | _ ->
            None ) }

let start body () = Effect.Deep.match_with body () handler

let push dom f run =
  f.queued <- true ;
  Queue.push {rfiber= f; run} dom.queue

(* Resume a parked sync. The caller got [Resume] or a claim with
   [resume_partner], so it is the one owner of this resume. *)
let resume dom s =
  let i = match C.state s.word with C.Done i -> i | _ -> assert false in
  let k = match s.k with Some k -> k | None -> assert false in
  s.k <- None ;
  push dom s.owner (fun () ->
      Atomic.set s.owner.current None ;
      Effect.Deep.continue k i )

(* Commit each waiter, the oldest first. *)
let wake_all dom waiters =
  List.iter
    (fun (s, i) ->
      match C.notify s.word i with C.Resume -> resume dom s | _ -> () )
    (Waiters.to_list waiters)

(* The registration of [fdr] on [dom]. The first block on a domain adds the fd
   to its poller. *)
let ensure_reg dom fdr =
  match Hashtbl.find_opt dom.regs fdr.slot with
  | Some reg when reg.fdr == fdr ->
      Ok reg
  | _ -> (
    match Atomic.get fdr.rc with
    | Closing _ | Closed ->
        (* The waiter retries, and the retry gets [EBADF]. *)
        Error None
    | Open _ -> (
        let token = Fdtab.token fdr in
        match
          World.perform "Net"
            (World.Submit (Op.Register (dom.index, fdr.os, token)))
        with
        | Error e ->
            Error (Some e)
        | Ok () ->
            let reg =
              { fdr
              ; rflag= false
              ; wflag= false
              ; rwait= Waiters.empty
              ; wwait= Waiters.empty }
            in
            Hashtbl.replace dom.regs fdr.slot reg ;
            ignore (Atomic.fetch_and_add fdr.doms (1 lsl dom.index)) ;
            Ok reg ) )

(* The wait for an fd. With [~clear], the fiber got [EAGAIN], so the flag of
   this domain is stale. If the Register fails, [err] gets the error, and the
   branch commits, so that the fiber raises it. *)
let fd_leaf ~write ~clear ~err fdr : leaf =
 fun dom s i ->
  match ensure_reg dom fdr with
  | Error e ->
      err := e ;
      ignore (C.self_commit s.word i)
  | Ok reg ->
      if clear then if write then reg.wflag <- false else reg.rflag <- false ;
      if if write then reg.wflag else reg.rflag then
        ignore (C.self_commit s.word i)
      else if write then reg.wwait <- Waiters.add s i reg.wwait
      else reg.rwait <- Waiters.add s i reg.rwait

(* The close of [fdr] on [dom]: remove the registration, commit its waiters, and
   remove the fd from the poller. *)
let close_reg dom fdr =
  match Hashtbl.find_opt dom.regs fdr.slot with
  | Some reg when reg.fdr == fdr ->
      Hashtbl.remove dom.regs fdr.slot ;
      ignore (Atomic.fetch_and_add fdr.doms (-(1 lsl dom.index))) ;
      wake_all dom reg.rwait ;
      wake_all dom reg.wwait ;
      ignore
        (World.perform "Net.close"
           (World.Submit (Op.Deregister (dom.index, fdr.os))) )
  | _ ->
      ()

let external_leaf id : leaf =
 fun dom s i -> Hashtbl.replace dom.externals id (s, i)

let on_ready dom = function
  | World.Ready.Fd {token; read; write} -> (
      let slot, gen = Fdtab.of_token token in
      match Hashtbl.find_opt dom.regs slot with
      | Some reg when reg.fdr.gen = gen ->
          if read then (
            reg.rflag <- true ;
            let w = reg.rwait in
            reg.rwait <- Waiters.empty ;
            wake_all dom w ) ;
          if write then (
            reg.wflag <- true ;
            let w = reg.wwait in
            reg.wwait <- Waiters.empty ;
            wake_all dom w )
      | _ ->
          () )
  | World.Ready.External id -> (
    match Hashtbl.find_opt dom.externals id with
    | Some (s, i) -> (
        Hashtbl.remove dom.externals id ;
        match C.notify s.word i with C.Resume -> resume dom s | _ -> () )
    | None ->
        () )
  | World.Ready.Alarm | World.Ready.Woken ->
      ()

(* The cancel contexts (design section 2.5, How a cancel reaches a fiber, and
   spec/tla/Scope.tla). *)

let next_nid = Atomic.make 0

let new_node ?parent kind =
  { nid= Atomic.fetch_and_add next_nid 1
  ; kind
  ; parent
  ; flag= Atomic.make None
  ; lock= Spin.make ()
  ; members= Hashtbl.create ~random:false 4
  ; children= Hashtbl.create ~random:false 4 }

let add_member_to n f =
  Spin.with_lock n.lock (fun () -> Hashtbl.replace n.members f.fid f)

let remove_member n f =
  Spin.with_lock n.lock (fun () -> Hashtbl.remove n.members f.fid)

(* The values of a table, in the order of their ids: the fork order. *)
let by_id tbl =
  Hashtbl.fold (fun id v acc -> (id, v) :: acc) tbl []
  |> List.sort compare |> List.map snd

(* The first cancelled context on the chain from [c], up to a shield. *)
let rec first_flag c =
  match c.kind with
  | Shield_node ->
      None
  | Scope_node _ | Daemon_node -> (
    match Atomic.get c.flag with
    | Some _ as e ->
        e
    | None -> (
      match c.parent with None -> None | Some p -> first_flag p ) )

(* [x] is on the chain from [c], with no shield between them. *)
let rec on_chain c x =
  c == x
  ||
  match c.kind with
  | Shield_node ->
      false
  | Scope_node _ | Daemon_node -> (
    match c.parent with None -> false | Some p -> on_chain p x )

(* The cancel of [x]: set the flag of [x] only, then walk [x] and its nested
   nodes, but not into a shield. Commit each sync of a member whose chain
   reaches [x]. It gives the syncs to resume. *)
let cancel_node x reason =
  ignore (Atomic.compare_and_set x.flag None (Some reason)) ;
  let found = ref [] in
  let rec visit n =
    let members, children =
      Spin.with_lock n.lock (fun () -> (by_id n.members, by_id n.children))
    in
    List.iter
      (fun f ->
        match Atomic.get f.current with
        | Some s ->
            found := s :: !found
        | None ->
            () )
      members ;
    (* A shield is never in [children], so the walk does not go into it. *)
    List.iter visit children
  in
  visit x ;
  List.filter_map
    (fun s ->
      if s.cancellable && on_chain s.sctx x then
        match C.notify s.word (-1) with C.Resume -> Some s | _ -> None
      else None )
    (List.rev !found)

(* The sync of a fiber, in the scheduler (design section 2.4). *)

(* The order of the scan. It performs no [Random] when [n <= 1]. *)
let shuffle n =
  let a = Array.init n Fun.id in
  for i = n - 1 downto 1 do
    let j = World.perform "sync" (World.Random (i + 1)) in
    let x = a.(i) in
    a.(i) <- a.(j) ;
    a.(j) <- x
  done ;
  a

let rec run_fiber : dom -> fiber -> step -> unit =
 fun dom fiber step ->
  match step with
  | Finished ->
      unregister dom.sched fiber
  | Suspended (Self, k) ->
      run_fiber dom fiber (Effect.Deep.continue k fiber)
  | Suspended (Fork (f, body), k) ->
      register dom.sched f ;
      push dom f (start body) ;
      run_fiber dom fiber (Effect.Deep.continue k ())
  | Suspended (Close_regs fdr, k) ->
      close_reg dom fdr ;
      run_fiber dom fiber (Effect.Deep.continue k ())
  | Suspended (Enqueue l, k) ->
      List.iter (resume dom) l ;
      run_fiber dom fiber (Effect.Deep.continue k ())
  | Suspended (Yield, k) ->
      (* The fiber reads the flags of its chain when it runs again, so a yield
         is a cancellation point. *)
      push dom fiber (fun () ->
          Effect.Deep.continue k
            (Option.is_some (first_flag (List.hd fiber.ctxs))) )
  | Suspended (Sync r, k) ->
      sync dom fiber r k

and sync dom fiber (r : sync_req) (k : (int, step) Effect.Deep.continuation) =
  let s =
    { word= C.make (Commit.Id.next dom.ids)
    ; owner= fiber
    ; sctx= List.hd fiber.ctxs
    ; cancellable= not r.nocancel
    ; k= None }
  in
  (* Publish the sync, then read the flags (the Dekker pair with the walk). *)
  Atomic.set fiber.current (Some s) ;
  if s.cancellable && Option.is_some (first_flag s.sctx) then
    ignore (C.self_commit s.word (-1)) ;
  let n = Array.length r.leaves in
  let order = shuffle n in
  let rec scan j =
    if j < n && not (is_done s) then (
      r.leaves.(order.(j)) dom s order.(j) ;
      scan (j + 1) )
  in
  scan 0 ;
  s.k <- Some k ;
  match C.park s.word with
  | `Parked ->
      ()
  | `Done i ->
      s.k <- None ;
      Atomic.set fiber.current None ;
      run_fiber dom fiber (Effect.Deep.continue k i)

(* The loop of one domain (design section 2.5). *)

(* A busy domain polls each [k_poll] switches (open question 3). *)
let k_poll = 61

let now_ns () = Clock.to_ns Mono (World.perform "sched" (World.Now Mono))

(* Expire the entries up to now. A timer never fires before its deadline: the
   slack only rounds the alarm up, so the deadlines in one slot of the slack
   share one alarm. *)
let expire dom =
  let limit = now_ns () in
  let rec loop () =
    match Heap.top dom.heap with
    | Some e when Int64.compare e.deadline limit <= 0 ->
        Heap.pop dom.heap ;
        ( match C.notify e.esync.word e.branch with
        | C.Resume ->
            resume dom e.esync
        | C.In_scan | C.Lost _ ->
            () ) ;
        loop ()
    | _ ->
        ()
  in
  loop ()

(* The alarm: the earliest live deadline, rounded up to the slack. The rounding
   saturates at the largest time. *)
let alarm dom =
  Heap.drop_stale dom.heap ;
  match Heap.top dom.heap with
  | None ->
      None
  | Some e ->
      let slack = dom.sched.slack in
      let d = e.deadline in
      let d =
        if Int64.compare slack 0L <= 0 then d
        else
          let r = Int64.rem d slack in
          if Int64.equal r 0L then d
          else
            let up = Int64.sub slack r in
            if
              ( Int64.compare d (Int64.sub Int64.max_int up)
              > 0 )
              [@mutate off "at the limit, the sum is max_int, the same value"]
            then Int64.max_int
            else Int64.add d up
      in
      Some (Clock.of_ns Mono d)

let wait dom timeout =
  expire dom ;
  (* The time can pass after the last poll: if an entry expired, the domain
     polls instead of a sleep. *)
  let timeout = if Queue.is_empty dom.queue then timeout else World.Wait.Poll in
  let w = {World.Wait.timeout; deadline= alarm dom} in
  List.iter (on_ready dom) (World.perform "sched" (World.Wait (dom.index, w))) ;
  expire dom

let rec loop dom =
  if not dom.sched.finished then (
    ( match Queue.take_opt dom.queue with
    | Some r ->
        r.rfiber.queued <- false ;
        dom.switches <- dom.switches + 1 ;
        if
          ( dom.switches >= k_poll )
          [@mutate off "61 or 62 switches: k_poll is a start value"]
        then (
          dom.switches <- 0 ;
          wait dom World.Wait.Poll ) ;
        World.perform "sched" (World.Switch dom.index) ;
        run_fiber dom r.rfiber (r.run ())
    | None ->
        (* [wait] polls instead of a sleep if the poll made a fiber ready. *)
        wait dom World.Wait.Poll ;
        if not dom.sched.finished then wait dom World.Wait.Sleep ) ;
    loop dom )

(* The scopes (design section 2.5, Fibers, scopes and cancellation). *)

let closed_bit = 1

let nd_one = 1 lsl 1

let all_one = 1 lsl 32

let nd_count c = (c lsr 1) land 0x7fff_ffff

let all_count c = c lsr 32

let self () = prog "Fiber.self" Self

let enqueue = function [] -> () | l -> prog "Fiber" (Enqueue l)

(* Fill an Ivar and resume its waiters. *)
let fill_resume iv v =
  match Iv.fill iv v with
  | None ->
      false
  | Some resumed ->
      enqueue resumed ; true

let state_of node =
  match node.kind with
  | Scope_node st ->
      st
  | Shield_node | Daemon_node ->
      assert false (* Only a scope node has members that end. *)

(* A failed member stores its exception before it ends, then it cancels the
   scope. A [Cancelled] is not a failure. *)
let fail node e bt =
  match e with
  | Cancelled _ ->
      ()
  | _ ->
      let st = state_of node in
      let rec store () =
        let l = Atomic.get st.failures in
        if not (Atomic.compare_and_set st.failures l ((e, bt) :: l)) then
          store ()
      in
      store () ;
      enqueue (cancel_node node e)

(* The end of a member: one CAS on the counts. The end of the last member sets
   the closed bit and fills the join. When the last member that is not a daemon
   ends, the scope cancels its daemons. *)
let member_end node ~daemon =
  let st = state_of node in
  let rec dec () =
    let c = Atomic.get st.counts in
    let c' = c - all_one - if daemon then 0 else nd_one in
    let c' = if all_count c' = 0 then c' lor closed_bit else c' in
    if Atomic.compare_and_set st.counts c c' then c' else dec ()
  in
  let c = dec () in
  if all_count c = 0 then ignore (fill_resume st.join ())
  else if (not daemon) && nd_count c = 0 then
    let ds = Spin.with_lock node.lock (fun () -> by_id st.daemons) in
    List.iter (fun d -> enqueue (cancel_node d Daemons_end)) ds

(* Add a member with a CAS that succeeds only if the closed bit is clear. A fork
   also reads the flags of the chain (design section 2.5, Fork into a scope that
   ends). *)
let add_member node ~daemon =
  let st = state_of node in
  if Option.is_some (first_flag node) then raise Scope_closed ;
  let rec inc () =
    let c = Atomic.get st.counts in
    if c land closed_bit <> 0 || (daemon && nd_count c = 0) then
      raise Scope_closed ;
    let c' = c + all_one + if daemon then 0 else nd_one in
    if not (Atomic.compare_and_set st.counts c c') then inc ()
  in
  inc ()

let run_body scope body =
  match body () with
  | () ->
      ()
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      fail scope e bt

let fork ?name scope body =
  add_member scope ~daemon:false ;
  let f = new_fiber ?name [scope] in
  add_member_to scope f ;
  prog "Fiber.fork"
    (Fork
       ( f
       , fun () ->
           run_body scope body ;
           remove_member scope f ;
           member_end scope ~daemon:false ) )

(* A daemon has its own node under the scope. The count and the link of the node
   are one step under the lock of the scope, so the end of the last member that
   is not a daemon sees the node (it reads the daemons under the same lock). The
   daemon removes its node when it ends. *)
let fork_daemon ?name scope body =
  let st = state_of scope in
  let d = new_node ~parent:scope Daemon_node in
  let f = new_fiber ?name [d] in
  Hashtbl.replace d.members f.fid f ;
  Spin.with_lock scope.lock (fun () ->
      add_member scope ~daemon:true ;
      Hashtbl.replace scope.children d.nid d ;
      Hashtbl.replace st.daemons d.nid d ) ;
  prog "Fiber.fork_daemon"
    (Fork
       ( f
       , fun () ->
           run_body scope body ;
           Spin.with_lock scope.lock (fun () ->
               Hashtbl.remove scope.children d.nid ;
               Hashtbl.remove st.daemons d.nid ) ;
           member_end scope ~daemon:true ) )

let sync_req ~nocancel leaves = prog "sync" (Sync {leaves; nocancel})

let new_scope_state () =
  { counts= Atomic.make (all_one + nd_one)
  ; failures= Atomic.make []
  ; join= Iv.make ()
  ; daemons= Hashtbl.create ~random:false 4 }

let reraise (e, bt) = Printexc.raise_with_backtrace e bt

(* The end of the body of a scope: store a failure, end the body as a member,
   then join. The join ignores cancels. If the body gave a value, the scope
   gives it, also if the scope was cancelled: [with_timeout] never drops a
   value. *)
let finish_scope fiber node result =
  (match result with Error (e, bt) -> fail node e bt | Ok _ -> ()) ;
  remove_member node fiber ;
  member_end node ~daemon:false ;
  let st = state_of node in
  ignore (sync_req ~nocancel:true [|Iv.leaf st.join|]) ;
  ( match node.parent with
  | Some p ->
      Spin.with_lock p.lock (fun () -> Hashtbl.remove p.children node.nid)
  | None ->
      () ) ;
  match List.rev (Atomic.get st.failures) with
  | [] -> (
    match result with Ok v -> v | Error e -> reraise e )
  | [e] ->
      reraise e
  | l ->
      raise (Multiple l)

let scope f =
  let fiber = self () in
  let parent = List.hd fiber.ctxs in
  let node = new_node ~parent (Scope_node (new_scope_state ())) in
  Hashtbl.replace node.members fiber.fid fiber ;
  Spin.with_lock parent.lock (fun () ->
      Hashtbl.replace parent.children node.nid node ) ;
  fiber.ctxs <- node :: fiber.ctxs ;
  let result =
    match f node with
    | v ->
        Ok v
    | exception e ->
        Error (e, Printexc.get_raw_backtrace ())
  in
  fiber.ctxs <- List.tl fiber.ctxs ;
  finish_scope fiber node result

let shield f =
  let fiber = self () in
  let node = new_node ~parent:(List.hd fiber.ctxs) Shield_node in
  fiber.ctxs <- node :: fiber.ctxs ;
  let pop () = fiber.ctxs <- List.tl fiber.ctxs in
  match f () with
  | v ->
      pop () ; v
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      pop () ;
      Printexc.raise_with_backtrace e bt

(* [Self] first: outside a fiber, the cancel raises [Not_in_fiber] before it
   changes a word that nobody would then resume. *)
let cancel scope reason =
  ignore (self ()) ;
  enqueue (cancel_node scope reason)

(* The reason of the cancel that ended a sync of this fiber. *)
let reason () =
  let fiber = self () in
  match first_flag (List.hd fiber.ctxs) with
  | Some e ->
      e
  | None ->
      assert false (* A sync commits with -1 only after a flag. *)

(* Run [main] as the first fiber, in the root scope. When [main] returns, the
   root scope cancels the other fibers and waits for them. *)
let run ?(slack = Span.ms 1) ?on_create main =
  let t =
    { doms= [||]
    ; slack= Span.to_ns slack
    ; fibers= Hashtbl.create ~random:false 64
    ; fibers_lock= Spin.make ()
    ; finished= false }
  in
  let dom =
    { index= 0
    ; queue= Queue.create ()
    ; heap= Heap.create ()
    ; ids= Commit.Id.gen ~domain:0 ~max_domains:1
    ; sched= t
    ; switches= 0
    ; regs= Hashtbl.create ~random:false 64
    ; externals= Hashtbl.create ~random:false 8 }
  in
  t.doms <- [|dom|] ;
  Option.iter (fun f -> f t) on_create ;
  let root = new_node (Scope_node (new_scope_state ())) in
  let fiber = new_fiber ~name:"main" [root] in
  Hashtbl.replace root.members fiber.fid fiber ;
  let result = ref None in
  let body () =
    let r =
      match main () with
      | v ->
          cancel root Main_returned ; Ok v
      | exception e ->
          Error (e, Printexc.get_raw_backtrace ())
    in
    result :=
      Some
        ( match finish_scope fiber root r with
        | v ->
            Ok v
        | exception e ->
            Error (e, Printexc.get_raw_backtrace ()) ) ;
    t.finished <- true
  in
  register t fiber ;
  push dom fiber (start body) ;
  ( try loop dom
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      raise (Internal_error (e, bt)) ) ;
  match !result with
  | Some (Ok v) ->
      v
  | Some (Error e) ->
      reraise e
  | None ->
      assert false
