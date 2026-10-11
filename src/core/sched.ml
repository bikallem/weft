(* The scheduler, the events, the fibers and the scopes (design sections 2.4 and
   2.5). Each domain runs [loop] on its own state. The domains share the scopes,
   the sync words, the fd records, the rings for the steal, the overflow queue
   and the idle set. *)

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
  ; chain: handler
        (** The handlers of the user on the chain of scopes, the outermost
            first. Each fiber of the node runs inside it. *) }

(* A handler of the user (design section 2.5, Effect handlers of a scope). *)
and handler = {handle: 'a. (unit -> 'a) -> 'a}

and kind =
  | Scope_node of scope_state
  | Shield_node of bool Atomic.t
  | Daemon_node

(* The flag of a shield node tells that it is open: the cancels from outside go
   through it. Only [Condition.wait] opens a shield, for the time of its wait
   (design section 2.5, Locks for fiber code). *)
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

(* The state machine of Eio's [Rcfd]. A registration holds a user. *)
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
  | Dom_index : int req  (** The domain that runs the fiber, for the tests. *)
  | Enqueue : sync list -> unit req
  | Close_notices : fdrec -> unit req
      (** Send a close notice to each domain with a registration. *)
  | Job : 'a Op.blocking * (unit -> unit) option -> (int, Op.error) result req
      (** Start a job of the pool for the domain that runs the fiber. The
          function runs at its completion. *)
  | Detach_job : int -> unit req
      (** The fiber does not wait for the job any more. *)

and sync_req = {leaves: leaf array; nocancel: bool}

(** The part of a base event that the scheduler runs in the scan, with the
    branch index. It commits the branch if it is ready, or it registers an
    offer. *)
and leaf = dom -> sync -> int -> unit

and dom =
  { index: int
  ; ring: ready Ring.t  (** The local queue. *)
  ; heap: heap
  ; ids: Commit.Id.gen
  ; sched: t
  ; mutable switches: int
  ; mutable ticks: int
        (** The takes since the last take of the overflow queue. *)
  ; mutable rng: int
        (** The PRNG of the scan order and of the steal. The world gives its
            seed when the domain starts. *)
  ; mutable wake: bool  (** A push to do a wake at the next safe point. *)
  ; stats: stats  (** The counters of the domain, for the measurements. *)
  ; mutable held: int
        (** The jobs that this domain started, that hold the user of an fd, and
            that did not complete. *)
  ; regs: (int, reg) Hashtbl.t  (** The registrations, by slot. *)
  ; mailbox: fdrec list Atomic.t  (** The close notices, the newest first. *) }

and ready = {rfiber: fiber; run: unit -> step}

(* The counters of a domain (open questions 3, 4 and 8). Only the owner writes
   them. *)
and stats =
  { mutable wakes: int  (** The wakes that the pushes did. *)
  ; mutable edges: int  (** The fd events. *)
  ; mutable idle_edges: int  (** The fd events with no live waiter. *)
  ; mutable busy_polls: int  (** The polls of a busy domain. *) }

and t =
  { mutable doms: dom array
  ; slack: int64
  ; fibers: (int, fiber) Hashtbl.t  (** The live fibers, for the debug. *)
  ; fibers_lock: Spin.t
  ; simulated: bool
        (** The simulator: it needs a switch point at each fiber, and the table
            of fibers for its checks. Production has neither. *)
  ; finished: bool Atomic.t
  ; idle: int Atomic.t
        (** The bit of each domain that sleeps, or is about to. *)
  ; overflow: ready Queue.t  (** The fibers of full rings, under its lock. *)
  ; overflow_lock: Spin.t
  ; externals: (int, ext) Hashtbl.t
        (** The completions of [Ready.External], by id, under its lock. *)
  ; externals_lock: Spin.t
  ; signals: (Op.signal, sigstate) Hashtbl.t
        (** The count and the waiters of each signal, under its lock. *)
  ; signals_lock: Spin.t }

(* A signal (design section 2.7, Signals). A commit of a waiter takes the count,
   and it puts the count in the cell of the waiter before the commit. *)
and sigstate =
  { mutable count: int
  ; mutable swait: (sync * int * int ref) list  (** The newest first. *) }

(* A completion that the world gives with [Ready.External]: the end of a job of
   the pool, or of [Sim.settle]. *)
and ext =
  { mutable ewait: (sync * int) option  (** The waiter, if one. *)
  ; mutable arrived: bool  (** The completion came before the waiter. *)
  ; mutable dropped: bool
        (** The fiber left before its wait started, for example after a cancel.
        *)
  ; on_done: (unit -> unit) option
        (** For a job that holds the user of an fd: the release, at the
            completion, also when the waiter left. *) }

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
  (* A mutex, not a spin lock: the table is global, and its critical sections
     have no atomic operation and no effect. *)
  let lock = Mutex.create ()

  let gens = ref (Array.make 64 0)

  let free = ref []

  let next = ref 0

  let alloc os =
    Mutex.protect lock (fun () ->
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

  let release r = Mutex.protect lock (fun () -> free := r.slot :: !free)

  (* The slot and the generation in one [int]. A slot is less than 2^30. *)
  let token r = (r.slot lsl 32) lor r.gen

  let of_token t = (t lsr 32, t land 0xffff_ffff)
end

(* The users of an fd record (Eio's [Rcfd], design section 2.7, close, and
   spec/tla/Fd.tla). *)
module Rc = struct
  (* close(2), by the side that made the state [Closed]. *)
  let close_os fdr =
    ignore (World.perform "Net.close" (World.Submit (Op.Close fdr.os))) ;
    Fdtab.release fdr

  (* Add a user. Only in [Open]. *)
  let rec acquire fdr =
    match Atomic.get fdr.rc with
    | Open n as old ->
        Atomic.compare_and_set fdr.rc old (Open (n + 1)) || acquire fdr
    | Closing _ | Closed ->
        false

  (* The side that changes [Closing 0] to [Closed] calls close(2), and only that
     side. *)
  let rec release fdr =
    match Atomic.get fdr.rc with
    | Open n as old ->
        if not (Atomic.compare_and_set fdr.rc old (Open (n - 1))) then
          release fdr
    | Closing 1 as old ->
        if Atomic.compare_and_set fdr.rc old Closed then close_os fdr
        else release fdr
    | Closing n as old ->
        if not (Atomic.compare_and_set fdr.rc old (Closing (n - 1))) then
          release fdr
    | Closed ->
        assert false

  (* The start of a close: [Open 0] is the last use, so it closes the fd at
     once, and [Open n] gives [Closing n]. It gives [true] if users remain: then
     the domains in the set get a close notice. A second close does nothing. *)
  let rec close fdr =
    match Atomic.get fdr.rc with
    | Open 0 as old ->
        if Atomic.compare_and_set fdr.rc old Closed then (close_os fdr ; false)
        else close fdr
    | Open n as old ->
        Atomic.compare_and_set fdr.rc old (Closing n) || close fdr
    | Closing _ | Closed ->
        false
end

(* The fibers. *)

let next_fid = Atomic.make 0

(* A fiber with no name has the name "": [Sched_debug] names it. *)
let new_fiber ?(name = "") ctxs =
  let fid = Atomic.fetch_and_add next_fid 1 in
  {fid; name; ctxs; current= Atomic.make None; queued= false}

(* The table of live fibers exists only in the simulator. *)
let register t f =
  if t.simulated then
    Spin.with_lock t.fibers_lock (fun () -> Hashtbl.replace t.fibers f.fid f)

let unregister t f =
  if t.simulated then
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

let start_fiber body () = Effect.Deep.match_with body () handler

(* The idle set (design section 2.5, Idle and wake, and spec/tla/Wake.tla). *)
module Idle = struct
  let bit d = 1 lsl d

  (* Only [d] adds its bit, and the bit is clear: [d] removes it at the end of
     each idle turn, or a waker took it. *)
  let add t d = ignore (Atomic.fetch_and_add t.idle (bit d))

  (* Take [d] out of the set. It gives [true] if [d] was in it. *)
  let rec remove t d =
    let v = Atomic.get t.idle in
    if v land bit d = 0 then false
    else Atomic.compare_and_set t.idle v (v land lnot (bit d)) || remove t d

  (* Take one domain out of the set, the lowest that is not [self], and give
     it. *)
  let rec take t ~self =
    let v = Atomic.get t.idle land lnot (bit self) in
    if v = 0 then None
    else
      let d =
        let rec low i = if v land bit i <> 0 then i else low (i + 1) in
        low 0
      in
      if remove t d then Some d else take t ~self
end

(* A push to the local queue. A full ring moves its older half and the new fiber
   to the overflow queue, as Go does, so the order stays FIFO. If a domain is
   idle, the push marks a wake: the scheduler does it at the next safe point,
   because a wake is a world effect, and a lock can be held here. *)
let push dom f run =
  f.queued <- true ;
  let r = {rfiber= f; run} in
  let t = dom.sched in
  if not (Ring.push dom.ring r) then
    Spin.with_lock t.overflow_lock (fun () ->
        for _ = 1 to Ring.size / 2 do
          match Ring.pop dom.ring with
          | Some x ->
              Queue.push x t.overflow
          | None ->
              ()
        done ;
        Queue.push r t.overflow ) ;
  if Atomic.get t.idle <> 0 then dom.wake <- true

(* Wake one idle domain if a push marked it. No lock is held here. *)
let flush_wake dom =
  if dom.wake then (
    dom.wake <- false ;
    match Idle.take dom.sched ~self:dom.index with
    | Some d ->
        dom.stats.wakes <- dom.stats.wakes + 1 ;
        World.perform "sched" (World.Wake d)
    | None ->
        () )

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

(* The registration of [fdr] on [dom], in the order of spec/tla/Fd.tla: add a
   user, set the domain bit, read the state, then add the fd to the poller.
   [Error None] tells that the fd is closing: the waiter retries and gets
   [EBADF]. *)
(* The live registration of [fdr] on [dom], if one. *)
let live_reg dom fdr =
  match Hashtbl.find_opt dom.regs fdr.slot with
  | Some reg when reg.fdr == fdr ->
      Some reg
  | _ ->
      None

let ensure_reg dom fdr =
  match live_reg dom fdr with
  | Some reg ->
      Ok reg
  | None -> (
      if not (Rc.acquire fdr) then Error None
      else
        let bit = 1 lsl dom.index in
        ignore (Atomic.fetch_and_add fdr.doms bit) ;
        let undo () =
          ignore (Atomic.fetch_and_add fdr.doms (-bit)) ;
          Rc.release fdr
        in
        match Atomic.get fdr.rc with
        | Closing _ | Closed ->
            undo () ; Error None
        | Open _ -> (
            let token = Fdtab.token fdr in
            match
              World.perform "Net"
                (World.Submit (Op.Register (dom.index, fdr.os, token)))
            with
            | Error e ->
                undo () ; Error (Some e)
            | Ok () ->
                let reg =
                  { fdr
                  ; rflag= false
                  ; wflag= false
                  ; rwait= Waiters.empty
                  ; wwait= Waiters.empty }
                in
                Hashtbl.replace dom.regs fdr.slot reg ;
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

(* Remove a live registration of [dom]: clear the bit, commit its waiters (on a
   close, they retry and get [EBADF]), remove the fd from the poller, and
   release the user of the registration. A close notice and the removal of an
   unused registration both do it, on the thread of the domain, so the two never
   interleave. *)
let remove_reg dom reg =
  let fdr = reg.fdr in
  Hashtbl.remove dom.regs fdr.slot ;
  ignore (Atomic.fetch_and_add fdr.doms (-(1 lsl dom.index))) ;
  wake_all dom reg.rwait ;
  wake_all dom reg.wwait ;
  ignore
    (World.perform "Net.close"
       (World.Submit (Op.Deregister (dom.index, fdr.os))) ) ;
  Rc.release fdr

(* A close notice on [dom] acts only on a live registration. *)
let close_notice dom fdr = Option.iter (remove_reg dom) (live_reg dom fdr)

let rec post dom fdr =
  let l = Atomic.get dom.mailbox in
  if not (Atomic.compare_and_set dom.mailbox l (fdr :: l)) then post dom fdr

(* Take the close notices of [dom], the oldest first. *)
let take_mail dom =
  List.iter (close_notice dom) (List.rev (Atomic.exchange dom.mailbox []))

(* Send a close notice to each domain in the set of [fdr], and wake it. The
   closer acts on its own notice at once. *)
let close_notices dom fdr =
  let set = Atomic.get fdr.doms in
  Array.iter
    (fun d ->
      if set land (1 lsl d.index) <> 0 then
        if d == dom then close_notice dom fdr
        else (
          post d fdr ;
          World.perform "Net.close" (World.Wake d.index) ) )
    dom.sched.doms

(* The wait for a completion. If it came first, the branch commits. *)
let external_leaf id : leaf =
 fun dom s i ->
  let t = dom.sched in
  Spin.with_lock t.externals_lock (fun () ->
      match Hashtbl.find_opt t.externals id with
      | Some e when e.arrived ->
          Hashtbl.remove t.externals id ;
          ignore (C.self_commit s.word i)
      | Some e ->
          e.ewait <- Some (s, i)
      | None ->
          Hashtbl.replace t.externals id
            {ewait= Some (s, i); arrived= false; dropped= false; on_done= None} )

(* The fiber does not wait for the completion of [id] any more. *)
let drop_external t id =
  Spin.with_lock t.externals_lock (fun () ->
      match Hashtbl.find_opt t.externals id with
      | Some e when e.arrived ->
          Hashtbl.remove t.externals id
      | Some e ->
          e.dropped <- true
      | None ->
          () )

(* Start a job: the domain records its completion before the fiber can wait for
   it. *)
let start_job dom job on_done =
  let r = World.perform "job" (World.Submit (Op.Start (dom.index, job))) in
  ( match r with
  | Ok id ->
      if Option.is_some on_done then dom.held <- dom.held + 1 ;
      let t = dom.sched in
      Spin.with_lock t.externals_lock (fun () ->
          Hashtbl.replace t.externals id
            {ewait= None; arrived= false; dropped= false; on_done} )
  | Error _ ->
      () ) ;
  r

(* The signals (design section 2.7, Signals). *)

let sigstate t sg =
  match Hashtbl.find_opt t.signals sg with
  | Some st ->
      st
  | None ->
      let st = {count= 0; swait= []} in
      Hashtbl.replace t.signals sg st ;
      st

(* A waiter of [received sg]: it takes the count if the count is not 0. If not,
   it waits, and the waiters with a [Done] sync go. *)
let signal_leaf sg cell : leaf =
 fun dom s i ->
  let t = dom.sched in
  Spin.with_lock t.signals_lock (fun () ->
      let st = sigstate t sg in
      if st.count > 0 then (
        cell := st.count ;
        match C.self_commit s.word i with
        | Ok () ->
            st.count <- 0
        | Error _ ->
            () )
      else
        st.swait <-
          (s, i, cell)
          :: List.filter (fun (s, _, _) -> not (is_done s)) st.swait )

(* [n] more of [sg]: the oldest waiter that can commit takes the count. *)
let deliver dom sg n =
  let t = dom.sched in
  let resumed =
    Spin.with_lock t.signals_lock (fun () ->
        let st = sigstate t sg in
        st.count <- st.count + n ;
        let rec give resumed = function
          | [] ->
              (resumed, [])
          | ws when st.count = 0 ->
              (resumed, ws)
          | (s, i, cell) :: rest -> (
              cell := st.count ;
              match C.notify s.word i with
              | C.Resume ->
                  st.count <- 0 ;
                  (s :: resumed, rest)
              | C.In_scan ->
                  st.count <- 0 ;
                  (resumed, rest)
              | C.Lost _ ->
                  give resumed rest )
        in
        let resumed, rest = give [] (List.rev st.swait) in
        st.swait <- List.rev rest ;
        resumed )
  in
  List.iter (resume dom) resumed

let on_ready dom = function
  | World.Ready.Signal (sg, n) ->
      deliver dom sg n
  | World.Ready.Fd {token; read; write} -> (
      let slot, gen = Fdtab.of_token token in
      match Hashtbl.find_opt dom.regs slot with
      | Some reg when reg.fdr.gen = gen ->
          let live w = List.exists (fun (s, _) -> not (is_done s)) w.ws in
          let rlive = live reg.rwait and wlive = live reg.wwait in
          dom.stats.edges <- dom.stats.edges + 1 ;
          if
            not
              ((read && rlive) || (write && wlive))
            [@mutate off "the condition of a counter of the measurements only"]
          then dom.stats.idle_edges <- dom.stats.idle_edges + 1 ;
          let others = Atomic.get reg.fdr.doms land lnot (1 lsl dom.index) in
          if others <> 0 && not (rlive || wlive) then
            (* An edge with no live waiter, and another domain has a
               registration: the fiber of the fd moved. The domain removes its
               registration (design section 2.7, An fd in many pollers). *)
            remove_reg dom reg
          else (
            if read then (
              reg.rflag <- true ;
              let w = reg.rwait in
              reg.rwait <- Waiters.empty ;
              wake_all dom w ) ;
            if write then (
              reg.wflag <- true ;
              let w = reg.wwait in
              reg.wwait <- Waiters.empty ;
              wake_all dom w ) )
      | _ ->
          () )
  | World.Ready.External id -> (
      let t = dom.sched in
      let e =
        Spin.with_lock t.externals_lock (fun () ->
            match Hashtbl.find_opt t.externals id with
            | Some e
              when ( Option.is_some e.ewait || e.dropped )
                   [@mutate
                     off
                       "the dscheck test of the jobs kills this mutant, and \
                        dscheck is not in the mutation run"] ->
                Hashtbl.remove t.externals id ;
                Some e
            | Some e ->
                e.arrived <- true ;
                Some e
            | None ->
                None )
      in
      match e with
      | Some e -> (
          Option.iter
            (fun f ->
              dom.held <- dom.held - 1 ;
              f () )
            e.on_done ;
          match e.ewait with
          | Some (s, i) -> (
            match C.notify s.word i with C.Resume -> resume dom s | _ -> () )
          | None ->
              () )
      | None ->
          () )
  | World.Ready.Alarm | World.Ready.Woken ->
      ()

(* The cancel contexts (design section 2.5, How a cancel reaches a fiber, and
   spec/tla/Scope.tla). *)

let next_nid = Atomic.make 0

let no_handler = {handle= (fun f -> f ())}

(* A nested scope composes its handler inside the chain of its parent. *)
let new_node ?parent ?handler kind =
  let outer = match parent with Some p -> p.chain | None -> no_handler in
  let chain =
    match handler with
    | None ->
        outer
    | Some h ->
        {handle= (fun f -> outer.handle (fun () -> h.handle f))}
  in
  { nid= Atomic.fetch_and_add next_nid 1
  ; kind
  ; parent
  ; flag= Atomic.make None
  ; lock= Spin.make ()
  ; members= Hashtbl.create ~random:false 4
  ; children= Hashtbl.create ~random:false 4
  ; chain }

let add_member_to n f =
  Spin.with_lock n.lock (fun () -> Hashtbl.replace n.members f.fid f)

let remove_member n f =
  Spin.with_lock n.lock (fun () -> Hashtbl.remove n.members f.fid)

(* The values of a table, in the order of their ids: the fork order. *)
let by_id tbl =
  Hashtbl.fold (fun id v acc -> (id, v) :: acc) tbl []
  |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
  |> List.map snd

(* The first cancelled context on the chain from [c], up to a shield. *)
let rec first_flag c =
  match c.kind with
  | Shield_node o when not (Atomic.get o) ->
      None
  | Shield_node _ ->
      Option.bind c.parent first_flag
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
  | Shield_node o when not (Atomic.get o) ->
      false
  | Shield_node _ ->
      Option.fold ~none:false ~some:(fun p -> on_chain p x) c.parent
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

(* The PRNG of a domain: SplitMix64 on 63 bits. *)
let next_rand dom =
  dom.rng <-
    (dom.rng + 0x1E3779B97F4A7C15) [@mutate off "a constant of the PRNG"] ;
  let z = dom.rng in
  let z = z lxor (z lsr 30) * 0x3F58476D1CE4E5B9 in
  let z = z lxor (z lsr 27) * 0x14D049BB133111EB in
  z lxor (z lsr 31) land max_int

(* The order of the scan, from the PRNG of the domain: a random order when more
   than one branch is ready (design section 2.4). *)
let shuffle dom n =
  let a = Array.init n Fun.id in
  for i = n - 1 downto 1 do
    let j = next_rand dom mod (i + 1) in
    let x = a.(i) in
    a.(i) <- a.(j) ;
    a.(j) <- x
  done ;
  a

let rec run_fiber : dom -> fiber -> step -> unit =
 fun dom fiber step ->
  flush_wake dom ;
  match step with
  | Finished ->
      unregister dom.sched fiber
  | Suspended (Self, k) ->
      run_fiber dom fiber (Effect.Deep.continue k fiber)
  | Suspended (Dom_index, k) ->
      run_fiber dom fiber (Effect.Deep.continue k dom.index)
  | Suspended (Fork (f, body), k) ->
      register dom.sched f ;
      push dom f (start_fiber body) ;
      (* Wake an idle domain before the fiber goes on, so that it can take the
         new fiber at once. *)
      flush_wake dom ;
      run_fiber dom fiber (Effect.Deep.continue k ())
  | Suspended (Job (job, on_done), k) ->
      run_fiber dom fiber (Effect.Deep.continue k (start_job dom job on_done))
  | Suspended (Detach_job id, k) ->
      drop_external dom.sched id ;
      ignore (World.perform "job" (World.Submit (Op.Detach id))) ;
      run_fiber dom fiber (Effect.Deep.continue k ())
  | Suspended (Close_notices fdr, k) ->
      close_notices dom fdr ;
      flush_wake dom ;
      run_fiber dom fiber (Effect.Deep.continue k ())
  | Suspended (Enqueue l, k) ->
      List.iter (resume dom) l ;
      flush_wake dom ;
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
  let order = shuffle dom n in
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
let k_poll = ref 61

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

(* A poll or a sleep. The time can pass after the last poll: if the mailbox or
   an expired entry made a fiber ready, the domain polls instead of a sleep. *)
let wait dom timeout =
  take_mail dom ;
  expire dom ;
  flush_wake dom ;
  let timeout = if Ring.is_empty dom.ring then timeout else World.Wait.Poll in
  let w = {World.Wait.timeout; deadline= alarm dom} in
  List.iter (on_ready dom) (World.perform "sched" (World.Wait (dom.index, w))) ;
  take_mail dom ;
  expire dom ;
  flush_wake dom

let take_overflow t =
  Spin.with_lock t.overflow_lock (fun () -> Queue.take_opt t.overflow)

(* A steal: try each other domain one time, from a start that the PRNG of the
   domain selects. *)
let steal dom =
  let doms = dom.sched.doms in
  let n = Array.length doms in
  if n = 1 then None
  else
    let start = next_rand dom mod n in
    let rec try_ i =
      if i = n then None
      else
        let v = doms.((start + i) mod n) in
        if v == dom then try_ (i + 1)
        else
          match Ring.steal ~from:v.ring ~into:dom.ring with
          | Some r ->
              (* The thief has more fibers in its ring: it wakes one more idle
                 domain, as Go does. *)
              if
                ( (not (Ring.is_empty dom.ring))
                && Atomic.get dom.sched.idle <> 0 )
                [@mutate off "the wake chain gives only parallelism"]
              then dom.wake <- true ;
              Some r
          | None ->
              try_ (i + 1)
    in
    try_ 0

(* Steps 2 to 4 of the loop: the local queue, the overflow queue, then a steal.
   Each 61st take tries the overflow queue first, so that its fibers do not
   starve while the ring is never empty (Go does the same). *)
let take dom =
  dom.ticks <- dom.ticks + 1 ;
  let first =
    if
      ( dom.ticks >= 61 )
      [@mutate off "61 or 62 takes: 61 is a start value, as in Go"]
    then (
      dom.ticks <- 0 ;
      take_overflow dom.sched )
    else None
  in
  match first with
  | Some r ->
      Some r
  | None -> (
    match Ring.pop dom.ring with
    | Some r ->
        Some r
    | None -> (
      match take_overflow dom.sched with Some r -> Some r | None -> steal dom )
    )

(* The second check of a domain that is in [idle]: did [main] end, or is there a
   fiber in a queue that it can take? The check needs [finished]: a poll in the
   same turn of the loop can take the wake of the end. It needs no check of the
   mailbox: [wait] takes the mailbox after each poll. *)
let work_for dom =
  let t = dom.sched in
  Atomic.get t.finished
  ||
  if Array.exists (fun d -> not (Ring.is_empty d.ring)) t.doms then true
  else
    Spin.with_lock t.overflow_lock (fun () -> not (Queue.is_empty t.overflow))
    [@mutate
      off
        "a lost steal of the overflow queue costs only parallelism: the domain \
         that filled it takes it when its ring is empty"]

let run_ready dom r =
  r.rfiber.queued <- false ;
  dom.switches <- dom.switches + 1 ;
  if
    ( dom.switches >= !k_poll )
    [@mutate off "61 or 62 switches: k_poll is a start value"]
  then (
    dom.switches <- 0 ;
    dom.stats.busy_polls <- dom.stats.busy_polls + 1 ;
    wait dom World.Wait.Poll ) ;
  if
    dom.sched
      .simulated
      [@mutate
        off
          "production handles Switch with no action, and in the simulator one \
           switch point more or less changes only the order that the seed \
           selects"]
  then World.perform "sched" World.Switch ;
  run_fiber dom r.rfiber (r.run ()) ;
  flush_wake dom

(* The loop of one domain. It ends when [main] ended. Each close came before the
   end of [main], because the root scope waited for each fiber. Thus the domain
   takes its last close notices when it leaves the loop, and each registration
   releases its user. A job that a fiber detached can still hold the user of an
   fd: the domain waits for its completion, so that the fd closes. *)
let rec loop dom =
  let t = dom.sched in
  if Atomic.get t.finished then (
    take_mail dom ;
    if dom.held > 0 then (wait dom World.Wait.Sleep ; loop dom) )
  else (
    ( match take dom with
    | Some r ->
        run_ready dom r
    | None ->
        wait dom World.Wait.Poll ;
        (* Idle: add the domain to [idle], then check again (Wake.tla). The
           check sees a fiber that the poll made ready, and the end. *)
        Idle.add t dom.index ;
        if not (work_for dom) then wait dom World.Wait.Sleep ;
        ignore (Idle.remove t dom.index) ) ;
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
  | Shield_node _ | Daemon_node ->
      assert false (* Only a scope node has members that end. *)

(* A failed member stores its exception before it ends, then it cancels the
   scope. A [Cancelled] is not a failure. It gives the syncs to resume. *)
let fail node e bt =
  match e with
  | Cancelled _ ->
      []
  | _ ->
      let st = state_of node in
      let rec store () =
        let l = Atomic.get st.failures in
        if not (Atomic.compare_and_set st.failures l ((e, bt) :: l)) then
          store ()
      in
      store () ; cancel_node node e

(* The end of a member: one CAS on the counts. The end of the last member sets
   the closed bit and fills the join. When the last member that is not a daemon
   ends, the scope cancels its daemons. It gives the syncs to resume. *)
let member_end node ~daemon =
  let st = state_of node in
  let rec dec () =
    let c = Atomic.get st.counts in
    let c' = c - all_one - if daemon then 0 else nd_one in
    let c' = if all_count c' = 0 then c' lor closed_bit else c' in
    if Atomic.compare_and_set st.counts c c' then c' else dec ()
  in
  let c = dec () in
  if all_count c = 0 then Option.value ~default:[] (Iv.fill st.join ())
  else if (not daemon) && nd_count c = 0 then
    let ds = Spin.with_lock node.lock (fun () -> by_id st.daemons) in
    List.concat_map (fun d -> cancel_node d Daemons_end) ds
  else []

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

(* The body of a fiber runs inside the chain of handlers of its scope. *)
let run_body scope body =
  match scope.chain.handle body with
  | () ->
      ()
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      enqueue (fail scope e bt)

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
           enqueue (member_end scope ~daemon:false) ) )

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
           enqueue (member_end scope ~daemon:true) ) )

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
  (match result with Error (e, bt) -> enqueue (fail node e bt) | Ok _ -> ()) ;
  remove_member node fiber ;
  enqueue (member_end node ~daemon:false) ;
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

(* The body runs inside [handler] only: the fiber already runs inside the chain
   of its outer scopes. *)
let scope ?handler f =
  let fiber = self () in
  let parent = List.hd fiber.ctxs in
  let node = new_node ~parent ?handler (Scope_node (new_scope_state ())) in
  Hashtbl.replace node.members fiber.fid fiber ;
  Spin.with_lock parent.lock (fun () ->
      Hashtbl.replace parent.children node.nid node ) ;
  fiber.ctxs <- node :: fiber.ctxs ;
  let result =
    match
      match handler with
      | None ->
          f node
      | Some h ->
          h.handle (fun () -> f node)
    with
    | v ->
        Ok v
    | exception e ->
        Error (e, Printexc.get_raw_backtrace ())
  in
  fiber.ctxs <- List.tl fiber.ctxs ;
  finish_scope fiber node result

(* [f] gets the shield node. *)
let shield_node f =
  let fiber = self () in
  let node =
    new_node ~parent:(List.hd fiber.ctxs) (Shield_node (Atomic.make false))
  in
  fiber.ctxs <- node :: fiber.ctxs ;
  let pop () = fiber.ctxs <- List.tl fiber.ctxs in
  match f node with
  | v ->
      pop () ; v
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      pop () ;
      Printexc.raise_with_backtrace e bt

let shield f = shield_node (fun _ -> f ())

(* A cancellation point with no wait: it raises the cancel of the chain. *)
let check_cancel () =
  match first_flag (List.hd (self ()).ctxs) with
  | Some r ->
      raise (Cancelled r)
  | None ->
      ()

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

(* The scheduler of [domains] domains. *)
let create ?(slack = Span.ms 1) ?(simulated = false) ~domains () =
  if domains < 1 || domains > 62 then
    invalid_arg "run: the number of domains is not in the range 1 to 62" ;
  let t =
    { doms= [||]
    ; slack= Span.to_ns slack
    ; fibers= Hashtbl.create ~random:false 64
    ; fibers_lock= Spin.make ()
    ; simulated
    ; finished= Atomic.make false
    ; idle= Atomic.make 0
    ; overflow= Queue.create ()
    ; overflow_lock= Spin.make ()
    ; externals= Hashtbl.create ~random:false 8
    ; externals_lock= Spin.make ()
    ; signals= Hashtbl.create ~random:false 4
    ; signals_lock= Spin.make () }
  in
  t.doms <-
    Array.init domains (fun index ->
        { index
        ; ring= Ring.create ()
        ; heap= Heap.create ()
        ; ids= Commit.Id.gen ~domain:index ~max_domains:domains
        ; sched= t
        ; switches= 0
        ; ticks= 0
        ; rng= 0
        ; wake= false
        ; stats= {wakes= 0; edges= 0; idle_edges= 0; busy_polls= 0}
        ; held= 0
        ; regs= Hashtbl.create ~random:false 64
        ; mailbox= Atomic.make [] } ) ;
  t

(* Put [main] as the first fiber on domain 0, in the root scope. When [main]
   returns, the root scope cancels the other fibers and waits for them. Then the
   end of [main] wakes each domain, so that each loop ends. *)
let start t main =
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
    let r =
      match finish_scope fiber root r with
      | v ->
          fun () -> v
      | exception e ->
          let bt = Printexc.get_raw_backtrace () in
          fun () -> Printexc.raise_with_backtrace e bt
    in
    result := Some r ;
    Atomic.set t.finished true ;
    Array.iter (fun d -> World.perform "sched" (World.Wake d.index)) t.doms
  in
  register t fiber ;
  push t.doms.(0) fiber (start_fiber body) ;
  (* The value of [main], or its exception. *)
  fun () -> match !result with Some r -> r () | None -> assert false

(* Run the loop of a domain. A bug in the scheduler stops the program. *)
let run_domain dom =
  try
    dom.rng <- World.perform "sched" (World.Random (1 lsl 30)) ;
    loop dom
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    Atomic.set dom.sched.finished true ;
    raise (Internal_error (e, bt))
