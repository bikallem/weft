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
  ; mutable queued: bool  (** The fiber is in a queue. For [Lost_wakeup]. *)
  ; pinned: bool  (** The fiber runs only on domain 0 (M3). *) }

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
            first. Each fiber of the node runs inside it. *)
  ; fids: int Atomic.t
        (** The next fiber id. All nodes of one run share it (design section
            2.5, The id of a fiber). *) }

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

(* [Empty (run, waiters)]: [run] is the id of the run that created the Ivar or
   that last added a waiter, or -1 (no run). A fill from outside a fiber uses it
   to drop a fill of a run that ended (design section 2.5, A value from outside
   a fiber). *)
and 'a ivar_state = Empty of int * waiters | Full of 'a

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
            share the word. *)
  ; mutable streak: int
        (** The bytes of the reads with no wait in sequence (M3). Only a hint,
            so a race between two domains does no harm. *)
  ; fkind: fd_kind }

(* The operations that an fd takes. Only a UDP socket takes [sendto] and
   [recvfrom]. An end of a pipe has one direction, also on FreeBSD, where a pipe
   goes in the two directions. *)
and fd_kind = Any | Udp | Read_end | Write_end

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
  | Clear_ready : fdrec * bool -> unit req
      (** An operation with no wait got [EAGAIN]: clear the read flag, or the
          write flag with [true], of the registration of this domain. *)
  | Job : 'a Op.blocking * (unit -> unit) option -> (int, Op.error) result req
      (** Start a job of the pool for the domain that runs the fiber. The
          function runs at its completion. *)
  | Detach_job : int -> unit req
      (** The fiber does not wait for the job any more. *)
  | Unwatch_signal : Op.signal -> int req
      (** The world stopped the watch of the signal. Each waiter raises, and the
          count that no fiber took goes to the fiber. *)
  | Cancel_at : int64 * ctx -> C.t req
      (** The deadline of [with_timeout]: an entry in the heap of the domain
          that cancels the node. The word is the state of the entry. *)

and sync_req = {leaves: leaf array; nocancel: bool}

(** The part of a base event that the scheduler runs in the scan, with the
    branch index. It commits the branch if it is ready, or it registers an
    offer. *)
and leaf = dom -> sync -> int -> unit

and dom =
  { index: int
  ; ring: ready Ring.t  (** The local queue. *)
  ; next: ready option Atomic.t
        (** The next slot: a fiber that a fiber of this domain woke. The owner
            puts and takes, and a thief takes with a CAS (M3). *)
  ; mutable next_runs: int  (** The runs from the next slot in sequence. *)
  ; mutable pinned_runs: int
        (** On domain 0: the runs from the pinned queue in sequence. *)
  ; heap: heap
  ; pheap: heap  (** The [Posix] deadlines (M3). *)
  ; ids: Commit.Id.gen
  ; sched: t
  ; mutable switches: int
  ; mutable ticks: int
        (** The takes since the last take of the overflow queue. *)
  ; mutable rng: int
        (** The PRNG of the scan order and of the steal. The world gives its
            seed when the domain starts. *)
  ; mutable wake: bool  (** A push to do a wake at the next safe point. *)
  ; mutable wake0: bool
        (** A push to the pinned queue: wake domain 0 at the next safe point. *)
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
  ; mutable busy_polls: int  (** The polls of a busy domain. *)
  ; mutable resumes: int
        (** The fibers that a commit resumed: progress for [Livelock]. *) }

and t =
  { run_id: int  (** The id of the run. *)
  ; beats: (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
        (** The beat of each domain, out of the OCaml heap: one more at the
            start of a fiber and one more at its end, so an odd value tells that
            a fiber runs. The watchdog of a world reads it (M3). *)
  ; mutable doms: dom array
  ; slack: int64
  ; fibers: (int, fiber) Hashtbl.t  (** The live fibers, for the debug. *)
  ; fibers_lock: Spin.t
  ; simulated: bool
        (** The simulator: it needs a switch point at each fiber, and the table
            of fibers for its checks. Production has neither. *)
  ; finished: bool Atomic.t
  ; idle: int Atomic.t
        (** The bit of each domain that sleeps, or is about to. *)
  ; searching: int Atomic.t
        (** The domains that steal at this time (the Go rules, M3). *)
  ; pinned_q: ready Queue.t
        (** The pinned fibers: only domain 0 takes them. Under its lock. *)
  ; pinned_lock: Spin.t
  ; pinned_n: int Atomic.t
        (** The length of the pinned queue: domain 0 reads it with no lock. *)
  ; unawaited: int Atomic.t
        (** The promises of the run that failed, with no fiber that got the
            exception. The simulator forces a GC at its end only if it is not 0.
        *)
  ; inject: (dom -> unit) list Atomic.t
        (** The global injection queue: the fills from outside a fiber, the
            newest first. It is lock-free, because a finaliser can push to it
            (M3). *)
  ; overflow: ready Queue.t  (** The fibers of full rings, under its lock. *)
  ; overflow_lock: Spin.t
  ; externals: (int, ext) Hashtbl.t
        (** The completions of [Ready.External], by id, under its lock. *)
  ; externals_lock: Spin.t
  ; signals: (Op.signal, sigstate) Hashtbl.t
        (** The count and the waiters of each signal, under its lock. *)
  ; signals_lock: Spin.t }

(* A signal (design section 2.7, Signals). A commit of a waiter takes the count,
   and it puts the count in the cell of the waiter before the commit. After an
   unwatch, the cell gets -1, and the waiter raises. *)
and sigstate =
  { mutable count: int
  ; mutable swait: (sync * int * int ref) list  (** The newest first. *)
  ; mutable unwatched: bool  (** After [Signal.unwatch]. *) }

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

and entry =
  { deadline: int64
  ; seq: int
  ; esync: sync
  ; branch: int
  ; cancels: ctx option
        (** The deadline of [with_timeout] (M3): at the deadline, the owner
            cancels this node. [esync] is then only a state word: [Waiting]
            while the entry is live, [Done 1] after the caller ended it, and
            [Done 0] after the owner fired it. *) }

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
  let push ?cancels h ~deadline esync branch =
    let e = {deadline; seq= h.next_seq; esync; branch; cancels} in
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

(* The run that takes the fills from outside a fiber: its scheduler, and the
   function that gives a request to it (design section 2.5, A value from outside
   a fiber). [run] sets it. The simulator gives the requests to its own
   queue. *)
let outside : (t * ((dom -> unit) -> unit)) option Atomic.t = Atomic.make None

(* The tag of an Ivar that no run made. *)
let no_run = -1

let active_run () =
  match Atomic.get outside with Some (t, _) -> t.run_id | None -> no_run

(* The atomic register of an Ivar (design section 2.4, spec/tla/Ivar.tla). *)
module Iv = struct
  let make () : 'a ivar = Atomic.make (Empty (active_run (), Waiters.empty))

  (* The scan of a reader of [run]: a poll, then a register with a CAS on
     Empty. *)
  let rec register (iv : 'a ivar) run s i =
    match Atomic.get iv with
    | Full _ ->
        ignore (C.self_commit s.word i)
    | Empty (_, w) as old ->
        if not (Atomic.compare_and_set iv old (Empty (run, Waiters.add s i w)))
        then register iv run s i

  (* [fill iv v] gives [Some resumed] with the syncs to resume, or [None] if the
     Ivar is full. *)
  let rec fill (iv : 'a ivar) v =
    match Atomic.get iv with
    | Full _ ->
        None
    | Empty (_, w) as old ->
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

  let leaf iv : leaf = fun dom s i -> register iv dom.sched.run_id s i
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

  let alloc ?(kind = Any) os =
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
        { slot
        ; gen
        ; os
        ; rc= Atomic.make (Open 0)
        ; doms= Atomic.make 0
        ; streak= 0
        ; fkind= kind } )

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

(* A fiber gets the next id of the nodes of its run. Thus the ids of a run start
   at 0, and in the simulator a seed gives the same ids. A fiber with no name
   has the name "": [Sched_debug] names it. *)
let new_fiber ?(name = "") ?(pinned = false) ctxs =
  let fid = Atomic.fetch_and_add (List.hd ctxs).fids 1 in
  {fid; name; ctxs; current= Atomic.make None; queued= false; pinned}

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

  (* Take one domain out of the set, the lowest in [mask], and give it. *)
  let rec take_in t mask =
    let v = Atomic.get t.idle land mask in
    if v = 0 then None
    else
      let d =
        let rec low i = if v land bit i <> 0 then i else low (i + 1) in
        low 0
      in
      if remove t d then Some d else take_in t mask

  (* The lowest idle domain that is not [self]. *)
  let take t ~self = take_in t (lnot (bit self))

  let take_any t = take_in t (-1)
end

(* A push to the local queue. A full ring moves its older half and the new fiber
   to the overflow queue, as Go does, so the order stays FIFO. If a domain is
   idle, the push marks a wake: the scheduler does it at the next safe point,
   because a wake is a world effect, and a lock can be held here. *)
(* A pinned fiber goes to the pinned queue, and the push wakes domain 0
   directly: another domain cannot run it (Wake.tla). The count comes after the
   push, and domain 0 reads it after it adds itself to [idle]: a Dekker pair
   with the wake. *)
let push_pinned dom r =
  let t = dom.sched in
  Spin.with_lock t.pinned_lock (fun () -> Queue.push r t.pinned_q) ;
  Atomic.incr t.pinned_n ;
  if
    ( dom.index <> 0 )
    [@mutate
      off
        "the dscheck test of a pinned push kills it; dscheck is not in the \
         mutation run, and in the simulator the wake of a timer wakes domain 0 \
         too"]
  then dom.wake0 <- true

let has_pinned dom = dom.index = 0 && Atomic.get dom.sched.pinned_n > 0

(* A push wakes an idle domain, but not while a domain searches: that domain
   finds the fiber (the Go rules). *)
let mark_wake dom =
  let t = dom.sched in
  if Atomic.get t.idle <> 0 && Atomic.get t.searching = 0 then dom.wake <- true

let push_ready dom r =
  r.rfiber.queued <- true ;
  let t = dom.sched in
  if r.rfiber.pinned then push_pinned dom r
  else (
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
    mark_wake dom )

let push dom f run = push_ready dom {rfiber= f; run}

(* A fiber that a fiber of [dom] woke goes to the next slot, and the fiber that
   was in the slot goes to the tail (design section 2.5, Where a woken fiber
   goes). A thief can take the slot, so the push wakes an idle domain by the
   normal rule. *)
let push_next dom f run =
  if f.pinned then push dom f run
  else (
    f.queued <- true ;
    Option.iter (push_ready dom)
      (Atomic.exchange dom.next (Some {rfiber= f; run})) ;
    mark_wake dom )

(* Wake one idle domain if a push marked it. No lock is held here. *)
let flush_wake dom =
  if dom.wake0 then (
    dom.wake0 <- false ;
    if Idle.remove dom.sched 0 then (
      dom.stats.wakes <-
        (dom.stats.wakes + 1) [@mutate off "a counter of the measurements"] ;
      World.perform "sched" (World.Wake 0) ) ) ;
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
let resume ?(next = false) dom s =
  let i = match C.state s.word with C.Done i -> i | _ -> assert false in
  let k = match s.k with Some k -> k | None -> assert false in
  s.k <- None ;
  dom.stats.resumes <- dom.stats.resumes + 1 ;
  (if next then push_next else push) dom s.owner (fun () ->
      Atomic.set s.owner.current None
      [@mutate
        off
          "the next sync of the fiber replaces a Done sync, and each reader of \
           current ignores a Done sync"] ;
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
      | ((Some e)
         [@mutate
           off
             "only the domain of the job takes its completion, and it does not \
              poll between the start of the job and the wait or the detach"] )
        when e.arrived ->
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
      | ((Some e)
         [@mutate
           off
             "only the domain of the job takes its completion, and it does not \
              poll between the start of the job and the wait or the detach"] )
        when e.arrived ->
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
      let st = {count= 0; swait= []; unwatched= false} in
      Hashtbl.replace t.signals sg st ;
      st

(* A waiter of [received sg]: it takes the count if the count is not 0. If not,
   it waits, and the waiters with a [Done] sync go. After an unwatch, it commits
   with -1 in the cell. *)
let signal_leaf sg cell : leaf =
 fun dom s i ->
  let t = dom.sched in
  Spin.with_lock t.signals_lock (fun () ->
      let st = sigstate t sg in
      if st.unwatched then (
        cell := -1 ;
        ignore (C.self_commit s.word i) )
      else if st.count > 0 then (
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

(* [n] more of [sg]: the oldest waiter that can commit takes the count. After an
   unwatch, the world sends the signal again, and it gets its action (design
   section 2.7, The end of a watch). *)
let deliver dom sg n =
  let t = dom.sched in
  let unwatched, resumed =
    Spin.with_lock t.signals_lock (fun () ->
        let st = sigstate t sg in
        if st.unwatched then (true, [])
        else (
          st.count <- st.count + n ;
          let rec give resumed = function
            | [] ->
                (resumed, [])
            | (ws
               [@mutate
                 off
                   "each world gives a count of 1 or more, and the give stops \
                    at the first commit, so the count is not 0 here"] )
              when st.count = 0 ->
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
          (false, resumed) ) )
  in
  if unwatched then ignore (World.perform "sched" (World.Submit (Op.Resend sg)))
  else List.iter (resume dom) resumed

(* The end of the watch of [sg], after the world stopped it. Each waiter commits
   with -1, and the count that no fiber took goes to the caller. A later
   [deliver] sends the signal again. *)
let unwatch_signal dom sg =
  let t = dom.sched in
  let n, resumed =
    Spin.with_lock t.signals_lock (fun () ->
        let st = sigstate t sg in
        let n = st.count in
        st.unwatched <- true ;
        st.count <- 0 ;
        let resumed =
          List.filter_map
            (fun (s, i, cell) ->
              cell := -1 ;
              match C.notify s.word i with
              | C.Resume ->
                  Some s
              | C.In_scan | C.Lost _ ->
                  None )
            st.swait
        in
        st.swait <- [] ;
        (n, resumed) )
  in
  List.iter (resume dom) resumed ;
  n

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
  | World.Ready.Alarm | World.Ready.Posix_alarm | World.Ready.Woken ->
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
  ; chain
  ; fids= (match parent with Some p -> p.fids | None -> Atomic.make 0) }

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
let one = [|0|]

let shuffle dom n =
  if n = 1 then one
  else
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
  | Suspended (Cancel_at (deadline, node), k) ->
      let s =
        { word= C.make (Commit.Id.next dom.ids)
        ; owner= fiber
        ; sctx= node
        ; cancellable= false
        ; k= None }
      in
      Heap.push dom.heap ~cancels:node ~deadline s 0 ;
      run_fiber dom fiber (Effect.Deep.continue k s.word)
  | Suspended (Unwatch_signal sg, k) ->
      let n = unwatch_signal dom sg in
      flush_wake dom ;
      run_fiber dom fiber (Effect.Deep.continue k n)
  | Suspended (Detach_job id, k) ->
      drop_external dom.sched id ;
      ignore (World.perform "job" (World.Submit (Op.Detach id))) ;
      run_fiber dom fiber (Effect.Deep.continue k ())
  | Suspended (Clear_ready (fdr, write), k) ->
      Option.iter
        (fun reg -> if write then reg.wflag <- false else reg.rflag <- false)
        (live_reg dom fdr) ;
      run_fiber dom fiber (Effect.Deep.continue k ())
  | Suspended (Close_notices fdr, k) ->
      close_notices dom fdr ;
      flush_wake dom ;
      run_fiber dom fiber (Effect.Deep.continue k ())
  | Suspended (Enqueue l, k) ->
      (* One fiber that this fiber woke goes to the next slot, many go to the
         tail. *)
      ( match l with
      | [s] ->
          resume ~next:true dom s
      | l ->
          List.iter (resume dom) l ) ;
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
  let j = ref 0 in
  while !j < n && not (is_done s) do
    r.leaves.(order.(!j)) dom s order.(!j) ;
    incr j
  done ;
  s.k <- Some k ;
  match C.park s.word with
  | `Parked ->
      ()
  | `Done i ->
      s.k <- None ;
      Atomic.set fiber.current None
      [@mutate
        off
          "the next sync of the fiber replaces a Done sync, and each reader of \
           current ignores a Done sync"] ;
      run_fiber dom fiber (Effect.Deep.continue k i)

(* The loop of one domain (design section 2.5). *)

(* A busy domain polls each [k_poll] switches (open question 3). *)
let k_poll = ref 61

let now_ns () = Clock.to_ns Mono (World.perform "sched" (World.Now Mono))

let posix_ns () = Clock.to_ns Posix (World.perform "sched" (World.Now Posix))

(* Expire the entries up to now. A timer never fires before its deadline: the
   slack only rounds the alarm up, so the deadlines in one slot of the slack
   share one alarm. *)
let expire_heap dom heap limit =
  let rec loop () =
    match Heap.top heap with
    | Some e when Int64.compare e.deadline limit <= 0 ->
        Heap.pop heap ;
        ( match (C.notify e.esync.word e.branch, e.cancels) with
        | C.Resume, _ ->
            resume dom e.esync
        | C.In_scan, Some node ->
            (* The deadline of [with_timeout] came first. *)
            List.iter (resume dom) (cancel_node node Timeout)
        | C.In_scan, None | C.Lost _, _ ->
            () ) ;
        loop ()
    | _ ->
        ()
  in
  loop ()

(* The alarm: the earliest live deadline, rounded up to the slack. The rounding
   saturates at the largest time. *)
(* The [Posix] heap reads [Now Posix] only when it holds an entry. *)
let expire dom =
  expire_heap dom dom.heap (now_ns ()) ;
  if dom.pheap.size > 0 then expire_heap dom dom.pheap (posix_ns ())

let alarm_of dom heap =
  Heap.drop_stale heap ;
  match Heap.top heap with
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
      Some d

let alarm dom = Option.map (Clock.of_ns Mono) (alarm_of dom dom.heap)

let posix_alarm dom = Option.map (Clock.of_ns Posix) (alarm_of dom dom.pheap)

(* A poll or a sleep. The time can pass after the last poll: if the mailbox or
   an expired entry made a fiber ready, the domain polls instead of a sleep. *)
let wait dom timeout =
  take_mail dom
  [@mutate
    off
      "a close notice comes with a wake, so this wait ends at once, and the \
       take after it gets the notice"] ;
  expire dom ;
  flush_wake dom ;
  (* The expire can resume a pinned fiber on domain 0 with no wake. *)
  let timeout =
    if
      Ring.is_empty dom.ring
      && Option.is_none (Atomic.get dom.next)
      && not (has_pinned dom)
    then timeout
    else World.Wait.Poll
  in
  let w =
    {World.Wait.timeout; deadline= alarm dom; posix_deadline= posix_alarm dom}
  in
  List.iter (on_ready dom) (World.perform "sched" (World.Wait (dom.index, w))) ;
  take_mail dom
  [@mutate
    off
      "the take at the start of the next wait gets the notice, and this domain \
       does not sleep before that take"] ;
  expire dom ;
  flush_wake dom

let take_overflow t =
  Spin.with_lock t.overflow_lock (fun () -> Queue.take_opt t.overflow)

(* A steal: try each other domain one time, from a start that the PRNG of the
   domain selects. *)
(* A domain searches only if fewer than half of the domains search (at least
   one). It gives [false] if it cannot search. *)
let start_search (t : t) =
  let limit = Int.max 1 (Array.length t.doms / 2) in
  let rec inc () =
    let s = Atomic.get t.searching in
    ( s < limit )
    [@mutate
      off "the limit gives only performance: Wake.tla passes with no limit"]
    && (Atomic.compare_and_set t.searching s (s + 1) || inc ())
  in
  inc ()

(* The steal of the Go rules (design section 2.5, Idle and wake, and
   spec/tla/Wake.tla). A searching domain that finds no work decreases
   [searching] before its second check. The last searching domain that finds
   work wakes one idle domain, because the pushes woke nobody while it
   searched. *)
let steal dom =
  let t = dom.sched in
  let doms = t.doms in
  let n = Array.length doms in
  if n = 1 || not (start_search t) then None
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
              Some r
          | None ->
              try_ (i + 1)
    in
    (* Then the next slots, with a CAS: a fiber that waits behind a long
       computation of the fiber that woke it can go on. *)
    let rec next_of i =
      if i = n then None
      else
        let v = doms.((start + i) mod n) in
        match Atomic.get v.next with
        | Some r as cur when v != dom && Atomic.compare_and_set v.next cur None
          ->
            Some r
        | _ ->
            next_of (i + 1)
    in
    let r = match try_ 0 with Some r -> Some r | None -> next_of 0 in
    if
      ( Atomic.fetch_and_add t.searching (-1)
      = 1 )
      [@mutate
        off
          "the last wake gives only performance: Wake.tla passes with no last \
           wake"]
      && Option.is_some r
    then
      if
        ( Atomic.get t.idle <> 0 )
        [@mutate
          off
            "the last wake gives only performance: Wake.tla passes with no \
             last wake"]
      then dom.wake <- true ;
    r

(* The next slot runs at most [next_limit] times in sequence. Then its fiber
   goes to the tail, so that two fibers that wake each other do not starve the
   local queue (open question 5). The pinned queue has the same limit. *)
let next_limit = 3

let take_next dom =
  match
    match Atomic.get dom.next with
    | None ->
        None
    | Some _ ->
        Atomic.exchange dom.next None
  with
  | None ->
      dom.next_runs <- 0 ;
      None
  | Some r ->
      if dom.next_runs < next_limit then (
        dom.next_runs <- dom.next_runs + 1 ;
        Some r )
      else (
        dom.next_runs <- 0 ;
        push_ready dom r ;
        None )

(* Steps 2 to 4 of the loop: the local queue, the overflow queue, then a steal.
   Each 61st take tries the overflow queue first, so that its fibers do not
   starve while the ring is never empty (Go does the same). *)
(* The requests of the injection queue, the oldest first: each one fills an
   Ivar on this domain. *)
let take_injected dom =
  let t = dom.sched in
  if Atomic.get t.inject <> [] then
    List.iter (fun req -> req dom) (List.rev (Atomic.exchange t.inject []))

let take_pinned dom =
  if not (has_pinned dom) then None
  else
    let t = dom.sched in
    let r =
      Spin.with_lock t.pinned_lock (fun () -> Queue.take_opt t.pinned_q)
    in
    if
      Option.is_some r
      [@mutate off "a take that finds no fiber corrects the count"]
    then Atomic.decr t.pinned_n ;
    r

(* Domain 0 takes the pinned queue first, but at most [next_limit] times in
   sequence: a pinned fiber that yields must not starve the local queue. *)
let take_pinned_first dom =
  if dom.pinned_runs >= next_limit then (
    dom.pinned_runs <- 0 ;
    None )
  else
    match take_pinned dom with
    | Some r ->
        dom.pinned_runs <- dom.pinned_runs + 1 ;
        Some r
    | None ->
        dom.pinned_runs <- 0 ;
        None

let take dom =
  dom.ticks <- dom.ticks + 1 ;
  let first =
    if
      ( dom.ticks >= 61 )
      [@mutate off "61 or 62 takes: 61 is a start value, as in Go"]
    then (
      dom.ticks <- 0 ;
      take_injected dom ;
      take_overflow dom.sched )
    else None
  in
  match match first with Some r -> Some r | None -> take_pinned_first dom with
  | Some r ->
      Some r
  | None -> (
    match take_next dom with
    | Some r ->
        Some r
    | None -> (
      match Ring.pop dom.ring with
      | Some r ->
          Some r
      | None -> (
        match take_overflow dom.sched with
        | Some r ->
            Some r
        | None -> (
            (* The fills from outside resume their fibers to this domain. *)
            take_injected dom ;
            match Ring.pop dom.ring with
            | Some r ->
                Some r
            | None -> (
              match take_pinned dom with Some r -> Some r | None -> steal dom )
            ) ) ) )

(* The second check of a domain that is in [idle]: did [main] end, or is there a
   fiber in a queue that it can take? The check needs [finished]: a poll in the
   same turn of the loop can take the wake of the end. It needs no check of the
   mailbox: [wait] takes the mailbox after each poll. *)
let work_for dom =
  let t = dom.sched in
  Atomic.get t.finished
  || Atomic.get t.inject <> []
  || has_pinned dom
  || Array.exists
       (fun d ->
         (not (Ring.is_empty d.ring)) || Option.is_some (Atomic.get d.next) )
       t.doms
  || (Spin.with_lock t.overflow_lock (fun () ->
          not (Queue.is_empty t.overflow) )
  [@mutate
    off
      "a lost steal of the overflow queue costs only parallelism: the domain \
       that filled it takes it when its ring is empty"] )

(* Only the domain writes its beat. *)
let beat dom =
  let b = dom.sched.beats in
  Bigarray.Array1.unsafe_set b dom.index
    (Bigarray.Array1.unsafe_get b dom.index + 1)

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
  beat dom ;
  run_fiber dom r.rfiber (r.run ()) ;
  beat dom ;
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

let enqueue = function
  | ([]
     [@mutate
       off
         "each caller is in a fiber, and an Enqueue of no fiber resumes \
          nothing: only a marked wake can come earlier"] ) ->
      ()
  | l ->
      prog "Fiber" (Enqueue l)

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

let fork ?name ?pinned scope body =
  add_member scope ~daemon:false ;
  let f = new_fiber ?name ?pinned [scope] in
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

let next_run = Atomic.make 0

(* The maximum number of domains of a run. The domain bits of an fd record are
   in one [int]. *)
let max_domains = 62

(* [check_domains name domains] raises [Invalid_argument] if [domains] is not in
   the range 1 to [max_domains]. The error text starts with [name]. The
   scheduler and the worlds use the same check. *)
let check_domains name domains =
  if domains < 1 || domains > max_domains then
    invalid_arg
      (Printf.sprintf "%s: the number of domains is not in the range 1 to %d"
         name max_domains )

(* The scheduler of [domains] domains. *)
let create ?(slack = Span.us 100) ?(simulated = false) ~domains () =
  check_domains "run" domains ;
  let t =
    { run_id= Atomic.fetch_and_add next_run 1
    ; beats=
        (let b =
           Bigarray.Array1.create Bigarray.int Bigarray.c_layout domains
         in
         Bigarray.Array1.fill b 0 ; b )
    ; doms= [||]
    ; slack= Span.to_ns slack
    ; fibers= Hashtbl.create ~random:false 64
    ; fibers_lock= Spin.make ()
    ; simulated
    ; finished= Atomic.make false
    ; idle= Atomic.make 0
    ; searching= Atomic.make 0
    ; pinned_q= Queue.create ()
    ; pinned_lock= Spin.make ()
    ; pinned_n= Atomic.make 0
    ; unawaited= Atomic.make 0
    ; inject= Atomic.make []
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
        ; next= Atomic.make None
        ; next_runs= 0
        ; pinned_runs= 0
        ; heap= Heap.create ()
        ; pheap= Heap.create ()
        ; ids= Commit.Id.gen ~domain:index ~max_domains:domains
        ; sched= t
        ; switches= 0
        ; ticks= 0
        ; rng= 0
        ; wake= false
        ; wake0= false
        ; stats= {wakes= 0; edges= 0; idle_edges= 0; busy_polls= 0; resumes= 0}
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

(* A push to an atomic list with a CAS: no lock. A finaliser can do it. *)
let rec push_list l x =
  let old = Atomic.get l in
  if not (Atomic.compare_and_set l old (x :: old)) then push_list l x

(* Push a request to the injection queue: no lock, no sync word. *)
let inject t req = push_list t.inject req

(* It touches no sync word, no waiter list and no lock: a domain does the fill
   at a safe point. It gives [false] if no run takes the fill, or if the Ivar is
   of a run that ended: the waiters of that run are dead, and no fiber of the
   active run waits for it. *)
let fill_from_outside iv v =
  match Atomic.get outside with
  | None ->
      false
  | Some (t, _)
    when match Atomic.get iv with
         | Empty (run, _) ->
             run <> no_run && run <> t.run_id
         | Full _ ->
             false ->
      false
  | Some (_, deliver) ->
      deliver (fun dom ->
          match Iv.fill iv v with
          | Some l ->
              List.iter (resume dom) l
          | None ->
              () ) ;
      true

(* Run the loop of a domain. A bug in the scheduler stops the program. *)
let run_domain dom =
  try
    dom.rng <- World.perform "sched" (World.Random (1 lsl 30)) ;
    loop dom
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    Atomic.set dom.sched.finished true ;
    raise (Internal_error (e, bt))
