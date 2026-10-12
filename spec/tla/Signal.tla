------------------------------- MODULE Signal -------------------------------
(***************************************************************************)
(* A model of the end of a watch: Signal.unwatch (docs/design.md, section *)
(* 2.7, The end of a watch).                                               *)
(*                                                                         *)
(* The parts:                                                              *)
(*   - The sender sends the signal Sends times. Before the end of the     *)
(*     watch, a signal goes to the source of the world. After it, the     *)
(*     signal gets its action.                                             *)
(*   - The source. On Linux, the signals that are pending in the kernel   *)
(*     while each thread blocks the signal. The signalfd reads them. On   *)
(*     kqueue, the count of the handler of the world. On kqueue, a        *)
(*     handler that started before the restore of the old action can add  *)
(*     to the count later.                                                 *)
(*   - Domain 0 takes the source, then it delivers the signals under the  *)
(*     lock of the scheduler: to the waiter, to the count, or, after the  *)
(*     mark, again to the process (Op.Resend).                            *)
(*   - The waiter does Syncs syncs of received. Under the lock, it takes  *)
(*     the count, or it waits. After the mark, its sync raises.           *)
(*   - The unwatch. The world stops the watch first. Linux: the signalfd  *)
(*     does not take the signal any more, then the thread unblocks it,    *)
(*     and the kernel gives the pending signals their action. kqueue: the *)
(*     old action comes back, then the world takes the count and sends it *)
(*     again. Then the scheduler marks the signal under the lock, takes   *)
(*     the count, and the waiter raises. The count goes to Op.Resend.     *)
(*   - Linux: the thread of the unwatch can end before domain 0. Then no  *)
(*     thread unblocks the signal, until domain 0 unblocks it in its own  *)
(*     thread at its next wait.                                            *)
(*                                                                         *)
(* The property: each signal goes to a fiber or gets its action, exactly  *)
(* one time, and no signal gets its action before the unwatch starts.     *)
(*                                                                         *)
(* The Bug* constants plant the bugs, to show that the model finds them.  *)
(***************************************************************************)
EXTENDS Naturals

CONSTANTS
  Scenario,          \* "linux" or "kqueue"
  BugDropAfterMark,  \* a deliver after the mark drops the signals
  BugNoCountTake,    \* the mark leaves the count of the scheduler
  BugWaiterStays,    \* the mark does not commit the waiter
  BugLeafNoCheck,    \* a sync after the mark waits, as before the unwatch
  BugTakeWatched,    \* kqueue: domain 0 takes only the watched signals
  BugNoMainUnblock   \* Linux: domain 0 does not unblock the signal

Sends == 2
Syncs == 2

Snd == 1
Dom == 2
Wtr == 3
Unw == 4
Thr == 5

(* --algorithm Signal {
variables
  sent = 0,
  acted = 0,          \* the signals that got their action
  taken = 0,          \* the signals that the waiter took
  pending = 0,        \* the source of the world
  fdwatch = TRUE,     \* Linux: the signalfd takes the signal
  unblocked = FALSE,  \* a new signal gets its action
  marked = FALSE,     \* the mark of the scheduler (st.unwatched)
  count = 0,          \* the count of the scheduler (st.count)
  lock = 0,           \* the lock of the signals of the scheduler
  waiting = FALSE,    \* the waiter is in the list of the waiters
  wres = "none",      \* what the sync of the waiter got
  started = FALSE,    \* the unwatch started
  mainReq = FALSE,    \* Linux: domain 0 must unblock the signal
  mainUnblocked = FALSE; \* Linux: the thread of domain 0 unblocked it

\* Op.Resend: a signal to the process itself.
macro resend(n) {
  if (unblocked) { acted := acted + n; } else { pending := pending + n; };
}

fair process (snd = Snd)
{
s1:
  if (sent < Sends) {
    sent := sent + 1;
    if (unblocked) {
      acted := acted + 1;
      goto s1;
    } else if (Scenario = "linux") {
      pending := pending + 1;
      goto s1;
    };
  } else {
    goto Done;
  };
s2:
  \* kqueue: the handler started before the restore, and it adds now.
  pending := pending + 1;
  goto s1;
}

fair process (dom = Dom)
variables k = 0;
{
r1:
  either {
    await pending > 0;
    await Scenario = "kqueue" \/ fdwatch;
    await ~(BugTakeWatched /\ Scenario = "kqueue" /\ unblocked);
    k := pending;
    pending := 0;
  } or {
    \* Linux: at its wait, domain 0 unblocks the signal in its thread.
    await Scenario = "linux" /\ mainReq /\ ~mainUnblocked;
    await ~BugNoMainUnblock;
    mainUnblocked := TRUE;
    unblocked := TRUE;
    acted := acted + pending;
    pending := 0;
    goto r1;
  } or {
    await pc[Snd] = "Done" /\ pc[Unw] = "Done" /\ pending = 0;
    goto Done;
  };
r2:
  await lock = 0;
  lock := Dom;
r3:
  if (marked) {
    if (~BugDropAfterMark) { resend(k); };
  } else if (waiting) {
    taken := taken + k;
    wres := "count";
    waiting := FALSE;
  } else {
    count := count + k;
  };
  k := 0;
  lock := 0;
  goto r1;
}

fair process (wtr = Wtr)
variables n = 0;
{
w1:
  if (n < Syncs /\ wres # "raise") {
    n := n + 1;
    wres := "none";
  } else {
    goto Done;
  };
w2:
  await lock = 0;
  lock := Wtr;
w3:
  if (marked /\ ~BugLeafNoCheck) {
    wres := "raise";
  } else if (count > 0) {
    taken := taken + count;
    count := 0;
    wres := "count";
  } else {
    waiting := TRUE;
  };
  lock := 0;
w4:
  await wres # "none";
  goto w1;
}

fair process (unw = Unw)
variables c = 0;
{
u1:
  started := TRUE;
  if (Scenario = "linux") { fdwatch := FALSE; } else { unblocked := TRUE; };
u2:
  \* Linux: the unblock, and the kernel gives the pending signals their
  \* action. kqueue: the world takes the count and sends it again.
  unblocked := TRUE;
  acted := acted + pending;
  pending := 0;
  if (Scenario = "linux") { mainReq := TRUE; };
u3:
  await lock = 0;
  lock := Unw;
u4:
  marked := TRUE;
  if (~BugNoCountTake) {
    c := count;
    count := 0;
  };
  if (waiting /\ ~BugWaiterStays) {
    wres := "raise";
    waiting := FALSE;
  };
  lock := 0;
u5:
  resend(c);
  c := 0;
}

\* Linux: the thread of the unwatch ends, for example with its domain.
fair process (thr = Thr)
{
t1:
  await pc[Unw] \notin {"u1", "u2"};
  if (Scenario = "linux" /\ ~mainUnblocked) { unblocked := FALSE; };
}
} *)

\* BEGIN TRANSLATION
VARIABLES sent, acted, taken, pending, fdwatch, unblocked, marked, count, 
          lock, waiting, wres, started, mainReq, mainUnblocked, pc, k, n, c

vars == << sent, acted, taken, pending, fdwatch, unblocked, marked, count, 
           lock, waiting, wres, started, mainReq, mainUnblocked, pc, k, n, c
        >>

ProcSet == {Snd} \cup {Dom} \cup {Wtr} \cup {Unw} \cup {Thr}

Init == (* Global variables *)
        /\ sent = 0
        /\ acted = 0
        /\ taken = 0
        /\ pending = 0
        /\ fdwatch = TRUE
        /\ unblocked = FALSE
        /\ marked = FALSE
        /\ count = 0
        /\ lock = 0
        /\ waiting = FALSE
        /\ wres = "none"
        /\ started = FALSE
        /\ mainReq = FALSE
        /\ mainUnblocked = FALSE
        (* Process dom *)
        /\ k = 0
        (* Process wtr *)
        /\ n = 0
        (* Process unw *)
        /\ c = 0
        /\ pc = [self \in ProcSet |-> CASE self = Snd -> "s1"
                                        [] self = Dom -> "r1"
                                        [] self = Wtr -> "w1"
                                        [] self = Unw -> "u1"
                                        [] self = Thr -> "t1"]

s1 == /\ pc[Snd] = "s1"
      /\ IF sent < Sends
            THEN /\ sent' = sent + 1
                 /\ IF unblocked
                       THEN /\ acted' = acted + 1
                            /\ pc' = [pc EXCEPT ![Snd] = "s1"]
                            /\ UNCHANGED pending
                       ELSE /\ IF Scenario = "linux"
                                  THEN /\ pending' = pending + 1
                                       /\ pc' = [pc EXCEPT ![Snd] = "s1"]
                                  ELSE /\ pc' = [pc EXCEPT ![Snd] = "s2"]
                                       /\ UNCHANGED pending
                            /\ acted' = acted
            ELSE /\ pc' = [pc EXCEPT ![Snd] = "Done"]
                 /\ UNCHANGED << sent, acted, pending >>
      /\ UNCHANGED << taken, fdwatch, unblocked, marked, count, lock, waiting, 
                      wres, started, mainReq, mainUnblocked, k, n, c >>

s2 == /\ pc[Snd] = "s2"
      /\ pending' = pending + 1
      /\ pc' = [pc EXCEPT ![Snd] = "s1"]
      /\ UNCHANGED << sent, acted, taken, fdwatch, unblocked, marked, count, 
                      lock, waiting, wres, started, mainReq, mainUnblocked, k, 
                      n, c >>

snd == s1 \/ s2

r1 == /\ pc[Dom] = "r1"
      /\ \/ /\ pending > 0
            /\ Scenario = "kqueue" \/ fdwatch
            /\ ~(BugTakeWatched /\ Scenario = "kqueue" /\ unblocked)
            /\ k' = pending
            /\ pending' = 0
            /\ pc' = [pc EXCEPT ![Dom] = "r2"]
            /\ UNCHANGED <<acted, unblocked, mainUnblocked>>
         \/ /\ Scenario = "linux" /\ mainReq /\ ~mainUnblocked
            /\ ~BugNoMainUnblock
            /\ mainUnblocked' = TRUE
            /\ unblocked' = TRUE
            /\ acted' = acted + pending
            /\ pending' = 0
            /\ pc' = [pc EXCEPT ![Dom] = "r1"]
            /\ k' = k
         \/ /\ pc[Snd] = "Done" /\ pc[Unw] = "Done" /\ pending = 0
            /\ pc' = [pc EXCEPT ![Dom] = "Done"]
            /\ UNCHANGED <<acted, pending, unblocked, mainUnblocked, k>>
      /\ UNCHANGED << sent, taken, fdwatch, marked, count, lock, waiting, wres, 
                      started, mainReq, n, c >>

r2 == /\ pc[Dom] = "r2"
      /\ lock = 0
      /\ lock' = Dom
      /\ pc' = [pc EXCEPT ![Dom] = "r3"]
      /\ UNCHANGED << sent, acted, taken, pending, fdwatch, unblocked, marked, 
                      count, waiting, wres, started, mainReq, mainUnblocked, k, 
                      n, c >>

r3 == /\ pc[Dom] = "r3"
      /\ IF marked
            THEN /\ IF ~BugDropAfterMark
                       THEN /\ IF unblocked
                                  THEN /\ acted' = acted + k
                                       /\ UNCHANGED pending
                                  ELSE /\ pending' = pending + k
                                       /\ acted' = acted
                       ELSE /\ TRUE
                            /\ UNCHANGED << acted, pending >>
                 /\ UNCHANGED << taken, count, waiting, wres >>
            ELSE /\ IF waiting
                       THEN /\ taken' = taken + k
                            /\ wres' = "count"
                            /\ waiting' = FALSE
                            /\ count' = count
                       ELSE /\ count' = count + k
                            /\ UNCHANGED << taken, waiting, wres >>
                 /\ UNCHANGED << acted, pending >>
      /\ k' = 0
      /\ lock' = 0
      /\ pc' = [pc EXCEPT ![Dom] = "r1"]
      /\ UNCHANGED << sent, fdwatch, unblocked, marked, started, mainReq, 
                      mainUnblocked, n, c >>

dom == r1 \/ r2 \/ r3

w1 == /\ pc[Wtr] = "w1"
      /\ IF n < Syncs /\ wres # "raise"
            THEN /\ n' = n + 1
                 /\ wres' = "none"
                 /\ pc' = [pc EXCEPT ![Wtr] = "w2"]
            ELSE /\ pc' = [pc EXCEPT ![Wtr] = "Done"]
                 /\ UNCHANGED << wres, n >>
      /\ UNCHANGED << sent, acted, taken, pending, fdwatch, unblocked, marked, 
                      count, lock, waiting, started, mainReq, mainUnblocked, k, 
                      c >>

w2 == /\ pc[Wtr] = "w2"
      /\ lock = 0
      /\ lock' = Wtr
      /\ pc' = [pc EXCEPT ![Wtr] = "w3"]
      /\ UNCHANGED << sent, acted, taken, pending, fdwatch, unblocked, marked, 
                      count, waiting, wres, started, mainReq, mainUnblocked, k, 
                      n, c >>

w3 == /\ pc[Wtr] = "w3"
      /\ IF marked /\ ~BugLeafNoCheck
            THEN /\ wres' = "raise"
                 /\ UNCHANGED << taken, count, waiting >>
            ELSE /\ IF count > 0
                       THEN /\ taken' = taken + count
                            /\ count' = 0
                            /\ wres' = "count"
                            /\ UNCHANGED waiting
                       ELSE /\ waiting' = TRUE
                            /\ UNCHANGED << taken, count, wres >>
      /\ lock' = 0
      /\ pc' = [pc EXCEPT ![Wtr] = "w4"]
      /\ UNCHANGED << sent, acted, pending, fdwatch, unblocked, marked, 
                      started, mainReq, mainUnblocked, k, n, c >>

w4 == /\ pc[Wtr] = "w4"
      /\ wres # "none"
      /\ pc' = [pc EXCEPT ![Wtr] = "w1"]
      /\ UNCHANGED << sent, acted, taken, pending, fdwatch, unblocked, marked, 
                      count, lock, waiting, wres, started, mainReq, 
                      mainUnblocked, k, n, c >>

wtr == w1 \/ w2 \/ w3 \/ w4

u1 == /\ pc[Unw] = "u1"
      /\ started' = TRUE
      /\ IF Scenario = "linux"
            THEN /\ fdwatch' = FALSE
                 /\ UNCHANGED unblocked
            ELSE /\ unblocked' = TRUE
                 /\ UNCHANGED fdwatch
      /\ pc' = [pc EXCEPT ![Unw] = "u2"]
      /\ UNCHANGED << sent, acted, taken, pending, marked, count, lock, 
                      waiting, wres, mainReq, mainUnblocked, k, n, c >>

u2 == /\ pc[Unw] = "u2"
      /\ unblocked' = TRUE
      /\ acted' = acted + pending
      /\ pending' = 0
      /\ IF Scenario = "linux"
            THEN /\ mainReq' = TRUE
            ELSE /\ TRUE
                 /\ UNCHANGED mainReq
      /\ pc' = [pc EXCEPT ![Unw] = "u3"]
      /\ UNCHANGED << sent, taken, fdwatch, marked, count, lock, waiting, wres, 
                      started, mainUnblocked, k, n, c >>

u3 == /\ pc[Unw] = "u3"
      /\ lock = 0
      /\ lock' = Unw
      /\ pc' = [pc EXCEPT ![Unw] = "u4"]
      /\ UNCHANGED << sent, acted, taken, pending, fdwatch, unblocked, marked, 
                      count, waiting, wres, started, mainReq, mainUnblocked, k, 
                      n, c >>

u4 == /\ pc[Unw] = "u4"
      /\ marked' = TRUE
      /\ IF ~BugNoCountTake
            THEN /\ c' = count
                 /\ count' = 0
            ELSE /\ TRUE
                 /\ UNCHANGED << count, c >>
      /\ IF waiting /\ ~BugWaiterStays
            THEN /\ wres' = "raise"
                 /\ waiting' = FALSE
            ELSE /\ TRUE
                 /\ UNCHANGED << waiting, wres >>
      /\ lock' = 0
      /\ pc' = [pc EXCEPT ![Unw] = "u5"]
      /\ UNCHANGED << sent, acted, taken, pending, fdwatch, unblocked, started, 
                      mainReq, mainUnblocked, k, n >>

u5 == /\ pc[Unw] = "u5"
      /\ IF unblocked
            THEN /\ acted' = acted + c
                 /\ UNCHANGED pending
            ELSE /\ pending' = pending + c
                 /\ acted' = acted
      /\ c' = 0
      /\ pc' = [pc EXCEPT ![Unw] = "Done"]
      /\ UNCHANGED << sent, taken, fdwatch, unblocked, marked, count, lock, 
                      waiting, wres, started, mainReq, mainUnblocked, k, n >>

unw == u1 \/ u2 \/ u3 \/ u4 \/ u5

t1 == /\ pc[Thr] = "t1"
      /\ pc[Unw] \notin {"u1", "u2"}
      /\ IF Scenario = "linux" /\ ~mainUnblocked
            THEN /\ unblocked' = FALSE
            ELSE /\ TRUE
                 /\ UNCHANGED unblocked
      /\ pc' = [pc EXCEPT ![Thr] = "Done"]
      /\ UNCHANGED << sent, acted, taken, pending, fdwatch, marked, count, 
                      lock, waiting, wres, started, mainReq, mainUnblocked, k, 
                      n, c >>

thr == t1

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == snd \/ dom \/ wtr \/ unw \/ thr
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(snd)
        /\ WF_vars(dom)
        /\ WF_vars(wtr)
        /\ WF_vars(unw)
        /\ WF_vars(thr)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION

-----------------------------------------------------------------------------
(* The properties. *)

\* A signal of the late handler of kqueue: sent, but not yet in the source.
Late == IF pc[Snd] = "s2" THEN 1 ELSE 0

\* No signal is lost or doubled: each signal that the sender sent is in one
\* place.
Conserved == sent = acted + taken + pending + count + k + c + Late

\* No signal gets its action before the unwatch starts.
NoEarlyAction == acted > 0 => started

AllDone == \A q \in ProcSet : pc[q] = "Done"

\* At the end, each signal went to the waiter or got its action.
EndState == AllDone => acted + taken = Sends

=============================================================================
