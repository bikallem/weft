------------------------------ MODULE Watchdog ------------------------------
(***************************************************************************)
(* A model of the park of the watchdog (docs/design.md, section 2.5, The  *)
(* limit of a busy domain, and src/posix/weft_watchdog.c).                *)
(*                                                                         *)
(* The parts:                                                              *)
(*   - N domains. Each domain sleeps and wakes Rounds times, and then it  *)
(*     stays awake: for example, a fiber computes with no yield. Before a *)
(*     sleep, the domain adds 1 to the atomic count `sleeping` (the stub  *)
(*     weft_watchdog_sleep). After the sleep, it subtracts 1 (the stub    *)
(*     weft_watchdog_awake). If the old value was N, all domains slept,   *)
(*     and the watchdog can be parked. Then the domain takes the mutex,   *)
(*     and if `parked` is set, it clears it and signals the condition.    *)
(*   - The watchdog thread. It holds the mutex, except in its waits. At   *)
(*     each check, it reads `sleeping`. If the count is N, it sets        *)
(*     `parked` and waits on the condition while `parked` is set. If not, *)
(*     it does the timed wait of half the limit.                          *)
(*   - The condition: `waiting` tells that the thread is in the wait set. *)
(*     A signal removes it from the set. A signal with no waiter is lost. *)
(*                                                                         *)
(* The properties: the watchdog is parked only while all domains sleep,   *)
(* or while one domain goes to clear `parked` (ParkOK). The watchdog      *)
(* never stays parked while a domain is awake (NoStuckPark).              *)
(*                                                                         *)
(* The Bug* constants plant the bugs, to show that the model finds them.  *)
(***************************************************************************)
EXTENDS Naturals

CONSTANTS
  N,             \* the number of domains
  Rounds,        \* the sleeps of each domain
  BugReadOutside, \* the watchdog reads `sleeping` before it takes the mutex
  BugNoLock      \* the awake reads `parked` and signals with no mutex

W == N + 1
Doms == 1..N

(* --algorithm Watchdog {
variables
  sleeping = 0,     \* the atomic count of the domains that sleep
  parked = FALSE,   \* the flag of the park, under the mutex
  waiting = FALSE,  \* the watchdog thread is in the wait set of the condition
  lock = 0;         \* the holder of the mutex, 0 for no holder

fair process (dom \in Doms)
variables r = 0, old = 0;
{
d1:
  if (r < Rounds) {
    \* weft_watchdog_sleep, then the sleep.
    sleeping := sleeping + 1;
    r := r + 1;
  } else {
    \* The domain stays awake.
    goto Done;
  };
d2:
  \* The end of the sleep: weft_watchdog_awake, an atomic fetch and
  \* subtract.
  old := sleeping;
  sleeping := sleeping - 1;
  if (old # N) { goto d1; }
  else if (BugNoLock) { goto d4; };
d3:
  await lock = 0;
  lock := self;
d4:
  if (parked) {
    parked := FALSE;
    waiting := FALSE;
  };
d5:
  if (lock = self) { lock := 0; };
  goto d1;
}

fair process (dog = W)
variables v = 0;
{
w0:
  if (BugReadOutside) { v := sleeping; };
w1:
  \* The end of the timed wait: the thread takes the mutex again.
  await lock = 0;
  lock := W;
w2:
  if (~BugReadOutside) { v := sleeping; };
w3:
  if (v = N) { parked := TRUE; } else { goto w6; };
w4:
  \* while (parked) wait: the wait releases the mutex.
  if (parked) {
    lock := 0;
    waiting := TRUE;
  } else {
    goto w6;
  };
w5:
  await ~waiting;
  await lock = 0;
  lock := W;
  goto w4;
w6:
  \* The timed wait of half the limit releases the mutex.
  lock := 0;
  goto w0;
}
} *)

\* BEGIN TRANSLATION
VARIABLES sleeping, parked, waiting, lock, pc, r, old, v

vars == << sleeping, parked, waiting, lock, pc, r, old, v >>

ProcSet == (Doms) \cup {W}

Init == (* Global variables *)
        /\ sleeping = 0
        /\ parked = FALSE
        /\ waiting = FALSE
        /\ lock = 0
        (* Process dom *)
        /\ r = [self \in Doms |-> 0]
        /\ old = [self \in Doms |-> 0]
        (* Process dog *)
        /\ v = 0
        /\ pc = [self \in ProcSet |-> CASE self \in Doms -> "d1"
                                        [] self = W -> "w0"]

d1(self) == /\ pc[self] = "d1"
            /\ IF r[self] < Rounds
                  THEN /\ sleeping' = sleeping + 1
                       /\ r' = [r EXCEPT ![self] = r[self] + 1]
                       /\ pc' = [pc EXCEPT ![self] = "d2"]
                  ELSE /\ pc' = [pc EXCEPT ![self] = "Done"]
                       /\ UNCHANGED << sleeping, r >>
            /\ UNCHANGED << parked, waiting, lock, old, v >>

d2(self) == /\ pc[self] = "d2"
            /\ old' = [old EXCEPT ![self] = sleeping]
            /\ sleeping' = sleeping - 1
            /\ IF old'[self] # N
                  THEN /\ pc' = [pc EXCEPT ![self] = "d1"]
                  ELSE /\ IF BugNoLock
                             THEN /\ pc' = [pc EXCEPT ![self] = "d4"]
                             ELSE /\ pc' = [pc EXCEPT ![self] = "d3"]
            /\ UNCHANGED << parked, waiting, lock, r, v >>

d3(self) == /\ pc[self] = "d3"
            /\ lock = 0
            /\ lock' = self
            /\ pc' = [pc EXCEPT ![self] = "d4"]
            /\ UNCHANGED << sleeping, parked, waiting, r, old, v >>

d4(self) == /\ pc[self] = "d4"
            /\ IF parked
                  THEN /\ parked' = FALSE
                       /\ waiting' = FALSE
                  ELSE /\ TRUE
                       /\ UNCHANGED << parked, waiting >>
            /\ pc' = [pc EXCEPT ![self] = "d5"]
            /\ UNCHANGED << sleeping, lock, r, old, v >>

d5(self) == /\ pc[self] = "d5"
            /\ IF lock = self
                  THEN /\ lock' = 0
                  ELSE /\ TRUE
                       /\ lock' = lock
            /\ pc' = [pc EXCEPT ![self] = "d1"]
            /\ UNCHANGED << sleeping, parked, waiting, r, old, v >>

dom(self) == d1(self) \/ d2(self) \/ d3(self) \/ d4(self) \/ d5(self)

w0 == /\ pc[W] = "w0"
      /\ IF BugReadOutside
            THEN /\ v' = sleeping
            ELSE /\ TRUE
                 /\ v' = v
      /\ pc' = [pc EXCEPT ![W] = "w1"]
      /\ UNCHANGED << sleeping, parked, waiting, lock, r, old >>

w1 == /\ pc[W] = "w1"
      /\ lock = 0
      /\ lock' = W
      /\ pc' = [pc EXCEPT ![W] = "w2"]
      /\ UNCHANGED << sleeping, parked, waiting, r, old, v >>

w2 == /\ pc[W] = "w2"
      /\ IF ~BugReadOutside
            THEN /\ v' = sleeping
            ELSE /\ TRUE
                 /\ v' = v
      /\ pc' = [pc EXCEPT ![W] = "w3"]
      /\ UNCHANGED << sleeping, parked, waiting, lock, r, old >>

w3 == /\ pc[W] = "w3"
      /\ IF v = N
            THEN /\ parked' = TRUE
                 /\ pc' = [pc EXCEPT ![W] = "w4"]
            ELSE /\ pc' = [pc EXCEPT ![W] = "w6"]
                 /\ UNCHANGED parked
      /\ UNCHANGED << sleeping, waiting, lock, r, old, v >>

w4 == /\ pc[W] = "w4"
      /\ IF parked
            THEN /\ lock' = 0
                 /\ waiting' = TRUE
                 /\ pc' = [pc EXCEPT ![W] = "w5"]
            ELSE /\ pc' = [pc EXCEPT ![W] = "w6"]
                 /\ UNCHANGED << waiting, lock >>
      /\ UNCHANGED << sleeping, parked, r, old, v >>

w5 == /\ pc[W] = "w5"
      /\ ~waiting
      /\ lock = 0
      /\ lock' = W
      /\ pc' = [pc EXCEPT ![W] = "w4"]
      /\ UNCHANGED << sleeping, parked, waiting, r, old, v >>

w6 == /\ pc[W] = "w6"
      /\ lock' = 0
      /\ pc' = [pc EXCEPT ![W] = "w0"]
      /\ UNCHANGED << sleeping, parked, waiting, r, old, v >>

dog == w0 \/ w1 \/ w2 \/ w3 \/ w4 \/ w5 \/ w6

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == dog
           \/ (\E self \in Doms: dom(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in Doms : WF_vars(dom(self))
        /\ WF_vars(dog)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION

-----------------------------------------------------------------------------
(* The properties. *)

TypeOK ==
  /\ sleeping \in 0..N
  /\ parked \in BOOLEAN
  /\ waiting \in BOOLEAN
  /\ lock \in {0, W} \cup Doms

\* A domain that saw the old value N and did not yet read `parked`.
Pending(d) == old[d] = N /\ pc[d] \in {"d3", "d4"}

\* The watchdog is parked only while all domains sleep, or while the domain
\* that woke first goes to clear `parked`.
ParkOK == parked => (sleeping = N \/ \E d \in Doms : Pending(d))

\* The watchdog never stays parked while a domain is awake. A domain stays
\* awake at the end, so a lost signal keeps the watchdog parked forever.
NoStuckPark == (sleeping < N) ~> ~parked

=============================================================================
