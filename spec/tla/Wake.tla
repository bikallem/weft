-------------------------------- MODULE Wake --------------------------------
(***************************************************************************)
(* A model of the idle and wake protocol of docs/design.md, section 2.5:  *)
(* the loop of one domain, the steal, `idle` and `searching` in the Go    *)
(* order, the next slot, the pinned queue of domain 0 and the global       *)
(* injection queue.                                                        *)
(*                                                                         *)
(* Three domains run the loop. The work items:                             *)
(*   1  is in the local queue of domain 0 at the start. When it runs, it  *)
(*      wakes item 2 into the next slot of its domain, and it wakes the   *)
(*      pinned item 3.                                                    *)
(*   3  is pinned: only domain 0 can run it.                              *)
(*   4  comes from a thread outside the domains (fill_from_outside), into *)
(*      the injection queue, at any time.                                  *)
(*                                                                         *)
(* A domain that sleeps waits for its wake count (an eventfd). A wake     *)
(* before the sleep is not lost. A waker takes the domain out of `idle`.  *)
(* The model checks that each item runs, and that no state has all        *)
(* domains asleep while work waits in a queue.                             *)
(***************************************************************************)
EXTENDS Integers, FiniteSets, Sequences

CONSTANTS
  BugDecAfterCheck,   \* a searching domain decreases `searching` after its second check
  BugNoRecheck,       \* a domain that goes idle does not check the queues again
  BugPinnedGeneric,   \* a push to the pinned queue wakes any idle domain
  BugNoNextSteal,     \* a thief cannot take a next slot
  SimpNoSearchLimit,  \* a simplification: all domains can search
  SimpNoLastWake,     \* a simplification: the last searching domain wakes nobody
  SimpNoSearching     \* a simplification: no `searching` count, a push wakes each time

Doms  == {0, 1, 2}
Items == {1, 2, 3, 4}
Limit == IF SimpNoSearchLimit THEN 3 ELSE 1   \* at most half of the domains search

(* --algorithm Wake {
variables
  local = [d \in Doms |-> IF d = 0 THEN <<1>> ELSE <<>>],
  next = [d \in Doms |-> 0],
  pinned = <<>>,
  inj = <<>>,
  idle = {},
  searching = 0,
  wakeCnt = [d \in Doms |-> 0],
  ran = {},
  ranOn = [i \in Items |-> -1];

define {
  WorkFor(d) ==
    \/ next[d] # 0 \/ local[d] # <<>> \/ inj # <<>>
    \/ (d = 0 /\ pinned # <<>>)
    \/ \E v \in Doms \ {d} : local[v] # <<>> \/ (~BugNoNextSteal /\ next[v] # 0)
  WorkExists ==
    \/ inj # <<>> \/ pinned # <<>>
    \/ \E d \in Doms : local[d] # <<>> \/ next[d] # 0
}

\* Wake one idle domain, by the normal rule of a push.
macro wakeOne() {
  if (idle # {} /\ (SimpNoSearching \/ searching = 0)) {
    with (v \in idle) { idle := idle \ {v}; wakeCnt[v] := wakeCnt[v] + 1; };
  };
}

fair process (dom \in Doms)
variables cur = 0, isSearch = FALSE, victims = {};
{
take:
  while (TRUE) {
    \* Steps 1 to 3 of the loop: the pinned queue (domain 0), the next slot,
    \* the local queue, the injection queue.
    if (self = 0 /\ pinned # <<>>) { cur := Head(pinned); pinned := Tail(pinned); }
    else if (next[self] # 0) { cur := next[self]; next[self] := 0; }
    else if (local[self] # <<>>) { cur := Head(local[self]); local[self] := Tail(local[self]); }
    else if (inj # <<>>) { cur := Head(inj); inj := Tail(inj); }
    else { cur := 0; };
    if (cur # 0) { goto run; };
st0:
    \* Step 4: steal. Only `Limit` domains search at one time.
    if (SimpNoSearching) { isSearch := FALSE; }
    else if (searching < Limit) { searching := searching + 1; isSearch := TRUE; }
    else { isSearch := FALSE; goto id1; };
st1:
    victims := Doms \ {self};
st2:
    while (victims # {} /\ cur = 0) {
      with (v \in victims) {
        if (local[v] # <<>>) {
          \* Take half of the queue of v, from the head.
          with (k = (Len(local[v]) + 1) \div 2) {
            cur := Head(local[v]);
            local := [local EXCEPT ![self] = local[self] \o SubSeq(local[v], 2, k),
                                   ![v] = SubSeq(local[v], k + 1, Len(local[v]))];
          };
        } else if (~BugNoNextSteal /\ next[v] # 0) {
          cur := next[v]; next[v] := 0;          \* a CAS on the next slot
        };
        victims := victims \ {v};
      };
    };
st3:
    if (cur # 0) {
      \* A searching domain that finds work.
      if (isSearch) { searching := searching - 1; isSearch := FALSE; };
st4:
      if (~SimpNoLastWake /\ searching = 0 /\ idle # {}) {   \* the last searching domain
        with (v \in idle) { idle := idle \ {v}; wakeCnt[v] := wakeCnt[v] + 1; };
      };
      goto run;
    };
id0:
    \* A searching domain that finds no work: the decrease comes first.
    if (isSearch /\ ~BugDecAfterCheck) { searching := searching - 1; isSearch := FALSE; };
id1:
    idle := idle \cup {self};
id2:
    \* The second check, after the domain is in `idle`.
    if (~BugNoRecheck /\ WorkFor(self)) {
      idle := idle \ {self};
      if (isSearch) { searching := searching - 1; isSearch := FALSE; };
      goto take;
    };
id3:
    if (isSearch) { searching := searching - 1; isSearch := FALSE; };   \* the bug order
sleep:
    await wakeCnt[self] > 0;
    wakeCnt[self] := wakeCnt[self] - 1;
    idle := idle \ {self};
    goto take;
run:
    \* Run the item. Item 1 wakes item 2 and the pinned item 3.
    ran := ran \cup {cur};
    ranOn[cur] := self;
    if (cur = 1) {
      if (next[self] # 0) { local[self] := Append(local[self], next[self]); };
      next[self] := 2;                          \* the hand-off to the next slot
r2:
      wakeOne();                                \* the push reads `idle` after it
r3:
      pinned := Append(pinned, 3);
r4:
      if (BugPinnedGeneric) { wakeOne(); }
      else { idle := idle \ {0}; wakeCnt[0] := wakeCnt[0] + 1; };   \* wake domain 0
    };
r5:
    cur := 0;
  };
}

\* A thread outside the domains: it pushes item 4 to the injection queue.
fair process (outside = 10)
{
x1:
  inj := Append(inj, 4);
x2:
  wakeOne();
}
} *)

\* BEGIN TRANSLATION
VARIABLES local, next, pinned, inj, idle, searching, wakeCnt, ran, ranOn, pc

(* define statement *)
WorkFor(d) ==
  \/ next[d] # 0 \/ local[d] # <<>> \/ inj # <<>>
  \/ (d = 0 /\ pinned # <<>>)
  \/ \E v \in Doms \ {d} : local[v] # <<>> \/ (~BugNoNextSteal /\ next[v] # 0)
WorkExists ==
  \/ inj # <<>> \/ pinned # <<>>
  \/ \E d \in Doms : local[d] # <<>> \/ next[d] # 0

VARIABLES cur, isSearch, victims

vars == << local, next, pinned, inj, idle, searching, wakeCnt, ran, ranOn, pc, 
           cur, isSearch, victims >>

ProcSet == (Doms) \cup {10}

Init == (* Global variables *)
        /\ local = [d \in Doms |-> IF d = 0 THEN <<1>> ELSE <<>>]
        /\ next = [d \in Doms |-> 0]
        /\ pinned = <<>>
        /\ inj = <<>>
        /\ idle = {}
        /\ searching = 0
        /\ wakeCnt = [d \in Doms |-> 0]
        /\ ran = {}
        /\ ranOn = [i \in Items |-> -1]
        (* Process dom *)
        /\ cur = [self \in Doms |-> 0]
        /\ isSearch = [self \in Doms |-> FALSE]
        /\ victims = [self \in Doms |-> {}]
        /\ pc = [self \in ProcSet |-> CASE self \in Doms -> "take"
                                        [] self = 10 -> "x1"]

take(self) == /\ pc[self] = "take"
              /\ IF self = 0 /\ pinned # <<>>
                    THEN /\ cur' = [cur EXCEPT ![self] = Head(pinned)]
                         /\ pinned' = Tail(pinned)
                         /\ UNCHANGED << local, next, inj >>
                    ELSE /\ IF next[self] # 0
                               THEN /\ cur' = [cur EXCEPT ![self] = next[self]]
                                    /\ next' = [next EXCEPT ![self] = 0]
                                    /\ UNCHANGED << local, inj >>
                               ELSE /\ IF local[self] # <<>>
                                          THEN /\ cur' = [cur EXCEPT ![self] = Head(local[self])]
                                               /\ local' = [local EXCEPT ![self] = Tail(local[self])]
                                               /\ inj' = inj
                                          ELSE /\ IF inj # <<>>
                                                     THEN /\ cur' = [cur EXCEPT ![self] = Head(inj)]
                                                          /\ inj' = Tail(inj)
                                                     ELSE /\ cur' = [cur EXCEPT ![self] = 0]
                                                          /\ inj' = inj
                                               /\ local' = local
                                    /\ next' = next
                         /\ UNCHANGED pinned
              /\ IF cur'[self] # 0
                    THEN /\ pc' = [pc EXCEPT ![self] = "run"]
                    ELSE /\ pc' = [pc EXCEPT ![self] = "st0"]
              /\ UNCHANGED << idle, searching, wakeCnt, ran, ranOn, isSearch, 
                              victims >>

st0(self) == /\ pc[self] = "st0"
             /\ IF SimpNoSearching
                   THEN /\ isSearch' = [isSearch EXCEPT ![self] = FALSE]
                        /\ pc' = [pc EXCEPT ![self] = "st1"]
                        /\ UNCHANGED searching
                   ELSE /\ IF searching < Limit
                              THEN /\ searching' = searching + 1
                                   /\ isSearch' = [isSearch EXCEPT ![self] = TRUE]
                                   /\ pc' = [pc EXCEPT ![self] = "st1"]
                              ELSE /\ isSearch' = [isSearch EXCEPT ![self] = FALSE]
                                   /\ pc' = [pc EXCEPT ![self] = "id1"]
                                   /\ UNCHANGED searching
             /\ UNCHANGED << local, next, pinned, inj, idle, wakeCnt, ran, 
                             ranOn, cur, victims >>

st1(self) == /\ pc[self] = "st1"
             /\ victims' = [victims EXCEPT ![self] = Doms \ {self}]
             /\ pc' = [pc EXCEPT ![self] = "st2"]
             /\ UNCHANGED << local, next, pinned, inj, idle, searching, 
                             wakeCnt, ran, ranOn, cur, isSearch >>

st2(self) == /\ pc[self] = "st2"
             /\ IF victims[self] # {} /\ cur[self] = 0
                   THEN /\ \E v \in victims[self]:
                             /\ IF local[v] # <<>>
                                   THEN /\ LET k == (Len(local[v]) + 1) \div 2 IN
                                             /\ cur' = [cur EXCEPT ![self] = Head(local[v])]
                                             /\ local' = [local EXCEPT ![self] = local[self] \o SubSeq(local[v], 2, k),
                                                                       ![v] = SubSeq(local[v], k + 1, Len(local[v]))]
                                        /\ next' = next
                                   ELSE /\ IF ~BugNoNextSteal /\ next[v] # 0
                                              THEN /\ cur' = [cur EXCEPT ![self] = next[v]]
                                                   /\ next' = [next EXCEPT ![v] = 0]
                                              ELSE /\ TRUE
                                                   /\ UNCHANGED << next, cur >>
                                        /\ local' = local
                             /\ victims' = [victims EXCEPT ![self] = victims[self] \ {v}]
                        /\ pc' = [pc EXCEPT ![self] = "st2"]
                   ELSE /\ pc' = [pc EXCEPT ![self] = "st3"]
                        /\ UNCHANGED << local, next, cur, victims >>
             /\ UNCHANGED << pinned, inj, idle, searching, wakeCnt, ran, ranOn, 
                             isSearch >>

st3(self) == /\ pc[self] = "st3"
             /\ IF cur[self] # 0
                   THEN /\ IF isSearch[self]
                              THEN /\ searching' = searching - 1
                                   /\ isSearch' = [isSearch EXCEPT ![self] = FALSE]
                              ELSE /\ TRUE
                                   /\ UNCHANGED << searching, isSearch >>
                        /\ pc' = [pc EXCEPT ![self] = "st4"]
                   ELSE /\ pc' = [pc EXCEPT ![self] = "id0"]
                        /\ UNCHANGED << searching, isSearch >>
             /\ UNCHANGED << local, next, pinned, inj, idle, wakeCnt, ran, 
                             ranOn, cur, victims >>

st4(self) == /\ pc[self] = "st4"
             /\ IF ~SimpNoLastWake /\ searching = 0 /\ idle # {}
                   THEN /\ \E v \in idle:
                             /\ idle' = idle \ {v}
                             /\ wakeCnt' = [wakeCnt EXCEPT ![v] = wakeCnt[v] + 1]
                   ELSE /\ TRUE
                        /\ UNCHANGED << idle, wakeCnt >>
             /\ pc' = [pc EXCEPT ![self] = "run"]
             /\ UNCHANGED << local, next, pinned, inj, searching, ran, ranOn, 
                             cur, isSearch, victims >>

id0(self) == /\ pc[self] = "id0"
             /\ IF isSearch[self] /\ ~BugDecAfterCheck
                   THEN /\ searching' = searching - 1
                        /\ isSearch' = [isSearch EXCEPT ![self] = FALSE]
                   ELSE /\ TRUE
                        /\ UNCHANGED << searching, isSearch >>
             /\ pc' = [pc EXCEPT ![self] = "id1"]
             /\ UNCHANGED << local, next, pinned, inj, idle, wakeCnt, ran, 
                             ranOn, cur, victims >>

id1(self) == /\ pc[self] = "id1"
             /\ idle' = (idle \cup {self})
             /\ pc' = [pc EXCEPT ![self] = "id2"]
             /\ UNCHANGED << local, next, pinned, inj, searching, wakeCnt, ran, 
                             ranOn, cur, isSearch, victims >>

id2(self) == /\ pc[self] = "id2"
             /\ IF ~BugNoRecheck /\ WorkFor(self)
                   THEN /\ idle' = idle \ {self}
                        /\ IF isSearch[self]
                              THEN /\ searching' = searching - 1
                                   /\ isSearch' = [isSearch EXCEPT ![self] = FALSE]
                              ELSE /\ TRUE
                                   /\ UNCHANGED << searching, isSearch >>
                        /\ pc' = [pc EXCEPT ![self] = "take"]
                   ELSE /\ pc' = [pc EXCEPT ![self] = "id3"]
                        /\ UNCHANGED << idle, searching, isSearch >>
             /\ UNCHANGED << local, next, pinned, inj, wakeCnt, ran, ranOn, 
                             cur, victims >>

id3(self) == /\ pc[self] = "id3"
             /\ IF isSearch[self]
                   THEN /\ searching' = searching - 1
                        /\ isSearch' = [isSearch EXCEPT ![self] = FALSE]
                   ELSE /\ TRUE
                        /\ UNCHANGED << searching, isSearch >>
             /\ pc' = [pc EXCEPT ![self] = "sleep"]
             /\ UNCHANGED << local, next, pinned, inj, idle, wakeCnt, ran, 
                             ranOn, cur, victims >>

sleep(self) == /\ pc[self] = "sleep"
               /\ wakeCnt[self] > 0
               /\ wakeCnt' = [wakeCnt EXCEPT ![self] = wakeCnt[self] - 1]
               /\ idle' = idle \ {self}
               /\ pc' = [pc EXCEPT ![self] = "take"]
               /\ UNCHANGED << local, next, pinned, inj, searching, ran, ranOn, 
                               cur, isSearch, victims >>

run(self) == /\ pc[self] = "run"
             /\ ran' = (ran \cup {cur[self]})
             /\ ranOn' = [ranOn EXCEPT ![cur[self]] = self]
             /\ IF cur[self] = 1
                   THEN /\ IF next[self] # 0
                              THEN /\ local' = [local EXCEPT ![self] = Append(local[self], next[self])]
                              ELSE /\ TRUE
                                   /\ local' = local
                        /\ next' = [next EXCEPT ![self] = 2]
                        /\ pc' = [pc EXCEPT ![self] = "r2"]
                   ELSE /\ pc' = [pc EXCEPT ![self] = "r5"]
                        /\ UNCHANGED << local, next >>
             /\ UNCHANGED << pinned, inj, idle, searching, wakeCnt, cur, 
                             isSearch, victims >>

r2(self) == /\ pc[self] = "r2"
            /\ IF idle # {} /\ (SimpNoSearching \/ searching = 0)
                  THEN /\ \E v \in idle:
                            /\ idle' = idle \ {v}
                            /\ wakeCnt' = [wakeCnt EXCEPT ![v] = wakeCnt[v] + 1]
                  ELSE /\ TRUE
                       /\ UNCHANGED << idle, wakeCnt >>
            /\ pc' = [pc EXCEPT ![self] = "r3"]
            /\ UNCHANGED << local, next, pinned, inj, searching, ran, ranOn, 
                            cur, isSearch, victims >>

r3(self) == /\ pc[self] = "r3"
            /\ pinned' = Append(pinned, 3)
            /\ pc' = [pc EXCEPT ![self] = "r4"]
            /\ UNCHANGED << local, next, inj, idle, searching, wakeCnt, ran, 
                            ranOn, cur, isSearch, victims >>

r4(self) == /\ pc[self] = "r4"
            /\ IF BugPinnedGeneric
                  THEN /\ IF idle # {} /\ (SimpNoSearching \/ searching = 0)
                             THEN /\ \E v \in idle:
                                       /\ idle' = idle \ {v}
                                       /\ wakeCnt' = [wakeCnt EXCEPT ![v] = wakeCnt[v] + 1]
                             ELSE /\ TRUE
                                  /\ UNCHANGED << idle, wakeCnt >>
                  ELSE /\ idle' = idle \ {0}
                       /\ wakeCnt' = [wakeCnt EXCEPT ![0] = wakeCnt[0] + 1]
            /\ pc' = [pc EXCEPT ![self] = "r5"]
            /\ UNCHANGED << local, next, pinned, inj, searching, ran, ranOn, 
                            cur, isSearch, victims >>

r5(self) == /\ pc[self] = "r5"
            /\ cur' = [cur EXCEPT ![self] = 0]
            /\ pc' = [pc EXCEPT ![self] = "take"]
            /\ UNCHANGED << local, next, pinned, inj, idle, searching, wakeCnt, 
                            ran, ranOn, isSearch, victims >>

dom(self) == take(self) \/ st0(self) \/ st1(self) \/ st2(self) \/ st3(self)
                \/ st4(self) \/ id0(self) \/ id1(self) \/ id2(self)
                \/ id3(self) \/ sleep(self) \/ run(self) \/ r2(self)
                \/ r3(self) \/ r4(self) \/ r5(self)

x1 == /\ pc[10] = "x1"
      /\ inj' = Append(inj, 4)
      /\ pc' = [pc EXCEPT ![10] = "x2"]
      /\ UNCHANGED << local, next, pinned, idle, searching, wakeCnt, ran, 
                      ranOn, cur, isSearch, victims >>

x2 == /\ pc[10] = "x2"
      /\ IF idle # {} /\ (SimpNoSearching \/ searching = 0)
            THEN /\ \E v \in idle:
                      /\ idle' = idle \ {v}
                      /\ wakeCnt' = [wakeCnt EXCEPT ![v] = wakeCnt[v] + 1]
            ELSE /\ TRUE
                 /\ UNCHANGED << idle, wakeCnt >>
      /\ pc' = [pc EXCEPT ![10] = "Done"]
      /\ UNCHANGED << local, next, pinned, inj, searching, ran, ranOn, cur, 
                      isSearch, victims >>

outside == x1 \/ x2

Next == outside
           \/ (\E self \in Doms: dom(self))

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in Doms : WF_vars(dom(self))
        /\ WF_vars(outside)

\* END TRANSLATION

-----------------------------------------------------------------------------
\* The pinned item runs only on domain 0.
PinnedOnZero == ranOn[3] \in {-1, 0}

\* Each item runs at most one time, and a running count stays correct.
SearchingOK == searching \in 0..Limit

\* No lost wakeup: when each domain sleeps with no wake and the outside
\* thread ended, no work waits and each item ran.
Asleep(d) == pc[d] = "sleep" /\ wakeCnt[d] = 0
NoLostWakeup ==
  ((\A d \in Doms : Asleep(d)) /\ pc[10] = "Done")
     => (~WorkExists /\ ran = Items)

\* With weak fairness, each item runs.
AllRun == <>(ran = Items)
=============================================================================
