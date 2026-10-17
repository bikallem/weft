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
  SimpNoSearching,    \* a simplification: no `searching` count, a push wakes each time
  BugTokenAfterRemove,\* the waker gives the token after it takes the domain out of `idle`
  BugTokenRetry,      \* a waker that takes its token back tries another idle domain
  SimpNoToken         \* the Go rules before the review of 2026-10-16: no token

Doms  == {0, 1, 2}
Items == {1, 2, 3, 4}
Limit == IF SimpNoSearchLimit THEN 3 ELSE 1   \* at most half of the domains search
NoDom == 9                                     \* no domain: the waker selected none

(* --algorithm Wake {
variables
  local = [d \in Doms |-> IF d = 0 THEN <<1>> ELSE <<>>],
  next = [d \in Doms |-> 0],
  pinned = <<>>,
  inj = <<>>,
  idle = {},
  searching = 0,
  got = [d \in Doms |-> FALSE],
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

\* Wake one idle domain in one step, with no token: the outside thread, the
\* planted bug of the pinned push, and each wake of SimpNoToken.
macro wakeOne() {
  if (idle # {} /\ (SimpNoSearching \/ searching = 0)) {
    with (v \in idle) { idle := idle \ {v}; wakeCnt[v] := wakeCnt[v] + 1; };
  };
}

fair process (dom \in Doms)
variables cur = 0, isSearch = FALSE, victims = {}, wv = NoDom, back = "run";
{
take:
  while (TRUE) {
    \* A token that a waker gave: the domain searches with it.
    if (got[self]) { got[self] := FALSE; isSearch := TRUE; };
    \* Steps 1 to 3 of the loop: the pinned queue (domain 0), the next slot,
    \* the local queue, the injection queue.
    if (self = 0 /\ pinned # <<>>) { cur := Head(pinned); pinned := Tail(pinned); }
    else if (next[self] # 0) { cur := next[self]; next[self] := 0; }
    else if (local[self] # <<>>) { cur := Head(local[self]); local[self] := Tail(local[self]); }
    else if (inj # <<>>) { cur := Head(inj); inj := Tail(inj); }
    else { cur := 0; };
    \* A domain with a token that finds work gives the token back (st3).
    if (cur # 0 /\ isSearch) { goto st3; }
    else if (cur # 0) { goto run; };
st0:
    \* Step 4: steal. Only `Limit` domains search at one time. A domain with a
    \* token searches with no increase.
    if (isSearch) { skip; }
    else if (SimpNoSearching) { isSearch := FALSE; }
    else if (searching < Limit) { searching := searching + 1; isSearch := TRUE; }
    else { isSearch := FALSE; goto id0; };
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
        if (SimpNoToken) {
          with (v \in idle) { idle := idle \ {v}; wakeCnt[v] := wakeCnt[v] + 1; };
          goto run;
        } else { back := "run"; goto w1; };
      } else { goto run; };
    };
id0:
    \* A searching domain that finds no work: the decrease comes first. Then
    \* the poll of the loop, which takes each wake that came before it.
    if (isSearch /\ ~BugDecAfterCheck) { searching := searching - 1; isSearch := FALSE; };
    wakeCnt[self] := 0;
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
      \* The push reads `idle` after it.
      if (SimpNoToken) { wakeOne(); goto r3; }
      else { back := "r3"; goto w1; };
r3:
      pinned := Append(pinned, 3);
      back := "run";
r4:
      if (BugPinnedGeneric) { wakeOne(); }
      else { idle := idle \ {0}; wakeCnt[0] := wakeCnt[0] + 1; };   \* wake domain 0
    };
r5:
    cur := 0;
    goto take;
    \* The wake with a token (the Go rule of wakep): a CAS of `searching` from
    \* 0 to 1, then the count goes to the woken domain, which searches.
w1:
    if (idle # {} /\ (SimpNoSearching \/ searching = 0)) { searching := searching + 1; }
    else if (back = "run") { goto run; } else { goto r3; };
w2:
    \* Select an idle domain. The token comes before the removal from `idle`.
    if (idle = {}) {
      searching := searching - 1;
      if (back = "run") { goto run; } else { goto r3; };
    } else {
      with (v \in idle) {
        wv := v;
        if (BugTokenAfterRemove) { idle := idle \ {v}; } else { got[v] := TRUE; };
      };
    };
w3:
    if (BugTokenAfterRemove) {
      got[wv] := TRUE;
    } else if (wv \in idle) {
      idle := idle \ {wv};
    } else if (got[wv]) {
      \* The domain left `idle` and did not take the token: take it back. The
      \* domain is awake, so it searches or checks again before it sleeps.
      got[wv] := FALSE; wv := NoDom;
      if (BugTokenRetry) { goto w2; } else { goto w5; };
    } else {
      \* The domain took the token: it searches, so it needs no wake.
      wv := NoDom;
      if (back = "run") { goto run; } else { goto r3; };
    };
w4:
    wakeCnt[wv] := wakeCnt[wv] + 1;
    wv := NoDom;
    if (back = "run") { goto run; } else { goto r3; };
w5:
    \* The count of the token goes back.
    searching := searching - 1;
    if (back = "run") { goto run; } else { goto r3; };
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
VARIABLES local, next, pinned, inj, idle, searching, got, wakeCnt, ran, ranOn, 
          pc

(* define statement *)
WorkFor(d) ==
  \/ next[d] # 0 \/ local[d] # <<>> \/ inj # <<>>
  \/ (d = 0 /\ pinned # <<>>)
  \/ \E v \in Doms \ {d} : local[v] # <<>> \/ (~BugNoNextSteal /\ next[v] # 0)
WorkExists ==
  \/ inj # <<>> \/ pinned # <<>>
  \/ \E d \in Doms : local[d] # <<>> \/ next[d] # 0

VARIABLES cur, isSearch, victims, wv, back

vars == << local, next, pinned, inj, idle, searching, got, wakeCnt, ran, 
           ranOn, pc, cur, isSearch, victims, wv, back >>

ProcSet == (Doms) \cup {10}

Init == (* Global variables *)
        /\ local = [d \in Doms |-> IF d = 0 THEN <<1>> ELSE <<>>]
        /\ next = [d \in Doms |-> 0]
        /\ pinned = <<>>
        /\ inj = <<>>
        /\ idle = {}
        /\ searching = 0
        /\ got = [d \in Doms |-> FALSE]
        /\ wakeCnt = [d \in Doms |-> 0]
        /\ ran = {}
        /\ ranOn = [i \in Items |-> -1]
        (* Process dom *)
        /\ cur = [self \in Doms |-> 0]
        /\ isSearch = [self \in Doms |-> FALSE]
        /\ victims = [self \in Doms |-> {}]
        /\ wv = [self \in Doms |-> NoDom]
        /\ back = [self \in Doms |-> "run"]
        /\ pc = [self \in ProcSet |-> CASE self \in Doms -> "take"
                                        [] self = 10 -> "x1"]

take(self) == /\ pc[self] = "take"
              /\ IF got[self]
                    THEN /\ got' = [got EXCEPT ![self] = FALSE]
                         /\ isSearch' = [isSearch EXCEPT ![self] = TRUE]
                    ELSE /\ TRUE
                         /\ UNCHANGED << got, isSearch >>
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
              /\ IF cur'[self] # 0 /\ isSearch'[self]
                    THEN /\ pc' = [pc EXCEPT ![self] = "st3"]
                    ELSE /\ IF cur'[self] # 0
                               THEN /\ pc' = [pc EXCEPT ![self] = "run"]
                               ELSE /\ pc' = [pc EXCEPT ![self] = "st0"]
              /\ UNCHANGED << idle, searching, wakeCnt, ran, ranOn, victims, 
                              wv, back >>

st0(self) == /\ pc[self] = "st0"
             /\ IF isSearch[self]
                   THEN /\ TRUE
                        /\ pc' = [pc EXCEPT ![self] = "st1"]
                        /\ UNCHANGED << searching, isSearch >>
                   ELSE /\ IF SimpNoSearching
                              THEN /\ isSearch' = [isSearch EXCEPT ![self] = FALSE]
                                   /\ pc' = [pc EXCEPT ![self] = "st1"]
                                   /\ UNCHANGED searching
                              ELSE /\ IF searching < Limit
                                         THEN /\ searching' = searching + 1
                                              /\ isSearch' = [isSearch EXCEPT ![self] = TRUE]
                                              /\ pc' = [pc EXCEPT ![self] = "st1"]
                                         ELSE /\ isSearch' = [isSearch EXCEPT ![self] = FALSE]
                                              /\ pc' = [pc EXCEPT ![self] = "id0"]
                                              /\ UNCHANGED searching
             /\ UNCHANGED << local, next, pinned, inj, idle, got, wakeCnt, ran, 
                             ranOn, cur, victims, wv, back >>

st1(self) == /\ pc[self] = "st1"
             /\ victims' = [victims EXCEPT ![self] = Doms \ {self}]
             /\ pc' = [pc EXCEPT ![self] = "st2"]
             /\ UNCHANGED << local, next, pinned, inj, idle, searching, got, 
                             wakeCnt, ran, ranOn, cur, isSearch, wv, back >>

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
             /\ UNCHANGED << pinned, inj, idle, searching, got, wakeCnt, ran, 
                             ranOn, isSearch, wv, back >>

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
             /\ UNCHANGED << local, next, pinned, inj, idle, got, wakeCnt, ran, 
                             ranOn, cur, victims, wv, back >>

st4(self) == /\ pc[self] = "st4"
             /\ IF ~SimpNoLastWake /\ searching = 0 /\ idle # {}
                   THEN /\ IF SimpNoToken
                              THEN /\ \E v \in idle:
                                        /\ idle' = idle \ {v}
                                        /\ wakeCnt' = [wakeCnt EXCEPT ![v] = wakeCnt[v] + 1]
                                   /\ pc' = [pc EXCEPT ![self] = "run"]
                                   /\ back' = back
                              ELSE /\ back' = [back EXCEPT ![self] = "run"]
                                   /\ pc' = [pc EXCEPT ![self] = "w1"]
                                   /\ UNCHANGED << idle, wakeCnt >>
                   ELSE /\ pc' = [pc EXCEPT ![self] = "run"]
                        /\ UNCHANGED << idle, wakeCnt, back >>
             /\ UNCHANGED << local, next, pinned, inj, searching, got, ran, 
                             ranOn, cur, isSearch, victims, wv >>

id0(self) == /\ pc[self] = "id0"
             /\ IF isSearch[self] /\ ~BugDecAfterCheck
                   THEN /\ searching' = searching - 1
                        /\ isSearch' = [isSearch EXCEPT ![self] = FALSE]
                   ELSE /\ TRUE
                        /\ UNCHANGED << searching, isSearch >>
             /\ wakeCnt' = [wakeCnt EXCEPT ![self] = 0]
             /\ pc' = [pc EXCEPT ![self] = "id1"]
             /\ UNCHANGED << local, next, pinned, inj, idle, got, ran, ranOn, 
                             cur, victims, wv, back >>

id1(self) == /\ pc[self] = "id1"
             /\ idle' = (idle \cup {self})
             /\ pc' = [pc EXCEPT ![self] = "id2"]
             /\ UNCHANGED << local, next, pinned, inj, searching, got, wakeCnt, 
                             ran, ranOn, cur, isSearch, victims, wv, back >>

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
             /\ UNCHANGED << local, next, pinned, inj, got, wakeCnt, ran, 
                             ranOn, cur, victims, wv, back >>

id3(self) == /\ pc[self] = "id3"
             /\ IF isSearch[self]
                   THEN /\ searching' = searching - 1
                        /\ isSearch' = [isSearch EXCEPT ![self] = FALSE]
                   ELSE /\ TRUE
                        /\ UNCHANGED << searching, isSearch >>
             /\ pc' = [pc EXCEPT ![self] = "sleep"]
             /\ UNCHANGED << local, next, pinned, inj, idle, got, wakeCnt, ran, 
                             ranOn, cur, victims, wv, back >>

sleep(self) == /\ pc[self] = "sleep"
               /\ wakeCnt[self] > 0
               /\ wakeCnt' = [wakeCnt EXCEPT ![self] = wakeCnt[self] - 1]
               /\ idle' = idle \ {self}
               /\ pc' = [pc EXCEPT ![self] = "take"]
               /\ UNCHANGED << local, next, pinned, inj, searching, got, ran, 
                               ranOn, cur, isSearch, victims, wv, back >>

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
             /\ UNCHANGED << pinned, inj, idle, searching, got, wakeCnt, cur, 
                             isSearch, victims, wv, back >>

r2(self) == /\ pc[self] = "r2"
            /\ IF SimpNoToken
                  THEN /\ IF idle # {} /\ (SimpNoSearching \/ searching = 0)
                             THEN /\ \E v \in idle:
                                       /\ idle' = idle \ {v}
                                       /\ wakeCnt' = [wakeCnt EXCEPT ![v] = wakeCnt[v] + 1]
                             ELSE /\ TRUE
                                  /\ UNCHANGED << idle, wakeCnt >>
                       /\ pc' = [pc EXCEPT ![self] = "r3"]
                       /\ back' = back
                  ELSE /\ back' = [back EXCEPT ![self] = "r3"]
                       /\ pc' = [pc EXCEPT ![self] = "w1"]
                       /\ UNCHANGED << idle, wakeCnt >>
            /\ UNCHANGED << local, next, pinned, inj, searching, got, ran, 
                            ranOn, cur, isSearch, victims, wv >>

r3(self) == /\ pc[self] = "r3"
            /\ pinned' = Append(pinned, 3)
            /\ back' = [back EXCEPT ![self] = "run"]
            /\ pc' = [pc EXCEPT ![self] = "r4"]
            /\ UNCHANGED << local, next, inj, idle, searching, got, wakeCnt, 
                            ran, ranOn, cur, isSearch, victims, wv >>

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
            /\ UNCHANGED << local, next, pinned, inj, searching, got, ran, 
                            ranOn, cur, isSearch, victims, wv, back >>

r5(self) == /\ pc[self] = "r5"
            /\ cur' = [cur EXCEPT ![self] = 0]
            /\ pc' = [pc EXCEPT ![self] = "take"]
            /\ UNCHANGED << local, next, pinned, inj, idle, searching, got, 
                            wakeCnt, ran, ranOn, isSearch, victims, wv, back >>

w1(self) == /\ pc[self] = "w1"
            /\ IF idle # {} /\ (SimpNoSearching \/ searching = 0)
                  THEN /\ searching' = searching + 1
                       /\ pc' = [pc EXCEPT ![self] = "w2"]
                  ELSE /\ IF back[self] = "run"
                             THEN /\ pc' = [pc EXCEPT ![self] = "run"]
                             ELSE /\ pc' = [pc EXCEPT ![self] = "r3"]
                       /\ UNCHANGED searching
            /\ UNCHANGED << local, next, pinned, inj, idle, got, wakeCnt, ran, 
                            ranOn, cur, isSearch, victims, wv, back >>

w2(self) == /\ pc[self] = "w2"
            /\ IF idle = {}
                  THEN /\ searching' = searching - 1
                       /\ IF back[self] = "run"
                             THEN /\ pc' = [pc EXCEPT ![self] = "run"]
                             ELSE /\ pc' = [pc EXCEPT ![self] = "r3"]
                       /\ UNCHANGED << idle, got, wv >>
                  ELSE /\ \E v \in idle:
                            /\ wv' = [wv EXCEPT ![self] = v]
                            /\ IF BugTokenAfterRemove
                                  THEN /\ idle' = idle \ {v}
                                       /\ got' = got
                                  ELSE /\ got' = [got EXCEPT ![v] = TRUE]
                                       /\ idle' = idle
                       /\ pc' = [pc EXCEPT ![self] = "w3"]
                       /\ UNCHANGED searching
            /\ UNCHANGED << local, next, pinned, inj, wakeCnt, ran, ranOn, cur, 
                            isSearch, victims, back >>

w3(self) == /\ pc[self] = "w3"
            /\ IF BugTokenAfterRemove
                  THEN /\ got' = [got EXCEPT ![wv[self]] = TRUE]
                       /\ pc' = [pc EXCEPT ![self] = "w4"]
                       /\ UNCHANGED << idle, wv >>
                  ELSE /\ IF wv[self] \in idle
                             THEN /\ idle' = idle \ {wv[self]}
                                  /\ pc' = [pc EXCEPT ![self] = "w4"]
                                  /\ UNCHANGED << got, wv >>
                             ELSE /\ IF got[wv[self]]
                                        THEN /\ got' = [got EXCEPT ![wv[self]] = FALSE]
                                             /\ wv' = [wv EXCEPT ![self] = NoDom]
                                             /\ IF BugTokenRetry
                                                   THEN /\ pc' = [pc EXCEPT ![self] = "w2"]
                                                   ELSE /\ pc' = [pc EXCEPT ![self] = "w5"]
                                        ELSE /\ wv' = [wv EXCEPT ![self] = NoDom]
                                             /\ IF back[self] = "run"
                                                   THEN /\ pc' = [pc EXCEPT ![self] = "run"]
                                                   ELSE /\ pc' = [pc EXCEPT ![self] = "r3"]
                                             /\ got' = got
                                  /\ idle' = idle
            /\ UNCHANGED << local, next, pinned, inj, searching, wakeCnt, ran, 
                            ranOn, cur, isSearch, victims, back >>

w4(self) == /\ pc[self] = "w4"
            /\ wakeCnt' = [wakeCnt EXCEPT ![wv[self]] = wakeCnt[wv[self]] + 1]
            /\ wv' = [wv EXCEPT ![self] = NoDom]
            /\ IF back[self] = "run"
                  THEN /\ pc' = [pc EXCEPT ![self] = "run"]
                  ELSE /\ pc' = [pc EXCEPT ![self] = "r3"]
            /\ UNCHANGED << local, next, pinned, inj, idle, searching, got, 
                            ran, ranOn, cur, isSearch, victims, back >>

w5(self) == /\ pc[self] = "w5"
            /\ searching' = searching - 1
            /\ IF back[self] = "run"
                  THEN /\ pc' = [pc EXCEPT ![self] = "run"]
                  ELSE /\ pc' = [pc EXCEPT ![self] = "r3"]
            /\ UNCHANGED << local, next, pinned, inj, idle, got, wakeCnt, ran, 
                            ranOn, cur, isSearch, victims, wv, back >>

dom(self) == take(self) \/ st0(self) \/ st1(self) \/ st2(self) \/ st3(self)
                \/ st4(self) \/ id0(self) \/ id1(self) \/ id2(self)
                \/ id3(self) \/ sleep(self) \/ run(self) \/ r2(self)
                \/ r3(self) \/ r4(self) \/ r5(self) \/ w1(self) \/ w2(self)
                \/ w3(self) \/ w4(self) \/ w5(self)

x1 == /\ pc[10] = "x1"
      /\ inj' = Append(inj, 4)
      /\ pc' = [pc EXCEPT ![10] = "x2"]
      /\ UNCHANGED << local, next, pinned, idle, searching, got, wakeCnt, ran, 
                      ranOn, cur, isSearch, victims, wv, back >>

x2 == /\ pc[10] = "x2"
      /\ IF idle # {} /\ (SimpNoSearching \/ searching = 0)
            THEN /\ \E v \in idle:
                      /\ idle' = idle \ {v}
                      /\ wakeCnt' = [wakeCnt EXCEPT ![v] = wakeCnt[v] + 1]
            ELSE /\ TRUE
                 /\ UNCHANGED << idle, wakeCnt >>
      /\ pc' = [pc EXCEPT ![10] = "Done"]
      /\ UNCHANGED << local, next, pinned, inj, searching, got, ran, ranOn, 
                      cur, isSearch, victims, wv, back >>

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

\* No lost token: a domain that sleeps with no wake holds a token only while
\* its waker did not yet wake it.
TokenOK ==
  \A d \in Doms : (Asleep(d) /\ got[d]) => \E w \in Doms : wv[w] = d

\* With weak fairness, each item runs.
AllRun == <>(ran = Items)
=============================================================================
