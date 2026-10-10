------------------------------- MODULE Commit -------------------------------
(***************************************************************************)
(* A model of the commit protocol of docs/design.md, section 2.4.          *)
(*                                                                         *)
(* Each fiber does one sync with a choose over its branches. A sync has   *)
(* one state word: W (Waiting), P (Parked), C (Claimed) or D (Done). The   *)
(* id of a sync is the number of its fiber, so the ids are totally         *)
(* ordered.                                                                *)
(*                                                                         *)
(* Each label of the PlusCal code is one atomic step. A CAS on a word is   *)
(* one step, so each await on a word is in the same step as its CAS.       *)
(*                                                                         *)
(* The parts of the protocol:                                              *)
(*   - The scan: for each branch, find a partner and commit, or register   *)
(*     an offer. A channel branch holds the lock of the channel for this   *)
(*     (the atomic register).                                              *)
(*   - A claim of two words, in id order. A word that is C makes the       *)
(*     claimer wait. A word that is D is stale, or it is the own word of a *)
(*     sync that a partner committed.                                      *)
(*   - One owner for the resume: a claimer resumes the partner only if it *)
(*     replaced P. A fiber that ends its scan with W changes it to P.      *)
(*   - One-word partners (a cancel, a timer) change W or P to D, and they  *)
(*     wait while the word is C.                                           *)
(*   - A ready branch with no partner commits itself (W to D).             *)
(*                                                                         *)
(* A parked fiber ends its process. A resume is a count in "resumes", so a *)
(* fiber that nobody resumes is not a deadlock of TLC. Each await that     *)
(* waits forever is a deadlock, and TLC reports it.                        *)
(*                                                                         *)
(* The Bug* constants plant the bugs of the reviews, to show that the     *)
(* model finds them.                                                       *)
(***************************************************************************)
EXTENDS Naturals, FiniteSets

CONSTANTS
  Scenario,        \* "cross", "stale" or "self": the branches of each fiber
  BugNoParked,     \* a claimer always resumes its partner (no Parked state)
  BugNoIdOrder,    \* a claimer claims its own word first, not the lower id
  BugDropOnClaimed,\* a one-word partner gives up when the word is C
  BugNoLock        \* "find a partner, else register" with no channel lock

Fibers == CASE Scenario = "cross" -> 1..2
          []   Scenario = "stale" -> 1..3
          []   Scenario = "self"  -> 1..2
Chans  == 1..2

B(k, c) == [kind |-> k, ch |-> c]

\* The branches of each fiber.
Br(f) ==
  CASE Scenario = "cross" ->
         IF f = 1 THEN {B("send", 1), B("recv", 2)}
                  ELSE {B("send", 2), B("recv", 1)}
  []   Scenario = "stale" ->
         IF f = 1 THEN {B("recv", 1), B("timer", 0)}
         ELSE IF f = 2 THEN {B("send", 1), B("recv", 2)}
         ELSE {B("send", 1), B("send", 2)}
  []   Scenario = "self" ->
         IF f = 1 THEN {B("ready", 0), B("recv", 1)}
                  ELSE {B("send", 1)}

\* The fiber that the canceller cancels (0: no canceller).
CancelTarget == 1
\* The fibers with a timer branch.
TimerFibers == {f \in Fibers : \E b \in Br(f) : b.kind = "timer"}

Opp(k) == IF k = "send" THEN "recv" ELSE "send"
Min(a, b) == IF a < b THEN a ELSE b
Max(a, b) == IF a < b THEN b ELSE a

NoRes == [k |-> "none", c |-> 0, p |-> 0]

(* --algorithm Commit {
variables
  word = [f \in Fibers |-> "W"],
  res = [f \in Fibers |-> NoRes],
  resumes = [f \in Fibers |-> 0],
  offers = [c \in Chans |-> {}],
  lock = [c \in Chans |-> 0],
  timerReg = {};

fair process (fib \in Fibers)
variables
  todo = Br(self), b = B("none", 0), p = 0, lo = 0, hi = 0,
  oldLo = "W", oldHi = "W", cand = {};
{
scan:
  while (todo # {} /\ word[self] # "D") {
    with (x \in todo) { b := x; todo := todo \ {x} };
    if (b.kind = "ready") {
selfc:
      await word[self] # "C";
      if (word[self] = "W") {
        word[self] := "D";
        res[self] := [k |-> "ready", c |-> 0, p |-> 0];
      };
    } else if (b.kind = "timer") {
treg:
      timerReg := timerReg \cup {self};
    } else {
lockc:
      if (~BugNoLock) { await lock[b.ch] = 0; lock[b.ch] := self; };
look:
      cand := {q \in Fibers \ {self} : <<q, Opp(b.kind)>> \in offers[b.ch]};
      if (cand = {}) {
reg:
        \* A separate step: without the lock, a partner can look in between.
        offers[b.ch] := offers[b.ch] \cup {<<self, b.kind>>};
      } else {
        with (q \in cand) { p := q };
        if (BugNoIdOrder) { lo := self; hi := p; }
        else { lo := Min(self, p); hi := Max(self, p); };
claimLo:
        await word[lo] # "C";
        if (word[lo] = "D") {
          if (lo # self) {
            \* A stale offer: remove it and look again.
            offers[b.ch] := offers[b.ch] \ {<<p, Opp(b.kind)>>};
            goto look;
          };
          \* Own word is D: a partner committed this sync during the scan.
        } else {
          oldLo := word[lo];
          word[lo] := "C";
claimHi:
          await word[hi] # "C";
          if (word[hi] = "D") {
            word[lo] := oldLo;
            if (hi # self) {
              offers[b.ch] := offers[b.ch] \ {<<p, Opp(b.kind)>>};
              goto look;
            };
          } else {
            oldHi := word[hi];
            word[hi] := "C";
commit:
            word[self] := "D" || word[p] := "D";
            res[self] := [k |-> "chan", c |-> b.ch, p |-> p]
              || res[p] := [k |-> "chan", c |-> b.ch, p |-> self];
            offers[b.ch] := offers[b.ch] \ {<<p, Opp(b.kind)>>};
            \* Resume the partner only if this claim replaced P.
            if (BugNoParked \/ (IF p = lo THEN oldLo ELSE oldHi) = "P") {
              resumes[p] := resumes[p] + 1;
            };
          };
        };
      };
unlock:
      if (~BugNoLock) { lock[b.ch] := 0; };
    };
  };
endscan:
  await word[self] # "C";
  if (word[self] = "W") {
    word[self] := "P";
    goto Done;          \* parked: a partner resumes it, or nobody does
  };
cont:
  resumes[self] := resumes[self] + 1;
}

\* A cancel: a one-word partner of the cancelled fiber.
fair process (canc = 100)
variable old = "W";
{
c1:
  if (CancelTarget \in Fibers) {
    await BugDropOnClaimed \/ word[CancelTarget] # "C";
    if (word[CancelTarget] \in {"W", "P"}) {
      old := word[CancelTarget];
      word[CancelTarget] := "D";
      res[CancelTarget] := [k |-> "cancel", c |-> 0, p |-> 0];
      if (old = "P") { resumes[CancelTarget] := resumes[CancelTarget] + 1; };
    };
  };
}

\* A timer: a one-word partner that fires only after its fiber registered it.
fair process (tim \in {200 + f : f \in TimerFibers})
variable told = "W";
{
t1:
  \* The timer fires only after its fiber registered it. A fiber that ended
  \* before it registered the timer has no timer.
  await (self - 200) \in timerReg \/ pc[self - 200] = "Done";
  if ((self - 200) \notin timerReg) { goto Done; };
t2:
  await BugDropOnClaimed \/ word[self - 200] # "C";
  if (word[self - 200] \in {"W", "P"}) {
    told := word[self - 200];
    word[self - 200] := "D";
    res[self - 200] := [k |-> "timer", c |-> 0, p |-> 0];
    if (told = "P") { resumes[self - 200] := resumes[self - 200] + 1; };
  };
}
} *)

\* BEGIN TRANSLATION
VARIABLES word, res, resumes, offers, lock, timerReg, pc, todo, b, p, lo, hi, 
          oldLo, oldHi, cand, old, told

vars == << word, res, resumes, offers, lock, timerReg, pc, todo, b, p, lo, hi, 
           oldLo, oldHi, cand, old, told >>

ProcSet == (Fibers) \cup {100} \cup ({200 + f : f \in TimerFibers})

Init == (* Global variables *)
        /\ word = [f \in Fibers |-> "W"]
        /\ res = [f \in Fibers |-> NoRes]
        /\ resumes = [f \in Fibers |-> 0]
        /\ offers = [c \in Chans |-> {}]
        /\ lock = [c \in Chans |-> 0]
        /\ timerReg = {}
        (* Process fib *)
        /\ todo = [self \in Fibers |-> Br(self)]
        /\ b = [self \in Fibers |-> B("none", 0)]
        /\ p = [self \in Fibers |-> 0]
        /\ lo = [self \in Fibers |-> 0]
        /\ hi = [self \in Fibers |-> 0]
        /\ oldLo = [self \in Fibers |-> "W"]
        /\ oldHi = [self \in Fibers |-> "W"]
        /\ cand = [self \in Fibers |-> {}]
        (* Process canc *)
        /\ old = "W"
        (* Process tim *)
        /\ told = [self \in {200 + f : f \in TimerFibers} |-> "W"]
        /\ pc = [self \in ProcSet |-> CASE self \in Fibers -> "scan"
                                        [] self = 100 -> "c1"
                                        [] self \in {200 + f : f \in TimerFibers} -> "t1"]

scan(self) == /\ pc[self] = "scan"
              /\ IF todo[self] # {} /\ word[self] # "D"
                    THEN /\ \E x \in todo[self]:
                              /\ b' = [b EXCEPT ![self] = x]
                              /\ todo' = [todo EXCEPT ![self] = todo[self] \ {x}]
                         /\ IF b'[self].kind = "ready"
                               THEN /\ pc' = [pc EXCEPT ![self] = "selfc"]
                               ELSE /\ IF b'[self].kind = "timer"
                                          THEN /\ pc' = [pc EXCEPT ![self] = "treg"]
                                          ELSE /\ pc' = [pc EXCEPT ![self] = "lockc"]
                    ELSE /\ pc' = [pc EXCEPT ![self] = "endscan"]
                         /\ UNCHANGED << todo, b >>
              /\ UNCHANGED << word, res, resumes, offers, lock, timerReg, p, 
                              lo, hi, oldLo, oldHi, cand, old, told >>

selfc(self) == /\ pc[self] = "selfc"
               /\ word[self] # "C"
               /\ IF word[self] = "W"
                     THEN /\ word' = [word EXCEPT ![self] = "D"]
                          /\ res' = [res EXCEPT ![self] = [k |-> "ready", c |-> 0, p |-> 0]]
                     ELSE /\ TRUE
                          /\ UNCHANGED << word, res >>
               /\ pc' = [pc EXCEPT ![self] = "scan"]
               /\ UNCHANGED << resumes, offers, lock, timerReg, todo, b, p, lo, 
                               hi, oldLo, oldHi, cand, old, told >>

treg(self) == /\ pc[self] = "treg"
              /\ timerReg' = (timerReg \cup {self})
              /\ pc' = [pc EXCEPT ![self] = "scan"]
              /\ UNCHANGED << word, res, resumes, offers, lock, todo, b, p, lo, 
                              hi, oldLo, oldHi, cand, old, told >>

lockc(self) == /\ pc[self] = "lockc"
               /\ IF ~BugNoLock
                     THEN /\ lock[b[self].ch] = 0
                          /\ lock' = [lock EXCEPT ![b[self].ch] = self]
                     ELSE /\ TRUE
                          /\ lock' = lock
               /\ pc' = [pc EXCEPT ![self] = "look"]
               /\ UNCHANGED << word, res, resumes, offers, timerReg, todo, b, 
                               p, lo, hi, oldLo, oldHi, cand, old, told >>

look(self) == /\ pc[self] = "look"
              /\ cand' = [cand EXCEPT ![self] = {q \in Fibers \ {self} : <<q, Opp(b[self].kind)>> \in offers[b[self].ch]}]
              /\ IF cand'[self] = {}
                    THEN /\ pc' = [pc EXCEPT ![self] = "reg"]
                         /\ UNCHANGED << p, lo, hi >>
                    ELSE /\ \E q \in cand'[self]:
                              p' = [p EXCEPT ![self] = q]
                         /\ IF BugNoIdOrder
                               THEN /\ lo' = [lo EXCEPT ![self] = self]
                                    /\ hi' = [hi EXCEPT ![self] = p'[self]]
                               ELSE /\ lo' = [lo EXCEPT ![self] = Min(self, p'[self])]
                                    /\ hi' = [hi EXCEPT ![self] = Max(self, p'[self])]
                         /\ pc' = [pc EXCEPT ![self] = "claimLo"]
              /\ UNCHANGED << word, res, resumes, offers, lock, timerReg, todo, 
                              b, oldLo, oldHi, old, told >>

reg(self) == /\ pc[self] = "reg"
             /\ offers' = [offers EXCEPT ![b[self].ch] = offers[b[self].ch] \cup {<<self, b[self].kind>>}]
             /\ pc' = [pc EXCEPT ![self] = "unlock"]
             /\ UNCHANGED << word, res, resumes, lock, timerReg, todo, b, p, 
                             lo, hi, oldLo, oldHi, cand, old, told >>

claimLo(self) == /\ pc[self] = "claimLo"
                 /\ word[lo[self]] # "C"
                 /\ IF word[lo[self]] = "D"
                       THEN /\ IF lo[self] # self
                                  THEN /\ offers' = [offers EXCEPT ![b[self].ch] = offers[b[self].ch] \ {<<p[self], Opp(b[self].kind)>>}]
                                       /\ pc' = [pc EXCEPT ![self] = "look"]
                                  ELSE /\ pc' = [pc EXCEPT ![self] = "unlock"]
                                       /\ UNCHANGED offers
                            /\ UNCHANGED << word, oldLo >>
                       ELSE /\ oldLo' = [oldLo EXCEPT ![self] = word[lo[self]]]
                            /\ word' = [word EXCEPT ![lo[self]] = "C"]
                            /\ pc' = [pc EXCEPT ![self] = "claimHi"]
                            /\ UNCHANGED offers
                 /\ UNCHANGED << res, resumes, lock, timerReg, todo, b, p, lo, 
                                 hi, oldHi, cand, old, told >>

claimHi(self) == /\ pc[self] = "claimHi"
                 /\ word[hi[self]] # "C"
                 /\ IF word[hi[self]] = "D"
                       THEN /\ word' = [word EXCEPT ![lo[self]] = oldLo[self]]
                            /\ IF hi[self] # self
                                  THEN /\ offers' = [offers EXCEPT ![b[self].ch] = offers[b[self].ch] \ {<<p[self], Opp(b[self].kind)>>}]
                                       /\ pc' = [pc EXCEPT ![self] = "look"]
                                  ELSE /\ pc' = [pc EXCEPT ![self] = "unlock"]
                                       /\ UNCHANGED offers
                            /\ oldHi' = oldHi
                       ELSE /\ oldHi' = [oldHi EXCEPT ![self] = word[hi[self]]]
                            /\ word' = [word EXCEPT ![hi[self]] = "C"]
                            /\ pc' = [pc EXCEPT ![self] = "commit"]
                            /\ UNCHANGED offers
                 /\ UNCHANGED << res, resumes, lock, timerReg, todo, b, p, lo, 
                                 hi, oldLo, cand, old, told >>

commit(self) == /\ pc[self] = "commit"
                /\ word' = [word EXCEPT ![self] = "D",
                                        ![p[self]] = "D"]
                /\ res' = [res EXCEPT ![self] = [k |-> "chan", c |-> b[self].ch, p |-> p[self]],
                                      ![p[self]] = [k |-> "chan", c |-> b[self].ch, p |-> self]]
                /\ offers' = [offers EXCEPT ![b[self].ch] = offers[b[self].ch] \ {<<p[self], Opp(b[self].kind)>>}]
                /\ IF BugNoParked \/ (IF p[self] = lo[self] THEN oldLo[self] ELSE oldHi[self]) = "P"
                      THEN /\ resumes' = [resumes EXCEPT ![p[self]] = resumes[p[self]] + 1]
                      ELSE /\ TRUE
                           /\ UNCHANGED resumes
                /\ pc' = [pc EXCEPT ![self] = "unlock"]
                /\ UNCHANGED << lock, timerReg, todo, b, p, lo, hi, oldLo, 
                                oldHi, cand, old, told >>

unlock(self) == /\ pc[self] = "unlock"
                /\ IF ~BugNoLock
                      THEN /\ lock' = [lock EXCEPT ![b[self].ch] = 0]
                      ELSE /\ TRUE
                           /\ lock' = lock
                /\ pc' = [pc EXCEPT ![self] = "scan"]
                /\ UNCHANGED << word, res, resumes, offers, timerReg, todo, b, 
                                p, lo, hi, oldLo, oldHi, cand, old, told >>

endscan(self) == /\ pc[self] = "endscan"
                 /\ word[self] # "C"
                 /\ IF word[self] = "W"
                       THEN /\ word' = [word EXCEPT ![self] = "P"]
                            /\ pc' = [pc EXCEPT ![self] = "Done"]
                       ELSE /\ pc' = [pc EXCEPT ![self] = "cont"]
                            /\ word' = word
                 /\ UNCHANGED << res, resumes, offers, lock, timerReg, todo, b, 
                                 p, lo, hi, oldLo, oldHi, cand, old, told >>

cont(self) == /\ pc[self] = "cont"
              /\ resumes' = [resumes EXCEPT ![self] = resumes[self] + 1]
              /\ pc' = [pc EXCEPT ![self] = "Done"]
              /\ UNCHANGED << word, res, offers, lock, timerReg, todo, b, p, 
                              lo, hi, oldLo, oldHi, cand, old, told >>

fib(self) == scan(self) \/ selfc(self) \/ treg(self) \/ lockc(self)
                \/ look(self) \/ reg(self) \/ claimLo(self)
                \/ claimHi(self) \/ commit(self) \/ unlock(self)
                \/ endscan(self) \/ cont(self)

c1 == /\ pc[100] = "c1"
      /\ IF CancelTarget \in Fibers
            THEN /\ BugDropOnClaimed \/ word[CancelTarget] # "C"
                 /\ IF word[CancelTarget] \in {"W", "P"}
                       THEN /\ old' = word[CancelTarget]
                            /\ word' = [word EXCEPT ![CancelTarget] = "D"]
                            /\ res' = [res EXCEPT ![CancelTarget] = [k |-> "cancel", c |-> 0, p |-> 0]]
                            /\ IF old' = "P"
                                  THEN /\ resumes' = [resumes EXCEPT ![CancelTarget] = resumes[CancelTarget] + 1]
                                  ELSE /\ TRUE
                                       /\ UNCHANGED resumes
                       ELSE /\ TRUE
                            /\ UNCHANGED << word, res, resumes, old >>
            ELSE /\ TRUE
                 /\ UNCHANGED << word, res, resumes, old >>
      /\ pc' = [pc EXCEPT ![100] = "Done"]
      /\ UNCHANGED << offers, lock, timerReg, todo, b, p, lo, hi, oldLo, oldHi, 
                      cand, told >>

canc == c1

t1(self) == /\ pc[self] = "t1"
            /\ (self - 200) \in timerReg \/ pc[self - 200] = "Done"
            /\ IF (self - 200) \notin timerReg
                  THEN /\ pc' = [pc EXCEPT ![self] = "Done"]
                  ELSE /\ pc' = [pc EXCEPT ![self] = "t2"]
            /\ UNCHANGED << word, res, resumes, offers, lock, timerReg, todo, 
                            b, p, lo, hi, oldLo, oldHi, cand, old, told >>

t2(self) == /\ pc[self] = "t2"
            /\ BugDropOnClaimed \/ word[self - 200] # "C"
            /\ IF word[self - 200] \in {"W", "P"}
                  THEN /\ told' = [told EXCEPT ![self] = word[self - 200]]
                       /\ word' = [word EXCEPT ![self - 200] = "D"]
                       /\ res' = [res EXCEPT ![self - 200] = [k |-> "timer", c |-> 0, p |-> 0]]
                       /\ IF told'[self] = "P"
                             THEN /\ resumes' = [resumes EXCEPT ![self - 200] = resumes[self - 200] + 1]
                             ELSE /\ TRUE
                                  /\ UNCHANGED resumes
                  ELSE /\ TRUE
                       /\ UNCHANGED << word, res, resumes, told >>
            /\ pc' = [pc EXCEPT ![self] = "Done"]
            /\ UNCHANGED << offers, lock, timerReg, todo, b, p, lo, hi, oldLo, 
                            oldHi, cand, old >>

tim(self) == t1(self) \/ t2(self)

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == canc
           \/ (\E self \in Fibers: fib(self))
           \/ (\E self \in {200 + f : f \in TimerFibers}: tim(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in Fibers : WF_vars(fib(self))
        /\ WF_vars(canc)
        /\ \A self \in {200 + f : f \in TimerFibers} : WF_vars(tim(self))

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION

-----------------------------------------------------------------------------
(* The properties. *)

TypeOK ==
  /\ word \in [Fibers -> {"W", "P", "C", "D"}]
  /\ resumes \in [Fibers -> 0..3]

\* Each continuation has at most one resume.
AtMostOneResume == \A f \in Fibers : resumes[f] <= 1

\* A word is D exactly when the sync has a result.
DoneHasResult == \A f \in Fibers : (word[f] = "D") <=> (res[f].k # "none")

\* A rendezvous pairs the two syncs, on one channel, with opposite kinds.
RendezvousPairs ==
  \A f \in Fibers : res[f].k = "chan" =>
     LET g == res[f].p  c == res[f].c IN
       /\ res[g] = [k |-> "chan", c |-> c, p |-> f]
       /\ \E b1 \in Br(f), b2 \in Br(g) :
             b1.ch = c /\ b2.ch = c /\ b1.kind # b2.kind
             /\ b1.kind \in {"send", "recv"} /\ b2.kind \in {"send", "recv"}

\* No lost rendezvous: two parked syncs never have matching offers.
NoLostRendezvous ==
  \A f, g \in Fibers : \A c \in Chans :
     (f # g /\ word[f] = "P" /\ word[g] = "P") =>
        ~(<<f, "send">> \in offers[c] /\ <<g, "recv">> \in offers[c])

AllDone == \A q \in ProcSet : pc[q] = "Done"

\* Termination (made by the PlusCal translator, with weak fairness for
\* each process): each process ends. Thus no deadlock occurs, and no loop
\* runs forever, for example a claimer that retries forever.

\* At the end: each D sync was resumed one time, each P sync zero times,
\* no word is C, and the cancel took effect.
EndState ==
  AllDone =>
    /\ \A f \in Fibers : word[f] \in {"P", "D"}
    /\ \A f \in Fibers : (word[f] = "D") => (resumes[f] = 1)
    /\ \A f \in Fibers : (word[f] = "P") => (resumes[f] = 0)
    /\ (CancelTarget \in Fibers) => (word[CancelTarget] = "D")

=============================================================================
