------------------------------ MODULE Semaphore ------------------------------
(***************************************************************************)
(* A model of Fiber.Semaphore (docs/design.md, section 2.5, Locks for     *)
(* fiber code). The acquire is a base event of a choose (section 2.4).    *)
(*                                                                         *)
(* The semaphore has a short lock, a count of free permits and a FIFO     *)
(* queue of offers. Each fiber does one sync. A sync has one state word:  *)
(* W (Waiting), P (Parked) or D (Done). Commit.tla examines the C         *)
(* (Claimed) state: the steps of this model wait while a word is C, so    *)
(* this model has no C.                                                    *)
(*                                                                         *)
(* The parts of the protocol:                                              *)
(*   - The acquire branch of a scan, under the lock: if a permit is free, *)
(*     the sync commits itself (W to D) and takes the permit. If the      *)
(*     commit finds D, it takes nothing. If no permit is free, it adds an *)
(*     offer at the tail of the queue.                                     *)
(*   - A release, under the lock: it takes the offer at the head of the   *)
(*     queue and commits its word with a one-word commit (W or P to D).   *)
(*     If the word is D, the sync of the offer lost its choose or got a   *)
(*     cancel: the release takes the next offer. If the queue is empty,   *)
(*     the permit becomes free.                                            *)
(*   - A timer and a cancel: one-word partners of a sync.                 *)
(*                                                                         *)
(* Each sync has at most one acquire branch. A second acquire branch of   *)
(* the same sync on the same semaphore is only one more stale offer for   *)
(* a release. A dscheck test examines it (test/dscheck).                   *)
(*                                                                         *)
(* A fiber that gets the permit holds it, then releases it. A parked      *)
(* fiber waits for its resume, so a lost wakeup is a deadlock of TLC.     *)
(*                                                                         *)
(* The Bug* constants plant the bugs, to show that the model finds them.  *)
(***************************************************************************)
EXTENDS Naturals, Sequences, FiniteSets

CONSTANTS
  Scenario,      \* "choose", "cancel" or "two"
  BugNoLock,     \* the acquire reads the count and adds its offer later
  BugTakeFirst,  \* the acquire takes the permit before its commit
  BugTakeOnLost, \* a release gives the permit to the head with no commit
  BugAnyWaiter   \* a release gives the permit to any offer, not the head

\* The permits, the fibers, and the fibers that hold a permit at the start.
N       == IF Scenario = "two" THEN 2 ELSE 1
Fibers  == IF Scenario = "two" THEN 1..4 ELSE 1..3
Holders == IF Scenario = "two" THEN {1, 2} ELSE {1}

\* The branches of the sync of each fiber. A holder does no sync.
Br(f) ==
  IF f \in Holders THEN {}
  ELSE CASE Scenario = "choose" -> IF f = 2 THEN {"acq", "timer"} ELSE {"acq"}
       []   Scenario = "cancel" -> {"acq"}
       []   Scenario = "two"    -> IF f = 3 THEN {"acq", "timer"} ELSE {"acq"}

\* The fiber that the canceller cancels (0: no cancel).
CancelTarget == CASE Scenario = "choose" -> 0
                []   Scenario = "cancel" -> 2
                []   Scenario = "two"    -> 4

TimerFibers == {f \in Fibers : "timer" \in Br(f)}

\* The sequence s without its element at index i.
Remove(s, i) == SubSeq(s, 1, i - 1) \o SubSeq(s, i + 1, Len(s))

(* --algorithm Semaphore {
variables
  word = [f \in Fibers |-> IF f \in Holders THEN "D" ELSE "W"],
  res = [f \in Fibers |-> IF f \in Holders THEN "acq" ELSE "none"],
  resumes = [f \in Fibers |-> IF f \in Holders THEN 1 ELSE 0],
  free = N - Cardinality(Holders),
  held = Holders,         \* the fibers that hold a permit
  queue = <<>>,           \* the offers of the acquires, the oldest first
  lock = 0,
  timerReg = {},
  unfair = FALSE;         \* a release passed over an older live offer

fair process (fib \in Fibers)
variables todo = Br(self), b = "none", n = 0, w = 0, i = 0;
{
start:
  if (self \in Holders) { goto rlock; };
scan:
  while (todo # {} /\ word[self] # "D") {
    with (x \in todo) { b := x; todo := todo \ {x} };
    if (b = "timer") {
treg:
      timerReg := timerReg \cup {self};
    } else {
alock:
      if (~BugNoLock) { await lock = 0; lock := self; };
aread:
      n := free;
aact:
      \* The lock holds free and queue, so this step can read and write
      \* them after the read of aread.
      if (n > 0) {
        if (BugTakeFirst) {
          free := n - 1;
        } else if (word[self] = "W") {
          word[self] := "D";
          res[self] := "acq";
          held := held \cup {self};
          free := n - 1;
        };
      } else {
        queue := Append(queue, self);
      };
acas:
      \* Only with the bug: the commit comes after the take, and the permit
      \* is gone also if the commit finds D.
      if (BugTakeFirst /\ n > 0 /\ word[self] = "W") {
        word[self] := "D";
        res[self] := "acq";
        held := held \cup {self};
      };
aunlock:
      if (~BugNoLock) { lock := 0; };
    };
  };
endscan:
  if (word[self] = "W") {
    word[self] := "P";
park:
    \* The fiber waits for the side that commits its word.
    await resumes[self] = 1;
  } else {
cont:
    resumes[self] := resumes[self] + 1;
  };
after:
  if (res[self] # "acq") { goto Done; };
  \* The fiber holds a permit. It releases it.
rlock:
  await lock = 0;
  lock := self;
rgive:
  if (queue = <<>>) {
    free := free + 1;
    held := held \ {self};
  } else {
    if (BugAnyWaiter) {
      with (k \in 1..Len(queue)) { i := k };
    } else {
      i := 1;
    };
    w := queue[i];
    if (\E j \in 1..(i - 1) : word[queue[j]] # "D"
          /\ word[w] # "D") {
      unfair := TRUE;
    };
    queue := Remove(queue, i);
    if (word[w] \in {"W", "P"}) {
      if (word[w] = "P") { resumes[w] := resumes[w] + 1; };
      word[w] := "D";
      res[w] := "acq";
      held := (held \ {self}) \cup {w};
    } else if (BugTakeOnLost) {
      held := held \ {self};
    } else {
      goto rgive;
    };
  };
runlock:
  lock := 0;
}

\* A cancel: a one-word partner of the cancelled fiber.
fair process (canc = 100)
{
c1:
  if (CancelTarget \in Fibers /\ word[CancelTarget] \in {"W", "P"}) {
    if (word[CancelTarget] = "P") {
      resumes[CancelTarget] := resumes[CancelTarget] + 1;
    };
    word[CancelTarget] := "D";
    res[CancelTarget] := "cancel";
  };
}

\* A timer: a one-word partner that fires only after its fiber registered it.
fair process (tim \in {200 + f : f \in TimerFibers})
{
t1:
  await (self - 200) \in timerReg \/ pc[self - 200] = "Done";
  if ((self - 200) \in timerReg /\ word[self - 200] \in {"W", "P"}) {
    if (word[self - 200] = "P") {
      resumes[self - 200] := resumes[self - 200] + 1;
    };
    word[self - 200] := "D";
    res[self - 200] := "timer";
  };
}
} *)

\* BEGIN TRANSLATION
VARIABLES word, res, resumes, free, held, queue, lock, timerReg, unfair, pc, 
          todo, b, n, w, i

vars == << word, res, resumes, free, held, queue, lock, timerReg, unfair, pc, 
           todo, b, n, w, i >>

ProcSet == (Fibers) \cup {100} \cup ({200 + f : f \in TimerFibers})

Init == (* Global variables *)
        /\ word = [f \in Fibers |-> IF f \in Holders THEN "D" ELSE "W"]
        /\ res = [f \in Fibers |-> IF f \in Holders THEN "acq" ELSE "none"]
        /\ resumes = [f \in Fibers |-> IF f \in Holders THEN 1 ELSE 0]
        /\ free = N - Cardinality(Holders)
        /\ held = Holders
        /\ queue = <<>>
        /\ lock = 0
        /\ timerReg = {}
        /\ unfair = FALSE
        (* Process fib *)
        /\ todo = [self \in Fibers |-> Br(self)]
        /\ b = [self \in Fibers |-> "none"]
        /\ n = [self \in Fibers |-> 0]
        /\ w = [self \in Fibers |-> 0]
        /\ i = [self \in Fibers |-> 0]
        /\ pc = [self \in ProcSet |-> CASE self \in Fibers -> "start"
                                        [] self = 100 -> "c1"
                                        [] self \in {200 + f : f \in TimerFibers} -> "t1"]

start(self) == /\ pc[self] = "start"
               /\ IF self \in Holders
                     THEN /\ pc' = [pc EXCEPT ![self] = "rlock"]
                     ELSE /\ pc' = [pc EXCEPT ![self] = "scan"]
               /\ UNCHANGED << word, res, resumes, free, held, queue, lock, 
                               timerReg, unfair, todo, b, n, w, i >>

scan(self) == /\ pc[self] = "scan"
              /\ IF todo[self] # {} /\ word[self] # "D"
                    THEN /\ \E x \in todo[self]:
                              /\ b' = [b EXCEPT ![self] = x]
                              /\ todo' = [todo EXCEPT ![self] = todo[self] \ {x}]
                         /\ IF b'[self] = "timer"
                               THEN /\ pc' = [pc EXCEPT ![self] = "treg"]
                               ELSE /\ pc' = [pc EXCEPT ![self] = "alock"]
                    ELSE /\ pc' = [pc EXCEPT ![self] = "endscan"]
                         /\ UNCHANGED << todo, b >>
              /\ UNCHANGED << word, res, resumes, free, held, queue, lock, 
                              timerReg, unfair, n, w, i >>

treg(self) == /\ pc[self] = "treg"
              /\ timerReg' = (timerReg \cup {self})
              /\ pc' = [pc EXCEPT ![self] = "scan"]
              /\ UNCHANGED << word, res, resumes, free, held, queue, lock, 
                              unfair, todo, b, n, w, i >>

alock(self) == /\ pc[self] = "alock"
               /\ IF ~BugNoLock
                     THEN /\ lock = 0
                          /\ lock' = self
                     ELSE /\ TRUE
                          /\ lock' = lock
               /\ pc' = [pc EXCEPT ![self] = "aread"]
               /\ UNCHANGED << word, res, resumes, free, held, queue, timerReg, 
                               unfair, todo, b, n, w, i >>

aread(self) == /\ pc[self] = "aread"
               /\ n' = [n EXCEPT ![self] = free]
               /\ pc' = [pc EXCEPT ![self] = "aact"]
               /\ UNCHANGED << word, res, resumes, free, held, queue, lock, 
                               timerReg, unfair, todo, b, w, i >>

aact(self) == /\ pc[self] = "aact"
              /\ IF n[self] > 0
                    THEN /\ IF BugTakeFirst
                               THEN /\ free' = n[self] - 1
                                    /\ UNCHANGED << word, res, held >>
                               ELSE /\ IF word[self] = "W"
                                          THEN /\ word' = [word EXCEPT ![self] = "D"]
                                               /\ res' = [res EXCEPT ![self] = "acq"]
                                               /\ held' = (held \cup {self})
                                               /\ free' = n[self] - 1
                                          ELSE /\ TRUE
                                               /\ UNCHANGED << word, res, free, 
                                                               held >>
                         /\ queue' = queue
                    ELSE /\ queue' = Append(queue, self)
                         /\ UNCHANGED << word, res, free, held >>
              /\ pc' = [pc EXCEPT ![self] = "acas"]
              /\ UNCHANGED << resumes, lock, timerReg, unfair, todo, b, n, w, 
                              i >>

acas(self) == /\ pc[self] = "acas"
              /\ IF BugTakeFirst /\ n[self] > 0 /\ word[self] = "W"
                    THEN /\ word' = [word EXCEPT ![self] = "D"]
                         /\ res' = [res EXCEPT ![self] = "acq"]
                         /\ held' = (held \cup {self})
                    ELSE /\ TRUE
                         /\ UNCHANGED << word, res, held >>
              /\ pc' = [pc EXCEPT ![self] = "aunlock"]
              /\ UNCHANGED << resumes, free, queue, lock, timerReg, unfair, 
                              todo, b, n, w, i >>

aunlock(self) == /\ pc[self] = "aunlock"
                 /\ IF ~BugNoLock
                       THEN /\ lock' = 0
                       ELSE /\ TRUE
                            /\ lock' = lock
                 /\ pc' = [pc EXCEPT ![self] = "scan"]
                 /\ UNCHANGED << word, res, resumes, free, held, queue, 
                                 timerReg, unfair, todo, b, n, w, i >>

endscan(self) == /\ pc[self] = "endscan"
                 /\ IF word[self] = "W"
                       THEN /\ word' = [word EXCEPT ![self] = "P"]
                            /\ pc' = [pc EXCEPT ![self] = "park"]
                       ELSE /\ pc' = [pc EXCEPT ![self] = "cont"]
                            /\ word' = word
                 /\ UNCHANGED << res, resumes, free, held, queue, lock, 
                                 timerReg, unfair, todo, b, n, w, i >>

park(self) == /\ pc[self] = "park"
              /\ resumes[self] = 1
              /\ pc' = [pc EXCEPT ![self] = "after"]
              /\ UNCHANGED << word, res, resumes, free, held, queue, lock, 
                              timerReg, unfair, todo, b, n, w, i >>

cont(self) == /\ pc[self] = "cont"
              /\ resumes' = [resumes EXCEPT ![self] = resumes[self] + 1]
              /\ pc' = [pc EXCEPT ![self] = "after"]
              /\ UNCHANGED << word, res, free, held, queue, lock, timerReg, 
                              unfair, todo, b, n, w, i >>

after(self) == /\ pc[self] = "after"
               /\ IF res[self] # "acq"
                     THEN /\ pc' = [pc EXCEPT ![self] = "Done"]
                     ELSE /\ pc' = [pc EXCEPT ![self] = "rlock"]
               /\ UNCHANGED << word, res, resumes, free, held, queue, lock, 
                               timerReg, unfair, todo, b, n, w, i >>

rlock(self) == /\ pc[self] = "rlock"
               /\ lock = 0
               /\ lock' = self
               /\ pc' = [pc EXCEPT ![self] = "rgive"]
               /\ UNCHANGED << word, res, resumes, free, held, queue, timerReg, 
                               unfair, todo, b, n, w, i >>

rgive(self) == /\ pc[self] = "rgive"
               /\ IF queue = <<>>
                     THEN /\ free' = free + 1
                          /\ held' = held \ {self}
                          /\ pc' = [pc EXCEPT ![self] = "runlock"]
                          /\ UNCHANGED << word, res, resumes, queue, unfair, w, 
                                          i >>
                     ELSE /\ IF BugAnyWaiter
                                THEN /\ \E k \in 1..Len(queue):
                                          i' = [i EXCEPT ![self] = k]
                                ELSE /\ i' = [i EXCEPT ![self] = 1]
                          /\ w' = [w EXCEPT ![self] = queue[i'[self]]]
                          /\ IF \E j \in 1..(i'[self] - 1) : word[queue[j]] # "D"
                                  /\ word[w'[self]] # "D"
                                THEN /\ unfair' = TRUE
                                ELSE /\ TRUE
                                     /\ UNCHANGED unfair
                          /\ queue' = Remove(queue, i'[self])
                          /\ IF word[w'[self]] \in {"W", "P"}
                                THEN /\ IF word[w'[self]] = "P"
                                           THEN /\ resumes' = [resumes EXCEPT ![w'[self]] = resumes[w'[self]] + 1]
                                           ELSE /\ TRUE
                                                /\ UNCHANGED resumes
                                     /\ word' = [word EXCEPT ![w'[self]] = "D"]
                                     /\ res' = [res EXCEPT ![w'[self]] = "acq"]
                                     /\ held' = ((held \ {self}) \cup {w'[self]})
                                     /\ pc' = [pc EXCEPT ![self] = "runlock"]
                                ELSE /\ IF BugTakeOnLost
                                           THEN /\ held' = held \ {self}
                                                /\ pc' = [pc EXCEPT ![self] = "runlock"]
                                           ELSE /\ pc' = [pc EXCEPT ![self] = "rgive"]
                                                /\ held' = held
                                     /\ UNCHANGED << word, res, resumes >>
                          /\ free' = free
               /\ UNCHANGED << lock, timerReg, todo, b, n >>

runlock(self) == /\ pc[self] = "runlock"
                 /\ lock' = 0
                 /\ pc' = [pc EXCEPT ![self] = "Done"]
                 /\ UNCHANGED << word, res, resumes, free, held, queue, 
                                 timerReg, unfair, todo, b, n, w, i >>

fib(self) == start(self) \/ scan(self) \/ treg(self) \/ alock(self)
                \/ aread(self) \/ aact(self) \/ acas(self) \/ aunlock(self)
                \/ endscan(self) \/ park(self) \/ cont(self) \/ after(self)
                \/ rlock(self) \/ rgive(self) \/ runlock(self)

c1 == /\ pc[100] = "c1"
      /\ IF CancelTarget \in Fibers /\ word[CancelTarget] \in {"W", "P"}
            THEN /\ IF word[CancelTarget] = "P"
                       THEN /\ resumes' = [resumes EXCEPT ![CancelTarget] = resumes[CancelTarget] + 1]
                       ELSE /\ TRUE
                            /\ UNCHANGED resumes
                 /\ word' = [word EXCEPT ![CancelTarget] = "D"]
                 /\ res' = [res EXCEPT ![CancelTarget] = "cancel"]
            ELSE /\ TRUE
                 /\ UNCHANGED << word, res, resumes >>
      /\ pc' = [pc EXCEPT ![100] = "Done"]
      /\ UNCHANGED << free, held, queue, lock, timerReg, unfair, todo, b, n, w, 
                      i >>

canc == c1

t1(self) == /\ pc[self] = "t1"
            /\ (self - 200) \in timerReg \/ pc[self - 200] = "Done"
            /\ IF (self - 200) \in timerReg /\ word[self - 200] \in {"W", "P"}
                  THEN /\ IF word[self - 200] = "P"
                             THEN /\ resumes' = [resumes EXCEPT ![self - 200] = resumes[self - 200] + 1]
                             ELSE /\ TRUE
                                  /\ UNCHANGED resumes
                       /\ word' = [word EXCEPT ![self - 200] = "D"]
                       /\ res' = [res EXCEPT ![self - 200] = "timer"]
                  ELSE /\ TRUE
                       /\ UNCHANGED << word, res, resumes >>
            /\ pc' = [pc EXCEPT ![self] = "Done"]
            /\ UNCHANGED << free, held, queue, lock, timerReg, unfair, todo, b, 
                            n, w, i >>

tim(self) == t1(self)

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
  /\ word \in [Fibers -> {"W", "P", "D"}]
  /\ resumes \in [Fibers -> 0..3]
  /\ free \in 0..N
  /\ held \subseteq Fibers

\* No permit is lost or doubled: each permit is free or with one holder.
Conserved == free + Cardinality(held) = N

\* A fiber holds a permit only if its sync committed to the acquire.
HeldAcquired == \A f \in held : res[f] = "acq"

\* Each continuation has at most one resume.
AtMostOneResume == \A f \in Fibers : resumes[f] <= 1

\* A permit is free only if the queue is empty: a release removes each
\* stale offer and gives a permit to a waiter before it makes the permit
\* free. Thus a new acquire never takes a permit before an older waiter
\* (FIFO), and a parked waiter never waits while a permit is free.
NoIdlePermit == free > 0 => queue = <<>>

\* A release gives the permit to the oldest live offer (FIFO).
Fifo == ~unfair

AllDone == \A q \in ProcSet : pc[q] = "Done"

\* Termination (made by the PlusCal translator, with weak fairness for
\* each process): each process ends. Thus no fiber waits forever for a
\* permit.

\* At the end: all permits are free, each sync ended with one resume, and
\* the cancel took effect.
EndState ==
  AllDone =>
    /\ free = N
    /\ held = {}
    /\ \A f \in Fibers : word[f] = "D" /\ resumes[f] = 1
    /\ (CancelTarget \in Fibers) => (res[CancelTarget] \in {"cancel", "acq"})

=============================================================================
