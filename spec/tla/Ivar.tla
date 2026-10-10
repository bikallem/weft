-------------------------------- MODULE Ivar --------------------------------
(***************************************************************************)
(* A model of the atomic register of an Ivar (docs/design.md, section     *)
(* 2.4) and of Ivar.fill_from_outside (section 2.5).                       *)
(*                                                                         *)
(* The Ivar has one atomic state: Empty with a set of waiters, or Full.    *)
(* A reader registers with a CAS on Empty. A fill changes the state to     *)
(* Full with a CAS, then it commits each waiter with the one-word commit   *)
(* of section 2.4 (W or P to D, and a wait while the word is C).           *)
(*                                                                         *)
(* The fibers:                                                             *)
(*   R1  syncs on choose [Ivar.read iv; Chan.recv c].                      *)
(*   R2  syncs on Ivar.read iv.                                            *)
(*   A   a domain thread. It sends on c to R1, so it claims the word of   *)
(*       R1 (C). In the claim it allocates, and the GC can run a          *)
(*       finaliser on the same thread. The finaliser fills iv from        *)
(*       outside a fiber. Later the loop of A takes the requests of the   *)
(*       injection queue and fills iv.                                     *)
(*   B   a second domain thread that can also take the requests.          *)
(*                                                                         *)
(* The finaliser runs on the thread of A, so its steps are steps of A.    *)
(* A cannot go on until the finaliser returns.                             *)
(***************************************************************************)
EXTENDS Naturals, FiniteSets, Sequences

CONSTANTS
  BugDirectFill,        \* fill_from_outside commits the waiters itself
  BugNonAtomicRegister  \* a reader reads the state, then writes it later

Readers == {"R1", "R2"}

(* --algorithm Ivar {
variables
  word = [r \in Readers |-> "W"],
  res = [r \in Readers |-> "none"],
  resumes = [r \in Readers |-> 0],
  full = FALSE,                \* the state of iv: Full, or Empty with waiters
  waiters = {},
  chanOffer = FALSE,           \* R1 has an offer on c
  requests = <<>>,             \* the injection queue: fills from outside
  filled = 0;                  \* the number of fills of iv

\* A one-word commit of reader r with result v (section 2.4).
macro commitOne(r, v) {
  await word[r] # "C";
  if (word[r] \in {"W", "P"}) {
    if (word[r] = "P") { resumes[r] := resumes[r] + 1; };
    word[r] := "D";
    res[r] := v;
  };
}

\* A normal Ivar.fill on a domain. It is a procedure, so that A and B can
\* both call it.
procedure fill()
variables todo = {};
{
fl1:
  if (~full) {
    full := TRUE;              \* the CAS Empty -> Full takes the waiters
    todo := waiters;
    waiters := {};
    filled := filled + 1;
  };
fl2:
  while (todo # {}) {
    with (r \in todo) {
      commitOne(r, "ivar");
      todo := todo \ {r};
    };
  };
fl3:
  return;
}

\* A reader of the Ivar. R1 also has an offer on c.
fair process (rd \in Readers)
variables seen = FALSE;
{
s1:
  \* The poll of the Ivar branch.
  if (full) {
    if (word[self] = "W") { word[self] := "D"; res[self] := "ivar"; };
    goto s4;
  } else { skip; };
s2:
  \* The register of the Ivar branch.
  if (BugNonAtomicRegister) {
    seen := full;              \* a read ...
  } else {
    \* One CAS on Empty: it fails if the state is Full.
    if (~full) { waiters := waiters \cup {self}; }
    else if (word[self] = "W") { word[self] := "D"; res[self] := "ivar"; };
  };
s2b:
  if (BugNonAtomicRegister) {
    \* ... and a write later, on the state that it read.
    if (~seen) { waiters := waiters \cup {self}; }
    else if (word[self] = "W") { word[self] := "D"; res[self] := "ivar"; };
  };
s3:
  if (self = "R1") { chanOffer := TRUE; };
s4:
  \* The end of the scan: W to P, or continue if a partner committed.
  await word[self] # "C";
  if (word[self] = "W") { word[self] := "P"; goto Done; };
s5:
  resumes[self] := resumes[self] + 1;
}

\* A: a domain thread that sends to R1, with a finaliser in its claim.
fair process (dA = "A")
variables old = "W", finaliser = FALSE;
{
a1:
  await chanOffer;
  await word["R1"] # "C";
  if (word["R1"] = "D") { goto a4; }           \* R1 already committed
  else {
    old := word["R1"];
    word["R1"] := "C";                         \* A holds the word of R1
  };
a2:
  \* An allocation point in the claim: the GC can run a finaliser here.
  either { finaliser := TRUE; } or { skip; };
a2f:
  if (finaliser) {
    if (BugDirectFill) {
      \* The finaliser fills iv itself, on the thread of A.
      call fill();
    } else {
      \* The finaliser only puts a request in the injection queue.
      requests := Append(requests, "fill");
    };
  };
a3:
  \* The claim completes: the send commits R1 on c.
  word["R1"] := "D";
  res["R1"] := "chan";
  if (old = "P") { resumes["R1"] := resumes["R1"] + 1; };
a4:
  \* The loop of A: take a request at a safe point.
  if (requests # <<>>) {
    requests := Tail(requests);
    call fill();
  };
a5:
  \* A fiber of A fills iv in the normal way, if nobody filled it yet.
  if (~finaliser) { call fill(); };
}

\* B: a second domain thread that also takes requests.
fair process (dB = "B")
{
b1:
  if (requests # <<>>) {
    requests := Tail(requests);
    call fill();
  };
}
} *)

\* BEGIN TRANSLATION
VARIABLES word, res, resumes, full, waiters, chanOffer, requests, filled, pc, 
          stack, todo, seen, old, finaliser

vars == << word, res, resumes, full, waiters, chanOffer, requests, filled, pc, 
           stack, todo, seen, old, finaliser >>

ProcSet == (Readers) \cup {"A"} \cup {"B"}

Init == (* Global variables *)
        /\ word = [r \in Readers |-> "W"]
        /\ res = [r \in Readers |-> "none"]
        /\ resumes = [r \in Readers |-> 0]
        /\ full = FALSE
        /\ waiters = {}
        /\ chanOffer = FALSE
        /\ requests = <<>>
        /\ filled = 0
        (* Procedure fill *)
        /\ todo = [ self \in ProcSet |-> {}]
        (* Process rd *)
        /\ seen = [self \in Readers |-> FALSE]
        (* Process dA *)
        /\ old = "W"
        /\ finaliser = FALSE
        /\ stack = [self \in ProcSet |-> << >>]
        /\ pc = [self \in ProcSet |-> CASE self \in Readers -> "s1"
                                        [] self = "A" -> "a1"
                                        [] self = "B" -> "b1"]

fl1(self) == /\ pc[self] = "fl1"
             /\ IF ~full
                   THEN /\ full' = TRUE
                        /\ todo' = [todo EXCEPT ![self] = waiters]
                        /\ waiters' = {}
                        /\ filled' = filled + 1
                   ELSE /\ TRUE
                        /\ UNCHANGED << full, waiters, filled, todo >>
             /\ pc' = [pc EXCEPT ![self] = "fl2"]
             /\ UNCHANGED << word, res, resumes, chanOffer, requests, stack, 
                             seen, old, finaliser >>

fl2(self) == /\ pc[self] = "fl2"
             /\ IF todo[self] # {}
                   THEN /\ \E r \in todo[self]:
                             /\ word[r] # "C"
                             /\ IF word[r] \in {"W", "P"}
                                   THEN /\ IF word[r] = "P"
                                              THEN /\ resumes' = [resumes EXCEPT ![r] = resumes[r] + 1]
                                              ELSE /\ TRUE
                                                   /\ UNCHANGED resumes
                                        /\ word' = [word EXCEPT ![r] = "D"]
                                        /\ res' = [res EXCEPT ![r] = "ivar"]
                                   ELSE /\ TRUE
                                        /\ UNCHANGED << word, res, resumes >>
                             /\ todo' = [todo EXCEPT ![self] = todo[self] \ {r}]
                        /\ pc' = [pc EXCEPT ![self] = "fl2"]
                   ELSE /\ pc' = [pc EXCEPT ![self] = "fl3"]
                        /\ UNCHANGED << word, res, resumes, todo >>
             /\ UNCHANGED << full, waiters, chanOffer, requests, filled, stack, 
                             seen, old, finaliser >>

fl3(self) == /\ pc[self] = "fl3"
             /\ pc' = [pc EXCEPT ![self] = Head(stack[self]).pc]
             /\ todo' = [todo EXCEPT ![self] = Head(stack[self]).todo]
             /\ stack' = [stack EXCEPT ![self] = Tail(stack[self])]
             /\ UNCHANGED << word, res, resumes, full, waiters, chanOffer, 
                             requests, filled, seen, old, finaliser >>

fill(self) == fl1(self) \/ fl2(self) \/ fl3(self)

s1(self) == /\ pc[self] = "s1"
            /\ IF full
                  THEN /\ IF word[self] = "W"
                             THEN /\ word' = [word EXCEPT ![self] = "D"]
                                  /\ res' = [res EXCEPT ![self] = "ivar"]
                             ELSE /\ TRUE
                                  /\ UNCHANGED << word, res >>
                       /\ pc' = [pc EXCEPT ![self] = "s4"]
                  ELSE /\ TRUE
                       /\ pc' = [pc EXCEPT ![self] = "s2"]
                       /\ UNCHANGED << word, res >>
            /\ UNCHANGED << resumes, full, waiters, chanOffer, requests, 
                            filled, stack, todo, seen, old, finaliser >>

s2(self) == /\ pc[self] = "s2"
            /\ IF BugNonAtomicRegister
                  THEN /\ seen' = [seen EXCEPT ![self] = full]
                       /\ UNCHANGED << word, res, waiters >>
                  ELSE /\ IF ~full
                             THEN /\ waiters' = (waiters \cup {self})
                                  /\ UNCHANGED << word, res >>
                             ELSE /\ IF word[self] = "W"
                                        THEN /\ word' = [word EXCEPT ![self] = "D"]
                                             /\ res' = [res EXCEPT ![self] = "ivar"]
                                        ELSE /\ TRUE
                                             /\ UNCHANGED << word, res >>
                                  /\ UNCHANGED waiters
                       /\ seen' = seen
            /\ pc' = [pc EXCEPT ![self] = "s2b"]
            /\ UNCHANGED << resumes, full, chanOffer, requests, filled, stack, 
                            todo, old, finaliser >>

s2b(self) == /\ pc[self] = "s2b"
             /\ IF BugNonAtomicRegister
                   THEN /\ IF ~seen[self]
                              THEN /\ waiters' = (waiters \cup {self})
                                   /\ UNCHANGED << word, res >>
                              ELSE /\ IF word[self] = "W"
                                         THEN /\ word' = [word EXCEPT ![self] = "D"]
                                              /\ res' = [res EXCEPT ![self] = "ivar"]
                                         ELSE /\ TRUE
                                              /\ UNCHANGED << word, res >>
                                   /\ UNCHANGED waiters
                   ELSE /\ TRUE
                        /\ UNCHANGED << word, res, waiters >>
             /\ pc' = [pc EXCEPT ![self] = "s3"]
             /\ UNCHANGED << resumes, full, chanOffer, requests, filled, stack, 
                             todo, seen, old, finaliser >>

s3(self) == /\ pc[self] = "s3"
            /\ IF self = "R1"
                  THEN /\ chanOffer' = TRUE
                  ELSE /\ TRUE
                       /\ UNCHANGED chanOffer
            /\ pc' = [pc EXCEPT ![self] = "s4"]
            /\ UNCHANGED << word, res, resumes, full, waiters, requests, 
                            filled, stack, todo, seen, old, finaliser >>

s4(self) == /\ pc[self] = "s4"
            /\ word[self] # "C"
            /\ IF word[self] = "W"
                  THEN /\ word' = [word EXCEPT ![self] = "P"]
                       /\ pc' = [pc EXCEPT ![self] = "Done"]
                  ELSE /\ pc' = [pc EXCEPT ![self] = "s5"]
                       /\ word' = word
            /\ UNCHANGED << res, resumes, full, waiters, chanOffer, requests, 
                            filled, stack, todo, seen, old, finaliser >>

s5(self) == /\ pc[self] = "s5"
            /\ resumes' = [resumes EXCEPT ![self] = resumes[self] + 1]
            /\ pc' = [pc EXCEPT ![self] = "Done"]
            /\ UNCHANGED << word, res, full, waiters, chanOffer, requests, 
                            filled, stack, todo, seen, old, finaliser >>

rd(self) == s1(self) \/ s2(self) \/ s2b(self) \/ s3(self) \/ s4(self)
               \/ s5(self)

a1 == /\ pc["A"] = "a1"
      /\ chanOffer
      /\ word["R1"] # "C"
      /\ IF word["R1"] = "D"
            THEN /\ pc' = [pc EXCEPT !["A"] = "a4"]
                 /\ UNCHANGED << word, old >>
            ELSE /\ old' = word["R1"]
                 /\ word' = [word EXCEPT !["R1"] = "C"]
                 /\ pc' = [pc EXCEPT !["A"] = "a2"]
      /\ UNCHANGED << res, resumes, full, waiters, chanOffer, requests, filled, 
                      stack, todo, seen, finaliser >>

a2 == /\ pc["A"] = "a2"
      /\ \/ /\ finaliser' = TRUE
         \/ /\ TRUE
            /\ UNCHANGED finaliser
      /\ pc' = [pc EXCEPT !["A"] = "a2f"]
      /\ UNCHANGED << word, res, resumes, full, waiters, chanOffer, requests, 
                      filled, stack, todo, seen, old >>

a2f == /\ pc["A"] = "a2f"
       /\ IF finaliser
             THEN /\ IF BugDirectFill
                        THEN /\ stack' = [stack EXCEPT !["A"] = << [ procedure |->  "fill",
                                                                     pc        |->  "a3",
                                                                     todo      |->  todo["A"] ] >>
                                                                 \o stack["A"]]
                             /\ todo' = [todo EXCEPT !["A"] = {}]
                             /\ pc' = [pc EXCEPT !["A"] = "fl1"]
                             /\ UNCHANGED requests
                        ELSE /\ requests' = Append(requests, "fill")
                             /\ pc' = [pc EXCEPT !["A"] = "a3"]
                             /\ UNCHANGED << stack, todo >>
             ELSE /\ pc' = [pc EXCEPT !["A"] = "a3"]
                  /\ UNCHANGED << requests, stack, todo >>
       /\ UNCHANGED << word, res, resumes, full, waiters, chanOffer, filled, 
                       seen, old, finaliser >>

a3 == /\ pc["A"] = "a3"
      /\ word' = [word EXCEPT !["R1"] = "D"]
      /\ res' = [res EXCEPT !["R1"] = "chan"]
      /\ IF old = "P"
            THEN /\ resumes' = [resumes EXCEPT !["R1"] = resumes["R1"] + 1]
            ELSE /\ TRUE
                 /\ UNCHANGED resumes
      /\ pc' = [pc EXCEPT !["A"] = "a4"]
      /\ UNCHANGED << full, waiters, chanOffer, requests, filled, stack, todo, 
                      seen, old, finaliser >>

a4 == /\ pc["A"] = "a4"
      /\ IF requests # <<>>
            THEN /\ requests' = Tail(requests)
                 /\ stack' = [stack EXCEPT !["A"] = << [ procedure |->  "fill",
                                                         pc        |->  "a5",
                                                         todo      |->  todo["A"] ] >>
                                                     \o stack["A"]]
                 /\ todo' = [todo EXCEPT !["A"] = {}]
                 /\ pc' = [pc EXCEPT !["A"] = "fl1"]
            ELSE /\ pc' = [pc EXCEPT !["A"] = "a5"]
                 /\ UNCHANGED << requests, stack, todo >>
      /\ UNCHANGED << word, res, resumes, full, waiters, chanOffer, filled, 
                      seen, old, finaliser >>

a5 == /\ pc["A"] = "a5"
      /\ IF ~finaliser
            THEN /\ stack' = [stack EXCEPT !["A"] = << [ procedure |->  "fill",
                                                         pc        |->  "Done",
                                                         todo      |->  todo["A"] ] >>
                                                     \o stack["A"]]
                 /\ todo' = [todo EXCEPT !["A"] = {}]
                 /\ pc' = [pc EXCEPT !["A"] = "fl1"]
            ELSE /\ pc' = [pc EXCEPT !["A"] = "Done"]
                 /\ UNCHANGED << stack, todo >>
      /\ UNCHANGED << word, res, resumes, full, waiters, chanOffer, requests, 
                      filled, seen, old, finaliser >>

dA == a1 \/ a2 \/ a2f \/ a3 \/ a4 \/ a5

b1 == /\ pc["B"] = "b1"
      /\ IF requests # <<>>
            THEN /\ requests' = Tail(requests)
                 /\ stack' = [stack EXCEPT !["B"] = << [ procedure |->  "fill",
                                                         pc        |->  "Done",
                                                         todo      |->  todo["B"] ] >>
                                                     \o stack["B"]]
                 /\ todo' = [todo EXCEPT !["B"] = {}]
                 /\ pc' = [pc EXCEPT !["B"] = "fl1"]
            ELSE /\ pc' = [pc EXCEPT !["B"] = "Done"]
                 /\ UNCHANGED << requests, stack, todo >>
      /\ UNCHANGED << word, res, resumes, full, waiters, chanOffer, filled, 
                      seen, old, finaliser >>

dB == b1

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == dA \/ dB
           \/ (\E self \in ProcSet: fill(self))
           \/ (\E self \in Readers: rd(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in Readers : WF_vars(rd(self))
        /\ WF_vars(dA) /\ WF_vars(fill("A"))
        /\ WF_vars(dB) /\ WF_vars(fill("B"))

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION

-----------------------------------------------------------------------------
AtMostOneResume == \A r \in Readers : resumes[r] <= 1

\* A Full Ivar has no waiters, and it is filled one time.
FullNoWaiters == full => waiters = {}
OneFill == filled <= 1

AllDone == \A q \in ProcSet : pc[q] = "Done"

\* At the end: each reader is Done and was resumed one time, and no
\* request stays in the queue.
EndState ==
  AllDone =>
    /\ \A r \in Readers : word[r] = "D" /\ resumes[r] = 1
    /\ requests = <<>>

\* Termination (from the translator): with weak fairness, each process
\* ends. A finaliser that waits forever on a word of its own thread is a
\* deadlock.
=============================================================================
