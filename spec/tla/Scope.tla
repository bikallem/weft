------------------------------- MODULE Scope --------------------------------
(***************************************************************************)
(* A model of the scopes and the cancellation of docs/design.md,           *)
(* section 2.5.                                                            *)
(*                                                                         *)
(* The tree of cancel contexts:                                            *)
(*                                                                         *)
(*   R  root scope            members: F0 (body), K                        *)
(*   S  scope under R         members: F0 (body), G1, G3 (if K forks it)   *)
(*   D2 daemon context in S   member:  G2 (a daemon of S)                  *)
(*   H  shield node in S      made by G1                                   *)
(*   T  scope under H         members: G1 (body), TT (the timer of         *)
(*                            with_timeout)                                *)
(*                                                                         *)
(* Three scenarios keep the state space small. Each one runs F0 and the   *)
(* canceller, and some of the other fibers:                                *)
(*   "error"   G1 fails, and G2 is a daemon of S.                          *)
(*   "shield"  G1 runs its cleanup in a shield with a with_timeout (TT).   *)
(*   "fork"    K forks G3 into S while S can end.                          *)
(*   "condwait" as "shield", but the sync of G1 is a Condition.wait in    *)
(*             use_rw ~protect:true: the wait opens the shield node H, so *)
(*             the cancels of S and R also reach it (M2).                 *)
(*                                                                         *)
(* Each fiber waits in syncs that only a cancel can end, so each fiber     *)
(* ends only if the cancellation reaches it. The canceller can cancel S,   *)
(* and it always cancels R in the end. G1 either raises an exception (the  *)
(* scope fails and cancels S), or it runs a cleanup in a shield with a     *)
(* with_timeout of its own.                                                *)
(*                                                                         *)
(* The protocol of the design:                                             *)
(*   - A sync publishes its record (context, word), then reads the         *)
(*     cancelled flags of its contexts up to the nearest shield. If one is *)
(*     set, it commits itself with the cancel. Else it parks.              *)
(*   - A cancel of X sets the flag of X, then walks X and its nested       *)
(*     scopes. It does not go into a shield node. It commits the sync of a *)
(*     member only if the context chain of that sync reaches X with no     *)
(*     shield between them (W or P to D).                                  *)
(*   - A scope made inside a shield links under the shield node.           *)
(*   - A new nested scope reads the flag of its parent again.              *)
(*   - The join of a scope waits for all members, and it ignores cancels.  *)
(*     When the body and the other members end, the scope cancels its     *)
(*     daemons.                                                            *)
(*   - A member that fails stores its exception before it ends.            *)
(*   - A fork and the end of the last member are one CAS on the scope.     *)
(*                                                                         *)
(* The commit protocol itself (Claimed, two-word claims) is in Commit.tla. *)
(* Here a commit is one atomic step on the word of the sync.               *)
(***************************************************************************)
EXTENDS Naturals, FiniteSets, Sequences

CONSTANTS
  Scenario,           \* "error", "shield", "fork" or "condwait": the fibers that run
  BugCondOuterCtx,    \* the condition wait takes the outer context, with H shut
  BugJoinCancellable, \* the join of a scope is a normal sync
  BugShieldNotNode,   \* a scope inside a shield links under the outer scope
  SimpNoRecheck,      \* a simplification: a new nested scope does not read its parent again
  BugNoChainCheck,    \* the walk commits any sync of a member
  BugExnAfterEnd,     \* a failed member ends before it stores its exception
  BugNoFlagCheck,     \* a sync parks without a read of the flags
  SimpNoChildFlags    \* a simplification: the walk sets only the flag of x

Fibers == {"F0", "G1", "G2", "G3", "K", "TT"}
Nodes  == {"R", "S", "D2", "H", "T"}
IsShield(n) == n = "H"
Parent(n) ==
  CASE n = "R"  -> "none"
  []   n = "S"  -> "R"
  []   n = "D2" -> "S"
  []   n = "H"  -> "S"
  []   n = "T"  -> IF BugShieldNotNode THEN "S" ELSE "H"

\* X is on the chain of contexts from c, with no shut shield between them.
\* [open] tells that the shield node H is open.
RECURSIVE OnChain(_, _, _)
OnChain(c, x, open) ==
  IF c = x THEN TRUE
  ELSE IF c = "none" \/ (IsShield(c) /\ ~open) THEN FALSE
  ELSE OnChain(Parent(c), x, open)

\* A sync record: its context, its word, the reason of its commit, and if H
\* was open when it was published.
NoRec == [ctx |-> "none", w |-> "none", rs |-> "none", op |-> FALSE]

(* --algorithm Scope {
variables
  cancelled = [n \in Nodes |-> FALSE],
  linked = [n \in Nodes |-> n = "R"],
  members = [n \in Nodes |->
    IF n = "R" THEN (IF Scenario = "fork" THEN {"F0", "K"} ELSE {"F0"}) ELSE {}],
  closed = FALSE,            \* the closed bit of S
  exnS = "none",             \* the stored exception of S
  rec = [f \in Fibers |-> NoRec],
  g1Path = "none",           \* "error" or "shield"
  g3 = IF Scenario = "fork" THEN "none" ELSE "closed",  \* the fork of K into S
  sRes = "none",             \* the result of Fiber.scope S
  tRes = "none",             \* the scope whose cancel ended G1's sync in T
  toRes = "none",            \* the result of with_timeout in G1
  hOpen = FALSE;             \* the condition wait opened the shield node H

define {
  \* The first cancelled context on the chain from c, up to a shield.
  RECURSIVE FirstFlag(_)
  FirstFlag(c) ==
    IF c = "none" \/ (IsShield(c) /\ ~hOpen) THEN "none"
    ELSE IF cancelled[c] THEN c ELSE FirstFlag(Parent(c))
}

\* A sync that only a cancel can end: publish, read the flags, park, wait.
macro publish(f, c) { rec[f] := [ctx |-> c, w |-> "W", rs |-> "none", op |-> hOpen]; }
macro readFlags(f, c) {
  if (~BugNoFlagCheck /\ FirstFlag(c) # "none" /\ rec[f].w = "W") {
    rec[f] := [rec[f] EXCEPT !.w = "D", !.rs = FirstFlag(c)];
  };
}
macro park(f) { if (rec[f].w = "W") { rec[f].w := "P"; }; }

\* The cancel of x.
procedure walk(x)
variables wt = {}, wf = {}, fx = "none";
{
w1:
  cancelled[x] := TRUE;
  wt := {x};
w2:
  \* One node at a time, under the lock of that node: take its members, and
  \* mark and visit its nested scopes. The walk does not go into a shield.
  while (wt # {}) {
    with (n \in wt) {
      wf := wf \cup members[n];
      wt := (wt \ {n}) \cup {z \in Nodes : linked[z] /\ Parent(z) = n /\ ~IsShield(z)};
      if (~SimpNoChildFlags) {
        cancelled := [z \in Nodes |->
          IF linked[z] /\ Parent(z) = n /\ ~IsShield(z) THEN TRUE ELSE cancelled[z]];
      };
    };
  };
w3:
  \* After the locks: commit each sync that the cancel can reach.
  while (wf # {}) {
    with (g \in wf) { fx := g; wf := wf \ {g}; };
    if (rec[fx].w \in {"W", "P"} /\ (BugNoChainCheck \/ OnChain(rec[fx].ctx, x, hOpen))) {
      rec[fx] := [rec[fx] EXCEPT !.w = "D", !.rs = x];
    };
  };
w4:
  return;
}

\* F0: the body of R, and the body of the scope S.
fair process (f0 = "F0") {
a1:
  \* Fiber.scope makes S with its body as the first member, and links it
  \* under R. Only after this step can another fiber hold the value S.
  linked["S"] := TRUE || members["S"] := {"F0"};
a2:
  if (~SimpNoRecheck /\ cancelled["R"]) { cancelled["S"] := TRUE; };
a3:
  linked["D2"] := TRUE
    || members["S"] := members["S"] \cup (IF Scenario = "fork" THEN {} ELSE {"G1"})
    || members["D2"] := IF Scenario = "error" THEN {"G2"} ELSE {};
a4:
  publish("F0", "S");                                         \* the body waits
a5:
  readFlags("F0", "S");
a6:
  park("F0");
a7:
  await rec["F0"].w = "D";
  members["S"] := members["S"] \ {"F0"};                \* the body raised
a8:
  \* The join. It waits for the members that are not daemons.
  if (BugJoinCancellable /\ FirstFlag("S") # "none") { goto a11; };
a8w:
  await members["S"] = {};
a9:
  call walk("D2");                                      \* cancel the daemons
a10:
  await members["S"] = {} /\ members["D2"] = {};
  closed := TRUE;                                       \* the last member ended
a11:
  sRes := IF exnS # "none" THEN "exn"
          ELSE IF FirstFlag("S") # "none" THEN "cancelled" ELSE "ok";
a12:
  publish("F0", "R");                                         \* back in R: one more sync
a13:
  readFlags("F0", "R");
a14:
  park("F0");
a15:
  await rec["F0"].w = "D";
  members["R"] := members["R"] \ {"F0"};
a16:
  await members["R"] = {};                              \* the join of R
}

\* G1: a member of S. It fails, or it runs a cleanup in a shield.
fair process (g1 = "G1") {
b0:
  if (Scenario = "fork") { goto Done; };
bw:
  await "G1" \in members["S"];
  if (Scenario = "error") {
    g1Path := "error";
b1:
    if (BugExnAfterEnd) { members["S"] := members["S"] \ {"G1"}; }
    else { exnS := IF exnS = "none" THEN "E1" ELSE exnS; };
b2:
    if (BugExnAfterEnd) { exnS := IF exnS = "none" THEN "E1" ELSE exnS; }
    else { members["S"] := members["S"] \ {"G1"}; };
b3:
    call walk("S");                                     \* the scope fails
  } else {
    g1Path := "shield";
c1:
    linked["H"] := TRUE;                                \* Fiber.shield
c2:
    linked["T"] := TRUE || members["T"] := {"G1", "TT"};  \* with_timeout
c3:
    if (~SimpNoRecheck /\ cancelled[Parent("T")]) { cancelled["T"] := TRUE; };
c4:
    \* Condition.wait opens H before it publishes its sync.
    if (Scenario = "condwait" /\ ~BugCondOuterCtx) { hOpen := TRUE; };
c4p:
    publish("G1", IF Scenario = "condwait" /\ BugCondOuterCtx THEN "S" ELSE "T");
c5:
    readFlags("G1", rec["G1"].ctx);
c6:
    park("G1");
c7:
    await rec["G1"].w = "D";
    tRes := rec["G1"].rs;
    hOpen := FALSE;
c8:
    await members["T"] = {"G1"};                        \* the join of T
    members["T"] := {};
    toRes := IF tRes = "T" THEN "Timeout" ELSE "Cancelled";
c9:
    publish("G1", "S");                                       \* after the shield
c10:
    readFlags("G1", "S");
c11:
    park("G1");
c12:
    await rec["G1"].w = "D";
    members["S"] := members["S"] \ {"G1"};
  };
}

\* G2: a daemon of S. Only a cancel ends it.
fair process (g2 = "G2") {
d0:
  if (Scenario # "error") { goto Done; };
dw:
  await "G2" \in members["D2"];
d1:
  publish("G2", "D2");
d2:
  readFlags("G2", "D2");
d3:
  park("G2");
d4:
  await rec["G2"].w = "D";
  members["D2"] := {};
}

\* K: a member of R that holds the scope value S and forks G3 into it.
fair process (k = "K") {
k0:
  if (Scenario # "fork") { goto Done; };
k1:
  \* One CAS on the state word of S: fork only if S is open.
  \* The code reads the flags of the whole chain of S, as a sync does.
  if (linked["S"] /\ ~closed /\ FirstFlag("S") = "none") {
    members["S"] := members["S"] \cup {"G3"};
    g3 := "forked";
  } else {
    g3 := "closed";                                     \* Scope_closed
  };
k2:
  members["R"] := members["R"] \ {"K"};
}

\* G3: the fiber that K forks into S.
fair process (g3p = "G3") {
e0:
  await g3 # "none";
  if (g3 = "closed") { goto Done; };
e1:
  publish("G3", "S");
e2:
  readFlags("G3", "S");
e3:
  park("G3");
e4:
  await rec["G3"].w = "D";
  members["S"] := members["S"] \ {"G3"};
}

\* TT: the timer of with_timeout in G1. It fires after its sleep.
fair process (tt = "TT") {
t0:
  if (Scenario \notin {"shield", "condwait"}) { goto Done; };
t1:
  await linked["T"];
  call walk("T");
t2:
  members["T"] := members["T"] \ {"TT"};
}

\* The canceller: maybe a cancel of S, and in the end a cancel of R.
fair process (canc = "C") {
x0:
  either { call walk("S"); } or { skip; };
x1:
  call walk("R");
}
} *)

\* BEGIN TRANSLATION
CONSTANT defaultInitValue
VARIABLES cancelled, linked, members, closed, exnS, rec, g1Path, g3, sRes, 
          tRes, toRes, hOpen, pc, stack

(* define statement *)
RECURSIVE FirstFlag(_)
FirstFlag(c) ==
  IF c = "none" \/ (IsShield(c) /\ ~hOpen) THEN "none"
  ELSE IF cancelled[c] THEN c ELSE FirstFlag(Parent(c))

VARIABLES x, wt, wf, fx

vars == << cancelled, linked, members, closed, exnS, rec, g1Path, g3, sRes, 
           tRes, toRes, hOpen, pc, stack, x, wt, wf, fx >>

ProcSet == {"F0"} \cup {"G1"} \cup {"G2"} \cup {"K"} \cup {"G3"} \cup {"TT"} \cup {"C"}

Init == (* Global variables *)
        /\ cancelled = [n \in Nodes |-> FALSE]
        /\ linked = [n \in Nodes |-> n = "R"]
        /\ members =         [n \in Nodes |->
                     IF n = "R" THEN (IF Scenario = "fork" THEN {"F0", "K"} ELSE {"F0"}) ELSE {}]
        /\ closed = FALSE
        /\ exnS = "none"
        /\ rec = [f \in Fibers |-> NoRec]
        /\ g1Path = "none"
        /\ g3 = (IF Scenario = "fork" THEN "none" ELSE "closed")
        /\ sRes = "none"
        /\ tRes = "none"
        /\ toRes = "none"
        /\ hOpen = FALSE
        (* Procedure walk *)
        /\ x = [ self \in ProcSet |-> defaultInitValue]
        /\ wt = [ self \in ProcSet |-> {}]
        /\ wf = [ self \in ProcSet |-> {}]
        /\ fx = [ self \in ProcSet |-> "none"]
        /\ stack = [self \in ProcSet |-> << >>]
        /\ pc = [self \in ProcSet |-> CASE self = "F0" -> "a1"
                                        [] self = "G1" -> "b0"
                                        [] self = "G2" -> "d0"
                                        [] self = "K" -> "k0"
                                        [] self = "G3" -> "e0"
                                        [] self = "TT" -> "t0"
                                        [] self = "C" -> "x0"]

w1(self) == /\ pc[self] = "w1"
            /\ cancelled' = [cancelled EXCEPT ![x[self]] = TRUE]
            /\ wt' = [wt EXCEPT ![self] = {x[self]}]
            /\ pc' = [pc EXCEPT ![self] = "w2"]
            /\ UNCHANGED << linked, members, closed, exnS, rec, g1Path, g3, 
                            sRes, tRes, toRes, hOpen, stack, x, wf, fx >>

w2(self) == /\ pc[self] = "w2"
            /\ IF wt[self] # {}
                  THEN /\ \E n \in wt[self]:
                            /\ wf' = [wf EXCEPT ![self] = wf[self] \cup members[n]]
                            /\ wt' = [wt EXCEPT ![self] = (wt[self] \ {n}) \cup {z \in Nodes : linked[z] /\ Parent(z) = n /\ ~IsShield(z)}]
                            /\ IF ~SimpNoChildFlags
                                  THEN /\ cancelled' =            [z \in Nodes |->
                                                       IF linked[z] /\ Parent(z) = n /\ ~IsShield(z) THEN TRUE ELSE cancelled[z]]
                                  ELSE /\ TRUE
                                       /\ UNCHANGED cancelled
                       /\ pc' = [pc EXCEPT ![self] = "w2"]
                  ELSE /\ pc' = [pc EXCEPT ![self] = "w3"]
                       /\ UNCHANGED << cancelled, wt, wf >>
            /\ UNCHANGED << linked, members, closed, exnS, rec, g1Path, g3, 
                            sRes, tRes, toRes, hOpen, stack, x, fx >>

w3(self) == /\ pc[self] = "w3"
            /\ IF wf[self] # {}
                  THEN /\ \E g \in wf[self]:
                            /\ fx' = [fx EXCEPT ![self] = g]
                            /\ wf' = [wf EXCEPT ![self] = wf[self] \ {g}]
                       /\ IF rec[fx'[self]].w \in {"W", "P"} /\ (BugNoChainCheck \/ OnChain(rec[fx'[self]].ctx, x[self], hOpen))
                             THEN /\ rec' = [rec EXCEPT ![fx'[self]] = [rec[fx'[self]] EXCEPT !.w = "D", !.rs = x[self]]]
                             ELSE /\ TRUE
                                  /\ rec' = rec
                       /\ pc' = [pc EXCEPT ![self] = "w3"]
                  ELSE /\ pc' = [pc EXCEPT ![self] = "w4"]
                       /\ UNCHANGED << rec, wf, fx >>
            /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, 
                            g3, sRes, tRes, toRes, hOpen, stack, x, wt >>

w4(self) == /\ pc[self] = "w4"
            /\ pc' = [pc EXCEPT ![self] = Head(stack[self]).pc]
            /\ wt' = [wt EXCEPT ![self] = Head(stack[self]).wt]
            /\ wf' = [wf EXCEPT ![self] = Head(stack[self]).wf]
            /\ fx' = [fx EXCEPT ![self] = Head(stack[self]).fx]
            /\ x' = [x EXCEPT ![self] = Head(stack[self]).x]
            /\ stack' = [stack EXCEPT ![self] = Tail(stack[self])]
            /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, 
                            g1Path, g3, sRes, tRes, toRes, hOpen >>

walk(self) == w1(self) \/ w2(self) \/ w3(self) \/ w4(self)

a1 == /\ pc["F0"] = "a1"
      /\ /\ linked' = [linked EXCEPT !["S"] = TRUE]
         /\ members' = [members EXCEPT !["S"] = {"F0"}]
      /\ pc' = [pc EXCEPT !["F0"] = "a2"]
      /\ UNCHANGED << cancelled, closed, exnS, rec, g1Path, g3, sRes, tRes, 
                      toRes, hOpen, stack, x, wt, wf, fx >>

a2 == /\ pc["F0"] = "a2"
      /\ IF ~SimpNoRecheck /\ cancelled["R"]
            THEN /\ cancelled' = [cancelled EXCEPT !["S"] = TRUE]
            ELSE /\ TRUE
                 /\ UNCHANGED cancelled
      /\ pc' = [pc EXCEPT !["F0"] = "a3"]
      /\ UNCHANGED << linked, members, closed, exnS, rec, g1Path, g3, sRes, 
                      tRes, toRes, hOpen, stack, x, wt, wf, fx >>

a3 == /\ pc["F0"] = "a3"
      /\ /\ linked' = [linked EXCEPT !["D2"] = TRUE]
         /\ members' = [members EXCEPT !["S"] = members["S"] \cup (IF Scenario = "fork" THEN {} ELSE {"G1"}),
                                       !["D2"] = IF Scenario = "error" THEN {"G2"} ELSE {}]
      /\ pc' = [pc EXCEPT !["F0"] = "a4"]
      /\ UNCHANGED << cancelled, closed, exnS, rec, g1Path, g3, sRes, tRes, 
                      toRes, hOpen, stack, x, wt, wf, fx >>

a4 == /\ pc["F0"] = "a4"
      /\ rec' = [rec EXCEPT !["F0"] = [ctx |-> "S", w |-> "W", rs |-> "none", op |-> hOpen]]
      /\ pc' = [pc EXCEPT !["F0"] = "a5"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                      sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

a5 == /\ pc["F0"] = "a5"
      /\ IF ~BugNoFlagCheck /\ FirstFlag("S") # "none" /\ rec["F0"].w = "W"
            THEN /\ rec' = [rec EXCEPT !["F0"] = [rec["F0"] EXCEPT !.w = "D", !.rs = FirstFlag("S")]]
            ELSE /\ TRUE
                 /\ rec' = rec
      /\ pc' = [pc EXCEPT !["F0"] = "a6"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                      sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

a6 == /\ pc["F0"] = "a6"
      /\ IF rec["F0"].w = "W"
            THEN /\ rec' = [rec EXCEPT !["F0"].w = "P"]
            ELSE /\ TRUE
                 /\ rec' = rec
      /\ pc' = [pc EXCEPT !["F0"] = "a7"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                      sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

a7 == /\ pc["F0"] = "a7"
      /\ rec["F0"].w = "D"
      /\ members' = [members EXCEPT !["S"] = members["S"] \ {"F0"}]
      /\ pc' = [pc EXCEPT !["F0"] = "a8"]
      /\ UNCHANGED << cancelled, linked, closed, exnS, rec, g1Path, g3, sRes, 
                      tRes, toRes, hOpen, stack, x, wt, wf, fx >>

a8 == /\ pc["F0"] = "a8"
      /\ IF BugJoinCancellable /\ FirstFlag("S") # "none"
            THEN /\ pc' = [pc EXCEPT !["F0"] = "a11"]
            ELSE /\ pc' = [pc EXCEPT !["F0"] = "a8w"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                      g3, sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

a8w == /\ pc["F0"] = "a8w"
       /\ members["S"] = {}
       /\ pc' = [pc EXCEPT !["F0"] = "a9"]
       /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                       g3, sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

a9 == /\ pc["F0"] = "a9"
      /\ /\ stack' = [stack EXCEPT !["F0"] = << [ procedure |->  "walk",
                                                  pc        |->  "a10",
                                                  wt        |->  wt["F0"],
                                                  wf        |->  wf["F0"],
                                                  fx        |->  fx["F0"],
                                                  x         |->  x["F0"] ] >>
                                              \o stack["F0"]]
         /\ x' = [x EXCEPT !["F0"] = "D2"]
      /\ wt' = [wt EXCEPT !["F0"] = {}]
      /\ wf' = [wf EXCEPT !["F0"] = {}]
      /\ fx' = [fx EXCEPT !["F0"] = "none"]
      /\ pc' = [pc EXCEPT !["F0"] = "w1"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                      g3, sRes, tRes, toRes, hOpen >>

a10 == /\ pc["F0"] = "a10"
       /\ members["S"] = {} /\ members["D2"] = {}
       /\ closed' = TRUE
       /\ pc' = [pc EXCEPT !["F0"] = "a11"]
       /\ UNCHANGED << cancelled, linked, members, exnS, rec, g1Path, g3, sRes, 
                       tRes, toRes, hOpen, stack, x, wt, wf, fx >>

a11 == /\ pc["F0"] = "a11"
       /\ sRes' = (IF exnS # "none" THEN "exn"
                   ELSE IF FirstFlag("S") # "none" THEN "cancelled" ELSE "ok")
       /\ pc' = [pc EXCEPT !["F0"] = "a12"]
       /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                       g3, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

a12 == /\ pc["F0"] = "a12"
       /\ rec' = [rec EXCEPT !["F0"] = [ctx |-> "R", w |-> "W", rs |-> "none", op |-> hOpen]]
       /\ pc' = [pc EXCEPT !["F0"] = "a13"]
       /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                       sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

a13 == /\ pc["F0"] = "a13"
       /\ IF ~BugNoFlagCheck /\ FirstFlag("R") # "none" /\ rec["F0"].w = "W"
             THEN /\ rec' = [rec EXCEPT !["F0"] = [rec["F0"] EXCEPT !.w = "D", !.rs = FirstFlag("R")]]
             ELSE /\ TRUE
                  /\ rec' = rec
       /\ pc' = [pc EXCEPT !["F0"] = "a14"]
       /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                       sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

a14 == /\ pc["F0"] = "a14"
       /\ IF rec["F0"].w = "W"
             THEN /\ rec' = [rec EXCEPT !["F0"].w = "P"]
             ELSE /\ TRUE
                  /\ rec' = rec
       /\ pc' = [pc EXCEPT !["F0"] = "a15"]
       /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                       sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

a15 == /\ pc["F0"] = "a15"
       /\ rec["F0"].w = "D"
       /\ members' = [members EXCEPT !["R"] = members["R"] \ {"F0"}]
       /\ pc' = [pc EXCEPT !["F0"] = "a16"]
       /\ UNCHANGED << cancelled, linked, closed, exnS, rec, g1Path, g3, sRes, 
                       tRes, toRes, hOpen, stack, x, wt, wf, fx >>

a16 == /\ pc["F0"] = "a16"
       /\ members["R"] = {}
       /\ pc' = [pc EXCEPT !["F0"] = "Done"]
       /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                       g3, sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

f0 == a1 \/ a2 \/ a3 \/ a4 \/ a5 \/ a6 \/ a7 \/ a8 \/ a8w \/ a9 \/ a10
         \/ a11 \/ a12 \/ a13 \/ a14 \/ a15 \/ a16

b0 == /\ pc["G1"] = "b0"
      /\ IF Scenario = "fork"
            THEN /\ pc' = [pc EXCEPT !["G1"] = "Done"]
            ELSE /\ pc' = [pc EXCEPT !["G1"] = "bw"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                      g3, sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

bw == /\ pc["G1"] = "bw"
      /\ "G1" \in members["S"]
      /\ IF Scenario = "error"
            THEN /\ g1Path' = "error"
                 /\ pc' = [pc EXCEPT !["G1"] = "b1"]
            ELSE /\ g1Path' = "shield"
                 /\ pc' = [pc EXCEPT !["G1"] = "c1"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g3, sRes, 
                      tRes, toRes, hOpen, stack, x, wt, wf, fx >>

b1 == /\ pc["G1"] = "b1"
      /\ IF BugExnAfterEnd
            THEN /\ members' = [members EXCEPT !["S"] = members["S"] \ {"G1"}]
                 /\ exnS' = exnS
            ELSE /\ exnS' = (IF exnS = "none" THEN "E1" ELSE exnS)
                 /\ UNCHANGED members
      /\ pc' = [pc EXCEPT !["G1"] = "b2"]
      /\ UNCHANGED << cancelled, linked, closed, rec, g1Path, g3, sRes, tRes, 
                      toRes, hOpen, stack, x, wt, wf, fx >>

b2 == /\ pc["G1"] = "b2"
      /\ IF BugExnAfterEnd
            THEN /\ exnS' = (IF exnS = "none" THEN "E1" ELSE exnS)
                 /\ UNCHANGED members
            ELSE /\ members' = [members EXCEPT !["S"] = members["S"] \ {"G1"}]
                 /\ exnS' = exnS
      /\ pc' = [pc EXCEPT !["G1"] = "b3"]
      /\ UNCHANGED << cancelled, linked, closed, rec, g1Path, g3, sRes, tRes, 
                      toRes, hOpen, stack, x, wt, wf, fx >>

b3 == /\ pc["G1"] = "b3"
      /\ /\ stack' = [stack EXCEPT !["G1"] = << [ procedure |->  "walk",
                                                  pc        |->  "Done",
                                                  wt        |->  wt["G1"],
                                                  wf        |->  wf["G1"],
                                                  fx        |->  fx["G1"],
                                                  x         |->  x["G1"] ] >>
                                              \o stack["G1"]]
         /\ x' = [x EXCEPT !["G1"] = "S"]
      /\ wt' = [wt EXCEPT !["G1"] = {}]
      /\ wf' = [wf EXCEPT !["G1"] = {}]
      /\ fx' = [fx EXCEPT !["G1"] = "none"]
      /\ pc' = [pc EXCEPT !["G1"] = "w1"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                      g3, sRes, tRes, toRes, hOpen >>

c1 == /\ pc["G1"] = "c1"
      /\ linked' = [linked EXCEPT !["H"] = TRUE]
      /\ pc' = [pc EXCEPT !["G1"] = "c2"]
      /\ UNCHANGED << cancelled, members, closed, exnS, rec, g1Path, g3, sRes, 
                      tRes, toRes, hOpen, stack, x, wt, wf, fx >>

c2 == /\ pc["G1"] = "c2"
      /\ /\ linked' = [linked EXCEPT !["T"] = TRUE]
         /\ members' = [members EXCEPT !["T"] = {"G1", "TT"}]
      /\ pc' = [pc EXCEPT !["G1"] = "c3"]
      /\ UNCHANGED << cancelled, closed, exnS, rec, g1Path, g3, sRes, tRes, 
                      toRes, hOpen, stack, x, wt, wf, fx >>

c3 == /\ pc["G1"] = "c3"
      /\ IF ~SimpNoRecheck /\ cancelled[Parent("T")]
            THEN /\ cancelled' = [cancelled EXCEPT !["T"] = TRUE]
            ELSE /\ TRUE
                 /\ UNCHANGED cancelled
      /\ pc' = [pc EXCEPT !["G1"] = "c4"]
      /\ UNCHANGED << linked, members, closed, exnS, rec, g1Path, g3, sRes, 
                      tRes, toRes, hOpen, stack, x, wt, wf, fx >>

c4 == /\ pc["G1"] = "c4"
      /\ IF Scenario = "condwait" /\ ~BugCondOuterCtx
            THEN /\ hOpen' = TRUE
            ELSE /\ TRUE
                 /\ hOpen' = hOpen
      /\ pc' = [pc EXCEPT !["G1"] = "c4p"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                      g3, sRes, tRes, toRes, stack, x, wt, wf, fx >>

c4p == /\ pc["G1"] = "c4p"
       /\ rec' = [rec EXCEPT !["G1"] = [ctx |-> (IF Scenario = "condwait" /\ BugCondOuterCtx THEN "S" ELSE "T"), w |-> "W", rs |-> "none", op |-> hOpen]]
       /\ pc' = [pc EXCEPT !["G1"] = "c5"]
       /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                       sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

c5 == /\ pc["G1"] = "c5"
      /\ IF ~BugNoFlagCheck /\ FirstFlag((rec["G1"].ctx)) # "none" /\ rec["G1"].w = "W"
            THEN /\ rec' = [rec EXCEPT !["G1"] = [rec["G1"] EXCEPT !.w = "D", !.rs = FirstFlag((rec["G1"].ctx))]]
            ELSE /\ TRUE
                 /\ rec' = rec
      /\ pc' = [pc EXCEPT !["G1"] = "c6"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                      sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

c6 == /\ pc["G1"] = "c6"
      /\ IF rec["G1"].w = "W"
            THEN /\ rec' = [rec EXCEPT !["G1"].w = "P"]
            ELSE /\ TRUE
                 /\ rec' = rec
      /\ pc' = [pc EXCEPT !["G1"] = "c7"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                      sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

c7 == /\ pc["G1"] = "c7"
      /\ rec["G1"].w = "D"
      /\ tRes' = rec["G1"].rs
      /\ hOpen' = FALSE
      /\ pc' = [pc EXCEPT !["G1"] = "c8"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                      g3, sRes, toRes, stack, x, wt, wf, fx >>

c8 == /\ pc["G1"] = "c8"
      /\ members["T"] = {"G1"}
      /\ members' = [members EXCEPT !["T"] = {}]
      /\ toRes' = (IF tRes = "T" THEN "Timeout" ELSE "Cancelled")
      /\ pc' = [pc EXCEPT !["G1"] = "c9"]
      /\ UNCHANGED << cancelled, linked, closed, exnS, rec, g1Path, g3, sRes, 
                      tRes, hOpen, stack, x, wt, wf, fx >>

c9 == /\ pc["G1"] = "c9"
      /\ rec' = [rec EXCEPT !["G1"] = [ctx |-> "S", w |-> "W", rs |-> "none", op |-> hOpen]]
      /\ pc' = [pc EXCEPT !["G1"] = "c10"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                      sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

c10 == /\ pc["G1"] = "c10"
       /\ IF ~BugNoFlagCheck /\ FirstFlag("S") # "none" /\ rec["G1"].w = "W"
             THEN /\ rec' = [rec EXCEPT !["G1"] = [rec["G1"] EXCEPT !.w = "D", !.rs = FirstFlag("S")]]
             ELSE /\ TRUE
                  /\ rec' = rec
       /\ pc' = [pc EXCEPT !["G1"] = "c11"]
       /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                       sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

c11 == /\ pc["G1"] = "c11"
       /\ IF rec["G1"].w = "W"
             THEN /\ rec' = [rec EXCEPT !["G1"].w = "P"]
             ELSE /\ TRUE
                  /\ rec' = rec
       /\ pc' = [pc EXCEPT !["G1"] = "c12"]
       /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                       sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

c12 == /\ pc["G1"] = "c12"
       /\ rec["G1"].w = "D"
       /\ members' = [members EXCEPT !["S"] = members["S"] \ {"G1"}]
       /\ pc' = [pc EXCEPT !["G1"] = "Done"]
       /\ UNCHANGED << cancelled, linked, closed, exnS, rec, g1Path, g3, sRes, 
                       tRes, toRes, hOpen, stack, x, wt, wf, fx >>

g1 == b0 \/ bw \/ b1 \/ b2 \/ b3 \/ c1 \/ c2 \/ c3 \/ c4 \/ c4p \/ c5 \/ c6
         \/ c7 \/ c8 \/ c9 \/ c10 \/ c11 \/ c12

d0 == /\ pc["G2"] = "d0"
      /\ IF Scenario # "error"
            THEN /\ pc' = [pc EXCEPT !["G2"] = "Done"]
            ELSE /\ pc' = [pc EXCEPT !["G2"] = "dw"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                      g3, sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

dw == /\ pc["G2"] = "dw"
      /\ "G2" \in members["D2"]
      /\ pc' = [pc EXCEPT !["G2"] = "d1"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                      g3, sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

d1 == /\ pc["G2"] = "d1"
      /\ rec' = [rec EXCEPT !["G2"] = [ctx |-> "D2", w |-> "W", rs |-> "none", op |-> hOpen]]
      /\ pc' = [pc EXCEPT !["G2"] = "d2"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                      sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

d2 == /\ pc["G2"] = "d2"
      /\ IF ~BugNoFlagCheck /\ FirstFlag("D2") # "none" /\ rec["G2"].w = "W"
            THEN /\ rec' = [rec EXCEPT !["G2"] = [rec["G2"] EXCEPT !.w = "D", !.rs = FirstFlag("D2")]]
            ELSE /\ TRUE
                 /\ rec' = rec
      /\ pc' = [pc EXCEPT !["G2"] = "d3"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                      sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

d3 == /\ pc["G2"] = "d3"
      /\ IF rec["G2"].w = "W"
            THEN /\ rec' = [rec EXCEPT !["G2"].w = "P"]
            ELSE /\ TRUE
                 /\ rec' = rec
      /\ pc' = [pc EXCEPT !["G2"] = "d4"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                      sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

d4 == /\ pc["G2"] = "d4"
      /\ rec["G2"].w = "D"
      /\ members' = [members EXCEPT !["D2"] = {}]
      /\ pc' = [pc EXCEPT !["G2"] = "Done"]
      /\ UNCHANGED << cancelled, linked, closed, exnS, rec, g1Path, g3, sRes, 
                      tRes, toRes, hOpen, stack, x, wt, wf, fx >>

g2 == d0 \/ dw \/ d1 \/ d2 \/ d3 \/ d4

k0 == /\ pc["K"] = "k0"
      /\ IF Scenario # "fork"
            THEN /\ pc' = [pc EXCEPT !["K"] = "Done"]
            ELSE /\ pc' = [pc EXCEPT !["K"] = "k1"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                      g3, sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

k1 == /\ pc["K"] = "k1"
      /\ IF linked["S"] /\ ~closed /\ FirstFlag("S") = "none"
            THEN /\ members' = [members EXCEPT !["S"] = members["S"] \cup {"G3"}]
                 /\ g3' = "forked"
            ELSE /\ g3' = "closed"
                 /\ UNCHANGED members
      /\ pc' = [pc EXCEPT !["K"] = "k2"]
      /\ UNCHANGED << cancelled, linked, closed, exnS, rec, g1Path, sRes, tRes, 
                      toRes, hOpen, stack, x, wt, wf, fx >>

k2 == /\ pc["K"] = "k2"
      /\ members' = [members EXCEPT !["R"] = members["R"] \ {"K"}]
      /\ pc' = [pc EXCEPT !["K"] = "Done"]
      /\ UNCHANGED << cancelled, linked, closed, exnS, rec, g1Path, g3, sRes, 
                      tRes, toRes, hOpen, stack, x, wt, wf, fx >>

k == k0 \/ k1 \/ k2

e0 == /\ pc["G3"] = "e0"
      /\ g3 # "none"
      /\ IF g3 = "closed"
            THEN /\ pc' = [pc EXCEPT !["G3"] = "Done"]
            ELSE /\ pc' = [pc EXCEPT !["G3"] = "e1"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                      g3, sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

e1 == /\ pc["G3"] = "e1"
      /\ rec' = [rec EXCEPT !["G3"] = [ctx |-> "S", w |-> "W", rs |-> "none", op |-> hOpen]]
      /\ pc' = [pc EXCEPT !["G3"] = "e2"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                      sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

e2 == /\ pc["G3"] = "e2"
      /\ IF ~BugNoFlagCheck /\ FirstFlag("S") # "none" /\ rec["G3"].w = "W"
            THEN /\ rec' = [rec EXCEPT !["G3"] = [rec["G3"] EXCEPT !.w = "D", !.rs = FirstFlag("S")]]
            ELSE /\ TRUE
                 /\ rec' = rec
      /\ pc' = [pc EXCEPT !["G3"] = "e3"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                      sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

e3 == /\ pc["G3"] = "e3"
      /\ IF rec["G3"].w = "W"
            THEN /\ rec' = [rec EXCEPT !["G3"].w = "P"]
            ELSE /\ TRUE
                 /\ rec' = rec
      /\ pc' = [pc EXCEPT !["G3"] = "e4"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, g1Path, g3, 
                      sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

e4 == /\ pc["G3"] = "e4"
      /\ rec["G3"].w = "D"
      /\ members' = [members EXCEPT !["S"] = members["S"] \ {"G3"}]
      /\ pc' = [pc EXCEPT !["G3"] = "Done"]
      /\ UNCHANGED << cancelled, linked, closed, exnS, rec, g1Path, g3, sRes, 
                      tRes, toRes, hOpen, stack, x, wt, wf, fx >>

g3p == e0 \/ e1 \/ e2 \/ e3 \/ e4

t0 == /\ pc["TT"] = "t0"
      /\ IF Scenario \notin {"shield", "condwait"}
            THEN /\ pc' = [pc EXCEPT !["TT"] = "Done"]
            ELSE /\ pc' = [pc EXCEPT !["TT"] = "t1"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                      g3, sRes, tRes, toRes, hOpen, stack, x, wt, wf, fx >>

t1 == /\ pc["TT"] = "t1"
      /\ linked["T"]
      /\ /\ stack' = [stack EXCEPT !["TT"] = << [ procedure |->  "walk",
                                                  pc        |->  "t2",
                                                  wt        |->  wt["TT"],
                                                  wf        |->  wf["TT"],
                                                  fx        |->  fx["TT"],
                                                  x         |->  x["TT"] ] >>
                                              \o stack["TT"]]
         /\ x' = [x EXCEPT !["TT"] = "T"]
      /\ wt' = [wt EXCEPT !["TT"] = {}]
      /\ wf' = [wf EXCEPT !["TT"] = {}]
      /\ fx' = [fx EXCEPT !["TT"] = "none"]
      /\ pc' = [pc EXCEPT !["TT"] = "w1"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                      g3, sRes, tRes, toRes, hOpen >>

t2 == /\ pc["TT"] = "t2"
      /\ members' = [members EXCEPT !["T"] = members["T"] \ {"TT"}]
      /\ pc' = [pc EXCEPT !["TT"] = "Done"]
      /\ UNCHANGED << cancelled, linked, closed, exnS, rec, g1Path, g3, sRes, 
                      tRes, toRes, hOpen, stack, x, wt, wf, fx >>

tt == t0 \/ t1 \/ t2

x0 == /\ pc["C"] = "x0"
      /\ \/ /\ /\ stack' = [stack EXCEPT !["C"] = << [ procedure |->  "walk",
                                                       pc        |->  "x1",
                                                       wt        |->  wt["C"],
                                                       wf        |->  wf["C"],
                                                       fx        |->  fx["C"],
                                                       x         |->  x["C"] ] >>
                                                   \o stack["C"]]
               /\ x' = [x EXCEPT !["C"] = "S"]
            /\ wt' = [wt EXCEPT !["C"] = {}]
            /\ wf' = [wf EXCEPT !["C"] = {}]
            /\ fx' = [fx EXCEPT !["C"] = "none"]
            /\ pc' = [pc EXCEPT !["C"] = "w1"]
         \/ /\ TRUE
            /\ pc' = [pc EXCEPT !["C"] = "x1"]
            /\ UNCHANGED <<stack, x, wt, wf, fx>>
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                      g3, sRes, tRes, toRes, hOpen >>

x1 == /\ pc["C"] = "x1"
      /\ /\ stack' = [stack EXCEPT !["C"] = << [ procedure |->  "walk",
                                                 pc        |->  "Done",
                                                 wt        |->  wt["C"],
                                                 wf        |->  wf["C"],
                                                 fx        |->  fx["C"],
                                                 x         |->  x["C"] ] >>
                                             \o stack["C"]]
         /\ x' = [x EXCEPT !["C"] = "R"]
      /\ wt' = [wt EXCEPT !["C"] = {}]
      /\ wf' = [wf EXCEPT !["C"] = {}]
      /\ fx' = [fx EXCEPT !["C"] = "none"]
      /\ pc' = [pc EXCEPT !["C"] = "w1"]
      /\ UNCHANGED << cancelled, linked, members, closed, exnS, rec, g1Path, 
                      g3, sRes, tRes, toRes, hOpen >>

canc == x0 \/ x1

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == f0 \/ g1 \/ g2 \/ k \/ g3p \/ tt \/ canc
           \/ (\E self \in ProcSet: walk(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ WF_vars(f0) /\ WF_vars(walk("F0"))
        /\ WF_vars(g1) /\ WF_vars(walk("G1"))
        /\ WF_vars(g2)
        /\ WF_vars(k)
        /\ WF_vars(g3p)
        /\ WF_vars(tt) /\ WF_vars(walk("TT"))
        /\ WF_vars(canc) /\ WF_vars(walk("C"))

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION

-----------------------------------------------------------------------------
(* The properties. *)

\* No fiber outlives its scope: when Fiber.scope S returns, S has no member.
NoOrphan == sRes # "none" => (members["S"] = {} /\ members["D2"] = {})

\* The exception of a failed member is never lost.
ExnNotLost == (g1Path = "error" /\ exnS # "none" /\ sRes # "none") => sRes = "exn"
ExnSeen == (g1Path = "error" /\ pc["G1"] = "Done" /\ sRes # "none") => sRes = "exn"

\* The shield: a cancel from outside never ends the sync inside the shield.
\* A condition wait opens the shield, so the property is not for it.
ShieldHolds == Scenario = "condwait" \/ tRes \in {"none", "T"}

\* with_timeout raises Timeout only for its own deadline.
TimeoutOK == Scenario = "condwait" \/ toRes \in {"none", "Timeout"}

\* A cancel of X ends only a sync whose context chain reaches X.
NoForeignCancel ==
  \A f \in Fibers : rec[f].w = "D" => OnChain(rec[f].ctx, rec[f].rs, rec[f].op)

\* The condition wait: when the timer of with_timeout ended its walk of T, the
\* wait of G1 is not parked (the review of M2, F1).
TimerReachesWait ==
  (Scenario = "condwait" /\ pc["TT"] \in {"t2", "Done"} /\ pc["G1"] = "c7")
    => rec["G1"].w = "D"

\* The closed bit: a closed scope has no member, and a fork after it fails.
ClosedEmpty == closed => (members["S"] = {} /\ members["D2"] = {})

\* Termination (from the translator, with weak fairness): each process ends.
\* Thus each cancel reaches each fiber that it must reach, and each join ends.
=============================================================================
