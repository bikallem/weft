--------------------------------- MODULE Fd ---------------------------------
(***************************************************************************)
(* A model of the fd record of docs/design.md, section 2.7: the state     *)
(* machine of Eio's Rcfd, the registrations for each domain (decision 8), *)
(* the close notices and the removal of a registration.                   *)
(*                                                                         *)
(* One socket X has the fd number 7. The record of X has one atomic       *)
(* state: open, closing or closed, with a count of users. Each            *)
(* registration in the poller of a domain holds one user.                 *)
(*                                                                         *)
(* The processes:                                                          *)
(*   WA, WB  fibers that block on X, on domain A and on domain B. Each    *)
(*           registers X in the poller of its domain, then waits.          *)
(*   Edge    data arrives for WA. Then the fiber moves, so domain A can   *)
(*           remove its registration (an edge with no live waiter).       *)
(*   C       a fiber that closes X.                                        *)
(*   L       a fiber that does a late read on X, at any time.             *)
(*   DA, DB  domains A and B: each one takes a close notice from its      *)
(*           mailbox.                                                      *)
(*   N       after close(2), a new socket Y gets the same number 7, and   *)
(*           domain A registers it.                                        *)
(*                                                                         *)
(* The registration, the removal and the handling of a notice of one     *)
(* domain all run on the thread of that domain, and the thread takes its  *)
(* mailbox only between fibers. Thus they never interleave: the model     *)
(* holds busy[d] for each of them.                                         *)
(*                                                                         *)
(* The kernel: epoll_ctl DEL finds the file by the current file of the    *)
(* fd number. With DupAlive, another process holds a copy of X, so close  *)
(* (2) does not remove the registrations of X.                             *)
(***************************************************************************)
EXTENDS Integers, FiniteSets

CONSTANTS
  DupAlive,          \* another process holds a dup of X
  BugCountOnly,      \* a use count and a closed bit, not the state machine
  BugNoBitCas,       \* a notice and a removal do not decide with a CAS on the bit
  BugRegisterOrder,  \* Register reads the state before it sets its domain bit
  BugRegNoUser       \* a registration holds no user

Doms == {"A", "B"}

(* --algorithm Fd {
variables
  st = "open",                 \* the state of the record of X
  users = 0,
  dset = {},                   \* the domains that registered X
  regLive = [d \in Doms |-> FALSE],
  waiting = [d \in Doms |-> FALSE],
  wres = [d \in Doms |-> "none"],
  mb = [d \in Doms |-> FALSE],  \* a close notice for X in the mailbox
  kfd = "X",                   \* the file of fd number 7 in the kernel
  regs = {},                   \* the epoll registrations: <<domain, file>>
  closes = 0,                  \* the close(2) calls for X
  wrongClose = FALSE,
  badUse = FALSE,
  yDeleted = FALSE,
  regsAtClose = {},
  busy = [x \in Doms |-> FALSE];  \* the thread of the domain runs a sequence

define {
  XRegs == {r \in regs : r[2] = "X"}
}

\* close(2) of X: the kernel releases the number.
macro sysClose() {
  closes := closes + 1;
  if (kfd # "X") { wrongClose := TRUE; };
  regsAtClose := XRegs;
  kfd := "none";
  if (~DupAlive) { regs := regs \ XRegs; };
}

\* Remove one user. The side that changes closing 0 to closed calls close(2).
macro release() {
  users := users - 1;
  if (users = 0 /\ st = "closing") {
    st := "closed";
    sysClose();
  };
}

\* epoll_ctl DEL of fd number 7 in the set of domain d.
macro epollDel(d) {
  if (kfd # "none") {
    if (kfd = "Y" /\ <<d, "Y">> \in regs) { yDeleted := TRUE; };
    regs := regs \ {<<d, kfd>>};
  };
}

\* A fiber that blocks on X on domain d.
fair process (w \in {"WA", "WB"})
variables me = "none";
{
g0:
  me := IF self = "WA" THEN "A" ELSE "B";
g1:
  await ~busy[me];
  busy[me] := TRUE;
g1a:
  \* Register: add a user (a CAS only in open).
  if (BugRegNoUser) { if (st # "open") { wres[me] := "ebadf"; busy[me] := FALSE; goto Done; }; }
  else if (st = "open") { users := users + 1; }
  else { wres[me] := "ebadf"; busy[me] := FALSE; goto Done; };
g2:
  if (BugRegisterOrder) {
    if (st # "open") { if (~BugRegNoUser) { release(); }; wres[me] := "ebadf"; busy[me] := FALSE; goto Done; };
  } else {
    dset := dset \cup {me};               \* set the domain bit, then read
  };
g3:
  if (BugRegisterOrder) {
    dset := dset \cup {me};
  } else if (st # "open") {
    \* Closing: undo, and give EBADF to the waiter.
    dset := dset \ {me};
    if (~BugRegNoUser) { release(); };
    wres[me] := "ebadf";
    busy[me] := FALSE;
    goto Done;
  };
g4:
  regs := regs \cup {<<me, "X">>};       \* EPOLL_CTL_ADD
  regLive[me] := TRUE;
  waiting[me] := TRUE;
  busy[me] := FALSE;
g5:
  await wres[me] # "none";
}

\* Data arrives for WA. Then WA moves, and domain A removes its
\* registration: an edge with no live waiter.
fair process (edge = "Edge")
{
e1:
  await waiting["A"] \/ pc["WA"] = "Done";
  if (waiting["A"] /\ wres["A"] = "none") {
    wres["A"] := "data";
    waiting["A"] := FALSE;
  };
e2:
  either {
    await ~busy["A"];
    busy["A"] := TRUE;
e2a:
    \* The removal. With the CAS, only one side clears the bit.
    if (regLive["A"] /\ (BugNoBitCas \/ "A" \in dset)) {
      dset := dset \ {"A"};
e3:
      epollDel("A");
      regLive["A"] := FALSE;
e4:
      if (~BugRegNoUser) { release(); };
    };
e5:
    busy["A"] := FALSE;
  } or { skip; };
}

\* The close of X.
fair process (cl = "C")
variables ds = {};
{
c1:
  if (st = "open") {
    \* open n to closing n. With no user, the closer is the last use.
    st := IF users = 0 THEN "closed" ELSE "closing";
    if (users = 0) { sysClose(); };
  } else { goto Done; };
c2:
  ds := dset;                            \* set closing, then read the set
c3:
  mb := [x \in Doms |-> mb[x] \/ x \in ds];
}

\* A late read on X.
fair process (late = "L")
{
l1:
  either {
    if (BugCountOnly) {
      users := users + 1;                \* the count first, then the bit
    } else if (st = "open") {
      users := users + 1;
    } else { goto Done; };
l2:
    if (BugCountOnly /\ st # "open") {
      \* The closed bit is set: remove the use. A last use calls close(2).
      users := users - 1;
      if (users = 0) { st := "closed"; sysClose(); };
      goto Done;
    };
l3:
    if (kfd # "X") { badUse := TRUE; };  \* the syscall on number 7
l4:
    release();
  } or { skip; };
}

\* Domain d takes a close notice from its mailbox.
fair process (dm \in {"DA", "DB"})
variables md = "none";
{
m0:
  md := IF self = "DA" THEN "A" ELSE "B";
m1:
  await mb[md] \/ pc["C"] = "Done";
  if (~mb[md]) { goto Done; };
m1a:
  await ~busy[md];
  busy[md] := TRUE;
m2:
  \* Act only for a live registration, and only if this side clears the bit.
  if (regLive[md] /\ (BugNoBitCas \/ md \in dset)) {
    dset := dset \ {md};
    if (waiting[md] /\ wres[md] = "none") { wres[md] := "ebadf"; waiting[md] := FALSE; };
m3:
    epollDel(md);
    regLive[md] := FALSE;
m4:
    if (~BugRegNoUser) { release(); };
  };
m5:
  busy[md] := FALSE;
}

\* After close(2), a new socket Y gets the number 7 on domain A.
fair process (nw = "N")
{
n1:
  await kfd = "none";
  kfd := "Y";
n2:
  regs := regs \cup {<<"A", "Y">>};
}
} *)

\* BEGIN TRANSLATION
VARIABLES st, users, dset, regLive, waiting, wres, mb, kfd, regs, closes, 
          wrongClose, badUse, yDeleted, regsAtClose, busy, pc

(* define statement *)
XRegs == {r \in regs : r[2] = "X"}

VARIABLES me, ds, md

vars == << st, users, dset, regLive, waiting, wres, mb, kfd, regs, closes, 
           wrongClose, badUse, yDeleted, regsAtClose, busy, pc, me, ds, md >>

ProcSet == ({"WA", "WB"}) \cup {"Edge"} \cup {"C"} \cup {"L"} \cup ({"DA", "DB"}) \cup {"N"}

Init == (* Global variables *)
        /\ st = "open"
        /\ users = 0
        /\ dset = {}
        /\ regLive = [d \in Doms |-> FALSE]
        /\ waiting = [d \in Doms |-> FALSE]
        /\ wres = [d \in Doms |-> "none"]
        /\ mb = [d \in Doms |-> FALSE]
        /\ kfd = "X"
        /\ regs = {}
        /\ closes = 0
        /\ wrongClose = FALSE
        /\ badUse = FALSE
        /\ yDeleted = FALSE
        /\ regsAtClose = {}
        /\ busy = [x \in Doms |-> FALSE]
        (* Process w *)
        /\ me = [self \in {"WA", "WB"} |-> "none"]
        (* Process cl *)
        /\ ds = {}
        (* Process dm *)
        /\ md = [self \in {"DA", "DB"} |-> "none"]
        /\ pc = [self \in ProcSet |-> CASE self \in {"WA", "WB"} -> "g0"
                                        [] self = "Edge" -> "e1"
                                        [] self = "C" -> "c1"
                                        [] self = "L" -> "l1"
                                        [] self \in {"DA", "DB"} -> "m0"
                                        [] self = "N" -> "n1"]

g0(self) == /\ pc[self] = "g0"
            /\ me' = [me EXCEPT ![self] = IF self = "WA" THEN "A" ELSE "B"]
            /\ pc' = [pc EXCEPT ![self] = "g1"]
            /\ UNCHANGED << st, users, dset, regLive, waiting, wres, mb, kfd, 
                            regs, closes, wrongClose, badUse, yDeleted, 
                            regsAtClose, busy, ds, md >>

g1(self) == /\ pc[self] = "g1"
            /\ ~busy[me[self]]
            /\ busy' = [busy EXCEPT ![me[self]] = TRUE]
            /\ pc' = [pc EXCEPT ![self] = "g1a"]
            /\ UNCHANGED << st, users, dset, regLive, waiting, wres, mb, kfd, 
                            regs, closes, wrongClose, badUse, yDeleted, 
                            regsAtClose, me, ds, md >>

g1a(self) == /\ pc[self] = "g1a"
             /\ IF BugRegNoUser
                   THEN /\ IF st # "open"
                              THEN /\ wres' = [wres EXCEPT ![me[self]] = "ebadf"]
                                   /\ busy' = [busy EXCEPT ![me[self]] = FALSE]
                                   /\ pc' = [pc EXCEPT ![self] = "Done"]
                              ELSE /\ pc' = [pc EXCEPT ![self] = "g2"]
                                   /\ UNCHANGED << wres, busy >>
                        /\ users' = users
                   ELSE /\ IF st = "open"
                              THEN /\ users' = users + 1
                                   /\ pc' = [pc EXCEPT ![self] = "g2"]
                                   /\ UNCHANGED << wres, busy >>
                              ELSE /\ wres' = [wres EXCEPT ![me[self]] = "ebadf"]
                                   /\ busy' = [busy EXCEPT ![me[self]] = FALSE]
                                   /\ pc' = [pc EXCEPT ![self] = "Done"]
                                   /\ users' = users
             /\ UNCHANGED << st, dset, regLive, waiting, mb, kfd, regs, closes, 
                             wrongClose, badUse, yDeleted, regsAtClose, me, ds, 
                             md >>

g2(self) == /\ pc[self] = "g2"
            /\ IF BugRegisterOrder
                  THEN /\ IF st # "open"
                             THEN /\ IF ~BugRegNoUser
                                        THEN /\ users' = users - 1
                                             /\ IF users' = 0 /\ st = "closing"
                                                   THEN /\ st' = "closed"
                                                        /\ closes' = closes + 1
                                                        /\ IF kfd # "X"
                                                              THEN /\ wrongClose' = TRUE
                                                              ELSE /\ TRUE
                                                                   /\ UNCHANGED wrongClose
                                                        /\ regsAtClose' = XRegs
                                                        /\ kfd' = "none"
                                                        /\ IF ~DupAlive
                                                              THEN /\ regs' = regs \ XRegs
                                                              ELSE /\ TRUE
                                                                   /\ regs' = regs
                                                   ELSE /\ TRUE
                                                        /\ UNCHANGED << st, 
                                                                        kfd, 
                                                                        regs, 
                                                                        closes, 
                                                                        wrongClose, 
                                                                        regsAtClose >>
                                        ELSE /\ TRUE
                                             /\ UNCHANGED << st, users, kfd, 
                                                             regs, closes, 
                                                             wrongClose, 
                                                             regsAtClose >>
                                  /\ wres' = [wres EXCEPT ![me[self]] = "ebadf"]
                                  /\ busy' = [busy EXCEPT ![me[self]] = FALSE]
                                  /\ pc' = [pc EXCEPT ![self] = "Done"]
                             ELSE /\ pc' = [pc EXCEPT ![self] = "g3"]
                                  /\ UNCHANGED << st, users, wres, kfd, regs, 
                                                  closes, wrongClose, 
                                                  regsAtClose, busy >>
                       /\ dset' = dset
                  ELSE /\ dset' = (dset \cup {me[self]})
                       /\ pc' = [pc EXCEPT ![self] = "g3"]
                       /\ UNCHANGED << st, users, wres, kfd, regs, closes, 
                                       wrongClose, regsAtClose, busy >>
            /\ UNCHANGED << regLive, waiting, mb, badUse, yDeleted, me, ds, md >>

g3(self) == /\ pc[self] = "g3"
            /\ IF BugRegisterOrder
                  THEN /\ dset' = (dset \cup {me[self]})
                       /\ pc' = [pc EXCEPT ![self] = "g4"]
                       /\ UNCHANGED << st, users, wres, kfd, regs, closes, 
                                       wrongClose, regsAtClose, busy >>
                  ELSE /\ IF st # "open"
                             THEN /\ dset' = dset \ {me[self]}
                                  /\ IF ~BugRegNoUser
                                        THEN /\ users' = users - 1
                                             /\ IF users' = 0 /\ st = "closing"
                                                   THEN /\ st' = "closed"
                                                        /\ closes' = closes + 1
                                                        /\ IF kfd # "X"
                                                              THEN /\ wrongClose' = TRUE
                                                              ELSE /\ TRUE
                                                                   /\ UNCHANGED wrongClose
                                                        /\ regsAtClose' = XRegs
                                                        /\ kfd' = "none"
                                                        /\ IF ~DupAlive
                                                              THEN /\ regs' = regs \ XRegs
                                                              ELSE /\ TRUE
                                                                   /\ regs' = regs
                                                   ELSE /\ TRUE
                                                        /\ UNCHANGED << st, 
                                                                        kfd, 
                                                                        regs, 
                                                                        closes, 
                                                                        wrongClose, 
                                                                        regsAtClose >>
                                        ELSE /\ TRUE
                                             /\ UNCHANGED << st, users, kfd, 
                                                             regs, closes, 
                                                             wrongClose, 
                                                             regsAtClose >>
                                  /\ wres' = [wres EXCEPT ![me[self]] = "ebadf"]
                                  /\ busy' = [busy EXCEPT ![me[self]] = FALSE]
                                  /\ pc' = [pc EXCEPT ![self] = "Done"]
                             ELSE /\ pc' = [pc EXCEPT ![self] = "g4"]
                                  /\ UNCHANGED << st, users, dset, wres, kfd, 
                                                  regs, closes, wrongClose, 
                                                  regsAtClose, busy >>
            /\ UNCHANGED << regLive, waiting, mb, badUse, yDeleted, me, ds, md >>

g4(self) == /\ pc[self] = "g4"
            /\ regs' = (regs \cup {<<me[self], "X">>})
            /\ regLive' = [regLive EXCEPT ![me[self]] = TRUE]
            /\ waiting' = [waiting EXCEPT ![me[self]] = TRUE]
            /\ busy' = [busy EXCEPT ![me[self]] = FALSE]
            /\ pc' = [pc EXCEPT ![self] = "g5"]
            /\ UNCHANGED << st, users, dset, wres, mb, kfd, closes, wrongClose, 
                            badUse, yDeleted, regsAtClose, me, ds, md >>

g5(self) == /\ pc[self] = "g5"
            /\ wres[me[self]] # "none"
            /\ pc' = [pc EXCEPT ![self] = "Done"]
            /\ UNCHANGED << st, users, dset, regLive, waiting, wres, mb, kfd, 
                            regs, closes, wrongClose, badUse, yDeleted, 
                            regsAtClose, busy, me, ds, md >>

w(self) == g0(self) \/ g1(self) \/ g1a(self) \/ g2(self) \/ g3(self)
              \/ g4(self) \/ g5(self)

e1 == /\ pc["Edge"] = "e1"
      /\ waiting["A"] \/ pc["WA"] = "Done"
      /\ IF waiting["A"] /\ wres["A"] = "none"
            THEN /\ wres' = [wres EXCEPT !["A"] = "data"]
                 /\ waiting' = [waiting EXCEPT !["A"] = FALSE]
            ELSE /\ TRUE
                 /\ UNCHANGED << waiting, wres >>
      /\ pc' = [pc EXCEPT !["Edge"] = "e2"]
      /\ UNCHANGED << st, users, dset, regLive, mb, kfd, regs, closes, 
                      wrongClose, badUse, yDeleted, regsAtClose, busy, me, ds, 
                      md >>

e2 == /\ pc["Edge"] = "e2"
      /\ \/ /\ ~busy["A"]
            /\ busy' = [busy EXCEPT !["A"] = TRUE]
            /\ pc' = [pc EXCEPT !["Edge"] = "e2a"]
         \/ /\ TRUE
            /\ pc' = [pc EXCEPT !["Edge"] = "Done"]
            /\ busy' = busy
      /\ UNCHANGED << st, users, dset, regLive, waiting, wres, mb, kfd, regs, 
                      closes, wrongClose, badUse, yDeleted, regsAtClose, me, 
                      ds, md >>

e2a == /\ pc["Edge"] = "e2a"
       /\ IF regLive["A"] /\ (BugNoBitCas \/ "A" \in dset)
             THEN /\ dset' = dset \ {"A"}
                  /\ pc' = [pc EXCEPT !["Edge"] = "e3"]
             ELSE /\ pc' = [pc EXCEPT !["Edge"] = "e5"]
                  /\ dset' = dset
       /\ UNCHANGED << st, users, regLive, waiting, wres, mb, kfd, regs, 
                       closes, wrongClose, badUse, yDeleted, regsAtClose, busy, 
                       me, ds, md >>

e3 == /\ pc["Edge"] = "e3"
      /\ IF kfd # "none"
            THEN /\ IF kfd = "Y" /\ <<"A", "Y">> \in regs
                       THEN /\ yDeleted' = TRUE
                       ELSE /\ TRUE
                            /\ UNCHANGED yDeleted
                 /\ regs' = regs \ {<<"A", kfd>>}
            ELSE /\ TRUE
                 /\ UNCHANGED << regs, yDeleted >>
      /\ regLive' = [regLive EXCEPT !["A"] = FALSE]
      /\ pc' = [pc EXCEPT !["Edge"] = "e4"]
      /\ UNCHANGED << st, users, dset, waiting, wres, mb, kfd, closes, 
                      wrongClose, badUse, regsAtClose, busy, me, ds, md >>

e4 == /\ pc["Edge"] = "e4"
      /\ IF ~BugRegNoUser
            THEN /\ users' = users - 1
                 /\ IF users' = 0 /\ st = "closing"
                       THEN /\ st' = "closed"
                            /\ closes' = closes + 1
                            /\ IF kfd # "X"
                                  THEN /\ wrongClose' = TRUE
                                  ELSE /\ TRUE
                                       /\ UNCHANGED wrongClose
                            /\ regsAtClose' = XRegs
                            /\ kfd' = "none"
                            /\ IF ~DupAlive
                                  THEN /\ regs' = regs \ XRegs
                                  ELSE /\ TRUE
                                       /\ regs' = regs
                       ELSE /\ TRUE
                            /\ UNCHANGED << st, kfd, regs, closes, wrongClose, 
                                            regsAtClose >>
            ELSE /\ TRUE
                 /\ UNCHANGED << st, users, kfd, regs, closes, wrongClose, 
                                 regsAtClose >>
      /\ pc' = [pc EXCEPT !["Edge"] = "e5"]
      /\ UNCHANGED << dset, regLive, waiting, wres, mb, badUse, yDeleted, busy, 
                      me, ds, md >>

e5 == /\ pc["Edge"] = "e5"
      /\ busy' = [busy EXCEPT !["A"] = FALSE]
      /\ pc' = [pc EXCEPT !["Edge"] = "Done"]
      /\ UNCHANGED << st, users, dset, regLive, waiting, wres, mb, kfd, regs, 
                      closes, wrongClose, badUse, yDeleted, regsAtClose, me, 
                      ds, md >>

edge == e1 \/ e2 \/ e2a \/ e3 \/ e4 \/ e5

c1 == /\ pc["C"] = "c1"
      /\ IF st = "open"
            THEN /\ st' = (IF users = 0 THEN "closed" ELSE "closing")
                 /\ IF users = 0
                       THEN /\ closes' = closes + 1
                            /\ IF kfd # "X"
                                  THEN /\ wrongClose' = TRUE
                                  ELSE /\ TRUE
                                       /\ UNCHANGED wrongClose
                            /\ regsAtClose' = XRegs
                            /\ kfd' = "none"
                            /\ IF ~DupAlive
                                  THEN /\ regs' = regs \ XRegs
                                  ELSE /\ TRUE
                                       /\ regs' = regs
                       ELSE /\ TRUE
                            /\ UNCHANGED << kfd, regs, closes, wrongClose, 
                                            regsAtClose >>
                 /\ pc' = [pc EXCEPT !["C"] = "c2"]
            ELSE /\ pc' = [pc EXCEPT !["C"] = "Done"]
                 /\ UNCHANGED << st, kfd, regs, closes, wrongClose, 
                                 regsAtClose >>
      /\ UNCHANGED << users, dset, regLive, waiting, wres, mb, badUse, 
                      yDeleted, busy, me, ds, md >>

c2 == /\ pc["C"] = "c2"
      /\ ds' = dset
      /\ pc' = [pc EXCEPT !["C"] = "c3"]
      /\ UNCHANGED << st, users, dset, regLive, waiting, wres, mb, kfd, regs, 
                      closes, wrongClose, badUse, yDeleted, regsAtClose, busy, 
                      me, md >>

c3 == /\ pc["C"] = "c3"
      /\ mb' = [x \in Doms |-> mb[x] \/ x \in ds]
      /\ pc' = [pc EXCEPT !["C"] = "Done"]
      /\ UNCHANGED << st, users, dset, regLive, waiting, wres, kfd, regs, 
                      closes, wrongClose, badUse, yDeleted, regsAtClose, busy, 
                      me, ds, md >>

cl == c1 \/ c2 \/ c3

l1 == /\ pc["L"] = "l1"
      /\ \/ /\ IF BugCountOnly
                  THEN /\ users' = users + 1
                       /\ pc' = [pc EXCEPT !["L"] = "l2"]
                  ELSE /\ IF st = "open"
                             THEN /\ users' = users + 1
                                  /\ pc' = [pc EXCEPT !["L"] = "l2"]
                             ELSE /\ pc' = [pc EXCEPT !["L"] = "Done"]
                                  /\ users' = users
         \/ /\ TRUE
            /\ pc' = [pc EXCEPT !["L"] = "Done"]
            /\ users' = users
      /\ UNCHANGED << st, dset, regLive, waiting, wres, mb, kfd, regs, closes, 
                      wrongClose, badUse, yDeleted, regsAtClose, busy, me, ds, 
                      md >>

l2 == /\ pc["L"] = "l2"
      /\ IF BugCountOnly /\ st # "open"
            THEN /\ users' = users - 1
                 /\ IF users' = 0
                       THEN /\ st' = "closed"
                            /\ closes' = closes + 1
                            /\ IF kfd # "X"
                                  THEN /\ wrongClose' = TRUE
                                  ELSE /\ TRUE
                                       /\ UNCHANGED wrongClose
                            /\ regsAtClose' = XRegs
                            /\ kfd' = "none"
                            /\ IF ~DupAlive
                                  THEN /\ regs' = regs \ XRegs
                                  ELSE /\ TRUE
                                       /\ regs' = regs
                       ELSE /\ TRUE
                            /\ UNCHANGED << st, kfd, regs, closes, wrongClose, 
                                            regsAtClose >>
                 /\ pc' = [pc EXCEPT !["L"] = "Done"]
            ELSE /\ pc' = [pc EXCEPT !["L"] = "l3"]
                 /\ UNCHANGED << st, users, kfd, regs, closes, wrongClose, 
                                 regsAtClose >>
      /\ UNCHANGED << dset, regLive, waiting, wres, mb, badUse, yDeleted, busy, 
                      me, ds, md >>

l3 == /\ pc["L"] = "l3"
      /\ IF kfd # "X"
            THEN /\ badUse' = TRUE
            ELSE /\ TRUE
                 /\ UNCHANGED badUse
      /\ pc' = [pc EXCEPT !["L"] = "l4"]
      /\ UNCHANGED << st, users, dset, regLive, waiting, wres, mb, kfd, regs, 
                      closes, wrongClose, yDeleted, regsAtClose, busy, me, ds, 
                      md >>

l4 == /\ pc["L"] = "l4"
      /\ users' = users - 1
      /\ IF users' = 0 /\ st = "closing"
            THEN /\ st' = "closed"
                 /\ closes' = closes + 1
                 /\ IF kfd # "X"
                       THEN /\ wrongClose' = TRUE
                       ELSE /\ TRUE
                            /\ UNCHANGED wrongClose
                 /\ regsAtClose' = XRegs
                 /\ kfd' = "none"
                 /\ IF ~DupAlive
                       THEN /\ regs' = regs \ XRegs
                       ELSE /\ TRUE
                            /\ regs' = regs
            ELSE /\ TRUE
                 /\ UNCHANGED << st, kfd, regs, closes, wrongClose, 
                                 regsAtClose >>
      /\ pc' = [pc EXCEPT !["L"] = "Done"]
      /\ UNCHANGED << dset, regLive, waiting, wres, mb, badUse, yDeleted, busy, 
                      me, ds, md >>

late == l1 \/ l2 \/ l3 \/ l4

m0(self) == /\ pc[self] = "m0"
            /\ md' = [md EXCEPT ![self] = IF self = "DA" THEN "A" ELSE "B"]
            /\ pc' = [pc EXCEPT ![self] = "m1"]
            /\ UNCHANGED << st, users, dset, regLive, waiting, wres, mb, kfd, 
                            regs, closes, wrongClose, badUse, yDeleted, 
                            regsAtClose, busy, me, ds >>

m1(self) == /\ pc[self] = "m1"
            /\ mb[md[self]] \/ pc["C"] = "Done"
            /\ IF ~mb[md[self]]
                  THEN /\ pc' = [pc EXCEPT ![self] = "Done"]
                  ELSE /\ pc' = [pc EXCEPT ![self] = "m1a"]
            /\ UNCHANGED << st, users, dset, regLive, waiting, wres, mb, kfd, 
                            regs, closes, wrongClose, badUse, yDeleted, 
                            regsAtClose, busy, me, ds, md >>

m1a(self) == /\ pc[self] = "m1a"
             /\ ~busy[md[self]]
             /\ busy' = [busy EXCEPT ![md[self]] = TRUE]
             /\ pc' = [pc EXCEPT ![self] = "m2"]
             /\ UNCHANGED << st, users, dset, regLive, waiting, wres, mb, kfd, 
                             regs, closes, wrongClose, badUse, yDeleted, 
                             regsAtClose, me, ds, md >>

m2(self) == /\ pc[self] = "m2"
            /\ IF regLive[md[self]] /\ (BugNoBitCas \/ md[self] \in dset)
                  THEN /\ dset' = dset \ {md[self]}
                       /\ IF waiting[md[self]] /\ wres[md[self]] = "none"
                             THEN /\ wres' = [wres EXCEPT ![md[self]] = "ebadf"]
                                  /\ waiting' = [waiting EXCEPT ![md[self]] = FALSE]
                             ELSE /\ TRUE
                                  /\ UNCHANGED << waiting, wres >>
                       /\ pc' = [pc EXCEPT ![self] = "m3"]
                  ELSE /\ pc' = [pc EXCEPT ![self] = "m5"]
                       /\ UNCHANGED << dset, waiting, wres >>
            /\ UNCHANGED << st, users, regLive, mb, kfd, regs, closes, 
                            wrongClose, badUse, yDeleted, regsAtClose, busy, 
                            me, ds, md >>

m3(self) == /\ pc[self] = "m3"
            /\ IF kfd # "none"
                  THEN /\ IF kfd = "Y" /\ <<md[self], "Y">> \in regs
                             THEN /\ yDeleted' = TRUE
                             ELSE /\ TRUE
                                  /\ UNCHANGED yDeleted
                       /\ regs' = regs \ {<<md[self], kfd>>}
                  ELSE /\ TRUE
                       /\ UNCHANGED << regs, yDeleted >>
            /\ regLive' = [regLive EXCEPT ![md[self]] = FALSE]
            /\ pc' = [pc EXCEPT ![self] = "m4"]
            /\ UNCHANGED << st, users, dset, waiting, wres, mb, kfd, closes, 
                            wrongClose, badUse, regsAtClose, busy, me, ds, md >>

m4(self) == /\ pc[self] = "m4"
            /\ IF ~BugRegNoUser
                  THEN /\ users' = users - 1
                       /\ IF users' = 0 /\ st = "closing"
                             THEN /\ st' = "closed"
                                  /\ closes' = closes + 1
                                  /\ IF kfd # "X"
                                        THEN /\ wrongClose' = TRUE
                                        ELSE /\ TRUE
                                             /\ UNCHANGED wrongClose
                                  /\ regsAtClose' = XRegs
                                  /\ kfd' = "none"
                                  /\ IF ~DupAlive
                                        THEN /\ regs' = regs \ XRegs
                                        ELSE /\ TRUE
                                             /\ regs' = regs
                             ELSE /\ TRUE
                                  /\ UNCHANGED << st, kfd, regs, closes, 
                                                  wrongClose, regsAtClose >>
                  ELSE /\ TRUE
                       /\ UNCHANGED << st, users, kfd, regs, closes, 
                                       wrongClose, regsAtClose >>
            /\ pc' = [pc EXCEPT ![self] = "m5"]
            /\ UNCHANGED << dset, regLive, waiting, wres, mb, badUse, yDeleted, 
                            busy, me, ds, md >>

m5(self) == /\ pc[self] = "m5"
            /\ busy' = [busy EXCEPT ![md[self]] = FALSE]
            /\ pc' = [pc EXCEPT ![self] = "Done"]
            /\ UNCHANGED << st, users, dset, regLive, waiting, wres, mb, kfd, 
                            regs, closes, wrongClose, badUse, yDeleted, 
                            regsAtClose, me, ds, md >>

dm(self) == m0(self) \/ m1(self) \/ m1a(self) \/ m2(self) \/ m3(self)
               \/ m4(self) \/ m5(self)

n1 == /\ pc["N"] = "n1"
      /\ kfd = "none"
      /\ kfd' = "Y"
      /\ pc' = [pc EXCEPT !["N"] = "n2"]
      /\ UNCHANGED << st, users, dset, regLive, waiting, wres, mb, regs, 
                      closes, wrongClose, badUse, yDeleted, regsAtClose, busy, 
                      me, ds, md >>

n2 == /\ pc["N"] = "n2"
      /\ regs' = (regs \cup {<<"A", "Y">>})
      /\ pc' = [pc EXCEPT !["N"] = "Done"]
      /\ UNCHANGED << st, users, dset, regLive, waiting, wres, mb, kfd, closes, 
                      wrongClose, badUse, yDeleted, regsAtClose, busy, me, ds, 
                      md >>

nw == n1 \/ n2

(* Allow infinite stuttering to prevent deadlock on termination. *)
Terminating == /\ \A self \in ProcSet: pc[self] = "Done"
               /\ UNCHANGED vars

Next == edge \/ cl \/ late \/ nw
           \/ (\E self \in {"WA", "WB"}: w(self))
           \/ (\E self \in {"DA", "DB"}: dm(self))
           \/ Terminating

Spec == /\ Init /\ [][Next]_vars
        /\ \A self \in {"WA", "WB"} : WF_vars(w(self))
        /\ WF_vars(edge)
        /\ WF_vars(cl)
        /\ WF_vars(late)
        /\ \A self \in {"DA", "DB"} : WF_vars(dm(self))
        /\ WF_vars(nw)

Termination == <>(\A self \in ProcSet: pc[self] = "Done")

\* END TRANSLATION

-----------------------------------------------------------------------------
CloseOnce == closes <= 1
NoWrongClose == ~wrongClose
NoUseAfterClose == ~badUse
NoWrongDel == ~yDeleted
UsersOK == users >= 0
\* close(2) comes after each EPOLL_CTL_DEL of X.
DelBeforeClose == closes > 0 => regsAtClose = {}
\* Termination (from the translator): each process ends. Thus each waiter
\* gets data or EBADF, close(2) occurs, and the new socket can come.
=============================================================================
