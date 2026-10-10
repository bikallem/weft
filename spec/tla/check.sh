#!/usr/bin/env bash
# Model-check the TLA+ models of the design with TLC.
# Run it in the dev shell of the repository:  nix develop -c spec/tla/check.sh
#
#   Commit.tla  the commit protocol of section 2.4
#   Scope.tla   the scopes and the cancellation of section 2.5
#   Ivar.tla    the atomic register of an Ivar, and fill_from_outside
#   Wake.tla    the loop of a domain: idle and wake, steal, next slot, pinned
#   Fd.tla      the fd record: Rcfd, the registrations, the close notices
#
# MODELS selects the models, for example: MODELS="Ivar Scope" check.sh
#
# Each line runs one case. A "pass" case uses the protocol of the design.
# A "fail" case plants one bug, and TLC must find a violation or a deadlock.
# The script gives exit status 1 if a result is not the expected one.
set -u
cd "$(dirname "$0")"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
status=0
n=0
MODELS=${MODELS:-"Commit Scope Ivar Wake Fd"}
want() { case " $MODELS " in *" $1 "*) return 0 ;; *) return 1 ;; esac; }

# run MODULE EXPECT "INVARIANTS" NAME=VALUE...
run() {
  local module=$1 expect=$2 invariants=$3
  shift 3
  n=$((n + 1))
  local cfg="$work/case$n.cfg"
  {
    echo "SPECIFICATION Spec"
    echo "CONSTANTS"
    for c in "$@"; do echo "  ${c%%=*} = ${c#*=}"; done
    echo "INVARIANTS $invariants"
    echo "PROPERTIES ${PROPS:-Termination}"
    echo "CHECK_DEADLOCK ${DEADLOCK:-TRUE}"
  } > "$cfg"
  local out="$work/out$n.txt"
  tlc -workers auto -metadir "$work/states$n" -config "$cfg" "$module.tla" > "$out" 2>&1
  local result=fail
  grep -q "No error has been found" "$out" && result=pass
  local why states
  why=$(grep -m1 -E "^Error: (Invariant [A-Za-z]+ is violated|Deadlock reached|Temporal properties were violated)" "$out" | sed 's/^Error: //')
  states=$(grep -oE "[0-9]+ distinct states found" "$out" | tail -1)
  local bugs
  bugs=$(printf '%s\n' "$@" | grep '=TRUE' | sed 's/=TRUE//' | paste -sd, -)
  local scen
  scen=$(printf '%s\n' "$@" | grep '^Scenario=' | sed 's/^Scenario="\(.*\)"$/\1/')
  printf '%-6s %-6s %-6s %-20s -> %s %s (%s)\n' "$module" "$expect" "$scen" "${bugs:-no bug}" \
    "$result" "${why:-}" "${states:-?}"
  if [ "$result" != "$expect" ]; then
    status=1
    echo "---- unexpected result, the end of the TLC output:"; tail -40 "$out"
  fi
}

commit() { want Commit || return 0 # expect scenario noParked noIdOrder dropOnClaimed noLock
  run Commit "$1" \
    "TypeOK AtMostOneResume DoneHasResult RendezvousPairs NoLostRendezvous EndState" \
    "Scenario=\"$2\"" BugNoParked="$3" BugNoIdOrder="$4" BugDropOnClaimed="$5" BugNoLock="$6"
}
#      expect scenario noParked noIdOrder drop  noLock
commit pass   cross    FALSE    FALSE     FALSE FALSE
commit pass   stale    FALSE    FALSE     FALSE FALSE
commit pass   self     FALSE    FALSE     FALSE FALSE
commit fail   cross    TRUE     FALSE     FALSE FALSE
commit fail   cross    FALSE    TRUE      FALSE FALSE
commit fail   stale    FALSE    FALSE     TRUE  FALSE
commit fail   cross    FALSE    FALSE     FALSE TRUE

scope() { want Scope || return 0 # expect scenario joinCancellable shieldNotNode noRecheck noChainCheck exnAfterEnd noFlagCheck
  run Scope "$1" \
    "NoOrphan ExnNotLost ExnSeen ShieldHolds TimeoutOK NoForeignCancel ClosedEmpty" \
    "Scenario=\"$2\"" BugJoinCancellable="$3" BugShieldNotNode="$4" BugNoRecheck="$5" \
    BugNoChainCheck="$6" BugExnAfterEnd="$7" BugNoFlagCheck="$8" \
    defaultInitValue=defaultInitValue
}
#     expect scenario join  shield recheck chain exn   flag
scope pass   error    FALSE FALSE  FALSE   FALSE FALSE FALSE
scope pass   shield   FALSE FALSE  FALSE   FALSE FALSE FALSE
scope pass   fork     FALSE FALSE  FALSE   FALSE FALSE FALSE
scope fail   error    TRUE  FALSE  FALSE   FALSE FALSE FALSE
scope fail   shield   FALSE TRUE   FALSE   FALSE FALSE FALSE
scope pass   shield   FALSE FALSE  TRUE    FALSE FALSE FALSE
scope pass   fork     FALSE FALSE  TRUE    FALSE FALSE FALSE
scope fail   error    FALSE FALSE  FALSE   TRUE  FALSE FALSE
scope fail   error    FALSE FALSE  FALSE   FALSE TRUE  FALSE
scope fail   fork     FALSE FALSE  FALSE   FALSE FALSE TRUE

ivar() { want Ivar || return 0 # expect directFill nonAtomicRegister
  run Ivar "$1" "AtMostOneResume FullNoWaiters OneFill EndState" \
    BugDirectFill="$2" BugNonAtomicRegister="$3" defaultInitValue=defaultInitValue
}
#    expect direct nonAtomic
ivar pass   FALSE  FALSE
ivar fail   TRUE   FALSE
ivar fail   FALSE  TRUE

# The domains loop forever, so a sleep at the end is not a deadlock. The
# liveness property is AllRun: each item runs.
wake() { want Wake || return 0 # expect decAfterCheck noRecheck pinnedGeneric noNextSteal
  PROPS=AllRun DEADLOCK=FALSE run Wake "$1" "PinnedOnZero SearchingOK NoLostWakeup" \
    BugDecAfterCheck="$2" BugNoRecheck="$3" BugPinnedGeneric="$4" BugNoNextSteal="$5"
}
#    expect decAfter noRecheck pinned noNext
wake pass   FALSE    FALSE     FALSE  FALSE
wake fail   TRUE     FALSE     FALSE  FALSE
wake fail   FALSE    TRUE      FALSE  FALSE
wake fail   FALSE    FALSE     TRUE   FALSE
wake pass   FALSE    FALSE     FALSE  TRUE

fd() { want Fd || return 0 # expect dupAlive countOnly noBitCas registerOrder regNoUser
  run Fd "$1" "CloseOnce NoWrongClose NoUseAfterClose NoWrongDel UsersOK DelBeforeClose" \
    DupAlive="$2" BugCountOnly="$3" BugNoBitCas="$4" BugRegisterOrder="$5" BugRegNoUser="$6"
}
#  expect dup   count bitCas regOrder noUser
fd pass   FALSE FALSE FALSE  FALSE    FALSE
fd pass   TRUE  FALSE FALSE  FALSE    FALSE
fd fail   TRUE  TRUE  FALSE  FALSE    FALSE
fd pass   TRUE  FALSE TRUE   FALSE    FALSE
fd fail   TRUE  FALSE FALSE  TRUE     FALSE
fd fail   TRUE  FALSE FALSE  FALSE    TRUE
exit $status
