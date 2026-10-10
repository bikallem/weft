#!/usr/bin/env bash
# Model-check the TLA+ models of the design with TLC.
# Run it in the dev shell of the repository:  nix develop -c spec/tla/check.sh
#
#   Commit.tla  the commit protocol of section 2.4
#   Scope.tla   the scopes and the cancellation of section 2.5
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
    echo "PROPERTIES Termination"
    echo "CHECK_DEADLOCK TRUE"
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

commit() { [ -n "${ONLY_SCOPE:-}" ] && return 0 # expect scenario noParked noIdOrder dropOnClaimed noLock
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

scope() { # expect scenario joinCancellable shieldNotNode noRecheck noChainCheck exnAfterEnd noFlagCheck
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
exit $status
