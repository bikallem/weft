#!/usr/bin/env bash
# Model-check the commit protocol (Commit.tla) with TLC.
# Run it in the dev shell of the repository:  nix develop -c spec/tla/check.sh
#
# Each line runs one scenario. "pass" cases use the protocol of the design.
# "fail" cases plant one bug, and TLC must find a violation or a deadlock.
set -u
cd "$(dirname "$0")"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
status=0

run() { # expect scenario noParked noIdOrder dropOnClaimed noLock
  local expect=$1 scen=$2
  local cfg="$work/$scen-$3$4$5$6.cfg"
  cat > "$cfg" <<CFG
SPECIFICATION Spec
CONSTANTS
  Scenario = "$scen"
  BugNoParked = $3
  BugNoIdOrder = $4
  BugDropOnClaimed = $5
  BugNoLock = $6
INVARIANTS
  TypeOK AtMostOneResume DoneHasResult RendezvousPairs NoLostRendezvous EndState
PROPERTIES
  Termination
CHECK_DEADLOCK TRUE
CFG
  local out="$work/out.txt"
  tlc -workers auto -metadir "$work/states" -config "$cfg" Commit.tla > "$out" 2>&1
  local rc=$?
  local result
  if grep -q "No error has been found" "$out"; then result=pass; else result=fail; fi
  local why
  why=$(grep -m1 -E "^Error: (Invariant [A-Za-z]+ is violated|Deadlock reached|Temporal properties were violated)" "$out" | sed 's/^Error: //')
  local states
  states=$(grep -oE "[0-9]+ distinct states found" "$out" | tail -1)
  printf '%-6s %-6s noParked=%-5s noIdOrder=%-5s drop=%-5s noLock=%-5s -> %s %s %s\n' \
    "$expect" "$scen" "$3" "$4" "$5" "$6" "$result" "${why:-}" "(${states:-?})"
  if [ "$result" != "$expect" ]; then
    status=1
    echo "---- unexpected result, TLC output (rc=$rc):"; tail -40 "$out"
  fi
}

#   expect scenario  noParked noIdOrder drop  noLock
run pass   cross     FALSE    FALSE     FALSE FALSE
run pass   stale     FALSE    FALSE     FALSE FALSE
run pass   self      FALSE    FALSE     FALSE FALSE
run fail   cross     TRUE     FALSE     FALSE FALSE
run fail   cross     FALSE    TRUE      FALSE FALSE
run fail   stale     FALSE    FALSE     TRUE  FALSE
run fail   cross     FALSE    FALSE     FALSE TRUE
exit $status
