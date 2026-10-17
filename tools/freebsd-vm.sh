#!/usr/bin/env bash
# Run a local FreeBSD 15.1 VM to build and test the kqueue world.
#
# Usage:
#   tools/freebsd-vm.sh start
#   tools/freebsd-vm.sh stop
#   tools/freebsd-vm.sh ssh [command]
#   tools/freebsd-vm.sh sync <dir>
#   tools/freebsd-vm.sh test <dir> [dune args]
#
# The VM directory (WEFT_FREEBSD_VM) contains disk.qcow2, seed.iso and
# id_ed25519. The README tells how to make these files.
set -eu

vm=${WEFT_FREEBSD_VM:-$HOME/.local/share/weft-freebsd-vm}
port=${WEFT_FREEBSD_VM_PORT:-2223}
pidfile=$vm/qemu.pid

# The SSH options do not use the SSH configuration of the user.
ssh_opts=(-F /dev/null -i "$vm/id_ed25519" -p "$port" -o IdentitiesOnly=yes
  -o UserKnownHostsFile="$vm/known_hosts" -o StrictHostKeyChecking=accept-new
  -o ConnectTimeout=5 -o LogLevel=ERROR)

vm_ssh() { ssh "${ssh_opts[@]}" root@127.0.0.1 "$@"; }

running() { [ -f "$pidfile" ] && kill -0 "$(cat "$pidfile")" 2>/dev/null; }

start() {
  if ! running; then
    # The guest network is a QEMU user network. Only 127.0.0.1 gets the
    # SSH forward.
    nix shell nixpkgs#qemu -c qemu-system-x86_64 \
      -machine q35,accel=kvm -cpu host -smp 4 -m 6G \
      -drive file="$vm/disk.qcow2",if=virtio \
      -drive file="$vm/seed.iso",media=cdrom \
      -netdev user,id=n0,hostfwd=tcp:127.0.0.1:"$port"-:22 \
      -device virtio-net-pci,netdev=n0 \
      -display none -serial file:"$vm/console.log" \
      -daemonize -pidfile "$pidfile"
  fi
  for _ in $(seq 120); do
    if vm_ssh true 2>/dev/null; then return 0; fi
    sleep 2
  done
  echo "freebsd-vm: SSH did not come on. Examine $vm/console.log." >&2
  exit 1
}

stop() {
  running || return 0
  vm_ssh 'shutdown -p now' || true
  pid=$(cat "$pidfile")
  for _ in $(seq 60); do
    kill -0 "$pid" 2>/dev/null || { rm -f "$pidfile"; return 0; }
    sleep 2
  done
  echo "freebsd-vm: the VM did not stop. Kill PID $pid." >&2
  exit 1
}

# Copy a worktree to /root/<name of the worktree> in the VM. The copy keeps
# the _build directory of the VM.
sync() {
  dir=$(cd "${1:?give a directory}" && pwd)
  rsync -a --delete --exclude _build --exclude .git \
    -e "ssh ${ssh_opts[*]}" "$dir/" root@127.0.0.1:"$(basename "$dir")/"
}

run_test() {
  dir=$(cd "${1:?give a directory}" && pwd)
  shift
  sync "$dir"
  if [ $# -eq 0 ]; then
    cmd='dune build && dune runtest test/kqueue'
  else
    cmd="dune $(printf '%q ' "$@")"
  fi
  # The environment agrees with the freebsd job of the CI.
  vm_ssh "cd $(basename "$dir") && export OPAMROOTISOK=1 \
WINDTRAP_VERBOSE=1 WINDTRAP_TIMEOUT=120 && eval \"\$(opam env)\" && $cmd"
}

case ${1:-} in
  start) start ;;
  stop) stop ;;
  ssh) shift; vm_ssh "$@" ;;
  sync) shift; sync "$@" ;;
  test) shift; run_test "$@" ;;
  *) sed -n '2,10s/^# \{0,1\}//p' "$0" >&2; exit 2 ;;
esac
