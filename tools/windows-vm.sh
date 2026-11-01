#!/usr/bin/env bash
# Run a local Windows Server 2025 VM to build and test weft on Windows.
#
# Usage:
#   tools/windows-vm.sh install
#   tools/windows-vm.sh opam
#   tools/windows-vm.sh start
#   tools/windows-vm.sh stop
#   tools/windows-vm.sh ssh [command]
#   tools/windows-vm.sh sync <dir>
#   tools/windows-vm.sh test <dir> [dune args]
#   tools/windows-vm.sh screen
#
# The VM directory (WEFT_WINDOWS_VM) contains disk.qcow2, password and
# id_ed25519. The install command makes them from server2025-eval.iso. The
# opam command installs OCaml and the test tools in the VM. The README tells
# how to get the ISO.
set -eu

vm=${WEFT_WINDOWS_VM:-$HOME/.local/share/weft-windows-vm}
port=${WEFT_WINDOWS_VM_PORT:-2224}
pidfile=$vm/qemu.pid
here=$(cd "$(dirname "$0")" && pwd)

# The SSH options do not use the SSH configuration of the user.
ssh_opts=(-F /dev/null -i "$vm/id_ed25519" -o IdentitiesOnly=yes
  -o UserKnownHostsFile="$vm/known_hosts" -o StrictHostKeyChecking=accept-new
  -o ConnectTimeout=5 -o LogLevel=ERROR)

# The shell of the VM is cmd.
vm_ssh() { ssh "${ssh_opts[@]}" -p "$port" Administrator@127.0.0.1 "$@"; }

# Runs a PowerShell script in the VM. The encoded command prevents the
# quoting rules of cmd. With no progress records, PowerShell writes no CLIXML
# to stderr.
# shellcheck disable=SC2016
vm_ps() {
  vm_ssh "powershell -NoProfile -NonInteractive -EncodedCommand \
$(printf '$ProgressPreference = "SilentlyContinue"\n%s' "$1" | iconv -t UTF-16LE | base64 -w0)"
}

running() { [ -f "$pidfile" ] && kill -0 "$(cat "$pidfile")" 2>/dev/null; }

# Starts QEMU with the disk and the extra arguments. The disk is on the AHCI
# controller of q35, and the network card is an e1000e: Windows has drivers
# for the two. The monitor socket serves the screen command.
qemu() {
  nix shell nixpkgs#qemu -c qemu-system-x86_64 \
    -machine q35,accel=kvm \
    -cpu host,hv_relaxed,hv_spinlocks=0x1fff,hv_vapic,hv_time,hv_vpindex,hv_synic,hv_stimer \
    -smp 4 -m 6G -rtc base=utc \
    -drive file="$vm/disk.qcow2",if=ide \
    -netdev user,id=n0,hostfwd=tcp:127.0.0.1:"$port"-:22 \
    -device e1000e,netdev=n0 \
    -display none -monitor unix:"$vm/monitor.sock",server,nowait \
    -daemonize -pidfile "$pidfile" "$@"
}

# Waits for SSH, for $1 tries of 5 s.
wait_ssh() {
  for _ in $(seq "$1"); do
    if vm_ssh 'exit 0' 2>/dev/null; then return 0; fi
    sleep 5
  done
  echo "windows-vm: SSH did not come on. Use the screen command." >&2
  exit 1
}

# Installs Windows on a new disk. The setup takes about 5 minutes, and it
# needs no input.
install() {
  iso=$vm/server2025-eval.iso
  [ -f "$iso" ] || { echo "windows-vm: $iso is missing." >&2; exit 1; }
  [ -e "$vm/disk.qcow2" ] && { echo "windows-vm: $vm/disk.qcow2 exists." >&2; exit 1; }
  running && { echo "windows-vm: the VM runs." >&2; exit 1; }
  [ -f "$vm/id_ed25519" ] || ssh-keygen -q -t ed25519 -N '' -C weft-windows-vm -f "$vm/id_ed25519"
  if [ ! -f "$vm/password" ]; then
    # Windows wants uppercase and lowercase letters, digits and a symbol.
    (umask 077 && printf 'Weft-%s-9z' "$(openssl rand -hex 12)" >"$vm/password")
  fi
  rm -f "$vm/known_hosts"
  dir=$(mktemp -d)
  sed "s/@PASSWORD@/$(cat "$vm/password")/" "$here/windows-vm/autounattend.xml" >"$dir/autounattend.xml"
  cp "$here/windows-vm/setup.ps1" "$dir/"
  cp "$vm/id_ed25519.pub" "$dir/authorized_keys"
  nix shell nixpkgs#xorriso -c xorriso -as mkisofs -quiet -J -R -V UNATTEND \
    -o "$vm/unattend.iso" "$dir"
  rm -rf "$dir"
  nix shell nixpkgs#qemu -c qemu-img create -q -f qcow2 "$vm/disk.qcow2" 60G
  qemu -drive file="$iso",media=cdrom -drive file="$vm/unattend.iso",media=cdrom \
    -boot once=d
  wait_ssh 360
  # The disc has the password.
  stop
  rm -f "$vm/unattend.iso"
}

# Runs windows-vm/opam.ps1 in the VM. The file goes with scp, because cmd
# accepts at most 8191 characters in a command. Then sshd starts again, and
# thus the next SSH session gets the new PATH.
opam() {
  scp "${ssh_opts[@]}" -P "$port" -q "$here/windows-vm/opam.ps1" \
    Administrator@127.0.0.1:weft-opam.ps1
  vm_ssh 'powershell -NoProfile -NonInteractive -ExecutionPolicy Bypass -File weft-opam.ps1 && del weft-opam.ps1'
  vm_ssh 'powershell -NoProfile -Command Restart-Service sshd' || true
  wait_ssh 12
}

start() {
  running || qemu
  wait_ssh 60
}

stop() {
  running || return 0
  vm_ssh 'shutdown /s /t 0' || true
  pid=$(cat "$pidfile")
  for _ in $(seq 60); do
    kill -0 "$pid" 2>/dev/null || { rm -f "$pidfile"; return 0; }
    sleep 2
  done
  echo "windows-vm: the VM did not stop. Kill PID $pid." >&2
  exit 1
}

# Writes the screen of the VM to $vm/screen.png.
screen() {
  echo "screendump $vm/screen.png -f png" | socat - UNIX-CONNECT:"$vm/monitor.sock" >/dev/null
  echo "$vm/screen.png"
}

# Copy a worktree to C:\Users\Administrator\<name of the worktree> in the VM.
# Windows has no rsync. Thus the copy first deletes all of the old copy
# except _build, then it extracts a tar file of the worktree.
sync() {
  dir=$(cd "${1:?give a directory}" && pwd)
  name=$(basename "$dir")
  vm_ps "\$d = '$name'
if (Test-Path \$d) {
  Get-ChildItem -Force \$d | Where-Object Name -ne '_build' | Remove-Item -Recurse -Force
} else { New-Item -ItemType Directory \$d | Out-Null }"
  tmp=$(mktemp)
  tar -C "$dir" --exclude=./_build --exclude=./.git -cf "$tmp" .
  scp "${ssh_opts[@]}" -P "$port" -q "$tmp" Administrator@127.0.0.1:weft-sync.tar
  rm -f "$tmp"
  vm_ssh "tar -xf weft-sync.tar -C $name && del weft-sync.tar"
}

run_test() {
  dir=$(cd "${1:?give a directory}" && pwd)
  shift
  sync "$dir"
  if [ $# -eq 0 ]; then
    cmd='opam exec -- dune build && opam exec -- dune runtest'
  else
    cmd="opam exec -- dune $*"
  fi
  # The environment agrees with the CI.
  vm_ssh "cd $(basename "$dir") && set \"WINDTRAP_VERBOSE=1\" && \
set \"WINDTRAP_TIMEOUT=120\" && $cmd"
}

case ${1:-} in
  install) install ;;
  opam) opam ;;
  start) start ;;
  stop) stop ;;
  ssh) shift; vm_ssh "$@" ;;
  sync) shift; sync "$@" ;;
  test) shift; run_test "$@" ;;
  screen) screen ;;
  *) sed -n '2,12s/^# \{0,1\}//p' "$0" >&2; exit 2 ;;
esac
