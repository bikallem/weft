# weft

Weft is a structured concurrency library for OCaml 5. You write direct-style
code with fibers, and weft runs the fibers on all of the domains of the
machine. Each fiber is in a scope, and a scope waits for all of its fibers. The
events of weft come from Concurrent ML, so you can combine a receive, a
timeout and a cancel into one choice. Each call to the operating system is an
effect. Thus the same program runs on Linux, macOS, FreeBSD and Windows, and
in a deterministic simulator with a virtual clock and an in-memory network.

```ocaml
open Weft

let () =
  Weft.run (module Weft_os) (fun () ->
      Fiber.scope (fun s ->
          Fiber.fork s (fun () ->
              Time.sleep Mono (Span.ms 100) ;
              print_endline "world" ) ;
          print_endline "hello" ) )
```

**Status:** pre-release (version 0.0.0). The API can change. Linux is the main
platform. The CI tests macOS, FreeBSD and Windows too.

## Why weft

- **Direct style.** There are no monads and no callbacks. A read is
  `Net.read c buf 0 n`, and a fiber that waits does not block its domain.
- **Structured concurrency.** Each fiber is in a scope. A scope waits for all
  of its fibers. If a fiber fails, the scope cancels the other fibers and
  gives the error to the caller. No fiber stays alive after its scope ends.
- **Events that you can combine.** `choose`, `wrap`, `guard` and `with_nack`
  from Concurrent ML work on channels, timers, Ivars, variables, sockets,
  signals and child processes. You do not need a special `select` for each
  type of source.
- **All cores.** One call starts a scheduler on each domain. An idle domain
  takes work from a busy domain.
- **Deterministic tests of the real code.** The simulator runs your program,
  not a model of it. A seed gives the same order of events each time. A test
  with a timeout of 10 s takes no real time. When a test fails, run the same
  seed again to get the same failure.
- **No context value to carry.** Fiber code uses the clock, the network and
  the files directly. You do not give an environment or a capability to each
  function. The world (the OS or the simulator) handles the effects.
- **A small core.** The core library has no C code and does not link `unix`.
  The worlds of the OS have their own C stubs.
- **Strict checks of the library.** TLA+ models check the shared protocols,
  and `dscheck` checks the lock-free code. Mutation tests make sure that the
  tests find each change to the code.

## When weft is not a good fit

- You need a stable API now. Weft is pre-release.
- You need a large set of protocol libraries, for example HTTP or TLS. Weft
  gives sockets, files, DNS and processes, but not these protocols.
- You use OCaml 4 or an OCaml 5 version older than 5.5.

## Features

- **Fibers:** `Fiber.scope`, `fork`, `fork_daemon`, `fork_isolated` (a server
  connection that fails does not stop the server), promises,
  `Fiber.with_timeout`, `Fiber.shield` and `Fiber.cancel`. A pinned fiber
  stays on domain 0, for example for a C library that is not thread-safe.
- **Events:** `sync`, `choose`, `wrap`, `guard`, `with_nack`, `always` and
  `never`, on `Chan`, `Ivar`, `Var`, `Time`, `Net`, `Signal` and `Process`.
- **Synchronization:** channels, Ivars, variables with change events, and
  `Fiber.Mutex`, `Fiber.Condition` and `Fiber.Semaphore`. A lock suspends the
  fiber, not the domain.
- **Time:** a monotonic clock and a wall clock. A wall-clock timer follows a
  step of the system clock. Near deadlines share one alarm.
- **Network:** TCP, UDP and Unix sockets, `Dns.resolve` and `Dns.reverse`.
- **Files and processes:** files, directories, pipes, child processes with
  pipes, and signals as events. A pool of C threads runs the calls that can
  block, so they do not stop a domain.
- **Production worlds:** epoll on Linux, kqueue on macOS and FreeBSD, and I/O
  completion ports on Windows. Each domain has its own poller.
- **A watchdog:** it writes a warning to `stderr` when a fiber runs for more
  than 10 ms with no switch.
- **A simulator:** a virtual clock, virtual domains, a network with latency,
  jitter, short reads, packet loss and resets, a file system in memory, and
  fake processes. It finds a deadlock, a lost wakeup and a livelock, and it
  names the fibers that wait.

## Examples

The [`examples`](examples) directory has an example for each major function.
Each example has its code in a library that uses only the API of weft, a
program for the OS, and a test of the same code in the simulator.
`dune runtest` runs the tests.

| Example | What it shows | Program |
|---|---|---|
| [`echo`](examples/echo) | A TCP server, `fork_isolated`, `Fiber.with_timeout` | `echo_server.exe [port]` |
| [`parallel`](examples/parallel) | A parallel map on all domains, a race of promises, a cancel that stops the siblings, a cleanup in `Fiber.shield` | `parallel_main.exe [n]` |
| [`pool`](examples/pool) | Workers on a `Chan`, an RPC event with `guard` and `with_nack`, a rate limit with `Fiber.Semaphore` | `pool_main.exe` |
| [`happy`](examples/happy) | `Dns.resolve`, a race of connects (RFC 8305) with `choose`, `Time.after` and `Promise.cancel` | `happy_main.exe [host] [port]` |
| [`chat`](examples/chat) | A chat server, a mailbox with `Fiber.Mutex` and `Fiber.Condition`, a graceful stop at `SIGINT` | `chat_main.exe [port]` |
| [`reload`](examples/reload) | A configuration in a `Var`, a reload from a `File` at `SIGHUP`, a worker that waits on `Var.changed` | `reload_main.exe [file]` |
| [`du`](examples/du) | A concurrent walk with `Dir` and `File`, a scope for each directory, a limit of the operations | `du_main.exe [dir] [limit]` |
| [`supervisor`](examples/supervisor) | `Process.spawn` with a pipe, a restart with a backoff, a stop at `SIGTERM` | `supervisor_main.exe prog [args]` |
| [`cron`](examples/cron) | A daily job on the wall clock with `Time.at Posix`, `Time.to_utc`, the steps of the clock | `cron_main.exe HH:MM` |
| [`sim_tour`](examples/sim_tour) | The simulator: a deadlock report, network faults over 100 seeds, the replay of a seed, the manual clock, UDP with loss | test only |

Run a program with `dune exec`, for example
`dune exec examples/du/du_main.exe -- src`.

### An echo server

The server code uses only the API of weft
([`examples/echo/echo.ml`](examples/echo/echo.ml)):

```ocaml
open Weft

(* Send back what the client sends. A client that sends nothing for 10 s loses
   its connection. *)
let handle c =
  let buf = Bytes.create 4096 in
  let rec loop () =
    match
      Fiber.with_timeout Mono (Span.s 10) (fun () -> Net.read c buf 0 4096)
    with
    | 0 | (exception Fiber.Timeout) ->
        ()
    | n ->
        Net.write c (Bytes.sub_string buf 0 n) ;
        loop ()
  in
  Fun.protect ~finally:(fun () -> Net.close c) loop

(* Each connection is a fiber. A connection that fails does not stop the
   server. *)
let serve l =
  Fiber.scope (fun s ->
      while true do
        let c, _ = Net.accept l in
        Fiber.fork_isolated s
          ~on_error:(fun e _ -> prerr_endline ("echo: " ^ Printexc.to_string e))
          (fun () -> handle c)
      done )
```

Run it on the OS ([`examples/echo/echo_server.ml`](examples/echo/echo_server.ml)):

```ocaml
let () =
  Weft.run (module Weft_os) (fun () ->
      Echo.serve (Net.listen (Net.Tcp ("127.0.0.1", 7000))) )
```

`Weft_os` is the world of the OS that the build runs on: epoll on Linux,
kqueue on macOS and FreeBSD, and I/O completion ports on Windows. A program
for one OS can give its world directly, for example
`Weft.run (module Weft_linux) main`. `Weft.run` uses up to 4 domains by
default. Set the number with `~domains`.

### A test of the same server in the simulator

This test ([`examples/echo/test_echo.ml`](examples/echo/test_echo.ml)) runs
the server for 100 seeds, on 2 virtual domains, with a latency of 5 ms. The
idle timeout of 10 s takes no real time, because the virtual clock moves to
the next deadline:

```ocaml
let () =
  for seed = 1 to 100 do
    Weft_sim.run ~seed ~domains:2 (fun sim ->
        Weft_sim.set_latency sim (Span.ms 5) ;
        let a = Weft_sim.addr sim "server:7" in
        let l = Net.listen a in
        Fiber.scope (fun s ->
            Fiber.fork_daemon s (fun () -> Echo.serve l) ;
            assert (Echo.ask a "hello" = "hello") ;
            (* A client that sends nothing: the server closes it after 10 s. *)
            let c = Net.connect a in
            let t0 = Time.now Mono in
            assert (Net.read c (Bytes.create 1) 0 1 = 0) ;
            let waited = Span.to_ns (Time.diff Mono (Time.now Mono) t0) in
            assert (waited >= 10_000_000_000L && waited < 10_100_000_000L) ;
            Net.close c ) )
  done
```

### Events: a request or a tick

`choose` waits for the first event that is ready. `wrap` gives each event a
result of the same type:

```ocaml
open Weft

(* Do each request, and write a report each second. *)
let rec loop requests count =
  match
    sync
      (choose
         [ wrap (Chan.recv requests) (fun r -> `Request r)
         ; wrap (Time.after Mono (Span.s 1)) (fun () -> `Tick) ] )
  with
  | `Request r ->
      print_endline r ;
      loop requests (count + 1)
  | `Tick ->
      Printf.printf "%d requests\n%!" count ;
      loop requests 0
```

### Two tasks at the same time

The two functions can run on different domains. If one of them fails, the
scope cancels the other, and `both` raises the error:

```ocaml
open Weft

let both f g =
  let x = ref None in
  let y =
    Fiber.scope (fun s ->
        Fiber.fork s (fun () -> x := Some (f ())) ;
        g () )
  in
  (Option.get !x, y)
```

## Get weft

Weft is not in the opam repository yet. Pin it from GitHub:

```sh
opam pin add weft https://github.com/bikallem/weft.git
```

Then add the libraries to your `dune` file:

```dune
(executable
 (name main)
 (libraries weft weft.os))
```

| Library | Contents |
|---|---|
| `weft` | The core: fibers, events, I/O, and `Weft.run`. |
| `weft.os` | `Weft_os`: the world of the OS that the build runs on. |
| `weft.linux` | `Weft_linux`: the Linux world (epoll). |
| `weft.kqueue` | `Weft_kqueue`: the world of macOS and FreeBSD (kqueue). |
| `weft.windows` | `Weft_windows`: the Windows world (I/O completion ports). |
| `weft.sim` | `Weft_sim`: the simulator. |
| `weft.posix` | The parts that the worlds of the OS share. |

With Nix, the flake gives weft as a package (see
[Use weft from another flake](#use-weft-from-another-flake)).

## Requirements

- OCaml 5.5 or later. The CI tests only OCaml 5.5.
- Linux: Linux 5.4 or later, and glibc 2.39 or later.
- macOS or FreeBSD: the CI tests macOS 15 and FreeBSD 15.1. OpenBSD and NetBSD
  are not tested.
- Windows: OCaml with MinGW-w64, for example from opam 2.2 or later. The CI
  (`windows-latest`) and the development VM have Windows Server 2025. Other
  Windows versions and the MSVC port of OCaml are not tested. A refused connect takes about 2 s,
  because Windows tries it again. `Process.kill` ends a child with the exit
  code 1, and Ctrl-C comes as `SIGINT`.
- The core and the simulator need no OS calls. They build on each system that
  OCaml 5 supports.

## Development

The development tools come from the Nix flake:

```sh
nix develop -c dune build
nix develop -c dune runtest
nix develop -c spec/tla/check.sh     # the TLA+ models of the design
```

The main test suites are in `test`:

- `test/core` and `test/sim`: the core and the simulator.
- `test/portable`: the tests that run on each world (the simulator, Linux,
  kqueue and Windows).
- `test/linux` and `test/linux_md`: the Linux world. `test/linux_md` has the
  tests of many domains.
- `test/kqueue`: the kqueue world. It runs only on macOS and FreeBSD.
- `test/windows`: the Windows world. It runs only on Windows.

### Mutation tests

The tests use a fork of [windtrap](https://github.com/bikallem/windtrap), and
the flake gives it. A mutation test changes the code of the libraries, one
mutant at a time. Then it makes sure that a test gives a failure for each
mutant:

```sh
make mutate         # the mutants on the lines that changed since origin/main
make mutate-all     # all the mutants of the libraries
make help           # the other targets and the variables
```

The CI tests the mutants of the changed lines of each pull request. Section
2.17 of the design gives the rules and the known survivors.

### The kqueue world in a FreeBSD VM

On a Linux host with KVM, `tools/freebsd-vm.sh` runs a FreeBSD 15.1 VM in
QEMU (from nixpkgs), with 4 CPUs and 6 GB of memory:

```sh
tools/freebsd-vm.sh start              # start the VM and wait for SSH
tools/freebsd-vm.sh test .             # dune build and dune runtest test/kqueue
tools/freebsd-vm.sh test . runtest     # or other dune arguments
tools/freebsd-vm.sh ssh                # a shell in the VM
tools/freebsd-vm.sh stop               # shut down the VM
```

The `test` command copies the worktree to the VM with rsync (without `_build`
and `.git`). Then it builds the worktree with the opam switch of the VM. The VM
covers the kqueue world on FreeBSD. It does not cover the macOS paths.

The VM directory is `~/.local/share/weft-freebsd-vm` (or
`$WEFT_FREEBSD_VM`). Make it one time:

1. Get `FreeBSD-15.1-RELEASE-amd64-BASIC-CLOUDINIT-ufs.qcow2.xz` from the
   `VM-IMAGES` directory of download.freebsd.org. Do a check of its SHA256.
2. Decompress it to `disk.qcow2`, and resize it to 30 GB with `qemu-img`.
3. Make an SSH key `id_ed25519` in the VM directory.
4. Make `seed.iso` (volume label `cidata`) with a `meta-data` file and an
   executable `user-data` script that adds the key to
   `/root/.ssh/authorized_keys`.
5. Run `tools/freebsd-vm.sh start`. Then, in the VM, do the steps of the
   `freebsd` job of `.github/workflows/ci.yml`, and `pkg install -y rsync`.

### The Windows world in a Windows Server VM

On a Linux host with KVM, `tools/windows-vm.sh` runs a Windows Server 2025
VM (Server Core, with no desktop) in QEMU, with 4 CPUs and 6 GB of memory.
The VM builds weft with OCaml 5.5 (MinGW-w64), and it runs all of the tests
that build on Windows, also `test/windows`.

```sh
tools/windows-vm.sh start              # start the VM and wait for SSH
tools/windows-vm.sh test .             # dune build and dune runtest
tools/windows-vm.sh test . runtest     # or other dune arguments
tools/windows-vm.sh ssh                # a cmd shell in the VM
tools/windows-vm.sh screen             # a screenshot of the console
tools/windows-vm.sh stop               # shut down the VM
```

The `test` command copies the worktree to the VM with tar and scp (without
`_build` and `.git`). Then it builds the worktree with the opam switch of the
VM.

The VM directory is `~/.local/share/weft-windows-vm` (or
`$WEFT_WINDOWS_VM`). Make it one time:

1. Get the ISO of Windows Server 2025 from the
   [Evaluation Center](https://www.microsoft.com/evalcenter/download-windows-server-2025)
   (64-bit, English). Save it as `server2025-eval.iso` in the VM directory.
2. Run `tools/windows-vm.sh install`. It makes an SSH key and a password,
   installs Windows with no user input (about 5 minutes), and enables SSH.
3. Run `tools/windows-vm.sh start`, then `tools/windows-vm.sh opam`. The
   `opam` command runs `tools/windows-vm/opam.ps1` in the VM. The script
   installs MinGit, opam, OCaml 5.5 and the tools of the CI (about 10
   minutes).

The license of the evaluation ends 180 days after the install. Then delete
`disk.qcow2` and do steps 2 and 3 again.

### Use weft from another flake

The flake gives weft as a package. To build it with the OCaml of your own
scope:

```nix
inputs.weft = {
  url = "github:bikallem/weft";
  inputs.nixpkgs.follows = "nixpkgs";
};

ocamlPackages = pkgs.ocaml-ng.ocamlPackages_5_5.overrideScope (final: prev: {
  weft = final.callPackage weft.lib.package { };
});
```

Then add `ocamlPackages.weft` to your packages. Inside
`with ocamlPackages; [ ... ]`, the name `weft` is the flake input, not the
package. The flake also has `overlays.default`, which adds weft to
`ocaml-ng.ocamlPackages_5_5`, and `packages.<system>.weft`.

## Design

[`docs/design.md`](docs/design.md) gives the design, the decisions and the
measurements. The TLA+ models of the shared protocols are in
[`spec/tla`](spec/tla).

## License

ISC.
