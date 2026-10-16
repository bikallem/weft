# weft

Weft is a concurrency library for OCaml 5. It runs fibers on OCaml domains,
and its events come from Concurrent ML. Each operating system call is an
effect. Thus the same program runs on Linux or in a deterministic simulator,
with a virtual clock and an in-memory network.

**Status:** pre-release (version 0.0.0). The API can change. The milestones
M0 to M3 of the design are complete on Linux and in the simulator. The kqueue
world (macOS and FreeBSD) runs the main portable tests in CI. Some of its
facts are not verified yet (design section 2.7).

## Features

- **Fibers and scopes.** `Fiber.scope` waits for all of its fibers. An error
  in a fiber cancels the other fibers of its scope. `Fiber.with_timeout`,
  `Fiber.shield` and promises are also available. Pinned fibers stay on
  domain 0.
- **Events from Concurrent ML.** `sync`, `choose`, `wrap`, `guard` and
  `with_nack` work on channels, Ivars, timers and sockets.
- **Many domains.** A domain steals work from the others, as in the Go
  scheduler. `run ~domains:n` selects the number of domains.
- **Locks for fiber code.** `Fiber.Mutex`, `Fiber.Condition` and
  `Fiber.Semaphore` suspend the fiber, not the domain.
- **I/O.** TCP, UDP and Unix sockets, pipes, files, directories, DNS, child
  processes and signals. A C thread pool runs the operations that can block.
- **A watchdog.** In the Linux and kqueue worlds, `run ~watchdog` writes a
  warning to `stderr` when a fiber runs for more than the limit with no
  switch. The default limit is 10 ms.
- **A simulator.** `Weft_sim.run ~seed` runs the program with a virtual clock,
  a network with latency and faults, a file system in memory, and fake
  processes. A seed always gives the same order of events. The simulator finds
  a deadlock, a lost wakeup and a livelock, and tells which fiber waits.

## An example

This echo server uses only the API of weft
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

let serve l =
  Fiber.scope (fun s ->
      while true do
        let c, _ = Net.accept l in
        Fiber.fork_isolated s
          ~on_error:(fun e _ -> prerr_endline ("echo: " ^ Printexc.to_string e))
          (fun () -> handle c)
      done )
```

On Linux ([`examples/echo/echo_server.ml`](examples/echo/echo_server.ml)):

```ocaml
let () =
  Weft_linux.run (fun () -> Echo.serve (Net.listen (Net.Tcp ("127.0.0.1", 7000))))
```

On macOS or FreeBSD, use `Weft_kqueue.run` in place of `Weft_linux.run`.

In the simulator ([`examples/echo/test_echo.ml`](examples/echo/test_echo.ml)),
the same server runs for 100 seeds on 2 virtual domains. The idle timeout of
10 s takes no real time, because the virtual clock moves to the next deadline:

```ocaml
Weft_sim.run ~seed ~domains:2 (fun sim ->
    Weft_sim.set_latency sim (Span.ms 5) ;
    let a = Weft_sim.addr sim "server:7" in
    let l = Net.listen a in
    Fiber.scope (fun s ->
        Fiber.fork_daemon s (fun () -> Echo.serve l) ;
        assert (Echo.ask a "hello" = "hello") ) )
```

## Requirements

- OCaml 5.5 or later. The CI and the tests use only OCaml 5.5, so the package
  does not claim an older version.
- For the Linux world: Linux 5.4 or later (pidfd), and glibc 2.39 or later
  (`pidfd_spawnp`).
- For the kqueue world: macOS or FreeBSD. The CI tests macOS 15 and
  FreeBSD 14.3. OpenBSD and NetBSD are not tested.
- The core and the simulator need no OS calls. They build on each system that
  OCaml 5 supports, and they do not link `unix`.

## Build and test

The development tools come from the Nix flake:

```sh
nix develop -c dune build
nix develop -c dune runtest
nix develop -c spec/tla/check.sh     # the TLA+ models of the design
```

The libraries are `weft` (the core), `weft.sim` (the simulator),
`weft.linux` (the Linux world), `weft.kqueue` (the world for macOS and
FreeBSD) and `weft.posix` (the parts that the two worlds share).

The main test suites are in `test`:

- `test/core` and `test/sim`: the core and the simulator.
- `test/portable`: the tests that run on each world (the simulator, Linux and
  kqueue).
- `test/linux` and `test/linux_md`: the Linux world. `test/linux_md` has the
  tests of many domains.
- `test/kqueue`: the kqueue world. It runs only on macOS and FreeBSD.

### Mutation testing

The tests use a fork of [windtrap](https://github.com/bikallem/windtrap), and
the flake gives it. Mutation testing changes the code of the libraries, one
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

On a Linux host with KVM, `tools/freebsd-vm.sh` runs a FreeBSD 14.3 VM in
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

1. Get `FreeBSD-14.3-RELEASE-amd64-BASIC-CLOUDINIT-ufs.qcow2.xz` from the
   FreeBSD archive. Do a check of its SHA256.
2. Decompress it to `disk.qcow2`, and resize it to 30 GB with `qemu-img`.
3. Make an SSH key `id_ed25519` in the VM directory.
4. Make `seed.iso` (volume label `cidata`) with a `meta-data` file and an
   executable `user-data` script that adds the key to
   `/root/.ssh/authorized_keys`.
5. Run `tools/freebsd-vm.sh start`. Then, in the VM, do the steps of the
   `freebsd` job of `.github/workflows/ci.yml`, and `pkg install -y rsync`.

## Use weft from another flake

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

[`docs/design.md`](docs/design.md) gives the design, the measurements and the
reviews of each milestone. The TLA+ models of the shared protocols are in
[`spec/tla`](spec/tla).

## License

ISC.
