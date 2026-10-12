# weft

Weft is a concurrency library for OCaml 5. It runs fibers on OCaml domains,
and its events come from Concurrent ML. Each operating system call is an
effect. Thus the same program runs on Linux or in a deterministic simulator,
with a virtual clock and an in-memory network.

**Status:** pre-release (version 0.0.0). The API can change. The Linux world
is complete. The kqueue world for macOS and FreeBSD does not exist yet.

## Features

- **Fibers and scopes.** `Fiber.scope` waits for all of its fibers. An error
  in a fiber cancels the other fibers of its scope. `Fiber.with_timeout`,
  `Fiber.shield` and promises are also available.
- **Events from Concurrent ML.** `sync`, `choose`, `wrap`, `guard` and
  `with_nack` work on channels, Ivars, timers and sockets.
- **Many domains.** A domain steals work from the others, as in the Go
  scheduler. `run ~domains:n` selects the number of domains.
- **I/O.** TCP, UDP and Unix sockets, pipes, files, directories, DNS, child
  processes and signals. A C thread pool runs the operations that can block.
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

- OCaml 5.3 or later. The CI tests OCaml 5.5.
- For the Linux world: Linux 5.4 or later (pidfd), and glibc 2.39 or later
  (`pidfd_spawnp`).
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
