# A concurrency library with effects for the outside world

This document has two parts:

- Part 1 is a summary of the caravan session of 2026-10-07 and 2026-10-08. That session compared how async libraries implement timers and waits, and how each library makes time deterministic in tests.
- Part 2 is a design for a new library. The design takes parts from affect, CML and Flix. All calls to the operating system (OS) are effects, and a handler that you plug in decides how to do them.

---

# Part 1: Summary of the session

## 1.1 The problem

Caravan needed exact tests for its timeouts, for example "a request head that is not complete after 10 s gives a 408". A probe with a manual test clock gave these results:

- **The arming race.** The server arms its deadline when its code reads `now`. The test advanced the clock before the server ran. Thus the server armed "now + 10 s" from the advanced time. This occurred in all of the 35 runs.
- **A sleep in the kernel does not see a fake clock.** The domain sleeps in `select` with a timeout that it computed from the clock. An advance of a fake clock does not end that sleep. The 408 came 2 to 4 s late.
- **A yield does not help.** Affect polls the fds only when no call is ready. A test call that yields stays ready, so the server never sees its socket.

The conclusion: a timer is armed by a code event, not by a time event. Only a component that sees all tasks knows when all of them are blocked. That component is the scheduler.

## 1.2 How affect does timers

- `Mtime.wait_for'` reads the clock in its guard and puts the deadline `start + span` in a heap. After this, the timer is only a number in the heap.
- A blocked call is not in a queue. The fd table, the deadline heap or a waiter list holds its `unblock` closure.
- A domain checks the fds and the timers only when no call is ready. Then `progress_to_next_deadline` reads the clock, wakes the expired deadlines and gives the span to the next deadline.
- The last active domain calls `select` with that span as the timeout. When `select` returns, the next unblocker call reads the clock again and wakes the timer.
- The fd unblocker is `select`, and a comment calls it a toy. `select` cannot use fd numbers of 1024 or more.
- Affect has no negative acknowledgements (nack). When the read of a `choose` wins, the deadline of the timer stays in the heap until it expires.
- An effect that you perform in an `Action.guard` or an `Action.map` raises `Effect.Unhandled`, because guards and maps run in the scheduler.

## 1.3 The libraries that we examined

| Library | Real timers | Deterministic test time | How the test knows that all tasks are blocked |
|---|---|---|---|
| CML (SML/NJ) | A list sorted by deadline. A `SIGALRM` tick (20 ms) polls the timers and the fds (timeout 0). | None | — |
| Guile Fibers | A timer wheel (1 ms). `epoll` waits until the next timer, once in each turn. | None | — |
| affect `master` | A deadline heap. `select` waits until the next deadline, only when no call is ready. | None | — |
| eio | A sleep queue in each domain. The `IO` job in the run queue polls. The clock is injected (`now`, `sleep_until`). | `Eio_mock`: a different scheduler, one domain, fake IO | A FIFO yield, or auto-advance when the run queue is empty |
| Go | Runtime timers | `testing/synctest`: a bubble with fake time | `synctest.Wait`, and auto-advance when all goroutines are durably blocked |
| Tokio | A time driver (a wheel) | Paused time, `current_thread` only | Auto-advance after an I/O poll with timeout 0 |
| Trio | A deadline heap. The I/O wait uses the timeout to the next deadline. | `MockClock` with `jump` and auto-jump | `wait_all_tasks_blocked` |
| Kotlin coroutines | A dispatcher with a delay queue | `TestCoroutineScheduler` | `advanceUntilIdle`, `runCurrent` |
| .NET 8 | `TimeProvider.System` | `FakeTimeProvider` | None |
| JavaScript (Sinon, Jest) | libuv: a min-heap and a poll timeout | Fake `setTimeout` and `Date` | `tickAsync` |
| Flix | `Thread.sleep` | The effects `Clock` and `Sleep`. A test gives a different handler. | Not necessary for code on one thread. Effects cannot enter a `spawn`. |
| Linux kernel | `timerfd`, `timer_create`, `io_uring` timeouts | No fake monotonic clock. `libfaketime` fakes only `now`. Simulators (UML time-travel, QEMU `icount`, Shadow) jump to the next timer when idle. | The simulator's kernel |

## 1.4 The lessons

1. **Each system with deterministic time puts it in the runtime.** The runtime knows when all tasks are blocked. It then jumps to the next deadline, one deadline at a time. A large manual jump over chained timers gives incorrect deadlines.
2. **"Blocked" is not enough.** A call can be blocked while its event already occurred, for example data that is already in the socket. The correct state is "quiescent": no call is ready, and a poll with timeout 0 finds nothing. Go uses the term "durably blocked" for a similar state.
3. **A clock reads, and a timer is an event source.** This was your rule. Flix makes the same split with `Clock` and `Sleep`.
4. **Use one alarm for each poller, not one fd for each timer.** A timer is one wait with a deadline, and a program can have thousands of them. The alarm is the one event source that wakes the poller at the earliest deadline. On Linux it is a `timerfd` in the epoll set. On BSD and macOS it is an `EVFILT_TIMER` in the kqueue.
5. **epoll and kqueue remove the limits of `select`, but not fd exhaustion.** They remove the scan of all fds and the `FD_SETSIZE` limit of 1024. Each `timerfd` is still one fd under `RLIMIT_NOFILE`.
6. **A handler intercepts only the code in its scope.** Flix cannot fake a timeout that races a channel, because the wait occurs in another thread. Affect cannot handle effects in guards. Thus the seam must be at the scheduler, where all concurrent waits occur.
7. **A `choose` must tell the losing branches.** Without a nack, a fake timer cannot know that a `choose` dropped it. Guile Fibers removes the timer from the wheel immediately when another operation wins.
8. **There are two time lines.** The monotonic line is for durations and timeouts. The POSIX (wall) line is for calendar points and the `Date` header. One `CLOCK` module type with an abstract `t` covers the two lines, and the type system prevents a mix.

## 1.5 A correction about CML

CML gives timers and I/O the same form at the API level. `timeOutEvt`, `atTimeEvt` and the I/O events are all first-class events, and `choose` races them. But the SML/NJ implementation does not put timers in `select`. It keeps a sorted list, and it checks the list and polls the fds (timeout 0) at each `SIGALRM` tick. Thus a CML timer can fire up to one quantum (20 ms) late.

The design "a timer is an event in the poller" came later in the session. It is the `CLOCK` + `ALARM` design with `timerfd` and `EVFILT_TIMER` (section 1.4, lesson 4). Part 2 keeps the two ideas: CML events at the API, and one alarm in the poller at the OS level.

## 1.6 What caravan did

Caravan added a time seam in its own code, with no change in affect (PR 9, merged):

- `Time.t` holds a clock (`now`), a timer (`at`) and span arithmetic. There are two values, `Time.mono` and `Time.posix`.
- The config, the flow and the WebSocket code carry a `Time.t` value. `Fake_time` gives a fake clock and a fake timer that wakes calls with `Cell.Once`.

This works, but it has costs. Each layer must carry the `Time.t` value. A layer that forgets it uses the real clock, and the compiler cannot see this, because the two time lines have the same type. Code in other libraries, for example a TLS library, bypasses the seam. The sockets are still real, so the tests depend on loopback timing.

---

# Part 2: The design

## 2.1 Goals

- **One seam for the outside world.** The program reads the time, does I/O, waits and gets random numbers only through effects. The library does not use a value that each layer must carry.
- **Plug the handler that you need.** Production uses epoll or kqueue. Tests use a simulator. The program code does not change.
- **Events in the CML form.** A timer, a socket read, a channel message and a process exit are all first-class events. `choose` races them.
- **Timers in the poller.** Production uses one alarm in the poller of each domain, not one fd for each timer.
- **Exact tests.** In the simulator, time moves only when the program is quiescent. The test can make exact negative checks, for example "no response at 9.999 s".

## 2.2 Two layers of effects

The main idea: the effects are in two layers. The seam for the outside world is below the scheduler, not above it.

```text
  fiber code      (guards and wrap functions run here too)
     |  program effects: Sync, Fork
     |  world effects from fiber code: Now, Submit, Random
     v
  scheduler       (each domain: queues, poller registrations, a heap; shared: scopes, sync words)
     |  world effects from the scheduler: Now, Submit, Wait, Wake, Random, Switch
     v
  world handler   one of: Linux (epoll), Bsd (kqueue), Poll, Sim
```

- **Program effects** go from a fiber to the scheduler. The scheduler handles them. They are the same in production and in tests.
- **World effects** go from fiber code or from the scheduler to the world handler. The scheduler does not handle them, so they pass through it. They are the only way to reach the OS.

This order solves lesson 6. The scheduler runs inside the world handler. Thus a world effect from any place in a fiber or in the scheduler reaches the world handler. In affect, an effect in a guard raises `Effect.Unhandled`.

`Fiber.yield` is a program effect of its own. The scheduler puts the fiber at the tail of its queue. When the fiber runs again, it reads the flags of its chain of cancel contexts, so a yield is also a cancellation point (section 2.5). The fiber has no sync while it is in the queue, so a cancel cannot resume it a second time. An earlier version made a yield a `sync` on a special event. The review of M0 showed that this form puts one more field in each sync record for the yield alone.

`Wait` is the only world effect that blocks. It has two forms:

- A poll, with timeout 0. A busy domain does it each `K` fiber switches, and an idle domain does it before it sleeps.
- A sleep, with no timeout argument. The alarm of the domain ends it at the earliest deadline of the domain. A domain does it only when it found no fiber and the poll found nothing.

**Quiescence.** One domain in a sleep is not enough. The program is quiescent only when all domains are in a sleep with nothing ready (section 2.8, The `wait` steps). The simulator sees each sleep, so it knows this state with no test code in the scheduler. In the session, `wait_quiescent` needed a change in the scheduler. In this design, the world handler gets this information from the contract of `Wait`. The other checks of the simulator (section 2.8) read the state of the scheduler through a small private interface, `Sched_debug`. It gives the queues, the scopes and the sync words, and it changes nothing.

## 2.3 The world interface

A world is a module type. A function makes a handler from it. You get a typed contract from the module type, and you get a plug that needs no argument in the program code from the effect.

```ocaml
module type WORLD = sig
  type t

  val now : t -> 'time Clock.line -> 'time
  (** The time on a line ([Mono] or [Posix]). It never blocks. *)

  val submit : t -> 'a Op.t -> ('a, Op.error) result
  (** One attempt of an OS operation, for example read, write, accept or
      connect. It never blocks. It gives [Error Would_block] if the fd is
      not ready. [Register (domain, fd, token)] adds an fd to the
      poller of one domain. *)

  val wait : t -> domain:int -> Wait.t -> Ready.t list
  (** The only blocking call. The domain waits on its own poller.
      [Wait.t] holds the timeout (zero or none) and the earliest
      deadline of the domain. The result tells which fds are ready, if
      the alarm went off, and if [wake] occurred. *)

  val wake : t -> domain:int -> unit
  (** Ends the [wait] of domain [domain]. Any domain or thread can call
      it. A [wake] before the [wait] is not lost. *)

  val random : t -> int -> int
  (** [random w n] is in the range 0 to [n - 1]. *)

  val random_bytes : t -> bytes -> unit
  (** Fills the buffer with random bytes, for keys and nonces.
      Production uses [getrandom] or [arc4random_buf]. *)
end

val run :
  ?domains:int -> ?slack:Span.t ->
  (module WORLD with type t = 'w) -> 'w -> (unit -> 'a) -> 'a
(** [run (module W) w main] installs the world handler and the scheduler,
    then runs [main] as the first fiber. [~domains] is in milestone M1. *)
```

The signals are an argument of the world, for example `Weft_linux.run ~signals` (M2). The world must block them before it starts a thread, and only the world starts the threads.

The effect constructors (`Now`, `Submit`, `Wait`, `Wake`, `Random`, `Switch`) are private. The `.mli` does not export them. Thus only `run` and `Sim.run` can handle them, and fiber code cannot catch them by accident. Fiber code calls functions such as `Time.now Mono` or `Net.read fd buf`.

**The fd records.** The scheduler keeps one record for each open fd, and all domains share the table of records. A record holds a use count, a closed bit, a generation number and the set of domains that registered the fd (section 2.7). The first time that a fiber blocks on an fd on domain `d`, the scheduler of `d` submits `Register (d, fd, token)`. `Deregister (d, fd)` removes it. The token is an `int64` with the index of the record and the generation. The world puts the token in `epoll_data` or in `udata` of kqueue. `Ready.t` gives the token back.

The scheduler gives the domain index to `wait` and `wake`. A world never uses `Domain.self` or domain-local storage (DLS) to find a domain. In the simulator, all of the virtual domains run on one real domain, so DLS gives one value for all of them.

The production worlds implement `WORLD`, and `run` makes the handler from the module. The simulator is not a module of type `WORLD`. It handles the same effects with its own handler, because it must keep the continuation of a virtual domain at each switch point (section 2.8). `Sim.run` installs this handler. The two give the same contract to the scheduler.

A blocking OS call that has no readiness model goes through `Submit` as a `Blocking` operation. Examples are `getaddrinfo` and a read from a regular file. epoll does not accept a regular file (`epoll_ctl` gives `EPERM`). The production worlds run these operations on thread pools, or later on `io_uring`. The simulator answers them directly.

**The thread pool is in C (milestone M2).** Its threads never run OCaml code. The reason: in OCaml 5, each systhread runs under the lock of the domain that created it, and a busy domain gives the lock to another thread only at the tick, each 50 ms (`st_stubs.c`). Thus a pool thread in OCaml can wait up to 50 ms after each operation. Also:

- Each domain has a mailbox: a C queue for its completed operations. A pool thread puts a completed operation in the mailbox of the domain that submitted it, then writes the `wake` fd of that domain. If that domain is busy, it finds the mailbox in its next poll.
- A `Blocking` operation never uses the memory of the user. A read fills a C buffer of the pool, and the fiber copies the data into its `bytes` or `Bigarray` after the completion. A write copies the data into a C buffer before the submit. Thus a detached operation (section 2.5, Blocking operations) never writes into user memory, also if the GC moved or freed it. I estimate the cost of the copy at about 0.1 ms for each MiB.
- There are two pools: one for `Dns`, and one for `File` and `Dir`. Thus a DNS server that does not answer cannot stop the file operations. Each pool has a limit on detached operations, with a start value of half of its threads. When a pool is full for more than 1 s, the library writes a warning to `stderr`.
- Each library thread blocks all signals: the pool threads and the watchdog (section 2.5). The OCaml 5 signal handler records a signal for the whole process from any thread, but it does not wake a domain that waits in `epoll_wait` or `poll` (`runtime/signals.c`). Thus the library keeps its own threads out of the delivery.
- A user thread from `Thread.create` inside `run` runs under the lock of its domain. Thus it competes with the scheduler of that domain, and it gets the lock only at the 50 ms tick.

**Own stubs for non-blocking fds.** `Submit` uses stubs of the library, not the `Unix` stubs. The `Unix` stubs enter a blocking section and copy through a buffer of 64 KiB on the stack (`read_unix.c`). The own stubs do not enter a blocking section, so they must never block:

- A socket uses `recv` and `send` with `MSG_DONTWAIT`. This flag applies to the call. `O_NONBLOCK` applies to the open file description, which other processes can share and change. If another process cleared it, a plain `read` would block while it holds the runtime lock, and the next minor GC would stop all domains.
- A pipe or a tty has no such flag for each call. Thus the library does not set `O_NONBLOCK` on the fds 0, 1 and 2, and it uses a blocking section for a pipe or a tty that it did not create.

### The I/O modules (decision 6)

The library wraps the full Unix I/O API. Milestone M0 has `Net` with TCP and Unix sockets. Milestone M2 adds the other modules of the table. Fiber code thus has no reason to call `Unix` directly. A direct call has two problems:

- The simulator does not see it, so the test is not deterministic.
- In production, a blocking call stops the domain and all of its fibers.

Each operation is a constructor of the GADT `'a Op.t`, for example `Read : fd * bytes * int * int -> int Op.t`. The world implements each constructor once.

| Module | Operations | Production | Simulator |
|---|---|---|---|
| `Net` | Sockets: TCP, UDP, Unix domain. `listen`, `accept`, `connect`, `read`, `write`, `sendto`, `recvfrom`, `shutdown`, socket options | Non-blocking fds in the poller | The in-memory network |
| `Pipe` | `pipe`, `read`, `write` | Non-blocking fds in the poller | In-memory buffers |
| `File` | `open`, `read`, `write`, `pread`, `pwrite`, `fsync`, `stat`, `truncate` | `Blocking` operations on the thread pool | An in-memory file system |
| `Dir` | `mkdir`, `readdir`, `rename`, `unlink`, `rmdir` | `Blocking` operations on the thread pool | The in-memory file system |
| `Process` | `spawn`, `wait`, `kill`, the exit event | `pidfd_spawn` and a `pidfd` on Linux, `EVFILT_PROC` on kqueue | A table of fake processes that the test scripts |
| `Signal` | The signal event | `signalfd`, `EVFILT_SIGNAL` | The test sends the signal |
| `Dns` | `getaddrinfo`, `getnameinfo` | `Blocking` operations on the thread pool | A name table that the test gives |

"The full Unix I/O API" does not include these calls:

- `fork`. In OCaml 5, `Unix.fork` raises `Failure` if the program ever spawned a domain. `Process.spawn` takes its place.
- The calls that only block, for example `Unix.sleep` and `Unix.select`. `Time.sleep` and `choose` take their place.

**Direct calls to `Unix`.** A wrapper does not stop a library that calls `Unix` directly. This call goes past the world with no warning, as in caravan. Thus:

- The core library and the simulator do not depend on the `unix` library. `Op.error` is a type of the library, not `Unix.error`.
- The guard executable `test/nounix` links `weft` and `weft.sim` and has `(forbidden_libraries unix)` in its dune stanza. The build then gives an error if one of these libraries links `unix`. This field applies to executables and tests, not to libraries. Eio uses it in `tests/nounix/dune`. Each library of a layer that must stay on the seam (milestone M2) goes into the guard too. The `threads` library needs `unix`, so these libraries cannot link `threads`.
- The test suites use windtrap (section 2.17), and windtrap links `unix`. Thus a suite cannot have `(forbidden_libraries unix)`, and the guard is a separate executable with no test library. The code of a simulator test must not call `Unix`. The build does not find such a call.
- The production executable links `unix` through the Linux or kqueue world. The program code does not link it.
- The check finds only the `unix` library. It does not find a library with its own C stubs for the time or for random numbers. I think that the clock packages of `mtime` and `ptime` and the entropy of `mirage-crypto-rng` work in this way, but I did not verify it. A test can add them to `forbidden_libraries`.
- The check does not find the stdlib I/O: `open_in`, `In_channel` and `Out_channel` on files, `Sys.readdir`, `Sys.command`, `Sys.file_exists`, `Filename.temp_file`, `Sys.time` and `Sys.getenv`. These calls go past the world, and in production they block the domain.
- The library and the simulator make each `Hashtbl` with `~random:false`. If they did not, `OCAMLRUNPARAM=R` or a call to `Hashtbl.randomize ()` by any library would give each table a random seed (`hashtbl.mli`).

## 2.4 Events (from CML and affect)

The names follow CML (decision 2).

```ocaml
type 'a event

val sync : 'a event -> 'a                    (* program effect *)
val choose : 'a event list -> 'a event
val wrap : 'a event -> ('a -> 'b) -> 'b event
val guard : (unit -> 'a event) -> 'a event   (* runs at sync time *)
val with_nack : (unit event -> 'a event) -> 'a event
val always : 'a -> 'a event
val never : 'a event
```

The base events:

| Event | Poll | Block |
|---|---|---|
| `Net.readable fd`, `Net.writable fd` | The flag in the registration of the current domain (section 2.7) | Add the fiber to the waiters of that registration. Register the fd if it is new on this domain. |
| `Time.at Mono t` | `Now Mono >= t` | Add the deadline to the heap of the current domain |
| `Time.after Mono span` | A guard reads `Now Mono` and gives `Time.at Mono (now + span)` | As `at` |
| `Chan.recv c`, `Chan.send c v` | A waiting partner | Add to the waiter list of `c` |
| `Ivar.read v` | The value is set | Add to the waiters of `v` with a CAS on its state (The atomic register, below) |
| `Process.exit pid` | The table of the world | A world interest |
| `Signal.received s` | The count of `s` since the last commit of a `received s` event | A world interest (section 2.7, Signals) |

Each base event has a poll and a block, as in affect. A base event needs no nack. When a `sync` commits to one branch, the sync word of its other offers is `Done`, and the owners of these offers drop them when they meet them (sections 2.5 and 2.7). Thus a timer that loses a `choose` stays in its heap as a stale entry until its owner drops it.

`with_nack` comes from CML, for the protocols of the user. For example, a guard sends a request to a server and gives the nack event to the server. If the `choose` commits to another branch, the scheduler enables the nack, and the server knows that the client left. Thus `with_nack` solves lesson 7 for user protocols.

### Where guards and wrap functions run

`sync ev` does its steps in the fiber, as CML does:

1. It runs all of the guards in `ev`, depth first, and makes the nack events of `with_nack`.
2. It performs `Sync` with the result: a tree of base events with no guards.
3. The scheduler commits one branch and resumes the fiber.
4. The fiber runs the `wrap` functions of the committed branch.

Thus a guard or a `wrap` function can perform any effect, also `sync` and `Fiber.fork`. The scheduler never runs user code. An exception in a guard goes to the fiber that called `sync`.

### The commit across domains

Two partners on two domains can try to commit to the same `sync` at the same time. Exactly one must win, and exactly one side must resume the fiber. The base comes from Parallel CML (Reppy, Russo and Xiao, 2009) and from affect's `Blocked` state.

**The state.** Each `sync` has an id and one atomic state word: `Waiting`, `Parked`, `Claimed` or `Done i`, where `i` is the committed branch.

- A new `sync` starts as `Waiting`.
- Each domain gives ids from its own counter: id = local count × the maximum number of domains + the domain index. The ids are unique and totally ordered, and no counter is shared between domains.

**The scan.** The fiber that syncs takes its branches in the `Random` order. If the `sync` has only one branch of the user, it does not perform `Random`. For each branch:

1. If a partner offer is present, it tries to commit with that partner (A claim of two words, below).
2. If no partner offer is present, it registers its own offer on the branch.
3. It reads its own word. If the word is `Done`, a partner committed during the scan. The fiber removes its other offers and continues.

At the end of the scan, the fiber changes its word from `Waiting` to `Parked` with a CAS. If the CAS succeeds, the fiber is parked. If the word is `Done`, a partner committed, and the fiber continues. If the word is `Claimed`, it retries until the word changes.

**One owner for the resume.** A claimer keeps the value that it replaced. If it sets the word back, it restores that value. If it completes the commit, it resumes the fiber only if that value was `Parked`. If the value was `Waiting`, the fiber is still in its scan, and step 3 continues it. Thus each continuation has exactly one resume.

**A claim of two words.** The fiber in its scan claims its own word and the word of the partner, and it claims the word with the lower id first:

- A CAS changes each word from `Waiting` or `Parked` to `Claimed`.
- If a word is `Claimed`, the claimer waits with `Atomic.Backoff` until the word changes. The claimer holds only words with a lower id, so this wait is safe.
- If a word is `Done`, the offer is stale. The claimer restores each word that it holds, removes the offer and goes to the next one.
- When it holds the two words, it completes the operation and sets the two words to `Done`. It resumes the partner only if the partner was `Parked`.

**One-word partners.** A cancel, a timer, an fd event, `Ivar.fill`, a nack and a completed `Blocking` operation change only the word of the blocked fiber. If the word is `Claimed`, they retry with `Atomic.Backoff` until the word is `Waiting`, `Parked` or `Done`. They stop only at `Done`. They never drop the event because the word is `Claimed`. affect's `synced_unblock_is_ours` does the same.

**A ready branch with no partner.** Some branches are ready with no partner: `always`, an `Ivar` that is full, a set fd flag and an expired deadline. The fiber commits such a branch itself, with a CAS on its own word from `Waiting` to `Done i`. If the CAS finds `Done`, a cancel or a partner won, and the fiber takes that result. `Fiber.yield` is not a sync (section 2.2): a fiber in the queue after a yield has no sync, so a cancel cannot resume it a second time.

**The atomic register.** The scan reads "if a partner offer is present, commit, else register". For a shared object, this check and this act must be one atomic step. If not, this occurs: X does `recv c` on domain A and Y does `send c v` on domain B at the same time. Each finds no partner, each registers, and the two sleep forever. Thus each shared object has one point where the order is decided:

- An `Ivar`, the join of a scope and a nack event have one atomic state: `Empty of waiters` or `Full of value`. A waiter registers with a CAS on `Empty`. A fill changes the state to `Full` with a CAS, then it commits each waiter. affect's `cell.ml` uses the same form.
- A `Chan` has a short spin lock. The fiber holds the lock only while it looks for a partner, registers its offer or claims the words. It never holds two of these locks, and it never holds one across a world effect. A fiber that holds a sync word never waits for such a lock, so the order of section "Why there is no deadlock" still holds. Parallel CML uses a lock for each channel in the same way.
- `Signal.received` takes the count only after it holds the word. If the count is 0, it restores the word (milestone M2).

**Atomic locations.** Each location in this protocol, and in each Dekker pair of section 2.5, is an `Atomic.t`. It is written with `Atomic.set`, `Atomic.exchange` or a CAS. OCaml gives sequential consistency only among atomic locations. A plain field write and an `Atomic.get` compile to two plain `mov` instructions on amd64, so the CPU can change their order. `Atomic.Backoff` is not in the stdlib (5.5.1). The library has its own module, as affect has (`affect__base.ml`).

**Why there is no deadlock.** A side that holds a word waits only for a word with a higher id. Thus no cycle of waits can occur. Without the fixed order, this cycle can occur: fiber X syncs on `choose [send c1 v; recv c2]`, and fiber Y syncs on `choose [send c2 w; recv c1]` at the same time. Each one holds its own word and waits for the word of the other forever.

**Own offers.** A partner skips an offer of its own `sync`, for example in `choose [send c v; recv c]`.

**In the simulator.** A claim holds a word for a short time, with no world effect inside. Thus a virtual domain never sees a `Claimed` word of another virtual domain, and the retry paths on `Claimed` never run in a simulator test. The simulator cannot find a race here, so `dscheck` must examine this protocol (section 2.14, milestone M1). The unit tests run each retry path with a test instance of `ATOMIC`. Before each read or CAS, it can change the word, for example to `Claimed` and then to `Parked` (`test/core/test_commit.ml`). These tests find a path that does not retry, but not a race.

**A TLA+ model.** `spec/tla/Commit.tla` is a model of this protocol in PlusCal, and the TLC model checker examines it. Each CAS on a word is one atomic step. The model has the scan, the atomic register with a lock for each channel, the claim of two words in id order, the `Parked` state, the one-word partners (a cancel and a timer) and the self-commit. TLC checks these properties for each order of the steps:

- No continuation has more than one resume.
- A rendezvous pairs two syncs on one channel, with opposite kinds.
- Two parked syncs never have matching offers (no lost rendezvous).
- At the end, each sync with `Done` had one resume, and the cancel took effect.
- With weak fairness, each process ends: no deadlock and no loop that runs forever.

Three scenarios pass: two fibers that sync on each other, a stale offer with a timer, and a ready branch with a cancel. To show that the model is useful, four planted bugs each give a failure: no `Parked` state (two resumes), no id order (the deadlock of "Why there is no deadlock"), a one-word partner that gives up on `Claimed` (a cancel that is lost), and no channel lock (a lost rendezvous). Section 2.16 lists all of the TLA+ models. The model is small: two or three fibers, one `sync` for each fiber, and two channels. Thus it does not cover a word that is used again, nacks, or more than three fibers. `dscheck` still examines the OCaml code in M1.

When more than one branch is ready, `choose` uses `Random` from the world to select a branch. In production this prevents starvation. In the simulator, the seed controls it, so a failed run can run again with the same result.

## 2.5 The scheduler

Each domain runs one scheduler instance inside the world handler. Milestone M0 has one domain. From milestone M1, `run ~domains:n` starts `n` domains. The default is `min 4 (Domain.recommended_domain_count ())`. In OCaml 5, each minor GC stops all domains, so more domains do not always give more throughput to an I/O server. The value 4 is a start value (open question 6). In the simulator, the domains are virtual domains (section 2.8).

### Fibers, scopes and cancellation

This is the model of structured concurrency. Each fiber belongs to a scope, and the scopes make a tree. The scopes come from Eio switches and Trio nurseries.

```text
root scope (from run or Sim.run)
 ├─ fiber: main
 ├─ fiber: Server.serve           (daemon)
 └─ fiber: handler
      └─ scope (Fiber.scope inside the handler)
           ├─ fiber: read side
           └─ fiber: write side
```

The scope is a value that you give to `Fiber.fork`. Thus the place of a new fiber in the tree is always explicit.

**The cancel context is dynamic.** Cancellation must know the innermost scope of the code that runs at this time. This is not always the scope that forked the fiber. For example, fiber F belongs to the root scope and calls `Fiber.scope g`. While F runs `g`, a cancel of the inner scope must reach F. After `g` returns, it must not. Thus each fiber has a hidden stack of cancel contexts. `Fiber.scope` and `Fiber.shield` push an entry, and they pop it when they return. The contexts also make a tree with two types of node: a scope and a shield. A shield is a node of the tree, with its own cancelled flag, and a cancel from outside never sets this flag. A scope that code makes inside a shield links under the shield node, not under the outer scope. Trio's cancel scopes and Eio's cancellation contexts have the same idea. But in Eio, each domain has its own tree of contexts, and a cancel always runs on the domain of the fiber (`lib_eio/core/cancel.ml`). In this design, the fibers of one scope can be on many domains. Thus the scopes need the rules of "The concurrency of scopes", below.

```ocaml
module Fiber : sig
  type scope
  type handler = { handle : 'a. (unit -> 'a) -> 'a }

  val scope : ?handler:handler -> (scope -> 'a) -> 'a
  (** Runs the function, then waits until all fibers of the scope end.
      Each fiber of the scope and of its nested scopes runs inside
      [handler]. [~handler] is in milestone M2. *)

  val fork : ?name:string -> ?pinned:bool -> scope -> (unit -> unit) -> unit
  (** [~pinned:true] puts the fiber on domain 0, the main domain.
      [~pinned] is in milestone M3. *)
  val fork_promise :
    ?name:string -> ?pinned:bool -> scope -> (unit -> 'a) -> 'a Promise.t
  val fork_daemon : ?name:string -> scope -> (unit -> unit) -> unit
  (** [~name] names the fiber in the reports of the simulator, for
      example in [Deadlock]. *)
  (** The scope cancels a daemon fiber when all other fibers end. *)

  val fork_isolated :
    ?pinned:bool -> scope ->
    on_error:(exn -> Printexc.raw_backtrace -> unit) ->
    (unit -> unit) -> unit
  (** If the fiber raises, [on_error] gets the exception, and the scope
      does not fail. Milestone M2. *)

  val cancel : scope -> exn -> unit
  val yield : unit -> unit

  val shield : (unit -> 'a) -> 'a
  (** In [f], a [sync] does not see the cancel of the outer scopes. *)

  val with_timeout : Clock.mono Clock.line -> Span.t -> (unit -> 'a) -> 'a
  (** Runs [f] in a new scope. If [f] does not end in time, the scope
      is cancelled, and the call raises [Timeout]. *)

  exception Cancelled of exn
  exception Multiple of (exn * Printexc.raw_backtrace) list
  exception Timeout
  exception Not_in_fiber of string
  exception Internal_error of exn * Printexc.raw_backtrace
  exception Scope_closed
end

module Promise : sig
  type 'a t

  val await : 'a t -> 'a
  (** Gives the value, or raises the exception of the fiber again, with
      its backtrace. *)

  val ready : 'a t -> 'a event
  (** The event form of [await], for [choose]. *)

  val cancel : 'a t -> unit
  (** Cancels only the fiber of this promise. Milestone M2. *)

  exception Cancelled
  (** [await] raises it if the fiber of the promise was cancelled.
      Milestone M2. *)
end
```

**Lifetime:**

- A scope ends only when all of its fibers end. No fiber outlives its scope.
- The body `f` of `Fiber.scope f` is a member of the scope, as a fiber is. `Fiber.scope` returns when `f` and all of the fibers end.
- `run` and `Sim.run` give `main` a root scope. When `main` returns, the root scope cancels the remaining fibers and waits for them. Then `run` gives the result of `main`.
- The scope cancels a daemon fiber when the body and all other fibers end. Each daemon runs in its own cancel context, a node under its scope, and this cancel sets the flag of that node. Thus a daemon that starts a `sync` after the cancel reads the flag and raises. Without its own node, such a daemon has no flag to read.
- **The join ignores cancels.** After the body returns, `Fiber.scope` waits for the other members. This wait is a `sync` with no hidden cancel branch, from this scope or from an outer scope. If a cancel could end the join, `Fiber.scope` would return while children still run, and an exception could be lost. After the join, `Fiber.scope` raises the stored exception, or `Multiple`. If no member failed, it gives the result of the body: the value, or the exception of the body, for example `Cancelled`. A body that gave a value keeps it also when the scope was cancelled after it, so that `with_timeout` never drops a value. The next `sync` of the fiber then raises the sticky cancel. Eio's `Switch.await_idle` uses `await_protect` in the same way (`lib_eio/core/switch.ml`).

**Errors:**

- If a fiber or the body raises an exception, the scope fails. It cancels the body and all of its other fibers. After all of them end, `Fiber.scope` raises the first exception. If more than one member raised an exception that is not `Cancelled`, it raises `Multiple`.
- A daemon fiber that raises makes the scope fail, as a normal fiber does.
- If the fiber of `fork_promise` raises, the promise holds the exception, and the scope does not fail. `Promise.await` raises the exception again. Eio has the same rule. Section "Promises" that follows gives the details.
- If the fiber of `fork_isolated` raises, `on_error` gets the exception and its backtrace, and the scope does not fail. Section "Exceptions" that follows gives the reason.
- If no fiber awaits a failed promise, its exception is lost. Milestone M3 adds a report: the promise has a finaliser (`Gc.finalise_last`), and if no fiber awaited the promise, the finaliser writes the exception to `stderr`, with no effect. The closure of the finaliser must not hold the promise, because then the promise stays reachable (`gc.mli`). Thus the "awaited" flag is in a separate cell. The finaliser catches each exception of `Printexc.to_string`, because a printer of the user can raise. In the simulator, `Sim.run` drops its own references, forces a full GC at its end and reports these promises. A report at the end of the scope is too early, because code can await a promise after its scope ended.

**Propagation:**

- A cancel goes down to all nested scopes, but it stops at a shield node. The scopes under a shield node do not get it.
- An error goes up only as an exception. The inner `Fiber.scope` raises it in its parent fiber, and the scope of the parent then fails.

Thus each direction has one path.

**Cancellation is a hidden branch of each `sync`.** This rule joins the CML events and the scopes. Each `sync ev` acts as `choose [ev; cancelled ctx]`, where `ctx` is the top of the fiber's stack of cancel contexts. As a result:

- A cancel from another domain is one more partner in the commit protocol (section 2.4). It is a one-word partner: it changes the word from `Waiting` or `Parked` to `Done`, and it retries while the word is `Claimed`. It resumes the fiber only if the word was `Parked`. One CAS thus decides a race between a cancel and a channel partner. A fiber never receives the value and also raises `Cancelled`.
- Before the `sync` raises, it does the same steps as a losing branch of a `choose`. It removes its offers and its heap entries, and it enables all of its nack events.
- If the cancel is present when `sync` starts, the cancel wins, also if `ev` is ready. Trio and Eio do the same. Without this rule, a loop on an event that is always ready never sees the cancel.
- A cancel is sticky. If the fiber catches `Cancelled` and continues, its next `sync` raises `Cancelled` again.

**How a cancel reaches a fiber.** The hidden branch does not register an offer on the shared context. If it did, all fibers of a scope would change one atomic value at each `sync`. A contended atomic costs about 40 ns with 8 domains and about 150 ns with 16 domains (measured on a 16-CPU machine). Thus:

- Each fiber has one atomic field, "the current sync word". The fiber sets it when a `sync` starts and clears it when the `sync` ends.
- Each `sync` has a record with its innermost cancel context. The record does not change, and the fiber publishes it before it sets the current sync word.
- A cancel of scope S sets the cancelled flag of S, and only that flag. It does not set the flags of the nested scopes. Then it walks the members of S and of its nested scopes, and it stops at each shield node. For each member that is in a `sync`, it reads the context of that `sync`. It commits the `sync` with the cancel only if the chain of contexts from that `sync` reaches S with no shield between them. Thus a cancel cannot reach a `sync` that the fiber started after it left S, or a `sync` inside a shield.
- The walk collects the commits under the lock of each scope. It resumes the fibers and calls `wake` only after it releases the lock, because `wake` is a world effect.
- Before a `sync` parks, it tests the cancelled flags of its contexts, up to the nearest shield. Thus a `sync` in a nested scope sees the flag of an outer scope, and the nested scopes need no flag of their own. A chain is short, and its reads have no contention.
- The cancel writes the flag and then reads the current sync word. The fiber writes its current sync word and then reads the flag. OCaml atomics are sequentially consistent, so at least one of the two sees the other. This is the Dekker pattern again, and `dscheck` must examine it.

**The concurrency of scopes.** The fibers of one scope can run on many domains at the same time. Thus:

- Each scope has one atomic state word with two counts and a closed bit: the members that are not daemons, and all members. A `fork` adds a member with a CAS that succeeds only if the closed bit is clear. When the first count is 0 and the body returned, the scope cancels its daemons. The end of the last member sets the closed bit with the same CAS. Thus a `fork` and the end of the last member cannot both win.
- A member that fails stores its exception before it decreases the counts. Thus the join always sees the exception.
- If `on_error` of `fork_isolated` raises, its exception goes to the outer scope, as an exception of a normal fiber does.
- Each scope has a short lock for its list of members and of nested scopes. A new nested scope links itself under this lock, under its parent node. The parent of a scope inside a shield is the shield node. The link has no cancel logic: a cancel that walks the list at the same time can miss the new scope, but each `sync` in it reads the flag of the cancelled scope through its chain. The TLA+ model of section 2.16 examined this.
- A scope never holds its lock across a world effect. Thus, in the simulator, the lock is never contended.

**Cancellation points.** Only `sync` and `Fiber.yield` are cancellation points. `Time.sleep` calls `sync`. An I/O function calls `sync` only when it must wait for its fd (after `EAGAIN`), so it is a cancellation point only then. A write to a socket with space, or a read of a socket with data, completes also in a cancelled fiber. `with_timeout` reads the flags of its chain when it starts, so in a cancelled context it raises that `Cancelled` at once. A fiber that computes and never calls `sync` cannot be cancelled. This agrees with "The limit of a busy domain" that follows.

**Blocking operations.** A `Blocking` operation on the thread pool cannot stop, but its fiber does not wait for it. On a cancel, the fiber detaches from the operation and raises `Cancelled` at once. Thus `with_timeout` around `Dns.resolve` ends at the deadline, also if `getaddrinfo` takes 30 s. The rules for a detached operation:

- It uses a C buffer of the pool, never the memory of the user (section 2.3). The pool frees the buffer when the operation completes.
- It keeps its place in the limit of detached operations of its pool until it completes (section 2.3).
- The pool drops its result. If the operation opened an fd, the pool closes that fd.
- Nobody knows if a detached `write` or `rename` occurred. Cancellation in other systems has the same semantics.

**Cleanup code.** Without a shield, a cancelled fiber that does I/O in `Fun.protect ~finally` gets `Cancelled` again at once. Thus it cannot, for example, send a WebSocket close frame. In `Fiber.shield f`, a `sync` does not see the cancel of the outer scopes. Trio has the same "shielded" scope.

**The shield is a one-way barrier:**

- **It stops the cancels from outside.** A cancel of an outer scope does not go into `f`, and it does not go into a scope that `f` makes, because that scope links under the shield node. The chain of a `sync` in `f` stops at the shield node, so it does not read the flag of the outer scope. The cancel is not lost. When `f` returns, the next `sync` of the fiber raises `Cancelled`.
- **The cancels from inside still work.** A scope that `f` makes can cancel its own code, for example with `with_timeout`.
- **It lets exceptions go out.** An exception in `f` goes out as usual, and it can make the outer scope fail.
- **It delays the end of the outer scope.** The outer scope waits for the fiber, so it waits until `f` returns.
- **It applies only to the code of this fiber.** If `f` forks a fiber into an outer scope, the shield does not protect that fiber.

Because the outer scope waits, a shield must have a time limit. If not, a cleanup can stop the program forever:

```ocaml
Fun.protect f ~finally:(fun () ->
  Fiber.shield (fun () ->
    Fiber.with_timeout Mono (Span.s 1) (fun () -> Ws.send_close c)))
```

The scope of `with_timeout` links under the shield node. Thus the cancel of the outer scope does not reach it, and its own timer can cancel it.

**Fork into a scope that ends.** Code can give a scope value to another fiber, for example to fork the handler of each connection. If the scope has ended, or a flag on its chain of contexts is set, `fork` raises `Fiber.Scope_closed`. The `fork` reads the chain, because a nested scope has no flag of its own. This exception is different from `Cancelled`. The reason: the fiber that called `fork` is not cancelled. If `fork` raised `Cancelled`, the scope of that fiber could take a real failure as a cancel.

**Timeouts.** For one event, CML already has a timeout: `choose [ev; Time.after Mono span]`. For a block of code, `Fiber.with_timeout` runs `f` in a new scope T. In milestone M0, T has one daemon fiber that does `Time.sleep` and then cancels T with `Timeout`. The result:

- If `f` gives a value, `with_timeout` gives that value. It never drops a value because the timer fired at the same time.
- It raises `Timeout` only if `f` raised the `Cancelled` of T's own deadline.
- If an outer scope is cancelled at the same time, it raises the `Cancelled` of the outer scope, not `Timeout`.

In the simulator, the sleep is a normal heap entry, so the result is exact. Milestone M3 can replace the daemon fiber with a deadline on the cancel context, as Trio does. That form costs one heap entry and no fiber, but it needs one more CAS on the entry: `Live` to `Dead` by the caller, and `Live` to `Fired` by the owner.

**A TLA+ model.** `spec/tla/Scope.tla` is a model of these rules in PlusCal, and TLC examines each order of the steps. The model has a root scope R, a scope S under R, the context of a daemon of S, a shield node in S and a scope T under the shield. Each fiber waits in a `sync` that only a cancel can end, so each fiber ends only if the cancel reaches it. Three scenarios keep the state space small:

- **error:** a member of S fails while a daemon of S runs.
- **shield:** a member of S runs a cleanup in a shield, with a `with_timeout` of its own.
- **fork:** a member of R forks a fiber into S while S can end.
- **condwait (M2):** as **shield**, but the `sync` is a `Condition.wait` that opens the shield node.

TLC checks these properties in each scenario:

- No fiber outlives its scope (`NoOrphan`).
- The exception of a failed member is not lost (`ExnNotLost`).
- A cancel from outside never ends a `sync` inside a shield (`ShieldHolds`), and `with_timeout` raises `Timeout` only for its own deadline (`TimeoutOK`).
- A cancel of X ends only a `sync` whose chain of contexts reaches X (`NoForeignCancel`).
- A closed scope has no members (`ClosedEmpty`).
- In **condwait**, when the timer of `with_timeout` ended its walk, the wait is not parked (`TimerReachesWait`). The two shield properties do not apply, because the wait opens the shield.
- With weak fairness, each process ends. Thus each cancel reaches each fiber that it must reach, and each join ends.

All three scenarios pass. Five planted bugs each give a failure: a join that a cancel can end, a scope in a shield that links under the outer scope, a walk with no check of the chain, a member that ends before it stores its exception, and a `sync` that parks with no read of the flags. Section 2.16 lists all of the TLA+ models. The model makes each commit one atomic step, and `Commit.tla` examines the commit itself. The model does not cover promises, `fork_isolated`, `Fiber.Mutex`, more than one nested scope in each scenario, or the deadline form of `with_timeout`.

**In the simulator.** The scope cancels its members in the fork order, so the seed sets the full result. After a scope ends, the simulator examines its state. It must find no fibers of that scope, and no live offers or heap entries. An offer or an entry is live if its sync word is not `Done`. Stale entries can stay until their owner drops them.

### Exceptions

**The backtrace.** An exception goes from a child fiber to the fiber that called `Fiber.scope`, which has a different stack. Thus:

- When a member raises, the scope stores the exception with `Printexc.get_raw_backtrace ()`.
- `Fiber.scope` raises it again with `Printexc.raise_with_backtrace`. `Multiple` holds a backtrace for each exception.
- `Promise.await` does the same with the exception of the promise. affect's `get` keeps the backtrace in the same way.

**A server must isolate each connection.** Suppose that `Server.serve` forks each handler with `Fiber.fork`. Then one handler that raises makes the server scope fail, and all other connections are cancelled. Thus a server forks each handler with `Fiber.fork_isolated ~on_error`. An exception of the handler goes to `on_error`, not to the server scope. Eio's `run_server` does the same with `~on_error`. A cancel of the server scope still goes down to each handler.

**A guard that raises.** Suppose that `sync (choose [a; b])` runs the guard of `a`, and this guard sends a request to a server with `with_nack`. Then the guard of `b` raises. Branch `a` never commits, so the server must get its nack. Thus, if a guard raises, `sync` enables the nacks of all guards that already ran, and then it raises the exception.

**Code that catches `Cancelled`.** A handler such as `try ... with _ -> ()` also catches `Cancelled`, and the fiber continues. The sticky cancel makes the next `sync` raise again. But a loop that catches all exceptions around each `sync` never ends. Thus:

- Code that catches all exceptions must raise `Cancelled` again, for example with `| Fiber.Cancelled _ as e -> raise e`.
- The simulator reports a fiber that catches `Cancelled` more than 100 times. The value 100 is a start value.

**Asynchronous exceptions are not permitted.** OCaml can raise an exception from a `Sys.signal` handler, from a finaliser, or as `Sys.Break` after `Sys.catch_break`. These exceptions come at a poll point. OCaml 5 polls at allocations, at function entries and at loop back edges. Thus this point can be inside the scheduler's own code, for example in a push to the local queue. The scheduler state is then half changed, and OCaml has no way to block these exceptions. Thus:

- A program that uses this library must not call `Sys.catch_break true`.
- A signal handler or a finaliser must not raise. Signals come as events (section 2.7).

**A bug in the scheduler.** If the scheduler itself raises, for example `Effect.Continuation_already_resumed`, its state is not known. Thus `run` stops all domains and raises `Fiber.Internal_error` with the original exception and its backtrace. It does not try to continue. In the simulator, the same seed gives the same failure, so the bug can be debugged.

**`Stack_overflow` and `Out_of_memory`.** In OCaml 5, each fiber has its own stack, which can grow up to a limit for each stack (`fiber.c`). A review verified that `Stack_overflow` in a fiber is a normal exception that code can catch (OCaml 5.5.1, Linux). Thus its scope fails in the normal way. After `Out_of_memory`, the scheduler can have no memory to continue. Thus `run` acts as for a bug in the scheduler.

**Which exception is first.** In production, "the first exception" of a scope depends on the timing of the domains. Two runs can raise different exceptions. In the simulator, the seed sets it. A test must not depend on which exception is first when many members fail.

### Promises

`fork_promise` is the form for a caller that wants the result or the exception. It acts as affect's `Fun.Async.call` with `get`:

- **Its own scope.** The fiber of a promise runs in a new scope inside the scope that `fork_promise` got. Thus `Promise.cancel p` cancels only that fiber, as affect's `cancel f` does. A cancel of the outer scope still goes down to it.
- **`await` and `ready`.** `Promise.await p` gives the value or raises the exception again, with its backtrace. `Promise.ready p` is the same as an event, as affect's `get'` is. Thus `sync (choose [Promise.ready p; Time.after Mono span])` races a result with a timeout.
- **Whose cancel it is.** If the fiber of the promise was cancelled, `await` raises `Promise.Cancelled`, not `Fiber.Cancelled`. Thus the caller can tell it from a cancel of its own context. affect's documentation of `mask_cancellation` describes the same problem.
- **A promise that nobody awaits.** The rule in "Errors" applies: the simulator reports it, and production writes it to `stderr`.

### Effect handlers of a scope (M2)

A user can have their own effects, for example a counter or a logger. A handler that the user installs in fiber code does not cover a fiber that the code forks, because that fiber runs under the scheduler. A user effect in the child then raises `Effect.Unhandled`. This is lesson 6 again, for user effects. The fix comes from affect's `Call_handler`:

- `Fiber.scope ~handler f` stores `handler` in the scope. The body `f` runs inside `handler.handle`.
- Each fiber of the scope runs inside the handler of its scope. A nested scope composes its handler inside the handler of the outer scope. Thus a fiber of a nested scope runs as `outer.handle (fun () -> inner.handle g)`, as in affect.
- The handler must be safe for parallel use. It can run on many domains at the same time, and a fiber can move to a different domain while it is suspended.
- A user handler cannot match the world effects or the program effects by name, because their constructors are private (section 2.3). These effects go through the user handler to the scheduler and to the world handler. But a user handler with a wildcard case in its `effc` catches all effects, also the private ones. Thus a user handler must give `None` for each effect that it does not know.

### Compared with affect's asynchronous functions

| | affect | This design |
|---|---|---|
| A call with a result | `Fun.Async.call` and `get` | `Fiber.fork_promise` and `Promise.await` |
| A call with no result | `call_trap_exn`. An exception goes to the uncaught exception handler. | `Fiber.fork`. An exception makes the scope fail. `Fiber.fork_isolated` gives it to `on_error`. |
| The scope | Implicit: the body of the function that made the call | Explicit: `Fiber.scope` |
| An exception that nobody gets | Silent, unless the call uses `call_trap_exn` | Not possible with `fork`. Reported for a promise. |
| Cancel | One call, with `cancel f` | A scope, or one promise |
| Mask the cancel | `mask_cancellation` | `Fiber.shield` |
| Pin to a domain | `~only_main` (the main domain) | `~pinned` (domain 0, the main domain) |
| Stealing | Full. A fiber that ran can move to another domain. | The same (decision 7) |
| User effect handlers | `Call_handler`, composed for descendants | `Fiber.scope ~handler`, composed for nested scopes |
| Priorities | Hints. `Fun.Async.main` ignores them at this time. | None in the first version |

### The state of each domain

The stealing is full, as in affect (decision 7). A thief can take any ready fiber, also a fiber that already ran. But the registrations of a fiber do not move. When a fiber blocks, it registers each offer on the domain where it blocks: an fd wait in the poller of that domain, and a timer in the heap of that domain. affect is different here: it keeps the fd waits of all domains in one shared map, and one domain at a time calls `select` (`affect_unix__select.ml`). Its comment proposes one map for each domain as a future idea. This design does that, so most of the state of a domain belongs only to its owner. Decisions 7 and 8 come in milestone M1.

| Part | What it holds | Who uses it |
|---|---|---|
| Local queue | The ready fibers of this domain, in FIFO order. A bounded ring, as in Go and Tokio. | The owner pushes at the tail and takes from the head. Thieves take from the head. |
| Next slot (M3) | One fiber that a fiber of this domain woke. It is atomic. | The owner. A thief can take it (A steal of the next slot, below). |
| Pinned queue (M3) | On domain 0 only: the fibers with `~pinned:true` | Any domain pushes. Only domain 0 takes. |
| Poller | The epoll set or the kqueue of this domain, with its fd registrations, its alarm and its `wake` fd. On domain 0, also the signal source. | Only the owner. `wake` can come from any thread. |
| Fd registrations | For each fd in the poller: a read flag, a write flag and the waiters | Only the owner |
| Deadline heap | The `Mono` deadlines of the fibers that blocked on this domain (section 2.6) | Only the owner adds and pops entries. Other domains change only the sync word of an entry. |
| Mailbox (M2) | A C queue for the completed `Blocking` operations and the close notices of this domain | Pool threads and other domains push. Only the owner takes. |
| PRNG | Selects the domain to steal from | Only the owner. Its seed comes from `World.random` when the domain starts. |
| Sync id counter | The ids of section 2.4 | Only the owner |

These parts are shared by all domains:

- The local queues and the next slots, for stealing
- The global injection queue (M3), for `Ivar.fill_from_outside`. It is lock-free, because a finaliser can push to it (section 2.5, Fiber code with no handler).
- The `idle` set (and in M3, the `searching` count, if it comes)
- The scopes and the sync words
- The fd records: the use count, the closed bit, the generation and the set of domains (section 2.7)

**What occurs when the event comes.** The owner of the registration finds the event. It commits each waiter with the one-word commit of section 2.4, and it pushes each resumed fiber to its own tail. Any domain can then steal the fiber. If a `sync` committed to another branch, its other registrations stay as stale entries. Their sync words are `Done`, so their owners drop them when they meet them.

### How a fiber finds its domain (M1)

A fiber can move to another domain while it is suspended. Thus the code of the fiber must never keep the state of one domain:

- The per-fiber handler of the program effects holds no state of a domain. When a fiber performs `Sync` or `Fork`, the handler gives a step value to the loop that resumed the fiber: `Suspended (k, op)` or `Finished`. That loop runs on the domain that runs the fiber at this time, so it uses its own state.
- The scheduler never uses DLS. affect finds its domain with DLS, but in the simulator, all virtual domains share one DLS.
- In the simulator, the world handler gives each continuation to one top-level loop. It never resumes another virtual domain inside a handler case. If it did, the stack would grow at each switch point.

### Where a woken fiber goes

| What wakes the fiber | Where it goes |
|---|---|
| A fiber on domain A, for example with `Chan.send`, `Ivar.fill` or a nack | The next slot of A. The fiber that was in the slot moves to the tail of A. |
| An fd event, a deadline or a completed `Blocking` operation that domain A finds | The tail of A |
| `Ivar.fill_from_outside` | The global injection queue |
| A new fiber from `Fiber.fork` on A | The tail of A |
| A fiber that yields | The tail of its domain, never the next slot |
| A fiber with `~pinned:true`, for any of these reasons | The pinned queue of domain 0. The waker wakes domain 0 directly. |

The next slot makes a hand-off fast. When F1 sends to F2 and then blocks, F2 runs next on the same domain, and its data is still in the CPU cache. Go and Tokio have the same slot. A yield never goes to the next slot, because then the fiber would run again at once.

**A limit on the next slot (M3).** Two fibers that send to each other can keep the next slot full forever. The local queue then never runs. Thus the next slot runs at most 3 times in sequence. After that, the fiber in the slot goes to the tail. Tokio has a similar limit. The value 3 is a start value, and it must be measured.

**A steal of the next slot (M3).** This steal is only for latency: the TLA+ model of `Wake.tla` showed that without it no wakeup is lost. It can stay out until a measurement shows that it is necessary. **Measured:** with no steal, F2 waited 50 ms behind a computation of 50 ms by F1, also on 4 domains, because no other domain could take it. With the steal and the wake at a push to the next slot, F2 ran after 0.1 ms. The wake costs about 10% on a ping-pong across 4 domains. Thus the steal is in M3. A thief first tries the local queues of all domains. If they are empty, it tries the next slots, and it takes a fiber from a next slot with a CAS. Suppose that F1 wakes F2 and then computes for 50 ms. Then an idle domain takes F2 from the next slot, and F2 does not wait 50 ms. Go also lets a thief take `runnext`. A push to the next slot wakes an idle domain with the normal rule of "Idle and wake", below.

I changed one point from my earlier answer. A Chase-Lev deque gives the owner LIFO order, and LIFO order is not fair to old fibers. Thus the local queue is FIFO, and only the next slot is LIFO.

### The loop of one domain

Steps 1 and 3 and the next slot come in milestone M3. Before M3, step 2 takes only from the local queue, and step 4 does not try the next slots.

1. On domain 0, take a fiber from the pinned queue first. affect has the same rule for `only_main`.
2. Take a fiber from the next slot, else from the head of the local queue.
3. Each 61st time, take from the overflow queue first (from M1), and in M3 also from the global injection queue. This prevents starvation of these queues. Go uses the same rule. A full ring moves its older half and the new fiber to the overflow queue, as Go's `runqputslow` does, so the order stays FIFO. The review of M1 found that without this rule, a fiber in the overflow queue never runs while more than 256 fibers yield.
4. If there is no fiber, steal. Select a domain with the PRNG and take half of its local queue. Try each domain one time. Then try the next slots.
5. If there is a fiber, perform `Switch`, then run the fiber until it blocks, yields or ends. Go to step 1.
6. If there is no fiber, poll. Read `Now`, expire the deadlines of this domain, take the mailbox, and `Wait` with timeout 0. If this made a fiber ready, go to step 1.
7. If nothing is ready, go idle (Idle and wake, below). Then `Wait` with no timeout on the poller of this domain, with the alarm set to its earliest deadline.

**A busy domain also polls.** Each `K` fiber switches, the domain does step 6 even if it has ready fibers. Without this rule, its timers and fds wait until its queue is empty. This was a weak point of affect in the session: affect checks the timers only when no call is ready. Eio and Guile Fibers check once in each turn. The value of `K` is an open question.

**The limit of a busy domain.** A domain sees its own fds and timers only when it polls. While a fiber computes without a switch, its domain does not poll. Then the fibers whose registrations are on that domain wait too, and other domains cannot poll for them. affect and Eio have the same limit. Thus:

- A fiber must call `Fiber.yield` in a long loop. Idle domains can then steal the fibers of the queue.
- In production (milestone M3), a watchdog thread in C reads a switch counter of each domain. If a domain has not switched for 10 ms, the watchdog writes a warning to `stderr` with the index of the domain. The counter is in memory outside the OCaml heap. The value 10 ms is a start value.

### Idle and wake (M1)

A lost wake-up is the typical bug here: domain A goes to sleep, and at the same time domain B pushes work that A must run. In M1, the protocol uses one atomic value that all domains share:

- `idle`: the set of domains that sleep in `Wait`, or that are about to sleep.

**A domain that finds no work (step 4 of the loop found nothing):**

1. It adds itself to `idle`.
2. It checks again: its next slot, its local queue, its mailbox, the global injection queue, the queues and next slots of the other domains, and on domain 0, the pinned queue.
3. If it finds work, it removes itself from `idle` and goes to step 1 of the loop. If not, it performs `Wait`.

**A domain or thread that pushes work:**

1. It pushes the fiber.
2. It reads `idle`. If `idle` is not empty, it takes one domain out of `idle` and wakes it with `wake`.
3. A push to the pinned queue wakes domain 0 directly. A different idle domain cannot run a pinned fiber.

OCaml `Atomic` operations are sequentially consistent. Thus one of the two sides always sees the other: the sleeper sees the work in its second check, or the pusher sees the sleeper in `idle`. This is the Dekker pattern. It needs a model check with `dscheck`, because the simulator does not try the other orders of the atomic operations between two switch points.

**What the second check protects in M1.** In M1, each push goes to the queue of the pusher, and the pusher runs its own queue. Thus without the second check, a domain can sleep while another domain has work, and only parallelism is lost: the code of M1 found this with a planted bug that gave no failure. The second check is a rule of liveness for the pushes from outside the domains (M3, `fill_from_outside`). The check also reads `finished`, because a poll in the same turn of the loop can take the wake of the end of `main`. It does not read the mailbox: a close notice wakes its domain, and `wait` takes the mailbox after each poll.

A `wake` that comes before the domain is in `Wait` is not lost. The `eventfd` keeps its count, and the next `Wait` returns at once. `EVFILT_USER` in kqueue keeps its trigger in the same way.

**The cost: more wakes.** A push wakes an idle domain each time that one exists, also when another domain already looks for work. Thus there can be more wakes than necessary, a small "thundering herd". Go and Tokio prevent this with a count of the domains that search for work ("spinning" threads in Go, "searching" workers in Tokio). Open question 8 asks for a measurement of this cost.

**The Go rules (M3, only if a measurement shows that they are necessary).** These rules lower the number of wakes, but the TLA+ model of `Wake.tla` showed that correctness does not need them:

- A `searching` count. A domain increases it when it starts step 4, and a push wakes nobody if it is not 0.
- At most half of the domains search at one time.
- The last searching domain that finds work wakes one idle domain.
- The order of the decrease: a searching domain that finds no work decreases `searching` before its second check. If the decrease comes after, this lost wakeup can occur. Domain S is the last searching domain, and its second check finds nothing. A thread then pushes to the injection queue, reads `searching = 1`, and wakes nobody. Then S decreases `searching` and sleeps, with work in the queue. The model found this race.

### Timers and domains

- A fiber that blocks with a timer puts its entry in the heap of the domain where it blocks. Only that domain pops the entry.
- When the deadline comes, the owner does the one-word commit on the sync word of the entry. If the commit succeeds, the fiber goes to the tail of the owner.
- A commit or a nack on another domain does not change the heap. It changes only the sync word. The owner drops an entry with a `Done` word when it pops it.
- **Stale entries.** The owner drops the stale entries at the head of the heap when it adds an entry and before it sets the alarm. Without this rule, a cancelled timeout still makes the alarm go off. When the heap has grown to twice its size after the last rebuild, the owner removes the stale entries in place and builds the heap again. Thus an add costs O(log n) on average.
- **Slack.** The owner rounds the alarm up to a multiple of the slack. Thus the deadlines in one slot of the slack share one alarm. The owner expires only the entries up to `now`, so a timer never fires before its deadline. An earlier version of this rule expired the entries up to `now + slack`. A test in M0 showed that this rule ends a sleep of 1 ms at once, against the M0 limit "a 10 ms sleep ends between 10 ms and 12 ms". The slack is 1 ms by default, and 0 in the simulator. The value can be changed.
- Each domain sets its own alarm from its own heap.

### Fibers that must stay on one domain (M3)

Some code must run on one thread, for example a C library that is not thread-safe. `Fiber.fork ~pinned:true scope f` puts the fiber on domain 0, the main domain, and thieves do not take it. affect's `only_main` does the same. "The current domain" is not a good target: the fiber that calls `fork` can itself move, so two pinned fibers could end on two domains.

A pinned fiber always blocks on domain 0, so its registrations are on domain 0. A wake puts it in the pinned queue of domain 0 and wakes domain 0 directly.

### Locks for fiber code (M2)

The library gives `Fiber.Mutex` and `Fiber.Condition`. They suspend the fiber, not the domain.

`Fiber.Mutex.use_rw ~protect m f` locks `m`, runs `f` and unlocks `m`. If `f` raises, the protected data can be in an incorrect state. Thus the mutex becomes poisoned, and each later `lock` raises `Fiber.Mutex.Poisoned`. `Cancelled` also poisons it. Thus:

- With `~protect:true`, `f` runs in a shield, so a cancel cannot stop it in the middle. Use this form when `f` does I/O, because a timeout could otherwise disable a shared mutex permanently.
- `Fiber.Mutex.use_ro m f` never poisons the mutex. It is for code that only reads the protected data.

Eio's mutex has the same API (`eio_mutex.mli`).

Fiber code must not hold a stdlib `Mutex` across a `sync` or a world effect, for two reasons:

- A second fiber on the same domain that locks it gets `Sys_error` (`EDEADLK`), because the domain is one OS thread. In the simulator, all virtual domains are on one OS thread, so this occurs for each lock from another fiber.
- With full stealing, the holder can resume on a different domain. Its `unlock` then gives `Sys_error` (`EPERM`), and the mutex stays locked forever. Each domain that waits on it then stops. A review verified these two results with a test program (`mutex.mli` agrees).

**A cancel in `Condition.wait` does not poison.** By contract, the protected data is correct at a wait point. Thus a `Cancelled` that comes out of `Condition.wait` does not poison the mutex, and `use_rw` raises it with no poison. Also, inside `use_rw ~protect:true`, `Condition.wait` sees the cancels from outside. If it did not, a consumer that waits for a producer that never comes would make its scope wait forever. Eio poisons in this case (`eio_mutex.ml`, `condition.ml`).

**How the wait sees the cancels (M2).** The shield node of `use_rw ~protect:true` has an open flag. `Condition.wait` opens the node before its `sync` publishes its record, and it shuts the node after the `sync`. While the node is open, the read of the flags and the check of the chain go through it. Thus the wait sees the cancels of the outer scopes and of the scopes inside the shield, for example a `with_timeout` around the wait. The walk of a cancel never goes into a shield node, but it finds the fiber as a member of the outer scope. The mutex holds the node only while a fiber holds the mutex, and `Condition.wait` clears it before the unlock, so a later holder never uses the node of another fiber. The review of M2 found that the first form (the wait used the contexts from outside the shield) missed the cancels from inside, and `Scope.tla` has a scenario for it (section 2.16).

`Fiber.Condition.wait` must lock the mutex again before it returns, also when the fiber is cancelled. That lock is a `sync`, so `Condition.wait` does it in a shield. Without the shield, the lock raises `Cancelled`, and the caller continues without the mutex. This shield has no time limit. It is an exception to the rule of the shield: the lock waits only for the other holders of the mutex, and each of them must unlock it.

For the same reason as the stdlib `Mutex`, fiber code must not use a spin loop that waits for another domain. In the simulator, no other virtual domain runs between two switch points, so the loop never ends.

### Fiber code with no handler

`run` installs the handler for all fibers. Code that does not run in a fiber has no handler:

- Threads from `Thread.create` and domains from `Domain.spawn` that user code starts
- `at_exit` functions
- Finalisers, `Sys.signal` handlers, `Gc.Memprof` callbacks and `Gc.create_alarm` functions. They run through a C callback, so even in a fiber they have no handler. A review verified this for finalisers and signal handlers with a test program.
- OCaml code that C calls back with `caml_callback`, also inside a fiber. Examples are the callbacks of a TLS library or of sqlite. Here the library raises `Fiber.Not_in_fiber` with a message that names a C callback.

In these places, a world effect raises `Effect.Unhandled`. The library catches it and raises `Fiber.Not_in_fiber` with the name of the function, for example `"Time.now"`.

**A value from outside a fiber (M3).** These places must not use `Ivar.fill`, for two reasons:

- OCaml runs finalisers and `Sys.signal` handlers at an allocation point, and the scheduler's own code allocates. Thus a finaliser can run while the owner changes its local ring or its next slot. A second push from `Ivar.fill` would then corrupt this state.
- A thread from `Thread.create` has no domain, and thus no next slot.

Thus these places use `Ivar.fill_from_outside`. It never touches a sync word, a waiter list or a lock. It puts a request `(ivar, value)` in the global injection queue, and then it calls `wake`. A domain loop takes the request at a safe point and does a normal `Ivar.fill`.

The reason: a finaliser runs on the thread of the code that it interrupts. Suppose that this code holds a sync word as `Claimed`, and the finaliser tries to commit that word. Then the finaliser retries forever, because the code that can release the word cannot continue. A review verified this with a test program: the finaliser saw `Claimed` for 50,000,000 tries. Some rules follow:

- `wake` needs the world, but these places have no handler. Thus `run` keeps the world in a global slot for this function only.
- The injection queue is lock-free. If it used a stdlib `Mutex`, a finaliser that pushes while the same thread holds the mutex would raise `Sys_error`.
- In the simulator, `fill_from_outside` only adds the request to a queue of the simulator, and the simulator gives it to the fiber only at quiescence. But the GC selects when the finaliser runs, and the GC depends on the allocations, on `OCAMLRUNPARAM` and on the tests that ran before in the same process. Thus a test with a fill from a finaliser gives the same result only in the same binary with the same history. The simulator logs each such fill with its switch number, and `Sim.run` refuses these fills unless the test enables them.

## 2.6 Time (from the caravan design)

The two time lines share one span type (decision 3). The library has its own time types. It does not use `Mtime` and `Ptime`, because it is new code (decision 1).

```ocaml
module Span : sig
  type t                    (* a signed count of nanoseconds, int64 *)
  val ns : int -> t
  val ms : int -> t
  val s : int -> t
  val of_float_s : float -> t
end

module Clock : sig
  type mono                 (* monotonic: durations and timeouts *)
  type posix                (* the wall clock: calendar points *)
  type 'time line = Mono : mono line | Posix : posix line
end

module Time : sig
  val now : 'time Clock.line -> 'time
  val add : 'time Clock.line -> 'time -> Span.t -> 'time option   (* None on overflow *)
  val diff : 'time Clock.line -> 'time -> 'time -> Span.t
  val at : Clock.mono Clock.line -> Clock.mono -> unit event
  val after : Clock.mono Clock.line -> Span.t -> unit event
  val sleep : Clock.mono Clock.line -> Span.t -> unit
  val posix_of_unix_s : float -> Clock.posix
  val posix_to_unix_s : Clock.posix -> float
end
```

- Code does not carry a clock. `Time.now Mono` performs `Now Mono`, and the world handler gives the time. This removes the `Config.time` and `Flow.time` fields that caravan needed.
- `mono` and `posix` are abstract and different types. Thus the compiler rejects a `posix` time in a `Mono` deadline, as `(module CLOCK with type t = ...)` did in caravan.
- One `Span.t` works with the two lines, for `add` and `diff`.
- **Real `Posix` timers (M3).** `at`, `after` and `sleep` take `'time Clock.line`. Each domain has a second heap for the `Posix` deadlines, and `Wait` gets the earliest one. The Linux world has a second `timerfd` on `CLOCK_REALTIME` with `TFD_TIMER_ABSTIME` and `TFD_TIMER_CANCEL_ON_SET`: a step of the wall clock makes it readable, and the domain expires its `Posix` heap with the new time and sets the alarm again. In the simulator, `Sim.set_posix` makes the step, and the quiescence moves the time to the `Mono` point where `Posix` reaches the deadline. A `Mono` timer never moves at a step.
- **Timers use only `Mono` until milestone M3.** `at`, `after` and `sleep` take only the `Mono` line. A `Posix` timer that the library converts to a `Mono` deadline at the arm does not follow a clock step, so the choice of the line would have no effect, or it would mislead. Thus there is one heap and one alarm for each domain. The calls keep the line argument, so milestone M3 can change the type to `'time Clock.line` and add a real `Posix` alarm with `TFD_TIMER_CANCEL_ON_SET`. Code that uses `Mono` does not change.
- With an int64 count of nanoseconds, a `posix` time can go up to the year 2262. `add` gives `None` on overflow. This prevents the defect from the session, where a wait of 2^63 ns or more ended at once.
- If `add` gives `None`, `Time.after` gives `never`, and `Time.sleep` sleeps until a cancel. A negative span gives an event that is ready at once.
- Each domain keeps one deadline heap. A timer is a heap entry. The alarm is one event source in the poller of the domain.

## 2.7 The production worlds

Each domain has its own poller (section 2.5). A fiber registers an fd in the poller of the domain where it blocks.

**Edge-triggered readiness (decision 5). From milestone M1, one registration for each domain (decision 8):**

- The first time that a fiber blocks on fd X on domain B, B adds X to its own epoll set with `EPOLL_CTL_ADD`, for read and write together, with `EPOLLET`. If X is ready at that time, epoll gives an event at once, so no edge is lost. A test on Linux 7.1 verified this: an `EPOLL_CTL_ADD` with `EPOLLET` of a readable socket gave one event at the next `epoll_wait`. A second epoll set that added the same socket later also got one event, and each set got the next edge.
- A later block on X on domain B needs no syscall.
- The read flag, the write flag and the waiters are in the registration of B, and only B uses them. B sets a flag at an event and commits each waiter. A fiber on B that gets `EAGAIN` clears the flag of B. Thus no atomic readiness word is necessary.
- A fiber reads or writes until the call gives `EAGAIN`. The I/O modules do this loop, so fiber code does not see it.
- One busy socket can keep its fiber ready for a long time (M3). A read loop of the user can then run without a switch, because each read finds data. A write blocks when the buffer is full, so it cannot keep the fiber busy in the same way. M0 has no yield in the I/O modules. M3 adds a yield after a fixed number of bytes, in the place that a measurement shows. **Done in M3:** after 64 KiB of reads with no wait on one fd, a read yields, and this yield is not a cancellation point, because the read already took the bytes. Measured (`bench/m3.ml`, a reader of a full pipe that works 20 µs on each 64 KiB, and a timer of 1 ms on the same domain): with no yield, the timer was late by up to 1 s. With a yield each 64 KiB, by 0.57 ms on average and 1.2 ms at most, for a loss of about 7% of the throughput. With 256 KiB, by 4.9 ms on average. The rest of the lateness comes from the poll of a busy domain each 61 switches (open question 3).

**An fd in many pollers (M1).** A fiber can block on X on domain A, move, and block on X on domain B later. Then X is in the two epoll sets, and A gets edges that nobody waits for. Linux gives a new edge for each arrival of data, also when the only fiber of the fd is busy and not blocked. Thus an edge with no live waiter is normal, also with one domain. A removes X from its set when an edge of X comes on A with no live waiter on A, and another domain also has a registration of X: then the fiber of X moved. The removal is an `EPOLL_CTL_DEL`, a change of the domain bit and the release of the user of the registration (see `close`, below). A close notice does the same steps. The two run on the thread of A, so they never interleave. If a fiber blocks on X on A again, A adds X again. Open question 4 has the measurements.

**The waiters.** A commit on another branch leaves a stale waiter in a registration. The owner drops the waiters with a `Done` sync word when it adds a waiter and when an event comes. Thus a registration holds its live waiters and at most the stale waiters since the last add. Without this rule, a loop such as `choose [Net.readable fd; Time.after Mono 1s]` on a quiet connection adds one stale waiter each second.

**`close`.** A fiber can close an fd while other fibers wait on it or use it. Thus:

- **The state machine of Eio's `Rcfd`.** The record has one atomic state: `Open of users`, `Closing of users` and `Closed`. A `submit` adds a user with a CAS only in `Open`. In `Closing` or `Closed`, it gets `EBADF` and adds no user. Thus a late `submit` after `close` cannot take a count from 0 to 1 and back to 0. `close` changes `Open n` to `Closing n`. The side that changes `Closing 0` to `Closed` with a CAS calls `close(2)`, and only that side. Thus `close(2)` occurs exactly one time (`lib_eio/unix/rcfd.ml`).
- **One record for each open.** User handles hold the record object, and the library never uses the same object for a new fd. Only the slot in the table is used again, with a new generation. The kernel can give the same fd number to a new file after `close(2)`. Thus `epoll_data` holds the index of the slot and the generation, not the fd number, and a domain drops an event with an old generation.
- `close` never blocks, so it is not a cancellation point. A cancelled fiber can thus call it in cleanup code.
- **Registrations hold a user (M1).** Each registration in the poller of a domain holds one user. Register adds a user, then sets its bit in the domain set, then reads the state. If the state is `Closing`, Register undoes its steps and commits its waiter with `EBADF`. `close` sets `Closing` and then reads the domain set. These are Dekker pairs of atomics, so one side always sees the other. Register runs on the thread of the domain that owns the poller, and it does not suspend the fiber. That thread takes its mailbox only between fibers. Thus a domain never takes a close notice between the steps of its own Register.
- **Close notices (M1).** `close` sends a close notice with the slot index and the generation to the mailbox of each domain in the set, and it wakes these domains. A domain acts on a notice only if it holds a live registration for that pair. The removal of section "An fd in many pollers" also needs a live registration. The two run on the thread of the domain, one after the other, so only the first one finds the registration live. That one clears the domain bit, commits the waiters with `EBADF`, does `EPOLL_CTL_DEL` and removes the user of the registration. Thus a user is removed exactly one time, and `EPOLL_CTL_DEL` never names a reused fd number. The domain bit needs no CAS: only the owner sets and clears its bit, and the closer only reads the word to know which domains get a notice. But the bits of all domains share one word. Thus the owner changes its bit with `fetch_and_add` of plus or minus the bit, not with a read and a store. With a read and a store, two domains that register at the same time can lose the bit of one of them, and `close` then sends no notice to that domain. The TLA+ model of section 2.16 has a set that changes one element in one step, so it did not find this race. The review of M0 found it.
- `close(2)` comes after each `EPOLL_CTL_DEL`. An epoll registration lives until all fds of its open file description close (epoll(7)). A review verified this: with a `dup` alive, the registration gave events after `close(2)`.
- `close(2)` can block, for example on a socket with `SO_LINGER` and on an NFS or FUSE file. Thus the library gives the `close(2)` of a `File` fd, and of a socket with `SO_LINGER`, to the pool (milestone M2).
- **Close-on-exec.** The library makes each fd with this flag: `epoll_create1 (EPOLL_CLOEXEC)`, `EFD_CLOEXEC`, `TFD_CLOEXEC`, `SOCK_CLOEXEC` for `socket`, `socketpair` and `accept4`, `O_CLOEXEC` for `File.open`, `pipe2`, `MSG_CMSG_CLOEXEC` for fds that come with `SCM_RIGHTS`, and `F_DUPFD_CLOEXEC` or `dup3` for a copy. Thus a child from `Process.spawn` does not keep a copy.

**Linux:**

- One epoll set for each domain, with `EPOLLIN | EPOLLOUT | EPOLLRDHUP | EPOLLET`.
- **The alarm.** Each domain has one `timerfd` on `CLOCK_MONOTONIC` with `TFD_TIMER_ABSTIME`, and it calls `timerfd_settime` only when its earliest deadline changed. `epoll_pwait2` (Linux 5.11) takes a timeout in nanoseconds, but the kernel adds slack of about 0.1% of the timeout. A review measured it on Linux 7.1: `epoll_pwait2` was 10 ms late on a 10 s timeout, and the `timerfd` was 33 to 58 µs late. Thus the `timerfd` stays, and `epoll_wait` has no timeout.
- `Now Mono` reads `CLOCK_MONOTONIC` with `clock_gettime`, the same clock as the alarm. Go and Tokio also use `CLOCK_MONOTONIC`. This clock does not count a system suspend, so a 10 s timeout does not fire directly at a resume.
- There is no `Posix` alarm (section 2.6).
- Each domain has an `eventfd` in its own epoll set, for `wake` and for its mailbox.
- Signals go through a `signalfd` in the epoll set of domain 0. Child processes go through a `pidfd` in the set of the domain where the fiber waits (milestone M2). The `pidfd` tells that the child exited, but the child stays a zombie until `waitid (P_PIDFD, ..., WEXITED)` (Linux 5.4). If other code sets `SIGCHLD` to `SIG_IGN`, the exit status is lost (`ECHILD`).

**BSD and macOS:**

- One kqueue for each domain. `EV_CLEAR` gives the edge-triggered behavior. The alarm is an `EVFILT_TIMER` entry, `wake` uses `EVFILT_USER`, signals use `EVFILT_SIGNAL` on domain 0, and child processes use `EVFILT_PROC`.
- The kqueue world always gives `EVFILT_TIMER` a relative span from `Now Mono`, not an absolute time. I think that an absolute `EVFILT_TIMER` uses the wall clock: `NOTE_ABSTIME` on FreeBSD, and `NOTE_ABSOLUTE` on macOS unless `NOTE_MACHTIME` is set. I did not verify this.
- I think that macOS can combine near timers unless the entry has `NOTE_CRITICAL` or `NOTE_LEEWAY`, and that a relative timer does not count a system sleep while `CLOCK_MONOTONIC` on macOS does. I think that `EVFILT_PROC` on a child that already exited gives `ESRCH` on macOS, so the world then calls `waitpid` with `WNOHANG`. I did not verify these points, or `EVFILT_USER` on OpenBSD and NetBSD.
- The kqueue world comes after the Linux world is measured (section 2.14).

**Poll:** a fallback for systems with neither epoll nor kqueue. It uses `poll(2)`, so it has no `FD_SETSIZE` limit, as `select` has. It uses a timeout in milliseconds and not an alarm. It cannot be edge-triggered, so it simulates the flags with a level-triggered poll. It comes only if a user needs it.

### Signals (M2)

**How the worlds wait on a signal:**

- **Linux:** a `signalfd` is in the epoll set. It is readable when one of its signals is pending. A `read` gives one 128-byte `signalfd_siginfo` record for each pending signal, and the read consumes the signal. The world reads until `EAGAIN`, as for any other fd.
- **BSD and macOS:** `EVFILT_SIGNAL` in the kqueue. It does not replace the normal delivery. It records each delivery, also for a signal set to `SIG_IGN`. Thus the world sets each signal to `SIG_IGN` and then watches it. The `data` field gives the number of deliveries since the last `kevent` call. I took this from my knowledge of the FreeBSD `kqueue(2)` man page, and I did not verify it on macOS.

**The rules for `signalfd` (from the Linux man page):**

1. **Block the signal first, in every thread.** If a signal is not blocked, the kernel does its normal action, and the signal does not go to the fd. Each thread has its own signal mask, and a new thread copies the mask of the thread that made it. A thread cannot block a signal in other threads later.
2. **A thread reads only its own signals and the process signals.** `kill`, Ctrl-C and `SIGTERM` go to the process, so all threads see them. `pthread_kill` goes to one thread, and only that thread sees it.
3. **Normal signals do not queue.** Two `SIGUSR1` signals before one read give one record. Only real-time signals (`SIGRTMIN` and higher) queue.
4. **`fork` problem.** After `fork`, the epoll of the child does not report the inherited `signalfd` as ready. The child must close it and make a new one. The world makes the fd with `SFD_NONBLOCK | SFD_CLOEXEC`.
5. **`SIGKILL` and `SIGSTOP` cannot be caught.** The fd ignores them.

**What the library does:**

- **`run ~signals:[...]`** blocks the selected signals in the main thread, before it starts a domain or a thread, including the thread pool. All of the threads thus copy the mask. The default set is empty. A program names each signal that it wants as an event, for example `~signals:[SIGINT; SIGTERM]`. With a default set, a program that does not wait on `SIGINT` does not stop at Ctrl-C.
- **Domain 0 holds the signal source.** The poller of domain 0 reads it. The event goes to the waiting fiber through the ready queues, and any domain can run that fiber. If domain 0 is busy, a signal waits until its next poll.
- **`SIGPIPE` is set to `SIG_IGN`.** A write to a closed socket then gives `EPIPE` and does not stop the process.
- **`Process.spawn` restores the mask in the child.** A child inherits the blocked mask, and without a change it never sees `SIGINT` or `SIGTERM`. `posix_spawn` with `POSIX_SPAWN_SETSIGMASK` prevents this. Also, a child inherits `SIG_IGN` across `exec`. Thus `POSIX_SPAWN_SETSIGDEF` resets `SIGPIPE` to `SIG_DFL`. On BSD, it also resets each watched signal, because the world sets these signals to `SIG_IGN`.
- **No `SIGCHLD`.** Child processes use `pidfd` (Linux) and `EVFILT_PROC` (kqueue). These give one event for each child, so rule 3 has no effect on them.
- **`pidfd_spawn`.** On Linux, `Process.spawn` uses `pidfd_spawn` (glibc 2.39 and later). Thus the `pidfd` exists before any code can reap the child. With an older glibc, it uses `posix_spawn` and then `pidfd_open`. Then the pid is safe only if `SIGCHLD` is not `SIG_IGN`, `SA_NOCLDWAIT` is not set, and no other code reaps the child (pidfd_open(2)).
- **The pool threads block all signals** (section 2.3).
- **Signals from a CPU fault are not events.** `SIGSEGV`, `SIGBUS` and `SIGFPE` go to the thread that caused them, at that instruction. They cannot wait in a queue.
- **The event:** `Signal.received s : int event` gives the number of times that `s` occurred since the last commit of a `received s` event. On Linux this number can be 1 for many normal signals (rule 3), so code must not depend on the exact count.
- **One fiber gets each signal.** The world keeps one count for each watched signal. A commit of `received s` takes the count and sets it to 0. If two fibers wait on `s`, only one of them gets it. A signal that comes when no fiber waits stays in the count until the next commit. For a broadcast, one fiber waits on the signal and fills an `Ivar` that many fibers read.
- **The simulator:** a test calls `Sim.signal sim SIGTERM`, and the simulator makes the event ready at the next switch point. A test can thus check a graceful shutdown with an exact result.

**Why not the other methods:**

- **The self-pipe method.** A signal handler writes one byte to a pipe in the poller. It works on all systems, and affect uses a similar "signal bypass fd". But in OCaml, the handler runs only at a safe point in OCaml code. I think that a domain asleep in `epoll_wait` gets `EINTR` only if the kernel delivers the signal to that thread. I did not verify this. The Poll world uses this method, because it has no other one.
- **`epoll_pwait` with a signal mask.** It changes the mask only during the wait. The signal then ends the wait with `EINTR`. This gives an interrupt, not an event.

**Not sure:**

- If C code makes its own thread before `run`, that thread does not have the blocked mask. A process signal can then go to that thread, and the kernel does the normal action.
- The tick thread of systhreads and the backup thread of each domain block all signals when the runtime makes them (`st_stubs.c`, `domain.c`). Thus they are not a problem. A review verified this. These threads stay after `Thread.join` and `Domain.join`, so a process often has more than one thread at `run`.
- Thus, on Linux, `run ~signals` reads the `SigBlk:` line of each `/proc/self/task/*/status`. It writes a warning to `stderr` only for a thread that does not block the selected signals.
- The OCaml 5 runtime installs no signal handler of its own. It only sets an alternate stack, and it changes `SIGPROF` only if a handler exists (`runtime/signals.c`). A review verified this.

## 2.8 The simulator world

The simulator handles the world effects in pure OCaml, on one OS thread, with its own handler (section 2.3). Its state is:

- A virtual time for each line. `Posix` follows `Mono` (`posix = p0 + (mono - m0)`), and a test can make `Posix` jump with `Sim.set_posix`. A jump changes only `Now Posix`. The timers are `Mono` timers (section 2.6), so they do not move, as in production.
- An in-memory network: listeners, connections and buffers. It can add latency, loss, reordering and partitions, all from the seed. These faults and the latency are off by default. A test enables them with `Sim.net_faults`.
- An agenda: the timed events of the simulator itself. Examples are a packet with latency, a fake process that exits at a time, and the time cost of a `Blocking` operation (open question 2).
- From milestone M2: an in-memory file system, a table of fake processes and a name table for `Dns`.
- A PRNG from the seed, for `random`, `random_bytes`, the network faults and the order of the virtual domains. The simulator has its own fixed PRNG, for example SplitMix64, and not `Stdlib.Random`, because the algorithm of `Stdlib.Random` can change between OCaml versions (it changed in 5.0).
- From milestone M1: one or more virtual domains (decision 4). The default number of virtual domains is a constant, not `Domain.recommended_domain_count ()`. Thus a seed gives the same result on a CI machine with 2 CPUs and on a laptop with 16 CPUs. The failure report shows the number.

### The `Sim` API

```ocaml
module Sim : sig
  type t

  val run :
    ?seed:int -> ?domains:int -> ?auto:bool -> (t -> 'a) -> 'a
  (** Runs [main] in the simulator. [~auto:true] (the default) moves the
      time at each quiescence. With [~auto:false], only [advance_until]
      moves the time. [~domains] is in milestone M1. *)

  val settle : t -> unit
  (** Waits until the program is quiescent. *)

  val advance_until : t -> Clock.mono -> unit
  (** Moves the time one deadline at a time, and settles at each one. *)

  val addr : t -> string -> Net.addr
  val set_latency : t -> Span.t -> unit       (* M0: the only fault *)
  val set_posix : t -> Clock.posix -> unit

  (* Milestone M2 *)
  type net_faults = { jitter : Span.t; short_io : bool; extra_edges : bool }
  val net_faults : t -> net_faults -> unit
  val set_blocking_cost : t -> Span.t -> unit  (* open question 2 *)
  val set_dns : t -> string -> dns_answer -> unit
  val add_program : t -> string -> (string list -> program) -> unit
  val signal : t -> Signal.t -> unit

  exception Deadlock of string list           (* the blocked fibers *)
  exception Lost_wakeup of string list
  exception Livelock of string list           (* milestone M3 *)
  exception Model_error of string
  (** The program broke the order of the OS, for example a close(2)
      before a Deregister. *)
end
```

### Virtual domains (M1)

A virtual domain is one scheduler instance with its own queues, poller registrations, heap, mailbox and wake source. The simulator world keeps the registrations of each virtual domain. The simulator runs all of the virtual domains on one OS thread:

1. Each world effect from a virtual domain is a switch point. The simulator keeps the continuation of that domain.
2. The scheduler also performs a `Switch` world effect each time it starts a different fiber, but only in the simulator: the scheduler knows that it runs in the simulator (`Sched.create ~simulated:true`), and production does not pay for the effect.
3. At each switch point, the PRNG selects the virtual domain that continues.

Work stealing between virtual domains is thus deterministic, and the seed sets the order. For this to work, the scheduler obeys three rules:

- It blocks only in `Wait`. It never blocks on a `Condition`. An idle domain waits in its own `Wait`, and another domain ends that wait with `wake`.
- It holds a lock only for a short time, and never across a world effect. Thus, in the simulator, a lock is never contended, and a CAS retry never waits for another virtual domain.
- It never uses DLS, and the world handler resumes each virtual domain from one top-level loop (section 2.5, How a fiber finds its domain).

Fiber code must obey the same rules (section 2.5, Locks for fiber code).

The simulator finds errors in the order of events between domains, at the level of world effects and fiber switches. It does not try the other orders of the atomic operations between two switch points, because a virtual domain does not stop between them. For the lock-free parts, a model checker such as `dscheck` is necessary (section 2.14, milestone M1).

### The model of the fds

The simulator copies the edge-triggered behavior of Linux strictly. If it gave more edges than Linux, it would hide code that does not read until `EAGAIN`. The rules:

- Each virtual domain gets its edges independently, as each epoll set does.
- A Register of an fd that is ready gives one edge at once. A review verified this on Linux, also after an `EPOLL_CTL_DEL` and a new `EPOLL_CTL_ADD`.
- By default, an edge comes when the state changes, for example when data arrives. A test can enable more edges from the seed with `Sim.net_faults`.
- The end of a stream and `EPOLLRDHUP` give edges.
- The simulator checks the order of production: a `close(2)` must come after the Deregister of each domain in the set, and a Deregister or an event must never name a closed or reused pair of slot and generation.

### The `wait` steps

The program is quiescent when each virtual domain is in `Wait` with nothing ready. Then the simulator does these steps in this order:

1. Check for lost wakeups (the list that follows). If one item is not true, a wake was lost. Raise `Lost_wakeup` with the fibers concerned.
2. If the network, the file system or the fake processes have a ready event, give it. Also give each pending `Sim.signal`, and in milestone M3 each pending fill from outside a fiber.
3. If a test fiber waits in `Sim.settle`, let it continue.
4. In auto mode, find the earliest time of all heaps of the virtual domains and of the agenda of the simulator. Set the time to it. Then give the alarm to each domain whose deadline is at or before the new time, and give each event of the agenda that is due. This is the Go and Tokio model, one deadline at a time.
5. If nothing of steps 2 to 4 occurred, raise `Deadlock` with a list of the blocked fibers. In manual mode, this also occurs when deadlines exist, because only `advance_until` moves the time. The message then tells that the simulator is in manual mode.

The check of step 1 reads the scheduler through `Sched_debug` (section 2.2). These items must be true:

- Each local queue, next slot, mailbox, the pinned queue and the injection queue is empty.
- No live waiter waits on an fd that the in-memory network tells is ready.
- No live heap entry has a deadline at or before the current time.
- No fiber has a sync word with `Done` and is not in a queue and not running.
- No domain has a `wake` that it did not take.

Without this check, the simulator takes a lost wakeup as quiescence and moves the time. A test then gives an incorrect timeout and not a clear error.

A `Blocking` operation (milestone M2) does not complete at once. The simulator puts its completion in the agenda, with a time cost and a later switch point from the seed. The completion goes through the mailbox of the domain, as in production. Thus the race of a cancel and a completion, and the detach path, run in simulator tests.

**Livelock (M3).** A loop such as `while Time.now Mono < t do Fiber.yield () done` ends in production. In the simulator, the program is never quiescent, so the time never moves, and the loop never ends. Go's `synctest` has the same property. Thus the simulator counts the switch points since the last quiescence in which the only world effects are `Now`, `Switch` and yields. If the count is more than 10^7, the simulator raises `Livelock` with the fibers that run. A test that streams much data with no timer is thus not a livelock. A test can change the limit, and 10^7 is a start value.

### A test example: the 408 from caravan

The test is a windtrap suite in a `test` stanza of dune (section 2.17). The `server` library is also in the guard `test/nounix` (section 2.3), so the build gives an error if it links `unix`:

```text
(test
 (name t408)
 (libraries weft weft.sim server windtrap))
```

```ocaml
(* t408.ml: a head that is not complete gives 408 after 10 s *)
let () =
  Sim.run ~seed:42 @@ fun sim ->
  Fiber.scope @@ fun scope ->
  let addr = Sim.addr sim "server:80" in
  let l = Net.listen addr in        (* before the fork: no race with connect *)
  Fiber.fork_daemon scope (fun () -> Server.serve config l);
  let c = Net.connect addr in
  Net.write c "GET / HT";
  let t0 = Time.now Mono in
  let r = Http.read_response c in
  assert (r.status = 408);
  assert (Time.diff Mono (Time.now Mono) t0 = Span.s 10)
```

The library code of `weft` and `weft.sim` has no inline tests (`let%test`). The runtime libraries of `ppx_windtrap` link `unix`, so an inline test would make each program that links the library link `unix` (section 2.17).

The test reads like production code. The read blocks, the program becomes quiescent, and the simulator jumps to 10 s. The time does not move while the program is not quiescent, and the latency is 0 by default. Thus the server arms its deadline at the same virtual time as `t0`, and the difference is exactly 10 s. From milestone M1, the same test runs with `~domains:4`.

The test opens the listener before it forks the server. If the server opened it, some seeds would run `connect` first and get `ECONNREFUSED`. When the body of the scope returns, the scope cancels the daemon server fiber, so the test ends. A check "no response at 9.999 s" is `sync (choose [response c; Time.after Mono (Span.ms 9999)])`. In auto mode, the 9.999 s deadline expires first, and the result is exact.

For a manual check point, `Sim.settle sim` waits until the program is quiescent. `Sim.advance_until sim t` moves the time one deadline at a time and settles at each deadline.

## 2.9 What makes the simulator deterministic

- **One OS thread.** The virtual domains run on one thread, and the seed sets their order.
- **Every input of the library comes from the world.** Time, I/O, random numbers, the `choose` order and the domain order go through world effects. The I/O modules cover the full Unix I/O API, so fiber code has no reason to call `Unix`.
- **Other inputs that the simulator does not control.** A direct call to `Unix`, the stdlib I/O (section 2.3), `Random.self_init`, a `Hashtbl` with a random seed, `Sys.time` and environment variables. A test must not use them, or must give them a fixed value.
- **The GC.** A fill from a finaliser (milestone M3) depends on the time of the GC. Such a test gives the same result only in the same binary with the same history (section 2.5).
- **The host.** The default number of virtual domains is a constant, and the PRNG is part of the library (section 2.8). Thus a seed does not depend on the machine or on the OCaml version.
- **No real fds.** The simulator does not use loopback sockets, so no data comes in from outside.
- **One seed.** When a test fails, its seed lets you run the same test again with the same result. A test can also run with many seeds to find more orders.

## 2.10 Why effects and not a value that each layer carries

| | A value that each layer carries (caravan) | A world effect (this design) |
|---|---|---|
| Code change in each layer | A `time` field or argument | None |
| A layer that forgets the seam | Uses the real clock without a warning | A layer that uses this library cannot forget it. A layer that calls `Unix`, the stdlib I/O or its own C stubs goes past it. `(forbidden_libraries unix)` finds only the use of the `unix` library (section 2.3). |
| Code in other libraries | Bypasses the seam | Uses the handler, if it uses this library |
| Fake sockets | A separate seam | The same handler |
| Type safety | The compiler checks the module types | OCaml 5 effects are not typed. A missing handler gives `Effect.Unhandled` when the program runs. `run` installs the handler for all fibers. Code outside a fiber has no handler, and the library raises `Fiber.Not_in_fiber` there (section 2.5). |
| Threads and domains | No problem | Effects do not cross `Domain.spawn` or `Thread.create`. `run` installs the handler in each domain that it starts. A thread or a domain that user code starts has no handler. |

Flix has the same idea (`@DefaultHandler`, and a test replaces the handler). Flix rejects a user effect in a `spawn` ("Illegal spawn effect"). This design prevents that problem, because the world handler is below the scheduler, and the library starts each domain with the handler.

The cost of an effect is a perform and a handler call for each world operation. A review measured 5.6 ns for one perform and continue (section 2.13). This is small compared with a syscall.

## 2.11 What comes from where

| Part | Source |
|---|---|
| Poll and block for each base event, ready calls separate from blocked calls, guards | affect |
| First-class events, `choose`, `wrap`, `guard`, `with_nack` | CML |
| Time and I/O as effects, a default handler, a test handler | Flix |
| Auto-advance when quiescent, one deadline at a time | Go `synctest`, Tokio |
| `Sim.settle` (wait until quiescent) | Trio `wait_all_tasks_blocked`, Go `synctest.Wait` |
| One alarm in the poller (`timerfd`, `EVFILT_TIMER`) | The session's `CLOCK` + `ALARM` design |
| `Mono` and `Posix` lines with a typed clock | The caravan time seam |
| A FIFO local queue, a next slot, a global injection queue, stealing half a queue | Go, Tokio |
| A queue for pinned fibers | affect `only_main_queue` |
| Scopes, daemon fibers and cancellation | Eio switches, Trio nurseries |
| `Fiber.shield`, and "the cancel wins at a checkpoint" | Trio |
| The commit protocol of `choose` across domains | Parallel CML (Reppy, Russo and Xiao, 2009) |
| Full stealing. The `Blocked` state with a retry on `Claimed`. Pinned fibers on the main domain. The atomic state of an `Ivar` (`cell.ml`). The idea of one fd map for each domain, from a comment in `affect_unix__select.ml`. | affect |
| The rules for the `searching` count (M3), a steal of the next slot | Go |
| A lock for each channel | Parallel CML |
| A join of a scope that ignores cancels | Eio `Switch.await_idle` |
| A deadline on a cancel context | Trio |
| The cancel contexts of a fiber, the shield | Trio cancel scopes, Eio cancellation contexts |
| The state machine of an fd record, and the one `close(2)` | Eio `Rcfd` |
| `fork_isolated ~on_error`, the poisoned mutex, `use_rw ~protect` and `use_ro` | Eio `run_server`, Eio `Mutex` |
| The handler of a scope, `Promise.ready`, `Promise.cancel` | affect `Call_handler`, `get'`, `cancel` |
| A simulator with a network and a seed | Shadow (from the session). FoundationDB, TigerBeetle, madsim and turmoil use the same idea. We did not examine them in the session. |

## 2.12 Decisions

Decisions 1 to 6 were made on 2026-10-09, and decisions 7 to 9 on 2026-10-10.

| No. | Question | Decision |
|---|---|---|
| 1 | Start point | New code. Parts of affect are copied where they fit. |
| 2 | Names | CML: `event`, `sync`, `choose`, `wrap`, `guard`, `with_nack` |
| 3 | Spans | One common `Span.t` for the two time lines |
| 4 | Many domains in the simulator | Yes, in the first version, as virtual domains |
| 5 | epoll mode | Edge-triggered (`EPOLLET`, and `EV_CLEAR` on kqueue) |
| 6 | Unix in fiber code | Wrap the full Unix I/O API |
| 7 | Stealing | Full stealing, as in affect. A fiber that ran can move, but its registrations stay on the domain where it blocked. |
| 8 | Fd registration | Edge-triggered, with one registration in each domain where a fiber blocks on the fd |
| 9 | Tests | windtrap 0.2.0: property, stateful and expect tests, coverage and mutation testing (section 2.17) |

Also on 2026-10-10, the user accepted the milestone plan of section 2.14. Decisions 4, 7 and 8 stay, and they come in milestone M1. Milestone M0 has one domain.

## 2.13 New open questions

1. **How close the simulator is to the OS (M2).** The in-memory file system and the fake processes must copy the OS errors, for example `ENOENT`, `EEXIST` and a short write. How much of this is necessary in the first version? **Answered** on 2026-10-11: the simulator copies the errors that the API of M2 documents (`ENOENT`, `EEXIST`, `ENOTDIR`, `EISDIR`, `ENOTEMPTY` and `EBADF`), and the short reads and writes are an option of `Sim.net_faults`.
2. **Blocking operations in the simulator (M2).** A `File` operation on the thread pool takes time in production. Does the simulator give it a time cost from the seed, or zero time? **Answered** on 2026-10-11: a time cost from the seed, in the range 0 to a limit that the test gives with `Sim.set_blocking_cost`. The limit is 0 by default, as the latency of the network. With a limit of 0, the operation occurs at once, and its fiber continues at a later switch point.
3. **The value of `K` (M1).** A busy domain polls each `K` fiber switches (section 2.5). A small `K` gives exact timers and costs more syscalls. A large `K` costs less but makes timers late under load. **Measured** (`bench/m1.ml`, 1 domain with 200 fibers that yield, a 1 ms timer, Linux, 16 CPUs): with `K` = 1, the domain does 1.8 million yields per second, and the timer is late by 109 µs on average. With `K` = 61, it does 13.8 million yields per second, and the timer is late by 19 µs on average and by 59 µs at p99. With `K` = 1024, it does 15.6 million yields per second, and the timer is late by 38 µs on average. `K` stays 61. After the review of M1 (no `Switch` in production), `K` = 61 gives 15.6 million yields per second, and the timer is late by 17 µs on average and by 37 µs at p99.
4. **The cost of edges that do nothing (M1).** With decision 8, an fd can stay in the poller of a domain that no longer waits on it (section 2.7). **Measured** on an echo server with 64 clients: with no removal, 50%, 75% and 87% of all edges had no live waiter on 2, 4 and 8 domains. With the removal of section 2.7 (An fd in many pollers), the values are 30%, 43% and 52%, with the same throughput. The rest are the edges of data that comes while the fiber still runs, as on one domain.
5. **The limit of the next slot (M3).** The start value is 3 runs in sequence (section 2.5). This must be measured. **Measured** (`bench/m3.ml`, two fibers that send to each other 200,000 times through two channels, and a third fiber that yields): on 1 domain, no next slot gives 1.15 million round trips per second, a limit of 3 gives 1.32 million, and no limit gives 1.57 million. But with no limit, the third fiber gets no yield during the ping-pong, so it starves. With a limit of 3, it gets 0.5 yields for each round trip. On 4 domains, another domain takes the third fiber, and the round trips are 0.85 to 1.0 million per second with each limit. The limit stays 3.
6. **The default number of domains (M1).** The start value is 4 (section 2.5). The GC pauses and the throughput must be measured for different numbers of domains. **Measured** on the echo server: 93,000, 161,000, 290,000 and 516,000 round trips per second on 1, 2, 4 and 8 domains. More domains gave more throughput up to 8 on this machine with 16 CPUs. I did not measure the GC pauses. The default stays `min 4 (Domain.recommended_domain_count ())` until M3 measures the pauses.
7. **The limits of the watchdog (M3) and the slack (M0).** The start values are 10 ms (section 2.5, The limit of a busy domain) and 1 ms (section 2.5, Timers and domains). **Measured** (`bench/m3.ml`, 4 domains, 1,000 fibers, 10 sleeps of 1 to 20 ms each): with a slack of 0, a sleep is late by 13 µs on average and by 340 µs at p99, with 5,981 calls of `timerfd_settime`. With 100 µs: 63 µs, 162 µs and 1,720 calls. With 1 ms: 475 µs, 1,288 µs and 410 calls. With 10 ms: 4.7 ms, 9.8 ms and 74 calls. A slack of 0 has a worse p99 than 100 µs, I think because of the many calls. The slack stays 1 ms until the user selects a value. The watchdog of 10 ms gave two reports on the loads of the benchmarks, and each one was correct: a loop of 100,000 forks and a loop of 200,000 calls of `with_timeout`, each with no switch for more than 10 ms. The limit stays 10 ms.
8. **The cost of more wakes (M1).** With no `searching` count, a push wakes an idle domain each time that one exists (section 2.5, Idle and wake). This must be measured on a server with many short fibers. If the cost is too high, M3 adds the Go rules. **Measured** with 100,000 short fibers: 1.1, 2.9, 2.1 and 0.7 million fibers per second on 1, 2, 4 and 8 domains, with 0, 0, 0.13 and 0.97 wakes for each fiber. On 8 domains, nearly each fork wakes an idle domain, and the throughput is 4 times less than on 2 domains. After the review of M1 (the scan order and the wakes), the values are 1.5, 3.5, 2.8 and 0.9 million fibers per second, with 0.8 wakes for each fiber on 8 domains. Thus the cost is still too high, and M3 adds the Go rules. On the echo server, the wakes are less than 0.01 for each round trip, because the domains are busy. **With the Go rules (M3):** 1.5, 3.0, 2.8 and 1.5 to 1.8 million fibers per second on 1, 2, 4 and 8 domains, with 0.40 to 0.51 wakes for each fiber on 8 domains (three runs). The echo server does not change. Thus the rules halve the wakes and double the throughput on 8 domains, but 8 domains are still slower than 2 on this test.

**An answer to an earlier question: the cost of `Switch`.** A review measured one perform and continue with one deep handler at 5.6 ns (OCaml 5.5.1, `-O3`, a loop with warm caches). Each more handler level costs about 3 to 5 ns. Thus the `Switch` effect is cheap, and the scheduler does not need to be a functor over the world. The real costs are the syscalls and the wakes between cores. For example, an empty `epoll_wait` costs about 0.26 µs, and `timerfd_settime` about 0.6 µs (measured).

## 2.14 The milestones

The work has four milestones, M0 to M3. Each milestone has a "done" list, and a milestone is complete only when all items of its list are true. The core comes first, and each later milestone adds parts on top of it with no change to the API of the core. A later milestone can change the inside of the core, for example to add domains. Many tests use only the public API, with no `Sim` call. These tests run on each world.

The sections of this document mark each part that does not come in M0 with its milestone, for example "(M2)".

### M0: one domain, on the simulator and on Linux

**Contents:**

- The two layers of effects and `WORLD` (sections 2.2 and 2.3). The simulator, and a Linux world with epoll (edge-triggered), the `timerfd` alarm and `eventfd`. One domain.
- `sync`, `choose`, `wrap`, `guard`, `with_nack`, `always`, `never`, `Chan` and `Ivar` (section 2.4).
- The commit protocol of section 2.4 as a separate module with no effects, a functor over `Atomic`. It is correct for many domains from the first day, but it runs with one domain. `dscheck` needs this form in M1.
- `Span`, `Clock`, `Time.now` on the two lines, and `at`, `after` and `sleep` on `Mono` (section 2.6).
- `Fiber.scope`, `fork`, `fork_daemon`, `cancel`, `yield` and `shield`, with the shield node, the join that ignores cancels, the sticky cancel, the backtraces and `Multiple` (section 2.5).
- `with_timeout` with a daemon fiber, and `fork_promise`, `Promise.await` and `Promise.ready`.
- `Net` with TCP and Unix sockets, and `close` with the `Rcfd` state machine (section 2.7).
- The simulator with the seed, auto mode and manual mode, `settle`, `Deadlock`, `Lost_wakeup`, the agenda and the strict model of the fds (section 2.8).

**Done when:**

- An echo server and a client run in one `Sim.run`. The client sends "hello" and gets "hello" back. The test passes with 1,000 seeds.
- The same echo server runs on the Linux world as a real process, and a client in a different process, for example `nc`, gets its data back.
- All tests that use only the public API pass on the simulator and on Linux.
- The 408 test of section 2.8 passes on the simulator with 1,000 seeds.
- `Time.sleep Mono (Span.s 3600)` ends in less than 1 s of real time on the simulator. After it, `Time.now Mono` shows exactly 3600 s more.
- On Linux, without load, a 10 ms sleep ends between 10 ms and 12 ms, and a 10 s sleep ends between 10 s and 10.002 s. The limits include the slack of 1 ms.
- Tests pass for each rule of section 2.5, Fibers, scopes and cancellation: an error in a child cancels the body, a cancel goes down to a nested scope, the cleanup example with a shield sends its frame, the join waits for all children also when a child fails, and `with_timeout` raises `Timeout` at exactly the deadline.
- The backtrace of a child is present after `Fiber.scope` and after `Promise.await`.
- A guard that raises enables the nacks of the guards that ran before it.
- A cancelled scope leaves no fibers, and no live heap entries or offers.
- A `send` and a `recv` that start at the same time on an empty channel meet.
- A test with a fiber that waits forever gets `Deadlock`, with the name of that fiber. A planted lost wakeup gives `Lost_wakeup`, not an incorrect timeout.
- 10,000 connections open and close on Linux with no fd leak, and a late `write` after `close` gives `EBADF` and closes nothing.
- The guard `test/nounix` with `(forbidden_libraries unix)` builds, and it links `weft` and `weft.sim`.
- Each module of the core has its windtrap tests: property tests for each law, expect tests for long text, and a mutation run with no survivor that is not dismissed with a reason (section 2.17).
- The TLA+ models of section 2.16 pass (`spec/tla/check.sh`).

**The state of M0 (2026-10-10).** The code is in `src/core`, `src/sim` and `src/linux`, and each item of the "done" list has a test. The code settled these points of the design:

- **The slack.** A timer never fires before its deadline (section 2.5, Timers and domains). A test of M0 found that the old rule ended a sleep of 1 ms at once.
- **`Net.read`** needs at least one byte. A read of 0 bytes gives 0, the same result as the end of the stream.
- **No yield in the I/O modules** in M0 (section 2.7).
- **`close` in M0** holds a user of the record while it removes the registration of its own domain and commits its waiters. Then it releases the user, and the side that makes the state `Closed` calls close(2). Thus close(2) comes after the Deregister. The close notices of section 2.7 come in M1.
- **The external completions.** `Ready.External id` lets the simulator end `Sim.settle` and `Sim.advance_until`. The completed `Blocking` operations of M2 can use the same form.
- **The fiber record.** Fiber code reaches the record of its own fiber with a program effect, `Self`. Only `Fiber.scope`, `Fiber.shield`, `with_timeout` and the reason of a cancel use it.
- **The faults of the simulator.** M0 has only the latency (`Sim.set_latency`). Loss, reordering and partitions come with `Sim.net_faults` in M2.
- **The tests.** The tests of the public API are in `test/portable`, and the simulator and Linux run the same tests. The exact virtual times are only in the simulator suite. A test of the spin lock with two domains skips itself in a mutation run, because a mutation run forks after the test started a domain.
- **The review of M0** (section 3.6) found 7 bugs, and each one now has a regression test that fails on the old code. It also found these risks for M1, which M1 must solve:
  - A Register holds no user, and its order is not the order of `Fd.tla` (user, then bit, then state, then ADD). On one domain this is safe, because no other fiber runs between the steps.
  - `push` does not read `idle` and calls no `wake`, and the loop has no second check before a sleep (section 2.5, Idle and wake).
  - The table of live fibers for `Sched_debug` costs a lock and a table change at each fork. M3 makes it a part of the simulator only.
  - A sync allocates about 130 words. M3 adds a fast path for one base event.
- **The world effects.** `Wake` and `Random_bytes` have no producer in M0, and the worlds ignore `Switch`. They stay in the contract, because M1 (`Wake`, `Switch`) and M2 (`Random_bytes` for keys) need them.

**What M0 proves:** the `WORLD` contract (`Wait`, `Submit`, `Register`, the tokens) works on a real OS and on the simulator. The core API is complete enough for a server with timeouts. The design meets a real OS in the first milestone, not at the end.

### M1: many domains

**Contents:**

- `run ~domains:n`, with full stealing as in decision 7, and the registrations for each domain of decision 8 (sections 2.5 and 2.7).
- A FIFO ring for each domain, a steal of half a queue, and the `idle` set with the second check. The step value to the loop, with no DLS.
- The cancel walk across domains, the scope state word and lock, the domain set of an fd record and the close notices.
- Virtual domains in the simulator (decision 4).
- `dscheck` for each shared protocol.

**Done when the TLA+ model of section 2.4 passes for the code of M1, and a `dscheck` test passes for each of these parts.** `dscheck` needs each part as a module with no effects, a functor over its traced `Atomic`. Each test also finds a planted bug. For example, remove the second check, and the `idle` test must give a failure.

- The local queue: a push by the owner and a steal by two thieves
- The `idle` protocol: a domain that goes idle while another domain pushes work
- The commit of `choose`: a commit during the scan, a one-word partner while a claimer holds the word, two partners and one parked `sync`, and two fibers that sync on each other
- The atomic register: a `send` and a `recv` on an empty channel at the same time, and a fill of an `Ivar` during a register
- The self-commit: a yield and a cancel at the same time
- The cancel: a cancel walk while a fiber starts a `sync`, and a cancel walk while a fiber leaves the scope
- The scope: a `fork` while the last member ends
- The fd record: a `submit` and a `close` at the same time, a late `submit` after `close(2)`, a Register and a `close` at the same time, and a close notice and a removal of section 2.7 at the same time

**Also done when:**

- The echo test, the 408 test, a chained-timer test and a work-stealing test pass on the simulator with `~domains:4` and 10,000 seeds.
- Each planted bug gives a failure for at least one seed: a lost wakeup, a timer that arms from the incorrect time, a server that does not get its nack, and a commit with no `Parked` state.
- A seed that gives a failure gives the same failure, with the same order of events, when it runs again.
- A fiber that a thief stole uses the state of the thief at its next effect.
- A fiber that blocks on one fd from two domains in sequence gets all of its data, on the simulator and on Linux.
- Open questions 3, 4, 6 and 8 have measurements.

**The state of M1 (2026-10-11).** Each item of the "done" list has a test, except one: I did not measure the GC pauses for open question 6. The code settled these points:

- **`dscheck` examines the real code.** The test `test/dscheck` copies the sources of the core, and its own module `Atomic` (the traced atomics of `dscheck`) shadows `Stdlib.Atomic` in these copies. Thus production keeps the inline atomics, and no part of the scheduler is a functor over `ATOMIC`. Its own module `Domain` ends a trace after a few spins with no write, because a spin loop has traces with no end. A liveness mode explores random orders with a high limit of spins, and finds a deadlock. Each of the 15 tests finds a planted bug.
- **`dscheck` sees only atomic locations.** The two lists of each side of a channel are atomic locations, also under the lock of the channel. With plain fields, `dscheck` could not interleave a look and a register, and the test did not find a channel with no lock.
- **The table of fd slots** has a stdlib `Mutex`, not a spin lock. It is global, and its critical sections have no atomic operation and no effect.
- **A wake is a world effect.** A push only marks the wake, and the scheduler does it at a point where it holds no lock. In the simulator, a wake is a switch point, and a switch inside the lock of a channel would stop each other virtual domain that takes the lock.
- **The end of a loop.** Each close came before the end of `main`, because the root scope waits for each fiber. Thus a domain takes its last close notices when it leaves its loop. Without this rule, a test on Linux with 4 domains leaked one fd in about 1 run of 5.
- **The tests of the public API** use an atomic log on many domains. A plain `ref` that fibers on many real domains change loses updates, and an order of two fibers is sure only on one domain.
- **The planted bugs** of the "done" list each gave a failure: a close notice with no wake (`Lost_wakeup`), the alarm from the heap of another domain (`Deadlock`), no nacks (`Deadlock`), and a commit with no `Parked` state (`Internal_error`).

**What M1 proves:** the protocols are correct with many domains, and the seed replays the order of the virtual domains.

### M2: the library layers

**Contents:** these parts sit on top of the core, so they need no change to the API of M0 or M1:

- `Signal`, `Process`, `Pipe`, `File`, `Dir` and `Dns` (decision 6), with their simulation (section 2.8)
- The two C thread pools, the mailboxes and the detach path (sections 2.3 and 2.5)
- `Fiber.Mutex` and `Fiber.Condition` (section 2.5, Locks for fiber code)
- `fork_isolated`, `Fiber.scope ~handler`, `Promise.cancel` and `Promise.Cancelled`
- The kqueue world, and the `Poll` world if a user needs it

**Done when:**

- Each operation of `File`, `Dir`, `Process` and `Dns` has a test on the simulator. The errors of open question 1 are present and have tests.
- A graceful shutdown test with `Sim.signal sim SIGTERM` passes on the simulator, and `SIGTERM` gives a graceful shutdown on Linux.
- A child from `Process.spawn` stops at `SIGINT`.
- `with_timeout` around a `Dns.resolve` that never answers ends at the deadline, on the simulator.
- One handler of `fork_isolated` that raises does not stop the other connections.
- `Promise.cancel` cancels only its own fiber, and `await` then raises `Promise.Cancelled`.
- A user effect from a fiber of a nested scope reaches the handler of the outer scope.
- All tests of M0 and M1 pass on macOS and on FreeBSD, and each kqueue fact in section 2.7 that has "I did not verify" is now verified or replaced.

**The state of M2 (2026-10-11).** Each item of the "done" list has a test, except the last one. This machine has only Linux, so the kqueue world does not exist yet. The code settled these points:

- **The jobs.** A `Blocking` operation is a job. `Op.Start (domain, op)` gives the job to the pool and gives its id. The completion comes to the `Wait` of that domain as `Ready.External id`, the same form as the end of `Sim.settle`. Then `Op.Finish (id, op)` gives the result with the type of `op`. On a cancel, the fiber sends `Op.Detach id` and raises at once.
- **A job holds a user of its fd** until the job completes, also after a detach. Thus the close(2) of a file waits for each job that uses the fd, and the pool never uses a number that the kernel gave to a new file. The simulator checks this rule: a close(2) while a job uses the fd is a `Model_error`.
- **The pool closes the files.** close(2) of a regular file can block, so the Linux world gives it to the pool with no completion.
- **The pools after a fork.** A child of `fork` has no workers, so the pools reset their state in the child (`pthread_atfork`). The mutation run of windtrap forks a child for each mutant.
- **The end of `run`.** A job that a fiber detached can still hold the user of an fd when `main` returns. The domain that started the job waits for its completion before its loop ends, so the fd closes. A job with no fd, for example `Dns.resolve`, does not hold the end of `run`.
- **The threads of a pool.** A new job starts a thread when the idle threads are fewer than the queued jobs, up to the maximum. Thus a job that blocks does not stop the next job. If a pool has no thread and cannot start one, its queued jobs complete with `EAGAIN`.
- **Not done in M2:** the limit of detached jobs of each pool and the warning after 1 s (section 2.3). A pool has a fixed maximum of threads: 16 for the files and 8 for the names.
- **The signals are an argument of the world** (section 2.3): `Weft_linux.run ~signals` and `Sim.run ~signals`. The guard of `Signal.received` asks the world if it watches the signal (`Op.Watch`). If not, it raises `Invalid_argument`. The world also gives the form of the signal that it delivers, so `Other_signal 10` and `SIGUSR1` are one event on Linux. `Sim.signal` of a signal that the program does not watch is a `Model_error`, because the OS does the default action.
- **The count of a signal.** A commit takes the count. A delivery puts the count in the cell of the waiter before the commit, because the owner can read the cell at once on another domain.
- **A child process.** `Process.spawn` uses `pidfd_spawnp` (glibc 2.39). The pidfd is a normal fd record in the poller. One fiber reaps the child with `waitid`, fills an Ivar with the status, and then closes the pidfd. The other fibers read the Ivar.
- **The faults of the network** (`Sim.net_faults`): a jitter, short reads and writes, and an edge with no change one time in 8. A write of at most 4096 bytes (`PIPE_BUF`) to a pipe is atomic, as on Linux. A TCP stream loses no data and keeps its order, so the loss and the reordering of packets show only as delays. The partitions need the host of each side of a connection, and the simulator does not record it. Thus the partitions are not done. With the faults, the tests found that the echo server of the tests dropped the rest of a short write.
- **One exception for all I/O.** `File.Error`, `Dir.Error`, `Pipe.Error`, `Process.Error` and `Dns.Error` are the same exception as `Net.Error`. The error type has five more constructors: `No_entry`, `Exists`, `Not_dir`, `Is_dir` and `Not_empty`. A top-level `Weft.Error` would hide the `Error` constructor of `result` after `open Weft`.
- **The mutation run** kills each mutant that a test reaches, or a comment with `[@mutate off]` gives the reason. Two tests of Linux start one domain only, because the mutation run forks a child for each mutant, and OCaml refuses a fork after a domain started. With `short_io`, a write of a file can be short too.
- **Not in M2:** UDP, `sendto`, `recvfrom` and the socket options of `Net` (section 2.3, The I/O modules), and the `Poll` world.

### M3: the tuning

**Contents:**

- The next slot, with its limit, and its steal if a measurement shows that it is necessary (section 2.5)
- The Go rules of section 2.5, Idle and wake, if open question 8 shows that M1 wakes too many domains
- The pinned queue, `~pinned`, and `Ivar.fill_from_outside` with the injection queue
- The deadline form of `with_timeout`
- The watchdog, the report of failed promises and `Livelock`
- Real `Posix` timers (section 2.6)

**Done when:**

- The watchdog reports a fiber that computes for 200 ms without a switch.
- A fill from a finaliser wakes its fiber, also while the same thread holds a sync word.
- A yield loop that waits for the time gives `Livelock`, and a long stream with no timer does not.
- Open questions 5 and 7 have measurements.

**The state of M3 (2026-10-11).** Each item of the "done" list has a test, and open questions 5, 7 and 8 have measurements (section 2.13). The code settled these points:

- **The Go rules** (section 2.5, Idle and wake) follow `Wake.tla`: a `searching` count with a limit of half of the domains, no wake at a push while a domain searches, the decrease before the second check, and a wake by the last searching domain that finds work. They replace the wake chain of M1. On 8 domains, they halve the wakes of 100,000 short fibers and double the throughput.
- **The next slot** gets one fiber that a fiber of the domain wakes: the partner of a channel, or the one fiber of an `Enqueue`. A list of more fibers goes to the tail. The limit is 3 runs in sequence. A thief takes a next slot with a CAS after the local queues, and a push to the next slot wakes an idle domain (open question 5).
- **Pinned fibers.** `~pinned:true` is on `fork`, `fork_promise` and `fork_isolated`. A push of a pinned fiber goes to the pinned queue and wakes domain 0 only if domain 0 is in `idle`: domain 0 checks the pinned queue in its second check, so the pair is a Dekker pair.
- **`Ivar.fill_from_outside`** gives `false` if no `run` is active. One `run` at a time takes these fills: `run` keeps its scheduler and the wake of its world in one global slot. A domain applies the requests of the injection queue each 61st take and before a steal.
- **The deadline form of `with_timeout`** uses a sync word as the state of its heap entry, so the heap drops an ended entry as a stale one. A call that ends at once costs 4.6 times less than with a daemon fiber.
- **The report of failed promises.** The closure of the finaliser holds the id of the run, not the scheduler: the scheduler can hold the promise in an old slot of a ring. A ring now clears the slot of each pop. The simulator forces a full GC at its end only if a promise of the run failed and no fiber awaited it.
- **The watchdog** reads a beat of each domain in a Bigarray: one more at the start and at the end of each fiber. It is in the Linux world (`Weft_linux.run ~watchdog`).
- **`Livelock`.** A switch point with only `Now`, `Switch`, `Random` or a `Wait` that gives nothing is not progress.
- **A yield in a read** after 64 KiB with no wait (section 2.7).
- **A sync on one base event** has a fast path: 60 words instead of 123.
- **Not done:** the slack stays 1 ms (open question 7 gives the data for a choice), and `Scope.tla` does not cover the deadline form of `with_timeout`.

### Why `dscheck` is in M1

The virtual domains run on one OS thread and change only at switch points. Thus the simulator never tries the other orders of the atomic operations between two switch points. `dscheck` tries all of these orders. An error in the shared protocols can make the simulator tests give incorrect results, so `dscheck` comes in the same milestone as the domains.

## 2.15 Later: record and replay

This is not in the first version.

- **Record** wraps a real world, for example Linux. It writes the result of each world effect to a log: each `Now` value, each `Submit` result with the bytes of each read, each `Wait` result and each `Random` value.
- **Replay** reads the log and gives the same results again, with no OS. A failure from production can then run again, deterministically, with a debugger. The tool `rr` does the same at the level of system calls.

It has three problems:

1. **Many domains.** A replay is exact only if the fibers run in the same order. With one domain, the world effects set the order. With many real domains, steals, channel operations and atomics between domains are not world effects, so the log does not record their order. Thus Record works fully only with `~domains:1`.
2. **Log size.** Each read writes its data to the log, and each `Now` adds an entry. A busy server makes a very large log.
3. **Privacy.** The log holds all of the request data, for example passwords and tokens.

## 2.16 Verification with TLA+

The protocols that run on many domains at the same time have models in PlusCal, in `spec/tla`. The TLC model checker examines each order of their steps. Each model has the protocol of this design, and a set of planted bugs. Each planted bug must give a failure. Thus the model can find the errors that matter, and it is not only a model that always passes. Some models also have simplifications (constants with the prefix `Simp`). A simplification must pass, and section 2.5 uses the ones that passed.

Run all of the models with `nix develop -c spec/tla/check.sh`. `MODELS="Ivar Fd"` selects some of them. The script runs `JOBS` cases at the same time (default 4), each one with `-lncheck final`, so that TLC checks the liveness property one time at the end. The 42 cases take about 16 minutes on a machine with 16 CPUs. The slowest case, `Wake.tla` with no bug and 15.3 million states, takes about 11 minutes, and it is the lower limit of the run time. The flake of the repository gives the TLA+ tools.

| Model | Protocol (section) | Properties | Planted bugs that fail |
|---|---|---|---|
| `Commit.tla` | The commit of `choose` (2.4) | At most one resume, correct rendezvous pairs, no lost rendezvous, a correct end state, termination | No `Parked` state, no id order, a partner that gives up on `Claimed`, no channel lock |
| `Scope.tla` | Scopes and cancellation (2.5), and the open shield of `Condition.wait` (M2) | No orphan fiber, no lost exception, the shield holds, no cancel of a foreign `sync`, a closed scope is empty, the timer reaches a condition wait, termination | A join that a cancel can end, a scope in a shield under the outer scope, no check of the chain, an exception after the end, no read of the flags, a condition wait with the outer context. Simplifications that pass: no flags on nested scopes, no second read at a link, and the two together. |
| `Ivar.tla` | The atomic register of an `Ivar`, and `Ivar.fill_from_outside` (2.4, 2.5) | At most one resume, one fill, no waiters after the fill, a correct end state, termination | A finaliser that commits the waiters itself, a register that is not atomic |
| `Wake.tla` | The loop of a domain: idle and wake, steal, next slot, pinned queue, injection queue (2.5) | Each item runs, a pinned item runs only on domain 0, no lost wakeup | The decrease of `searching` after the second check, no second check, a pinned push that wakes any domain. Simplifications that pass: no steal of a next slot, no search limit, no last wake, no `searching` count. |
| `Fd.tla` | The fd record: `Rcfd`, the registrations, the close notices, the removal (2.7) | One `close(2)`, no `close(2)` of another file, no use after `close(2)`, no `EPOLL_CTL_DEL` of a new file, `EPOLL_CTL_DEL` before `close(2)`, termination | A count and a closed bit, a Register that reads the state first, a registration with no user. A simplification that passes: no CAS on the domain bit. |

**What the models showed about the design:**

- **A daemon needs its own cancel context** (section 2.5, Lifetime). The design did not tell how the `sync` of a daemon sees the cancel of the daemons.
- **A cancel sets only one flag** (section 2.5, How a cancel reaches a fiber). The flags of nested scopes and the second read of the parent flag at a link are not necessary, because each `sync` reads the flags of its whole chain. The combined case passed in the three scenarios.
- **The domain bit of an fd record needs no CAS** (section 2.7, Close notices). The owner of the registration decides.
- **M1 needs no `searching` count** (section 2.5, Idle and wake). The count, its limit and the last wake are each only for performance. Without the count, the model also passes.
- **A thief that cannot take a next slot loses no wakeup** (`Wake.tla`). The steal of the next slot only makes the latency smaller.
- **A Register and a close notice of one domain never interleave** (section 2.7). The model of the fd record needs this rule of the design. Without it, a notice can arrive before the registration is live, and the waiter waits forever.

**A correction from the review of M0.** In `Scope.tla`, the fork of K checked only the flag of S. The design and the code read the flags of the whole chain of S, because the nested scopes have no flags of their own. The fork step now reads `FirstFlag("S")`, and the 16 cases of `Scope.tla` give the expected results.

**The limits.** The models are small: two or three fibers or domains in each scenario, a few work items, and one fd. Each model takes some details as one atomic step, and another model examines them, for example the commit in `Scope.tla` and `Ivar.tla`. The models do not cover the simulator, the OS facts, the handoff of `Fiber.Mutex`, promises or signals. `dscheck` still examines the OCaml code, because the OCaml code can differ from the model. In M2, two `dscheck` tests examine the completion of a job and the delivery of a signal, each with a cancel from another domain (section 3.8).

## 2.17 Tests with windtrap

The tests use windtrap 0.2.0 (decision 9). The flake builds windtrap and `ppx_windtrap` from the `v0.2.0` tag, because nixpkgs has only 0.1.0. Windtrap gives unit tests, property tests, stateful tests, expect tests, coverage and mutation testing from one library.

**Which test for which part:**

| The part | The test |
|---|---|
| A law, for example "a span in nanoseconds and back is the same span", or "a `choose` resumes the fiber one time" | A property (`prop`) over a generator. A `Law` verb when windtrap names the law. |
| A value with state: `Chan`, `Ivar`, the heap of timers, the fd records | A stateful test against a simple model. In M1, the same commands also run with `stateful ~domains:2`, and windtrap finds a result that no order of the calls explains. |
| A result that the design gives for chosen inputs | `test` with `equal`, or `cases` for a table |
| A long text: a trace of the simulator, the message of `Deadlock` | An expect test (`expect`), accepted with `dune promote` |
| The tests themselves | Coverage, and a mutation run on each module that a change touches |

**The layout:**

- `test/sim` and `test/linux` are windtrap suites. Each suite runs with `--corrected`, so `dune promote` accepts a new baseline. The tests that use only the public API run on each world.
- `test/nounix` is the guard of section 2.3. It has no test library.
- The libraries have the backends `ppx_windtrap.coverage` and `ppx_windtrap.mutate`. A backend does nothing until a build asks for it. Its runtime library, `windtrap.runtime`, uses only the stdlib, so the guard stays correct also with the instrumented libraries.
- The libraries do not use `ppx_windtrap` itself. Its runtime libraries include `windtrap`, which links `unix`. Thus there are no inline tests in `weft` or `weft.sim`, and a test of an inside part goes through a module that the library exports for tests, as `Sched_debug` (section 2.2).

**The commit protocol (M0).** `Commit.Make (A : ATOMIC)` in `src/core/commit.ml` has no effects, and it does not resume a fiber. Each function gives the caller its part of the resume: `claim` gives `Committed { resume_partner }`, `notify` gives `Resume`, `In_scan` or `Lost`, and `park` gives `` `Parked `` or `` `Done ``. The stateful test of `test/core` compares the protocol with a sequential model. Its invariant on each sync is the property of `Commit.tla`: at most one resume, and a suspended fiber always has a `Parked` word. Thus a lost wakeup fails the test at the call that caused it. In M1, the same commands run with `stateful ~domains:2`.

**The commands:**

- `dune runtest`: all suites.
- `dune runtest --force --instrument-with ppx_windtrap.coverage`, then `dune exec windtrap -- coverage`.
- `WINDTRAP_MUTATE=1 dune runtest --force --instrument-with ppx_windtrap.mutate`, then `dune exec windtrap -- mutants`.

**Two facts for M1.** I read them in the source of windtrap 0.2.0, but I did not do a test of them with domains:

- The runner of windtrap can run tests on worker domains (`Domain.spawn` in `lib/workers.ml`). A test that calls `run ~domains:n` then has more domains than `n` in the process.
- Mutation testing forks a child for each mutant. OCaml 5 refuses `Unix.fork` in a process that started a domain, and windtrap tells this in its message (`lib/mutate_loop.ml`). Thus a mutation run of a test with many domains can give an error.

---

# Part 3: Review history

This part records the reviews of the design and the fixes of each one. It is history: Part 2 is the design. Later changes replaced some fixes, and section 3.5 lists them.

## 3.1 The first review of 2026-10-10

An adversarial review found these 20 problems. The design at that time fixed all of them. The section numbers in the table are those of Part 2.

| No. | Problem | Fix |
|---|---|---|
| 1 | An edge-triggered flag can lose a wakeup across domains | The readiness word with a tick (2.7) |
| 2 | No commit protocol for `choose` across domains | The commit across domains (2.4) |
| 3 | A guard cannot perform a program effect | Guards and wrap functions run in the fiber (2.4) |
| 4 | No cancellation, no error path, no fiber lifetime | Scopes (2.5) |
| 5 | A busy domain blocks its own fds and timers | One shared poller with a driver token. Idle domains run expired timers (2.5, 2.7). |
| 6 | `wait` and `wake` do not know the domain | The `~domain` argument (2.3) |
| 7 | The default signal set stops Ctrl-C | The default set is empty (2.7) |
| 8 | A stdlib `Mutex` or a spin loop breaks the simulator | `Fiber.Mutex` and the rules for fiber code (2.5) |
| 9 | The 408 test has a race, and latency changes the result | Listen before the fork. Latency is 0 by default (2.8). |
| 10 | Section 2.10 claims that a layer cannot forget the seam | The claim is corrected. `(forbidden_libraries unix)` (2.3, 2.10) |
| 11 | Code outside a fiber gives `Effect.Unhandled` | `Fiber.Not_in_fiber`, and `Ivar.fill` with no effect (2.5) |
| 12 | A thread pool in OCaml can be slow to report | The thread pool is in C (2.3) |
| 13 | The next slot can starve the queue. Work can wait while domains sleep. | A limit of 3 runs. The last searching domain wakes one more (2.5). |
| 14 | `close` is not specified, and fd numbers are used again | A use count and a generation number (2.7) |
| 15 | A child inherits `SIG_IGN` | `SETSIGDEF` resets `SIGPIPE` and the watched signals (2.7) |
| 16 | The semantics of `Signal.received` are not clear | One fiber gets each signal (2.7) |
| 17 | The `Mono` clock is not selected | `CLOCK_MONOTONIC` for the alarm and for `Now` (2.7) |
| 18 | `Random` cannot give bytes for TLS | `random_bytes` (2.3) |
| 19 | Sections 2.3 and 2.7 disagree about fd registration | Registration in `submit` (2.3, 2.7) |
| 20 | "The full Unix API" cannot include `fork` | The list of excluded calls (2.3) |

## 3.2 The second review of 2026-10-10

A second adversarial review found these 16 problems. The design at that time fixed all of them.

| No. | Problem | Fix |
|---|---|---|
| 1 | Two fibers that sync on each other can deadlock in the commit | Claim the two words in id order (2.4) |
| 2 | A cancel goes through a shield into its nested scopes | A cancel stops at a shield (2.5) |
| 3 | "No hidden current scope" is false for cancellation | A stack of cancel contexts in each fiber (2.5) |
| 4 | A timeout cannot stop a `Blocking` operation | The fiber detaches from the operation (2.5) |
| 5 | `close` in a cancelled fiber leaks the fd | `close` never blocks. The last use calls `close(2)` (2.7). |
| 6 | `Ivar.fill` from a finaliser can corrupt the scheduler | `Ivar.fill_from_outside` (2.5) |
| 7 | The next slot cannot be stolen | A thief takes it if the switch count does not change (2.5) |
| 8 | Section 2.2 disagrees with the later sections | Section 2.2 is changed. Quiescence needs all domains (2.2). |
| 9 | `dscheck` does not test some races | Four more `dscheck` tests (2.14) |
| 10 | Nobody owns the fd record | The scheduler owns it, and `Register` gives a token to the world (2.3) |
| 11 | A completed `Blocking` operation can wait when all domains are busy | A completion `eventfd` in the shared poller (2.3) |
| 12 | `Condition.wait` can return without the mutex | The lock again in a shield (2.5) |
| 13 | `Time.after` on overflow is not specified | `never` (2.6) |
| 14 | A `Posix` jump in the simulator does not expire deadlines | `Sim.set_posix` acts as `TFD_TIMER_CANCEL_ON_SET` (2.8) |
| 15 | The exception of a failed promise can be lost | A report by the simulator and a line on `stderr` (2.5) |
| 16 | A tick thread can have the old signal mask | A warning from `run ~signals` (2.7) |

## 3.3 The additions about exceptions and affect, 2026-10-10

These 14 additions came from two questions: how exceptions in a fiber behave, and how the design compares with affect.

| No. | Gap | Addition |
|---|---|---|
| 1 | The backtrace of a child is lost | Store and raise again with the backtrace (2.5, Exceptions) |
| 2 | One failed connection stops the server | `Fiber.fork_isolated ~on_error` (2.5) |
| 3 | A guard that raises leaves its nacks closed | Enable the nacks of the guards that ran (2.5, Exceptions) |
| 4 | Code that catches all exceptions catches `Cancelled` | Raise it again. The simulator reports it (2.5, Exceptions). |
| 5 | Asynchronous exceptions can corrupt the scheduler | Not permitted (2.5, Exceptions) |
| 6 | A bug in the scheduler is not specified | `Fiber.Internal_error` (2.5, Exceptions) |
| 7 | A mutex held during an exception | `Fiber.Mutex.protect` and a poisoned mutex (2.5) |
| 8 | "The first exception" is not deterministic | Told in the design (2.5, Exceptions) |
| 9 | `Stack_overflow` and `Out_of_memory` | A normal exception, and `Internal_error` (2.5, Exceptions) |
| 10 | No event form of `await` | `Promise.ready` (2.5, Promises) |
| 11 | No cancel of one promise | `Promise.cancel` (2.5, Promises) |
| 12 | `await` cannot tell whose cancel it is | `Promise.Cancelled` (2.5, Promises) |
| 13 | `await` loses the backtrace | Raise again with the backtrace (2.5, Promises) |
| 14 | A user effect does not reach a forked fiber | `Fiber.scope ~handler` (2.5, Effect handlers of a scope) |

## 3.4 The third review and the decision about stealing, 2026-10-10

A third review, by a subagent, examined correctness, complexity and performance. It also measured some costs on this machine. After the review, decisions 7 and 8 replaced the shared poller. As a result, some fixes of sections 3.1 and 3.2 are gone: the shared poller with a driver token (3.1, no. 5), the readiness word with a tick (3.1, no. 1), and the completion `eventfd` in the shared poller (3.2, no. 11).

| No. | Problem | Fix |
|---|---|---|
| C1 | Two sides can resume the same fiber | The `Parked` state and one owner for the resume (2.4) |
| C2 | A one-word partner does not handle `Claimed` | Retry with `Atomic.Backoff` until `Done`, as affect does (2.4) |
| C3 | Nobody polls while a domain computes | A known limit, as in affect. `Fiber.yield` and a watchdog (2.5). |
| C4 | A stolen fiber can use the state of its old domain | A step value to the loop that resumes it. No DLS (2.5). |
| C5 | A detached operation can write into freed memory | Always a C buffer of the pool (2.3) |
| C6 | Two lost wakeups: the order of `searching`, and pinned fibers | The Go order. A pinned push wakes domain 0 (2.5). `Lost_wakeup` in the simulator (2.8). |
| C7 | Scopes cross domains with no concurrency rules. The claim about Eio is false. | A state word and a short lock for each scope (2.5). The claim is corrected. |
| C8 | A timeout poisons a shared mutex | `use_rw ~protect` and `use_ro` (2.5) |
| C9 | A thief that finds a full next slot sleeps | A thief tries the next slots after the queues (2.5) |
| C10 | Stale waiters fill the registrations of quiet fds | The owner drops them at each add (2.7) |
| C11 | A yield loop never becomes quiescent | `Livelock` in the simulator (2.8) |
| C12 | "Pinned to the current domain" is arbitrary | Pinned to domain 0 (2.5) |
| C13 | Detached operations fill the pool | Two pools and a limit (2.3) |
| C14 | `timerfd_settime` can give `ECANCELED` | No `Posix` alarm (2.6) |
| C15 | `pidfd_open` after `posix_spawn` has a race | `pidfd_spawn` (2.7) |
| C16 | `fill_from_outside` needs the world, and it breaks the determinism of the simulator | A global slot, and a queue in the simulator (2.5) |
| C17 | A report of a promise at the end of its scope is too early | A finaliser, and a forced GC at the end of `Sim.run` (2.5) |
| C18 | Some rules conflict | `Scope_closed`, the shield of `Condition.wait`, the wildcard rule (2.5) |
| C19 | The fd and signal rules are incomplete | Close-on-exec, `EPOLL_CTL_DEL` before `close(2)`, the order of the use count, signals blocked in the pools (2.3, 2.7) |
| X1 | Stealing makes much state shared | Decision 7: only the queues, scopes and sync words are shared (2.5) |
| X2 | The `Posix` alarm line is large for a rare need | A `Posix` deadline becomes a `Mono` deadline (2.6) |
| X3 | The full Unix API and its simulation are a large surface | Steps 1 to 6 give a minimal set. Step 7 gives the rest (2.14). |
| X4 | kqueue and `Select` | kqueue after the Linux measurements. `Poll` in place of `Select` (2.7). |
| P1 | The nested epoll costs about 1.8 µs for each cycle | Gone with decision 7 (2.7) |
| P2 | Cancelled timers cause alarms that do nothing | Drop stale heads, slack, `epoll_pwait2` (2.5, 2.7) |
| P3 | `with_timeout` forks a fiber on each call | A deadline on the cancel context (2.5) |
| P4 | A cancel offer on a shared context at each `sync` | A current sync word in each fiber, and a cancel walk (2.5) |
| P5 | Many domains and the minor GC | A default of 4 domains (2.5) |
| P6 | A global counter of sync ids | A counter for each domain (2.4) |
| P7 | The cost of `Switch` | Measured at 5.6 ns. No functor is necessary (2.13). |
| P8 | The `Unix` stubs enter a blocking section and copy | Own stubs for non-blocking fds (2.3) |

## 3.5 The fourth review and the milestones, 2026-10-10

Three subagents reviewed the design at the same time: one for the concurrency protocols (R), one for the OS facts, the OCaml facts and the simulator (F), and one for the complexity (K). They verified many facts with man pages, the OCaml 5.5.1 sources and test programs. After the review, the user accepted the milestones of section 2.14.

**The fixes that later changes replaced:**

| Fix | Replaced by |
|---|---|
| 3.1 no. 1: the readiness word with a tick | Flags that belong to one domain (decision 8) |
| 3.1 no. 5: the shared poller with a driver token | A poller for each domain (decision 7) |
| 3.1 no. 11: `Ivar.fill` with no effect | `Ivar.fill_from_outside` with a request queue (M3) |
| 3.2 no. 7: a steal of the next slot with a switch count | A steal with a CAS after the queues (M3) |
| 3.2 no. 11: a completion `eventfd` in the shared poller | A mailbox for each domain (M2) |
| 3.2 no. 14: `Sim.set_posix` acts as `TFD_TIMER_CANCEL_ON_SET` | `Sim.set_posix` changes only `Now Posix`, and timers use `Mono` until M3 |
| 3.2 no. 16: a warning if the process has more than one thread | A check of `SigBlk:` for each thread |
| 3.3 no. 7: `Fiber.Mutex.protect` | `use_rw ~protect` and `use_ro` (M2) |
| 3.4 C14 and X2: a `Posix` deadline as a `Mono` deadline | Timers on `Mono` only until M3 |
| 3.4 P2: `epoll_pwait2` in place of the `timerfd` | The `timerfd` stays, because `epoll_pwait2` is late on long timeouts |
| 3.4 P3: a deadline on the cancel context | A daemon fiber in M0, and the deadline form in M3 |
| The order of work in steps 1 to 7 | The milestones M0 to M3 |

**The findings and their fixes:**

| No. | Problem | Fix (section, milestone) |
|---|---|---|
| R1 | The shield is not in the scope tree, so the cleanup example fails | The shield is a node of the tree (2.5, M0) |
| R2 | A cancel can end the join of `Fiber.scope` | The join ignores cancels (2.5, M0) |
| R3 | A late `submit` can close a reused fd number | The `Rcfd` state machine (2.7, M0) |
| R4, F1 | A finaliser in `fill_from_outside` can wait forever on a `Claimed` word | A request queue. The domain does the fill (2.5, M3) |
| R5 | "Find a partner, else register" is not atomic | The atomic register (2.4, M0) |
| R6, F2 | Register, close notices and the removal of a registration race | Dekker pairs, the generation in each notice, one owner for the removal (2.7, M1) |
| R7 | A yield can be resumed twice | The self-commit of a ready branch (2.4, M0) |
| R8 | A cancel walk on a snapshot can cancel a `sync` outside its scope | The context in each `sync` record, and resumes after the lock (2.5, M1) |
| R9 | `Condition.wait` in `use_rw` poisons or waits forever | No poison at a wait point (2.5, M2) |
| R10 | One count cannot tell when to cancel the daemons, and an exception can be lost | Two counts, and the exception before the decrease (2.5, M0) |
| R11 | An edge with no waiter is normal, also with one domain | Remove only when the fiber moved (2.7, M1) |
| R12 | The result of `with_timeout` in a close race | Three rules (2.5, M0) |
| F3 | Auto-advance ignores the timed events of the simulator | The agenda (2.8, M0) |
| F4 | `epoll_pwait2` is 10 ms late on a 10 s timeout (measured) | The `timerfd` stays (2.7, M0) |
| F5 | A fill from a finaliser depends on the GC | Told in 2.9, logged, and off by default (2.5, M3) |
| F6 | `forbidden_libraries` finds only `unix` | The limits are listed, and each `Hashtbl` uses `~random:false` (2.3) |
| F7 | The 408 example does not compile, and its check cannot apply | A `test` stanza (2.8) |
| F8 | The Dekker arguments need `Atomic.t` | Atomic locations (2.4) |
| F9 | Another process can clear `O_NONBLOCK` | `MSG_DONTWAIT` for sockets (2.3, M0) |
| F10 | `Lost_wakeup` finds only work in a queue | Five checks (2.8, M0) |
| F11 | The model of the fds in the simulator is not given | The strict model (2.8, M0) |
| F12 | `Blocking` operations complete at once in the simulator | Through the agenda and the mailbox (2.8, M2) |
| F13 | The tick-thread concern is false | Removed. A check of `SigBlk:` (2.7, M2) |
| F14 | A seed depends on the host and on the OCaml version | A constant default and an own PRNG (2.8) |
| F15 | `close(2)` can block | The pool for `File` and `SO_LINGER` (2.7, M2) |
| F16 | The reasons against a stdlib `Mutex` are incorrect | Corrected (2.5) |
| F17 | `Livelock` gives false positives | A count since the last quiescence (2.8, M3) |
| F18 | kqueue facts | Relative spans, and the points that are not verified (2.7, M2) |
| F19 | The close-on-exec list and the zombie of a `pidfd` | The full list, and `waitid` (2.7) |
| F20 | The library threads and signals | All library threads block all signals (2.3) |
| F21 | C callbacks have no handler, and OCaml polls at more points | Listed (2.5) |
| F22 | `Atomic.Backoff` is not in the stdlib | An own module (2.4) |
| F23 | A user thread runs under the lock of its domain | Told in 2.3 |
| F24 | The finaliser of a promise must not hold the promise | A separate cell (2.5, M3) |
| K1 | About 20 shared states and 13 protocols, and `dscheck` covered 8 | The list of M1 covers them (2.14) |
| K2 | Full stealing multiplies the fd rules (challenges decision 7) | Decision 7 stays, in M1 |
| K3 | The cancel model has four layers | `with_timeout` with a fiber in M0, and the rest in M2 and M3 (2.5) |
| K4, K5, K7, K8 | Tuning and library parts at the depth of the core | Moved to M2 and M3 (2.14) |
| K6 | Base events do not need nacks | `with_nack` only for user protocols (2.4) |
| K9 | `Posix` timers do not do what users expect | Timers on `Mono` only until M3 (2.6) |
| K10 | Step 1 was the whole core, and Linux came only in step 5 | M0 has the simulator and Linux (2.14) |
| K11 | The checks of the simulator read the scheduler | `Sched_debug` (2.2) |
| Document | 18 contradictions and old texts | Corrected, and the history moved to Part 3 |

**A claim of mine that was incorrect:** I wrote that affect keeps the fd registrations on the domain where a fiber blocks. affect keeps one shared map for all domains (`affect_unix__select.ml`). Section 2.5 now tells this.

## 3.6 The review of M0, 2026-10-11

Three subagents reviewed the code of M0 (correctness, conformance to the TLA+ models, and complexity). Each finding of this table is fixed. Each bug has a regression test, and each test fails when the old code is put back.

| No. | Finding | Fix |
|---|---|---|
| C1 | In a cancelled context, `with_timeout` raises `Scope_closed`, and the scope records it as a failure | `with_timeout` reads the flags of its chain first and raises that `Cancelled` (2.5) |
| K1 | A deadline that passes between the poll and the sleep: the domain sleeps with the fiber in its queue | `wait` polls instead of a sleep when the queue is not empty |
| C3, T7 | A failed Register makes the waiter retry with no switch, and the domain stops | The waiter raises the error of the Register |
| C2, K3 | The node of a daemon stays in its scope after the daemon ends | The daemon removes its node when it ends |
| C4 | The slack rounding of a deadline near the largest time overflows, and the alarm fires at each sleep | The rounding saturates |
| T5 | An outer cancel at the deadline of `with_timeout` gives `Cancelled Timeout` | `with_timeout` raises the `Cancelled` of the outer context first (2.5) |
| T6 | `Ivar.fill`, `Fiber.cancel` and `Net.close` outside a fiber change the state and then raise | They perform `Self` first |
| T1 | M1: a `fork_daemon` and the end of the last member can miss the new daemon | The count and the link of the daemon are one step under the lock of the scope |
| T2 | M1: a read and a store of the shared domain word can lose a bit | `fetch_and_add` (2.7) |
| T4 | The text said that `Fiber.scope` raises `Cancelled` when its body gave a value | The text now agrees with the code (2.5) |
| T8 | The fork step of `Scope.tla` read only one flag | The step reads the chain (2.16) |
| K2 | The removal of a member is O(N) | The members and the nested nodes are hash tables. The walk sorts them by id, so the fork order stays. |
| K4 | Each register of a waiter or an offer filters the whole list | The lists drop stale entries only when they have grown to twice their live size |
| K5 | `fork_promise` makes a scope that nothing uses in M0 | No scope until `Promise.cancel` (M2) |
| K6, K10 | The simulator copies the world handler, and the aliases of `weft.ml` are not necessary | The simulator is a `WORLD` with one shared handler |
| K8, K9, K14, K15, K16, K17, K20 | Copies in `chan.ml`, a field for `yield` in each sync, the `leaf` record, `int64` tokens, defensive code, three copies of one expression, a rebuild with four copies | Each one is simpler |
| L1 to L6 | Small errors of the Linux world and of the simulator: the Unix connect, the address of a Unix peer, the port range, the seed, a cancelled `advance_until`, the backlog under latency | Each one is fixed |
| L7 | The text said that each I/O function is a cancellation point | The text now agrees with the code (2.5) |

These findings stay open, with their milestone: the Register order of `Fd.tla` and idle and wake (M1), the table of live fibers and the allocations of a sync (M3). The state of M0 in section 2.14 lists them.

## 3.7 The review of M1, 2026-10-11

Three subagents reviewed the code of M1 (correctness, conformance to the TLA+ models, and complexity). Each finding of this table is fixed. Each bug has a regression test that fails when the old code is put back.

| No. | Finding | Fix |
|---|---|---|
| T1, C1 | A fiber in the overflow queue never runs while more than 256 fibers yield, because a domain reads the overflow queue only when its ring is empty | Each 61st take reads the overflow queue first, and a full ring moves its older half to the overflow queue (2.5) |
| C2 | The `eventfd` and the `timerfd` had `EPOLLOUT`: each drain gave one more event, so each wake made two returns from `epoll_wait` | `EPOLLIN` only for these two fds |
| C3 | If a `Domain.spawn` failed, the domains that started went on with the closed fds of the world | `run_world` stops and joins them, then raises the error |
| T2, C4, T8 | A wake waited for the next effect of the fiber, a thief did not wake more domains, and a domain could take its own bit | A wake before the fiber goes on, the wake chain of Go after a steal of more than one fiber, and `Idle.take` skips the caller |
| T3 | A `dscheck` test let a thread take the mailbox during a Register, an order that the code cannot have (`busy[d]` of `Fd.tla`) | The thread of the domain does its Register and its mailbox in sequence |
| T4, T6 | No `dscheck` test for a one-word partner while a claim is put back, or for a `fork_daemon` at the end of the last member | Two tests, and each one finds a planted bug |
| T5 | The design said that a yield does a CAS on a sync word | The text now agrees with the code (2.4) |
| T7, C5, K4 | `on_ready` went on with a registration that it had removed | The two parts are in the `else` |
| K1, K2, K3 | The table of fibers and the names cost each fork, `Switch` cost each fiber start, and the order of the scan took one world effect for each branch, with one shared atomic counter on Linux | The table and `Switch` only in the simulator, and the order from the PRNG of the domain, seeded from the world when the domain starts (2.5) |
| K5 to K11 | Stale comments, copies of tests, a body that a test copied, a steal that made a list, small copies in the fd code | Each one is simpler |

## 3.8 The review of M2, 2026-10-11

Three subagents reviewed the code of M2 (correctness, conformance to the TLA+ models, and complexity). Each finding of this table is fixed. Each bug has a regression test that fails when the old code is put back, except two: C11 is a race with one reap on another domain, and the NUL check of C12 is one line.

| No. | Finding | Fix |
|---|---|---|
| F1, C1 | In `use_rw ~protect:true`, `Condition.wait` used the contexts from outside the shield, so a cancel of a scope inside the shield (for example `with_timeout`) never ended the wait | The wait opens the shield node of `use_rw` (2.5, Locks for fiber code), and `Scope.tla` has the scenario **condwait** |
| F2, C2 | A fiber that held the mutex with `use_ro` or `lock` used the contexts of the last `use_rw` holder in `Condition.wait` | The mutex holds the shield node only while its holder holds it |
| F3 | `Mutex.lock` did not read the flags when the mutex was free | `lock` is a cancellation point in each case |
| F4, C6 | A job that a fiber detached, and that was in the pool at the end of `main`, never released its fd | The domain waits for it at the end of its loop (2.14, the state of M2) |
| C3 | A second `Process.wait` waited only for the status, and the reaper could leave with no status | Each waiter also watches the pidfd |
| C4 | A job waited behind a job that blocked, while the pool could start a thread | A thread for each queued job that no idle thread takes |
| C5 | `pread` past the end of a file raised an exception in the simulator | It gives 0 bytes, as on Linux |
| C7 | A fiber that waited for `Other_signal 10` never got `SIGUSR1` | `Op.Watch` gives the form of the signal that the world delivers |
| C8 | The simulator watched each signal, and Linux only the signals of `run ~signals` | `Sim.run ~signals` |
| C9 | In the simulator, `SIGTSTP` and the signal 0 ended a fake program | They do not end it |
| C10 | With short writes, the simulator split a pipe write of at most `PIPE_BUF` bytes | Such a write is atomic |
| M1 | The mutation run found that a pipe with less than `PIPE_BUF` bytes of space was writable in the simulator. A writer of 4096 bytes then waited on a writable fd, and the check at quiescence gave `Lost_wakeup` | A pipe is writable only with `PIPE_BUF` bytes of space, as with the free page of Linux |
| C11 | `Process.kill` could raise for a child that another domain reaped at the same time | The error of the send is not an error after a reap |
| C12 | Some errors of the simulator file system were not the errors of Linux, and the NUL check of `spawn` did not cover the directory | The errors agree, and a portable test examines them on the two worlds |
| Q4 | No test examined the completion of a job or the delivery of a signal with a cancel from another domain | Two `dscheck` tests, and each one finds a planted bug |
| K1 to K9 | Copies of the range check and of the write loop, of the test helpers, of the errno table and of the close of a pipe end, two atomics in a promise, and a list for each event in the wait of Linux | Each one is simpler |
