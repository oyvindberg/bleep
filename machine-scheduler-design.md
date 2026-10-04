# Machine-wide scheduler for bleep-bsp — design + implementation plan

Working document. Captures the investigation, the decisions taken with the owner, the model, and the order of implementation. Source of truth for the
work; update it as the work progresses.

File:line references are to `zinc-classfile-api` @ 32bd08f17 / master of 2026-10-03; re-check them before relying on an exact line.

---

## 1. The problem

The owner's machine: macOS, 48 GB, 18 cores. Two compile servers routinely run at once (different bleep versions — bleep is developed alongside the
large `dlab` build), each with `compileServerMaxMemory: 12g`. When both run dlab tests, 1–4 GB test forks pile up and the machine passes ~44 GB, at which
point it stalls (kernel_task ~200%, compressor + swap). `bleep server top` showed one server with **16 test JVMs alive while 2 suites ran, +17.4 GB forked**.

Requirements:

- bleep **as a whole** — every server on the machine — stays within what the machine can take.
- **Exactly one fork per command is always guaranteed.** Two `bleep test` started by hand both progress. That is the user's autonomy: they started it.
- Beyond the guarantee, parallelism is constrained by the real state of the machine.
- Works on **macOS, Linux and Windows**, on the **user's own JVM** (whatever JDK the build uses), measuring **in-process** (forking a measurement tool is a
  last resort).
- As stateless as possible. Nothing learned, nothing persisted that can go stale.
- **One source of truth.** The scheduler replaces the existing abstractions; it does not sit next to them.
- **Memory is machine-wide, CPU is not** (§3.1). Running out of memory freezes the machine; too much CPU only slows it down.
- Old servers (pre-scheduler bleep versions) are ignored; they disappear with the next release.

## 2. Why it fails today

There is no cross-server coordination, and each server's own arithmetic is wrong.

1. **Every server owns the whole machine.** Each daemon builds its own `MachineResources` sized to all cores and all RAM (`BspServerDaemon.scala:240`). Two
   servers = 36 cpu permits and two independent fork budgets.
2. **The budget forgets the server.** The initial budget `RAM − server×1.25 − max(4 GB, RAM/4)` = 21.5 GB (`MachineResources.scala:476-479`) is replaced
   5 s later by the retune loop (`MachineResources.scala:320-348`), whose formula `physical − heldByOthers − slack` (`MachineMemory.scala:124-127`) no longer
   subtracts the server. A 12 GB server was observed with a **46,080 MB fork budget, fully reserved, 28 forks**.
3. **Compressed memory is invisible.** "Unreclaimable" is `vm_stat` anonymous + wired (`MachineMemory.scala:40-47`), excluding compressor pages and swap,
   while our own footprint (`phys_footprint`) includes compressed pages. `heldByOthers = max(0, unreclaimable − ours)` collapses under pressure: logged
   "others hold 379MB" with 7 GB in the compressor and 13.9 GB swapped.
4. **Two servers spend the same room.** Both read the same free memory and admit against it; each sees the other's forks only after they materialise.
   Observed: **58.9 GB of forks reserved across two servers** on a 48 GB machine, plus two 12 GB heaps.
5. **Idle forks hold memory.** No idle timeout, no cap; a fork is released only if *its own* server has a waiter (`JvmPool.scala:1029-1049`). Per-project
   JUnit batch forks are pooled although their key never recurs. Measured over one day: 1841 forks with an idle tail, p90 215 s, max 13 min.
6. **The OS's own pressure signal is never read.**

Also found: KSP forks are reserved twice (`TaskDag.scala:126` and `MultiWorkspaceBspServer.scala:1691-1693`); the retune loop swallows errors
(`MachineResources.scala:348`, `MachineMemory.scala:61-66`); the request registry `SharedWorkspaceState` is a global singleton
(`SharedWorkspaceState.scala:14`); `docs/usage/resource-management.mdx` documents only the 5-second initial budget.

## 3. Design overview

One **`MachineScheduler`** per server, replacing `MachineResources`, the retune loop, `ForkCostModel`, and the memory/admission logic inside `JvmPool`.
It runs **ticks**. A tick observes the machine, reads every server's published state, runs a **pure decision**, publishes this server's new state, and
then carries the decision out. Claims of new resources are serialised machine-wide by one OS file lock.

```
            ┌──────────── machine.lock (only taken by ticks that add claims) ────────────┐
 tick:  lock → probe machine → read all state.json → decide (pure) → write own state.json → unlock
                                                                                         ↓
                                                         start / reuse / evict forks, start tasks
```

### 3.1 Two resources, two scopes

Memory and CPU fail differently, so they are governed differently — by the same tick and the same `decide`, with different scopes:

| | Governs | Scope | Applies to |
|---|---|---|---|
| machine room (`state.json` + `machine.lock`) | memory | **machine-wide** | **forks only**: test, sourcegen, annotation processors, KSP, link, post-compile |
| `parallelism` | CPU | **per server** | everything that runs: compiles, discovery, AP resolution, and forks while they run work |
| `HeapPressureGate` | the server's own heap | per server | compiles |

- Memory exhaustion is shared and catastrophic: every process on the machine stalls. That is what needs coordination across servers.
- CPU oversubscription is shared fairly by the OS and only slows things down. Two servers × `parallelism` on the same cores is accepted.
- **Compiles are outside the machine scheduler.** They run in the server's heap, which is fixed by `-Xmx` and already in the machine's used memory, so
  they cannot claim memory beyond it. A compile needs a local cpu slot and the heap gate; it never takes `machine.lock`. A server that is only compiling
  never touches the lock or the other servers' files.
- `parallelism` keeps its current per-server scope and default (cores) but now means CPU only. It is the user's way to leave cores free, and the fix
  for containers whose visible core count is wrong.
- If cross-server CPU oversubscription proves harmful in practice, `parallelism` can later become machine-wide from the `cpuInUse` every server already
  publishes — an isolated change.

### 3.2 What exists, what moves, what goes

| Today | Becomes |
|---|---|
| `TaskDag.Dag` (pure, `TaskDag.scala:556-660`) | kept |
| running set in `executor.execute`'s `runningRef` (`TaskDag.scala:1487-1493`) | kept; the executor publishes its ready set and starts what it is granted |
| DAG admission via `tryReserve` / blocking `reserveUntilReleased` head path (`TaskDag.scala:1085-1119`) | **deleted** — decided by the scheduler |
| `MachineResources` (governor, `grantEligible`, `isContended`, `activeCompiles`, budget) | **deleted** |
| `retuneLoop`, `forkMemoryBudgetMb`, `MachineMemory.budgetFor`, `slackMb` | **deleted** |
| `ForkCostModel` | **deleted** — nothing is learned |
| `JvmPool`: `reserveMemoryForSpawn`, `evictOneIdle`, pool-or-destroy in `release`, per-pool `semaphore`, `startLimiter` | **deleted** — decided by the scheduler |
| `JvmPool`/`ManagedJvm` process mechanics: spawn, handshake, protocol, `kill`, `describeExit` | kept, behind a daemon-wide fork registry |
| `SharedWorkspaceState` global request registry | **deleted** — requests live in the scheduler, passed structurally |
| `HeapPressureGate.decide` (pure) | kept, called from `decide`, fed the scheduler's own compile count |
| `MachineMemory` / `ProcessMemory` probes | replaced by `MachineProbe` / `ForkProbe` (in-process, all three OSes) |
| `server.json` (client-written identity) | kept unchanged |

## 4. The model

All types are plain data; `decide` is a pure function of them. Sketch, not final names:

```scala
// Read under the lock, every claiming tick
case class MachineView(physicalMb: Long, usedMb: Long, pressure: Pressure, cores: Int, nowMs: Long)
sealed trait Pressure; object Pressure { case object Normal; case object Elevated; case object Critical }

// One user command. `bleep run` does not create one.
case class Request(id: RequestId, kind: RequestKind /* Compile | Test | Link | Sourcegen | ... */, startedAtMs: Long)

// Something a request's DAG could start now, in the DAG's priority order
sealed trait Demand { def request: RequestId; def taskId: TaskId; def cpu: Int }
case class InHeap(request: RequestId, taskId: TaskId, cpu: Int) extends Demand                      // compile, discover, AP resolve
case class ForkDemand(request: RequestId, taskId: TaskId, kind: ForkKind, key: ForkKey, boundMb: Long, cpu: Int) extends Demand
// ForkKind: TestSuite | TestBatch | Sourcegen | AnnotationProcessor | Ksp | Link | PostCompile

sealed trait ForkState
object ForkState {
  case object Starting extends ForkState                    // charged at boundMb
  case class Measured(footprintMb: Long, atMs: Long)        // in machine usedMb; remeasured at most every 1 s
}
case class RunningFork(id: ForkId, pid: Option[Long], owner: RequestId, key: ForkKey, kind: ForkKind, boundMb: Long,
                       state: ForkState, busy: Boolean, startedAtMs: Long)

case class MyState(requests: List[Request], forks: List[RunningFork], cpuInUse: Int, ready: List[Demand],
                   unstartedSuitesByKey: Map[ForkKey, Int], heap: HeapUsage, compilesRunning: Int)

sealed trait LockState
object LockState { case object Held; case class Unavailable(holder: String, heldForMs: Long) }

case class Params(headroomMb: Long /* OPEN, see §11 */, parallelism: Int /* this server's cpu slots; user config, re-read on change */,
                  heapThreshold: Double, maxNewForksPerTick: Int /* = 1 */)

case class Decision(reuse: List[(ForkDemand, ForkId)], spawn: List[ForkDemand], admitInHeap: List[InHeap],
                    evict: List[ForkId], publish: StateJson)

def decide(view: MachineView, others: List[StateJson], me: MyState, lock: LockState, params: Params): Decision
```

`boundMb` is the fork's footprint ceiling: its `-Xmx` (the build's `platform.jvmOptions`, else `testRunnerHeap`) plus non-heap overhead
(`heap + max(256, heap/4)`, `MachineResources.forkFootprintMb`).

## 5. The decision rules

In order. Every rule is a pure function of the inputs above.

1. **Room.** `ceiling = physical − headroom`. `room = ceiling − usedMb − Σ boundMb(forks in Starting, all servers)`.
   A fork is charged its bound from admission until it has run one full second; then it is measured, its memory is in `usedMb`, and it is
   remeasured at most once a second (measurements are for display and eviction choice, never for prediction).
2. **Pressure.** `Elevated` → no *new* forks beyond guarantees; a fork that already exists is reused as usual (an idle warm fork, or this request's busy
   shared fork), subject to the cpu slot — its memory is spent whether or not it works, unless rule 3 decides to evict it, which comes first. `Critical`
   → additionally evict every idle fork of this server, so nothing idle is left to reuse.
3. **Idle forks.** An idle fork stays warm **iff its key still has unstarted suites**; otherwise it is evicted immediately. Under room shortage, idle
   forks are evicted (oldest first) before anything new is admitted — only as many as make that admission fit; if none would, none is evicted. Evicted
   forks stay in the state, flagged, until their exit is reported: the process holds its memory until then. Guaranteed *reuse* (rule 5) runs before
   these evictions, so Critical pressure never evicts a warm fork only to spawn a cold one for the same key; guaranteed *spawns* run after them.
4. **Warm first.** For a ready test suite, in order of cost: an idle warm fork of the same key (free; another request's idle fork too, which then changes
   owner) → join this request's busy per-project shared fork of the same key → a new fork. A new fork costs memory, startup time and the per-tick spawn
   allowance; reuse costs none of them.
5. **Guarantee: one fork per command.** A request with no *working* fork — one running work for it; an idle fork of its own does not count, since the
   request is not progressing on it — and a ready `ForkDemand` gets one, regardless of `room`, `parallelism`, pressure or lock state: an idle warm fork
   of the key if there is one (unlimited), else a new fork. A per-project shared fork counts as that one fork. A request that is only compiling gets one
   in-heap slot the same way. Sourcegen, annotation processors, KSP, link count as forks; `bleep run` creates no request.
   **A guaranteed spawn is still a spawn**: it counts against `maxNewForksPerTick` and takes the slot before any non-guaranteed spawn, oldest request
   first. A guarantee that needs a new fork may therefore take several ticks to be met; it is never skipped.
6. **Beyond the guarantee.** Admit ready `ForkDemand`s in priority order — interleaved by rank across requests, oldest request first within a rank — while
   the demand fits the machine `room`, fits **this server's** cpu slots (`cpuInUse + demand.cpu ≤ parallelism`), and at most `maxNewForksPerTick = 1`
   new fork this tick in total (rule 5's included). New forks only when `lock = Held`; reusing a warm fork needs only the cpu slot.
7. **Compiles.** `InHeap` demands need a local cpu slot and `HeapPressureGate.decide` = `Admit`. No machine room, no lock: their memory is the server's
   heap, already in `usedMb`.
8. **Publish.** New forks enter `publish` as `Starting` with their bound, so the next lock holder counts them.

### 5.1 Idle servers yield their memory

A server that holds memory nobody is using, while someone else needs memory, shuts itself down. Conditions — all of them:

- **No connected clients.** Any open non-observer connection counts as in use, whether or not it has a request running — in practice an IDE through
  `bleep bsp`, or an MCP tool call in flight. Observers (`bleep server top`/`status`) do not count — the same rule as today's idle watchdog
  (`connectionRegistry.nonObserverCount`, `BspServerDaemon.scala` ~line 403). Between MCP calls a server is unconnected and may yield (see below).
- **Idle for a while**: no request for at least `idleYieldAfter` (open, §11).
- **Someone needs the memory**: another live server's `state.json` has `wantsMore`, or `Pressure ≥ Elevated`. Low memory with nobody waiting is not a
  reason to throw away a warm server.

Mechanics:

- An idle server's request tick returns immediately, so idle servers run their own slow check (every few seconds): one probe call for pressure, and the
  other servers' `state.json` read without the lock. Only a server about to shut down takes `machine.lock`.
- **One server yields per tick, decided under the lock**: it marks itself `shuttingDown` in its `state.json` so others do not also go; the
  longest-idle one goes first.
- Shutdown is the existing clean path (close the server socket; lock and pid/socket files released), so the next command simply spawns a fresh server.

Prerequisites found in the code:

- **`bleep bsp` does not reconnect.** When its server goes away, `BspProxy` closes its output and the IDE sees "build server disconnected". That is why
  a connected client blocks yielding.
- **MCP reconnects on its own.** `bleep mcp-server` runs `BspRifle.ensureRunning` + `connectWithRetry` for every tool call
  (`BleepMcpServer.scala` ~line 117), so a yielded server is replaced on the next call at the cost of one cold start. No presence connection is needed.
  `bleep server top`'s "started by bleep mcp-server … keeps this server in use" is only a label derived from the parent process; it should be reworded
  once yielding exists.

### 5.2 Busy servers shed the caches of idle workspaces

A server that is working, but serves several workspaces (worktrees), holds cached state for all of them: the resolved build in `BuildCache` (exploded
model, classpaths) and the Zinc analyses in `AnalysisCache`. Under the same trigger as §5.1 — `Pressure ≥ Elevated`, or another server `wantsMore` — it
drops that state for every workspace with no active request:

- `BuildCache.evict(workspace, variant)` (`BuildCache.scala` ~line 90) for each idle workspace, which also releases its analyses through `AnalysisCache`.
  Today these caches only shed by count (`maxCachedWorkspaces`) and age; this adds memory need as a reason.
- The cost is a cold load (build resolve, analysis read from disk) the next time that workspace is used — no correctness impact.
- Freed heap only reaches the machine when the GC returns it. The server runs ZGC with `-XX:ZUncommitDelay=30 -XX:ZCollectionInterval=5`
  (`BspRifleConfig.scala` ~line 117), so within ~30–35 s; where ZGC is not used (older JDKs, see "ZGC only where generational"), G1 returns memory far
  less readily — the shed still prevents growth but may not shrink the footprint.
- It is a graduated response: a busy server sheds idle workspaces (§5.2); an idle server sheds itself (§5.1). An idle server could take the §5.2 step
  first and yield only if that is not enough (open, §11).
- Decided per tick from data the tick already has (pressure, others' `wantsMore`, this server's active requests); no lock needed — it releases memory,
  it never claims it.

`parallelism` is per server, CPU only, read from the user config (re-read on change). Lowering it never kills running work; it is respected as work
finishes. A fork holds cpu slots while it runs work (a batch fork as many as suites it runs at once, as today); an idle warm fork holds none.

## 6. Shared state

### 6.1 Files

```
~/Library/Caches/build.bleep/machine.lock               ← the only file the scheduler locks (global)
~/Library/Caches/build.bleep/socket/<hash>/
    server.json      ← identity, written by the client at spawn — unchanged
    state.json       ← live scheduler state, written by the server — new
    lock, pid, socket, metrics.jsonl, output            ← unchanged
```

(`UserPaths.cacheDir`; the equivalent cache dirs on Linux/Windows.)

- Each server writes **only its own** `state.json`, as `state.json.tmp` + atomic rename. Nobody rewrites another server's data; a bug in one server cannot
  corrupt another's state; a crash mid-write leaves the previous version.
- Discovery reuses `ServerDirs.scan`. One server per socket dir at a time (the existing per-dir `lock`), so a restarted server simply overwrites
  `state.json` on its first tick.
- A `state.json` is **live iff** `ProcessHandle.of(pid)` exists and its `startInstant` equals the recorded `startedAtEpochMs`. Dead ones are ignored;
  `bleep server prune` removes them with their directory.
- `bleep server top` reads `state.json` files **without the lock**.
- Must be verified on Windows: rename over a file another process has open (JDK NIO opens with delete-sharing — test it in Windows CI).

### 6.2 `state.json` schema v1

```json
{
  "version": 1,
  "pid": 12345,
  "startedAtEpochMs": 1759474800000,
  "bleepVersion": "1.0.0-M15",
  "updatedAtEpochMs": 1759474812345,
  "requests": 2,
  "cpuInUse": 6,
  "wantsMore": true,
  "shuttingDown": false,
  "forks": [
    { "id": 17, "pid": 23456, "kind": "test-batch", "boundMb": 3840, "state": "starting", "startedAtEpochMs": 1759474812000 },
    { "id": 12, "pid": 23401, "kind": "test-suite", "boundMb": 2560, "state": "measured", "footprintMb": 1310, "startedAtEpochMs": 1759474790000 }
  ]
}
```

`cpuInUse` is published for display (`top`) only; no other server's decision uses it (§3.1). `shuttingDown` is §5.1's mark; a server that has set it
is still alive and its forks are still counted until it is gone.

Readers use the fields they know; an entry whose `version` they do not know is read as its `forks` and `cpuInUse` (the fields every version must keep),
and a document without those throws. Not stored because derivable: machine used memory, budgets, the server heap, anything learned.

### 6.3 `machine.lock` — what it protects

**Not the files** (single writer each, atomic rename) and **not ticks in general.** It makes **claiming new resources serial machine-wide**: the step
from "observe the machine and everyone's claims" to "publish my new claims". Without it, two servers each see 8 GB of room and together spend 16 GB.

A tick takes it only if it may *add* memory claims: a new fork beyond the guarantee, or a new guaranteed fork (published under the lock when available).
CPU is local and never needs it. Ticks that only admit compiles, reuse warm forks, evict, or record measurements never wait for it.

## 7. Ticks

- A tick can always be scheduled; with **no active requests it returns immediately** without touching the lock. A tick with no fork to start takes
  no lock and reads no other server's file.
- **Cadence:** finishing a tick schedules the next in `10 ms × number of live servers` — the machine as a whole ticks about every 10 ms.
- **Events tick immediately** (coalesced, like `TaskDag`'s wakeup queue, `TaskDag.scala:1374-1380`): request start/end, task ready/finished, fork ready/exit.
- Ticks run on a **dedicated thread**, not the cats-effect compute pool, so a saturated compile pool cannot delay a lock holder.
- **A dead scheduler kills the server.** A tick that throws (a probe that cannot read the machine, an unreadable `state.json`) ends the scheduler
  thread; the failure is logged, every later call throws it, and `TickRuntime`'s `onDeath` signal is wired by the daemon to its own shutdown (Phase C).
  A server that quietly stopped deciding would hold every request forever with no diagnostic.
- Under the lock, in order: take lock → probe machine (after the lock, so it includes every earlier claim) → read all `state.json` → `decide` → write own
  `state.json` → release in `finally`. Starting and killing processes, logging and anything else happen after release.
- Cost at 10 ms: all probes in-process (§9) — microseconds. The file is rewritten only when this server's entry changed.

## 8. Lock fail-safety

The lock is `FileChannel.lock` — `fcntl` on macOS/Linux, `LockFileEx` on Windows. **The kernel releases it when the process dies, including SIGKILL and
crashes.** A dead server can never hold it. The remaining danger is a holder that is alive but stuck (GC pause, debugger, SIGSTOP, disk stall, a bug).

1. **Tiny, self-contained critical section** (§7): no async boundary, no process start/kill, no logging, no other lock inside it.
2. **One opener.** `fcntl` locks are per process: closing *any* descriptor of the lock file in the server releases the lock. Exactly one code path ever
   opens `machine.lock`.
3. **Holder announces itself.** After acquiring, the holder writes `pid:startedAt acquiredAt=T` into the lock file. On Windows locks are mandatory for the
   locked range, so the lock is taken on a byte range beyond the content, leaving the content readable.
4. **Holder self-monitoring.** Hold time is measured per step; > 50 ms logs loudly with the breakdown.
5. **Bounded wait.** Waiters poll `tryLock` with a ~1 s deadline. On timeout the tick runs with `LockState.Unavailable(holder, heldForMs)` — a modelled
   state, not a swallowed error: rule 5 (guarantees) applies, rule 6 does not; reuse and eviction still apply. Reported in log, metrics and `top`
   ("machine lock held by pid 1234 for 3.2 s"). A guaranteed fork started this way is published on the next successful tick.
6. **Stuck holder's forks still count**: its `state.json` stays (live pid), so its forks are counted conservatively; their real memory is in `usedMb` anyway.
   Recovery is one tick after the holder dies or resumes.

## 9. Probes — in-process, every OS, any JDK

| | Linux | macOS | Windows |
|---|---|---|---|
| used memory | cgroup v2 `memory.current − inactive_file` (working set) when a `memory.max` limit is set, else `/proc/meminfo`: `MemTotal − MemAvailable` | JNI `host_statistics64`: anonymous + wired + **compressor** − **purgeable** pages | JNI `GlobalMemoryStatusEx`: total − available physical |
| physical | cgroup v2 `memory.max` when set, else `MemTotal` | `hw.memsize` | `GlobalMemoryStatusEx` |
| pressure | cgroup v2 `memory.pressure` when limited, else `/proc/pressure/memory` (PSI); absent → `Unavailable` | JNI `sysctlbyname("kern.memorystatus_vm_pressure_level")` | JNI `GlobalMemoryStatusEx` (load, commit vs limit) + `QueryMemoryResourceNotification` |
| fork footprint | `/proc/<pid>/status`: `RssAnon + VmSwap` (same number as `smaps_rollup`, 14 µs instead of 1.7 ms) | JNI `proc_pid_rusage` → `phys_footprint` | JNI `GetProcessMemoryInfo` → `PrivateUsage` |

- Linux is pure file reads, any JDK. macOS and Windows use one small C file called through plain JNI (primitives and `long[]` only, failures as status
  codes), so one implementation per OS works on every JDK. No FFM path (it is unavailable below JDK 22 / 21-preview, which is exactly the gap).
- **Containers:** host-wide `/proc/meminfo` cannot see a cgroup limit, so a limited container reads cgroup v2 instead (table above).
  Used is the cgroup's working set, `memory.current − inactive_file` (from `memory.stat`): inactive page cache is what the kernel drops first at no
  cost, so counting it would make every container doing builds look full. The same figure `docker stats` and the kubelet report, and the cgroup
  counterpart of the host's `MemTotal − MemAvailable`.
- **Purgeable pages (macOS)** are subtracted: apps mark them as discardable caches and the kernel drops them without compressing or swapping, so they are
  not "memory someone must pay to reclaim". The same subtraction Activity Monitor makes for "App Memory".
- A probe call that fails throws. **A missing pressure source is not a failure**: `RawPressure.Unavailable(reason)` (e.g. a Linux kernel without PSI,
  or RHEL's `psi=0` default) — a warning once at startup and in `top`, and the pressure brake is off; room still works (§9.1).
- On Windows the JDK's `OperatingSystemMXBean` returns the same `GlobalMemoryStatusEx` numbers (verified in CI); JNI is still needed for the low-memory
  notification and `PrivateUsage`.
- **Module:** probes, the C source and the loader live in their own module `bleep-machine-probes` (depends on bleep-core for the `MachineProbe`
  contract); bleep-bsp depends on it. bleep-core (published for scripts) carries no native code; the pure scheduler stays in bleep-core.
- **Native binaries are checked in**, built by CI, next to a SHA-256 of the C source they were built from. Packaging copies the committed files, so no
  developer needs a C toolchain unless they edit the C file, and `build` does not wait for native builds. A parallel CI job rebuilds them, tests the
  probes against the fresh build on every OS, fails if the recorded source hash does not match the C file, and uploads the fresh binaries to commit.
- **Architectures:** macOS ships one universal dylib (arm64 + x86_64, built on the arm64 runner); Windows x64, plus arm64 cross-compiled. The x86_64
  macOS and arm64 Windows slices are built but never run in CI — bleep supports neither — and are marked untested.
- Cost per call: macOS/Windows a few µs, Linux 14–23 µs.

### 9.1 Opting out: unconstrained scheduling

User config `machineScheduling: cooperative | unconstrained` (default `cooperative`). `unconstrained` runs the same tick and `decide` without the
machine-wide parts: no `state.json`, no `machine.lock`, no memory room, no pressure brake. What remains: per-server `parallelism`, the heap gate, warm-fork
reuse and idle eviction, one spawn per tick, and the guarantee. A server also runs `unconstrained` — with a loud warning at startup and in `top` — where
probes cannot run (an OS/architecture without a probe library). A missing pressure source alone does not switch modes; it only disables the brake.

### Pressure, normalised

| `Pressure` | macOS | Linux | Windows |
|---|---|---|---|
| `Normal` | level 1 | PSI `some avg10` below threshold | load and commit below thresholds |
| `Elevated` | level 2 (warning) | PSI `some avg10` > T₁ (≈10 %, to be measured) | memory load > ~90 % or commit near limit |
| `Critical` | level 4 (critical) | PSI `full avg10` > 0 | low-memory resource notification set |
| none | — | `RawPressure.Unavailable`: no PSI | — |

`RawPressure.Unavailable` normalises to no pressure signal: rules 2 and 3's pressure clauses do not fire; room, guarantees and eviction work as usual.

`usedMb` vs the ceiling predicts trouble; pressure catches the case where the prediction is fine but the machine is already reclaiming — the 44 GB cliff.

## 10. Implementation sequence

One commit per change. **At no point do two admission authorities coexist**: everything is built unwired, then a single cut-over commit switches and
deletes, then behaviour lands on top.

### Phase A — foundations (new code, unwired, no behaviour change)

1. **Linux probe** — `MachineProbe`/`ForkProbe` traits, `/proc` implementation, fixture tests.
2. **macOS JNI probe** — used (incl. compressor), pressure level, per-pid `phys_footprint`; arm64 only (Intel macOS is dropped); tests on CI.
3. **Windows JNI probe** — `GlobalMemoryStatusEx`, low-memory notification, `GetProcessMemoryInfo`; built for x64/arm64; tests on Windows CI.
4. **Pressure normalisation** — pure mapping per OS to `Normal/Elevated/Critical`, unit tests.
5. **Model + `decide`** — the types of §4 and the rules of §5, pure. Property tests: Σ new forks never exceed room; every request with nothing running
   gets exactly one fork within a tick regardless of room/lock/pressure/cpu; warm reuse before spawn; idle evicted before new admissions; ≤ 1 new fork per
   tick; this server's cpu ≤ parallelism beyond guarantees; compiles never depend on room or lock.
6. **`state.json`** — schema v1, atomic write, read, liveness via pid + start instant, `ServerDirs` integration; two-writer tests in a temp dir.
7. **`machine.lock`** — single opener, holder announcement, byte-range lock for Windows, bounded `tryLock`, hold-time measurement; multi-process tests
   (two small JVMs, not forks of real work), including SIGKILL of the holder and a SIGSTOPped holder.
8. **Tick runtime** — dedicated thread, event coalescing, cadence `10 ms × live servers`, empty-requests fast path, critical-section order; driven by fakes.

### Phase B — structural refactors (behaviour unchanged)

9. **Request registry out of the global** — replace `SharedWorkspaceState` with a registry passed structurally; `bleep/status` reads from it.
10. **Daemon-wide fork registry** — lift `ManagedJvm` tracking out of the per-request `JvmPool` into one registry per server; pools still decide as
    today. Gives the scheduler one place to see every fork.

### Phase C — cut-over (one commit)

11. **Switch to the scheduler and delete the old authority.** The DAG executor publishes ready sets and starts granted tasks; test-fork acquisition asks
    the scheduler for `Reuse | Spawn`; sourcegen/AP/KSP/link forks are demands; the heap gate is fed by the scheduler. Deleted in the same commit:
    `MachineResources`, `retuneLoop`, `forkMemoryBudgetMb`, `MachineMemory`, `budgetFor`, `ForkCostModel`, `reserveMemoryForSpawn`, `evictOneIdle`, the
    pool-or-destroy choice, per-pool `semaphore`, `startLimiter`, and the KSP double reservation. Large, but splitting it would mean two sources of truth.

### Phase D — on top

12. **`bleep server top` / `bleep/status`** show ceiling, used, pending, per-server guarantees and forks from `state.json`, plus lock holder/wait. **Done**:
    admin protocol v2 (`SchedulerDto`), `top` reads every server's `state.json` without the lock, names unconstrained servers and lock holders.
13. **Busy servers shed idle workspaces' caches** (§5.2). **Done**: `MemoryNeed` (pressure ≥ Elevated, or another server's `wantsMore`), the slow check
    every 3 s (provisional) on servers with nothing to claim, `BuildCache.shedIdle`, at most one shed per interval while the need lasts.
14. **Idle servers yield** (§5.1) — the idle-yield check and the `shuttingDown` state; reword `top`'s "keeps this server in use" label. **Done**:
    `idleSinceEpochMs` published in `state.json` (additive to v1), `Yield.candidate`/`Yield.goes`, one per tick under the lock, longest idle first.
15. **Metrics** — tick events (hold time, decision summary, pressure) into `metrics.jsonl`. **Done**: one `scheduler` line per busy second, `pressure`,
    `lock_unavailable`, `cache_shed` and `yield` as they happen; `bleep server metrics` draws them.
16. **Docs** — rewrite `docs/usage/resource-management.mdx` and the compile-server guide; remove mentions of the fork-memory budget; `parallelism`
    documented as CPU-only, per server. **Done.**
17. **End-to-end validation** — two servers on the owner's machine running dlab tests, watched with `bleep server top`; then one small run per OS in CI.
    **Open.**

Also landed in Phase D, by the owner's ruling: in-process linkers (Scala.js, Kotlin/JS) are in-heap `Link` demands answering to the heap gate like compiles;
Kotlin/JS and Kotlin/Native test discovery, which run the linked artifact, are `Discover` forks that report their process.

Optional before Phase A, only if freezes bite while this is built: two small fixes to the *current* code (count compressor pages; subtract the server's
own footprint in the retune), deleted again at step 11. The stopgap available today without code: `bleep server config parallelism 8` and
`test-runner-heap 1g` (the latter does not apply to projects with their own `-Xmx`).

## 11. Open questions

- **Headroom**: how the ceiling (`physical − headroom`) is chosen. **Provisional**: `max(4 GB, RAM/8)` in one place (`MachineSchedulingSetup.provisionalHeadroomMb`),
  no user setting. Still the design's single tunable input; revisit after step 17.
- **Linux PSI thresholds** for `Elevated`/`Critical` — **provisional** (`PressureThresholds.provisional`: some > 10 %); measure on a real Linux machine.
- **Windows thresholds** for memory load / commit — **provisional** (load > 90 %, commit > 0.90).
- **`idleYieldAfter`** — **provisional**: 5 minutes (`Yield.IdleYieldAfterMs`), one named value.
- **Shrink before shutdown?** **Resolved by construction**: an idle server has only idle workspaces, so the §5.2 shed on the same need empties its caches
  first; by the time it has been idle for `idleYieldAfter` the shrink has happened, and the yield is a plain shutdown. No separate step.
- **Slow-check cadence** — **provisional**: 3 s (`Ticker.SlowCheckIntervalMs`), well inside ZGC's ~30 s uncommit.
- **Two-stage test admission** (task slot, then fork, because the fork key needs the classpath computed in the handler): **kept for v1**.
- **In-heap linkers** — **ruled** (Phase D): Scala.js and Kotlin/JS linkers are in-heap `Link` demands, gated like compiles; Kotlin/Native's `konanc` is a fork.
- **Scala Native's clang/lld** — **open, for the owner**. The toolchain spawns them itself through `scala.sys.process`, many at once, with no hook to hand
  them out, so the link's fork stays charged at its bound and reports no process (over-charged, never unaccounted). Options: (a) fork the Scala Native
  linker into its own JVM, as Kotlin/Native already is — the clangs become that process's children and the tree is measured; costs a cold JVM and toolchain
  per link; (b) a multi-pid grant that measures a set of child pids observed under the server — an interface change to the one-pid-per-fork model.
- **Listing-process bound** — **provisional**: 256 MB (`TaskDag.ListingProcessBoundMb`) for node or a native binary listing its suites, until measured.
