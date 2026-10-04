package bleep.bsp.protocol

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

/** The bleep admin surface: what a daemon can be asked about itself, over the same JSON-RPC channel clients already use.
  *
  * These methods are deliberately exempt from the initialize gate. An observer connects, asks, and leaves without ever shipping a build — `bleep server ls`
  * must work from any directory, including one with no bleep.yaml at all.
  *
  * Version skew is expected here and is the whole point: `ls` and `kill` have to see daemons from *older* bleep versions, because those are exactly the ones
  * hogging memory. An old daemon answers an unknown method with JSON-RPC -32601 MethodNotFound, which is a loud, machine-detectable "too old" rather than a
  * half-populated response.
  */
object BleepServerAdmin {

  /** Bumped only when the payload shape changes incompatibly. Additive fields ride along on the absent-tolerant decoders.
    *
    * v2: the resource governor's `machine` ledger became the machine scheduler's `scheduler` view.
    */
  val ProtocolVersion = 2

  val StatusMethod = "bleep/status"
  val ShutdownMethod = "bleep/shutdown"
  val CopyStateMethod = "bleep/copyState"

  /** Methods that must work before `build/initialize`. */
  val Methods: Set[String] = Set(StatusMethod, ShutdownMethod, CopyStateMethod)
}

/** Copy one workspace's compiled state into a freshly created git worktree, so its first build starts from the parent's incremental baseline instead of cold.
  *
  * Runs in the daemon on purpose: it takes the same per-project locks compiles take (shared on each source project), so state is never copied mid-compile — the
  * one guarantee a client-side copy cannot give while other agents keep compiling the parent.
  *
  * `from`/`to` are absolute workspace roots (the directories containing bleep.yaml). `variant` is the build-variant directory name, defaulting to "normal".
  */
case class CopyStateRequest(from: String, to: String, variant: Option[String])

object CopyStateRequest {
  implicit val codec: Codec[CopyStateRequest] = deriveCodec
}

/** @param projects
  *   cross-project names whose state was copied
  */
/** @param bytesCopied
  *   logical size of the state that landed in the target worktree (apparent file sizes, summed after the clone). On APFS the clone shares blocks with the
  *   source, so this measures how much compiled state the new worktree starts with — not extra disk consumed.
  */
case class CopyStateResponse(projects: List[String], durationMs: Long, bytesCopied: Long)

object CopyStateResponse {
  implicit val codec: Codec[CopyStateResponse] = deriveCodec
}

/** @param observer
  *   true when the caller is only looking. Observer connections neither keep the daemon alive nor refresh its idle clock, so watching your servers never
  *   changes their lifecycle. Every read issued by `bleep server` sets this.
  */
case class StatusRequest(observer: Boolean)

object StatusRequest {
  implicit val codec: Codec[StatusRequest] = deriveCodec
}

/** What the machine looked like on the scheduler's last probing tick (design §6.1): the probe's reading, not an estimate.
  *
  * @param usedMb
  *   memory in use machine-wide, every process included; the forks the scheduler has measured are in here already
  * @param pressure
  *   `normal`, `elevated`, `critical` or `no-signal` — the OS's memory pressure, normalised (design §9)
  * @param pressureReason
  *   why there is no pressure signal, when `pressure` is `no-signal` or `warming-up`
  * @param sampledAgoMs
  *   how old this reading is: a server that has had nothing to claim has not probed since
  * @param churnPagesPerSecond
  *   macOS: the compressor's churn (compressions + decompressions per second, smoothed) — what the pressure is judged from (design §9)
  * @param pressureLevel
  *   the kernel's own level where it reports one (macOS: 1, 2, 4)
  * @param availableMb
  *   what a new process can take now without the OS reclaiming anything (design §5 rule 1); room for forks is this less the reserve and the starting forks'
  *   charges
  */
case class MachineViewDto(
    physicalMb: Long,
    usedMb: Long,
    availableMb: Long,
    pressure: String,
    pressureReason: Option[String],
    sampledAgoMs: Long,
    churnPagesPerSecond: Option[Long],
    pressureLevel: Option[Int]
)

object MachineViewDto {
  implicit val codec: Codec[MachineViewDto] = deriveCodec
}

/** The outcome of the scheduler's last try for `machine.lock`: `held`, `unavailable` (another server kept it past the wait, and the holder fields name it when
  * it had announced itself) or `not-needed` (nothing to claim, or unconstrained).
  */
case class LockDto(state: String, holderPid: Option[Long], holderStartedAtEpochMs: Option[Long], holderHeldForMs: Option[Long])

object LockDto {
  implicit val codec: Codec[LockDto] = deriveCodec
}

/** A fork the scheduler charges the machine for — a test JVM, a sourcegen script, a linker and the processes its toolchain spawns — as the scheduler sees it.
  *
  * @param pids
  *   the live processes under the grant as last reported; empty until the first exists
  * @param boundMb
  *   what it is charged while `measuredMb` is absent: its heap bound plus overhead
  * @param measuredMb
  *   its process tree's footprint once measured, what it is charged from then on
  * @param busyCpu
  *   cpu slots held by the work running on it; 0 for a warm fork between jobs
  * @param evicting
  *   told to exit, still counted until it has
  */
case class SchedulerForkDto(
    id: Long,
    pids: List[Long],
    request: String,
    kind: String,
    key: String,
    boundMb: Long,
    measuredMb: Option[Long],
    shared: Boolean,
    busyCpu: Int,
    evicting: Boolean,
    ageMs: Long
)

object SchedulerForkDto {
  implicit val codec: Codec[SchedulerForkDto] = deriveCodec
}

/** Work running in the server's own heap — a compile, a test discovery, annotation-processor resolution — holding `cpu` slots and no machine memory. */
case class InHeapTaskDto(request: String, taskId: String, kind: String, cpu: Int)

object InHeapTaskDto {
  implicit val codec: Codec[InHeapTaskDto] = deriveCodec
}

/** Something a request could start that the scheduler has not admitted: it is waiting for a cpu slot, for machine memory (`boundMb` says how much, for a fork),
  * for the lock, or for the heap gate.
  */
case class DemandDto(request: String, taskId: String, kind: String, cpu: Int, boundMb: Option[Long])

object DemandDto {
  implicit val codec: Codec[DemandDto] = deriveCodec
}

/** The machine scheduler's view, as of its last tick (design §10 step 12). This is what the server publishes to the other servers in its `state.json`, plus
  * what only it knows: the machine reading, the lock, and the demands it is holding back.
  *
  * @param mode
  *   `cooperative` — shares the machine's memory with the other servers through `machine.lock` — or `unconstrained`, with `unconstrainedReason` saying why
  * @param parallelism
  *   this server's cpu slots; per server, not machine-wide
  * @param reserveMb
  *   what of the available memory is never given to forks: room is `availableMb - reserveMb - pending`
  * @param liveServers
  *   servers counted on the last claiming tick, this one included
  * @param wantsMore
  *   it has demands it could not admit — the signal that makes idle servers yield and busy ones shed caches
  * @param shuttingDown
  *   it has decided to yield its memory and is on its way out; its forks still count until it is gone
  */
case class SchedulerDto(
    mode: String,
    unconstrainedReason: Option[String],
    parallelism: Int,
    reserveMb: Long,
    machine: Option[MachineViewDto],
    lock: LockDto,
    liveServers: Int,
    requests: Int,
    cpuInUse: Int,
    wantsMore: Boolean,
    shuttingDown: Boolean,
    inHeap: List[InHeapTaskDto],
    forks: List[SchedulerForkDto],
    waiting: List[DemandDto]
) {
  def compilesRunning: Int = inHeap.count(_.kind == SchedulerDto.CompileKind)
}

object SchedulerDto {
  implicit val codec: Codec[SchedulerDto] = deriveCodec

  val Cooperative = "cooperative"
  val Unconstrained = "unconstrained"

  /** `InHeapTaskDto.kind` of a compile, as `bleep.machine.InHeapKind.Compile.json` spells it. */
  val CompileKind = "compile"
}

case class ConnectionDto(
    connId: Int,
    connectedAtEpochMs: Long,
    observer: Boolean,
    clientName: Option[String],
    clientVersion: Option[String],
    workspace: Option[String]
)

object ConnectionDto {
  implicit val codec: Codec[ConnectionDto] = deriveCodec
}

case class OperationDto(operationId: String, operation: String, projects: List[String], startedAgoMs: Long)

object OperationDto {
  implicit val codec: Codec[OperationDto] = deriveCodec
}

case class WorkspaceDto(path: String, buildCached: Boolean, activeOperations: List[OperationDto])

object WorkspaceDto {
  implicit val codec: Codec[WorkspaceDto] = deriveCodec
}

case class BuildCacheDto(cachedWorkspaces: List[String], bound: Int)

object BuildCacheDto {
  implicit val codec: Codec[BuildCacheDto] = deriveCodec
}

case class AnalysisWorkspaceDto(workspace: String, entries: Int, fileBytes: Long)

object AnalysisWorkspaceDto {
  implicit val codec: Codec[AnalysisWorkspaceDto] = deriveCodec
}

case class AnalysisCacheDto(
    entries: Int,
    fileBytes: Long,
    internedClasses: Int,
    sharedAnalyses: Int,
    contentHits: Long,
    perWorkspace: List[AnalysisWorkspaceDto]
)

object AnalysisCacheDto {
  implicit val codec: Codec[AnalysisCacheDto] = deriveCodec
}

/** The config this daemon actually booted with — effective values, not what is on disk now.
  *
  * The difference is the point: these are read once at startup, so editing the config file changes nothing until a restart. `bleep server config show` diffs
  * this against disk and says so out loud rather than letting you believe a setting took effect.
  */
case class ServerConfigDto(
    parallelism: Int,
    /** As written in config — `"4g"`, `"512m"` — or absent when the computed default applies. */
    compileServerMaxMemory: Option[String],
    /** Default heap for test forks; a project's own `-Xmx` overrides it. Absent when bleep's own default applies. */
    testRunnerHeap: Option[String],
    maxCachedWorkspaces: Int,
    bspReadTimeoutMillis: Long,
    compileServerIdleTimeoutMillis: Long,
    testIdleTimeoutMinutes: Int,
    heapPressureThreshold: Double,
    /** `cooperative` or `unconstrained` (see `bleep.model.MachineScheduling`). `Option` so a status from a server without the field still decodes. */
    machineScheduling: Option[String]
)

object ServerConfigDto {
  implicit val codec: Codec[ServerConfigDto] = deriveCodec
}

/** Everything `bleep server status` and the `top` TUI render, in one round trip.
  *
  * Assembled from state the daemon already held but could never expose: the scheduler's snapshot, the two caches, the JVM sampler, the connection registry, and
  * the config it booted with.
  */
case class DaemonStatus(
    adminProtocolVersion: Int,
    bleepVersion: String,
    pid: Long,
    startedAtEpochMs: Long,
    socketDir: String,
    jvm: JvmStats,
    scheduler: SchedulerDto,
    connections: List[ConnectionDto],
    workspaces: List[WorkspaceDto],
    buildCache: BuildCacheDto,
    analysisCache: AnalysisCacheDto,
    config: ServerConfigDto,
    /** How long since this server last did anything for a real client — the clock the idle shutdown counts down.
      *
      * `Option` so that a daemon from before this field existed still decodes: these responses cross versions in practice, since every locally deployed
      * snapshot leaves the previous server running, and a missing field must read as "did not say" rather than failing the whole status.
      */
    idleMs: Option[Long]
)

object DaemonStatus {
  implicit val codec: Codec[DaemonStatus] = deriveCodec
}
