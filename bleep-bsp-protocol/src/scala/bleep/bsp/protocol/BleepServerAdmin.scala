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

  /** Bumped only when the payload shape changes incompatibly. Additive fields ride along on the absent-tolerant decoders. */
  val ProtocolVersion = 1

  val StatusMethod = "bleep/status"
  val ShutdownMethod = "bleep/shutdown"

  /** Methods that must work before `build/initialize`. */
  val Methods: Set[String] = Set(StatusMethod, ShutdownMethod)
}

/** @param observer
  *   true when the caller is only looking. Observer connections neither keep the daemon alive nor refresh its idle clock, so watching your servers never
  *   changes their lifecycle. Every read issued by `bleep server` sets this.
  */
case class StatusRequest(observer: Boolean)

object StatusRequest {
  implicit val codec: Codec[StatusRequest] = deriveCodec
}

/** One entry in the machine governor's ledger — a running or queued piece of work. */
case class MachineEntryDto(kind: String, label: String, cpu: Int, memoryMb: Long, ageMs: Long)

object MachineEntryDto {
  implicit val codec: Codec[MachineEntryDto] = deriveCodec
}

/** The resource governor's view: what the daemon is doing right now, and what is queued behind it. `waiting` is the queue depth `top` needs. */
case class MachineSnapshotDto(
    totalCpu: Int,
    usedCpu: Int,
    totalMemoryMb: Long,
    usedMemoryMb: Long,
    activeCompiles: Int,
    active: List[MachineEntryDto],
    waiting: List[MachineEntryDto]
)

object MachineSnapshotDto {
  implicit val codec: Codec[MachineSnapshotDto] = deriveCodec
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
    testRunnerMaxMemory: Option[String],
    maxCachedWorkspaces: Int,
    bspReadTimeoutMillis: Long,
    compileServerIdleTimeoutMillis: Long,
    testIdleTimeoutMinutes: Int,
    heapPressureThreshold: Double
)

object ServerConfigDto {
  implicit val codec: Codec[ServerConfigDto] = deriveCodec
}

/** Everything `bleep server status` and the `top` TUI render, in one round trip.
  *
  * Assembled from state the daemon already held but could never expose: the governor snapshot, the two caches, the JVM sampler, the connection registry, and
  * the config it booted with.
  */
case class DaemonStatus(
    adminProtocolVersion: Int,
    bleepVersion: String,
    pid: Long,
    startedAtEpochMs: Long,
    socketDir: String,
    jvm: JvmStats,
    machine: MachineSnapshotDto,
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
