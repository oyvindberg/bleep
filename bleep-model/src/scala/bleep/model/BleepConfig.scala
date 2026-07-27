package bleep.model

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

case class BleepConfig(
    compileServerMode: Option[CompileServerMode],
    authentications: Option[Authentications],
    logTiming: Option[Boolean],
    bspServerConfig: Option[BspServerConfig],
    remoteCacheCredentials: Option[RemoteCacheCredentials]
) {
  def compileServerModeOrDefault: CompileServerMode = compileServerMode.getOrElse(CompileServerMode.Shared)
  def bspServerConfigOrDefault: BspServerConfig = bspServerConfig.getOrElse(BspServerConfig.default)
}

object BleepConfig {
  val default = BleepConfig(
    compileServerMode = None,
    authentications = None,
    logTiming = None,
    bspServerConfig = None,
    remoteCacheCredentials = None
  )

  implicit val decoder: Decoder[BleepConfig] = deriveDecoder
  implicit val encoder: Encoder[BleepConfig] = deriveEncoder
}

/** BSP server configuration - applies to compile and test operations */
case class BspServerConfig(
    /** How many operations bleep may run at once on this machine — compiles, test forks, sourcegen. The single concurrency knob; everything else is derived
      * from it. Defaults to all cores.
      *
      * Machine-wide, not per invocation: the compile server is shared, reads this once at startup, and passes it to every fork. A per-client override would
      * mean whichever client happened to spawn the daemon silently configured it for everyone else.
      */
    parallelism: Option[Int],
    /** The same as a fraction of cores, e.g. `0.5` for half. Used when [[parallelism]] is unset. */
    parallelismRatio: Option[Double],
    /** Idle timeout for test suites in minutes — resets each time a test completes */
    testIdleTimeoutMinutes: Option[Int],
    /** Max heap for forked test runner JVMs, e.g. "512m", "2g". None = JVM default */
    testRunnerMaxMemory: Option[String],
    /** Max heap for forked sourcegen JVMs, e.g. "500m", "2g". None = JVM default */
    sourcegenMaxMemory: Option[String],
    /** Max heap for forked KSP runner JVMs (`KSPJvmMain`), e.g. "512m", "1500m". KSP bundles its own Analysis-API kotlinc which is memory-hungry on real builds
      * but tiny on toy fixtures — set this low for integration tests. None = JVM default.
      */
    kspRunnerMaxMemory: Option[String],
    /** Max heap for the compile server JVM, e.g. "4g". None = JVM default */
    compileServerMaxMemory: Option[String],
    /** Heap usage fraction (0.0–1.0) above which new compilations wait for memory. When other compilations are running and heap exceeds this threshold, the
      * server delays starting new compilations until memory is freed. Default: 0.80
      */
    heapPressureThreshold: Option[Double],
    /** Idle timeout on a client connection in minutes: how long the server waits for the next message before dropping the connection. This is per read, not per
      * session, so a long compile does not count against it — the client is silent, but so is the socket only until the next request arrives. Set to 0 to wait
      * forever. Default: 30
      */
    bspReadTimeoutMinutes: Option[Int],
    /** How long the shared compile server stays alive with no connected client before shutting itself down, in minutes. Bounds the daemon proliferation that
      * otherwise accumulates across worktrees, JVM bumps, and client redeploys — an abandoned server reaps itself instead of lingering for days. The clock
      * counts only fully-idle time: it resets whenever a client is connected, so a server mid-compile or serving an editor is never reaped. Set to 0 to disable
      * (stay alive forever). Default: 60
      */
    compileServerIdleTimeoutMinutes: Option[Int],
    /** How many workspaces' resolved builds [[bleep.bsp.BuildCache]] keeps before evicting the least recently used idle one.
      *
      * A resolved build is not small — exploded model, resolved classpaths, the Zinc analysis reachable from it — and the cache used to hold every workspace
      * the daemon had ever seen for its whole lifetime. Measured on a daemon serving 11 worktrees: the post-GC floor climbed monotonically from 3.5GB to 8.2GB
      * of a 12GB heap over 45 minutes, at which point compiles OOM'd at a concurrency of three. Bounding the cache bounds that floor.
      *
      * Only idle workspaces are evicted, so this is a cache size, not a limit on how many workspaces a daemon can serve. Default: 4. None = default.
      */
    maxCachedWorkspaces: Option[Int]
) {
  def effectiveParallelism: Int = {
    val cores = Runtime.getRuntime.availableProcessors
    parallelism
      .orElse(parallelismRatio.map(r => math.max(1, (cores * r).toInt)))
      .getOrElse(cores)
  }

  def effectiveTestIdleTimeoutMinutes: Int =
    testIdleTimeoutMinutes.getOrElse(BspServerConfig.DefaultTestIdleTimeoutMinutes)

  def effectiveHeapPressureThreshold: Double =
    heapPressureThreshold.getOrElse(BspServerConfig.DefaultHeapPressureThreshold)

  /** Idle read timeout in milliseconds, in the form `Socket.setSoTimeout` wants: 0 means wait forever. */
  def effectiveBspReadTimeoutMillis: Int = {
    val minutes = bspReadTimeoutMinutes.getOrElse(BspServerConfig.DefaultBspReadTimeoutMinutes)
    if (minutes < 0) sys.error(s"bspReadTimeoutMinutes must be >= 0 (0 disables the timeout), got $minutes")
    minutes * 60 * 1000
  }

  /** How many workspaces' builds the daemon caches. Always >= 1: the workspace being compiled has to stay cached while it compiles. */
  def effectiveMaxCachedWorkspaces: Int = {
    val n = maxCachedWorkspaces.getOrElse(BspServerConfig.DefaultMaxCachedWorkspaces)
    if (n < 1) sys.error(s"maxCachedWorkspaces must be >= 1, got $n")
    n
  }

  /** How long a fully-idle server waits before self-shutdown, in milliseconds. 0 means never. */
  def effectiveCompileServerIdleTimeoutMillis: Long = {
    val minutes = compileServerIdleTimeoutMinutes.getOrElse(BspServerConfig.DefaultCompileServerIdleTimeoutMinutes)
    if (minutes < 0) sys.error(s"compileServerIdleTimeoutMinutes must be >= 0 (0 disables idle shutdown), got $minutes")
    minutes.toLong * 60L * 1000L
  }
}

object BspServerConfig {
  // 2 min is the right default — no individual test in a healthy build should take longer than that to emit a single event. If a particular environment
  // (typically a cold mac CI runner downloading the Konan prebuilt) needs more, override via `~/.config/bleep/config.yaml` rather than relaxing the default.
  val DefaultTestIdleTimeoutMinutes: Int = 2
  val DefaultHeapPressureThreshold: Double = 0.80

  // Generous enough that an editor left open over lunch keeps its connection, short enough that connections orphaned by a crashed client (which never sends
  // build/exit, so the socket read would otherwise block forever) get reaped the same day.
  val DefaultBspReadTimeoutMinutes: Int = 30

  // An hour of no connected client is well past any editor's think-time or a paused CLI session, but short enough that daemons abandoned by closed worktrees,
  // JVM bumps, or client redeploys reap themselves within the day instead of piling up. The clock only counts fully-idle time (resets while any client is
  // connected), so this never interrupts a compile or a live editor.
  val DefaultCompileServerIdleTimeoutMinutes: Int = 60

  // Four builds is enough that an editor plus a couple of CLI worktrees all stay warm, and few enough that the retained floor stays a fraction of the heap
  // rather than most of it. Evicted entries are reloaded on next use, so this trades a cold build load for headroom.
  val DefaultMaxCachedWorkspaces: Int = 4

  val default: BspServerConfig = BspServerConfig(
    parallelism = None,
    parallelismRatio = None,
    testIdleTimeoutMinutes = None,
    testRunnerMaxMemory = None,
    sourcegenMaxMemory = None,
    kspRunnerMaxMemory = None,
    compileServerMaxMemory = None,
    heapPressureThreshold = None,
    bspReadTimeoutMinutes = None,
    compileServerIdleTimeoutMinutes = None,
    maxCachedWorkspaces = None
  )

  implicit val decoder: Decoder[BspServerConfig] = deriveDecoder
  implicit val encoder: Encoder[BspServerConfig] = deriveEncoder
}
