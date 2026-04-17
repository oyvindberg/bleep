package bleep.bsp

import bleep.bsp.Outcome.KillReason
import bleep.bsp.TaskDag.{CachePullTask, TaskResult}
import bleep.bsp.protocol.BleepBspProtocol
import bleep.commands.RemoteCache
import bleep.model.CrossProjectName
import bleep.{model, BleepException, ProjectDigest, S3Client, Started}
import cats.effect.{Deferred, IO}
import ryddig.Logger

import java.util.concurrent.atomic.AtomicReference
import scala.collection.immutable.SortedMap

/** Runtime state for remote-cache integration within a single BSP compile/test DAG run.
  *
  * Scoped to one `handleCompile` / `handleTest` invocation. Digests are precomputed once (bottom-up through the dep DAG) and shared across all per-project pull
  * operations so we don't re-walk the filesystem N times.
  *
  * When no remote-cache is configured in bleep.yaml OR credentials are missing OR the user passed `--no-cache`, this context is `Disabled`. The DAG won't have
  * cache tasks at all in that case — `isEnabled` controls whether the DAG builder adds them.
  *
  * Push is NOT handled here — `bleep remote-cache push` is the explicit command for uploading after a successful build.
  *
  * Implementations may be provided outside this file — in particular, tests supply fakes that don't talk to S3.
  */
trait RemoteCacheContext {

  /** Whether this build cycle uses the remote cache. When false, no cache tasks appear in the DAG. */
  def isEnabled: Boolean

  /** Attempt a pull for a single project.
    *
    * Outcomes — no fallback; errors propagate:
    *   - Cache hit: extracts the archive into `targetDir`, records the project as a hit (visible via `wasHit`), returns normally.
    *   - Cache miss / already-compiled locally: returns normally without recording a hit — the compile handler will run normally.
    *   - Any error (network failure, auth failure, corrupted archive, etc.): raises. The DAG handler converts this to `TaskResult.Failure`, downstream compile
    *     is skipped, and the build fails. Pass `--no-cache` to opt out if offline.
    */
  def tryPull(project: CrossProjectName): IO[Unit]

  /** True when a prior `tryPull` for this project resolved to a cache hit; the compile handler uses this to skip compilation. */
  def wasHit(project: CrossProjectName): IO[Boolean]
}

object RemoteCacheContext {

  /** Emitter for cache status events — the handler plumbs this to the BSP notification channel. */
  trait EventSink {
    def pullStarted(project: CrossProjectName, timestamp: Long): Unit
    def pullFinished(project: CrossProjectName, status: BleepBspProtocol.Event.CachePullStatus, durationMs: Long, bytes: Long, timestamp: Long): Unit
  }

  object EventSink {
    val noop: EventSink = new EventSink {
      def pullStarted(project: CrossProjectName, timestamp: Long): Unit = ()
      def pullFinished(project: CrossProjectName, status: BleepBspProtocol.Event.CachePullStatus, durationMs: Long, bytes: Long, timestamp: Long): Unit = ()
    }
  }

  object Disabled extends RemoteCacheContext {
    def isEnabled: Boolean = false
    def tryPull(project: CrossProjectName): IO[Unit] = IO.unit
    def wasHit(project: CrossProjectName): IO[Boolean] = IO.pure(false)
  }

  private class Enabled(
      started: Started,
      client: S3Client,
      prefix: String,
      digests: SortedMap[CrossProjectName, String],
      hits: AtomicReference[Set[CrossProjectName]],
      logger: Logger,
      eventSink: EventSink,
      requestTimeoutSeconds: Int
  ) extends RemoteCacheContext {

    def isEnabled: Boolean = true

    def tryPull(project: CrossProjectName): IO[Unit] =
      digests.get(project) match {
        case None         => IO.unit
        case Some(digest) =>
          IO.blocking {
            val start = System.currentTimeMillis()
            eventSink.pullStarted(project, start)
            val outcome = RemoteCache.tryPull(started, project, digest, client, prefix)
            val end = System.currentTimeMillis()
            outcome match {
              case RemoteCache.PullOutcome.Hit(bytes) =>
                logger.withContext("project", project.value).withContext("bytes", bytes).info("Remote cache HIT")
                eventSink.pullFinished(project, BleepBspProtocol.Event.CachePullStatus.Hit, end - start, bytes, end)
                hits.updateAndGet(_ + project): Unit
              case RemoteCache.PullOutcome.AlreadyCompiled =>
                eventSink.pullFinished(project, BleepBspProtocol.Event.CachePullStatus.AlreadyCompiled, end - start, 0L, end)
              case RemoteCache.PullOutcome.Miss =>
                eventSink.pullFinished(project, BleepBspProtocol.Event.CachePullStatus.Miss, end - start, 0L, end)
              case RemoteCache.PullOutcome.Error(reason) =>
                eventSink.pullFinished(project, BleepBspProtocol.Event.CachePullStatus.Error(reason), end - start, 0L, end)
                throw new BleepException.Text(
                  s"Remote cache pull failed for ${project.value}: $reason. " +
                    s"Pass --no-cache to opt out, or increase the request timeout (currently ${requestTimeoutSeconds}s) with:\n" +
                    s"  bleep config remote-cache request-timeout <seconds>"
                )
            }
          }
      }

    def wasHit(project: CrossProjectName): IO[Boolean] =
      IO(hits.get().contains(project))
  }

  /** Build the DAG-executor handler for `CachePullTask`.
    *
    * Races the pull IO against the kill signal. If kill wins, report `Killed`. Otherwise errors propagate naturally through the executor's `withRecovery` into
    * `TaskResult.Failure` — no fallback, no swallowing.
    */
  def pullHandler(
      cacheContext: RemoteCacheContext
  ): (CachePullTask, Deferred[IO, KillReason]) => IO[TaskResult] =
    (task, killSignal) =>
      killSignal.tryGet.flatMap {
        case Some(reason) => IO.pure(TaskResult.Killed(reason))
        case None         =>
          val waitForKill = killSignal.get.map[TaskResult](TaskResult.Killed.apply)
          val doPull = cacheContext.tryPull(task.project).as[TaskResult](TaskResult.Success)
          IO.race(doPull, waitForKill).map(_.merge)
      }

  /** Build a cache context for a compile/test cycle. Returns `Disabled` if cache is unavailable (no config, no credentials, or user opted out).
    *
    * Performs the `ProjectDigest.computeAll` walk synchronously — this runs once at the top of `handleCompile`/`handleTest` before the DAG starts executing.
    */
  def create(started: Started, noCache: Boolean, eventSink: EventSink): RemoteCacheContext =
    if (noCache) Disabled
    else
      RemoteCache.configOpt(started) match {
        case None         => Disabled
        case Some(config) =>
          RemoteCache.resolveCredentialsOpt(started) match {
            case None =>
              started.logger.warn("remote-cache configured but no credentials available; disabling cache for this build")
              Disabled
            case Some(credentials) =>
              val timeoutSeconds = started.config.bspServerConfigOrDefault.effectiveRemoteCacheRequestTimeoutSeconds
              val client = S3Client.fromConfig(started.logger, config, credentials, timeoutSeconds)
              val prefix = S3Client.keyPrefix(config)
              val digests = ProjectDigest.computeAll(started.build, started.buildPaths)
              val hits = new AtomicReference[Set[CrossProjectName]](Set.empty)
              new Enabled(started, client, prefix, digests, hits, started.logger, eventSink, timeoutSeconds)
          }
      }
}
