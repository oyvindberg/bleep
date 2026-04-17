package bleep.bsp

import bleep.bsp.Outcome.KillReason
import bleep.bsp.TaskDag.{CachePullTask, CachePushTask, TaskResult}
import bleep.bsp.protocol.BleepBspProtocol
import bleep.commands.RemoteCache
import bleep.model.CrossProjectName
import bleep.{model, ProjectDigest, S3Client, Started}
import cats.effect.{Deferred, IO}
import ryddig.Logger

import java.util.concurrent.atomic.AtomicReference
import scala.collection.immutable.SortedMap

/** Runtime state for remote-cache integration within a single BSP compile/test DAG run.
  *
  * Scoped to one `handleCompile` / `handleTest` invocation. Digests are precomputed once (bottom-up through the dep DAG) and shared across all per-project
  * cache operations so we don't re-walk the filesystem N times.
  *
  * When no remote-cache is configured in bleep.yaml OR credentials are missing OR the user passed `--no-cache`, this context is `Disabled`. The DAG won't have
  * cache tasks at all in that case — `isEnabled` controls whether the DAG builder adds them.
  */
/** Implementations may be provided outside this file — in particular, tests supply fakes that don't talk to S3. */
trait RemoteCacheContext {

  /** Whether this build cycle uses the remote cache. When false, no cache tasks appear in the DAG. */
  def isEnabled: Boolean

  /** Attempt a pull for a single project. Always reports Success for DAG purposes; on `Hit` the project is recorded in the internal hit-set so the compile
    * handler can short-circuit.
    */
  def tryPull(project: CrossProjectName): IO[Unit]

  /** Attempt a push for a single project. Blocks until the upload completes. Always reports Success for DAG purposes — upload errors are warnings. */
  def tryPush(project: CrossProjectName): IO[Unit]

  /** True when a prior `tryPull` for this project resolved to a cache hit; the compile handler uses this to skip compilation. */
  def wasHit(project: CrossProjectName): IO[Boolean]
}

object RemoteCacheContext {

  /** Emitter for cache status events — the handler plumbs this to the BSP notification channel. */
  trait EventSink {
    def pullStarted(project: CrossProjectName, timestamp: Long): Unit
    def pullFinished(project: CrossProjectName, status: BleepBspProtocol.Event.CachePullStatus, durationMs: Long, bytes: Long, timestamp: Long): Unit
    def pushStarted(project: CrossProjectName, timestamp: Long): Unit
    def pushFinished(project: CrossProjectName, status: BleepBspProtocol.Event.CachePushStatus, durationMs: Long, bytes: Long, timestamp: Long): Unit
  }

  object EventSink {
    val noop: EventSink = new EventSink {
      def pullStarted(project: CrossProjectName, timestamp: Long): Unit = ()
      def pullFinished(project: CrossProjectName, status: BleepBspProtocol.Event.CachePullStatus, durationMs: Long, bytes: Long, timestamp: Long): Unit = ()
      def pushStarted(project: CrossProjectName, timestamp: Long): Unit = ()
      def pushFinished(project: CrossProjectName, status: BleepBspProtocol.Event.CachePushStatus, durationMs: Long, bytes: Long, timestamp: Long): Unit = ()
    }
  }

  object Disabled extends RemoteCacheContext {
    def isEnabled: Boolean = false
    def tryPull(project: CrossProjectName): IO[Unit] = IO.unit
    def tryPush(project: CrossProjectName): IO[Unit] = IO.unit
    def wasHit(project: CrossProjectName): IO[Boolean] = IO.pure(false)
  }

  private class Enabled(
      started: Started,
      client: S3Client,
      prefix: String,
      digests: SortedMap[CrossProjectName, String],
      hits: AtomicReference[Set[CrossProjectName]],
      logger: Logger,
      eventSink: EventSink
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
            val (status, bytes, isHit) = outcome match {
              case RemoteCache.PullOutcome.Hit(b) =>
                logger.withContext("project", project.value).withContext("bytes", b).info("Remote cache HIT")
                (BleepBspProtocol.Event.CachePullStatus.Hit, b, true)
              case RemoteCache.PullOutcome.AlreadyCompiled =>
                (BleepBspProtocol.Event.CachePullStatus.AlreadyCompiled, 0L, false)
              case RemoteCache.PullOutcome.Miss =>
                (BleepBspProtocol.Event.CachePullStatus.Miss, 0L, false)
              case RemoteCache.PullOutcome.Error(reason) =>
                logger.withContext("project", project.value).withContext("reason", reason).warn("Remote cache pull failed, falling back to compile")
                (BleepBspProtocol.Event.CachePullStatus.Error(reason), 0L, false)
            }
            eventSink.pullFinished(project, status, end - start, bytes, end)
            if (isHit) hits.updateAndGet(_ + project): Unit else ()
          }
      }

    def tryPush(project: CrossProjectName): IO[Unit] =
      digests.get(project) match {
        case None         => IO.unit
        case Some(digest) =>
          IO.blocking {
            val start = System.currentTimeMillis()
            eventSink.pushStarted(project, start)
            val outcome = RemoteCache.tryPush(started, project, digest, client, prefix, force = false)
            val end = System.currentTimeMillis()
            val (status, bytes) = outcome match {
              case RemoteCache.PushOutcome.Success(b) =>
                logger.withContext("project", project.value).withContext("bytes", b).info("Remote cache push succeeded")
                (BleepBspProtocol.Event.CachePushStatus.Success, b)
              case RemoteCache.PushOutcome.AlreadyCached =>
                (BleepBspProtocol.Event.CachePushStatus.AlreadyCached, 0L)
              case RemoteCache.PushOutcome.NotCompiled =>
                (BleepBspProtocol.Event.CachePushStatus.Error("classes dir missing after compile"), 0L)
              case RemoteCache.PushOutcome.NotPortable(reason) =>
                logger.withContext("project", project.value).withContext("reason", reason).warn("Remote cache push skipped: not portable")
                (BleepBspProtocol.Event.CachePushStatus.Error(reason), 0L)
              case RemoteCache.PushOutcome.Error(reason) =>
                logger.withContext("project", project.value).withContext("reason", reason).warn("Remote cache push failed")
                (BleepBspProtocol.Event.CachePushStatus.Error(reason), 0L)
            }
            eventSink.pushFinished(project, status, end - start, bytes, end)
          }
      }

    def wasHit(project: CrossProjectName): IO[Boolean] =
      IO(hits.get().contains(project))
  }

  /** Build DAG-executor handlers for the cache pull and push tasks.
    *
    * Both handlers:
    *   - Honour the task kill signal by racing the IO against `killSignal.get`. If kill wins, report `Killed`.
    *   - Absorb unexpected exceptions from `tryPull`/`tryPush` and report `Success` — the remote cache is best-effort and must never fail the build.
    *     (`RemoteCacheContext.Enabled` already catches `NonFatal` inside `RemoteCache.tryPull/Push`; this outer guard defends against any exception that
    *     slipped past it, e.g. from `IO.blocking` shutdown races.)
    */
  def handlers(
      cacheContext: RemoteCacheContext
  ): (
      (CachePullTask, Deferred[IO, KillReason]) => IO[TaskResult],
      (CachePushTask, Deferred[IO, KillReason]) => IO[TaskResult]
  ) = {
    def raceWithKill(
        killSignal: Deferred[IO, KillReason],
        work: IO[Unit]
    ): IO[TaskResult] = {
      val waitForKill = killSignal.get.map[TaskResult](TaskResult.Killed.apply)
      val absorbed = work.handleError(_ => ()).as[TaskResult](TaskResult.Success)
      IO.race(absorbed, waitForKill).map(_.merge)
    }

    val pullHandler: (CachePullTask, Deferred[IO, KillReason]) => IO[TaskResult] =
      (task, killSignal) =>
        killSignal.tryGet.flatMap {
          case Some(reason) => IO.pure(TaskResult.Killed(reason))
          case None         => raceWithKill(killSignal, cacheContext.tryPull(task.project))
        }

    val pushHandler: (CachePushTask, Deferred[IO, KillReason]) => IO[TaskResult] =
      (task, killSignal) =>
        killSignal.tryGet.flatMap {
          case Some(reason) => IO.pure(TaskResult.Killed(reason))
          case None         => raceWithKill(killSignal, cacheContext.tryPush(task.project))
        }

    (pullHandler, pushHandler)
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
              val client = S3Client.fromConfig(started.logger, config, credentials)
              val prefix = S3Client.keyPrefix(config)
              val digests = ProjectDigest.computeAll(started.build, started.buildPaths)
              val hits = new AtomicReference[Set[CrossProjectName]](Set.empty)
              new Enabled(started, client, prefix, digests, hits, started.logger, eventSink)
          }
      }
}
