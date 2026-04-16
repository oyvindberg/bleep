package bleep.bsp

import bleep.commands.RemoteCache
import bleep.{model, ProjectDigest, S3Client, Started}
import bleep.bsp.protocol.BleepBspProtocol
import bleep.model.CrossProjectName
import cats.effect.IO
import ryddig.Logger

import scala.collection.immutable.SortedMap

/** Runtime state for remote-cache integration within a single BSP compile/test DAG run.
  *
  * Scoped to one `handleCompile` / `handleTest` invocation. Digests are precomputed once (bottom-up through the dep DAG) and shared across all per-project
  * cache operations so we don't re-walk the filesystem N times.
  *
  * When no remote-cache is configured in bleep.yaml OR credentials are missing OR the user passed `--no-cache`, this context is `Disabled` and the handler
  * short-circuits all cache logic to a no-op.
  */
sealed trait CompileCacheContext {
  def tryPull(crossName: CrossProjectName): IO[CompileCacheContext.PullResult]
  def schedulePush(crossName: CrossProjectName): IO[Unit]
}

object CompileCacheContext {

  /** Result of a pull attempt exposed to the compile handler. `Hit` lets the handler short-circuit compilation. */
  sealed trait PullResult
  object PullResult {
    case object Hit extends PullResult
    case object NotCached extends PullResult
    case object Disabled extends PullResult
  }

  /** Emitter for cache events — the handler plumbs this to the BSP notification channel. */
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

  object Disabled extends CompileCacheContext {
    def tryPull(crossName: CrossProjectName): IO[PullResult] = IO.pure(PullResult.Disabled)
    def schedulePush(crossName: CrossProjectName): IO[Unit] = IO.unit
  }

  private class Enabled(
      started: Started,
      client: S3Client,
      prefix: String,
      digests: SortedMap[CrossProjectName, String],
      logger: Logger,
      eventSink: EventSink
  ) extends CompileCacheContext {

    def tryPull(crossName: CrossProjectName): IO[PullResult] =
      digests.get(crossName) match {
        case None         => IO.pure(PullResult.Disabled)
        case Some(digest) =>
          IO.blocking {
            val start = System.currentTimeMillis()
            eventSink.pullStarted(crossName, start)
            val outcome = RemoteCache.tryPull(started, crossName, digest, client, prefix)
            val end = System.currentTimeMillis()
            val (status, bytes, result) = outcome match {
              case RemoteCache.PullOutcome.Hit(b) =>
                logger.withContext("project", crossName.value).withContext("bytes", b).info("Remote cache HIT")
                (BleepBspProtocol.Event.CachePullStatus.Hit, b, PullResult.Hit)
              case RemoteCache.PullOutcome.AlreadyCompiled =>
                (BleepBspProtocol.Event.CachePullStatus.AlreadyCompiled, 0L, PullResult.NotCached)
              case RemoteCache.PullOutcome.Miss =>
                (BleepBspProtocol.Event.CachePullStatus.Miss, 0L, PullResult.NotCached)
              case RemoteCache.PullOutcome.Error(reason) =>
                logger.withContext("project", crossName.value).withContext("reason", reason).warn("Remote cache pull failed, falling back to compile")
                (BleepBspProtocol.Event.CachePullStatus.Error(reason), 0L, PullResult.NotCached)
            }
            eventSink.pullFinished(crossName, status, end - start, bytes, end)
            result
          }
      }

    def schedulePush(crossName: CrossProjectName): IO[Unit] =
      digests.get(crossName) match {
        case None         => IO.unit
        case Some(digest) =>
          val push = IO.blocking {
            val start = System.currentTimeMillis()
            eventSink.pushStarted(crossName, start)
            val outcome = RemoteCache.tryPush(started, crossName, digest, client, prefix, force = false)
            val end = System.currentTimeMillis()
            val (status, bytes) = outcome match {
              case RemoteCache.PushOutcome.Success(b) =>
                logger.withContext("project", crossName.value).withContext("bytes", b).info("Remote cache push succeeded")
                (BleepBspProtocol.Event.CachePushStatus.Success, b)
              case RemoteCache.PushOutcome.AlreadyCached =>
                (BleepBspProtocol.Event.CachePushStatus.AlreadyCached, 0L)
              case RemoteCache.PushOutcome.NotCompiled =>
                // Shouldn't happen in practice since we only schedule after success, but map to Error
                (BleepBspProtocol.Event.CachePushStatus.Error("classes dir missing after compile"), 0L)
              case RemoteCache.PushOutcome.NotPortable(reason) =>
                logger.withContext("project", crossName.value).withContext("reason", reason).warn("Remote cache push skipped: not portable")
                (BleepBspProtocol.Event.CachePushStatus.Error(reason), 0L)
              case RemoteCache.PushOutcome.Error(reason) =>
                logger.withContext("project", crossName.value).withContext("reason", reason).warn("Remote cache push failed")
                (BleepBspProtocol.Event.CachePushStatus.Error(reason), 0L)
            }
            eventSink.pushFinished(crossName, status, end - start, bytes, end)
          }
          push.start.void
      }
  }

  /** Build a cache context for a compile/test cycle. Returns `Disabled` if cache is unavailable (no config, no credentials, or user opted out). */
  def create(started: Started, noCache: Boolean, eventSink: EventSink): CompileCacheContext =
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
              new Enabled(started, client, prefix, digests, started.logger, eventSink)
          }
      }
}
