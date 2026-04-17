package bleep
package commands

import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, Future as JFuture}

/** Remote build cache: pull pre-compiled classes from S3, push after building.
  *
  * Cache entries are per-project tar.gz archives keyed by content digest. The digest captures project config, source content, resource content, and transitive
  * dependency digests.
  *
  * Resources affect the digest but are NOT included in the archive (they're already on disk and don't need compilation).
  *
  * Zinc analysis IS included so incremental compilation works correctly after pulling.
  */
object RemoteCache {

  private val Parallelism = 16

  /** Outcome of a pull attempt for a single project */
  sealed trait PullOutcome
  object PullOutcome {
    case object AlreadyCompiled extends PullOutcome

    /** Cache HIT — classes and analysis were extracted into targetDir */
    case class Hit(bytes: Long) extends PullOutcome

    /** Cache MISS — server returned 404/non-200 */
    case object Miss extends PullOutcome

    /** Something went wrong (network error, extraction failure, etc.). Treat as miss but surface the reason */
    case class Error(reason: String) extends PullOutcome
  }

  /** Outcome of a push attempt for a single project */
  sealed trait PushOutcome
  object PushOutcome {
    case object NotCompiled extends PushOutcome
    case object AlreadyCached extends PushOutcome

    /** Cache upload succeeded */
    case class Success(bytes: Long) extends PushOutcome

    /** Portability check rejected the upload (absolute paths in analysis) */
    case class NotPortable(reason: String) extends PushOutcome

    /** Something went wrong (network error, archive error, etc.). */
    case class Error(reason: String) extends PushOutcome
  }

  case class Pull(projects: Array[model.CrossProjectName]) extends BleepBuildCommand {
    override def run(started: Started): Either[BleepException, Unit] =
      configOpt(started) match {
        case None =>
          Left(new BleepException.Text("No remote-cache configured in bleep.yaml. Add:\n  remote-cache:\n    uri: s3://bucket/prefix\n    region: us-east-1"))
        case Some(config) =>
          val credentials = resolveCredentials(started)
          val client =
            S3Client.fromConfig(started.logger, config, credentials, started.config.bspServerConfigOrDefault.effectiveRemoteCacheRequestTimeoutSeconds)
          val prefix = S3Client.keyPrefix(config)

          val digests = ProjectDigest.computeAll(started.build, started.buildPaths)
          val projectsToPull = if (projects.nonEmpty) projects.toSet else digests.keySet

          val pulled = AtomicInteger(0)
          val skipped = AtomicInteger(0)
          val notFound = AtomicInteger(0)
          val errored = AtomicInteger(0)

          val executor = Executors.newVirtualThreadPerTaskExecutor()
          val futures = new java.util.ArrayList[JFuture[?]]()

          projectsToPull.toList.sorted.foreach { crossName =>
            digests.get(crossName) match {
              case None =>
                started.logger.warn(s"Project ${crossName.value} not in build, skipping")
              case Some(digest) =>
                futures.add(
                  executor.submit(
                    (
                        () =>
                          tryPull(started, crossName, digest, client, prefix) match {
                            case PullOutcome.AlreadyCompiled =>
                              skipped.incrementAndGet()
                              started.logger.debug(s"${crossName.value}: already compiled, skipping")
                            case PullOutcome.Hit(bytes) =>
                              pulled.incrementAndGet()
                              started.logger.info(s"${crossName.value}: pulled from cache (${bytes / 1024}KB)")
                            case PullOutcome.Miss =>
                              notFound.incrementAndGet()
                              started.logger.debug(s"${crossName.value}: not in cache")
                            case PullOutcome.Error(reason) =>
                              errored.incrementAndGet()
                              started.logger.warn(s"${crossName.value}: pull error: $reason")
                          }
                    ): Runnable
                  )
                )
            }
          }

          futures.forEach(_.get())
          executor.shutdown()

          started.logger.info(
            s"Remote cache pull: ${pulled.get()} pulled, ${skipped.get()} already compiled, ${notFound.get()} not cached, ${errored.get()} errors (${projectsToPull.size} total)"
          )
          Right(())
      }
  }

  case class Push(projects: Array[model.CrossProjectName], force: Boolean) extends BleepBuildCommand {

    override def run(started: Started): Either[BleepException, Unit] =
      configOpt(started) match {
        case None =>
          Left(new BleepException.Text("No remote-cache configured in bleep.yaml"))
        case Some(config) =>
          val credentials = resolveCredentials(started)
          val client =
            S3Client.fromConfig(started.logger, config, credentials, started.config.bspServerConfigOrDefault.effectiveRemoteCacheRequestTimeoutSeconds)
          val prefix = S3Client.keyPrefix(config)

          val digests = ProjectDigest.computeAll(started.build, started.buildPaths)
          val projectsToPush = if (projects.nonEmpty) projects.toSet else digests.keySet

          val pushed = AtomicInteger(0)
          val skipped = AtomicInteger(0)
          val notCompiled = AtomicInteger(0)
          val errors = new java.util.concurrent.ConcurrentLinkedQueue[String]()

          val semaphore = new java.util.concurrent.Semaphore(Parallelism)
          val executor = Executors.newVirtualThreadPerTaskExecutor()
          val futures = new java.util.ArrayList[JFuture[?]]()

          projectsToPush.toList.sorted.foreach { crossName =>
            digests.get(crossName) match {
              case None =>
                started.logger.warn(s"Project ${crossName.value} not in build, skipping")
              case Some(digest) =>
                futures.add(executor.submit((() => {
                  semaphore.acquire()
                  val outcome =
                    try tryPush(started, crossName, digest, client, prefix, force)
                    finally semaphore.release()
                  outcome match {
                    case PushOutcome.NotCompiled =>
                      notCompiled.incrementAndGet()
                      started.logger.debug(s"${crossName.value}: not compiled, skipping")
                    case PushOutcome.AlreadyCached =>
                      skipped.incrementAndGet()
                      started.logger.debug(s"${crossName.value}: already in cache")
                    case PushOutcome.Success(bytes) =>
                      pushed.incrementAndGet()
                      started.logger.info(s"${crossName.value}: pushed to cache (${bytes / 1024}KB)")
                    case PushOutcome.NotPortable(reason) =>
                      errors.add(s"${crossName.value}: $reason")
                    case PushOutcome.Error(reason) =>
                      errors.add(s"${crossName.value}: $reason")
                  }
                }): Runnable))
            }
          }

          futures.forEach(_.get())
          executor.shutdown()

          val errorList = scala.jdk.CollectionConverters.IterableHasAsScala(errors).asScala.toList
          if (errorList.nonEmpty) {
            started.logger.info(
              s"Remote cache push: ${pushed.get()} pushed, ${errorList.size} failed, ${skipped.get()} already cached, ${notCompiled.get()} not compiled (${projectsToPush.size} total)"
            )
            Left(
              new BleepException.Text(
                s"${errorList.size} project(s) failed to push:\n${errorList.map(e => s"  - $e").mkString("\n")}"
              )
            )
          } else {
            started.logger.info(
              s"Remote cache push: ${pushed.get()} pushed, ${skipped.get()} already cached, ${notCompiled.get()} not compiled (${projectsToPush.size} total)"
            )
            Right(())
          }
      }
  }

  /** Attempt a pull for a single project. Never throws — network/io errors are returned as PullOutcome.Error. */
  def tryPull(
      started: Started,
      crossName: model.CrossProjectName,
      digest: String,
      client: S3Client,
      prefix: String
  ): PullOutcome = {
    val projectPaths = started.buildPaths.project(crossName, started.build.explodedProjects(crossName))
    if (Files.isDirectory(projectPaths.classes) && Files.list(projectPaths.classes).findAny().isPresent) {
      PullOutcome.AlreadyCompiled
    } else {
      val key = cacheKey(prefix, crossName, digest)
      try
        if (client.headObject(key)) {
          val archive = client.getObject(key)
          TarGz.unpack(archive, projectPaths.targetDir)
          PullOutcome.Hit(archive.length.toLong)
        } else {
          PullOutcome.Miss
        }
      catch {
        case scala.util.control.NonFatal(ex) =>
          PullOutcome.Error(s"${ex.getClass.getSimpleName}: ${ex.getMessage}")
      }
    }
  }

  /** Attempt a push for a single project. Never throws — network/io errors are returned as PushOutcome.Error. */
  def tryPush(
      started: Started,
      crossName: model.CrossProjectName,
      digest: String,
      client: S3Client,
      prefix: String,
      force: Boolean
  ): PushOutcome = {
    val projectPaths = started.buildPaths.project(crossName, started.build.explodedProjects(crossName))
    if (!Files.isDirectory(projectPaths.classes) || !Files.list(projectPaths.classes).findAny().isPresent) {
      PushOutcome.NotCompiled
    } else {
      val key = cacheKey(prefix, crossName, digest)
      try
        if (!force && client.headObject(key)) PushOutcome.AlreadyCached
        else {
          val absolutePaths = checkPortability(projectPaths.targetDir)
          absolutePaths match {
            case head :: _ =>
              PushOutcome.NotPortable(
                s"analysis contains ${absolutePaths.size} absolute path(s), e.g. '$head'. Kill BSP servers and recompile."
              )
            case Nil =>
              val archive = TarGz.pack(projectPaths.targetDir)
              client.putObject(key, archive)
              PushOutcome.Success(archive.length.toLong)
          }
        }
      catch {
        case scala.util.control.NonFatal(ex) =>
          PushOutcome.Error(s"${ex.getClass.getSimpleName}: ${ex.getMessage}")
      }
    }
  }

  /** Read portability warnings written by the BSP server after compile. Returns list of absolute paths (empty = portable). */
  private def checkPortability(targetDir: Path): List[String] = {
    val warningsFile = targetDir.resolve(".zinc/portability-warnings")
    if (!Files.exists(warningsFile)) return Nil
    Files.readString(warningsFile).linesIterator.filter(_.nonEmpty).toList
  }

  /** Cache configuration from bleep.yaml, if present. */
  def configOpt(started: Started): Option[model.RemoteCacheConfig] =
    started.build match {
      case fb: model.Build.FileBacked => fb.file.`remote-cache`
      case _                          => None
    }

  def cacheKey(prefix: String, crossName: model.CrossProjectName, digest: String): String = {
    val projectKey = crossName.value.replace('/', '-')
    if (prefix.isEmpty) s"$projectKey/$digest.tar.gz"
    else s"$prefix/$projectKey/$digest.tar.gz"
  }

  def resolveCredentials(started: Started): model.RemoteCacheCredentials =
    started.config.remoteCacheCredentials
      .orElse(model.RemoteCacheCredentials.fromEnv())
      .getOrElse(
        throw new BleepException.Text(
          "No remote cache credentials found. Set remoteCacheCredentials in ~/.config/bleep/config.yaml or BLEEP_REMOTE_CACHE_S3_ACCESS_KEY_ID/BLEEP_REMOTE_CACHE_S3_SECRET_ACCESS_KEY env vars."
        )
      )

  def resolveCredentialsOpt(started: Started): Option[model.RemoteCacheCredentials] =
    started.config.remoteCacheCredentials.orElse(model.RemoteCacheCredentials.fromEnv())
}
