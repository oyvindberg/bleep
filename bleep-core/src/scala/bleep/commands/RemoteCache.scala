package bleep
package commands

import bleep.analysis.{NoopManifestStore, ProjectCompileSuccess, ProjectLanguage}

import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, Future as JFuture}
import scala.jdk.StreamConverters.*

/** Remote build cache: pull pre-compiled classes from S3, push after building.
  *
  * Cache entries are per-project tar.gz archives keyed by content digest. The digest captures project config, source content, resource content, and transitive
  * dependency digests.
  *
  * Resources affect the digest but are NOT included in the archive (they're already on disk and don't need compilation).
  *
  * Zinc analysis IS included so incremental compilation works correctly after pulling. The noop manifest is per-machine state and is excluded from the archive
  * — Pull regenerates it locally so the next compile is a true noop hit.
  */
object RemoteCache {

  private val Parallelism = 16

  /** Per-machine noop manifest is regenerated locally after pull, never shipped. */
  private val NoopManifestFileName = "noop-manifest.bin"

  /** Predicate used to filter files when packing an archive for upload. Exposed so tests can verify the same exclusion behavior as production. */
  private[bleep] def packFilter(p: Path): Boolean =
    p.getFileName.toString != NoopManifestFileName

  case class Pull(projects: Array[model.CrossProjectName]) extends BleepBuildCommand {
    override def run(started: Started): Either[BleepException, Unit] = {
      val cacheConfig = started.build.remoteCache
      cacheConfig match {
        case None =>
          Left(new BleepException.Text("No remote-cache configured in bleep.yaml. Add:\n  remote-cache:\n    uri: s3://bucket/prefix\n    region: us-east-1"))
        case Some(config) =>
          val credentials = resolveCredentials(started)
          val client = S3Client.fromConfig(started.logger, config, credentials)
          val prefix = S3Client.keyPrefix(config)

          val digests = ProjectDigest.computeAll(started.build, started.buildPaths)
          val projectsToPull = if (projects.nonEmpty) projects.toSet else digests.keySet

          val pulled = AtomicInteger(0)
          val skipped = AtomicInteger(0)
          val notFound = AtomicInteger(0)

          val executor = Executors.newVirtualThreadPerTaskExecutor()
          val futures = new java.util.ArrayList[JFuture[?]]()

          projectsToPull.toList.sorted.foreach { crossName =>
            digests.get(crossName) match {
              case None =>
                started.logger.warn(s"Project ${crossName.value} not in build, skipping")
              case Some(digest) =>
                futures.add(executor.submit((() => {
                  val key = cacheKey(prefix, crossName, digest)
                  val projectPaths = started.buildPaths.project(crossName, started.build.explodedProjects(crossName))

                  if (Files.isDirectory(projectPaths.classes) && Files.list(projectPaths.classes).findAny().isPresent) {
                    skipped.incrementAndGet()
                    started.logger.debug(s"${crossName.value}: already compiled, skipping")
                  } else if (client.headObject(key)) {
                    val archive = client.getObject(key)
                    TarGz.unpack(archive, projectPaths.targetDir)
                    regenerateManifest(started, crossName, projectPaths)
                    pulled.incrementAndGet()
                    started.logger.info(s"${crossName.value}: pulled from cache (${archive.length / 1024}KB)")
                  } else {
                    notFound.incrementAndGet()
                    started.logger.debug(s"${crossName.value}: not in cache")
                  }
                }): Runnable))
            }
          }

          futures.forEach { f => f.get(); () }
          executor.shutdown()

          started.logger.info(
            s"Remote cache pull: ${pulled.get()} pulled, ${skipped.get()} already compiled, ${notFound.get()} not cached (${projectsToPull.size} total)"
          )
          Right(())
      }
    }
  }

  case class Push(projects: Array[model.CrossProjectName], force: Boolean) extends BleepBuildCommand {

    /** Read portability warnings written by the BSP server after compile. Returns list of absolute paths (empty = portable). */
    private def checkPortability(targetDir: Path): List[String] = {
      val warningsFile = targetDir.resolve(".zinc/portability-warnings")
      if (!Files.exists(warningsFile)) return Nil
      Files.readString(warningsFile).linesIterator.filter(_.nonEmpty).toList
    }

    override def run(started: Started): Either[BleepException, Unit] = {
      val cacheConfig = started.build.remoteCache
      cacheConfig match {
        case None =>
          Left(new BleepException.Text("No remote-cache configured in bleep.yaml"))
        case Some(config) =>
          val credentials = resolveCredentials(started)
          val client = S3Client.fromConfig(started.logger, config, credentials)
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
                  val key = cacheKey(prefix, crossName, digest)
                  val projectPaths = started.buildPaths.project(crossName, started.build.explodedProjects(crossName))

                  if (!Files.isDirectory(projectPaths.classes) || !Files.list(projectPaths.classes).findAny().isPresent) {
                    notCompiled.incrementAndGet()
                    started.logger.debug(s"${crossName.value}: not compiled, skipping")
                  } else if (!force && client.headObject(key)) {
                    skipped.incrementAndGet()
                    started.logger.debug(s"${crossName.value}: already in cache")
                  } else {
                    val absolutePaths = checkPortability(projectPaths.targetDir)
                    absolutePaths match {
                      case head :: _ =>
                        errors.add(
                          s"${crossName.value}: analysis contains ${absolutePaths.size} absolute path(s), e.g. '$head'. Kill BSP servers and recompile."
                        ): Unit
                      case Nil =>
                        val archive = TarGz.pack(projectPaths.targetDir, packFilter)
                        semaphore.acquire()
                        try {
                          client.putObject(key, archive)
                          pushed.incrementAndGet()
                          started.logger.info(s"${crossName.value}: pushed to cache (${archive.length / 1024}KB)")
                        } finally semaphore.release()
                    }
                  }
                }): Runnable))
            }
          }

          futures.forEach { f => f.get(); () }
          executor.shutdown()

          val errorList = scala.jdk.CollectionConverters.IterableHasAsScala(errors).asScala.toList
          if (errorList.nonEmpty) {
            started.logger.info(
              s"Remote cache push: ${pushed.get()} pushed, ${errorList.size} failed portability check, ${skipped.get()} already cached, ${notCompiled.get()} not compiled (${projectsToPush.size} total)"
            )
            Left(
              new BleepException.Text(
                s"${errorList.size} project(s) have non-portable analysis:\n${errorList.map(e => s"  - $e").mkString("\n")}"
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
  }

  private def cacheKey(prefix: String, crossName: model.CrossProjectName, digest: String): String = {
    val projectKey = crossName.value.replace('/', '-')
    if (prefix.isEmpty) s"$projectKey/$digest.tar.gz"
    else s"$prefix/$projectKey/$digest.tar.gz"
  }

  private def resolveCredentials(started: Started): model.RemoteCacheCredentials =
    started.config.remoteCacheCredentials
      .orElse(model.RemoteCacheCredentials.fromEnv())
      .getOrElse(
        throw new BleepException.Text(
          "No remote cache credentials found. Set remoteCacheCredentials in ~/.config/bleep/config.yaml or BLEEP_REMOTE_CACHE_S3_ACCESS_KEY_ID/BLEEP_REMOTE_CACHE_S3_SECRET_ACCESS_KEY env vars."
        )
      )

  /** After unpacking a project archive, write a fresh noop manifest stat'd against the local filesystem. The next compile then short-circuits via the pre-Zinc
    * fast-path instead of going through a Zinc no-op compile. No-op on Windows (ctime unavailable) and on non-Scala projects.
    */
  private def regenerateManifest(
      started: Started,
      crossName: model.CrossProjectName,
      projectPaths: ProjectPaths
  ): Unit = {
    if (!NoopManifestStore.ctimeAvailable) return

    val resolved = started.resolvedProject(crossName)
    val maybeLanguage = ProjectLanguage.fromResolvedScalaJava(resolved)
    if (maybeLanguage.isEmpty) return

    val analysisFile = projectPaths.targetDir.resolve(".zinc").resolve("analysis.zip")
    if (!Files.exists(analysisFile)) return

    val sourceDirs = projectPaths.sourcesDirs.all.toSet
    val sourceFiles = sourceDirs.toList.sorted.flatMap { dir =>
      if (Files.isDirectory(dir))
        scala.util
          .Using(Files.walk(dir)) { stream =>
            stream.toScala(List).filter(p => Files.isRegularFile(p) && (p.toString.endsWith(".scala") || p.toString.endsWith(".java")))
          }
          .getOrElse(Nil)
      else Nil
    }.toArray

    val classFiles =
      if (Files.isDirectory(projectPaths.classes))
        scala.util
          .Using(Files.walk(projectPaths.classes)) { stream =>
            stream.toScala(List).filter(p => Files.isRegularFile(p) && p.toString.endsWith(".class")).toSet
          }
          .getOrElse(Set.empty[Path])
      else Set.empty[Path]

    val deps = started.build.resolvedDependsOn.getOrElse(crossName, Set.empty)
    val dependencyAnalyses = deps.iterator.flatMap { dep =>
      val depPaths = started.projectPaths(dep)
      val depAnalysis = depPaths.targetDir.resolve(".zinc").resolve("analysis.zip")
      Some(depPaths.classes -> depAnalysis)
    }.toMap

    val result = ProjectCompileSuccess(projectPaths.classes, classFiles, Some(analysisFile))

    NoopManifestStore.regenerateFromLocal(
      analysisFile = analysisFile,
      sourceDirs = sourceDirs,
      sourceFiles = sourceFiles,
      dependencyAnalyses = dependencyAnalyses,
      language = maybeLanguage.get,
      ecjVersion = None,
      result = result
    ): Unit
    started.logger.debug(s"${crossName.value}: regenerated noop manifest")
  }
}
