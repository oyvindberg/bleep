package bleep
package commands

import bleep.analysis.{NoopManifestStore, OutputDeterminants, ProjectCompileSuccess, ProjectLanguage}

import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, Future as JFuture}
import scala.jdk.StreamConverters.*

/** Build cache: pull pre-compiled classes from a cache backend, push after building. Backends: S3-compatible services and local directories (see
  * [[CacheStore]]).
  *
  * Cache entries are per-project tar.gz archives keyed by content digest. The digest captures project config, source content, resource content, and transitive
  * dependency digests.
  *
  * Resources affect the digest but are NOT included in the archive (they're already on disk and don't need compilation).
  *
  * Zinc analysis IS included so incremental compilation works correctly after pulling. The noop manifest is per-machine state and is excluded from the archive
  * — Pull regenerates it locally so the next compile is a true noop hit.
  *
  * `Pull(..., unsafeSeedRef = Some(ref))` additionally tries, on an exact-digest miss, a digest computed as of `git merge-base ref HEAD` — see
  * [[PullOutcome.HitSeed]] and the `.zinc/unsafe-seed` marker it writes, which [[Push]] checks before publishing.
  */
object RemoteCache {

  /** Transfers at once, pull and push alike: a pool of this many platform threads (bleep targets JDK 17, which has no virtual threads). */
  private val Parallelism = 16

  /** What goes in an archive is decided by [[bleep.StateSharing]] — the same allow-list copy-state uses. Deny by default: this used to be a name-based
    * deny-list (only the noop manifest excluded) and shipped `.zinc/cache`, KSP caches and even lock files. Exposed so tests can verify the same behavior as
    * production.
    */
  private[bleep] def packFilter(variantBuildDir: Path)(p: Path): Boolean =
    bleep.StateSharing.isShareableIn(variantBuildDir)(p)

  /** What happened when [[Pull]] tried to populate one project. */
  sealed trait PullOutcome
  object PullOutcome {
    case object AlreadyCompiled extends PullOutcome
    final case class HitCurrent(bytes: Long) extends PullOutcome

    /** The current digest missed, but a digest computed as of `--unsafe-seed-ref`'s merge-base hit instead. The archive was unpacked, but — unlike
      * [[HitCurrent]] — the noop manifest was deliberately NOT regenerated: the following compile must actually run through Zinc's incremental invalidation
      * against the real current sources, not be treated as already up to date.
      */
    final case class HitSeed(bytes: Long, seedCommit: String, digest: String) extends PullOutcome
    case object MissNoSeedRequested extends PullOutcome // current digest missed; no --unsafe-seed-ref given
    case object MissSeedDigestUnavailable extends PullOutcome // --unsafe-seed-ref given, but this project's digest at that commit couldn't be determined
    case object MissSameDigestAsSeed extends PullOutcome // --unsafe-seed-ref given, but its digest equals the current one — already covered by the miss above
    case object MissSeedAlsoMissed extends PullOutcome // --unsafe-seed-ref given, digest differs from current and was computable, but the cache missed too
  }

  /** `.zinc/unsafe-seed`: written by [[Pull]] whenever a project is populated via [[PullOutcome.HitSeed]], read by [[Push]] to refuse publishing it under the
    * exact current digest unless `allowUnsafeSeeded` is set. The compiled state behind it was never verified against its own current inputs — only against
    * Zinc's incremental invalidation running on top of a real content gap — so letting it reach the cache under the current digest would let every future
    * puller trust it as a fully verified hit. Lives under `.zinc/`, which `bleep clean` removes wholesale along with the rest of the project's compiled state,
    * so the marker never outlives what it describes. Deliberately absent from [[bleep.StateSharing.variantDirEntries]], so neither `push` nor `copy-state` ever
    * propagates it.
    */
  private def unsafeSeedMarkerPath(projectPaths: ProjectPaths): Path =
    projectPaths.targetDir.resolve(".zinc").resolve("unsafe-seed")

  private def writeUnsafeSeedMarker(projectPaths: ProjectPaths, seedCommit: String, digest: String): Unit = {
    val marker = unsafeSeedMarkerPath(projectPaths)
    Files.createDirectories(marker.getParent)
    Files.writeString(marker, s"seedCommit: $seedCommit\nseedDigest: $digest\n"): Unit
  }

  private def readUnsafeSeedMarker(projectPaths: ProjectPaths): Option[String] = {
    val marker = unsafeSeedMarkerPath(projectPaths)
    if (Files.exists(marker)) Some(Files.readString(marker)) else None
  }

  case class Pull(projects: Array[model.CrossProjectName], unsafeSeedRef: Option[String]) extends BleepBuildCommand {
    override def run(started: Started): Either[BleepException, Unit] = {
      val cacheConfig = started.build.remoteCache
      cacheConfig match {
        case None =>
          Left(
            new BleepException.Text(
              "No remote-cache configured in bleep.yaml. Add:\n  remote-cache:\n    uri: s3://bucket/prefix\n    region: us-east-1\nor for a local directory (shared between checkouts/worktrees on this machine):\n  remote-cache:\n    uri: file:///path/to/cache"
            )
          )
        case Some(config) =>
          val (client, prefix) = storeFor(started, config)
          val buildDir = started.buildPaths.buildDir

          val currentDigests = ProjectDigest.computeAll(started.build, started.buildPaths)
          val projectsToPull = withoutUnsupported(started, if (projects.nonEmpty) projects.toSet else currentDigests.keySet)

          // Resolved once, up front, so every project's seed digest (if any) is known before the parallel pull loop starts — never computed lazily per miss.
          val seedCommit: Option[String] = unsafeSeedRef.map(ref => internal.gitOutput.mergeBase(buildDir, ref, "HEAD"))
          val seedDigests = seedCommit match {
            case Some(commit) => ProjectDigest.computeAllAt(started.build, started.buildPaths, commit)
            case None         => scala.collection.immutable.SortedMap.empty[model.CrossProjectName, String]
          }

          val outcomes = new java.util.concurrent.ConcurrentLinkedQueue[(model.CrossProjectName, PullOutcome)]()

          val executor = Executors.newFixedThreadPool(Parallelism)
          val futures = new java.util.ArrayList[JFuture[?]]()

          projectsToPull.toList.sorted.foreach { crossName =>
            currentDigests.get(crossName) match {
              case None =>
                started.logger.warn(s"Project ${crossName.value} not in build, skipping")
              case Some(currentDigest) =>
                futures.add(executor.submit((() => {
                  val projectPaths =
                    started.buildPaths.project(crossName, started.build.explodedProjects(crossName), started.build.crossPlatforms(crossName.name))

                  // `Files.list` holds an open directory stream until it is closed; one leaked descriptor per project adds up on a large build.
                  def alreadyCompiled: Boolean =
                    Files.isDirectory(projectPaths.classes) &&
                      scala.util.Using.resource(Files.list(projectPaths.classes))(_.findAny().isPresent)

                  def recordAndLog(outcome: PullOutcome, message: String, level: String): Unit = {
                    outcomes.add(crossName -> outcome): Unit
                    if (level == "info") started.logger.info(message) else started.logger.debug(message)
                  }

                  val currentKey = cacheKey(prefix, crossName, currentDigest)

                  if (alreadyCompiled) {
                    recordAndLog(PullOutcome.AlreadyCompiled, s"${crossName.value}: already compiled, skipping", "debug")
                  } else if (client.headObject(currentKey)) {
                    val archive = client.getObject(currentKey)
                    TarGz.unpack(archive, projectPaths.targetDir)
                    regenerateManifest(started, crossName, projectPaths)
                    recordAndLog(PullOutcome.HitCurrent(archive.length), s"${crossName.value}: pulled from cache (${archive.length / 1024}KB)", "info")
                  } else {
                    (seedCommit, seedDigests.get(crossName)) match {
                      case (None, _) =>
                        recordAndLog(PullOutcome.MissNoSeedRequested, s"${crossName.value}: not in cache", "debug")
                      case (Some(_), None) =>
                        recordAndLog(
                          PullOutcome.MissSeedDigestUnavailable,
                          s"${crossName.value}: not in cache (no comparable digest at ${unsafeSeedRef.getOrElse("")})",
                          "debug"
                        )
                      case (Some(_), Some(seedDigest)) if seedDigest == currentDigest =>
                        recordAndLog(PullOutcome.MissSameDigestAsSeed, s"${crossName.value}: not in cache", "debug")
                      case (Some(commit), Some(seedDigest)) =>
                        val seedKey = cacheKey(prefix, crossName, seedDigest)
                        if (client.headObject(seedKey)) {
                          val archive = client.getObject(seedKey)
                          TarGz.unpack(archive, projectPaths.targetDir)
                          writeUnsafeSeedMarker(projectPaths, commit, seedDigest)
                          recordAndLog(
                            PullOutcome.HitSeed(archive.length, commit, seedDigest),
                            s"${crossName.value}: missed current digest, pulled unsafe seed from $commit (${archive.length / 1024}KB) — incremental compile will run",
                            "info"
                          )
                        } else {
                          recordAndLog(PullOutcome.MissSeedAlsoMissed, s"${crossName.value}: not in cache (also missed seed from $commit)", "debug")
                        }
                    }
                  }
                }): Runnable))
            }
          }

          futures.forEach { f => f.get(); () }
          executor.shutdown()

          val results = scala.jdk.CollectionConverters.IterableHasAsScala(outcomes).asScala.toList.map(_._2)
          val hitCurrent = results.count(_.isInstanceOf[PullOutcome.HitCurrent])
          val hitSeed = results.count(_.isInstanceOf[PullOutcome.HitSeed])
          val alreadyCompiled = results.count(_ == PullOutcome.AlreadyCompiled)
          val missed = results.size - hitCurrent - hitSeed - alreadyCompiled

          val summary = seedCommit match {
            case Some(commit) =>
              s"Remote cache pull: $hitCurrent pulled, $hitSeed seeded from $commit, $alreadyCompiled already compiled, $missed not cached (${projectsToPull.size} total)"
            case None =>
              s"Remote cache pull: $hitCurrent pulled, $alreadyCompiled already compiled, $missed not cached (${projectsToPull.size} total)"
          }
          started.logger.info(summary)
          Right(())
      }
    }
  }

  case class Push(projects: Array[model.CrossProjectName], force: Boolean, allowUnsafeSeeded: Boolean) extends BleepBuildCommand {

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
          val (client, prefix) = storeFor(started, config)

          val digests = ProjectDigest.computeAll(started.build, started.buildPaths)
          val projectsToPush = withoutUnsupported(started, if (projects.nonEmpty) projects.toSet else digests.keySet)

          val pushed = AtomicInteger(0)
          val skipped = AtomicInteger(0)
          val notCompiled = AtomicInteger(0)
          val errors = new java.util.concurrent.ConcurrentLinkedQueue[String]()

          val executor = Executors.newFixedThreadPool(Parallelism)
          val futures = new java.util.ArrayList[JFuture[?]]()

          projectsToPush.toList.sorted.foreach { crossName =>
            digests.get(crossName) match {
              case None =>
                started.logger.warn(s"Project ${crossName.value} not in build, skipping")
              case Some(digest) =>
                futures.add(executor.submit((() => {
                  val key = cacheKey(prefix, crossName, digest)
                  val projectPaths =
                    started.buildPaths.project(crossName, started.build.explodedProjects(crossName), started.build.crossPlatforms(crossName.name))

                  // Closed, for the same reason as in [[Pull]].
                  def notCompiledYet: Boolean =
                    !Files.isDirectory(projectPaths.classes) ||
                      !scala.util.Using.resource(Files.list(projectPaths.classes))(_.findAny().isPresent)

                  if (notCompiledYet) {
                    notCompiled.incrementAndGet()
                    started.logger.debug(s"${crossName.value}: not compiled, skipping")
                  } else if (!force && client.headObject(key)) {
                    skipped.incrementAndGet()
                    started.logger.debug(s"${crossName.value}: already in cache")
                  } else {
                    val absolutePaths = checkPortability(projectPaths.targetDir)
                    val unsafeSeed = if (allowUnsafeSeeded) None else readUnsafeSeedMarker(projectPaths)
                    (absolutePaths, unsafeSeed) match {
                      case (head :: _, _) =>
                        errors.add(
                          s"${crossName.value}: analysis contains ${absolutePaths.size} absolute path(s), e.g. '$head'. Kill BSP servers and recompile."
                        ): Unit
                      case (Nil, Some(marker)) =>
                        errors.add(
                          s"${crossName.value}: compiled from an unsafe seed, never verified against its own current inputs (${marker.linesIterator.mkString(", ")}). Pass --allow-unsafe-seeded to push anyway."
                        ): Unit
                      case (Nil, None) =>
                        val archive = TarGz.pack(projectPaths.targetDir, packFilter(projectPaths.targetDir))
                        client.putObject(key, archive)
                        pushed.incrementAndGet()
                        started.logger.info(s"${crossName.value}: pushed to cache (${archive.length / 1024}KB)")
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
              s"Remote cache push: ${pushed.get()} pushed, ${errorList.size} refused, ${skipped.get()} already cached, ${notCompiled.get()} not compiled (${projectsToPush.size} total)"
            )
            Left(
              new BleepException.Text(
                s"${errorList.size} project(s) refused (non-portable analysis, or compiled from an unsafe seed):\n${errorList.map(e => s"  - $e").mkString("\n")}"
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

  /** Select the cache backend from the configured uri scheme.
    *
    * `file://` is a directory on the local filesystem — no credentials, the uri path is the cache root and the key prefix is empty. Anything else goes through
    * [[S3Client]] (s3:// or an S3-compatible HTTP endpoint) and requires credentials.
    */
  /** Why the remote cache does not handle `crossName` yet, if it does not.
    *
    * Projects compiled by a compiler built in the build, projects with a post-compile step, and projects compiled against one, all carry state the cache format
    * has not been designed for: a compiler snapshot, a second classes directory and the transform's record, per-class views of transformed classes. Until it is
    * decided how they should be cached, they are left out — named, never silently.
    */
  private[bleep] def unsupportedReason(build: model.Build, crossName: model.CrossProjectName): Option[String] = {
    val project = build.explodedProjects(crossName)
    if (project.scala.flatMap(_.compilerProject).isDefined) Some("it is compiled by a compiler built in this build (`scala.compilerProject`)")
    else if (project.postCompile.isDefined) Some("it has a `postCompile` step")
    else
      build.transitiveDependenciesFor(crossName).collectFirst {
        case (dep, depProject) if depProject.postCompile.isDefined => s"it compiles against ${dep.value}, which has a `postCompile` step"
      }
  }

  private def withoutUnsupported(started: Started, requested: Set[model.CrossProjectName]): Set[model.CrossProjectName] =
    requested.filter { crossName =>
      started.build.explodedProjects.get(crossName).flatMap(_ => unsupportedReason(started.build, crossName)) match {
        case Some(reason) =>
          started.logger.warn(s"${crossName.value}: not handled by the remote cache yet, because $reason")
          false
        case None => true
      }
    }

  private def storeFor(started: Started, config: model.RemoteCacheConfig): (CacheStore, String) =
    config.uri.getScheme match {
      case "file" => (LocalDirStore.fromUri(started.logger, config.uri), "")
      case _      => (S3Client.fromConfig(started.logger, config, resolveCredentials(started)), S3Client.keyPrefix(config))
    }

  private def cacheKey(prefix: String, crossName: model.CrossProjectName, digest: String): String = {
    val projectKey = crossName.fileSafeValue
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
    val ecjVersion = started.build.explodedProjects(crossName).java.flatMap(_.ecjVersion).map(_.version)
    val maybeLanguage = ProjectLanguage.fromResolvedScalaJava(resolved, ecjVersion)
    if (maybeLanguage.isEmpty) return

    val analysisFile = projectPaths.targetDir.resolve(".zinc").resolve("analysis.zip")
    if (!Files.exists(analysisFile)) return

    val sourceDirs = projectPaths.sourcesDirs.all(Usage.Compile).toSet
    val sourceFiles = sourceDirs.toList.sorted.flatMap { dir =>
      if (Files.isDirectory(dir))
        scala.util
          .Using(Files.walk(dir)) { stream =>
            stream.toScala(List).filter(p => Files.isRegularFile(p) && (p.toString.endsWith(".scala") || p.toString.endsWith(".java")))
          }
          .getOrElse(Nil)
      else Nil
    }.toArray

    // The compiler's directory: what zinc and the noop manifest know about. `classes` differs only for a post-compile project, whose script output is not
    // the compiler's.
    val classFiles =
      if (Files.isDirectory(projectPaths.compilerOutput))
        scala.util
          .Using(Files.walk(projectPaths.compilerOutput)) { stream =>
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

    val result = ProjectCompileSuccess(projectPaths.compilerOutput, classFiles, Some(analysisFile))

    NoopManifestStore.regenerateFromLocal(
      analysisFile = analysisFile,
      sourceDirs = sourceDirs,
      sourceFiles = sourceFiles,
      dependencyAnalyses = dependencyAnalyses,
      language = maybeLanguage.get,
      ecjVersion = ecjVersion,
      determinants = OutputDeterminants.none,
      result = result
    ): Unit
    started.logger.debug(s"${crossName.value}: regenerated noop manifest")
  }
}
