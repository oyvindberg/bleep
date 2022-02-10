package bleep

import bleep.internal.FileUtils.DeleteUnknowns
import bleep.internal.{generateBloopFiles, FileUtils, Lazy, Os}
import bleep.logging.Logger
import bloop.config.{Config => b, ConfigCodecs}
import com.github.plokhotnyuk.jsoniter_scala.core.{writeToString, WriterConfig}

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.util.{Failure, Success, Try}

/** @param build
  *   non-exploded variant, at least for now
  * @param bloopFiles
  *   will either all be resolved and written immediately if outdated, or read and parsed on demand
  */
case class Started(
    buildPaths: BuildPaths,
    rawBuild: model.Build,
    build: ExplodedBuild,
    bloopFiles: Map[model.CrossProjectName, Lazy[b.File]],
    activeProjectsFromPath: List[model.CrossProjectName],
    lazyResolver: Lazy[CoursierResolver],
    directories: UserPaths,
    logger: Logger
) {
  def resolver = lazyResolver.forceGet

  lazy val bloopProjects: List[b.Project] =
    bloopFiles.map { case (_, lazyProject) => lazyProject.forceGet.project }.toList

  def chosenProjects(maybeFromCommandLine: Option[List[model.CrossProjectName]]): List[model.CrossProjectName] =
    maybeFromCommandLine match {
      case Some(fromCommandLine) => fromCommandLine.sorted
      case None =>
        activeProjectsFromPath match {
          case Nil      => bloopFiles.keys.toList.sorted
          case nonEmpty => nonEmpty
        }
    }

  def chosenTestProjects(maybeFromCommandLine: Option[List[model.CrossProjectName]]): List[model.CrossProjectName] =
    chosenProjects(maybeFromCommandLine).filterNot(projectName => build.projects(projectName).testFrameworks.isEmpty)
}

object bootstrap {
  def forScript(scriptName: String)(f: Started => Unit): Unit = {
    val logger = logging.stdout(LogPatterns.interface(Instant.now, Some(scriptName)))

    from(logger, Os.cwd) match {
      case Left(buildException) => logger.error("Couldn't initialize bleep", buildException)
      case Right(started) =>
        Try(f(started)) match {
          case Failure(exception) => logger.error("failed :(", exception)
          case Success(_)         => ()
        }
    }
  }

  def fromCwd(logger: Logger): Either[BuildException, Started] =
    from(logger, Os.cwd)

  def from(logger: Logger, cwd: Path): Either[BuildException, Started] =
    try Right(unsafeFrom(logger, cwd))
    catch { case x: BuildException => Left(x) }

  @throws[BuildException]
  def unsafeFrom(logger: Logger, cwd: Path): Started =
    findBleepJson(cwd) match {
      case Some(bleepJsonPath) =>
        val buildPaths = BuildPaths.fromBleepJson(bleepJsonPath)
        unsafeFrom(logger, cwd, buildPaths)
      case None => throw new BuildException.BuildNotFound(cwd)
    }

  def unsafeFrom(logger: Logger, cwd: Path, buildPaths: BuildPaths): Started = {
    val t0 = System.currentTimeMillis()

    model.parseBuild(Files.readString(buildPaths.bleepJsonFile)) match {
      case Left(th) => throw new BuildException.InvalidJson(buildPaths.bleepJsonFile, th)
      case Right(build) =>
        val directories = UserPaths.fromAppDirs
        val lazyResolver: Lazy[CoursierResolver] =
          Lazy(CoursierResolver(logger, downloadSources = true, directories))

        val currentHash = build.toString.hashCode().toString
        val oldHash = Try(Files.readString(buildPaths.digestFile, UTF_8)).toOption
        val explodedBuild = ExplodedBuild.of(build)
        val activeProjects: List[model.CrossProjectName] =
          explodedBuild.projects.flatMap { case (crossProjectName, p) =>
            val folder = buildPaths.buildDir / p.folder.getOrElse(RelPath.force(crossProjectName.name.value))
            if (folder.startsWith(cwd)) Some(crossProjectName)
            else None
          }.toList

        val bloopFiles = if (oldHash.contains(currentHash)) {
          logger.debug(s"${buildPaths.bleepBloopDir} up to date")

          explodedBuild.projects.map { case (crossProjectName, _) =>
            val load = Lazy(readAndParseBloopFile(buildPaths.bleepBloopDir.resolve(crossProjectName.value + ".json")))
            (crossProjectName, load)
          }
        } else {
          val lazyBloopFiles: SortedMap[model.CrossProjectName, Lazy[b.File]] =
            generateBloopFiles(explodedBuild, buildPaths, lazyResolver.forceGet)

          val fileMap = bloopFileMap(lazyBloopFiles)
            .updated(RelPath.relativeTo(buildPaths.bleepBloopDir, buildPaths.digestFile), currentHash)

          val synced = FileUtils.sync(
            folder = buildPaths.bleepBloopDir,
            fileRelMap = fileMap,
            deleteUnknowns = DeleteUnknowns.Yes(maxDepth = Some(1)),
            soft = true
          )

          val syncDetails = synced.groupBy(_._2).collect { case (synced, files) if files.nonEmpty => s"$synced: (${files.size})" }.mkString(", ")
          logger.info(s"Wrote ${lazyBloopFiles.size} files to ${buildPaths.bleepBloopDir}: $syncDetails")
          lazyBloopFiles
        }
        val td = System.currentTimeMillis() - t0
        logger.info(s"bootstrapped in $td ms")
        Started(buildPaths, build, explodedBuild, bloopFiles, activeProjects, lazyResolver, directories, logger)
    }
  }

  // keep looking up until we find build file
  def findBleepJson(from: Path): Option[Path] = {
    def is(f: File): Option[File] =
      Option(f.list()) match {
        case Some(list) if list.contains(Defaults.BuildFileName) => Some(new File(f, Defaults.BuildFileName))
        case _                                                   => None
      }

    def search(f: File): Option[File] =
      is(f).orElse(Option(f.getParentFile).flatMap(search))

    search(from.toFile).map(_.toPath)
  }

  def bloopFileMap(lazyBloopFiles: Map[model.CrossProjectName, Lazy[b.File]]): Map[RelPath, String] =
    lazyBloopFiles.map { case (projectName, bloopFile) =>
      val string = writeToString(bloopFile.forceGet, WriterConfig.withIndentionStep(2))(ConfigCodecs.codecFile)
      val file = RelPath(List(projectName.value + ".json"))
      (file, string)
    }
}
