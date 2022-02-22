package bleep

import bleep.BuildPaths.Mode
import bleep.internal.FileUtils.DeleteUnknowns
import bleep.internal.{generateBloopFiles, FileUtils, Lazy, Os}
import bleep.logging.Logger
import bloop.config.{Config, ConfigCodecs}
import com.github.plokhotnyuk.jsoniter_scala.core.{writeToString, WriterConfig}

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.util.{Failure, Success, Try}

object bootstrap {
  def forScript(scriptName: String)(f: Started => Unit): Unit = {
    val logger = logging.stdout(LogPatterns.interface(Instant.now, Some(scriptName))).untyped

    from(logger, Os.cwd, Mode.Normal, rewrites = Nil) match {
      case Left(buildException) =>
        logger.error("Couldn't initialize bleep", buildException)
        sys.exit(1)
      case Right(started) =>
        Try(f(started)) match {
          case Failure(exception) =>
            logger.error("failed :(", exception)
            sys.exit(1)
          case Success(_) =>
            ()
        }
    }
  }

  def from(logger: Logger, cwd: Path, mode: Mode, rewrites: List[Rewrite]): Either[BuildException, Started] =
    buildPaths(cwd, mode).flatMap(buildPaths => from(logger, buildPaths, rewrites))

  def from(logger: Logger, buildPaths: BuildPaths, rewrites: List[Rewrite]): Either[BuildException, Started] = {
    val t0 = System.currentTimeMillis()

    try
      model.parseBuild(Files.readString(buildPaths.bleepJsonFile)) match {
        case Left(th) => throw new BuildException.InvalidJson(buildPaths.bleepJsonFile, th)
        case Right(build) =>
          val userPaths = UserPaths.fromAppDirs
          val lazyResolver: Lazy[CoursierResolver] =
            Lazy(CoursierResolver(logger, downloadSources = true, userPaths))

          val explodedBuild = rewrites.foldLeft(ExplodedBuild.of(build)) { case (b, patch) => patch(b) }

          val activeProjects: List[model.CrossProjectName] =
            explodedBuild.projects.flatMap { case (crossProjectName, p) =>
              val folder = buildPaths.buildDir / p.folder.getOrElse(RelPath.force(crossProjectName.name.value))
              if (folder.startsWith(buildPaths.cwd)) Some(crossProjectName)
              else None
            }.toList

          val bloopFiles: Map[model.CrossProjectName, Lazy[Config.File]] =
            syncBloopFiles(logger, buildPaths, lazyResolver, explodedBuild)

          val td = System.currentTimeMillis() - t0
          logger.info(s"bootstrapped in $td ms")
          Right(Started(buildPaths, rewrites, build, explodedBuild, bloopFiles, activeProjects, lazyResolver, userPaths, logger))
      }
    catch {
      case x: BuildException => Left(x)
      case th: Throwable     => Left(new BuildException.Cause(th, "couldn't initialize bleep"))
    }
  }

  def buildPaths(cwd: Path, mode: Mode): Either[BuildException.BuildNotFound, BuildPaths] =
    findBleepJson(cwd) match {
      case Some(bleepJsonPath) =>
        Right(BuildPaths.fromBleepJson(cwd, bleepJsonPath, mode))

      case None => Left(new BuildException.BuildNotFound(cwd))
    }

  private def syncBloopFiles(
      logger: Logger,
      buildPaths: BuildPaths,
      lazyResolver: Lazy[CoursierResolver],
      explodedBuild: ExplodedBuild
  ): Map[model.CrossProjectName, Lazy[Config.File]] = {
    val currentHash = explodedBuild.projects.toVector.sortBy(_._1).hashCode().toString
    val oldHash = Try(Files.readString(buildPaths.digestFile, UTF_8)).toOption

    if (oldHash.contains(currentHash)) {
      logger.debug(s"${buildPaths.bleepBloopDir} up to date")

      explodedBuild.projects.map { case (crossProjectName, _) =>
        val load = Lazy(readAndParseBloopFile(buildPaths.bleepBloopDir.resolve(crossProjectName.value + ".json")))
        (crossProjectName, load)
      }
    } else {
      logger.warn(s"Refreshing ${buildPaths.bleepBloopDir}...")

      val lazyBloopFiles: SortedMap[model.CrossProjectName, Lazy[Config.File]] =
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
      logger.warn(s"Wrote ${lazyBloopFiles.size} files to ${buildPaths.bleepBloopDir}: $syncDetails")
      lazyBloopFiles
    }
  }

  // keep looking up until we find build file
  def findBleepJson(from: Path): Option[Path] = {
    def is(f: File): Option[File] =
      Option(f.list()) match {
        case Some(list) if list.contains(constants.BuildFileName) => Some(new File(f, constants.BuildFileName))
        case _                                                    => None
      }

    def search(f: File): Option[File] =
      is(f).orElse(Option(f.getParentFile).flatMap(search))

    search(from.toFile).map(_.toPath)
  }

  def bloopFileMap(lazyBloopFiles: Map[model.CrossProjectName, Lazy[Config.File]]): Map[RelPath, String] =
    lazyBloopFiles.map { case (projectName, bloopFile) =>
      val string = writeToString(bloopFile.forceGet, WriterConfig.withIndentionStep(2))(ConfigCodecs.codecFile)
      val file = RelPath(List(projectName.value + ".json"))
      (file, string)
    }
}
