package bleep

import bleep.internal.{Lazy, Os}
import bleep.logging.Logger
import bloop.config.{Config => b, ConfigCodecs}
import cats.data.NonEmptyList
import com.github.plokhotnyuk.jsoniter_scala
import com.github.plokhotnyuk.jsoniter_scala.core.writeToString

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.collection.immutable.SortedMap
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

/** @param build
  *   non-exploded variant, at least for now
  * @param bloopFiles
  *   will either all be resolved and written immediately if outdated, or read and parsed on demand
  */
case class Started(
    buildPaths: BuildPaths,
    build: model.Build,
    bloopFiles: Map[model.ProjectName, Lazy[b.File]],
    activeProjectFromPath: Option[model.ProjectName],
    lazyResolver: Lazy[CoursierResolver],
    directories: UserPaths,
    logger: Logger
) {
  def resolver = lazyResolver.forceGet

  lazy val projects: List[b.Project] =
    bloopFiles.map { case (_, lazyProject) => lazyProject.forceGet.project }.toList

  def chosenProjects(maybeFromCommandLine: Option[NonEmptyList[model.ProjectName]]): List[model.ProjectName] =
    maybeFromCommandLine match {
      case Some(fromCommandLine) => fromCommandLine.toList
      case None =>
        activeProjectFromPath match {
          case Some(value) => List(value)
          case None        => bloopFiles.keys.toList
        }
    }
}

object bootstrap {
  // suitable for scripts
  def simple(f: Started => Unit): Unit = {
    val logger = logging.stdout
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

  def from(logger: Logger, cwd: Path): Either[BuildException, Started] = {
    val t0 = System.currentTimeMillis()
    val directories = UserPaths.fromAppDirs

    val lazyResolver: Lazy[CoursierResolver] = Lazy {
      CoursierResolver(
        ExecutionContext.global,
        logger,
        downloadSources = true,
        Some(directories.cacheDir),
        CoursierResolver.Authentications.fromFile(directories.coursierRepositoriesJson, logger)
      )
    }

    findBleepJson(cwd) match {
      case Some(bleepJsonPath) =>
        model.parseBuild(Files.readString(bleepJsonPath)) match {
          case Left(th) => Left(new BuildException.InvalidJson(bleepJsonPath, th))
          case Right(build) =>
            val buildPaths = BuildPaths(bleepJsonPath)
            val currentHash = build.toString.hashCode().toString
            val oldHash = Try(Files.readString(buildPaths.digestFile, UTF_8)).toOption

            val activeProject: Option[model.ProjectName] = {
              val withRelativeLength =
                build.projects.flatMap { case (name, p) =>
                  val folder = buildPaths.buildDir / p.folder.getOrElse(RelPath.force(name.value))
                  val relative = cwd.relativize(folder)
                  if (relative.iterator().asScala.contains("..")) None
                  else Some((name, relative.getNameCount))
                }

              withRelativeLength.values.minOption.flatMap { min =>
                withRelativeLength.filter(_._2 == min).keys.toList match {
                  case one :: Nil => Some(one)
                  case _          => None
                }
              }
            }

            val bloopFiles = if (oldHash.contains(currentHash)) {
              logger.debug(s"${buildPaths.bleepBloopDir} up to date")

              build.projects.map { case (projectName, _) =>
                val load = Lazy(readBloopFile(buildPaths.bleepBloopDir, projectName))
                (projectName, load)
              }
            } else {
              val bloopFiles: SortedMap[model.ProjectName, Lazy[b.File]] =
                generateBloopFiles(build, buildPaths, lazyResolver.forceGet)

              Files.createDirectories(buildPaths.bleepBloopDir)
              bloopFiles.foreach { case (projectName, lazyP) =>
                val p = lazyP.forceGet(projectName.value)
                val json = writeToString(p, jsoniter_scala.core.WriterConfig.withIndentionStep(2))(ConfigCodecs.codecFile)
                val toPath = buildPaths.bleepBloopDir / (projectName.value + ".json")
                Files.writeString(toPath, json, UTF_8)
              }
              Files.writeString(buildPaths.digestFile, currentHash, UTF_8)
              logger.debug(s"Wrote ${bloopFiles.size} files to ${buildPaths.bleepBloopDir}")
              bloopFiles
            }
            val td = System.currentTimeMillis() - t0
            logger.info(s"bootstrapped in $td ms")
            Right(Started(buildPaths, build, bloopFiles, activeProject, lazyResolver, directories, logger))
        }
      case None => Left(new BuildException.BuildNotFound(cwd))
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
}
