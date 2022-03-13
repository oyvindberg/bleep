package bleep

import bleep.BuildPaths.Mode
import bleep.internal.{generateBloopFiles, FileUtils, Lazy, Os}
import bleep.logging.Logger
import bloop.config.{Config, ConfigCodecs}
import com.github.plokhotnyuk.jsoniter_scala.core.{writeToString, WriterConfig}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.util.{Failure, Success, Try}

object bootstrap {
  def forScript(scriptName: String)(f: Started => Unit): Unit = {
    val logger = logging.stdout(LogPatterns.interface(Instant.now, Some(scriptName), noColor = false)).untyped

    Prebootstrapped.find(Os.cwd, Mode.Normal, logger).flatMap(pre => from(pre, rewrites = Nil)) match {
      case Left(buildException) =>
        BuildException.fatal("Couldn't initialize bleep", logger, buildException)
      case Right(started) =>
        Try(f(started)) match {
          case Failure(th) => BuildException.fatal("failed :(", logger, th)
          case Success(_)  => ()
        }
    }
  }

  def from(pre: Prebootstrapped, rewrites: List[Rewrite]): Either[BuildException, Started] = {
    val t0 = System.currentTimeMillis()

    try
      model.parseBuild(Files.readString(pre.buildPaths.bleepJsonFile)) match {
        case Left(th) => Left(new BuildException.InvalidJson(pre.buildPaths.bleepJsonFile, th))
        case Right(build) =>
          val explodedBuild = rewrites.foldLeft(ExplodedBuild.of(build)) { case (b, patch) => patch(b) }

          val activeProjects: List[model.CrossProjectName] =
            explodedBuild.projects.flatMap { case (crossProjectName, p) =>
              val folder = pre.buildPaths.buildDir / p.folder.getOrElse(RelPath.force(crossProjectName.name.value))
              if (folder.startsWith(pre.buildPaths.cwd)) Some(crossProjectName)
              else None
            }.toList

          val bloopFiles: Map[model.CrossProjectName, Lazy[Config.File]] =
            syncBloopFiles(pre.logger, pre.buildPaths, pre.lazyResolver, explodedBuild)

          val td = System.currentTimeMillis() - t0
          pre.logger.info(s"bootstrapped in $td ms")
          Right(Started(pre.buildPaths, rewrites, build, explodedBuild, bloopFiles, activeProjects, pre.lazyResolver, pre.userPaths, pre.logger))
      }
    catch {
      case x: BuildException => Left(x)
      case th: Throwable     => Left(new BuildException.Cause(th, "couldn't initialize bleep"))
    }
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

      val fileMap = bloopFileMap(lazyBloopFiles).updated(RelPath.relativeTo(buildPaths.bleepBloopDir, buildPaths.digestFile), currentHash)

      val synced = FileUtils.sync(
        folder = buildPaths.bleepBloopDir,
        fileRelMap = fileMap,
        deleteUnknowns = FileUtils.DeleteUnknowns.Yes(maxDepth = Some(1)),
        soft = true
      )

      val syncDetails = synced.groupBy(_._2).collect { case (synced, files) if files.nonEmpty => s"$synced: (${files.size})" }.mkString(", ")
      logger.warn(s"Wrote ${lazyBloopFiles.size} files to ${buildPaths.bleepBloopDir}: $syncDetails")
      lazyBloopFiles
    }
  }

  def bloopFileMap(lazyBloopFiles: Map[model.CrossProjectName, Lazy[Config.File]]): Map[RelPath, String] =
    lazyBloopFiles.map { case (projectName, bloopFile) =>
      val string = writeToString(bloopFile.forceGet, WriterConfig.withIndentionStep(2))(ConfigCodecs.codecFile)
      val file = RelPath(List(projectName.value + ".json"))
      (file, string)
    }
}
