package bleep

import bleep.BuildPaths.Mode
import bleep.internal.{Lazy, Os}

import java.nio.file.Files
import java.time.Instant
import scala.util.{Failure, Success, Try}

object bootstrap {
  def forScript(scriptName: String)(f: Started => Unit): Unit = {
    val logger = logging.stdout(LogPatterns.interface(Instant.now, Some(scriptName), noColor = false)).untyped

    Prebootstrapped.find(Os.cwd, Mode.Normal, logger).flatMap(pre => from(pre, GenBloopFiles.SyncToDisk, rewrites = Nil)) match {
      case Left(buildException) =>
        BuildException.fatal("Couldn't initialize bleep", logger, buildException)
      case Right(started) =>
        Try(f(started)) match {
          case Failure(th) => BuildException.fatal("failed :(", logger, th)
          case Success(_)  => ()
        }
    }
  }

  def from(pre: Prebootstrapped, genBloopFiles: GenBloopFiles, rewrites: List[Rewrite]): Either[BuildException, Started] = {
    val t0 = System.currentTimeMillis()

    try
      model.parseBuild(Files.readString(pre.buildPaths.bleepJsonFile)) match {
        case Left(th) => Left(new BuildException.InvalidJson(pre.buildPaths.bleepJsonFile, th))
        case Right(build) =>
          val lazyResolver = Lazy(CoursierResolver(build.resolvers.values, pre.logger, downloadSources = true, pre.userPaths))
          val explodedBuild = rewrites.foldLeft(ExplodedBuild.of(build)) { case (b, patch) => patch(b) }

          val activeProjects: List[model.CrossProjectName] =
            if (pre.buildPaths.cwd == pre.buildPaths.buildDir) explodedBuild.projects.keys.toList
            else
              explodedBuild.projects.flatMap { case (crossProjectName, p) =>
                val folder = pre.buildPaths.buildDir / p.folder.getOrElse(RelPath.force(crossProjectName.name.value))
                if (folder.startsWith(pre.buildPaths.cwd)) Some(crossProjectName)
                else None
              }.toList

          val bloopFiles: GenBloopFiles.Files =
            genBloopFiles(pre.logger, pre.buildPaths, lazyResolver, explodedBuild)

          val td = System.currentTimeMillis() - t0
          pre.logger.info(s"bootstrapped in $td ms")
          Right(Started(pre.buildPaths, rewrites, build, explodedBuild, bloopFiles, activeProjects, lazyResolver, pre.userPaths, pre.logger))
      }
    catch {
      case x: BuildException => Left(x)
      case th: Throwable     => Left(new BuildException.Cause(th, "couldn't initialize bleep"))
    }
  }
}
