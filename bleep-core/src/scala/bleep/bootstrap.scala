package bleep

import bleep.BuildPaths.Mode
import bleep.internal.Os

import java.time.Instant
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object bootstrap {
  def forScript(scriptName: String)(f: (Started, Commands) => Unit): Unit = {
    val logger = logging.stdout(LogPatterns.interface(Instant.now, Some(scriptName), noColor = false)).untyped
    val buildLoader = BuildLoader.find(Os.cwd)
    val buildPaths = BuildPaths(Os.cwd, buildLoader, Mode.Normal)
    val maybeStarted = for {
      existingBuild <- buildLoader.existing
      pre = Prebootstrapped(buildPaths, logger, existingBuild)
      started <- from(pre, GenBloopFiles.SyncToDisk, rewrites = Nil, BleepConfig.lazyForceLoad(pre.userPaths))
    } yield started

    maybeStarted match {
      case Left(buildException) =>
        BuildException.fatal("Couldn't initialize bleep", logger, buildException)
      case Right(started) =>
        Try(f(started, new Commands(started))) match {
          case Failure(th) => BuildException.fatal("failed :(", logger, th)
          case Success(_)  => ()
        }
    }
  }

  def from(pre: Prebootstrapped, genBloopFiles: GenBloopFiles, rewrites: List[Rewrite], lazyConfig: Lazy[BleepConfig]): Either[BuildException, Started] = {
    val t0 = System.currentTimeMillis()

    try
      pre.build.forceGet.map { build =>
        val lazyResolver = lazyConfig.map(bleepConfig =>
          CoursierResolver(
            build.resolvers.values,
            pre.logger,
            downloadSources = true,
            pre.userPaths.coursierCacheDir,
            bleepConfig.authentications,
            Some(build.$version)
          )
        )
        val explodedBuild = rewrites.foldLeft(ExplodedBuild.of(build)) { case (b, patch) => patch(b) }

        val activeProjects: List[model.CrossProjectName] =
          if (pre.buildPaths.cwd == pre.buildPaths.buildDir) explodedBuild.projects.keys.toList
          else
            explodedBuild.projects.flatMap { case (crossProjectName, p) =>
              val folder = pre.buildPaths.buildDir / p.folder.getOrElse(RelPath.force(crossProjectName.name.value))
              if (folder.startsWith(pre.buildPaths.cwd)) Some(crossProjectName)
              else None
            }.toList

        val ec = ExecutionContext.global
        val fetchNode = new FetchNode(pre.logger, ec)
        val bloopFiles: GenBloopFiles.Files =
          genBloopFiles(pre.logger, pre.buildPaths, lazyResolver, explodedBuild, fetchNode)

        val td = System.currentTimeMillis() - t0
        pre.logger.info(s"bootstrapped in $td ms")

        Started(
          prebootstrapped = pre,
          rewrites = rewrites,
          build = explodedBuild,
          bloopFiles = bloopFiles,
          activeProjectsFromPath = activeProjects,
          lazyConfig = lazyConfig,
          resolver = lazyResolver,
          executionContext = ec
        )
      }
    catch {
      case x: BuildException => Left(x)
      case th: Throwable     => Left(new BuildException.Cause(th, "couldn't initialize bleep"))
    }
  }
}
