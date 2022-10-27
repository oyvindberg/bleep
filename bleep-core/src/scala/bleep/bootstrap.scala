package bleep

import bleep.BuildPaths.Mode
import bleep.internal.{fatal, Os}
import bleep.logging.jsonEvents
import bleep.rewrites.BuildRewrite

import java.time.Instant
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object bootstrap {
  def forScript(scriptName: String)(f: (Started, Commands) => Unit): Unit = {
    val logAsJson = sys.env.contains(jsonEvents.CallerProcessAcceptsJsonEvents)

    val logger = {
      val base = if (logAsJson) logging.stdoutJson() else logging.stdout(LogPatterns.interface(Instant.now, noColor = false))
      base.untyped.withPath(scriptName)
    }

    val buildLoader = BuildLoader.find(Os.cwd)
    val buildPaths = BuildPaths(Os.cwd, buildLoader, Mode.Normal)
    val maybeStarted = for {
      existingBuild <- buildLoader.existing
      pre = Prebootstrapped(buildPaths, logger, existingBuild)
      started <- from(pre, GenBloopFiles.SyncToDisk, rewrites = Nil, BleepConfigOps.lazyForceLoad(pre.userPaths))
    } yield started

    maybeStarted match {
      case Left(buildException) =>
        fatal("Couldn't initialize bleep", logger, buildException)
      case Right(started) =>
        Try(f(started, new Commands(started))) match {
          case Failure(th) => fatal("failed :(", logger, th)
          case Success(_)  => ()
        }
    }
  }

  def from(
      pre: Prebootstrapped,
      genBloopFiles: GenBloopFiles,
      rewrites: List[BuildRewrite],
      lazyConfig: Lazy[model.BleepConfig],
      resolver: CoursierResolver.Factory = CoursierResolver.Factory.default
  ): Either[BleepException, Started] = {
    val t0 = System.currentTimeMillis()

    try
      pre.existingBuild.buildFile.forceGet.map { buildFile =>
        val lazyResolver = lazyConfig.map(bleepConfig => resolver(pre, bleepConfig, buildFile))
        val build = rewrites.foldLeft[model.Build](model.Build.FileBacked(buildFile)) { case (b, rewrite) => rewrite(b) }

        val activeProjects: List[model.CrossProjectName] =
          if (pre.buildPaths.cwd == pre.buildPaths.buildDir) build.explodedProjects.keys.toList
          else
            build.explodedProjects.flatMap { case (crossProjectName, p) =>
              val folder = pre.buildPaths.buildDir / p.folder.getOrElse(RelPath.force(crossProjectName.name.value))
              if (folder.startsWith(pre.buildPaths.cwd)) Some(crossProjectName)
              else None
            }.toList

        val ec = ExecutionContext.global
        val fetchNode = new FetchNode(new BleepCacheLogger(pre.logger), ec)
        val bloopFiles: GenBloopFiles.Files =
          genBloopFiles(pre.logger, pre.buildPaths, lazyResolver, build, fetchNode)

        val td = System.currentTimeMillis() - t0
        pre.logger.info(s"bootstrapped in $td ms")

        Started(
          prebootstrapped = pre,
          rewrites = rewrites,
          build = build,
          bloopFiles = bloopFiles,
          activeProjectsFromPath = activeProjects,
          lazyConfig = lazyConfig,
          resolver = lazyResolver,
          executionContext = ec
        )
      }
    catch {
      case x: BleepException => Left(x)
      case th: Throwable     => Left(new BleepException.Cause(th, "couldn't initialize bleep"))
    }
  }
}
