package bleep

import bleep.internal.{fatal, Os}
import bleep.logging.{jsonEvents, LogLevel}
import bleep.rewrites.BuildRewrite

import java.time.Instant
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object bootstrap {
  def forScript(scriptName: String, commonOpts: CommonOpts, rewrites: List[BuildRewrite] = Nil)(f: (Started, Commands) => Unit): Unit = {
    val logAsJson = sys.env.contains(jsonEvents.CallerProcessAcceptsJsonEvents)

    val logger = {
      val logger0 =
        if (logAsJson) logging.stdoutJson()
        else logging.stdout(LogPatterns.interface(Instant.now, noColor = commonOpts.noColor), disableProgress = commonOpts.noBspProgress)

      val logger1 = logger0.withPath(s"[script $scriptName]")
      val logger2 = if (commonOpts.debug || logAsJson) logger1 else logger1.minLogLevel(LogLevel.info)
      logger2.untyped
    }

    val buildLoader = BuildLoader.find(Os.cwd)
    val buildVariant = model.BuildVariant(rewrites.map(_.name))
    val buildPaths = BuildPaths(Os.cwd, buildLoader, buildVariant)
    val maybeStarted = for {
      existingBuild <- buildLoader.existing
      pre = Prebootstrapped(buildPaths, logger, existingBuild)
      started <- from(pre, GenBloopFiles.SyncToDisk, rewrites, BleepConfigOps.lazyForceLoad(pre.userPaths), CoursierResolver.Factory.default)
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
      resolver: CoursierResolver.Factory
  ): Either[BleepException, Started] = {
    val t0 = System.currentTimeMillis()

    try
      pre.existingBuild.buildFile.forceGet.map { buildFile =>
        val lazyResolver = lazyConfig.map(bleepConfig => resolver(pre, bleepConfig, buildFile))
        val build = rewrites.foldLeft[model.Build](model.Build.FileBacked(buildFile)) { case (b, rewrite) => rewrite(b) }

        val activeProjects: Option[Array[model.CrossProjectName]] =
          if (pre.buildPaths.cwd == pre.buildPaths.buildDir) None
          else {
            val chosen =
              build.explodedProjects.flatMap { case (crossProjectName, p) =>
                val folder = pre.buildPaths.project(crossProjectName, p).dir
                if (folder.startsWith(pre.buildPaths.cwd)) Some(crossProjectName)
                else None
              }.toArray

            if (chosen.length > 0 && chosen.length != build.explodedProjects.size) {
              pre.logger.info(
                s"${chosen.length} of ${build.explodedProjects.size} projects active from ${pre.buildPaths.cwd}. run `bleep projects` to see which"
              )
            }
            Some(chosen).filter(_.nonEmpty)
          }

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
