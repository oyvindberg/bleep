package bleep

import bleep.internal.{bleepLoggers, fatal, FileUtils}
import bleep.rewrites.BuildRewrite

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object bootstrap {
  def forScript(scriptName: String, commonOpts: CommonOpts, rewrites: List[BuildRewrite] = Nil)(f: (Started, Commands) => Unit): Unit = {
    val userPaths = UserPaths.fromAppDirs
    val bleepConfig = BleepConfigOps.loadOrDefault(userPaths).orThrow
    val buildLoader = BuildLoader.find(FileUtils.cwd)
    val buildVariant = model.BuildVariant(rewrites.map(_.name))
    val buildPaths = BuildPaths(FileUtils.cwd, buildLoader, buildVariant)

    val exitCode: ExitCode =
      bleepLoggers.stdoutNoLogFile(bleepConfig, commonOpts).map(l => l.withPath(s"[script $scriptName]")).use { logger =>
        val ec = ExecutionContext.global
        val maybeStarted = for {
          existingBuild <- buildLoader.existing
          pre = Prebootstrapped(logger, userPaths, buildPaths, existingBuild, ec)
          genBloopFiles = GenBloopFiles.SyncToDisk
          started <- from(pre, genBloopFiles, rewrites, bleepConfig, CoursierResolver.Factory.default)
        } yield started

        maybeStarted match {
          case Left(buildException) =>
            fatal("Couldn't initialize bleep", logger, buildException)
          case Right(started) =>
            Try(f(started, new Commands(started))) match {
              case Failure(th) => fatal(s"Failed to run script: `$scriptName`.", logger, th)
              case Success(_)  => ExitCode.Success
            }
        }
      }

    exitCode match {
      case ExitCode.Success => ()
      case ExitCode.Failure => System.exit(exitCode.value)
    }
  }

  def from(
      pre: Prebootstrapped,
      genBloopFiles: GenBloopFiles,
      rewrites: List[BuildRewrite],
      config: model.BleepConfig,
      resolverFactory: CoursierResolver.Factory
  ): Either[BleepException, Started] = {
    def go(pre: Prebootstrapped, config: model.BleepConfig, rewrites: List[BuildRewrite]): Either[BleepException, Started] = {
      val t0 = System.currentTimeMillis()
      try
        pre.existingBuild.buildFile.forceGet.map { buildFile =>
          val resolver = resolverFactory(pre, config, buildFile)
          val build = rewrites.foldLeft[model.Build](model.Build.FileBacked(buildFile)) { case (b, rewrite) => rewrite(b, pre.buildPaths) }
          val bleepExecutable = Lazy(BleepExecutable.getCommand(resolver, pre, forceJvm = false))

          val activeProjects: Option[Array[model.CrossProjectName]] =
            if (pre.buildPaths.cwd == pre.buildPaths.buildDir) None
            else {
              val chosen =
                build.explodedProjects.flatMap { case (crossProjectName, p) =>
                  val projectPaths = pre.buildPaths.project(crossProjectName, p)
                  val underFolder = projectPaths.dir.startsWith(pre.buildPaths.cwd)
                  def underSources = projectPaths.sourcesDirs.all.exists(_.startsWith(pre.buildPaths.cwd))
                  def underResources = projectPaths.resourcesDirs.all.exists(_.startsWith(pre.buildPaths.cwd))
                  val underAny = underFolder || underSources || underResources
                  if (underAny) Some(crossProjectName)
                  else None
                }.toArray

              if (chosen.length > 0 && chosen.length != build.explodedProjects.size) {
                pre.logger.info(
                  s"${chosen.length} of ${build.explodedProjects.size} projects active from ${pre.buildPaths.cwd}. run `bleep projects` to see which"
                )
              }
              Some(chosen).filter(_.nonEmpty)
            }

          val bloopFiles: GenBloopFiles.Files =
            genBloopFiles(pre, resolver, build)

          val td = System.currentTimeMillis() - t0
          pre.logger.info(s"bootstrapped in $td ms")

          Started(pre, rewrites, build, bloopFiles, activeProjects, config, resolver, bleepExecutable)(reloadUsing = go)
        }
      catch {
        case x: BleepException => Left(x)
        case th: Throwable     => Left(new BleepException.Cause(th, "couldn't initialize bleep"))
      }
    }

    go(pre, config, rewrites)
  }
}
