package bleep

import bleep.BuildPaths.Mode
import bleep.bsp.BspImpl
import bleep.internal.{Os, ProjectGlobs}
import bleep.logging._
import cats.data.NonEmptyList
import cats.syntax.apply._
import cats.syntax.foldable._
import com.monovore.decline._

import java.io.{BufferedWriter, PrintStream}
import java.nio.file.Path
import java.time.Instant
import scala.util.{Failure, Success, Try}

object Main {
  val cwd: Path =
    Os.cwd

  val stringArgs: Opts[List[String]] =
    Opts.arguments[String]().orNone.map(args => args.fold(List.empty[String])(_.toList))

  object metavars {
    val projectName = "project name"
    val testProjectName = "test project name"
    val platformName = "platform name"
    val scalaVersion = "scala version"
  }
  def mainOpts(logger: Logger, buildPaths: BuildPaths): Opts[BleepCommand] = {
    val maybeStarted = bootstrap.from(logger, buildPaths, rewrites = Nil)
    def forceStarted() = maybeStarted match {
      case Left(buildException) =>
        logger.error("couldn't initialize build", buildException)
        sys.exit(1)
      case Right(started) => started
    }

    val maybeGlobs = maybeStarted.map(ProjectGlobs.apply)
    val nothing = Map.empty[String, Iterable[model.CrossProjectName]]

    val nothing = Map.empty[String, Iterable[model.CrossProjectName]]
    val projectNames: Opts[Option[List[model.CrossProjectName]]] =
      Opts.arguments(metavars.projectName)(Argument.fromMap(metavars.projectName, maybeGlobs.fold(_ => nothing, _.projectNameMap))).map(_.toList.flatten).orNone

    val projectNamesNoExpand: Opts[Option[List[String]]] =
      Opts
        .arguments(metavars.projectName)(Argument.fromMap(metavars.projectName, maybeGlobs.fold(_ => nothing, _.projectNameMap).map { case (s, _) => (s, s) }))
        .map(_.toList)
        .orNone

    val testProjectNames: Opts[Option[List[model.CrossProjectName]]] =
      Opts
        .arguments(metavars.testProjectName)(Argument.fromMap(metavars.testProjectName, maybeGlobs.fold(_ => nothing, _.testProjectNameMap)))
        .map(_.toList.flatten)
        .orNone

    val possibleScalaVersions: Map[String, Versions.Scala] =
      List(Versions.Scala3, Versions.Scala213, Versions.Scala212).map(v => (v.binVersion.replace("\\.", ""), v)).toMap

    lazy val ret: Opts[BleepCommand] = List(
      List(
        Opts.subcommand("build", "rewrite build")(
          List(
            Opts.subcommand("new", "create new build in current directory")(
              (
                CommonOpts.opts,
                Opts
                  .options("platform", "specify wanted platform(s)", metavar = metavars.platformName, short = "p")(
                    Argument.fromMap(metavars.platformName, model.PlatformId.All.map(p => (p.value, p)).toMap)
                  )
                  .withDefault(NonEmptyList.of(model.PlatformId.Jvm)),
                Opts
                  .options("scala", "specify scala version(s)", "s", metavars.scalaVersion)(Argument.fromMap(metavars.scalaVersion, possibleScalaVersions))
                  .withDefault(NonEmptyList.of(Versions.Scala3)),
                Opts.argument[String](metavars.projectName)
              ).mapN { case (_, platforms, scalas, name) => commands.BuildCreateNew(logger, cwd, platforms, scalas, name) }
            ),
            Opts.subcommand("templates-reapply", "reapply templates.")(
              CommonOpts.opts.map(_ => commands.BuildReapplyTemplates(forceStarted()))
            ),
            Opts.subcommand("templates-generate-new", "throw away existing templates and infer new")(
              CommonOpts.opts.map(_ => commands.BuildReinferTemplates(forceStarted(), Set.empty))
            )
          ).foldK
        ),
        Opts.subcommand("compile", "compile projects")(
          (CommonOpts.opts, projectNames).mapN { case (opts, projectNames) => commands.Compile(forceStarted(), opts, projectNames) }
        ),
        Opts.subcommand("test", "test projects")(
          (CommonOpts.opts, testProjectNames).mapN { case (opts, projectNames) => commands.Test(forceStarted(), opts, projectNames) }
        ),
        Opts.subcommand("setup-ide", "generate ./bsp/bleep.json so IDEs can import build")(
          (CommonOpts.opts, projectNamesNoExpand).mapN { case (_, projectNames) =>
            commands.SetupIde(buildPaths, logger, projectNames)
          }
        ),
        Opts.subcommand("clean", "clean")(
          (CommonOpts.opts, projectNames).mapN { case (opts, projectNames) => commands.Clean(forceStarted(), opts, projectNames) }
        ),
        Opts.subcommand("projects", "show projects under current directory")(
          (CommonOpts.opts, projectNames).mapN { case (_, projectNames) =>
            new BleepCommand {
              override def run(): Unit =
                forceStarted().chosenProjects(projectNames).map(_.value).sorted.foreach(forceStarted().logger.info(_))
            }
          }
        ),
        Opts.subcommand("projects-test", "show test projects under current directory")(
          (CommonOpts.opts, testProjectNames).mapN { case (_, projectNames) =>
            new BleepCommand {
              override def run(): Unit =
                forceStarted().chosenTestProjects(projectNames).map(_.value).sorted.foreach(forceStarted().logger.info(_))
            }
          }
        ),
        Opts.subcommand("patch", "Apply patch from standard-in or file")(
          (CommonOpts.opts, Opts.option[Path]("file", "patch file, defaults to std-in").orNone).mapN((opts, file) => commands.Patch(forceStarted(), opts, file))
        ),
        Opts.subcommand("import", "import existing build from files in .bloop")(
          commands.Import.opts.map { opts =>
            commands.Import(BuildPaths.fromBuildDir(cwd, cwd, Mode.Normal), logger, opts)
          }
        ),
        Opts.subcommand("_complete", "tab-completions")(
          (Opts.argument[String]("COMP_LINE"), Opts.argument[Int]("COMP_CWORD"), Opts.argument[Int]("COMP_POINT")).mapN {
            case (compLine, compCword, compPoint) =>
              new BleepCommand {
                override def run(): Unit = {
                  val msg = List("no completion because build couldn't be bootstrapped")
                  val completer = new Completer({
                    case metavars.platformName    => model.PlatformId.All.map(_.value)
                    case metavars.scalaVersion    => possibleScalaVersions.keys.toList
                    case metavars.projectName     => maybeGlobs.fold(_ => msg, _.projectNameMap.keys.toList)
                    case metavars.testProjectName => maybeGlobs.fold(_ => msg, _.testProjectNameMap.keys.toList)
                    case _                        => Nil
                  })
                  completer.bash(compLine, compCword, compPoint)(ret).foreach(c => println(c.value))
                }
              }
          }
        )
      ),
      maybeStarted match {
        case Left(_) =>
          Nil
        case Right(started) =>
          started.build.scripts.map { case (scriptName, scriptDefs) =>
            Opts.subcommand(scriptName.value, s"run script ${scriptName.value}")(
              stringArgs.map(args => commands.Script(started, scriptName, scriptDefs, args))
            )
          }
      }
    ).flatten.foldK

    ret
  }

  def main(args: Array[String]): Unit =
    args.headOption match {
      case Some("bsp") =>
        val buildPaths = bootstrap.buildPaths(cwd, Mode.BSP) match {
          case Left(th)     => throw th
          case Right(value) => value
        }

        val logFileResource: TypedLoggerResource[BufferedWriter] =
          logging.path(buildPaths.logFile, LogPatterns.logFile)

        val logResource: LoggerResource =
          logFileResource.map { logFile =>
            val stderr = logging.stderr(LogPatterns.logFile).filter(LogLevel.warn)
            logFile.flushing.zipWith(stderr)
          }.untyped

        logResource.use { logger =>
          try BspImpl.run(buildPaths, logger)
          catch {
            case th: Throwable =>
              logger.error("uncaught error", th)
              throw th
          }
        }

      case _ =>
        val buildPaths = bootstrap.buildPaths(cwd, Mode.Normal) match {
          case Left(th)     => throw th
          case Right(value) => value
        }

        val loggerResource: LoggerResource =
          if (args.headOption.contains("_complete"))
            // we can not log to stderr when completing. should be used sparingly
            LoggerResource.pure(logging.stderr(LogPatterns.logFile).filter(LogLevel.warn)).untyped
          else {
            // setup logging to stdout
            val stdout: TypedLogger[PrintStream] =
              logging
                .stdout {
                  if (args.contains("--no-color") || System.console() == null) LogPatterns.logFile
                  else LogPatterns.interface(Instant.now, None)
                }
                .filter(if (args.contains("--debug")) LogLevel.debug else LogLevel.info)

            // and to logfile, without any filtering
            val logFileResource: TypedLoggerResource[BufferedWriter] =
              logging.path(buildPaths.logFile, LogPatterns.logFile)

            LoggerResource.pure(stdout).zipWith(logFileResource).untyped
          }

        loggerResource.use { logger =>
          Command("bleep", "Bleeping fast build!")(mainOpts(logger, buildPaths)).parse(args.toIndexedSeq, sys.env) match {
            case Left(help) => System.err.println(help)
            case Right(cmd) =>
              Try(cmd.run()) match {
                case Failure(unexpected) =>
                  logger.error("Error while running command", unexpected)
                case Success(_) =>
                  ()
              }
          }
        }
    }
}
