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
import java.nio.file.{Path, Paths}
import java.time.Instant
import scala.util.{Failure, Success, Try}

object Main {
  val stringArgs: Opts[List[String]] =
    Opts.arguments[String]().orNone.map(args => args.fold(List.empty[String])(_.toList))
  val possibleScalaVersions: Map[String, Versions.Scala] =
    List(Versions.Scala3, Versions.Scala213, Versions.Scala212).map(v => (v.binVersion.replace("\\.", ""), v)).toMap

  object metavars {
    val projectNameExact = "project name exact"
    val projectName = "project name"
    val testProjectName = "test project name"
    val platformName = "platform name"
    val scalaVersion = "scala version"
  }

  def mainOpts(
      cwd: Path,
      logger: Logger,
      buildPaths: BuildPaths,
      maybeStarted: Either[BuildException, Started],
      maybeGlobs: Option[ProjectGlobs]
  ): Opts[BleepCommand] = {
    def forceStarted() = maybeStarted match {
      case Left(buildException) =>
        logger.error("couldn't initialize build", buildException)
        sys.exit(1)
      case Right(started) => started
    }

    def argumentFrom[A](defmeta: String, nameToValue: Option[Map[String, A]]): Argument[A] =
      Argument.fromMap(defmeta, nameToValue.getOrElse(Map.empty))

    val projectNames: Opts[Option[List[model.CrossProjectName]]] =
      Opts.arguments(metavars.projectName)(argumentFrom(metavars.projectName, maybeGlobs.map(_.projectNameMap))).map(_.toList.flatten).orNone

    val projectName: Opts[model.CrossProjectName] =
      Opts.argument(metavars.projectNameExact)(argumentFrom(metavars.projectNameExact, maybeGlobs.map(_.exactProjectMap)))

    val projectNamesNoExpand: Opts[Option[List[String]]] =
      Opts
        .arguments(metavars.projectName)(argumentFrom(metavars.projectName, maybeGlobs.map(_.projectNameMap.map { case (s, _) => (s, s) })))
        .map(_.toList)
        .orNone

    val testProjectNames: Opts[Option[List[model.CrossProjectName]]] =
      Opts.arguments(metavars.testProjectName)(argumentFrom(metavars.testProjectName, maybeGlobs.map(_.testProjectNameMap))).map(_.toList.flatten).orNone

    lazy val ret: Opts[BleepCommand] = {
      val allCommands = List(
        List(
          Opts.subcommand("build", "rewrite build")(
            List(
              Opts.subcommand("new", "create new build in current directory")(
                (
                  Opts
                    .options("platform", "specify wanted platform(s)", metavar = metavars.platformName, short = "p")(
                      Argument.fromMap(metavars.platformName, model.PlatformId.All.map(p => (p.value, p)).toMap)
                    )
                    .withDefault(NonEmptyList.of(model.PlatformId.Jvm)),
                  Opts
                    .options("scala", "specify scala version(s)", "s", metavars.scalaVersion)(Argument.fromMap(metavars.scalaVersion, possibleScalaVersions))
                    .withDefault(NonEmptyList.of(Versions.Scala3)),
                  Opts.argument[String]("wanted project name")
                ).mapN { case (platforms, scalas, name) => commands.BuildCreateNew(logger, cwd, platforms, scalas, name) }
              ),
              Opts.subcommand("templates-reapply", "reapply templates.")(
                Opts(commands.BuildReapplyTemplates(forceStarted()))
              ),
              Opts.subcommand("templates-generate-new", "throw away existing templates and infer new")(
                Opts(commands.BuildReinferTemplates(forceStarted(), Set.empty))
              )
            ).foldK
          ),
          Opts.subcommand("compile", "compile projects")(
            projectNames.map(projectNames => commands.Compile(forceStarted(), projectNames))
          ),
          Opts.subcommand("test", "test projects")(
            testProjectNames.map(projectNames => commands.Test(forceStarted(), projectNames))
          ),
          Opts.subcommand("run", "run project")(
            (
              projectName,
              Opts
                .option[String](
                  "class",
                  "explicitly override main class. If not set, bleep will first look in the build file, then fall back to looking into compiled class files"
                )
                .orNone,
              Opts.arguments[String]("arguments").map(_.toList).withDefault(List.empty)
            ).mapN { case (projectName, mainClass, arguments) =>
              commands.Run(forceStarted(), projectName, mainClass, arguments)
            }
          ),
          Opts.subcommand("setup-ide", "generate ./bsp/bleep.json so IDEs can import build")(
            projectNamesNoExpand.map(projectNames => commands.SetupIde(buildPaths, logger, projectNames))
          ),
          Opts.subcommand("clean", "clean")(
            projectNames.map(projectNames => commands.Clean(forceStarted(), projectNames))
          ),
          Opts.subcommand("projects", "show projects under current directory")(
            projectNames.map { projectNames =>
              new BleepCommand {
                override def run(): Unit =
                  forceStarted().chosenProjects(projectNames).map(_.value).sorted.foreach(forceStarted().logger.info(_))
              }
            }
          ),
          Opts.subcommand("projects-test", "show test projects under current directory")(
            testProjectNames.map { projectNames =>
              new BleepCommand {
                override def run(): Unit =
                  forceStarted().chosenTestProjects(projectNames).map(_.value).sorted.foreach(forceStarted().logger.info(_))
              }
            }
          ),
          Opts.subcommand("patch", "Apply patch from standard-in or file")(
            Opts.option[Path]("file", "patch file, defaults to std-in").orNone.map(file => commands.Patch(forceStarted(), file))
          ),
          Opts.subcommand("import", "import existing build from files in .bloop")(
            commands.Import.opts.map { opts =>
              commands.Import(BuildPaths.fromBuildDir(cwd, cwd, Mode.Normal), logger, opts)
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
      )

      CommonOpts.opts *> allCommands.flatten.foldK
    }

    ret
  }

  // there is a flag to change build directory. we need to intercept that very early
  def cwdFor(opts: CommonOpts): Path =
    opts.directory match {
      case Some(str) if str.startsWith("/") => Paths.get(str)
      case Some(str)                        => Os.cwd / str
      case None                             => Os.cwd
    }

  def main(_args: Array[String]): Unit =
    _args.toList match {
      case "_complete" :: compLine :: compCword :: compPoint :: _ =>
        val args = Completer.bashToArgs(compLine, compCword.toInt, compPoint.toInt)
        val (_, restArgs) = CommonOpts.parse(args)
        // accept -d after completion point
        val (commonOpts, _) = CommonOpts.parse(compLine.split(" ").toList)
        val cwd = cwdFor(commonOpts)
        // we can not log to stderr when completing. should be used sparingly
        val logger = logging.stderr(LogPatterns.logFile).filter(LogLevel.warn).untyped

        val buildPaths = bootstrap.buildPaths(cwd, Mode.Normal) match {
          case Left(th) =>
            logger.error(th.getMessage)
            sys.exit(1)
          case Right(value) => value
        }

        val maybeStarted = bootstrap.from(logger, buildPaths, rewrites = Nil)
        val maybeGlobs = maybeStarted.toOption.map(ProjectGlobs.apply)

        val msg = List("no completion because build couldn't be bootstrapped")
        val completer = new Completer({
          case metavars.platformName     => model.PlatformId.All.map(_.value)
          case metavars.scalaVersion     => possibleScalaVersions.keys.toList
          case metavars.projectNameExact => maybeGlobs.fold(msg)(_.exactProjectMap.keys.toList)
          case metavars.projectName      => maybeGlobs.fold(msg)(_.projectNameMap.keys.toList)
          case metavars.testProjectName  => maybeGlobs.fold(msg)(_.testProjectNameMap.keys.toList)
          case _                         => Nil
        })
        val opts = mainOpts(cwd, logger, buildPaths, maybeStarted, maybeGlobs)

        completer.completeOpts(restArgs)(opts).value.foreach(c => println(c.value))

      case "bsp" :: args =>
        val (commonOpts, _) = CommonOpts.parse(args)
        val cwd = cwdFor(commonOpts)
        val stderr = logging.stderr(LogPatterns.logFile).filter(LogLevel.warn)
        val buildPaths = bootstrap.buildPaths(cwd, Mode.BSP) match {
          case Left(th) =>
            stderr.error("Couldn't find build", th)
            sys.exit(1)
          case Right(value) => value
        }

        val logFileResource: TypedLoggerResource[BufferedWriter] =
          logging.path(buildPaths.logFile, LogPatterns.logFile)

        val logResource: LoggerResource =
          logFileResource.map(logFile => logFile.flushing.zipWith(stderr)).untyped

        logResource.use { logger =>
          try BspImpl.run(buildPaths, logger)
          catch {
            case th: Throwable =>
              logger.error("uncaught error", th)
              sys.exit(1)
          }
        }

      case _args =>
        val (commonOpts, restArgs) = CommonOpts.parse(_args)
        val cwd = cwdFor(commonOpts)

        val stdout: TypedLogger[PrintStream] = {
          val pattern =
            if (commonOpts.noColor || System.console() == null) LogPatterns.logFile
            else LogPatterns.interface(Instant.now, None)
          logging.stdout(pattern).filter(if (commonOpts.debug) LogLevel.debug else LogLevel.info)
        }

        val buildPaths = bootstrap.buildPaths(cwd, Mode.Normal) match {
          case Left(th) =>
            stdout.error("Couldn't find build", th)
            sys.exit(1)

          case Right(value) => value
        }

        val loggerResource: LoggerResource = {
          // and to logfile, without any filtering
          val logFileResource: TypedLoggerResource[BufferedWriter] =
            logging.path(buildPaths.logFile, LogPatterns.logFile)

          LoggerResource.pure(stdout).zipWith(logFileResource).untyped
        }

        loggerResource.use { logger =>
          val maybeStarted = bootstrap.from(logger, buildPaths, rewrites = Nil)
          val maybeGlobs = maybeStarted.toOption.map(ProjectGlobs.apply)
          val opts = mainOpts(cwd, logger, buildPaths, maybeStarted, maybeGlobs)

          Command("bleep", "Bleeping fast build!")(opts).parse(restArgs, sys.env) match {
            case Left(help) => System.err.println(help)
            case Right(cmd) =>
              Try(cmd.run()) match {
                case Failure(unexpected) =>
                  logger.error("Error while running command", unexpected)
                  sys.exit(1)
                case Success(_) =>
                  ()
              }
          }
        }
    }
}
