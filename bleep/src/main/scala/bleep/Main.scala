package bleep

import bleep.BuildException.fatal
import bleep.BuildPaths.Mode
import bleep.bsp.BspImpl
import bleep.internal.{Lazy, Os, ProjectGlobs}
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

  def noBuildOpts(logger: Logger, cwd: Path): Opts[BleepCommand] = {
    val buildPaths = BuildPaths.fromBuildDir(cwd, cwd, Mode.Normal)
    val userPaths = UserPaths.fromAppDirs
    List(
      Opts.subcommand("build", "rewrite build")(newCommand(logger, cwd)),
      setupIdeCmd(buildPaths, logger, None),
      importCmd(buildPaths, logger),
      compileServerCmd(logger, userPaths, buildPaths, Lazy(CoursierResolver(Nil, logger, downloadSources = false, directories = userPaths)))
    ).foldK
  }

  def argumentFrom[A](defmeta: String, nameToValue: Option[Map[String, A]]): Argument[A] =
    Argument.fromMap(defmeta, nameToValue.getOrElse(Map.empty))

  def hasBuildOpts(started: Started, globs: ProjectGlobs): Opts[BleepCommand] = {

    val projectNames: Opts[List[model.CrossProjectName]] =
      Opts
        .arguments(metavars.projectName)(argumentFrom(metavars.projectName, Some(globs.projectNameMap)))
        .map(_.toList.flatten)
        .orNone
        .map(started.chosenProjects)

    val projectName: Opts[model.CrossProjectName] =
      Opts.argument(metavars.projectNameExact)(argumentFrom(metavars.projectNameExact, Some(globs.exactProjectMap)))

    val testProjectNames: Opts[List[model.CrossProjectName]] =
      Opts
        .arguments(metavars.testProjectName)(argumentFrom(metavars.testProjectName, Some(globs.testProjectNameMap)))
        .map(_.toList.flatten)
        .orNone
        .map(started.chosenTestProjects)

    lazy val ret: Opts[BleepCommand] = {
      val allCommands = List(
        List[Opts[BleepCommand]](
          Opts.subcommand("build", "rewrite build")(
            List(
              newCommand(started.logger, started.buildPaths.cwd),
              Opts.subcommand("templates-reapply", "reapply templates.")(
                Opts(commands.BuildReapplyTemplates(started))
              ),
              Opts.subcommand("templates-generate-new", "throw away existing templates and infer new")(
                Opts(commands.BuildReinferTemplates(started, Set.empty))
              )
            ).foldK
          ),
          Opts.subcommand("compile", "compile projects")(
            projectNames.map(projectNames => commands.Compile(started, projectNames))
          ),
          Opts.subcommand("test", "test projects")(
            testProjectNames.map(projectNames => commands.Test(started, projectNames))
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
              commands.Run(started, projectName, mainClass, arguments)
            }
          ),
          setupIdeCmd(started.buildPaths, started.logger, Some(globs.projectNameMap)),
          Opts.subcommand("clean", "clean")(
            projectNames.map(projectNames => commands.Clean(started, projectNames))
          ),
          Opts.subcommand("projects", "show projects under current directory")(
            projectNames.map(projectNames => () => Right(projectNames.map(_.value).sorted.foreach(started.logger.info(_))))
          ),
          Opts.subcommand("projects-test", "show test projects under current directory")(
            testProjectNames.map(projectNames => () => Right(projectNames.map(_.value).sorted.foreach(started.logger.info(_))))
          ),
          Opts.subcommand("patch", "Apply patch from standard-in or file")(
            Opts.option[Path]("file", "patch file, defaults to std-in").orNone.map(file => commands.Patch(started, file))
          ),
          importCmd(started.buildPaths, started.logger),
          compileServerCmd(started.logger, started.userPaths, started.buildPaths, started.resolver)
        ),
        started.build.scripts.map { case (scriptName, _) =>
          Opts.subcommand(scriptName.value, s"run script ${scriptName.value}")(
            stringArgs.map(args => commands.Script(started, scriptName, args))
          )
        }
      )

      CommonOpts.opts *> allCommands.flatten.foldK
    }

    ret
  }

  def compileServerCmd(logger: Logger, userPaths: UserPaths, buildPaths: BuildPaths, lazyResolver: Lazy[CoursierResolver]): Opts[BleepCommand] =
    Opts.subcommand(
      "compile-server",
      "You can speed up normal usage by keeping the bloop compile server running between invocations. This is where you control it"
    )(
      List(
        Opts.subcommand("start", "will start a shared bloop compile server and leave it running")(Opts {
          commands.CompileServerStart(logger, userPaths, buildPaths, lazyResolver)
        }),
        Opts.subcommand("stop", "will stop a shared bloop compile server (if any) and will make bleep start temporary servers until you call start again")(
          Opts {
            commands.CompileServerStop(logger, userPaths, buildPaths, lazyResolver)
          }
        )
      ).foldK
    )

  def importCmd(buildPaths: BuildPaths, logger: Logger): Opts[BleepCommand] =
    Opts.subcommand("import", "import existing build from files in .bloop")(
      commands.Import.opts.map(opts => commands.Import(sbtBuildDir = buildPaths.cwd, buildPaths, logger, opts))
    )

  def setupIdeCmd(buildPaths: BuildPaths, logger: Logger, projectNameMap: Option[Map[String, Iterable[model.CrossProjectName]]]): Opts[BleepCommand] = {
    val projectNamesNoExpand: Opts[Option[List[String]]] =
      Opts
        .arguments(metavars.projectName)(argumentFrom(metavars.projectName, projectNameMap.map(_.map { case (s, _) => (s, s) })))
        .map(_.toList)
        .orNone

    Opts.subcommand("setup-ide", "generate ./bsp/bleep.json so IDEs can import build")(
      projectNamesNoExpand.map(projectNames => commands.SetupIde(buildPaths, logger, projectNames))
    )
  }

  def newCommand(logger: Logger, cwd: Path): Opts[BleepCommand] =
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
    )

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

        val completions = Prebootstrapped.find(cwd, Mode.Normal, logger) match {
          case Left(_) =>
            val completer = new Completer({
              case metavars.platformName => model.PlatformId.All.map(_.value)
              case metavars.scalaVersion => possibleScalaVersions.keys.toList
              case _                     => Nil
            })
            completer.completeOpts(restArgs)(noBuildOpts(logger, cwd))
          case Right(pre) =>
            bootstrap.from(pre, rewrites = Nil) match {
              case Left(th) => fatal("couldn't load build", logger, th)

              case Right(started) =>
                val globs = ProjectGlobs(started)

                val completer = new Completer({
                  case metavars.platformName     => model.PlatformId.All.map(_.value)
                  case metavars.scalaVersion     => possibleScalaVersions.keys.toList
                  case metavars.projectNameExact => globs.exactProjectMap.keys.toList
                  case metavars.projectName      => globs.projectNameMap.keys.toList
                  case metavars.testProjectName  => globs.testProjectNameMap.keys.toList
                  case _                         => Nil
                })
                completer.completeOpts(restArgs)(hasBuildOpts(started, globs))
            }
        }

        completions.value.foreach(c => println(c.value))

      case "bsp" :: args =>
        val (commonOpts, _) = CommonOpts.parse(args)
        val cwd = cwdFor(commonOpts)
        val stderr = logging.stderr(LogPatterns.logFile).filter(LogLevel.warn)
        val buildPaths = BuildPaths.find(cwd, Mode.BSP) match {
          case Left(th)     => fatal("Couldn't find build", stderr.untyped, th)
          case Right(value) => value
        }

        val logFileResource: TypedLoggerResource[BufferedWriter] =
          logging.path(buildPaths.logFile, LogPatterns.logFile)

        val logResource: LoggerResource =
          logFileResource.map(logFile => logFile.flushing.zipWith(stderr)).untyped

        logResource.use { logger =>
          try BspImpl.run(Prebootstrapped(buildPaths, logger))
          catch {
            case th: Throwable => fatal("uncaught error", logger, th)
          }
        }

      case _args =>
        val (commonOpts, restArgs) = CommonOpts.parse(_args)
        val cwd = cwdFor(commonOpts)

        val stdout: TypedLogger[PrintStream] = {
          val pattern = LogPatterns.interface(Instant.now, None, noColor = commonOpts.noColor)
          logging.stdout(pattern).filter(if (commonOpts.debug) LogLevel.debug else LogLevel.info)
        }

        BuildPaths.find(cwd, Mode.Normal) match {
          case Left(_) =>
            run(stdout.untyped, noBuildOpts(stdout.untyped, cwd), restArgs)

          case Right(buildPaths) =>
            val loggerResource: LoggerResource = {
              // and to logfile, without any filtering
              val logFileResource: TypedLoggerResource[BufferedWriter] =
                logging.path(buildPaths.logFile, LogPatterns.logFile)

              LoggerResource.pure(stdout).zipWith(logFileResource).untyped
            }

            loggerResource.use { logger =>
              bootstrap.from(Prebootstrapped(buildPaths, logger), rewrites = Nil) match {
                case Left(th)       => fatal("Error while loading build", logger, th)
                case Right(started) => run(logger, hasBuildOpts(started, ProjectGlobs(started)), restArgs)
              }
            }
        }
    }

  def run(logger: Logger, opts: Opts[BleepCommand], restArgs: List[String]): Unit =
    Command("bleep", "Bleeping fast build!")(opts).parse(restArgs, sys.env) match {
      case Left(help) => System.err.println(help)
      case Right(cmd) =>
        Try(cmd.run()) match {
          case Failure(th)       => fatal("command failed", logger, th)
          case Success(Left(th)) => fatal("command failed", logger, th)
          case Success(Right(_)) => ()
        }
    }
}
