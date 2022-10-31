package bleep

import bleep.BuildPaths.Mode
import bleep.bsp.BspImpl
import bleep.commands.{DistOptions, PublishLocalOptions}
import bleep.internal.{fatal, Os}
import bleep.logging._
import bleep.packaging.PublishLayout
import cats.data.NonEmptyList
import cats.syntax.apply._
import cats.syntax.foldable._
import com.monovore.decline._
import coursier.jvm.{Execve, JvmIndex}

import java.io.{BufferedWriter, PrintStream}
import java.nio.file.{Path, Paths}
import java.time.Instant
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

object Main {
  val stringArgs: Opts[List[String]] =
    Opts.arguments[String]().orNone.map(args => args.fold(List.empty[String])(_.toList))

  val possibleScalaVersions: Map[String, model.VersionScala] =
    List(model.VersionScala.Scala3, model.VersionScala.Scala213, model.VersionScala.Scala212).map(v => (v.binVersion.replace("\\.", ""), v)).toMap

  object metavars {
    val projectNameExact = "project name exact"
    val projectName = "project name"
    val projectNameNoCross = "project name (not cross id)"
    val testProjectName = "test project name"
    val platformName = "platform name"
    val scalaVersion = "scala version"
  }

  def noBuildOpts(logger: Logger, buildPaths: BuildPaths, buildLoader: BuildLoader.NonExisting): Opts[BleepCommand] = {
    val userPaths = UserPaths.fromAppDirs
    val resolver =
      BleepConfigOps
        .lazyForceLoad(userPaths)
        .map(bleepConfig => CoursierResolver(Nil, logger, downloadSources = false, cacheIn = userPaths.coursierCacheDir, bleepConfig.authentications, None))

    CommonOpts.opts *> List(
      Opts.subcommand("build", "rewrite build")(newCommand(logger, buildPaths.cwd)),
      setupIdeCmd(buildPaths, logger, None),
      importCmd(buildLoader, buildPaths, logger),
      compileServerCmd(logger, userPaths, resolver)
    ).foldK
  }

  def argumentFrom[A](defmeta: String, nameToValue: Option[Map[String, A]]): Argument[A] =
    Argument.fromMap(defmeta, nameToValue.getOrElse(Map.empty))

  def hasBuildOpts(started: Started): Opts[BleepCommand] = {
    val projectNamesNoCross: Opts[NonEmptyList[model.ProjectName]] =
      Opts
        .arguments(metavars.projectNameNoCross)(argumentFrom(metavars.projectNameNoCross, Some(started.globs.projectNamesNoCrossMap)))

    val projectNames: Opts[List[model.CrossProjectName]] =
      Opts
        .arguments(metavars.projectName)(argumentFrom(metavars.projectName, Some(started.globs.projectNameMap)))
        .map(_.toList.flatten)
        .orNone
        .map(started.chosenProjects)

    val projectName: Opts[model.CrossProjectName] =
      Opts.argument(metavars.projectNameExact)(argumentFrom(metavars.projectNameExact, Some(started.globs.exactProjectMap)))

    val testProjectNames: Opts[List[model.CrossProjectName]] =
      Opts
        .arguments(metavars.testProjectName)(argumentFrom(metavars.testProjectName, Some(started.globs.testProjectNameMap)))
        .map(_.toList.flatten)
        .orNone
        .map(started.chosenTestProjects)

    val mainClass = Opts
      .option[String](
        "class",
        "explicitly override main class. If not set, bleep will first look in the build file, then fall back to looking into compiled class files"
      )
      .orNone

    lazy val ret: Opts[BleepCommand] = {
      val allCommands = List(
        List[Opts[BleepCommand]](
          Opts.subcommand("build", "rewrite build")(
            List(
              newCommand(started.logger, started.buildPaths.cwd),
              Opts.subcommand("create-directories", "create all source and resource folders for project(s)")(
                projectNames.map(names => commands.BuildCreateDirectories(started, names))
              ),
              Opts.subcommand("templates-reapply", "apply existing templates again")(
                Opts(commands.BuildReapplyTemplates(started))
              ),
              Opts.subcommand("templates-generate-new", "throw away existing templates and infer new")(
                Opts(commands.BuildReinferTemplates(started, Set.empty))
              ),
              Opts.subcommand("update-deps", "updates to newest versions of all dependencies")(
                Opts(commands.BuildUpdateDeps(started))
              ),
              Opts.subcommand(
                "move-files-into-bleep-layout",
                "move source files around from sbt file layout to bleep layout. Your build will no longer have any `sbt-scope` or `folder` set."
              )(
                Opts(commands.BuildMoveFilesIntoBleepLayout(started))
              ),
              Opts.subcommand("diff", "diff exploded projects compared to git HEAD or wanted revision")(
                commands.BuildDiff.opts.map(opts => commands.BuildDiff(started, opts))
              ),
              Opts.subcommand("show", "show projects in their different versions.")(
                List(
                  Opts.subcommand("short", "the projects as you wrote it in YAML")(
                    projectNamesNoCross.map(names => commands.BuildShow.Short(started, names))
                  ),
                  Opts.subcommand("exploded", "the cross projects as you wrote it in YAML after templates have been applied")(
                    projectNames.map(names => commands.BuildShow.Exploded(started, names))
                  ),
                  Opts.subcommand("bloop", "the cross projects as they appear to bloop, that is with all absolute paths and so on")(
                    projectNames.map(names => commands.BuildShow.Bloop(started, names))
                  )
                ).foldK
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
              mainClass,
              Opts.arguments[String]("arguments").map(_.toList).withDefault(List.empty)
            ).mapN { case (projectName, mainClass, arguments) =>
              commands.Run(started, projectName, mainClass, arguments)
            }
          ),
          setupIdeCmd(started.buildPaths, started.logger, Some(started.globs.projectNameMap)),
          Opts.subcommand("clean", "clean")(
            projectNames.map(projectNames => commands.Clean(started, projectNames))
          ),
          Opts.subcommand("projects", "show projects under current directory")(
            projectNames.map(projectNames => () => Right(projectNames.map(_.value).sorted.foreach(started.logger.info(_))))
          ),
          Opts.subcommand("projects-test", "show test projects under current directory")(
            testProjectNames.map(projectNames => () => Right(projectNames.map(_.value).sorted.foreach(started.logger.info(_))))
          ),
          importCmd(started.prebootstrapped.existingBuild, started.buildPaths, started.logger),
          compileServerCmd(started.prebootstrapped.logger, started.prebootstrapped.userPaths, started.resolver),
          Opts.subcommand("publish-local", "publishes your project locally") {
            (
              Opts.option[String]("groupId", "organization you will publish under"),
              Opts.option[String]("version", "version you will publish"),
              Opts.option[Path]("to", s"specify a folder other than ${constants.ivy2Path}").orNone,
              Opts.flag("maven-layout", "publish with maven layout instead of ivy").orFalse,
              projectNames
            ).mapN { case (groupId, version, to, mavenLayout, projects) =>
              val options = PublishLocalOptions(
                groupId = groupId,
                version = version,
                to = to,
                projects,
                publishLayout = if (mavenLayout) PublishLayout.Maven() else PublishLayout.Ivy
              )
              commands.PublishLocal(started, options)
            }
          },
          Opts.subcommand("dist", "creates a folder with a runnable distribution") {
            (
              projectName,
              mainClass,
              Opts.argument[Path]("path").orNone
            ).mapN { case (projectName, mainClass, overridePath) =>
              val options = DistOptions(
                projectName,
                overrideMain = mainClass,
                overridePath = overridePath
              )
              commands.Dist(started, options)
            }
          }
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

  def compileServerCmd(logger: Logger, userPaths: UserPaths, lazyResolver: Lazy[CoursierResolver]): Opts[BleepCommand] =
    Opts.subcommand(
      "compile-server",
      "You can speed up normal usage by keeping the bloop compile server running between invocations. This is where you control it"
    )(
      List(
        Opts.subcommand(
          "auto-shutdown-disable",
          "leave compile servers running between bleep invocations. this gets much better performance at the cost of memory"
        )(Opts {
          commands.CompileServerSetMode(logger, userPaths, lazyResolver, model.CompileServerMode.Shared)
        }),
        Opts.subcommand("auto-shutdown-enable", "shuts down compile server after between bleep invocation. this is slower, but conserves memory")(Opts {
          commands.CompileServerSetMode(logger, userPaths, lazyResolver, model.CompileServerMode.NewEachInvocation)
        }),
        Opts.subcommand("stop-all", "will stop all shared bloop compile servers")(
          Opts {
            commands.CompileServerStopAll(logger, userPaths, lazyResolver)
          }
        )
      ).foldK
    )

  def importCmd(buildLoader: BuildLoader, buildPaths: BuildPaths, logger: Logger): Opts[BleepCommand] =
    Opts.subcommand("import", "import existing build from files in .bloop")(
      commands.Import.opts.map { opts =>
        val existingBuild = buildLoader.existing.flatMap(_.buildFile.forceGet).toOption

        commands.Import(existingBuild, sbtBuildDir = buildPaths.cwd, buildPaths, logger, opts, model.BleepVersion.current)
      }
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
          .withDefault(NonEmptyList.of(model.VersionScala.Scala3)),
        Opts.argument[String]("wanted project name")
      ).mapN { case (platforms, scalas, name) => commands.BuildCreateNew(logger, cwd, platforms, scalas, name, model.BleepVersion.current) }
    )

  // there is a flag to change build directory. we need to intercept that very early
  def cwdFor(opts: CommonOpts): Path =
    opts.directory match {
      case Some(str) if str.startsWith("/") => Paths.get(str)
      case Some(str)                        => Os.cwd / str
      case None                             => Os.cwd
    }

  def maybeRunWithDifferentVersion(args: Array[String], buildLoader: BuildLoader, logger: Logger, opts: CommonOpts): Unit =
    buildLoader match {
      case BuildLoader.NonExisting(_) => ()
      case existing: BuildLoader.Existing =>
        existing.wantedVersion.forceGet match {
          case Right(wantedVersion) if model.BleepVersion.current == wantedVersion || wantedVersion == model.BleepVersion.dev => ()
          case Right(wantedVersion) if model.BleepVersion.current.isDevelopment =>
            logger.info(s"Not launching Bleep version ${wantedVersion.value} (from ${existing.bleepYaml}) because you're running a snapshot")
          case Right(wantedVersion) if opts.dev =>
            logger.info(s"Not launching Bleep version ${wantedVersion.value} (from ${existing.bleepYaml}) because you specified --dev")
          case Right(wantedVersion) =>
            logger.info(s"Launching Bleep version ${wantedVersion.value} as requested in ${existing.bleepYaml}")
            val cacheLogger = new BleepCacheLogger(logger)
            FetchBleepRelease(wantedVersion, cacheLogger, ExecutionContext.global) match {
              case Left(buildException) =>
                fatal("", logger, buildException)
              case Right(binaryPath) if JvmIndex.currentOs.contains("windows") =>
                val status = scala.sys.process.Process(binaryPath.toString :: args.toList, Os.cwd.toFile, sys.env.toSeq: _*).!<
                sys.exit(status)
              case Right(path) =>
                Execve.execve(path.toString, path.toString +: args, sys.env.map { case (k, v) => s"$k=$v" }.toArray)
                sys.error("should not be reached")
            }
          case Left(throwable) =>
            fatal("Couldn't load build", logger, throwable)
        }
    }

  def main(_args: Array[String]): Unit =
    _args.toList match {
      case "_complete" :: compLine :: compCword :: compPoint :: _ =>
        val args = Completer.bashToArgs(compLine, compCword.toInt, compPoint.toInt)
        val (_, restArgs) = CommonOpts.parse(args)
        // accept -d after completion point
        val (commonOpts, _) = CommonOpts.parse(compLine.split(" ").toList)
        val cwd = cwdFor(commonOpts)
        // we can not log to stdout when completing. logger should be used sparingly
        val logger = logging.stderr(LogPatterns.logFile).filter(LogLevel.warn).untyped
        val buildLoader = BuildLoader.find(cwd)
        maybeRunWithDifferentVersion(_args, buildLoader, logger, commonOpts)

        val buildPaths = BuildPaths(cwd, buildLoader, Mode.Normal)

        val completions = buildLoader match {
          case noBuild: BuildLoader.NonExisting =>
            val completer = new Completer({
              case metavars.platformName => model.PlatformId.All.map(_.value)
              case metavars.scalaVersion => possibleScalaVersions.keys.toList
              case _                     => Nil
            })
            completer.completeOpts(restArgs)(noBuildOpts(logger, buildPaths, noBuild))
          case existing: BuildLoader.Existing =>
            val userPaths = UserPaths.fromAppDirs
            val pre = Prebootstrapped(logger, userPaths, buildPaths, existing)

            val bleepConfig = BleepConfigOps.lazyForceLoad(pre.userPaths)

            bootstrap.from(pre, GenBloopFiles.SyncToDisk, rewrites = Nil, bleepConfig) match {
              case Left(th) => fatal("couldn't load build", logger, th)

              case Right(started) =>
                val completer = new Completer({
                  case metavars.platformName       => model.PlatformId.All.map(_.value)
                  case metavars.scalaVersion       => possibleScalaVersions.keys.toList
                  case metavars.projectNameExact   => started.globs.exactProjectMap.keys.toList
                  case metavars.projectNameNoCross => started.globs.projectNamesNoCrossMap.keys.toList
                  case metavars.projectName        => started.globs.projectNameMap.keys.toList
                  case metavars.testProjectName    => started.globs.testProjectNameMap.keys.toList
                  case _                           => Nil
                })
                completer.completeOpts(restArgs)(hasBuildOpts(started))
            }
        }

        completions.value.foreach(c => println(c.value))

      case "bsp" :: args =>
        val (commonOpts, _) = CommonOpts.parse(args)
        val cwd = cwdFor(commonOpts)
        val stderr = logging.stderr(LogPatterns.logFile).filter(LogLevel.warn)
        val buildLoader = BuildLoader.find(cwd)
        val userPaths = UserPaths.fromAppDirs
        maybeRunWithDifferentVersion(_args, buildLoader, stderr.untyped, commonOpts)

        val buildPaths = BuildPaths(cwd, buildLoader, Mode.BSP)

        val logFileResource: TypedLoggerResource[BufferedWriter] =
          logging.path(buildPaths.logFile, LogPatterns.logFile)

        val logResource: LoggerResource =
          logFileResource.map(logFile => logFile.flushing.zipWith(stderr)).untyped

        logResource.use { logger =>
          buildLoader.existing.map(existing => Prebootstrapped(logger, userPaths, buildPaths, existing)) match {
            case Left(be) => fatal("", logger, be)
            case Right(pre) =>
              try BspImpl.run(pre)
              catch {
                case th: Throwable => fatal("uncaught error", logger, th)
              }
          }
        }

      case args =>
        val logAsJson = sys.env.contains(jsonEvents.CallerProcessAcceptsJsonEvents)
        val (commonOpts, restArgs) = CommonOpts.parse(args)
        val cwd = cwdFor(commonOpts)

        val stdout: TypedLogger[PrintStream] =
          if (logAsJson) logging.stdoutJson()
          else {
            val pattern = LogPatterns.interface(Instant.now, noColor = commonOpts.noColor)
            logging.stdout(pattern).filter(if (commonOpts.debug) LogLevel.debug else LogLevel.info)
          }
        val buildLoader = BuildLoader.find(cwd)
        maybeRunWithDifferentVersion(_args, buildLoader, stdout.untyped, commonOpts)

        def stdoutAndFileLogging(buildPaths: BuildPaths): LoggerResource =
          if (logAsJson) LoggerResource.pure(stdout.untyped)
          else {
            // and to logfile, without any filtering
            val logFileResource: TypedLoggerResource[BufferedWriter] =
              logging.path(buildPaths.logFile, LogPatterns.logFile).map(_.flushing)

            LoggerResource.pure(stdout).zipWith(logFileResource).untyped
          }

        val buildPaths = BuildPaths(cwd, buildLoader, Mode.Normal)
        buildLoader match {
          case noBuild: BuildLoader.NonExisting =>
            stdoutAndFileLogging(buildPaths).use { logger =>
              run(logger, noBuildOpts(stdout.untyped, buildPaths, noBuild), restArgs)
            }
          case existing: BuildLoader.Existing =>
            stdoutAndFileLogging(buildPaths).use { logger =>
              val userPaths = UserPaths.fromAppDirs
              val pre = Prebootstrapped(logger, userPaths, buildPaths, existing)
              val bleepConfig = BleepConfigOps.lazyForceLoad(pre.userPaths)
              bootstrap.from(pre, GenBloopFiles.SyncToDisk, rewrites = Nil, bleepConfig) match {
                case Left(th)       => fatal("Error while loading build", logger, th)
                case Right(started) => run(logger, hasBuildOpts(started), restArgs)
              }
            }
        }
    }

  def run(logger: Logger, opts: Opts[BleepCommand], restArgs: List[String]): Unit =
    Command("bleep", s"Bleeping fast build! (version ${model.BleepVersion.current.value})")(opts).parse(restArgs, sys.env) match {
      case Left(help) =>
        System.err.println(help)
        System.exit(1)
      case Right(cmd) =>
        Try(cmd.run()) match {
          case Failure(th)       => fatal("command failed", logger, th)
          case Success(Left(th)) => fatal("command failed", logger, th)
          case Success(Right(_)) => ()
        }
    }
}
