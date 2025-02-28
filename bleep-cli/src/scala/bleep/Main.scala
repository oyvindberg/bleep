package bleep

import bleep.bsp.BspImpl
import bleep.internal.{bleepLoggers, fatal, logException, FileUtils}
import bleep.packaging.ManifestCreator
import cats.data.NonEmptyList
import cats.syntax.apply.*
import cats.syntax.foldable.*
import com.monovore.decline.*
import coursier.jvm.Execve
import ryddig.Logger

import java.nio.file.{Path, Paths}
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Properties, Success, Try}

object Main {
  private def isGraalvmNativeImage: Boolean =
    sys.props.contains("org.graalvm.nativeimage.imagecode")

  if (Properties.isWin && isGraalvmNativeImage)
    // have to be initialized before running (new Argv0).get because Argv0SubstWindows uses csjniutils library
    // The DLL loaded by LoadWindowsLibrary is statically linke/d in
    // the Scala CLI native image, no need to manually load it.
    coursier.jniutils.LoadWindowsLibrary.assumeInitialized()

  val stringArgs: Opts[List[String]] =
    Opts.arguments[String]().orNone.map(args => args.fold(List.empty[String])(_.toList))

  val possibleScalaVersions: Map[String, model.VersionScala] =
    List(model.VersionScala.Scala3, model.VersionScala.Scala213, model.VersionScala.Scala212).map(v => (v.binVersion.replace("\\.", ""), v)).toMap

  object metavars {
    val projectNameExact = "project name exact"
    val projectName = "project name"
    val projectNameNoCross = "project name (not cross id)"
    val testProjectName = "test project name"
    val hasSourceGenProject = "project name (with sourcegen)"
    val platformName = "platform name"
    val scalaVersion = "scala version"
  }

  val commonOpts: Opts[CommonOpts] =
    (
      Opts.flag("no-color", "enable CI-friendly output").orFalse,
      Opts.flag("debug", "enable more output").orFalse,
      Opts.option[String]("directory", "specify a project directory to use, instead of the current working directory", "d").orNone,
      Opts.flag("dev", "use the current bleep binary and don't launch the one specified in bleep.yaml").orFalse,
      Opts.flag("no-bsp-progress", "don't show compilation progress. good for CI").orFalse,
      Opts.flag("log-as-json", "bleep internal: for running bleep scripts").orFalse
    ).mapN(CommonOpts.apply)

  def noBuildOpts(logger: Logger, userPaths: UserPaths, buildPaths: BuildPaths, buildLoader: BuildLoader.NonExisting): Opts[BleepNoBuildCommand] =
    commonOpts *> List(
      Opts.subcommand("build", "create new build")(newCommand(logger, userPaths, buildPaths.cwd)),
      importCmd(buildLoader, userPaths, buildPaths, logger),
      configCommand(logger, userPaths),
      installTabCompletions(userPaths, logger)
    ).foldK

  def argumentFrom[A](defmeta: String, nameToValue: Option[Map[String, A]]): Argument[A] =
    Argument.fromMap(defmeta, nameToValue.getOrElse(Map.empty))

  def hasBuildOpts(started: Started): Opts[BleepBuildCommand] = {
    val projectNamesNoCross: Opts[NonEmptyList[model.ProjectName]] =
      Opts
        .arguments(metavars.projectNameNoCross)(argumentFrom(metavars.projectNameNoCross, Some(started.globs.projectNamesNoCrossMap)))

    val projectNames: Opts[Array[model.CrossProjectName]] =
      Opts
        .arguments(metavars.projectName)(argumentFrom(metavars.projectName, Some(started.globs.projectNameMap)))
        .map(_.toList.toArray.flatten)
        .orNone
        .map(started.chosenProjects)

    val projectNameNoCross: Opts[model.ProjectName] =
      Opts.argument(metavars.projectNameNoCross)(argumentFrom(metavars.projectNameNoCross, Some(started.globs.projectNamesNoCrossMap)))

    val projectName: Opts[model.CrossProjectName] =
      Opts.argument(metavars.projectNameExact)(argumentFrom(metavars.projectNameExact, Some(started.globs.exactProjectMap)))

    val projectNamesNoExpand: Opts[Option[List[String]]] =
      Opts
        .arguments(metavars.projectName)(argumentFrom(metavars.projectName, Some(started.globs.projectNameMap).map(_.map { case (s, _) => (s, s) })))
        .map(_.toList)
        .orNone

    val testProjectNames: Opts[Array[model.CrossProjectName]] =
      Opts
        .arguments(metavars.testProjectName)(argumentFrom(metavars.testProjectName, Some(started.globs.testProjectNameMap)))
        .map(_.toList.toArray.flatten)
        .orNone
        .map(started.chosenTestProjects)

    val testSuitesOnly: Opts[Option[NonEmptyList[String]]] =
      Opts
        .options[String](
          "only",
          "Test only a subset of test suite class names. Class name can be fully qualified to disambiguate",
          "o"
        )
        .orNone
    val testSuitesExclude: Opts[Option[NonEmptyList[String]]] =
      Opts
        .options[String](
          "exclude",
          "Exclude specific test suite class names. Class name can be fully qualified to disambiguate. Takes precedence over --only",
          "x"
        )
        .orNone

    val hasSourcegenProjectNames: Opts[Array[model.CrossProjectName]] =
      Opts
        .arguments(metavars.hasSourceGenProject)(argumentFrom(metavars.hasSourceGenProject, Some(started.globs.hasSourceGenProjectNameMap)))
        .map(_.toList.toArray.flatten)
        .orNone
        .map(started.chosenHasSourceGenProjects)

    val mainClass = Opts
      .option[String](
        "class",
        "explicitly override main class. If not set, bleep will first look in the build file, then fall back to looking into compiled class files"
      )
      .orNone

    val watch = Opts.flag("watch", "start in watch mode", "w").orFalse

    val isReleaseMode = Opts.flag("release", "force linking in release mode", "r").orFalse

    val updateAsScalaSteward = Opts
      .flag(
        "steward",
        "Use same upgrade strategy as Scala Steward. Updates to the latest patch version at the same minor and major version. If the dependency is already on the latest patch version, it updates to the latest minor version at the same major version. And if the dependency is already on the latest minor version, it updates to the latest major version."
      )
      .orFalse

    val updateWithPrerelease = Opts.flag("prerelease", "Allow upgrading to prerelease version if there is any.").orFalse

    val updateSingleOrgOrModule = Opts.argument[String]("The dependency to update, alternatively only the organization name can be passed")

    lazy val ret: Opts[BleepBuildCommand] = {
      val allCommands = List(
        List[Opts[BleepBuildCommand]](
          Opts.subcommand("build", "rewrite build")(
            List(
              Opts.subcommand("create-directories", "create all source and resource folders for project(s)")(
                projectNames.map(names => commands.BuildCreateDirectories(names))
              ),
              Opts.subcommand("normalize", "normalize build (deduplicate, sort, etc)")(
                Opts(commands.BuildNormalize)
              ),
              Opts.subcommand("templates-reapply", "apply existing templates again")(
                Opts(commands.BuildReapplyTemplates)
              ),
              Opts.subcommand("project-rename", "rename project")(
                (projectNameNoCross, Opts.argument[String]("new project name")).mapN { case (from, to) =>
                  new commands.BuildProjectRename(from, model.ProjectName(to))
                }
              ),
              Opts.subcommand("project-merge-into", "merge first project into second")(
                (projectNameNoCross, projectNameNoCross).mapN { case (projectName, into) =>
                  new commands.BuildProjectMergeInto(projectName, into)
                }
              ),
              Opts.subcommand("projects-move", "move projects")(
                (Opts.argument[String]("new parent folder"), projectNamesNoCross).mapN { case (parentFolder, projectNames) =>
                  new commands.BuildProjectMove(Path.of(parentFolder).toAbsolutePath, projectNames)
                }
              ),
              Opts.subcommand("templates-generate-new", "throw away existing templates and infer new")(
                Opts(commands.BuildReinferTemplates(Set.empty))
              ),
              Opts.subcommand("update-deps", "updates to newest versions of all dependencies")(
                (updateAsScalaSteward, updateWithPrerelease).mapN { case (sw, prerelease) =>
                  commands.BuildUpdateDeps.apply(sw, prerelease, None)
                }
              ),
              Opts.subcommand("update-dep", "update a single dependency or dependencies of a single organization to newest version(s)")(
                (updateSingleOrgOrModule, updateAsScalaSteward, updateWithPrerelease).mapN { case (singleDep, sw, prerelease) =>
                  commands.BuildUpdateDeps.apply(sw, prerelease, Some(singleDep))
                }
              ),
              Opts.subcommand(
                "move-files-into-bleep-layout",
                "move source files around from sbt file layout to bleep layout. Your build will no longer have any `sbt-scope` or `folder` set."
              )(
                Opts(commands.BuildMoveFilesIntoBleepLayout)
              ),
              Opts.subcommand("diff", "diff exploded projects compared to git HEAD or wanted revision")(
                List(
                  Opts.subcommand("exploded", "show projects after applying templates")(
                    (projectNames, commands.BuildDiff.opts).mapN { case (names, opts) =>
                      commands.BuildDiff(opts, names)
                    }
                  ),
                  Opts.subcommand("bloop", "show projects as seen by bloop")(
                    (projectNames, commands.BuildDiff.opts).mapN { case (names, opts) =>
                      commands.BuildDiffBloop(opts, names)
                    }
                  )
                ).foldK
              ),
              Opts.subcommand("show", "show projects in their different versions.")(
                List(
                  Opts.subcommand("short", "the projects as you wrote it in YAML")(
                    projectNamesNoCross.map(commands.BuildShow.Short.apply)
                  ),
                  Opts.subcommand("exploded", "the cross projects as you wrote it in YAML after templates have been applied")(
                    projectNames.map(commands.BuildShow.Exploded.apply)
                  ),
                  Opts.subcommand("bloop", "the cross projects as they appear to bloop, that is with all absolute paths and so on")(
                    projectNames.map(commands.BuildShow.Bloop.apply)
                  )
                ).foldK
              ),
              Opts.subcommand("evicted", "show eviction warnings for project")(
                projectNames.map(projectNames => commands.Evicted(projectNames))
              ),
              newCommand(started.pre.logger, started.pre.userPaths, started.buildPaths.cwd)
            ).foldK
          ),
          Opts.subcommand("compile", "compile projects")(
            (watch, projectNames).mapN { case (watch, projectNames) => commands.Compile(watch, projectNames) }
          ),
          Opts.subcommand("link", "link projects")(
            (watch, projectNames, isReleaseMode).mapN { case (watch, projectNames, isReleaseMode) =>
              commands.Link(watch, projectNames, isReleaseMode)
            }
          ),
          Opts.subcommand("sourcegen", "run source generators for projects")(
            (watch, hasSourcegenProjectNames).mapN { case (watch, projectNames) => commands.SourceGen(watch, projectNames) }
          ),
          Opts.subcommand("test", "test projects")(
            (watch, testProjectNames, testSuitesOnly, testSuitesExclude).mapN { case (watch, projectNames, testSuitesOnly, testSuitesExclude) =>
              commands.Test(watch, projectNames, testSuitesOnly, testSuitesExclude)
            }
          ),
          Opts.subcommand("list-tests", "list tests in projects")(
            testProjectNames.map { projectNames =>
              commands.ListTests(projectNames)
            }
          ),
          Opts.subcommand("run", "run project")(
            (
              projectName,
              mainClass,
              Opts.arguments[String]("arguments").map(_.toList).withDefault(List.empty),
              watch
            ).mapN { case (projectName, mainClass, arguments, watch) =>
              commands.Run(projectName, mainClass, arguments, raw = true, watch = watch)
            }
          ),
          Opts.subcommand("setup-ide", "generate ./bsp/bleep.json so IDEs can import build")(
            (projectNamesNoExpand, Opts.flag("force-jvm", "force BSP running through JVM").orFalse).mapN { case (projectNames, forceJvm) =>
              commands.SetupIde(projectNames, forceJvm)
            }
          ),
          Opts.subcommand("clean", "clean")(
            projectNames.map(projectNames => commands.Clean(projectNames))
          ),
          Opts.subcommand("projects", "show projects under current directory")(
            projectNames.map(projectNames => _ => Right(projectNames.map(_.value).sorted.foreach(started.logger.info(_))))
          ),
          Opts.subcommand("projects-test", "show test projects under current directory")(
            testProjectNames.map(projectNames => _ => Right(projectNames.map(_.value).sorted.foreach(started.logger.info(_))))
          ), {
            // edge case: import an sbt build in a folder underneath one where you have a bleep build
            val buildLoader = BuildLoader.nonExisting(started.buildPaths.cwd)
            val buildPaths = BuildPaths(started.buildPaths.cwd, buildLoader, model.BuildVariant.Normal)
            importCmd(buildLoader, started.userPaths, buildPaths, started.logger)
          },
          configCommand(started.pre.logger, started.pre.userPaths),
          installTabCompletions(started.userPaths, started.pre.logger),
          Opts.subcommand("publish-local", "publishes your project locally") {
            (
              Opts.option[String]("groupId", "organization you will publish under"),
              Opts.option[String]("version", "version you will publish"),
              Opts.option[Path]("to", s"publish to a maven repository at given path").orNone,
              projectNames,
              watch
            ).mapN { case (groupId, version, to, projects, watch) =>
              val publishTarget = to match {
                case Some(path) => commands.PublishLocal.CustomMaven(model.Repository.MavenFolder(name = None, path))
                case None       => commands.PublishLocal.LocalIvy
              }
              val options = commands.PublishLocal.Options(
                groupId = groupId,
                version = version,
                publishTarget = publishTarget,
                projects,
                ManifestCreator.default
              )
              commands.PublishLocal(watch, options)
            }
          },
          Opts.subcommand("dist", "creates a folder with a runnable distribution") {
            (
              projectName,
              mainClass,
              Opts.argument[Path]("path").orNone,
              watch
            ).mapN { case (projectName, mainClass, overridePath, watch) =>
              val options = commands.Dist.Options(
                projectName,
                overrideMain = mainClass,
                overridePath = overridePath
              )
              commands.Dist(started, watch, options)
            }
          },
          Opts.subcommand("fmt", "runs scalafmt") {
            Opts.flag("check", "ensure that all files are already formatted").orFalse.map(commands.Scalafmt.apply)
          },
          Opts.subcommand("setup-dev-script", "setup a bash script which can run the code bleep has compiled")(
            (projectName, Opts.option[String]("main-class", "override main class").orNone).mapN { case (projectNames, main) =>
              new commands.SetupDevScript(started, projectNames, main)
            }
          )
        ),
        started.build.scripts.map { case (scriptName, _) =>
          Opts.subcommand(scriptName.value, s"run script ${scriptName.value}")(
            (watch, stringArgs).mapN { case (watch, args) => commands.Script(scriptName, args, watch) }
          )
        }
      )

      commonOpts *> allCommands.flatten.foldK
    }

    ret
  }

  def configCommand(logger: Logger, userPaths: UserPaths): Opts[BleepCommand] =
    Opts.subcommand("config", "configure bleep here")(
      List(
        Opts.subcommand[BleepCommand]("file", "show configuration file location")(
          Opts(() => Right(logger.warn(s"Config file is found in ${userPaths.configYaml}")))
        ),
        Opts.subcommand[BleepCommand]("log-timing-enable", "enable timing info in logs")(
          Opts(() => BleepConfigOps.rewritePersisted(logger, userPaths)(_.copy(logTiming = Some(true))).map(_ => ()))
        ),
        Opts.subcommand[BleepCommand]("log-timing-disable", "disable timing info in logs")(
          Opts(() => BleepConfigOps.rewritePersisted(logger, userPaths)(_.copy(logTiming = Some(false))).map(_ => ()))
        ),
        Opts.subcommand(
          "compile-server",
          "You can speed up normal usage by keeping the bloop compile server running between invocations. This is where you control it"
        )(
          List(
            Opts.subcommand(
              "auto-shutdown-disable",
              "leave compile servers running between bleep invocations. this gets much better performance at the cost of memory"
            )(Opts {
              commands.CompileServerSetMode(logger, userPaths, model.CompileServerMode.Shared)
            }),
            Opts.subcommand("auto-shutdown-enable", "shuts down compile server after between bleep invocation. this is slower, but conserves memory")(Opts {
              commands.CompileServerSetMode(logger, userPaths, model.CompileServerMode.NewEachInvocation)
            }),
            Opts.subcommand("stop-all", "will stop all shared bloop compile servers")(
              Opts {
                commands.CompileServerStopAll(logger, userPaths)
              }
            )
          ).foldK
        )
      ).foldK
    )

  def importCmd(buildLoader: BuildLoader, userPaths: UserPaths, buildPaths: BuildPaths, logger: Logger): Opts[BleepCommand] =
    Opts.subcommand("import", "import existing build from files in .bloop")(
      sbtimport.ImportOptions.opts.map { opts =>
        val cacheLogger = new BleepCacheLogger(logger)
        val fetchJvm = new FetchJvm(Some(userPaths.resolveJvmCacheDir), cacheLogger, ec)

        val existingBuild = buildLoader.existing.flatMap(_.buildFile.forceGet).toOption

        commands.Import(existingBuild, sbtBuildDir = buildPaths.cwd, fetchJvm, buildPaths, logger, opts, model.BleepVersion.current)
      }
    )

  def installTabCompletions(userPaths: UserPaths, logger: Logger): Opts[BleepCommand] =
    List(
      Opts.subcommand("install-tab-completions-bash", "Install tab completions for bash")(
        Opts
          .flag("stdout", "send completion configuration to stdout")
          .orFalse
          .map { stdout =>
            commands.InstallBashTabCompletions(logger, stdout)
          }
      ),
      Opts.subcommand("install-tab-completions-zsh", "Install tab completions for zsh")(
        Opts
          .flag("stdout", "send completion configuration to stdout")
          .orFalse
          .map { stdout =>
            commands.InstallZshTabCompletions(userPaths, logger, stdout)
          }
      )
    ).foldK

  def newCommand(logger: Logger, userPaths: UserPaths, cwd: Path): Opts[BleepCommand] =
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
      ).mapN { case (platforms, scalas, name) =>
        commands.BuildCreateNew(logger, userPaths, cwd, platforms, scalas, name, model.BleepVersion.current, CoursierResolver.Factory.default)
      }
    )

  // there is a flag to change build directory. we need to intercept that very early
  def cwdFor(opts: CommonOpts): Path =
    opts.directory match {
      case Some(str) if str.startsWith("/") => Paths.get(str)
      case Some(str)                        => FileUtils.cwd / str
      case None                             => FileUtils.cwd
    }

  val ec: ExecutionContext = ExecutionContext.global

  def maybeRunWithDifferentVersion(args: Array[String], logger: Logger, buildLoader: BuildLoader, opts: CommonOpts): ExitCode =
    buildLoader match {
      case BuildLoader.NonExisting(_) => ExitCode.Success
      case existing: BuildLoader.Existing =>
        val correctPlatform = OsArch.current match {
          case OsArch.MacosArm64(freedFromJail) => !freedFromJail
          case _                                => true
        }

        def go(wantedVersion: model.BleepVersion): ExitCode = {
          val cacheLogger = new BleepCacheLogger(logger)

          OsArch.current match {
            case hasNativeImage: OsArch.HasNativeImage =>
              FetchBleepRelease(wantedVersion, cacheLogger, ec, hasNativeImage) match {
                case Left(buildException) =>
                  fatal("couldn't download bleep release", logger, buildException)
                case Right(binaryPath) if OsArch.current.os == model.Os.Windows || !isGraalvmNativeImage =>
                  val status = scala.sys.process.Process(binaryPath.toString :: args.toList, FileUtils.cwd.toFile, sys.env.toSeq: _*).!<
                  sys.exit(status)
                case Right(path) =>
                  Execve.execve(path.toString, path.toString +: args, sys.env.map { case (k, v) => s"$k=$v" }.toArray)
                  sys.error("should not be reached")
              }
            case other =>
              fatal(
                s"No native image available for $other, so the bleep launcher cannot run the release requested by the build file. " +
                  s"You can pass `--dev` to run the code you have and disregard the version wanted by the build. " +
                  s"Otherwise see https://github.com/oyvindberg/bleep/issues/260 for how you can help out",
                logger
              )
          }
        }

        existing.wantedVersion.forceGet match {
          case Right(wantedVersion) if (model.BleepVersion.current == wantedVersion && correctPlatform) || wantedVersion == model.BleepVersion.dev =>
            ExitCode.Success
          case Right(wantedVersion) if model.BleepVersion.current.isDevelopment =>
            logger.info(s"Not launching Bleep version ${wantedVersion.value} (from ${existing.bleepYaml}) because you're running a snapshot")
            ExitCode.Success
          case Right(wantedVersion) if opts.dev =>
            logger.info(s"Not launching Bleep version ${wantedVersion.value} (from ${existing.bleepYaml}) because you specified --dev")
            ExitCode.Success
          case Right(wantedVersion) if !correctPlatform =>
            logger.warn(
              s"Launching Bleep version ${wantedVersion.value} for ARM64 because you ran the AMD64 build. This is probably because you're running coursier compiled for AMD64. You can either try the coursier M1 runner at https://github.com/VirtusLab/coursier-m1 , or download and install bleep manually. Note that bleep will work this way, but startup will be slower."
            )
            go(wantedVersion)
          case Right(wantedVersion) =>
            logger.info(s"Launching Bleep version ${wantedVersion.value} as requested in ${existing.bleepYaml}")
            go(wantedVersion)
          case Left(throwable) =>
            fatal("Couldn't load build", logger, throwable)
        }
    }

  def _main(_args: Array[String]): ExitCode = {
    val userPaths = UserPaths.fromAppDirs

    val exitCode: ExitCode = _args.toList match {
      case "_complete-zsh" :: current :: words =>
        // accept -d after completion point
        val (commonOpts, _) = CommonOpts.parse(words)

        val newWords = words.drop(1 /* arg0 */ ).padTo(current.toInt - 1, "")
        withCompletions(_args, userPaths, commonOpts, newWords) { completions =>
          println(Zsh.print(completions.value))
          ExitCode.Success
        }

      case "_complete" :: compLine :: compCword :: compPoint :: _ =>
        val args = Completer.bashToArgs(compLine, compCword.toInt, compPoint.toInt)
        // accept -d after completion point
        val (commonOpts, _) = CommonOpts.parse(compLine.split(" ").toList)

        withCompletions(_args, userPaths, commonOpts, args) { completions =>
          completions.value.foreach(c => println(c.value))
          ExitCode.Success
        }

      case "selftest" :: Nil =>
        // checks that JNI libraries are successfully loaded
        val (commonOpts, _) = CommonOpts.parse(Nil)
        FileWatching(bleepLoggers.stderrWarn(commonOpts), Map(FileUtils.cwd -> List(())))(println(_)).run(FileWatching.StopWhen.Immediately)
        println("OK")
        ExitCode.Success

      case "bsp" :: args =>
        val (commonOpts, _) = CommonOpts.parse(args)
        val cwd = cwdFor(commonOpts)
        val buildLoader = BuildLoader.find(cwd)
        maybeRunWithDifferentVersion(_args, bleepLoggers.stderrAll(commonOpts), buildLoader, commonOpts).andThen {
          val buildPaths = BuildPaths(cwd, buildLoader, model.BuildVariant.BSP)
          val config = BleepConfigOps.loadOrDefault(userPaths).orThrow

          bleepLoggers.stderrAndFileLogging(config, commonOpts, buildPaths).use { logger =>
            buildLoader.existing.map(existing => Prebootstrapped(logger, userPaths, buildPaths, existing, ec)) match {
              case Left(be) => fatal("", logger, be)
              case Right(pre) =>
                try BspImpl.run(pre)
                catch {
                  case th: Throwable => fatal("uncaught error", logger, th)
                }
            }
          }
        }

      case args =>
        val (commonOpts, restArgs) = CommonOpts.parse(args)
        val cwd = cwdFor(commonOpts)

        val buildLoader = BuildLoader.find(cwd)
        val config = BleepConfigOps.loadOrDefault(userPaths).orThrow

        // initialize just stdout logger first to avoid creating log file if we're just booting a new version immediately
        val exitCode = bleepLoggers.stdoutNoLogFile(config, commonOpts).use { logger =>
          maybeRunWithDifferentVersion(_args, logger, buildLoader, commonOpts)
        }

        exitCode.andThen {
          val buildPaths = BuildPaths(cwd, buildLoader, model.BuildVariant.Normal)

          bleepLoggers.stdoutAndFileLogging(config, commonOpts, buildPaths).use { logger =>
            buildLoader match {
              case noBuild: BuildLoader.NonExisting =>
                run(noBuildOpts(logger, userPaths, buildPaths, noBuild), restArgs, logger)(_.run())
              case existing: BuildLoader.Existing =>
                val pre = Prebootstrapped(logger, userPaths, buildPaths, existing, ec)
                bootstrap.from(pre, GenBloopFiles.SyncToDisk, rewrites = Nil, config, CoursierResolver.Factory.default) match {
                  case Left(th)       => fatal("Error while loading build", logger, th)
                  case Right(started) => run(hasBuildOpts(started), restArgs, logger)(_.run(started))
                }
            }
          }
        }
    }
    exitCode
  }

  def main(args: Array[String]): Unit =
    System.exit(_main(args).value)

  def withCompletions(_args: Array[String], userPaths: UserPaths, commonOpts: CommonOpts, args: List[String])(f: Completer.Res => ExitCode): ExitCode = {
    val (_, restArgs) = CommonOpts.parse(args)
    val cwd = cwdFor(commonOpts)
    // we can not log to stdout when completing. logger should be used sparingly
    val stderr = bleepLoggers.stderrWarn(commonOpts)
    val buildLoader = BuildLoader.find(cwd)
    maybeRunWithDifferentVersion(_args, stderr, buildLoader, commonOpts).andThen {

      val buildPaths = BuildPaths(cwd, buildLoader, model.BuildVariant.Normal)

      val completions = buildLoader match {
        case noBuild: BuildLoader.NonExisting =>
          val completer = new Completer({
            case metavars.platformName => model.PlatformId.All.map(_.value)
            case metavars.scalaVersion => possibleScalaVersions.keys.toList
            case _                     => Nil
          })
          completer.completeOpts(restArgs)(noBuildOpts(stderr, userPaths, buildPaths, noBuild))
        case existing: BuildLoader.Existing =>
          val pre = Prebootstrapped(stderr, userPaths, buildPaths, existing, ec)

          val config = BleepConfigOps.loadOrDefault(pre.userPaths).orThrow

          bootstrap.from(pre, GenBloopFiles.SyncToDisk, rewrites = Nil, config, CoursierResolver.Factory.default) match {
            case Left(th) =>
              logException("couldn't load build", stderr, th)
              Completer.Res.NoMatch

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
      f(completions)
    }
  }

  private def run[Cmd](opts: Opts[Cmd], restArgs: List[String], logger: Logger)(runCommand: Cmd => Either[BleepException, Unit]): ExitCode =
    Command("bleep", s"Bleeping fast build! (version ${model.BleepVersion.current.value})")(opts).parse(restArgs, sys.env) match {
      case Left(help) =>
        help.errors match {
          case List() =>
            System.out.println(help)
            ExitCode.Success
          case _ =>
            System.err.println(help)
            ExitCode.Failure
        }
      case Right(cmd) =>
        Try(runCommand(cmd)) match {
          case Failure(th)        => fatal("command failed unexpectedly! This really shouldn't happen. Please report.", logger, th)
          case Success(Left(th))  => fatal("command failed", logger, th)
          case Success(Right(())) => ExitCode.Success
        }
    }
}
