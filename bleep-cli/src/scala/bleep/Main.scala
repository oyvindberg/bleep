package bleep

import bleep.internal.{bleepLoggers, fatal, logException, BspClientDisplayProgress, FileUtils}
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

  // Install SIGINFO handler on macOS (Ctrl+T) for thread dumps
  if (Properties.isMac) {
    try
      sun.misc.Signal.handle(new sun.misc.Signal("INFO"), (_: sun.misc.Signal) => internal.ChildProcessDiagnostics.dumpAll(System.err))
    catch {
      case _: Exception => // Signal handling not available
    }
  }

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
    val hasSourceGenProject = "project name (with sourcegen)"
    val platformName = "platform name"
    val scalaVersion = "scala version"
    val projectOrScriptName = "project or script name"
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

  /** Output mode for request/response commands. Defaults to text. */
  val outputMode: Opts[OutputMode] =
    Opts
      .option[String]("output", "output format: text or json", "o")
      .withDefault("text")
      .map {
        case "json" => OutputMode.Json
        case _      => OutputMode.Text
      }

  /** Logging opts parsed by decline (not manually pre-parsed). Used in the build command path. */
  val loggingOpts: Opts[LoggingOpts] =
    (
      Opts.flag("no-color", "enable CI-friendly output").orFalse,
      Opts.flag("debug", "enable more output").orFalse,
      Opts.flag("no-bsp-progress", "don't show compilation progress. good for CI").orFalse,
      Opts.flag("log-as-json", "bleep internal: for running bleep scripts").orFalse.map(_ || LoggingOpts.defaultLogAsJson)
    ).mapN(LoggingOpts.apply)

  def noBuildOpts(logger: Logger, userPaths: UserPaths, buildPaths: BuildPaths, buildLoader: BuildLoader.NonExisting): Opts[BleepNoBuildCommand] =
    commonOpts *> List(
      Opts.subcommand("build", "create new build")(newCommand(logger, userPaths, buildPaths.cwd)),
      importCmd(buildLoader, userPaths, buildPaths, logger),
      importMavenCmd(buildPaths, logger),
      configCommand(logger, userPaths),
      installTabCompletions(userPaths, logger),
      Opts.subcommand("server-metrics", "open BSP server metrics dashboard in browser")(
        Opts.argument[Long]("pid").orNone.map(pid => commands.ServerMetrics(logger, userPaths, pid))
      )
    ).foldK

  def argumentFrom[A](defmeta: String, nameToValue: Option[Map[String, A]]): Argument[A] =
    Argument.fromMap(defmeta, nameToValue.getOrElse(Map.empty))

  def hasBuildOpts(started: Started): Opts[(LoggingOpts, BleepBuildCommand)] = {
    val projectNamesNoCross: Opts[NonEmptyList[model.ProjectName]] =
      Opts
        .arguments(metavars.projectNameNoCross)(argumentFrom(metavars.projectNameNoCross, Some(started.globs.projectNamesNoCrossMap)))

    val projectNames: Opts[Array[model.CrossProjectName]] =
      Opts
        .arguments(metavars.projectName)(argumentFrom(metavars.projectName, Some(started.globs.projectNameMap)))
        .map(_.toList.toArray.flatten)
        .orNone
        .map(started.chosenProjects)

    /** Like projectNames but returns empty array when nothing specified, so publish commands can auto-discover publishable projects. */
    val publishProjectNames: Opts[Array[model.CrossProjectName]] =
      Opts
        .arguments(metavars.projectName)(argumentFrom(metavars.projectName, Some(started.globs.projectNameMap)))
        .map(_.toList.toArray.flatten)
        .orNone
        .map(_.getOrElse(Array.empty))

    val projectNameNoCross: Opts[model.ProjectName] =
      Opts.argument(metavars.projectNameNoCross)(argumentFrom(metavars.projectNameNoCross, Some(started.globs.projectNamesNoCrossMap)))

    val projectName: Opts[model.CrossProjectName] =
      Opts.argument(metavars.projectNameExact)(argumentFrom(metavars.projectNameExact, Some(started.globs.exactProjectMap)))

    val projectNamesNoExpand: Opts[Option[List[String]]] =
      Opts
        .arguments(metavars.projectName)(argumentFrom(metavars.projectName, Some(started.globs.projectNameMap).map(_.map { case (s, _) => (s, s) })))
        .map(_.toList)
        .orNone

    /** Accepts all project names. When none given, defaults to test projects only. Non-test projects will just be compiled. */
    val testProjectNames: Opts[Array[model.CrossProjectName]] =
      Opts
        .arguments(metavars.projectName)(argumentFrom(metavars.projectName, Some(started.globs.projectNameMap)))
        .map(_.toList.toArray.flatten)
        .orNone
        .map {
          case Some(explicit) => explicit
          case None           => started.chosenTestProjects(None)
        }

    val only: Opts[Option[NonEmptyList[String]]] =
      Opts
        .options[String](
          "only",
          "Test only a subset of test suite class names. Class name can be fully qualified to disambiguate",
          "o"
        )
        .orNone
    val exclude: Opts[Option[NonEmptyList[String]]] =
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

    val cancel = Opts.flag("cancel", "cancel any running build before starting").orFalse

    val commonBuildOpts: Opts[commands.CommonBuildOpts] = (
      (
        Opts.flag("no-tui", "disable TUI, show summary only (for CI/agents)").orFalse,
        Opts.flag("quiet", "alias for --no-tui", "q").orFalse
      ).mapN(_ || _),
      Opts.flag("flamegraph", "generate execution trace (open in chrome://tracing or ui.perfetto.dev)").orFalse,
      cancel
    ).mapN { case (noTui, flamegraph, cancel) =>
      commands.CommonBuildOpts(
        displayMode = commands.DisplayMode.fromFlags(noTui),
        flamegraph = flamegraph,
        cancel = cancel
      )
    }

    val isReleaseMode = Opts.flag("release", "force linking in release mode", "r").orFalse

    // Link options
    val sourceMapsOpt: Opts[Option[Boolean]] =
      Opts
        .flag("source-maps", "enable source maps (JS platforms)")
        .map(_ => true)
        .orElse(Opts.flag("no-source-maps", "disable source maps (JS platforms)").map(_ => false))
        .orNone

    val minifyOpt: Opts[Option[Boolean]] =
      Opts
        .flag("minify", "enable minification (Scala.js)")
        .map(_ => true)
        .orElse(Opts.flag("no-minify", "disable minification (Scala.js)").map(_ => false))
        .orNone

    val moduleKindOpt: Opts[Option[commands.LinkOptions.ModuleKind]] = Opts
      .option[String]("module-kind", "JS module kind: commonjs, esmodule, nomodule")
      .mapValidated { s =>
        commands.LinkOptions.ModuleKind.fromString(s) match {
          case Some(mk) => cats.data.Validated.valid(mk)
          case None     => cats.data.Validated.invalidNel(s"Invalid module kind: $s. Valid values: commonjs, esmodule, nomodule")
        }
      }
      .orNone

    val ltoOpt: Opts[Option[commands.LinkOptions.LTO]] = Opts
      .option[String]("lto", "Link-time optimization (Scala Native): none, thin, full")
      .mapValidated { s =>
        commands.LinkOptions.LTO.fromString(s) match {
          case Some(l) => cats.data.Validated.valid(l)
          case None    => cats.data.Validated.invalidNel(s"Invalid LTO: $s. Valid values: none, thin, full")
        }
      }
      .orNone

    val optimizeOpt: Opts[Option[Boolean]] =
      Opts
        .flag("optimize", "enable optimizations/DCE (all non-JVM platforms)")
        .map(_ => true)
        .orElse(Opts.flag("no-optimize", "disable optimizations/DCE (all non-JVM platforms)").map(_ => false))
        .orNone

    val debugInfoOpt: Opts[Option[Boolean]] =
      Opts
        .flag("debug-info", "include debug info (native platforms)")
        .map(_ => true)
        .orElse(Opts.flag("no-debug-info", "exclude debug info (native platforms)").map(_ => false))
        .orNone

    val updateAsScalaSteward = Opts
      .flag(
        "steward",
        "Use same upgrade strategy as Scala Steward. Updates to the latest patch version at the same minor and major version. If the dependency is already on the latest patch version, it updates to the latest minor version at the same major version. And if the dependency is already on the latest minor version, it updates to the latest major version."
      )
      .orFalse

    val updateWithPrerelease = Opts.flag("prerelease", "Allow upgrading to prerelease version if there is any.").orFalse

    val updateSingleOrgOrModule = Opts.argument[String]("The dependency to update, alternatively only the organization name can be passed")

    lazy val ret: Opts[(LoggingOpts, BleepBuildCommand)] = {
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
              Opts.subcommand("diff", "diff effective project config compared to git HEAD or wanted revision")(
                Opts.subcommand("effective", "show projects after applying templates")(
                  (projectNames, commands.BuildDiff.opts, outputMode).mapN { case (names, opts, mode) =>
                    commands.BuildDiff(opts, names, mode)
                  }
                )
              ),
              Opts.subcommand("show", "show projects in their different versions.")(
                List(
                  Opts.subcommand("short", "the projects as you wrote it in YAML")(
                    (projectNamesNoCross, outputMode).mapN(commands.BuildShow.Short.apply)
                  ),
                  Opts.subcommand("effective", "the final project configuration after all templates have been applied")(
                    (projectNames, outputMode).mapN(commands.BuildShow.Effective.apply)
                  )
                ).foldK
              ),
              Opts.subcommand("evicted", "show eviction warnings for project")(
                (projectNames, outputMode).mapN((projectNames, mode) => commands.Evicted(projectNames, mode))
              ),
              Opts.subcommand("invalidated", "compute which projects need rebuild/retest vs a base git commit")(
                commands.BuildInvalidated.opts
              ),
              newCommand(started.pre.logger, started.pre.userPaths, started.buildPaths.cwd)
            ).foldK
          ),
          Opts.subcommand("compile", "compile projects")(
            (
              watch,
              projectNames,
              (
                Opts.flag("no-tui", "disable TUI, show summary only (for CI/agents)").orFalse,
                Opts.flag("quiet", "alias for --no-tui", "q").orFalse
              ).mapN(_ || _),
              Opts.flag("diff-watch", "watch mode with per-project diffs between cycles").orFalse,
              Opts.flag("flamegraph", "generate execution trace (open in chrome://tracing or ui.perfetto.dev)").orFalse,
              cancel
            ).mapN { case (watch, projectNames, noTui, diffWatch, flamegraph, cancel) =>
              val (effectiveWatch, effectiveDisplayMode) =
                if (diffWatch) (true, commands.DisplayMode.DiffWatch)
                else (watch, commands.DisplayMode.fromFlags(noTui))
              commands.ReactiveBsp.compile(effectiveWatch, projectNames, effectiveDisplayMode, flamegraph, cancel)
            }
          ),
          Opts.subcommand("link", "link projects")(
            (
              watch,
              projectNames,
              isReleaseMode,
              sourceMapsOpt,
              minifyOpt,
              moduleKindOpt,
              ltoOpt,
              optimizeOpt,
              debugInfoOpt,
              (
                Opts.flag("no-tui", "disable TUI, show summary only (for CI/agents)").orFalse,
                Opts.flag("quiet", "alias for --no-tui", "q").orFalse
              ).mapN(_ || _),
              Opts.flag("flamegraph", "generate execution trace (open in chrome://tracing or ui.perfetto.dev)").orFalse,
              cancel
            ).mapN { case (watch, projectNames, release, sourceMaps, minify, moduleKind, lto, optimize, debugInfo, noTui, flamegraph, cancel) =>
              val linkOptions = commands.LinkOptions(
                releaseMode = release,
                sourceMaps = sourceMaps,
                minify = minify,
                moduleKind = moduleKind,
                lto = lto,
                optimize = optimize,
                debugInfo = debugInfo
              )
              commands.ReactiveBsp.link(watch, projectNames, commands.DisplayMode.fromFlags(noTui), linkOptions, flamegraph, cancel)
            }
          ),
          Opts.subcommand("sourcegen", "run source generators for projects")(
            (watch, hasSourcegenProjectNames).mapN { case (watch, projectNames) => commands.SourceGen(watch, projectNames) }
          ),
          Opts.subcommand("test", "test projects")(
            (
              watch,
              testProjectNames,
              // Multiple aliases for disabling TUI - for different use cases
              (
                Opts.flag("no-tui", "disable TUI, show summary only (for CI/agents)").orFalse,
                Opts.flag("quiet", "alias for --no-tui", "q").orFalse,
                Opts.flag("summary-only", "alias for --no-tui").orFalse
              ).mapN(_ || _ || _),
              Opts.flag("diff-watch", "watch mode with per-project diffs between cycles").orFalse,
              Opts.options[String]("jvm-opt", "JVM options for forked test processes").orEmpty,
              Opts.options[String]("test-arg", "arguments passed to test framework").orEmpty,
              only,
              exclude,
              Opts.flag("flamegraph", "generate execution trace (open in chrome://tracing or ui.perfetto.dev)").orFalse,
              cancel,
              Opts.option[String]("junit-report", "write JUnit XML reports to this directory").orNone
            ).mapN { case (watch, projectNames, noTui, diffWatch, jvmOpts, testArgs, only, exclude, flamegraph, cancel, junitReportDir) =>
              val (effectiveWatch, effectiveDisplayMode) =
                if (diffWatch) (true, commands.DisplayMode.DiffWatch)
                else (watch, commands.DisplayMode.fromFlags(noTui))
              commands.ReactiveBsp.test(
                watch = effectiveWatch,
                projects = projectNames,
                displayMode = effectiveDisplayMode,
                jvmOptions = jvmOpts.toList,
                testArgs = testArgs.toList,
                only = only.map(_.toList).getOrElse(Nil),
                exclude = exclude.map(_.toList).getOrElse(Nil),
                flamegraph = flamegraph,
                cancel = cancel,
                junitReportDir = junitReportDir.map(java.nio.file.Paths.get(_))
              )
            }
          ),
          Opts.subcommand("list-tests", "list tests in projects")(
            (testProjectNames, outputMode).mapN { (projectNames, mode) =>
              commands.ListTests(projectNames, mode)
            }
          ),
          Opts.subcommand("run", "run project or script")(
            (
              Opts.argument[String](metavars.projectOrScriptName)(
                Argument.fromMap(
                  metavars.projectOrScriptName,
                  started.globs.exactProjectMap.map { case (k, _) => k -> k } ++
                    started.build.scripts.keys.map(s => s.value -> s.value)
                )
              ),
              mainClass,
              Opts.arguments[String]("arguments").map(_.toList).withDefault(List.empty),
              watch,
              commonBuildOpts
            ).mapN { case (nameArg, mainClass, arguments, watch, buildOpts) =>
              // Check if the argument is a script name first
              started.build.scripts.keys.find(_.value == nameArg) match {
                case Some(scriptName) =>
                  // Run as script
                  commands.Script(scriptName, arguments, watch)
                case None =>
                  // Run as project
                  started.globs.exactProjectMap.get(nameArg) match {
                    case Some(projectName) =>
                      commands.Run(projectName, mainClass, arguments, raw = true, watch = watch, buildOpts = buildOpts)
                    case None =>
                      // Return a command that will fail with a helpful error
                      new BleepBuildCommand {
                        override def run(started: Started): Either[BleepException, Unit] =
                          Left(new BleepException.Text(s"'$nameArg' is not a valid project or script name"))
                      }
                  }
              }
            }
          ),
          Opts.subcommand("setup-ide", "generate ./bsp/bleep.json so IDEs can import build")(
            (projectNamesNoExpand, Opts.flag("force-jvm", "force BSP running through JVM").orFalse).mapN { case (projectNames, forceJvm) =>
              commands.SetupIde(projectNames, forceJvm)
            }
          ),
          Opts.subcommand("setup-mcp-server", "generate .mcp.json so AI agents (Claude Code, etc.) can compile and test")(
            Opts.flag("force-jvm", "force MCP server running through JVM").orFalse.map { forceJvm =>
              commands.SetupMcpServer(forceJvm)
            }
          ),
          Opts.subcommand("clean", "clean")(
            projectNames.map(projectNames => commands.Clean(projectNames))
          ),
          Opts.subcommand("projects", "show projects under current directory")(
            (projectNames, outputMode).mapN { (projectNames, mode) =>
              new BleepBuildCommand {
                override def run(started: Started): Either[BleepException, Unit] = {
                  val sorted = projectNames.map(_.value).sorted.toList
                  mode match {
                    case OutputMode.Text => sorted.foreach(started.logger.info(_))
                    case OutputMode.Json => CommandResult.print(CommandResult.success(ProjectList(sorted)))
                  }
                  Right(())
                }
              }
            }
          ),
          Opts.subcommand("projects-test", "show test projects under current directory")(
            (testProjectNames, outputMode).mapN { (projectNames, mode) =>
              new BleepBuildCommand {
                override def run(started: Started): Either[BleepException, Unit] = {
                  val sorted = projectNames.map(_.value).sorted.toList
                  mode match {
                    case OutputMode.Text => sorted.foreach(started.logger.info(_))
                    case OutputMode.Json => CommandResult.print(CommandResult.success(ProjectList(sorted)))
                  }
                  Right(())
                }
              }
            }
          ),
          Opts.subcommand("extract-info", "JSON output for IDE integration")(
            List(
              Opts.subcommand("all", "output all info in a single JSON object")(
                Opts(commands.ExtractInfo.All)
              ),
              Opts.subcommand("project-graph", "output all projects with their dependencies")(
                Opts(commands.ExtractInfo.ProjectGraph)
              ),
              Opts.subcommand("project-groups", "output project groups for bulk selection")(
                Opts(commands.ExtractInfo.ProjectGroups)
              ),
              Opts.subcommand("scripts", "output all scripts with project and main class")(
                Opts(commands.ExtractInfo.Scripts)
              ),
              Opts.subcommand("sourcegen", "output all sourcegen definitions")(
                Opts(commands.ExtractInfo.SourceGen)
              )
            ).foldK
          ), {
            // edge case: import an sbt build in a folder underneath one where you have a bleep build
            val buildLoader = BuildLoader.nonExisting(started.buildPaths.cwd)
            val buildPaths = BuildPaths(started.buildPaths.cwd, buildLoader, model.BuildVariant.Normal)
            importCmd(buildLoader, started.userPaths, buildPaths, started.logger)
          }, {
            val buildPaths0 = BuildPaths(started.buildPaths.cwd, BuildLoader.nonExisting(started.buildPaths.cwd), model.BuildVariant.Normal)
            importMavenCmd(buildPaths0, started.logger)
          },
          configCommand(started.pre.logger, started.pre.userPaths),
          installTabCompletions(started.userPaths, started.pre.logger),
          Opts.subcommand("server-metrics", "open BSP server metrics dashboard in browser")(
            Opts.argument[Long]("pid").orNone.map(pid => commands.ServerMetrics(started.pre.logger, started.pre.userPaths, pid))
          ),
          Opts.subcommand("publish-local", "publishes your project locally (deprecated: use 'publish local')") {
            (
              Opts.option[String]("groupId", "organization you will publish under"),
              Opts.option[String]("version", "version you will publish"),
              Opts.option[Path]("to", s"publish to a maven repository at given path").orNone,
              projectNames,
              watch,
              commonBuildOpts
            ).mapN { case (groupId, version, to, projects, watch, buildOpts) =>
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
              commands.PublishLocal(watch, options, buildOpts)
            }
          },
          Opts.subcommand("publish", "publish artifacts to a named resolver, local-ivy, or sonatype") {
            val dynVerFallback: () => String = () => new bleep.plugin.dynver.DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile).version

            def publishOpts(target: commands.Publish.Target): Opts[BleepBuildCommand] =
              (
                Opts.option[String]("version", "version to publish (default: from git tags)").orNone,
                Opts.flag("assert-release", "fail if git state would produce a snapshot version").orFalse,
                Opts.flag("dry-run", "show what would be published without uploading").orFalse,
                publishProjectNames,
                commonBuildOpts
              ).mapN { case (version, assertRel, dryRun, projects, buildOpts) =>
                commands.Publish(
                  false,
                  commands.Publish.Options(version, Some(dynVerFallback), assertRel, dryRun, target, projects, ManifestCreator.default),
                  buildOpts
                )
              }

            val builtinSubcommands: List[Opts[BleepBuildCommand]] = List(
              Opts.subcommand("local-ivy", "publish to local ivy2 cache")(publishOpts(commands.Publish.Target.LocalIvy)),
              Opts.subcommand("sonatype", "publish to Sonatype / Maven Central (with GPG signing)") {
                (
                  Opts.option[String]("version", "version to publish (default: from git tags)").orNone,
                  Opts.flag("assert-release", "fail if git state would produce a snapshot version").orFalse,
                  publishProjectNames,
                  commonBuildOpts
                ).mapN { case (version, assertRel, projects, buildOpts) =>
                  commands.PublishSonatype(
                    commands.PublishSonatype.Options(version, assertRel, projects, ManifestCreator.default),
                    buildOpts
                  )
                }
              },
              Opts.subcommand[BleepBuildCommand]("setup", "interactive setup wizard for publishing")(
                Opts(commands.PublishSetup(started.pre.logger, started.pre.userPaths, Some(started)))
              )
            )

            // Dynamic subcommands from named resolvers in bleep.yaml
            val resolverSubcommands: List[Opts[BleepBuildCommand]] =
              started.build.resolvers.values
                .flatMap(_.name)
                .filterNot(n => commands.Publish.ReservedNames.contains(n.value))
                .map { resolverName =>
                  Opts.subcommand(resolverName.value, s"publish to resolver '${resolverName.value}'")(
                    publishOpts(commands.Publish.Target.Resolver(resolverName))
                  )
                }

            (builtinSubcommands ++ resolverSubcommands).foldK
          },
          Opts.subcommand("dist", "creates a folder with a runnable distribution") {
            (
              projectName,
              mainClass,
              Opts.argument[Path]("path").orNone,
              watch,
              commonBuildOpts
            ).mapN { case (projectName, mainClass, overridePath, watch, buildOpts) =>
              val options = commands.Dist.Options(
                projectName,
                overrideMain = mainClass,
                overridePath = overridePath
              )
              commands.Dist(watch, options, buildOpts)
            }
          },
          Opts.subcommand("fmt", "format Scala and Java source files") {
            (Opts.flag("check", "ensure that all files are already formatted").orFalse, projectNames).mapN { case (check, projects) =>
              commands.Fmt(check, projects)
            }
          },
          Opts.subcommand("remote-cache", "push and pull compiled classes to/from a remote S3-compatible cache")(
            List(
              Opts.subcommand("pull", "pull cached compiled classes from remote cache")(
                projectNames.map(names => commands.RemoteCache.Pull(names))
              ),
              Opts.subcommand("push", "push compiled classes to remote cache")(
                (projectNames, Opts.flag("force", "overwrite existing cache entries").orFalse).mapN(commands.RemoteCache.Push.apply)
              )
            ).foldK
          ),
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

      (loggingOpts, allCommands.flatten.foldK).tupled
    }

    ret
  }

  private def updateBspServerConfig(f: model.BspServerConfig => model.BspServerConfig)(config: model.BleepConfig): model.BleepConfig =
    config.copy(bspServerConfig = Some(f(config.bspServerConfigOrDefault)))

  def configCommand(logger: Logger, userPaths: UserPaths): Opts[BleepCommand] =
    Opts.subcommand("config", "configure bleep here")(
      List(
        Opts.subcommand[BleepCommand]("file", "show configuration file location")(
          outputMode.map { mode => () =>
            mode match {
              case OutputMode.Text =>
                logger.warn(s"Config file is found in ${userPaths.configYaml}")
                Right(())
              case OutputMode.Json =>
                CommandResult.print(CommandResult.success(ConfigLocation(userPaths.configYaml.toString)))
                Right(())
            }
          }
        ),
        Opts.subcommand[BleepCommand]("log-timing-enable", "enable timing info in logs")(
          Opts(() => BleepConfigOps.rewritePersisted(logger, userPaths)(_.copy(logTiming = Some(true))).map(_ => ()))
        ),
        Opts.subcommand[BleepCommand]("log-timing-disable", "disable timing info in logs")(
          Opts(() => BleepConfigOps.rewritePersisted(logger, userPaths)(_.copy(logTiming = Some(false))).map(_ => ()))
        ),
        Opts.subcommand(
          "compile-server",
          "You can speed up normal usage by keeping the compile server running between invocations. This is where you control it"
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
            Opts.subcommand("stop-all", "will stop all shared compile servers")(
              Opts {
                commands.CompileServerStopAll(logger, userPaths)
              }
            ),
            Opts.subcommand[BleepCommand]("max-memory", "set max heap for compile server JVM (e.g. 4g, 2048m)")(
              Opts.argument[String]("size").map { size => () =>
                BleepConfigOps.rewritePersisted(logger, userPaths)(updateBspServerConfig(_.copy(compileServerMaxMemory = Some(size)))).map(_ => ())
              }
            ),
            Opts.subcommand[BleepCommand]("max-memory-clear", "remove compile server max heap setting (use JVM default)")(
              Opts(() => BleepConfigOps.rewritePersisted(logger, userPaths)(updateBspServerConfig(_.copy(compileServerMaxMemory = None))).map(_ => ()))
            ),
            Opts.subcommand[BleepCommand](
              "heap-pressure-threshold",
              "set heap usage fraction (0.0-1.0) above which new compilations wait for memory (default: 0.80)"
            )(
              Opts.argument[Double]("threshold").map { threshold => () =>
                BleepConfigOps
                  .rewritePersisted(logger, userPaths)(updateBspServerConfig(_.copy(heapPressureThreshold = Some(threshold))))
                  .map(_ => ())
              }
            ),
            Opts.subcommand[BleepCommand]("heap-pressure-threshold-clear", "remove heap pressure threshold setting (use default: 0.80)")(
              Opts(() => BleepConfigOps.rewritePersisted(logger, userPaths)(updateBspServerConfig(_.copy(heapPressureThreshold = None))).map(_ => ()))
            )
          ).foldK
        ),
        Opts.subcommand("test-runner", "configure test runner JVM settings")(
          List(
            Opts.subcommand[BleepCommand]("max-memory", "set max heap for test runner JVMs (e.g. 512m, 2g)")(
              Opts.argument[String]("size").map { size => () =>
                BleepConfigOps.rewritePersisted(logger, userPaths)(updateBspServerConfig(_.copy(testRunnerMaxMemory = Some(size)))).map(_ => ())
              }
            ),
            Opts.subcommand[BleepCommand]("max-memory-clear", "remove test runner max heap setting (use JVM default)")(
              Opts(() => BleepConfigOps.rewritePersisted(logger, userPaths)(updateBspServerConfig(_.copy(testRunnerMaxMemory = None))).map(_ => ()))
            )
          ).foldK
        ),
        Opts.subcommand("auth", "configure authentication for private repositories (Artifact Registry, GitHub Packages, GitLab)")(
          List(
            Opts.subcommand[BleepCommand]("setup", "interactive setup for private repository authentication")(
              Opts(() => commands.PublishSetup(logger, userPaths, None).run())
            ),
            Opts.subcommand[BleepCommand]("list", "list configured authentications")(
              outputMode.map(mode => () => commands.ConfigAuthList(logger, userPaths, mode).run())
            ),
            Opts.subcommand[BleepCommand]("remove", "remove authentication for a repository URI prefix")(
              Opts.argument[String]("uri-prefix").map { uriStr => () =>
                commands.ConfigAuthRemove(logger, userPaths, java.net.URI.create(uriStr)).run()
              }
            )
          ).foldK
        ),
        Opts.subcommand("remote-cache", "configure remote build cache credentials")(
          List(
            Opts.subcommand[BleepCommand]("setup", "interactive setup for S3 remote cache credentials")(
              Opts(commands.ConfigRemoteCacheSetup(logger, userPaths))
            ),
            Opts.subcommand[BleepCommand]("clear", "remove remote cache credentials from config")(
              Opts(() => BleepConfigOps.rewritePersisted(logger, userPaths)(_.copy(remoteCacheCredentials = None)).map(_ => ()))
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

  def importMavenCmd(buildPaths: BuildPaths, logger: Logger): Opts[BleepCommand] =
    Opts.subcommand("import-maven", "import existing Maven build from pom.xml")(
      mavenimport.MavenImportOptions.opts.map { opts =>
        commands.ImportMaven(mavenBuildDir = buildPaths.cwd, buildPaths, logger, opts, model.BleepVersion.current)
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
  def cwdFor(directory: Option[String]): Path =
    directory match {
      case Some(str) if str.startsWith("/") => Paths.get(str)
      case Some(str)                        => FileUtils.cwd / str
      case None                             => FileUtils.cwd
    }

  val ec: ExecutionContext = ExecutionContext.global

  def maybeRunWithDifferentVersion(args: Array[String], logger: Logger, buildLoader: BuildLoader, dev: Boolean): ExitCode =
    buildLoader match {
      case BuildLoader.NonExisting(_)     => ExitCode.Success
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
          case Right(wantedVersion) if dev =>
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

    // Enable ANSI support on Windows
    try
      io.github.alexarchambault.nativeterm.NativeTerminal.setupAnsi()
    catch {
      case _: Exception => // Ignore any failures during initialization
    }

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
        val logger = bleepLoggers.stderrWarn(commonOpts)
        FileWatching(logger, Map(FileUtils.cwd -> List(())))(println(_)).run(FileWatching.StopWhen.Immediately)
        // verify TerminalSizeCache initialization works (SIGWINCH doesn't exist on Windows)
        BspClientDisplayProgress(logger).discard()
        println("OK")
        ExitCode.Success

      case "bsp" :: args =>
        val (commonOpts, _) = CommonOpts.parse(args)
        val cwd = cwdFor(commonOpts.directory)
        val buildLoader = BuildLoader.find(cwd)
        maybeRunWithDifferentVersion(_args, bleepLoggers.stderrAll(commonOpts), buildLoader, commonOpts.dev).andThen {
          val buildPaths = BuildPaths(cwd, buildLoader, model.BuildVariant.BSP)
          val config = BleepConfigOps.loadOrDefault(userPaths).orThrow

          bleepLoggers.stderrAndFileLogging(config, commonOpts, buildPaths).use { logger =>
            buildLoader.existing.map(existing => Prebootstrapped(logger, userPaths, buildPaths, existing, ec)) match {
              case Left(be)   => fatal("", logger, be)
              case Right(pre) =>
                bsp.BspProxy.run(pre)
            }
          }
        }

      case "mcp-server" :: args =>
        val (commonOpts, _) = CommonOpts.parse(args)
        val cwd = cwdFor(commonOpts.directory)
        val buildLoader = BuildLoader.find(cwd)
        maybeRunWithDifferentVersion(_args, bleepLoggers.stderrAll(commonOpts), buildLoader, commonOpts.dev).andThen {
          val buildPaths = BuildPaths(cwd, buildLoader, model.BuildVariant.Normal)
          val config = BleepConfigOps.loadOrDefault(userPaths).orThrow

          bleepLoggers.stderrAndFileLogging(config, commonOpts, buildPaths).use { logger =>
            buildLoader.existing.map(existing => Prebootstrapped(logger, userPaths, buildPaths, existing, ec)) match {
              case Left(be)   => fatal("", logger, be)
              case Right(pre) =>
                mcp.McpServerRunner.run(pre)
            }
          }
        }

      case args =>
        val (preOpts, restArgs) = PreBootstrapOpts.parse(args)
        val cwd = cwdFor(preOpts.directory)

        val buildLoader = BuildLoader.find(cwd)
        val config = BleepConfigOps.loadOrDefault(userPaths).orThrow

        // Use storing logger during version check + bootstrap. Real logger created after decline parsing.
        val storingLogger = bleepLoggers.silent

        val exitCode = maybeRunWithDifferentVersion(_args, storingLogger, buildLoader, preOpts.dev)

        exitCode.andThen {
          val buildPaths = BuildPaths(cwd, buildLoader, model.BuildVariant.Normal)

          buildLoader match {
            case noBuild: BuildLoader.NonExisting =>
              // No-build path: use CommonOpts (old flow) since noBuildOpts commands capture logger at construction
              val commonOpts = CommonOpts.parse(args)._1
              bleepLoggers.stdoutAndFileLogging(config, commonOpts, buildPaths).use { logger =>
                run(noBuildOpts(logger, userPaths, buildPaths, noBuild), restArgs, logger)(_.run())
              }
            case existing: BuildLoader.Existing =>
              val pre = Prebootstrapped(storingLogger, userPaths, buildPaths, existing, ec)
              bootstrap.from(pre, ResolveProjects.InMemory, rewrites = Nil, config, CoursierResolver.Factory.default) match {
                case Left(th) =>
                  // Bootstrap failed — create fallback logger for error output
                  val fallbackOpts = CommonOpts(
                    noColor = false,
                    debug = false,
                    directory = preOpts.directory,
                    dev = preOpts.dev,
                    noBspProgress = false,
                    logAsJson = LoggingOpts.defaultLogAsJson
                  )
                  bleepLoggers.stdoutAndFileLogging(config, fallbackOpts, buildPaths).use { logger =>
                    replayStoredMessages(storingLogger, logger)
                    fatal("Error while loading build", logger, th)
                  }
                case Right(started) =>
                  runBuildCommand(hasBuildOpts(started), restArgs, preOpts, storingLogger, config, buildPaths, started)
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
    val cwd = cwdFor(commonOpts.directory)
    // we can not log to stdout when completing. logger should be used sparingly
    val stderr = bleepLoggers.stderrWarn(commonOpts)
    val buildLoader = BuildLoader.find(cwd)
    maybeRunWithDifferentVersion(_args, stderr, buildLoader, commonOpts.dev).andThen {

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

          bootstrap.from(pre, ResolveProjects.InMemory, rewrites = Nil, config, CoursierResolver.Factory.default) match {
            case Left(th) =>
              logException("couldn't load build", stderr, th)
              Completer.Res.NoMatch

            case Right(started) =>
              val completer = new Completer({
                case metavars.platformName        => model.PlatformId.All.map(_.value)
                case metavars.scalaVersion        => possibleScalaVersions.keys.toList
                case metavars.projectNameExact    => started.globs.exactProjectMap.keys.toList
                case metavars.projectNameNoCross  => started.globs.projectNamesNoCrossMap.keys.toList
                case metavars.projectName         => started.globs.projectNameMap.keys.toList
                case metavars.projectOrScriptName => started.globs.exactProjectMap.keys.toList ++ started.build.scripts.keys.map(_.value)
                case _                            => Nil
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

  /** Parse a build command with decline, create the real logger from LoggingOpts, replay stored bootstrap messages, then run. */
  private def runBuildCommand(
      opts: Opts[(LoggingOpts, BleepBuildCommand)],
      restArgs: List[String],
      preOpts: PreBootstrapOpts,
      storingLogger: Logger,
      config: model.BleepConfig,
      buildPaths: BuildPaths,
      started: Started
  ): ExitCode =
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
      case Right((loggingOpts, cmd)) =>
        val commonOpts = loggingOpts.toCommonOpts(preOpts)
        // JSON output mode: route logging to stderr so stdout is clean JSON
        val jsonOutput = hasJsonOutput(restArgs)
        val loggerResource =
          if (jsonOutput) bleepLoggers.stderrAndFileLogging(config, commonOpts, buildPaths)
          else bleepLoggers.stdoutAndFileLogging(config, commonOpts, buildPaths)
        loggerResource.use { logger =>
          replayStoredMessages(storingLogger, logger)
          val startedWithLogger = started.withLogger(logger)
          Try(cmd.run(startedWithLogger)) match {
            case Failure(th: BleepException) if jsonOutput =>
              CommandResult.print(CommandResult.failure(th))
              ExitCode.Failure
            case Failure(th) if jsonOutput =>
              CommandResult.print(CommandResult.Failure(th.getMessage))
              ExitCode.Failure
            case Failure(th)                     => fatal("command failed unexpectedly! This really shouldn't happen. Please report.", logger, th)
            case Success(Left(th)) if jsonOutput =>
              CommandResult.print(CommandResult.failure(th))
              ExitCode.Failure
            case Success(Left(th))  => fatal("command failed", logger, th)
            case Success(Right(())) => ExitCode.Success
          }
        }
    }

  /** Check if restArgs contain -o json or --output json or --json (for stderr routing). */
  private def hasJsonOutput(restArgs: List[String]): Boolean = {
    val args = restArgs.toArray
    var i = 0
    while (i < args.length) {
      if (args(i) == "--json") return true
      if ((args(i) == "-o" || args(i) == "--output") && i + 1 < args.length && args(i + 1) == "json") return true
      i += 1
    }
    false
  }

  /** Replay messages buffered by a StoringLogger to a real logger. */
  private def replayStoredMessages(storingLogger: Logger, target: Logger): Unit =
    storingLogger match {
      case sl: ryddig.TypedLogger[?] if sl.underlying.isInstanceOf[Array[?]] =>
        sl.underlying.asInstanceOf[Array[ryddig.Stored]].foreach { stored =>
          val msg = stored.message.plainText
          stored.metadata.logLevel match {
            case ryddig.LogLevel.debug => target.debug(msg)
            case ryddig.LogLevel.info  => target.info(msg)
            case ryddig.LogLevel.warn  => target.warn(msg)
            case ryddig.LogLevel.error => target.error(msg)
            case _                     => target.info(msg)
          }
        }
      case _ => ()
    }
}
