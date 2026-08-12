package bleep

import bleep.internal.{bleepLoggers, fatal, logException, BspClientDisplayProgress, FileUtils}
import bleep.packaging.ManifestCreator
import cats.data.{NonEmptyList, Validated}
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
      sun.misc.Signal
        .handle(
          new sun.misc.Signal("INFO"),
          (_: sun.misc.Signal) => internal.ChildProcessDiagnostics.dumpAll(System.err, jvmBinDirs = Nil, extraPids = Nil)
        ): Unit
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

  // Note: --directory/-d and --dev are stripped by PreBootstrapOpts before decline runs.
  // Only logging flags remain for decline to parse.

  /** Output mode for request/response commands. Defaults to text. */
  val outputMode: Opts[OutputMode] =
    Opts
      .option[String]("output", "output format: text, json, or raw", "o")
      .withDefault("text")
      .map {
        case "json" => OutputMode.Json
        case "raw"  => OutputMode.Raw
        case _      => OutputMode.Text
      }

  def noBuildOpts(logger: Logger, userPaths: UserPaths, buildPaths: BuildPaths, buildLoader: BuildLoader.NonExisting): Opts[BleepNoBuildCommand] =
    List(
      Opts.subcommand("build", "create, edit, inspect, and refactor bleep.yaml (here in a no-build context: only the `new` subcommand is reachable)")(
        newCommand(logger, userPaths, buildPaths.cwd)
      ),
      importCmd(buildLoader, userPaths, buildPaths, logger),
      importMavenCmd(buildPaths, logger),
      configCommand(userPaths).map(mkCommand => mkCommand(logger)),
      installTabCompletions(userPaths, logger),
      serverCommand(userPaths, currentWorkspace = None).map(mkCommand => mkCommand(logger)),
      Opts.subcommand("server-metrics", "(deprecated: use `bleep server metrics`) open BSP server metrics dashboard in browser")(
        (Opts.argument[Long]("pid").orNone, metricsFileOpt).mapN((pid, file) => commands.ServerMetrics(logger, userPaths, pid, file))
      )
    ).foldK

  /** `bleep server` — everything about the compile server daemons.
    *
    * Present in both command trees on purpose: daemons are user-global, not per-build. Being unable to list or kill your servers from a directory that happens
    * to have no bleep.yaml would be exactly backwards, since a stray daemon is most annoying when you are not in the project it belongs to.
    *
    * Yields a `Logger => BleepCommand` rather than a finished command because a `Started` carries a `StoringLogger` while decline is still parsing, and only
    * swaps in the real one to run the command (see `Started.withLogger` and [[runBuildCommand]]). A command that captured the logger when its `Opts` were built
    * would write every line into the void.
    */
  def serverCommand(userPaths: UserPaths, currentWorkspace: Option[java.nio.file.Path]): Opts[Logger => BleepCommand] =
    Opts.subcommand("server", "inspect and control the compile servers (bsp daemons) running on this machine")(
      List[Opts[Logger => BleepCommand]](
        Opts.subcommand("top", "live dashboard of every compile server — also what bare `bleep server` runs")(
          Opts.unit.map(_ => (logger: Logger) => commands.server.ServerTop(logger, userPaths, currentWorkspace))
        ),
        Opts.subcommand("ls", "list every compile server on this machine, running or not")(
          outputMode.map(mode => (logger: Logger) => commands.server.ServerLs(logger, userPaths, mode, currentWorkspace))
        ),
        Opts.subcommand("status", "detailed status for one compile server")(
          (Opts.argument[String]("id").orNone, outputMode).mapN((id, mode) =>
            (logger: Logger) => commands.server.ServerStatus(logger, userPaths, id, mode, currentWorkspace)
          )
        ),
        Opts.subcommand("kill", "stop compile servers — gracefully over the protocol, escalating only if that does not work")(
          (
            Opts.arguments[String]("id").orEmpty,
            Opts.flag("all", "stop every compile server on this machine").orFalse,
            Opts.flag("force", "skip the graceful path: kill immediately and delete the socket directory").orFalse
          ).mapN { (ids, all, force) => (logger: Logger) =>
            commands.server
              .ServerKill(logger, userPaths, ids, all = all, force = force, deleteDir = force, deprecatedAlias = None, currentWorkspace = currentWorkspace)
          }
        ),
        Opts.subcommand("stop-all", "stop every compile server — exactly `kill --all`, spelled the way people reach for it")(
          // Same command, same flags, same behaviour. It briefly kept the old force-and-delete semantics for script parity, but two commands you use daily
          // behaving differently is a worse tax than the parity was worth.
          Opts
            .flag("force", "skip the graceful path: kill immediately and delete the socket directory")
            .orFalse
            .map(force =>
              (logger: Logger) =>
                commands.server
                  .ServerKill(logger, userPaths, Nil, all = true, force = force, deleteDir = force, deprecatedAlias = None, currentWorkspace = currentWorkspace)
            )
        ),
        Opts.subcommand("restart", "stop a compile server so the next build starts a fresh one with the current config")(
          (Opts.arguments[String]("id").orEmpty, Opts.flag("all", "restart every running compile server").orFalse).mapN { (ids, all) => (logger: Logger) =>
            commands.server.ServerRestart(logger, userPaths, ids, all = all, currentWorkspace = currentWorkspace)
          }
        ),
        Opts.subcommand("log", "print a compile server's log; -f to follow")(
          (
            Opts.argument[String]("id").orNone,
            Opts.option[Int]("lines", "how many lines to print (default 100)", "n").withDefault(100),
            Opts.flag("follow", "keep printing as the log grows", "f").orFalse,
            Opts
              .option[Int]("generation", "0 = current log, 1 or 2 = rotated (a crashed daemon's log is usually here)")
              .withDefault(0)
          ).mapN { (id, lines, follow, generation) => (logger: Logger) =>
            commands.server.ServerLog(logger, userPaths, id, lines, follow, generation, currentWorkspace)
          }
        ),
        Opts.subcommand("metrics", "open the compile server metrics dashboard in a browser")(
          (Opts.argument[Long]("pid").orNone, metricsFileOpt).mapN((pid, file) => (logger: Logger) => commands.ServerMetrics(logger, userPaths, pid, file))
        ),
        Opts.subcommand("config", "show and change the settings compile servers start with")(
          serverConfigCommand(userPaths, currentWorkspace)
        ),
        // Bare `bleep server` opens the dashboard. Last in the list so every named subcommand is matched first, and it only claims the no-argument case.
        Opts.unit.map(_ => (logger: Logger) => commands.server.ServerTop(logger, userPaths, currentWorkspace))
      ).foldK
    )

  /** The compile server's settings, one subcommand per knob.
    *
    * Bespoke rather than a generic `config set <key> <value>`: each knob gets its own name, arity, metavar, help text and validation, which is what makes the
    * surface discoverable and the errors specific. Only the decline surface is per-knob — every one of them runs the same [[commands.server.ServerConfigSet]].
    *
    * Five of these had no command at all before and could only be set by hand-editing the YAML: parallelism-ratio, heap-pressure-threshold, test-idle-timeout,
    * sourcegen-max-memory and ksp-runner-max-memory.
    */
  private def serverConfigCommand(userPaths: UserPaths, currentWorkspace: Option[java.nio.file.Path]): Opts[Logger => BleepCommand] = {
    def set(knob: String, description: String)(update: model.BspServerConfig => model.BspServerConfig): Opts[Logger => BleepCommand] =
      Opts.subcommand(knob, description)(
        Opts.unit.map(_ => (logger: Logger) => commands.server.ServerConfigSet(logger, userPaths, knob, update, deprecatedAlias = None))
      )

    def setValue[A](knob: String, description: String, metavar: String)(
        validate: A => Unit
    )(update: (model.BspServerConfig, A) => model.BspServerConfig)(using argument: Argument[A]): Opts[Logger => BleepCommand] =
      Opts.subcommand(knob, description)(
        Opts.argument[A](metavar).map { value => (logger: Logger) =>
          validate(value)
          commands.server.ServerConfigSet(logger, userPaths, knob, config => update(config, value), deprecatedAlias = None)
        }
      )

    /** A knob that was renamed: the old spelling keeps working, writes the same field, and says what to type instead. */
    def renamedValue[A](oldKnob: String, newKnob: String, metavar: String)(
        update: (model.BspServerConfig, A) => model.BspServerConfig
    )(using argument: Argument[A]): Opts[Logger => BleepCommand] =
      Opts.subcommand(oldKnob, s"(deprecated: use `bleep server config $newKnob`)")(
        Opts.argument[A](metavar).map { value => (logger: Logger) =>
          commands.server
            .ServerConfigSet(logger, userPaths, newKnob, config => update(config, value), deprecatedAlias = Some(s"bleep server config $oldKnob"))
        }
      )

    def renamed(oldKnob: String, newKnob: String)(update: model.BspServerConfig => model.BspServerConfig): Opts[Logger => BleepCommand] =
      Opts.subcommand(oldKnob, s"(deprecated: use `bleep server config $newKnob`)")(
        Opts.unit.map(_ =>
          (logger: Logger) => commands.server.ServerConfigSet(logger, userPaths, newKnob, update, deprecatedAlias = Some(s"bleep server config $oldKnob"))
        )
      )

    def positive(knob: String)(n: Int): Unit =
      if (n < 1) throw new BleepException.Text(s"$knob must be >= 1, got $n")

    def nonNegative(knob: String)(n: Int): Unit =
      if (n < 0) throw new BleepException.Text(s"$knob must be >= 0 (0 disables it), got $n")

    List[Opts[Logger => BleepCommand]](
      Opts.subcommand("show", "every setting, what it is set to, and what the running server actually booted with")(
        outputMode.map(mode => (logger: Logger) => commands.server.ServerConfigShow(logger, userPaths, mode, currentWorkspace))
      ),
      // Named for what it is, rather than for the behaviour it switches off. `auto-shutdown-disable` meant "shared" and `auto-shutdown-enable` meant
      // "one server per invocation", which reads backwards often enough to be worth renaming.
      Opts.subcommand("mode", "shared (keep the server warm between invocations) or per-invocation (shut it down each time, slower but frees memory)")(
        Opts.argument[String]("shared|per-invocation").map { mode => (logger: Logger) =>
          val compileServerMode = mode match {
            case "shared"         => model.CompileServerMode.Shared
            case "per-invocation" => model.CompileServerMode.NewEachInvocation
            case other            => throw new BleepException.Text(s"mode must be `shared` or `per-invocation`, got `$other`")
          }
          commands.CompileServerSetMode(logger, userPaths, compileServerMode)
        }
      ),
      setValue[Int]("parallelism", "how many operations may run at once — compiles, test forks, sourcegen (default: one per core)", "n")(
        positive("parallelism")
      )((config, n) => config.copy(parallelism = Some(n))),
      set("parallelism-clear", "remove the parallelism setting (back to default: one per core)")(_.copy(parallelism = None, parallelismRatio = None)),
      setValue[Double]("parallelism-ratio", "the same as a fraction of cores, e.g. 0.5 for half. Used when parallelism is unset", "ratio") { ratio =>
        if (ratio <= 0 || ratio > 1) throw new BleepException.Text(s"parallelism-ratio must be in (0, 1], got $ratio")
      }((config, ratio) => config.copy(parallelismRatio = Some(ratio))),
      set("parallelism-ratio-clear", "remove the parallelism ratio")(_.copy(parallelismRatio = None)),
      setValue[String]("max-memory", "max heap for the compile server JVM (e.g. 4g, 2048m)", "size")(_ => ())((config, size) =>
        config.copy(compileServerMaxMemory = Some(size))
      ),
      set("max-memory-clear", "remove the compile server max heap setting (back to default: 1/4 of physical RAM, clamped to 4g..16g)")(
        _.copy(compileServerMaxMemory = None)
      ),
      setValue[Int](
        "max-cached-workspaces",
        "how many workspaces' builds stay warm in the server (default: one per GB of server heap, at least 2). Evicted ones reload on next use",
        "n"
      )(positive("max-cached-workspaces"))((config, n) => config.copy(maxCachedWorkspaces = Some(n))),
      set("max-cached-workspaces-clear", "remove the setting (back to default: one per GB of server heap, at least 2)")(_.copy(maxCachedWorkspaces = None)),
      setValue[Int]("read-timeout", "minutes the server waits for a client's next message before dropping the connection; 0 waits forever", "minutes")(
        nonNegative("read-timeout")
      )((config, n) => config.copy(bspReadTimeoutMinutes = Some(n))),
      set("read-timeout-clear", "remove the read timeout setting (back to default: 30 minutes)")(_.copy(bspReadTimeoutMinutes = None)),
      setValue[Int]("idle-timeout", "minutes a server with no connected client stays alive before shutting itself down; 0 disables", "minutes")(
        nonNegative("idle-timeout")
      )((config, n) => config.copy(compileServerIdleTimeoutMinutes = Some(n))),
      set("idle-timeout-clear", "remove the idle timeout setting (back to default: 60 minutes)")(_.copy(compileServerIdleTimeoutMinutes = None)),
      setValue[Double]("heap-pressure-threshold", "heap fraction (0.0-1.0) above which new compilations wait for memory (default: 0.80)", "fraction") {
        fraction =>
          if (fraction <= 0 || fraction > 1) throw new BleepException.Text(s"heap-pressure-threshold must be in (0, 1], got $fraction")
      }((config, fraction) => config.copy(heapPressureThreshold = Some(fraction))),
      set("heap-pressure-threshold-clear", "remove the setting (back to default: 0.80)")(_.copy(heapPressureThreshold = None)),
      setValue[String](
        "test-runner-heap",
        "default heap for forked test runner JVMs (e.g. 512m, 2g). A project's own platform.jvmOptions -Xmx overrides it",
        "size"
      )(_ => ())((config, size) => config.copy(testRunnerHeap = Some(size))),
      set("test-runner-heap-clear", s"remove the setting (back to bleep's default: ${MachineResources.DefaultForkHeapMb}m per test fork)")(
        _.copy(testRunnerHeap = None)
      ),
      // Renamed because it never was a maximum: a project's own -Xmx has always outranked it, and calling it `max` had people believing a per-project heap
      // could not exceed it.
      renamedValue[String]("test-runner-max-memory", "test-runner-heap", "size")((config, size) => config.copy(testRunnerHeap = Some(size))),
      renamed("test-runner-max-memory-clear", "test-runner-heap-clear")(_.copy(testRunnerHeap = None)),
      setValue[Int]("test-idle-timeout", "minutes a test suite may go without emitting an event before it is considered stuck (default: 2)", "minutes")(
        positive("test-idle-timeout")
      )((config, n) => config.copy(testIdleTimeoutMinutes = Some(n))),
      set("test-idle-timeout-clear", "remove the test idle timeout setting (back to default: 2 minutes)")(_.copy(testIdleTimeoutMinutes = None)),
      setValue[String]("sourcegen-max-memory", "max heap for forked sourcegen JVMs (e.g. 500m, 2g)", "size")(_ => ())((config, size) =>
        config.copy(sourcegenMaxMemory = Some(size))
      ),
      set("sourcegen-max-memory-clear", "remove the sourcegen max heap setting (back to the JVM default)")(_.copy(sourcegenMaxMemory = None)),
      setValue[String]("ksp-runner-max-memory", "max heap for forked KSP runner JVMs (e.g. 512m, 1500m)", "size")(_ => ())((config, size) =>
        config.copy(kspRunnerMaxMemory = Some(size))
      ),
      set("ksp-runner-max-memory-clear", "remove the KSP runner max heap setting (back to the JVM default)")(_.copy(kspRunnerMaxMemory = None))
    ).foldK
  }

  def argumentFrom[A](defmeta: String, nameToValue: Option[Map[String, A]]): Argument[A] =
    Argument.fromMap(defmeta, nameToValue.getOrElse(Map.empty))

  def hasBuildOpts(started: Started): Opts[BleepBuildCommand] = {
    val projectNamesNoCross: Opts[NonEmptyList[model.ProjectName]] =
      Opts
        .arguments(metavars.projectNameNoCross)(using argumentFrom(metavars.projectNameNoCross, Some(started.globs.projectNamesNoCrossMap)))

    val projectNames: Opts[Array[model.CrossProjectName]] =
      Opts
        .arguments(metavars.projectName)(using argumentFrom(metavars.projectName, Some(started.globs.projectNameMap)))
        .map(_.toList.toArray.flatten)
        .orNone
        .map(started.chosenProjects)

    /** Like projectNames but returns empty array when nothing specified, so publish commands can auto-discover publishable projects. */
    val publishProjectNames: Opts[Array[model.CrossProjectName]] =
      Opts
        .arguments(metavars.projectName)(using argumentFrom(metavars.projectName, Some(started.globs.projectNameMap)))
        .map(_.toList.toArray.flatten)
        .orNone
        .map(_.getOrElse(Array.empty))

    val projectNameNoCross: Opts[model.ProjectName] =
      Opts.argument(metavars.projectNameNoCross)(using argumentFrom(metavars.projectNameNoCross, Some(started.globs.projectNamesNoCrossMap)))

    val projectName: Opts[model.CrossProjectName] =
      Opts.argument(metavars.projectNameExact)(using argumentFrom(metavars.projectNameExact, Some(started.globs.exactProjectMap)))

    val projectNamesNoExpand: Opts[Option[List[String]]] =
      Opts
        .arguments(metavars.projectName)(using argumentFrom(metavars.projectName, Some(started.globs.projectNameMap).map(_.map { case (s, _) => (s, s) })))
        .map(_.toList)
        .orNone

    /** Accepts all project names. When none given, defaults to test projects only. Non-test projects will just be compiled. */
    val testProjectNames: Opts[Array[model.CrossProjectName]] =
      Opts
        .arguments(metavars.projectName)(using argumentFrom(metavars.projectName, Some(started.globs.projectNameMap)))
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

    // Tag-based filtering. Patterns are declared in `bleep.yaml`'s per-project `testTags`. `Argument.fromMap` both enforces known tag names at parse time
    // (typos fail loudly) and drives decline's tab-completion via started.globs.testTagsMap.
    val onlyTag: Opts[Option[NonEmptyList[String]]] =
      Opts
        .options[String](
          "only-tag",
          "Run only tests tagged with these names (declared in `testTags` per project). Multiple are OR-combined."
        )(using argumentFrom("tag", Some(started.globs.testTagsMap)))
        .orNone

    val excludeTag: Opts[Option[NonEmptyList[String]]] =
      Opts
        .options[String](
          "exclude-tag",
          "Exclude tests tagged with these names. Takes precedence over --only-tag."
        )(using argumentFrom("tag", Some(started.globs.testTagsMap)))
        .orNone

    val hasSourcegenProjectNames: Opts[Array[model.CrossProjectName]] =
      Opts
        .arguments(metavars.hasSourceGenProject)(using argumentFrom(metavars.hasSourceGenProject, Some(started.globs.hasSourceGenProjectNameMap)))
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

    lazy val ret: Opts[BleepBuildCommand] = {
      val allCommands = List(
        List[Opts[BleepBuildCommand]](
          Opts.subcommand("build", "create, edit, inspect, and refactor bleep.yaml")(
            List(
              Opts.subcommand("create-directories", "create all source and resource folders for project(s)")(
                projectNames.map(names => commands.BuildCreateDirectories(names))
              ),
              Opts.subcommand("normalize", "normalize build (deduplicate, sort, etc)")(
                Opts(commands.BuildNormalize)
              ),
              Opts.subcommand("templates-reapply", "re-merge templates into projects (run after editing a template definition to propagate changes)")(
                Opts(commands.BuildReapplyTemplates)
              ),
              Opts.subcommand("project-rename", "rename a project; references in dependsOn and templates update automatically")(
                (projectNameNoCross, Opts.argument[String]("new project name")).mapN { case (from, to) =>
                  new commands.BuildProjectRename(from, model.ProjectName(to))
                }
              ),
              Opts.subcommand(
                "project-merge-into",
                "merge the first project's sources, dependencies, and config into the second; the first project is deleted"
              )(
                (projectNameNoCross, projectNameNoCross).mapN { case (projectName, into) =>
                  new commands.BuildProjectMergeInto(projectName, into)
                }
              ),
              Opts.subcommand("projects-move", "move one or more projects into a different parent folder on disk")(
                (Opts.argument[String]("new parent folder"), projectNamesNoCross).mapN { case (parentFolder, projectNames) =>
                  new commands.BuildProjectMove(Path.of(parentFolder).toAbsolutePath, projectNames)
                }
              ),
              Opts.subcommand("templates-generate-new", "throw away existing templates and infer new")(
                Opts(commands.BuildReinferTemplates(Set.empty))
              ),
              Opts.subcommand("update-deps", "update every dependency in bleep.yaml to its newest available version")(
                (updateAsScalaSteward, updateWithPrerelease).mapN { case (sw, prerelease) =>
                  commands.BuildUpdateDeps.apply(sw, prerelease, None)
                }
              ),
              Opts.subcommand("update-dep", "update a single dependency (or every dependency from one organization) to the newest available version")(
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
              Opts.subcommand("diff", "diff the effective (template-resolved) project config between git revisions")(
                Opts.subcommand("effective", "diff the effective (template-resolved) project config against git HEAD or another revision")(
                  (projectNames, commands.BuildDiff.opts, outputMode).mapN { case (names, opts, mode) =>
                    commands.BuildDiff(opts, names, mode)
                  }
                )
              ),
              Opts.subcommand("show", "show project configuration as YAML, either as written or with templates applied")(
                List(
                  Opts.subcommand("short", "show the project's YAML exactly as written in bleep.yaml")(
                    (projectNamesNoCross, outputMode).mapN(commands.BuildShow.Short.apply)
                  ),
                  Opts.subcommand("effective", "show the project's final YAML after templates and inheritance are merged in")(
                    (projectNames, outputMode).mapN(commands.BuildShow.Effective.apply)
                  )
                ).foldK
              ),
              Opts.subcommand("evicted", "report dependency conflicts (evictions) so you can decide which library version schemes apply")(
                (projectNames, outputMode).mapN((projectNames, mode) => commands.Evicted(projectNames, mode))
              ),
              Opts.subcommand("invalidated", "list projects whose source or config changed vs a base commit (and their transitive dependents)")(
                commands.BuildInvalidated.opts
              ),
              newCommand(started.pre.logger, started.pre.userPaths, started.buildPaths.cwd)
            ).foldK
          ),
          Opts.subcommand("compile", "compile selected projects (or every project), respects the dependency graph; supports watch mode")(
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
          Opts.subcommand("link", "link JS or Native projects (Scala.js / Scala Native / Kotlin/JS / Kotlin/Native), produces .js or a native executable")(
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
          Opts.subcommand("test", "run tests in selected test projects (or every isTestProject in the build)")(
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
              onlyTag,
              excludeTag,
              Opts.flag("flamegraph", "generate execution trace (open in chrome://tracing or ui.perfetto.dev)").orFalse,
              cancel,
              Opts.option[String]("junit-report", "write JUnit XML reports to this directory").orNone
            ).mapN { case (watch, projectNames, noTui, diffWatch, jvmOpts, testArgs, only, exclude, onlyTag, excludeTag, flamegraph, cancel, junitReportDir) =>
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
                includeTags = onlyTag.map(_.toList).getOrElse(Nil),
                excludeTags = excludeTag.map(_.toList).getOrElse(Nil),
                flamegraph = flamegraph,
                cancel = cancel,
                junitReportDir = junitReportDir.map(java.nio.file.Paths.get(_)),
                clientEnv = bleep.bsp.protocol.BleepBspProtocol.ClientEnv.current()
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
              Opts.argument[String](metavars.projectOrScriptName)(using
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
          Opts.subcommand("clean", "delete compile output and generated sources for selected projects (or the whole build)")(
            projectNames.map(projectNames => commands.Clean(projectNames))
          ),
          Opts.subcommand("copy-state", "copy compiled state from a sibling git worktree, so the first build here starts from its incremental baseline")(
            Opts
              .argument[String]("source worktree")
              .map(fromStr => commands.CopyState(from = fromStr))
          ),
          Opts.subcommand("requests", "list this workspace's recorded compile/test requests (written by the compile server, kept per worktree)")(
            Opts(commands.Requests.ListRequests)
          ),
          Opts.subcommand("details", "print the full transcript of a recorded compile/test request as JSON (latest when no id is given)")(
            (
              Opts.argument[Long]("request id").orNone,
              Opts.option[String]("project", "only events belonging to this project").orNone,
              Opts
                .option[String]("query", "case-insensitive regex over messages, paths, suite/test names and stack traces; only matching items are returned")
                .orNone,
              Opts.option[Int]("limit", "max number of items (diagnostics for compile, failures for test) to return").orNone,
              Opts.option[Int]("offset", "skip the first N items before applying limit").orNone
            ).mapN { case (id, project, query, limit, offset) =>
              commands.Requests.Details(id, project, query, limit, offset)
            }
          ),
          Opts.subcommand("diff", "diff two recorded requests: what logically changed (or, with --timing, what got slower/faster)")(
            (
              Opts.argument[Long]("base request id"),
              Opts.argument[Long]("target request id"),
              Opts.flag("timing", "compare durations (jitter-suppressed) instead of logical outcome").orFalse,
              Opts.option[Int]("limit", "--timing only: max entries per list (slower/faster/slowestInTarget)").orNone,
              Opts.option[String]("base-dir", "resolve the base id in another workspace's history (e.g. the worktree this one was forked from)").orNone
            ).mapN { case (base, target, timing, limit, baseDir) =>
              commands.Requests.Diff(base, target, timing, limit, baseDir.map(java.nio.file.Paths.get(_)))
            }
          ),
          Opts.subcommand("projects", "show projects under current directory")(
            (projectNames, outputMode).mapN { (projectNames, mode) =>
              new BleepBuildCommand {
                override def run(started: Started): Either[BleepException, Unit] = {
                  val sorted = projectNames.map(_.value).sorted.toList
                  mode match {
                    case OutputMode.Text => sorted.foreach(started.logger.info(_))
                    case OutputMode.Json => CommandResult.print(CommandResult.success(ProjectList(sorted)))
                    case OutputMode.Raw  => sorted.foreach(println)
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
                    case OutputMode.Raw  => sorted.foreach(println)
                  }
                  Right(())
                }
              }
            }
          ),
          Opts.subcommand("extract-info", "emit structured JSON about the build, used by IDE plugins and external tooling")(
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
          configCommand(started.pre.userPaths).map(mkCommand =>
            new BleepBuildCommand {
              // As with `serverCommand`: the real logger only arrives with `started` at run time. Capturing one here would capture the StoringLogger decline
              // uses while parsing, which is why `bleep config ...` inside a build wrote its changes without printing a word.
              override def run(started: Started): Either[BleepException, Unit] = mkCommand(started.logger).run()
            }
          ),
          installTabCompletions(started.userPaths, started.pre.logger),
          serverCommand(started.pre.userPaths, currentWorkspace = Some(started.buildPaths.buildDir)).map(mkCommand =>
            new BleepBuildCommand {
              // The logger arrives with `started` at run time — capturing it earlier would capture the StoringLogger used during parsing.
              override def run(started: Started): Either[BleepException, Unit] = mkCommand(started.logger).run()
            }
          ),
          Opts.subcommand("server-metrics", "(deprecated: use `bleep server metrics`) open BSP server metrics dashboard in browser")(
            (Opts.argument[Long]("pid").orNone, metricsFileOpt).mapN((pid, file) =>
              commands.ServerMetrics(started.pre.logger, started.pre.userPaths, pid, file)
            )
          ),
          Opts.subcommand("publish-local", "publishes your project locally (deprecated: use 'publish local-ivy')") {
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
            // `dynverSonatypeSnapshots = true` to match the two other places that derive a version from git:
            // `GenerateResources` (which bakes `BleepVersion.current` into the client) and `PublishSonatype`.
            // Without it a snapshot publishes as `1.0.0-M10+46-abc1234` while the client it was built alongside
            // asks coursier for `1.0.0-M10+46-abc1234-SNAPSHOT` — so `bleep publish local-ivy` produced jars no
            // client would ever resolve, silently leaving the previously released server in play.
            val dynVerFallback: () => String =
              () => new bleep.plugin.dynver.DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true).version

            def publishOpts(target: commands.Publish.Target): Opts[BleepBuildCommand] =
              publishOptsWith(Opts.unit.map(_ => target))

            def publishOptsWith(targetOpts: Opts[commands.Publish.Target]): Opts[BleepBuildCommand] =
              (
                Opts.option[String]("version", "version to publish (default: from git tags)").orNone,
                Opts.flag("assert-release", "fail if git state would produce a snapshot version").orFalse,
                Opts.flag("dry-run", "show what would be published without uploading").orFalse,
                publishProjectNames,
                commonBuildOpts,
                targetOpts
              ).mapN { case (version, assertRel, dryRun, projects, buildOpts, target) =>
                commands.Publish(
                  false,
                  commands.Publish.Options(version, Some(dynVerFallback), assertRel, dryRun, target, projects, ManifestCreator.default),
                  buildOpts
                )
              }

            val builtinSubcommands: List[Opts[BleepBuildCommand]] = List(
              Opts.subcommand("local-ivy", "publish to local ivy2 cache")(
                publishOptsWith(
                  Opts
                    .option[Path]("to", "write the ivy layout here instead of ~/.ivy2/local")
                    .orNone
                    .map(commands.Publish.Target.LocalIvy.apply)
                )
              ),
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
          Opts.subcommand("fmt", "format Scala, Java, and Kotlin source files") {
            (Opts.flag("check", "ensure that all files are already formatted").orFalse, projectNames).mapN { case (check, projects) =>
              commands.Fmt(check, projects)
            }
          },
          Opts.subcommand("remote-cache", "push and pull compiled classes to/from a build cache (S3-compatible service or local directory)")(
            List(
              Opts.subcommand("pull", "pull cached compiled classes from the cache")(
                projectNames.map(names => commands.RemoteCache.Pull(names))
              ),
              Opts.subcommand("push", "push compiled classes to the cache")(
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

      allCommands.flatten.foldK
    }

    ret
  }

  /** An old `bleep config compile-server <knob>` invocation, routed through the same command the new spelling uses so the two cannot diverge. */
  private def deprecatedKnob(
      logger: Logger,
      userPaths: UserPaths,
      knob: String,
      update: model.BspServerConfig => model.BspServerConfig
  ): Either[BleepException, Unit] =
    commands.server
      .ServerConfigSet(logger, userPaths, knob, update, deprecatedAlias = Some(s"bleep config compile-server $knob"))
      .run()

  def configCommand(userPaths: UserPaths): Opts[Logger => BleepCommand] =
    Opts.subcommand("config", "manage user-level bleep configuration: auth, compile-server / test-runner JVM settings, remote-cache credentials, log timing")(
      List(
        Opts.subcommand[Logger => BleepCommand]("file", "show configuration file location")(
          outputMode.map { mode => (logger: Logger) => () =>
            mode match {
              case OutputMode.Text =>
                logger.warn(s"Config file is found in ${userPaths.configYaml}")
                Right(())
              case OutputMode.Json =>
                CommandResult.print(CommandResult.success(ConfigLocation(userPaths.configYaml.toString)))
                Right(())
              case OutputMode.Raw =>
                println(userPaths.configYaml)
                Right(())
            }
          }
        ),
        Opts.subcommand[Logger => BleepCommand]("log-timing-enable", "enable timing info in logs")(
          Opts((logger: Logger) => () => BleepConfigOps.rewritePersisted(logger, userPaths)(_.copy(logTiming = Some(true))).map(_ => ()))
        ),
        Opts.subcommand[Logger => BleepCommand]("log-timing-disable", "disable timing info in logs")(
          Opts((logger: Logger) => () => BleepConfigOps.rewritePersisted(logger, userPaths)(_.copy(logTiming = Some(false))).map(_ => ()))
        ),
        Opts.subcommand(
          "compile-server",
          "You can speed up normal usage by keeping the compile server running between invocations. This is where you control it"
        )(
          List(
            Opts.subcommand(
              "auto-shutdown-disable",
              "(deprecated: use `bleep server config mode shared`) leave compile servers running between bleep invocations"
            )(Opts { (logger: Logger) =>
              commands.CompileServerSetMode(logger, userPaths, model.CompileServerMode.Shared)
            }),
            Opts.subcommand(
              "auto-shutdown-enable",
              "(deprecated: use `bleep server config mode per-invocation`) shut down the compile server between bleep invocations"
            )(
              Opts { (logger: Logger) =>
                commands.CompileServerSetMode(logger, userPaths, model.CompileServerMode.NewEachInvocation)
              }
            ),
            Opts.subcommand[Logger => BleepCommand]("stop-all", "(deprecated: use `bleep server stop-all`) stop every shared compile server currently running")(
              Opts.flag("force", "skip the graceful path: kill immediately and delete the socket directory").orFalse.map { force => (logger: Logger) => () =>
                // Same command and same defaults as `bleep server stop-all`, so the old spelling does not quietly do something different from the new one.
                // Kept working for two releases because it is in scripts and muscle memory; removal is M13.
                commands.server
                  .ServerKill(
                    logger,
                    userPaths,
                    Nil,
                    all = true,
                    force = force,
                    deleteDir = force,
                    deprecatedAlias = Some("bleep config compile-server stop-all"),
                    currentWorkspace = None
                  )
                  .run()
              }
            ),
            Opts.subcommand[Logger => BleepCommand]("max-memory", "(deprecated: use `bleep server config max-memory`) set max heap for compile server JVM")(
              Opts.argument[String]("size").map { size => (logger: Logger) => () =>
                deprecatedKnob(logger, userPaths, "max-memory", _.copy(compileServerMaxMemory = Some(size)))
              }
            ),
            Opts.subcommand[Logger => BleepCommand](
              "max-memory-clear",
              "remove compile server max heap setting (back to default: 1/4 of physical RAM, clamped to 4g..16g)"
            )(
              Opts((logger: Logger) => () => deprecatedKnob(logger, userPaths, "max-memory-clear", _.copy(compileServerMaxMemory = None)))
            ),
            Opts.subcommand[Logger => BleepCommand](
              "parallelism",
              "(deprecated: use `bleep server config parallelism`) set how many operations may run at once — compiles, test forks, sourcegen (default: one per core)"
            )(
              Opts.argument[Int]("n").map { n => (logger: Logger) => () =>
                if (n < 1) throw new BleepException.Text(s"parallelism must be >= 1, got $n")
                deprecatedKnob(logger, userPaths, "parallelism", _.copy(parallelism = Some(n)))
              }
            ),
            Opts.subcommand[Logger => BleepCommand](
              "parallelism-clear",
              "(deprecated: use `bleep server config parallelism-clear`) remove the parallelism setting (back to default: one per core)"
            )(
              Opts((logger: Logger) => () => deprecatedKnob(logger, userPaths, "parallelism-clear", _.copy(parallelism = None, parallelismRatio = None)))
            ),
            Opts.subcommand[Logger => BleepCommand](
              "max-cached-workspaces",
              "set how many workspaces' builds stay warm in the server (default: one per GB of server heap, at least 2). Evicted ones reload on next use"
            )(
              Opts.argument[Int]("n").map { n => (logger: Logger) => () =>
                if (n < 1) throw new BleepException.Text(s"max-cached-workspaces must be >= 1, got $n")
                deprecatedKnob(logger, userPaths, "max-cached-workspaces", _.copy(maxCachedWorkspaces = Some(n)))
              }
            ),
            Opts.subcommand[Logger => BleepCommand](
              "max-cached-workspaces-clear",
              "(deprecated: use `bleep server config max-cached-workspaces-clear`) remove the setting (back to default: one per GB of server heap, at least 2)"
            )(
              Opts((logger: Logger) => () => deprecatedKnob(logger, userPaths, "max-cached-workspaces-clear", _.copy(maxCachedWorkspaces = None)))
            ),
            Opts.subcommand[Logger => BleepCommand](
              "read-timeout",
              "set minutes the server waits for a client's next message before dropping the connection, 0 to wait forever (default: 30)"
            )(
              Opts.argument[Int]("minutes").map { minutes => (logger: Logger) => () =>
                if (minutes < 0) throw new BleepException.Text(s"read-timeout must be >= 0 (0 waits forever), got $minutes")
                deprecatedKnob(logger, userPaths, "read-timeout", _.copy(bspReadTimeoutMinutes = Some(minutes)))
              }
            ),
            Opts.subcommand[Logger => BleepCommand]("read-timeout-clear", "remove read timeout setting (use default: 30 minutes)")(
              Opts((logger: Logger) => () => deprecatedKnob(logger, userPaths, "read-timeout-clear", _.copy(bspReadTimeoutMinutes = None)))
            ),
            Opts.subcommand[Logger => BleepCommand](
              "idle-timeout",
              "set minutes the server stays alive with no connected client before shutting itself down, 0 to stay alive forever (default: 60)"
            )(
              Opts.argument[Int]("minutes").map { minutes => (logger: Logger) => () =>
                if (minutes < 0) throw new BleepException.Text(s"idle-timeout must be >= 0 (0 stays alive forever), got $minutes")
                deprecatedKnob(logger, userPaths, "idle-timeout", _.copy(compileServerIdleTimeoutMinutes = Some(minutes)))
              }
            ),
            Opts.subcommand[Logger => BleepCommand]("idle-timeout-clear", "remove idle timeout setting (use default: 60 minutes)")(
              Opts((logger: Logger) => () => deprecatedKnob(logger, userPaths, "idle-timeout-clear", _.copy(compileServerIdleTimeoutMinutes = None)))
            )
          ).foldK
        ),
        Opts.subcommand("test-runner", "configure test runner JVM settings")(
          List(
            Opts.subcommand[Logger => BleepCommand](
              "max-memory",
              "(deprecated: use `bleep server config test-runner-heap`) set default heap for test runner JVMs (e.g. 512m, 2g)"
            )(
              Opts.argument[String]("size").map { size => (logger: Logger) => () =>
                deprecatedKnob(logger, userPaths, "test-runner-heap", _.copy(testRunnerHeap = Some(size)))
              }
            ),
            Opts.subcommand[Logger => BleepCommand](
              "max-memory-clear",
              "(deprecated: use `bleep server config test-runner-heap-clear`) remove the test runner heap setting (use bleep's default)"
            )(
              Opts((logger: Logger) => () => deprecatedKnob(logger, userPaths, "test-runner-heap-clear", _.copy(testRunnerHeap = None)))
            )
          ).foldK
        ),
        Opts.subcommand("auth", "configure authentication for private repositories (Artifact Registry, GitHub Packages, GitLab)")(
          List(
            Opts.subcommand[Logger => BleepCommand]("setup", "interactive setup for private repository authentication")(
              Opts((logger: Logger) => () => commands.PublishSetup(logger, userPaths, None).run())
            ),
            Opts.subcommand[Logger => BleepCommand]("list", "list configured authentications")(
              outputMode.map(mode => (logger: Logger) => () => commands.ConfigAuthList(logger, userPaths, mode).run())
            ),
            Opts.subcommand[Logger => BleepCommand]("remove", "remove authentication for a repository URI prefix")(
              Opts.argument[String]("uri-prefix").map { uriStr => (logger: Logger) => () =>
                commands.ConfigAuthRemove(logger, userPaths, java.net.URI.create(uriStr)).run()
              }
            )
          ).foldK
        ),
        Opts.subcommand("remote-cache", "configure remote build cache credentials")(
          List(
            Opts.subcommand[Logger => BleepCommand]("setup", "interactive setup for S3 remote cache credentials")(
              Opts((logger: Logger) => commands.ConfigRemoteCacheSetup(logger, userPaths))
            ),
            Opts.subcommand[Logger => BleepCommand]("clear", "remove remote cache credentials from config")(
              Opts((logger: Logger) => () => BleepConfigOps.rewritePersisted(logger, userPaths)(_.copy(remoteCacheCredentials = None)).map(_ => ()))
            )
          ).foldK
        )
      ).foldK
    )

  def importCmd(buildLoader: BuildLoader, userPaths: UserPaths, buildPaths: BuildPaths, logger: Logger): Opts[BleepCommand] =
    Opts.subcommand("import", "import an existing sbt build by reading the Bloop JSON files in .bloop (run after `sbt bloopInstall`)")(
      sbtimport.ImportOptions.opts.map { opts =>
        val cacheLogger = new BleepCacheLogger(logger)
        val fetchJvm = new FetchJvm(Some(userPaths.resolveJvmCacheDir), cacheLogger, ec)

        val existingBuild = buildLoader.existing.flatMap(_.buildFile.forceGet).toOption

        commands.Import(existingBuild, sbtBuildDir = buildPaths.cwd, fetchJvm, buildPaths, logger, opts, model.BleepVersion.current)
      }
    )

  def importMavenCmd(buildPaths: BuildPaths, logger: Logger): Opts[BleepCommand] =
    Opts.subcommand(
      "import-maven",
      "import an existing Maven build by reading pom.xml (uses `mvn help:effective-pom` to resolve parent POMs and dependencyManagement)"
    )(
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
    Opts.subcommand("new", "scaffold a new bleep workspace in the current directory: bleep.yaml, a project, a sibling test project, hello-world source")(
      (
        Opts
          .option[String]("lang", "specify language: java, kotlin, or scala (default: scala)", short = "l")
          .mapValidated { s =>
            commands.BuildCreateNew.Language.byName.get(s) match {
              case Some(lang) => Validated.valid(lang)
              case None       =>
                Validated.invalidNel(s"unknown lang '$s'; expected one of ${commands.BuildCreateNew.Language.byName.keys.toList.sorted.mkString(", ")}")
            }
          }
          .withDefault(commands.BuildCreateNew.Language.Scala),
        Opts
          .options("platform", "specify wanted platform(s) (Scala only)", metavar = metavars.platformName, short = "p")(using
            Argument.fromMap(metavars.platformName, model.PlatformId.All.map(p => (p.value, p)).toMap)
          )
          .withDefault(NonEmptyList.of(model.PlatformId.Jvm)),
        Opts
          .options("scala", "specify scala version(s) (Scala only)", "s", metavars.scalaVersion)(using
            Argument.fromMap(metavars.scalaVersion, possibleScalaVersions)
          )
          .withDefault(NonEmptyList.of(model.VersionScala.Scala3)),
        Opts.argument[String]("wanted project name")
      ).mapN { case (language, platforms, scalas, name) =>
        commands.BuildCreateNew(logger, userPaths, cwd, language, platforms, scalas, name, model.BleepVersion.current, CoursierResolver.Factory.default)
      }
    )

  // there is a flag to change build directory. we need to intercept that very early
  def cwdFor(directory: Option[String]): Path =
    directory match {
      case Some(str) if str.startsWith("/") => Paths.get(str)
      case Some(str)                        => FileUtils.cwd / str
      case None                             => FileUtils.cwd
    }

  /** Point `server-metrics` at a `metrics.jsonl` from anywhere — a CI artifact, a colleague's machine — so the dashboard and the file you download are the same
    * mechanism rather than two ways of looking at the same bytes.
    */
  private val metricsFileOpt: Opts[Option[Path]] =
    Opts.option[Path]("file", "read this metrics.jsonl instead of the newest local one (e.g. downloaded from CI)").orNone

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
                  val status = scala.sys.process.Process(binaryPath.toString :: args.toList, FileUtils.cwd.toFile, sys.env.toSeq*).!<
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

  /** `NativeTerminal.setupAnsi()` in [[_main]] only reaches kernel32 when stdout is a real console, and no CI runner is one — GitHub Actions redirects stdout
    * to a pipe. So the whole FFM path went untested on Windows, and we shipped native images with no `foreign` downcall metadata for kernel32: every Windows
    * user crashed on their first command with MissingForeignRegistrationError (#601) while CI stayed green. Reach kernel32 unconditionally from `selftest`,
    * which the Windows CI job does run, so the downcalls have to link.
    *
    * `getSize` rather than `setupAnsi` because it has no error path: it calls `GetStdHandle` (the exact downcall that crashed in #601) and
    * `GetConsoleScreenBufferInfo`, discards the return code, and reads whatever the struct holds — off a pipe that is a harmless 0x0. `setupAnsi` would be the
    * more faithful reproduction, but off a pipe `GetConsoleMode` fails and the error path runs into a bug in native-terminal 0.0.9.1: `FormatMessageW`'s
    * descriptor is built with `FunctionDescriptor.of(C_INT, ...)` yet invoked as `invokeExact(...)V`, so `getLastErrorMessage` throws WrongMethodTypeException
    * on any JVM. Real users have a real console, `GetConsoleMode` succeeds, and that path never runs — so it does not affect #601.
    */
  private def selftestWindowsKernel32(): Unit =
    if (Properties.isWin)
      println(s"kernel32 console size: ${io.github.alexarchambault.nativeterm.internal.WindowsTerm.getSize()}")

  /** Same class of guard as [[selftestWindowsKernel32]], for the other FFM surface in the binary.
    *
    * On JDK 22+ coursier-paths resolves Windows known folders through `SHGetKnownFolderPath`/`CLSIDFromString` downcalls (multi-release classes under
    * `META-INF/versions/22`), which need `foreign` reachability metadata or the native image aborts with MissingForeignRegistrationError. `_main` forces
    * `UserPaths.fromAppDirs` before anything else, so linking is already covered by any command — printing the result is what makes a *wrong* answer visible,
    * and the absolute-path check turns a silently degraded resolution into a failed CI step rather than a cache directory in the wrong place.
    */
  private def selftestUserPaths(userPaths: UserPaths): Unit = {
    println(s"user cache dir: ${userPaths.cacheDir}")
    println(s"user config dir: ${userPaths.configDir}")
    List("cache" -> userPaths.cacheDir, "config" -> userPaths.configDir).foreach { case (what, path) =>
      if (!path.isAbsolute) sys.error(s"resolved $what dir is not absolute: $path")
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
        val (preOpts, _) = PreBootstrapOpts.parse(words)

        val newWords = words.drop(1 /* arg0 */ ).padTo(current.toInt - 1, "")
        withCompletions(_args, userPaths, preOpts, newWords) { completions =>
          println(Zsh.print(completions.value))
          ExitCode.Success
        }

      case "_complete" :: compLine :: compCword :: compPoint :: _ =>
        val args = Completer.bashToArgs(compLine, compCword.toInt, compPoint.toInt)
        // accept -d after completion point
        val (preOpts, _) = PreBootstrapOpts.parse(compLine.split(" ").toList)

        withCompletions(_args, userPaths, preOpts, args) { completions =>
          completions.value.foreach(c => println(c.value))
          ExitCode.Success
        }

      case "selftest" :: Nil =>
        // checks that JNI libraries are successfully loaded
        val defaultOpts = PreBootstrapOpts.parse(Nil)._1.toLoggingOpts
        val logger = bleepLoggers.stderrWarn(defaultOpts)
        FileWatching(logger, Map(FileUtils.cwd -> List(())))(println(_)).run(FileWatching.StopWhen.Immediately)
        // verify TerminalSizeCache initialization works (SIGWINCH doesn't exist on Windows)
        BspClientDisplayProgress(logger).discard()
        selftestWindowsKernel32()
        selftestUserPaths(userPaths)
        println("OK")
        ExitCode.Success

      case "bsp" :: args =>
        val (preOpts, _) = PreBootstrapOpts.parse(args)
        val lopts = preOpts.toLoggingOpts
        val cwd = cwdFor(preOpts.directory)
        val buildLoader = BuildLoader.find(cwd)
        maybeRunWithDifferentVersion(_args, bleepLoggers.stderrAll(lopts), buildLoader, preOpts.dev).andThen {
          val buildPaths = BuildPaths(cwd, buildLoader, model.BuildVariant.BSP)
          val config = BleepConfigOps.loadOrDefault(userPaths).orThrow

          bleepLoggers.stderrAndFileLogging(config, lopts, buildPaths).use { logger =>
            buildLoader.existing.map(existing => Prebootstrapped(logger, userPaths, buildPaths, existing, ec)) match {
              case Left(be)   => fatal("", logger, be)
              case Right(pre) =>
                bsp.BspProxy.run(pre)
            }
          }
        }

      case "mcp-server" :: args =>
        // Deliberately no build loading and no version dispatch here: the MCP server is workspace-free at boot.
        // Every tool call names its workspace via the required `directory` parameter and bootstraps it fresh.
        val (preOpts, _) = PreBootstrapOpts.parse(args)
        val lopts = preOpts.toLoggingOpts
        mcp.McpServerRunner.run(bleepLoggers.stderrAll(lopts), userPaths, ec)

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
              // No-build path: create logger from pre-parsed opts (noBuildOpts commands capture logger at construction)
              bleepLoggers.stdoutAndFileLogging(config, preOpts.toLoggingOpts, buildPaths).use { logger =>
                run(noBuildOpts(logger, userPaths, buildPaths, noBuild), restArgs, logger)(_.run())
              }
            case existing: BuildLoader.Existing =>
              val pre = Prebootstrapped(storingLogger, userPaths, buildPaths, existing, ec)
              bootstrap.from(pre, ResolveProjects.InMemory, rewrites = Nil, config, CoursierResolver.Factory.default) match {
                case Left(th) =>
                  // Bootstrap failed, create fallback logger for error output
                  bleepLoggers.stdoutAndFileLogging(config, preOpts.toLoggingOpts, buildPaths).use { logger =>
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

  def withCompletions(_args: Array[String], userPaths: UserPaths, preOpts: PreBootstrapOpts, args: List[String])(f: Completer.Res => ExitCode): ExitCode = {
    val (_, restArgs) = PreBootstrapOpts.parse(args)
    val cwd = cwdFor(preOpts.directory)
    // we can not log to stdout when completing. logger should be used sparingly
    val lopts = preOpts.toLoggingOpts
    val stderr = bleepLoggers.stderrWarn(lopts)
    val buildLoader = BuildLoader.find(cwd)
    maybeRunWithDifferentVersion(_args, stderr, buildLoader, preOpts.dev).andThen {

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

  /** Scripts receive their trailing arguments verbatim, including tokens that start with `--`.
    *
    * decline's `Opts.arguments` rejects anything that looks like an option (`--clients` becomes "Unexpected option: --clients"), so before parsing we insert
    * decline's `--` end-of-options separator right after the script name. decline then treats every following token as a positional argument and hands the raw
    * list to the script's `run(started, commands, args)`.
    *
    * Only applied when the invocation actually targets a script (`bleep <script> ...` or `bleep run <script> ...`), so every built-in subcommand keeps its
    * normal flag parsing. A `--` the user typed themselves is left untouched (`PreBootstrapOpts` already forwards it), so there is never a double insertion.
    * The script's own `--watch`/`-w` flag, when present, must come immediately after the script name; it is kept in front of the inserted separator so watch
    * mode still works, while everything after it is forwarded to the script raw.
    */
  private[bleep] def insertScriptArgSeparator(restArgs: List[String], scriptNames: Set[String]): List[String] = {
    def withSeparator(scriptName: String, rest: List[String]): List[String] = {
      val (leadingWatchFlags, scriptArgs) = rest.span(arg => arg == "--watch" || arg == "-w")
      (scriptName :: leadingWatchFlags) ::: ("--" :: scriptArgs)
    }

    if (restArgs.contains("--")) restArgs
    else
      restArgs match {
        case scriptName :: rest if scriptNames.contains(scriptName)          => withSeparator(scriptName, rest)
        case "run" :: scriptName :: rest if scriptNames.contains(scriptName) => "run" :: withSeparator(scriptName, rest)
        case other                                                           => other
      }
  }

  /** Parse a build command with decline, create the real logger from LoggingOpts, replay stored bootstrap messages, then run. */
  private def runBuildCommand(
      opts: Opts[BleepBuildCommand],
      restArgs: List[String],
      preOpts: PreBootstrapOpts,
      storingLogger: Logger,
      config: model.BleepConfig,
      buildPaths: BuildPaths,
      started: Started
  ): ExitCode = {
    val parseArgs = insertScriptArgSeparator(restArgs, started.build.scripts.keys.map(_.value).toSet)
    Command("bleep", s"Bleeping fast build! (version ${model.BleepVersion.current.value})")(opts).parse(parseArgs, sys.env) match {
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
        val loggingOpts = preOpts.toLoggingOpts
        // Machine-readable output (json/raw): route logging to stderr so stdout is clean
        val machineOutput = hasMachineOutput(restArgs)
        val loggerResource =
          if (machineOutput) bleepLoggers.stderrAndFileLogging(config, loggingOpts, buildPaths)
          else bleepLoggers.stdoutAndFileLogging(config, loggingOpts, buildPaths)
        loggerResource.use { logger =>
          replayStoredMessages(storingLogger, logger)
          val startedWithLogger = started.withLogger(logger)
          Try(cmd.run(startedWithLogger)) match {
            case Failure(th: BleepException) if machineOutput =>
              CommandResult.print(CommandResult.failure(th))
              ExitCode.Failure
            case Failure(th) if machineOutput =>
              CommandResult.print(CommandResult.Failure(th.getMessage))
              ExitCode.Failure
            case Failure(th)                        => fatal("command failed unexpectedly! This really shouldn't happen. Please report.", logger, th)
            case Success(Left(th)) if machineOutput =>
              CommandResult.print(CommandResult.failure(th))
              ExitCode.Failure
            case Success(Left(th))  => fatal("command failed", logger, th)
            case Success(Right(())) => ExitCode.Success
          }
        }
    }
  }

  /** Check if restArgs request machine-readable output (json or raw), logs should go to stderr. */
  private def hasMachineOutput(restArgs: List[String]): Boolean =
    restArgs.exists {
      case "--json" | "--output=json" | "-ojson" | "--output=raw" | "-oraw" => true
      case _                                                                => false
    } || restArgs.sliding(2).exists {
      case List("-o" | "--output", "json" | "raw") => true
      case _                                       => false
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
            case null                  => target.info(msg)
          }
        }
      case _ => ()
    }
}
