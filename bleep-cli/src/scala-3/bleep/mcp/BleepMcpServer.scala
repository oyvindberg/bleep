package bleep.mcp

import bleep._
import bleep.bsp.{BspBuildData, BspRequestHelper, BspRifle, BspRifleConfig, BspServerBuilder, BuildServerWithLifecycle, SetupBleepBsp}
import bleep.bsp.protocol.BleepBspProtocol
import bleep.bsp.protocol.DiagnosticSeverity
import bleep.internal.BspClientDisplayProgress
import bleep.history.{DiffBase, Transcript, TranscriptDiff, TranscriptFormat, TranscriptStore}
import cats.effect._
import cats.effect.std.Queue
import ch.epfl.scala.bsp4j
import ch.linkyard.mcp.server.{CallContext, McpError, McpServer, ToolFunction}
import ch.linkyard.mcp.protocol
import io.circe.Json
import ryddig.Logger

import java.nio.file.{Files, Path}
import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.*

/** Strip ANSI escape sequences from text. Subprocess output and compiler diagnostics often contain color codes that are noise for MCP clients. */
private[mcp] val AnsiPattern = java.util.regex.Pattern.compile("\u001b\\[[0-9;]*[a-zA-Z]")
private[mcp] def stripAnsi(s: String): String = AnsiPattern.matcher(s).replaceAll("")

/** MCP server for bleep that exposes compile, test and project info to AI agents.
  *
  * Stateless by design: every tool call carries a required `directory`, is bootstrapped fresh from that workspace's bleep.yaml, opens its own connection to the
  * bleep-bsp daemon, and disconnects when done — exactly what a CLI invocation does, at the same cost. One MCP server therefore serves every git worktree a
  * session touches, always against current configuration, and holds nothing that can go stale or leak when a worktree is removed.
  *
  * Nothing is kept in memory between calls — not even run history. The bleep-bsp daemon persists a transcript of every compile/test request into the workspace
  * itself (`<workspace>/.bleep/builds/<variant>/history/`), and `bleep.history.list` / `bleep.history.show` / `bleep.history.diff` read those files directly
  * (no daemon connection, no bootstrap). History is therefore per-worktree, shared with the CLI and IDEs, and survives MCP server restarts.
  */
class BleepMcpServer(logger: Logger, userPaths: UserPaths, ec: ExecutionContext) extends McpServer[IO] {

  override def initialize(
      client: McpServer.Client[IO],
      info: McpServer.ConnectionInfo[IO]
  ): Resource[IO, McpServer.Session[IO]] =
    for {
      _ <- Resource.eval(IO {
        val caps = client.capabilities
        logger.info(
          s"MCP client connected: ${client.clientInfo.name} ${client.clientInfo.version} (sampling=${caps.sampling.isDefined}, elicitation=${caps.elicitation.isDefined}, roots=${caps.roots.isDefined})"
        )
      })
    } yield new BleepMcpSession

  // ========================================================================
  // Per-call workspace bootstrap
  // ========================================================================

  /** Bootstrap a workspace from scratch, exactly like a CLI invocation: find the build from `directory`, check the wanted bleep version, load config fresh,
    * resolve the build. No caching — a JVM change or any other edit to bleep.yaml takes effect on the next call.
    */
  private def bootstrapFor(directory: String): IO[Started] = IO.blocking {
    val dir = Path.of(directory)
    if (!dir.isAbsolute) throw new BleepException.Text(s"directory must be an absolute path, got: $directory")
    if (!Files.isDirectory(dir)) throw new BleepException.Text(s"directory does not exist: $directory")

    val buildLoader = BuildLoader.find(dir)
    val existing = buildLoader.existing match {
      case Left(_: BleepException.BuildNotFound) => throw BleepMcpServer.notABleepBuild(directory)
      case Left(be)                              => throw be
      case Right(existing)                       => existing
    }

    existing.wantedVersion.forceGet match {
      case Left(th)      => throw new BleepException.Cause(th, s"couldn't read $$version from ${existing.bleepYaml}")
      case Right(wanted) =>
        val ok = wanted == model.BleepVersion.current || wanted == model.BleepVersion.dev || model.BleepVersion.current.isDevelopment
        if (!ok)
          throw new BleepException.Text(
            s"Build at ${existing.bleepYaml} wants bleep ${wanted.value}, but this MCP server runs ${model.BleepVersion.current.value}. " +
              s"Install the matching bleep binary and call bleep.restart, or use the bleep CLI directly — it re-launches the wanted version."
          )
    }

    val buildPaths = BuildPaths(dir, buildLoader, model.BuildVariant.Normal)
    val config = BleepConfigOps.loadOrDefault(userPaths).orThrow
    val pre = Prebootstrapped(logger, userPaths, buildPaths, existing, ec)
    bootstrap.from(pre, ResolveProjects.InMemory, rewrites = Nil, config, CoursierResolver.Factory.default).orThrow
  }

  private def setupBspConfig(started: Started): Either[BleepException, BspRifleConfig] =
    started.bspServerClasspathSource match {
      case bsp.BspServerClasspathSource.FromCoursier(resolver) =>
        SetupBleepBsp(
          compileServerMode = started.config.compileServerModeOrDefault,
          config = started.config,
          resolvedJvm = started.resolvedJvm.forceGet,
          userPaths = started.pre.userPaths,
          resolver = resolver,
          logger = started.logger,
          javaSemanticdbVersion = bsp.SetupBleepBsp.DefaultJavaSemanticdbVersion
        )
      case _: bsp.BspServerClasspathSource.InProcess =>
        Left(new BleepException.Text("MCP server does not support in-process BSP mode"))
    }

  /** One BSP connection for the duration of one tool call: ensure the daemon runs, connect, run the initialize handshake with this workspace's build, and tear
    * everything down when the call completes. The daemon keeps all expensive state; connecting is cheap.
    */
  private def bspSession(started: Started, bspConfig: BspRifleConfig, client: bsp4j.BuildClient): Resource[IO, BuildServerWithLifecycle] =
    for {
      _ <- Resource.eval(BspRifle.ensureRunning(bspConfig, started.logger))
      lifecycle <- BspRifle
        .connectWithRetry(bspConfig, started.logger)
        .flatMap(connection => BspServerBuilder.create(connection, client, traceFile = None, onCleanup = None))
      _ <- Resource.eval(
        BspServerBuilder.initializeSession(
          server = lifecycle.server,
          clientName = "bleep-mcp",
          clientVersion = model.BleepVersion.current.value,
          rootUri = started.buildPaths.buildDir.toUri.toString,
          buildData = Some(BspBuildData.Payload.from(started)),
          listening = lifecycle.listening
        )
      )
    } yield lifecycle

  /** The advertised tool list, built without a client connection. `ToolFunction.Info.effect` is a security control, not documentation: MCP clients read
    * `readOnlyHint` to decide what may run unattended, so a tool that executes anything from the checkout must never claim it. Pinned by tests in
    * `bleep.McpToolEffectTest`.
    */
  private[bleep] def declaredTools: List[ToolFunction[IO]] = new BleepMcpSession().toolList

  private class BleepMcpSession extends McpServer.Session[IO] with McpServer.ToolProvider[IO] {

    override val serverInfo: protocol.Initialize.PartyInfo =
      protocol.Initialize.PartyInfo("bleep", model.BleepVersion.current.value)

    override def instructions: IO[Option[String]] =
      IO.pure(Some(BleepMcpServer.instructionsText))

    /** Effect annotations here are load-bearing. `ReadOnly` is only for tools that read files and build model — never for a tool that compiles, tests, runs,
      * generates sources, formats or deletes, because those either execute code the checkout controls (macros, annotation processors, sourcegen scripts, test
      * bodies, main methods) or write to disk. Each declaration below states which of the two it is.
      */
    private[mcp] val toolList: List[ToolFunction[IO]] =
      List(
        compileTool,
        testTool,
        historyListTool,
        historyShowTool,
        historyDiffTool,
        historyDiffTimingTool,
        testSuitesTool,
        sourcegenTool,
        fmtTool,
        cleanTool,
        copyStateTool,
        buildTool,
        buildResolvedTool,
        projectsTool,
        programsTool,
        scriptsTool,
        runTool,
        restartTool
      )

    override val tools: IO[List[ToolFunction[IO]]] = IO.pure(toolList)

    // ========================================================================
    // Tools
    // ========================================================================

    /** Build a text tool whose failures surface as MCP tool errors — an `isError` result carrying the message and cause chain — instead of the transport
      * layer's bare `-32603 Internal error` (which carries no diagnostics at all). The full stack trace goes to the MCP server log.
      */
    private def textTool[A: com.melvinlow.json.schema.JsonSchemaEncoder: io.circe.Decoder](
        info: ToolFunction.Info,
        f: (A, CallContext[IO]) => IO[String],
        meta: Option[io.circe.JsonObject]
    ): ToolFunction[IO] =
      ToolFunction.text[IO, A](
        info,
        (a, context) =>
          f(a, context).handleErrorWith {
            case e: ToolFunction.ToolError      => IO.raiseError(e)
            case e: McpError.McpErrorException  => IO.raiseError(e)
            case scala.util.control.NonFatal(e) =>
              IO(logger.error(s"${info.name} failed", e)) >>
                IO.raiseError(
                  ToolFunction.ToolError(
                    List(protocol.Content.Text(s"${info.name} failed: ${describeFailure(e)}", None, protocol.Meta.empty)),
                    protocol.Meta.empty
                  )
                )
            case e => IO.raiseError(e)
          },
        meta
      )

    /** Message plus cause chain — the useful message (e.g. "BSP server connection lost") is often nested inside an ExecutionException. */
    private def describeFailure(e: Throwable): String =
      Iterator
        .iterate(e)(_.getCause)
        .takeWhile(_ != null)
        .take(5)
        .map(t => Option(t.getMessage).filter(_.nonEmpty).getOrElse(t.getClass.getSimpleName))
        .distinct
        .mkString("; caused by: ")

    private def compileTool: ToolFunction[IO] = textTool[CompileArgs](
      ToolFunction.Info(
        "bleep.compile",
        Some("Compile"),
        Some(
          "Compile bleep projects. Executes build-supplied code: sourcegen scripts, annotation processors, symbol processors and macros all run during a compile, and it writes generated sources and compile output into the workspace. Only for bleep builds — if there is no bleep.yaml in or above the directory, this is not the project's build tool; use its own build system (Maven/Gradle/sbt/...). Returns compact summary (error counts) plus a historyId. Errors stream per-project as they finish. Call bleep.history.show with the historyId for full diagnostics. The inner loop after an edit: pass diffBase:\"previous\" (or a pinned historyId) and the response gains a \"diff\" section with what changed — new/resolved diagnostics — instead of you re-reading full results."
        ),
        // NOT read-only: a compile runs sourcegen scripts, annotation processors, KSP and macros — arbitrary code from the checkout, with the user's
        // privileges — and rewrites generated sources and compile output. Never idempotent: that code runs again on every call.
        ToolFunction.Effect.Destructive(idempotent = false),
        isOpenWorld = true
      ),
      (args, context) => bootstrapFor(args.directory).flatMap(started => executeCompile(started, args.projects, args.diffBase, context)),
      None
    )

    private def testTool: ToolFunction[IO] = textTool[TestArgs](
      ToolFunction.Info(
        "bleep.test",
        Some("Test"),
        Some(
          "Run tests for bleep projects. Executes the test bodies in the checkout — arbitrary code with your privileges, which can touch the filesystem, the network and any system the tests reach — after compiling, which itself runs sourcegen scripts, annotation processors and macros. Only for bleep builds — if there is no bleep.yaml in or above the directory, this is not the project's build tool; use its own build system (Maven/Gradle/sbt/...). Returns compact summary (pass/fail counts) plus a historyId. Failures stream as they occur. Call bleep.history.show with the historyId for full failure messages/stacktraces. The inner loop after an edit: pass diffBase:\"previous\" (or a pinned historyId) and the response gains a \"diff\" section with newly failing/fixed tests instead of you re-reading full results."
        ),
        // NOT read-only: running tests is executing the checkout's code. Test bodies write files, mutate databases, call services — bleep neither knows nor
        // constrains what they do — and the compile that precedes them runs sourcegen/annotation processors/macros too.
        ToolFunction.Effect.Destructive(idempotent = false),
        isOpenWorld = true
      ),
      (args, context) => bootstrapFor(args.directory).flatMap(started => executeTest(started, args.projects, args.only, args.exclude, args.diffBase, context)),
      None
    )

    private def historyListTool: ToolFunction[IO] = textTool[DirArgs](
      ToolFunction.Info(
        "bleep.history.list",
        Some("List Run History"),
        Some(
          "List `directory`'s recorded compile/test runs, oldest first: historyId, timestamp, mode, targets and which client ran it — the same listing `bleep history` prints, as JSON. Pure file read — no build, no daemon."
        ),
        // Genuinely read-only: reads `<workspace>/.bleep/builds/<variant>/history/*.json` and nothing else. No bootstrap, no daemon, no network, no build code.
        ToolFunction.Effect.ReadOnly,
        isOpenWorld = false
      ),
      (args, _) => historyList(args),
      None
    )

    private def historyShowTool: ToolFunction[IO] = textTool[HistoryShowArgs](
      ToolFunction.Info(
        "bleep.history.show",
        Some("Show History Entry"),
        Some(
          "Full transcript of a completed compile/test run in `directory`'s per-worktree history: every diagnostic, every test failure with stack trace. Pass the historyId from a compile/test response, or omit it for the workspace's most recent run. Search it with `query` (case-insensitive regex over messages, paths, suite/test names, stack traces) instead of paging; project/limit/offset paginate. Pure file read — no build, no daemon."
        ),
        // Genuinely read-only: one transcript file, parsed and filtered. No bootstrap, no daemon, no network, no build code.
        ToolFunction.Effect.ReadOnly,
        isOpenWorld = false
      ),
      (args, _) => historyShow(args),
      None
    )

    private def historyDiffTool: ToolFunction[IO] = textTool[DiffArgs](
      ToolFunction.Info(
        "bleep.history.diff",
        Some("Diff Two Runs"),
        Some(
          "Mechanical diff between two completed compile/test runs in `directory`'s history: what LOGICALLY changed. Tests: newly failing/fixed/skipped/added/removed, still-failing with changed messages. Compiles: per-project reason and status transitions, invalidated files, new/resolved diagnostics. Durations never enter the comparison — two runs with the same outcome diff as identical regardless of timing jitter. `baseDirectory` resolves base in another worktree (copy-state verification). Use bleep.history.diff-timing for duration comparisons. The canonical after-edit question: rerun, then diff the two historyIds."
        ),
        // Genuinely read-only: two transcript files, compared in memory. No bootstrap, no daemon, no network, no build code.
        ToolFunction.Effect.ReadOnly,
        isOpenWorld = false
      ),
      (args, _) =>
        IO.blocking {
          val (baseTranscript, targetTranscript) = diffTranscripts(args)
          bleep.history.TranscriptDiff.mechanical(baseTranscript, targetTranscript).noSpaces
        },
      None
    )

    private def historyDiffTimingTool: ToolFunction[IO] = textTool[DiffArgs](
      ToolFunction.Info(
        "bleep.history.diff-timing",
        Some("Diff Run Timings"),
        Some(
          "Duration comparison between two completed compile/test runs in `directory`'s history: which tests/projects got slower or faster (deltas below max(50ms, 20% of base) are suppressed as jitter), plus the slowest items of the target run in absolute terms. Timing lives here, separate from bleep.history.diff, so the mechanical diff stays deterministic. Same `directory`/`baseDirectory` semantics as bleep.history.diff."
        ),
        // Genuinely read-only: two transcript files, compared in memory. No bootstrap, no daemon, no network, no build code.
        ToolFunction.Effect.ReadOnly,
        isOpenWorld = false
      ),
      (args, _) =>
        IO.blocking {
          val (baseTranscript, targetTranscript) = diffTranscripts(args)
          bleep.history.TranscriptDiff
            .timing(baseTranscript, targetTranscript, args.limit.getOrElse(bleep.history.TranscriptDiff.DefaultTimingLimit))
            .noSpaces
        },
      None
    )

    /** Both sides of a diff, read from the per-worktree store(s). `baseDirectory` resolves the base id in another worktree's history. */
    private def diffTranscripts(args: DiffArgs): (bleep.history.Transcript, bleep.history.Transcript) = {
      val targetPaths = historyWorkspacePaths(args.directory, what = "directory")
      val basePaths = args.baseDirectory match {
        case Some(dir) => historyWorkspacePaths(dir, what = "baseDirectory")
        case None      => targetPaths
      }
      (TranscriptStore.read(basePaths, args.base), TranscriptStore.read(targetPaths, args.target))
    }

    /** BuildPaths for the transcript store of the workspace at `directory` — a pure path derivation (find bleep.yaml, Normal variant), deliberately NOT a
      * bootstrap: reading history must not resolve a build or spawn a daemon.
      */
    private def historyWorkspacePaths(directory: String, what: String): BuildPaths = {
      val dir = Path.of(directory)
      if (!dir.isAbsolute) throw new BleepException.Text(s"$what must be an absolute path, got: $directory")
      try commands.History.workspacePaths(dir, what)
      catch { case _: BleepException.BuildNotFound => throw BleepMcpServer.notABleepBuild(directory) }
    }

    private def testSuitesTool: ToolFunction[IO] = textTool[ProjectsArgs](
      ToolFunction.Info(
        "bleep.test.suites",
        Some("Test Suites"),
        Some(
          "Discover test suites in compiled test projects without running them. No test body runs, but discovery loads classes from the build's classpath and instantiates its test frameworks inside the compile daemon, so it is not a pure read. Projects must be compiled first. Returns test class names grouped by project."
        ),
        // NOT read-only: ClasspathTestDiscovery builds a URLClassLoader over the project's classpath, loads classes from it and instantiates the test
        // framework classes it finds (`munit.Framework` and friends) — code from the checkout's dependencies, running in the daemon. It writes nothing of its
        // own, hence Additive rather than Destructive, and repeating it discovers the same suites.
        ToolFunction.Effect.Additive(idempotent = true),
        isOpenWorld = true
      ),
      (args, _) => bootstrapFor(args.directory).flatMap(started => discoverTestSuites(started, args.projects)),
      None
    )

    private def sourcegenTool: ToolFunction[IO] = textTool[ProjectsArgs](
      ToolFunction.Info(
        "bleep.sourcegen",
        Some("Source Generate"),
        Some(
          "Run source generators for bleep projects. Only affects projects that have sourcegen scripts defined. A sourcegen script is a program from the checkout: it runs with your privileges and overwrites the generated sources it owns."
        ),
        // NOT read-only, and not merely additive: sourcegen forks the build's own scripts (arbitrary code) and overwrites previously generated sources.
        ToolFunction.Effect.Destructive(idempotent = false),
        isOpenWorld = true
      ),
      (args, _) =>
        bootstrapFor(args.directory).flatMap { started =>
          val allProjects = resolveProjects(started, args.projects)
          val sourcegenProjects = allProjects.filter { crossName =>
            started.build.explodedProjects(crossName).sourcegen.values.nonEmpty
          }
          if (sourcegenProjects.isEmpty) {
            IO.pure("""{"success":true,"message":"No projects with sourcegen scripts found."}""")
          } else {
            IO.fromEither(commands.SourceGen(false, sourcegenProjects).run(started))
              .as(
                Json
                  .obj(
                    "success" -> Json.fromBoolean(true),
                    "projects" -> Json.arr(sourcegenProjects.map(p => Json.fromString(p.value)).toList*)
                  )
                  .noSpaces
              )
          }
        },
      None
    )

    private def fmtTool: ToolFunction[IO] = textTool[ProjectsArgs](
      ToolFunction.Info(
        "bleep.fmt",
        Some("Format"),
        Some(
          "Format Scala and Java source files using scalafmt and google-java-format. Rewrites the source files in place — run it on a clean or committed tree if you want the change to be reviewable."
        ),
        // NOT read-only: it rewrites existing files in the working tree. Idempotent — formatting formatted sources changes nothing further.
        ToolFunction.Effect.Destructive(idempotent = true),
        isOpenWorld = true
      ),
      (args, _) =>
        bootstrapFor(args.directory).flatMap { started =>
          val projects = resolveProjects(started, args.projects)
          IO.fromEither(commands.Fmt(check = false, projects = projects).run(started))
            .as(
              Json
                .obj(
                  "success" -> Json.fromBoolean(true),
                  "projects" -> Json.fromInt(projects.length)
                )
                .noSpaces
            )
        },
      None
    )

    private def cleanTool: ToolFunction[IO] = textTool[ProjectsArgs](
      ToolFunction.Info(
        "bleep.clean",
        Some("Clean"),
        Some("Delete build outputs for bleep projects. Removes compiled classes and other build artifacts."),
        // Deletes files it did not create in this call. Idempotent: cleaning a clean project changes nothing.
        ToolFunction.Effect.Destructive(idempotent = true),
        isOpenWorld = false
      ),
      (args, _) =>
        bootstrapFor(args.directory).flatMap { started =>
          val projects = resolveProjects(started, args.projects)
          if (projects.isEmpty) {
            IO.pure("""{"success":true,"message":"No projects to clean."}""")
          } else {
            IO.fromEither(commands.Clean(projects).run(started))
              .as(
                Json
                  .obj(
                    "success" -> Json.fromBoolean(true),
                    "projects" -> Json.arr(projects.map(p => Json.fromString(p.value)).toList*)
                  )
                  .noSpaces
              )
          }
        },
      None
    )

    private def copyStateTool: ToolFunction[IO] = textTool[CopyStateArgs](
      ToolFunction.Info(
        "bleep.copy-state",
        Some("Copy State"),
        Some(
          "Seed a freshly created git worktree with compiled state from the worktree it was forked off, so its first build compiles only the diff instead of everything. Call this once, right after creating a worktree, before compiling in it. The copy runs in the compile daemon under per-project locks, so it is safe while the parent keeps compiling."
        ),
        // Writes into the target workspace, but only into a workspace that has no state yet — the daemon refuses when `.bleep/projects` already exists, so
        // nothing is ever overwritten. That refusal is also why it is not idempotent: the second call fails.
        ToolFunction.Effect.Additive(idempotent = false),
        isOpenWorld = true
      ),
      (args, _) =>
        bootstrapFor(args.directory).flatMap { started =>
          IO.fromEither(commands.CopyState(from = args.from).exec(started)).map { response =>
            Json
              .obj(
                "projects" -> Json.arr(response.projects.map(Json.fromString)*),
                "durationMs" -> Json.fromLong(response.durationMs),
                "bytesCopied" -> Json.fromLong(response.bytesCopied)
              )
              .noSpaces
          }
        },
      None
    )

    private def buildTool: ToolFunction[IO] = textTool[BuildArgs](
      ToolFunction.Info(
        "bleep.build.effective",
        Some("Effective Build Config"),
        Some(
          "Show the effective project configuration after all templates have been applied. Shows dependencies, scala/java/kotlin version, platform, source layout, test frameworks — everything from bleep.yaml fully expanded. Does NOT include resolved classpaths or compiled output paths. Use bleep.projects for a quick dependency overview instead."
        ),
        // Read-only: bootstrapping parses bleep.yaml and expands templates. Dependency resolution is lazy and is not forced here, so nothing is fetched,
        // nothing is written, and no build code runs.
        ToolFunction.Effect.ReadOnly,
        isOpenWorld = false
      ),
      (args, _) => bootstrapFor(args.directory).flatMap(started => showBuildConfig(started, args.projects)),
      None
    )

    private def buildResolvedTool: ToolFunction[IO] = textTool[BuildArgs](
      ToolFunction.Info(
        "bleep.build.resolved",
        Some("Resolved Build Config"),
        Some(
          "Show the fully resolved project configuration: actual classpath JARs, source directories, compiler JARs, classes output directory, and all compilation inputs. This is what the compiler sees. Requires projects to be compiled first (classpath resolution happens during compilation)."
        ),
        // Read-only for the workspace and runs no build code, but forcing the resolved projects makes coursier resolve — and, on a cold cache, download —
        // the declared dependencies from the build's repositories. Hence open-world.
        ToolFunction.Effect.ReadOnly,
        isOpenWorld = true
      ),
      (args, _) => bootstrapFor(args.directory).flatMap(started => showResolvedConfig(started, args.projects)),
      None
    )

    private def projectsTool: ToolFunction[IO] = textTool[DirArgs](
      ToolFunction.Info(
        "bleep.projects",
        Some("List Projects"),
        Some("List all projects in the build with their dependencies and whether they are test projects."),
        // Read-only: bleep.yaml parsed and expanded in memory. No fetching, no writing, no build code.
        ToolFunction.Effect.ReadOnly,
        isOpenWorld = false
      ),
      (args, _) => bootstrapFor(args.directory).flatMap(started => listProjects(started)),
      None
    )

    private def programsTool: ToolFunction[IO] = textTool[DirArgs](
      ToolFunction.Info(
        "bleep.programs",
        Some("List Programs"),
        Some("List projects that have a mainClass defined (runnable programs). Shows project name, main class, and platform."),
        // Read-only: bleep.yaml parsed and expanded in memory. No fetching, no writing, no build code.
        ToolFunction.Effect.ReadOnly,
        isOpenWorld = false
      ),
      (args, _) => bootstrapFor(args.directory).flatMap(started => listPrograms(started)),
      None
    )

    private def scriptsTool: ToolFunction[IO] = textTool[DirArgs](
      ToolFunction.Info(
        "bleep.scripts",
        Some("List Scripts"),
        Some("List scripts defined in the build. Scripts are named entry points that compile and run a specific main class."),
        // Read-only: lists the `scripts:` entries from bleep.yaml. Listing a script never runs it — that is bleep.run.
        ToolFunction.Effect.ReadOnly,
        isOpenWorld = false
      ),
      (args, _) => bootstrapFor(args.directory).flatMap(started => listScripts(started)),
      None
    )

    private def runTool: ToolFunction[IO] = textTool[RunArgs](
      ToolFunction.Info(
        "bleep.run",
        Some("Run"),
        Some(
          "Compile and run a project or script. Executes that program in a forked JVM with your privileges — whatever it does to the filesystem, the network or any system it reaches, it does for real. Checks scripts first, then projects. Returns stdout/stderr and exit code. Has a timeout to prevent hanging on long-running processes."
        ),
        // NOT read-only: this is arbitrary code execution by definition, preceded by a compile (which runs sourcegen/annotation processors/macros).
        ToolFunction.Effect.Destructive(idempotent = false),
        isOpenWorld = true
      ),
      (args, context) =>
        bootstrapFor(args.directory).flatMap { started =>
          val timeoutSeconds = args.timeoutSeconds.getOrElse(60)
          runProjectOrScript(started, args.name, args.args, args.mainClass, timeoutSeconds, context)
        },
      None
    )

    private def restartTool: ToolFunction[IO] = textTool[NoArgs](
      ToolFunction.Info(
        "bleep.restart",
        Some("Restart"),
        Some(
          "Restart the MCP server process. Use after producing a new bleep binary or when the server is in a bad state. The process exits and Claude Code will relaunch it. Wait a few seconds before calling other tools."
        ),
        // Kills this process, which drops every in-flight tool call on the connection. Idempotent: a second restart lands in the same place.
        ToolFunction.Effect.Destructive(idempotent = true),
        isOpenWorld = false
      ),
      (_, _) =>
        IO {
          logger.info("MCP server restart requested, exiting process")
          val daemon = new Thread(() => {
            // Flush stdout so the JSON-RPC response reaches the client before we exit
            System.out.flush()
            Thread.sleep(500)
            System.out.flush()
            // System.exit lets shutdown hooks and Resource finalizers run
            // (closes BSP connection gracefully). Watchdog halts after 5s
            // in case cleanup hangs.
            val watchdog = new Thread(() => {
              Thread.sleep(5000)
              Runtime.getRuntime.halt(1)
            })
            watchdog.setDaemon(true)
            watchdog.start()
            System.exit(0)
          })
          daemon.setDaemon(true)
          daemon.start()
          """{"restarting":true,"message":"Process exiting. Tools will be available again in a few seconds."}"""
        },
      None
    )

    // ========================================================================
    // Tool implementations
    // ========================================================================

    private def resolveProjects(started: Started, names: List[String]): Array[model.CrossProjectName] =
      if (names.isEmpty) {
        started.chosenProjects(None)
      } else {
        names.flatMap { name =>
          started.globs.projectNameMap.getOrElse(name, Array.empty[model.CrossProjectName])
        }.toArray
      }

    private def resolveTestProjects(started: Started, names: List[String]): Array[model.CrossProjectName] =
      if (names.isEmpty) {
        started.chosenTestProjects(None)
      } else {
        names.flatMap { name =>
          started.globs.testProjectNameMap.getOrElse(name, Array.empty[model.CrossProjectName])
        }.toArray
      }

    /** A failed tool call is the only place the agent ever sees why the server died. If the server log records an OutOfMemoryError death, append that
      * explanation (with the fix) to the failure.
      */
    private def diagnoseOomOnFailure[A](bspConfig: BspRifleConfig)(io: IO[A]): IO[A] =
      io.handleErrorWith { e =>
        BspRifle.oomCrashExplanation(bspConfig).flatMap {
          case Some(oom) => IO.raiseError(new BleepException.Cause(e, s"${e.getMessage}. $oom"))
          case None      => IO.raiseError(e)
        }
      }

    /** Resolve and validate `diffBase` BEFORE the build starts: a bad base is a tool error that never costs a compile, and resolving up front pins the base so
      * concurrent clients writing history entries mid-run cannot shift what "previous" meant.
      */
    private def resolveDiffBase(started: Started, mode: String, diffBase: Option[String]): IO[Option[Transcript]] =
      IO.blocking(diffBase.map(s => DiffBase.resolve(started.buildPaths, mode, DiffBase.parse(s))))

    /** Attach the mechanical diff against the pre-resolved base. `historyId == None` is the sanctioned transcript-write failure: the build already ran, so the
      * tool call must not fail over a missing diff — the section says so instead.
      */
    private def withDiff(json: Json, diffBaseTranscript: Option[Transcript], historyId: Option[Long], started: Started): Json =
      diffBaseTranscript match {
        case None       => json
        case Some(base) =>
          val diff = historyId match {
            case Some(id) => TranscriptDiff.mechanical(base, TranscriptStore.read(started.buildPaths, id))
            case None     =>
              Json.obj("unavailable" -> Json.fromString("this run produced no history entry (transcript write failed); the diff could not be computed"))
          }
          json.deepMerge(Json.obj("diff" -> diff))
      }

    /** Execute a compile on its own BSP connection. Reports progress heartbeat, streams failures, returns compact summary + historyId. */
    private def executeCompile(
        started: Started,
        projectNames: List[String],
        diffBase: Option[String],
        context: CallContext[IO]
    ): IO[String] = {
      val targetProjects = resolveProjects(started, projectNames)

      if (targetProjects.isEmpty) {
        return IO.pure("No projects to compile.")
      }

      for {
        diffBaseTranscript <- resolveDiffBase(started, "compile", diffBase)
        bspConfig <- IO.fromEither(setupBspConfig(started))
        eventQueue <- Queue.unbounded[IO, Option[BleepBspProtocol.Event]]
        collectedEvents <- Ref.of[IO, List[BleepBspProtocol.Event]](Nil)
        done <- Ref.of[IO, Boolean](false)
        client = new McpBspClient(eventQueue, started.logger)

        consumerFiber <- consumeAndLogEvents(eventQueue, collectedEvents, context).start
        heartbeatFiber <- heartbeat(collectedEvents, done, "compile", internal.TransitiveProjects(started.build, targetProjects).all.length, context).start

        // The daemon persists the transcript and returns its id in the response; the streamed events are only collected for the compact summary below.
        historyId <- diagnoseOomOnFailure(bspConfig) {
          val targets = BspQuery.buildTargets(started.buildPaths, targetProjects)
          bspSession(started, bspConfig, client)
            .use { lifecycle =>
              BspRequestHelper.callCancellable(
                {
                  val params = new bsp4j.CompileParams(targets)
                  params.setOriginId(UUID.randomUUID().toString)
                  lifecycle.server.buildTargetCompile(params)
                },
                lifecycle.listening
              )
            }
            .map(historyIdFromCompileResult)
        }.guarantee(
          eventQueue.offer(None) >>
            consumerFiber.joinWithNever >>
            done.set(true) >>
            heartbeatFiber.cancel
        )

        events <- collectedEvents.get.map(_.reverse)
      } yield withDiff(
        withHistoryId(TranscriptFormat.formatCompileResult(events, verbose = false, query = None, limit = None, offset = None), historyId),
        diffBaseTranscript,
        historyId,
        started
      ).noSpaces
    }

    /** The daemon-assigned transcript id from a compile response. None when the daemon carried none (transcript write failed, or older daemon). */
    private def historyIdFromCompileResult(result: bsp4j.CompileResult): Option[Long] =
      for {
        dataKind <- Option(result.getDataKind)
        if dataKind == BleepBspProtocol.HistoryIdDataKind
        data <- Option(result.getData)
        payload <- BleepBspProtocol.HistoryIdPayload.decode(data.toString).toOption
      } yield payload.historyId

    /** Execute a test run on its own BSP connection. Reports progress heartbeat, streams failures, returns compact summary + historyId. */
    private def executeTest(
        started: Started,
        projectNames: List[String],
        only: List[String],
        exclude: List[String],
        diffBase: Option[String],
        context: CallContext[IO]
    ): IO[String] = {
      val targetProjects = resolveTestProjects(started, projectNames)

      if (targetProjects.isEmpty) {
        return IO.pure("No test projects found.")
      }

      for {
        diffBaseTranscript <- resolveDiffBase(started, "test", diffBase)
        bspConfig <- IO.fromEither(setupBspConfig(started))
        eventQueue <- Queue.unbounded[IO, Option[BleepBspProtocol.Event]]
        collectedEvents <- Ref.of[IO, List[BleepBspProtocol.Event]](Nil)
        done <- Ref.of[IO, Boolean](false)
        testRunResult <- Ref.of[IO, Option[BleepBspProtocol.TestRunResult]](None)
        client = new McpBspClient(eventQueue, started.logger)

        consumerFiber <- consumeAndLogEvents(eventQueue, collectedEvents, context).start
        heartbeatFiber <- heartbeat(collectedEvents, done, "test", internal.TransitiveProjects(started.build, targetProjects).all.length, context).start

        _ <- diagnoseOomOnFailure(bspConfig) {
          val targets = BspQuery.buildTargets(started.buildPaths, targetProjects)
          bspSession(started, bspConfig, client)
            .use { lifecycle =>
              BspRequestHelper.callCancellable(
                {
                  val params = new bsp4j.TestParams(targets)
                  params.setOriginId(UUID.randomUUID().toString)
                  val testOptions = BleepBspProtocol.TestOptions(Nil, Nil, only, exclude, Nil, Nil, false, BleepBspProtocol.ClientEnv.current())
                  params.setDataKind(BleepBspProtocol.TestOptionsDataKind)
                  params.setData(com.google.gson.JsonParser.parseString(BleepBspProtocol.TestOptions.encode(testOptions)))
                  lifecycle.server.buildTargetTest(params)
                },
                lifecycle.listening
              )
            }
            .flatMap { result =>
              // Extract TestRunResult from response
              IO {
                for {
                  dataKind <- Option(result.getDataKind)
                  if dataKind == BleepBspProtocol.TestRunResultDataKind
                  data <- Option(result.getData)
                  jsonStr = data.toString
                  decoded <- BleepBspProtocol.TestRunResult.decode(jsonStr).toOption
                } testRunResult.set(Some(decoded)).unsafeRunSync()(using cats.effect.unsafe.implicits.global)
              }
            }
        }.guarantee(
          eventQueue.offer(None) >>
            consumerFiber.joinWithNever >>
            done.set(true) >>
            heartbeatFiber.cancel
        )

        events <- collectedEvents.get.map(_.reverse)
        trr <- testRunResult.get
      } yield withDiff(
        withHistoryId(
          TranscriptFormat.formatTestResult(events, trr, includeThrowables = false, query = None, limit = None, offset = None),
          trr.flatMap(_.historyId)
        ),
        diffBaseTranscript,
        trr.flatMap(_.historyId),
        started
      ).noSpaces
    }

    /** Attach the daemon-assigned transcript id when the response carried one; its absence (failed transcript write, older daemon) is sanctioned and simply
      * means the summary cannot be expanded via bleep.history.show later.
      */
    private def withHistoryId(json: Json, historyId: Option[Long]): Json =
      historyId match {
        case Some(id) => json.deepMerge(Json.obj("historyId" -> Json.fromLong(id)))
        case None     => json
      }

    /** The workspace's recorded runs, oldest first — the same listing `bleep history` prints, as JSON. A pure read of `directory`'s per-worktree history. */
    private def historyList(args: DirArgs): IO[String] =
      IO.blocking {
        val buildPaths = historyWorkspacePaths(args.directory, what = "directory")
        val entries = TranscriptStore.list(buildPaths).map { id =>
          val t = TranscriptStore.read(buildPaths, id)
          Json.obj(
            "historyId" -> Json.fromLong(t.id),
            "timestampMs" -> Json.fromLong(t.timestampMs),
            "mode" -> Json.fromString(t.mode),
            "targets" -> Json.arr(t.targets.map(Json.fromString)*),
            "client" -> Json.fromString(t.client)
          )
        }
        Json.obj("entries" -> Json.arr(entries*)).noSpaces
      }

    /** Full transcript of one history entry, by id or most recent — a pure read of `directory`'s per-worktree history. */
    private def historyShow(args: HistoryShowArgs): IO[String] =
      IO.blocking {
        val buildPaths = historyWorkspacePaths(args.directory, what = "directory")
        val transcript = args.historyId match {
          case Some(id) => TranscriptStore.read(buildPaths, id)
          case None     => TranscriptStore.readLatest(buildPaths)
        }
        TranscriptFormat.details(transcript, args.project, args.query, args.limit, args.offset).noSpaces
      }

    /** Show effective project configuration after templates have been applied. */
    private def showBuildConfig(started: Started, projectNames: List[String]): IO[String] = IO {
      val projects = if (projectNames.isEmpty) {
        started.build.explodedProjects.toList
      } else {
        val resolved = resolveProjects(started, projectNames)
        resolved.toList.flatMap { cpn =>
          started.build.explodedProjects.get(cpn).map(p => (cpn, p))
        }
      }
      val entries = projects.sortBy(_._1.value).map { case (crossName, p) =>
        val exploded = p.copy(cross = model.JsonMap.empty, `extends` = model.JsonSet.empty)
        crossName.value -> io.circe.Encoder[model.Project].apply(exploded)
      }
      Json.obj(entries*).noSpaces
    }

    /** Show fully resolved project configuration with actual paths. */
    private def showResolvedConfig(started: Started, projectNames: List[String]): IO[String] = IO {
      val crossNames = if (projectNames.isEmpty) {
        started.build.explodedProjects.keys.toList.sorted
      } else {
        resolveProjects(started, projectNames).toList
      }
      val entries = crossNames.flatMap { cpn =>
        started.resolvedProjects.get(cpn).map { lazyResolved =>
          val rp = lazyResolved.forceGet
          cpn.value -> io.circe.Encoder[ResolvedProject].apply(rp)
        }
      }
      Json.obj(entries*).noSpaces
    }

    private def listProjects(started: Started): IO[String] = IO {
      val projects = started.build.explodedProjects.toList.map { case (crossName, p) =>
        Json.obj(
          "name" -> Json.fromString(crossName.value),
          "dependsOn" -> Json.arr(p.dependsOn.values.toList.map(d => Json.fromString(d.value))*),
          "isTest" -> Json.fromBoolean(p.isTestProject.getOrElse(false))
        )
      }
      Json.arr(projects*).noSpaces
    }

    /** Discover test suites via BSP buildTarget/scalaTestClasses. Projects must be compiled first. */
    @scala.annotation.nowarn("msg=buildTargetScalaTestClasses")
    private def discoverTestSuites(started: Started, projectNames: List[String]): IO[String] = {
      val targetProjects = resolveTestProjects(started, projectNames)

      if (targetProjects.isEmpty) {
        return IO.pure("""{"projects":[]}""")
      }

      for {
        bspConfig <- IO.fromEither(setupBspConfig(started))
        result <- diagnoseOomOnFailure(bspConfig) {
          val targets = BspQuery.buildTargets(started.buildPaths, targetProjects)
          bspSession(started, bspConfig, BspClientDisplayProgress(started.logger)).use { lifecycle =>
            BspRequestHelper.callCancellable(
              lifecycle.server.buildTargetScalaTestClasses(new bsp4j.ScalaTestClassesParams(targets)),
              lifecycle.listening
            )
          }
        }
      } yield {
        val items = result.getItems.asScala.toList.flatMap { item =>
          BspQuery.projectFromBuildTarget(started)(item.getTarget).map { projectName =>
            Json.obj(
              "project" -> Json.fromString(projectName.value),
              "suites" -> Json.arr(item.getClasses.asScala.toList.map(Json.fromString)*)
            )
          }
        }
        Json.obj("projects" -> Json.arr(items*)).noSpaces
      }
    }

    /** List projects that have a mainClass defined (runnable programs). */
    private def listPrograms(started: Started): IO[String] = IO {
      val programs = started.build.explodedProjects.toList
        .filter { case (_, p) => p.platform.flatMap(_.mainClass).isDefined }
        .sortBy(_._1.value)
        .map { case (crossName, p) =>
          Json.obj(
            "project" -> Json.fromString(crossName.value),
            "mainClass" -> Json.fromString(p.platform.flatMap(_.mainClass).get),
            "platform" -> Json.fromString(p.platform.flatMap(_.name).map(_.value).getOrElse("jvm"))
          )
        }
      Json.arr(programs*).noSpaces
    }

    /** List scripts defined in the build. */
    private def listScripts(started: Started): IO[String] = IO {
      val scripts = started.build.scripts.toList.sortBy(_._1.value).flatMap { case (scriptName, scriptDefs) =>
        scriptDefs.values.collect { case model.ScriptDef.Main(project, main, _) =>
          Json.obj(
            "name" -> Json.fromString(scriptName.value),
            "project" -> Json.fromString(project.value),
            "mainClass" -> Json.fromString(main)
          )
        }
      }
      Json.arr(scripts*).noSpaces
    }

    /** Run a project or script. Checks scripts first, then projects. Compiles, then executes subprocess. */
    private def runProjectOrScript(
        started: Started,
        name: String,
        args: List[String],
        mainClassOverride: Option[String],
        timeoutSeconds: Int,
        context: CallContext[IO]
    ): IO[String] = {
      val scriptMatch = started.build.scripts.keys.find(_.value == name)
      scriptMatch match {
        case Some(sn) =>
          val scriptDefs = started.build.scripts(sn).values
          scriptDefs.headOption match {
            case Some(model.ScriptDef.Main(project, main, _)) =>
              runProject(started, project, Some(main), args, timeoutSeconds, context)
            case _ =>
              IO.raiseError(new BleepException.Text(s"Script '$name' has no main class definition"))
          }
        case None =>
          started.globs.exactProjectMap.get(name) match {
            case Some(projectName) =>
              runProject(started, projectName, mainClassOverride, args, timeoutSeconds, context)
            case None =>
              IO.raiseError(new BleepException.Text(s"'$name' is not a valid project or script name"))
          }
      }
    }

    /** Compile and run a single project. Captures stdout/stderr. */
    private def runProject(
        started: Started,
        project: model.CrossProjectName,
        mainClassOverride: Option[String],
        args: List[String],
        timeoutSeconds: Int,
        context: CallContext[IO]
    ): IO[String] =
      for {
        // Compile first
        _ <- context.log(protocol.LoggingLevel.Info, s"Compiling ${project.value}...")
        _ <- compileSilently(started, Array(project))

        // Resolve main class
        mainClass <- IO {
          mainClassOverride
            .orElse(started.build.explodedProjects(project).platform.flatMap(_.mainClass))
            .getOrElse(throw new BleepException.Text(s"No main class for ${project.value}. Specify with 'mainClass' parameter."))
        }

        // Build JVM command
        cmd <- IO.fromEither(
          internal.jvmRunCommand(started.resolvedProject(project), started.resolvedJvm, project, Some(mainClass), args)
        )

        _ <- context.log(protocol.LoggingLevel.Info, s"Running $mainClass...")

        // Execute subprocess with timeout
        result <- executeSubprocess(cmd, started.buildPaths.cwd, timeoutSeconds)
      } yield {
        val (stdout, stderr, exitCode) = result
        Json
          .obj(
            "exitCode" -> Json.fromInt(exitCode),
            "stdout" -> Json.fromString(stdout),
            "stderr" -> Json.fromString(stderr)
          )
          .noSpaces
      }

    /** Compile projects via BSP without collecting events (for run tool). */
    private def compileSilently(started: Started, targetProjects: Array[model.CrossProjectName]): IO[Unit] =
      for {
        bspConfig <- IO.fromEither(setupBspConfig(started))
        result <- diagnoseOomOnFailure(bspConfig) {
          val targets = BspQuery.buildTargets(started.buildPaths, targetProjects)
          bspSession(started, bspConfig, BspClientDisplayProgress(started.logger)).use { lifecycle =>
            BspRequestHelper.callCancellable(
              {
                val params = new bsp4j.CompileParams(targets)
                lifecycle.server.buildTargetCompile(params)
              },
              lifecycle.listening
            )
          }
        }
        _ <- IO.raiseWhen(result.getStatusCode != bsp4j.StatusCode.OK)(
          new BleepException.Text(s"Compilation failed with status ${result.getStatusCode}")
        )
      } yield ()

    /** Execute a subprocess, capturing stdout and stderr separately. Returns (stdout, stderr, exitCode). */
    private def executeSubprocess(
        cmd: List[String],
        cwd: java.nio.file.Path,
        timeoutSeconds: Int
    ): IO[(String, String, Int)] = IO.interruptible {
      val builder = new java.lang.ProcessBuilder(cmd.asJava)
      builder.directory(cwd.toFile)
      val proc = builder.start()

      // Read stdout and stderr in daemon threads to prevent buffer deadlock
      val stdoutBuf = new java.io.ByteArrayOutputStream()
      val stderrBuf = new java.io.ByteArrayOutputStream()

      val stdoutThread = new Thread((() => { proc.getInputStream.transferTo(stdoutBuf); () }): Runnable)
      val stderrThread = new Thread((() => { proc.getErrorStream.transferTo(stderrBuf); () }): Runnable)
      stdoutThread.setDaemon(true)
      stderrThread.setDaemon(true)
      stdoutThread.start()
      stderrThread.start()

      val completed = proc.waitFor(timeoutSeconds.toLong, java.util.concurrent.TimeUnit.SECONDS)
      if (!completed) {
        proc.destroyForcibly()
        stdoutThread.join(1000)
        stderrThread.join(1000)
        throw new RuntimeException(s"Process timed out after ${timeoutSeconds}s")
      }

      stdoutThread.join(5000)
      stderrThread.join(5000)
      (stripAnsi(stdoutBuf.toString()), stripAnsi(stderrBuf.toString()), proc.exitValue())
    }

    // ========================================================================
    // Event consumption
    // ========================================================================

    /** Consume BSP events: collect all for the final response, and stream failures as they happen. */
    private def consumeAndLogEvents(
        eventQueue: Queue[IO, Option[BleepBspProtocol.Event]],
        collectedEvents: Ref[IO, List[BleepBspProtocol.Event]],
        context: CallContext[IO]
    ): IO[Unit] =
      eventQueue.take.flatMap {
        case Some(event) =>
          streamFailureLine(event, context) >>
            collectedEvents.update(event :: _) >>
            consumeAndLogEvents(eventQueue, collectedEvents, context)
        case None => IO.unit
      }

    /** Log via all available channels: notifications/message (model), notifications/progress (Claude Code UI), stderr (fallback). */
    private def streamNotification(context: CallContext[IO], level: protocol.LoggingLevel, message: String): IO[Unit] =
      context.reportProgress(0.0, None, Some(message)).attempt >>
        IO(System.err.println(message)) >>
        context.log(level, message)

    /** Periodic heartbeat that reports build progress. */
    private def heartbeat(
        collectedEvents: Ref[IO, List[BleepBspProtocol.Event]],
        done: Ref[IO, Boolean],
        operation: String,
        totalProjects: Int,
        context: CallContext[IO]
    ): IO[Unit] = {
      import BleepBspProtocol.{Event => E}
      import scala.concurrent.duration.*

      val tick: IO[Unit] = for {
        isDone <- done.get
        _ <-
          if (isDone) IO.unit
          else
            collectedEvents.get.flatMap { events =>
              val now = System.currentTimeMillis()
              val finished = events.collect { case e: E.CompileFinished => e }
              val startedEvents = events.collect { case e: E.CompileStarted => e }
              val finishedProjects = finished.map(_.project).toSet
              val inProgressEvents = startedEvents.filterNot(e => finishedProjects.contains(e.project))
              val failed = finished.count(_.status.isFailure)
              val suites = events.collect { case e: E.SuiteFinished => e }

              val suitesDiscovered = events.collect { case e: E.SuitesDiscovered => e.totalSuitesDiscovered }.maxOption.getOrElse(0)
              val testsRun = events.count { case _: E.TestFinished => true; case _ => false }

              val parts = List.newBuilder[String]
              if (finished.nonEmpty || startedEvents.nonEmpty) parts += s"${finished.size}/$totalProjects compiled"
              if (failed > 0) parts += s"$failed failed"
              if (inProgressEvents.nonEmpty) {
                val details = inProgressEvents.map { e =>
                  val elapsed = (now - e.timestamp) / 1000
                  s"${e.project} (${elapsed}s)"
                }
                parts += s"in progress: ${details.mkString(", ")}"
              }
              if (suitesDiscovered > 0) parts += s"${suites.size}/$suitesDiscovered suites done"
              else if (suites.nonEmpty) parts += s"${suites.size} suites done"
              if (testsRun > 0) parts += s"$testsRun tests run"
              val status = if (parts.result().nonEmpty) parts.result().mkString(", ") else "starting"

              streamNotification(context, protocol.LoggingLevel.Info, s"[$operation] $status...")
            }
      } yield ()

      (IO.sleep(1.second) >> tick).foreverM.void
    }

    /** Stream failures as compact one-liners the moment they happen, so the agent can react without waiting for the full build. */
    private def streamFailureLine(
        event: BleepBspProtocol.Event,
        context: CallContext[IO]
    ): IO[Unit] = {
      import BleepBspProtocol.{Event => E}
      event match {
        case e: E.CompileFinished if e.status.isFailure =>
          val errorCount = e.diagnostics.count(_.severity == DiagnosticSeverity.Error)
          val firstErrors = e.diagnostics.filter(_.severity == DiagnosticSeverity.Error).take(3).map(d => stripAnsi(d.message))
          val moreStr = if (errorCount > 3) s" (+${errorCount - 3} more)" else ""
          streamNotification(
            context,
            protocol.LoggingLevel.Error,
            s"${e.project}: $errorCount errors (${e.durationMs}ms). ${firstErrors.mkString("; ")}$moreStr"
          )

        case e: E.SuiteFinished if e.outcome.isFailure =>
          val countsStr = e.outcome match {
            case bleep.bsp.protocol.SuiteOutcome.Executed(passed, failed, _, _) => s"$passed passed, $failed failed"
            case bleep.bsp.protocol.SuiteOutcome.Empty                          => "discovered but executed 0 tests"
            case bleep.bsp.protocol.SuiteOutcome.NoFrameworkMatched             => "no test framework/engine claimed this suite"
            case bleep.bsp.protocol.SuiteOutcome.Errored(message, _)            => s"errored: $message"
          }
          streamNotification(context, protocol.LoggingLevel.Error, s"${e.project.value} ${e.suite.value}: $countsStr")

        case _: E.SuiteError | _: E.SuiteTimedOut | _: E.Error =>
          McpEventFilter.filter(event) match {
            case Some(json) => streamNotification(context, protocol.LoggingLevel.Error, json.noSpaces)
            case None       => IO.unit
          }

        case e: E.LinkFinished if !e.success =>
          McpEventFilter.filter(event) match {
            case Some(json) => streamNotification(context, protocol.LoggingLevel.Error, json.noSpaces)
            case None       => IO.unit
          }

        case _ => IO.unit
      }
    }

  }
}

object BleepMcpServer {
  def apply(logger: Logger, userPaths: UserPaths, ec: ExecutionContext): BleepMcpServer = new BleepMcpServer(logger, userPaths, ec)

  /** Claude Code truncates MCP server instructions to this many characters ("Server instructions truncated from 3384 to 2048 chars" in its logs) — anything
    * beyond the limit is silently invisible to the agent. `instructionsText` must stay within it, guarded by a test, and the scope guidance goes FIRST so it
    * survives any client's truncation.
    */
  val InstructionsCharLimit = 2048

  /** Server instructions, shown to the agent once per session.
    *
    * The scope section leads for a reason: this server is typically registered user-scope, so its tools are advertised in EVERY session — including sessions on
    * Maven/Gradle/sbt projects that bleep cannot build. An agent whose "compile" intent pattern-matches to bleep.compile in such a project must be redirected
    * in words it actually reads, and must never conclude anything about a project from bleep tool availability. The per-tool details live in the tool
    * descriptions, not here (see InstructionsCharLimit).
    */
  val instructionsText: String =
    """Bleep build tool MCP server.
      |
      |## Scope: bleep builds only
      |These tools only apply to bleep builds: workspaces with a bleep.yaml in the target directory or a parent.
      |If there is no bleep.yaml, bleep is not this project's build tool — use the project's own build system
      |(pom.xml -> Maven, build.gradle -> Gradle, build.sbt -> sbt). A "not part of a bleep build" error is a
      |definitive answer about the project, not a transient failure; do not retry. The reverse also holds:
      |missing or failing bleep tools say NOTHING about what kind of project this is — only bleep.yaml on disk
      |decides. If these tools vanish mid-session the MCP connection dropped (e.g. the bleep binary was
      |reinstalled): reconnect (/mcp) or fall back to the `bleep` CLI, which always works.
      |
      |## Workspaces
      |Every tool that acts on a build requires `directory`: the absolute path of the checkout you are working
      |in (any directory inside it works). One MCP server serves the whole session, including subagents in
      |other git worktrees, so each call states its target. Nothing is cached between calls; every call sees
      |the current build configuration.
      |
      |## Response model
      |Compile and test return a compact summary (error or pass/fail counts) plus a historyId; errors stream
      |as notifications while the build runs. For full diagnostics or stack traces call bleep.history.show
      |with that historyId. Run history is per-worktree: the compile daemon writes a transcript of every run
      |into the workspace itself, shared with the CLI (`bleep history`) and surviving MCP server restarts —
      |so the bleep.history.* tools take `directory` too.
      |
      |The agent inner loop: after an edit, pass diffBase:"previous" to bleep.compile/bleep.test to get a
      |"diff" section — newly failing/fixed tests, new/resolved diagnostics — instead of re-reading full
      |results. A numeric historyId pins a fixed base (e.g. the last green run); the base is validated before
      |the build starts, so a bad id fails without costing a build.""".stripMargin

  /** The definitive redirect for a `directory` that is not inside any bleep build. Worded for an agent mid-task: it must read as a final answer about the
    * project (so the agent switches to the right build tool immediately) and never as a transient bleep failure worth retrying.
    */
  def notABleepBuild(directory: String): BleepException =
    new BleepException.Text(
      s"$directory is not part of a bleep build: no ${BuildLoader.BuildFileName} exists there or in any parent directory. " +
        "This is a definitive answer about the project, not a transient failure — do not retry. " +
        "Build it with the project's own build system instead (pom.xml -> Maven, build.gradle -> Gradle, build.sbt -> sbt). " +
        "If you expected a bleep build here, the path is wrong: pass the checkout you meant."
    )
}

/** BSP client for one tool call's connection: decodes bleep protocol events into the call's queue, delegates everything else to progress display. Only one
  * operation runs per connection, so there is no originId routing to do.
  */
private[mcp] class McpBspClient(
    events: Queue[IO, Option[BleepBspProtocol.Event]],
    logger: Logger
) extends bsp4j.BuildClient {

  private val delegate = BspClientDisplayProgress(logger)

  override def onBuildShowMessage(params: bsp4j.ShowMessageParams): Unit =
    delegate.onBuildShowMessage(params)

  override def onBuildLogMessage(params: bsp4j.LogMessageParams): Unit =
    delegate.onBuildLogMessage(params)

  override def onBuildTaskStart(params: bsp4j.TaskStartParams): Unit =
    delegate.onBuildTaskStart(params)

  override def onBuildTaskProgress(params: bsp4j.TaskProgressParams): Unit = {
    val dataKind = Option(params.getDataKind)
    if (dataKind.contains(BleepBspProtocol.DataKind)) {
      Option(params.getData).foreach { jsonData =>
        val jsonStr = jsonData match {
          case s: String => s
          case other     => other.toString
        }
        BleepBspProtocol.decode(jsonStr) match {
          case Right(event) => events.offer(Some(event)).unsafeRunSync()(using cats.effect.unsafe.implicits.global)
          case Left(_)      => ()
        }
      }
    }
    delegate.onBuildTaskProgress(params)
  }

  override def onBuildTaskFinish(params: bsp4j.TaskFinishParams): Unit =
    delegate.onBuildTaskFinish(params)

  override def onBuildPublishDiagnostics(params: bsp4j.PublishDiagnosticsParams): Unit =
    delegate.onBuildPublishDiagnostics(params)

  override def onBuildTargetDidChange(params: bsp4j.DidChangeBuildTarget): Unit =
    delegate.onBuildTargetDidChange(params)

  override def onRunPrintStdout(params: bsp4j.PrintParams): Unit =
    delegate.onRunPrintStdout(params)

  override def onRunPrintStderr(params: bsp4j.PrintParams): Unit =
    delegate.onRunPrintStderr(params)
}
