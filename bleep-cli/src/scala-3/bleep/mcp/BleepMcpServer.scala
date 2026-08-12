package bleep.mcp

import bleep._
import bleep.bsp.{BspBuildData, BspRequestHelper, BspRifle, BspRifleConfig, BspServerBuilder, BuildServerWithLifecycle, SetupBleepBsp}
import bleep.bsp.protocol.BleepBspProtocol
import bleep.bsp.protocol.DiagnosticSeverity
import bleep.internal.BspClientDisplayProgress
import bleep.requests.{TranscriptFormat, TranscriptStore}
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
  * Nothing is kept in memory between calls — not even request history. The bleep-bsp daemon persists a transcript of every compile/test request into the
  * workspace itself (`<workspace>/.bleep/builds/<variant>/requests/`), and `bleep.details` / `bleep.diff` read those files directly (no daemon connection, no
  * bootstrap). History is therefore per-worktree, shared with the CLI and IDEs, and survives MCP server restarts.
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
      case Left(be)        => throw be
      case Right(existing) => existing
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

  private class BleepMcpSession extends McpServer.Session[IO] with McpServer.ToolProvider[IO] {

    override val serverInfo: protocol.Initialize.PartyInfo =
      protocol.Initialize.PartyInfo("bleep", model.BleepVersion.current.value)

    override def instructions: IO[Option[String]] =
      IO.pure(
        Some(
          """Bleep build tool MCP server.
          |
          |## Workspaces
          |Every tool that acts on a build requires `directory`: the absolute path of the checkout you are working in
          |(your current working directory works — bleep finds the build root from any directory inside it).
          |One MCP server serves the whole session, including subagents in other git worktrees, so each call states its target.
          |Nothing is cached between calls; every call sees the current build configuration.
          |
          |## Response model
          |Compile and test return a compact summary (error/warning or pass/fail counts) plus a requestId.
          |Errors stream as log notifications during the build so you see failures immediately.
          |For full diagnostics or stack traces, call bleep.details with that requestId.
          |Request history is per-worktree: the compile daemon writes a transcript of every compile/test into the
          |workspace itself, shared with the CLI (`bleep requests`) and IDEs, surviving MCP server restarts.
          |Ids therefore only mean something together with the workspace — details/diff take `directory` too.
          |
          |## Tools
          |- bleep.compile — compile projects. Returns compact summary + requestId. Streams errors per-project as they occur.
          |- bleep.test — run tests. Returns compact summary + requestId with pass/fail counts.
          |- bleep.details — full transcript of a completed compile/test request by requestId, read from `directory`'s per-worktree history. Search it with `query` (regex); project/limit/offset paginate.
          |- bleep.diff — what logically changed between two requests (base, target) in `directory`'s history: newly failing/fixed/skipped tests, compile invalidations, new/resolved diagnostics. Timing-free and deterministic. `baseDirectory` resolves base in another worktree (copy-state verification).
          |- bleep.diff-timing — what got slower/faster between two requests, jitter suppressed, plus the target run's slowest items. Same `directory`/`baseDirectory` semantics as bleep.diff.
          |- bleep.test.suites — discover test suites without running them (requires compiled code)
          |- bleep.sourcegen — run source generators for projects
          |- bleep.fmt — format Scala and Java source files
          |- bleep.clean — delete build outputs for projects
          |- bleep.copy-state — seed a fresh git worktree with the parent worktree's compiled state; call once after forking, before compiling
          |- bleep.projects — list all projects with dependencies
          |- bleep.programs — list projects that have a mainClass (runnable programs)
          |- bleep.scripts — list scripts defined in the build
          |- bleep.run — compile and run a project or script, returns stdout/stderr
          |- bleep.restart — restart the MCP server process (e.g. after producing a new bleep binary)""".stripMargin
        )
      )

    override val tools: IO[List[ToolFunction[IO]]] = IO(
      List(
        compileTool,
        testTool,
        detailsTool,
        diffTool,
        diffTimingTool,
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
    )

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

    private def compileTool: ToolFunction[IO] = textTool[ProjectsArgs](
      ToolFunction.Info(
        "bleep.compile",
        Some("Compile"),
        Some(
          "Compile bleep projects. Returns compact summary (error counts) plus a requestId. Errors stream per-project as they finish. Call bleep.details with the requestId for full diagnostics."
        ),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (args, context) => bootstrapFor(args.directory).flatMap(started => executeCompile(started, args.projects, context)),
      None
    )

    private def testTool: ToolFunction[IO] = textTool[TestArgs](
      ToolFunction.Info(
        "bleep.test",
        Some("Test"),
        Some(
          "Run tests for bleep projects. Returns compact summary (pass/fail counts) plus a requestId. Failures stream as they occur. Call bleep.details with the requestId for full failure messages/stacktraces."
        ),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (args, context) => bootstrapFor(args.directory).flatMap(started => executeTest(started, args.projects, args.only, args.exclude, context)),
      None
    )

    private def detailsTool: ToolFunction[IO] = textTool[DetailsArgs](
      ToolFunction.Info(
        "bleep.details",
        Some("Request Details"),
        Some(
          "Full transcript of a completed compile/test request in `directory`'s per-worktree history: every diagnostic, every test failure with stack trace. Pass the requestId from a compile/test response, or omit it for the workspace's most recent request. Search it with `query` (case-insensitive regex over messages, paths, suite/test names, stack traces) instead of paging; project/limit/offset paginate. Pure file read — no build, no daemon."
        ),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (args, _) => requestDetails(args),
      None
    )

    private def diffTool: ToolFunction[IO] = textTool[DiffArgs](
      ToolFunction.Info(
        "bleep.diff",
        Some("Diff Two Runs"),
        Some(
          "Mechanical diff between two completed compile/test requests in `directory`'s history: what LOGICALLY changed. Tests: newly failing/fixed/skipped/added/removed, still-failing with changed messages. Compiles: per-project reason and status transitions, invalidated files, new/resolved diagnostics. Durations never enter the comparison — two runs with the same outcome diff as identical regardless of timing jitter. `baseDirectory` resolves base in another worktree (copy-state verification). Use bleep.diff-timing for duration comparisons. The canonical after-edit question: rerun, then diff the two requestIds."
        ),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (args, _) =>
        IO.blocking {
          val (baseTranscript, targetTranscript) = diffTranscripts(args)
          bleep.requests.RequestDiff.mechanical(baseTranscript, targetTranscript).noSpaces
        },
      None
    )

    private def diffTimingTool: ToolFunction[IO] = textTool[DiffArgs](
      ToolFunction.Info(
        "bleep.diff-timing",
        Some("Diff Run Timings"),
        Some(
          "Duration comparison between two completed compile/test requests in `directory`'s history: which tests/projects got slower or faster (deltas below max(50ms, 20% of base) are suppressed as jitter), plus the slowest items of the target run in absolute terms. Timing lives here, separate from bleep.diff, so the mechanical diff stays deterministic. Same `directory`/`baseDirectory` semantics as bleep.diff."
        ),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (args, _) =>
        IO.blocking {
          val (baseTranscript, targetTranscript) = diffTranscripts(args)
          bleep.requests.RequestDiff
            .timing(baseTranscript, targetTranscript, args.limit.getOrElse(bleep.requests.RequestDiff.DefaultTimingLimit))
            .noSpaces
        },
      None
    )

    /** Both sides of a diff, read from the per-worktree store(s). `baseDirectory` resolves the base id in another worktree's history. */
    private def diffTranscripts(args: DiffArgs): (bleep.requests.Transcript, bleep.requests.Transcript) = {
      val targetPaths = requestsWorkspacePaths(args.directory, what = "directory")
      val basePaths = args.baseDirectory match {
        case Some(dir) => requestsWorkspacePaths(dir, what = "baseDirectory")
        case None      => targetPaths
      }
      (TranscriptStore.read(basePaths, args.base), TranscriptStore.read(targetPaths, args.target))
    }

    /** BuildPaths for the transcript store of the workspace at `directory` — a pure path derivation (find bleep.yaml, Normal variant), deliberately NOT a
      * bootstrap: reading history must not resolve a build or spawn a daemon.
      */
    private def requestsWorkspacePaths(directory: String, what: String): BuildPaths = {
      val dir = Path.of(directory)
      if (!dir.isAbsolute) throw new BleepException.Text(s"$what must be an absolute path, got: $directory")
      commands.Requests.workspacePaths(dir, what)
    }

    private def testSuitesTool: ToolFunction[IO] = textTool[ProjectsArgs](
      ToolFunction.Info(
        "bleep.test.suites",
        Some("Test Suites"),
        Some(
          "Discover test suites in compiled test projects without running them. Projects must be compiled first. Returns test class names grouped by project."
        ),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (args, _) => bootstrapFor(args.directory).flatMap(started => discoverTestSuites(started, args.projects)),
      None
    )

    private def sourcegenTool: ToolFunction[IO] = textTool[ProjectsArgs](
      ToolFunction.Info(
        "bleep.sourcegen",
        Some("Source Generate"),
        Some("Run source generators for bleep projects. Only affects projects that have sourcegen scripts defined."),
        ToolFunction.Effect.Additive(false),
        false
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
        Some("Format Scala and Java source files using scalafmt and google-java-format. Optionally limit to specific projects."),
        ToolFunction.Effect.Additive(false),
        false
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
        ToolFunction.Effect.Destructive(true),
        false
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
        ToolFunction.Effect.Additive(false),
        false
      ),
      (args, _) =>
        bootstrapFor(args.directory).flatMap { started =>
          IO.fromEither(commands.CopyState(from = args.from).exec(started)).map { response =>
            Json
              .obj(
                "projects" -> Json.arr(response.projects.map(Json.fromString)*),
                "durationMs" -> Json.fromLong(response.durationMs)
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
        ToolFunction.Effect.ReadOnly,
        false
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
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (args, _) => bootstrapFor(args.directory).flatMap(started => showResolvedConfig(started, args.projects)),
      None
    )

    private def projectsTool: ToolFunction[IO] = textTool[DirArgs](
      ToolFunction.Info(
        "bleep.projects",
        Some("List Projects"),
        Some("List all projects in the build with their dependencies and whether they are test projects."),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (args, _) => bootstrapFor(args.directory).flatMap(started => listProjects(started)),
      None
    )

    private def programsTool: ToolFunction[IO] = textTool[DirArgs](
      ToolFunction.Info(
        "bleep.programs",
        Some("List Programs"),
        Some("List projects that have a mainClass defined (runnable programs). Shows project name, main class, and platform."),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (args, _) => bootstrapFor(args.directory).flatMap(started => listPrograms(started)),
      None
    )

    private def scriptsTool: ToolFunction[IO] = textTool[DirArgs](
      ToolFunction.Info(
        "bleep.scripts",
        Some("List Scripts"),
        Some("List scripts defined in the build. Scripts are named entry points that compile and run a specific main class."),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (args, _) => bootstrapFor(args.directory).flatMap(started => listScripts(started)),
      None
    )

    private def runTool: ToolFunction[IO] = textTool[RunArgs](
      ToolFunction.Info(
        "bleep.run",
        Some("Run"),
        Some(
          "Compile and run a project or script. Checks scripts first, then projects. Returns stdout/stderr and exit code. Has a timeout to prevent hanging on long-running processes."
        ),
        ToolFunction.Effect.Additive(false),
        false
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
        ToolFunction.Effect.Destructive(true),
        false
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

    /** Execute a compile on its own BSP connection. Reports progress heartbeat, streams failures, returns compact summary + requestId. */
    private def executeCompile(
        started: Started,
        projectNames: List[String],
        context: CallContext[IO]
    ): IO[String] = {
      val targetProjects = resolveProjects(started, projectNames)

      if (targetProjects.isEmpty) {
        return IO.pure("No projects to compile.")
      }

      for {
        bspConfig <- IO.fromEither(setupBspConfig(started))
        eventQueue <- Queue.unbounded[IO, Option[BleepBspProtocol.Event]]
        collectedEvents <- Ref.of[IO, List[BleepBspProtocol.Event]](Nil)
        done <- Ref.of[IO, Boolean](false)
        client = new McpBspClient(eventQueue, started.logger)

        consumerFiber <- consumeAndLogEvents(eventQueue, collectedEvents, context).start
        heartbeatFiber <- heartbeat(collectedEvents, done, "compile", context).start

        // The daemon persists the transcript and returns its id in the response; the streamed events are only collected for the compact summary below.
        requestId <- diagnoseOomOnFailure(bspConfig) {
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
            .map(requestIdFromCompileResult)
        }.guarantee(
          eventQueue.offer(None) >>
            consumerFiber.joinWithNever >>
            done.set(true) >>
            heartbeatFiber.cancel
        )

        events <- collectedEvents.get.map(_.reverse)
      } yield withRequestId(TranscriptFormat.formatCompileResult(events, verbose = false, query = None, limit = None, offset = None), requestId)
    }

    /** The daemon-assigned transcript id from a compile response. None when the daemon carried none (transcript write failed, or older daemon). */
    private def requestIdFromCompileResult(result: bsp4j.CompileResult): Option[Long] =
      for {
        dataKind <- Option(result.getDataKind)
        if dataKind == BleepBspProtocol.RequestIdDataKind
        data <- Option(result.getData)
        payload <- BleepBspProtocol.RequestIdPayload.decode(data.toString).toOption
      } yield payload.requestId

    /** Execute a test run on its own BSP connection. Reports progress heartbeat, streams failures, returns compact summary + requestId. */
    private def executeTest(
        started: Started,
        projectNames: List[String],
        only: List[String],
        exclude: List[String],
        context: CallContext[IO]
    ): IO[String] = {
      val targetProjects = resolveTestProjects(started, projectNames)

      if (targetProjects.isEmpty) {
        return IO.pure("No test projects found.")
      }

      for {
        bspConfig <- IO.fromEither(setupBspConfig(started))
        eventQueue <- Queue.unbounded[IO, Option[BleepBspProtocol.Event]]
        collectedEvents <- Ref.of[IO, List[BleepBspProtocol.Event]](Nil)
        done <- Ref.of[IO, Boolean](false)
        testRunResult <- Ref.of[IO, Option[BleepBspProtocol.TestRunResult]](None)
        client = new McpBspClient(eventQueue, started.logger)

        consumerFiber <- consumeAndLogEvents(eventQueue, collectedEvents, context).start
        heartbeatFiber <- heartbeat(collectedEvents, done, "test", context).start

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
      } yield withRequestId(
        TranscriptFormat.formatTestResult(events, trr, includeThrowables = false, query = None, limit = None, offset = None),
        trr.flatMap(_.requestId)
      )
    }

    /** Attach the daemon-assigned transcript id when the response carried one; its absence (failed transcript write, older daemon) is sanctioned and simply
      * means the summary cannot be expanded via bleep.details later.
      */
    private def withRequestId(json: Json, requestId: Option[Long]): String =
      requestId match {
        case Some(id) => json.deepMerge(Json.obj("requestId" -> Json.fromLong(id))).noSpaces
        case None     => json.noSpaces
      }

    /** Full transcript of a completed request, by id or most recent — a pure read of `directory`'s per-worktree history. */
    private def requestDetails(args: DetailsArgs): IO[String] =
      IO.blocking {
        val buildPaths = requestsWorkspacePaths(args.directory, what = "directory")
        val transcript = args.requestId match {
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

              val parts = List.newBuilder[String]
              if (finished.nonEmpty) parts += s"${finished.size} compiled"
              if (failed > 0) parts += s"$failed failed"
              if (inProgressEvents.nonEmpty) {
                val details = inProgressEvents.map { e =>
                  val elapsed = (now - e.timestamp) / 1000
                  s"${e.project} (${elapsed}s)"
                }
                parts += s"in progress: ${details.mkString(", ")}"
              }
              if (suites.nonEmpty) parts += s"${suites.size} suites done"
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
