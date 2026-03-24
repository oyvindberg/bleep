package bleep.mcp

import bleep._
import bleep.bsp.{BspRequestHelper, BspRifle, BspRifleConfig, BspServerBuilder, SetupBleepBsp}
import bleep.bsp.protocol.BleepBspProtocol
import bleep.bsp.protocol.{CompileStatus, DiagnosticSeverity, TestStatus}
import bleep.internal.BspClientDisplayProgress
import bleep.testing.{BuildDiff, PreviousRunState, TestKey}
import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._
import ch.epfl.scala.bsp4j
import ch.linkyard.mcp.server.{CallContext, McpServer, ResourceTemplate, ToolFunction}
import ch.linkyard.mcp.protocol
import io.circe.Json

import java.nio.file.Files
import java.util.UUID
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch}
import java.util.concurrent.atomic.AtomicReference
import scala.jdk.CollectionConverters.*

/** Strip ANSI escape sequences from text. Subprocess output and compiler diagnostics often contain color codes that are noise for MCP clients. */
private[mcp] val AnsiPattern = java.util.regex.Pattern.compile("\u001b\\[[0-9;]*[a-zA-Z]")
private[mcp] def stripAnsi(s: String): String = AnsiPattern.matcher(s).replaceAll("")

/** MCP server for bleep that exposes compile, test, watch, and project info to AI agents.
  *
  * Connects to the bleep-bsp daemon (same server used by the TUI) and translates BSP events into MCP tool results and notifications. Runs on stdio transport.
  */
class BleepMcpServer(initialStarted: Started) extends McpServer[IO] {
  private val startedRef = new AtomicReference(initialStarted)
  private def started: Started = startedRef.get()

  override def initialize(
      client: McpServer.Client[IO],
      info: McpServer.ConnectionInfo[IO]
  ): Resource[IO, McpServer.Session[IO]] =
    for {
      bspConfig <- Resource.eval(IO.fromEither(setupBspConfig()))
      _ <- Resource.eval(IO {
        val caps = client.capabilities
        val clientName = client.clientInfo.name
        val clientVersion = client.clientInfo.version
        val sampling = caps.sampling.isDefined
        val elicitation = caps.elicitation.isDefined
        val roots = caps.roots.isDefined
        started.logger.info(
          s"MCP client connected: $clientName $clientVersion (sampling=$sampling, elicitation=$elicitation, roots=$roots)"
        )
      })

      // Create persistent BSP connection for the MCP session lifetime
      _ <- Resource.eval(BspRifle.ensureRunning(bspConfig, started.logger))
      connection <- BspRifle.connectWithRetry(bspConfig, started.logger)
      eventRoutes = new ConcurrentHashMap[String, Queue[IO, Option[BleepBspProtocol.Event]]]()
      diagnosticRoutes = new ConcurrentHashMap[String, bsp4j.PublishDiagnosticsParams => Unit]()
      sharedClient = new SharedMcpBspClient(eventRoutes, diagnosticRoutes, started.logger)
      lifecycle <- BspServerBuilder.create(connection, sharedClient)
      _ <- Resource.eval(BspServerBuilder.initializeSession(
        server = lifecycle.server,
        clientName = "bleep-mcp",
        clientVersion = model.BleepVersion.current.value,
        rootUri = started.buildPaths.buildDir.toUri.toString,
        buildData = None,
        listening = lifecycle.listening
      ))

      _ <- startBuildWatcher(client)
      watchJobs <- Resource.eval(Ref.of[IO, Map[JobId, WatchJob]](Map.empty))
      watchResults <- Resource.eval(Ref.of[IO, Map[JobId, WatchCycleResult]](Map.empty))
      buildHistory <- Resource.eval(Ref.of[IO, BuildHistory](BuildHistory.empty))
      _ <- Resource.onFinalize(
        watchJobs.get.flatMap { jobs =>
          jobs.values.toList.traverse_(_.cancel)
        }
      )
    } yield new BleepMcpSession(client, bspConfig, lifecycle.server, lifecycle.listening, eventRoutes, diagnosticRoutes, watchJobs, watchResults, buildHistory)

  private def setupBspConfig(): Either[BleepException, BspRifleConfig] =
    started.bspServerClasspathSource match {
      case bsp.BspServerClasspathSource.FromCoursier(resolver) =>
        SetupBleepBsp(
          compileServerMode = started.config.compileServerModeOrDefault,
          config = started.config,
          resolvedJvm = started.resolvedJvm.forceGet,
          userPaths = started.pre.userPaths,
          resolver = resolver,
          logger = started.logger,
          extraServerClasspath = Seq.empty
        )
      case _: bsp.BspServerClasspathSource.InProcess =>
        Left(new BleepException.Text("MCP server does not support in-process BSP mode"))
    }

  /** Watch bleep.yaml (and config/project selection) for changes and auto-reload the build. Sends a log notification to the MCP client when the build is
    * reloaded.
    */
  private def startBuildWatcher(client: McpServer.Client[IO]): Resource[IO, Unit] =
    Resource
      .make(
        IO {
          val watcher = BleepFileWatching.build(started.pre) { changedFiles =>
            started.reloadFromDisk() match {
              case Left(ex) =>
                val msg = s"Build file changed (${changedFiles.mkString(", ")}), but reload failed: ${ex.getMessage}"
                started.logger.error(msg)
                client.log(protocol.LoggingLevel.Error, None, msg).unsafeRunSync()(cats.effect.unsafe.implicits.global)
              case Right(None) =>
                () // parsed JSON identical, no actual change
              case Right(Some(newStarted)) =>
                startedRef.set(newStarted)
                val msg = s"Build reloaded (${changedFiles.mkString(", ")}). Project list and build model updated."
                newStarted.logger.info(msg)
                client.log(protocol.LoggingLevel.Info, None, msg).unsafeRunSync()(cats.effect.unsafe.implicits.global)
            }
          }
          val thread = new Thread(() => watcher.run(FileWatching.StopWhen.Never), "mcp-build-watcher")
          thread.setDaemon(true)
          thread.start()
          watcher
        }
      )(watcher => IO(watcher.close()))
      .void

  private class BleepMcpSession(
      _client: McpServer.Client[IO],
      bspConfig: BspRifleConfig,
      bspServer: bleep.bsp.BuildServer,
      bspListening: java.util.concurrent.Future[Void],
      eventRoutes: ConcurrentHashMap[String, Queue[IO, Option[BleepBspProtocol.Event]]],
      diagnosticRoutes: ConcurrentHashMap[String, bsp4j.PublishDiagnosticsParams => Unit],
      watchJobs: Ref[IO, Map[JobId, WatchJob]],
      watchResults: Ref[IO, Map[JobId, WatchCycleResult]],
      buildHistory: Ref[IO, BuildHistory]
  ) extends McpServer.Session[IO]
      with McpServer.ToolProvider[IO]
      with McpServer.ResourceProvider[IO] {

    override val serverInfo: protocol.Initialize.PartyInfo =
      protocol.Initialize.PartyInfo("bleep", model.BleepVersion.current.value)

    override def instructions: IO[Option[String]] =
      IO.pure(
        Some(
          """Bleep build tool MCP server.
          |
          |## Response model
          |Compile and test responses are always compact summaries (error/warning counts, diff against previous run).
          |Errors stream as log notifications during the build so you see failures immediately.
          |For full diagnostics, call bleep.status after a build completes.
          |Use verbose=true on compile/test only when you need every diagnostic in the response.
          |
          |## Tools
          |- bleep.compile — compile projects. Returns compact summary. Streams errors per-project as they occur.
          |- bleep.test — run tests. Returns compact summary with pass/fail counts and diff.
          |- bleep.status — get cached results from the last build. Use project/limit/offset to paginate large results.
          |- bleep.test.suites — discover test suites without running them (requires compiled code)
          |- bleep.sourcegen — run source generators for projects
          |- bleep.fmt — format Scala and Java source files
          |- bleep.clean — delete build outputs for projects
          |- bleep.watch — start background file watcher that recompiles on changes
          |- bleep.sync — wait for in-flight builds to finish, returns snapshot
          |- bleep.watch.stop — stop background watch jobs
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
        testSuitesTool,
        sourcegenTool,
        fmtTool,
        cleanTool,
        watchTool,
        syncTool,
        watchStopTool,
        buildTool,
        buildResolvedTool,
        projectsTool,
        programsTool,
        scriptsTool,
        runTool,
        statusTool,
        restartTool
      )
    )

    override def resources(after: Option[String]): fs2.Stream[IO, (String, protocol.Resource)] = {
      val res = protocol.Resource(
        "bleep://build/bleep.yaml",
        "bleep.yaml",
        None,
        Some("The bleep build configuration file"),
        Some("text/yaml"),
        None
      )
      fs2.Stream.emit(("1", res))
    }

    override def resource(uri: String, context: CallContext[IO]): IO[protocol.Resources.ReadResource.Response] =
      uri match {
        case "bleep://build/bleep.yaml" =>
          IO.blocking {
            val content = Files.readString(started.buildPaths.bleepYamlFile)
            protocol.Resources.ReadResource.Response(
              List(
                protocol.Resource.Contents.Text(
                  uri,
                  Some("text/yaml"),
                  content
                )
              )
            )
          }
        case other =>
          IO.raiseError(new RuntimeException(s"Unknown resource: $other"))
      }

    override def resourceTemplates(after: Option[String]): fs2.Stream[IO, (String, ResourceTemplate[IO])] =
      fs2.Stream.empty

    // ========================================================================
    // Tools
    // ========================================================================

    private def compileTool: ToolFunction[IO] = ToolFunction.text[IO, ProjectsArgs](
      ToolFunction.Info(
        "bleep.compile",
        Some("Compile"),
        Some(
          "Compile bleep projects. Returns compact summary (error counts, diff). Errors stream per-project as they finish. Call bleep.status for full diagnostic details. Use verbose=true only when you need every diagnostic in the response."
        ),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (args, context) => executeBspOperation(args.projects, args.verbose, context),
      None
    )

    private def testTool: ToolFunction[IO] = ToolFunction.text[IO, TestArgs](
      ToolFunction.Info(
        "bleep.test",
        Some("Test"),
        Some(
          "Run tests for bleep projects. Returns compact summary (pass/fail counts, diff). Failures stream as they occur. Call bleep.status for full details. Use verbose=true only when you need full failure messages/stacktraces."
        ),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (args, context) => executeBspTestOperation(args.projects, args.only, args.exclude, args.verbose, context),
      None
    )

    private def testSuitesTool: ToolFunction[IO] = ToolFunction.text[IO, ProjectsArgs](
      ToolFunction.Info(
        "bleep.test.suites",
        Some("Test Suites"),
        Some(
          "Discover test suites in compiled test projects without running them. Projects must be compiled first. Returns test class names grouped by project."
        ),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (args, _) => discoverTestSuites(args.projects),
      None
    )

    private def sourcegenTool: ToolFunction[IO] = ToolFunction.text[IO, ProjectsArgs](
      ToolFunction.Info(
        "bleep.sourcegen",
        Some("Source Generate"),
        Some("Run source generators for bleep projects. Only affects projects that have sourcegen scripts defined."),
        ToolFunction.Effect.Additive(false),
        false
      ),
      (args, _) => {
        val allProjects = resolveProjects(args.projects)
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

    private def fmtTool: ToolFunction[IO] = ToolFunction.text[IO, ProjectsArgs](
      ToolFunction.Info(
        "bleep.fmt",
        Some("Format"),
        Some("Format Scala and Java source files using scalafmt and google-java-format. Optionally limit to specific projects."),
        ToolFunction.Effect.Additive(false),
        false
      ),
      (args, _) => {
        val projects = resolveProjects(args.projects)
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

    private def cleanTool: ToolFunction[IO] = ToolFunction.text[IO, ProjectsArgs](
      ToolFunction.Info(
        "bleep.clean",
        Some("Clean"),
        Some("Delete build outputs for bleep projects. Removes compiled classes and other build artifacts."),
        ToolFunction.Effect.Destructive(true),
        false
      ),
      (args, _) => {
        val projects = resolveProjects(args.projects)
        if (projects.isEmpty) {
          IO.pure("""{"success":true,"message":"No projects to clean."}""")
        } else {
          for {
            _ <- IO.fromEither(commands.Clean(projects).run(started))
            projectNames = projects.map(_.value).toSet
            _ <- buildHistory.update(_.dropProjects(projectNames))
          } yield Json
            .obj(
              "success" -> Json.fromBoolean(true),
              "projects" -> Json.arr(projects.map(p => Json.fromString(p.value)).toList*)
            )
            .noSpaces
        }
      },
      None
    )

    private def watchTool: ToolFunction[IO] = ToolFunction.text[IO, WatchArgs](
      ToolFunction.Info(
        "bleep.watch",
        Some("Watch"),
        Some(
          "Start a background watch job that recompiles on file changes. Returns a job ID. Diagnostics stream via notifications. Use bleep.sync to get a clean snapshot, bleep.watch.stop to stop."
        ),
        ToolFunction.Effect.Additive(false),
        false
      ),
      (args, context) => {
        val mode = args.mode match {
          case "test" => BleepBspProtocol.BuildMode.Test
          case _      => BleepBspProtocol.BuildMode.Compile
        }
        startWatch(args.projects, mode, args.only, args.exclude, context)
      },
      None
    )

    private def syncTool: ToolFunction[IO] = ToolFunction.text[IO, NoArgs](
      ToolFunction.Info(
        "bleep.sync",
        Some("Sync"),
        Some(
          "Returns the latest results from active watch jobs (compile diagnostics, test results). If no watch jobs are running, does a fresh compile of all projects."
        ),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (_, context) =>
        for {
          results <- watchResults.get
          response <-
            if (results.nonEmpty) {
              val items = results.values.toList.sortBy(_.jobId.value).map { r =>
                Json.obj(
                  "jobId" -> Json.fromString(r.jobId.value),
                  "mode" -> Json.fromString(r.mode.value),
                  "result" -> io.circe.parser.parse(r.summary).fold(throw _, identity),
                  "timestampMs" -> Json.fromLong(r.timestampMs)
                )
              }
              IO.pure(Json.obj("watchResults" -> Json.arr(items*)).noSpaces)
            } else {
              executeBspOperation(Nil, true, context)
            }
        } yield response,
      None
    )

    private def watchStopTool: ToolFunction[IO] = ToolFunction.text[IO, JobIdArgs](
      ToolFunction.Info(
        "bleep.watch.stop",
        Some("Stop Watch"),
        Some("Stop a background watch job, or all watch jobs if no jobId is specified."),
        ToolFunction.Effect.Destructive(true),
        false
      ),
      (args, _) => stopWatch(args.jobId.map(new JobId(_))),
      None
    )

    private def buildTool: ToolFunction[IO] = ToolFunction.text[IO, BuildArgs](
      ToolFunction.Info(
        "bleep.build.effective",
        Some("Effective Build Config"),
        Some(
          "Show the effective project configuration after all templates have been applied. Shows dependencies, scala/java/kotlin version, platform, source layout, test frameworks — everything from bleep.yaml fully expanded. Does NOT include resolved classpaths or compiled output paths. Use bleep.projects for a quick dependency overview instead."
        ),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (args, _) => showBuildConfig(args.projects),
      None
    )

    private def buildResolvedTool: ToolFunction[IO] = ToolFunction.text[IO, BuildArgs](
      ToolFunction.Info(
        "bleep.build.resolved",
        Some("Resolved Build Config"),
        Some(
          "Show the fully resolved project configuration: actual classpath JARs, source directories, compiler JARs, classes output directory, and all compilation inputs. This is what the compiler sees. Requires projects to be compiled first (classpath resolution happens during compilation)."
        ),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (args, _) => showResolvedConfig(args.projects),
      None
    )

    private def projectsTool: ToolFunction[IO] = ToolFunction.text[IO, NoArgs](
      ToolFunction.Info(
        "bleep.projects",
        Some("List Projects"),
        Some("List all projects in the build with their dependencies and whether they are test projects."),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (_, _) => listProjects(),
      None
    )

    private def programsTool: ToolFunction[IO] = ToolFunction.text[IO, NoArgs](
      ToolFunction.Info(
        "bleep.programs",
        Some("List Programs"),
        Some("List projects that have a mainClass defined (runnable programs). Shows project name, main class, and platform."),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (_, _) => listPrograms(),
      None
    )

    private def scriptsTool: ToolFunction[IO] = ToolFunction.text[IO, NoArgs](
      ToolFunction.Info(
        "bleep.scripts",
        Some("List Scripts"),
        Some("List scripts defined in the build. Scripts are named entry points that compile and run a specific main class."),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (_, _) => listScripts(),
      None
    )

    private def runTool: ToolFunction[IO] = ToolFunction.text[IO, RunArgs](
      ToolFunction.Info(
        "bleep.run",
        Some("Run"),
        Some(
          "Compile and run a project or script. Checks scripts first, then projects. Returns stdout/stderr and exit code. Has a timeout to prevent hanging on long-running processes."
        ),
        ToolFunction.Effect.Additive(false),
        false
      ),
      (args, context) => {
        val timeoutSeconds = args.timeoutSeconds.getOrElse(60)
        runProjectOrScript(args.name, args.args, args.mainClass, timeoutSeconds, context)
      },
      None
    )

    private def statusTool: ToolFunction[IO] = ToolFunction.text[IO, StatusArgs](
      ToolFunction.Info(
        "bleep.status",
        Some("Build Status"),
        Some(
          "Show cached results from the last build/test run without re-running. Returns full diagnostics and test results. Use project/limit/offset to paginate large results."
        ),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      (args, _) =>
        buildHistory.get.map { history =>
          history.last match {
            case Some(run) =>
              val events = args.project match {
                case Some(proj) => filterEventsByProject(run.events, proj)
                case None       => run.events
              }
              val mode = run.mode
              if (mode == "test") formatTestResult(events, None, PreviousRunState.empty, true, includeThrowables = true, args.limit, args.offset)
              else formatCompileResult(events, PreviousRunState.empty, true, args.limit, args.offset)
            case None =>
              """{"message":"No previous build results. Run bleep.compile or bleep.test first."}"""
          }
        },
      None
    )

    private def restartTool: ToolFunction[IO] = ToolFunction.text[IO, NoArgs](
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
          started.logger.info("MCP server restart requested, exiting process")
          val daemon = new Thread(() => {
            // flush stdout so the JSON-RPC response reaches the client before we die
            System.out.flush()
            Thread.sleep(1000)
            System.out.flush()
            Runtime.getRuntime.halt(0)
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

    private def resolveProjects(names: List[String]): Array[model.CrossProjectName] =
      if (names.isEmpty) {
        started.chosenProjects(None)
      } else {
        names.flatMap { name =>
          started.globs.projectNameMap.getOrElse(name, Array.empty[model.CrossProjectName])
        }.toArray
      }

    private def resolveTestProjects(names: List[String]): Array[model.CrossProjectName] =
      if (names.isEmpty) {
        started.chosenTestProjects(None)
      } else {
        names.flatMap { name =>
          started.globs.testProjectNameMap.getOrElse(name, Array.empty[model.CrossProjectName])
        }.toArray
      }

    /** Execute a compile via BSP on the persistent connection. Reports progress heartbeat every 30s, returns compact summary. */
    private def executeBspOperation(
        projectNames: List[String],
        verbose: Boolean,
        context: CallContext[IO]
    ): IO[String] = {
      val targetProjects = resolveProjects(projectNames)

      if (targetProjects.isEmpty) {
        return IO.pure("No projects to compile.")
      }

      val originId = UUID.randomUUID().toString

      for {
        previousState <- buildHistory.get.map(_.previousRunState)
        eventQueue <- Queue.unbounded[IO, Option[BleepBspProtocol.Event]]
        collectedEvents <- Ref.of[IO, List[BleepBspProtocol.Event]](Nil)
        done <- Ref.of[IO, Boolean](false)

        // Register event routing for this operation
        _ <- IO.delay(eventRoutes.put(originId, eventQueue))
        _ <- IO.delay(diagnosticRoutes.put(originId, diagnosticCallback(context)))

        // Consumer fiber: filter events, log per-project diffs to MCP, collect for final response
        consumerFiber <- consumeAndLogEvents(eventQueue, collectedEvents, previousState, context).start

        // Heartbeat fiber: report progress every 30s
        heartbeatFiber <- heartbeat(collectedEvents, done, "compile", context).start

        _ <- {
          val targets = BspQuery.buildTargets(started.buildPaths, targetProjects)
          BspRequestHelper
            .callCancellable(
              {
                val params = new bsp4j.CompileParams(targets)
                params.setOriginId(originId)
                bspServer.buildTargetCompile(params)
              },
              bspListening
            )
            .void
        }.guarantee(
          IO.delay(eventRoutes.remove(originId)) >>
            IO.delay(diagnosticRoutes.remove(originId)) >>
            eventQueue.offer(None) >>
            consumerFiber.joinWithNever >>
            done.set(true) >>
            heartbeatFiber.cancel
        )

        events <- collectedEvents.get
        // Push to history
        _ <- buildHistory.update(_.push(BuildRun(System.currentTimeMillis(), "compile", events.reverse)))
      } yield formatCompileResult(events.reverse, previousState, verbose, None, None)
    }

    /** Execute a test via BSP on the persistent connection. Reports progress heartbeat every 30s, returns compact summary. */
    private def executeBspTestOperation(
        projectNames: List[String],
        only: List[String],
        exclude: List[String],
        verbose: Boolean,
        context: CallContext[IO]
    ): IO[String] = {
      val targetProjects = resolveTestProjects(projectNames)

      if (targetProjects.isEmpty) {
        return IO.pure("No test projects found.")
      }

      val originId = UUID.randomUUID().toString

      for {
        previousState <- buildHistory.get.map(_.previousRunState)
        eventQueue <- Queue.unbounded[IO, Option[BleepBspProtocol.Event]]
        collectedEvents <- Ref.of[IO, List[BleepBspProtocol.Event]](Nil)
        done <- Ref.of[IO, Boolean](false)
        testRunResult <- Ref.of[IO, Option[BleepBspProtocol.TestRunResult]](None)

        // Register event routing for this operation
        _ <- IO.delay(eventRoutes.put(originId, eventQueue))
        _ <- IO.delay(diagnosticRoutes.put(originId, diagnosticCallback(context)))

        consumerFiber <- consumeAndLogEvents(eventQueue, collectedEvents, previousState, context).start
        heartbeatFiber <- heartbeat(collectedEvents, done, "test", context).start

        _ <- {
          val targets = BspQuery.buildTargets(started.buildPaths, targetProjects)
          BspRequestHelper
            .callCancellable(
              {
                val params = new bsp4j.TestParams(targets)
                params.setOriginId(originId)
                val testOptions = BleepBspProtocol.TestOptions(Nil, Nil, only, exclude, false)
                params.setDataKind(BleepBspProtocol.TestOptionsDataKind)
                params.setData(com.google.gson.JsonParser.parseString(BleepBspProtocol.TestOptions.encode(testOptions)))
                bspServer.buildTargetTest(params)
              },
              bspListening
            )
            .flatMap { result =>
              // Extract TestRunResult from response
              IO {
                for {
                  dataKind <- Option(result.getDataKind)
                  if dataKind == BleepBspProtocol.TestRunResultDataKind
                  data <- Option(result.getData)
                  jsonStr = data.toString
                  decoded <- BleepBspProtocol.TestRunResult.decode(jsonStr).toOption
                } testRunResult.set(Some(decoded)).unsafeRunSync()(cats.effect.unsafe.implicits.global)
              }
            }
        }.guarantee(
          IO.delay(eventRoutes.remove(originId)) >>
            IO.delay(diagnosticRoutes.remove(originId)) >>
            eventQueue.offer(None) >>
            consumerFiber.joinWithNever >>
            done.set(true) >>
            heartbeatFiber.cancel
        )

        events <- collectedEvents.get
        trr <- testRunResult.get
        // Push to history
        _ <- buildHistory.update(_.push(BuildRun(System.currentTimeMillis(), "test", events.reverse)))
      } yield formatTestResult(events.reverse, trr, previousState, verbose, includeThrowables = verbose, None, None)
    }

    /** Start a background watch job. */
    private def startWatch(
        projectNames: List[String],
        mode: BleepBspProtocol.BuildMode,
        only: List[String],
        exclude: List[String],
        context: CallContext[IO]
    ): IO[String] = {
      val targetProjects = mode match {
        case BleepBspProtocol.BuildMode.Test => resolveTestProjects(projectNames)
        case _                               => resolveProjects(projectNames)
      }
      val jobId = new JobId(UUID.randomUUID().toString.take(8))

      for {
        fiber <- watchLoop(jobId, targetProjects, mode, only, exclude, context).start
        job = WatchJob(jobId, targetProjects.map(_.value).toList, mode, fiber)
        _ <- watchJobs.update(_ + (jobId -> job))
      } yield Json
        .obj(
          "jobId" -> Json.fromString(jobId.value),
          "watching" -> Json.arr(targetProjects.map(p => Json.fromString(p.value)).toList*)
        )
        .noSpaces
    }

    /** Background watch loop: file changes -> recompile -> emit events via persistent BSP connection.
      *
      * If the BSP server dies mid-cycle, logs the error and retries.
      */
    private def watchLoop(
        jobId: JobId,
        targetProjects: Array[model.CrossProjectName],
        mode: BleepBspProtocol.BuildMode,
        only: List[String],
        exclude: List[String],
        context: CallContext[IO]
    ): IO[Unit] = {
      val transitiveProjects = internal.TransitiveProjects(started.build, targetProjects)

      def cycle: IO[Unit] = {
        val originId = UUID.randomUUID().toString

        for {
          previousState <- buildHistory.get.map(_.previousRunState)
          _ <- context.log(protocol.LoggingLevel.Info, s"[${jobId.value}] Compiling...")
          eventQueue <- Queue.unbounded[IO, Option[BleepBspProtocol.Event]]
          collectedEvents <- Ref.of[IO, List[BleepBspProtocol.Event]](Nil)

          // Register event routing for this cycle
          _ <- IO.delay(eventRoutes.put(originId, eventQueue))
          _ <- IO.delay(diagnosticRoutes.put(originId, diagnosticCallback(context)))
          consumerFiber <- consumeAndLogEvents(eventQueue, collectedEvents, previousState, context).start

          _ <- {
            val targets = BspQuery.buildTargets(started.buildPaths, targetProjects)
            mode match {
              case BleepBspProtocol.BuildMode.Test =>
                BspRequestHelper
                  .callCancellable(
                    {
                      val params = new bsp4j.TestParams(targets)
                      params.setOriginId(originId)
                      val testOptions = BleepBspProtocol.TestOptions(Nil, Nil, only, exclude, false)
                      params.setDataKind(BleepBspProtocol.TestOptionsDataKind)
                      params.setData(com.google.gson.JsonParser.parseString(BleepBspProtocol.TestOptions.encode(testOptions)))
                      bspServer.buildTargetTest(params)
                    },
                    bspListening
                  )
                  .void
              case _ =>
                BspRequestHelper
                  .callCancellable(
                    {
                      val params = new bsp4j.CompileParams(targets)
                      params.setOriginId(originId)
                      bspServer.buildTargetCompile(params)
                    },
                    bspListening
                  )
                  .void
            }
          }.guarantee(
            IO.delay(eventRoutes.remove(originId)) >>
              IO.delay(diagnosticRoutes.remove(originId)) >>
              eventQueue.offer(None) >>
              consumerFiber.joinWithNever
          )

          events <- collectedEvents.get
          reversedEvents = events.reverse
          // Push to history
          modeStr = if (mode == BleepBspProtocol.BuildMode.Test) "test" else "compile"
          _ <- buildHistory.update(_.push(BuildRun(System.currentTimeMillis(), modeStr, reversedEvents)))

          summary =
            if (mode == BleepBspProtocol.BuildMode.Test) formatTestResult(reversedEvents, None, previousState, false, includeThrowables = false, None, None)
            else formatCompileResult(reversedEvents, previousState, false, None, None)
          watchMode = new WatchMode(if (mode == BleepBspProtocol.BuildMode.Test) "test" else "compile")
          _ <- watchResults.update(_ + (jobId -> WatchCycleResult(jobId, watchMode, summary, System.currentTimeMillis())))
          _ <- context.log(protocol.LoggingLevel.Info, s"[${jobId.value}] Cycle complete: $summary")
          _ <- notifyWatchResult(jobId, watchMode, summary)

          // Wait for file changes using callback-based pattern.
          // MUST use IO.interruptible so fiber.cancel can interrupt the blocking thread.
          _ <- context.log(protocol.LoggingLevel.Info, s"[${jobId.value}] Watching for file changes...")
          _ <- IO.interruptible {
            val latch = new CountDownLatch(1)
            val watcher = BleepFileWatching.projects(started, transitiveProjects) { _ =>
              latch.countDown()
            }
            val stopAfterChange = new FileWatching.StopWhen {
              override def shouldContinue(): Boolean = latch.getCount > 0
            }
            watcher.run(stopAfterChange, 100)
          }
        } yield ()
      }

      val resilientCycle: IO[Unit] = cycle.handleErrorWith { e =>
        context.log(protocol.LoggingLevel.Error, s"[${jobId.value}] Watch cycle failed: ${e.getClass.getSimpleName}: ${e.getMessage}") >>
          context.log(protocol.LoggingLevel.Info, s"[${jobId.value}] Retrying in 2s...") >>
          IO.sleep(scala.concurrent.duration.FiniteDuration(2, scala.concurrent.duration.SECONDS))
      }

      resilientCycle >> watchLoop(jobId, targetProjects, mode, only, exclude, context)
    }

    /** Stop watch job(s). Cancel with timeout so we never hang. */
    private def stopWatch(jobId: Option[JobId]): IO[String] = {
      val cancelTimeout = scala.concurrent.duration.FiniteDuration(5, scala.concurrent.duration.SECONDS)

      def cancelJob(job: WatchJob): IO[Unit] =
        job.cancel.timeoutTo(cancelTimeout, IO.unit)

      jobId match {
        case Some(id) =>
          watchJobs.modify { jobs =>
            jobs.get(id) match {
              case Some(job) =>
                (jobs - id, cancelJob(job) >> watchResults.update(_ - id).as(s"""{"stopped":"${id.value}"}"""))
              case None => (jobs, IO.pure(s"""{"error":"no such job: ${id.value}"}"""))
            }
          }.flatten
        case None =>
          watchJobs.modify { jobs =>
            val cancelAll = jobs.values.toList.traverse_(cancelJob)
            val count = jobs.size
            (Map.empty, (cancelAll >> watchResults.set(Map.empty)).as(s"""{"stopped":$count}"""))
          }.flatten
      }
    }

    /** Push watch results to the agent via sampling (if supported), otherwise just log. */
    private def notifyWatchResult(jobId: JobId, mode: WatchMode, summary: String): IO[Unit] = {
      val hasSampling = _client.capabilities.sampling.isDefined
      if (hasSampling) {
        val message = protocol.Sampling.Message(
          protocol.Role.User,
          protocol.Content.Text(
            s"[bleep watch ${jobId.value}] ${mode.value} cycle complete. Results:\n$summary\n\nAcknowledge these results briefly and take action if there are errors."
          )
        )
        _client
          .sample(List(message), 256)
          .void
          .handleErrorWith { e =>
            IO(started.logger.warn(s"Sampling failed (client may not support it): ${e.getMessage}"))
          }
      } else {
        IO.unit // Already logged via context.log above
      }
    }

    /** List all projects with dependencies. */
    /** Show effective project configuration after templates have been applied. */
    private def showBuildConfig(projectNames: List[String]): IO[String] = IO {
      val projects = if (projectNames.isEmpty) {
        started.build.explodedProjects.toList
      } else {
        val resolved = resolveProjects(projectNames)
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
    private def showResolvedConfig(projectNames: List[String]): IO[String] = IO {
      val crossNames = if (projectNames.isEmpty) {
        started.build.explodedProjects.keys.toList.sorted
      } else {
        resolveProjects(projectNames).toList
      }
      val entries = crossNames.flatMap { cpn =>
        started.resolvedProjects.get(cpn).map { lazyResolved =>
          val rp = lazyResolved.forceGet
          cpn.value -> io.circe.Encoder[ResolvedProject].apply(rp)
        }
      }
      Json.obj(entries*).noSpaces
    }

    private def listProjects(): IO[String] = IO {
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
    private def discoverTestSuites(
        projectNames: List[String]
    ): IO[String] = {
      val targetProjects = resolveTestProjects(projectNames)

      if (targetProjects.isEmpty) {
        return IO.pure("""{"projects":[]}""")
      }

      for {
        result <- {
          val targets = BspQuery.buildTargets(started.buildPaths, targetProjects)
          BspRequestHelper.callCancellable(
            bspServer.buildTargetScalaTestClasses(new bsp4j.ScalaTestClassesParams(targets)),
            bspListening
          )
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
    private def listPrograms(): IO[String] = IO {
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
    private def listScripts(): IO[String] = IO {
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
              runProject(project, Some(main), args, timeoutSeconds, context)
            case _ =>
              IO.raiseError(new BleepException.Text(s"Script '$name' has no main class definition"))
          }
        case None =>
          started.globs.exactProjectMap.get(name) match {
            case Some(projectName) =>
              runProject(projectName, mainClassOverride, args, timeoutSeconds, context)
            case None =>
              IO.raiseError(new BleepException.Text(s"'$name' is not a valid project or script name"))
          }
      }
    }

    /** Compile and run a single project. Captures stdout/stderr. */
    private def runProject(
        project: model.CrossProjectName,
        mainClassOverride: Option[String],
        args: List[String],
        timeoutSeconds: Int,
        context: CallContext[IO]
    ): IO[String] =
      for {
        // Compile first
        _ <- context.log(protocol.LoggingLevel.Info, s"Compiling ${project.value}...")
        _ <- compileSilently(Array(project))

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
    private def compileSilently(
        targetProjects: Array[model.CrossProjectName]
    ): IO[Unit] = {
      val targets = BspQuery.buildTargets(started.buildPaths, targetProjects)
      BspRequestHelper
        .callCancellable(
          {
            val params = new bsp4j.CompileParams(targets)
            bspServer.buildTargetCompile(params)
          },
          bspListening
        )
        .flatMap { result =>
          IO.raiseWhen(result.getStatusCode != bsp4j.StatusCode.OK)(
            new BleepException.Text(s"Compilation failed with status ${result.getStatusCode}")
          )
        }
    }

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

    /** Consume BSP events: collect all for the final response, and stream per-project diff lines.
      *
      * On CompileFinished: diffs against previous run's diagnostics for that project, emits a terse line. On SuiteFinished: diffs against previous run's test
      * results. Failures are always streamed. Successes get terse diff lines instead of silence.
      */
    private def consumeAndLogEvents(
        eventQueue: Queue[IO, Option[BleepBspProtocol.Event]],
        collectedEvents: Ref[IO, List[BleepBspProtocol.Event]],
        previousState: PreviousRunState,
        context: CallContext[IO]
    ): IO[Unit] =
      eventQueue.take.flatMap {
        case Some(event) =>
          val logIO = streamDiffLine(event, previousState, collectedEvents, context)
          logIO >>
            collectedEvents.update(event :: _) >>
            consumeAndLogEvents(eventQueue, collectedEvents, previousState, context)
        case None => IO.unit
      }

    /** Log via all available channels: notifications/message (model), notifications/progress (Claude Code UI), stderr (fallback). */
    private def streamNotification(context: CallContext[IO], level: protocol.LoggingLevel, message: String): IO[Unit] =
      context.reportProgress(0.0, None, Some(message)).attempt >>
        IO(System.err.println(message)) >>
        context.log(level, message)

    /** Periodic heartbeat that reports build progress every 30 seconds. */
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
              val failed = finished.count(_.status == CompileStatus.Failed)
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

    /** Stream per-project progress as compact one-liners. Errors/failures stream immediately so the agent can react without waiting for the full build. */
    private def streamDiffLine(
        event: BleepBspProtocol.Event,
        previousState: PreviousRunState,
        collectedEvents: Ref[IO, List[BleepBspProtocol.Event]],
        context: CallContext[IO]
    ): IO[Unit] = {
      import BleepBspProtocol.{Event => E}
      event match {
        case e: E.CompileFinished =>
          val errorCount = e.diagnostics.count(_.severity == DiagnosticSeverity.Error)
          val previousDiags = previousState.compileDiagnostics.getOrElse(e.project, Nil)
          val hasPrevious = previousState.compileDiagnostics.contains(e.project)

          if (e.status == CompileStatus.Failed) {
            // Stream failures immediately — agent needs to know ASAP
            if (hasPrevious) {
              val diff = BuildDiff.diffCompile(e.project, e.status, e.diagnostics, previousDiags, e.durationMs)
              streamNotification(context, protocol.LoggingLevel.Error, BuildDiff.formatCompileDiff(diff))
            } else {
              // First run, no diff baseline — report error count and first few messages
              val firstErrors = e.diagnostics.filter(_.severity == DiagnosticSeverity.Error).take(3).map(d => stripAnsi(d.message))
              val moreStr = if (errorCount > 3) s" (+${errorCount - 3} more)" else ""
              streamNotification(
                context,
                protocol.LoggingLevel.Error,
                s"${e.project}: $errorCount errors (${e.durationMs}ms). ${firstErrors.mkString("; ")}$moreStr"
              )
            }
          } else if (hasPrevious) {
            val diff = BuildDiff.diffCompile(e.project, e.status, e.diagnostics, previousDiags, e.durationMs)
            if (diff.fixedErrors > 0 || diff.newErrors > 0) {
              streamNotification(context, protocol.LoggingLevel.Info, BuildDiff.formatCompileDiff(diff))
            } else IO.unit
          } else IO.unit

        case e: E.SuiteFinished =>
          // Always stream suite results — compact one-liner with diff if available
          collectedEvents.get.flatMap { collected =>
            val suiteTests = collected.collect {
              case tf: E.TestFinished if tf.project == e.project && tf.suite == e.suite => tf
            }
            val newFailures = suiteTests.filter { tf =>
              val key = TestKey(tf.project, tf.suite, tf.test)
              val prev = previousState.testResults.get(key)
              val prevFailed = prev.exists(_.isFailure)
              tf.status.isFailure && !prevFailed
            }
            val fixedTests = suiteTests.filter { tf =>
              val key = TestKey(tf.project, tf.suite, tf.test)
              val prev = previousState.testResults.get(key)
              val prevFailed = prev.exists(_.isFailure)
              prevFailed && !tf.status.isFailure
            }

            // Stream if there are failures or diffs
            if (e.failed > 0 || newFailures.nonEmpty || fixedTests.nonEmpty) {
              val diffParts = List.newBuilder[String]
              fixedTests.foreach(t => diffParts += s"${t.test.value} fixed")
              newFailures.foreach(t => diffParts += s"${t.test.value} new failure")
              val details = diffParts.result()
              val countsStr = s"${e.passed} passed, ${e.failed} failed"
              val detailStr = if (details.nonEmpty) s" (${details.mkString(", ")})" else ""
              val level = if (e.failed > 0) protocol.LoggingLevel.Error else protocol.LoggingLevel.Info
              streamNotification(context, level, s"${e.project.value} ${e.suite.value}: $countsStr$detailStr")
            } else IO.unit
          }

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

    // ========================================================================
    // Diagnostic streaming
    // ========================================================================

    /** Diagnostic callback — no-op since we stream errors per-project via CompileFinished events in streamDiffLine. Per-diagnostic streaming was too verbose (N
      * individual JSON objects flooding the agent's context).
      */
    private def diagnosticCallback(context: CallContext[IO]): bsp4j.PublishDiagnosticsParams => Unit = _ => ()

    // ========================================================================
    // Filtering
    // ========================================================================

    /** Keep only events that belong to the given project. Events without a project field (e.g. SourcegenStarted) are dropped. */
    private def filterEventsByProject(events: List[BleepBspProtocol.Event], project: String): List[BleepBspProtocol.Event] = {
      import BleepBspProtocol.{Event => E}
      events.filter {
        case e: E.CompileStarted      => e.project.value == project
        case e: E.CompilationReason   => e.project.value == project
        case e: E.CompileProgress     => e.project.value == project
        case e: E.CompilePhaseChanged => e.project.value == project
        case e: E.CompileFinished     => e.project.value == project
        case e: E.CompileStalled      => e.project.value == project
        case e: E.CompileResumed      => e.project.value == project
        case e: E.LockContention      => e.project.value == project
        case e: E.LockAcquired        => e.project.value == project
        case e: E.LinkStarted         => e.project.value == project
        case e: E.LinkProgress        => e.project.value == project
        case e: E.LinkFinished        => e.project.value == project
        case e: E.DiscoveryStarted    => e.project.value == project
        case e: E.SuitesDiscovered    => e.project.value == project
        case e: E.SuiteStarted        => e.project.value == project
        case e: E.TestStarted         => e.project.value == project
        case e: E.TestFinished        => e.project.value == project
        case e: E.SuiteFinished       => e.project.value == project
        case _                        => false
      }
    }

    // ========================================================================
    // Formatting
    // ========================================================================

    /** Format compile result from protocol events. In diff mode (verbose=false with previous state), returns a terse diff summary. In verbose mode, returns
      * full diagnostics. When limit/offset are provided, the diagnostics array is sliced and a totalDiagnostics count is included.
      */
    private def formatCompileResult(
        events: List[BleepBspProtocol.Event],
        previousState: PreviousRunState,
        verbose: Boolean,
        limit: Option[Int],
        offset: Option[Int]
    ): String = {
      import BleepBspProtocol.{Event => E}

      val compileEvents = events.collect { case e: E.CompileFinished => e }
      val failedProjects = compileEvents.filter(_.status == CompileStatus.Failed)
      val allDiagnostics = compileEvents.flatMap(_.diagnostics)
      val errorCount = allDiagnostics.count(_.severity == DiagnosticSeverity.Error)
      val warningCount = allDiagnostics.count(_.severity == DiagnosticSeverity.Warning)
      val success = failedProjects.isEmpty

      if (verbose) {
        // Explicit verbose request — full diagnostics
        val allDiagnosticJsons = allDiagnostics.map { d =>
          val fields = List.newBuilder[(String, Json)]
          fields += "severity" -> Json.fromString(d.severity.wireValue)
          fields += "message" -> Json.fromString(stripAnsi(d.message))
          d.rendered.foreach(r => fields += "rendered" -> Json.fromString(stripAnsi(r)))
          d.path.foreach(p => fields += "path" -> Json.fromString(p))
          Json.obj(fields.result()*)
        }
        val totalDiagnostics = allDiagnosticJsons.size
        val sliced = {
          val afterOffset = offset.map(o => allDiagnosticJsons.drop(o)).getOrElse(allDiagnosticJsons)
          limit.map(l => afterOffset.take(l)).getOrElse(afterOffset)
        }
        val resultFields = List.newBuilder[(String, Json)]
        resultFields += "success" -> Json.fromBoolean(success)
        resultFields += "errors" -> Json.fromInt(errorCount)
        resultFields += "warnings" -> Json.fromInt(warningCount)
        resultFields += "totalDiagnostics" -> Json.fromInt(totalDiagnostics)
        resultFields += "diagnostics" -> Json.arr(sliced*)
        Json.obj(resultFields.result()*).noSpaces
      } else {
        // Compact summary — always. Agent uses bleep.status for details.
        val hasPrevious = previousState.compileDiagnostics.nonEmpty

        val fields = List.newBuilder[(String, Json)]
        fields += "success" -> Json.fromBoolean(success)
        fields += "errors" -> Json.fromInt(errorCount)
        fields += "warnings" -> Json.fromInt(warningCount)

        if (hasPrevious) {
          var totalNew = 0
          var totalFixed = 0
          compileEvents.foreach { e =>
            val prev = previousState.compileDiagnostics.getOrElse(e.project, Nil)
            val diff = BuildDiff.diffCompile(e.project, e.status, e.diagnostics, prev, e.durationMs)
            totalNew += diff.newErrors
            totalFixed += diff.fixedErrors
          }
          previousState.compileDiagnostics.keys.foreach { project =>
            if (!compileEvents.exists(_.project == project)) {
              val prev = previousState.compileDiagnostics(project)
              totalFixed += prev.count(_.severity == DiagnosticSeverity.Error)
            }
          }
          fields += "newErrors" -> Json.fromInt(totalNew)
          fields += "fixedErrors" -> Json.fromInt(totalFixed)
          fields += "summary" -> Json.fromString(BuildDiff.formatCompileSummary(compileEvents.size, errorCount, warningCount, totalNew, totalFixed))
        } else {
          val summaryParts = List.newBuilder[String]
          if (success) summaryParts += s"Build succeeded (${compileEvents.size} projects)"
          else summaryParts += s"Build failed: $errorCount errors in ${failedProjects.map(_.project).distinct.size} projects"
          if (warningCount > 0) summaryParts += s"$warningCount warnings"
          if (!success) summaryParts += "Use bleep.status for error details"
          fields += "summary" -> Json.fromString(summaryParts.result().mkString(". "))
        }

        if (failedProjects.nonEmpty) {
          fields += "failedProjects" -> Json.arr(failedProjects.map(_.project.value).distinct.map(Json.fromString)*)
          // Always include first 3 errors so the agent has something actionable
          val topErrors = allDiagnostics.filter(_.severity == DiagnosticSeverity.Error).take(3).map { d =>
            val df = List.newBuilder[(String, Json)]
            df += "message" -> Json.fromString(stripAnsi(d.message))
            d.path.foreach(p => df += "path" -> Json.fromString(p))
            Json.obj(df.result()*)
          }
          fields += "topErrors" -> Json.arr(topErrors*)
        }

        Json.obj(fields.result()*).noSpaces
      }
    }

    /** Format test result from protocol events. Always compact; verbose=true for full failure details. When limit/offset are provided, the failures array is
      * sliced and a totalFailures count is included. Summary counts (passed/failed) always reflect the full run.
      */
    private def formatTestResult(
        events: List[BleepBspProtocol.Event],
        testRunResult: Option[BleepBspProtocol.TestRunResult],
        previousState: PreviousRunState,
        verbose: Boolean,
        includeThrowables: Boolean,
        limit: Option[Int],
        offset: Option[Int]
    ): String = {
      import BleepBspProtocol.{Event => E}

      val testEvents = events.collect { case e: E.TestFinished => e }
      val failedTests = testEvents.filter(e => e.status.isFailure)
      val hasPrevious = previousState.testResults.nonEmpty

      val passed = testRunResult.map(_.totalPassed).getOrElse(testEvents.count(_.status == TestStatus.Passed))
      val failed = testRunResult.map(_.totalFailed).getOrElse(failedTests.size)
      val durationMs = testRunResult.map(_.durationMs)

      val fields = List.newBuilder[(String, Json)]
      fields += "success" -> Json.fromBoolean(failed == 0)
      fields += "passed" -> Json.fromInt(passed)
      fields += "failed" -> Json.fromInt(failed)
      testRunResult.foreach { trr =>
        fields += "skipped" -> Json.fromInt(trr.totalSkipped)
        fields += "ignored" -> Json.fromInt(trr.totalIgnored)
      }
      durationMs.foreach(d => fields += "durationMs" -> Json.fromLong(d))

      if (hasPrevious) {
        val newFailures = failedTests.count { tf =>
          val key = TestKey(tf.project, tf.suite, tf.test)
          !previousState.testResults.get(key).exists(_.isFailure)
        }
        val fixedTests = previousState.testResults.count { case (key, prev) =>
          val prevFailed = prev.isFailure
          val currentResult = testEvents.find(t => TestKey(t.project, t.suite, t.test) == key)
          prevFailed && currentResult.exists(t => !t.status.isFailure)
        }
        fields += "newFailures" -> Json.fromInt(newFailures)
        fields += "fixedTests" -> Json.fromInt(fixedTests)
        fields += "summary" -> Json.fromString(BuildDiff.formatTestSummary(passed, failed, newFailures, fixedTests))
      } else {
        val summaryParts = List.newBuilder[String]
        if (failed == 0) summaryParts += s"All $passed tests passed"
        else summaryParts += s"$failed tests failed, $passed passed"
        if (failed > 0) summaryParts += "Use bleep.status for failure details"
        fields += "summary" -> Json.fromString(summaryParts.result().mkString(". "))
      }

      // Include failure details: always include message, never inline full stack traces
      if (failedTests.nonEmpty) {
        val totalFailures = failedTests.size
        fields += "totalFailures" -> Json.fromInt(totalFailures)
        val slicedFailures = {
          val afterOffset = offset.map(o => failedTests.drop(o)).getOrElse(failedTests)
          limit.map(l => afterOffset.take(l)).getOrElse(afterOffset)
        }
        val failureJsons = slicedFailures.map { e =>
          val df = List.newBuilder[(String, Json)]
          df += "project" -> Json.fromString(e.project.value)
          df += "suite" -> Json.fromString(e.suite.value)
          df += "test" -> Json.fromString(e.test.value)
          df += "status" -> Json.fromString(e.status.wireValue)
          e.message.foreach(m => df += "message" -> Json.fromString(stripAnsi(m)))
          e.throwable.foreach { t =>
            if (includeThrowables) df += "throwable" -> Json.fromString(stripAnsi(t))
            else df += "throwable" -> Json.fromString("present. Use bleep.status for full stack trace")
          }
          Json.obj(df.result()*)
        }
        fields += "failures" -> Json.arr(failureJsons*)
      }

      Json.obj(fields.result()*).noSpaces
    }

  }
}

object BleepMcpServer {
  def apply(initialStarted: Started): BleepMcpServer = new BleepMcpServer(initialStarted)
}

/** Type-safe wrapper for watch job IDs. */
private[mcp] class JobId(val value: String) extends AnyVal

/** Type-safe wrapper for watch mode strings (compile/test). */
private[mcp] class WatchMode(val value: String) extends AnyVal

/** Tracks a background watch job. */
private[mcp] case class WatchJob(
    jobId: JobId,
    projects: List[String],
    mode: BleepBspProtocol.BuildMode,
    fiber: FiberIO[Unit]
) {
  def cancel: IO[Unit] = fiber.cancel
}

/** Result from the last watch cycle, stored for bleep.sync polling. */
private[mcp] case class WatchCycleResult(
    jobId: JobId,
    mode: WatchMode,
    summary: String,
    timestampMs: Long
)

/** BSP client that captures BleepBspProtocol events into a queue and streams diagnostics immediately. */
/** Shared BSP client for the persistent MCP-to-BSP connection.
  *
  * Routes events by `originId` to the correct tool call's event queue. Each concurrent BSP operation registers its queue in `eventRoutes` and sets `originId`
  * on the BSP request params. The BSP server echoes `originId` back on all progress events, enabling demux.
  */
private[mcp] class SharedMcpBspClient(
    eventRoutes: ConcurrentHashMap[String, Queue[IO, Option[BleepBspProtocol.Event]]],
    diagnosticRoutes: ConcurrentHashMap[String, bsp4j.PublishDiagnosticsParams => Unit],
    logger: ryddig.Logger
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
          case Right(event) =>
            val originId = Option(params.getOriginId)
            val targetQueue = originId.flatMap(id => Option(eventRoutes.get(id)))
            targetQueue match {
              case Some(queue) =>
                queue.offer(Some(event)).unsafeRunSync()(cats.effect.unsafe.implicits.global)
              case None =>
                // No specific route — broadcast to all active consumers
                eventRoutes.values().forEach { queue =>
                  queue.offer(Some(event)).unsafeRunSync()(cats.effect.unsafe.implicits.global)
                }
            }
          case Left(_) => ()
        }
      }
    }
    delegate.onBuildTaskProgress(params)
  }

  override def onBuildTaskFinish(params: bsp4j.TaskFinishParams): Unit =
    delegate.onBuildTaskFinish(params)

  override def onBuildPublishDiagnostics(params: bsp4j.PublishDiagnosticsParams): Unit = {
    val originId = Option(params.getOriginId)
    val targetCallback = originId.flatMap(id => Option(diagnosticRoutes.get(id)))
    targetCallback match {
      case Some(callback) => callback(params)
      case None =>
        // Broadcast to all active diagnostic consumers
        diagnosticRoutes.values().forEach(_(params))
    }
    delegate.onBuildPublishDiagnostics(params)
  }

  override def onBuildTargetDidChange(params: bsp4j.DidChangeBuildTarget): Unit =
    delegate.onBuildTargetDidChange(params)

  override def onRunPrintStdout(params: bsp4j.PrintParams): Unit =
    delegate.onRunPrintStdout(params)

  override def onRunPrintStderr(params: bsp4j.PrintParams): Unit =
    delegate.onRunPrintStderr(params)
}
