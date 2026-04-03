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
import io.circe.Json
import _root_.mcp.protocol.{Content, Implementation, LoggingLevel, ToolAnnotations}
import _root_.mcp.server.{McpServer, ResourceContext, ResourceDef, TaskMode, ToolContext, ToolDef}

import java.nio.file.Files
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference
import scala.jdk.CollectionConverters.*

/** Strip ANSI escape sequences from text. */
private[mcp] val AnsiPattern = java.util.regex.Pattern.compile("\u001b\\[[0-9;]*[a-zA-Z]")
private[mcp] def stripAnsi(s: String): String = AnsiPattern.matcher(s).replaceAll("")

/** MCP server for bleep that exposes compile, test, and project info to AI agents.
  *
  * Connects to the bleep-bsp daemon and translates BSP events into MCP tool results and notifications. Runs on stdio transport.
  *
  * Build operations run as async tasks — the client gets a task ID back immediately and receives progress updates. File watching is exposed
  * as a resource with an updates stream.
  */
object BleepMcpServer {

  def create(initialStarted: Started): Resource[IO, McpServer[IO]] = {
    val startedRef = new AtomicReference(initialStarted)
    def started: Started = startedRef.get()

    for {
      bspConfig <- Resource.eval(IO.fromEither(setupBspConfig(started)))

      _ <- Resource.eval(BspRifle.ensureRunning(bspConfig, started.logger))
      connection <- BspRifle.connectWithRetry(bspConfig, started.logger)
      eventRoutes = new ConcurrentHashMap[String, Queue[IO, Option[BleepBspProtocol.Event]]]()
      diagnosticRoutes = new ConcurrentHashMap[String, bsp4j.PublishDiagnosticsParams => Unit]()
      sharedClient = new SharedMcpBspClient(eventRoutes, diagnosticRoutes, started.logger)
      lifecycle <- BspServerBuilder.create(connection, sharedClient)
      _ <- Resource.eval(
        BspServerBuilder.initializeSession(
          server = lifecycle.server,
          clientName = "bleep-mcp",
          clientVersion = model.BleepVersion.current.value,
          rootUri = started.buildPaths.buildDir.toUri.toString,
          buildData = None,
          listening = lifecycle.listening
        )
      )

      buildHistory <- Resource.eval(Ref.of[IO, BuildHistory](BuildHistory.empty))

      // Queues to bridge file-watcher callbacks into fs2 streams
      bleepYamlChangeQueue <- Resource.eval(Queue.unbounded[IO, Unit])
      sourceChangeQueue <- Resource.eval(Queue.unbounded[IO, Unit])

      // Start bleep.yaml watcher on daemon thread
      _ <- Resource.make(IO {
        val watcher = BleepFileWatching.build(started.pre) { changedFiles =>
          started.reloadFromDisk() match {
            case Left(ex) =>
              started.logger.error(s"Build file changed (${changedFiles.mkString(", ")}), but reload failed: ${ex.getMessage}")
            case Right(None) =>
              () // parsed JSON identical, no actual change
            case Right(Some(newStarted)) =>
              startedRef.set(newStarted)
              newStarted.logger.info(s"Build reloaded (${changedFiles.mkString(", ")})")
              bleepYamlChangeQueue.offer(()).unsafeRunSync()(cats.effect.unsafe.implicits.global)
          }
        }
        val thread = new Thread(() => watcher.run(FileWatching.StopWhen.Never), "mcp-bleep-yaml-watcher")
        thread.setDaemon(true)
        thread.start()
        watcher
      })(watcher => IO(watcher.close()))

      // Start source file watcher on daemon thread
      _ <- Resource.make(IO {
        val allProjects = started.build.explodedProjects.keys.toArray
        val transitiveProjects = internal.TransitiveProjects(started.build, allProjects)
        val watcher = BleepFileWatching.projects(started, transitiveProjects) { _ =>
          sourceChangeQueue.offer(()).unsafeRunSync()(cats.effect.unsafe.implicits.global)
        }
        val thread = new Thread(() => watcher.run(FileWatching.StopWhen.Never), "mcp-source-watcher")
        thread.setDaemon(true)
        thread.start()
        watcher
      })(watcher => IO(watcher.close()))

      toolState = ToolState(
        startedRef = startedRef,
        bspConfig = bspConfig,
        bspServer = lifecycle.server,
        bspListening = lifecycle.listening,
        eventRoutes = eventRoutes,
        diagnosticRoutes = diagnosticRoutes,
        buildHistory = buildHistory
      )

      tools = createTools(toolState)
      resources = createResources(() => started, bleepYamlChangeQueue, sourceChangeQueue)

      server <- McpServer[IO](
        info = Implementation("bleep", model.BleepVersion.current.value),
        instructions = Some(instructions),
        tools = tools,
        resources = resources,
        tasksEnabled = true
      )
    } yield server
  }

  private val instructions: String =
    """Bleep build tool MCP server.
      |
      |Compile and test responses are compact summaries (error/warning counts, diff against previous run).
      |Errors stream as log notifications during the build so you see failures immediately.
      |For full diagnostics, call bleep.status after a build completes.
      |Use verbose=true on compile/test only when you need every diagnostic in the response.
      |
      |Build operations (compile, test, etc.) run as async tasks — you get a task ID back immediately and receive progress updates.
      |Subscribe to bleep://watch for source file change notifications, then call bleep.compile or bleep.test to rebuild.""".stripMargin

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
          extraServerClasspath = Seq.empty
        )
      case _: bsp.BspServerClasspathSource.InProcess =>
        Left(new BleepException.Text("MCP server does not support in-process BSP mode"))
    }

  // ========================================================================
  // Tool definitions
  // ========================================================================

  private def createTools(s: ToolState): List[ToolDef[IO, ?, ?]] =
    List(
      compileTool(s),
      testTool(s),
      testSuitesTool(s),
      sourcegenTool(s),
      fmtTool(s),
      cleanTool(s),
      buildTool(s),
      buildResolvedTool(s),
      projectsTool(s),
      programsTool(s),
      scriptsTool(s),
      runTool(s),
      statusTool(s),
      restartTool(s)
    )

  private def textResult(text: String): List[Content] = List(Content.Text(text))

  // --- Async tasks (long-running build operations) ---

  private def compileTool(s: ToolState): ToolDef[IO, ProjectsInput, Nothing] = ToolDef.unstructured[IO, ProjectsInput](
    name = "bleep.compile",
    description = Some(
      "Compile bleep projects. Returns compact summary (error counts, diff). Errors stream per-project as they finish. Call bleep.status for full diagnostic details. Use verbose=true only when you need every diagnostic in the response."
    ),
    taskMode = TaskMode.AsyncAllowed,
    annotations = Some(ToolAnnotations(title = Some("Compile"), readOnlyHint = Some(true), openWorldHint = Some(false)))
  ) { (input, ctx) =>
    executeBspOperation(s, input.projects, input.verbose, ctx).map(textResult)
  }

  private def testTool(s: ToolState): ToolDef[IO, TestInput, Nothing] = ToolDef.unstructured[IO, TestInput](
    name = "bleep.test",
    description = Some(
      "Run tests for bleep projects. Returns compact summary (pass/fail counts, diff). Failures stream as they occur. Call bleep.status for full details. Use verbose=true only when you need full failure messages/stacktraces."
    ),
    taskMode = TaskMode.AsyncAllowed,
    annotations = Some(ToolAnnotations(title = Some("Test"), readOnlyHint = Some(true), openWorldHint = Some(false)))
  ) { (input, ctx) =>
    executeBspTestOperation(s, input.projects, input.only, input.exclude, input.verbose, ctx)
      .map(textResult)
  }

  private def sourcegenTool(s: ToolState): ToolDef[IO, ProjectsInput, Nothing] = ToolDef.unstructured[IO, ProjectsInput](
    name = "bleep.sourcegen",
    description = Some("Run source generators for bleep projects. Only affects projects that have sourcegen scripts defined."),
    taskMode = TaskMode.AsyncAllowed,
    annotations = Some(ToolAnnotations(title = Some("Source Generate"), destructiveHint = Some(false), openWorldHint = Some(false)))
  ) { (input, _) =>
    val started = s.started
    val allProjects = resolveProjects(started, input.projects)
    val sourcegenProjects = allProjects.filter { crossName =>
      started.build.explodedProjects(crossName).sourcegen.values.nonEmpty
    }
    if (sourcegenProjects.isEmpty) {
      IO.pure(textResult("""{"success":true,"message":"No projects with sourcegen scripts found."}"""))
    } else {
      IO.fromEither(commands.SourceGen(false, sourcegenProjects).run(started))
        .as(
          textResult(
            Json
              .obj(
                "success" -> Json.fromBoolean(true),
                "projects" -> Json.arr(sourcegenProjects.map(p => Json.fromString(p.value)).toList*)
              )
              .noSpaces
          )
        )
    }
  }

  private def fmtTool(s: ToolState): ToolDef[IO, ProjectsInput, Nothing] = ToolDef.unstructured[IO, ProjectsInput](
    name = "bleep.fmt",
    description = Some("Format Scala and Java source files using scalafmt and google-java-format. Optionally limit to specific projects."),
    taskMode = TaskMode.AsyncAllowed,
    annotations = Some(ToolAnnotations(title = Some("Format"), destructiveHint = Some(false), openWorldHint = Some(false)))
  ) { (input, _) =>
    val projects = resolveProjects(s.started, input.projects)
    IO.fromEither(commands.Fmt(check = false, projects = projects).run(s.started))
      .as(
        textResult(
          Json.obj("success" -> Json.fromBoolean(true), "projects" -> Json.fromInt(projects.length)).noSpaces
        )
      )
  }

  private def cleanTool(s: ToolState): ToolDef[IO, ProjectsInput, Nothing] = ToolDef.unstructured[IO, ProjectsInput](
    name = "bleep.clean",
    description = Some("Delete build outputs for bleep projects. Removes compiled classes and other build artifacts."),
    taskMode = TaskMode.AsyncAllowed,
    annotations = Some(ToolAnnotations(title = Some("Clean"), destructiveHint = Some(true), openWorldHint = Some(false)))
  ) { (input, _) =>
    val projects = resolveProjects(s.started, input.projects)
    if (projects.isEmpty) {
      IO.pure(textResult("""{"success":true,"message":"No projects to clean."}"""))
    } else {
      for {
        _ <- IO.fromEither(commands.Clean(projects).run(s.started))
        projectNames = projects.map(_.value).toSet
        _ <- s.buildHistory.update(_.dropProjects(projectNames))
      } yield textResult(
        Json
          .obj(
            "success" -> Json.fromBoolean(true),
            "projects" -> Json.arr(projects.map(p => Json.fromString(p.value)).toList*)
          )
          .noSpaces
      )
    }
  }

  private def runTool(s: ToolState): ToolDef[IO, RunInput, Nothing] = ToolDef.unstructured[IO, RunInput](
    name = "bleep.run",
    description = Some(
      "Compile and run a project or script. Checks scripts first, then projects. Returns stdout/stderr and exit code. Has a timeout to prevent hanging on long-running processes."
    ),
    taskMode = TaskMode.AsyncAllowed,
    annotations = Some(ToolAnnotations(title = Some("Run"), destructiveHint = Some(false), openWorldHint = Some(false)))
  ) { (input, ctx) =>
    val timeoutSeconds = input.timeoutSeconds.getOrElse(60)
    runProjectOrScript(s, input.name, input.args, input.mainClass, timeoutSeconds, ctx).map(textResult)
  }

  // --- Sync tools (instant lookups) ---

  private def testSuitesTool(s: ToolState): ToolDef[IO, ProjectsInput, Nothing] = ToolDef.unstructured[IO, ProjectsInput](
    name = "bleep.test.suites",
    description = Some(
      "Discover test suites in compiled test projects without running them. Projects must be compiled first. Returns test class names grouped by project."
    ),
    taskMode = TaskMode.AsyncAllowed,
    annotations = Some(ToolAnnotations(title = Some("Test Suites"), readOnlyHint = Some(true), openWorldHint = Some(false)))
  ) { (input, _) =>
    discoverTestSuites(s, input.projects).map(textResult)
  }

  private def buildTool(s: ToolState): ToolDef[IO, BuildInput, Nothing] = ToolDef.unstructured[IO, BuildInput](
    name = "bleep.build.effective",
    description = Some(
      "Show the effective project configuration after all templates have been applied. Shows dependencies, scala/java/kotlin version, platform, source layout, test frameworks — everything from bleep.yaml fully expanded. Does NOT include resolved classpaths or compiled output paths. Use bleep.projects for a quick dependency overview instead."
    ),
    annotations = Some(ToolAnnotations(title = Some("Effective Build Config"), readOnlyHint = Some(true), openWorldHint = Some(false)))
  ) { (input, _) =>
    showBuildConfig(s, input.projects).map(textResult)
  }

  private def buildResolvedTool(s: ToolState): ToolDef[IO, BuildInput, Nothing] = ToolDef.unstructured[IO, BuildInput](
    name = "bleep.build.resolved",
    description = Some(
      "Show the fully resolved project configuration: actual classpath JARs, source directories, compiler JARs, classes output directory, and all compilation inputs. This is what the compiler sees. Requires projects to be compiled first (classpath resolution happens during compilation)."
    ),
    annotations = Some(ToolAnnotations(title = Some("Resolved Build Config"), readOnlyHint = Some(true), openWorldHint = Some(false)))
  ) { (input, _) =>
    showResolvedConfig(s, input.projects).map(textResult)
  }

  private def projectsTool(s: ToolState): ToolDef[IO, NoInput, Nothing] = ToolDef.unstructured[IO, NoInput](
    name = "bleep.projects",
    description = Some("List all projects in the build with their dependencies and whether they are test projects."),
    annotations = Some(ToolAnnotations(title = Some("List Projects"), readOnlyHint = Some(true), openWorldHint = Some(false)))
  ) { (_, _) =>
    listProjects(s).map(textResult)
  }

  private def programsTool(s: ToolState): ToolDef[IO, NoInput, Nothing] = ToolDef.unstructured[IO, NoInput](
    name = "bleep.programs",
    description = Some("List projects that have a mainClass defined (runnable programs). Shows project name, main class, and platform."),
    annotations = Some(ToolAnnotations(title = Some("List Programs"), readOnlyHint = Some(true), openWorldHint = Some(false)))
  ) { (_, _) =>
    listPrograms(s).map(textResult)
  }

  private def scriptsTool(s: ToolState): ToolDef[IO, NoInput, Nothing] = ToolDef.unstructured[IO, NoInput](
    name = "bleep.scripts",
    description = Some("List scripts defined in the build. Scripts are named entry points that compile and run a specific main class."),
    annotations = Some(ToolAnnotations(title = Some("List Scripts"), readOnlyHint = Some(true), openWorldHint = Some(false)))
  ) { (_, _) =>
    listScripts(s).map(textResult)
  }

  private def statusTool(s: ToolState): ToolDef[IO, StatusInput, Nothing] = ToolDef.unstructured[IO, StatusInput](
    name = "bleep.status",
    description = Some(
      "Show cached results from the last build/test run without re-running. Returns full diagnostics and test results. Use project/limit/offset to paginate large results."
    ),
    annotations = Some(ToolAnnotations(title = Some("Build Status"), readOnlyHint = Some(true), openWorldHint = Some(false)))
  ) { (input, _) =>
    s.buildHistory.get.map { history =>
      textResult(
        history.last match {
          case Some(run) =>
            val events = input.project match {
              case Some(proj) => filterEventsByProject(run.events, proj)
              case None       => run.events
            }
            if (run.mode == "test") formatTestResult(events, None, PreviousRunState.empty, true, includeThrowables = true, input.limit, input.offset)
            else formatCompileResult(events, PreviousRunState.empty, true, input.limit, input.offset)
          case None =>
            """{"message":"No previous build results. Run bleep.compile or bleep.test first."}"""
        }
      )
    }
  }

  private def restartTool(s: ToolState): ToolDef[IO, NoInput, Nothing] = ToolDef.unstructured[IO, NoInput](
    name = "bleep.restart",
    description = Some(
      "Restart the MCP server process. Use after producing a new bleep binary or when the server is in a bad state. The process exits and the client will relaunch it. Wait a few seconds before calling other tools."
    ),
    annotations = Some(ToolAnnotations(title = Some("Restart"), destructiveHint = Some(true), openWorldHint = Some(false)))
  ) { (_, _) =>
    IO {
      s.started.logger.info("MCP server restart requested, exiting process")
      val daemon = new Thread(() => {
        System.out.flush()
        Thread.sleep(500)
        System.out.flush()
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
      textResult("""{"restarting":true,"message":"Process exiting. Tools will be available again in a few seconds."}""")
    }
  }

  // ========================================================================
  // Resource definitions
  // ========================================================================

  private def createResources(
      started: () => Started,
      bleepYamlChangeQueue: Queue[IO, Unit],
      sourceChangeQueue: Queue[IO, Unit]
  ): List[ResourceDef[IO, ?]] = List(
    ResourceDef[IO, String](
      uri = "bleep://build/bleep.yaml",
      name = "bleep.yaml",
      description = Some("The bleep build configuration file. Subscribable — notifies when the build file changes and is reloaded."),
      mimeType = Some("text/yaml"),
      handler = (_: ResourceContext[IO]) => IO.blocking(Some(Files.readString(started().buildPaths.bleepYamlFile))),
      updates = fs2.Stream.fromQueueUnterminated(bleepYamlChangeQueue)
    ),
    ResourceDef[IO, String](
      uri = "bleep://watch",
      name = "File Watcher",
      description = Some("Source file watcher. Subscribable — emits notifications when source files in the build change. Call bleep.compile or bleep.test to rebuild."),
      mimeType = Some("application/json"),
      handler = (_: ResourceContext[IO]) =>
        IO.pure(Some(Json.obj("message" -> Json.fromString("Subscribe to receive file change notifications.")).noSpaces)),
      updates = fs2.Stream.fromQueueUnterminated(sourceChangeQueue)
    )
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

  private def executeBspOperation(
      s: ToolState,
      projectNames: List[String],
      verbose: Boolean,
      ctx: ToolContext[IO]
  ): IO[String] = {
    val targetProjects = resolveProjects(s.started, projectNames)

    if (targetProjects.isEmpty) {
      return IO.pure("No projects to compile.")
    }

    val originId = UUID.randomUUID().toString

    for {
      previousState <- s.buildHistory.get.map(_.previousRunState)
      eventQueue <- Queue.unbounded[IO, Option[BleepBspProtocol.Event]]
      collectedEvents <- Ref.of[IO, List[BleepBspProtocol.Event]](Nil)
      done <- Ref.of[IO, Boolean](false)

      _ <- IO.delay(s.eventRoutes.put(originId, eventQueue))
      _ <- IO.delay(s.diagnosticRoutes.put(originId, diagnosticCallback()))

      consumerFiber <- consumeAndLogEvents(eventQueue, collectedEvents, previousState, ctx).start
      heartbeatFiber <- heartbeat(collectedEvents, done, "compile", ctx).start

      _ <- {
        val targets = BspQuery.buildTargets(s.started.buildPaths, targetProjects)
        BspRequestHelper
          .callCancellable(
            {
              val params = new bsp4j.CompileParams(targets)
              params.setOriginId(originId)
              s.bspServer.buildTargetCompile(params)
            },
            s.bspListening
          )
          .void
      }.guarantee(
        IO.delay(s.eventRoutes.remove(originId)) >>
          IO.delay(s.diagnosticRoutes.remove(originId)) >>
          eventQueue.offer(None) >>
          consumerFiber.joinWithNever >>
          done.set(true) >>
          heartbeatFiber.cancel
      )

      events <- collectedEvents.get
      _ <- s.buildHistory.update(_.push(BuildRun(System.currentTimeMillis(), "compile", events.reverse)))
    } yield formatCompileResult(events.reverse, previousState, verbose, None, None)
  }

  private def executeBspTestOperation(
      s: ToolState,
      projectNames: List[String],
      only: List[String],
      exclude: List[String],
      verbose: Boolean,
      ctx: ToolContext[IO]
  ): IO[String] = {
    val targetProjects = resolveTestProjects(s.started, projectNames)

    if (targetProjects.isEmpty) {
      return IO.pure("No test projects found.")
    }

    val originId = UUID.randomUUID().toString

    for {
      previousState <- s.buildHistory.get.map(_.previousRunState)
      eventQueue <- Queue.unbounded[IO, Option[BleepBspProtocol.Event]]
      collectedEvents <- Ref.of[IO, List[BleepBspProtocol.Event]](Nil)
      done <- Ref.of[IO, Boolean](false)
      testRunResult <- Ref.of[IO, Option[BleepBspProtocol.TestRunResult]](None)

      _ <- IO.delay(s.eventRoutes.put(originId, eventQueue))
      _ <- IO.delay(s.diagnosticRoutes.put(originId, diagnosticCallback()))

      consumerFiber <- consumeAndLogEvents(eventQueue, collectedEvents, previousState, ctx).start
      heartbeatFiber <- heartbeat(collectedEvents, done, "test", ctx).start

      _ <- {
        val targets = BspQuery.buildTargets(s.started.buildPaths, targetProjects)
        BspRequestHelper
          .callCancellable(
            {
              val params = new bsp4j.TestParams(targets)
              params.setOriginId(originId)
              val testOptions = BleepBspProtocol.TestOptions(Nil, Nil, only, exclude, false)
              params.setDataKind(BleepBspProtocol.TestOptionsDataKind)
              params.setData(com.google.gson.JsonParser.parseString(BleepBspProtocol.TestOptions.encode(testOptions)))
              s.bspServer.buildTargetTest(params)
            },
            s.bspListening
          )
          .flatMap { result =>
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
        IO.delay(s.eventRoutes.remove(originId)) >>
          IO.delay(s.diagnosticRoutes.remove(originId)) >>
          eventQueue.offer(None) >>
          consumerFiber.joinWithNever >>
          done.set(true) >>
          heartbeatFiber.cancel
      )

      events <- collectedEvents.get
      trr <- testRunResult.get
      _ <- s.buildHistory.update(_.push(BuildRun(System.currentTimeMillis(), "test", events.reverse)))
    } yield formatTestResult(events.reverse, trr, previousState, verbose, includeThrowables = verbose, None, None)
  }

  private def showBuildConfig(s: ToolState, projectNames: List[String]): IO[String] = IO {
    val started = s.started
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

  private def showResolvedConfig(s: ToolState, projectNames: List[String]): IO[String] = IO {
    val started = s.started
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

  private def listProjects(s: ToolState): IO[String] = IO {
    val projects = s.started.build.explodedProjects.toList.map { case (crossName, p) =>
      Json.obj(
        "name" -> Json.fromString(crossName.value),
        "dependsOn" -> Json.arr(p.dependsOn.values.toList.map(d => Json.fromString(d.value))*),
        "isTest" -> Json.fromBoolean(p.isTestProject.getOrElse(false))
      )
    }
    Json.arr(projects*).noSpaces
  }

  private def discoverTestSuites(s: ToolState, projectNames: List[String]): IO[String] = {
    val targetProjects = resolveTestProjects(s.started, projectNames)
    if (targetProjects.isEmpty) {
      return IO.pure("""{"projects":[]}""")
    }
    for {
      result <- {
        val targets = BspQuery.buildTargets(s.started.buildPaths, targetProjects)
        BspRequestHelper.callCancellable(
          s.bspServer.buildTargetScalaTestClasses(new bsp4j.ScalaTestClassesParams(targets)),
          s.bspListening
        )
      }
    } yield {
      val items = result.getItems.asScala.toList.flatMap { item =>
        BspQuery.projectFromBuildTarget(s.started)(item.getTarget).map { projectName =>
          Json.obj(
            "project" -> Json.fromString(projectName.value),
            "suites" -> Json.arr(item.getClasses.asScala.toList.map(Json.fromString)*)
          )
        }
      }
      Json.obj("projects" -> Json.arr(items*)).noSpaces
    }
  }

  private def listPrograms(s: ToolState): IO[String] = IO {
    val programs = s.started.build.explodedProjects.toList
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

  private def listScripts(s: ToolState): IO[String] = IO {
    val scripts = s.started.build.scripts.toList.sortBy(_._1.value).flatMap { case (scriptName, scriptDefs) =>
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

  private def runProjectOrScript(
      s: ToolState,
      name: String,
      args: List[String],
      mainClassOverride: Option[String],
      timeoutSeconds: Int,
      ctx: ToolContext[IO]
  ): IO[String] = {
    val started = s.started
    val scriptMatch = started.build.scripts.keys.find(_.value == name)
    scriptMatch match {
      case Some(sn) =>
        val scriptDefs = started.build.scripts(sn).values
        scriptDefs.headOption match {
          case Some(model.ScriptDef.Main(project, main, _)) =>
            runProject(s, project, Some(main), args, timeoutSeconds, ctx)
          case _ =>
            IO.raiseError(new BleepException.Text(s"Script '$name' has no main class definition"))
        }
      case None =>
        started.globs.exactProjectMap.get(name) match {
          case Some(projectName) =>
            runProject(s, projectName, mainClassOverride, args, timeoutSeconds, ctx)
          case None =>
            IO.raiseError(new BleepException.Text(s"'$name' is not a valid project or script name"))
        }
    }
  }

  private def runProject(
      s: ToolState,
      project: model.CrossProjectName,
      mainClassOverride: Option[String],
      args: List[String],
      timeoutSeconds: Int,
      ctx: ToolContext[IO]
  ): IO[String] =
    for {
      _ <- ctx.log(LoggingLevel.info, Json.fromString(s"Compiling ${project.value}..."))
      _ <- compileSilently(s, Array(project))
      mainClass <- IO {
        mainClassOverride
          .orElse(s.started.build.explodedProjects(project).platform.flatMap(_.mainClass))
          .getOrElse(throw new BleepException.Text(s"No main class for ${project.value}. Specify with 'mainClass' parameter."))
      }
      cmd <- IO.fromEither(
        internal.jvmRunCommand(s.started.resolvedProject(project), s.started.resolvedJvm, project, Some(mainClass), args)
      )
      _ <- ctx.log(LoggingLevel.info, Json.fromString(s"Running $mainClass..."))
      result <- executeSubprocess(cmd, s.started.buildPaths.cwd, timeoutSeconds)
    } yield {
      val (stdout, stderr, exitCode) = result
      Json
        .obj("exitCode" -> Json.fromInt(exitCode), "stdout" -> Json.fromString(stdout), "stderr" -> Json.fromString(stderr))
        .noSpaces
    }

  private def compileSilently(s: ToolState, targetProjects: Array[model.CrossProjectName]): IO[Unit] = {
    val targets = BspQuery.buildTargets(s.started.buildPaths, targetProjects)
    BspRequestHelper
      .callCancellable(
        { val params = new bsp4j.CompileParams(targets); s.bspServer.buildTargetCompile(params) },
        s.bspListening
      )
      .flatMap { result =>
        IO.raiseWhen(result.getStatusCode != bsp4j.StatusCode.OK)(
          new BleepException.Text(s"Compilation failed with status ${result.getStatusCode}")
        )
      }
  }

  private def executeSubprocess(cmd: List[String], cwd: java.nio.file.Path, timeoutSeconds: Int): IO[(String, String, Int)] =
    IO.interruptible {
      val builder = new java.lang.ProcessBuilder(cmd.asJava)
      builder.directory(cwd.toFile)
      val proc = builder.start()
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
  // Event consumption & progress
  // ========================================================================

  private def consumeAndLogEvents(
      eventQueue: Queue[IO, Option[BleepBspProtocol.Event]],
      collectedEvents: Ref[IO, List[BleepBspProtocol.Event]],
      previousState: PreviousRunState,
      ctx: ToolContext[IO]
  ): IO[Unit] =
    eventQueue.take.flatMap {
      case Some(event) =>
        streamDiffLine(event, previousState, collectedEvents, ctx) >>
          collectedEvents.update(event :: _) >>
          consumeAndLogEvents(eventQueue, collectedEvents, previousState, ctx)
      case None => IO.unit
    }

  private def streamNotification(ctx: ToolContext[IO], level: LoggingLevel, message: String): IO[Unit] =
    ctx.reportProgress(0.0, None, Some(message)).attempt >>
      IO(System.err.println(message)) >>
      ctx.log(level, Json.fromString(message))

  private def heartbeat(
      collectedEvents: Ref[IO, List[BleepBspProtocol.Event]],
      done: Ref[IO, Boolean],
      operation: String,
      ctx: ToolContext[IO]
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

            streamNotification(ctx, LoggingLevel.info, s"[$operation] $status...")
          }
    } yield ()

    (IO.sleep(1.second) >> tick).foreverM.void
  }

  private def streamDiffLine(
      event: BleepBspProtocol.Event,
      previousState: PreviousRunState,
      collectedEvents: Ref[IO, List[BleepBspProtocol.Event]],
      ctx: ToolContext[IO]
  ): IO[Unit] = {
    import BleepBspProtocol.{Event => E}
    event match {
      case e: E.CompileFinished =>
        val errorCount = e.diagnostics.count(_.severity == DiagnosticSeverity.Error)
        val previousDiags = previousState.compileDiagnostics.getOrElse(e.project, Nil)
        val hasPrevious = previousState.compileDiagnostics.contains(e.project)

        if (e.status == CompileStatus.Failed) {
          if (hasPrevious) {
            val diff = BuildDiff.diffCompile(e.project, e.status, e.diagnostics, previousDiags, e.durationMs)
            streamNotification(ctx, LoggingLevel.error, BuildDiff.formatCompileDiff(diff))
          } else {
            val firstErrors = e.diagnostics.filter(_.severity == DiagnosticSeverity.Error).take(3).map(d => stripAnsi(d.message))
            val moreStr = if (errorCount > 3) s" (+${errorCount - 3} more)" else ""
            streamNotification(ctx, LoggingLevel.error, s"${e.project}: $errorCount errors (${e.durationMs}ms). ${firstErrors.mkString("; ")}$moreStr")
          }
        } else if (hasPrevious) {
          val diff = BuildDiff.diffCompile(e.project, e.status, e.diagnostics, previousDiags, e.durationMs)
          if (diff.fixedErrors > 0 || diff.newErrors > 0) streamNotification(ctx, LoggingLevel.info, BuildDiff.formatCompileDiff(diff))
          else IO.unit
        } else IO.unit

      case e: E.SuiteFinished =>
        collectedEvents.get.flatMap { collected =>
          val suiteTests = collected.collect {
            case tf: E.TestFinished if tf.project == e.project && tf.suite == e.suite => tf
          }
          val newFailures = suiteTests.filter { tf =>
            val key = TestKey(tf.project, tf.suite, tf.test)
            tf.status.isFailure && !previousState.testResults.get(key).exists(_.isFailure)
          }
          val fixedTests = suiteTests.filter { tf =>
            val key = TestKey(tf.project, tf.suite, tf.test)
            previousState.testResults.get(key).exists(_.isFailure) && !tf.status.isFailure
          }
          if (e.failed > 0 || newFailures.nonEmpty || fixedTests.nonEmpty) {
            val diffParts = List.newBuilder[String]
            fixedTests.foreach(t => diffParts += s"${t.test.value} fixed")
            newFailures.foreach(t => diffParts += s"${t.test.value} new failure")
            val details = diffParts.result()
            val countsStr = s"${e.passed} passed, ${e.failed} failed"
            val detailStr = if (details.nonEmpty) s" (${details.mkString(", ")})" else ""
            val level = if (e.failed > 0) LoggingLevel.error else LoggingLevel.info
            streamNotification(ctx, level, s"${e.project.value} ${e.suite.value}: $countsStr$detailStr")
          } else IO.unit
        }

      case _: E.SuiteError | _: E.SuiteTimedOut | _: E.Error =>
        McpEventFilter.filter(event) match {
          case Some(json) => streamNotification(ctx, LoggingLevel.error, json.noSpaces)
          case None       => IO.unit
        }

      case e: E.LinkFinished if !e.success =>
        McpEventFilter.filter(event) match {
          case Some(json) => streamNotification(ctx, LoggingLevel.error, json.noSpaces)
          case None       => IO.unit
        }

      case _ => IO.unit
    }
  }

  private def diagnosticCallback(): bsp4j.PublishDiagnosticsParams => Unit = _ => ()

  // ========================================================================
  // Filtering & formatting
  // ========================================================================

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
            totalFixed += previousState.compileDiagnostics(project).count(_.severity == DiagnosticSeverity.Error)
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

    if (failedTests.nonEmpty) {
      fields += "totalFailures" -> Json.fromInt(failedTests.size)
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

/** Shared state passed to all tool implementations. */
private[mcp] case class ToolState(
    startedRef: AtomicReference[Started],
    bspConfig: BspRifleConfig,
    bspServer: bleep.bsp.BuildServer,
    bspListening: java.util.concurrent.Future[Void],
    eventRoutes: ConcurrentHashMap[String, Queue[IO, Option[BleepBspProtocol.Event]]],
    diagnosticRoutes: ConcurrentHashMap[String, bsp4j.PublishDiagnosticsParams => Unit],
    buildHistory: Ref[IO, BuildHistory]
) {
  def started: Started = startedRef.get()
}

/** BSP client that routes events by originId to the correct tool call's event queue. */
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
      case None           => diagnosticRoutes.values().forEach(_(params))
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
