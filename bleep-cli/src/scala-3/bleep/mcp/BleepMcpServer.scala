package bleep.mcp

import bleep._
import bleep.bsp.{BspRequestHelper, BspRifle, BspRifleConfig, BspServerBuilder, SetupBleepBsp}
import bleep.bsp.protocol.BleepBspProtocol
import bleep.internal.BspClientDisplayProgress
import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._
import ch.epfl.scala.bsp4j
import ch.linkyard.mcp.server.{CallContext, McpServer, ResourceTemplate, ToolFunction}
import ch.linkyard.mcp.protocol
import io.circe.{Json, JsonObject}

import java.nio.file.Files
import java.util.UUID
import java.util.concurrent.CountDownLatch
import scala.jdk.CollectionConverters.*

/** MCP server for bleep that exposes compile, test, watch, and project info to AI agents.
  *
  * Connects to the bleep-bsp daemon (same server used by the TUI) and translates BSP events into MCP tool results and notifications. Runs on stdio transport.
  */
class BleepMcpServer(started: Started) extends McpServer[IO] {

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
      watchJobs <- Resource.eval(Ref.of[IO, Map[JobId, WatchJob]](Map.empty))
      watchResults <- Resource.eval(Ref.of[IO, Map[JobId, WatchCycleResult]](Map.empty))
      _ <- Resource.onFinalize(
        watchJobs.get.flatMap { jobs =>
          jobs.values.toList.traverse_(_.cancel)
        }
      )
    } yield new BleepMcpSession(client, bspConfig, watchJobs, watchResults)

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
          workingDir = started.buildPaths.cwd
        )
      case _: bsp.BspServerClasspathSource.InProcess =>
        Left(new BleepException.Text("MCP server does not support in-process BSP mode"))
    }

  private class BleepMcpSession(
      _client: McpServer.Client[IO],
      bspConfig: BspRifleConfig,
      watchJobs: Ref[IO, Map[JobId, WatchJob]],
      watchResults: Ref[IO, Map[JobId, WatchCycleResult]]
  ) extends McpServer.Session[IO]
      with McpServer.ToolProvider[IO]
      with McpServer.ResourceProvider[IO] {

    override val serverInfo: protocol.Initialize.PartyInfo =
      protocol.Initialize.PartyInfo("bleep", model.BleepVersion.current.value)

    override def instructions: IO[Option[String]] =
      IO.pure(
        Some(
          """Bleep build tool MCP server. Available tools:
          |- bleep.compile — compile projects, returns diagnostics
          |- bleep.test — run tests, returns pass/fail results
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
          |- bleep.run — compile and run a project or script, returns stdout/stderr""".stripMargin
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
        projectsTool,
        programsTool,
        scriptsTool,
        runTool
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

    private def compileTool: ToolFunction[IO] = ToolFunction.native[IO](
      ToolFunction.Info(
        "bleep.compile",
        Some("Compile"),
        Some("Compile bleep projects. Returns diagnostics (errors/warnings). Streams progress via notifications during execution."),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      compileTestArgsSchema,
      withErrorReporting { (args, context) =>
        val projectNames = extractProjectNames(args)
        executeBspOperation(bspConfig, projectNames, context)
      }
    )

    private def testTool: ToolFunction[IO] = ToolFunction.native[IO](
      ToolFunction.Info(
        "bleep.test",
        Some("Test"),
        Some("Run tests for bleep projects. Returns test results with pass/fail counts and failure details."),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      testArgsSchema,
      withErrorReporting { (args, context) =>
        val projectNames = extractProjectNames(args)
        val only = extractStringList(args, "only")
        val exclude = extractStringList(args, "exclude")
        executeBspTestOperation(bspConfig, projectNames, only, exclude, context)
      }
    )

    private def testSuitesTool: ToolFunction[IO] = ToolFunction.native[IO](
      ToolFunction.Info(
        "bleep.test.suites",
        Some("Test Suites"),
        Some(
          "Discover test suites in compiled test projects without running them. Projects must be compiled first. Returns test class names grouped by project."
        ),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      compileTestArgsSchema,
      withErrorReporting { (args, _) =>
        val projectNames = extractProjectNames(args)
        discoverTestSuites(bspConfig, projectNames)
      }
    )

    private def sourcegenTool: ToolFunction[IO] = ToolFunction.native[IO](
      ToolFunction.Info(
        "bleep.sourcegen",
        Some("Source Generate"),
        Some("Run source generators for bleep projects. Only affects projects that have sourcegen scripts defined."),
        ToolFunction.Effect.Additive(false),
        false
      ),
      compileTestArgsSchema,
      withErrorReporting { (args, _) =>
        val projectNames = extractProjectNames(args)
        val allProjects = resolveProjects(projectNames)
        val sourcegenProjects = allProjects.filter { crossName =>
          started.build.explodedProjects(crossName).sourcegen.values.nonEmpty
        }
        if (sourcegenProjects.isEmpty) {
          IO.pure(textResponse("""{"success":true,"message":"No projects with sourcegen scripts found."}"""))
        } else {
          IO.fromEither(commands.SourceGen(false, sourcegenProjects).run(started))
            .as(
              textResponse(
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
    )

    private def fmtTool: ToolFunction[IO] = ToolFunction.native[IO](
      ToolFunction.Info(
        "bleep.fmt",
        Some("Format"),
        Some("Format Scala and Java source files using scalafmt and google-java-format. Optionally limit to specific projects."),
        ToolFunction.Effect.Additive(false),
        false
      ),
      compileTestArgsSchema,
      withErrorReporting { (args, _) =>
        val projectNames = extractProjectNames(args)
        val projects = resolveProjects(projectNames)
        IO.fromEither(commands.Fmt(check = false, projects = projects).run(started))
          .as(
            textResponse(
              Json
                .obj(
                  "success" -> Json.fromBoolean(true),
                  "projects" -> Json.fromInt(projects.length)
                )
                .noSpaces
            )
          )
      }
    )

    private def cleanTool: ToolFunction[IO] = ToolFunction.native[IO](
      ToolFunction.Info(
        "bleep.clean",
        Some("Clean"),
        Some("Delete build outputs for bleep projects. Removes compiled classes and other build artifacts."),
        ToolFunction.Effect.Destructive(true),
        false
      ),
      compileTestArgsSchema,
      withErrorReporting { (args, _) =>
        val projectNames = extractProjectNames(args)
        val projects = resolveProjects(projectNames)
        if (projects.isEmpty) {
          IO.pure(textResponse("""{"success":true,"message":"No projects to clean."}"""))
        } else {
          IO.fromEither(commands.Clean(projects).run(started))
            .as(
              textResponse(
                Json
                  .obj(
                    "success" -> Json.fromBoolean(true),
                    "projects" -> Json.arr(projects.map(p => Json.fromString(p.value)).toList*)
                  )
                  .noSpaces
              )
            )
        }
      }
    )

    private def watchTool: ToolFunction[IO] = ToolFunction.native[IO](
      ToolFunction.Info(
        "bleep.watch",
        Some("Watch"),
        Some(
          "Start a background watch job that recompiles on file changes. Returns a job ID. Diagnostics stream via notifications. Use bleep.sync to get a clean snapshot, bleep.watch.stop to stop."
        ),
        ToolFunction.Effect.Additive(false),
        false
      ),
      watchArgsSchema,
      withErrorReporting { (args, context) =>
        val projectNames = extractProjectNames(args)
        val modeStr = args("mode").flatMap(_.asString).getOrElse("compile")
        val mode = modeStr match {
          case "test" => BleepBspProtocol.BuildMode.Test
          case _      => BleepBspProtocol.BuildMode.Compile
        }
        val only = extractStringList(args, "only")
        val exclude = extractStringList(args, "exclude")
        startWatch(projectNames, mode, only, exclude, context)
      }
    )

    private def syncTool: ToolFunction[IO] = ToolFunction.native[IO](
      ToolFunction.Info(
        "bleep.sync",
        Some("Sync"),
        Some(
          "Returns the latest results from active watch jobs (compile diagnostics, test results). If no watch jobs are running, does a fresh compile of all projects."
        ),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      syncArgsSchema,
      withErrorReporting { (_, context) =>
        for {
          results <- watchResults.get
          response <-
            if (results.nonEmpty) {
              val items = results.values.toList.sortBy(_.jobId.value).map { r =>
                Json.obj(
                  "jobId" -> Json.fromString(r.jobId.value),
                  "mode" -> Json.fromString(r.mode.value),
                  "result" -> io.circe.parser.parse(r.summary).getOrElse(Json.fromString(r.summary)),
                  "timestampMs" -> Json.fromLong(r.timestampMs)
                )
              }
              IO.pure(textResponse(Json.obj("watchResults" -> Json.arr(items*)).noSpaces))
            } else {
              executeBspOperation(bspConfig, Nil, context)
            }
        } yield response
      }
    )

    private def watchStopTool: ToolFunction[IO] = ToolFunction.native[IO](
      ToolFunction.Info(
        "bleep.watch.stop",
        Some("Stop Watch"),
        Some("Stop a background watch job, or all watch jobs if no jobId is specified."),
        ToolFunction.Effect.Destructive(true),
        false
      ),
      syncArgsSchema,
      withErrorReporting { (args, _) =>
        val jobId = args("jobId").flatMap(_.asString).map(new JobId(_))
        stopWatch(jobId)
      }
    )

    private def projectsTool: ToolFunction[IO] = ToolFunction.native[IO](
      ToolFunction.Info(
        "bleep.projects",
        Some("List Projects"),
        Some("List all projects in the build with their dependencies and whether they are test projects."),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      io.circe.JsonSchema.empty,
      withErrorReporting { (_, _) =>
        listProjects()
      }
    )

    private def programsTool: ToolFunction[IO] = ToolFunction.native[IO](
      ToolFunction.Info(
        "bleep.programs",
        Some("List Programs"),
        Some("List projects that have a mainClass defined (runnable programs). Shows project name, main class, and platform."),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      io.circe.JsonSchema.empty,
      withErrorReporting { (_, _) =>
        listPrograms()
      }
    )

    private def scriptsTool: ToolFunction[IO] = ToolFunction.native[IO](
      ToolFunction.Info(
        "bleep.scripts",
        Some("List Scripts"),
        Some("List scripts defined in the build. Scripts are named entry points that compile and run a specific main class."),
        ToolFunction.Effect.ReadOnly,
        false
      ),
      io.circe.JsonSchema.empty,
      withErrorReporting { (_, _) =>
        listScripts()
      }
    )

    private def runTool: ToolFunction[IO] = ToolFunction.native[IO](
      ToolFunction.Info(
        "bleep.run",
        Some("Run"),
        Some(
          "Compile and run a project or script. Checks scripts first, then projects. Returns stdout/stderr and exit code. Has a timeout to prevent hanging on long-running processes."
        ),
        ToolFunction.Effect.Additive(false),
        false
      ),
      runArgsSchema,
      withErrorReporting { (args, context) =>
        val name = args("name")
          .flatMap(_.asString)
          .getOrElse(
            throw new BleepException.Text("'name' parameter is required")
          )
        val runArgs = extractStringList(args, "args")
        val mainClass = args("mainClass").flatMap(_.asString)
        val timeoutSeconds = args("timeoutSeconds").flatMap(_.asNumber).flatMap(_.toInt).getOrElse(60)
        runProjectOrScript(name, runArgs, mainClass, timeoutSeconds, context)
      }
    )

    // ========================================================================
    // JSON Schemas for tool inputs
    // ========================================================================

    private val compileTestArgsSchema = io.circe.JsonSchema.obj(
      "projects" -> io.circe.JsonSchema.arrayOfStrings("Project names to compile. Omit or empty for all projects.")
    )

    private val testArgsSchema = io.circe.JsonSchema.obj(
      "projects" -> io.circe.JsonSchema.arrayOfStrings("Project names to test. Omit or empty for all test projects."),
      "only" -> io.circe.JsonSchema.arrayOfStrings("Only run these test class names."),
      "exclude" -> io.circe.JsonSchema.arrayOfStrings("Exclude these test class names.")
    )

    private val watchArgsSchema = io.circe.JsonSchema.obj(
      "projects" -> io.circe.JsonSchema.arrayOfStrings("Project names to watch."),
      "mode" -> io.circe.JsonSchema.stringEnum("compile", "test"),
      "only" -> io.circe.JsonSchema.arrayOfStrings("Only run these test class names (test mode only)."),
      "exclude" -> io.circe.JsonSchema.arrayOfStrings("Exclude these test class names (test mode only).")
    )

    private val syncArgsSchema = io.circe.JsonSchema.obj(
      "jobId" -> io.circe.JsonSchema.string("Watch job ID. Omit to sync all.")
    )

    private val runArgsSchema = {
      val schema = io.circe.JsonSchema.obj(
        "name" -> io.circe.JsonSchema.string("Project or script name to run. Scripts are checked first, then projects."),
        "args" -> io.circe.JsonSchema.arrayOfStrings("Arguments to pass to the program."),
        "mainClass" -> io.circe.JsonSchema.string("Override main class (projects only). Not needed for scripts."),
        "timeoutSeconds" -> io.circe.JsonSchema.integer("Timeout in seconds. Default 60.")
      )
      schema.add("required", Json.arr(Json.fromString("name")))
    }

    // ========================================================================
    // Tool implementations
    // ========================================================================

    private def extractProjectNames(args: JsonObject): List[String] =
      args("projects")
        .flatMap(_.asArray)
        .map(_.flatMap(_.asString).toList)
        .getOrElse(Nil)

    private def extractStringList(args: JsonObject, key: String): List[String] =
      args(key)
        .flatMap(_.asArray)
        .map(_.flatMap(_.asString).toList)
        .getOrElse(Nil)

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

    /** Execute a one-shot compile via BSP. Streams diagnostics via MCP notifications. */
    private def executeBspOperation(
        config: BspRifleConfig,
        projectNames: List[String],
        context: CallContext[IO]
    ): IO[protocol.Tool.CallTool.Response] = {
      val targetProjects = resolveProjects(projectNames)

      if (targetProjects.isEmpty) {
        return IO.pure(textResponse("No projects to compile."))
      }

      for {
        _ <- BspRifle.ensureRunning(config, started.logger)
        eventQueue <- Queue.unbounded[IO, Option[BleepBspProtocol.Event]]
        collectedEvents <- Ref.of[IO, List[Json]](Nil)

        bspClient = new McpBspClient(eventQueue, started.logger)

        // Consumer fiber: filter events, log to MCP, collect for final response
        consumerFiber <- consumeAndLogEvents(eventQueue, collectedEvents, context).start

        _ <- BspRifle
          .connectWithRetry(config, started.logger)
          .use { connection =>
            BspServerBuilder.create(connection, bspClient).use { lifecycle =>
              val server = lifecycle.server
              for {
                _ <- BspServerBuilder.initializeSession(
                  server = server,
                  clientName = "bleep-mcp",
                  clientVersion = model.BleepVersion.current.value,
                  rootUri = started.buildPaths.buildDir.toUri.toString
                )
                targets = BspQuery.buildTargets(started.buildPaths, targetProjects)
                _ <- BspRequestHelper.callCancellable {
                  val params = new bsp4j.CompileParams(targets)
                  server.buildTargetCompile(params)
                }.void
                _ <- IO.blocking(scala.util.Try(server.buildShutdown().get())).attempt.void
                _ <- IO.blocking(scala.util.Try(server.onBuildExit())).attempt.void
              } yield ()
            }
          }
          .guarantee(eventQueue.offer(None))

        _ <- consumerFiber.joinWithNever
        events <- collectedEvents.get
      } yield textResponse(formatCompileResult(events))
    }

    /** Execute a one-shot test via BSP. */
    private def executeBspTestOperation(
        config: BspRifleConfig,
        projectNames: List[String],
        only: List[String],
        exclude: List[String],
        context: CallContext[IO]
    ): IO[protocol.Tool.CallTool.Response] = {
      val targetProjects = resolveTestProjects(projectNames)

      if (targetProjects.isEmpty) {
        return IO.pure(textResponse("No test projects found."))
      }

      for {
        _ <- BspRifle.ensureRunning(config, started.logger)
        eventQueue <- Queue.unbounded[IO, Option[BleepBspProtocol.Event]]
        collectedEvents <- Ref.of[IO, List[Json]](Nil)

        bspClient = new McpBspClient(eventQueue, started.logger)
        consumerFiber <- consumeAndLogEvents(eventQueue, collectedEvents, context).start

        testRunResult <- Ref.of[IO, Option[BleepBspProtocol.TestRunResult]](None)

        _ <- BspRifle
          .connectWithRetry(config, started.logger)
          .use { connection =>
            BspServerBuilder.create(connection, bspClient).use { lifecycle =>
              val server = lifecycle.server
              for {
                _ <- BspServerBuilder.initializeSession(
                  server = server,
                  clientName = "bleep-mcp",
                  clientVersion = model.BleepVersion.current.value,
                  rootUri = started.buildPaths.buildDir.toUri.toString
                )
                targets = BspQuery.buildTargets(started.buildPaths, targetProjects)
                result <- BspRequestHelper.callCancellable {
                  val params = new bsp4j.TestParams(targets)
                  val testOptions = BleepBspProtocol.TestOptions(Nil, Nil, only, exclude, false)
                  params.setDataKind(BleepBspProtocol.TestOptionsDataKind)
                  params.setData(com.google.gson.JsonParser.parseString(BleepBspProtocol.TestOptions.encode(testOptions)))
                  server.buildTargetTest(params)
                }
                // Extract TestRunResult from response
                _ <- IO {
                  for {
                    dataKind <- Option(result.getDataKind)
                    if dataKind == BleepBspProtocol.TestRunResultDataKind
                    data <- Option(result.getData)
                    jsonStr = data.toString
                    decoded <- BleepBspProtocol.TestRunResult.decode(jsonStr).toOption
                  } testRunResult.set(Some(decoded)).unsafeRunSync()(cats.effect.unsafe.implicits.global)
                }
                _ <- IO.blocking(scala.util.Try(server.buildShutdown().get())).attempt.void
                _ <- IO.blocking(scala.util.Try(server.onBuildExit())).attempt.void
              } yield ()
            }
          }
          .guarantee(eventQueue.offer(None))

        _ <- consumerFiber.joinWithNever
        events <- collectedEvents.get
        trr <- testRunResult.get
      } yield textResponse(formatTestResult(events, trr))
    }

    /** Start a background watch job. */
    private def startWatch(
        projectNames: List[String],
        mode: BleepBspProtocol.BuildMode,
        only: List[String],
        exclude: List[String],
        context: CallContext[IO]
    ): IO[protocol.Tool.CallTool.Response] = {
      val targetProjects = mode match {
        case BleepBspProtocol.BuildMode.Test => resolveTestProjects(projectNames)
        case _                               => resolveProjects(projectNames)
      }
      val jobId = new JobId(UUID.randomUUID().toString.take(8))

      for {
        _ <- BspRifle.ensureRunning(bspConfig, started.logger)
        fiber <- watchLoop(jobId, targetProjects, mode, only, exclude, context).start
        job = WatchJob(jobId, targetProjects.map(_.value).toList, mode, fiber)
        _ <- watchJobs.update(_ + (jobId -> job))
      } yield textResponse(
        Json
          .obj(
            "jobId" -> Json.fromString(jobId.value),
            "watching" -> Json.arr(targetProjects.map(p => Json.fromString(p.value)).toList*)
          )
          .noSpaces
      )
    }

    /** Background watch loop: file changes -> recompile -> emit events. */
    private def watchLoop(
        jobId: JobId,
        targetProjects: Array[model.CrossProjectName],
        mode: BleepBspProtocol.BuildMode,
        only: List[String],
        exclude: List[String],
        context: CallContext[IO]
    ): IO[Unit] = {
      val transitiveProjects = internal.TransitiveProjects(started.build, targetProjects)

      def cycle: IO[Unit] =
        for {
          _ <- context.log(protocol.LoggingLevel.Info, s"[${jobId.value}] Compiling...")
          eventQueue <- Queue.unbounded[IO, Option[BleepBspProtocol.Event]]
          collectedEvents <- Ref.of[IO, List[Json]](Nil)
          bspClient = new McpBspClient(eventQueue, started.logger)
          consumerFiber <- consumeAndLogEvents(eventQueue, collectedEvents, context).start

          _ <- BspRifle
            .connectWithRetry(bspConfig, started.logger)
            .use { connection =>
              BspServerBuilder.create(connection, bspClient).use { lifecycle =>
                val server = lifecycle.server
                for {
                  _ <- BspServerBuilder.initializeSession(
                    server = server,
                    clientName = "bleep-mcp-watch",
                    clientVersion = model.BleepVersion.current.value,
                    rootUri = started.buildPaths.buildDir.toUri.toString
                  )
                  targets = BspQuery.buildTargets(started.buildPaths, targetProjects)
                  _ <- mode match {
                    case BleepBspProtocol.BuildMode.Test =>
                      BspRequestHelper.callCancellable {
                        val params = new bsp4j.TestParams(targets)
                        val testOptions = BleepBspProtocol.TestOptions(Nil, Nil, only, exclude, false)
                        params.setDataKind(BleepBspProtocol.TestOptionsDataKind)
                        params.setData(com.google.gson.JsonParser.parseString(BleepBspProtocol.TestOptions.encode(testOptions)))
                        server.buildTargetTest(params)
                      }.void
                    case _ =>
                      BspRequestHelper.callCancellable {
                        val params = new bsp4j.CompileParams(targets)
                        server.buildTargetCompile(params)
                      }.void
                  }
                  _ <- IO.blocking(scala.util.Try(server.buildShutdown().get())).attempt.void
                  _ <- IO.blocking(scala.util.Try(server.onBuildExit())).attempt.void
                } yield ()
              }
            }
            .guarantee(eventQueue.offer(None))

          _ <- consumerFiber.joinWithNever
          events <- collectedEvents.get

          summary = if (mode == BleepBspProtocol.BuildMode.Test) formatTestResult(events, None) else formatCompileResult(events)
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

      cycle >> watchLoop(jobId, targetProjects, mode, only, exclude, context)
    }

    /** Stop watch job(s). Cancel with timeout so we never hang. */
    private def stopWatch(jobId: Option[JobId]): IO[protocol.Tool.CallTool.Response] = {
      val cancelTimeout = scala.concurrent.duration.FiniteDuration(5, scala.concurrent.duration.SECONDS)

      def cancelJob(job: WatchJob): IO[Unit] =
        job.cancel.timeoutTo(cancelTimeout, IO.unit)

      jobId match {
        case Some(id) =>
          watchJobs.modify { jobs =>
            jobs.get(id) match {
              case Some(job) =>
                (jobs - id, cancelJob(job) >> watchResults.update(_ - id).as(textResponse(s"""{"stopped":"${id.value}"}""")))
              case None => (jobs, IO.pure(textResponse(s"""{"error":"no such job: ${id.value}"}""")))
            }
          }.flatten
        case None =>
          watchJobs.modify { jobs =>
            val cancelAll = jobs.values.toList.traverse_(cancelJob)
            val count = jobs.size
            (Map.empty, (cancelAll >> watchResults.set(Map.empty)).as(textResponse(s"""{"stopped":$count}""")))
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
    private def listProjects(): IO[protocol.Tool.CallTool.Response] = IO {
      val projects = started.build.explodedProjects.toList.map { case (crossName, p) =>
        Json.obj(
          "name" -> Json.fromString(crossName.value),
          "dependsOn" -> Json.arr(p.dependsOn.values.toList.map(d => Json.fromString(d.value))*),
          "isTest" -> Json.fromBoolean(p.isTestProject.getOrElse(false))
        )
      }
      textResponse(Json.arr(projects*).noSpaces)
    }

    /** Discover test suites via BSP buildTarget/scalaTestClasses. Projects must be compiled first. */
    private def discoverTestSuites(
        config: BspRifleConfig,
        projectNames: List[String]
    ): IO[protocol.Tool.CallTool.Response] = {
      val targetProjects = resolveTestProjects(projectNames)

      if (targetProjects.isEmpty) {
        return IO.pure(textResponse("""{"projects":[]}"""))
      }

      for {
        _ <- BspRifle.ensureRunning(config, started.logger)
        result <- BspRifle
          .connectWithRetry(config, started.logger)
          .use { connection =>
            val client = BspClientDisplayProgress(started.logger)
            BspServerBuilder.create(connection, client).use { lifecycle =>
              val server = lifecycle.server
              for {
                _ <- BspServerBuilder.initializeSession(
                  server = server,
                  clientName = "bleep-mcp",
                  clientVersion = model.BleepVersion.current.value,
                  rootUri = started.buildPaths.buildDir.toUri.toString
                )
                targets = BspQuery.buildTargets(started.buildPaths, targetProjects)
                classesResult <- IO.blocking(server.buildTargetScalaTestClasses(new bsp4j.ScalaTestClassesParams(targets)).get())
                _ <- IO.blocking(scala.util.Try(server.buildShutdown().get())).attempt.void
                _ <- IO.blocking(scala.util.Try(server.onBuildExit())).attempt.void
              } yield classesResult
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
        textResponse(Json.obj("projects" -> Json.arr(items*)).noSpaces)
      }
    }

    /** List projects that have a mainClass defined (runnable programs). */
    private def listPrograms(): IO[protocol.Tool.CallTool.Response] = IO {
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
      textResponse(Json.arr(programs*).noSpaces)
    }

    /** List scripts defined in the build. */
    private def listScripts(): IO[protocol.Tool.CallTool.Response] = IO {
      val scripts = started.build.scripts.toList.sortBy(_._1.value).flatMap { case (scriptName, scriptDefs) =>
        scriptDefs.values.collect { case model.ScriptDef.Main(project, main, _) =>
          Json.obj(
            "name" -> Json.fromString(scriptName.value),
            "project" -> Json.fromString(project.value),
            "mainClass" -> Json.fromString(main)
          )
        }
      }
      textResponse(Json.arr(scripts*).noSpaces)
    }

    /** Run a project or script. Checks scripts first, then projects. Compiles, then executes subprocess. */
    private def runProjectOrScript(
        name: String,
        args: List[String],
        mainClassOverride: Option[String],
        timeoutSeconds: Int,
        context: CallContext[IO]
    ): IO[protocol.Tool.CallTool.Response] = {
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
    ): IO[protocol.Tool.CallTool.Response] =
      for {
        // Compile first
        _ <- context.log(protocol.LoggingLevel.Info, s"Compiling ${project.value}...")
        _ <- compileSilently(bspConfig, Array(project))

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
        textResponse(
          Json
            .obj(
              "exitCode" -> Json.fromInt(exitCode),
              "stdout" -> Json.fromString(stdout),
              "stderr" -> Json.fromString(stderr)
            )
            .noSpaces
        )
      }

    /** Compile projects via BSP without collecting events (for run tool). */
    private def compileSilently(
        config: BspRifleConfig,
        targetProjects: Array[model.CrossProjectName]
    ): IO[Unit] =
      for {
        _ <- BspRifle.ensureRunning(config, started.logger)
        _ <- BspRifle
          .connectWithRetry(config, started.logger)
          .use { connection =>
            val client = BspClientDisplayProgress(started.logger)
            BspServerBuilder.create(connection, client).use { lifecycle =>
              val server = lifecycle.server
              for {
                _ <- BspServerBuilder.initializeSession(
                  server = server,
                  clientName = "bleep-mcp",
                  clientVersion = model.BleepVersion.current.value,
                  rootUri = started.buildPaths.buildDir.toUri.toString
                )
                targets = BspQuery.buildTargets(started.buildPaths, targetProjects)
                result <- BspRequestHelper.callCancellable {
                  val params = new bsp4j.CompileParams(targets)
                  server.buildTargetCompile(params)
                }
                _ <- IO.blocking(scala.util.Try(server.buildShutdown().get())).attempt.void
                _ <- IO.blocking(scala.util.Try(server.onBuildExit())).attempt.void
                _ <- IO.raiseWhen(result.getStatusCode != bsp4j.StatusCode.OK)(
                  new BleepException.Text(s"Compilation failed with status ${result.getStatusCode}")
                )
              } yield ()
            }
          }
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
      (stdoutBuf.toString(), stderrBuf.toString(), proc.exitValue())
    }

    // ========================================================================
    // Event consumption
    // ========================================================================

    /** Consume BSP events, filter, log via MCP, and collect for final response. */
    private def consumeAndLogEvents(
        eventQueue: Queue[IO, Option[BleepBspProtocol.Event]],
        collectedEvents: Ref[IO, List[Json]],
        context: CallContext[IO]
    ): IO[Unit] =
      eventQueue.take.flatMap {
        case Some(event) =>
          McpEventFilter.filter(event) match {
            case Some(json) =>
              val logLevel = eventLogLevel(event)
              context.log(logLevel, json) >>
                collectedEvents.update(json :: _) >>
                consumeAndLogEvents(eventQueue, collectedEvents, context)
            case None =>
              consumeAndLogEvents(eventQueue, collectedEvents, context)
          }
        case None => IO.unit
      }

    /** Determine appropriate MCP log level for a BSP event. */
    private def eventLogLevel(event: BleepBspProtocol.Event): protocol.LoggingLevel = {
      import BleepBspProtocol.{Event => E}
      event match {
        case e: E.CompileFinished if e.status == "failed"                     => protocol.LoggingLevel.Error
        case _: E.Error                                                       => protocol.LoggingLevel.Error
        case _: E.SuiteError                                                  => protocol.LoggingLevel.Error
        case _: E.SuiteTimedOut                                               => protocol.LoggingLevel.Warning
        case e: E.TestFinished if e.status == "failed" || e.status == "error" => protocol.LoggingLevel.Warning
        case _                                                                => protocol.LoggingLevel.Info
      }
    }

    // ========================================================================
    // Formatting
    // ========================================================================

    private def formatCompileResult(events: List[Json]): String = {
      val reversed = events.reverse
      val errors = reversed.filter(j => j.hcursor.get[String]("event").contains("CompileFinished") && j.hcursor.get[String]("status").contains("failed"))
      val diagnostics = reversed.flatMap(j => j.hcursor.get[List[Json]]("diagnostics").getOrElse(Nil))
      val errorCount = diagnostics.count(d => d.hcursor.get[String]("severity").contains("error"))
      val warningCount = diagnostics.count(d => d.hcursor.get[String]("severity").contains("warning"))
      val success = errors.isEmpty

      Json
        .obj(
          "success" -> Json.fromBoolean(success),
          "errors" -> Json.fromInt(errorCount),
          "warnings" -> Json.fromInt(warningCount),
          "diagnostics" -> Json.arr(diagnostics*)
        )
        .noSpaces
    }

    private def formatTestResult(events: List[Json], testRunResult: Option[BleepBspProtocol.TestRunResult]): String =
      testRunResult match {
        case Some(trr) =>
          Json
            .obj(
              "success" -> Json.fromBoolean(trr.totalFailed == 0),
              "passed" -> Json.fromInt(trr.totalPassed),
              "failed" -> Json.fromInt(trr.totalFailed),
              "skipped" -> Json.fromInt(trr.totalSkipped),
              "ignored" -> Json.fromInt(trr.totalIgnored),
              "durationMs" -> Json.fromLong(trr.durationMs),
              "failures" -> Json.arr(
                events.reverse
                  .filter(j =>
                    j.hcursor.get[String]("event").contains("TestFinished") &&
                      (j.hcursor.get[String]("status").contains("failed") || j.hcursor.get[String]("status").contains("error"))
                  )*
              )
            )
            .noSpaces
        case None =>
          val reversed = events.reverse
          val testResults = reversed.filter(j => j.hcursor.get[String]("event").contains("TestFinished"))
          val passed = testResults.count(j => j.hcursor.get[String]("status").contains("passed"))
          val failed = testResults.count(j => j.hcursor.get[String]("status").contains("failed") || j.hcursor.get[String]("status").contains("error"))
          val failures = testResults.filter(j => j.hcursor.get[String]("status").contains("failed") || j.hcursor.get[String]("status").contains("error"))

          Json
            .obj(
              "success" -> Json.fromBoolean(failed == 0),
              "passed" -> Json.fromInt(passed),
              "failed" -> Json.fromInt(failed),
              "failures" -> Json.arr(failures*)
            )
            .noSpaces
      }

    private def textResponse(text: String): protocol.Tool.CallTool.Response =
      protocol.Tool.CallTool.Response.Success(
        List(protocol.Content.Text(text)),
        None
      )

    private def errorResponse(message: String): protocol.Tool.CallTool.Response =
      protocol.Tool.CallTool.Response.Error(
        List(protocol.Content.Text(message))
      )

    /** Wrap a tool handler so exceptions become MCP error responses instead of opaque "Internal error". */
    private def withErrorReporting(
        f: (JsonObject, CallContext[IO]) => IO[protocol.Tool.CallTool.Response]
    ): (JsonObject, CallContext[IO]) => IO[protocol.Tool.CallTool.Response] =
      (args: JsonObject, context: CallContext[IO]) =>
        f(args, context).handleErrorWith { e =>
          val msg = s"${e.getClass.getSimpleName}: ${e.getMessage}"
          context.log(protocol.LoggingLevel.Error, msg) >>
            IO.pure(errorResponse(msg))
        }
  }
}

object BleepMcpServer {
  def apply(started: Started): BleepMcpServer = new BleepMcpServer(started)
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

/** BSP client that captures BleepBspProtocol events into a queue. */
private[mcp] class McpBspClient(
    eventQueue: Queue[IO, Option[BleepBspProtocol.Event]],
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
            eventQueue.offer(Some(event)).unsafeRunSync()(cats.effect.unsafe.implicits.global)
          case Left(_) => ()
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
