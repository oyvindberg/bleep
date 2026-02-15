package bleep
package commands

import bleep.bsp.{BspConnection, BspRequestHelper, BspRifle, BspRifleConfig, BspServerBuilder, BspServerClasspathSource, SetupBleepBsp}
import bleep.bsp.protocol.BleepBspProtocol
import bleep.bsp.protocol.BleepBspProtocol.BuildMode
import bleep.internal.{bleepLoggers, BspClientDisplayProgress}
import bleep.testing.{BuildDisplay, BuildEvent, BuildSummary, FancyBuildDisplay, JUnitXmlCollector}
import cats.effect._
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import ch.epfl.scala.bsp4j

import java.nio.file.Path
import scala.jdk.CollectionConverters._
import scala.util.Try
import java.util.concurrent.atomic.AtomicInteger

/** Unified reactive BSP command that handles compile, link, test, and run operations.
  *
  * All operations are delegated to bleep-bsp which handles parallel execution via TaskDag. Rich events are received via BleepBspProtocol over BSP
  * notifications.
  *
  * Benefits:
  *   - Unified TUI for all build operations
  *   - Tasks start as soon as dependencies are ready (handled by bleep-bsp's TaskDag)
  *   - Same FancyBuildDisplay for amazing TUI across all modes
  */
case class ReactiveBsp(
    watch: Boolean,
    projects: Array[model.CrossProjectName],
    mode: BuildMode,
    displayMode: DisplayMode,
    jvmOptions: List[String],
    testArgs: List[String],
    only: List[String],
    exclude: List[String],
    linkOptions: Option[LinkOptions],
    flamegraph: Boolean,
    cancel: Boolean,
    junitReportDir: Option[Path]
) extends BleepBuildCommand {

  override def run(started: Started): Either[BleepException, Unit] = {
    val targetProjects = projects.toSet

    if (targetProjects.isEmpty) {
      val modeLabel = mode match {
        case BuildMode.Compile   => "compile"
        case BuildMode.Link(_)   => "link"
        case BuildMode.Test      => "test"
        case BuildMode.Run(_, _) => "run"
      }
      started.logger.info(s"No projects to $modeLabel")
      return Right(())
    }

    started.bspServerClasspathSource match {
      case BspServerClasspathSource.InProcess(connect) =>
        runInProcess(started, connect, targetProjects)
      case BspServerClasspathSource.FromCoursier(resolver) =>
        SetupBleepBsp(
          compileServerMode = started.config.compileServerModeOrDefault,
          config = started.config,
          resolvedJvm = started.resolvedJvm.forceGet,
          userPaths = started.pre.userPaths,
          resolver = resolver,
          logger = started.logger,
          workingDir = started.buildPaths.cwd
        ) match {
          case Left(err) =>
            Left(err)
          case Right(config) =>
            runWithBleepBsp(started, config, targetProjects)
        }
    }
  }

  /** Simplified path for in-process BSP server (used by integration tests). No server lifecycle, no retry, no diag logs. */
  private def runInProcess(
      started: Started,
      connect: ryddig.Logger => Resource[IO, BspConnection],
      targetProjects: Set[model.CrossProjectName]
  ): Either[BleepException, Unit] = {
    val bspLogger = started.logger
    val diagLog: String => Unit = _ => ()

    val program: IO[BuildSummary] = for {
      eventQueue <- Queue.unbounded[IO, Option[BuildEvent]]
      displayAndCompletion <- BuildDisplay.create(false, started.logger, mode).map(d => (d, d.summary, IO.never[BuildSummary]))
      (display, signalCompletion, _) = displayAndCompletion

      junitCollector = junitReportDir.map(_ => new JUnitXmlCollector)
      bspClient = createBspClient(eventQueue, bspLogger, diagLog)
      consumeCount = new AtomicInteger(0)
      eventConsumerFiber <- consumeEvents(eventQueue, display, diagLog, consumeCount, junitCollector).start

      // Pass the rewritten build to BSP server so it can use ReplaceBleepDependencies-rewritten
      // build (with build.bleep::* deps removed and classpath overrides applied).
      // Without this, the BSP server loads the build from disk and tries to resolve
      // build.bleep::bleep-core:dev via Coursier, which fails in tests.
      buildData = {
        val classpathOverrides: Map[model.CrossProjectName, List[java.nio.file.Path]] =
          started.resolvedProjects.map { case (crossName, lazyResolved) =>
            crossName -> lazyResolved.forceGet.classpath
          }
        Some(
          bsp.BspBuildData.Payload(
            variantName = started.buildPaths.variant.name,
            build = started.build.dropBuildFile,
            classpathOverrides = classpathOverrides
          )
        )
      }

      _ <- connect(bspLogger).use { connection =>
        BspServerBuilder.create(connection, bspClient, None).use { lifecycle =>
          val server = lifecycle.server
          for {
            _ <- BspServerBuilder.initializeSession(
              server = server,
              clientName = "bleep",
              clientVersion = model.BleepVersion.current.value,
              rootUri = started.buildPaths.buildVariantDir.toUri.toString,
              buildData = buildData
            )
            targets = targetProjects.map(p => buildTarget(started.buildPaths, p)).toList
            _ <- executeBspRequest(server, targets, eventQueue, bspLogger, diagLog)
            _ <- IO.blocking(Try(server.buildShutdown().get())).attempt.void
            _ <- IO
              .blocking {
                val oldErr = System.err
                System.setErr(new java.io.PrintStream(new java.io.OutputStream { def write(b: Int): Unit = () }))
                try
                  Try(server.onBuildExit())
                finally
                  System.setErr(oldErr)
              }
              .attempt
              .void
          } yield ()
        }
      }

      _ <- eventQueue.offer(None)
      _ <- eventConsumerFiber.joinWithNever
      _ <- writeJUnitReports(started, junitReportDir, junitCollector)
      summary <- signalCompletion
      _ <- display.printSummary
    } yield summary

    try {
      val summary = program.unsafeRunSync()
      resultFromSummary(started, summary)
    } catch {
      case ex: Exception =>
        Left(new BleepException.Cause(ex, "Build failed"))
    }
  }

  private def runWithBleepBsp(
      started: Started,
      config: BspRifleConfig,
      targetProjects: Set[model.CrossProjectName]
  ): Either[BleepException, Unit] = {

    // Determine effective mode and logger upfront (no IO needed)
    val effectiveMode = DisplayMode.resolve(displayMode)
    if (displayMode == DisplayMode.Tui && effectiveMode == DisplayMode.NoTui) {
      bleep.testing.FancyBuildDisplay.checkSupport.left.foreach(reason => started.logger.warn(reason))
    }
    val isTui = effectiveMode == DisplayMode.Tui && FancyBuildDisplay.isSupported
    val bspLogger = effectiveMode match {
      case DisplayMode.Tui =>
        // Reinstall JUL bridge to file-only logger so lsp4j warnings don't clobber the TUI
        val fileLoggerResource = bleepLoggers.fileOnly(started.buildPaths.logFile).acquire()
        bleepLoggers.installJulBridge(fileLoggerResource.value)
        bleepLoggers.silent
      case DisplayMode.NoTui => started.logger
    }

    // Pre-create diagnostic log writer OUTSIDE IO monad for faster startup
    val modeSuffix = mode match {
      case BuildMode.Compile   => "compile"
      case BuildMode.Link(_)   => "link"
      case BuildMode.Test      => "test"
      case BuildMode.Run(_, _) => "run"
    }
    val diagLogFile = BspRifle.getOutputFile(config).getParent.resolve(s"client-diag-$modeSuffix.log")
    val diagLogWriter = new java.io.PrintWriter(new java.io.FileWriter(diagLogFile.toFile, false), true)
    val diagLog: String => Unit = { (msg: String) =>
      val ts = java.time.Instant.now()
      diagLogWriter.println(s"[DEBUG] $ts $msg")
    }

    val program: IO[BuildSummary] = for {
      _ <- BspRifle.ensureRunning(config, bspLogger)

      // Signal that gates the TUI — completed once BSP is connected.
      // The TUI fiber starts but waits on this before entering raw mode.
      bspReady <- Deferred[IO, Unit]

      // Create JUnit XML collector if report dir was specified
      junitCollector = junitReportDir.map(_ => new JUnitXmlCollector)

      // Create event queue for test events (used for all modes).
      // None is the poison pill — signals the consumer to stop.
      eventQueue <- Queue.unbounded[IO, Option[BuildEvent]]

      // Create display (TUI fiber starts but waits for bspReady before rendering)
      cancelBlockingSignalDefault <- Deferred[IO, Unit]
      displayAndCompletionAndQuit <- effectiveMode match {
        case DisplayMode.NoTui =>
          BuildDisplay.create(false, started.logger, mode).map(d => (d, d.summary, IO.never[BuildSummary], cancelBlockingSignalDefault))
        case DisplayMode.Tui =>
          if (FancyBuildDisplay.isSupported) {
            BuildDisplay.createFancy(mode, Some(diagLogWriter), readySignal = Some(bspReady))
          } else {
            BuildDisplay.create(true, started.logger, mode).map(d => (d, d.summary, IO.never[BuildSummary], cancelBlockingSignalDefault))
          }
      }
      (display, signalCompletion, waitForUserQuit, cancelBlockingSignal) = displayAndCompletionAndQuit

      // Create BSP client that intercepts events
      bspClient = createBspClient(eventQueue, bspLogger, diagLog)

      // Start event consumer BEFORE BSP operations.
      // Consumer blocks on queue.take and stops when it receives None (poison pill).
      consumeCount = new AtomicInteger(0)
      eventConsumerFiber <- consumeEvents(eventQueue, display, diagLog, consumeCount, junitCollector).start

      // Connect to bleep-bsp and execute the requested operation.
      // Extracted as a def so we can retry once if the server crashes.
      attemptBspOperation = BspRifle
        .connectWithRetry(config, bspLogger)
        .use { connection =>
          BspServerBuilder.create(connection, bspClient, config.traceFile, Some(diagLog)).use { lifecycle =>
            val server = lifecycle.server

            // Monitor the actual socket connection via lsp4j's listener thread.
            // MUST use IO.interruptible (not IO.blocking) so that when bspOp wins
            // the race, the FutureTask.get() call can be interrupted via Thread.interrupt().
            // IO.blocking does NOT interrupt threads on cancellation — it just marks the
            // fiber as cancelled and waits forever for the blocking call to return.
            val connectionDied: IO[Unit] =
              IO.interruptible(lifecycle.listening.get()).attempt.flatMap { result =>
                IO(
                  diagLog(
                    s"[CONNECTION_DIED] listening.get() returned: ${result.fold(ex => s"EXCEPTION: ${ex.getClass.getName}: ${ex.getMessage}", _ => "OK (void)")}"
                  )
                )
              }

            // Main BSP request: initialize session then run compile/test/link.
            // This is the part we race against connectionDied — if the server
            // crashes mid-request, we want to detect it and stop waiting.
            val mainRequest = for {
              _ <- IO(diagLog("[BSP_OP] Starting initialize"))
              _ <- BspServerBuilder.initializeSession(
                server = server,
                clientName = "bleep",
                clientVersion = model.BleepVersion.current.value,
                rootUri = started.buildPaths.buildVariantDir.toUri.toString
              )

              _ <- IO(diagLog(s"[BSP_OP] Initialize complete, starting main operation. mode=$mode targets=${targetProjects.map(_.value).mkString(",")}"))
              targets = targetProjects.map(p => buildTarget(started.buildPaths, p)).toList
              _ <- executeBspRequest(server, targets, eventQueue, bspLogger, diagLog)
            } yield ()

            // Shutdown is separate from the race — it runs unconditionally after
            // the main request completes (or connectionDied fires). When the server
            // receives buildShutdown + onBuildExit, it closes the connection, which
            // would cause connectionDied to fire. If shutdown were inside the race,
            // connectionDied could win during the shutdown sequence and cancel the
            // bspOp fiber, losing any post-request work (like event processing).
            val shutdown = for {
              _ <- IO(diagLog("[BSP_OP] Main operation complete, sending shutdown"))
              _ <- IO.blocking(Try(server.buildShutdown().get())).attempt.void
              _ <- IO.blocking(Try(server.onBuildExit())).attempt.void
            } yield ()

            // When TUI signals "cancel blocking work", send notification to server.
            // This fires only once (Deferred completes once); uses a fire-and-forget fiber.
            val cancelBlockingFiber: IO[FiberIO[Unit]] =
              (cancelBlockingSignal.get >> IO.blocking(server.cancelBlockingWork())).start

            // Signal TUI that BSP is connected (idempotent — on retry the
            // Deferred is already completed, so .attempt swallows the error).
            bspReady.complete(()).attempt.void >>
              cancelBlockingFiber.flatMap { cbFiber =>
                // Race the main BSP request against connection death.
                // If the server crashes mid-request, connectionDied completes and
                // mainRequest is cancelled. If mainRequest completes normally,
                // connectionDied is cancelled (interrupted).
                //
                // When connectionDied wins, the server has crashed — skip shutdown
                // (server is already dead) and return ServerCrashed so the caller
                // can retry with a fresh server.
                IO.race(connectionDied, mainRequest)
                  .flatMap {
                    case Left(_) =>
                      val rc = bspClient.asInstanceOf[ReactiveBspClient]
                      IO(
                        diagLog(
                          s"[RACE] connectionDied won - server crashed. started=${rc.suiteStartedCount.get()} finished=${rc.suiteFinishedCount.get()} emitted=${rc.emitCount.get()} emitFails=${rc.emitFailCount.get()}"
                        )
                      ).as(ReactiveBsp.BspAttemptResult.ServerCrashed)
                    case Right(_) =>
                      val rc = bspClient.asInstanceOf[ReactiveBspClient]
                      IO(
                        diagLog(
                          s"[RACE] bspOp won - success. started=${rc.suiteStartedCount.get()} finished=${rc.suiteFinishedCount.get()} emitted=${rc.emitCount.get()} emitFails=${rc.emitFailCount.get()}"
                        )
                      ) >> shutdown.as(ReactiveBsp.BspAttemptResult.Success)
                  }
                  .guarantee(cbFiber.cancel)
              }
          }
        }

      // Retry once if the server crashed (e.g. after `bleep clean` deleted class files).
      // Kill the dead server, start a fresh one, and reconnect.
      bspOperation = attemptBspOperation.flatMap {
        case ReactiveBsp.BspAttemptResult.Success => IO.unit
        case ReactiveBsp.BspAttemptResult.ServerCrashed =>
          IO(bspLogger.warn("BSP server crashed, restarting and retrying...")) >>
            IO(diagLog("[RETRY] Server crashed, killing stale server and retrying")) >>
            BspRifle.forceStop(config) >>
            BspRifle.ensureRunning(config, bspLogger) >>
            attemptBspOperation.void
      }

      // Race BSP operation with user quit - pressing 'q' cancels BSP immediately
      raceResult <- IO.race(waitForUserQuit.void, bspOperation).attempt
      _ <- raceResult match {
        case Left(err) =>
          IO(diagLog(s"[ERROR] BSP operation failed: ${err.getClass.getName}: ${err.getMessage}")) >>
            IO(started.logger.error(s"BSP operation failed: ${err.getMessage}"))
        case Right(_) => IO.unit
      }

      // Signal the event consumer to stop, then wait for it to finish processing
      // all queued events. The consumer processes events in FIFO order, so by the
      // time it hits the None poison pill, all real events have been handled.
      _ <- IO {
        val rc = bspClient.asInstanceOf[ReactiveBspClient]
        diagLog(
          s"[POISON] About to offer poison pill. started=${rc.suiteStartedCount.get()} finished=${rc.suiteFinishedCount.get()} emitted=${rc.emitCount.get()} emitFails=${rc.emitFailCount.get()}"
        )
      }
      _ <- eventQueue.offer(None)
      _ <- eventConsumerFiber.joinWithNever
      _ <- IO {
        val rc = bspClient.asInstanceOf[ReactiveBspClient]
        diagLog(
          s"[DONE] Consumer finished. started=${rc.suiteStartedCount.get()} finished=${rc.suiteFinishedCount.get()} emitted=${rc.emitCount.get()} emitFails=${rc.emitFailCount.get()}"
        )
      }

      // Write JUnit XML reports after all events have been consumed
      _ <- writeJUnitReports(started, junitReportDir, junitCollector)

      // Always signal completion to TUI (even if BSP failed or user quit)
      summary <- signalCompletion
      _ <- display.printSummary
      _ <- IO.delay(started.logger.info(s"  BSP server log: ${BspRifle.getOutputFile(config)}"))
      _ <-
        if (flamegraph)
          IO.delay(started.logger.info(s"  Flamegraph: ${started.buildPaths.dotBleepDir.resolve("trace.json")} (open in chrome://tracing or ui.perfetto.dev)"))
        else IO.unit
      _ <- IO(diagLogWriter.close())
    } yield summary

    val startTime = System.currentTimeMillis()

    var summaryOpt: Option[BuildSummary] = None
    var errorOpt: Option[Throwable] = None

    try {
      val summary = program.unsafeRunSync()
      summaryOpt = Some(summary)
      resultFromSummary(started, summary)
    } catch {
      case ex: Exception =>
        errorOpt = Some(ex)
        started.logger.debug(s"Build failed: ${ex.getMessage}")
        started.logger.debug(ex.getStackTrace.mkString("\n"))
        Left(new BleepException.Cause(ex, "Build failed"))
    } finally
      // ALWAYS print final summary, regardless of outcome
      if (isTui) {
        val durationMs = System.currentTimeMillis() - startTime
        summaryOpt match {
          case Some(summary) =>
            printFinalSummary(started, summary)
          case None =>
            errorOpt match {
              case Some(err) => printErrorSummary(started, err, durationMs)
              case None      => printCancelledSummary(started, durationMs)
            }
        }
        started.logger.info(s"  BSP server log: ${BspRifle.getOutputFile(config)}")
        if (flamegraph)
          started.logger.info(s"  Flamegraph: ${started.buildPaths.dotBleepDir.resolve("trace.json")} (open in chrome://tracing or ui.perfetto.dev)")
      }
  }

  private def printErrorSummary(started: Started, err: Throwable, durationMs: Long): Unit = {
    import scala.{Console => C}
    val logger = started.logger
    logger.info("")
    logger.info(s"${C.RED}${C.BOLD}x Build Failed${C.RESET}")
    logger.info("")
    logger.info(s"  Error: ${err.getMessage}")
    logger.info(s"  Duration: ${durationMs / 1000.0}s")
    logger.info("")
  }

  private def printCancelledSummary(started: Started, durationMs: Long): Unit = {
    import scala.{Console => C}
    val logger = started.logger
    logger.info("")
    logger.info(s"${C.YELLOW}${C.BOLD}! Build Cancelled${C.RESET}")
    logger.info("")
    logger.info(s"  Duration: ${durationMs / 1000.0}s")
    logger.info("")
  }

  /** Print final summary after TUI exits - ALWAYS shows what happened */
  private def printFinalSummary(started: Started, summary: BuildSummary): Unit = {
    val logger = started.logger
    BuildSummary.formatSummary(summary, mode).foreach(logger.info(_))
  }

  /** Create BSP client that intercepts events and forwards to display */
  private def createBspClient(
      eventQueue: Queue[IO, Option[BuildEvent]],
      logger: ryddig.Logger,
      diagLog: String => Unit
  ): bsp4j.BuildClient = {
    val baseClient = BspClientDisplayProgress(logger)
    new ReactiveBspClient(baseClient, eventQueue, diagLog)
  }

  /** Consume events from queue and forward to display. Stops on None (poison pill). */
  private def consumeEvents(
      queue: Queue[IO, Option[BuildEvent]],
      display: BuildDisplay,
      diagLog: String => Unit,
      count: AtomicInteger,
      junitCollector: Option[JUnitXmlCollector]
  ): IO[Unit] =
    queue.take.flatMap {
      case Some(event) =>
        val n = count.incrementAndGet()
        val collectJunit = IO(junitCollector.foreach(_.handle(event)))
        event match {
          case sf: BuildEvent.SuiteFinished =>
            IO(diagLog(s"[CONSUME#$n] SuiteFinished suite=${sf.suite}")) >> collectJunit >> display.handle(event) >> consumeEvents(
              queue,
              display,
              diagLog,
              count,
              junitCollector
            )
          case _: BuildEvent.TestRunCompleted =>
            IO(diagLog(s"[CONSUME#$n] TestRunCompleted")) >> collectJunit >> display.handle(event) >> consumeEvents(
              queue,
              display,
              diagLog,
              count,
              junitCollector
            )
          case _ =>
            collectJunit >> display.handle(event) >> consumeEvents(queue, display, diagLog, count, junitCollector)
        }
      case None =>
        IO(diagLog(s"[CONSUME] Poison pill received after ${count.get()} events")) // Done — all events processed
    }

  /** Execute BSP request based on mode (compile/test/link/run). Shared by runWithBleepBsp and runInProcess. */
  private def executeBspRequest(
      server: bsp.BuildServer,
      targets: List[bsp4j.BuildTargetIdentifier],
      eventQueue: Queue[IO, Option[BuildEvent]],
      bspLogger: ryddig.Logger,
      diagLog: String => Unit
  ): IO[Unit] = {
    val commonArgs = if (flamegraph) List("--flamegraph") else Nil

    mode match {
      case BuildMode.Compile =>
        BspRequestHelper.callCancellable {
          val params = new bsp4j.CompileParams(targets.asJava)
          if (commonArgs.nonEmpty) params.setArguments(commonArgs.asJava)
          server.buildTargetCompile(params)
        }.void

      case BuildMode.Link(_) =>
        BspRequestHelper.callCancellable {
          val params = new bsp4j.CompileParams(targets.asJava)
          val args = linkOptions.map(_.toArgs).getOrElse(List("--link")) ++ commonArgs
          params.setArguments(args.asJava)
          server.buildTargetCompile(params)
        }.void

      case BuildMode.Test =>
        BspRequestHelper
          .callCancellable {
            val params = new bsp4j.TestParams(targets.asJava)
            val testOptions = BleepBspProtocol.TestOptions(jvmOptions, testArgs, only, exclude, flamegraph)
            params.setDataKind(BleepBspProtocol.TestOptionsDataKind)
            params.setData(com.google.gson.JsonParser.parseString(BleepBspProtocol.TestOptions.encode(testOptions)))
            server.buildTargetTest(params)
          }
          .flatTap { testResult =>
            IO.delay {
              for {
                dataKind <- Option(testResult.getDataKind)
                if dataKind == BleepBspProtocol.TestRunResultDataKind
                data <- Option(testResult.getData)
                jsonStr = data.toString
                result <- BleepBspProtocol.TestRunResult.decode(jsonStr) match {
                  case Right(r) => Some(r)
                  case Left(err) =>
                    bspLogger.warn(s"Failed to decode TestRunResult: ${err.getMessage}, json: $jsonStr")
                    None
                }
              } yield result
            }.flatMap {
              case Some(result) =>
                IO(
                  diagLog(
                    s"[TestRunResult] total=${result.suitesTotal} completed=${result.suitesCompleted} cancelled=${result.suitesCancelled} passed=${result.totalPassed} failed=${result.totalFailed}"
                  )
                ) >>
                  eventQueue.offer(
                    Some(
                      BuildEvent.TestRunCompleted(
                        project = "",
                        totalPassed = result.totalPassed,
                        totalFailed = result.totalFailed,
                        totalSkipped = result.totalSkipped,
                        totalIgnored = result.totalIgnored,
                        suitesTotal = result.suitesTotal,
                        suitesCompleted = result.suitesCompleted,
                        suitesFailed = result.suitesFailed,
                        suitesCancelled = result.suitesCancelled,
                        durationMs = result.durationMs,
                        timestamp = System.currentTimeMillis()
                      )
                    )
                  )
              case None =>
                IO(diagLog("[TestRunResult] NOT decoded - None"))
            }.handleErrorWith { e =>
              IO(bspLogger.warn(s"Failed to extract TestRunResult from response: ${e.getMessage}"))
            }
          }
          .void

      case BuildMode.Run(_, _) =>
        BspRequestHelper.callCancellable {
          val params = new bsp4j.CompileParams(targets.asJava)
          server.buildTargetCompile(params)
        }.void
    }
  }

  /** Write JUnit XML reports if configured */
  private def writeJUnitReports(started: Started, reportDir: Option[Path], collector: Option[JUnitXmlCollector]): IO[Unit] =
    (reportDir, collector) match {
      case (Some(dir), Some(c)) =>
        IO {
          val files = c.writeReports(dir)
          files.foreach(f => started.logger.info(s"  JUnit XML report: $f"))
        }
      case _ => IO.unit
    }

  /** Convert build summary to result. Shared by runWithBleepBsp and runInProcess. */
  private def resultFromSummary(started: Started, summary: BuildSummary): Either[BleepException, Unit] =
    mode match {
      case BuildMode.Test =>
        val totalProblems = summary.testsFailed + summary.testsTimedOut + summary.testsCancelled
        if (totalProblems > 0 || summary.suitesCancelled > 0 || summary.compileFailures.nonEmpty) {
          val parts = List.newBuilder[String]
          parts += s"${summary.testsPassed} passed"
          if (summary.testsFailed > 0) parts += s"${summary.testsFailed} failed"
          if (summary.testsTimedOut > 0) parts += s"${summary.testsTimedOut} timed out"
          if (summary.testsCancelled > 0) parts += s"${summary.testsCancelled} cancelled"
          if (summary.suitesCancelled > 0) parts += s"${summary.suitesCancelled} suites cancelled"
          if (summary.compileFailures.nonEmpty) parts += s"${summary.compileFailures.size} compile failures"
          Left(new BleepException.Text(s"Tests failed: ${parts.result().mkString(", ")}"))
        } else {
          Right(())
        }
      case _ =>
        if (summary.compilesCancelled > 0 && summary.compileFailures.isEmpty) {
          Left(new BleepException.Text("Build cancelled by user"))
        } else if (summary.compileFailures.nonEmpty) {
          Left(new BleepException.Text(s"Build failed: ${summary.compileFailures.size} project(s) failed to compile"))
        } else {
          Right(())
        }
    }

  /** Build target identifier for a project */
  private def buildTarget(buildPaths: BuildPaths, name: model.CrossProjectName): bsp4j.BuildTargetIdentifier = {
    val id = buildPaths.buildVariantDir.toFile.toURI.toASCIIString.stripSuffix("/") + "/?id=" + name.value
    val amended = id.replace("file:///", "file:/")
    new bsp4j.BuildTargetIdentifier(amended)
  }
}

/** BSP client that converts BleepBspProtocol events to BuildEvents */
class ReactiveBspClient(
    delegate: bsp4j.BuildClient,
    eventQueue: Queue[IO, Option[BuildEvent]],
    diagLog: String => Unit
) extends bsp4j.BuildClient {

  val suiteStartedCount = new AtomicInteger(0)
  val suiteFinishedCount = new AtomicInteger(0)
  val emitCount = new AtomicInteger(0)
  val emitFailCount = new AtomicInteger(0)
  val onBuildTaskProgressCount = new AtomicInteger(0)

  private def emit(event: BuildEvent): Unit = {
    val n = emitCount.incrementAndGet()
    event match {
      case _: BuildEvent.SuiteStarted =>
        val c = suiteStartedCount.incrementAndGet()
        diagLog(s"[emit#$n] SuiteStarted #$c thread=${Thread.currentThread().getName}")
      case sf: BuildEvent.SuiteFinished =>
        val c = suiteFinishedCount.incrementAndGet()
        diagLog(s"[emit#$n] SuiteFinished #$c suite=${sf.suite} thread=${Thread.currentThread().getName}")
      case _: BuildEvent.TestRunCompleted =>
        diagLog(s"[emit#$n] TestRunCompleted started=${suiteStartedCount.get()} finished=${suiteFinishedCount.get()} thread=${Thread.currentThread().getName}")
      case _ => ()
    }
    try
      eventQueue.offer(Some(event)).unsafeRunSync()
    catch {
      case ex: Exception =>
        val f = emitFailCount.incrementAndGet()
        diagLog(s"[emit#$n] FAILED #$f: ${ex.getClass.getSimpleName}: ${ex.getMessage} thread=${Thread.currentThread().getName}")
        throw ex
    }
  }

  override def onBuildShowMessage(params: bsp4j.ShowMessageParams): Unit =
    delegate.onBuildShowMessage(params)

  override def onBuildLogMessage(params: bsp4j.LogMessageParams): Unit =
    delegate.onBuildLogMessage(params)

  override def onBuildTaskStart(params: bsp4j.TaskStartParams): Unit = {
    if (Option(params.getDataKind).contains("compile-task")) {
      Option(params.getData).foreach { data =>
        extractBuildTarget(data).foreach { project =>
          emit(BuildEvent.CompileStarted(project, System.currentTimeMillis()))
        }
      }
    }
    delegate.onBuildTaskStart(params)
  }

  override def onBuildTaskProgress(params: bsp4j.TaskProgressParams): Unit = {
    val dataKind = Option(params.getDataKind)
    val progressCount = onBuildTaskProgressCount.incrementAndGet()
    if (dataKind.contains(BleepBspProtocol.DataKind)) {
      Option(params.getData).foreach { jsonData =>
        val jsonStr = jsonData match {
          case s: String => s
          case other     => other.toString
        }

        BleepBspProtocol.decode(jsonStr) match {
          case Right(event) =>
            val eventType = event.getClass.getSimpleName
            // Log all suite-related events, and first 10 + every 500th other event
            event match {
              case _: BleepBspProtocol.Event.SuiteStarted | _: BleepBspProtocol.Event.SuiteFinished | _: BleepBspProtocol.Event.SuiteCancelled |
                  _: BleepBspProtocol.Event.SuiteError | _: BleepBspProtocol.Event.SuiteTimedOut =>
                diagLog(s"[progress#$progressCount] $eventType thread=${Thread.currentThread().getName}")
              case _ =>
                if (progressCount <= 10 || progressCount % 500 == 0)
                  diagLog(s"[progress#$progressCount] $eventType")
            }
            convertToBuildEvent(event).foreach(emit)
          case Left(err) =>
            diagLog(s"[progress#$progressCount] decode FAILED: ${err.getMessage} json=${jsonStr.take(200)}")
        }
      }
    } else {
      if (progressCount <= 3)
        diagLog(s"[progress#$progressCount] non-bleep dataKind=${dataKind.getOrElse("null")}")
    }
    delegate.onBuildTaskProgress(params)
  }

  override def onBuildTaskFinish(params: bsp4j.TaskFinishParams): Unit = {
    if (Option(params.getDataKind).contains("compile-report")) {
      Option(params.getData).foreach { data =>
        extractBuildTarget(data).foreach { project =>
          val status = if (params.getStatus == bsp4j.StatusCode.OK) "success" else "failed"
          emit(BuildEvent.CompileFinished(project, status, 0, System.currentTimeMillis()))
        }
      }
    }
    delegate.onBuildTaskFinish(params)
  }

  override def onBuildPublishDiagnostics(params: bsp4j.PublishDiagnosticsParams): Unit =
    delegate.onBuildPublishDiagnostics(params)

  override def onBuildTargetDidChange(params: bsp4j.DidChangeBuildTarget): Unit =
    delegate.onBuildTargetDidChange(params)

  override def onRunPrintStdout(params: bsp4j.PrintParams): Unit =
    delegate.onRunPrintStdout(params)

  override def onRunPrintStderr(params: bsp4j.PrintParams): Unit =
    delegate.onRunPrintStderr(params)

  /** Extract build target ID from bsp4j params data */
  private def extractBuildTarget(data: AnyRef): Option[String] =
    try
      data match {
        case obj: com.google.gson.JsonObject =>
          val target = obj.get("target") match {
            case t: com.google.gson.JsonObject => t
            case _                             => return None
          }
          val uri = target.get("uri") match {
            case str: com.google.gson.JsonPrimitive => str.getAsString
            case _                                  => return None
          }
          uri.split("\\?id=").lastOption
        case _ => None
      }
    catch {
      case _: Exception => None
    }

  /** Convert BleepBspProtocol event to BuildEvent */
  private def convertToBuildEvent(event: BleepBspProtocol.Event): Option[BuildEvent] = {
    import BleepBspProtocol.{Event => PE}

    event match {
      case PE.CompileStarted(project, timestamp) =>
        Some(BuildEvent.CompileStarted(project, timestamp))

      case PE.CompilationReason(project, reason, totalFiles, invalidatedFiles, changedDeps, timestamp) =>
        Some(BuildEvent.CompilationReason(project, reason, totalFiles, invalidatedFiles, changedDeps, timestamp))

      case PE.CompileProgress(project, percentage, timestamp) =>
        Some(BuildEvent.CompileProgress(project, percentage, timestamp))

      case PE.CompileFinished(project, status, durationMs, diagnostics, skippedBecause, timestamp) =>
        Some(BuildEvent.CompileFinished(project, status, durationMs, timestamp, diagnostics, skippedBecause))

      case PE.SuitesDiscovered(project, suites, totalDiscovered, timestamp) =>
        Some(BuildEvent.SuitesDiscovered(project, suites, totalDiscovered, timestamp))

      case PE.SuiteStarted(project, suite, timestamp) =>
        Some(BuildEvent.SuiteStarted(project, suite, timestamp))

      case PE.TestStarted(project, suite, test, timestamp) =>
        Some(BuildEvent.TestStarted(project, suite, test, timestamp))

      case PE.TestFinished(project, suite, test, status, durationMs, message, throwable, timestamp) =>
        val testStatus = bleep.testing.TestStatus.fromString(status)
        Some(BuildEvent.TestFinished(project, suite, test, testStatus, durationMs, message, throwable, timestamp))

      case PE.SuiteFinished(project, suite, passed, failed, skipped, ignored, durationMs, timestamp) =>
        Some(BuildEvent.SuiteFinished(project, suite, passed, failed, skipped, ignored, durationMs, timestamp))

      case PE.SuiteTimedOut(project, suite, timeoutMs, threadDump, timestamp) =>
        Some(BuildEvent.SuiteTimedOut(project, suite, timeoutMs, threadDump.map(d => bleep.testing.ThreadDumpInfo(0, Some(d), None)), timestamp))

      case PE.SuiteError(project, suite, error, exitCode, signal, durationMs, timestamp) =>
        Some(BuildEvent.SuiteError(project, suite, error, exitCode, signal, durationMs, timestamp))

      case PE.SuiteCancelled(project, suite, reason, timestamp) =>
        Some(BuildEvent.SuiteCancelled(project, suite, reason, timestamp))

      case PE.ProjectSkipped(project, reason, timestamp) =>
        Some(BuildEvent.ProjectSkipped(project, reason, timestamp))

      case PE.Output(project, suite, line, isError, timestamp) =>
        Some(BuildEvent.Output(project, suite, line, isError, timestamp))

      case PE.Error(message, details, timestamp) =>
        Some(BuildEvent.Error("", message, details, timestamp))

      case PE.LinkStarted(project, platform, timestamp) =>
        Some(BuildEvent.LinkStarted(project, platform, timestamp))

      case PE.LinkFinished(project, success, durationMs, _, timestamp, platform, error) =>
        if (success)
          Some(BuildEvent.LinkSucceeded(project, platform, durationMs, timestamp))
        else
          Some(BuildEvent.LinkFailed(project, platform, durationMs, error.getOrElse("Link failed"), timestamp))

      case PE.SourcegenStarted(scriptMain, forProjects, timestamp) =>
        Some(BuildEvent.SourcegenStarted(scriptMain, forProjects, timestamp))

      case PE.SourcegenFinished(scriptMain, success, durationMs, error, timestamp) =>
        Some(BuildEvent.SourcegenFinished(scriptMain, success, durationMs, error, timestamp))

      case PE.WorkspaceBusy(operation, projects, startedAgoMs, timestamp) =>
        Some(BuildEvent.WorkspaceBusy("", operation, projects, startedAgoMs, timestamp))

      case PE.WorkspaceReady(timestamp) =>
        Some(BuildEvent.WorkspaceReady("", timestamp))

      // Events not relevant for BuildDisplay
      case PE.TestRunFinished(_, _, _, _, _, _) => None
      case PE.DiscoveryStarted(_, _)            => None
      case PE.BuildInitialized(_, _, _)         => None
      case PE.ProjectStateChanged(_, _, _, _)   => None
      case PE.BuildFinished(_, _, _)            => None
      case PE.LinkProgress(_, _, _)             => None
      case PE.RunStarted(_, _, _)               => None
      case PE.RunOutput(_, _, _, _)             => None
      case PE.RunFinished(_, _, _, _)           => None
      case PE.LogMessage(_, _, _, _)            => None
    }
  }
}

object ReactiveBsp {
  private sealed trait BspAttemptResult
  private object BspAttemptResult {
    case object Success extends BspAttemptResult
    case object ServerCrashed extends BspAttemptResult
  }

  /** Create compile-bsp command */
  def compile(
      watch: Boolean,
      projects: Array[model.CrossProjectName],
      displayMode: DisplayMode,
      flamegraph: Boolean,
      cancel: Boolean
  ): ReactiveBsp = ReactiveBsp(
    watch = watch,
    projects = projects,
    mode = BuildMode.Compile,
    displayMode = displayMode,
    jvmOptions = Nil,
    testArgs = Nil,
    only = Nil,
    exclude = Nil,
    linkOptions = None,
    flamegraph = flamegraph,
    cancel = cancel,
    junitReportDir = None
  )

  /** Create test-bsp command */
  def test(
      watch: Boolean,
      projects: Array[model.CrossProjectName],
      displayMode: DisplayMode,
      jvmOptions: List[String],
      testArgs: List[String],
      only: List[String],
      exclude: List[String],
      flamegraph: Boolean,
      cancel: Boolean,
      junitReportDir: Option[Path]
  ): ReactiveBsp = ReactiveBsp(
    watch = watch,
    projects = projects,
    mode = BuildMode.Test,
    displayMode = displayMode,
    jvmOptions = jvmOptions,
    testArgs = testArgs,
    only = only,
    exclude = exclude,
    linkOptions = None,
    flamegraph = flamegraph,
    cancel = cancel,
    junitReportDir = junitReportDir
  )

  /** Create link-bsp command */
  def link(
      watch: Boolean,
      projects: Array[model.CrossProjectName],
      displayMode: DisplayMode,
      options: LinkOptions,
      flamegraph: Boolean,
      cancel: Boolean
  ): ReactiveBsp = ReactiveBsp(
    watch = watch,
    projects = projects,
    mode = BuildMode.Link(options.releaseMode),
    displayMode = displayMode,
    jvmOptions = Nil,
    testArgs = Nil,
    only = Nil,
    exclude = Nil,
    linkOptions = Some(options),
    flamegraph = flamegraph,
    cancel = cancel,
    junitReportDir = None
  )
}
