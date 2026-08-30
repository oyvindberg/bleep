package bleep.bsp

import bleep.analysis._
import bleep.bsp.protocol.KillReason
import bleep.bsp.TaskDag.LinkResult
import bleep.bsp.TestRunnerTypes.{RunnerEvent, TerminationReason, TestEventHandler, TestResult, TestSuite}
import bleep.bsp.protocol.{OutputChannel, TestStatus}
import cats.effect.{Deferred, IO, Resource}
import cats.effect.std.Semaphore
import java.io.{BufferedReader, InputStreamReader}
import java.net.{InetAddress, ServerSocket}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.Properties
import java.util.concurrent.atomic.AtomicReference
import scala.jdk.CollectionConverters._

/** Runner for Scala Native tests. */
object ScalaNativeTestRunner {

  /** Detected test framework. */
  sealed trait TestFramework {
    def name: String
  }
  object TestFramework {
    case object MUnit extends TestFramework { val name = "munit" }
    case object ScalaTest extends TestFramework { val name = "scalatest" }
    case object UTest extends TestFramework { val name = "utest" }
    case object Unknown extends TestFramework { val name = "unknown" }
  }

  /** Bridge a Deferred kill signal to CancellationToken. Delegates to Outcome.bridgeKillSignal, which returns a Resource that properly lifecycle-manages the
    * listener fiber.
    */
  private def bridgeKillSignal(killSignal: Deferred[IO, KillReason]): cats.effect.Resource[IO, CancellationToken] =
    Outcome.bridgeKillSignal(killSignal)

  /** Link a native test binary with embedded test runner. */
  def linkTestBinary(
      toolchain: ScalaNativeToolchain,
      classpath: Seq[Path],
      testMainClass: String,
      config: ScalaNativeLinkConfig,
      outputPath: Path,
      workDir: Path,
      logger: ScalaNativeToolchain.Logger,
      killSignal: Deferred[IO, KillReason]
  ): IO[LinkResult] =
    bridgeKillSignal(killSignal).use { cancellation =>
      IO.blocking {
        Files.createDirectories(workDir)
        Files.createDirectories(outputPath.getParent)
      } >> toolchain
        .link(config, classpath, testMainClass, outputPath, workDir, logger, cancellation)
        .map {
          case Outcome.ThreadOutcome.Completed(result) =>
            if (result.isSuccess) LinkResult.NativeSuccess(result.binary, wasUpToDate = false)
            else LinkResult.Failure(s"Linking failed with exit code ${result.exitCode}", List.empty)
          case Outcome.ThreadOutcome.Cancelled(_) =>
            LinkResult.Cancelled
          case Outcome.ThreadOutcome.Crashed(ex) =>
            LinkResult.Failure(ex.getMessage, List.empty)
        }
    }

  /** Discover test suites from a linked native binary. */
  def discoverSuites(
      binary: Path,
      @annotation.unused classpath: Seq[Path],
      killSignal: Deferred[IO, KillReason]
  ): IO[ProcessRunner.DiscoveryResult[List[TestSuite]]] =
    killSignal.tryGet.flatMap {
      case Some(reason)                        => IO.pure(ProcessRunner.DiscoveryResult.Killed(reason))
      case None if !Files.isExecutable(binary) =>
        IO.pure(ProcessRunner.DiscoveryResult.Failed(s"Binary is not executable: $binary"))
      case None =>
        val pb = new ProcessBuilder(binary.toAbsolutePath.toString, "--list-tests")
          .redirectErrorStream(true)

        val work = ProcessRunner
          .start(pb)
          .use { process =>
            ProcessRunner.lines(process.getInputStream).compile.toList.flatMap { outputLines =>
              IO.blocking(process.waitFor()).flatMap { exitCode =>
                if (exitCode == 0) {
                  IO.pure(
                    outputLines
                      .filter(_.nonEmpty)
                      .map { line =>
                        val name = line.split('.').lastOption.getOrElse(line)
                        TestSuite(name, line.trim)
                      }
                  )
                } else {
                  IO.raiseError(new RuntimeException(s"Native test discovery failed with exit code $exitCode"))
                }
              }
            }
          }

        Outcome.raceKill(killSignal)(work).map {
          case Left(result)  => ProcessRunner.DiscoveryResult.Found(result)
          case Right(reason) => ProcessRunner.DiscoveryResult.Killed(reason)
        }
    }

  /** Run tests in a Scala Native binary. */
  def runTests(
      binary: Path,
      suites: List[TestSuite],
      framework: TestFramework,
      eventHandler: TestEventHandler,
      env: Map[String, String],
      workingDir: Path,
      killSignal: Deferred[IO, KillReason]
  ): IO[TestResult] =
    killSignal.tryGet.flatMap {
      case Some(reason) => IO.pure(TestResult(0, 0, 0, 0, TerminationReason.Killed(reason)))
      case None         =>
        IO.blocking {
          if (!Files.isExecutable(binary)) {
            binary.toFile.setExecutable(true): Unit
          }
        } >> {
          val command = if (suites.isEmpty) {
            Seq(binary.toAbsolutePath.toString)
          } else {
            Seq(binary.toAbsolutePath.toString) ++ suites.map(_.fullyQualifiedName)
          }

          val pb = new ProcessBuilder(command.asJava)
            .directory(workingDir.toFile)
          env.foreach { case (k, v) => pb.environment().put(k, v) }

          Semaphore[IO](1).flatMap { parserLock =>
            val parser = framework match {
              case TestFramework.MUnit     => new MUnitOutputParser(eventHandler)
              case TestFramework.ScalaTest => new ScalaTestOutputParser(eventHandler)
              case TestFramework.UTest     => new UTestOutputParser(eventHandler)
              case TestFramework.Unknown   => new GenericOutputParser(eventHandler)
            }

            ProcessTestRunner.run(
              ProcessTestRunner.Config(
                processBuilder = pb,
                handleStdoutLine = { line =>
                  parserLock.permit.surround(IO.delay(parser.parseLine(line)))
                },
                handleStderrLine = { line =>
                  parserLock.permit.surround(IO.delay(parser.parseError(line)))
                },
                getRunState = IO.delay {
                  val counts = parser.getCounts
                  ProcessTestRunner.RunState(counts._1, counts._2, counts._3, counts._4, parser.unfinishedSuite)
                },
                eventHandler = eventHandler,
                killSignal = killSignal,
                killDescendants = true,
                preRun = IO.delay(eventHandler.onRunnerEvent(RunnerEvent.Started)),
                onNormalExit = IO.unit,
                cleanup = IO.unit
              )
            )
          }
        }
    }

  def detectFramework(classpath: Seq[Path]): TestFramework = {
    val classpathStr = classpath.map(_.toString).mkString
    if (classpathStr.contains("munit")) TestFramework.MUnit
    else if (classpathStr.contains("scalatest")) TestFramework.ScalaTest
    else if (classpathStr.contains("utest")) TestFramework.UTest
    else TestFramework.Unknown
  }

  /** The main class for Scala Native test binaries.
    *
    * All Scala Native test binaries use the same TestMain from the scala-native test interface. TestMain communicates with the JVM-side TestAdapter via a TCP
    * socket-based RPC protocol.
    */
  val TestMainClass: String = "scala.scalanative.testinterface.TestMain"

  /** Get the test main class for a Scala Native test binary.
    *
    * All frameworks use the same TestMain entry point from the scala-native test interface. The framework is detected at runtime by TestMain via the
    * sbt.testing.Framework SPI.
    */
  def getTestMainClass(@annotation.unused framework: TestFramework): String = TestMainClass

  /** Run tests in a Scala Native binary using the TestAdapter protocol.
    *
    * This is the proper way to communicate with binaries linked with TestMain. The TestAdapter opens a server socket, passes the port to the binary, and
    * communicates via RPC to discover and execute tests.
    *
    * @param binary
    *   the linked native test binary
    * @param suites
    *   the test suites to run (used for filtering)
    * @param framework
    *   the detected test framework
    * @param eventHandler
    *   handler for test events
    * @param env
    *   environment variables
    * @param workingDir
    *   working directory
    * @param scalaNativeVersion
    *   Scala Native version (e.g., "0.5.6")
    * @param killSignal
    *   signal for cancellation
    * @return
    *   test result
    */
  def runTestsViaAdapter(
      binary: Path,
      suites: List[TestSuite],
      // Unused, and was already unused one level down: the adapter asks the linked binary which frameworks it carries rather than being told.
      @annotation.unused framework: TestFramework,
      eventHandler: TestEventHandler,
      env: Map[String, String],
      scalaNativeVersion: String,
      classpath: List[Path],
      killSignal: Deferred[IO, KillReason]
  ): IO[TestResult] =
    killSignal.tryGet.flatMap {
      case Some(reason) => IO.pure(TestResult(0, 0, 0, 0, TerminationReason.Killed(reason)))
      case None         => runForked(binary, suites, eventHandler, env, scalaNativeVersion, classpath, killSignal)
    }

  /** Run the adapter in a forked JVM, so the test binary's output is captured.
    *
    * The adapter itself is unchanged — [[ScalaNativeTestFork]] calls exactly the same code this used to call in-process. What changes is *whose* file
    * descriptors the test binary inherits. Scala Native's `ProcessRunner` spawns it with a hardcoded `inheritIO()`, and in-process that meant the descriptors
    * of a detached bleep-bsp daemon, which go nowhere: a `println` in a Scala Native test reached neither the report nor any log. Forked, it inherits this
    * child's descriptors, which are pipes drained below into `onOutput`.
    *
    * Counts come from the event stream rather than from the adapter's own tally, so the numbers in the report and the numbers in the result cannot disagree.
    */
  private def runForked(
      binary: Path,
      suites: List[TestSuite],
      eventHandler: TestEventHandler,
      env: Map[String, String],
      scalaNativeVersion: String,
      classpath: List[Path],
      killSignal: Deferred[IO, KillReason]
  ): IO[TestResult] = {
    val counts = new AtomicReference(ProcessTestRunner.RunState(0, 0, 0, 0, None))

    def record(status: String, suite: String): Unit = {
      val _ = counts.updateAndGet { st =>
        status match {
          case "passed"  => st.copy(passed = st.passed + 1, currentSuite = Some(suite))
          case "skipped" => st.copy(skipped = st.skipped + 1, currentSuite = Some(suite))
          case "ignored" => st.copy(ignored = st.ignored + 1, currentSuite = Some(suite))
          // Everything else — failed, error, cancelled, pending — is a test that did not pass. Counting only the statuses named here and silently dropping
          // the rest is how a run reports fewer tests than it ran.
          case _ => st.copy(failed = st.failed + 1, currentSuite = Some(suite))
        }
      }
    }

    /** Read protocol lines off the socket and replay them onto the handler. A plain daemon thread rather than a fiber: every call here is a synchronous `Unit`
      * callback, and the handler on the other side does its own blocking dispatch — running that on a cats-effect compute thread is what starved the pool once
      * already.
      */
    def startProtocolReader(server: ServerSocket): Thread = {
      val body: Runnable = () =>
        try {
          val socket = server.accept()
          try {
            val reader = new BufferedReader(new InputStreamReader(socket.getInputStream, StandardCharsets.UTF_8))
            var line = reader.readLine()
            while (line != null) {
              bleep.testing.TestProtocol.decodeResponse(line) match {
                case Right(bleep.testing.TestProtocol.TestResponse.TestStarted(suite, test)) =>
                  eventHandler.onTestStarted(suite, test)
                case Right(tf: bleep.testing.TestProtocol.TestResponse.TestFinished) =>
                  record(tf.status, tf.suite)
                  eventHandler.onTestFinished(tf.suite, tf.test, statusOf(tf.status), tf.durationMs, tf.message, tf.throwable)
                case Right(bleep.testing.TestProtocol.TestResponse.Error(message, _)) =>
                  eventHandler.onOutput(suites.headOption.map(_.fullyQualifiedName).getOrElse("Tests"), message, OutputChannel.Stderr)
                // SuiteDone carries the fork's own tally, which is deliberately not used — see above. Ready/Log/ThreadDump are not part of this exchange.
                case Right(_)  => ()
                case Left(err) =>
                  eventHandler.onOutput(
                    suites.headOption.map(_.fullyQualifiedName).getOrElse("Tests"),
                    s"unparseable line from the Scala Native test fork: ${err.getMessage}",
                    OutputChannel.Stderr
                  )
              }
              line = reader.readLine()
            }
          } finally socket.close()
        } catch {
          // The socket is closed when the fork exits; that is how this loop ends.
          case _: java.io.IOException => ()
        }
      val t = new Thread(body, "scala-native-test-protocol")
      t.setDaemon(true)
      t.start()
      t
    }

    val acquire = IO.blocking {
      val server = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)
      val requestFile = Files.createTempFile("bleep-scala-native-test", ".properties")
      val props = new Properties()
      props.setProperty(ScalaNativeTestFork.Keys.Port, server.getLocalPort.toString)
      props.setProperty(ScalaNativeTestFork.Keys.ScalaNativeVersion, scalaNativeVersion)
      props.setProperty(ScalaNativeTestFork.Keys.Binary, binary.toAbsolutePath.toString)
      props.setProperty(ScalaNativeTestFork.Keys.Suites, suites.map(_.fullyQualifiedName).mkString(ScalaNativeTestFork.SuiteSeparator.toString))
      props.setProperty(ScalaNativeTestFork.Keys.Classpath, classpath.map(_.toAbsolutePath.toString).mkString(java.io.File.pathSeparator))
      env.foreach { case (k, v) => props.setProperty(ScalaNativeTestFork.Keys.EnvPrefix + k, v) }
      val out = Files.newOutputStream(requestFile)
      try props.store(out, "bleep Scala Native test fork request")
      finally out.close()
      (server, requestFile)
    }

    Resource
      .make(acquire) { case (server, requestFile) =>
        IO.blocking { server.close(); Files.deleteIfExists(requestFile): Unit }.attempt.void
      }
      .use { case (server, requestFile) =>
        // The daemon's own classpath: the fork runs bleep-bsp code, so it needs exactly what bleep-bsp is running with. `java.home` rather than a configured
        // JVM because the fork must be the same runtime as this process, not the build's target JVM.
        val javaBin = Path.of(System.getProperty("java.home"), "bin", "java").toString
        // Same two flags the test-JVM pool passes, for the same reason: without them every Scala Native run carries the JVM's `sun.misc.Unsafe`
        // deprecation notice, triggered by scala-library and actionable by nobody.
        val command = List(
          javaBin,
          "-XX:+IgnoreUnrecognizedVMOptions",
          "--sun-misc-unsafe-memory-access=allow",
          "-cp",
          System.getProperty("java.class.path"),
          "bleep.bsp.ScalaNativeTestFork",
          requestFile.toString
        )
        val pb = new ProcessBuilder(command.asJava)
        env.foreach { case (k, v) => pb.environment().put(k, v) }

        val suiteForOutput = suites.headOption.map(_.fullyQualifiedName).getOrElse("Tests")
        ProcessTestRunner.run(
          ProcessTestRunner.Config(
            processBuilder = pb,
            handleStdoutLine = line => IO.delay(eventHandler.onOutput(suiteForOutput, line, OutputChannel.Stdout)),
            handleStderrLine = line => IO.delay(eventHandler.onOutput(suiteForOutput, line, OutputChannel.Stderr)),
            getRunState = IO.delay(counts.get()),
            eventHandler = eventHandler,
            killSignal = killSignal,
            // The fork spawns the test binary, so killing only the JVM would leave the binary running.
            killDescendants = true,
            preRun = IO.delay(startProtocolReader(server)).void,
            onNormalExit = IO.unit,
            cleanup = IO.unit
          )
        )
      }
  }

  private def statusOf(name: String): TestStatus =
    name match {
      case "passed"    => TestStatus.Passed
      case "skipped"   => TestStatus.Skipped
      case "ignored"   => TestStatus.Ignored
      case "cancelled" => TestStatus.Cancelled
      case _           => TestStatus.Failed
    }

  /** A `scala.scalanative.build.Logger` that forwards the adapter's output into bleep's test output.
    *
    * This used to be `Logger.default`, which writes to the console of whatever JVM it runs in — here a *detached* bleep-bsp daemon whose own streams go
    * nowhere. Everything the adapter said, and everything it relayed from the test binary, went to a file descriptor nobody reads, which is why a Scala Native
    * test's `println` produced no `<system-out>` at all.
    *
    * The com channel is a `ServerSocket` (see `ComRunner`), not the binary's stdout, so the binary's own output is free for the program to use rather than
    * being multiplexed into a protocol.
    */
  private def nativeAdapterLogger(loader: ClassLoader, eventHandler: TestEventHandler, suite: String): AnyRef = {
    val loggerClass = loader.loadClass("scala.scalanative.build.Logger")
    val handler = new java.lang.reflect.InvocationHandler {
      def invoke(proxy: Any, method: java.lang.reflect.Method, rawArgs: Array[AnyRef]): AnyRef = {
        val args = if (rawArgs == null) Array.empty[AnyRef] else rawArgs
        def first: String = if (args.isEmpty || args(0) == null) "" else String.valueOf(args(0))
        method.getName match {
          case "error"                   => eventHandler.onOutput(suite, first, OutputChannel.Stderr); null
          case "warn" | "info" | "debug" => eventHandler.onOutput(suite, first, OutputChannel.Stdout); null
          case "trace"                   =>
            val t = if (args.isEmpty) null else args(0).asInstanceOf[Throwable]
            eventHandler.onOutput(suite, if (t == null) "" else String.valueOf(t), OutputChannel.Stderr)
            null
          case "running"  => eventHandler.onOutput(suite, s"running $first", OutputChannel.Stdout); null
          case "hashCode" => Integer.valueOf(System.identityHashCode(proxy))
          case "equals"   => java.lang.Boolean.valueOf(proxy.asInstanceOf[AnyRef] eq args(0))
          case "toString" => s"bleep scala-native adapter logger for $suite"
          // An unhandled method means the interface changed. A proxy cannot run the trait's own default implementation, so guessing here would silently
          // drop output; failing names the method instead.
          case other => throw new UnsupportedOperationException(s"scala.scalanative.build.Logger.$other is not handled by bleep's proxy")
        }
      }
    }
    java.lang.reflect.Proxy.newProxyInstance(loader, Array(loggerClass), handler)
  }

  private[bsp] def runTestsViaAdapterBlocking(
      binary: Path,
      suites: List[TestSuite],
      @annotation.unused framework: TestFramework,
      eventHandler: TestEventHandler,
      env: Map[String, String],
      scalaNativeVersion: String,
      classpath: List[Path]
  ): TestResult = {
    val instance = CompilerResolver.getScalaNativeTestRunner(scalaNativeVersion)
    val loader = instance.loader

    // Create TestAdapter.Config using builder pattern (Config is an interface in Scala 3)
    val configClass = loader.loadClass("scala.scalanative.testinterface.adapter.TestAdapter$Config")
    // Config.apply() returns a default config
    val configApply = configClass.getMethod("apply")
    var config: AnyRef = configApply.invoke(null)

    // Set binary file
    val withBinaryFile = configClass.getMethod("withBinaryFile", classOf[java.io.File])
    config = withBinaryFile.invoke(config, binary.toFile).asInstanceOf[AnyRef]

    // Set env vars
    val scalaEnvMap = SbtTestingBridge.ScalaColl.toMap(env, loader)
    val mapClass = loader.loadClass("scala.collection.immutable.Map")
    val withEnvVars = configClass.getMethod("withEnvVars", mapClass)
    config = withEnvVars.invoke(config, scalaEnvMap).asInstanceOf[AnyRef]

    // Set logger
    val buildLoggerClass = loader.loadClass("scala.scalanative.build.Logger")
    val withLogger = configClass.getMethod("withLogger", buildLoggerClass)
    // The suite the adapter's output is attributed to. The adapter runs the whole binary, so its diagnostics are not per-suite; naming the first requested
    // suite keeps the output attached to something the report already has, rather than inventing a suite that never ran.
    val outputSuite = suites.headOption.map(_.fullyQualifiedName).getOrElse("Tests")
    config = withLogger.invoke(config, nativeAdapterLogger(loader, eventHandler, outputSuite)).asInstanceOf[AnyRef]

    // Create TestAdapter
    val adapterClass = loader.loadClass("scala.scalanative.testinterface.adapter.TestAdapter")
    val adapterConstructor = adapterClass.getConstructor(configClass)
    val adapter = adapterConstructor.newInstance(config.asInstanceOf[AnyRef])

    try {
      eventHandler.onRunnerEvent(RunnerEvent.Started)

      // Every framework bleep knows about, not just the one `detectFramework` guessed from jar names. That guess only distinguishes munit, ScalaTest and utest,
      // so a Scala Native project using anything else — ScalaCheck, specs2, ZIO Test — was told "No test framework found in native binary" even though its
      // suites had been discovered. The adapter reports which of these the binary actually contains, which is a better answer than any guess.
      val classNames = SbtTestingBridge.knownFrameworkClassNames
      val scalaClassNames = SbtTestingBridge.ScalaColl.toList(List(SbtTestingBridge.ScalaColl.toList(classNames, loader)), loader)

      val loadMethod = adapterClass.getMethod("loadFrameworks", loader.loadClass("scala.collection.immutable.List"))
      val frameworksResult = loadMethod.invoke(adapter, scalaClassNames)

      val foundFramework = SbtTestingBridge.ScalaColl
        .fromList[Any](frameworksResult, loader)
        .flatMap(opt => SbtTestingBridge.ScalaColl.fromOption[sbt.testing.Framework](opt, loader))
        .headOption

      foundFramework match {
        case None =>
          val message = s"No test framework found in native binary. Tried: ${classNames.mkString(", ")}"
          eventHandler.onRunnerEvent(RunnerEvent.Error(message, None))
          TestResult(0, 0, 0, 0, TerminationReason.Error(message))

        case Some(sbtFramework) =>
          val result = SbtTestingBridge.runSuites(sbtFramework, suites, eventHandler, loader, SbtTestingBridge.moduleDetector(classpath))
          eventHandler.onRunnerEvent(RunnerEvent.ProcessExited(0))
          result
      }
    } finally
      // Closing the adapter kills the native process.
      adapterClass.getMethod("close").invoke(adapter): Unit
  }

  // Output Parsers

  private trait OutputParser {
    def parseLine(line: String): Unit
    def parseError(line: String): Unit
    def getCounts: (Int, Int, Int, Int)
    def unfinishedSuite: Option[String]
  }

  private class MUnitOutputParser(handler: TestEventHandler) extends OutputParser {
    private var currentSuite: Option[String] = None
    private var passed = 0
    private var failed = 0
    private var skipped = 0
    private val ignored = 0

    private val suiteStartPattern = """^\s*(\S+):$""".r
    private val testPassedPattern = """^\s*\+\s+(.+?)\s+(\d+(?:\.\d+)?[a-z]+)$""".r
    private val testErrorPattern = """^\s*X\s+(.+?)\s+(\d+(?:\.\d+)?[a-z]+)$""".r
    private val summaryPattern = """^\s*(\d+)\s+tests,\s+(\d+)\s+passed,\s+(\d+)\s+failed""".r

    override def parseLine(line: String): Unit =
      line match {
        case suiteStartPattern(suite) =>
          currentSuite.foreach { s =>
            val counts = (passed, failed, skipped)
            handler.onSuiteFinished(s, counts._1, counts._2, counts._3)
          }
          currentSuite = Some(suite)
          handler.onSuiteStarted(suite)
          passed = 0
          failed = 0
          skipped = 0

        case testPassedPattern(testName, duration) =>
          currentSuite.foreach { suite =>
            handler.onTestStarted(suite, testName)
            val durationMs = parseDuration(duration)
            handler.onTestFinished(suite, testName, TestStatus.Passed, durationMs, None, None)
          }
          passed += 1

        case testErrorPattern(testName, duration) =>
          currentSuite.foreach { suite =>
            handler.onTestStarted(suite, testName)
            val durationMs = parseDuration(duration)
            handler.onTestFinished(suite, testName, TestStatus.Failed, durationMs, None, None)
          }
          failed += 1

        case summaryPattern(_, _, _) =>
          currentSuite.foreach { suite =>
            handler.onSuiteFinished(suite, passed, failed, skipped)
          }
          currentSuite = None

        case _ =>
          currentSuite.foreach { suite =>
            if (line.trim.nonEmpty) {
              handler.onOutput(suite, line, OutputChannel.Stdout)
            }
          }
      }

    override def parseError(line: String): Unit =
      currentSuite.foreach { suite =>
        handler.onOutput(suite, line, OutputChannel.Stderr)
      }

    override def getCounts: (Int, Int, Int, Int) = (passed, failed, skipped, ignored)
    override def unfinishedSuite: Option[String] = currentSuite

    private def parseDuration(s: String): Long = {
      val numPattern = """(\d+(?:\.\d+)?)([a-z]+)""".r
      s match {
        case numPattern(num, unit) =>
          val value = num.toDouble
          unit match {
            case "ms" => value.toLong
            case "s"  => (value * 1000).toLong
            case "m"  => (value * 60000).toLong
            case _    => value.toLong
          }
        case _ => 0L
      }
    }
  }

  private class ScalaTestOutputParser(handler: TestEventHandler) extends OutputParser {
    private var currentSuite: Option[String] = None
    private var passed = 0
    private var failed = 0
    private var skipped = 0
    private var ignored = 0

    private val suiteStartPattern = """^(\S+):$""".r
    private val testPassedPattern = """^-\s+(.+)$""".r
    private val testFailedPattern = """^-\s+(.+)\s+\*\*\*\s+FAILED\s+\*\*\*$""".r
    private val testIgnoredPattern = """^-\s+(.+)\s+!!! IGNORED !!!$""".r

    override def parseLine(line: String): Unit =
      line match {
        case suiteStartPattern(suite) if !line.contains("-") =>
          currentSuite.foreach { s =>
            handler.onSuiteFinished(s, passed, failed, skipped)
          }
          currentSuite = Some(suite)
          handler.onSuiteStarted(suite)
          passed = 0
          failed = 0
          skipped = 0

        case testFailedPattern(testName) =>
          currentSuite.foreach { suite =>
            handler.onTestStarted(suite, testName)
            handler.onTestFinished(suite, testName, TestStatus.Failed, 0, None, None)
          }
          failed += 1

        case testIgnoredPattern(testName) =>
          currentSuite.foreach { suite =>
            handler.onTestStarted(suite, testName)
            handler.onTestFinished(suite, testName, TestStatus.Ignored, 0, None, None)
          }
          ignored += 1

        case testPassedPattern(testName) =>
          currentSuite.foreach { suite =>
            handler.onTestStarted(suite, testName)
            handler.onTestFinished(suite, testName, TestStatus.Passed, 0, None, None)
          }
          passed += 1

        case _ =>
          currentSuite.foreach { suite =>
            if (line.trim.nonEmpty) {
              handler.onOutput(suite, line, OutputChannel.Stdout)
            }
          }
      }

    override def parseError(line: String): Unit =
      currentSuite.foreach { suite =>
        handler.onOutput(suite, line, OutputChannel.Stderr)
      }

    override def getCounts: (Int, Int, Int, Int) = (passed, failed, skipped, ignored)
    override def unfinishedSuite: Option[String] = currentSuite
  }

  private class UTestOutputParser(handler: TestEventHandler) extends OutputParser {
    private var currentSuite: Option[String] = None
    private var passed = 0
    private var failed = 0
    private var skipped = 0
    private val ignored = 0

    private val testPassedPattern = """^\+\s+(.+?)\s+(\d+)ms$""".r
    private val testFailedPattern = """^X\s+(.+?)\s+(\d+)ms$""".r

    override def parseLine(line: String): Unit =
      line match {
        case testPassedPattern(testPath, duration) =>
          val parts = testPath.split("\\.")
          val suite = parts.init.mkString(".")
          val test = parts.lastOption.getOrElse(testPath)

          if (!currentSuite.contains(suite)) {
            currentSuite.foreach { s =>
              handler.onSuiteFinished(s, passed, failed, skipped)
            }
            currentSuite = Some(suite)
            handler.onSuiteStarted(suite)
            passed = 0
            failed = 0
            skipped = 0
          }

          handler.onTestStarted(suite, test)
          handler.onTestFinished(suite, test, TestStatus.Passed, duration.toLong, None, None)
          passed += 1

        case testFailedPattern(testPath, duration) =>
          val parts = testPath.split("\\.")
          val suite = parts.init.mkString(".")
          val test = parts.lastOption.getOrElse(testPath)

          if (!currentSuite.contains(suite)) {
            currentSuite.foreach { s =>
              handler.onSuiteFinished(s, passed, failed, skipped)
            }
            currentSuite = Some(suite)
            handler.onSuiteStarted(suite)
            passed = 0
            failed = 0
            skipped = 0
          }

          handler.onTestStarted(suite, test)
          handler.onTestFinished(suite, test, TestStatus.Failed, duration.toLong, None, None)
          failed += 1

        case _ =>
          currentSuite.foreach { suite =>
            if (line.trim.nonEmpty) {
              handler.onOutput(suite, line, OutputChannel.Stdout)
            }
          }
      }

    override def parseError(line: String): Unit =
      currentSuite.foreach { suite =>
        handler.onOutput(suite, line, OutputChannel.Stderr)
      }

    override def getCounts: (Int, Int, Int, Int) = (passed, failed, skipped, ignored)
    override def unfinishedSuite: Option[String] = currentSuite
  }

  private class GenericOutputParser(handler: TestEventHandler) extends OutputParser {
    private var passed = 0
    private var failed = 0
    private val defaultSuite = "Tests"

    private val passPattern = """(?i)(pass|ok|success|\+)""".r
    private val failPattern = """(?i)(fail|error|x)""".r

    override def parseLine(line: String): Unit = {
      if (passPattern.findFirstIn(line).isDefined) {
        passed += 1
      } else if (failPattern.findFirstIn(line).isDefined) {
        failed += 1
      }
      handler.onOutput(defaultSuite, line, OutputChannel.Stdout)
    }

    override def parseError(line: String): Unit =
      handler.onOutput(defaultSuite, line, OutputChannel.Stderr)

    override def getCounts: (Int, Int, Int, Int) = (passed, failed, 0, 0)
    override def unfinishedSuite: Option[String] = None
  }
}
