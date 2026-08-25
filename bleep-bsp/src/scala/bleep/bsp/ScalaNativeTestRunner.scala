package bleep.bsp

import bleep.analysis._
import bleep.bsp.protocol.KillReason
import bleep.bsp.TaskDag.LinkResult
import bleep.bsp.TestRunnerTypes.{RunnerEvent, TerminationReason, TestEventHandler, TestResult, TestSuite}
import bleep.bsp.protocol.{OutputChannel, TestStatus}
import cats.effect.{Deferred, IO}
import cats.effect.std.Semaphore
import java.nio.file.{Files, Path}
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
      framework: TestFramework,
      eventHandler: TestEventHandler,
      env: Map[String, String],
      scalaNativeVersion: String,
      classpath: List[Path],
      killSignal: Deferred[IO, KillReason]
  ): IO[TestResult] =
    killSignal.tryGet.flatMap {
      case Some(reason) => IO.pure(TestResult(0, 0, 0, 0, TerminationReason.Killed(reason)))
      case None         =>
        val work = IO.interruptible {
          runTestsViaAdapterBlocking(binary, suites, framework, eventHandler, env, scalaNativeVersion, classpath)
        }

        Outcome
          .raceKill(killSignal)(work)
          .flatMap {
            case Left(result)  => IO.pure(result)
            case Right(reason) =>
              IO.pure(TestResult(0, 0, 0, 0, TerminationReason.Killed(reason)))
          }
          .handleErrorWith { e =>
            // `getMessage` is null for plenty of exceptions, and a null here reaches the user as an empty failure reason. The class name always says something.
            val message = Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.getClass.getName)
            IO.pure(TestResult(0, 1, 0, 0, TerminationReason.Error(message)))
          }
    }

  private def runTestsViaAdapterBlocking(
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
    val buildLoggerCompanion = loader.loadClass("scala.scalanative.build.Logger$")
    val buildLoggerObj = buildLoggerCompanion.getField("MODULE$").get(null)
    val defaultLogger = buildLoggerCompanion.getMethod("default").invoke(buildLoggerObj)
    val withLogger = configClass.getMethod("withLogger", buildLoggerClass)
    config = withLogger.invoke(config, defaultLogger).asInstanceOf[AnyRef]

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
            handler.onTestFinished(suite, testName, TestStatus.Passed, durationMs, None)
          }
          passed += 1

        case testErrorPattern(testName, duration) =>
          currentSuite.foreach { suite =>
            handler.onTestStarted(suite, testName)
            val durationMs = parseDuration(duration)
            handler.onTestFinished(suite, testName, TestStatus.Failed, durationMs, None)
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
            handler.onTestFinished(suite, testName, TestStatus.Failed, 0, None)
          }
          failed += 1

        case testIgnoredPattern(testName) =>
          currentSuite.foreach { suite =>
            handler.onTestStarted(suite, testName)
            handler.onTestFinished(suite, testName, TestStatus.Ignored, 0, None)
          }
          ignored += 1

        case testPassedPattern(testName) =>
          currentSuite.foreach { suite =>
            handler.onTestStarted(suite, testName)
            handler.onTestFinished(suite, testName, TestStatus.Passed, 0, None)
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
          handler.onTestFinished(suite, test, TestStatus.Passed, duration.toLong, None)
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
          handler.onTestFinished(suite, test, TestStatus.Failed, duration.toLong, None)
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
