package bleep.bsp

import bleep.analysis._
import bleep.bsp.Outcome.KillReason
import bleep.bsp.TaskDag.LinkResult
import bleep.bsp.TestRunnerTypes.{RunnerEvent, TerminationReason, TestEventHandler, TestResult, TestSuite}
import bleep.bsp.protocol.{OutputChannel, TestStatus}
import cats.effect.{Deferred, IO}
import cats.effect.std.Semaphore
import cats.syntax.all._
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

  /** Bridge a Deferred kill signal to CancellationToken for toolchains that still use CancellationToken. */
  private def bridgeKillSignal(killSignal: Deferred[IO, KillReason]): IO[CancellationToken] = {
    import java.util.concurrent.atomic.AtomicBoolean
    import scala.collection.mutable.ListBuffer

    IO.delay {
      val cancelled = new AtomicBoolean(false)
      val callbacks = ListBuffer[() => Unit]()

      new CancellationToken {
        def isCancelled: Boolean = cancelled.get()
        def cancel(): Unit =
          if (cancelled.compareAndSet(false, true)) {
            callbacks.synchronized {
              callbacks.foreach(cb => cb())
            }
          }
        def onCancel(callback: () => Unit): Unit =
          callbacks.synchronized {
            if (cancelled.get()) callback()
            else { callbacks += callback: Unit }
          }
      }
    }.flatTap { token =>
      // Start a fiber that watches the kill signal and triggers the token when killed
      killSignal.get.flatMap(_ => IO.delay(token.cancel())).start.void
    }
  }

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
    bridgeKillSignal(killSignal).flatMap { cancellation =>
      IO.blocking {
        Files.createDirectories(workDir)
        Files.createDirectories(outputPath.getParent)
      } >> {
        val work = toolchain
          .link(
            config,
            classpath,
            testMainClass,
            outputPath,
            workDir,
            logger,
            cancellation
          )
          .map { result =>
            if (result.isSuccess) LinkResult.NativeSuccess(result.binary, wasUpToDate = false)
            else LinkResult.Failure(s"Linking failed with exit code ${result.exitCode}", List.empty)
          }

        Outcome
          .raceKill(killSignal)(work)
          .map {
            case Left(result) => result
            case Right(_)     => LinkResult.Cancelled
          }
          .handleErrorWith { ex =>
            IO.pure(LinkResult.Failure(ex.getMessage, List.empty))
          }
      }
    }

  /** Discover test suites from a linked native binary. */
  def discoverSuites(
      binary: Path,
      classpath: Seq[Path],
      killSignal: Deferred[IO, KillReason]
  ): IO[ProcessRunner.DiscoveryResult[List[TestSuite]]] =
    killSignal.tryGet.flatMap {
      case Some(reason) => IO.pure(ProcessRunner.DiscoveryResult.Killed(reason))
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

  private def discoverFromClasspath(classpath: Seq[Path]): List[TestSuite] =
    classpath.flatMap { path =>
      if (Files.isDirectory(path)) {
        findTestClasses(path)
      } else {
        List.empty
      }
    }.toList

  private def findTestClasses(dir: Path): List[TestSuite] = {
    import scala.jdk.StreamConverters._
    import scala.util.Using
    Using(Files.walk(dir)) { stream =>
      stream
        .toScala(List)
        .filter(p => p.toString.endsWith(".class") || p.toString.endsWith(".nir"))
        .filter { p =>
          val name = p.getFileName.toString
          name.contains("Test") || name.contains("Suite") || name.contains("Spec")
        }
        .map { p =>
          val relative = dir.relativize(p).toString
          val fqn = relative
            .replace("/", ".")
            .replace("\\", ".")
            .replaceAll("\\.(class|nir)$", "")
          val simpleName = fqn.split('.').lastOption.getOrElse(fqn)
          TestSuite(simpleName, fqn)
        }
    }.get // Fail loudly if directory walk fails (permission error, etc.)
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
      case None =>
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

            val work = ProcessRunner.start(pb).use { process =>
              TestRunnerTypes.startKillWatcher(process, killSignal, killDescendants = true).flatMap { killFiber =>
                IO.delay(eventHandler.onRunnerEvent(RunnerEvent.Started)) >> {
                  val stdout = ProcessRunner.lines(process.getInputStream).evalMap { line =>
                    parserLock.permit.surround(IO.delay(parser.parseLine(line)))
                  }
                  val stderr = ProcessRunner.lines(process.getErrorStream).evalMap { line =>
                    parserLock.permit.surround(IO.delay(parser.parseError(line)))
                  }

                  (stdout.compile.drain, stderr.compile.drain).parTupled.void >>
                    IO.blocking(process.waitFor()).flatMap { exitCode =>
                      // Check if process was killed by our kill watcher
                      killSignal.tryGet.map {
                        case Some(reason) =>
                          eventHandler.onRunnerEvent(RunnerEvent.Killed(reason))
                          val counts = parser.getCounts
                          TestResult(counts._1, counts._2, counts._3, counts._4, TerminationReason.Killed(reason))
                        case None =>
                          val counts = parser.getCounts
                          val (passed, failed, skipped, ignored) = counts
                          val exitResult = TestRunnerTypes.interpretExitCode(exitCode, parser.unfinishedSuite, passed, failed, eventHandler)
                          TestResult(passed, exitResult.adjustedFailed, skipped, ignored, exitResult.terminationReason)
                      }
                    }
                }.guarantee(killFiber.cancel)
              }
            }

            Outcome.raceKill(killSignal)(work).flatMap {
              case Left(result) => IO.pure(result)
              case Right(reason) =>
                IO.delay {
                  eventHandler.onRunnerEvent(RunnerEvent.Killed(reason))
                  val counts = parser.getCounts
                  TestResult(counts._1, counts._2, counts._3, counts._4, TerminationReason.Killed(reason))
                }
            }
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
  def getTestMainClass(framework: TestFramework): String = TestMainClass

  /** Framework class names for sbt.testing.Framework SPI discovery. */
  private val frameworkClassNames: Map[TestFramework, List[String]] = Map(
    TestFramework.MUnit -> List("munit.Framework"),
    TestFramework.ScalaTest -> List("org.scalatest.tools.Framework", "org.scalatest.tools.ScalaTestFramework"),
    TestFramework.UTest -> List("utest.runner.Framework"),
    TestFramework.Unknown -> List("munit.Framework", "org.scalatest.tools.Framework", "utest.runner.Framework")
  )

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
      workingDir: Path,
      scalaNativeVersion: String,
      killSignal: Deferred[IO, KillReason]
  ): IO[TestResult] =
    killSignal.tryGet.flatMap {
      case Some(reason) => IO.pure(TestResult(0, 0, 0, 0, TerminationReason.Killed(reason)))
      case None =>
        val work = IO.interruptible {
          runTestsViaAdapterBlocking(binary, suites, framework, eventHandler, env, workingDir, scalaNativeVersion)
        }

        Outcome
          .raceKill(killSignal)(work)
          .flatMap {
            case Left(result) => IO.pure(result)
            case Right(reason) =>
              IO.pure(TestResult(0, 0, 0, 0, TerminationReason.Killed(reason)))
          }
          .handleErrorWith { e =>
            IO.pure(TestResult(0, 1, 0, 0, TerminationReason.Error(e.getMessage)))
          }
    }

  private def runTestsViaAdapterBlocking(
      binary: Path,
      suites: List[TestSuite],
      framework: TestFramework,
      eventHandler: TestEventHandler,
      env: Map[String, String],
      workingDir: Path,
      scalaNativeVersion: String
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
    val scalaEnvMap = toScalaMap(env, loader)
    val mapClass = loader.loadClass("scala.collection.immutable.Map")
    val withEnvVars = configClass.getMethod("withEnvVars", mapClass)
    config = withEnvVars.invoke(config, scalaEnvMap.asInstanceOf[AnyRef]).asInstanceOf[AnyRef]

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

      // loadFrameworks takes List[List[String]] - list of framework class name alternatives
      val classNames = frameworkClassNames.getOrElse(framework, frameworkClassNames(TestFramework.Unknown))
      val scalaClassNames = toScalaList(List(toScalaList(classNames, loader)), loader)

      val loadMethod = adapterClass.getMethod("loadFrameworks", loader.loadClass("scala.collection.immutable.List"))
      val frameworksResult = loadMethod.invoke(adapter, scalaClassNames.asInstanceOf[AnyRef])

      // Result is List[Option[sbt.testing.Framework]] - convert to Java
      val frameworksList = fromScalaList[Any](frameworksResult, loader)
      val foundFramework = frameworksList.flatMap(opt => fromScalaOption[sbt.testing.Framework](opt, loader)).headOption

      foundFramework match {
        case None =>
          eventHandler.onRunnerEvent(RunnerEvent.Error("No test framework found in native binary", None))
          TestResult(0, 0, 0, 0, TerminationReason.Error("No test framework found"))

        case Some(sbtFramework) =>
          // Create runner
          val runner = sbtFramework.runner(Array.empty[String], Array.empty[String], loader)

          // Create TaskDefs from suites
          // TaskDef(fullyQualifiedName, fingerprint, explicitlySpecified, selectors)
          val fingerprint: sbt.testing.Fingerprint = new sbt.testing.SubclassFingerprint {
            def superclassName(): String = "java.lang.Object"
            def isModule(): Boolean = false
            def requireNoArgConstructor(): Boolean = true
          }
          val taskDefs = suites.map { suite =>
            new sbt.testing.TaskDef(
              suite.fullyQualifiedName,
              fingerprint,
              false,
              Array(new sbt.testing.SuiteSelector)
            )
          }.toArray

          // If no specific suites, discover all
          val tasks = if (taskDefs.isEmpty) {
            // Get all tasks by passing empty array - framework discovers
            runner.tasks(Array.empty)
          } else {
            runner.tasks(taskDefs)
          }

          // Per-suite counters (suite name → (passed, failed, skipped, ignored))
          val suiteCounts = new scala.collection.mutable.HashMap[String, (Int, Int, Int, Int)]()

          // Execute each task
          val sbtEventHandler = new sbt.testing.EventHandler {
            def handle(event: sbt.testing.Event): Unit = {
              val suiteName = event.fullyQualifiedName()
              val testName = event.selector() match {
                case ts: sbt.testing.TestSelector       => ts.testName()
                case ns: sbt.testing.NestedTestSelector => ns.testName()
                case _                                  => event.fullyQualifiedName()
              }

              eventHandler.onTestStarted(suiteName, testName)

              val status = event.status() match {
                case sbt.testing.Status.Success =>
                  val (p, f, s, i) = suiteCounts.getOrElse(suiteName, (0, 0, 0, 0))
                  suiteCounts(suiteName) = (p + 1, f, s, i)
                  TestStatus.Passed
                case sbt.testing.Status.Failure =>
                  val (p, f, s, i) = suiteCounts.getOrElse(suiteName, (0, 0, 0, 0))
                  suiteCounts(suiteName) = (p, f + 1, s, i)
                  TestStatus.Failed
                case sbt.testing.Status.Error =>
                  val (p, f, s, i) = suiteCounts.getOrElse(suiteName, (0, 0, 0, 0))
                  suiteCounts(suiteName) = (p, f + 1, s, i)
                  TestStatus.Error
                case sbt.testing.Status.Skipped =>
                  val (p, f, s, i) = suiteCounts.getOrElse(suiteName, (0, 0, 0, 0))
                  suiteCounts(suiteName) = (p, f, s + 1, i)
                  TestStatus.Skipped
                case sbt.testing.Status.Ignored =>
                  val (p, f, s, i) = suiteCounts.getOrElse(suiteName, (0, 0, 0, 0))
                  suiteCounts(suiteName) = (p, f, s, i + 1)
                  TestStatus.Ignored
                case sbt.testing.Status.Canceled =>
                  val (p, f, s, i) = suiteCounts.getOrElse(suiteName, (0, 0, 0, 0))
                  suiteCounts(suiteName) = (p, f, s + 1, i)
                  TestStatus.Cancelled
                case sbt.testing.Status.Pending =>
                  val (p, f, s, i) = suiteCounts.getOrElse(suiteName, (0, 0, 0, 0))
                  suiteCounts(suiteName) = (p, f, s + 1, i)
                  TestStatus.Pending
              }

              val durationMs = event.duration()
              val message = Option(event.throwable()).flatMap { opt =>
                if (opt.isDefined) Some(opt.get().getMessage)
                else None
              }

              eventHandler.onTestFinished(suiteName, testName, status, durationMs, message)
            }
          }

          val sbtLoggers = Array[sbt.testing.Logger](new sbt.testing.Logger {
            def ansiCodesSupported(): Boolean = false
            def error(msg: String): Unit = eventHandler.onOutput("", msg, OutputChannel.Stderr)
            def warn(msg: String): Unit = eventHandler.onOutput("", msg, OutputChannel.Stdout)
            def info(msg: String): Unit = eventHandler.onOutput("", msg, OutputChannel.Stdout)
            def debug(msg: String): Unit = ()
            def trace(t: Throwable): Unit = eventHandler.onOutput("", t.toString, OutputChannel.Stderr)
          })

          // Track suites
          val suiteNames = scala.collection.mutable.Set[String]()

          tasks.foreach { task =>
            val suiteName = task.taskDef().fullyQualifiedName()
            if (suiteNames.add(suiteName)) {
              eventHandler.onSuiteStarted(suiteName)
            }

            val nestedTasks = task.execute(sbtEventHandler, sbtLoggers)
            // Execute nested tasks recursively
            def executeNested(nested: Array[sbt.testing.Task]): Unit =
              nested.foreach { nt =>
                val ntSuite = nt.taskDef().fullyQualifiedName()
                if (suiteNames.add(ntSuite)) {
                  eventHandler.onSuiteStarted(ntSuite)
                }
                val moreNested = nt.execute(sbtEventHandler, sbtLoggers)
                executeNested(moreNested)
              }
            executeNested(nestedTasks)
          }

          // Finish all tracked suites with per-suite counts
          suiteNames.foreach { name =>
            val (p, f, s, _) = suiteCounts.getOrElse(name, (0, 0, 0, 0))
            eventHandler.onSuiteFinished(name, p, f, s)
          }

          // runner.done() signals the native process to shut down.
          // It may throw if the native process's RPC handler doesn't support the done opcode,
          // but by this point all tests have already executed and results are captured.
          try runner.done()
          catch { case _: Exception => () }
          eventHandler.onRunnerEvent(RunnerEvent.ProcessExited(0))
          val totalPassed = suiteCounts.values.map(_._1).sum
          val totalFailed = suiteCounts.values.map(_._2).sum
          val totalSkipped = suiteCounts.values.map(_._3).sum
          val totalIgnored = suiteCounts.values.map(_._4).sum
          TestResult(totalPassed, totalFailed, totalSkipped, totalIgnored, TerminationReason.Completed)
      }
    } finally
      // Close adapter (kills native process)
      try {
        val closeMethod = adapterClass.getMethod("close")
        closeMethod.invoke(adapter): Unit
      } catch { case _: Exception => () }
  }

  private def toScalaMap(javaMap: Map[String, String], loader: ClassLoader): Any = {
    val mapCompanion = loader.loadClass("scala.collection.immutable.Map$")
    val mapObj = mapCompanion.getField("MODULE$").get(null)
    val emptyMethod = mapCompanion.getMethod("empty")
    var result = emptyMethod.invoke(mapObj)
    val updatedMethod = result.getClass.getMethod("updated", classOf[Object], classOf[Object])
    javaMap.foreach { case (k, v) =>
      result = updatedMethod.invoke(result, k, v)
    }
    result
  }

  private def toScalaList(javaList: List[Any], loader: ClassLoader): Any = {
    val listCompanion = loader.loadClass("scala.collection.immutable.List$")
    val listObj = listCompanion.getField("MODULE$").get(null)
    val nilClass = loader.loadClass("scala.collection.immutable.Nil$")
    val nilObj = nilClass.getField("MODULE$").get(null)
    val consClass = loader.loadClass("scala.collection.immutable.$colon$colon")
    val consConstructor = consClass.getConstructor(classOf[Object], loader.loadClass("scala.collection.immutable.List"))
    javaList.foldRight(nilObj: Any) { (elem, acc) =>
      consConstructor.newInstance(elem.asInstanceOf[AnyRef], acc.asInstanceOf[AnyRef])
    }
  }

  private def fromScalaList[A](scalaList: Any, loader: ClassLoader): List[A] = {
    val result = scala.collection.mutable.ListBuffer[A]()
    var current = scalaList
    val nilClass = loader.loadClass("scala.collection.immutable.Nil$")
    val nilObj = nilClass.getField("MODULE$").get(null)
    while (current != nilObj) {
      val headMethod = current.getClass.getMethod("head")
      val tailMethod = current.getClass.getMethod("tail")
      result += headMethod.invoke(current).asInstanceOf[A]
      current = tailMethod.invoke(current)
    }
    result.toList
  }

  private def fromScalaOption[A](scalaOption: Any, loader: ClassLoader): Option[A] = {
    val noneClass = loader.loadClass("scala.None$")
    val noneObj = noneClass.getField("MODULE$").get(null)
    if (scalaOption == noneObj) None
    else {
      val getMethod = scalaOption.getClass.getMethod("get")
      Some(getMethod.invoke(scalaOption).asInstanceOf[A])
    }
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
