package bleep.analysis

import bleep.bsp.*
import bleep.bsp.protocol.{KillReason, OutputChannel, TestStatus}
import bleep.model.{CrossProjectName, ProjectName}
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import org.scalatest.concurrent.TimeLimits
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}

import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.concurrent.duration.*

/** Edge case integration tests for all platforms.
  *
  * Tests error handling, reporting, and recovery for:
  *   - Exceptions thrown during test execution
  *   - System.exit() calls
  *   - Infinite loops and timeouts
  *   - Invalid output formats
  *   - Missing binaries/files
  *   - Corrupted output
  *
  * All tests have explicit timeouts to prevent hanging the test suite.
  */
class EdgeCaseIntegrationTest extends AnyFunSuite with Matchers with TimeLimits {

  // Timeouts for different test categories. Each limit guards against a genuine hang rather than measuring speed. Most tests here start `node` or a native
  // binary, and process startup on a loaded CI runner is not instant, so each limit sits far under the suite-level idle timeout rather than tight against what
  // a developer machine does.
  val quickTimeout = Span(30, Seconds) // Tests that should complete without hanging
  val mediumTimeout = Span(10, Seconds) // Tests with moderate work
  val cancellationTimeout = Span(5, Seconds) // Tests that rely on cancellation (should be fast)

  def createTempDir(prefix: String): Path =
    Files.createTempDirectory(prefix)

  private def isUnixLike: Boolean =
    System.getProperty("os.name").toLowerCase.contains("linux") ||
      System.getProperty("os.name").toLowerCase.contains("mac")

  // ==========================================================================
  // Scala Native Edge Cases
  // ==========================================================================

  test("Scala Native: handles missing binary") {
    failAfter(quickTimeout) {
      val handler = new RecordingNativeTestEventHandler()
      val nonExistentPath = Path.of("/non/existent/binary")

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        res <- ScalaNativeTestRunner.runTests(
          nonExistentPath,
          List.empty,
          TestRunnerTypes.TestFramework.Unknown,
          handler,
          Map.empty,
          Path.of("."),
          killSignal
        )
      } yield res).attempt.unsafeRunSync()

      result.isLeft || result.exists(r => !r.isSuccess) shouldBe true
    }
  }

  test("Scala Native: handles non-executable file") {
    failAfter(quickTimeout) {
      val tempDir = createTempDir("native-nonexec-test")
      try {
        val binary = tempDir.resolve("not-executable")
        Files.writeString(binary, "not a real binary")
        // Don't set executable flag

        val handler = new RecordingNativeTestEventHandler()
        val result = (for {
          killSignal <- Outcome.neverKillSignal
          res <- ScalaNativeTestRunner.runTests(
            binary,
            List.empty,
            TestRunnerTypes.TestFramework.Unknown,
            handler,
            Map.empty,
            tempDir,
            killSignal
          )
        } yield res).attempt.unsafeRunSync()

        // The contract is that the problem is reported rather than swallowed into a pass — same formulation as the missing-binary test above. Unix reaches
        // that via Right(unsuccessful): the runner sets the exec bit, runs the file, and it fails. Windows has no exec bit, so the file is executable by
        // permission but is not a valid executable image, CreateProcess rejects it, and the failure arrives as Left. Both report; neither passes.
        result.isLeft || result.exists(r => !r.isSuccess) shouldBe true
      } finally deleteRecursively(tempDir)
    }
  }

  test("Scala Native: handles binary that crashes (SEGV)") {
    failAfter(quickTimeout) {
      assume(isUnixLike, "Unix-like OS required")

      val tempDir = createTempDir("native-crash-test")
      try {
        val binary = tempDir.resolve("crash-binary")
        Files.writeString(
          binary,
          """#!/bin/bash
          |echo "Starting..."
          |kill -SEGV $$
          |""".stripMargin
        )
        binary.toFile.setExecutable(true)

        val handler = new RecordingNativeTestEventHandler()
        val result = (for {
          killSignal <- Outcome.neverKillSignal
          res <- ScalaNativeTestRunner.runTests(
            binary,
            List.empty,
            TestRunnerTypes.TestFramework.Unknown,
            handler,
            Map.empty,
            tempDir,
            killSignal
          )
        } yield res).unsafeRunSync()

        result.isSuccess shouldBe false
      } finally deleteRecursively(tempDir)
    }
  }

  test("Scala Native: handles various exit codes") {
    failAfter(mediumTimeout) {
      assume(isUnixLike, "Unix-like OS required")

      val tempDir = createTempDir("native-exitcode-test")
      try
        for (exitCode <- Seq(0, 1, 42, 127)) {
          val binary = tempDir.resolve(s"exit-$exitCode")
          Files.writeString(
            binary,
            s"""#!/bin/bash
            |echo "MySuite:"
            |echo "+ test1 10ms"
            |exit $exitCode
            |""".stripMargin
          )
          binary.toFile.setExecutable(true)

          val handler = new RecordingNativeTestEventHandler()
          val result = (for {
            killSignal <- Outcome.neverKillSignal
            res <- ScalaNativeTestRunner.runTests(
              binary,
              List.empty,
              TestRunnerTypes.TestFramework.MUnit,
              handler,
              Map.empty,
              tempDir,
              killSignal
            )
          } yield res).unsafeRunSync()

          result.terminationReason should not be a[TestRunnerTypes.TerminationReason.Killed]
        }
      finally
        deleteRecursively(tempDir)
    }
  }

  test("Scala Native: handles infinite loop with cancellation") {
    failAfter(cancellationTimeout) {
      assume(isUnixLike, "Unix-like OS required")

      val tempDir = createTempDir("native-infinite-test")
      try {
        val binary = tempDir.resolve("infinite-binary")
        Files.writeString(
          binary,
          """#!/bin/bash
          |while true; do sleep 0.01; done
          |""".stripMargin
        )
        binary.toFile.setExecutable(true)

        val handler = new RecordingNativeTestEventHandler()

        val result = (for {
          killSignal <- Deferred[IO, KillReason]
          _ <- (IO.sleep(100.milliseconds) >> killSignal.complete(KillReason.UserRequest)).start
          r <- ScalaNativeTestRunner.runTests(
            binary,
            List.empty,
            TestRunnerTypes.TestFramework.Unknown,
            handler,
            Map.empty,
            tempDir,
            killSignal
          )
        } yield r).unsafeRunSync()

        result.terminationReason shouldBe a[TestRunnerTypes.TerminationReason.Killed]
      } finally deleteRecursively(tempDir)
    }
  }

  test("Scala Native: handles MUnit output format") {
    failAfter(quickTimeout) {
      assume(isUnixLike, "Unix-like OS required")

      val tempDir = createTempDir("native-munit-test")
      try {
        val binary = tempDir.resolve("munit-binary")
        Files.writeString(
          binary,
          """#!/bin/bash
          |echo "MySuite:"
          |echo "+ passingTest 5ms"
          |echo "X failingTest 10ms"
          |echo "2 tests, 1 passed, 1 failed"
          |exit 1
          |""".stripMargin
        )
        binary.toFile.setExecutable(true)

        val handler = new RecordingNativeTestEventHandler()
        val result = (for {
          killSignal <- Outcome.neverKillSignal
          res <- ScalaNativeTestRunner.runTests(
            binary,
            List.empty,
            TestRunnerTypes.TestFramework.MUnit,
            handler,
            Map.empty,
            tempDir,
            killSignal
          )
        } yield res).unsafeRunSync()

        result.isSuccess shouldBe false
        result.terminationReason should not be a[TestRunnerTypes.TerminationReason.Killed]
      } finally deleteRecursively(tempDir)
    }
  }

  // ==========================================================================
  // Kotlin/JS Edge Cases
  // ==========================================================================

  test("Kotlin/JS: handles missing JS file") {
    failAfter(quickTimeout) {
      val handler = new RecordingKotlinTestEventHandler()
      val nonExistentPath = Path.of("/non/existent/kotlin.js")

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        res <- KotlinTestRunner.Js.runTests(
          nonExistentPath,
          List.empty,
          handler,
          PlatformTestHelper.nodeBinary,
          Map.empty,
          killSignal
        )
      } yield res).attempt.unsafeRunSync()

      result.isLeft || result.exists(r => !r.isSuccess) shouldBe true
    }
  }

  test("Kotlin/JS: handles exception in test code") {
    failAfter(quickTimeout) {

      val tempDir = createTempDir("kotlinjs-exception-test")
      try {
        val jsFile = tempDir.resolve("kotlin-exception.js")
        Files.writeString(
          jsFile,
          """
          |console.log('##kotlin-test##suite-started|ExceptionSuite');
          |throw new Error('Kotlin test exception!');
          |""".stripMargin
        )

        val handler = new RecordingKotlinTestEventHandler()
        val result = (for {
          killSignal <- Outcome.neverKillSignal
          res <- KotlinTestRunner.Js.runTests(
            jsFile,
            List(TestRunnerTypes.TestSuite("ExceptionSuite", "ExceptionSuite")),
            handler,
            PlatformTestHelper.nodeBinary,
            Map.empty,
            killSignal
          )
        } yield res).unsafeRunSync()

        result.terminationReason should not be a[TestRunnerTypes.TerminationReason.Killed]
      } finally deleteRecursively(tempDir)
    }
  }

  test("Kotlin/JS: handles slow infinite output with cancellation") {
    failAfter(cancellationTimeout) {

      val tempDir = createTempDir("kotlinjs-infinite-test")
      try {
        val jsFile = tempDir.resolve("kotlin-infinite.js")
        // Top-level infinite loop that blocks require() forever,
        // producing output periodically. The kill signal must destroy the process.
        Files.writeString(
          jsFile,
          """
          |let tick = 0;
          |while(true) {
          |  const start = Date.now();
          |  while(Date.now() - start < 20) {}
          |  console.log('tick ' + tick++);
          |}
          |""".stripMargin
        )

        val handler = new RecordingKotlinTestEventHandler()

        val result = (for {
          killSignal <- Deferred[IO, KillReason]
          _ <- (IO.sleep(200.milliseconds) >> killSignal.complete(KillReason.UserRequest)).start
          r <- KotlinTestRunner.Js.runTests(
            jsFile,
            List(TestRunnerTypes.TestSuite("InfiniteSuite", "InfiniteSuite")),
            handler,
            PlatformTestHelper.nodeBinary,
            Map.empty,
            killSignal
          )
        } yield r).unsafeRunSync()

        result.terminationReason shouldBe a[TestRunnerTypes.TerminationReason.Killed]
      } finally deleteRecursively(tempDir)
    }
  }

  test("Kotlin/JS: handles blocking infinite loop with cancellation") {
    failAfter(cancellationTimeout) {

      val tempDir = createTempDir("kotlinjs-blocking-test")
      try {
        val jsFile = tempDir.resolve("kotlin-blocking.js")
        // Top-level synchronous infinite loop that blocks require() forever.
        // The kill signal must destroy the process.
        Files.writeString(
          jsFile,
          """
          |while(true) {}
          |""".stripMargin
        )

        val handler = new RecordingKotlinTestEventHandler()

        val result = (for {
          killSignal <- Deferred[IO, KillReason]
          _ <- (IO.sleep(200.milliseconds) >> killSignal.complete(KillReason.UserRequest)).start
          r <- KotlinTestRunner.Js.runTests(
            jsFile,
            List(TestRunnerTypes.TestSuite("BlockingSuite", "BlockingSuite")),
            handler,
            PlatformTestHelper.nodeBinary,
            Map.empty,
            killSignal
          )
        } yield r).unsafeRunSync()

        result.terminationReason shouldBe a[TestRunnerTypes.TerminationReason.Killed]
      } finally deleteRecursively(tempDir)
    }
  }

  // ==========================================================================
  // Kotlin/Native Edge Cases
  // ==========================================================================

  test("Kotlin/Native: handles missing binary") {
    failAfter(quickTimeout) {
      val handler = new RecordingKotlinTestEventHandler()
      val nonExistentPath = Path.of("/non/existent/kotlin-native")

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        res <- KotlinTestRunner.Native.runTests(
          nonExistentPath,
          List.empty,
          handler,
          Map.empty,
          Path.of("."),
          killSignal
        )
      } yield res).attempt.unsafeRunSync()

      result.isLeft || result.exists(r => !r.isSuccess) shouldBe true
    }
  }

  test("Kotlin/Native: handles crash during execution") {
    failAfter(quickTimeout) {
      assume(isUnixLike, "Unix-like OS required")

      val tempDir = createTempDir("kotlinnative-crash-test")
      try {
        val binary = tempDir.resolve("kotlin-crash")
        Files.writeString(
          binary,
          """#!/bin/bash
          |echo "[==========] Running tests from MySuite"
          |kill -SEGV $$
          |""".stripMargin
        )
        binary.toFile.setExecutable(true)

        val handler = new RecordingKotlinTestEventHandler()
        val result = (for {
          killSignal <- Outcome.neverKillSignal
          res <- KotlinTestRunner.Native.runTests(
            binary,
            List.empty,
            handler,
            Map.empty,
            tempDir,
            killSignal
          )
        } yield res).unsafeRunSync()

        result.isSuccess shouldBe false
      } finally deleteRecursively(tempDir)
    }
  }

  // ==========================================================================
  // Link Executor Edge Cases
  // ==========================================================================

  test("LinkExecutor: JVM platform returns NotApplicable immediately") {
    failAfter(quickTimeout) {
      val linkTask = TaskDag.LinkTask(
        project = CrossProjectName(ProjectName("test"), None),
        platform = TaskDag.LinkPlatform.Jvm,
        releaseMode = false,
        isTest = false
      )

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        outcome <- LinkExecutor.execute(
          linkTask,
          classpath = Seq.empty,
          mainClass = None,
          baseOutputDir = Path.of("/tmp/link-test"),
          logger = LinkExecutor.LinkLogger.Silent,
          killSignal = killSignal
        )
      } yield outcome).unsafeRunSync()

      result._1 shouldBe TaskDag.TaskResult.Success
      result._2 shouldBe TaskDag.LinkResult.NotApplicable
    }
  }

  test("LinkExecutor: pre-cancelled returns Cancelled immediately") {
    failAfter(quickTimeout) {
      val linkTask = TaskDag.LinkTask(
        project = CrossProjectName(ProjectName("test"), None),
        platform = TaskDag.LinkPlatform.ScalaJs("1.16.0", "3.3.3", ScalaJsLinkConfig.Debug),
        releaseMode = false,
        isTest = false
      )

      val result = (for {
        killSignal <- cats.effect.Deferred[IO, bleep.bsp.protocol.KillReason]
        _ <- killSignal.complete(bleep.bsp.protocol.KillReason.UserRequest)
        outcome <- LinkExecutor.execute(
          linkTask,
          classpath = Seq.empty,
          mainClass = None,
          baseOutputDir = Path.of("/tmp/link-test"),
          logger = LinkExecutor.LinkLogger.Silent,
          killSignal = killSignal
        )
      } yield outcome).unsafeRunSync()

      result._1 shouldBe a[TaskDag.TaskResult.Killed]
      result._2 shouldBe TaskDag.LinkResult.Cancelled
    }
  }

  // ==========================================================================
  // Helpers
  // ==========================================================================

  /** The callbacks arrive on the runner's reader threads — stdout and stderr drain independently — so every append is synchronized and every read takes a
    * snapshot under the same lock. Unguarded `mutable.Buffer` appends raced inside `ensureSize` and threw `ArrayIndexOutOfBoundsException: arraycopy` out of a
    * test that had nothing to do with concurrency.
    */
  class RecordingTestEventHandler extends TestRunnerTypes.TestEventHandler {
    private val testStartsBuffer = mutable.Buffer[(String, String)]()
    private val testFinishesBuffer = mutable.Buffer[(String, String, TestStatus, Long, Option[String])]()
    private val suiteStartsBuffer = mutable.Buffer[String]()
    private val suiteFinishesBuffer = mutable.Buffer[(String, Int, Int, Int)]()
    private val outputsBuffer = mutable.Buffer[(String, String, OutputChannel)]()

    def testStarts: List[(String, String)] = synchronized(testStartsBuffer.toList)
    def testFinishes: List[(String, String, TestStatus, Long, Option[String])] = synchronized(testFinishesBuffer.toList)
    def suiteStarts: List[String] = synchronized(suiteStartsBuffer.toList)
    def suiteFinishes: List[(String, Int, Int, Int)] = synchronized(suiteFinishesBuffer.toList)
    def outputs: List[(String, String, OutputChannel)] = synchronized(outputsBuffer.toList)

    def onTestStarted(suite: String, test: String): Unit = synchronized(testStartsBuffer += ((suite, test)))
    def onTestFinished(suite: String, test: String, status: TestStatus, durationMs: Long, message: Option[String]): Unit =
      synchronized(testFinishesBuffer += ((suite, test, status, durationMs, message)))
    def onSuiteStarted(suite: String): Unit = synchronized(suiteStartsBuffer += suite)
    def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = synchronized(suiteFinishesBuffer += ((suite, passed, failed, skipped)))
    def onOutput(suite: String, line: String, channel: OutputChannel): Unit = synchronized(outputsBuffer += ((suite, line, channel)))
  }

  // ==========================================================================
  // Truncated Output Detection (all platforms)
  // ==========================================================================

  test("Scala Native: detects truncated output when process exits mid-suite") {
    failAfter(quickTimeout) {
      assume(isUnixLike, "Unix-like OS required")

      val tempDir = createTempDir("native-truncated-test")
      try {
        val binary = tempDir.resolve("truncated-binary")
        Files.writeString(
          binary,
          """#!/bin/bash
          |echo "TruncatedSuite:"
          |# Exit before any tests or suite summary are emitted
          |exit 0
          |""".stripMargin
        )
        binary.toFile.setExecutable(true)

        val handler = new RecordingNativeTestEventHandler()
        val result = (for {
          killSignal <- Outcome.neverKillSignal
          res <- ScalaNativeTestRunner.runTests(
            binary,
            List.empty,
            TestRunnerTypes.TestFramework.MUnit,
            handler,
            Map.empty,
            tempDir,
            killSignal
          )
        } yield res).unsafeRunSync()

        result.terminationReason should not be a[TestRunnerTypes.TerminationReason.Killed]
        result.isSuccess shouldBe false
        result.failed should be >= 1
        result.terminationReason shouldBe a[TestRunnerTypes.TerminationReason.TruncatedOutput]
      } finally deleteRecursively(tempDir)
    }
  }

  test("Kotlin/Native: detects truncated output when process exits mid-suite") {
    failAfter(quickTimeout) {
      assume(isUnixLike, "Unix-like OS required")

      val tempDir = createTempDir("kotlin-native-truncated-test")
      try {
        val binary = tempDir.resolve("truncated-binary")
        Files.writeString(
          binary,
          """#!/bin/bash
          |echo "[----------] 1 test from TruncatedSuite"
          |# Exit before any tests or summary are emitted
          |exit 0
          |""".stripMargin
        )
        binary.toFile.setExecutable(true)

        val handler = new RecordingKotlinTestEventHandler()
        val result = (for {
          killSignal <- Outcome.neverKillSignal
          res <- KotlinTestRunner.Native.runTests(
            binary,
            List.empty,
            handler,
            Map.empty,
            tempDir,
            killSignal
          )
        } yield res).unsafeRunSync()

        result.terminationReason should not be a[TestRunnerTypes.TerminationReason.Killed]
        result.isSuccess shouldBe false
        result.failed shouldBe 1
        result.terminationReason shouldBe a[TestRunnerTypes.TerminationReason.TruncatedOutput]
      } finally deleteRecursively(tempDir)
    }
  }

  class RecordingNativeTestEventHandler extends TestRunnerTypes.TestEventHandler {
    val testStarts = mutable.Buffer[(String, String)]()
    val testFinishes = mutable.Buffer[(String, String, TestStatus, Long, Option[String])]()
    val suiteStarts = mutable.Buffer[String]()
    val suiteFinishes = mutable.Buffer[(String, Int, Int, Int)]()
    val outputs = mutable.Buffer[(String, String, OutputChannel)]()

    def onTestStarted(suite: String, test: String): Unit = testStarts += ((suite, test))
    def onTestFinished(suite: String, test: String, status: TestStatus, durationMs: Long, message: Option[String]): Unit =
      testFinishes += ((suite, test, status, durationMs, message))
    def onSuiteStarted(suite: String): Unit = suiteStarts += suite
    def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = suiteFinishes += ((suite, passed, failed, skipped))
    def onOutput(suite: String, line: String, channel: OutputChannel): Unit = outputs += ((suite, line, channel))
  }

  class RecordingKotlinTestEventHandler extends TestRunnerTypes.TestEventHandler {
    val testStarts = mutable.Buffer[(String, String)]()
    val testFinishes = mutable.Buffer[(String, String, TestStatus, Long, Option[String])]()
    val suiteStarts = mutable.Buffer[String]()
    val suiteFinishes = mutable.Buffer[(String, Int, Int, Int)]()
    val outputs = mutable.Buffer[(String, String, OutputChannel)]()

    def onTestStarted(suite: String, test: String): Unit = testStarts += ((suite, test))
    def onTestFinished(suite: String, test: String, status: TestStatus, durationMs: Long, message: Option[String]): Unit =
      testFinishes += ((suite, test, status, durationMs, message))
    def onSuiteStarted(suite: String): Unit = suiteStarts += suite
    def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = suiteFinishes += ((suite, passed, failed, skipped))
    def onOutput(suite: String, line: String, channel: OutputChannel): Unit = outputs += ((suite, line, channel))
  }
}
