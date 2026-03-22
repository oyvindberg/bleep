package bleep.analysis

import bleep.bsp.{KotlinTestRunner, LinkExecutor, Outcome, ScalaJsTestRunner, ScalaNativeTestRunner, TaskDag, TestRunnerTypes}
import bleep.bsp.protocol.TestStatus
import bleep.bsp.Outcome.KillReason
import bleep.model.{CrossProjectName, ProjectName}
import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import cats.syntax.traverse._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import org.scalatest.concurrent.TimeLimits
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.concurrent.duration._

/** Tests for timeout handling and resource cleanup.
  *
  * Verifies:
  *   - Proper process termination on cancellation
  *   - Resource cleanup (temp files, processes)
  *   - Concurrent cancellation safety
  *   - Parallel execution safety
  *
  * All tests have explicit timeouts to prevent hanging.
  */
class TimeoutAndResourceTest extends AnyFunSuite with Matchers with TimeLimits {

  val quickTimeout = Span(60, Seconds)
  val mediumTimeout = Span(10, Seconds)
  val parallelTimeout = Span(20, Seconds)

  def createTempDir(prefix: String): Path =
    Files.createTempDirectory(prefix)

  def deleteRecursively(path: Path): Unit =
    if (Files.exists(path)) {
      if (Files.isDirectory(path)) {
        import scala.jdk.StreamConverters._
        Files.list(path).toScala(List).foreach(deleteRecursively)
      }
      Files.delete(path)
    }

  private def isUnixLike: Boolean =
    System.getProperty("os.name").toLowerCase.contains("linux") ||
      System.getProperty("os.name").toLowerCase.contains("mac")

  // ==========================================================================
  // Cancellation Timing Tests
  // ==========================================================================

  test("Scala.js: immediate cancellation returns quickly") {
    failAfter(quickTimeout) {

      val tempDir = createTempDir("scalajs-immediate-cancel")
      try {
        val jsFile = tempDir.resolve("slow-test.js")
        Files.writeString(
          jsFile,
          """
          |setTimeout(() => { console.log('done'); }, 30000);
          |""".stripMargin
        )

        val handler = new RecordingHandler()

        val startTime = System.currentTimeMillis()
        val result = (for {
          killSignal <- Deferred[IO, KillReason]
          _ <- killSignal.complete(KillReason.UserRequest) // Cancel immediately
          res <- ScalaJsTestRunner.runTests(
            jsFile,
            ScalaJsLinkConfig.ModuleKind.CommonJSModule,
            List.empty,
            handler,
            ScalaJsTestRunner.NodeEnvironment.Node,
            Map.empty,
            killSignal
          )
        } yield res).unsafeRunSync()
        val duration = System.currentTimeMillis() - startTime

        result.terminationReason shouldBe a[TestRunnerTypes.TerminationReason.Killed]
        duration should be < 5000L
      } finally deleteRecursively(tempDir)
    }
  }

  test("Scala.js: cancellation during execution terminates process") {
    failAfter(quickTimeout) {

      val tempDir = createTempDir("scalajs-mid-cancel")
      try {
        // This JS file blocks synchronously during require() by spawning a sleep process.
        // The runner script calls require() which executes this code, blocking the main thread.
        // When the kill signal fires, destroyForcibly() kills the Node process.
        val jsFile = tempDir.resolve("long-test.js")
        Files.writeString(
          jsFile,
          """
          |require('child_process').execFileSync('sleep', ['60']);
          |""".stripMargin
        )

        val handler = new RecordingHandler()

        val startTime = System.currentTimeMillis()
        val result = (for {
          killSignal <- Deferred[IO, KillReason]
          _ <- (IO.sleep(500.milliseconds) >> killSignal.complete(KillReason.UserRequest)).start
          res <- ScalaJsTestRunner.runTests(
            jsFile,
            ScalaJsLinkConfig.ModuleKind.CommonJSModule,
            List.empty,
            handler,
            ScalaJsTestRunner.NodeEnvironment.Node,
            Map.empty,
            killSignal
          )
        } yield res).unsafeRunSync()
        val duration = System.currentTimeMillis() - startTime

        result.terminationReason shouldBe a[TestRunnerTypes.TerminationReason.Killed]
        duration should be < 10000L
      } finally deleteRecursively(tempDir)
    }
  }

  test("Scala Native: immediate cancellation returns quickly") {
    failAfter(quickTimeout) {
      assume(isUnixLike, "Unix-like OS required")

      val tempDir = createTempDir("native-immediate-cancel")
      try {
        val binary = tempDir.resolve("slow-binary")
        Files.writeString(
          binary,
          """#!/bin/bash
          |sleep 60
          |""".stripMargin
        )
        binary.toFile.setExecutable(true)

        val handler = new RecordingNativeHandler()

        val startTime = System.currentTimeMillis()
        val result = (for {
          killSignal <- Deferred[IO, KillReason]
          _ <- killSignal.complete(KillReason.UserRequest) // Cancel immediately
          res <- ScalaNativeTestRunner.runTests(
            binary,
            List.empty,
            ScalaNativeTestRunner.TestFramework.Unknown,
            handler,
            Map.empty,
            tempDir,
            killSignal
          )
        } yield res).unsafeRunSync()
        val duration = System.currentTimeMillis() - startTime

        result.terminationReason shouldBe a[TestRunnerTypes.TerminationReason.Killed]
        duration should be < 5000L
      } finally deleteRecursively(tempDir)
    }
  }

  // ==========================================================================
  // Concurrent Kill Signal Tests
  // ==========================================================================

  test("multiple concurrent kill signal completions are safe") {
    failAfter(quickTimeout) {
      val result = (for {
        killSignal <- Deferred[IO, KillReason]
        // Try to complete the deferred from multiple fibers concurrently
        fibers <- (1 to 10).toList.traverse(_ => killSignal.complete(KillReason.UserRequest).start)
        _ <- fibers.traverse(_.join)
        // At least one completion should succeed
        reason <- killSignal.get
      } yield reason).unsafeRunSync()

      result shouldBe KillReason.UserRequest
    }
  }

  test("neverKillSignal does not complete") {
    failAfter(quickTimeout) {
      val result = (for {
        killSignal <- Outcome.neverKillSignal
        // Try to get from the never signal with a timeout - should not complete
        maybeReason <- IO.race(killSignal.get, IO.sleep(100.milliseconds))
      } yield maybeReason).unsafeRunSync()

      // Should have completed on the right (sleep finished) not left (kill signal)
      result shouldBe a[Right[_, _]]
    }
  }

  test("Deferred kill signal starts uncompleted") {
    failAfter(quickTimeout) {
      val result = (for {
        killSignal <- Deferred[IO, KillReason]
        // Try to get with timeout - should not be completed yet
        maybeReasonBefore <- IO.race(killSignal.get, IO.sleep(50.milliseconds))
        // Complete it
        _ <- killSignal.complete(KillReason.UserRequest)
        // Now it should be completed
        reasonAfter <- killSignal.get
      } yield (maybeReasonBefore, reasonAfter)).unsafeRunSync()

      result._1 shouldBe a[Right[_, _]] // Not completed before
      result._2 shouldBe KillReason.UserRequest // Completed after
    }
  }

  // ==========================================================================
  // Parallel Execution Safety
  // ==========================================================================

  test("multiple test runs can execute in parallel") {
    failAfter(parallelTimeout) {

      val tempDir = createTempDir("scalajs-parallel")
      try {
        // Create multiple test files that output protocol events synchronously.
        // The runner script require()s these files, so all console.log calls execute
        // synchronously during require() before the runner calls process.exit(0).
        val testFiles = (1 to 3).map { i =>
          val jsFile = tempDir.resolve(s"test-$i.js")
          Files.writeString(
            jsFile,
            s"""
            |console.log('##scalajs-test##suite-started|Suite$i');
            |console.log('##scalajs-test##test-started|Suite$i|test1');
            |console.log('##scalajs-test##test-finished|Suite$i|test1|passed|${i * 10}|');
            |console.log('##scalajs-test##suite-finished|Suite$i|1|0|0');
            |""".stripMargin
          )
          jsFile
        }

        // Run all tests in parallel using Cats Effect parSequence
        import cats.syntax.parallel._
        val allResults = (for {
          killSignal <- Outcome.neverKillSignal
          results <- testFiles.toList.map { jsFile =>
            val handler = new RecordingHandler()
            ScalaJsTestRunner.runTests(
              jsFile,
              ScalaJsLinkConfig.ModuleKind.CommonJSModule,
              List.empty,
              handler,
              ScalaJsTestRunner.NodeEnvironment.Node,
              Map.empty,
              killSignal
            )
          }.parSequence
        } yield results).unsafeRunSync()

        // All runs should complete without hanging or being cancelled.
        allResults.foreach { result =>
          result.terminationReason shouldBe TestRunnerTypes.TerminationReason.Completed
        }
      } finally deleteRecursively(tempDir)
    }
  }

  // ==========================================================================
  // Process Kill Tests
  // ==========================================================================

  test("Scala Native: process is terminated on cancellation") {
    failAfter(quickTimeout) {
      assume(isUnixLike, "Unix-like OS required")

      val tempDir = createTempDir("native-process-kill")
      try {
        // Use exec to replace the shell process with sleep directly.
        // Without exec, destroyForcibly() kills bash but the child sleep
        // process inherits the pipe and keeps readLine() blocked.
        val binary = tempDir.resolve("long-runner")
        Files.writeString(
          binary,
          """#!/bin/sh
          |echo "Started"
          |exec sleep 60
          |""".stripMargin
        )
        binary.toFile.setExecutable(true)

        val handler = new RecordingNativeHandler()

        val startTime = System.currentTimeMillis()
        val result = (for {
          killSignal <- Deferred[IO, KillReason]
          _ <- (IO.sleep(200.milliseconds) >> killSignal.complete(KillReason.UserRequest)).start
          res <- ScalaNativeTestRunner.runTests(
            binary,
            List.empty,
            ScalaNativeTestRunner.TestFramework.Unknown,
            handler,
            Map.empty,
            tempDir,
            killSignal
          )
        } yield res).unsafeRunSync()
        val duration = System.currentTimeMillis() - startTime

        result.terminationReason shouldBe a[TestRunnerTypes.TerminationReason.Killed]
        duration should be < 5000L
      } finally deleteRecursively(tempDir)
    }
  }

  // ==========================================================================
  // Helpers
  // ==========================================================================

  class RecordingHandler extends TestRunnerTypes.TestEventHandler {
    val outputs = mutable.Buffer[(String, String, Boolean)]()
    def onTestStarted(suite: String, test: String): Unit = {}
    def onTestFinished(suite: String, test: String, status: TestStatus, durationMs: Long, message: Option[String]): Unit = {}
    def onSuiteStarted(suite: String): Unit = {}
    def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = {}
    def onOutput(suite: String, line: String, isError: Boolean): Unit = outputs += ((suite, line, isError))
  }

  class RecordingNativeHandler extends TestRunnerTypes.TestEventHandler {
    val outputs = mutable.Buffer[(String, String, Boolean)]()
    def onTestStarted(suite: String, test: String): Unit = {}
    def onTestFinished(suite: String, test: String, status: TestStatus, durationMs: Long, message: Option[String]): Unit = {}
    def onSuiteStarted(suite: String): Unit = {}
    def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = {}
    def onOutput(suite: String, line: String, isError: Boolean): Unit = outputs += ((suite, line, isError))
  }
}
