package bleep.analysis

import bleep.bsp.protocol.{KillReason, OutputChannel, TestStatus}
import bleep.bsp.{Outcome, ScalaNativeTestRunner, TestRunnerTypes}
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import cats.syntax.traverse.*
import org.scalatest.concurrent.TimeLimits
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}

import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.concurrent.duration.*

/** Tests for timeout handling and resource cleanup.
  *
  * Verifies:
  *   - Proper process termination on cancellation
  *   - Resource cleanup (temp files, processes)
  *   - Concurrent cancellation safety
  *
  * All tests have explicit timeouts to prevent hanging.
  */
class TimeoutAndResourceTest extends AnyFunSuite with Matchers with TimeLimits {

  val quickTimeout = Span(60, Seconds)
  val mediumTimeout = Span(10, Seconds)

  /** A cancelled run that takes longer than this has a broken cancellation path. The two Scala Native cancellation tests in this suite use this limit.
    *
    * Each of those tests starts a job that would take 30s or 60s on its own. Returning in any fraction of that duration is the evidence that the kill reached
    * the process. The limit is deliberately generous. A tight wall-clock number measures how loaded the machine is rather than the cancellation path. These
    * tests run alongside the rest of the suite. A healthy run finishes in under a second. The worst run seen on a loaded CI runner took 6074ms. The shortest
    * workload being ruled out takes 30s.
    */
  val cancellationShortCircuitMs = 15000L

  def createTempDir(prefix: String): Path =
    Files.createTempDirectory(prefix)

  private def isUnixLike: Boolean =
    System.getProperty("os.name").toLowerCase.contains("linux") ||
      System.getProperty("os.name").toLowerCase.contains("mac")

  // ==========================================================================
  // Cancellation Timing Tests
  // ==========================================================================

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
            TestRunnerTypes.TestFramework.Unknown,
            handler,
            Map.empty,
            tempDir,
            killSignal
          )
        } yield res).unsafeRunSync()
        val duration = System.currentTimeMillis() - startTime

        result.terminationReason shouldBe a[TestRunnerTypes.TerminationReason.Killed]
        duration should be < cancellationShortCircuitMs
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
      result shouldBe a[Right[?, ?]]
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

      result._1 shouldBe a[Right[?, ?]] // Not completed before
      result._2 shouldBe KillReason.UserRequest // Completed after
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
            TestRunnerTypes.TestFramework.Unknown,
            handler,
            Map.empty,
            tempDir,
            killSignal
          )
        } yield res).unsafeRunSync()
        val duration = System.currentTimeMillis() - startTime

        result.terminationReason shouldBe a[TestRunnerTypes.TerminationReason.Killed]
        duration should be < cancellationShortCircuitMs
      } finally deleteRecursively(tempDir)
    }
  }

  // ==========================================================================
  // Helpers
  // ==========================================================================

  class RecordingHandler extends TestRunnerTypes.TestEventHandler {
    val outputs = mutable.Buffer[(String, String, OutputChannel)]()
    def onTestStarted(suite: String, test: String): Unit = {}
    def onTestFinished(suite: String, test: String, status: TestStatus, durationMs: Long, message: Option[String]): Unit = {}
    def onSuiteStarted(suite: String): Unit = {}
    def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = {}
    def onOutput(suite: String, line: String, channel: OutputChannel): Unit = outputs += ((suite, line, channel))
  }

  class RecordingNativeHandler extends TestRunnerTypes.TestEventHandler {
    val outputs = mutable.Buffer[(String, String, OutputChannel)]()
    def onTestStarted(suite: String, test: String): Unit = {}
    def onTestFinished(suite: String, test: String, status: TestStatus, durationMs: Long, message: Option[String]): Unit = {}
    def onSuiteStarted(suite: String): Unit = {}
    def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = {}
    def onOutput(suite: String, line: String, channel: OutputChannel): Unit = outputs += ((suite, line, channel))
  }
}
