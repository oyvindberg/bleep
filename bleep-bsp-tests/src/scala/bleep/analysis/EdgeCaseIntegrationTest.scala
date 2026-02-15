package bleep.analysis

import bleep.bsp.{KotlinTestRunner, LinkExecutor, Outcome, ScalaJsTestRunner, ScalaNativeTestRunner, TaskDag}
import bleep.bsp.Outcome.KillReason
import bleep.model.{CrossProjectName, ProjectName}
import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import org.scalatest.concurrent.TimeLimits
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.concurrent.duration._

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

  // Timeouts for different test categories
  val quickTimeout = Span(5, Seconds) // Tests that should complete almost instantly
  val mediumTimeout = Span(10, Seconds) // Tests with moderate work
  val cancellationTimeout = Span(5, Seconds) // Tests that rely on cancellation (should be fast)

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
  // Scala.js Edge Cases
  // ==========================================================================

  test("Scala.js: handles uncaught exception in test") {
    failAfter(quickTimeout) {

      val tempDir = createTempDir("scalajs-exception-test")
      try {
        val jsFile = tempDir.resolve("test-exception.js")
        Files.writeString(
          jsFile,
          """
          |console.log('##scalajs-test##suite-started|ExceptionSuite');
          |console.log('##scalajs-test##test-started|ExceptionSuite|throwingTest');
          |throw new Error('Unexpected error in test!');
          |""".stripMargin
        )

        val handler = new RecordingTestEventHandler()
        val result = (for {
          killSignal <- Outcome.neverKillSignal
          res <- ScalaJsTestRunner.runTests(
            jsFile,
            ScalaJsLinkConfig.ModuleKind.CommonJSModule,
            List(ScalaJsTestRunner.TestSuite("ExceptionSuite", "ExceptionSuite")),
            handler,
            ScalaJsTestRunner.NodeEnvironment.Node,
            Map.empty,
            killSignal
          )
        } yield res).unsafeRunSync()

        // Process crashed, but we handle gracefully
        result.terminationReason should not be a[ScalaJsTestRunner.TerminationReason.Killed]
        handler.suiteStarts should contain("ExceptionSuite")
      } finally deleteRecursively(tempDir)
    }
  }

  test("Scala.js: handles process.exit(1) in test") {
    failAfter(quickTimeout) {

      val tempDir = createTempDir("scalajs-exit-test")
      try {
        val jsFile = tempDir.resolve("test-exit.js")
        Files.writeString(
          jsFile,
          """
          |console.log('##scalajs-test##suite-started|ExitSuite');
          |console.log('##scalajs-test##test-started|ExitSuite|exitTest');
          |process.exit(1);
          |""".stripMargin
        )

        val handler = new RecordingTestEventHandler()
        val result = (for {
          killSignal <- Outcome.neverKillSignal
          res <- ScalaJsTestRunner.runTests(
            jsFile,
            ScalaJsLinkConfig.ModuleKind.CommonJSModule,
            List(ScalaJsTestRunner.TestSuite("ExitSuite", "ExitSuite")),
            handler,
            ScalaJsTestRunner.NodeEnvironment.Node,
            Map.empty,
            killSignal
          )
        } yield res).unsafeRunSync()

        result.terminationReason should not be a[ScalaJsTestRunner.TerminationReason.Killed]
        result.failed shouldBe 1
        result.terminationReason shouldBe a[ScalaJsTestRunner.TerminationReason.TruncatedOutput]
        handler.suiteStarts should contain("ExitSuite")
      } finally deleteRecursively(tempDir)
    }
  }

  test("Scala.js: handles process.exit(0) mid-test") {
    failAfter(quickTimeout) {

      val tempDir = createTempDir("scalajs-exit0-test")
      try {
        val jsFile = tempDir.resolve("test-exit0.js")
        Files.writeString(
          jsFile,
          """
          |console.log('##scalajs-test##suite-started|EarlyExitSuite');
          |console.log('##scalajs-test##test-started|EarlyExitSuite|test1');
          |console.log('##scalajs-test##test-finished|EarlyExitSuite|test1|passed|10|');
          |process.exit(0);
          |""".stripMargin
        )

        val handler = new RecordingTestEventHandler()
        val result = (for {
          killSignal <- Outcome.neverKillSignal
          res <- ScalaJsTestRunner.runTests(
            jsFile,
            ScalaJsLinkConfig.ModuleKind.CommonJSModule,
            List(ScalaJsTestRunner.TestSuite("EarlyExitSuite", "EarlyExitSuite")),
            handler,
            ScalaJsTestRunner.NodeEnvironment.Node,
            Map.empty,
            killSignal
          )
        } yield res).unsafeRunSync()

        // test-finished was emitted but suite-finished was not (process.exit cut it off)
        handler.testFinishes.count(_._3 == ScalaJsTestRunner.TestStatus.Passed) shouldBe 1
        // Runner detects the truncated suite
        result.failed shouldBe 1
        result.terminationReason shouldBe a[ScalaJsTestRunner.TerminationReason.TruncatedOutput]
      } finally deleteRecursively(tempDir)
    }
  }

  test("Scala.js: handles syntax error in JS file") {
    failAfter(quickTimeout) {

      val tempDir = createTempDir("scalajs-syntax-test")
      try {
        val jsFile = tempDir.resolve("test-syntax.js")
        Files.writeString(
          jsFile,
          """
          |console.log('starting');
          |function broken( {  // Syntax error - missing )
          |  return 42;
          |}
          |""".stripMargin
        )

        val handler = new RecordingTestEventHandler()
        val result = (for {
          killSignal <- Outcome.neverKillSignal
          res <- ScalaJsTestRunner.runTests(
            jsFile,
            ScalaJsLinkConfig.ModuleKind.CommonJSModule,
            List(ScalaJsTestRunner.TestSuite("SyntaxErrorSuite", "SyntaxErrorSuite")),
            handler,
            ScalaJsTestRunner.NodeEnvironment.Node,
            Map.empty,
            killSignal
          )
        } yield res).unsafeRunSync()

        // Should not crash the runner
        result.terminationReason should not be a[ScalaJsTestRunner.TerminationReason.Killed]
      } finally deleteRecursively(tempDir)
    }
  }

  test("Scala.js: handles slow infinite output with cancellation") {
    failAfter(cancellationTimeout) {

      val tempDir = createTempDir("scalajs-infinite-test")
      try {
        val jsFile = tempDir.resolve("test-infinite.js")
        // Top-level infinite loop that blocks vm.runInNewContext forever,
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

        val handler = new RecordingTestEventHandler()

        val result = (for {
          killSignal <- Deferred[IO, KillReason]
          _ <- (IO.sleep(200.milliseconds) >> killSignal.complete(KillReason.UserRequest)).start
          r <- ScalaJsTestRunner.runTests(
            jsFile,
            ScalaJsLinkConfig.ModuleKind.CommonJSModule,
            List(ScalaJsTestRunner.TestSuite("InfiniteSuite", "InfiniteSuite")),
            handler,
            ScalaJsTestRunner.NodeEnvironment.Node,
            Map.empty,
            killSignal
          )
        } yield r).unsafeRunSync()

        result.terminationReason shouldBe a[ScalaJsTestRunner.TerminationReason.Killed]
      } finally deleteRecursively(tempDir)
    }
  }

  test("Scala.js: handles blocking infinite loop with cancellation") {
    failAfter(cancellationTimeout) {

      val tempDir = createTempDir("scalajs-blocking-test")
      try {
        val jsFile = tempDir.resolve("test-blocking.js")
        // Top-level synchronous infinite loop that blocks vm.runInNewContext forever.
        // The kill signal must destroy the process.
        Files.writeString(
          jsFile,
          """
          |while(true) {}
          |""".stripMargin
        )

        val handler = new RecordingTestEventHandler()

        val result = (for {
          killSignal <- Deferred[IO, KillReason]
          _ <- (IO.sleep(200.milliseconds) >> killSignal.complete(KillReason.UserRequest)).start
          r <- ScalaJsTestRunner.runTests(
            jsFile,
            ScalaJsLinkConfig.ModuleKind.CommonJSModule,
            List(ScalaJsTestRunner.TestSuite("BlockingSuite", "BlockingSuite")),
            handler,
            ScalaJsTestRunner.NodeEnvironment.Node,
            Map.empty,
            killSignal
          )
        } yield r).unsafeRunSync()

        result.terminationReason shouldBe a[ScalaJsTestRunner.TerminationReason.Killed]
      } finally deleteRecursively(tempDir)
    }
  }

  test("Scala.js: handles missing JS file") {
    failAfter(quickTimeout) {
      val handler = new RecordingTestEventHandler()
      val nonExistentPath = Path.of("/non/existent/path/test.js")

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        res <- ScalaJsTestRunner.runTests(
          nonExistentPath,
          ScalaJsLinkConfig.ModuleKind.CommonJSModule,
          List(ScalaJsTestRunner.TestSuite("Missing", "Missing")),
          handler,
          ScalaJsTestRunner.NodeEnvironment.Node,
          Map.empty,
          killSignal
        )
      } yield res).attempt.unsafeRunSync()

      // Should handle gracefully - either error or failed result
      result.isLeft || result.exists(r => !r.isSuccess) shouldBe true
    }
  }

  test("Scala.js: handles malformed test event output") {
    failAfter(quickTimeout) {

      val tempDir = createTempDir("scalajs-malformed-test")
      try {
        val jsFile = tempDir.resolve("test-malformed.js")
        Files.writeString(
          jsFile,
          """
          |console.log('##scalajs-test##suite-started|GoodSuite');
          |console.log('##scalajs-test##test-started|GoodSuite|test1');
          |console.log('##scalajs-test##garbage|not|valid|format');
          |console.log('##scalajs-test##');
          |console.log('##scalajs-test##test-finished|');
          |console.log('##scalajs-test##test-finished|GoodSuite|test1|passed|5|');
          |console.log('##scalajs-test##suite-finished|GoodSuite|1|0|0');
          |process.exit(0);
          |""".stripMargin
        )

        val handler = new RecordingTestEventHandler()
        val result = (for {
          killSignal <- Outcome.neverKillSignal
          res <- ScalaJsTestRunner.runTests(
            jsFile,
            ScalaJsLinkConfig.ModuleKind.CommonJSModule,
            List(ScalaJsTestRunner.TestSuite("GoodSuite", "GoodSuite")),
            handler,
            ScalaJsTestRunner.NodeEnvironment.Node,
            Map.empty,
            killSignal
          )
        } yield res).unsafeRunSync()

        // Should still process valid events
        result.passed shouldBe 1
      } finally deleteRecursively(tempDir)
    }
  }

  test("Scala.js: handles stderr output") {
    failAfter(quickTimeout) {

      val tempDir = createTempDir("scalajs-stderr-test")
      try {
        val jsFile = tempDir.resolve("test-stderr.js")
        Files.writeString(
          jsFile,
          """
          |console.log('##scalajs-test##suite-started|StderrSuite');
          |console.log('##scalajs-test##test-started|StderrSuite|test1');
          |console.error('This is an error message to stderr');
          |console.log('##scalajs-test##test-finished|StderrSuite|test1|passed|5|');
          |console.log('##scalajs-test##suite-finished|StderrSuite|1|0|0');
          |""".stripMargin
        )

        val handler = new RecordingTestEventHandler()
        val result = (for {
          killSignal <- Outcome.neverKillSignal
          res <- ScalaJsTestRunner.runTests(
            jsFile,
            ScalaJsLinkConfig.ModuleKind.CommonJSModule,
            List(ScalaJsTestRunner.TestSuite("StderrSuite", "StderrSuite")),
            handler,
            ScalaJsTestRunner.NodeEnvironment.Node,
            Map.empty,
            killSignal
          )
        } yield res).unsafeRunSync()

        result.passed shouldBe 1
        handler.outputs.exists(_._3) shouldBe true // isError = true
      } finally deleteRecursively(tempDir)
    }
  }

  test("Scala.js: handles large output") {
    failAfter(mediumTimeout) {

      val tempDir = createTempDir("scalajs-largeoutput-test")
      try {
        // Reduced from 10000 to 1000 lines for faster test
        val jsFile = tempDir.resolve("test-large.js")
        Files.writeString(
          jsFile,
          """
          |console.log('##scalajs-test##suite-started|LargeSuite');
          |console.log('##scalajs-test##test-started|LargeSuite|largeOutputTest');
          |for (let i = 0; i < 1000; i++) {
          |  console.log('Line ' + i + ': ' + 'x'.repeat(50));
          |}
          |console.log('##scalajs-test##test-finished|LargeSuite|largeOutputTest|passed|100|');
          |console.log('##scalajs-test##suite-finished|LargeSuite|1|0|0');
          |process.exit(0);
          |""".stripMargin
        )

        val handler = new RecordingTestEventHandler()
        val result = (for {
          killSignal <- Outcome.neverKillSignal
          res <- ScalaJsTestRunner.runTests(
            jsFile,
            ScalaJsLinkConfig.ModuleKind.CommonJSModule,
            List(ScalaJsTestRunner.TestSuite("LargeSuite", "LargeSuite")),
            handler,
            ScalaJsTestRunner.NodeEnvironment.Node,
            Map.empty,
            killSignal
          )
        } yield res).unsafeRunSync()

        result.passed shouldBe 1
        handler.outputs.size should be > 100
      } finally deleteRecursively(tempDir)
    }
  }

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
          ScalaNativeTestRunner.TestFramework.Unknown,
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
            ScalaNativeTestRunner.TestFramework.Unknown,
            handler,
            Map.empty,
            tempDir,
            killSignal
          )
        } yield res).attempt.unsafeRunSync()

        // Should handle (either by auto-setting exec or failing gracefully)
        result.isRight shouldBe true
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
            ScalaNativeTestRunner.TestFramework.Unknown,
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
              ScalaNativeTestRunner.TestFramework.MUnit,
              handler,
              Map.empty,
              tempDir,
              killSignal
            )
          } yield res).unsafeRunSync()

          result.terminationReason should not be a[ScalaNativeTestRunner.TerminationReason.Killed]
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
            ScalaNativeTestRunner.TestFramework.Unknown,
            handler,
            Map.empty,
            tempDir,
            killSignal
          )
        } yield r).unsafeRunSync()

        result.terminationReason shouldBe a[ScalaNativeTestRunner.TerminationReason.Killed]
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
            ScalaNativeTestRunner.TestFramework.MUnit,
            handler,
            Map.empty,
            tempDir,
            killSignal
          )
        } yield res).unsafeRunSync()

        result.isSuccess shouldBe false
        result.terminationReason should not be a[ScalaNativeTestRunner.TerminationReason.Killed]
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
            List(KotlinTestRunner.TestSuite("ExceptionSuite", "ExceptionSuite")),
            handler,
            Map.empty,
            killSignal
          )
        } yield res).unsafeRunSync()

        result.terminationReason should not be a[KotlinTestRunner.TerminationReason.Killed]
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
            List(KotlinTestRunner.TestSuite("InfiniteSuite", "InfiniteSuite")),
            handler,
            Map.empty,
            killSignal
          )
        } yield r).unsafeRunSync()

        result.terminationReason shouldBe a[KotlinTestRunner.TerminationReason.Killed]
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
            List(KotlinTestRunner.TestSuite("BlockingSuite", "BlockingSuite")),
            handler,
            Map.empty,
            killSignal
          )
        } yield r).unsafeRunSync()

        result.terminationReason shouldBe a[KotlinTestRunner.TerminationReason.Killed]
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
        killSignal <- cats.effect.Deferred[IO, bleep.bsp.Outcome.KillReason]
        _ <- killSignal.complete(bleep.bsp.Outcome.KillReason.UserRequest)
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

  class RecordingTestEventHandler extends ScalaJsTestRunner.TestEventHandler {
    val testStarts = mutable.Buffer[(String, String)]()
    val testFinishes = mutable.Buffer[(String, String, ScalaJsTestRunner.TestStatus, Long, Option[String])]()
    val suiteStarts = mutable.Buffer[String]()
    val suiteFinishes = mutable.Buffer[(String, Int, Int, Int)]()
    val outputs = mutable.Buffer[(String, String, Boolean)]()

    def onTestStarted(suite: String, test: String): Unit = testStarts += ((suite, test))
    def onTestFinished(suite: String, test: String, status: ScalaJsTestRunner.TestStatus, durationMs: Long, message: Option[String]): Unit =
      testFinishes += ((suite, test, status, durationMs, message))
    def onSuiteStarted(suite: String): Unit = suiteStarts += suite
    def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = suiteFinishes += ((suite, passed, failed, skipped))
    def onOutput(suite: String, line: String, isError: Boolean): Unit = outputs += ((suite, line, isError))
  }

  // ==========================================================================
  // Truncated Output Detection (all platforms)
  // ==========================================================================

  test("Scala.js: detects truncated output when process exits mid-suite") {
    failAfter(quickTimeout) {

      val tempDir = createTempDir("scalajs-truncated-test")
      try {
        val jsFile = tempDir.resolve("test-truncated.js")
        Files.writeString(
          jsFile,
          """
          |console.log('##scalajs-test##suite-started|TruncatedSuite');
          |console.log('##scalajs-test##test-started|TruncatedSuite|test1');
          |console.log('##scalajs-test##test-finished|TruncatedSuite|test1|passed|5|');
          |// Exit before suite-finished is emitted
          |process.exit(0);
          |""".stripMargin
        )

        val handler = new RecordingTestEventHandler()
        val result = (for {
          killSignal <- Outcome.neverKillSignal
          res <- ScalaJsTestRunner.runTests(
            jsFile,
            ScalaJsLinkConfig.ModuleKind.CommonJSModule,
            List(ScalaJsTestRunner.TestSuite("TruncatedSuite", "TruncatedSuite")),
            handler,
            ScalaJsTestRunner.NodeEnvironment.Node,
            Map.empty,
            killSignal
          )
        } yield res).unsafeRunSync()

        result.terminationReason should not be a[ScalaJsTestRunner.TerminationReason.Killed]
        result.isSuccess shouldBe false
        result.failed shouldBe 1
        result.terminationReason shouldBe a[ScalaJsTestRunner.TerminationReason.TruncatedOutput]
      } finally deleteRecursively(tempDir)
    }
  }

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
            ScalaNativeTestRunner.TestFramework.MUnit,
            handler,
            Map.empty,
            tempDir,
            killSignal
          )
        } yield res).unsafeRunSync()

        result.terminationReason should not be a[ScalaNativeTestRunner.TerminationReason.Killed]
        result.isSuccess shouldBe false
        result.failed should be >= 1
        result.terminationReason shouldBe a[ScalaNativeTestRunner.TerminationReason.TruncatedOutput]
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

        result.terminationReason should not be a[KotlinTestRunner.TerminationReason.Killed]
        result.isSuccess shouldBe false
        result.failed shouldBe 1
        result.terminationReason shouldBe a[KotlinTestRunner.TerminationReason.TruncatedOutput]
      } finally deleteRecursively(tempDir)
    }
  }

  class RecordingNativeTestEventHandler extends ScalaNativeTestRunner.TestEventHandler {
    val testStarts = mutable.Buffer[(String, String)]()
    val testFinishes = mutable.Buffer[(String, String, ScalaNativeTestRunner.TestStatus, Long, Option[String])]()
    val suiteStarts = mutable.Buffer[String]()
    val suiteFinishes = mutable.Buffer[(String, Int, Int, Int)]()
    val outputs = mutable.Buffer[(String, String, Boolean)]()

    def onTestStarted(suite: String, test: String): Unit = testStarts += ((suite, test))
    def onTestFinished(suite: String, test: String, status: ScalaNativeTestRunner.TestStatus, durationMs: Long, message: Option[String]): Unit =
      testFinishes += ((suite, test, status, durationMs, message))
    def onSuiteStarted(suite: String): Unit = suiteStarts += suite
    def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = suiteFinishes += ((suite, passed, failed, skipped))
    def onOutput(suite: String, line: String, isError: Boolean): Unit = outputs += ((suite, line, isError))
  }

  class RecordingKotlinTestEventHandler extends KotlinTestRunner.TestEventHandler {
    val testStarts = mutable.Buffer[(String, String)]()
    val testFinishes = mutable.Buffer[(String, String, KotlinTestRunner.TestStatus, Long, Option[String])]()
    val suiteStarts = mutable.Buffer[String]()
    val suiteFinishes = mutable.Buffer[(String, Int, Int, Int)]()
    val outputs = mutable.Buffer[(String, String, Boolean)]()

    def onTestStarted(suite: String, test: String): Unit = testStarts += ((suite, test))
    def onTestFinished(suite: String, test: String, status: KotlinTestRunner.TestStatus, durationMs: Long, message: Option[String]): Unit =
      testFinishes += ((suite, test, status, durationMs, message))
    def onSuiteStarted(suite: String): Unit = suiteStarts += suite
    def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = suiteFinishes += ((suite, passed, failed, skipped))
    def onOutput(suite: String, line: String, isError: Boolean): Unit = outputs += ((suite, line, isError))
  }
}
