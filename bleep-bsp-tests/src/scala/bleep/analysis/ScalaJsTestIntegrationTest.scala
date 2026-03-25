package bleep.analysis

import bleep.bsp.{LinkExecutor, Outcome, ScalaJsTestRunner, TestRunnerTypes}
import bleep.bsp.protocol.{OutputChannel, TestStatus}
import bleep.bsp.Outcome.KillReason
import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.concurrent.duration._

/** Integration tests for Scala.js test execution.
  *
  * Tests:
  *   - Test discovery from linked JS
  *   - Test execution with different frameworks
  *   - Test event handling
  *   - Cancellation support
  */
class ScalaJsTestIntegrationTest extends AnyFunSuite with Matchers {

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

  // ==========================================================================
  // Test Event Handler
  // ==========================================================================

  class RecordingEventHandler extends TestRunnerTypes.TestEventHandler {
    val testStarts = mutable.Buffer[(String, String)]()
    val testFinishes = mutable.Buffer[(String, String, TestStatus, Long, Option[String])]()
    val suiteStarts = mutable.Buffer[String]()
    val suiteFinishes = mutable.Buffer[(String, Int, Int, Int)]()
    val outputs = mutable.Buffer[(String, String, OutputChannel)]()

    def onTestStarted(suite: String, test: String): Unit =
      testStarts += ((suite, test))

    def onTestFinished(suite: String, test: String, status: TestStatus, durationMs: Long, message: Option[String]): Unit =
      testFinishes += ((suite, test, status, durationMs, message))

    def onSuiteStarted(suite: String): Unit =
      suiteStarts += suite

    def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit =
      suiteFinishes += ((suite, passed, failed, skipped))

    def onOutput(suite: String, line: String, channel: OutputChannel): Unit =
      outputs += ((suite, line, channel))
  }

  // ==========================================================================
  // TestStatus Tests
  // ==========================================================================

  test("TestStatus: all statuses") {
    TestStatus.Passed shouldBe a[TestStatus]
    TestStatus.Failed shouldBe a[TestStatus]
    TestStatus.Skipped shouldBe a[TestStatus]
    TestStatus.Ignored shouldBe a[TestStatus]
    TestStatus.Cancelled shouldBe a[TestStatus]
  }

  // ==========================================================================
  // TestResult Tests
  // ==========================================================================

  test("TestResult: success when no failures") {
    val result = TestRunnerTypes.TestResult(
      passed = 5,
      failed = 0,
      skipped = 1,
      ignored = 0,
      terminationReason = TestRunnerTypes.TerminationReason.Completed
    )

    result.isSuccess shouldBe true
    result.passed shouldBe 5
    result.skipped shouldBe 1
  }

  test("TestResult: not success when failures") {
    val result = TestRunnerTypes.TestResult(
      passed = 4,
      failed = 1,
      skipped = 0,
      ignored = 0,
      terminationReason = TestRunnerTypes.TerminationReason.Completed
    )

    result.isSuccess shouldBe false
  }

  test("TestResult: not success when cancelled") {
    val result = TestRunnerTypes.TestResult(
      passed = 5,
      failed = 0,
      skipped = 0,
      ignored = 0,
      terminationReason = TestRunnerTypes.TerminationReason.Killed(bleep.bsp.Outcome.KillReason.UserRequest)
    )

    result.isSuccess shouldBe false
    result.terminationReason shouldBe a[TestRunnerTypes.TerminationReason.Killed]
  }

  // ==========================================================================
  // DiscoveredSuites Tests
  // ==========================================================================

  test("DiscoveredSuites: stores framework and suites") {
    val suites = List(
      TestRunnerTypes.TestSuite("MySuite", "com.example.MySuite"),
      TestRunnerTypes.TestSuite("OtherSuite", "com.example.OtherSuite")
    )
    val discovered = ScalaJsTestRunner.DiscoveredSuites("munit.Framework", suites)

    discovered.framework shouldBe "munit.Framework"
    discovered.suites should have size 2
  }

  // ==========================================================================
  // NodeEnvironment Tests
  // ==========================================================================

  test("NodeEnvironment: Node and JSDOM") {
    ScalaJsTestRunner.NodeEnvironment.Node shouldBe a[ScalaJsTestRunner.NodeEnvironment]

    val jsdom = ScalaJsTestRunner.NodeEnvironment.JSDOM("http://localhost")
    jsdom.url shouldBe "http://localhost"
  }

  // ==========================================================================
  // Test Execution with Mock JS
  // ==========================================================================

  test("runTests: with simple passing JS test") {

    val tempDir = createTempDir("scalajs-test-runner")
    try {
      // Create a mock test JS file that outputs expected format
      val jsFile = tempDir.resolve("test-runner.js")
      Files.writeString(
        jsFile,
        """
        |// Mock test output in expected format
        |console.log('##scalajs-test##suite-started|MySuite');
        |console.log('##scalajs-test##test-started|MySuite|test1');
        |console.log('##scalajs-test##test-finished|MySuite|test1|passed|10|');
        |console.log('##scalajs-test##test-started|MySuite|test2');
        |console.log('##scalajs-test##test-finished|MySuite|test2|passed|5|');
        |console.log('##scalajs-test##suite-finished|MySuite|2|0|0');
        |process.exit(0);
        |""".stripMargin
      )

      val handler = new RecordingEventHandler()
      val suites = List(TestRunnerTypes.TestSuite("MySuite", "MySuite"))

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        res <- ScalaJsTestRunner.runTests(
          jsFile,
          ScalaJsLinkConfig.ModuleKind.CommonJSModule,
          suites,
          handler,
          ScalaJsTestRunner.NodeEnvironment.Node,
          Map.empty,
          killSignal
        )
      } yield res).unsafeRunSync()

      result.passed shouldBe 2
      result.failed shouldBe 0
      result.terminationReason shouldBe TestRunnerTypes.TerminationReason.Completed

      handler.suiteStarts should contain("MySuite")
      handler.testStarts should have size 2
      handler.testFinishes.count(_._3 == TestStatus.Passed) shouldBe 2
    } finally deleteRecursively(tempDir)
  }

  test("runTests: with failing test") {

    val tempDir = createTempDir("scalajs-test-runner")
    try {
      val jsFile = tempDir.resolve("test-runner.js")
      Files.writeString(
        jsFile,
        """
        |console.log('##scalajs-test##suite-started|MySuite');
        |console.log('##scalajs-test##test-started|MySuite|passing');
        |console.log('##scalajs-test##test-finished|MySuite|passing|passed|10|');
        |console.log('##scalajs-test##test-started|MySuite|failing');
        |console.log('##scalajs-test##test-finished|MySuite|failing|failed|5|assertion failed');
        |console.log('##scalajs-test##suite-finished|MySuite|1|1|0');
        |process.exit(1);
        |""".stripMargin
      )

      val handler = new RecordingEventHandler()
      val suites = List(TestRunnerTypes.TestSuite("MySuite", "MySuite"))

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        res <- ScalaJsTestRunner.runTests(
          jsFile,
          ScalaJsLinkConfig.ModuleKind.CommonJSModule,
          suites,
          handler,
          ScalaJsTestRunner.NodeEnvironment.Node,
          Map.empty,
          killSignal
        )
      } yield res).unsafeRunSync()

      result.passed shouldBe 1
      result.failed shouldBe 1
      result.isSuccess shouldBe false

      val failedTest = handler.testFinishes.find(_._3 == TestStatus.Failed).get
      failedTest._2 shouldBe "failing"
      failedTest._5 shouldBe Some("assertion failed")
    } finally deleteRecursively(tempDir)
  }

  test("runTests: respects cancellation") {

    val tempDir = createTempDir("scalajs-test-runner")
    try {
      // Create a test that takes a while
      val jsFile = tempDir.resolve("test-runner.js")
      Files.writeString(
        jsFile,
        """
        |const start = Date.now();
        |while (Date.now() - start < 10000) {
        |  // Busy loop for 10 seconds
        |}
        |console.log('done');
        |""".stripMargin
      )

      val handler = new RecordingEventHandler()
      val suites = List(TestRunnerTypes.TestSuite("MySuite", "MySuite"))

      val result = (for {
        killSignal <- Deferred[IO, KillReason]
        _ <- (IO.sleep(100.milliseconds) >> killSignal.complete(KillReason.UserRequest)).start
        r <- ScalaJsTestRunner.runTests(
          jsFile,
          ScalaJsLinkConfig.ModuleKind.CommonJSModule,
          suites,
          handler,
          ScalaJsTestRunner.NodeEnvironment.Node,
          Map.empty,
          killSignal
        )
      } yield r).unsafeRunSync()

      result.terminationReason shouldBe a[TestRunnerTypes.TerminationReason.Killed]
    } finally deleteRecursively(tempDir)
  }
}
