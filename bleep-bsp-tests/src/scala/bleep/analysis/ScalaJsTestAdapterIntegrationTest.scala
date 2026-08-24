package bleep.analysis

import bleep.analysis.PlatformTestHelper.assertCompleted
import bleep.bsp.{Outcome, ScalaJsTestRunner, TestRunnerTypes}
import bleep.bsp.ScalaNativeTestRunner.TestFramework
import bleep.bsp.TestRunnerTypes.TestSuite
import bleep.bsp.protocol.{OutputChannel, TestStatus}
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import scala.collection.mutable

/** Records every call the runner makes, letting a test assert on the sequence rather than on a summary count. */
class AdapterEventRecorder extends TestRunnerTypes.TestEventHandler {
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

/** Runs a real Scala.js munit suite through `org.scalajs.testing.adapter.TestAdapter`.
  *
  * The suite compiles Scala sources to `.sjsir`, links them with the test module initializer, and hands the linked JavaScript to the runner. A link with no
  * main class and `isTest = true` names `org.scalajs.testing.bridge.Bridge.start` as its entry point, which is the socket the adapter connects to.
  */
class ScalaJsTestAdapterIntegrationTest extends AnyFunSuite with Matchers with PlatformTestHelper {

  /** One suite with one passing test and one failing test. The failing test pins that the runner reports a failure rather than reporting the whole suite as
    * passed.
    */
  private val munitSource =
    """package example
      |
      |class ArithmeticSuite extends munit.FunSuite {
      |  test("addition adds") {
      |    assertEquals(1 + 1, 2)
      |  }
      |
      |  test("subtraction is deliberately wrong") {
      |    assertEquals(3 - 1, 5)
      |  }
      |}
      |""".stripMargin

  /** 
    * @return
    *   the linked main module, which the adapter runs under Node
    */
  private def compileAndLinkMunitSuite(tempDir: Path): Path = {
    val srcDir = tempDir.resolve("src")
    writeScalaSource(srcDir, "example", "ArithmeticSuite.scala", munitSource)

    val classesDir = tempDir.resolve("classes")
    val compileClasspath = compileForScalaJsWithDeps(
      srcDir,
      classesDir,
      DefaultScalaVersion,
      DefaultScalaJsVersion,
      CompilerTestLibraries.munitScalaJsLibrary
    )

    val linkDir = tempDir.resolve("linked")
    Files.createDirectories(linkDir)

    val linkClasspath = (compileClasspath ++ CompilerTestLibraries.scalaJsTestBridgeLibrary(DefaultScalaJsVersion)).distinct

    val linkResult = ScalaJsToolchain
      .forVersion(DefaultScalaJsVersion, DefaultScalaVersion)
      .link(
        ScalaJsLinkConfig.Debug.copy(moduleKind = ScalaJsLinkConfig.ModuleKind.CommonJSModule),
        linkClasspath,
        None,
        linkDir,
        "main",
        ScalaJsToolchain.Logger.Silent,
        CancellationToken.never,
        isTest = true
      )
      .unsafeRunSync()
      .assertCompleted

    assert(linkResult.isSuccess, "Linking the munit test suite failed")
    linkResult.mainModule
  }

  test("ScalaJsTestRunner.runTestsViaAdapter: reports one passing and one failing munit test") {
    withTempDir("sjs-adapter-munit") { tempDir =>
      val linkedJs = compileAndLinkMunitSuite(tempDir)
      val recorder = new AdapterEventRecorder()
      val suites = List(TestSuite("ArithmeticSuite", "example.ArithmeticSuite"))

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        res <- ScalaJsTestRunner.runTestsViaAdapter(
          linkedJs,
          ScalaJsLinkConfig.ModuleKind.CommonJSModule,
          suites,
          TestFramework.MUnit,
          recorder,
          nodeBinary,
          Map.empty,
          DefaultScalaJsVersion,
          killSignal
        )
      } yield res).unsafeRunSync()

      result.passed shouldBe 1
      result.failed shouldBe 1
      result.terminationReason shouldBe TestRunnerTypes.TerminationReason.Completed

      recorder.suiteStarts should contain("example.ArithmeticSuite")
      recorder.testFinishes.count(_._3 == TestStatus.Passed) shouldBe 1
      recorder.testFinishes.count(_._3 == TestStatus.Failed) shouldBe 1
      recorder.suiteFinishes should contain(("example.ArithmeticSuite", 1, 1, 0))
    }
  }
}
