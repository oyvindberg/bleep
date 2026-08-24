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

/** Records every call the runner makes. A test then asserts on the sequence of calls rather than on a summary count. */
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
  * This test compiles Scala sources to `.sjsir`, links those files as a test module, and hands the linked JavaScript to the runner.
  */
class ScalaJsTestAdapterIntegrationTest extends AnyFunSuite with Matchers with PlatformTestHelper {

  /** This source declares one suite with one passing test and one failing test. A runner that reported the whole suite as passed would fail on the second test.
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

  /** @return the linked main module. The adapter runs that module under Node. */
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

  test("CompilerResolver.getScalaJsTestAdapter: loads the adapter and the Node.js environment") {
    val loader = CompilerResolver.getScalaJsTestAdapter(DefaultScalaJsVersion).loader

    loader.loadClass("org.scalajs.testing.adapter.TestAdapter").getName shouldBe "org.scalajs.testing.adapter.TestAdapter"
    loader.loadClass("org.scalajs.jsenv.nodejs.NodeJSEnv").getName shouldBe "org.scalajs.jsenv.nodejs.NodeJSEnv"
  }

  /** `CompilerTopLoader` hands `sbt.testing.*` to bleep's own classloader. A `Framework` the isolated loader returns is therefore assignable to bleep's
    * `sbt.testing.Framework`. Narrowing that delegation would break the runner with a `ClassCastException` at run time. This assertion fails at test time
    * instead.
    */
  test("CompilerResolver.getScalaJsTestAdapter: loads sbt.testing.Framework from bleep's own classloader") {
    val loader = CompilerResolver.getScalaJsTestAdapter(DefaultScalaJsVersion).loader

    loader.loadClass("sbt.testing.Framework") shouldBe classOf[sbt.testing.Framework]
  }

  test("CompilerResolver.getScalaJsTestAdapter: caches one instance per Scala.js version") {
    val first = CompilerResolver.getScalaJsTestAdapter(DefaultScalaJsVersion)
    val second = CompilerResolver.getScalaJsTestAdapter(DefaultScalaJsVersion)

    first.loader shouldBe second.loader
  }
}
