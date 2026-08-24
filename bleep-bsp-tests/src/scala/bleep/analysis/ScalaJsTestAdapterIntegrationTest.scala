package bleep.analysis

import bleep.analysis.PlatformTestHelper.assertCompleted
import bleep.bsp.{Outcome, RecordingHandler, ScalaJsTestRunner, SuiteFinished}
import bleep.bsp.TestRunnerTypes
import bleep.bsp.TestRunnerTypes.{TestFramework, TestSuite}
import bleep.bsp.protocol.TestStatus
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

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

  /** utest reaches its suites through a `TestSuite` object rather than a class. */
  private val utestSource =
    """package example
      |
      |import utest._
      |
      |object ArithmeticSuite extends TestSuite {
      |  val tests = Tests {
      |    test("addition adds") {
      |      assert(1 + 1 == 2)
      |    }
      |
      |    test("subtraction is deliberately wrong") {
      |      assert(3 - 1 == 5)
      |    }
      |  }
      |}
      |""".stripMargin

  /** Compile one source for Scala.js and link it as a test module.
    *
    * @param frameworkJars
    *   the test framework compiled for Scala.js, which the source imports
    * @param moduleKind
    *   the module kind to link with. The runner must pick the matching `Input` case to load the result.
    * @return
    *   the linked main module. The adapter runs that module under Node.
    */
  private def compileAndLink(
      tempDir: Path,
      source: String,
      frameworkJars: Seq[Path],
      moduleKind: ScalaJsLinkConfig.ModuleKind
  ): Path = {
    val srcDir = tempDir.resolve("src")
    writeScalaSource(srcDir, "example", "ArithmeticSuite.scala", source)

    val classesDir = tempDir.resolve("classes")
    val compileClasspath = compileForScalaJsWithDeps(srcDir, classesDir, DefaultScalaVersion, DefaultScalaJsVersion, frameworkJars)

    val linkDir = tempDir.resolve("linked")
    Files.createDirectories(linkDir)

    val linkClasspath = (compileClasspath ++ CompilerTestLibraries.scalaJsTestBridgeLibrary(DefaultScalaJsVersion)).distinct

    val linkResult = ScalaJsToolchain
      .forVersion(DefaultScalaJsVersion, DefaultScalaVersion)
      .link(
        ScalaJsLinkConfig.Debug.copy(moduleKind = moduleKind),
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

    assert(linkResult.isSuccess, "Linking the test suite failed")
    linkResult.mainModule
  }

  private def runThroughAdapter(
      linkedJs: Path,
      moduleKind: ScalaJsLinkConfig.ModuleKind,
      framework: TestFramework,
      recorder: RecordingHandler
  ): TestRunnerTypes.TestResult =
    (for {
      killSignal <- Outcome.neverKillSignal
      result <- ScalaJsTestRunner.runTestsViaAdapter(
        linkedJs,
        moduleKind,
        List(TestSuite("ArithmeticSuite", "example.ArithmeticSuite")),
        framework,
        recorder,
        nodeBinary,
        Map.empty,
        DefaultScalaJsVersion,
        killSignal
      )
    } yield result).unsafeRunSync()

  test("ScalaJsTestRunner.runTestsViaAdapter: reports one passing and one failing munit test") {
    withTempDir("sjs-adapter-munit") { tempDir =>
      val linkedJs = compileAndLink(tempDir, munitSource, CompilerTestLibraries.munitScalaJsLibrary, ScalaJsLinkConfig.ModuleKind.CommonJSModule)
      val recorder = new RecordingHandler()

      val result = runThroughAdapter(linkedJs, ScalaJsLinkConfig.ModuleKind.CommonJSModule, TestFramework.MUnit, recorder)

      result.passed shouldBe 1
      result.failed shouldBe 1
      result.terminationReason shouldBe TestRunnerTypes.TerminationReason.Completed

      recorder.suiteStarts should contain("example.ArithmeticSuite")
      recorder.testFinishes.count(_.status == TestStatus.Passed) shouldBe 1
      recorder.testFinishes.count(_.status == TestStatus.Failed) shouldBe 1
      recorder.suiteFinishes should contain(SuiteFinished("example.ArithmeticSuite", 1, 1, 0))
      recorder.testFinishes.map(_.test) should contain theSameElementsAs Seq("addition adds", "subtraction is deliberately wrong")
    }
  }

  test("ScalaJsTestRunner.runTestsViaAdapter: reports one passing and one failing utest test") {
    withTempDir("sjs-adapter-utest") { tempDir =>
      val linkedJs = compileAndLink(tempDir, utestSource, CompilerTestLibraries.utestScalaJsLibrary, ScalaJsLinkConfig.ModuleKind.CommonJSModule)
      val recorder = new RecordingHandler()

      val result = runThroughAdapter(linkedJs, ScalaJsLinkConfig.ModuleKind.CommonJSModule, TestFramework.UTest, recorder)

      result.passed shouldBe 1
      result.failed shouldBe 1
      result.terminationReason shouldBe TestRunnerTypes.TerminationReason.Completed
      recorder.suiteStarts should contain("example.ArithmeticSuite")
    }
  }

  /** An ESModule link produces a file the adapter can only load through `Input.ESModule`. Passing the wrong `Input` case fails the run.
    */
  test("ScalaJsTestRunner.runTestsViaAdapter: runs a suite linked as an ESModule") {
    withTempDir("sjs-adapter-esmodule") { tempDir =>
      val linkedJs = compileAndLink(tempDir, munitSource, CompilerTestLibraries.munitScalaJsLibrary, ScalaJsLinkConfig.ModuleKind.ESModule)
      val recorder = new RecordingHandler()

      val result = runThroughAdapter(linkedJs, ScalaJsLinkConfig.ModuleKind.ESModule, TestFramework.MUnit, recorder)

      result.passed shouldBe 1
      result.failed shouldBe 1
      recorder.suiteStarts should contain("example.ArithmeticSuite")
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
