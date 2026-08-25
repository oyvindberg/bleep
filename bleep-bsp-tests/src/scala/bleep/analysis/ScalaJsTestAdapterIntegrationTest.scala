package bleep.analysis

import bleep.analysis.PlatformTestHelper.assertCompleted
import bleep.bsp.TestRunnerTypes.{TestFramework, TestSuite}
import bleep.bsp.protocol.{KillReason, TestStatus}
import bleep.bsp.{Outcome, RecordingHandler, ScalaJsTestRunner, TestRunnerTypes}
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** Runs real Scala.js suites through `org.scalajs.testing.adapter.TestAdapter`.
  *
  * Each test compiles Scala sources to `.sjsir`. Each test then links those files as a test module and hands the linked JavaScript to the runner.
  */
class ScalaJsTestAdapterIntegrationTest extends AnyFunSuite with Matchers with BeforeAndAfterAll with PlatformTestHelper {

  /** Every module this suite links goes in a subdirectory of this directory.
    */
  private lazy val linkRoot: Path = createTempDir("sjs-adapter-links")

  override def afterAll(): Unit = deleteRecursively(linkRoot)

  /** Every fixture in this suite gives its suite class this name. A single class name keeps the `TestSuite` value each run asks for identical to the
    * `TestSuite` value every other run asks for.
    */
  private val suiteClassName = "example.ArithmeticSuite"

  private val killedByUser: TestRunnerTypes.TerminationReason =
    TestRunnerTypes.TerminationReason.Killed(KillReason.UserRequest)

  /** A cancelled run that takes longer than this has hung. The spinning suite never returns on its own.
    */
  private val cancellationTimeoutMs = 30000L

  /** This source declares one suite with one passing test and one failing test. A runner that reported the whole suite as passed would fail the assertion on
    * the failing test.
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

  /** `bleep test` hands one environment to every platform. A test that asks for an environment variable must find that variable under Node as well as on the
    * JVM. This suite passes or fails purely from `process.env`. Two tests run this suite. The first test sets the variable. The second test leaves the variable
    * unset.
    */
  private val envProbeSource =
    """package example
      |
      |import scala.scalajs.js
      |
      |class ArithmeticSuite extends munit.FunSuite {
      |  test("the node process sees the variable the runner set") {
      |    val fromNode = js.Dynamic.global.process.env.selectDynamic("BLEEP_JS_ENV_PROBE")
      |    assertEquals(fromNode.toString, "from-client")
      |  }
      |}
      |""".stripMargin

  /** A test that never returns. The cancellation cases race a kill signal against this suite. */
  private val spinningSource =
    """package example
      |
      |class ArithmeticSuite extends munit.FunSuite {
      |  test("this test never returns") {
      |    while (true) {}
      |  }
      |}
      |""".stripMargin

  /** Compile one source for Scala.js and link it as a test module.
    *
    * @param fixtureName
    *   the subdirectory of [[linkRoot]] this fixture compiles and links in
    * @param frameworkJars
    *   the test framework the source imports, compiled for Scala.js
    * @param moduleKind
    *   the module kind to link with. The runner must pick the matching `Input` case to load the result.
    * @return
    *   the linked main module. The adapter runs that module under Node.
    */
  private def compileAndLink(
      fixtureName: String,
      source: String,
      frameworkJars: Seq[Path],
      moduleKind: ScalaJsLinkConfig.ModuleKind
  ): Path = {
    val fixtureDir = linkRoot.resolve(fixtureName)
    val srcDir = fixtureDir.resolve("src")
    writeScalaSource(srcDir, "example", "ArithmeticSuite.scala", source)

    val classesDir = fixtureDir.resolve("classes")
    val compileClasspath = compileForScalaJsWithDeps(srcDir, classesDir, DefaultScalaVersion, DefaultScalaJsVersion, frameworkJars)

    val linkDir = fixtureDir.resolve("linked")
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

  private lazy val munitCommonJsModule: Path =
    compileAndLink("munit-commonjs", munitSource, CompilerTestLibraries.munitScalaJsLibrary, ScalaJsLinkConfig.ModuleKind.CommonJSModule)

  private lazy val munitEsModule: Path =
    compileAndLink("munit-esmodule", munitSource, CompilerTestLibraries.munitScalaJsLibrary, ScalaJsLinkConfig.ModuleKind.ESModule)

  private lazy val utestCommonJsModule: Path =
    compileAndLink("utest-commonjs", utestSource, CompilerTestLibraries.utestScalaJsLibrary, ScalaJsLinkConfig.ModuleKind.CommonJSModule)

  private lazy val envProbeModule: Path =
    compileAndLink("env-probe", envProbeSource, CompilerTestLibraries.munitScalaJsLibrary, ScalaJsLinkConfig.ModuleKind.CommonJSModule)

  private lazy val spinningModule: Path =
    compileAndLink("spinning", spinningSource, CompilerTestLibraries.munitScalaJsLibrary, ScalaJsLinkConfig.ModuleKind.CommonJSModule)

  private def runThroughAdapter(
      linkedJs: Path,
      moduleKind: ScalaJsLinkConfig.ModuleKind,
      framework: TestFramework,
      env: Map[String, String],
      recorder: RecordingHandler
  ): TestRunnerTypes.TestResult =
    (for {
      killSignal <- Outcome.neverKillSignal
      result <- ScalaJsTestRunner.runTestsViaAdapter(
        linkedJs,
        moduleKind,
        List(TestSuite("ArithmeticSuite", suiteClassName)),
        framework,
        recorder,
        nodeBinary,
        env,
        DefaultScalaJsVersion,
        killSignal
      )
    } yield result).unsafeRunSync()

  test("ScalaJsTestRunner.runTestsViaAdapter: reports one passing and one failing munit test") {
    val recorder = new RecordingHandler()

    val result = runThroughAdapter(munitCommonJsModule, ScalaJsLinkConfig.ModuleKind.CommonJSModule, TestFramework.MUnit, Map.empty, recorder)

    result.passed shouldBe 1
    result.failed shouldBe 1
    result.terminationReason shouldBe TestRunnerTypes.TerminationReason.Completed

    recorder.suiteStarts should contain(suiteClassName)
    recorder.testFinishes.count(_._3 == TestStatus.Passed) shouldBe 1
    recorder.testFinishes.count(_._3 == TestStatus.Failed) shouldBe 1
    recorder.suiteFinishes should contain((suiteClassName, 1, 1, 0))
    recorder.testFinishes.map(_._2) should contain theSameElementsAs Seq("addition adds", "subtraction is deliberately wrong")
  }

  test("ScalaJsTestRunner.runTestsViaAdapter: reports one passing and one failing utest test") {
    val recorder = new RecordingHandler()

    val result = runThroughAdapter(utestCommonJsModule, ScalaJsLinkConfig.ModuleKind.CommonJSModule, TestFramework.UTest, Map.empty, recorder)

    result.passed shouldBe 1
    result.failed shouldBe 1
    result.terminationReason shouldBe TestRunnerTypes.TerminationReason.Completed
    recorder.suiteStarts should contain(suiteClassName)
  }

  /** An ESModule link produces a file the adapter can only load through `Input.ESModule`. Passing the wrong `Input` case fails the run. */
  test("ScalaJsTestRunner.runTestsViaAdapter: runs a suite linked as an ESModule") {
    val recorder = new RecordingHandler()

    val result = runThroughAdapter(munitEsModule, ScalaJsLinkConfig.ModuleKind.ESModule, TestFramework.MUnit, Map.empty, recorder)

    result.passed shouldBe 1
    result.failed shouldBe 1
    recorder.suiteStarts should contain(suiteClassName)
  }

  test("ScalaJsTestRunner.runTestsViaAdapter: forwards env vars to the node process") {
    assert(sys.env.get("BLEEP_JS_ENV_PROBE").isEmpty, "probe var must not be set in this JVM or the test proves nothing")

    val result = runThroughAdapter(
      envProbeModule,
      ScalaJsLinkConfig.ModuleKind.CommonJSModule,
      TestFramework.MUnit,
      Map("BLEEP_JS_ENV_PROBE" -> "from-client"),
      new RecordingHandler()
    )

    result.passed shouldBe 1
    result.failed shouldBe 0
  }

  /** This test is the control for the test that sets the variable. The probe genuinely fails without the variable. A runner that ignored its `env` argument
    * could not pass both tests.
    */
  test("ScalaJsTestRunner.runTestsViaAdapter: the env probe fails without the variable") {
    val result =
      runThroughAdapter(envProbeModule, ScalaJsLinkConfig.ModuleKind.CommonJSModule, TestFramework.MUnit, Map.empty, new RecordingHandler())

    result.passed shouldBe 0
    result.failed shouldBe 1
  }

  /** A kill signal that has already completed stops the run before the adapter starts Node. */
  test("ScalaJsTestRunner.runTestsViaAdapter: a kill signal that already fired stops the run") {
    val result = (for {
      killSignal <- Deferred[IO, KillReason]
      _ <- killSignal.complete(KillReason.UserRequest)
      res <- ScalaJsTestRunner.runTestsViaAdapter(
        munitCommonJsModule,
        ScalaJsLinkConfig.ModuleKind.CommonJSModule,
        List(TestSuite("ArithmeticSuite", suiteClassName)),
        TestFramework.MUnit,
        new RecordingHandler(),
        nodeBinary,
        Map.empty,
        DefaultScalaJsVersion,
        killSignal
      )
    } yield res).unsafeRunSync()

    result.terminationReason shouldBe killedByUser
  }

  /** The spinning suite blocks Node's event loop forever. Cancellation has to stop the adapter rather than wait for the suite. */
  test("ScalaJsTestRunner.runTestsViaAdapter: cancellation stops a suite that never returns") {
    val startTime = System.currentTimeMillis()
    val result = (for {
      killSignal <- Deferred[IO, KillReason]
      _ <- (IO.sleep(2.seconds) >> killSignal.complete(KillReason.UserRequest)).start
      res <- ScalaJsTestRunner.runTestsViaAdapter(
        spinningModule,
        ScalaJsLinkConfig.ModuleKind.CommonJSModule,
        List(TestSuite("ArithmeticSuite", suiteClassName)),
        TestFramework.MUnit,
        new RecordingHandler(),
        nodeBinary,
        Map.empty,
        DefaultScalaJsVersion,
        killSignal
      )
    } yield res).unsafeRunSync()
    val duration = System.currentTimeMillis() - startTime

    result.terminationReason shouldBe killedByUser
    duration should be < cancellationTimeoutMs
  }

  /** Three adapters run at once, each with its own Node process. A runner that shared process state between runs would report the wrong counts. */
  /** The node processes this JVM started that are still running.
    *
    * The adapter starts node through a `ProcessBuilder`, which makes every node process a direct child of this JVM. A cancelled run that leaves one of these
    * behind leaves it spinning on a core until someone kills it.
    */
  private def liveNodeChildren(): Set[Long] =
    ProcessHandle
      .current()
      .children()
      .iterator()
      .asScala
      .filter(child => child.info().command().orElse("") == nodeBinary)
      .map(_.pid())
      .toSet

  test("ScalaJsTestRunner.runTestsViaAdapter: cancellation leaves no node process behind") {
    val before = liveNodeChildren()

    val result = (for {
      killSignal <- Deferred[IO, KillReason]
      _ <- (IO.sleep(2.seconds) >> killSignal.complete(KillReason.UserRequest)).start
      res <- ScalaJsTestRunner.runTestsViaAdapter(
        spinningModule,
        ScalaJsLinkConfig.ModuleKind.CommonJSModule,
        List(TestSuite("ArithmeticSuite", suiteClassName)),
        TestFramework.MUnit,
        new RecordingHandler(),
        nodeBinary,
        Map.empty,
        DefaultScalaJsVersion,
        killSignal
      )
    } yield res).unsafeRunSync()

    result.terminationReason shouldBe killedByUser

    val leaked = liveNodeChildren() -- before
    withClue(s"node processes still running after the kill: $leaked") {
      leaked shouldBe empty
    }
  }

  test("ScalaJsTestRunner.runTestsViaAdapter: three runs execute in parallel") {
    import cats.syntax.parallel.*

    val results = (for {
      killSignal <- Outcome.neverKillSignal
      results <- List
        .fill(3)(munitCommonJsModule)
        .map { linkedJs =>
          ScalaJsTestRunner.runTestsViaAdapter(
            linkedJs,
            ScalaJsLinkConfig.ModuleKind.CommonJSModule,
            List(TestSuite("ArithmeticSuite", suiteClassName)),
            TestFramework.MUnit,
            new RecordingHandler(),
            nodeBinary,
            Map.empty,
            DefaultScalaJsVersion,
            killSignal
          )
        }
        .parSequence
    } yield results).unsafeRunSync()

    results.foreach { result =>
      result.terminationReason shouldBe TestRunnerTypes.TerminationReason.Completed
      result.passed shouldBe 1
      result.failed shouldBe 1
    }
  }

  test("CompilerResolver.getScalaJsTestAdapter: loads the adapter and the Node.js environment") {
    val loader = CompilerResolver.getScalaJsTestAdapter(DefaultScalaJsVersion).loader

    loader.loadClass("org.scalajs.testing.adapter.TestAdapter").getName shouldBe "org.scalajs.testing.adapter.TestAdapter"
    loader.loadClass("org.scalajs.jsenv.nodejs.NodeJSEnv").getName shouldBe "org.scalajs.jsenv.nodejs.NodeJSEnv"
  }

  /** `CompilerTopLoader` hands `sbt.testing.*` to bleep's own classloader. A `Framework` the isolated loader returns is therefore assignable to bleep's
    * `sbt.testing.Framework`. Narrowing that delegation would break the runner with a `ClassCastException` at run time. This assertion fails at test time
    * instead of at run time.
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
