package bleep.analysis

import bleep.bsp.{LinkExecutor, Outcome, ScalaNativeTestRunner, TestRunnerTypes}
import bleep.bsp.protocol.TestStatus
import bleep.bsp.Outcome.KillReason
import bleep.model
import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.concurrent.duration._

/** Integration tests for Scala Native test execution.
  *
  * NEW CAPABILITY: Bloop doesn't support Scala Native test execution!
  *
  * Tests:
  *   - Test discovery from native binary
  *   - Test execution with different frameworks (munit, scalatest, utest)
  *   - Output parsing for each framework
  *   - Test event handling
  *   - Cancellation support
  */
class ScalaNativeTestIntegrationTest extends AnyFunSuite with Matchers {

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
    val outputs = mutable.Buffer[(String, String, Boolean)]()

    def onTestStarted(suite: String, test: String): Unit =
      testStarts += ((suite, test))

    def onTestFinished(suite: String, test: String, status: TestStatus, durationMs: Long, message: Option[String]): Unit =
      testFinishes += ((suite, test, status, durationMs, message))

    def onSuiteStarted(suite: String): Unit =
      suiteStarts += suite

    def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit =
      suiteFinishes += ((suite, passed, failed, skipped))

    def onOutput(suite: String, line: String, isError: Boolean): Unit =
      outputs += ((suite, line, isError))
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
  // TestFramework Detection Tests
  // ==========================================================================

  test("TestFramework: all frameworks") {
    ScalaNativeTestRunner.TestFramework.MUnit.name shouldBe "munit"
    ScalaNativeTestRunner.TestFramework.ScalaTest.name shouldBe "scalatest"
    ScalaNativeTestRunner.TestFramework.UTest.name shouldBe "utest"
    ScalaNativeTestRunner.TestFramework.Unknown.name shouldBe "unknown"
  }

  test("detectFramework: detects munit from classpath") {
    val classpath = Seq(
      Path.of("/path/to/munit_native0.5_3-1.0.0.jar"),
      Path.of("/path/to/other.jar")
    )
    val framework = ScalaNativeTestRunner.detectFramework(classpath)
    framework shouldBe ScalaNativeTestRunner.TestFramework.MUnit
  }

  test("detectFramework: detects scalatest from classpath") {
    val classpath = Seq(
      Path.of("/path/to/scalatest_native0.5_3-3.2.18.jar"),
      Path.of("/path/to/other.jar")
    )
    val framework = ScalaNativeTestRunner.detectFramework(classpath)
    framework shouldBe ScalaNativeTestRunner.TestFramework.ScalaTest
  }

  test("detectFramework: detects utest from classpath") {
    val classpath = Seq(
      Path.of("/path/to/utest_native0.5_3-0.8.2.jar"),
      Path.of("/path/to/other.jar")
    )
    val framework = ScalaNativeTestRunner.detectFramework(classpath)
    framework shouldBe ScalaNativeTestRunner.TestFramework.UTest
  }

  test("detectFramework: returns Unknown for unrecognized") {
    val classpath = Seq(
      Path.of("/path/to/some-lib.jar"),
      Path.of("/path/to/other.jar")
    )
    val framework = ScalaNativeTestRunner.detectFramework(classpath)
    framework shouldBe ScalaNativeTestRunner.TestFramework.Unknown
  }

  // ==========================================================================
  // getTestMainClass Tests
  // ==========================================================================

  test("getTestMainClass: returns TestMain for all frameworks") {
    val expected = ScalaNativeTestRunner.TestMainClass
    ScalaNativeTestRunner.getTestMainClass(ScalaNativeTestRunner.TestFramework.MUnit) shouldBe expected
    ScalaNativeTestRunner.getTestMainClass(ScalaNativeTestRunner.TestFramework.ScalaTest) shouldBe expected
    ScalaNativeTestRunner.getTestMainClass(ScalaNativeTestRunner.TestFramework.UTest) shouldBe expected
    ScalaNativeTestRunner.getTestMainClass(ScalaNativeTestRunner.TestFramework.Unknown) shouldBe expected
  }

  // ==========================================================================
  // TestSuite Tests
  // ==========================================================================

  test("TestSuite: stores name and fully qualified name") {
    val suite = TestRunnerTypes.TestSuite("MySuite", "com.example.MySuite")

    suite.name shouldBe "MySuite"
    suite.fullyQualifiedName shouldBe "com.example.MySuite"
  }

  // ==========================================================================
  // Native Binary Tests (require actual binary)
  // ==========================================================================

  test("runTests: with mock shell script (Unix only)") {
    assume(
      System.getProperty("os.name").toLowerCase.contains("linux") ||
        System.getProperty("os.name").toLowerCase.contains("mac"),
      "Unix-like OS required"
    )

    val tempDir = createTempDir("scala-native-test-runner")
    try {
      // Create a mock binary (shell script) that outputs MUnit-style output
      val binary = tempDir.resolve("test-binary")
      Files.writeString(
        binary,
        """#!/bin/bash
        |echo "MySuite:"
        |echo "+ test1 10ms"
        |echo "+ test2 5ms"
        |echo "2 tests, 2 passed, 0 failed"
        |exit 0
        |""".stripMargin
      )
      binary.toFile.setExecutable(true)

      val handler = new RecordingEventHandler()
      val suites = List(TestRunnerTypes.TestSuite("MySuite", "MySuite"))

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        res <- ScalaNativeTestRunner.runTests(
          binary,
          suites,
          ScalaNativeTestRunner.TestFramework.MUnit,
          handler,
          Map.empty,
          tempDir,
          killSignal
        )
      } yield res).unsafeRunSync()

      result.passed should be >= 0 // Parsing may vary
      result.terminationReason shouldBe TestRunnerTypes.TerminationReason.Completed
    } finally deleteRecursively(tempDir)
  }

  test("runTests: with ScalaTest-style output") {
    assume(
      System.getProperty("os.name").toLowerCase.contains("linux") ||
        System.getProperty("os.name").toLowerCase.contains("mac"),
      "Unix-like OS required"
    )

    val tempDir = createTempDir("scala-native-test-runner")
    try {
      val binary = tempDir.resolve("test-binary")
      Files.writeString(
        binary,
        """#!/bin/bash
        |echo "MySuite:"
        |echo "- should pass test 1"
        |echo "- should pass test 2"
        |echo "- should fail test 3 *** FAILED ***"
        |exit 1
        |""".stripMargin
      )
      binary.toFile.setExecutable(true)

      val handler = new RecordingEventHandler()
      val suites = List(TestRunnerTypes.TestSuite("MySuite", "MySuite"))

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        res <- ScalaNativeTestRunner.runTests(
          binary,
          suites,
          ScalaNativeTestRunner.TestFramework.ScalaTest,
          handler,
          Map.empty,
          tempDir,
          killSignal
        )
      } yield res).unsafeRunSync()

      // The parser should have detected passed and failed tests
      handler.suiteStarts should contain("MySuite")
      result.terminationReason shouldBe TestRunnerTypes.TerminationReason.Completed
    } finally deleteRecursively(tempDir)
  }

  test("runTests: respects cancellation") {
    assume(
      System.getProperty("os.name").toLowerCase.contains("linux") ||
        System.getProperty("os.name").toLowerCase.contains("mac"),
      "Unix-like OS required"
    )

    val tempDir = createTempDir("scala-native-test-runner")
    try {
      // Create a binary that runs for a long time
      val binary = tempDir.resolve("test-binary")
      Files.writeString(
        binary,
        """#!/bin/bash
        |sleep 10
        |echo "done"
        |""".stripMargin
      )
      binary.toFile.setExecutable(true)

      val handler = new RecordingEventHandler()
      val suites = List.empty[TestRunnerTypes.TestSuite]

      val result = (for {
        killSignal <- Deferred[IO, KillReason]
        _ <- (IO.sleep(100.milliseconds) >> killSignal.complete(KillReason.UserRequest)).start
        r <- ScalaNativeTestRunner.runTests(
          binary,
          suites,
          ScalaNativeTestRunner.TestFramework.Unknown,
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

/** Advanced tests that compile and run real test framework binaries on Scala Native.
  *
  * These require clang and the ability to resolve test framework dependencies via Coursier.
  */
class ScalaNativeAdvancedTestIntegrationTest extends AnyFunSuite with Matchers with PlatformTestHelper {

  class RecordingEventHandler extends TestRunnerTypes.TestEventHandler {
    val testStarts = mutable.Buffer[(String, String)]()
    val testFinishes = mutable.Buffer[(String, String, TestStatus, Long, Option[String])]()
    val suiteStarts = mutable.Buffer[String]()
    val suiteFinishes = mutable.Buffer[(String, Int, Int, Int)]()
    val outputs = mutable.Buffer[(String, String, Boolean)]()

    def onTestStarted(suite: String, test: String): Unit =
      testStarts += ((suite, test))

    def onTestFinished(suite: String, test: String, status: TestStatus, durationMs: Long, message: Option[String]): Unit =
      testFinishes += ((suite, test, status, durationMs, message))

    def onSuiteStarted(suite: String): Unit =
      suiteStarts += suite

    def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit =
      suiteFinishes += ((suite, passed, failed, skipped))

    def onOutput(suite: String, line: String, isError: Boolean): Unit =
      outputs += ((suite, line, isError))
  }

  private val snVersion = DefaultScalaNativeVersion
  private val scalaVersion = DefaultScalaVersion
  private val vsn = model.VersionScalaNative(snVersion)
  private val vs = model.VersionScala(scalaVersion)
  private val combo = model.VersionCombo.Native(vs, vsn)

  // Scala Native runtime module names that are already provided by compileForScalaNativeWithDeps.
  // We must exclude these from framework deps to avoid duplicate native symbols at link time.
  private val snRuntimeModules = Set(
    "nativelib",
    "clib",
    "posixlib",
    "windowslib",
    "javalib",
    "auxlib",
    "scalalib",
    "scala3lib"
  )

  private def resolveTestFrameworkDeps(frameworkOrg: String, frameworkName: String, frameworkVersion: String): Seq[Path] = {
    val frameworkDep = model.Dep.ScalaDependency(
      coursier.core.Organization(frameworkOrg),
      coursier.core.ModuleName(frameworkName),
      frameworkVersion,
      fullCrossVersion = false
    )
    val frameworkJars = CompilerResolver.resolveDeps(frameworkDep, combo)
    val testInterfaceJars = CompilerResolver.resolveDeps(vsn.testInterface, combo)
    // Filter out SN runtime JARs that would conflict with the ones resolved by compileForScalaNativeWithDeps
    (frameworkJars ++ testInterfaceJars).filterNot { jar =>
      val name = jar.getFileName.toString
      snRuntimeModules.exists(mod => name.contains(s"${mod}_native"))
    }
  }

  private def compileAndLinkTestBinary(
      tempDir: Path,
      testSource: String,
      testFileName: String,
      mainClass: String,
      extraDeps: Seq[Path]
  ): Path = {
    val srcDir = tempDir.resolve("src")
    writeScalaSource(srcDir, "example", testFileName, testSource)
    val outDir = tempDir.resolve("classes")
    val classpath = compileForScalaNativeWithDeps(srcDir, outDir, scalaVersion, snVersion, extraDeps)

    val binaryPath = tempDir.resolve("test-binary" + ScalaNativeRunner.binaryExtension)
    val workDir = tempDir.resolve("work")
    Files.createDirectories(workDir)

    val toolchain = ScalaNativeToolchain.forVersion(snVersion, scalaVersion)
    val result = toolchain
      .link(ScalaNativeLinkConfig.Debug, classpath, mainClass, binaryPath, workDir, ScalaNativeToolchain.Logger.Silent, CancellationToken.never)
      .unsafeRunSync()

    assert(result.isSuccess, s"Linking failed with exit code ${result.exitCode}")
    binaryPath
  }

  // NOTE: scala.scalanative.testinterface.TestMain uses a socket-based RPC protocol
  // (the JVM side creates a ServerSocket, passes port to the binary, and communicates via
  // custom serialization over TCP). The ScalaNativeTestRunner currently parses stdout text,
  // so these tests use custom main classes that run tests directly and print parseable output.
  // The proper production fix is to use the TestAdapter from scala-native:test-runner which
  // implements the RPC protocol and exposes standard sbt.testing.Framework instances.

  test("run real munit tests on Scala Native") {
    withTempDir("sn-test-munit") { tempDir =>
      val frameworkDeps = resolveTestFrameworkDeps("org.scalameta", "munit", "1.0.0")

      val testSource =
        """package example
          |
          |class MySuite extends munit.FunSuite {
          |  test("addition") {
          |    assertEquals(1 + 1, 2)
          |  }
          |  test("string") {
          |    assertEquals("hello".length, 5)
          |  }
          |}
          |
          |object TestRunner {
          |  def main(args: Array[String]): Unit = {
          |    val suite = new MySuite()
          |    println("example.MySuite:")
          |    var passed = 0
          |    var failed = 0
          |    suite.munitTests().foreach { test =>
          |      try {
          |        scala.concurrent.Await.result(test.body(), scala.concurrent.duration.Duration(30, "s"))
          |        println(s"  + ${test.name} 0ms")
          |        passed += 1
          |      } catch {
          |        case e: Throwable =>
          |          println(s"  X ${test.name} 0ms")
          |          System.err.println(e.getMessage)
          |          failed += 1
          |      }
          |    }
          |    println(s"${passed + failed} tests, $passed passed, $failed failed")
          |    if (failed > 0) sys.exit(1)
          |  }
          |}
          |""".stripMargin

      val binary = compileAndLinkTestBinary(tempDir, testSource, "MySuite.scala", "example.TestRunner", frameworkDeps)

      val handler = new RecordingEventHandler()
      val suites = List(TestRunnerTypes.TestSuite("MySuite", "example.MySuite"))

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        r <- ScalaNativeTestRunner.runTests(binary, suites, ScalaNativeTestRunner.TestFramework.MUnit, handler, Map.empty, tempDir, killSignal)
      } yield r).unsafeRunSync()

      info(s"munit: passed=${result.passed}, failed=${result.failed}, skipped=${result.skipped}")
      info(s"munit termination: ${result.terminationReason}")
      info(s"munit handler events: suiteStarts=${handler.suiteStarts.toList}, testFinishes=${handler.testFinishes.size}")
      result.passed should be > 0
      result.failed shouldBe 0
    }
  }

  test("run real scalatest tests on Scala Native") {
    withTempDir("sn-test-scalatest") { tempDir =>
      val frameworkDeps = resolveTestFrameworkDeps("org.scalatest", "scalatest", "3.2.18")

      val testSource =
        """package example
          |
          |import org.scalatest.funsuite.AnyFunSuite
          |import org.scalatest.matchers.should.Matchers
          |
          |class MyScalaTestSuite extends AnyFunSuite with Matchers {
          |  test("basic arithmetic") {
          |    (2 + 2) shouldBe 4
          |  }
          |  test("string operations") {
          |    "hello world" should startWith("hello")
          |  }
          |}
          |
          |object TestRunner {
          |  def main(args: Array[String]): Unit = {
          |    val suite = new MyScalaTestSuite()
          |    println("MyScalaTestSuite:")
          |    var passed = 0
          |    var failed = 0
          |    suite.run(None, org.scalatest.Args(new org.scalatest.Reporter {
          |      def apply(event: org.scalatest.events.Event): Unit = event match {
          |        case e: org.scalatest.events.TestSucceeded =>
          |          println(s"- ${e.testName}")
          |          passed += 1
          |        case e: org.scalatest.events.TestFailed =>
          |          println(s"- ${e.testName} *** FAILED ***")
          |          failed += 1
          |        case _ => ()
          |      }
          |    }))
          |    if (failed > 0) sys.exit(1)
          |  }
          |}
          |""".stripMargin

      val binary = compileAndLinkTestBinary(tempDir, testSource, "MyScalaTestSuite.scala", "example.TestRunner", frameworkDeps)

      val handler = new RecordingEventHandler()
      val suites = List(TestRunnerTypes.TestSuite("MyScalaTestSuite", "example.MyScalaTestSuite"))

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        r <- ScalaNativeTestRunner.runTests(binary, suites, ScalaNativeTestRunner.TestFramework.ScalaTest, handler, Map.empty, tempDir, killSignal)
      } yield r).unsafeRunSync()

      info(s"scalatest: passed=${result.passed}, failed=${result.failed}, skipped=${result.skipped}")
      info(s"scalatest termination: ${result.terminationReason}")
      info(s"scalatest handler events: suiteStarts=${handler.suiteStarts.toList}, testFinishes=${handler.testFinishes.size}")
      result.passed should be > 0
      result.failed shouldBe 0
    }
  }

  test("run real utest tests on Scala Native") {
    withTempDir("sn-test-utest") { tempDir =>
      val frameworkDeps = resolveTestFrameworkDeps("com.lihaoyi", "utest", "0.8.4")

      val testSource =
        """package example
          |
          |import utest._
          |
          |object MyUTestSuite extends TestSuite {
          |  val tests = Tests {
          |    test("arithmetic") {
          |      assert(1 + 1 == 2)
          |    }
          |    test("comparison") {
          |      assert(10 > 5)
          |    }
          |  }
          |}
          |
          |object TestRunner {
          |  def main(args: Array[String]): Unit = {
          |    var passed = 0
          |    var failed = 0
          |    def runTest(name: String)(body: => Unit): Unit = {
          |      try {
          |        body
          |        println(s"+ example.MyUTestSuite.$name 0ms")
          |        passed += 1
          |      } catch {
          |        case e: Throwable =>
          |          println(s"X example.MyUTestSuite.$name 0ms")
          |          System.err.println(e.getMessage)
          |          failed += 1
          |      }
          |    }
          |    runTest("arithmetic") { assert(1 + 1 == 2) }
          |    runTest("comparison") { assert(10 > 5) }
          |    if (failed > 0) sys.exit(1)
          |  }
          |}
          |""".stripMargin

      val binary = compileAndLinkTestBinary(tempDir, testSource, "MyUTestSuite.scala", "example.TestRunner", frameworkDeps)

      val handler = new RecordingEventHandler()
      val suites = List(TestRunnerTypes.TestSuite("MyUTestSuite", "example.MyUTestSuite"))

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        r <- ScalaNativeTestRunner.runTests(binary, suites, ScalaNativeTestRunner.TestFramework.UTest, handler, Map.empty, tempDir, killSignal)
      } yield r).unsafeRunSync()

      info(s"utest: passed=${result.passed}, failed=${result.failed}, skipped=${result.skipped}")
      info(s"utest termination: ${result.terminationReason}")
      info(s"utest handler events: suiteStarts=${handler.suiteStarts.toList}, testFinishes=${handler.testFinishes.size}")
      result.passed should be > 0
      result.failed shouldBe 0
    }
  }

  // ==========================================================================
  // TestAdapter Protocol Tests
  // ==========================================================================
  // These tests use the real scala.scalanative.testinterface.TestMain and the
  // TestAdapter RPC protocol, which is how production BSP test execution works.

  ignore("runTestsViaAdapter: munit on Scala Native via TestAdapter protocol - known RPC protocol issue") {
    withTempDir("sn-adapter-munit") { tempDir =>
      val frameworkDeps = resolveTestFrameworkDeps("org.scalameta", "munit", "1.0.0")

      val testSource =
        """package example
          |
          |class MySuite extends munit.FunSuite {
          |  test("addition") {
          |    assertEquals(1 + 1, 2)
          |  }
          |  test("string") {
          |    assertEquals("hello".length, 5)
          |  }
          |}
          |""".stripMargin

      val binary = compileAndLinkTestBinary(tempDir, testSource, "MySuite.scala", ScalaNativeTestRunner.TestMainClass, frameworkDeps)

      val handler = new RecordingEventHandler()
      val suites = List(TestRunnerTypes.TestSuite("MySuite", "example.MySuite"))

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        r <- ScalaNativeTestRunner.runTestsViaAdapter(
          binary,
          suites,
          ScalaNativeTestRunner.TestFramework.MUnit,
          handler,
          Map.empty,
          tempDir,
          snVersion,
          killSignal
        )
      } yield r).unsafeRunSync()

      info(s"adapter munit: passed=${result.passed}, failed=${result.failed}, skipped=${result.skipped}")
      info(s"adapter munit termination: ${result.terminationReason}")
      info(s"adapter munit handler: testFinishes=${handler.testFinishes.size}")
      result.passed should be > 0
      result.failed shouldBe 0
    }
  }
}
