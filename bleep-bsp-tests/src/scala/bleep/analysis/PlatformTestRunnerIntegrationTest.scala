package bleep.analysis

import bleep.bsp.BuildLoader
import bleep.bsp.protocol.BleepBspProtocol
import org.scalatest.concurrent.TimeLimits
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** These tests assert that `BspServer.handleTest` picks a test runner from the platform a `ProjectConfig` declares.
  *
  * A JVM project runs its tests under `java -cp` with the framework the classpath declares. A Scala.js project links first. Node then runs the linked
  * JavaScript. A Scala Native project links a binary first. The binary then runs.
  *
  * Each test travels the whole BSP protocol. A request enters `BspTestHarness`, reaches `BspServer`, reaches `handleTest`, and reaches one test runner.
  */
class PlatformTestRunnerIntegrationTest extends AnyFunSuite with Matchers with TimeLimits with PlatformTestHelper {

  val mediumTimeout: Span = Span(180, Seconds)

  def createTempWorkspace(prefix: String): Path = {
    val dir = Files.createTempDirectory(prefix)
    Files.createDirectories(dir.resolve("src"))
    Files.createDirectories(dir.resolve("target/classes"))
    dir
  }

  // ============================================================================
  // Platform Detection Tests
  // ============================================================================

  test("BSP: JVM test project compiles and is recognized as test target") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-jvm-test-platform-")
      try {
        val srcFile = workspace.resolve("src/MyTest.scala")
        Files.writeString(
          srcFile,
          """package example
            |
            |class MyTest {
            |  def testBasic(): Unit = {
            |    assert(1 + 1 == 2)
            |  }
            |}
            |""".stripMargin
        )

        val classpath = CompilerResolver.resolveScalaLibrary("3.7.4").toList
        val config = BspTestHarness.ProjectConfig(
          name = "jvm-test-project",
          sources = Set(workspace.resolve("src")),
          classpath = classpath,
          languageConfig = ScalaConfig("3.7.4", Nil),
          dependsOn = Set.empty,
          isTest = true,
          platform = BuildLoader.Platform.Jvm
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()
          val targets = client.buildTargets()
          targets.targets should have size 1

          val target = targets.targets.head
          target.tags should contain("test")

          val compileResult = client.compile(List(target.id))
          compileResult.statusCode.value shouldBe 1 // Ok

          info("JVM test project compiled successfully via BSP")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BSP: Scala.js test project configured with ScalaJs platform") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-scalajs-test-platform-")
      try {
        val srcFile = workspace.resolve("src/JsTest.scala")
        Files.writeString(
          srcFile,
          """package example
            |
            |class JsTest {
            |  def testBasic(): Unit = {
            |    assert(1 + 1 == 2)
            |  }
            |}
            |""".stripMargin
        )

        val classpath = CompilerResolver.resolveScalaLibrary("3.7.4").toList
        val config = BspTestHarness.ProjectConfig.scalaJs(
          name = "scalajs-test-project",
          sources = Set(workspace.resolve("src")),
          scalaVersion = "3.7.4",
          sjsVersion = "1.16.0",
          classpath = classpath,
          isTest = true
        )

        config.platform shouldBe a[BuildLoader.Platform.ScalaJs]
        val sjsPlatform = config.platform.asInstanceOf[BuildLoader.Platform.ScalaJs]
        sjsPlatform.sjsVersion shouldBe "1.16.0"
        sjsPlatform.scalaVersion shouldBe "3.7.4"

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()
          val targets = client.buildTargets()
          targets.targets should have size 1
          targets.targets.head.tags should contain("test")

          // The sources are valid Scala. The compile succeeds.
          val compileResult = client.compile(List(targets.targets.head.id))
          compileResult.statusCode.value shouldBe 1 // Ok

          info("Scala.js test project compiled and configured with correct platform")
        }
      } finally deleteRecursively(workspace)
    }
  }

  /** Node sets `process.versions.node` to the Node version running this suite. A JVM test runner never links `scala.scalajs.js`. */
  private val platformSuiteSource =
    """package example
      |
      |import scala.scalajs.js
      |
      |class PlatformSuite extends munit.FunSuite {
      |  test("the suite runs under node") {
      |    val nodeVersion = js.Dynamic.global.process.versions.node.asInstanceOf[String]
      |    assert(nodeVersion.nonEmpty, "process.versions.node was empty")
      |  }
      |}
      |""".stripMargin

  test("BSP: Scala.js munit suite runs through the test adapter") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-scalajs-adapter-")
      try {
        Files.writeString(workspace.resolve("src/PlatformSuite.scala"), platformSuiteSource)

        val classpath =
          (CompilerResolver.resolveScalaLibrary(DefaultScalaVersion) ++
            CompilerTestLibraries.munitScalaJsLibrary ++
            CompilerTestLibraries.scalaJsTestBridgeLibrary(DefaultScalaJsVersion)).distinct.toList

        val config = BspTestHarness.ProjectConfig.scalaJs(
          name = "scalajs-adapter-suite",
          sources = Set(workspace.resolve("src")),
          scalaVersion = DefaultScalaVersion,
          sjsVersion = DefaultScalaJsVersion,
          classpath = classpath,
          isTest = true
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()
          val targets = client.buildTargets()
          targets.targets should have size 1

          val testResult = client.test(targets.targets.map(_.id))

          withClue(client.events.mkString("\n")) {
            testResult.statusCode.value shouldBe 1

            // The status code alone would also be Ok for a run that discovered no suite at all. The counts say what actually ran.
            val runResult = BleepBspProtocol.TestRunResult
              .decode(new String(testResult.data.get.value, StandardCharsets.UTF_8))
              .fold(error => fail(s"Could not read the test run result: $error"), identity)

            runResult.suitesTotal shouldBe 1
            runResult.suitesCompleted shouldBe 1
            runResult.totalPassed shouldBe 1
            runResult.totalFailed shouldBe 0
          }
        }
      } finally deleteRecursively(workspace)
    }
  }

  /** Three suites in one file. One compile and one link produce the module for all three suites. Each suite extracts `process.versions.node` the way
    * [[platformSuiteSource]] extracts it.
    */
  private val threeSuiteSource =
    """package example
      |
      |import scala.scalajs.js
      |
      |object NodeVersion {
      |  def read(): String = js.Dynamic.global.process.versions.node.asInstanceOf[String]
      |}
      |
      |class AlphaSuite extends munit.FunSuite {
      |  test("alpha runs under node") {
      |    assert(NodeVersion.read().nonEmpty, "process.versions.node was empty")
      |  }
      |}
      |
      |class BetaSuite extends munit.FunSuite {
      |  test("beta runs under node") {
      |    assert(NodeVersion.read().nonEmpty, "process.versions.node was empty")
      |  }
      |}
      |
      |class GammaSuite extends munit.FunSuite {
      |  test("gamma runs under node") {
      |    assert(NodeVersion.read().nonEmpty, "process.versions.node was empty")
      |  }
      |}
      |""".stripMargin

  /** Returns the last path segment of every directory under `root`. */
  private def directoryNamesUnder(root: Path): List[String] = {
    val stream = Files.walk(root)
    try stream.iterator().asScala.filter(Files.isDirectory(_)).map(_.getFileName.toString).toList
    finally stream.close()
  }

  /** The DAG runs one `LinkTask` per test project. A suite task that linked for itself would write a `link-output` directory beside the DAG's own output. Three
    * suite tasks running at once would write that directory three times over.
    */
  test("BSP: three Scala.js suites in one project link once") {
    failAfter(Span(360, Seconds)) {
      val workspace = createTempWorkspace("bsp-scalajs-one-link-")
      try {
        Files.writeString(workspace.resolve("src/ThreeSuites.scala"), threeSuiteSource)

        val classpath =
          (CompilerResolver.resolveScalaLibrary(DefaultScalaVersion) ++
            CompilerTestLibraries.munitScalaJsLibrary ++
            CompilerTestLibraries.scalaJsTestBridgeLibrary(DefaultScalaJsVersion)).distinct.toList

        val config = BspTestHarness.ProjectConfig.scalaJs(
          name = "scalajs-one-link",
          sources = Set(workspace.resolve("src")),
          scalaVersion = DefaultScalaVersion,
          sjsVersion = DefaultScalaJsVersion,
          classpath = classpath,
          isTest = true
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()
          val targets = client.buildTargets()

          // `BspClient.test` waits 120 seconds. This run compiles three Scala.js suites, links the three suites, and starts three node processes. That work
          // approaches 120 seconds on an unloaded machine. That work passes 120 seconds while the rest of this class runs alongside. This async request takes
          // a timeout of its own rather than raising the timeout every other test shares. 300 seconds still fails long before a stalled run would.
          val startedAt = System.currentTimeMillis()
          val testResult = client
            .testAsync(targets.targets.map(_.id))
            .awaitWithTimeout(300000)
            .getOrElse(fail(s"buildTarget/test did not reply. Events:\n${client.events.mkString("\n")}"))
          info(s"buildTarget/test replied after ${System.currentTimeMillis() - startedAt}ms")

          withClue(client.events.mkString("\n")) {
            val runResult = BleepBspProtocol.TestRunResult
              .decode(new String(testResult.data.get.value, StandardCharsets.UTF_8))
              .fold(error => fail(s"Could not read the test run result: $error"), identity)

            runResult.suitesTotal shouldBe 3
            runResult.suitesCompleted shouldBe 3
            runResult.totalPassed shouldBe 3
            runResult.totalFailed shouldBe 0
          }

          withClue(directoryNamesUnder(workspace).mkString("\n")) {
            directoryNamesUnder(workspace) should not contain "link-output"
          }
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BSP: Scala Native test project configured with ScalaNative platform") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-scalanative-test-platform-")
      try {
        val srcFile = workspace.resolve("src/NativeTest.scala")
        Files.writeString(
          srcFile,
          """package example
            |
            |class NativeTest {
            |  def testBasic(): Unit = {
            |    assert(1 + 1 == 2)
            |  }
            |}
            |""".stripMargin
        )

        val classpath = CompilerResolver.resolveScalaLibrary("3.7.4").toList
        val config = BspTestHarness.ProjectConfig.scalaNative(
          name = "scalanative-test-project",
          sources = Set(workspace.resolve("src")),
          scalaVersion = "3.7.4",
          snVersion = "0.5.6",
          classpath = classpath,
          isTest = true
        )

        config.platform shouldBe a[BuildLoader.Platform.ScalaNative]
        val snPlatform = config.platform.asInstanceOf[BuildLoader.Platform.ScalaNative]
        snPlatform.snVersion shouldBe "0.5.6"
        snPlatform.scalaVersion shouldBe "3.7.4"

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()
          val targets = client.buildTargets()
          targets.targets should have size 1
          targets.targets.head.tags should contain("test")

          val compileResult = client.compile(List(targets.targets.head.id))
          compileResult.statusCode.value shouldBe 1 // Ok

          info("Scala Native test project compiled and configured with correct platform")
        }
      } finally deleteRecursively(workspace)
    }
  }

  // ============================================================================
  // Multi-Project Platform Tests
  // ============================================================================

  test("BSP: mixed JVM and Scala.js projects coexist") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-mixed-platform-")
      try {
        // JVM source
        val jvmSrcDir = workspace.resolve("jvm-src")
        Files.createDirectories(jvmSrcDir)
        Files.writeString(
          jvmSrcDir.resolve("JvmLib.scala"),
          """package example
            |object JvmLib {
            |  def greet: String = "Hello from JVM"
            |}
            |""".stripMargin
        )

        // JS source
        val jsSrcDir = workspace.resolve("js-src")
        Files.createDirectories(jsSrcDir)
        Files.writeString(
          jsSrcDir.resolve("JsLib.scala"),
          """package example
            |object JsLib {
            |  def greet: String = "Hello from JS"
            |}
            |""".stripMargin
        )

        val classpath = CompilerResolver.resolveScalaLibrary("3.7.4").toList

        val jvmConfig = BspTestHarness.ProjectConfig(
          name = "jvm-lib",
          sources = Set(jvmSrcDir),
          classpath = classpath,
          languageConfig = ScalaConfig("3.7.4", Nil),
          dependsOn = Set.empty,
          isTest = false,
          platform = BuildLoader.Platform.Jvm
        )

        val jsConfig = BspTestHarness.ProjectConfig(
          name = "js-lib",
          sources = Set(jsSrcDir),
          classpath = classpath,
          languageConfig = ScalaConfig("3.7.4", Nil),
          dependsOn = Set.empty,
          isTest = false,
          platform = BuildLoader.Platform.ScalaJs("1.16.0", "3.7.4")
        )

        BspTestHarness.withProjects(workspace, List(jvmConfig, jsConfig)) { client =>
          client.initialize()
          val targets = client.buildTargets()
          targets.targets should have size 2

          val compileResult = client.compile(targets.targets.map(_.id))
          compileResult.statusCode.value shouldBe 1 // Ok

          info("Mixed JVM and Scala.js projects compiled successfully")
        }
      } finally deleteRecursively(workspace)
    }
  }

  // ============================================================================
  // Platform Default Tests
  // ============================================================================

  test("BSP: default platform is JVM when not specified") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-default-platform-")
      try {
        Files.writeString(
          workspace.resolve("src/Lib.scala"),
          """package example
            |object Lib { def x: Int = 42 }
            |""".stripMargin
        )

        val classpath = CompilerResolver.resolveScalaLibrary("3.7.4").toList
        val config = BspTestHarness.ProjectConfig.scala(
          name = "default-platform",
          sources = Set(workspace.resolve("src")),
          scalaVersion = "3.7.4",
          classpath = classpath,
          isTest = false
        )

        config.platform shouldBe BuildLoader.Platform.Jvm

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()
          val targets = client.buildTargets()
          targets.targets should have size 1

          val compileResult = client.compile(List(targets.targets.head.id))
          compileResult.statusCode.value shouldBe 1

          info("Default platform is JVM as expected")
        }
      } finally deleteRecursively(workspace)
    }
  }
}
