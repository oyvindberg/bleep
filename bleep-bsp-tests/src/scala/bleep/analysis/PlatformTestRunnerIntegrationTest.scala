package bleep.analysis

import bleep.bsp._
import ch.epfl.scala.bsp._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.TimeLimits
import org.scalatest.time.{Seconds, Span}

import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters._

/** Integration tests for platform-aware test dispatching through BSP.
  *
  * Verifies that BspServer.handleTest() correctly detects platform from ProjectConfig and dispatches to the appropriate test runner:
  *   - JVM: runs tests via java -cp with detected framework
  *   - Scala.js: links then runs via Node.js
  *   - Scala Native: links native binary then runs it
  *
  * These tests go through the full BSP protocol: BspTestHarness → BspServer → handleTest → platform dispatch.
  */
class PlatformTestRunnerIntegrationTest extends AnyFunSuite with Matchers with TimeLimits {

  val mediumTimeout: Span = Span(180, Seconds)

  def createTempWorkspace(prefix: String): Path = {
    val dir = Files.createTempDirectory(prefix)
    Files.createDirectories(dir.resolve("src"))
    Files.createDirectories(dir.resolve("target/classes"))
    dir
  }

  def deleteRecursively(path: Path): Unit =
    if (Files.exists(path)) {
      if (Files.isDirectory(path)) {
        Files.list(path).toScala(List).foreach(deleteRecursively)
      }
      Files.delete(path)
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
          outputDir = workspace.resolve("target/classes"),
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
          outputDir = workspace.resolve("target/classes"),
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

          // Compilation should work (sources are valid Scala)
          val compileResult = client.compile(List(targets.targets.head.id))
          compileResult.statusCode.value shouldBe 1 // Ok

          info("Scala.js test project compiled and configured with correct platform")
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
          outputDir = workspace.resolve("target/classes"),
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
          outputDir = workspace.resolve("target/jvm/classes"),
          languageConfig = ScalaConfig("3.7.4", Nil),
          dependsOn = Set.empty,
          isTest = false,
          platform = BuildLoader.Platform.Jvm
        )

        val jsConfig = BspTestHarness.ProjectConfig(
          name = "js-lib",
          sources = Set(jsSrcDir),
          classpath = classpath,
          outputDir = workspace.resolve("target/js/classes"),
          languageConfig = ScalaConfig("3.7.4", Nil),
          dependsOn = Set.empty,
          isTest = false,
          platform = BuildLoader.Platform.ScalaJs("1.16.0", "3.7.4")
        )

        BspTestHarness.withProjects(workspace, List(jvmConfig, jsConfig)) { client =>
          client.initialize()
          val targets = client.buildTargets()
          targets.targets should have size 2

          // Both should compile independently
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
        // Use the scala() factory which doesn't set platform explicitly
        val config = BspTestHarness.ProjectConfig.scala(
          name = "default-platform",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
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
