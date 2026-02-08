package bleep.analysis

import bleep.bsp._
import ch.epfl.scala.bsp._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.TimeLimits
import org.scalatest.time.{Seconds, Span}

import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters._

/** Integration tests for running main classes through BSP protocol.
  *
  * Tests compile + run scenarios for Java, Scala, and Kotlin by:
  *   1. Writing source files with main methods
  *   2. Compiling via BSP
  *   3. Running via buildTarget/run
  *   4. Verifying output through log messages
  *
  * These tests mirror RunAndTestIntegrationTest but go through the full BSP protocol.
  */
class BspRunIntegrationTest extends AnyFunSuite with Matchers with TimeLimits {

  val mediumTimeout: Span = Span(120, Seconds)

  // ============================================================================
  // Helper Methods
  // ============================================================================

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

  def scalaLibraryClasspath(version: String): List[Path] =
    CompilerResolver.resolveScalaLibrary(version).toList

  def kotlinLibraryClasspath(version: String): List[Path] =
    CompilerResolver.resolveKotlinLibrary(version).toList

  // ============================================================================
  // Scala Run Tests via BSP
  // ============================================================================

  test("BSP Scala: compile and run main class") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-scala-run-")
      try {
        val srcFile = workspace.resolve("src/Main.scala")
        Files.writeString(
          srcFile,
          """object Main {
            |  def main(args: Array[String]): Unit = {
            |    val message = if (args.isEmpty) "Hello, World!" else args.mkString(" ")
            |    println(message)
            |    println(s"Args count: ${args.length}")
            |  }
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig.scala(
          name = "scalaproject",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.3.3",
          classpath = scalaLibraryClasspath("3.3.3"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()

          val targets = client.buildTargets()
          targets.targets should not be empty
          val targetId = targets.targets.head.id

          // Compile first
          val compileResult = client.compile(List(targetId))
          compileResult.statusCode.value shouldBe 1
          info("Compilation successful")

          client.clear()

          // Run with arguments
          val runResult = client.run(targetId, "Main", List("test", "args"))

          info(s"Run status: ${runResult.statusCode}")

          runResult.statusCode.value shouldBe 1 // StatusCode.Ok

          // Check output
          val logs = client.logMessages
          info(s"Log messages: $logs")

          logs.exists(_.contains("test args")) shouldBe true
          logs.exists(_.contains("Args count: 2")) shouldBe true
          info("Scala run via BSP successful")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BSP Scala: run main class without arguments") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-scala-run-noargs-")
      try {
        val srcFile = workspace.resolve("src/Hello.scala")
        Files.writeString(
          srcFile,
          """object Hello {
            |  def main(args: Array[String]): Unit = {
            |    println("Hello from Scala!")
            |  }
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig.scala(
          name = "scalaproject",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.3.3",
          classpath = scalaLibraryClasspath("3.3.3"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()

          val targets = client.buildTargets()
          val targetId = targets.targets.head.id

          val compileResult = client.compile(List(targetId))
          compileResult.statusCode.value shouldBe 1

          client.clear()

          val runResult = client.run(targetId, "Hello", Nil)
          runResult.statusCode.value shouldBe 1

          val logs = client.logMessages
          logs.exists(_.contains("Hello from Scala!")) shouldBe true
          info("Scala run without args via BSP successful")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BSP Scala: run main class that exits with error code") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-scala-run-error-")
      try {
        val srcFile = workspace.resolve("src/Failer.scala")
        Files.writeString(
          srcFile,
          """object Failer {
            |  def main(args: Array[String]): Unit = {
            |    println("About to fail...")
            |    System.exit(1)
            |  }
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig.scala(
          name = "scalaproject",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.3.3",
          classpath = scalaLibraryClasspath("3.3.3"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()

          val targets = client.buildTargets()
          val targetId = targets.targets.head.id

          val compileResult = client.compile(List(targetId))
          compileResult.statusCode.value shouldBe 1

          client.clear()

          val runResult = client.run(targetId, "Failer", Nil)

          // Should fail with error status
          runResult.statusCode.value shouldBe 2 // StatusCode.Error

          info("Scala run with error exit detected via BSP")
        }
      } finally deleteRecursively(workspace)
    }
  }

  // ============================================================================
  // Java Run Tests via BSP
  // ============================================================================

  test("BSP Java: compile and run main class") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-java-run-")
      try {
        val srcFile = workspace.resolve("src/Main.java")
        Files.writeString(
          srcFile,
          """public class Main {
            |    public static void main(String[] args) {
            |        String message = args.length == 0 ? "Hello, World!" : String.join(" ", args);
            |        System.out.println(message);
            |        System.out.println("Args count: " + args.length);
            |    }
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig(
          name = "javaproject",
          sources = Set(workspace.resolve("src")),
          classpath = Nil,
          outputDir = workspace.resolve("target/classes"),
          languageConfig = JavaConfig(),
          dependsOn = Set.empty,
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()

          val targets = client.buildTargets()
          val targetId = targets.targets.head.id

          val compileResult = client.compile(List(targetId))
          compileResult.statusCode.value shouldBe 1
          info("Java compilation successful")

          client.clear()

          val runResult = client.run(targetId, "Main", List("hello", "java"))
          runResult.statusCode.value shouldBe 1

          val logs = client.logMessages
          logs.exists(_.contains("hello java")) shouldBe true
          logs.exists(_.contains("Args count: 2")) shouldBe true
          info("Java run via BSP successful")
        }
      } finally deleteRecursively(workspace)
    }
  }

  // ============================================================================
  // Kotlin Run Tests via BSP
  // ============================================================================

  test("BSP Kotlin: compile and run main class") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-kotlin-run-")
      try {
        val srcFile = workspace.resolve("src/Main.kt")
        Files.writeString(
          srcFile,
          """fun main(args: Array<String>) {
            |    val message = if (args.isEmpty()) "Hello, World!" else args.joinToString(" ")
            |    println(message)
            |    println("Args count: ${args.size}")
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig(
          name = "kotlinproject",
          sources = Set(workspace.resolve("src")),
          classpath = kotlinLibraryClasspath("2.3.0"),
          outputDir = workspace.resolve("target/classes"),
          languageConfig = KotlinConfig(version = "2.3.0"),
          dependsOn = Set.empty,
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()

          val targets = client.buildTargets()
          val targetId = targets.targets.head.id

          val compileResult = client.compile(List(targetId))
          compileResult.statusCode.value shouldBe 1
          info("Kotlin compilation successful")

          client.clear()

          // Kotlin main class name is MainKt for top-level fun main
          val runResult = client.run(targetId, "MainKt", List("hello", "kotlin"))
          runResult.statusCode.value shouldBe 1

          val logs = client.logMessages
          logs.exists(_.contains("hello kotlin")) shouldBe true
          logs.exists(_.contains("Args count: 2")) shouldBe true
          info("Kotlin run via BSP successful")
        }
      } finally deleteRecursively(workspace)
    }
  }

  // ============================================================================
  // Async Run Tests (for cancellation)
  // ============================================================================

  test("BSP: async run can be cancelled") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-async-run-")
      try {
        // Write a long-running main class
        val srcFile = workspace.resolve("src/SlowMain.scala")
        Files.writeString(
          srcFile,
          """object SlowMain {
            |  def main(args: Array[String]): Unit = {
            |    println("Starting slow operation...")
            |    Thread.sleep(30000) // 30 seconds
            |    println("Finished!")
            |  }
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig.scala(
          name = "scalaproject",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.3.3",
          classpath = scalaLibraryClasspath("3.3.3"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()

          val targets = client.buildTargets()
          val targetId = targets.targets.head.id

          val compileResult = client.compile(List(targetId))
          compileResult.statusCode.value shouldBe 1

          client.clear()

          // Start async run
          val handle = client.runAsync(targetId, "SlowMain", Nil)
          info(s"Started async run with request ID: ${handle.requestId}")

          // Wait briefly then cancel
          Thread.sleep(500)
          handle.cancel()
          info("Sent cancel request")

          // Await result — should complete quickly after cancellation
          val result = handle.awaitWithTimeout(5000)
          result match {
            case Some(r) =>
              info(s"Run completed with status: ${r.statusCode}")
              r.statusCode.value shouldBe 3 // StatusCode.Cancelled
            case None =>
              fail("Run did not respond within 5s after cancel — cancellation is broken")
          }

          // Verify client is still functional
          val targets2 = client.buildTargets()
          targets2.targets should not be empty
          info("Client still functional after async run cancel")
        }
      } finally deleteRecursively(workspace)
    }
  }

  // ============================================================================
  // Run Error Handling
  // ============================================================================

  test("BSP: run nonexistent main class returns error") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-run-noclass-")
      try {
        val srcFile = workspace.resolve("src/Hello.scala")
        Files.writeString(
          srcFile,
          """object Hello {
            |  def greet: String = "Hello"
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig.scala(
          name = "scalaproject",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.3.3",
          classpath = scalaLibraryClasspath("3.3.3"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()

          val targets = client.buildTargets()
          val targetId = targets.targets.head.id

          val compileResult = client.compile(List(targetId))
          compileResult.statusCode.value shouldBe 1

          client.clear()

          // Try to run non-existent main class
          val runResult = client.run(targetId, "NonExistent", Nil)

          // Should fail
          runResult.statusCode.value shouldBe 2 // Error
          info("Run with nonexistent class correctly returns error")
        }
      } finally deleteRecursively(workspace)
    }
  }
}
