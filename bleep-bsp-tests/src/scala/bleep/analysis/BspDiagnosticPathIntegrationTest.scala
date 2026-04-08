package bleep.analysis

import bleep.bsp._
import ch.epfl.scala.bsp._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.TimeLimits
import org.scalatest.time.{Seconds, Span}

import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters._

/** Integration tests for BSP diagnostic file paths (issue #552).
  *
  * Verifies that diagnostic URIs sent via build/publishDiagnostics contain real filesystem paths, not marker-prefixed virtual IDs like `${BASE}/src/...`. The
  * portable analysis mappers rewrite source paths to markers for cross-machine portability, but diagnostics sent to IDE clients must use real paths so editors
  * can navigate to error locations.
  *
  * Tests all three languages: Java, Scala, and Kotlin.
  */
class BspDiagnosticPathIntegrationTest extends AnyFunSuite with Matchers with TimeLimits {

  val mediumTimeout: Span = Span(120, Seconds)

  def createTempWorkspace(prefix: String): Path = {
    val dir = Files.createTempDirectory(prefix)
    Files.createDirectories(dir.resolve("src"))
    Files.createDirectories(dir.resolve("target/classes"))
    dir
  }

  def deleteRecursively(path: Path): Unit =
    if (Files.exists(path)) {
      if (Files.isDirectory(path))
        Files.list(path).toScala(List).foreach(deleteRecursively)
      Files.delete(path)
    }

  def scalaLibraryClasspath(version: String): List[Path] =
    CompilerResolver.resolveScalaLibrary(version).toList

  def kotlinLibraryClasspath(version: String): List[Path] =
    CompilerResolver.resolveKotlinLibrary(version).toList

  /** Assert that all file-attributed diagnostic events have real filesystem URIs (no markers). */
  def assertDiagnosticURIsAreReal(
      events: List[BspTestHarness.BspEvent],
      expectedFileName: String
  ): Unit = {
    val errorDiags = events.collect {
      case d: BspTestHarness.BspEvent.PublishDiagnostics if d.diagnostics.nonEmpty && !d.uri.contains("unknown") => d
    }
    errorDiags should not be empty

    errorDiags.foreach { d =>
      info(s"  uri=${d.uri}")
      // Must NOT contain marker prefixes
      d.uri should not include "${BASE}"
      d.uri should not include "${LIB}"
      d.uri should not include "${JDK}"
      d.uri should not include "${IVY}"

      // Must be a proper file:// URI pointing to the actual source file
      d.uri should startWith("file:")
      d.uri should include(expectedFileName)

      // The URI should resolve to the actual file on disk
      val uriPath = Path.of(java.net.URI.create(d.uri))
      Files.exists(uriPath) shouldBe true
      info(s"  resolved to existing file: $uriPath")
    }
  }

  // ============================================================================
  // Java
  // ============================================================================

  test("Java: diagnostic URIs use real paths through error-fix-error cycle") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-diag-java-")
      try {
        val srcFile = workspace.resolve("src/Hello.java")
        Files.writeString(
          srcFile,
          """public class Hello {
            |    public static void main(String[] args) {
            |        System.out.println("hello");
            |    }
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig(
          name = "java-diag-test",
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
          val targetIds = targets.targets.map(_.id)

          // Compile valid code
          val r1 = client.compile(targetIds)
          r1.statusCode.value shouldBe 1

          // Break: missing semicolon
          client.clear()
          Files.writeString(
            srcFile,
            """public class Hello {
              |    public static void main(String[] args) {
              |        System.out.println("hello")
              |    }
              |}
              |""".stripMargin
          )
          val r2 = client.compile(targetIds)
          r2.statusCode.value shouldBe 2
          assertDiagnosticURIsAreReal(client.events, "Hello.java")
          info("Java: broken → diagnostics have real URIs")

          // Fix
          client.clear()
          Files.writeString(
            srcFile,
            """public class Hello {
              |    public static void main(String[] args) {
              |        System.out.println("hello");
              |    }
              |}
              |""".stripMargin
          )
          val r3 = client.compile(targetIds)
          r3.statusCode.value shouldBe 1

          val clearEvents = client.events.collect {
            case d: BspTestHarness.BspEvent.PublishDiagnostics if d.reset && d.diagnostics.isEmpty => d
          }
          clearEvents.foreach { d =>
            d.uri should not include "${BASE}"
          }
          info("Java: fixed → diagnostics cleared")
        }
      } finally deleteRecursively(workspace)
    }
  }

  // ============================================================================
  // Scala
  // ============================================================================

  test("Scala: diagnostic URIs use real paths through error-fix-error cycle") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-diag-scala-")
      try {
        val srcFile = workspace.resolve("src/Hello.scala")
        Files.writeString(
          srcFile,
          """object Hello {
            |  def greet: String = "hello"
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig.scala(
          name = "scala-diag-test",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.3.3",
          classpath = scalaLibraryClasspath("3.3.3"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()
          val targets = client.buildTargets()
          val targetIds = targets.targets.map(_.id)

          // Compile valid code
          val r1 = client.compile(targetIds)
          r1.statusCode.value shouldBe 1

          // Break: type error
          client.clear()
          Files.writeString(
            srcFile,
            """object Hello {
              |  val x: Int = "not an int"
              |}
              |""".stripMargin
          )
          val r2 = client.compile(targetIds)
          r2.statusCode.value shouldBe 2
          assertDiagnosticURIsAreReal(client.events, "Hello.scala")
          info("Scala: broken → diagnostics have real URIs")

          // Fix
          client.clear()
          Files.writeString(
            srcFile,
            """object Hello {
              |  val x: Int = 42
              |}
              |""".stripMargin
          )
          val r3 = client.compile(targetIds)
          r3.statusCode.value shouldBe 1

          val clearEvents = client.events.collect {
            case d: BspTestHarness.BspEvent.PublishDiagnostics if d.reset && d.diagnostics.isEmpty => d
          }
          clearEvents.foreach { d =>
            d.uri should not include "${BASE}"
          }
          info("Scala: fixed → diagnostics cleared")
        }
      } finally deleteRecursively(workspace)
    }
  }

  // ============================================================================
  // Kotlin
  // ============================================================================

  test("Kotlin: diagnostic URIs use real paths through error-fix-error cycle") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-diag-kotlin-")
      try {
        val srcFile = workspace.resolve("src/Hello.kt")
        Files.writeString(
          srcFile,
          """object Hello {
            |    fun greet(): String = "hello"
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig(
          name = "kotlin-diag-test",
          sources = Set(workspace.resolve("src")),
          classpath = kotlinLibraryClasspath("2.3.0"),
          outputDir = workspace.resolve("target/classes"),
          languageConfig = KotlinConfig("2.3.0"),
          dependsOn = Set.empty,
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()
          val targets = client.buildTargets()
          val targetIds = targets.targets.map(_.id)

          // Compile valid code
          val r1 = client.compile(targetIds)
          r1.statusCode.value shouldBe 1

          // Break: type error
          client.clear()
          Files.writeString(
            srcFile,
            """object Hello {
              |    val x: Int = "not an int"
              |}
              |""".stripMargin
          )
          val r2 = client.compile(targetIds)
          r2.statusCode.value shouldBe 2
          assertDiagnosticURIsAreReal(client.events, "Hello.kt")
          info("Kotlin: broken → diagnostics have real URIs")

          // Fix
          client.clear()
          Files.writeString(
            srcFile,
            """object Hello {
              |    val x: Int = 42
              |}
              |""".stripMargin
          )
          val r3 = client.compile(targetIds)
          r3.statusCode.value shouldBe 1

          val clearEvents = client.events.collect {
            case d: BspTestHarness.BspEvent.PublishDiagnostics if d.reset && d.diagnostics.isEmpty => d
          }
          clearEvents.foreach { d =>
            d.uri should not include "${BASE}"
          }
          info("Kotlin: fixed → diagnostics cleared")
        }
      } finally deleteRecursively(workspace)
    }
  }
}
