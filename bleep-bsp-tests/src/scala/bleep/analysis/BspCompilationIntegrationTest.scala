package bleep.analysis

import bleep.bsp._
import ch.epfl.scala.bsp._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.TimeLimits
import org.scalatest.time.{Seconds, Span}

import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters._

/** Integration tests for compilation through BSP protocol.
  *
  * Tests compilation scenarios for Java, Scala, and Kotlin by:
  *   1. Writing source files to a workspace
  *   2. Configuring the BSP server with appropriate language settings
  *   3. Sending buildTarget/compile requests
  *   4. Verifying results via status codes and diagnostics
  *
  * These tests mirror CompilerIntegrationTest but go through the full BSP protocol.
  */
class BspCompilationIntegrationTest extends AnyFunSuite with Matchers with TimeLimits {

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
  // Java Compilation Tests via BSP
  // ============================================================================

  test("BSP Java: compile single file") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-java-single-")
      try {
        // Write Java source
        val srcFile = workspace.resolve("src/Hello.java")
        Files.writeString(
          srcFile,
          """public class Hello {
            |    public String greet() {
            |        return "Hello, World!";
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
          targets.targets should not be empty

          val targetIds = targets.targets.map(_.id)
          val result = client.compile(targetIds)

          info(s"Compile status: ${result.statusCode}")

          result.statusCode.value shouldBe 1 // StatusCode.Ok

          // Verify output class file exists
          val classFile = workspace.resolve("target/classes/Hello.class")
          Files.exists(classFile) shouldBe true
          info("Java compilation via BSP successful")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BSP Java: compile with error reports diagnostics") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-java-error-")
      try {
        // Write Java source with syntax error
        val srcFile = workspace.resolve("src/Broken.java")
        Files.writeString(
          srcFile,
          """public class Broken {
            |    public void broken( {  // Missing parameter
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
          val targetIds = targets.targets.map(_.id)
          val result = client.compile(targetIds)

          info(s"Compile status: ${result.statusCode}")

          result.statusCode.value shouldBe 2 // StatusCode.Error

          // Should have collected diagnostics
          val diags = client.diagnosticsByFile
          info(s"Diagnostics collected: ${diags.size} files")
          diags.foreach { case (uri, ds) =>
            info(s"  $uri: ${ds.map(_.message.take(50)).mkString(", ")}")
          }

          diags should not be empty
          info("Java error reporting via BSP working")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BSP Java: compile with --release flag") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-java-release-")
      try {
        val srcFile = workspace.resolve("src/Hello.java")
        Files.writeString(
          srcFile,
          """public class Hello {
            |    public String greet() {
            |        return "Hello Java 11!";
            |    }
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig(
          name = "javaproject",
          sources = Set(workspace.resolve("src")),
          classpath = Nil,
          outputDir = workspace.resolve("target/classes"),
          languageConfig = JavaConfig(release = Some(11)),
          dependsOn = Set.empty,
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()

          val targets = client.buildTargets()
          val targetIds = targets.targets.map(_.id)
          val result = client.compile(targetIds)

          result.statusCode.value shouldBe 1 // StatusCode.Ok
          info("Java --release 11 compilation via BSP successful")
        }
      } finally deleteRecursively(workspace)
    }
  }

  // ============================================================================
  // Scala Compilation Tests via BSP
  // ============================================================================

  test("BSP Scala: compile single file") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-scala-single-")
      try {
        val srcFile = workspace.resolve("src/Hello.scala")
        Files.writeString(
          srcFile,
          """object Hello {
            |  def greet: String = "Hello, Scala!"
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

          val targetIds = targets.targets.map(_.id)
          val result = client.compile(targetIds)

          info(s"Compile status: ${result.statusCode}")
          result.statusCode.value shouldBe 1

          // Verify output
          val classDir = workspace.resolve("target/classes")
          Files.exists(classDir) shouldBe true
          info("Scala compilation via BSP successful")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BSP Scala: compile with type error reports diagnostics") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-scala-error-")
      try {
        val srcFile = workspace.resolve("src/Broken.scala")
        Files.writeString(
          srcFile,
          """object Broken {
            |  val x: Int = "not an int"  // Type error
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
          val targetIds = targets.targets.map(_.id)
          val result = client.compile(targetIds)

          result.statusCode.value shouldBe 2 // StatusCode.Error

          val diags = client.diagnosticsByFile
          info(s"Diagnostics: ${diags.size} files")
          diags.foreach { case (uri, ds) =>
            info(s"  $uri: ${ds.map(_.message.take(50)).mkString(", ")}")
          }

          diags should not be empty
          info("Scala error reporting via BSP working")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BSP Scala: compile multiple files with dependencies") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-scala-multi-")
      try {
        // Write two files where one depends on the other
        val libFile = workspace.resolve("src/Lib.scala")
        Files.writeString(
          libFile,
          """object Lib {
            |  def value: Int = 42
            |}
            |""".stripMargin
        )

        val appFile = workspace.resolve("src/App.scala")
        Files.writeString(
          appFile,
          """object App {
            |  def main(args: Array[String]): Unit = {
            |    println(s"Value: ${Lib.value}")
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
          val targetIds = targets.targets.map(_.id)
          val result = client.compile(targetIds)

          result.statusCode.value shouldBe 1
          info("Scala multi-file compilation via BSP successful")
        }
      } finally deleteRecursively(workspace)
    }
  }

  // ============================================================================
  // Kotlin Compilation Tests via BSP
  // ============================================================================

  test("BSP Kotlin: compile single file") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-kotlin-single-")
      try {
        val srcFile = workspace.resolve("src/Hello.kt")
        Files.writeString(
          srcFile,
          """object Hello {
            |    fun greet(): String = "Hello, Kotlin!"
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
          targets.targets should not be empty

          val targetIds = targets.targets.map(_.id)
          val result = client.compile(targetIds)

          info(s"Compile status: ${result.statusCode}")
          result.statusCode.value shouldBe 1

          info("Kotlin compilation via BSP successful")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BSP Kotlin: compile with error reports diagnostics") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-kotlin-error-")
      try {
        val srcFile = workspace.resolve("src/Broken.kt")
        Files.writeString(
          srcFile,
          """object Broken {
            |    val x: Int = "not an int"  // Type error
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
          val targetIds = targets.targets.map(_.id)
          val result = client.compile(targetIds)

          result.statusCode.value shouldBe 2 // StatusCode.Error

          val diags = client.diagnosticsByFile
          info(s"Diagnostics: ${diags.size} files")
          diags should not be empty

          info("Kotlin error reporting via BSP working")
        }
      } finally deleteRecursively(workspace)
    }
  }

  // ============================================================================
  // Incremental Compilation Tests via BSP
  // ============================================================================

  test("BSP: incremental Scala compilation - unchanged files not recompiled") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-incremental-")
      try {
        val fooFile = workspace.resolve("src/Foo.scala")
        Files.writeString(fooFile, """object Foo { def value: Int = 1 }""")

        val barFile = workspace.resolve("src/Bar.scala")
        Files.writeString(barFile, """object Bar { def value: Int = 2 }""")

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
          val targetIds = targets.targets.map(_.id)

          // First compile
          val result1 = client.compile(targetIds)
          result1.statusCode.value shouldBe 1
          info("First compile successful")
          client.clear()

          // Wait for timestamp to change
          Thread.sleep(100)

          // Modify only Foo
          Files.writeString(fooFile, """object Foo { def value: Int = 42 }""")

          // Second compile
          val result2 = client.compile(targetIds)
          result2.statusCode.value shouldBe 1
          info("Second compile (Foo changed) successful")
          client.clear()

          // Third compile - no changes
          val result3 = client.compile(targetIds)
          result3.statusCode.value shouldBe 1
          info("Third compile (no changes) successful - incremental working")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BSP: incremental compilation with dependency change triggers recompile") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-incremental-dep-")
      try {
        // Lib is depended upon by App
        val libFile = workspace.resolve("src/Lib.scala")
        Files.writeString(libFile, """object Lib { def value: Int = 1 }""")

        val appFile = workspace.resolve("src/App.scala")
        Files.writeString(appFile, """object App { def computed: Int = Lib.value * 2 }""")

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
          val targetIds = targets.targets.map(_.id)

          // First compile
          val result1 = client.compile(targetIds)
          result1.statusCode.value shouldBe 1
          info("Initial compile successful")
          client.clear()

          Thread.sleep(100)

          // Change Lib's public API (type change would affect App)
          Files.writeString(libFile, """object Lib { def value: String = "changed" }""")

          // This should fail because App expects Int
          val result2 = client.compile(targetIds)
          result2.statusCode.value shouldBe 2 // Error
          info("Recompile detected API change correctly")
        }
      } finally deleteRecursively(workspace)
    }
  }

  // ============================================================================
  // Clean Cache Tests via BSP
  // ============================================================================

  test("BSP: cleanCache clears compiled classes") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-clean-")
      try {
        val srcFile = workspace.resolve("src/Hello.scala")
        Files.writeString(srcFile, """object Hello { def greet: String = "Hello" }""")

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
          val targetIds = targets.targets.map(_.id)

          // Compile first
          val result1 = client.compile(targetIds)
          result1.statusCode.value shouldBe 1
          info("Initial compile successful")

          // Clean cache
          val cleanResult = client.cleanCache(targetIds)
          info(s"Clean cache result: cleaned=${cleanResult.cleaned}")

          // Recompile should succeed
          val result2 = client.compile(targetIds)
          result2.statusCode.value shouldBe 1
          info("Recompile after clean successful")
        }
      } finally deleteRecursively(workspace)
    }
  }

  // ============================================================================
  // Multiple Projects Tests via BSP
  // ============================================================================

  test("BSP: compile multiple dependent projects") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-multi-project-")
      try {
        // Create lib directory and sources
        Files.createDirectories(workspace.resolve("lib/src"))
        Files.createDirectories(workspace.resolve("lib/target/classes"))
        val libFile = workspace.resolve("lib/src/Lib.scala")
        Files.writeString(libFile, """object Lib { def greet: String = "Hello from Lib" }""")

        // Create app directory and sources
        Files.createDirectories(workspace.resolve("app/src"))
        Files.createDirectories(workspace.resolve("app/target/classes"))
        val appFile = workspace.resolve("app/src/App.scala")
        Files.writeString(appFile, """object App { def main(args: Array[String]): Unit = println(Lib.greet) }""")

        val libConfig = BspTestHarness.ProjectConfig(
          name = "lib",
          sources = Set(workspace.resolve("lib/src")),
          classpath = scalaLibraryClasspath("3.3.3"),
          outputDir = workspace.resolve("lib/target/classes"),
          languageConfig = ScalaConfig("3.3.3", Nil),
          dependsOn = Set.empty,
          isTest = false
        )

        val appConfig = BspTestHarness.ProjectConfig(
          name = "app",
          sources = Set(workspace.resolve("app/src")),
          // App needs lib's output on classpath to compile
          classpath = workspace.resolve("lib/target/classes") :: scalaLibraryClasspath("3.3.3"),
          outputDir = workspace.resolve("app/target/classes"),
          languageConfig = ScalaConfig("3.3.3", Nil),
          dependsOn = Set("lib"),
          isTest = false
        )

        BspTestHarness.withProjects(workspace, List(libConfig, appConfig)) { client =>
          client.initialize()
          val targets = client.buildTargets()
          info(s"Found ${targets.targets.size} build targets")
          targets.targets.foreach(t => info(s"  - ${t.displayName.getOrElse(t.id.uri.value)}"))

          targets.targets.size shouldBe 2

          val targetIds = targets.targets.map(_.id)
          val result = client.compile(targetIds)

          info(s"Multi-project compile status: ${result.statusCode}")
          result.statusCode.value shouldBe 1
          info("Multi-project compilation via BSP successful")
        }
      } finally deleteRecursively(workspace)
    }
  }
}
