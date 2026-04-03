package bleep.analysis

import bleep.bsp._
import ch.epfl.scala.bsp._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.TimeLimits
import org.scalatest.time.{Seconds, Span}

import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters._

/** Integration tests for BSP diagnostic reset protocol (issue #526).
  *
  * Verifies that:
  *   - First diagnostic per file in a compile cycle uses reset=true
  *   - Fixed files get empty diagnostics with reset=true to clear stale errors
  *   - Multiple errors in the same file: first gets reset=true, rest get reset=false
  */
class BspDiagnosticResetIntegrationTest extends AnyFunSuite with Matchers with TimeLimits {

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

  test("BSP diagnostic reset: fixing an error clears stale diagnostics") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-diag-reset-")
      try {
        // Step 1: Write broken Java source
        val srcFile = workspace.resolve("src/Broken.java")
        Files.writeString(
          srcFile,
          """public class Broken {
            |    public void broken( {  // Syntax error
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

          // Step 2: Compile with error — should produce diagnostics with reset=true
          val result1 = client.compile(targetIds)
          result1.statusCode.value shouldBe 2 // Error

          val diagEvents1 = client.events.collect { case d: BspTestHarness.BspEvent.PublishDiagnostics => d }
          diagEvents1 should not be empty
          info(s"First compile: ${diagEvents1.size} diagnostic notification(s)")

          // The first diagnostic for the broken file should have reset=true
          val firstDiagForFile = diagEvents1.head
          firstDiagForFile.reset shouldBe true
          firstDiagForFile.diagnostics should not be empty
          info(s"First diagnostic reset=${firstDiagForFile.reset}, errors=${firstDiagForFile.diagnostics.map(_.message.take(50))}")

          // Step 3: Fix the source file
          client.clear()
          Files.writeString(
            srcFile,
            """public class Broken {
              |    public void fixed() {
              |    }
              |}
              |""".stripMargin
          )

          // Step 4: Recompile — should succeed and clear old diagnostics
          val result2 = client.compile(targetIds)
          result2.statusCode.value shouldBe 1 // Ok

          // Check that a reset diagnostic was sent to clear the old error
          val diagEvents2 = client.events.collect { case d: BspTestHarness.BspEvent.PublishDiagnostics => d }
          info(s"Second compile: ${diagEvents2.size} diagnostic notification(s)")
          diagEvents2.foreach(d => info(s"  uri=${d.uri.split('/').last}, reset=${d.reset}, diags=${d.diagnostics.size}"))

          // There should be a PublishDiagnostics with reset=true and empty diagnostics for the fixed file
          val brokenFileUri = diagEvents1.find(_.uri.contains("Broken.java")).map(_.uri).get
          val clearNotifications = diagEvents2.filter(d => d.uri == brokenFileUri && d.reset && d.diagnostics.isEmpty)
          clearNotifications should not be empty
          info("Stale diagnostics correctly cleared via empty reset=true notification")

          // The collected diagnostics for the specific file should now be empty
          val remainingDiagsForFile = client.diagnosticsByFile.getOrElse(brokenFileUri, Nil)
          remainingDiagsForFile should be(empty)
          info("Client-side diagnostic state correctly cleared for the fixed file")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BSP diagnostic reset: first diagnostic per file gets reset=true, subsequent get reset=false") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-diag-multi-error-")
      try {
        // Write a file with multiple errors
        val srcFile = workspace.resolve("src/MultiError.java")
        Files.writeString(
          srcFile,
          """public class MultiError {
            |    public void a( { }
            |    public void b( { }
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
          result.statusCode.value shouldBe 2 // Error

          val diagEvents = client.events.collect { case d: BspTestHarness.BspEvent.PublishDiagnostics => d }
          info(s"Got ${diagEvents.size} diagnostic notification(s)")
          diagEvents.foreach(d => info(s"  uri=${d.uri.split('/').last}, reset=${d.reset}, diags=${d.diagnostics.size}"))

          // If multiple diagnostics are sent for the same file, only the first should have reset=true
          if (diagEvents.size > 1) {
            val sameFileDiags = diagEvents.groupBy(_.uri)
            sameFileDiags.foreach { case (uri, diags) =>
              val resetValues = diags.map(_.reset)
              // First should be true, rest should be false
              resetValues.head shouldBe true
              resetValues.tail.foreach(_ shouldBe false)
              info(s"File ${uri.split('/').last}: reset values = $resetValues (correct)")
            }
          } else {
            // Single notification — should have reset=true
            diagEvents.head.reset shouldBe true
            info("Single diagnostic notification with reset=true (correct)")
          }
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BSP diagnostic reset: re-introducing error after fix sends reset=true") {
    failAfter(mediumTimeout) {
      val workspace = createTempWorkspace("bsp-diag-reintroduce-")
      try {
        val srcFile = workspace.resolve("src/Toggle.java")

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

          // Cycle 1: broken
          Files.writeString(srcFile, "public class Toggle { public void x( { } }")
          val r1 = client.compile(targetIds)
          r1.statusCode.value shouldBe 2
          val diags1 = client.events.collect { case d: BspTestHarness.BspEvent.PublishDiagnostics if d.diagnostics.nonEmpty => d }
          diags1 should not be empty
          diags1.head.reset shouldBe true
          info("Cycle 1 (broken): diagnostics with reset=true")

          // Cycle 2: fixed
          client.clear()
          Files.writeString(srcFile, "public class Toggle { public void x() { } }")
          val r2 = client.compile(targetIds)
          r2.statusCode.value shouldBe 1
          val clearDiags = client.events.collect { case d: BspTestHarness.BspEvent.PublishDiagnostics if d.reset && d.diagnostics.isEmpty => d }
          clearDiags should not be empty
          info("Cycle 2 (fixed): clear notification with reset=true, empty diagnostics")

          // Cycle 3: broken again
          client.clear()
          Files.writeString(srcFile, "public class Toggle { public void x( { } }")
          val r3 = client.compile(targetIds)
          r3.statusCode.value shouldBe 2
          val diags3 = client.events.collect { case d: BspTestHarness.BspEvent.PublishDiagnostics if d.diagnostics.nonEmpty => d }
          diags3 should not be empty
          diags3.head.reset shouldBe true
          info("Cycle 3 (broken again): new diagnostics with reset=true")
        }
      } finally deleteRecursively(workspace)
    }
  }
}
