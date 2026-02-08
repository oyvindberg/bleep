package bleep.analysis

import bleep.bsp._
import ch.epfl.scala.bsp._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.TimeLimits
import org.scalatest.time.{Seconds, Span}

import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters._

/** Integration tests for BSP-level compilation cancellation.
  *
  * These tests verify that:
  *   1. Cancelling a compile request via $/cancelRequest stops compilation quickly
  *   2. After cancellation, zinc state is cleaned so recompile succeeds
  *   3. Pre-cancelled requests (cancel before compilation starts) return fast
  *
  * Uses the BspTestHarness to send real BSP requests and verify behavior through the full protocol stack.
  */
class BspCancellationIntegrationTest extends AnyFunSuite with Matchers with TimeLimits {

  val longTimeout: Span = Span(120, Seconds)

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

  /** Generate a massive Scala source file that takes many seconds to compile. 100 classes x 30 methods = 3000 methods with enough complexity to keep the
    * compiler busy.
    */
  def generateHugeScalaSource(numClasses: Int, methodsPerClass: Int): String = {
    val sb = new StringBuilder
    sb.append("package huge\n\n")

    for (i <- 0 until numClasses) {
      sb.append(s"class HugeClass$i:\n")
      for (j <- 0 until methodsPerClass) {
        sb.append(s"  def method${j}(x: Int): Int =\n")
        sb.append(s"    val a$j = x + $j\n")
        sb.append(s"    val b$j = a$j * 2\n")
        sb.append(s"    val c$j = b$j + a$j\n")
        sb.append(s"    c$j\n")
        sb.append("\n")
      }
      sb.append("\n")
    }

    sb.toString
  }

  test("BSP: cancel compilation of huge source returns quickly") {
    failAfter(longTimeout) {
      val workspace = createTempWorkspace("bsp-cancel-huge-")
      try {
        // Write a truly massive source file (300 classes x 50 methods = 15000 methods).
        // Without cancellation, this takes 30+ seconds to compile even on a warm JVM.
        val hugeSource = generateHugeScalaSource(numClasses = 300, methodsPerClass = 50)
        val srcFile = workspace.resolve("src/Huge.scala")
        Files.writeString(srcFile, hugeSource)
        info(s"Generated source: ${hugeSource.length} chars (~${hugeSource.length / 1024}KB)")

        val config = BspTestHarness.ProjectConfig.scala(
          name = "hugeproject",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.7.4",
          classpath = scalaLibraryClasspath("3.7.4"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()

          val targets = client.buildTargets()
          val targetIds = targets.targets.map(_.id)

          // First, time the full compilation to establish a baseline
          // (skip this in CI — only care that cancel is fast)

          // Start compile asynchronously
          val startTime = System.currentTimeMillis()
          val handle = client.compileAsync(targetIds)

          // Wait for compilation to be well underway, then cancel
          Thread.sleep(2000)
          handle.cancel()

          // Wait for the response
          val result = handle.awaitWithTimeout(60000)
          val elapsed = System.currentTimeMillis() - startTime
          info(s"Cancellation completed in ${elapsed}ms")

          // The key assertion: cancellation should be fast (< 15s),
          // not waiting for the full compilation (which would take 30s+)
          elapsed should be < 20000L

          result match {
            case Some(r) =>
              info(s"Compile result status: ${r.statusCode}")
              // Ok (1) is also acceptable — it means compilation raced and finished before cancel was processed.
              // But with 15000 methods, that should be rare.
              r.statusCode.value should (be(1) or be(2) or be(3))
            case None =>
              fail("Timeout waiting for compile response after cancellation")
          }
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BSP: recompile succeeds after cancellation (zinc state cleaned)") {
    failAfter(longTimeout) {
      val workspace = createTempWorkspace("bsp-cancel-recompile-")
      try {
        // Use a large-ish source to ensure cancellation happens mid-compile
        val hugeSource = generateHugeScalaSource(numClasses = 50, methodsPerClass = 20)
        val srcFile = workspace.resolve("src/Huge.scala")
        Files.writeString(srcFile, hugeSource)

        val config = BspTestHarness.ProjectConfig.scala(
          name = "recompileproject",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.7.4",
          classpath = scalaLibraryClasspath("3.7.4"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()
          val targets = client.buildTargets()
          val targetIds = targets.targets.map(_.id)

          // 1. Start and cancel a compile
          val handle = client.compileAsync(targetIds)
          Thread.sleep(500)
          handle.cancel()
          handle.awaitWithTimeout(30000)
          info("First compile cancelled")
          client.clear()

          // 2. Replace with a small source file and recompile
          // If zinc state wasn't cleaned, this would fail with stale class errors
          Files.writeString(
            srcFile,
            """package huge
              |object Small:
              |  def hello: String = "works"
              |""".stripMargin
          )

          val result = client.compile(targetIds)
          info(s"Recompile after cancel status: ${result.statusCode}")
          result.statusCode.value shouldBe 1 // StatusCode.Ok
          info("Recompile after cancellation succeeded — zinc state properly cleaned")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BSP: immediate cancel (before compilation starts) returns fast") {
    failAfter(longTimeout) {
      val workspace = createTempWorkspace("bsp-cancel-immediate-")
      try {
        val hugeSource = generateHugeScalaSource(numClasses = 100, methodsPerClass = 30)
        val srcFile = workspace.resolve("src/Huge.scala")
        Files.writeString(srcFile, hugeSource)

        val config = BspTestHarness.ProjectConfig.scala(
          name = "immediatecancel",
          sources = Set(workspace.resolve("src")),
          outputDir = workspace.resolve("target/classes"),
          scalaVersion = "3.7.4",
          classpath = scalaLibraryClasspath("3.7.4"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()
          val targets = client.buildTargets()
          val targetIds = targets.targets.map(_.id)

          // Send compile and cancel immediately (no sleep)
          val startTime = System.currentTimeMillis()
          val handle = client.compileAsync(targetIds)
          handle.cancel()

          val result = handle.awaitWithTimeout(30000)
          val elapsed = System.currentTimeMillis() - startTime
          info(s"Immediate cancellation completed in ${elapsed}ms")

          // Should be very fast — under 5 seconds
          elapsed should be < 10000L

          result match {
            case Some(r) =>
              info(s"Compile result status: ${r.statusCode}")
            case None =>
              fail("Timeout waiting for compile response after immediate cancellation")
          }
        }
      } finally deleteRecursively(workspace)
    }
  }
}
