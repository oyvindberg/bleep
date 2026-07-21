package bleep.analysis

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

  test("BSP: cancel compilation of huge source produces Cancelled status") {
    failAfter(longTimeout) {
      val workspace = createTempWorkspace("bsp-cancel-huge-")
      try {
        val hugeSource = generateHugeScalaSource(numClasses = 300, methodsPerClass = 50)
        val srcFile = workspace.resolve("src/Huge.scala")
        Files.writeString(srcFile, hugeSource)
        info(s"Generated source: ${hugeSource.length} chars (~${hugeSource.length / 1024}KB)")

        val config = BspTestHarness.ProjectConfig.scala(
          name = "hugeproject",
          sources = Set(workspace.resolve("src")),
          scalaVersion = "3.7.4",
          classpath = scalaLibraryClasspath("3.7.4"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()
          val targets = client.buildTargets()
          val targetIds = targets.targets.map(_.id)
          val handle = client.compileAsync(targetIds)
          Thread.sleep(100)
          handle.cancel()
          val result = handle.awaitWithTimeout(longTimeout.toMillis)
          result match {
            case Some(r) => r.statusCode shouldBe StatusCode.Cancelled
            case None    => fail("Timeout waiting for compile response after cancellation")
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
          result.statusCode shouldBe StatusCode.Ok
          info("Recompile after cancellation succeeded — zinc state properly cleaned")
        }
      } finally deleteRecursively(workspace)
    }
  }

  test("BSP: immediate cancel (before compilation starts) produces Cancelled status") {
    failAfter(longTimeout) {
      val workspace = createTempWorkspace("bsp-cancel-immediate-")
      try {
        val hugeSource = generateHugeScalaSource(numClasses = 100, methodsPerClass = 30)
        val srcFile = workspace.resolve("src/Huge.scala")
        Files.writeString(srcFile, hugeSource)

        val config = BspTestHarness.ProjectConfig.scala(
          name = "immediatecancel",
          sources = Set(workspace.resolve("src")),
          scalaVersion = "3.7.4",
          classpath = scalaLibraryClasspath("3.7.4"),
          isTest = false
        )

        BspTestHarness.withProject(workspace, config) { client =>
          client.initialize()
          val targets = client.buildTargets()
          val targetIds = targets.targets.map(_.id)

          val handle = client.compileAsync(targetIds)
          handle.cancel()

          // failAfter(longTimeout) bounds the whole test. The request was cancelled before compilation could start, so the result MUST be Cancelled.
          val result = handle.awaitWithTimeout(longTimeout.toMillis)
          result match {
            case Some(r) =>
              info(s"Compile result status: ${r.statusCode}")
              r.statusCode shouldBe StatusCode.Cancelled
            case None =>
              fail("Timeout waiting for compile response after immediate cancellation")
          }
        }
      } finally deleteRecursively(workspace)
    }
  }
}
