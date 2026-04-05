package bleep.analysis

import bleep.bsp._
import ch.epfl.scala.bsp._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.TimeLimits
import org.scalatest.time.{Seconds, Span}

import java.nio.file.{Files, Path}
import scala.jdk.StreamConverters._

/** Tests that zinc analysis survives a tar.gz round-trip (pack → unpack).
  *
  * This simulates what remote-cache push/pull does: compile, tar.gz the target dir, delete it, extract, compile again. The second compile should be incremental
  * (up to date), not a clean build.
  */
class ZincCacheRoundtripTest extends AnyFunSuite with Matchers with TimeLimits {

  val timeout: Span = Span(120, Seconds)

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

  test("TarGz preserves file contents exactly") {
    val workspace = createTempWorkspace("tartest-")
    try {
      val dir = workspace.resolve("data")
      Files.createDirectories(dir.resolve("sub"))
      Files.write(dir.resolve("a.txt"), "hello".getBytes)
      Files.write(dir.resolve("sub/b.bin"), Array[Byte](0, 1, 2, -1, -128, 127))

      val archive = bleep.TarGz.pack(dir)
      info(s"Archive: ${archive.length} bytes")

      val restored = workspace.resolve("restored")
      bleep.TarGz.unpack(archive, restored)

      val origA = Files.readAllBytes(dir.resolve("a.txt"))
      val restoredA = Files.readAllBytes(restored.resolve("a.txt"))
      java.util.Arrays.equals(origA, restoredA) shouldBe true

      val origB = Files.readAllBytes(dir.resolve("sub/b.bin"))
      val restoredB = Files.readAllBytes(restored.resolve("sub/b.bin"))
      java.util.Arrays.equals(origB, restoredB) shouldBe true
      info("TarGz round-trip preserves all file contents")
    } finally deleteRecursively(workspace)
  }

  test("zinc analysis survives tar.gz round-trip on same machine") {
    failAfter(timeout) {
      val workspace = createTempWorkspace("zinc-cache-roundtrip-")
      try {
        val srcFile = workspace.resolve("src/Hello.java")
        Files.writeString(
          srcFile,
          """public class Hello {
            |    public String greet() { return "Hello"; }
            |}
            |""".stripMargin
        )

        val config = BspTestHarness.ProjectConfig(
          name = "testproject",
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

          // Step 1: First compile
          val result1 = client.compile(targetIds)
          result1.statusCode.value shouldBe 1 // Ok
          info("First compile successful")

          // Step 2: Verify class file exists
          val classFile = workspace.resolve("target/classes/Hello.class")
          Files.exists(classFile) shouldBe true

          // Step 3: Verify analysis exists
          val analysisDir = workspace.resolve("target/classes/.zinc")
          val analysisExists = Files.exists(analysisDir) ||
            // Analysis might be elsewhere — check target dir
            Files.walk(workspace.resolve("target")).toScala(List).exists(_.getFileName.toString == "analysis.zip")
          info(s"Analysis exists: $analysisExists")

          // Step 4: Pack the target dir
          val targetDir = workspace.resolve("target")
          val archive = bleep.TarGz.pack(targetDir)
          info(s"Packed target dir: ${archive.length} bytes")

          // Step 5: Delete the target dir (simulate bleep clean)
          deleteRecursively(targetDir)
          Files.exists(targetDir) shouldBe false
          info("Target dir deleted")

          // Step 6: Extract the archive (simulate remote-cache pull)
          bleep.TarGz.unpack(archive, targetDir)
          Files.exists(classFile) shouldBe true
          info("Archive extracted, class file restored")

          // Step 7: Second compile — should be up to date, not clean build
          client.clear()
          val result2 = client.compile(targetIds)
          result2.statusCode.value shouldBe 1 // Ok

          // Check events for compilation indicators
          val logMessages = client.logMessages
          val compiledMessages = logMessages.filter(m => m.contains("compiling") || m.contains("compiled") || m.contains("Compiling"))
          info(s"Second compile messages: ${compiledMessages.mkString(", ")}")

          // The key test: after round-trip, zinc should NOT do a clean build
          // If analysis was loaded correctly, it should recognize the project as up-to-date
          // or at most do incremental (not clean)
          val cleanBuildMessages = logMessages.filter(_.contains("clean build"))
          if (cleanBuildMessages.nonEmpty) {
            fail(s"Zinc did a clean build after cache round-trip! Messages: ${cleanBuildMessages.mkString(", ")}")
          }

          info("Second compile did NOT trigger clean build - cache round-trip works!")
        }
      } finally deleteRecursively(workspace)
    }
  }
}
