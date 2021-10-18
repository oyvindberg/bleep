package bleep

import bloop.config.{Config, ConfigCodecs}
import com.github.plokhotnyuk.jsoniter_scala.core.{writeToString, WriterConfig}
import org.scalactic.TripleEqualsSupport
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

class OutputSnapshotTest extends AnyFunSuite with TripleEqualsSupport {
  val resolver = new CoursierResolver(scala.concurrent.ExecutionContext.global, downloadSources = true)
  val workspaceDirBase = Paths.get("bloop-test-files").toAbsolutePath

  def anonymize(str: String): String = {
    val home = System.getProperty("user.home")
    str.replace(home, "<HOME>")
  }

  val isCi: Boolean =
    sys.env.contains("BUILD_NUMBER") || sys.env.contains("CI") // from sbt

  def writeAndCompare(workspaceDir: Path, bloopFiles: Seq[Config.File]): Assertion = {
    bloopFiles.foreach { bloopFile =>
      val json = writeToString(bloopFile, WriterConfig.withIndentionStep(2))(ConfigCodecs.codecFile)
      val file = workspaceDir / (bloopFile.project.name + ".json")
      val anonymizedJson = anonymize(json)
      if (isCi) {
        val existing = Files.readString(file)
        assert(existing === anonymizedJson)
      } else {
        Files.createDirectories(workspaceDir)
        Files.write(file, anonymizedJson.getBytes(StandardCharsets.UTF_8))
      }
    }
    if (isCi) succeed
    else pending
  }

  test("test1") {
    val workspaceDir = workspaceDirBase / "test1"
    val input = ResourceReader.resourceAsString("/test1.json")
    val parsedProject = parseProjectFile(input)
    val bloopFiles = generateBloopFiles(parsedProject, workspaceDir, resolver)
    writeAndCompare(
      workspaceDir,
      bloopFiles.map { case (projectName, lazyProject) =>
        lazyProject.forceGet(projectName.value)
      }.toList
    )
  }
}
