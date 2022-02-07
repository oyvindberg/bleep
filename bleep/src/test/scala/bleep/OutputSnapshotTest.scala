package bleep

import bleep.internal.generateBloopFiles
import bleep.logging.Logger
import bloop.config.{Config, ConfigCodecs}
import com.github.plokhotnyuk.jsoniter_scala.core.{writeToString, WriterConfig}
import coursier.paths.CoursierPaths
import org.scalactic.TripleEqualsSupport
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.util.Properties

class OutputSnapshotTest extends AnyFunSuite with TripleEqualsSupport {
  val resolver = CoursierResolver(Logger.DevNull, downloadSources = true, None, CoursierResolver.Authentications.empty)
  val workspaceDirBase = Paths.get("bloop-test-files").toAbsolutePath

  def anonymize(str: String): String =
    str
      .replace(CoursierPaths.cacheDirectory().toString, "<COURSIER>")
      .replace(System.getProperty("user.dir"), "<PROJECT>")
      .replace(System.getProperty("user.home"), "<HOME>")

  val isCi: Boolean =
    sys.env.contains("BUILD_NUMBER") || sys.env.contains("CI") // from sbt

  def writeAndCompare(workspaceDir: Path, bloopFiles: Seq[Config.File]): Assertion =
    if (Properties.isWin) pending // let's deal with this later
    else {
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
    val Right(build) = model.parseBuild(input)
    val bloopFiles = generateBloopFiles(ExplodedBuild.of(build), BuildPaths(workspaceDir / ".bleep.json"), resolver)
    writeAndCompare(
      workspaceDir,
      bloopFiles.map { case (projectName, lazyProject) =>
        lazyProject.forceGet(projectName.value)
      }.toList
    )
  }
}
