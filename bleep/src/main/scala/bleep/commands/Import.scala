package bleep
package commands

import bleep.internal.{importBloopFilesFromSbt, normalize, Os, ShortenAndSortJson, Templates}
import bleep.logging.Logger
import io.circe.syntax._

import java.nio.file.Files

case class Import(logger: Logger, ignoreWhenInferringTemplates: Set[model.ProjectName]) extends BleepCommand {

  override def run(): Unit = {
    val buildPaths = BuildPaths(Os.cwd / Defaults.BuildFileName)

//    cli("sbt 'set Global / bloopConfigDir := baseDirectory.value / s\".bleep/import/bloop-${scalaBinaryVersion.value}\"' +bloopInstall")(buildPaths.buildDir)

    val build0 = importBloopFilesFromSbt(logger, buildPaths)
    val normalizedBuild = normalize(build0)
    val build = Templates(normalizedBuild, ignoreWhenInferringTemplates)

    Files.writeString(
      buildPaths.bleepJsonFile,
      build.asJson.foldWith(ShortenAndSortJson).spaces2
    )

    logger.info(s"Imported ${build0.projects.size} cross targets for ${build.projects.value.size} projects")
  }
}
