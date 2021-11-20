package bleep
package commands

import bleep.internal.{Os, ShortenJson}
import bleep.logging.Logger
import io.circe.syntax._

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

case class Import(logger: Logger) extends BleepCommand {
  override def run(): Unit = {
    val buildPaths = BuildPaths(Os.cwd / Defaults.BuildFileName)

    val build = deduplicateBuild(importBloopFilesFromSbt(logger, buildPaths))
    Files.writeString(
      buildPaths.bleepJsonFile,
      build.asJson.foldWith(ShortenJson).spaces2,
      UTF_8
    )
    logger.info(s"Imported ${build.projects.size} projects")
  }
}
