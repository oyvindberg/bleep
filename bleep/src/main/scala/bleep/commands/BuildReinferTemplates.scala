package bleep
package commands

import bleep.internal.{normalize, ShortenAndSortJson, Templates}
import io.circe.syntax._

import java.nio.file.Files

case class BuildReinferTemplates(started: Started, ignoreWhenInferringTemplates: Set[model.ProjectName]) extends BleepCommand {
  override def run(): Unit = {
    val normalizedBuild = normalize(started.build)
    val droppedTemplates = normalizedBuild.dropTemplates

    val build = Templates(droppedTemplates, ignoreWhenInferringTemplates)

    Files.writeString(
      started.buildPaths.bleepJsonFile,
      build.asJson.foldWith(ShortenAndSortJson).spaces2
    )
  }
}
