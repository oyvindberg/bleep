package bleep
package commands

import bleep.internal.{normalizeBuild, ShortenAndSortJson, Templates}
import io.circe.syntax._

import java.nio.file.Files

case class BuildReapplyTemplates(started: Started) extends BleepCommand {
  override def run(): Unit = {
    val normalizedBuild = normalizeBuild(started.build)
    val build = Templates.reapply(normalizedBuild, started.rawBuild.templates)

    Files.writeString(
      started.buildPaths.bleepJsonFile,
      build.asJson.foldWith(ShortenAndSortJson).spaces2
    )
    ()
  }
}
