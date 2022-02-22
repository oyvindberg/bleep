package bleep
package commands

import bleep.internal.{ShortenAndSortJson, Templates}
import bleep.rewrites.normalizeBuild
import io.circe.syntax._

import java.nio.file.Files

case class BuildReinferTemplates(started: Started, ignoreWhenInferringTemplates: Set[model.ProjectName]) extends BleepCommand {
  override def run(): Unit = {
    val normalizedBuild = normalizeBuild(started.build)
    val droppedTemplates = normalizedBuild.dropTemplates

    val build = Templates(droppedTemplates, ignoreWhenInferringTemplates)

    // fail if we have done illegal rewrites during templating
    ExplodedBuild.diffProjects(Defaults.add(normalizedBuild).dropTemplates, ExplodedBuild.of(build).dropTemplates) match {
      case empty if empty.isEmpty => ()
      case diffs =>
        started.logger.error("Project templating did illegal rewrites. Please report this as a bug")
        diffs.foreach { case (projectName, msg) => started.logger.withContext(projectName).error(msg) }
    }

    Files.writeString(
      started.buildPaths.bleepJsonFile,
      build.asJson.foldWith(ShortenAndSortJson).spaces2
    )
    ()
  }
}
