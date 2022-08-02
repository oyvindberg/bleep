package bleep
package commands

import bleep.internal.{formatsCrossProjectName, FileUtils, Templates}
import bleep.model.ExplodedBuild
import bleep.rewrites.{normalizeBuild, Defaults}
import bleep.toYaml.asYamlString

case class BuildReinferTemplates(started: Started, ignoreWhenInferringTemplates: Set[model.ProjectName]) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val normalizedBuild = normalizeBuild(started.build)
    val droppedTemplates = normalizedBuild.dropTemplates

    val build = Templates(started.logger, droppedTemplates, ignoreWhenInferringTemplates)

    // fail if we have done illegal rewrites during templating
    ExplodedBuild.diffProjects(Defaults.add(normalizedBuild).dropTemplates, ExplodedBuild.of(build).dropTemplates) match {
      case empty if empty.isEmpty => ()
      case diffs =>
        started.logger.error("Project templating did illegal rewrites. Please report this as a bug")
        diffs.foreach { case (projectName, msg) => started.logger.withContext(projectName).error(msg) }
    }

    FileUtils.writeString(
      started.buildPaths.bleepYamlFile,
      asYamlString(build)
    )
    Right(())
  }
}
