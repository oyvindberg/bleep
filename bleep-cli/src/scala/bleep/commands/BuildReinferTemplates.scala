package bleep
package commands

import bleep.internal.BleepTemplateLogger
import bleep.rewrites.{normalizeBuild, Defaults}
import bleep.templates.templatesInfer

case class BuildReinferTemplates(started: Started, ignoreWhenInferringTemplates: Set[model.ProjectName]) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    // require that the build is from file, which means it may have templates
    val build0 = started.build.requireFileBacked(ctx = "command templates-generate-new")

    // normalize to make results of template inference better. drop existing file/template structure
    val normalizedBuild = normalizeBuild(build0.dropBuildFile.dropTemplates)

    val newBuildFile = templatesInfer(
      logger = new BleepTemplateLogger(started.logger),
      build = normalizedBuild,
      ignoreWhenInferringTemplates
    )

    // fail if we have done illegal rewrites during templating
    model.Build.diffProjects(
      before = Defaults.add(normalizedBuild),
      after = model.Build.FileBacked(newBuildFile).dropBuildFile.dropTemplates
    ) match {
      case empty if empty.isEmpty => ()
      case diffs =>
        started.logger.error("Project templating did illegal rewrites. Please report this as a bug")
        diffs.foreach { case (projectName, msg) => started.logger.withContext(projectName).error(msg) }
    }

    yaml.writeShortened(newBuildFile, started.buildPaths.bleepYamlFile)
    Right(())
  }
}
