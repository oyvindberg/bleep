package bleep
package commands

import bleep.internal.{writeYamlLogged, BleepTemplateLogger}
import bleep.rewrites.normalizeBuild
import bleep.templates.{mineTemplates, templatesInfer}

case class BuildReinferTemplates(ignoreWhenInferringTemplates: Set[model.ProjectName]) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    // require that the build is from file, which means it may have templates
    val build0 = started.build.requireFileBacked(ctx = "command templates-generate-new")

    // normalize to make results of template inference better. drop existing file/template structure
    val normalizedBuild = normalizeBuild(build0.dropBuildFile.dropTemplates, started.buildPaths)

    val newBuildFile = templatesInfer(
      logger = new BleepTemplateLogger(started.logger),
      build = normalizedBuild,
      ignoreWhenInferringTemplates,
      costs = mineTemplates.Costs.default
    )
    Right(writeYamlLogged(started.logger, "Wrote update build", newBuildFile, started.buildPaths.bleepYamlFile))
  }
}
