package bleep
package commands

import bleep.internal.writeYamlLogged
import bleep.rewrites.normalizeBuild
import bleep.templates.templatesReapply

object BuildReapplyTemplates extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val build = started.build.requireFileBacked(ctx = "command templates-reapply")
    val buildPaths = started.buildPaths
    val newBuild = normalize(build, buildPaths)
    Right(writeYamlLogged(started.logger, "Wrote update build", newBuild.file, buildPaths.bleepYamlFile))
  }

  def normalize(build0: model.Build.FileBacked, buildPaths: BuildPaths): model.Build.FileBacked = {
    val build1 = normalizeBuild(build0, buildPaths)
    val build2 = templatesReapply(build1)
    val build3 = normalizeBuild(build2, buildPaths)
    build3
  }
}
