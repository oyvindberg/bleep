package bleep
package commands

import bleep.rewrites.normalizeBuild
import bleep.templates.templatesReapply

object BuildReapplyTemplates extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val build = started.build.requireFileBacked(ctx = "command templates-reapply")
    val build1 = normalizeBuild(build)
    val build2 = templatesReapply(build1)
    val build3 = normalizeBuild(build2)
    yaml.writeShortened(build3.file, started.buildPaths.bleepYamlFile)
    Right(())
  }
}
