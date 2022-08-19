package bleep
package commands

import bleep.rewrites.normalizeBuild
import bleep.templates.templatesReapply

case class BuildReapplyTemplates(started: Started) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val build = started.build.requireFileBacked(ctx = "command templates-reapply")
    val build1 = normalizeBuild(build)
    val build2 = templatesReapply(build1)
    yaml.writeShortened(build2.file, started.buildPaths.bleepYamlFile)
    Right(())
  }
}
