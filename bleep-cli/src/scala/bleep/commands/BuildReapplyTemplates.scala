package bleep
package commands

import bleep.internal.Templates
import bleep.rewrites.normalizeBuild

case class BuildReapplyTemplates(started: Started) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val normalizedBuild = normalizeBuild(started.build)
    val build = Templates.reapply(normalizedBuild, started.rawBuild.templates)
    yaml.writeShortened(build, started.buildPaths.bleepYamlFile)
    Right(())
  }
}
