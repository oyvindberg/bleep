package bleep
package commands

import bleep.internal.{FileUtils, Templates}
import bleep.rewrites.normalizeBuild
import bleep.toYaml.asYamlString

case class BuildReapplyTemplates(started: Started) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val normalizedBuild = normalizeBuild(started.build)
    val build = Templates.reapply(normalizedBuild, started.rawBuild.templates)

    FileUtils.writeString(started.buildPaths.bleepYamlFile, asYamlString(build))
    Right(())
  }
}
