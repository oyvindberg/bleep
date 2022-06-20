package bleep
package commands

import bleep.internal.{FileUtils, Templates, asYamlString}
import bleep.rewrites.normalizeBuild

case class BuildReapplyTemplates(started: Started) extends BleepCommand {
  override def run(): Either[BuildException, Unit] = {
    val normalizedBuild = normalizeBuild(started.build)
    val build = Templates.reapply(normalizedBuild, started.rawBuild.templates)

    FileUtils.writeString(started.buildPaths.bleepYamlFile, asYamlString(build))
    Right(())
  }
}
