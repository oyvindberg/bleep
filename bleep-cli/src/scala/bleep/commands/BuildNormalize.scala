package bleep
package commands

import bleep.rewrites.normalizeBuild

case class BuildNormalize(started: Started) extends BleepCommand {
  override def run(): Either[BleepException, Unit] = {
    val build = started.build.requireFileBacked(ctx = "command normalize")
    val build1 = normalizeBuild(build)
    yaml.writeShortened(build1.file, started.buildPaths.bleepYamlFile)
    Right(())
  }
}
