package bleep
package commands

import bleep.rewrites.normalizeBuild

object BuildNormalize extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val build = started.build.requireFileBacked(ctx = "command normalize")
    val build1 = normalizeBuild(build)
    yaml.writeShortened(build1.file, started.buildPaths.bleepYamlFile)
    Right(())
  }
}
