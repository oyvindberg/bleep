package bleep
package commands

import bleep.internal.writeYamlLogged
import bleep.rewrites.normalizeBuild

object BuildNormalize extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val build = started.build.requireFileBacked(ctx = "command normalize")
    val build1 = normalizeBuild(build, started.buildPaths)
    Right(writeYamlLogged(started.logger, "Wrote update build", build1.file, started.buildPaths.bleepYamlFile))
  }
}
