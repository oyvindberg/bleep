package bleep

import bleep.logging.Logger

/** At this point we assert that we *have* a build. it's not necessarily loaded yet
  */
case class Prebootstrapped(logger: Logger, userPaths: UserPaths, buildPaths: BuildPaths, existingBuild: BuildLoader.Existing) {
  def fresh: Either[BleepException, Prebootstrapped] =
    BuildLoader
      .inDirectory(existingBuild.buildDirectory)
      .existing
      .map(newExisting => copy(existingBuild = newExisting))
}

object Prebootstrapped {
  def apply(buildPaths: BuildPaths, logger: Logger, existing: BuildLoader.Existing): Prebootstrapped = {
    val userPaths = UserPaths.fromAppDirs
    Prebootstrapped(logger, userPaths, buildPaths, existing)
  }
}
