package bleep

import bleep.BuildPaths.Mode
import bleep.internal.Lazy
import bleep.logging.Logger

import java.nio.file.Path

case class Prebootstrapped(
    logger: Logger,
    userPaths: UserPaths,
    buildPaths: BuildPaths,
    lazyResolver: Lazy[CoursierResolver]
)

object Prebootstrapped {
  def find(cwd: Path, mode: Mode, logger: Logger): Either[BuildException, Prebootstrapped] =
    BuildPaths.find(cwd, mode).map(buildPaths => apply(buildPaths, logger))

  def apply(buildPaths: BuildPaths, logger: Logger): Prebootstrapped = {
    val userPaths = UserPaths.fromAppDirs
    Prebootstrapped(
      logger,
      userPaths,
      buildPaths,
      Lazy(CoursierResolver(logger, downloadSources = true, userPaths))
    )
  }
}
