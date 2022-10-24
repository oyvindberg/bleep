package bleep
package internal

import bleep.logging.Logger
import bleep.model.Build

object jvmOrSystem {
  def apply(started: Started): model.Jvm =
    apply(started.build, started.logger)

  def apply(build: model.Build, logger: Logger): model.Jvm =
    apply(
      build match {
        case Build.Exploded(_, _, _, jvm, _) => jvm
        case Build.FileBacked(file)          => file.jvm
      },
      logger
    )

  def apply(maybeJvm: Option[model.Jvm], logger: Logger): model.Jvm =
    maybeJvm match {
      case Some(jvm) => jvm
      case None =>
        logger.warn(
          s"You build uses the default system JVM, which can change outside the build. for stable builds over time, let bleep manage your chosen JVM by adding it to ${BuildLoader.BuildFileName}"
        )
        model.Jvm.system
    }
}
