package bleep
package commands

import bleep.logging.Logger
import bleep.{BleepException, Lazy}

case class CompileServerSetMode(logger: Logger, userPaths: UserPaths, lazyResolver: Lazy[CoursierResolver], mode: model.CompileServerMode)
    extends BleepCommand {
  override def run(): Either[BleepException, Unit] =
    BleepConfigOps.rewritePersisted(logger, userPaths)(_.copy(compileServerMode = mode)).map(_ => ())
}
