package bleep
package commands

import ryddig.Logger

case class CompileServerSetMode(logger: Logger, userPaths: UserPaths, mode: model.CompileServerMode) extends BleepCommand {
  override def run(): Either[BleepException, Unit] =
    BleepConfigOps.rewritePersisted(logger, userPaths)(_.copy(compileServerMode = Some(mode))).map(_ => ())
}
