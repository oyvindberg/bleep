package bleep
package commands

import bleep.BleepException
import cats.syntax.traverse._

import scala.build.bloop.BloopServer

case class Script(started: Started, name: model.ScriptName, args: List[String]) extends BleepCommandRemote(started) {
  override def runWithServer(bloop: BloopServer): Either[BleepException, Unit] =
    started.build
      .scripts(name)
      .values
      .traverse { case model.ScriptDef(project, main) => Run(started, project, Some(main), args = args, raw = false).runWithServer(bloop) }
      .map(_ => ())
}
