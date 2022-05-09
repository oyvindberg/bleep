package bleep
package commands

import cats.syntax.traverse._

import scala.build.bloop.BloopServer

case class Script(started: Started, name: model.ScriptName, args: List[String]) extends BleepCommandRemote(started) {
  override def runWithServer(bloop: BloopServer): Either[BuildException, Unit] =
    started.build
      .scripts(name)
      .values
      .traverse { case model.ScriptDef(project, main) => new Run(started, project, Some(main), args = Nil).runWithServer(bloop) }
      .map(_ => ())
}
