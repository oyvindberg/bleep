package bleep
package commands

import cats.syntax.traverse._

import scala.build.bloop.BloopServer

case class Script(started: Started, name: model.ScriptName, scriptDefs: JsonList[model.ScriptDef], args: List[String]) extends BleepCommandRemote {
  override def runWithServer(bloop: BloopServer): Either[BuildException, Unit] =
    scriptDefs.values
      .traverse { case model.ScriptDef(project, main) => new Run(started, project, Some(main), args = Nil).run() }
      .map(_ => ())
}
