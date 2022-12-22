package bleep
package commands

import bleep.BleepException
import cats.syntax.traverse._

import scala.build.bloop.BloopServer

case class Script(name: model.ScriptName, args: List[String], watch: Boolean) extends BleepCommandRemote(watch) {
  override def chosenProjects(started: Started): Array[model.CrossProjectName] =
    started.build.scripts(name).values.map(_.project).distinct.toArray

  override def runWithServer(started: Started, bloop: BloopServer): Either[BleepException, Unit] =
    started.build
      .scripts(name)
      .values
      .traverse { case model.ScriptDef(project, main) => Run(project, Some(main), args = args, raw = false, watch = false).runWithServer(started, bloop) }
      .map(_ => ())
}
