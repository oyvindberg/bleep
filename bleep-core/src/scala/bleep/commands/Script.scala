package bleep
package commands

import bleep.BleepException
import bleep.internal.traverseish
import bleep.model.ScriptDef

import scala.build.bloop.BloopServer

case class Script(name: model.ScriptName, args: List[String], watch: Boolean) extends BleepCommandRemote(watch) {
  override def watchableProjects(started: Started): Array[model.CrossProjectName] =
    started.build
      .scripts(name)
      .values
      .map { case model.ScriptDef.Main(project, _, _) =>
        project
      }
      .distinct
      .toArray

  override def runWithServer(started: Started, bloop: BloopServer): Either[BleepException, Unit] =
    Script.run(started, bloop, started.build.scripts(name).values, args, watch)
}

object Script {
  def run(started: Started, bloop: BloopServer, scriptDefs: Seq[ScriptDef], args: List[String], watch: Boolean): Either[BleepException, Unit] =
    traverseish.runAll(scriptDefs) { case model.ScriptDef.Main(project, main, _) =>
      Run(project, Some(main), args = args, raw = false, watch = watch).runWithServer(started, bloop)
    }
}
