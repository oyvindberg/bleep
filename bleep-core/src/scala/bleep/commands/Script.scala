package bleep
package commands

import bleep.internal.{traverseish, TransitiveProjects}
import bloop.rifle.BuildServer

case class Script(name: model.ScriptName, args: List[String], watch: Boolean) extends BleepCommandRemote(watch) {
  override def watchableProjects(started: Started): TransitiveProjects = {
    val scriptProjects = started.build
      .scripts(name)
      .values
      .map { case model.ScriptDef.Main(project, _, _) => project }
      .distinct
      .toArray
    TransitiveProjects(started.build, scriptProjects)
  }

  override def runWithServer(started: Started, bloop: BuildServer): Either[BleepException, Unit] =
    Script.run(started, bloop, started.build.scripts(name).values, args, watch)
}

object Script {
  def run(started: Started, bloop: BuildServer, scriptDefs: Seq[model.ScriptDef], args: List[String], watch: Boolean): Either[BleepException, Unit] =
    traverseish.runAll(scriptDefs) { case model.ScriptDef.Main(project, main, _) =>
      Run(project, Some(main), args = args, raw = true, watch = watch).runWithServer(started, bloop)
    }
}
