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
      .map {
        case model.ScriptDef.Main(project, _, _) => project
        case model.ScriptDef.Shell(_, _, _)      => throw new BleepException.Text("cannot `--watch shell` scripts for now")
      }
      .distinct
      .toArray

  override def runWithServer(started: Started, bloop: BloopServer): Either[BleepException, Unit] =
    Script.run(started, bloop, started.build.scripts(name).values, args, watch)
}

object Script {
  def run(started: Started, bloop: BloopServer, scriptDefs: Seq[ScriptDef], args: List[String], watch: Boolean): Either[BleepException, Unit] =
    traverseish.runAll(scriptDefs) {
      case model.ScriptDef.Main(project, main, _) =>
        Run(project, Some(main), args = args, raw = false, watch = watch).runWithServer(started, bloop)
      case shell @ model.ScriptDef.Shell(_, _, _) =>
        val cmd = internal.ScriptShelloutCommand.getForShellScript(shell)
        try {
          cli(
            action = "run script",
            cwd = started.buildPaths.cwd,
            cmd = cmd,
            logger = started.logger,
            out = cli.Out.ViaLogger(started.logger),
            in = cli.In.Attach
          )
          Right(())
        } catch {
          case x: BleepException => Left(x)
        }
    }
}
