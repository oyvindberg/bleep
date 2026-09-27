package bleep
package commands

import bleep.internal.traverseish

case class Script(name: model.ScriptName, args: List[String], watch: Boolean) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] =
    Script.run(started, started.build.scripts(name).values, args, watch)
}

object Script {
  private val scriptBuildOpts: CommonBuildOpts =
    CommonBuildOpts(DisplayMode.NoTui, flamegraph = false, cancel = false)

  def run(started: Started, scriptDefs: Seq[model.ScriptDef], args: List[String], watch: Boolean): Either[BleepException, Unit] =
    traverseish.runAll(scriptDefs) {
      case model.ScriptDef.Main(_, main, _, inputs) if inputs.values.nonEmpty =>
        // `inputs` orders a sourcegen script after the projects it reads; a script run from the command line has no build step to be ordered in
        Left(new BleepException.Text(s"script $main declares `inputs`, which only applies to `sourcegen:` scripts"))
      case model.ScriptDef.Main(project, main, _, _) =>
        Run(project, Some(main), args = args, raw = true, watch = watch, buildOpts = scriptBuildOpts).run(started)
    }
}
