package bleep

import bleep.commands.PublishLocal

class Commands(started: Started) {
  private def force(cmd: BleepCommand): Unit =
    cmd.run().orThrow

  def clean(projects: List[model.CrossProjectName]): Unit =
    force(commands.Clean(started, projects.toArray))

  def compile(projects: List[model.CrossProjectName], watch: Boolean = false): Unit =
    force(commands.Compile(started, watch, projects.toArray))

  def run(
      project: model.CrossProjectName,
      maybeOverriddenMain: Option[String] = None,
      args: List[String] = Nil,
      raw: Boolean = false,
      watch: Boolean = false
  ): Unit =
    force(commands.Run(started, project, maybeOverriddenMain, args, raw, watch))

  def test(projects: List[model.CrossProjectName], watch: Boolean = false): Unit =
    force(commands.Test(started, watch, projects.toArray))

  def script(name: model.ScriptName, args: List[String], watch: Boolean = false): Unit =
    force(commands.Script(started, name, args, watch))

  def publishLocal(options: PublishLocal.Options, watch: Boolean = false): Unit =
    force(commands.PublishLocal(started, watch, options))
}
