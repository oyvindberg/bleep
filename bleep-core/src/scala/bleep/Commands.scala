package bleep

import bleep.commands.PublishLocalOptions

class Commands(started: Started) {
  private def force(cmd: BleepCommand): Unit =
    cmd.run().orThrow

  def clean(projects: List[model.CrossProjectName]): Unit =
    force(commands.Clean(started, projects))

  def compile(projects: List[model.CrossProjectName]): Unit =
    force(commands.Compile(started, projects))

  def run(project: model.CrossProjectName, maybeOverriddenMain: Option[String] = None, args: List[String] = Nil): Unit =
    force(commands.Run(started, project, maybeOverriddenMain, args))

  def test(projects: List[model.CrossProjectName]): Unit =
    force(commands.Test(started, projects))

  def script(name: model.ScriptName, args: List[String]): Unit =
    force(commands.Script(started, name, args))

  def publishLocal(options: PublishLocalOptions): Unit =
    force(commands.PublishLocal(started, options))
}
