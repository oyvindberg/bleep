package bleep

import bleep.commands.PublishLocal

class Commands(started: Started) {
  private def force(cmd: BleepCommand): Unit =
    cmd.run().orThrow

  def clean(projects: List[model.CrossProjectName]): Unit =
    force(commands.Clean(started, projects.toArray))

  def compile(projects: List[model.CrossProjectName]): Unit =
    force(commands.Compile(started, projects.toArray))

  def run(project: model.CrossProjectName, maybeOverriddenMain: Option[String] = None, args: List[String] = Nil): Unit =
    force(commands.Run(started, project, maybeOverriddenMain, args))

  def test(projects: List[model.CrossProjectName]): Unit =
    force(commands.Test(started, projects.toArray))

  def script(name: model.ScriptName, args: List[String]): Unit =
    force(commands.Script(started, name, args))

  def publishLocal(options: PublishLocal.Options): Unit =
    force(commands.PublishLocal(started, options))
}
