package bleep

class Commands(started: Started) {
  private def force(cmd: BleepCommand): Unit =
    cmd.run() match {
      case Left(th) => throw th
      case Right(_) => ()
    }

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
}
