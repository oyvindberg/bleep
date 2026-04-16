package bleep

import bleep.commands.PublishLocal
import cats.data.NonEmptyList

class Commands(started: Started) {
  private def force(cmd: BleepBuildCommand): Unit =
    cmd.run(started).orThrow

  private val noTuiBuildOpts: commands.CommonBuildOpts =
    commands.CommonBuildOpts(
      displayMode = commands.DisplayMode.NoTui,
      flamegraph = false,
      cancel = false
    )

  def clean(projects: List[model.CrossProjectName]): Unit =
    force(commands.Clean(projects.toArray))

  def compile(projects: List[model.CrossProjectName], watch: Boolean): Unit =
    force(commands.ReactiveBsp.compile(watch, projects.toArray, commands.DisplayMode.NoTui, flamegraph = false, cancel = false, noCache = false))

  def compile(projects: List[model.CrossProjectName]): Unit =
    compile(projects, watch = false)

  def run(
      project: model.CrossProjectName,
      maybeOverriddenMain: Option[String] = None,
      args: List[String] = Nil,
      raw: Boolean = false,
      watch: Boolean = false
  ): Unit =
    force(commands.Run(project, maybeOverriddenMain, args, raw, watch, noTuiBuildOpts))

  def test(
      projects: List[model.CrossProjectName],
      watch: Boolean = false,
      only: Option[NonEmptyList[String]],
      exclude: Option[NonEmptyList[String]]
  ): Unit =
    force(
      commands.ReactiveBsp.test(
        watch = watch,
        projects = projects.toArray,
        displayMode = commands.DisplayMode.NoTui,
        jvmOptions = Nil,
        testArgs = Nil,
        only = only.map(_.toList).getOrElse(Nil),
        exclude = exclude.map(_.toList).getOrElse(Nil),
        flamegraph = false,
        cancel = false,
        junitReportDir = None,
        noCache = false
      )
    )

  def script(name: model.ScriptName, args: List[String], watch: Boolean = false): Unit =
    force(commands.Script(name, args, watch))

  def publishLocal(options: PublishLocal.Options, watch: Boolean = false): Unit =
    force(commands.PublishLocal(watch, options, noTuiBuildOpts))
}
