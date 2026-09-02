package bleep

import bleep.commands.{LinkOptions, Publish, PublishLocal, PublishSonatype}
import bleep.testing.BuildSummary
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

  /** Compile `projects`, returning what the run reported.
    *
    * A failed compile still throws, so a caller who only wants "did it work" can go on ignoring the result. The summary is there for the caller who needs to
    * know more than that — [[bleep.testing.BuildSummary.noOp]] answers "did anything actually recompile", which is the signal a deploy step needs to skip
    * itself when the previous run already produced the same artifacts.
    *
    * Under `watch = true` there is no single run to summarise, so this returns [[bleep.testing.BuildSummary.empty]] when the watch ends.
    */
  def compile(projects: List[model.CrossProjectName], watch: Boolean = false): BuildSummary = {
    val cmd = commands.ReactiveBsp
      .compile(watch, projects.toArray, commands.DisplayMode.NoTui, flamegraph = false, cancel = false, diffBase = None, diffOutput = OutputMode.Text)
    if (watch) {
      force(cmd)
      BuildSummary.empty
    } else cmd.runReportingSummary(started).orThrow
  }

  /** Link `projects` — Scala.js, Scala Native, Kotlin/JS or Kotlin/Native — the way `bleep link` does.
    *
    * [[bleep.commands.LinkOptions.Debug]] and [[bleep.commands.LinkOptions.Release]] are the two a caller usually wants; the rest of the fields map one to one
    * onto the command line's flags.
    *
    * [[bleep.testing.BuildSummary.linkedOutputs]] is where the link put things, taken from the linker rather than from the directory layout. A caller that
    * needs the linked JavaScript — to copy it into a jar, to serve it — reads it from there instead of rebuilding `link-output/<mode>/js/main.js` by hand,
    * which is a path bleep owns and has already changed once.
    *
    * Under `watch = true` there is no single run to summarise, so this returns [[bleep.testing.BuildSummary.empty]] when the watch ends.
    */
  def link(projects: List[model.CrossProjectName], options: LinkOptions, watch: Boolean = false): BuildSummary = {
    val cmd = commands.ReactiveBsp.link(watch, projects.toArray, commands.DisplayMode.NoTui, options, flamegraph = false, cancel = false)
    if (watch) {
      force(cmd)
      BuildSummary.empty
    } else cmd.runReportingSummary(started).orThrow
  }

  def run(
      project: model.CrossProjectName,
      maybeOverriddenMain: Option[String] = None,
      args: List[String] = Nil,
      raw: Boolean = false,
      watch: Boolean = false
  ): Unit =
    force(commands.Run(project, maybeOverriddenMain, args, raw, watch, noTuiBuildOpts))

  /** Run the tests in `projects`, returning what the run reported.
    *
    * A failing test still throws, so the summary describes a run in which everything passed: counts, suites, durations. The detail a caller most wants on a
    * *failing* run — `failures`, `cancelledSuites` — is therefore out of reach here, because the throw gets there first. Ask for a non-throwing entry point if
    * you need to inspect failures rather than propagate them.
    */
  def test(
      projects: List[model.CrossProjectName],
      watch: Boolean = false,
      only: Option[NonEmptyList[String]],
      exclude: Option[NonEmptyList[String]],
      includeTags: Option[NonEmptyList[String]],
      excludeTags: Option[NonEmptyList[String]]
  ): BuildSummary = {
    val cmd =
      commands.ReactiveBsp.test(
        watch = watch,
        projects = projects.toArray,
        displayMode = commands.DisplayMode.NoTui,
        jvmOptions = Nil,
        testArgs = Nil,
        only = only.map(_.toList).getOrElse(Nil),
        exclude = exclude.map(_.toList).getOrElse(Nil),
        includeTags = includeTags.map(_.toList).getOrElse(Nil),
        excludeTags = excludeTags.map(_.toList).getOrElse(Nil),
        flamegraph = false,
        cancel = false,
        junitReportDir = None,
        diffBase = None,
        diffOutput = OutputMode.Text,
        clientEnv = bleep.bsp.protocol.BleepBspProtocol.ClientEnv.current(noColor = bleep.PreBootstrapOpts.noColorRequested)
      )
    if (watch) {
      force(cmd)
      BuildSummary.empty
    } else cmd.runReportingSummary(started).orThrow
  }

  def script(name: model.ScriptName, args: List[String], watch: Boolean = false): Unit =
    force(commands.Script(name, args, watch))

  def publishLocal(options: PublishLocal.Options, watch: Boolean = false): Unit =
    force(commands.PublishLocal(watch, options, noTuiBuildOpts))

  def publish(options: Publish.Options, watch: Boolean = false): Unit =
    force(commands.Publish(watch, options, noTuiBuildOpts))

  def publishSonatype(options: PublishSonatype.Options): Unit =
    force(commands.PublishSonatype(options, noTuiBuildOpts))
}
