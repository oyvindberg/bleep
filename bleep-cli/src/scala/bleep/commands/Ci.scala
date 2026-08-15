package bleep
package commands

import bleep.history.DiffBase
import bleep.internal.TransitiveProjects

/** One CI-shaped run: a single build pass over a project selection, optionally scoped to what a git diff invalidated, optionally repeated under `--watch`.
  *
  * Three commands are built out of this:
  *   - `bleep ci` — compile everything and test everything, in ONE pass
  *   - `bleep compile --invalidated[=<ref>]`
  *   - `bleep test --invalidated[=<ref>]`
  *
  * Why it exists: the recipe this replaces was {{{bleep build invalidated --base origin/main | xargs bleep compile}}} and it fails open. When nothing is
  * invalidated `bleep build invalidated` prints nothing, `xargs` runs the command anyway with no arguments, and `bleep compile` with no arguments compiles the
  * whole build — the "only build what changed" pipeline quietly turning into a full build precisely when there was nothing to do. Here the selection never
  * leaves the process: an empty invalidated set builds nothing, and says so.
  */
case class Ci(
    phase: Ci.Phase,
    scope: Ci.Scope,
    watch: Boolean
) extends BleepBuildCommand {

  override def run(started: Started): Either[BleepException, Unit] =
    if (watch) WatchMode.run(started, s => TransitiveProjects(s.build, phase.select(s, None)))(runOnce)
    else runOnce(started)

  private[bleep] def runOnce(started: Started): Either[BleepException, Unit] = {
    val projects: Array[model.CrossProjectName] =
      scope match {
        case Ci.Scope.Everything(explicit) =>
          phase.select(started, explicit)

        case Ci.Scope.Invalidated(maybeBase) =>
          val base = BuildInvalidated.resolveBase(started, maybeBase)
          val invalidated = BuildInvalidated.compute(started, base)
          if (invalidated.isEmpty) {
            // The whole point of the flag. Nothing invalidated means nothing is built — never "no arguments, so build everything".
            started.logger.info(s"Nothing invalidated vs $base. Nothing to ${phase.label}.")
            return Right(())
          }
          started.logger.info(s"Invalidated vs $base: ${invalidated.size} project(s): ${invalidated.toList.map(_.value).mkString(", ")}")
          phase.select(started, Some(invalidated.toArray))
      }

    if (projects.isEmpty) {
      started.logger.info(s"No projects to ${phase.label}")
      Right(())
    } else
      // `watch = false`: when we are watching, the loop is ours and each cycle is one full pass.
      phase.command(projects, cycleDiffBase(started), false).run(started)
  }

  /** Under `--watch` the loop is ours, so each cycle builds a command with `watch = false` — and bare `--diff` then resolves to "the most recent entry of this
    * mode", which after the first cycle is the previous cycle's entry. That is exactly the rolling behaviour of `bleep compile --watch --diff`. The one case
    * that differs is the very first cycle with no history at all, where the strict resolution inside [[ReactiveBsp]] would fail the run over a missing base;
    * drop the flag for that single cycle instead, which is what rolling does too.
    */
  private def cycleDiffBase(started: Started): Option[DiffBase] =
    phase.diffBase match {
      case Some(DiffBase.Previous) if watch && DiffBase.previous(started.buildPaths, phase.historyMode).isEmpty => None
      case other                                                                                                => other
    }
}

object Ci {

  /** Which projects a run acts on. */
  sealed trait Scope
  object Scope {

    /** No `--invalidated`: the projects named on the command line, or the phase's own default when none were named. */
    case class Everything(explicit: Option[Array[model.CrossProjectName]]) extends Scope

    /** `--invalidated[=<ref>]`: whatever a diff against `base` invalidated, recomputed on every watch cycle. `None` resolves to this branch's upstream, see
      * [[BuildInvalidated.resolveBase]].
      */
    case class Invalidated(base: Option[String]) extends Scope
  }

  /** What a run does.
    *
    * @param label
    *   how the phase is named in the messages that explain an empty selection ("compile", "test", "compile and test").
    * @param historyMode
    *   the run-history mode this phase records under, and therefore what a bare `--diff` resolves against: `"compile"` or `"test"`.
    * @param select
    *   turns candidate projects into the ones this phase acts on. `None` means "no candidates given", i.e. everything this phase would build on its own.
    * @param diffBase
    *   the `--diff` base as parsed from the command line, before the per-cycle adjustment in [[Ci.cycleDiffBase]].
    * @param command
    *   the real command, given the final project selection, the diff base for this cycle, and whether it drives its own watch loop. [[Ci]] always passes
    *   `false` — it owns the loop — but the same phase describes plain `bleep compile` / `bleep test`, which do let [[ReactiveBsp]] watch.
    */
  case class Phase(
      label: String,
      historyMode: String,
      select: (Started, Option[Array[model.CrossProjectName]]) => Array[model.CrossProjectName],
      diffBase: Option[DiffBase],
      command: (Array[model.CrossProjectName], Option[DiffBase], Boolean) => ReactiveBsp
  )

  def compilePhase(
      displayMode: DisplayMode,
      flamegraph: Boolean,
      cancel: Boolean,
      diffBase: Option[DiffBase],
      diffOutput: OutputMode
  ): Phase =
    Phase(
      label = "compile",
      historyMode = "compile",
      select = (started, candidates) => started.chosenProjects(candidates),
      diffBase = diffBase,
      command = (projects, cycleDiffBase, watch) =>
        ReactiveBsp.compile(
          watch = watch,
          projects = projects,
          displayMode = displayMode,
          flamegraph = flamegraph,
          cancel = cancel,
          diffBase = cycleDiffBase,
          diffOutput = diffOutput
        )
    )

  /** `bleep test`: the test projects among the candidates. Their upstream dependencies still get compiled — the test task graph compiles the transitive closure
    * of whatever it is asked to test — but a non-test project is not itself a target.
    */
  def testPhase(
      displayMode: DisplayMode,
      jvmOptions: List[String],
      testArgs: List[String],
      only: List[String],
      exclude: List[String],
      includeTags: List[String],
      excludeTags: List[String],
      flamegraph: Boolean,
      cancel: Boolean,
      junitReportDir: Option[java.nio.file.Path],
      diffBase: Option[DiffBase],
      diffOutput: OutputMode,
      clientEnv: Map[String, String]
  ): Phase =
    testCommandPhase(
      label = "test",
      select = (started, candidates) => started.chosenTestProjects(candidates),
      displayMode = displayMode,
      jvmOptions = jvmOptions,
      testArgs = testArgs,
      only = only,
      exclude = exclude,
      includeTags = includeTags,
      excludeTags = excludeTags,
      flamegraph = flamegraph,
      cancel = cancel,
      junitReportDir = junitReportDir,
      diffBase = diffBase,
      diffOutput = diffOutput,
      clientEnv = clientEnv
    )

  /** `bleep ci`: compile everything and test everything, as ONE build pass rather than a compile run followed by a test run.
    *
    * The trick is that this is just a test run whose targets are *every* project instead of only the test projects. The test task graph already compiles the
    * transitive closure of its targets, so handing it the whole build gives one task graph that compiles every project — libraries upstream of a test project,
    * and projects no test depends on at all — and runs the suites of the projects that have them, at full parallelism. A project with no test suites simply
    * discovers none and is left compiled, which is what `bleep test some-library` has always done.
    *
    * Running compile and then test instead would compile everything twice over (the second pass finding it up to date, but still paying a full BSP round trip
    * and serializing the two halves), and would report two summaries and two history entries for one CI job.
    */
  def ciPhase(
      displayMode: DisplayMode,
      jvmOptions: List[String],
      testArgs: List[String],
      includeTags: List[String],
      excludeTags: List[String],
      flamegraph: Boolean,
      cancel: Boolean,
      junitReportDir: Option[java.nio.file.Path],
      diffBase: Option[DiffBase],
      diffOutput: OutputMode,
      clientEnv: Map[String, String]
  ): Phase =
    testCommandPhase(
      label = "compile and test",
      select = (started, candidates) => started.chosenProjects(candidates),
      displayMode = displayMode,
      jvmOptions = jvmOptions,
      testArgs = testArgs,
      only = Nil,
      exclude = Nil,
      includeTags = includeTags,
      excludeTags = excludeTags,
      flamegraph = flamegraph,
      cancel = cancel,
      junitReportDir = junitReportDir,
      diffBase = diffBase,
      diffOutput = diffOutput,
      clientEnv = clientEnv
    )

  private def testCommandPhase(
      label: String,
      select: (Started, Option[Array[model.CrossProjectName]]) => Array[model.CrossProjectName],
      displayMode: DisplayMode,
      jvmOptions: List[String],
      testArgs: List[String],
      only: List[String],
      exclude: List[String],
      includeTags: List[String],
      excludeTags: List[String],
      flamegraph: Boolean,
      cancel: Boolean,
      junitReportDir: Option[java.nio.file.Path],
      diffBase: Option[DiffBase],
      diffOutput: OutputMode,
      clientEnv: Map[String, String]
  ): Phase =
    Phase(
      label = label,
      historyMode = "test",
      select = select,
      diffBase = diffBase,
      command = (projects, cycleDiffBase, watch) =>
        ReactiveBsp.test(
          watch = watch,
          projects = projects,
          displayMode = displayMode,
          jvmOptions = jvmOptions,
          testArgs = testArgs,
          only = only,
          exclude = exclude,
          includeTags = includeTags,
          excludeTags = excludeTags,
          flamegraph = flamegraph,
          cancel = cancel,
          junitReportDir = junitReportDir,
          diffBase = cycleDiffBase,
          diffOutput = diffOutput,
          clientEnv = clientEnv
        )
    )
}
