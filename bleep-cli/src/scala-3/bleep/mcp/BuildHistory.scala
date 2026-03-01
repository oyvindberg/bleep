package bleep.mcp

import bleep.bsp.protocol.BleepBspProtocol
import bleep.testing.PreviousRunState

/** Two-slot ring buffer storing the last two build runs for diff computation.
  *
  * Used by the MCP server to diff consecutive compile/test results. The `last` field holds the most recent run, `previous` holds the one before it.
  */
case class BuildHistory(
    last: Option[BuildRun],
    previous: Option[BuildRun]
) {
  def push(run: BuildRun): BuildHistory =
    BuildHistory(last = Some(run), previous = last)

  /** Build a PreviousRunState from the last run's events for diff computation */
  def previousRunState: PreviousRunState =
    last match {
      case Some(run) => PreviousRunState.fromProtocolEvents(run.events)
      case None      => PreviousRunState.empty
    }

  /** Drop events for the given projects from both runs (e.g. after clean) */
  def dropProjects(projectValues: Set[String]): BuildHistory =
    BuildHistory(
      last = last.map(_.dropProjects(projectValues)),
      previous = previous.map(_.dropProjects(projectValues))
    )
}

object BuildHistory {
  val empty: BuildHistory = BuildHistory(None, None)
}

/** A single build run with its raw events */
case class BuildRun(
    timestampMs: Long,
    mode: String, // "compile" or "test"
    events: List[BleepBspProtocol.Event]
) {

  /** Remove events for the given projects so diff state doesn't reference stale data after clean */
  def dropProjects(projectValues: Set[String]): BuildRun = {
    import BleepBspProtocol.Event as E
    val filtered = events.filter {
      case e: E.CompileStarted    => !projectValues.contains(e.project)
      case e: E.CompileFinished   => !projectValues.contains(e.project)
      case e: E.CompileProgress   => !projectValues.contains(e.project)
      case e: E.SuiteStarted      => !projectValues.contains(e.project)
      case e: E.SuiteFinished     => !projectValues.contains(e.project)
      case e: E.SuiteError        => !projectValues.contains(e.project)
      case e: E.SuiteTimedOut     => !projectValues.contains(e.project)
      case e: E.TestStarted       => !projectValues.contains(e.project)
      case e: E.TestFinished      => !projectValues.contains(e.project)
      case e: E.LinkStarted       => !projectValues.contains(e.project)
      case e: E.LinkFinished      => !projectValues.contains(e.project)
      case _                      => true // keep non-project events (BuildFinished, TestRunFinished, etc.)
    }
    copy(events = filtered)
  }
}
