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
}

object BuildHistory {
  val empty: BuildHistory = BuildHistory(None, None)
}

/** A single build run with its raw events */
case class BuildRun(
    timestampMs: Long,
    mode: String, // "compile" or "test"
    events: List[BleepBspProtocol.Event]
)
