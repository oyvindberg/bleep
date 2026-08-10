package bleep.mcp

import bleep.bsp.protocol.BleepBspProtocol

/** A completed compile/test request, kept so `bleep.details` can return the full transcript later.
  *
  * Entries are keyed by an explicit request id and never claim to describe current build state — they are the record of what THAT run produced. This is what
  * makes them immune to the staleness that killed the old "last build" cache: a compile run outside MCP doesn't invalidate the transcript of request #42, it
  * just isn't request #42.
  */
case class CompletedRequest(
    requestId: Long,
    timestampMs: Long,
    workspace: String,
    mode: String, // "compile" or "test"
    events: List[BleepBspProtocol.Event],
    testRunResult: Option[BleepBspProtocol.TestRunResult]
)

/** Bounded ring of completed requests, newest last. Ids are monotonically increasing and never reused. */
case class RequestLog(nextId: Long, entries: Vector[CompletedRequest]) {
  def push(
      timestampMs: Long,
      workspace: String,
      mode: String,
      events: List[BleepBspProtocol.Event],
      testRunResult: Option[BleepBspProtocol.TestRunResult]
  ): (RequestLog, Long) = {
    val entry = CompletedRequest(nextId, timestampMs, workspace, mode, events, testRunResult)
    (RequestLog(nextId + 1, (entries :+ entry).takeRight(RequestLog.MaxEntries)), nextId)
  }

  def byId(requestId: Long): Option[CompletedRequest] = entries.find(_.requestId == requestId)

  def latest: Option[CompletedRequest] = entries.lastOption
}

object RequestLog {
  val MaxEntries = 32
  val empty: RequestLog = RequestLog(nextId = 1, entries = Vector.empty)
}
