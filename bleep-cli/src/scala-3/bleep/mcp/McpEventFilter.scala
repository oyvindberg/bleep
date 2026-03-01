package bleep.mcp

import bleep.bsp.protocol.BleepBspProtocol
import io.circe.Json

/** Filters BleepBspProtocol events to the subset that is actionable for an AI agent.
  *
  * Of the ~38 event variants, only 11 carry information an agent can act on. The rest are TUI progress noise (percentages, phase changes, process states) that
  * would waste context window.
  */
object McpEventFilter {

  /** Filter an event. Returns Some(json) for agent-actionable events, None for noise. */
  def filter(event: BleepBspProtocol.Event): Option[Json] = {
    import BleepBspProtocol.Event as E
    event match {
      case e: E.CompileStarted =>
        Some(
          Json.obj(
            "event" -> Json.fromString("CompileStarted"),
            "project" -> Json.fromString(e.project)
          )
        )

      case e: E.CompileFinished =>
        val fields = List.newBuilder[(String, Json)]
        fields += "event" -> Json.fromString("CompileFinished")
        fields += "project" -> Json.fromString(e.project)
        fields += "status" -> Json.fromString(e.status)
        fields += "durationMs" -> Json.fromLong(e.durationMs)
        if (e.diagnostics.nonEmpty) {
          fields += "diagnostics" -> Json.arr(
            e.diagnostics.map { d =>
              val diagFields = List.newBuilder[(String, Json)]
              diagFields += "severity" -> Json.fromString(d.severity)
              diagFields += "message" -> Json.fromString(stripAnsi(d.message))
              d.rendered.foreach(r => diagFields += "rendered" -> Json.fromString(stripAnsi(r)))
              d.path.foreach(p => diagFields += "path" -> Json.fromString(p))
              Json.obj(diagFields.result()*)
            }*
          )
        }
        e.skippedBecause.foreach { reason =>
          fields += "skippedBecause" -> Json.fromString(reason)
        }
        Some(Json.obj(fields.result()*))

      case e: E.TestFinished =>
        val fields = List.newBuilder[(String, Json)]
        fields += "event" -> Json.fromString("TestFinished")
        fields += "project" -> Json.fromString(e.project)
        fields += "suite" -> Json.fromString(e.suite)
        fields += "test" -> Json.fromString(e.test)
        fields += "status" -> Json.fromString(e.status)
        fields += "durationMs" -> Json.fromLong(e.durationMs)
        e.message.foreach(m => fields += "message" -> Json.fromString(m))
        e.throwable.foreach(t => fields += "throwable" -> Json.fromString(t))
        Some(Json.obj(fields.result()*))

      case e: E.SuiteFinished =>
        Some(
          Json.obj(
            "event" -> Json.fromString("SuiteFinished"),
            "project" -> Json.fromString(e.project),
            "suite" -> Json.fromString(e.suite),
            "passed" -> Json.fromInt(e.passed),
            "failed" -> Json.fromInt(e.failed),
            "skipped" -> Json.fromInt(e.skipped),
            "ignored" -> Json.fromInt(e.ignored),
            "durationMs" -> Json.fromLong(e.durationMs)
          )
        )

      case e: E.SuiteError =>
        val fields = List.newBuilder[(String, Json)]
        fields += "event" -> Json.fromString("SuiteError")
        fields += "project" -> Json.fromString(e.project)
        fields += "suite" -> Json.fromString(e.suite)
        fields += "error" -> Json.fromString(e.error)
        e.exitCode.foreach(c => fields += "exitCode" -> Json.fromInt(c))
        e.signal.foreach(s => fields += "signal" -> Json.fromInt(s))
        fields += "durationMs" -> Json.fromLong(e.durationMs)
        Some(Json.obj(fields.result()*))

      case e: E.SuiteTimedOut =>
        Some(
          Json.obj(
            "event" -> Json.fromString("SuiteTimedOut"),
            "project" -> Json.fromString(e.project),
            "suite" -> Json.fromString(e.suite),
            "timeoutMs" -> Json.fromLong(e.timeoutMs)
          )
        )

      case e: E.LinkFinished =>
        val fields = List.newBuilder[(String, Json)]
        fields += "event" -> Json.fromString("LinkFinished")
        fields += "project" -> Json.fromString(e.project)
        fields += "success" -> Json.fromBoolean(e.success)
        fields += "durationMs" -> Json.fromLong(e.durationMs)
        fields += "platform" -> Json.fromString(e.platform)
        e.error.foreach(err => fields += "error" -> Json.fromString(err))
        Some(Json.obj(fields.result()*))

      case e: E.SourcegenFinished =>
        val fields = List.newBuilder[(String, Json)]
        fields += "event" -> Json.fromString("SourcegenFinished")
        fields += "scriptMain" -> Json.fromString(e.scriptMain)
        fields += "success" -> Json.fromBoolean(e.success)
        fields += "durationMs" -> Json.fromLong(e.durationMs)
        e.error.foreach(err => fields += "error" -> Json.fromString(err))
        Some(Json.obj(fields.result()*))

      case e: E.BuildFinished =>
        Some(
          Json.obj(
            "event" -> Json.fromString("BuildFinished"),
            "success" -> Json.fromBoolean(e.success),
            "durationMs" -> Json.fromLong(e.durationMs)
          )
        )

      case e: E.TestRunFinished =>
        Some(
          Json.obj(
            "event" -> Json.fromString("TestRunFinished"),
            "totalPassed" -> Json.fromInt(e.totalPassed),
            "totalFailed" -> Json.fromInt(e.totalFailed),
            "totalSkipped" -> Json.fromInt(e.totalSkipped),
            "totalIgnored" -> Json.fromInt(e.totalIgnored),
            "durationMs" -> Json.fromLong(e.durationMs)
          )
        )

      case e: E.Error =>
        val fields = List.newBuilder[(String, Json)]
        fields += "event" -> Json.fromString("Error")
        fields += "message" -> Json.fromString(e.message)
        e.details.foreach(d => fields += "details" -> Json.fromString(d))
        Some(Json.obj(fields.result()*))

      // All other events are TUI progress noise — not actionable
      case _ => None
    }
  }
}
