package bleep.mcp

import bleep.bsp.protocol.BleepBspProtocol
import io.circe.Json
import io.circe.syntax.*

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
            "project" -> e.project.asJson
          )
        )

      case e: E.CompileFinished =>
        val fields = List.newBuilder[(String, Json)]
        fields += "event" -> Json.fromString("CompileFinished")
        fields += "project" -> e.project.asJson
        fields += "status" -> e.status.asJson
        fields += "durationMs" -> Json.fromLong(e.durationMs)
        if (e.diagnostics.nonEmpty) {
          fields += "diagnostics" -> Json.arr(
            e.diagnostics.map { d =>
              val diagFields = List.newBuilder[(String, Json)]
              diagFields += "severity" -> d.severity.asJson
              diagFields += "message" -> Json.fromString(stripAnsi(d.message))
              d.rendered.foreach(r => diagFields += "rendered" -> Json.fromString(stripAnsi(r)))
              d.path.foreach(p => diagFields += "path" -> Json.fromString(p))
              Json.obj(diagFields.result()*)
            }*
          )
        }
        e.skippedBecause.foreach { reason =>
          fields += "skippedBecause" -> reason.asJson
        }
        Some(Json.obj(fields.result()*))

      case e: E.TestFinished =>
        val fields = List.newBuilder[(String, Json)]
        fields += "event" -> Json.fromString("TestFinished")
        fields += "project" -> e.project.asJson
        fields += "suite" -> e.suite.asJson
        fields += "test" -> e.test.asJson
        fields += "status" -> e.status.asJson
        fields += "durationMs" -> Json.fromLong(e.durationMs)
        e.message.foreach(m => fields += "message" -> Json.fromString(m))
        e.throwable.foreach { t =>
          val collapsed = bleep.testing.StackTraceCycles.collapse(t).mkString("\n")
          fields += "throwable" -> Json.fromString(collapsed)
        }
        Some(Json.obj(fields.result()*))

      case e: E.SuiteFinished =>
        Some(
          Json.obj(
            "event" -> Json.fromString("SuiteFinished"),
            "project" -> e.project.asJson,
            "suite" -> e.suite.asJson,
            "passed" -> Json.fromInt(e.passed),
            "failed" -> Json.fromInt(e.failed),
            "skipped" -> Json.fromInt(e.skipped),
            "ignored" -> Json.fromInt(e.ignored),
            "durationMs" -> Json.fromLong(e.durationMs)
          )
        )

      case e: E.SuiteError =>
        import bleep.bsp.protocol.ProcessExit
        val fields = List.newBuilder[(String, Json)]
        fields += "event" -> Json.fromString("SuiteError")
        fields += "project" -> e.project.asJson
        fields += "suite" -> e.suite.asJson
        fields += "error" -> Json.fromString(e.error)
        e.processExit match {
          case ProcessExit.ExitCode(code) => fields += "exitCode" -> Json.fromInt(code)
          case ProcessExit.Signal(signal) => fields += "signal" -> Json.fromInt(signal)
          case ProcessExit.Unknown        => ()
        }
        fields += "durationMs" -> Json.fromLong(e.durationMs)
        Some(Json.obj(fields.result()*))

      case e: E.SuiteTimedOut =>
        Some(
          Json.obj(
            "event" -> Json.fromString("SuiteTimedOut"),
            "project" -> e.project.asJson,
            "suite" -> e.suite.asJson,
            "timeoutMs" -> Json.fromLong(e.timeoutMs)
          )
        )

      case e: E.LinkFinished =>
        val fields = List.newBuilder[(String, Json)]
        fields += "event" -> Json.fromString("LinkFinished")
        fields += "project" -> e.project.asJson
        fields += "success" -> Json.fromBoolean(e.success)
        fields += "durationMs" -> Json.fromLong(e.durationMs)
        fields += "platform" -> e.platform.asJson
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
