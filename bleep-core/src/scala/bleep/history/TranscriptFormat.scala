package bleep.history

import bleep.BleepException
import bleep.bsp.protocol.{BleepBspProtocol, DiagnosticSeverity, TestStatus}
import io.circe.Json

/** Renders a request [[Transcript]] (or its raw event stream) as agent/human-consumable JSON: compact summaries after a run, and the full "details" view with
  * every diagnostic and stack trace. Shared verbatim between `bleep history show` (CLI) and `bleep.history.show` (MCP) so the two surfaces cannot drift; moved
  * here from the MCP server, mechanically, when the CLI grew the same commands.
  */
object TranscriptFormat {

  /** Strip ANSI escape sequences from text. Subprocess output and compiler diagnostics often contain color codes that are noise for structured output. */
  private val AnsiPattern = java.util.regex.Pattern.compile("\u001b\\[[0-9;]*[a-zA-Z]")
  def stripAnsi(s: String): String = AnsiPattern.matcher(s).replaceAll("")

  /** Compile the user's search query, failing loudly on an invalid regex. */
  def compileQuery(query: String): java.util.regex.Pattern =
    try java.util.regex.Pattern.compile(query, java.util.regex.Pattern.CASE_INSENSITIVE)
    catch {
      case e: java.util.regex.PatternSyntaxException => throw new BleepException.Text(s"query is not a valid regex: ${e.getMessage}")
    }

  /** The full "details" view of one transcript: verbose formatting (every diagnostic / stack trace) plus the transcript's header fields, with the same
    * project/query/limit/offset narrowing everywhere.
    */
  def details(transcript: Transcript, project: Option[String], query: Option[String], limit: Option[Int], offset: Option[Int]): Json = {
    val events = project match {
      case Some(proj) => filterEventsByProject(transcript.events, proj)
      case None       => transcript.events
    }
    val pattern = query.map(compileQuery)
    val result =
      if (transcript.mode == "test") formatTestResult(events, transcript.testRunResult, includeThrowables = true, pattern, limit, offset)
      else formatCompileResult(events, verbose = true, pattern, limit, offset)
    result.deepMerge(
      Json.obj(
        "historyId" -> Json.fromLong(transcript.id),
        "mode" -> Json.fromString(transcript.mode),
        "workspace" -> Json.fromString(transcript.workspace),
        "timestampMs" -> Json.fromLong(transcript.timestampMs),
        "targets" -> Json.arr(transcript.targets.map(Json.fromString)*),
        "client" -> Json.fromString(transcript.client)
      )
    )
  }

  /** Keep only events that belong to the given project. Events without a project field (e.g. SourcegenStarted) are dropped. */
  def filterEventsByProject(events: List[BleepBspProtocol.Event], project: String): List[BleepBspProtocol.Event] = {
    import BleepBspProtocol.{Event => E}
    events.filter {
      case e: E.CompileStarted      => e.project.value == project
      case e: E.CompilationReason   => e.project.value == project
      case e: E.CompileProgress     => e.project.value == project
      case e: E.CompilePhaseChanged => e.project.value == project
      case e: E.CompileFinished     => e.project.value == project
      case e: E.CompileStalled      => e.project.value == project
      case e: E.CompileResumed      => e.project.value == project
      case e: E.LockContention      => e.project.value == project
      case e: E.LockAcquired        => e.project.value == project
      case e: E.LinkStarted         => e.project.value == project
      case e: E.LinkProgress        => e.project.value == project
      case e: E.LinkFinished        => e.project.value == project
      case e: E.DiscoveryStarted    => e.project.value == project
      case e: E.SuitesDiscovered    => e.project.value == project
      case e: E.SuiteStarted        => e.project.value == project
      case e: E.TestStarted         => e.project.value == project
      case e: E.TestFinished        => e.project.value == project
      case e: E.SuiteFinished       => e.project.value == project
      case _                        => false
    }
  }

  /** True if the pattern matches any of the given nullable/optional text fields. */
  private def matchesAny(pattern: java.util.regex.Pattern, fields: Iterable[String]): Boolean =
    fields.exists(f => pattern.matcher(f).find())

  private def plural(n: Int, word: String): String =
    if (n == 1) s"$n $word" else s"$n ${word}s"

  /** Format compile result from protocol events. Compact mode returns counts, a summary line and the first few errors; verbose mode (details) returns every
    * diagnostic. A query narrows the diagnostics array to matching entries (message, rendered, path); summary counts always reflect the full run. When
    * limit/offset are provided, the diagnostics array is sliced and a totalDiagnostics count is included.
    */
  def formatCompileResult(
      events: List[BleepBspProtocol.Event],
      verbose: Boolean,
      query: Option[java.util.regex.Pattern],
      limit: Option[Int],
      offset: Option[Int]
  ): Json = {
    import BleepBspProtocol.{Event => E}

    val compileEvents = events.collect { case e: E.CompileFinished => e }
    val failedProjects = compileEvents.filter(_.status.isFailure)
    val allDiagnostics = compileEvents.flatMap(_.diagnostics)
    val errorCount = allDiagnostics.count(_.severity == DiagnosticSeverity.Error)
    val warningCount = allDiagnostics.count(_.severity == DiagnosticSeverity.Warning)
    val success = failedProjects.isEmpty

    if (verbose) {
      val consideredDiagnostics = query match {
        case Some(p) => allDiagnostics.filter(d => matchesAny(p, List(stripAnsi(d.message)) ++ d.rendered.map(stripAnsi) ++ d.path))
        case None    => allDiagnostics
      }
      val allDiagnosticJsons = consideredDiagnostics.map { d =>
        val fields = List.newBuilder[(String, Json)]
        fields += "severity" -> Json.fromString(d.severity.wireValue)
        fields += "message" -> Json.fromString(stripAnsi(d.message))
        d.rendered.foreach(r => fields += "rendered" -> Json.fromString(stripAnsi(r)))
        d.path.foreach(p => fields += "path" -> Json.fromString(p))
        d.line.foreach(l => fields += "line" -> Json.fromInt(l))
        d.column.foreach(c => fields += "column" -> Json.fromInt(c))
        Json.obj(fields.result()*)
      }
      val totalDiagnostics = allDiagnosticJsons.size
      val sliced = {
        val afterOffset = offset.map(o => allDiagnosticJsons.drop(o)).getOrElse(allDiagnosticJsons)
        limit.map(l => afterOffset.take(l)).getOrElse(afterOffset)
      }
      val resultFields = List.newBuilder[(String, Json)]
      resultFields += "success" -> Json.fromBoolean(success)
      resultFields += "errors" -> Json.fromInt(errorCount)
      resultFields += "warnings" -> Json.fromInt(warningCount)
      query.foreach(p => resultFields += "query" -> Json.fromString(p.pattern))
      resultFields += "totalDiagnostics" -> Json.fromInt(totalDiagnostics)
      resultFields += "diagnostics" -> Json.arr(sliced*)
      Json.obj(resultFields.result()*)
    } else {
      val fields = List.newBuilder[(String, Json)]
      fields += "success" -> Json.fromBoolean(success)
      fields += "errors" -> Json.fromInt(errorCount)
      fields += "warnings" -> Json.fromInt(warningCount)

      val summaryParts = List.newBuilder[String]
      if (success) summaryParts += s"Build succeeded (${plural(compileEvents.size, "project")})"
      else summaryParts += s"Build failed: ${plural(errorCount, "error")} in ${plural(failedProjects.map(_.project).distinct.size, "project")}"
      if (warningCount > 0) summaryParts += plural(warningCount, "warning")
      if (!success) summaryParts += "Use bleep.history.show with this historyId for all diagnostics"
      fields += "summary" -> Json.fromString(summaryParts.result().mkString(". "))

      if (failedProjects.nonEmpty) {
        fields += "failedProjects" -> Json.arr(failedProjects.map(_.project.value).distinct.map(Json.fromString)*)
        // Always include first 3 errors so the agent has something actionable
        val topErrors = allDiagnostics.filter(_.severity == DiagnosticSeverity.Error).take(3).map { d =>
          val df = List.newBuilder[(String, Json)]
          df += "message" -> Json.fromString(stripAnsi(d.message))
          d.path.foreach(p => df += "path" -> Json.fromString(p))
          d.line.foreach(l => df += "line" -> Json.fromInt(l))
          d.column.foreach(c => df += "column" -> Json.fromInt(c))
          Json.obj(df.result()*)
        }
        fields += "topErrors" -> Json.arr(topErrors*)
      }

      Json.obj(fields.result()*)
    }
  }

  /** Format test result from protocol events. Compact mode elides stack traces; verbose mode (details) includes them. A query narrows the failures array to
    * matching entries (suite, test, message, stack trace); summary counts always reflect the full run. When limit/offset are provided, the failures array is
    * sliced and a totalFailures count is included.
    */
  def formatTestResult(
      events: List[BleepBspProtocol.Event],
      testRunResult: Option[BleepBspProtocol.TestRunResult],
      includeThrowables: Boolean,
      query: Option[java.util.regex.Pattern],
      limit: Option[Int],
      offset: Option[Int]
  ): Json = {
    import BleepBspProtocol.{Event => E}

    val testEvents = events.collect { case e: E.TestFinished => e }
    val allFailedTests = testEvents.filter(e => e.status.isFailure)
    val failedTests = query match {
      case Some(p) =>
        allFailedTests.filter(e => matchesAny(p, List(e.project.value, e.suite.value, e.test.value) ++ e.message.map(stripAnsi) ++ e.throwable.map(stripAnsi)))
      case None => allFailedTests
    }

    val passed = testRunResult.map(_.totalPassed).getOrElse(testEvents.count(_.status == TestStatus.Passed))
    val failed = testRunResult.map(_.totalFailed).getOrElse(allFailedTests.size)
    val durationMs = testRunResult.map(_.durationMs)

    val fields = List.newBuilder[(String, Json)]
    fields += "success" -> Json.fromBoolean(failed == 0)
    fields += "passed" -> Json.fromInt(passed)
    fields += "failed" -> Json.fromInt(failed)
    testRunResult.foreach { trr =>
      fields += "skipped" -> Json.fromInt(trr.totalSkipped)
      fields += "ignored" -> Json.fromInt(trr.totalIgnored)
    }
    durationMs.foreach(d => fields += "durationMs" -> Json.fromLong(d))

    val summaryParts = List.newBuilder[String]
    if (failed == 0) summaryParts += s"${plural(passed, "test")} passed"
    else summaryParts += s"${plural(failed, "test")} failed, $passed passed"
    if (failed > 0) summaryParts += "Use bleep.history.show with this historyId for failure details"
    fields += "summary" -> Json.fromString(summaryParts.result().mkString(". "))

    // Include failure details: always include message, never inline full stack traces in compact mode
    if (failedTests.nonEmpty) {
      val totalFailures = failedTests.size
      query.foreach(p => fields += "query" -> Json.fromString(p.pattern))
      fields += "totalFailures" -> Json.fromInt(totalFailures)
      val slicedFailures = {
        val afterOffset = offset.map(o => failedTests.drop(o)).getOrElse(failedTests)
        limit.map(l => afterOffset.take(l)).getOrElse(afterOffset)
      }
      val failureJsons = slicedFailures.map { e =>
        val df = List.newBuilder[(String, Json)]
        df += "project" -> Json.fromString(e.project.value)
        df += "suite" -> Json.fromString(e.suite.value)
        df += "test" -> Json.fromString(e.test.value)
        df += "status" -> Json.fromString(e.status.wireValue)
        e.message.foreach(m => df += "message" -> Json.fromString(stripAnsi(m)))
        e.throwable.foreach { t =>
          if (includeThrowables) {
            val collapsed = bleep.testing.StackTraceCycles.collapse(stripAnsi(t)).mkString("\n")
            df += "throwable" -> Json.fromString(collapsed)
          } else df += "throwable" -> Json.fromString("present. Use bleep.history.show with this historyId for the full stack trace")
        }
        Json.obj(df.result()*)
      }
      fields += "failures" -> Json.arr(failureJsons*)
    }

    Json.obj(fields.result()*)
  }
}
