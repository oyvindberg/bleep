package bleep.history

import bleep.BleepException
import bleep.bsp.protocol.{BleepBspProtocol, DiagnosticSeverity}
import bleep.testing.{BuildEvent, BuildState, BuildStateReducer, BuildSummary, FailureCategory}
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

  /** Replay a protocol event stream through the reducer behind `bleep test`'s display and exit code. [[bleep.testing.BuildSummary.toEither]] on the result is
    * the single verdict on whether the run succeeded — the same verdict for the CLI, the MCP tools and transcript rendering, so a run cannot pass on one
    * surface and fail on another. When the run's response payload carried a [[BleepBspProtocol.TestRunResult]], its counts are authoritative (request-response,
    * so never lost like notifications) and are folded in exactly like the CLI does.
    */
  private def replay(events: List[BleepBspProtocol.Event], testRunResult: Option[BleepBspProtocol.TestRunResult]): BuildSummary = {
    val trrEvent = testRunResult.map { trr =>
      BuildEvent.TestRunCompleted(
        totalPassed = trr.totalPassed,
        totalFailed = trr.totalFailed,
        totalSkipped = trr.totalSkipped,
        totalIgnored = trr.totalIgnored,
        suitesTotal = trr.suitesTotal,
        suitesCompleted = trr.suitesCompleted,
        suitesFailed = trr.suitesFailed,
        suitesCancelled = trr.suitesCancelled,
        durationMs = trr.durationMs,
        timestamp = 0L
      )
    }
    val state = (events.flatMap(BuildEvent.fromProtocol) ++ trrEvent).foldLeft(BuildState.empty)(BuildStateReducer.reduce)
    state.toSummary(durationMs = testRunResult.map(_.durationMs).getOrElse(0L), wasCancelled = false)
  }

  /** JSON sections for everything that failed around the main work — projects skipped because a dependency failed, link/sourcegen/annotation-processor/
    * symbol-processor failures, suites that never ran. Shared by the compile and test formatters so neither surface can under-report; compile failures
    * themselves are rendered by each formatter in its own diagnostics shape.
    */
  private def infrastructureFields(summary: BuildSummary, events: List[BleepBspProtocol.Event], verbose: Boolean): List[(String, Json)] = {
    import BleepBspProtocol.{Event => E}

    val fields = List.newBuilder[(String, Json)]
    if (summary.skippedProjects.nonEmpty) {
      val jsons = summary.skippedProjects.map { sp =>
        Json.obj("project" -> Json.fromString(sp.project.value), "reason" -> Json.fromString(sp.reason))
      }
      fields += "skippedProjects" -> Json.arr(jsons*)
    }
    if (summary.linkFailures.nonEmpty) {
      val jsons = summary.linkFailures.map { lf =>
        Json.obj("project" -> Json.fromString(lf.project.value), "platform" -> Json.fromString(lf.platform.wireValue), "error" -> Json.fromString(lf.error))
      }
      fields += "linkFailures" -> Json.arr(jsons*)
    }
    val sourcegenFailures = events.collect { case e: E.SourcegenFinished if !e.success => e }
    if (sourcegenFailures.nonEmpty) {
      val jsons = sourcegenFailures.map { sg =>
        Json.obj("script" -> Json.fromString(sg.scriptMain), "error" -> Json.fromString(sg.error.getOrElse("sourcegen failed")))
      }
      fields += "sourcegenFailures" -> Json.arr(jsons*)
    }
    val apFailures = events.collect { case e: E.ResolveAnnotationProcessorsFinished if !e.success => e }
    if (apFailures.nonEmpty) {
      val jsons = apFailures.map { ap =>
        Json.obj("project" -> Json.fromString(ap.project.value), "error" -> Json.fromString(ap.error.getOrElse("annotation processor resolution failed")))
      }
      fields += "annotationProcessorFailures" -> Json.arr(jsons*)
    }
    val kspFailures = events.collect { case e: E.RunSymbolProcessorsFinished if !e.success => e }
    if (kspFailures.nonEmpty) {
      val jsons = kspFailures.map { ksp =>
        Json.obj("project" -> Json.fromString(ksp.project.value), "error" -> Json.fromString(ksp.error.getOrElse("symbol processor run failed")))
      }
      fields += "symbolProcessorFailures" -> Json.arr(jsons*)
    }
    if (summary.cancelledSuites.nonEmpty) {
      fields += "suitesNotRun" -> Json.fromInt(summary.cancelledSuites.size)
      val shown = if (verbose) summary.cancelledSuites else summary.cancelledSuites.take(10)
      val jsons = shown.map { cs =>
        val df = List.newBuilder[(String, Json)]
        df += "project" -> Json.fromString(cs.project.value)
        df += "suite" -> Json.fromString(cs.suite.value)
        cs.reason.foreach(r => df += "reason" -> Json.fromString(r))
        Json.obj(df.result()*)
      }
      fields += "cancelledSuites" -> Json.arr(jsons*)
    }
    fields.result()
  }

  /** Format compile result from protocol events. Compact mode returns counts, a summary line and the first few errors; verbose mode (details) returns every
    * diagnostic. A query narrows the diagnostics array to matching entries (message, rendered, path); summary counts always reflect the full run. When
    * limit/offset are provided, the diagnostics array is sliced and a totalDiagnostics count is included.
    *
    * `success` is NOT "no project's compiler reported errors": a failed sourcegen script, annotation-processor or symbol-processor resolution, or link leaves
    * every downstream compile merely *skipped*, with zero compile failures to count. The run is replayed through the same reducer as `bleep compile`'s display
    * and succeeds only if [[bleep.testing.BuildSummary.toEither]] says so.
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
    val summary = replay(events, testRunResult = None)
    val problem: Option[String] = summary.toEither.left.toOption.map(_.getMessage)
    val success = problem.isEmpty

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
      resultFields ++= infrastructureFields(summary, events, verbose = true)
      Json.obj(resultFields.result()*)
    } else {
      val fields = List.newBuilder[(String, Json)]
      fields += "success" -> Json.fromBoolean(success)
      fields += "errors" -> Json.fromInt(errorCount)
      fields += "warnings" -> Json.fromInt(warningCount)

      val summaryParts = List.newBuilder[String]
      problem match {
        case None                               => summaryParts += s"Build succeeded (${plural(compileEvents.size, "project")})"
        case Some(_) if failedProjects.nonEmpty =>
          summaryParts += s"Build failed: ${plural(errorCount, "error")} in ${plural(failedProjects.map(_.project).distinct.size, "project")}"
        case Some(reason) => summaryParts += reason // no compiler error to blame: sourcegen/processor/link failure or a cancelled run
      }
      if (warningCount > 0) summaryParts += plural(warningCount, "warning")
      if (!success) summaryParts += "Use bleep.history.show with this historyId for details"
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
      fields ++= infrastructureFields(summary, events, verbose = false)

      Json.obj(fields.result()*)
    }
  }

  /** Format test result from protocol events. Compact mode elides stack traces; verbose mode (details) includes them. A query narrows the failures array to
    * matching entries (suite, test, message, stack trace); summary counts always reflect the full run. When limit/offset are provided, the failures array is
    * sliced and a totalFailures count is included.
    *
    * `success` is NOT "no test failed". The event stream is replayed through [[bleep.testing.BuildStateReducer]] — the same reducer behind `bleep test`'s
    * display — and the run succeeds only if [[bleep.testing.BuildSummary.toEither]] says so. That gate covers everything that can sink a test run before or
    * beside the tests themselves: compile failures (including a dependency failing so the test project never compiled), link failures, sourcegen,
    * annotation-processor and symbol-processor failures, errored/empty/timed-out/cancelled suites, a crashed compile server, and suites that completed without
    * executing a single test. A broken build must never render as "0 tests passed".
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

    val summary = replay(events, testRunResult)
    val problem: Option[String] = summary.toEither.left.toOption.map(_.getMessage)

    val passed = summary.testsPassed
    val failed = summary.testsFailed
    val durationMs = testRunResult.map(_.durationMs)

    val fields = List.newBuilder[(String, Json)]
    fields += "success" -> Json.fromBoolean(problem.isEmpty)
    fields += "passed" -> Json.fromInt(passed)
    fields += "failed" -> Json.fromInt(failed)
    fields += "skipped" -> Json.fromInt(summary.testsSkipped)
    fields += "ignored" -> Json.fromInt(summary.testsIgnored)
    durationMs.foreach(d => fields += "durationMs" -> Json.fromLong(d))

    val summaryParts = List.newBuilder[String]
    problem match {
      case None =>
        val testsObserved = passed + failed + summary.testsSkipped + summary.testsIgnored
        if (testsObserved == 0) summaryParts += "0 tests executed: no test suites ran. Check project selection and only/exclude filters"
        else summaryParts += s"${plural(passed, "test")} passed"
      case Some(reason) =>
        summaryParts += reason
        summaryParts += "Use bleep.history.show with this historyId for details"
    }
    fields += "summary" -> Json.fromString(summaryParts.result().mkString(". "))

    // The reducer's failure list is canonical: every per-test failure, plus one synthetic entry per suite-level problem (suite errored / empty / no framework
    // matched / timed out / process died / build error), so nothing that went wrong is representable only as a count. The reducer does not carry stack traces;
    // re-attach them from the raw TestFinished events by key.
    val rawByKey: Map[(String, String, String), E.TestFinished] =
      testEvents.iterator.map(e => ((e.project.value, e.suite.value, e.test.value), e)).toMap

    val allFailures: List[(bleep.testing.TestFailure, String, Option[String])] = summary.failures.map { f =>
      val raw = rawByKey.get((f.project.value, f.suite.value, f.test.value))
      val status = raw.map(_.status.wireValue).getOrElse {
        f.category match {
          case FailureCategory.TestFailed   => "failed"
          case FailureCategory.Timeout      => "timeout"
          case FailureCategory.Cancelled    => "cancelled"
          case FailureCategory.ProcessError => "error"
          case FailureCategory.BuildError   => "error"
        }
      }
      (f, status, raw.flatMap(_.throwable).orElse(f.throwable))
    }
    val failedTests = query match {
      case Some(p) =>
        allFailures.filter { case (f, _, throwable) =>
          matchesAny(p, List(f.project.value, f.suite.value, f.test.value) ++ f.message.map(stripAnsi) ++ throwable.map(stripAnsi))
        }
      case None => allFailures
    }

    // Include failure details: always include message, never inline full stack traces in compact mode
    if (failedTests.nonEmpty) {
      val totalFailures = failedTests.size
      query.foreach(p => fields += "query" -> Json.fromString(p.pattern))
      fields += "totalFailures" -> Json.fromInt(totalFailures)
      val slicedFailures = {
        val afterOffset = offset.map(o => failedTests.drop(o)).getOrElse(failedTests)
        limit.map(l => afterOffset.take(l)).getOrElse(afterOffset)
      }
      val failureJsons = slicedFailures.map { case (f, status, throwable) =>
        val df = List.newBuilder[(String, Json)]
        df += "project" -> Json.fromString(f.project.value)
        df += "suite" -> Json.fromString(f.suite.value)
        df += "test" -> Json.fromString(f.test.value)
        df += "status" -> Json.fromString(status)
        f.message.foreach(m => df += "message" -> Json.fromString(stripAnsi(m)))
        throwable.foreach { t =>
          if (includeThrowables) {
            val collapsed = bleep.testing.StackTraceCycles.collapse(stripAnsi(t)).mkString("\n")
            df += "throwable" -> Json.fromString(collapsed)
          } else df += "throwable" -> Json.fromString("present. Use bleep.history.show with this historyId for the full stack trace")
        }
        Json.obj(df.result()*)
      }
      fields += "failures" -> Json.arr(failureJsons*)
    }

    // Everything that failed before or beside the tests, so the reader never has to guess why nothing (or less than expected) ran.
    if (summary.compileFailures.nonEmpty) {
      fields += "failedProjects" -> Json.arr(summary.compileFailures.map(_.project.value).distinct.map(Json.fromString)*)
      val compileErrors = summary.compileFailures.flatMap(_.diagnostics).filter(_.severity == DiagnosticSeverity.Error)
      fields += "compileErrors" -> Json.fromInt(compileErrors.size)
      val shownErrors = if (includeThrowables) compileErrors else compileErrors.take(3)
      val errorJsons = shownErrors.map { d =>
        val df = List.newBuilder[(String, Json)]
        df += "message" -> Json.fromString(stripAnsi(d.message))
        if (includeThrowables) d.rendered.foreach(r => df += "rendered" -> Json.fromString(stripAnsi(r)))
        d.path.foreach(p => df += "path" -> Json.fromString(p))
        d.line.foreach(l => df += "line" -> Json.fromInt(l))
        d.column.foreach(c => df += "column" -> Json.fromInt(c))
        Json.obj(df.result()*)
      }
      fields += "topErrors" -> Json.arr(errorJsons*)
    }
    fields ++= infrastructureFields(summary, events, verbose = includeThrowables)

    Json.obj(fields.result()*)
  }
}
