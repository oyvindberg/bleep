package bleep.history

import bleep.BleepException
import bleep.bsp.protocol.{BleepBspProtocol, SuiteOutcome, TestStatus}
import io.circe.Json

/** Diffs between two request [[Transcript]]s, answering "what changed between run A and run B?" without any daemon call and without any staleness: both inputs
  * are immutable records of their own runs. The transcripts may come from different worktrees — see the identity rules below.
  *
  * Two deliberately separate operations:
  *
  *   - [[mechanical]] — the logical diff. It is computed over a projection of the transcript that CONTAINS NO TIME: statuses, messages, diagnostics,
  *     invalidation facts. Not "durations zeroed out" — durations never enter the compared data structures, so two runs with the same logical outcome diff as
  *     identical no matter how their timings jittered. Deterministic input ⇒ deterministic diff.
  *   - [[timing]] — the duration comparison, where jitter is the enemy instead: per-item deltas are reported only above a significance threshold (`max(50ms,
  *     20% of base)`), plus the absolute slowest items of the target run.
  *
  * Test identity is `(project, suite, test)`; compile identity is the project. Diagnostic identity is `(severity, path, message)` — the line number is an
  * attribute, not identity, so an edit that only shifts a diagnostic down the file does not report it as new + resolved.
  *
  * ==Path identity across worktrees==
  *
  * Transcripts store paths ABSOLUTE (greppable, clickable), but identity is computed against each side's own workspace root: a path under the root is
  * relativized, and occurrences of the root embedded in a diagnostic's message are replaced with a `BASE` marker — ephemerally, per side, never serialized and
  * never shown. This makes the same diagnostic in a parent and its fork compare as the same, while paths OUTSIDE any workspace (coursier jars, the JDK) stay
  * absolute for identity too, which is correct on one machine: the same jar genuinely is the same file in both worktrees. For a single-workspace diff the
  * relativization is applied to both sides against the same root, so it is an identity-preserving no-op.
  */
object TranscriptDiff {

  // ==========================================================================
  // Shared
  // ==========================================================================

  private val AnsiPattern = java.util.regex.Pattern.compile("\u001b\\[[0-9;]*[a-zA-Z]")
  private def stripAnsi(s: String): String = AnsiPattern.matcher(s).replaceAll("")

  private val BaseMarker = "${BASE}"

  /** Relativize `p` against this side's workspace root — identity only, never display. Separator-agnostic: the workspace root comes from BuildPaths (native
    * separators) while compiler-reported paths may use either style on Windows, and `File.separator` of the machine READING the transcript is irrelevant to the
    * machine that wrote it. Everything is normalized to forward slashes before comparison — for the ephemeral identity key only.
    */
  private def relPath(workspace: String, p: String): String = {
    val ws = workspace.replace('\\', '/')
    val root = if (ws.endsWith("/")) ws else ws + "/"
    val pn = p.replace('\\', '/')
    if (pn.startsWith(root)) pn.substring(root.length) else pn
  }

  /** Replace occurrences of this side's workspace root embedded in free text — identity only, never display. Both separator spellings of the root are replaced:
    * messages quote paths in whatever style the compiler printed them.
    */
  private def relMessage(workspace: String, message: String): String = {
    val fwd = workspace.replace('\\', '/').reverse.dropWhile(_ == '/').reverse
    val back = fwd.replace('/', '\\')
    message
      .replace(back + "\\", BaseMarker + "/")
      .replace(back, BaseMarker)
      .replace(fwd + "/", BaseMarker + "/")
      .replace(fwd, BaseMarker)
  }

  private def requireSameMode(base: Transcript, target: Transcript): Unit =
    if (base.mode != target.mode)
      throw new BleepException.Text(s"Cannot diff a ${base.mode} request (#${base.id}) against a ${target.mode} request (#${target.id}).")

  private def header(base: Transcript, target: Transcript): List[(String, Json)] = {
    val fields = List.newBuilder[(String, Json)]
    fields += "base" -> Json.obj("historyId" -> Json.fromLong(base.id), "workspace" -> Json.fromString(base.workspace))
    fields += "target" -> Json.obj("historyId" -> Json.fromLong(target.id), "workspace" -> Json.fromString(target.workspace))
    fields += "mode" -> Json.fromString(base.mode)
    if (base.workspace != target.workspace)
      fields += "crossWorkspace" -> Json.fromBoolean(true)
    fields.result()
  }

  private def plural(n: Int, word: String): String = if (n == 1) s"$n $word" else s"$n ${word}s"

  // ==========================================================================
  // Extraction: the time-free projections
  // ==========================================================================

  private case class TestKey(project: String, suite: String, test: String)
  private object TestKey {
    implicit val ordering: Ordering[TestKey] = Ordering.by(k => (k.project, k.suite, k.test))
  }

  /** Everything the mechanical diff compares about a test. No durations, no timestamps — by type, not by convention. */
  private case class TestFacts(status: TestStatus, message: Option[String])

  private def testFacts(entry: Transcript): Map[TestKey, TestFacts] =
    entry.events.collect { case e: BleepBspProtocol.Event.TestFinished =>
      TestKey(e.project.value, e.suite.value, e.test.value) -> TestFacts(e.status, e.message.map(stripAnsi))
    }.toMap // a rerun of the same test within one request: last event wins

  private def testDurations(entry: Transcript): Map[TestKey, Long] =
    entry.events.collect { case e: BleepBspProtocol.Event.TestFinished =>
      TestKey(e.project.value, e.suite.value, e.test.value) -> e.durationMs
    }.toMap

  /** Suite-level terminal outcome KIND. Per-test transitions already cover pass/fail counts inside an executed suite, so only the kind — executed, empty,
    * no-framework-matched, errored, timed-out, cancelled — is compared at suite level.
    */
  private def suiteOutcomeKinds(entry: Transcript): Map[(String, String), String] =
    entry.events.collect {
      case e: BleepBspProtocol.Event.SuiteFinished =>
        val kind = e.outcome match {
          case _: SuiteOutcome.Executed        => "executed"
          case SuiteOutcome.Empty              => "empty"
          case SuiteOutcome.NoFrameworkMatched => "no-framework-matched"
          case _: SuiteOutcome.Errored         => "errored"
        }
        (e.project.value, e.suite.value) -> kind
      case e: BleepBspProtocol.Event.SuiteError     => (e.project.value, e.suite.value) -> "errored"
      case e: BleepBspProtocol.Event.SuiteTimedOut  => (e.project.value, e.suite.value) -> "timed-out"
      case e: BleepBspProtocol.Event.SuiteCancelled => (e.project.value, e.suite.value) -> "cancelled"
    }.toMap

  /** Everything the mechanical diff compares about a compiled project. No durations, no timestamps. Diagnostic identity is relativized against this
    * transcript's own workspace; the display form kept alongside is the original absolute one.
    */
  private case class CompileFacts(
      reason: Option[String],
      invalidatedFiles: Set[String],
      changedDependencies: Set[String],
      status: Option[String],
      skippedBecause: Option[String],
      diagnostics: Map[DiagKey, DiagDisplay]
  )

  private case class DiagKey(severity: String, path: Option[String], message: String)
  private object DiagKey {
    implicit val ordering: Ordering[DiagKey] = Ordering.by(k => (k.severity, k.path.getOrElse(""), k.message))
  }

  /** The presentation half of a diagnostic: this side's absolute path and the lines it appeared on. */
  private case class DiagDisplay(path: Option[String], message: String, lines: List[Int])

  private def compileFacts(entry: Transcript): Map[String, CompileFacts] = {
    import BleepBspProtocol.{Event => E}
    val empty = CompileFacts(None, Set.empty, Set.empty, None, None, Map.empty)
    entry.events.foldLeft(Map.empty[String, CompileFacts]) {
      case (acc, e: E.CompilationReason) =>
        val prev = acc.getOrElse(e.project.value, empty)
        acc.updated(
          e.project.value,
          prev.copy(reason = Some(e.reason.wireValue), invalidatedFiles = e.invalidatedFiles.toSet, changedDependencies = e.changedDependencies.toSet)
        )
      case (acc, e: E.CompileFinished) =>
        val prev = acc.getOrElse(e.project.value, empty)
        val diags: Map[DiagKey, DiagDisplay] =
          e.diagnostics
            .groupBy { d =>
              val message = stripAnsi(d.message)
              DiagKey(d.severity.wireValue, d.path.map(relPath(entry.workspace, _)), relMessage(entry.workspace, message))
            }
            .map { case (key, ds) =>
              key -> DiagDisplay(ds.head.path, stripAnsi(ds.head.message), ds.flatMap(_.line).sorted)
            }
        acc.updated(e.project.value, prev.copy(status = Some(e.status.wireValue), skippedBecause = e.skippedBecause.map(_.value), diagnostics = diags))
      case (acc, _) => acc
    }
  }

  private def compileDurations(entry: Transcript): Map[String, Long] =
    entry.events.collect { case e: BleepBspProtocol.Event.CompileFinished => e.project.value -> e.durationMs }.toMap

  // ==========================================================================
  // Mechanical diff
  // ==========================================================================

  private def statusGroup(s: TestStatus): String = s match {
    case TestStatus.Passed                                                                          => "pass"
    case TestStatus.Failed | TestStatus.Error | TestStatus.Timeout | TestStatus.Cancelled           => "fail"
    case TestStatus.Skipped | TestStatus.Ignored | TestStatus.AssumptionFailed | TestStatus.Pending => "skip"
  }

  def mechanical(base: Transcript, target: Transcript): Json = {
    requireSameMode(base, target)
    if (base.mode == "test") mechanicalTest(base, target) else mechanicalCompile(base, target)
  }

  private def testEntry(key: TestKey, extra: (String, Json)*): Json =
    Json.obj(
      (List(
        "project" -> Json.fromString(key.project),
        "suite" -> Json.fromString(key.suite),
        "test" -> Json.fromString(key.test)
      ) ++ extra.toList)*
    )

  private def mechanicalTest(base: Transcript, target: Transcript): Json = {
    val b = testFacts(base)
    val t = testFacts(target)
    val keys = (b.keySet ++ t.keySet).toList.sorted

    val newlyFailing = List.newBuilder[Json]
    val fixed = List.newBuilder[Json]
    val stillFailing = List.newBuilder[Json]
    val newlySkipped = List.newBuilder[Json]
    val unskipped = List.newBuilder[Json]
    val otherStatusChanges = List.newBuilder[Json]
    val added = List.newBuilder[Json]
    val removed = List.newBuilder[Json]
    var stillFailingChangedCount = 0

    keys.foreach { key =>
      (b.get(key), t.get(key)) match {
        case (None, Some(tf)) =>
          added += testEntry(key, "status" -> Json.fromString(tf.status.wireValue))
        case (Some(bf), None) =>
          removed += testEntry(key, "status" -> Json.fromString(bf.status.wireValue))
        case (Some(bf), Some(tf)) =>
          val transition = List("from" -> Json.fromString(bf.status.wireValue), "to" -> Json.fromString(tf.status.wireValue))
          (statusGroup(bf.status), statusGroup(tf.status)) match {
            case ("pass", "fail") | ("skip", "fail") =>
              newlyFailing += testEntry(key, (transition ++ tf.message.map(m => "message" -> Json.fromString(m)))*)
            case ("fail", "pass") =>
              fixed += testEntry(key, transition*)
            case ("skip", "pass") =>
              unskipped += testEntry(key, transition*)
            case ("fail", "fail") =>
              val messageChanged =
                bf.message.map(relMessage(base.workspace, _)) != tf.message.map(relMessage(target.workspace, _)) || bf.status != tf.status
              if (messageChanged) stillFailingChangedCount += 1
              stillFailing += testEntry(
                key,
                (transition
                  ++ List("messageChanged" -> Json.fromBoolean(messageChanged))
                  ++ tf.message.map(m => "message" -> Json.fromString(m)))*
              )
            case ("pass", "skip") | ("fail", "skip") =>
              newlySkipped += testEntry(key, (transition ++ tf.message.map(m => "reason" -> Json.fromString(m)))*)
            case ("skip", "skip") if bf.status != tf.status =>
              otherStatusChanges += testEntry(key, transition*)
            case _ => () // pass->pass, or identical skip: no logical difference
          }
        case (None, None) => () // unreachable: key came from one of the maps
      }
    }

    val suiteChanges = {
      val bs = suiteOutcomeKinds(base)
      val ts = suiteOutcomeKinds(target)
      (bs.keySet ++ ts.keySet).toList.sorted.flatMap { key =>
        (bs.get(key), ts.get(key)) match {
          case (Some(from), Some(to)) if from != to =>
            Some(
              Json.obj(
                "project" -> Json.fromString(key._1),
                "suite" -> Json.fromString(key._2),
                "from" -> Json.fromString(from),
                "to" -> Json.fromString(to)
              )
            )
          case _ => None // suite added/removed is already visible through added/removed tests
        }
      }
    }

    val sections: List[(String, List[Json])] = List(
      "newlyFailing" -> newlyFailing.result(),
      "fixed" -> fixed.result(),
      "stillFailing" -> stillFailing.result(),
      "newlySkipped" -> newlySkipped.result(),
      "unskipped" -> unskipped.result(),
      "statusChanges" -> otherStatusChanges.result(),
      "added" -> added.result(),
      "removed" -> removed.result(),
      "suiteOutcomeChanges" -> suiteChanges
    )

    // stillFailing with an unchanged message is context, not a difference — a diff of two identical failing runs is identical.
    val differenceCount =
      sections.collect { case (name, items) if name != "stillFailing" => items.size }.sum + stillFailingChangedCount

    val summary =
      if (differenceCount == 0) "No logical differences."
      else
        sections
          .collect { case (name, items) if items.nonEmpty && name != "stillFailing" => s"${items.size} $name" }
          .concat(if (stillFailingChangedCount > 0) List(s"$stillFailingChangedCount stillFailing with changed failure") else Nil)
          .mkString(", ")

    Json.obj(
      (header(base, target)
        ++ List(
          "identical" -> Json.fromBoolean(differenceCount == 0),
          "summary" -> Json.fromString(summary)
        )
        ++ sections.collect { case (name, items) if items.nonEmpty => name -> Json.arr(items*) })*
    )
  }

  private def mechanicalCompile(base: Transcript, target: Transcript): Json = {
    val b = compileFacts(base)
    val t = compileFacts(target)
    val keys = (b.keySet ++ t.keySet).toList.sorted

    val projectsAdded = List.newBuilder[Json]
    val projectsRemoved = List.newBuilder[Json]
    val changed = List.newBuilder[Json]

    // Display uses each side's own absolute path — new diagnostics the target's, resolved ones the base's — so diff output stays greppable and clickable.
    def diagJson(key: DiagKey, display: DiagDisplay): Json =
      Json.obj(
        (List("severity" -> Json.fromString(key.severity), "message" -> Json.fromString(display.message))
          ++ display.path.map(p => "path" -> Json.fromString(p))
          ++ (if (display.lines.nonEmpty) List("lines" -> Json.arr(display.lines.map(Json.fromInt)*)) else Nil))*
      )

    keys.foreach { project =>
      (b.get(project), t.get(project)) match {
        case (None, Some(tf)) =>
          projectsAdded += Json.obj(
            (("project" -> Json.fromString(project)) +: tf.reason.map(r => "reason" -> Json.fromString(r)).toList)*
          )
        case (Some(_), None) =>
          projectsRemoved += Json.obj("project" -> Json.fromString(project))
        case (Some(bf), Some(tf)) =>
          val fields = List.newBuilder[(String, Json)]

          if (bf.reason != tf.reason)
            fields += "reason" -> Json.obj(
              "from" -> bf.reason.fold(Json.Null)(Json.fromString),
              "to" -> tf.reason.fold(Json.Null)(Json.fromString)
            )
          if (bf.status != tf.status)
            fields += "status" -> Json.obj(
              "from" -> bf.status.fold(Json.Null)(Json.fromString),
              "to" -> tf.status.fold(Json.Null)(Json.fromString)
            )
          if (bf.skippedBecause != tf.skippedBecause)
            fields += "skippedBecause" -> Json.obj(
              "from" -> bf.skippedBecause.fold(Json.Null)(Json.fromString),
              "to" -> tf.skippedBecause.fold(Json.Null)(Json.fromString)
            )

          val invalidatedAdded = (tf.invalidatedFiles -- bf.invalidatedFiles).toList.sorted
          val invalidatedRemoved = (bf.invalidatedFiles -- tf.invalidatedFiles).toList.sorted
          if (invalidatedAdded.nonEmpty) fields += "invalidatedFilesAdded" -> Json.arr(invalidatedAdded.map(Json.fromString)*)
          if (invalidatedRemoved.nonEmpty) fields += "invalidatedFilesRemoved" -> Json.arr(invalidatedRemoved.map(Json.fromString)*)

          val depsAdded = (tf.changedDependencies -- bf.changedDependencies).toList.sorted
          val depsRemoved = (bf.changedDependencies -- tf.changedDependencies).toList.sorted
          if (depsAdded.nonEmpty) fields += "changedDependenciesAdded" -> Json.arr(depsAdded.map(Json.fromString)*)
          if (depsRemoved.nonEmpty) fields += "changedDependenciesRemoved" -> Json.arr(depsRemoved.map(Json.fromString)*)

          val newDiags = (tf.diagnostics.keySet -- bf.diagnostics.keySet).toList.sorted
          val resolvedDiags = (bf.diagnostics.keySet -- tf.diagnostics.keySet).toList.sorted
          if (newDiags.nonEmpty) fields += "newDiagnostics" -> Json.arr(newDiags.map(k => diagJson(k, tf.diagnostics(k)))*)
          if (resolvedDiags.nonEmpty) fields += "resolvedDiagnostics" -> Json.arr(resolvedDiags.map(k => diagJson(k, bf.diagnostics(k)))*)

          val result = fields.result()
          if (result.nonEmpty) changed += Json.obj((("project" -> Json.fromString(project)) +: result)*)
        case (None, None) => ()
      }
    }

    val addedList = projectsAdded.result()
    val removedList = projectsRemoved.result()
    val changedList = changed.result()
    val differenceCount = addedList.size + removedList.size + changedList.size

    val summaryParts = List.newBuilder[String]
    if (changedList.nonEmpty) summaryParts += s"${plural(changedList.size, "project")} changed"
    if (addedList.nonEmpty) summaryParts += s"${addedList.size} only in target"
    if (removedList.nonEmpty) summaryParts += s"${removedList.size} only in base"
    val summary = if (differenceCount == 0) "No logical differences." else summaryParts.result().mkString(", ")

    Json.obj(
      (header(base, target)
        ++ List(
          "identical" -> Json.fromBoolean(differenceCount == 0),
          "summary" -> Json.fromString(summary)
        )
        ++ (if (changedList.nonEmpty) List("changed" -> Json.arr(changedList*)) else Nil)
        ++ (if (addedList.nonEmpty) List("projectsAdded" -> Json.arr(addedList*)) else Nil)
        ++ (if (removedList.nonEmpty) List("projectsRemoved" -> Json.arr(removedList*)) else Nil))*
    )
  }

  // ==========================================================================
  // Timing diff
  // ==========================================================================

  /** A duration delta is signal above `max(50ms, 20% of base)`, jitter below it. */
  private def significant(baseMs: Long, deltaMs: Long): Boolean =
    math.abs(deltaMs) >= math.max(50L, math.ceil(baseMs * 0.2).toLong)

  val DefaultTimingLimit = 15

  def timing(base: Transcript, target: Transcript, limit: Int): Json = {
    requireSameMode(base, target)
    if (base.mode == "test")
      timingFor(
        base,
        target,
        testDurations(base),
        testDurations(target),
        (k: TestKey) => List("project" -> Json.fromString(k.project), "suite" -> Json.fromString(k.suite), "test" -> Json.fromString(k.test)),
        limit
      )
    else
      timingFor(base, target, compileDurations(base), compileDurations(target), (p: String) => List("project" -> Json.fromString(p)), limit)
  }

  private def timingFor[K: Ordering](
      base: Transcript,
      target: Transcript,
      baseDurations: Map[K, Long],
      targetDurations: Map[K, Long],
      itemJson: K => List[(String, Json)],
      limit: Int
  ): Json = {
    val common = baseDurations.keySet.intersect(targetDurations.keySet)
    val deltas = common.toList.map(k => (k, baseDurations(k), targetDurations(k), targetDurations(k) - baseDurations(k)))
    val significantDeltas = deltas.filter { case (_, b, _, d) => significant(b, d) }
    val suppressed = deltas.size - significantDeltas.size

    def deltaJson(entry: (K, Long, Long, Long)): Json = {
      val (key, b, t, d) = entry
      Json.obj(
        (itemJson(key) ++ List(
          "baseMs" -> Json.fromLong(b),
          "targetMs" -> Json.fromLong(t),
          "deltaMs" -> Json.fromLong(d)
        ))*
      )
    }

    // Ties break on the item key, never on hash-set iteration order: the same two transcripts must render the same JSON on every run of the diff.
    val regressions = significantDeltas.filter(_._4 > 0).sortBy { case (k, _, _, d) => (-d, k) }.take(limit)
    val improvements = significantDeltas.filter(_._4 < 0).sortBy { case (k, _, _, d) => (d, k) }.take(limit)

    val slowestInTarget = targetDurations.toList
      .sortBy { case (k, ms) => (-ms, k) }
      .take(limit)
      .map { case (key, ms) => Json.obj((itemJson(key) ++ List("durationMs" -> Json.fromLong(ms)))*) }

    val totalBase = baseDurations.values.sum
    val totalTarget = targetDurations.values.sum

    val summary = {
      val parts = List.newBuilder[String]
      parts += s"total ${totalBase}ms -> ${totalTarget}ms"
      if (regressions.nonEmpty) parts += s"${regressions.size} slower"
      if (improvements.nonEmpty) parts += s"${improvements.size} faster"
      if (regressions.isEmpty && improvements.isEmpty) parts += "no significant per-item changes"
      parts.result().mkString(", ")
    }

    Json.obj(
      (header(base, target)
        ++ List(
          "totalBaseMs" -> Json.fromLong(totalBase),
          "totalTargetMs" -> Json.fromLong(totalTarget),
          "totalDeltaMs" -> Json.fromLong(totalTarget - totalBase),
          "threshold" -> Json.fromString("max(50ms, 20% of base)"),
          "insignificantDeltasSuppressed" -> Json.fromInt(suppressed),
          "summary" -> Json.fromString(summary)
        )
        ++ (if (regressions.nonEmpty) List("slower" -> Json.arr(regressions.map(deltaJson)*)) else Nil)
        ++ (if (improvements.nonEmpty) List("faster" -> Json.arr(improvements.map(deltaJson)*)) else Nil)
        ++ List("slowestInTarget" -> Json.arr(slowestInTarget*)))*
    )
  }
}
