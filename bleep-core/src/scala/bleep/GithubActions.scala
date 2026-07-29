package bleep

import bleep.bsp.protocol.{BleepBspProtocol, DiagnosticSeverity}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

/** Emits the two things GitHub Actions reads out of a job: workflow commands on stdout, and markdown appended to `$GITHUB_STEP_SUMMARY`.
  *
  * Both are plain text protocols, so there is no dependency and nothing to configure — a run outside Actions produces neither. Detection is `GITHUB_ACTIONS`,
  * which the runner sets to `true` for every step, exactly as `@actions/core` does it.
  *
  * @param annotationsEnabled
  *   whether `::error`/`::warning` annotations are emitted
  * @param stepSummaryFile
  *   `$GITHUB_STEP_SUMMARY` if the runner provided one
  */
case class GithubActions(annotationsEnabled: Boolean, stepSummaryFile: Option[Path]) {

  /** Annotate a source location so the diagnostic shows up inline on the pull request diff.
    *
    * `file` must be relative to the repository root — the runner resolves it against the workspace, and an absolute path silently fails to attach to any line.
    * See [[GithubActions.relativize]].
    */
  def annotate(severity: DiagnosticSeverity, file: Option[String], line: Option[Int], column: Option[Int], title: String, message: String): Unit =
    if (annotationsEnabled) {
      val command = severity match {
        case DiagnosticSeverity.Error   => "error"
        case DiagnosticSeverity.Warning => "warning"
        case DiagnosticSeverity.Info    => "notice"
      }
      val props = List(
        file.map(f => "file" -> f),
        line.map(l => "line" -> l.toString),
        column.map(c => "col" -> c.toString),
        Some("title" -> title)
      ).flatten
        .map { case (k, v) => s"$k=${GithubActions.escapeProperty(v)}" }
        .mkString(",")

      println(s"::$command $props::${GithubActions.escapeData(message)}")
    }

  /** Append a markdown section to the job summary, rendered on the run page. No-op when the runner did not provide the file. */
  def appendStepSummary(markdown: String): Unit =
    stepSummaryFile.foreach { file =>
      val bytes = (markdown.stripLineEnd + "\n").getBytes(StandardCharsets.UTF_8)
      Files.write(file, bytes, StandardOpenOption.CREATE, StandardOpenOption.APPEND).discard()
    }
}

object GithubActions {

  /** `GITHUB_ACTIONS=true` is set for every step the runner executes, and is what `@actions/core` keys off. */
  def fromEnv(env: Map[String, String]): GithubActions =
    GithubActions(
      annotationsEnabled = env.get("GITHUB_ACTIONS").contains("true"),
      stepSummaryFile = env.get("GITHUB_STEP_SUMMARY").filter(_.nonEmpty).map(Path.of(_))
    )

  val disabled: GithubActions = GithubActions(annotationsEnabled = false, stepSummaryFile = None)

  /** Annotations only attach to a line when the path is relative to the repo root. Diagnostics carry absolute paths, so strip the workspace prefix; a file
    * outside the build (a dependency source, say) has no place on the diff and loses its position rather than pointing at the wrong file.
    */
  def relativize(buildDir: Path, path: String): Option[String] =
    try {
      val abs = Path.of(path).toAbsolutePath.normalize()
      val base = buildDir.toAbsolutePath.normalize()
      if (abs.startsWith(base)) Some(base.relativize(abs).toString.replace('\\', '/')) else None
    } catch {
      // Path.of throws on strings the platform cannot parse as a path. A diagnostic is not worth failing a build over, and the
      // annotation is decoration on top of output the user already has.
      case _: java.nio.file.InvalidPathException => None
    }

  /** GitHub caps how many annotations it renders per level per step (10 at time of writing); the rest reach the log but never the diff. So the budget is spent
    * worst-first: errors before warnings, and positioned diagnostics before unpositioned ones, since an annotation with no file cannot appear inline anyway.
    */
  val maxAnnotationsPerLevel: Int = 10

  def rank(d: BleepBspProtocol.Diagnostic): (Int, Int) = {
    val bySeverity = d.severity match {
      case DiagnosticSeverity.Error   => 0
      case DiagnosticSeverity.Warning => 1
      case DiagnosticSeverity.Info    => 2
    }
    (bySeverity, if (d.path.isDefined) 0 else 1)
  }

  /** Workflow-command data must not contain raw newlines or the runner truncates at the first one. */
  private[bleep] def escapeData(s: String): String =
    s.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A")

  /** Property values additionally escape the delimiters of the `k=v,k=v` list. */
  private[bleep] def escapeProperty(s: String): String =
    escapeData(s).replace(":", "%3A").replace(",", "%2C")
}
