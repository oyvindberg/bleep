package bleep.history

import bleep.testing.BleepConsole as C
import io.circe.{ACursor, Json}

/** Pure rendering of a [[TranscriptDiff]] document to human-readable terminal text.
  *
  * The input is the diff structure itself — the same JSON document `--out json` prints, `bleep history show`-adjacent tooling stores, and the MCP server
  * returns — so the two output formats can never disagree: both are renderings of one value. The function is total over documents [[TranscriptDiff]] produces
  * (mechanical compile, mechanical test, timing) and fails loudly on anything else rather than guessing.
  *
  * Colors come from [[bleep.testing.BleepConsole]], which yields empty strings in no-color mode, so this stays a pure function of (document, color mode).
  */
object TranscriptDiffRender {

  def text(diff: Json): String = {
    val c = diff.hcursor
    if (c.downField("totalBaseMs").succeeded) timing(c)
    else if (get[String](c, "mode") == "test") mechanicalTest(c)
    else mechanicalCompile(c)
  }

  // ==========================================================================
  // Document access: the renderer only accepts documents TranscriptDiff wrote
  // ==========================================================================

  private def get[T: io.circe.Decoder](c: ACursor, field: String): T =
    c.get[T](field) match {
      case Right(value) => value
      case Left(err)    => sys.error(s"not a diff document: missing or malformed field '$field' ($err)")
    }

  private def opt[T: io.circe.Decoder](c: ACursor, field: String): Option[T] =
    c.get[Option[T]](field) match {
      case Right(value) => value
      case Left(err)    => sys.error(s"not a diff document: malformed field '$field' ($err)")
    }

  private def arr(c: ACursor, field: String): List[ACursor] =
    c.downField(field).success match {
      case None     => Nil
      case Some(hc) =>
        hc.values match {
          case Some(values) => values.map(_.hcursor: ACursor).toList
          case None         => sys.error(s"not a diff document: field '$field' is not an array")
        }
    }

  // ==========================================================================
  // Shared building blocks
  // ==========================================================================

  private case class Side(historyId: Long, workspace: String)

  private def side(c: ACursor, field: String): Side = {
    val sc = c.downField(field)
    Side(get[Long](sc, "historyId"), get[String](sc, "workspace"))
  }

  private def header(c: ACursor, kind: String): List[String] = {
    val base = side(c, "base")
    val target = side(c, "target")
    val crossWorkspace = opt[Boolean](c, "crossWorkspace").contains(true)
    val ids = s"${C.BOLD}$kind${C.RESET}  #${base.historyId} ${C.CYAN}→${C.RESET} #${target.historyId}"
    if (crossWorkspace) List(ids, s"  across worktrees: ${base.workspace} ${C.CYAN}→${C.RESET} ${target.workspace}")
    else List(ids)
  }

  /** display paths relative to the workspace that produced them, keeping paths outside the workspace absolute */
  private def relDisplay(workspace: String, p: String): String = {
    val root = if (workspace.endsWith("/")) workspace else workspace + "/"
    if (p.startsWith(root)) p.substring(root.length) else p
  }

  private def fmtMs(ms: Long): String =
    if (math.abs(ms) >= 10000) f"${ms / 1000.0}%.1fs" else s"${ms}ms"

  private def fmtDelta(ms: Long): String = if (ms >= 0) s"+${fmtMs(ms)}" else s"-${fmtMs(-ms)}"

  private def indented(prefix: String, message: String): List[String] =
    message.linesIterator.toList match {
      case Nil          => Nil
      case head :: rest => (prefix + head) :: rest.map(line => " " * prefix.length + line)
    }

  private def transition(c: ACursor, field: String): Option[(Option[String], Option[String])] =
    c.downField(field).success.map(hc => (opt[String](hc, "from"), opt[String](hc, "to")))

  private def colorStatus(status: String): String =
    status match {
      case "failed"  => s"${C.RED}$status${C.RESET}"
      case "success" => s"${C.GREEN}$status${C.RESET}"
      case other     => s"${C.YELLOW}$other${C.RESET}"
    }

  // ==========================================================================
  // Mechanical compile
  // ==========================================================================

  private def mechanicalCompile(c: ACursor): String = {
    val lines = List.newBuilder[String]
    lines ++= header(c, "compile diff")

    if (get[Boolean](c, "identical")) {
      lines += s"${C.GREEN}identical${C.RESET} — no logical differences"
      return lines.result().mkString("\n")
    }

    lines += get[String](c, "summary")

    val targetWs = side(c, "target").workspace
    val baseWs = side(c, "base").workspace

    def diagnostic(dc: ACursor, sign: String, color: String, workspace: String): List[String] = {
      val severity = get[String](dc, "severity")
      val message = get[String](dc, "message")
      val lineNos = opt[List[Int]](dc, "lines").getOrElse(Nil)
      val location = opt[String](dc, "path").map { p =>
        val rel = relDisplay(workspace, p)
        if (lineNos.isEmpty) rel else s"$rel:${lineNos.mkString(",")}"
      }
      indented(s"    $color$sign $severity${C.RESET} ", message) ++ location.toList.map(loc => s"      ${C.CYAN}$loc${C.RESET}")
    }

    def fileList(pc: ACursor, field: String, sign: String, workspace: String): List[String] =
      opt[List[String]](pc, field).getOrElse(Nil) match {
        case Nil   => Nil
        case files =>
          val shown = files.take(8).map(f => s"      $sign ${relDisplay(workspace, f)}")
          val more = if (files.size > 8) List(s"        … ${files.size - 8} more") else Nil
          s"    $field:" :: shown ++ more
      }

    arr(c, "changed").foreach { pc =>
      val project = get[String](pc, "project")
      lines += ""

      // skippedBecause decorates the status it belongs to: `success → skipped (waiting on core)`
      val skipped = transition(pc, "skippedBecause")
      def sideLabel(status: Option[String], waitingOn: Option[Option[String]]): String = {
        val statusStr = status.fold("-")(colorStatus)
        waitingOn.flatten.fold(statusStr)(dep => s"$statusStr (waiting on $dep)")
      }
      val statusLine = transition(pc, "status").map { case (from, to) =>
        s"${sideLabel(from, skipped.map(_._1))} ${C.CYAN}→${C.RESET} ${sideLabel(to, skipped.map(_._2))}"
      }
      lines += s"  ${C.BOLD}$project${C.RESET}${statusLine.fold("")(s => s"  $s")}"

      // a skippedBecause change without a status change still deserves a line of its own
      if (statusLine.isEmpty)
        skipped.foreach { case (from, to) => lines += s"    waiting on: ${from.getOrElse("-")} ${C.CYAN}→${C.RESET} ${to.getOrElse("-")}" }

      transition(pc, "reason").foreach { case (from, to) =>
        lines += s"    reason: ${from.getOrElse("-")} ${C.CYAN}→${C.RESET} ${to.getOrElse("-")}"
      }

      arr(pc, "newDiagnostics").foreach(dc => lines ++= diagnostic(dc, "+", C.RED, targetWs))
      arr(pc, "resolvedDiagnostics").foreach(dc => lines ++= diagnostic(dc, "-", C.GREEN, baseWs))

      lines ++= fileList(pc, "invalidatedFilesAdded", "+", targetWs)
      lines ++= fileList(pc, "invalidatedFilesRemoved", "-", baseWs)
      lines ++= fileList(pc, "changedDependenciesAdded", "+", targetWs)
      lines ++= fileList(pc, "changedDependenciesRemoved", "-", baseWs)
    }

    val added = arr(c, "projectsAdded")
    if (added.nonEmpty) {
      lines += ""
      lines += s"  only in #${side(c, "target").historyId}:"
      added.foreach { pc =>
        val reason = opt[String](pc, "reason").fold("")(r => s"  ($r)")
        lines += s"    ${C.GREEN}+${C.RESET} ${get[String](pc, "project")}$reason"
      }
    }
    val removed = arr(c, "projectsRemoved")
    if (removed.nonEmpty) {
      lines += ""
      lines += s"  only in #${side(c, "base").historyId}:"
      removed.foreach(pc => lines += s"    ${C.RED}-${C.RESET} ${get[String](pc, "project")}")
    }

    lines.result().mkString("\n")
  }

  // ==========================================================================
  // Mechanical test
  // ==========================================================================

  private def mechanicalTest(c: ACursor): String = {
    val lines = List.newBuilder[String]
    lines ++= header(c, "test diff")

    if (get[Boolean](c, "identical")) {
      // identical must not read as an all-clear when both runs are identically broken: the document carries the
      // still-failing tests as context, so name them instead of printing a green checkline next to a red build
      val stillFailing = arr(c, "stillFailing")
      if (stillFailing.isEmpty) lines += s"${C.GREEN}identical${C.RESET} — no logical differences"
      else {
        val n = stillFailing.size
        lines += s"${C.YELLOW}identical${C.RESET} — no logical differences, but ${if (n == 1) "1 test is" else s"$n tests are"} still failing:"
        stillFailing.foreach { tc =>
          lines += s"  ${C.RED}x${C.RESET} ${get[String](tc, "test")}  ${C.CYAN}(${get[String](tc, "project")})${C.RESET}"
        }
      }
      return lines.result().mkString("\n")
    }

    lines += get[String](c, "summary")

    def entry(tc: ACursor, icon: String, extra: ACursor => Option[String]): List[String] = {
      val test = get[String](tc, "test")
      val project = get[String](tc, "project")
      val headline = s"  $icon $test  ${C.CYAN}($project)${C.RESET}"
      headline :: extra(tc).toList.flatMap(msg => indented("      ", msg))
    }

    def sectionOf(field: String, title: String, icon: String, extra: ACursor => Option[String]): Unit =
      arr(c, field) match {
        case Nil   => ()
        case items =>
          lines += ""
          lines += title
          items.foreach(tc => lines ++= entry(tc, icon, extra))
      }

    def message(tc: ACursor): Option[String] = opt[String](tc, "message")
    def noExtra(tc: ACursor): Option[String] = { val _ = tc; None }
    def statusTransition(tc: ACursor): Option[String] =
      (opt[String](tc, "from"), opt[String](tc, "to")) match {
        case (Some(from), Some(to)) => Some(s"$from -> $to")
        case _                      => None
      }

    sectionOf("newlyFailing", s"${C.RED}newly failing${C.RESET}", s"${C.RED}x${C.RESET}", message)
    sectionOf("fixed", s"${C.GREEN}fixed${C.RESET}", s"${C.GREEN}+${C.RESET}", noExtra)
    sectionOf(
      "stillFailing",
      s"${C.YELLOW}still failing${C.RESET}",
      s"${C.YELLOW}x${C.RESET}",
      tc => if (opt[Boolean](tc, "messageChanged").contains(true)) message(tc).map(m => s"failure changed:\n$m") else None
    )
    sectionOf("newlySkipped", s"${C.YELLOW}newly skipped${C.RESET}", "~", tc => opt[String](tc, "reason"))
    sectionOf("unskipped", "unskipped", s"${C.GREEN}+${C.RESET}", noExtra)
    sectionOf("statusChanges", "status changes", "~", statusTransition)
    sectionOf("added", s"${C.GREEN}added${C.RESET}", s"${C.GREEN}+${C.RESET}", tc => opt[String](tc, "status"))
    sectionOf("removed", s"${C.RED}removed${C.RESET}", s"${C.RED}-${C.RESET}", noExtra)

    arr(c, "suiteOutcomeChanges") match {
      case Nil    => ()
      case suites =>
        lines += ""
        lines += s"${C.YELLOW}suite outcome changes${C.RESET}"
        suites.foreach { sc =>
          val suite = get[String](sc, "suite")
          val project = get[String](sc, "project")
          lines += s"  ~ $suite  ${get[String](sc, "from")} ${C.CYAN}→${C.RESET} ${get[String](sc, "to")}  ${C.CYAN}($project)${C.RESET}"
        }
    }

    lines.result().mkString("\n")
  }

  // ==========================================================================
  // Timing
  // ==========================================================================

  private def timing(c: ACursor): String = {
    val lines = List.newBuilder[String]
    lines ++= header(c, s"timing diff (${get[String](c, "mode")})")

    val totalBase = get[Long](c, "totalBaseMs")
    val totalTarget = get[Long](c, "totalTargetMs")
    val totalDelta = get[Long](c, "totalDeltaMs")
    val deltaColor = if (totalDelta > 0) C.RED else if (totalDelta < 0) C.GREEN else ""
    val suppressed = get[Int](c, "insignificantDeltasSuppressed")
    lines += s"total ${fmtMs(totalBase)} ${C.CYAN}→${C.RESET} ${fmtMs(totalTarget)} ($deltaColor${fmtDelta(totalDelta)}${C.RESET})"
    lines += s"threshold ${get[String](c, "threshold")}, $suppressed insignificant ${if (suppressed == 1) "delta" else "deltas"} suppressed"

    def label(ic: ACursor): String = {
      val name = opt[String](ic, "test").getOrElse(get[String](ic, "project"))
      val project = opt[String](ic, "test").map(_ => s"  ${C.CYAN}(${get[String](ic, "project")})${C.RESET}").getOrElse("")
      s"$name$project"
    }

    def deltaSection(field: String, title: String, color: String): Unit =
      arr(c, field) match {
        case Nil   => ()
        case items =>
          lines += ""
          lines += s"$color$title${C.RESET}"
          items.foreach { ic =>
            val baseMs = get[Long](ic, "baseMs")
            val targetMs = get[Long](ic, "targetMs")
            val deltaMs = get[Long](ic, "deltaMs")
            lines += f"  ${fmtMs(baseMs)}%9s ${C.CYAN}→${C.RESET} ${fmtMs(targetMs)}%9s  ($color${fmtDelta(deltaMs)}${C.RESET})  ${label(ic)}"
          }
      }

    deltaSection("slower", "slower", C.RED)
    deltaSection("faster", "faster", C.GREEN)

    arr(c, "slowestInTarget") match {
      case Nil   => ()
      case items =>
        lines += ""
        lines += s"slowest in #${side(c, "target").historyId}"
        items.foreach(ic => lines += f"  ${fmtMs(get[Long](ic, "durationMs"))}%9s  ${label(ic)}")
    }

    lines.result().mkString("\n")
  }
}
