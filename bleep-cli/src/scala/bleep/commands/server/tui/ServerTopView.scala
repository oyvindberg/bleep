package bleep
package commands
package server
package tui

import bleep.bsp.ServerState
import bleep.bsp.protocol.DaemonStatus
import bleep.testing.FancyBuildDisplay.Palette
import jatatui.core.layout.Flex
import jatatui.core.style.Style
import jatatui.react.Element
import jatatui.react.Components._
import jatatui.core.text.{Line, Span, Text}
import jatatui.widgets.Borders
import jatatui.widgets.block.Block
import jatatui.widgets.paragraph.Paragraph

import scala.jdk.CollectionConverters._

import java.time.Duration

/** The dashboard, as a pure function of state.
  *
  * Nothing here touches a terminal, a clock or a socket — given a [[ServerTopState]] it returns an `Element`, which a test can render into an off-screen buffer
  * and compare as text. That is the whole reason `update` and the polling live elsewhere.
  *
  * The layout leans on three levels of emphasis rather than one flat wall of text: section headings in accent, labels dim, values bright — and the numbers that
  * answer "is this server busy, or fat" get gauges, so they read at a glance instead of having to be parsed.
  */
object ServerTopView {
  import ServerTopState._

  // The same theme the build display and the picker use, so the three look like one program. Crucially every cell carries the background: this palette is built
  // for a dark one, and on a terminal supplying its own light background the text is close to invisible.
  private def style(color: jatatui.core.style.Color): Style = Palette.style(color)
  private def bold(color: jatatui.core.style.Color): Style = Palette.bold(color)

  private val LabelWidth = 14

  /** Everything you can act on is clickable, and every click is a [[ServerTopState.Msg]] — the same messages the keys produce, through the same pure `update`.
    * Mouse and keyboard cannot drift apart because there is only one path.
    *
    * `dispatch` is the one impure thing the view is handed. Tests pass a recorder, click at a coordinate through jatatui's harness, and assert on what came
    * out, so the click targets are covered without a terminal.
    */
  def render(state: ServerTopState, dispatch: Msg => Unit): Element =
    // Paint the background first and let everything else land on top. Widgets that set no background of their own leave these cells alone, so this reaches the
    // borders and the empty space below the last line too.
    stack(
      // Clear first: a Block with only a style set recolours cells without replacing their symbols, so characters from a previous, longer frame stayed on
      // screen underneath — a line from the Overview was still visible after switching to a shorter tab.
      widget(jatatui.widgets.Clear.INSTANCE),
      widget(Block.empty().withStyle(Palette.background)),
      column(
        length(1, text("", style(Palette.textDim))),
        length(1, header(state, dispatch)),
        length(1, text("", style(Palette.textDim))),
        fill(
          1,
          state.screen match {
            case Screen.Main => mainBody(state, dispatch)
            case Screen.Dead => deadPane(state, dispatch)
          }
        ),
        length(1, text("", style(Palette.textDim))),
        length(1, footer(state, dispatch))
      )
    )

  /** Wrap anything in its own click target. The area is the element's own, so the hit box is exactly what you see. */
  private def clickable(msg: => Msg, dispatch: Msg => Unit, inner: Element): Element =
    component { ctx =>
      ctx.onClick(() => dispatch(msg))
      inner
    }

  // ── header ──────────────────────────────────────────────────────

  /** Just the title, the count, and the way to the stopped servers. The numbers that matter get the summary below, where they have room to be read. */
  private def header(state: ServerTopState, dispatch: Msg => Unit): Element = {
    val live = state.live
    val running = live.count(_.info.isRunning)
    val wedged = live.length - running
    // Held slots sum across servers because each is really holding them, but capacity does not: every server on this machine sees the same cores, so adding
    // their totals claimed 36 slots on an 18-core machine.
    val busySlots = live.flatMap(_.status).map(_.machine.usedCpu).sum
    val totalSlots = live.flatMap(_.status).map(_.machine.totalCpu).maxOption.getOrElse(0)
    val queued = live.flatMap(_.status).map(_.machine.waiting.size).sum

    val parts = List(
      Some(s"$running running"),
      Option.when(wedged > 0)(s"$wedged wedged"),
      Option.when(busySlots > 0)(s"$busySlots of $totalSlots slots busy" + (if (queued > 0) s", $queued queued" else ""))
    ).flatten

    val dead = state.dead
    val deadLabel = state.screen match {
      case Screen.Main => s" [ d  ${dead.size} stopped · ${dead.map(_.info.sizeMb).sum} MB ] "
      case Screen.Dead => " [ esc  back to running ] "
    }

    row(
      length(23, text(" BLEEP COMPILE SERVERS", bold(Palette.info))),
      fill(1, text(parts.mkString(" · "), style(Palette.textMuted))),
      length(deadLabel.length, clickable(Msg.Key(KeyPress.ShowDead), dispatch, text(deadLabel, Palette.boldOnSurface(Palette.textMuted))))
    )
  }

  // ── main view ───────────────────────────────────────────────────

  /** Three levels, top to bottom: how much of this machine bleep is using and the one thing most responsible; one line per server, newest first; and the
    * selected server in detail. Each level answers its question without the one below it.
    */
  private def mainBody(state: ServerTopState, dispatch: Msg => Unit): Element =
    component { ctx =>
      val height = ctx.area().map[Int](_.height).orElse(30)
      val summary = summaryLines(state)
      if (state.live.isEmpty)
        column(length(summary.length, widget(Paragraph.of(Text.fromLines(summary.asJava)))), fill(1, text("", style(Palette.textDim))))
      else {
        val listHeight = math.max(3, math.min(state.live.length + 2, height / 3))
        column(
          length(summary.length, widget(Paragraph.of(Text.fromLines(summary.asJava)))),
          length(1, text("", style(Palette.textDim))),
          length(listHeight, serverList(state, dispatch)),
          fill(1, detail(state, dispatch))
        )
      }
    }

  private val BarWidth = 32

  /** The answer to "why is my machine slow", as far as bleep is concerned: its share of memory and of CPU, and one sentence naming the main cause. */
  private def summaryLines(state: ServerTopState): List[Line] = {
    val live = state.live
    if (live.isEmpty)
      List(
        lineOf("  No compile servers running — bleep is not using this machine right now.", Palette.textMuted),
        lineOf(if (state.dead.isEmpty) "" else s"  ${state.dead.size} stopped ones left directories behind; [d] lists them.", Palette.textDim)
      )
    else {
      val memoryMb = live.flatMap(_.totalFootprintMb).sum
      val unmeasured = live.count(_.totalFootprintMb.isEmpty)
      val cores = live.flatMap(state.treeCpuPercent).sum / 100.0
      val physical = state.machine.physicalMemoryMb

      val memoryCaption =
        (if (physical > 0) s"${mb(memoryMb)} of ${mb(physical)} — ${(ratio(memoryMb, physical) * 100).round}% of this machine" else mb(memoryMb)) +
          (if (unmeasured > 0) s"  ($unmeasured server${if (unmeasured == 1) "" else "s"} could not be measured)" else "")
      val coresCaption = f"$cores%.1f of ${state.machine.cores} cores"

      val (verdictText, verdictColor) = verdict(state)
      List(
        barLine("Memory", if (physical > 0) ratio(memoryMb, physical) else 0.0, memoryCaption),
        barLine("CPU", ratio((cores * 100).round, state.machine.cores.toLong * 100), coresCaption),
        boldLineOf(s"  $verdictText", verdictColor)
      )
    }
  }

  private def barLine(label: String, value: Double, caption: String): Line = {
    val filled = math.max(0, math.min(BarWidth, math.round(value * BarWidth).toInt))
    val color = colorFor(value)
    Line.from(
      Span.styled(s"  ${label.padTo(8, ' ')}", bold(Palette.text)),
      Span.styled("█" * filled, style(color)),
      Span.styled("░" * (BarWidth - filled), style(Palette.border)),
      Span.styled(s"  $caption", bold(Palette.text))
    )
  }

  /** One sentence naming what most of the memory is. Servers from other bleep versions come first when they hold a real share: nothing new connects to them, so
    * whatever keeps them alive is a client started long ago and still pinned to that version — that is the thing to fix, and it is named.
    */
  private def verdict(state: ServerTopState): (String, jatatui.core.style.Color) = {
    val measured = state.live.flatMap(row => row.totalFootprintMb.map(row -> _))
    val total = measured.map(_._2).sum
    val outdated = measured.filter(_._1.isOutdated)
    val outdatedMb = outdated.map(_._2).sum

    if (measured.isEmpty) ("Nothing could be measured on this platform.", Palette.textDim)
    else if (outdatedMb >= 1024 && outdatedMb * 4 >= total) {
      val keeper = outdated.sortBy(-_._2).flatMap(_._1.parent).headOption
      val keptBy =
        keeper.map(p => s", kept alive by ${p.label} (pid ${p.pid}${p.startedAtEpochMs.map(s => s", up ${humanDuration(state.nowMs - s)}").getOrElse("")})")
      val servers = if (outdated.size == 1) "1 server" else s"${outdated.size} servers"
      (s"▲ ${mb(outdatedMb)} of it is $servers from other bleep versions than this one${keptBy.getOrElse("")}", Palette.warning)
    } else {
      val (biggest, biggestMb) = measured.maxBy(_._2)
      (s"Largest: ${biggest.name} at ${mb(biggestMb)}, ${doing(biggest)._1}", Palette.text)
    }
  }

  /** A few words for what a server is doing, for the one line it gets in the list. */
  private def doing(row: ServerRow): (String, jatatui.core.style.Color) =
    row.status match {
      case None if row.info.state == ServerState.Wedged => ("wedged — alive but not answering", Palette.error)
      case None                                         => (row.error.map(e => s"cannot ask: ${e.message}").getOrElse("not answering"), Palette.warning)
      case Some(status)                                 =>
        val machine = status.machine
        val working = machine.active.filter(_.cpu > 0)
        working.headOption match {
          case Some(first) =>
            val more = if (working.size > 1) s" +${working.size - 1} more" else ""
            val queued = if (machine.waiting.nonEmpty) s", ${machine.waiting.size} queued" else ""
            (s"${verb(first.kind)} ${shortLabel(first.label)}$more$queued", Palette.accent)
          case None if machine.waiting.nonEmpty => (s"stalled — ${machine.waiting.size} queued, nothing running", Palette.warning)
          case None                             => ("idle", Palette.textDim)
        }
    }

  private def verb(kind: String): String = kind match {
    case "Compile"       => "compiling"
    case "TestFork"      => "testing"
    case "SourcegenFork" => "generating sources for"
    case "KspFork"       => "processing symbols for"
    case other           => other.toLowerCase
  }

  /** Ledger labels carry their kind as a prefix — `compile:dquery-generated/dquery/ast` — which the verb already says. */
  private def shortLabel(label: String): String = label.indexOf(':') match {
    case -1 => label
    case i  => label.substring(i + 1)
  }

  // ── the server list ─────────────────────────────────────────────

  /** Live servers, newest first, one line each — no detail, so a dozen fit. The bars are what make the expensive one stand out; the selected one is opened
    * below.
    */
  private def serverList(state: ServerTopState, dispatch: Msg => Unit): Element =
    component { ctx =>
      val area = ctx.area()
      val innerHeight = area.map[Int](a => math.max(1, a.height - 2)).orElse(10)
      val innerWidth = area.map[Int](a => math.max(1, a.width - 2)).orElse(100)
      val ordered = state.live.zipWithIndex
      val cursor = math.max(0, ordered.indexWhere(_._2 == state.selected))
      val offset = math.max(0, math.min(cursor - innerHeight + 1, ordered.length - innerHeight))
      val visible = ordered.slice(offset, offset + innerHeight)
      val biggest = ordered.flatMap(_._1.totalFootprintMb).maxOption.getOrElse(0L)

      ctx.onClick { (event: jatatui.react.MouseEvent) =>
        area.ifPresent(a => visible.lift(event.y - a.y - 1).foreach { case (_, index) => dispatch(Msg.SelectRow(index)) })
      }

      val lines = visible.map { case (row, index) => serverListLine(state, row, index == state.selected, biggest, innerWidth) }
      box(" servers ", Borders.ALL, widget(Paragraph.of(Text.fromLines(lines.asJava))))
    }

  private def serverListLine(state: ServerTopState, row: ServerRow, selected: Boolean, biggestMb: Long, width: Int): Line = {
    def st(color: jatatui.core.style.Color, emphasised: Boolean): Style =
      if (selected && emphasised) Palette.boldOnSurface(color) else if (selected) Palette.onSurface(color) else if (emphasised) bold(color) else style(color)

    val (marker, markerColor) = row.info.state match {
      case ServerState.Running => ("●", Palette.success)
      case _                   => ("!", Palette.error)
    }
    val memory = row.totalFootprintMb
    // In proportion to the biggest server, not the machine: the summary already says what share of the machine they have, and against 48 GB every server
    // drew the same single cell. Here the bar's job is to compare servers with each other.
    val scale = math.max(1L, biggestMb)
    val barWidth = 12
    def cells(mb: Long): Int = math.min(barWidth, math.round(ratio(mb, scale) * barWidth).toInt)
    // Stacked: the daemon's own memory, then its forks', so the bar says at a glance whether a server is fat itself or is carrying a fleet of test JVMs.
    val selfCells = row.selfFootprintMb.map(m => math.max(if (m > 0) 1 else 0, cells(m))).getOrElse(0)
    val forkedCells = row.forkedFootprintMb.map(m => math.max(if (m > 0) 1 else 0, math.min(barWidth - selfCells, cells(m)))).getOrElse(0)
    // The daemon's own figure, and what its forks add — "3.7 GB +3.7 GB forked". The total is the bar's length and the list's order.
    val forkedText = row.forkedFootprintMb.filter(_ > 0).map(f => s"+${mb(f)} forked").getOrElse("")
    val (doingText, doingColor) = doing(row)
    // A server up for days is the one to ask about: it has outlived any build that needed it, and is being kept alive by something long-running.
    val uptime = row.startedAtEpochMs.map(state.nowMs - _)
    val uptimeColor = if (uptime.exists(_ >= 24L * 3600 * 1000)) Palette.warning else Palette.textDim

    // Version and JVM are what make two servers two servers (both are in the socket directory's hash), so both are shown. Older versions are told apart by
    // colour; the summary says why that matters.
    val tag = row.info.identity.map(id => s"${shortVersion(id.bleepVersion)} · ${shortJvm(id.jvmName, id.jvmVersion)}").getOrElse("unknown version")
    // The this-build marker sits beside the hash, on the left, because it is the one label a reader acts on and a narrow terminal cuts from the right. The
    // column only exists when some server is this build's, so it costs nothing otherwise.
    val markerColumn = if (state.live.exists(_.isCurrent)) 14 else 0
    val tagWidth = math.min(48, math.max(0, width * 2 / 5))

    val fixed = List(
      Span.styled(if (selected) "▸" else " ", st(Palette.info, emphasised = true)),
      Span.styled(s"$marker ", st(markerColor, emphasised = true)),
      Span.styled(row.name.padTo(11, ' '), st(Palette.text, emphasised = true)),
      Span.styled((if (row.isCurrent) "← this build" else "").padTo(markerColumn, ' '), st(Palette.accent, emphasised = true)),
      Span.styled(row.selfFootprintMb.orElse(memory).map(mb).getOrElse("—").reverse.padTo(8, ' ').reverse + " ", st(Palette.text, emphasised = true)),
      Span.styled(forkedText.padTo(16, ' '), st(Palette.accent, emphasised = false)),
      Span.styled("█" * selfCells, st(Palette.info, emphasised = false)),
      Span.styled("█" * forkedCells, st(Palette.accent, emphasised = false)),
      Span.styled("░" * (barWidth - selfCells - forkedCells), st(Palette.border, emphasised = false)),
      Span.styled(state.treeCpuPercent(row).map(pctOfCore).getOrElse("").reverse.padTo(6, ' ').reverse, st(Palette.textMuted, emphasised = false)),
      Span.styled(uptime.map(u => s"up ${humanDuration(u)}").getOrElse("").reverse.padTo(10, ' ').reverse + "  ", st(uptimeColor, emphasised = false))
    )
    val fixedWidth = fixed.map(_.content.length).sum
    val doingSpans = fitSpans(List(Span.styled(doingText, st(doingColor, emphasised = false))), math.max(0, width - fixedWidth - tagWidth), selected)
    val tagColor = if (row.isCurrent) Palette.accent else if (row.isOutdated) Palette.warning else Palette.info
    val tagText = if (tag.length > tagWidth) tag.take(math.max(0, tagWidth - 1)) + "…" else tag.padTo(tagWidth, ' ')
    Line.from((fixed ++ doingSpans :+ Span.styled(tagText, st(tagColor, emphasised = false)))*)
  }

  /** Cut a run of spans to exactly `width` cells — truncating with an ellipsis, or padding — so the columns after it start at the same place on every line. */
  private def fitSpans(spans: List[Span], width: Int, selected: Boolean): List[Span] = {
    val out = List.newBuilder[Span]
    var used = 0
    spans.foreach { span =>
      val content = span.content
      val room = width - used
      if (room > 0) {
        if (content.length <= room) { out += span; used += content.length }
        else {
          out += Span.styled(content.take(math.max(0, room - 1)) + "…", span.style)
          used = width
        }
      }
    }
    val padStyle = if (selected) Palette.onSurface(Palette.text) else style(Palette.text)
    out += Span.styled(" " * math.max(0, width - used), padStyle)
    out.result()
  }

  // ── the processes tab ───────────────────────────────────────────

  /** Right-hand columns of every tree line. Fixed, so memory and CPU line up down the whole tree whatever the indentation. */
  private val MemoryWidth = 10
  private val CpuWidth = 7
  private val AgeWidth = 9
  private val ColumnsWidth = MemoryWidth + CpuWidth + AgeWidth

  /** One line of the tree: the indented description on the left, and the three numbers on the right. */
  private case class TreeLine(left: List[Span], memory: String, cpu: String, age: String)

  /** The selected server's work, then its processes as a tree — the daemon and every JVM it forked, each charged for everything beneath it. Headed by who
    * started it, which for an old server is usually the answer to why it is still here.
    */
  private def processesPane(state: ServerTopState, row: ServerRow): Element =
    component { ctx =>
      val width = ctx.area().map[Int](a => math.max(1, a.width - 2)).orElse(100)

      val startedBy = row.parent match {
        case Some(parent) =>
          val age = parent.startedAtEpochMs.map(started => s", running ${humanDuration(state.nowMs - started)}").getOrElse("")
          List(lineOf(s"  started by ${parent.label} (pid ${parent.pid}$age) — it keeps this server in use while it lives", Palette.textMuted), Line.empty())
        case None => Nil
      }

      val tasks = row.status.map(taskLines).getOrElse(Nil)
      val processes: List[TreeLine] = row.processes match {
        case None      => List(TreeLine(List(Span.styled("no process to measure — it exited, or never recorded a pid", style(Palette.textDim))), "", "", ""))
        case Some(all) =>
          val children = all.filter(_.parentPid.isDefined).groupBy(_.parentPid.get)
          all.filter(_.parentPid.isEmpty).flatMap(root => processLines(state, root, children, row.status))
      }

      val heading = List(
        sectionOf(if (tasks.isEmpty) "WORK — none, idle" else s"WORK — ${tasks.size}")
      )
      val lines =
        startedBy ++ heading ++ tasks.map(renderTreeLine(_, width)) ++
          List(Line.empty(), sectionOf("PROCESSES — memory and cpu include everything beneath each one")) ++ processes.map(renderTreeLine(_, width))
      box("", Borders.ALL, widget(Paragraph.of(Text.fromLines(lines.asJava))))
    }

  private def renderTreeLine(line: TreeLine, width: Int): Line = {
    val leftWidth = math.max(0, width - ColumnsWidth)
    Line.from(
      (fitSpans(Span.styled("  ", style(Palette.text)) :: line.left, leftWidth, selected = false) ++ List(
        Span.styled(line.memory.reverse.padTo(MemoryWidth, ' ').reverse, bold(Palette.text)),
        Span.styled(line.cpu.reverse.padTo(CpuWidth, ' ').reverse, style(Palette.textMuted)),
        Span.styled(line.age.reverse.padTo(AgeWidth, ' ').reverse, style(Palette.textDim))
      ))*
    )
  }

  /** The work the governor has admitted or queued. Forks' memory reservations are left out: they are not work, and the process tree shows them as what they are
    * — processes.
    */
  private def taskLines(status: DaemonStatus): List[TreeLine] = {
    val machine = status.machine
    val running = machine.active.filter(_.cpu > 0).map { entry =>
      TreeLine(
        List(
          Span.styled("▸ ", bold(Palette.accent)),
          Span.styled(workName(entry.kind, 1).padTo(12, ' '), style(Palette.accent)),
          Span.styled(entry.label, style(Palette.text)),
          Span.styled(s"  ${entry.cpu} slot${if (entry.cpu == 1) "" else "s"}", style(Palette.textDim))
        ),
        "",
        "",
        humanDuration(entry.ageMs)
      )
    }
    val waiting = machine.waiting.map { entry =>
      TreeLine(
        List(
          Span.styled("· ", bold(Palette.warning)),
          Span.styled(workName(entry.kind, 1).padTo(12, ' '), style(Palette.warning)),
          Span.styled(entry.label, style(Palette.textMuted)),
          Span.styled(s"  queued for ${entry.cpu} slot${if (entry.cpu == 1) "" else "s"}", style(Palette.warning))
        ),
        "",
        "",
        humanDuration(entry.ageMs)
      )
    }
    running ++ waiting
  }

  /** A process and everything beneath it. The memory and CPU columns are the subtree's — what this process costs the machine including what it spawned — and a
    * process with children also says what it costs on its own.
    */
  private def processLines(
      state: ServerTopState,
      process: ProcessTree.Sample,
      children: Map[Long, List[ProcessTree.Sample]],
      daemon: Option[DaemonStatus]
  ): List[TreeLine] = {
    val kids = children.getOrElse(process.pid, Nil).sortBy(p => (p.startedAtEpochMs.getOrElse(0L), p.pid))
    val subtree = subtreeOf(process, children)
    val memory = subtree.flatMap(_.footprintMb)
    val cpu = subtree.flatMap(p => state.cpuPercent.get(p.pid))

    val heap = daemon.map(status => s"  heap ${mb(status.jvm.heapUsedMb)}/${mb(status.jvm.heapMaxMb)}").getOrElse("")
    val self = if (kids.nonEmpty) process.footprintMb.map(own => s"  self ${mb(own)}").getOrElse("") else ""

    val line = TreeLine(
      List(
        Span.styled(process.label, bold(if (daemon.isDefined) Palette.text else Palette.textMuted)),
        Span.styled(s"  pid ${process.pid}", style(Palette.textDim)),
        Span.styled(heap, style(Palette.textMuted)),
        Span.styled(self, style(Palette.textDim))
      ),
      memory = if (memory.isEmpty) "n/a" else mb(memory.sum),
      cpu = if (cpu.isEmpty) "" else pctOfCore(cpu.sum),
      age = process.startedAtEpochMs.map(started => humanDuration(state.nowMs - started)).getOrElse("")
    )
    line :: withBranches(kids.map(kid => processLines(state, kid, children, None)))
  }

  private def subtreeOf(process: ProcessTree.Sample, children: Map[Long, List[ProcessTree.Sample]]): List[ProcessTree.Sample] =
    process :: children.getOrElse(process.pid, Nil).flatMap(subtreeOf(_, children))

  /** Hang a list of subtrees off a parent with box-drawing branches: `├─` before every subtree but the last, `└─` before the last, and the matching rail down
    * the left of each subtree's own descendants.
    */
  private def withBranches(subtrees: List[List[TreeLine]]): List[TreeLine] = {
    val branch = style(Palette.border)
    subtrees.zipWithIndex.flatMap { case (subtree, i) =>
      val last = i == subtrees.length - 1
      subtree match {
        case Nil          => Nil
        case head :: tail =>
          head.copy(left = Span.styled(if (last) "└─ " else "├─ ", branch) :: head.left) ::
            tail.map(line => line.copy(left = Span.styled(if (last) "   " else "│  ", branch) :: line.left))
      }
    }
  }

  /** `1.0.0-M11+32-80f5bbb7-SNAPSHOT` carries about six useful characters. The shared prefix and suffix are noise in a column meant to show a difference. */
  private def shortVersion(version: String): String =
    version.stripPrefix("1.0.0-").stripSuffix("-SNAPSHOT")

  /** The JvmKey's `name` already carries the version — `graalvm-community:25.0.1` — and its `version` is the JVM *index*, almost always `default`. Appending
    * that made the column overflow and truncate to `graalvm:25.0.1 …`, hiding the very thing it exists to show. Only a non-default index is worth the width.
    */
  private def shortJvm(name: String, index: String): String = {
    val flavour = name.replace("-community", "")
    if (index == "default") flavour else s"$flavour ($index)"
  }

  // ── stopped servers ─────────────────────────────────────────────

  /** Socket directories with no process behind them, biggest first — the screen you come to in order to clear them out. */
  private def deadPane(state: ServerTopState, dispatch: Msg => Unit): Element =
    component { ctx =>
      ctx.onScroll { event =>
        event.kind match {
          case jatatui.react.MouseEvent.Kind.SCROLL_UP   => dispatch(Msg.ScrollDead(-3))
          case jatatui.react.MouseEvent.Kind.SCROLL_DOWN => dispatch(Msg.ScrollDead(3))
          case _                                         => ()
        }
      }
      val dead = state.dead.sortBy(row => (-row.info.sizeBytes, row.hash))
      val totalMb = dead.map(_.info.sizeMb).sum

      val intro = List(
        lineOf(
          "  What is left of compile servers that are no longer running: logs, metrics and server.json. None of it uses memory or CPU — only",
          Palette.textDim
        ),
        lineOf("  disk. A crashed server's log is the evidence for why; read it with `bleep server log <id> --generation 1` before clearing.", Palette.textDim),
        Line.empty(),
        Line.from(
          Span.styled("  " + "server".padTo(10, ' '), style(Palette.textDim)),
          Span.styled("state".padTo(16, ' '), style(Palette.textDim)),
          Span.styled("bleep".padTo(26, ' '), style(Palette.textDim)),
          Span.styled("jvm".padTo(22, ' '), style(Palette.textDim)),
          Span.styled("on disk".reverse.padTo(9, ' ').reverse, style(Palette.textDim))
        )
      )

      val rows =
        if (dead.isEmpty) List(lineOf("  nothing — every server directory has a live server behind it", Palette.textDim))
        else
          dead.drop(state.deadScroll).map { row =>
            val color = row.info.state match {
              case ServerState.Dead(true) => Palette.error
              case _                      => Palette.textMuted
            }
            val version = row.info.identity.map(id => shortVersion(id.bleepVersion)).getOrElse("unknown")
            val jvm = row.info.identity.map(id => shortJvm(id.jvmName, id.jvmVersion)).getOrElse("unknown")
            Line.from(
              Span.styled("  " + row.hash.take(8).padTo(10, ' '), style(Palette.text)),
              Span.styled(row.info.state.label.padTo(16, ' '), style(color)),
              Span.styled(fit(version, 26), style(Palette.textMuted)),
              Span.styled(fit(jvm, 22), style(Palette.textMuted)),
              Span.styled(s"${row.info.sizeMb} MB".reverse.padTo(9, ' ').reverse, style(Palette.text))
            )
          }

      box(s" stopped servers — ${dead.size}, $totalMb MB on disk ", Borders.ALL, widget(Paragraph.of(Text.fromLines((intro ++ rows).asJava))))
    }

  /** Pad to the column, or cut with an ellipsis and always leave a space, so a long value cannot run into its neighbour. */
  private def fit(content: String, width: Int): String =
    if (content.length >= width) content.take(math.max(0, width - 2)) + "… " else content.padTo(width, ' ')

  private def mb(value: Long): String =
    if (value >= 1024) f"${value / 1024.0}%.1f GB" else s"$value MB"

  /** A share of one core, the way `top` counts: 250% is two and a half cores. */
  private def pctOfCore(value: Double): String = f"$value%.0f%%"

  // ── detail ──────────────────────────────────────────────────────

  private def detail(state: ServerTopState, dispatch: Msg => Unit): Element =
    state.selectedRow match {
      case None      => packed(" detail ", List(text("nothing to show", style(Palette.textDim))))
      case Some(row) =>
        // Processes, log and startup come from outside the daemon and work whether or not it answers. The rest is the daemon's own account, and a server that
        // cannot be asked says why rather than rendering an empty pane that looks like "nothing is happening".
        def asked(lines: DaemonStatus => List[Line]): Element =
          row.status match {
            case Some(status) => textPane(lines(status))
            case None         =>
              textPane(
                List(lineOf(s"  ${row.error.map(_.message).getOrElse(s"${row.info.state.label} — not answering, so it cannot report this")}", Palette.warning))
              )
          }
        column(
          length(1, tabBar(state, dispatch)),
          fill(
            1,
            state.tab match {
              case Tab.Processes => processesPane(state, row)
              case Tab.Log       => logPane(state, dispatch)
              // Overview is a short, fixed set of rows and needs real elements for its gauges. The others are plain text of unbounded length — a workspace
              // list, a queue, a classpath — and one element per line means one layout constraint per line, solved every frame. 202 classpath entries froze
              // the dashboard outright.
              case Tab.Overview   => asked(overviewLines)
              case Tab.Config     => asked(configLines)
              case Tab.Workspaces => asked(workspaceLines)
              case Tab.Activity   => asked(activityLines(row, _))
              case Tab.Startup    => startupPane(state, row.info.identity, dispatch)
            }
          )
        )
    }

  /** Hand-rolled rather than the `tabs` intrinsic, because each title needs its own click target. */
  private def tabBar(state: ServerTopState, dispatch: Msg => Unit): Element = {
    val cells = Tab.all.map { tab =>
      val selected = tab == state.tab
      val label = if (selected) s"[${tab.title}]" else s" ${tab.title} "
      val cellStyle = if (selected) Palette.boldOnSurface(Palette.info) else style(Palette.textDim)
      length(tab.title.length + 3, clickable(Msg.SelectTab(tab), dispatch, text(label, cellStyle)))
    }
    row((cells :+ fill(1, text("", style(Palette.textDim))))*)
  }

  /** Written out in words rather than abbreviations.
    *
    * The numbers are only useful if you know what they mean: "live set" and "heap used" answer different questions, and "fork mem" answers one most people do
    * not know they have. Each row says what it is measuring, and the three that answer "is this server in trouble" get a bar, because a bar answers that at a
    * glance where a pair of numbers has to be read and divided.
    *
    * Built as lines rather than elements like every other pane: a container solves a layout constraint per child, and with more lines than rows it drops one
    * from the *middle* rather than clipping the end — the CAPACITY heading disappeared exactly that way.
    */
  private def overviewLines(status: DaemonStatus): List[Line] = {
    val jvm = status.jvm
    val machine = status.machine

    val retained =
      if (jvm.heapLiveMb < 0) "not reported by this JVM"
      else s"${jvm.heapLiveMb} MB still held after the last collection"

    val collections = jvm.gc.filter(_.count > 0)
    val gcSummary =
      if (collections.isEmpty) "none yet"
      else collections.map(gc => s"${gc.name.replace("ZGC ", "")}: ${gc.count} runs taking ${gc.timeMs} ms").mkString("   ")

    List(
      statusLine(status),
      fieldOf("Last activity", lastActivity(status)),
      Line.empty(),
      sectionOf("MEMORY — how much of its heap this server is using"),
      gaugeLine("Heap in use", ratio(jvm.heapUsedMb, jvm.heapMaxMb), s"${jvm.heapUsedMb} MB of ${jvm.heapMaxMb} MB"),
      fieldOf("Retained", retained),
      fieldOf("Committed", s"${jvm.heapCommittedMb} MB reserved from the OS, ${jvm.nonHeapUsedMb} MB outside the heap"),
      fieldOf("Collections", gcSummary),
      Line.empty(),
      sectionOf("CAPACITY — what this server may spend on compiling"),
      gaugeLine("Compile slots", ratio(machine.usedCpu.toLong, machine.totalCpu.toLong), s"${machine.usedCpu} of ${machine.totalCpu} in use"),
      gaugeLine("Memory for forks", ratio(machine.usedMemoryMb, machine.totalMemoryMb), s"${machine.usedMemoryMb} MB of ${machine.totalMemoryMb} MB"),
      fieldOf("Threads", s"${jvm.threads} alive, peak ${jvm.peakThreads}, ${jvm.daemonThreads} of them background"),
      fieldOf("Processor", s"${pct(jvm.cpuProcess)} of the machine used by this server, ${pct(jvm.cpuSystem)} used in total"),
      fieldOf("Open files", jvm.openFileDescriptors.map(count => s"$count file descriptors").getOrElse("not reported on this platform")),
      Line.empty(),
      sectionOf("WHAT IT IS KEEPING WARM — so the next build does not pay for it again"),
      fieldOf("Builds", s"${status.buildCache.cachedWorkspaces.size} of ${status.buildCache.bound} workspaces cached"),
      fieldOf(
        "Compile analysis",
        s"${status.analysisCache.entries} entries, ${status.analysisCache.fileBytes / (1024 * 1024)} MB, " +
          s"${status.analysisCache.sharedAnalyses} shared between workspaces"
      )
    )
  }

  /** One sentence for what this server is doing, in the place the eye lands first.
    *
    * Counting compiles alone undersells it badly: a server running one compile and sixteen test suites reported "1 compiling", because `activeCompiles` counts
    * exactly what its name says. What makes a server busy is the slots it is holding, whatever kind of work holds them, so that is what this says.
    */
  private def statusLine(status: DaemonStatus): Line = {
    val machine = status.machine
    val working = machine.active.filter(_.cpu > 0)
    val forks = machine.active.filter(entry => entry.cpu == 0 && entry.memoryMb > 0)

    val breakdown = working.groupBy(_.kind).toList.sortBy(-_._2.size).map { case (kind, entries) => s"${entries.size} ${workName(kind, entries.size)}" }
    val waiting = if (machine.waiting.nonEmpty) s", ${machine.waiting.size} waiting for capacity" else ""

    val (summary, color) =
      if (working.nonEmpty) {
        val slots = s"${machine.usedCpu} of ${machine.totalCpu} slots"
        (s"Busy — $slots: ${breakdown.mkString(", ")}$waiting", Palette.accent)
      } else if (machine.waiting.nonEmpty) (s"Stalled — nothing running, ${machine.waiting.size} waiting for capacity", Palette.warning)
      else if (forks.nonEmpty) (s"Idle — ${forks.size} forked JVM(s) kept warm, holding ${forks.map(_.memoryMb).sum} MB", Palette.text)
      else if (status.connections.exists(!_.observer)) ("Idle, with a client connected", Palette.text)
      else ("Idle, nobody connected", Palette.textDim)

    boldLineOf(s"  $summary", color)
  }

  /** The governor's kind names are internal ("TestFork", "SourcegenFork"); these are what the work is called. */
  private def workName(kind: String, count: Int): String = {
    val singular = kind match {
      case "Compile"       => "compile"
      case "TestFork"      => "test suite"
      case "SourcegenFork" => "sourcegen"
      case "KspFork"       => "symbol processor"
      case other           => other.toLowerCase
    }
    if (count == 1) singular else s"${singular}s"
  }

  /** How long since the server last did anything for a real client — the same clock its idle shutdown counts down, so it also says how long it has left. */
  private def lastActivity(status: DaemonStatus): String =
    status.idleMs match {
      case None       => "not reported by this server"
      case Some(idle) =>
        val timeout = status.config.compileServerIdleTimeoutMillis
        val ago = if (idle < 1000) "just now" else s"${humanDuration(idle)} ago"
        if (timeout <= 0) s"$ago (idle shutdown disabled)"
        else if (status.connections.exists(!_.observer)) s"$ago — a client is connected, so the idle clock is not running"
        else s"$ago — shuts down after ${humanDuration(timeout)} idle"
    }

  /** A bar drawn as text rather than with the gauge widget, so the whole pane can be one paragraph. Also lets the bar and its percentage share one colour. */
  private def gaugeLine(label: String, value: Double, caption: String): Line = {
    val width = 30
    val filled = math.max(0, math.min(width, math.round(value * width).toInt))
    val color = colorFor(value)
    Line.from(
      Span.styled(s"  ${label.padTo(LabelWidth + 4, ' ')}", style(Palette.textDim)),
      Span.styled(f"${(value * 100).toInt}%3d%% ", bold(color)),
      Span.styled("█" * filled, style(color)),
      Span.styled("─" * (width - filled), style(Palette.border)),
      Span.styled(s"  $caption", style(Palette.text))
    )
  }

  /** Which workspaces this server is holding, and what each is doing — the answer to "whose build is this". */
  private def workspaceLines(status: DaemonStatus): List[Line] =
    if (status.workspaces.isEmpty) List(lineOf("  No workspaces loaded — nothing has asked this server to build anything yet.", Palette.textDim))
    else
      sectionOf(s"${status.workspaces.size} WORKSPACE(S) LOADED") ::
        status.workspaces.flatMap { workspace =>
          val cached = if (workspace.buildCached) "build cached" else "build not cached"
          val busy = if (workspace.activeOperations.isEmpty) "idle" else s"${workspace.activeOperations.size} operation(s) running"
          List(
            boldLineOf(s"  ${workspace.path}", Palette.text),
            lineOf(s"      $cached · $busy", Palette.textDim)
          ) ++ workspace.activeOperations.map { op =>
            lineOf(s"      ▸ ${op.operation}  ${op.projects.mkString(", ")}  started ${humanDuration(op.startedAgoMs)} ago", Palette.accent)
          }
        }

  /** What the server is doing right now, and what is stacked up behind it.
    *
    * Work and forked JVMs are listed apart because they are charged for different things, and mixing them reads as nonsense: a suite costs a slot and no
    * memory, while the JVM it runs in costs memory and no slot.
    *
    * The slot is charged to the work, not the process, so a suite running in a fork appears twice — once above holding the slot, once below holding the memory.
    * A fork with no suite is between jobs and kept warm, still holding its footprint. Either way the total is right: one slot per running suite.
    */
  private def activityLines(row: ServerRow, status: DaemonStatus): List[Line] = {
    val machine = status.machine
    val (forks, work) = machine.active.partition(entry => entry.cpu == 0 && entry.memoryMb > 0)

    val running =
      if (work.isEmpty) List(lineOf("  Nothing running.", Palette.textDim))
      else
        work.map(entry =>
          lineOf(
            f"  ▸ ${entry.kind}%-10s ${entry.label}%-40s ${entry.cpu}%d slot(s), running ${humanDuration(entry.ageMs)}%s",
            Palette.accent
          )
        )

    // What the forks actually cost, as opposed to what the governor set aside for them. Every process under the daemon, not just direct children: a fork can
    // spawn its own. `None` from a daemon too old to report processes, or a platform that cannot measure.
    val measuredForks = row.processes.map(_.filter(_.parentPid.isDefined)).map(_.flatMap(_.footprintMb)).filter(_.nonEmpty).map(_.sum)
    val measured = measuredForks match {
      case Some(total) => s", ${total} MB measured"
      case None        => ", actual use not measurable here"
    }

    val forkLines =
      if (forks.isEmpty) Nil
      else
        List(
          Line.empty(),
          sectionOf(s"FORKED JVMS — ${forks.size}, ${forks.map(_.memoryMb).sum} MB reserved$measured"),
          lineOf(
            "  Reserved is what the governor sets aside before a fork starts: its heap bound plus overhead, until a fork of that kind has run and its",
            Palette.textDim
          ),
          lineOf("  peak was measured. Measured is what they hold right now — each one is on the Processes tab, by pid.", Palette.textDim),
          lineOf(
            "  A running suite's slot is charged above, to the work. Forks with no work are between suites, kept warm rather than restarted.",
            Palette.textDim
          )
        ) ++ forks.map(entry => lineOf(f"  ▪ ${entry.label}%-46s ${entry.memoryMb}%5d MB reserved, alive ${humanDuration(entry.ageMs)}%s", Palette.textMuted))

    val queue =
      if (machine.waiting.isEmpty) List(lineOf("  Nothing waiting — the server has capacity to spare.", Palette.textDim))
      else
        machine.waiting.map(entry =>
          lineOf(
            f"  · ${entry.kind}%-10s ${entry.label}%-30s wants ${entry.cpu}%d slot(s) and ${entry.memoryMb}%d MB, waiting ${humanDuration(entry.ageMs)}%s",
            Palette.warning
          )
        )

    val clients = status.connections.map { connection =>
      val who = connection.clientName.getOrElse(if (connection.observer) "an observer, watching only" else "unidentified")
      val version = connection.clientVersion.map(v => s" $v").getOrElse("")
      val workspace = connection.workspace.map(w => s" — $w").getOrElse("")
      lineOf(s"  #${connection.connId} $who$version$workspace", if (connection.observer) Palette.textDim else Palette.textMuted)
    }

    val heading =
      if (work.isEmpty) "RUNNING NOW — nothing"
      else s"RUNNING NOW — ${work.size} operation(s), ${machine.activeCompiles} of them compiles"

    List(sectionOf(heading)) ++ running ++ forkLines ++
      List(Line.empty(), sectionOf(s"WAITING FOR CAPACITY — ${machine.waiting.size}")) ++ queue ++
      List(Line.empty(), sectionOf(s"CONNECTED CLIENTS — ${status.connections.size}")) ++ clients
  }

  private def configLines(status: DaemonStatus): List[Line] = {
    val booted = status.config
    List(
      sectionOf("SETTINGS THIS SERVER STARTED WITH"),
      fieldOf("Parallelism", s"${booted.parallelism} operations at once"),
      fieldOf("Cached builds", s"up to ${booted.maxCachedWorkspaces} workspaces kept warm"),
      fieldOf("Read timeout", s"${booted.bspReadTimeoutMillis / 60000} minutes before dropping a silent client"),
      fieldOf("Idle timeout", s"${booted.compileServerIdleTimeoutMillis / 60000} minutes with no client before shutting down"),
      fieldOf("Heap pressure", s"new compiles wait above ${(booted.heapPressureThreshold * 100).toInt}% heap"),
      fieldOf("Max memory", booted.compileServerMaxMemory.getOrElse("bleep's default")),
      fieldOf(
        "Test runner",
        booted.testRunnerHeap.map(m => s"$m per forked test JVM, unless the project states its own").getOrElse("bleep's default per forked test JVM")
      ),
      Line.empty(),
      lineOf("  These were read when the server started. `bleep server config show` compares them with the file on disk,", Palette.textDim),
      lineOf("  and `bleep server restart` applies anything that has changed since.", Palette.textDim)
    )
  }

  // ── panes that build their own widgets ──────────────────────────

  /** A pane of plain text as a single widget.
    *
    * One element per line makes the layout solver do work proportional to the number of lines, every frame — fine for a dozen rows, fatal for a few hundred.
    * Text whose length is driven by data goes through here instead.
    */
  private def textPane(lines: List[Line]): Element =
    box("", Borders.ALL, widget(Paragraph.of(Text.fromLines(lines.asJava))))

  /** The tail of the server's own log, scrollable, with a scrollbar. Sizing needs the pane's real height, which only the render context knows. */
  private def logPane(state: ServerTopState, dispatch: Msg => Unit): Element =
    component { ctx =>
      ctx.onScroll { event =>
        val delta = event.kind match {
          case jatatui.react.MouseEvent.Kind.SCROLL_UP   => 3
          case jatatui.react.MouseEvent.Kind.SCROLL_DOWN => -3
          case _                                         => 0
        }
        if (delta != 0) dispatch(Msg.ScrollLog(delta))
      }

      val height = ctx.area().map[Int](area => math.max(1, area.height - 2)).orElse(20)
      val total = state.logTail.length
      val end = math.max(0, total - state.logScrollFromBottom)
      val visible = state.logTail.slice(math.max(0, end - height), end)

      val body =
        if (state.logTail.isEmpty) Paragraph.of(Text.raw("  no log yet")).withStyle(style(Palette.textDim))
        else Paragraph.of(Text.fromLines(visible.map(logLine).asJava))

      val title = if (state.followingLog) " log — following new lines " else f" log — ${state.logScrollFromBottom}%d lines back "
      box(title, Borders.ALL, row(fill(1, widget(body)), length(1, widget(scrollbar(total, end, height)))))
    }

  /** Coloured by level, so a wall of log is skimmable rather than uniform. */
  private def logLine(line: String): Line = {
    val color =
      if (line.contains("[error]") || line.contains("ERROR")) Palette.error
      else if (line.contains("[warn ]") || line.contains("WARN")) Palette.warning
      else Palette.textMuted
    Line.from(Span.styled(line, style(color)))
  }

  /** A plain track with a proportional thumb. Drawn here rather than with the stateful scrollbar widget because the position is already in the state, which
    * keeps the pane a pure function of it.
    */
  private def scrollbar(total: Int, end: Int, height: Int): jatatui.core.widgets.Widget =
    if (total <= height) Paragraph.of(Text.fromLines(List.fill(height)(Line.from(Span.styled("│", style(Palette.border)))).asJava))
    else {
      val thumbSize = math.max(1, math.min(height, (height.toDouble / total * height).toInt))
      val maxStart = math.max(1, total - height)
      val position = math.max(0, end - height).toDouble / maxStart
      val thumbStart = math.max(0, math.min(height - thumbSize, math.round(position * (height - thumbSize)).toInt))
      val cells = (0 until height).map { index =>
        val inThumb = index >= thumbStart && index < thumbStart + thumbSize
        Line.from(Span.styled(if (inThumb) "┃" else "│", style(if (inThumb) Palette.info else Palette.border)))
      }
      Paragraph.of(Text.fromLines(cells.toList.asJava))
    }

  /** How the server was launched, scrollable in both directions.
    *
    * A classpath is a couple of hundred entries of long absolute paths, so it overflows the pane on both axes. Paragraph takes a scroll offset for each, which
    * keeps this one widget — windowing it by hand would mean slicing every line to the visible columns on every frame.
    */
  private def startupPane(state: ServerTopState, identity: Option[bleep.bsp.ServerJson], dispatch: Msg => Unit): Element =
    component { ctx =>
      ctx.onScroll { event =>
        event.kind match {
          case jatatui.react.MouseEvent.Kind.SCROLL_UP   => dispatch(Msg.ScrollStartup(-3, 0))
          case jatatui.react.MouseEvent.Kind.SCROLL_DOWN => dispatch(Msg.ScrollStartup(3, 0))
          // The react layer has no horizontal scroll kind, so sideways wheel events are handled in the loop straight off the crossterm event instead.
          case _ => ()
        }
      }

      val lines = startupLines(identity)
      val height = ctx.area().map[Int](area => math.max(1, area.height - 2)).orElse(20)
      val width = ctx.area().map[Int](area => math.max(1, area.width - 2)).orElse(80)

      // Clamped here rather than in the state, which knows neither how many lines there are nor how big the pane is.
      val scrollY = math.min(state.startupScrollY, math.max(0, lines.length - height))
      val widest = lines.map(_.width()).maxOption.getOrElse(0)
      val maxScrollX = math.max(0, widest - width)
      if (maxScrollX != state.startupMaxScrollX) dispatch(Msg.StartupBounds(maxScrollX))
      val scrollX = math.min(state.startupScrollX, maxScrollX)

      val position = s" — line ${scrollY + 1} of ${lines.length}" + (if (widest > width) s", column ${scrollX + 1}" else "")

      box(
        s" how this server was started$position ",
        Borders.ALL,
        widget(Paragraph.of(Text.fromLines(lines.asJava)).withScroll(new jatatui.widgets.paragraph.Scroll(scrollY, scrollX)))
      )
    }

  private def startupLines(identity: Option[bleep.bsp.ServerJson]): List[Line] =
    identity match {
      case None =>
        List(
          sectionOf("HOW THIS SERVER WAS STARTED"),
          lineOf("  Unknown — this server was started by a bleep too old to record it.", Palette.textDim),
          lineOf("  Restart it and this tab will show its java binary, options and classpath.", Palette.textDim)
        )
      case Some(json) =>
        val classpath = classpathOf(json.command)
        List(
          sectionOf("HOW THIS SERVER WAS STARTED"),
          fieldOf("Java binary", json.javaBin),
          fieldOf("JVM", s"${json.jvmName} ${json.jvmVersion}"),
          fieldOf("Main class", json.serverMainClass),
          fieldOf("Working dir", json.workingDir),
          Line.empty(),
          sectionOf(s"JVM OPTIONS — ${json.javaOpts.size}")
        ) ++
          (if (json.javaOpts.isEmpty) List(lineOf("  none", Palette.textDim)) else json.javaOpts.map(option => lineOf(s"  $option", Palette.text))) ++
          List(
            Line.empty(),
            sectionOf(s"CLASSPATH — ${classpath.size} entries"),
            lineOf("  ← → scroll sideways; these are long absolute paths", Palette.textDim)
          ) ++ classpath.zipWithIndex.map { case (entry, index) => lineOf(f"  ${index + 1}%3d  $entry", Palette.textMuted) }
    }

  /** The classpath as the daemon was given it — the argument after `-cp` in the recorded argv. */
  private def classpathOf(command: List[String]): List[String] =
    command.sliding(2).collectFirst { case List("-cp", classpath) => classpath.split(java.io.File.pathSeparator).toList }.getOrElse(Nil)

  private def lineOf(content: String, color: jatatui.core.style.Color): Line = Line.from(Span.styled(content, style(color)))
  private def boldLineOf(content: String, color: jatatui.core.style.Color): Line = Line.from(Span.styled(content, bold(color)))
  private def sectionOf(title: String): Line = boldLineOf(s" $title", Palette.info)

  private def fieldOf(label: String, value: String): Line =
    Line.from(Span.styled(s"  ${label.padTo(LabelWidth + 4, ' ')}", style(Palette.textDim)), Span.styled(value, style(Palette.text)))

  // ── building blocks ─────────────────────────────────────────────

  /** Green until it matters, amber while it fills, red when it is the reason something is slow. */
  private def colorFor(value: Double): jatatui.core.style.Color =
    if (value >= 0.9) Palette.error else if (value >= 0.7) Palette.warning else Palette.success

  private def ratio(used: Long, total: Long): Double =
    if (total <= 0) 0.0 else math.max(0.0, math.min(1.0, used.toDouble / total.toDouble))

  /** Lines stack from the top, one row each. Without this the box shares its height out among the children and a handful of lines end up spread down the pane
    * with gaps between them.
    */
  private def packed(title: String, lines: List[Element]): Element =
    box(title, Borders.ALL, lines.map(line => length(1, line))*).`with`(props => props.withFlex(Flex.Start))

  private def footer(state: ServerTopState, dispatch: Msg => Unit): Element =
    state.pending match {
      case Some(confirm) =>
        // The answer is clickable too, so a confirmation never strands someone who reached for the mouse.
        row(
          length(confirm.prompt.length + 2, text(s" ${confirm.prompt}", bold(Palette.error))),
          length(7, clickable(Msg.Key(KeyPress.Yes), dispatch, text(" [yes]", bold(Palette.error)))),
          fill(1, clickable(Msg.Key(KeyPress.No), dispatch, text(" [no]", bold(Palette.textMuted))))
        )
      case None =>
        state.message match {
          case Some(message) => text(s" $message", style(Palette.info))
          case None          => buttons(state, dispatch)
        }
    }

  /** The actions, as a row of buttons rather than a legend. They are the things you came to do, so they look pressable and are. */
  private def buttons(state: ServerTopState, dispatch: Msg => Unit): Element = {
    val (actions, hint) = state.screen match {
      case Screen.Main =>
        (
          List(
            ("k", "kill", KeyPress.Kill, Palette.error),
            ("r", "restart", KeyPress.Restart, Palette.warning),
            ("d", "stopped servers", KeyPress.ShowDead, Palette.textMuted),
            ("⇥", "tab", KeyPress.NextTab, Palette.info),
            ("q", "quit", KeyPress.Quit, Palette.textMuted)
          ),
          "   ←→ tabs   ↑↓ select"
        )
      case Screen.Dead =>
        (
          List(
            ("c", "clear all", KeyPress.PruneDead, Palette.error),
            ("esc", "back", KeyPress.Back, Palette.info),
            ("q", "quit", KeyPress.Quit, Palette.textMuted)
          ),
          "   ↑↓ scroll"
        )
    }

    val cells = actions.map { case (key, label, press, color) =>
      val width = key.length + label.length + 6
      length(width, clickable(Msg.Key(press), dispatch, text(s" [ $key $label ] ", Palette.boldOnSurface(color))))
    }

    row((text(" ", style(Palette.textDim)) :: cells.map(identity) ::: List(fill(1, text(hint, style(Palette.textDim)))))*)
  }

  private def pct(value: Double): String = if (value < 0) "n/a" else f"${value * 100}%.0f%%"

  private def humanDuration(ms: Long): String = {
    val d = Duration.ofMillis(math.max(0L, ms))
    if (d.toDays > 0) s"${d.toDays}d${d.toHoursPart}h"
    else if (d.toHours > 0) s"${d.toHours}h${d.toMinutesPart}m"
    else if (d.toMinutes > 0) s"${d.toMinutes}m${d.toSecondsPart}s"
    else s"${d.toSeconds}s"
  }
}
