package bleep
package commands
package server
package tui

import bleep.bsp.ServerState
import bleep.bsp.protocol.DaemonStatus
import bleep.testing.FancyBuildDisplay.Palette
import jatatui.components.gauge.LineGaugeProps
import jatatui.components.{Components => Widgets}
import jatatui.core.layout.Flex
import jatatui.core.style.Style
import jatatui.react.Components._
import jatatui.react.Element
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
      widget(Block.empty().withStyle(Palette.background)),
      column(
        length(1, text("", style(Palette.textDim))),
        length(1, header(state)),
        length(1, text("", style(Palette.textDim))),
        length(serverListHeight(state), serverList(state, dispatch)),
        fill(1, detail(state, dispatch)),
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

  /** The machine-level answer, before any server is selected: how many, how busy, and how much disk the stopped ones are still holding. */
  private def header(state: ServerTopState): Element = {
    val running = state.rows.count(_.info.isRunning)
    val stopped = state.rows.length - running
    val compiling = state.rows.flatMap(_.status).map(_.machine.activeCompiles).sum
    val queued = state.rows.flatMap(_.status).map(_.machine.waiting.size).sum
    val litterMb = state.rows.filterNot(_.info.isRunning).map(_.info.sizeMb).sum

    val parts = List(Some(s"$running running"), Option.when(stopped > 0)(s"$stopped stopped"), Option.when(litterMb > 0)(s"${litterMb}MB on disk")).flatten
    val activity = if (compiling > 0) s"   ● $compiling compiling" + (if (queued > 0) s", $queued queued" else "") else ""

    row(
      length(23, text(" BLEEP COMPILE SERVERS", bold(Palette.info))),
      fill(1, text(parts.mkString(" · ") + activity, style(if (compiling > 0) Palette.accent else Palette.textMuted)))
    )
  }

  // ── server list ─────────────────────────────────────────────────

  private def serverListHeight(state: ServerTopState): Int =
    math.max(3, math.min(state.rows.length, 8) + 2)

  private def serverList(state: ServerTopState, dispatch: Msg => Unit): Element = {
    val rows: List[Element] =
      if (state.rows.isEmpty) List(text("  no compile servers — one starts on the next build", style(Palette.textDim)))
      else
        state.rows.zipWithIndex.map { case (row, index) =>
          clickable(Msg.SelectRow(index), dispatch, serverListRow(row, selected = index == state.selected, state.nowMs))
        }

    packed(" servers ", rows)
  }

  private def serverListRow(serverRow: ServerRow, selected: Boolean, nowMs: Long): Element = {
    val (marker, color) = serverRow.info.state match {
      case ServerState.Running => ("●", Palette.success)
      case ServerState.Wedged  => ("!", Palette.error)
      case _                   => ("◇", Palette.textDim)
    }

    // The selected row sits on the raised surface, the same way the build display lifts its summary panel. The cursor is then obvious from shape as well as
    // colour, which matters on terminals where these greys sit close together.
    def cell(content: String, cellColor: jatatui.core.style.Color, emphasised: Boolean): Element = {
      val cellStyle =
        if (selected && emphasised) Palette.boldOnSurface(cellColor)
        else if (selected) Palette.onSurface(cellColor)
        else if (emphasised) bold(cellColor)
        else style(cellColor)
      text(content, cellStyle)
    }

    val busy = serverRow.status.map(_.machine).map { machine =>
      if (machine.activeCompiles > 0) s"${machine.activeCompiles} compiling" + (if (machine.waiting.nonEmpty) s" +${machine.waiting.size}q" else "")
      else if (machine.waiting.nonEmpty) s"${machine.waiting.size} queued"
      else "idle"
    }

    val heap = serverRow.status.map(status => f"${status.jvm.heapUsedMb}%d/${status.jvm.heapMaxMb}%d MB").getOrElse("")
    val counts = serverRow.status.map(status => s"${status.workspaces.size} ws  ${status.connections.size} cl").getOrElse("")
    val uptime = serverRow.status.map(status => humanDuration(nowMs - status.startedAtEpochMs)).getOrElse("")

    // A stopped server has nothing to report but is still holding disk, which is the reason to care about it at all.
    val stoppedNote = if (serverRow.info.isRunning) "unreachable" else s"${serverRow.info.sizeMb} MB on disk"

    val cells =
      if (serverRow.status.isEmpty)
        List(
          2 -> cell(if (selected) " ▸" else "  ", Palette.accent, emphasised = true),
          2 -> cell(s"$marker ", color, emphasised = false),
          18 -> cell(serverRow.hash, Palette.textDim, emphasised = selected),
          9 -> cell(serverRow.info.state.label, color, emphasised = false),
          40 -> cell(stoppedNote, Palette.textDim, emphasised = false),
          13 -> cell("", Palette.accent, emphasised = false)
        )
      else
        List(
          2 -> cell(if (selected) " ▸" else "  ", Palette.accent, emphasised = true),
          2 -> cell(s"$marker ", color, emphasised = false),
          18 -> cell(serverRow.hash, Palette.text, emphasised = selected),
          9 -> cell(serverRow.info.state.label, color, emphasised = false),
          14 -> cell(heap, Palette.textMuted, emphasised = false),
          12 -> cell(counts, Palette.textMuted, emphasised = false),
          9 -> cell(uptime, Palette.textMuted, emphasised = false),
          16 -> cell(busy.getOrElse(""), if (busy.contains("idle")) Palette.textDim else Palette.accent, emphasised = !busy.contains("idle")),
          13 -> cell(if (serverRow.isCurrent) "← this build" else "", Palette.accent, emphasised = true)
        )

    row(cells.map { case (width, element) => length(width, element) }*)
  }

  // ── detail ──────────────────────────────────────────────────────

  private def detail(state: ServerTopState, dispatch: Msg => Unit): Element =
    state.selectedRow match {
      case None      => packed(" detail ", List(text("nothing to show", style(Palette.textDim))))
      case Some(row) =>
        row.status match {
          case None =>
            // A row we could not ask says why, rather than rendering an empty pane that looks like "nothing is happening".
            packed(
              s" ${row.hash} ",
              List(text(row.error.map(_.message).getOrElse(s"${row.info.state.label} — nothing to report"), style(Palette.warning)))
            )
          case Some(status) =>
            val lines = state.tab match {
              case Tab.Overview   => overview(status)
              case Tab.Workspaces => workspaces(status)
              case Tab.Activity   => activity(status)
              case Tab.Log        => Nil // rendered by its own pane, which needs to know its height
              case Tab.Config     => config(status, row.info.identity)
            }
            column(
              length(1, tabBar(state, dispatch)),
              fill(1, if (state.tab == Tab.Log) logPane(state, dispatch) else packed("", lines))
            )
        }
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

  private def overview(status: DaemonStatus): List[Element] = {
    val jvm = status.jvm
    val live = if (jvm.heapLiveMb < 0) "n/a" else s"${jvm.heapLiveMb}MB"

    val gcSummary = jvm.gc.filter(_.count > 0) match {
      case Nil  => "none yet"
      case some => some.map(gc => s"${gc.name.replace("ZGC ", "")} ${gc.count}×/${gc.timeMs}ms").mkString("   ")
    }

    List(
      section("MEMORY"),
      gaugeRow("heap", ratio(jvm.heapUsedMb, jvm.heapMaxMb), s"${jvm.heapUsedMb} / ${jvm.heapMaxMb} MB"),
      field("live set", s"$live   committed ${jvm.heapCommittedMb}MB   non-heap ${jvm.nonHeapUsedMb}MB"),
      field("gc", gcSummary),
      section("MACHINE"),
      gaugeRow("cpu", ratio(status.machine.usedCpu.toLong, status.machine.totalCpu.toLong), s"${status.machine.usedCpu} / ${status.machine.totalCpu}"),
      gaugeRow(
        "fork memory",
        ratio(status.machine.usedMemoryMb, status.machine.totalMemoryMb),
        s"${status.machine.usedMemoryMb} / ${status.machine.totalMemoryMb} MB"
      ),
      field("threads", s"${jvm.threads} (peak ${jvm.peakThreads}, ${jvm.daemonThreads} daemon)"),
      field("load", s"process ${pct(jvm.cpuProcess)}   system ${pct(jvm.cpuSystem)}   fds ${jvm.openFileDescriptors.map(_.toString).getOrElse("n/a")}"),
      section("CACHES"),
      field("builds", s"${status.buildCache.cachedWorkspaces.size} of ${status.buildCache.bound} cached"),
      field(
        "analysis",
        s"${status.analysisCache.entries} entries, ${status.analysisCache.fileBytes / (1024 * 1024)}MB, ${status.analysisCache.sharedAnalyses} shared"
      )
    )
  }

  /** Which workspaces this server is holding, and what each is doing — the answer to "whose build is this". */
  private def workspaces(status: DaemonStatus): List[Element] =
    if (status.workspaces.isEmpty) List(text("  no workspaces loaded", style(Palette.textDim)))
    else
      section(s"${status.workspaces.size} LOADED") ::
        status.workspaces.flatMap { workspace =>
          val cached = if (workspace.buildCached) "build cached" else "build not cached"
          val busy = if (workspace.activeOperations.isEmpty) "idle" else s"${workspace.activeOperations.size} active"
          List(
            text(s"  ${workspace.path}", bold(Palette.text)),
            text(s"      $cached · $busy", style(Palette.textDim))
          ) ++ workspace.activeOperations.map { op =>
            text(s"      ▸ ${op.operation}  ${op.projects.mkString(", ")}  ${humanDuration(op.startedAgoMs)}", style(Palette.accent))
          }
        }

  /** What the server is doing right now, and what is stacked up behind it. */
  private def activity(status: DaemonStatus): List[Element] = {
    val machine = status.machine

    val running =
      if (machine.active.isEmpty) List(text("  nothing running", style(Palette.textDim)))
      else
        machine.active.map(entry =>
          text(
            f"  ▸ ${entry.kind}%-10s ${entry.label}%-30s cpu ${entry.cpu}%d   ${entry.memoryMb}%dMB   ${humanDuration(entry.ageMs)}%s",
            style(Palette.accent)
          )
        )

    val queue =
      if (machine.waiting.isEmpty) List(text("  queue empty", style(Palette.textDim)))
      else
        machine.waiting.map(entry =>
          text(
            f"  · ${entry.kind}%-10s ${entry.label}%-30s wants cpu ${entry.cpu}%d, ${entry.memoryMb}%dMB   waited ${humanDuration(entry.ageMs)}%s",
            style(Palette.warning)
          )
        )

    val clients = status.connections.map { connection =>
      val who = connection.clientName.getOrElse(if (connection.observer) "observer" else "unidentified")
      val version = connection.clientVersion.map(v => s" $v").getOrElse("")
      val workspace = connection.workspace.map(w => s" — $w").getOrElse("")
      text(s"  #${connection.connId} $who$version$workspace", style(if (connection.observer) Palette.textDim else Palette.textMuted))
    }

    List(section(s"RUNNING · ${machine.activeCompiles} compiling")) ++ running ++
      List(section(s"QUEUE · ${machine.waiting.size} waiting")) ++ queue ++
      List(section(s"CLIENTS · ${status.connections.size} connected")) ++ clients
  }

  /** The tail of the server's own log, scrollable, with a scrollbar.
    *
    * Rendered as one paragraph rather than one element per line. The layout solver runs over every child of a container, so a few hundred children cost a
    * constraint solve per frame at ten frames a second — which is what made this pane stutter and then stop responding. One widget, one solve.
    *
    * Sizing needs the pane's real height, which only the render context knows, so this is a component rather than a plain element.
    */
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

      val title = if (state.followingLog) " log — following " else f" log — ${state.logScrollFromBottom}%d lines back "
      box(
        title,
        Borders.ALL,
        row(
          fill(1, widget(body)),
          length(1, widget(scrollbar(total, end, height)))
        )
      )
    }

  /** Coloured by level, so a wall of log is skimmable rather than uniform. */
  private def logLine(line: String): Line = {
    val color =
      if (line.contains("[error]") || line.contains("ERROR")) Palette.error
      else if (line.contains("[warn ]") || line.contains("WARN")) Palette.warning
      else Palette.textMuted
    Line.from(Span.styled(line, style(color)))
  }

  /** A plain track with a proportional thumb. Drawn here rather than with the stateful scrollbar widget because the position is already in the state, and this
    * keeps the pane a pure function of it.
    */
  private def scrollbar(total: Int, end: Int, height: Int): jatatui.core.widgets.Widget =
    if (total <= height) Paragraph.of(Text.fromLines(List.fill(height)(Line.from(Span.styled("│", style(Palette.border)))).asJava))
    else {
      val thumbSize = math.max(1, (height.toDouble / total * height).toInt)
      val maxStart = math.max(1, total - height)
      val position = math.max(0, end - height).toDouble / maxStart
      val thumbStart = math.min(height - thumbSize, math.round(position * (height - thumbSize)).toInt)
      val cells = (0 until height).map { index =>
        val inThumb = index >= thumbStart && index < thumbStart + thumbSize
        Line.from(Span.styled(if (inThumb) "┃" else "│", style(if (inThumb) Palette.info else Palette.border)))
      }
      Paragraph.of(Text.fromLines(cells.toList.asJava))
    }

  private def config(status: DaemonStatus, identity: Option[bleep.bsp.ServerJson]): List[Element] = {
    val booted = status.config
    List(
      section("AS BOOTED"),
      field("parallelism", booted.parallelism.toString),
      field("max cached", s"${booted.maxCachedWorkspaces} workspaces"),
      field("read timeout", s"${booted.bspReadTimeoutMillis / 60000}m"),
      field("idle timeout", s"${booted.compileServerIdleTimeoutMillis / 60000}m"),
      field("heap pressure", booted.heapPressureThreshold.toString),
      field("max memory", booted.compileServerMaxMemory.getOrElse("default")),
      field("test runner", booted.testRunnerMaxMemory.getOrElse("default")),
      text("  `bleep server config show` compares these with the file on disk", style(Palette.textDim))
    ) ++ startup(identity)
  }

  /** How this server was actually launched, from the `server.json` written at spawn: the JVM, its options, and the classpath it was given.
    *
    * The classpath is the answer to "why is this server behaving like a different version of bleep", which is otherwise only recoverable by reading the daemon
    * log. It is long, so it is summarised and then listed one entry per line.
    */
  private def startup(identity: Option[bleep.bsp.ServerJson]): List[Element] =
    identity match {
      case None       => List(section("STARTED WITH"), text("  unknown — this server predates the recorded launch command", style(Palette.textDim)))
      case Some(json) =>
        val classpath = classpathOf(json.command)
        List(
          section("STARTED WITH"),
          field("java", json.javaBin),
          field("jvm", s"${json.jvmName}:${json.jvmVersion}"),
          field("main class", json.serverMainClass),
          field("working dir", json.workingDir),
          field("java options", if (json.javaOpts.isEmpty) "none" else json.javaOpts.mkString(" ")),
          field("classpath", s"${classpath.size} entries")
        ) ++ classpath.map(entry => text(s"      $entry", style(Palette.textDim)))
    }

  /** The classpath as the daemon was given it — the argument after `-cp` in the recorded argv. */
  private def classpathOf(command: List[String]): List[String] =
    command.sliding(2).collectFirst { case List("-cp", classpath) => classpath.split(java.io.File.pathSeparator).toList }.getOrElse(Nil)

  // ── building blocks ─────────────────────────────────────────────

  private def section(title: String): Element = text(s" $title", bold(Palette.info))

  private def field(label: String, value: String): Element =
    row(
      length(LabelWidth + 2, text(s"  $label", style(Palette.textDim))),
      fill(1, text(value, style(Palette.text)))
    )

  /** A bar for the numbers that answer "is this server busy, or fat". Those read at a glance where two numbers and a slash have to be parsed. */
  private def gaugeRow(label: String, value: Double, caption: String): Element =
    row(
      length(LabelWidth + 2, text(s"  $label", style(Palette.textDim))),
      // No title: the titled variant wraps the bar in a block, which at one row tall renders as an empty box rather than a gauge.
      length(20, Widgets.lineGauge(LineGaugeProps.of(value).withFilledStyle(style(colorFor(value))).withUnfilledStyle(style(Palette.border)))),
      fill(1, text(s"  $caption", style(Palette.text)))
    )

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
          case None          => buttons(dispatch)
        }
    }

  /** The actions, as a row of buttons rather than a legend. They are the things you came to do, so they look pressable and are. */
  private def buttons(dispatch: Msg => Unit): Element = {
    val actions = List(
      ("k", "kill", KeyPress.Kill, Palette.error),
      ("r", "restart", KeyPress.Restart, Palette.warning),
      ("⇥", "tab", KeyPress.NextTab, Palette.info),
      ("q", "quit", KeyPress.Quit, Palette.textMuted)
    )

    val cells = actions.map { case (key, label, press, color) =>
      val width = key.length + label.length + 6
      length(width, clickable(Msg.Key(press), dispatch, text(s" [ $key $label ] ", Palette.boldOnSurface(color))))
    }

    row((text(" ", style(Palette.textDim)) :: cells.map(identity) ::: List(fill(1, text("   ←→ tabs   ↑↓ select", style(Palette.textDim)))))*)
  }

  private def pct(value: Double): String = if (value < 0) "n/a" else f"${value * 100}%.0f%%"

  private def humanDuration(ms: Long): String = {
    val d = Duration.ofMillis(math.max(0L, ms))
    if (d.toHours > 0) s"${d.toHours}h${d.toMinutesPart}m"
    else if (d.toMinutes > 0) s"${d.toMinutes}m${d.toSecondsPart}s"
    else s"${d.toSeconds}s"
  }
}
