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
            // Panes that need their own height, their own scrolling or their own widgets build themselves below; this is only the element-based Overview.
            val lines = if (state.tab == Tab.Overview) overview(status) else Nil
            column(
              length(1, tabBar(state, dispatch)),
              fill(
                1,
                state.tab match {
                  case Tab.Log => logPane(state, dispatch)
                  // Overview is a short, fixed set of rows and needs real elements for its gauges. The others are plain text of unbounded length — a workspace
                  // list, a queue, a classpath — and one element per line means one layout constraint per line, solved every frame. 202 classpath entries
                  // froze the dashboard outright.
                  case Tab.Overview   => packed("", lines)
                  case Tab.Config     => textPane(configLines(status))
                  case Tab.Workspaces => textPane(workspaceLines(status))
                  case Tab.Activity   => textPane(activityLines(status))
                  case Tab.Startup    => startupPane(state, row.info.identity, dispatch)
                }
              )
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

  /** Written out in words rather than abbreviations.
    *
    * The numbers here are only useful if you know what they mean: "live set" and "heap used" answer different questions, and "fork mem" answers one most people
    * do not know they have. Each row says what it is measuring, and the three that answer "is this server in trouble" get a full-width bar, because a bar
    * answers that at a glance where a pair of numbers has to be read and divided.
    */
  private def overview(status: DaemonStatus): List[Element] = {
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
      section("MEMORY — how much of its heap this server is using"),
      bigGauge("Heap in use", ratio(jvm.heapUsedMb, jvm.heapMaxMb), s"${jvm.heapUsedMb} MB of ${jvm.heapMaxMb} MB"),
      field("Retained", retained),
      field("Committed", s"${jvm.heapCommittedMb} MB reserved from the OS, ${jvm.nonHeapUsedMb} MB outside the heap"),
      field("Collections", gcSummary),
      blank,
      section("CAPACITY — what this server may spend on compiling"),
      bigGauge("Compile slots", ratio(machine.usedCpu.toLong, machine.totalCpu.toLong), s"${machine.usedCpu} of ${machine.totalCpu} in use"),
      bigGauge("Memory for forks", ratio(machine.usedMemoryMb, machine.totalMemoryMb), s"${machine.usedMemoryMb} MB of ${machine.totalMemoryMb} MB"),
      field("Threads", s"${jvm.threads} alive, peak ${jvm.peakThreads}, ${jvm.daemonThreads} of them background"),
      field("Processor", s"${pct(jvm.cpuProcess)} of the machine used by this server, ${pct(jvm.cpuSystem)} used in total"),
      field("Open files", jvm.openFileDescriptors.map(count => s"$count file descriptors").getOrElse("not reported on this platform")),
      blank,
      section("WHAT IT IS KEEPING WARM — so the next build does not pay for it again"),
      field("Builds", s"${status.buildCache.cachedWorkspaces.size} of ${status.buildCache.bound} workspaces cached"),
      field(
        "Compile analysis",
        s"${status.analysisCache.entries} entries, ${status.analysisCache.fileBytes / (1024 * 1024)} MB, " +
          s"${status.analysisCache.sharedAnalyses} shared between workspaces"
      )
    )
  }

  private val blank: Element = text("", style(Palette.textDim))

  /** A full-width bar with the label above the numbers beside it. Wider than the old inline gauge because these three are the ones worth seeing from across the
    * room, and a 20-column bar cannot show the difference between 70% and 80%.
    */
  private def bigGauge(label: String, value: Double, caption: String): Element =
    row(
      length(LabelWidth + 6, text(s"  $label", style(Palette.textDim))),
      length(3, text(f"${(value * 100).toInt}%2d%%", bold(colorFor(value)))),
      length(
        34,
        // Without an explicit empty label the widget prints its own percentage, which lands next to ours and disagrees about rounding.
        Widgets.lineGauge(LineGaugeProps.of(value).withLabel("").withFilledStyle(style(colorFor(value))).withUnfilledStyle(style(Palette.border)))
      ),
      fill(1, text(s"  $caption", style(Palette.text)))
    )

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

  /** What the server is doing right now, and what is stacked up behind it. */
  private def activityLines(status: DaemonStatus): List[Line] = {
    val machine = status.machine

    val running =
      if (machine.active.isEmpty) List(lineOf("  Nothing running.", Palette.textDim))
      else
        machine.active.map(entry =>
          lineOf(
            f"  ▸ ${entry.kind}%-10s ${entry.label}%-30s using ${entry.cpu}%d slot(s), ${entry.memoryMb}%d MB, for ${humanDuration(entry.ageMs)}%s",
            Palette.accent
          )
        )

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

    List(sectionOf(s"RUNNING NOW — ${machine.activeCompiles} compiling")) ++ running ++
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
      fieldOf("Test runner", booted.testRunnerMaxMemory.map(m => s"$m per forked test JVM").getOrElse("the JVM default")),
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
      val scrollX = math.min(state.startupScrollX, math.max(0, widest - width))

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

  private def section(title: String): Element = text(s" $title", bold(Palette.info))

  private def field(label: String, value: String): Element =
    row(
      length(LabelWidth + 2, text(s"  $label", style(Palette.textDim))),
      fill(1, text(value, style(Palette.text)))
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
