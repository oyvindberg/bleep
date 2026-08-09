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
    math.max(4, math.min(state.rows.length, 8) + 3)

  private def serverList(state: ServerTopState, dispatch: Msg => Unit): Element = {
    val rows: List[Element] =
      if (state.rows.isEmpty) List(text("  no compile servers — one starts on the next build", style(Palette.textDim)))
      else
        state.rows.zipWithIndex.map { case (row, index) =>
          clickable(Msg.SelectRow(index), dispatch, serverListRow(row, selected = index == state.selected, state.nowMs))
        }

    packed(" servers ", heading :: rows)
  }

  /** Because the row now carries several unlabelled columns, and "which of these is the JVM" should not need working out. */
  private val heading: Element =
    row(
      List(
        4 -> text("", style(Palette.textDim)),
        10 -> text("server", style(Palette.textDim)),
        13 -> text("", style(Palette.textDim)),
        9 -> text("state", style(Palette.textDim)),
        22 -> text("bleep version", style(Palette.textDim)),
        26 -> text("jvm", style(Palette.textDim)),
        14 -> text("heap", style(Palette.textDim)),
        10 -> text("load", style(Palette.textDim)),
        9 -> text("uptime", style(Palette.textDim)),
        16 -> text("doing", style(Palette.textDim))
      ).map { case (width, element) => length(width, element) }*
    )

  /** Sentinel width meaning "take whatever is left of the line". */
  private val RestOfLine = -1

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

    // What actually makes this a separate server. The socket directory is a hash of bleep version + JVM + java options, so two rows differing in any of these
    // are two servers by design — showing the hash alone leaves "why are there four of these" unanswerable without digging.
    val version = serverRow.info.identity.map(_.bleepVersion).getOrElse("unknown version")
    val jvm = serverRow.info.identity.map(id => s"${id.jvmName} ${id.jvmVersion}").getOrElse("unknown jvm")

    // A stopped server has nothing to report but is still holding disk, which is the reason to care about it at all.
    val stoppedNote = if (serverRow.info.isRunning) "unreachable" else s"${serverRow.info.sizeMb} MB on disk"

    val cells =
      if (serverRow.status.isEmpty)
        List(
          2 -> cell(if (selected) " ▸" else "  ", Palette.accent, emphasised = true),
          2 -> cell(s"$marker ", color, emphasised = false),
          10 -> cell(serverRow.hash.take(8), Palette.textDim, emphasised = selected),
          13 -> cell("", Palette.accent, emphasised = false),
          9 -> cell(serverRow.info.state.label, color, emphasised = false),
          22 -> cell(version, Palette.textDim, emphasised = false),
          26 -> cell(jvm, Palette.textDim, emphasised = false),
          RestOfLine -> cell(stoppedNote, Palette.textDim, emphasised = false)
        )
      else
        List(
          2 -> cell(if (selected) " ▸" else "  ", Palette.accent, emphasised = true),
          2 -> cell(s"$marker ", color, emphasised = false),
          10 -> cell(serverRow.hash.take(8), Palette.text, emphasised = selected),
          // Kept to the left on purpose: it is the one column that must survive a narrow terminal, since it is what tells you which server is yours.
          13 -> cell(if (serverRow.isCurrent) "← this build" else "", Palette.accent, emphasised = true),
          9 -> cell(serverRow.info.state.label, color, emphasised = false),
          22 -> cell(version, Palette.info, emphasised = false),
          26 -> cell(jvm, Palette.textMuted, emphasised = false),
          14 -> cell(heap, Palette.textMuted, emphasised = false),
          10 -> cell(counts, Palette.textMuted, emphasised = false),
          9 -> cell(uptime, Palette.textMuted, emphasised = false),
          RestOfLine -> cell(busy.getOrElse(""), if (busy.contains("idle")) Palette.textDim else Palette.accent, emphasised = !busy.contains("idle"))
        )

    // `fill` for the last column so the row uses whatever width is left rather than overflowing a fixed budget.
    row(cells.map { case (width, element) => if (width == RestOfLine) fill(1, element) else length(width, element) }*)
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
            column(
              length(1, tabBar(state, dispatch)),
              fill(
                1,
                state.tab match {
                  case Tab.Log => logPane(state, dispatch)
                  // Overview is a short, fixed set of rows and needs real elements for its gauges. The others are plain text of unbounded length — a workspace
                  // list, a queue, a classpath — and one element per line means one layout constraint per line, solved every frame. 202 classpath entries
                  // froze the dashboard outright.
                  case Tab.Overview   => textPane(overviewLines(status))
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
    * Everything else on this tab is a number you have to interpret; this says whether anything is happening at all, which is the question most visits are
    * actually asking.
    */
  private def statusLine(status: DaemonStatus): Line = {
    val machine = status.machine
    val compiling = machine.active.map(_.label).distinct

    val (summary, color) =
      if (machine.active.nonEmpty) {
        val what = if (compiling.size <= 3) compiling.mkString(", ") else s"${compiling.take(3).mkString(", ")} and ${compiling.size - 3} more"
        val queued = if (machine.waiting.nonEmpty) s", ${machine.waiting.size} waiting for capacity" else ""
        (s"Building $what$queued", Palette.accent)
      } else if (machine.waiting.nonEmpty) (s"${machine.waiting.size} operation(s) waiting for capacity", Palette.warning)
      else if (status.connections.exists(!_.observer)) ("Idle, with a client connected", Palette.text)
      else ("Idle, nobody connected", Palette.textDim)

    boldLineOf(s"  $summary", color)
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
  private def activityLines(status: DaemonStatus): List[Line] = {
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

    val forkLines =
      if (forks.isEmpty) Nil
      else
        List(
          Line.empty(),
          sectionOf(s"FORKED JVMS — ${forks.size} holding ${forks.map(_.memoryMb).sum} MB between them"),
          lineOf("  A running suite's slot is charged above, to the work; these rows are the processes and the memory they hold.", Palette.textDim),
          lineOf("  Forks with no work above are between suites, kept warm rather than restarted.", Palette.textDim)
        ) ++ forks.map(entry => lineOf(f"  ▪ ${entry.label}%-46s ${entry.memoryMb}%5d MB, alive ${humanDuration(entry.ageMs)}%s", Palette.textMuted))

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
