package bleep
package commands
package server
package tui

import bleep.bsp.ServerState
import bleep.testing.FancyBuildDisplay.Palette
import jatatui.core.layout.Flex
import jatatui.core.style.Style
import jatatui.react.Components._
import jatatui.react.Element
import jatatui.widgets.Borders
import jatatui.widgets.block.Block

import java.time.Duration

/** The dashboard, as a pure function of state.
  *
  * Nothing here touches a terminal, a clock or a socket — given a [[ServerTopState]] it returns an `Element`, which a test can render into an off-screen buffer
  * and compare as text. That is the whole reason `update` and the polling live elsewhere.
  */
object ServerTopView {
  import ServerTopState._

  // The same theme the build display and the picker use, so the three look like one program. Crucially every cell carries the background: this palette is built
  // for a dark one, and on a terminal supplying its own light background the text is close to invisible.
  private def style(color: jatatui.core.style.Color): Style = Palette.style(color)
  private def bold(color: jatatui.core.style.Color): Style = Palette.bold(color)

  def render(state: ServerTopState): Element =
    // Paint the background first and let everything else land on top. Widgets that set no background of their own leave these cells alone, so this reaches the
    // borders and the empty space below the last line too.
    stack(
      widget(Block.empty().withStyle(Palette.background)),
      column(
        length(serverListHeight(state), serverList(state)),
        fill(1, detail(state)),
        length(1, footer(state))
      )
    )

  /** Lines stack from the top, one row each. Without this the box shares its height out among the children and a handful of lines end up spread down the pane
    * with gaps between them.
    */
  private def packed(title: String, lines: List[Element]): Element =
    box(title, Borders.ALL, lines.map(line => length(1, line))*).`with`(props => props.withFlex(Flex.Start))

  private def serverListHeight(state: ServerTopState): Int =
    math.max(3, math.min(state.rows.length, 8) + 2)

  private def serverList(state: ServerTopState): Element = {
    val rows: List[Element] =
      if (state.rows.isEmpty) List(text("no compile servers — one starts on the next build", style(Palette.textDim)))
      else
        state.rows.zipWithIndex.map { case (row, index) =>
          val selected = index == state.selected
          val marker = row.info.state match {
            case ServerState.Running => "●"
            case ServerState.Wedged  => "!"
            case _                   => "◇"
          }
          val color = row.info.state match {
            case ServerState.Running => Palette.success
            case ServerState.Wedged  => Palette.error
            case _                   => Palette.textDim
          }
          val heap = row.status.map(s => s"${s.jvm.heapUsedMb}/${s.jvm.heapMaxMb}MB").getOrElse("—")
          val uptime = row.status.map(s => humanDuration(state.nowMs - s.startedAtEpochMs)).getOrElse("—")
          val clients = row.status.map(_.connections.size.toString).getOrElse("—")
          val mine = if (row.isCurrent) " ← this build" else ""
          val cursor = if (selected) "▸" else " "

          text(
            f"$cursor $marker ${row.hash}%-17s ${row.info.state.label}%-8s heap $heap%-14s up $uptime%-8s $clients clients$mine",
            if (selected) bold(color) else style(color)
          )
        }

    packed(" compile servers ", rows)
  }

  private def detail(state: ServerTopState): Element =
    state.selectedRow match {
      case None      => packed(" detail ", List(text("nothing to show", style(Palette.textDim))))
      case Some(row) =>
        val title = s" ${state.tab.title} — ${row.hash} "
        row.status match {
          case None =>
            // A row we could not ask says why, rather than rendering an empty pane that looks like "nothing is happening".
            packed(title, List(text(row.error.map(_.message).getOrElse(s"${row.info.state.label} — nothing to report"), style(Palette.warning))))
          case Some(status) =>
            val lines = state.tab match {
              case Tab.Overview   => overview(status)
              case Tab.Workspaces => workspaces(status)
              case Tab.Activity   => activity(status)
              case Tab.Config     => config(status)
            }
            packed(title, lines)
        }
    }

  private def overview(status: bleep.bsp.protocol.DaemonStatus): List[Element] = {
    val live = if (status.jvm.heapLiveMb < 0) "n/a" else s"${status.jvm.heapLiveMb}MB"
    List(
      text(f"heap     ${status.jvm.heapUsedMb}%d/${status.jvm.heapMaxMb}%dMB   live $live   committed ${status.jvm.heapCommittedMb}MB", style(Palette.text)),
      text(f"threads  ${status.jvm.threads}%d (peak ${status.jvm.peakThreads}%d)   classes ${status.jvm.loadedClasses}%d", style(Palette.text)),
      text(
        f"cpu      process ${pct(status.jvm.cpuProcess)}%s   system ${pct(status.jvm.cpuSystem)}%s   fds ${status.jvm.openFileDescriptors.map(_.toString).getOrElse("n/a")}%s",
        style(Palette.text)
      ),
      text(
        f"machine  cpu ${status.machine.usedCpu}%d/${status.machine.totalCpu}%d   fork mem ${status.machine.usedMemoryMb}%d/${status.machine.totalMemoryMb}%dMB",
        style(Palette.text)
      ),
      text(s"builds   ${status.buildCache.cachedWorkspaces.size}/${status.buildCache.bound} cached", style(Palette.text)),
      text(s"analysis ${status.analysisCache.entries} entries, ${status.analysisCache.fileBytes / (1024 * 1024)}MB", style(Palette.text))
    ) ++ status.jvm.gc.map(gc => text(s"gc       ${gc.name}: ${gc.count} collections, ${gc.timeMs}ms", style(Palette.textDim)))
  }

  private def workspaces(status: bleep.bsp.protocol.DaemonStatus): List[Element] =
    if (status.workspaces.isEmpty) List(text("no workspaces", style(Palette.textDim)))
    else
      status.workspaces.flatMap { workspace =>
        val cached = if (workspace.buildCached) "cached" else "not cached"
        text(s"${workspace.path} ($cached)", style(Palette.text)) ::
          workspace.activeOperations.map(op => text(s"  ${op.operation} ${op.projects.mkString(", ")} (${op.startedAgoMs / 1000}s)", style(Palette.accent)))
      }

  private def activity(status: bleep.bsp.protocol.DaemonStatus): List[Element] = {
    val active =
      status.machine.active.map(e => text(s"running  ${e.kind} ${e.label}  cpu ${e.cpu}  ${e.memoryMb}MB  ${e.ageMs / 1000}s", style(Palette.accent)))
    val waiting =
      status.machine.waiting.map(e => text(s"queued   ${e.kind} ${e.label}  cpu ${e.cpu}  ${e.memoryMb}MB  ${e.ageMs / 1000}s", style(Palette.warning)))
    val clients = status.connections.map { c =>
      val who = c.clientName.getOrElse(if (c.observer) "observer" else "unidentified")
      text(s"client   #${c.connId} $who${c.workspace.map(w => s" — $w").getOrElse("")}", style(Palette.textMuted))
    }
    val all = active ++ waiting ++ clients
    if (all.isEmpty) List(text("idle", style(Palette.textDim))) else all
  }

  private def config(status: bleep.bsp.protocol.DaemonStatus): List[Element] = {
    val c = status.config
    List(
      text(s"parallelism             ${c.parallelism}", style(Palette.text)),
      text(s"max-cached-workspaces   ${c.maxCachedWorkspaces}", style(Palette.text)),
      text(s"read-timeout            ${c.bspReadTimeoutMillis / 60000}m", style(Palette.text)),
      text(s"idle-timeout            ${c.compileServerIdleTimeoutMillis / 60000}m", style(Palette.text)),
      text(s"heap-pressure-threshold ${c.heapPressureThreshold}", style(Palette.text)),
      text(s"max-memory              ${c.compileServerMaxMemory.getOrElse("—")}", style(Palette.text)),
      text(s"test-runner-max-memory  ${c.testRunnerMaxMemory.getOrElse("—")}", style(Palette.text)),
      text("as booted — `bleep server config show` compares this with the file", style(Palette.textDim))
    )
  }

  private def footer(state: ServerTopState): Element =
    state.pending match {
      case Some(confirm) => text(confirm.prompt, bold(Palette.error))
      case None          =>
        state.message match {
          case Some(message) => text(message, style(Palette.info))
          case None          => text("q quit   ↑↓ select   ⇥ tab   k kill   r restart", style(Palette.textDim))
        }
    }

  private def pct(value: Double): String = if (value < 0) "n/a" else f"${value * 100}%.0f%%"

  private def humanDuration(ms: Long): String = {
    val d = Duration.ofMillis(math.max(0L, ms))
    if (d.toHours > 0) s"${d.toHours}h${d.toMinutesPart}m"
    else if (d.toMinutes > 0) s"${d.toMinutes}m${d.toSecondsPart}s"
    else s"${d.toSeconds}s"
  }
}
