package bleep
package commands
package server
package tui

import bleep.bsp.{AdminError, ServerDirInfo}
import bleep.bsp.protocol.DaemonStatus

/** One server as the dashboard knows it: what the directory scan found, plus whatever the daemon last said about itself. */
case class ServerRow(
    info: ServerDirInfo,
    status: Option[DaemonStatus],
    error: Option[AdminError],
    isCurrent: Boolean
) {
  def hash: String = info.hash
  def heapUsedMb: Long = status.map(_.jvm.heapUsedMb).getOrElse(0L)
  def heapMaxMb: Long = status.map(_.jvm.heapMaxMb).getOrElse(0L)
}

/** What is on screen. Immutable, and deliberately free of anything that could not be constructed by a test.
  *
  * `now` is a field rather than a call to the clock, for the same reason [[bleep.bsp.ConnectionRegistry]] takes its clock as a parameter: every uptime and age
  * this renders is a subtraction against it, and a snapshot test that cannot fix "now" can only assert on things that do not move.
  */
case class ServerTopState(
    rows: List[ServerRow],
    selected: Int,
    tab: ServerTopState.Tab,
    pending: Option[ServerTopState.Confirm],
    message: Option[String],
    logTail: List[String],
    /** How far back from the newest line the log is scrolled. Counted from the bottom, not the top, because the interesting end of a log is the end: at 0 the
      * view follows new lines, and it stays followed however many arrive.
      */
    logScrollFromBottom: Int,
    startupScrollY: Int,
    startupScrollX: Int,
    nowMs: Long,
    quit: Boolean
) {
  def selectedRow: Option[ServerRow] = rows.lift(selected)

  /** True while the log view is pinned to the newest line, which is where it starts and where it returns when you scroll back down. */
  def followingLog: Boolean = logScrollFromBottom == 0
}

object ServerTopState {

  sealed trait Tab { def title: String }
  object Tab {
    case object Overview extends Tab { val title = "Overview" }
    case object Workspaces extends Tab { val title = "Workspaces" }
    case object Activity extends Tab { val title = "Activity" }
    case object Log extends Tab { val title = "Log" }
    case object Config extends Tab { val title = "Config" }

    /** How the server was launched: java binary, options, and the classpath it was handed. Its own tab because a classpath is hundreds of long lines — it needs
      * room and it needs scrolling in both directions, which no other pane does.
      */
    case object Startup extends Tab { val title = "Startup" }

    val all: List[Tab] = List(Overview, Workspaces, Activity, Log, Config, Startup)
  }

  /** A destructive action waiting for y/n. Killing a compile server can throw away a running build, so it is never one keystroke away. */
  case class Confirm(action: Action, hash: String) {
    def prompt: String = s"${action.verb} $hash? (y/n)"
  }

  sealed trait Action { def verb: String }
  object Action {
    case object Kill extends Action { val verb = "kill" }
    case object Restart extends Action { val verb = "restart" }
  }

  sealed trait Msg
  object Msg {
    case class Refreshed(rows: List[ServerRow], nowMs: Long) extends Msg

    /** The tail of the selected server's log, read from disk by the loop. Kept out of [[ServerRow]] because it is only ever wanted for one server at a time. */
    case class LogTail(lines: List[String]) extends Msg
    case class Key(key: KeyPress) extends Msg
    case class ActionFinished(message: String) extends Msg

    /** Pointed at directly, rather than arrived at with the arrow keys. Clicks are their own messages instead of synthesised keystrokes so that "select this
      * row" cannot be confused with "move down one", which behave differently when the list changes underneath them.
      */
    case class SelectRow(index: Int) extends Msg
    case class SelectTab(tab: Tab) extends Msg

    /** Positive scrolls back into history, negative returns towards the newest line. */
    case class ScrollLog(delta: Int) extends Msg

    /** Scrolls the startup pane, which is the only one wide enough to need an x axis. */
    case class ScrollStartup(dy: Int, dx: Int) extends Msg
  }

  /** The keys the dashboard reacts to, named rather than passed through as crossterm events, so `update` never touches the terminal library. */
  sealed trait KeyPress
  object KeyPress {
    case object Up extends KeyPress
    case object Down extends KeyPress
    case object NextTab extends KeyPress

    /** The arrows are deliberately not "previous/next tab": what they do depends on the pane. On Startup they scroll sideways, because a classpath is far wider
      * than any terminal; everywhere else they move between tabs.
      */
    case object Left extends KeyPress
    case object Right extends KeyPress
    case object Quit extends KeyPress
    case object Kill extends KeyPress
    case object Restart extends KeyPress
    case object Yes extends KeyPress
    case object No extends KeyPress
  }

  def initial(nowMs: Long): ServerTopState =
    ServerTopState(
      rows = Nil,
      selected = 0,
      tab = Tab.Overview,
      pending = None,
      message = None,
      logTail = Nil,
      logScrollFromBottom = 0,
      startupScrollY = 0,
      startupScrollX = 0,
      nowMs = nowMs,
      quit = false
    )

  /** A side effect the loop should perform. Returned rather than done, so `update` stays a pure function of state and message. */
  sealed trait Effect
  object Effect {
    case class Perform(action: Action, row: ServerRow) extends Effect
  }
}

object ServerTopUpdate {
  import ServerTopState._

  def update(state: ServerTopState, msg: Msg): (ServerTopState, List[Effect]) = msg match {
    case Msg.Refreshed(rows, nowMs) =>
      // Servers come and go while you watch. Keep the selection on the same server rather than on the same index, so a row disappearing above the cursor does
      // not silently move it onto a different daemon — which would matter a great deal the next time `k` is pressed.
      val previouslySelected = state.selectedRow.map(_.hash)
      val selected = previouslySelected.flatMap(hash => Option(rows.indexWhere(_.hash == hash)).filter(_ >= 0)).getOrElse(clamp(state.selected, rows.length))
      (state.copy(rows = rows, selected = clamp(selected, rows.length), nowMs = nowMs), Nil)

    case Msg.ActionFinished(message) =>
      (state.copy(message = Some(message), pending = None), Nil)

    case Msg.SelectRow(index) =>
      // A click while a confirmation is up dismisses it: the prompt names one server, and pointing at another plainly means "not that one". A different server
      // means a different log, so the view goes back to following.
      (
        state.copy(selected = clamp(index, state.rows.length), message = None, pending = None, logScrollFromBottom = 0, startupScrollY = 0, startupScrollX = 0),
        Nil
      )

    case Msg.SelectTab(tab) =>
      (state.copy(tab = tab), Nil)

    case Msg.LogTail(lines) =>
      // Scrolled-back readers keep their place as new lines arrive; followers stay pinned to the end. Without this, tailing a busy server would drag the view
      // out from under anyone trying to read it.
      (state.copy(logTail = lines, logScrollFromBottom = clamp(state.logScrollFromBottom, lines.length)), Nil)

    case Msg.ScrollStartup(dy, dx) =>
      // No upper bound here: the pane knows its own content and clamps when it renders. Clamping in the state would mean the state needing to know how many
      // classpath entries there are and how tall the pane is, which is exactly the knowledge it does not have.
      (state.copy(startupScrollY = math.max(0, state.startupScrollY + dy), startupScrollX = math.max(0, state.startupScrollX + dx)), Nil)

    case Msg.ScrollLog(delta) =>
      (state.copy(logScrollFromBottom = clamp(state.logScrollFromBottom + delta, state.logTail.length)), Nil)

    case Msg.Key(key) =>
      state.pending match {
        case Some(confirm) =>
          key match {
            case KeyPress.Yes =>
              state.rows.find(_.hash == confirm.hash) match {
                case Some(row) =>
                  (state.copy(pending = None, message = Some(s"${confirm.action.verb}ing ${row.hash}…")), List(Effect.Perform(confirm.action, row)))
                case None => (state.copy(pending = None, message = Some(s"${confirm.hash} is gone")), Nil)
              }
            case KeyPress.No | KeyPress.Quit => (state.copy(pending = None, message = None), Nil)
            case _                           => (state, Nil)
          }

        case None =>
          key match {
            case KeyPress.Quit => (state.copy(quit = true), Nil)
            // On the Log tab the arrows scroll the log, which is what they are for when a log is what you are looking at. Rows stay selectable by clicking.
            case KeyPress.Up if state.tab == Tab.Log     => (state.copy(logScrollFromBottom = clamp(state.logScrollFromBottom + 1, state.logTail.length)), Nil)
            case KeyPress.Down if state.tab == Tab.Log   => (state.copy(logScrollFromBottom = clamp(state.logScrollFromBottom - 1, state.logTail.length)), Nil)
            case KeyPress.Up if state.tab == Tab.Startup => (state.copy(startupScrollY = math.max(0, state.startupScrollY - 1)), Nil)
            case KeyPress.Down if state.tab == Tab.Startup  => (state.copy(startupScrollY = state.startupScrollY + 1), Nil)
            case KeyPress.Up                                => (state.copy(selected = clamp(state.selected - 1, state.rows.length), message = None), Nil)
            case KeyPress.Down                              => (state.copy(selected = clamp(state.selected + 1, state.rows.length), message = None), Nil)
            case KeyPress.NextTab                           => (state.copy(tab = shiftTab(state.tab, 1)), Nil)
            case KeyPress.Left if state.tab == Tab.Startup  => (state.copy(startupScrollX = math.max(0, state.startupScrollX - 8)), Nil)
            case KeyPress.Right if state.tab == Tab.Startup => (state.copy(startupScrollX = state.startupScrollX + 8), Nil)
            case KeyPress.Left                              => (state.copy(tab = shiftTab(state.tab, -1)), Nil)
            case KeyPress.Right                             => (state.copy(tab = shiftTab(state.tab, 1)), Nil)
            case KeyPress.Kill                              => (confirming(state, Action.Kill), Nil)
            case KeyPress.Restart                           => (confirming(state, Action.Restart), Nil)
            case KeyPress.Yes | KeyPress.No                 => (state, Nil)
          }
      }
  }

  /** Only servers that are actually running can be stopped, and saying so beats a confirmation prompt for something that will do nothing. */
  private def confirming(state: ServerTopState, action: Action): ServerTopState =
    state.selectedRow match {
      case None                             => state.copy(message = Some("no server selected"))
      case Some(row) if !row.info.isRunning => state.copy(message = Some(s"${row.hash} is already ${row.info.state.label}"))
      case Some(row)                        => state.copy(pending = Some(Confirm(action, row.hash)), message = None)
    }

  /** Wraps in both directions, so ← from the first tab lands on the last rather than doing nothing. */
  private def shiftTab(tab: Tab, by: Int): Tab = {
    val all = Tab.all
    all(((all.indexOf(tab) + by) % all.length + all.length) % all.length)
  }

  private def clamp(index: Int, size: Int): Int =
    if (size <= 0) 0 else math.max(0, math.min(index, size - 1))
}
