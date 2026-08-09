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
    nowMs: Long,
    quit: Boolean
) {
  def selectedRow: Option[ServerRow] = rows.lift(selected)
}

object ServerTopState {

  sealed trait Tab { def title: String }
  object Tab {
    case object Overview extends Tab { val title = "Overview" }
    case object Workspaces extends Tab { val title = "Workspaces" }
    case object Activity extends Tab { val title = "Activity" }
    case object Config extends Tab { val title = "Config" }

    val all: List[Tab] = List(Overview, Workspaces, Activity, Config)
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
    case class Key(key: KeyPress) extends Msg
    case class ActionFinished(message: String) extends Msg
  }

  /** The keys the dashboard reacts to, named rather than passed through as crossterm events, so `update` never touches the terminal library. */
  sealed trait KeyPress
  object KeyPress {
    case object Up extends KeyPress
    case object Down extends KeyPress
    case object NextTab extends KeyPress
    case object Quit extends KeyPress
    case object Kill extends KeyPress
    case object Restart extends KeyPress
    case object Yes extends KeyPress
    case object No extends KeyPress
  }

  def initial(nowMs: Long): ServerTopState =
    ServerTopState(rows = Nil, selected = 0, tab = Tab.Overview, pending = None, message = None, nowMs = nowMs, quit = false)

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
            case KeyPress.Quit              => (state.copy(quit = true), Nil)
            case KeyPress.Up                => (state.copy(selected = clamp(state.selected - 1, state.rows.length), message = None), Nil)
            case KeyPress.Down              => (state.copy(selected = clamp(state.selected + 1, state.rows.length), message = None), Nil)
            case KeyPress.NextTab           => (state.copy(tab = nextTab(state.tab)), Nil)
            case KeyPress.Kill              => (confirming(state, Action.Kill), Nil)
            case KeyPress.Restart           => (confirming(state, Action.Restart), Nil)
            case KeyPress.Yes | KeyPress.No => (state, Nil)
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

  private def nextTab(tab: Tab): Tab = {
    val all = Tab.all
    all((all.indexOf(tab) + 1) % all.length)
  }

  private def clamp(index: Int, size: Int): Int =
    if (size <= 0) 0 else math.max(0, math.min(index, size - 1))
}
