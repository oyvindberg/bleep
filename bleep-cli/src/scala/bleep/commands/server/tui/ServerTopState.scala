package bleep
package commands
package server
package tui

import bleep.bsp.{AdminError, ServerDirInfo, ServerState}
import bleep.bsp.protocol.DaemonStatus

/** One server as the dashboard knows it: what the directory scan found, what the daemon last said about itself, and what its processes cost, measured from
  * outside.
  *
  * @param processes
  *   the daemon and every process under it, sampled by the client. `None` when there is no process to sample — no pid recorded, or it exited between the scan
  *   and the sample.
  * @param parent
  *   the process that started the daemon, while it is still alive: the client keeping an old server in use.
  * @param isOutdated
  *   recorded with a different bleep version than this client, and not the server this build uses. Nothing new connects to it; only clients pinned to that
  *   version — an editor, an MCP server — keep it busy.
  */
case class ServerRow(
    info: ServerDirInfo,
    status: Option[DaemonStatus],
    error: Option[AdminError],
    isCurrent: Boolean,
    processes: Option[List[ProcessTree.Sample]],
    parent: Option[ProcessTree.Parent],
    isOutdated: Boolean
) {
  def hash: String = info.hash

  /** Still has a process behind it, so it is costing the machine something: running, or wedged — alive but not answering, which is worse. Everything else is a
    * directory on disk, and lives in the dead-servers view rather than crowding these out of the main one.
    */
  def isLive: Boolean = info.state == ServerState.Running || info.state == ServerState.Wedged

  /** The daemon's process id — what you would `kill`, and what Activity Monitor and `top` list it by. From the OS sample first, then the daemon's own account,
    * then the pid file, which only records what the spawning client believed.
    */
  def pid: Option[Long] = processes.flatMap(_.find(_.parentPid.isEmpty)).map(_.pid).orElse(status.map(_.pid)).orElse(info.pid)

  /** How the dashboard names a server: by pid when it has one, since that is what every other tool on the machine calls it. */
  def name: String = pid.map(p => s"pid $p").getOrElse(hash.take(8))

  /** When the daemon started: from the OS, which knows for every server, wedged ones included; else from the daemon's own account. */
  def startedAtEpochMs: Option[Long] =
    processes.flatMap(_.find(_.parentPid.isEmpty)).flatMap(_.startedAtEpochMs).orElse(status.map(_.startedAtEpochMs))

  /** The daemon process alone — its heap and everything else the JVM holds, without the JVMs it forked. */
  def selfFootprintMb: Option[Long] = processes.flatMap(_.find(_.parentPid.isEmpty)).flatMap(_.footprintMb)

  /** Everything the daemon has forked — test JVMs, sourcegen, KSP — and anything they forked in turn. */
  def forkedFootprintMb: Option[Long] = {
    val measured = processes.getOrElse(Nil).filter(_.parentPid.isDefined).flatMap(_.footprintMb)
    if (measured.isEmpty) None else Some(measured.sum)
  }

  /** The daemon and everything under it, summed. `None` when nothing in the tree could be measured, which is different from measuring zero. */
  def totalFootprintMb: Option[Long] = {
    val measured = processes.getOrElse(Nil).flatMap(_.footprintMb)
    if (measured.isEmpty) None else Some(measured.sum)
  }
}

/** What is on screen. Immutable, and deliberately free of anything that could not be constructed by a test.
  *
  * `now` is a field rather than a call to the clock, for the same reason [[bleep.bsp.ConnectionRegistry]] takes its clock as a parameter: every uptime and age
  * this renders is a subtraction against it, and a snapshot test that cannot fix "now" can only assert on things that do not move.
  */
case class ServerTopState(
    rows: List[ServerRow],
    /** Index into [[live]], not [[rows]]: the main view only ever shows live servers, so that is what the cursor moves over. */
    selected: Int,
    screen: ServerTopState.Screen,
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
    /** How far the startup pane can scroll sideways, as the pane last measured it. Only the pane knows its width and its widest line, so it reports this back
      * ([[ServerTopState.Msg.StartupBounds]]); with it the offset is clamped here, and the arrows know when they are at an edge and should change tab instead.
      */
    startupMaxScrollX: Int,
    deadScroll: Int,
    /** The previous cumulative CPU reading per pid and when it was taken. A rate needs two readings; this is the first of them. */
    cpuSamples: Map[Long, ServerTopState.CpuSample],
    /** Per pid, the share of one core used between the last two readings — 100 is one core flat out. Absent until a pid has been seen twice. */
    cpuPercent: Map[Long, Double],
    machine: ServerTopState.Machine,
    nowMs: Long,
    quit: Boolean
) {
  def live: List[ServerRow] = rows.filter(_.isLive)
  def dead: List[ServerRow] = rows.filterNot(_.isLive)
  def selectedRow: Option[ServerRow] = live.lift(selected)

  /** A process and everything under it, as a share of one core. `None` until at least one of them has a rate. */
  def treeCpuPercent(row: ServerRow): Option[Double] = {
    val rates = row.processes.getOrElse(Nil).flatMap(p => cpuPercent.get(p.pid))
    if (rates.isEmpty) None else Some(rates.sum)
  }

  /** True while the log view is pinned to the newest line, which is where it starts and where it returns when you scroll back down. */
  def followingLog: Boolean = logScrollFromBottom == 0
}

object ServerTopState {

  case class CpuSample(cpuTimeMs: Long, atMs: Long)

  /** The main view is the live servers as a tree; stopped ones get a screen of their own, because a hundred of them pushed the live ones off the bottom. */
  sealed trait Screen
  object Screen {
    case object Main extends Screen
    case object Dead extends Screen
  }

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

    /** The selected server's work and processes, with what each costs. First, because it is the detail behind the line you selected. */
    case object Processes extends Tab { val title = "Processes" }

    val all: List[Tab] = List(Processes, Overview, Workspaces, Activity, Log, Config, Startup)
  }

  /** A destructive action waiting for y/n. Killing a compile server can throw away a running build, and pruning deletes the logs of a crash you may still want
    * to read, so neither is ever one keystroke away.
    */
  sealed trait Confirm { def prompt: String }
  object Confirm {

    /** Keyed by hash, which is stable for the server's whole life; named by pid, which is what the reader recognises. */
    case class OnServer(action: Action, hash: String, name: String) extends Confirm {
      def prompt: String = s"${action.verb} $name ($hash)? (y/n)"
    }
    case class PruneDead(count: Int, sizeMb: Long) extends Confirm {
      def prompt: String = s"delete $count stopped server director${if (count == 1) "y" else "ies"}, ${sizeMb} MB of logs and metrics? (y/n)"
    }
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

    /** The startup pane's horizontal extent, reported by the pane whenever it changes: a different server, a resized terminal. */
    case class StartupBounds(maxScrollX: Int) extends Msg

    /** Scrolls the dead-servers list. */
    case class ScrollDead(delta: Int) extends Msg
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

    /** Esc: out of the dead-servers view, or out of the program from the main one. */
    case object Back extends KeyPress
    case object ShowDead extends KeyPress
    case object PruneDead extends KeyPress
    case object Kill extends KeyPress
    case object Restart extends KeyPress
    case object Yes extends KeyPress
    case object No extends KeyPress
  }

  /** What the machine has, for putting bleep's use in proportion. `physicalMemoryMb` is 0 where the platform would not say. */
  case class Machine(physicalMemoryMb: Long, cores: Int)

  def initial(nowMs: Long, machine: Machine): ServerTopState =
    ServerTopState(
      rows = Nil,
      selected = 0,
      screen = Screen.Main,
      tab = Tab.Processes,
      pending = None,
      message = None,
      logTail = Nil,
      logScrollFromBottom = 0,
      startupScrollY = 0,
      startupScrollX = 0,
      startupMaxScrollX = 0,
      deadScroll = 0,
      cpuSamples = Map.empty,
      cpuPercent = Map.empty,
      machine = machine,
      nowMs = nowMs,
      quit = false
    )

  /** A side effect the loop should perform. Returned rather than done, so `update` stays a pure function of state and message. */
  sealed trait Effect
  object Effect {
    case class Perform(action: Action, row: ServerRow) extends Effect
    case object PruneDead extends Effect
  }
}

object ServerTopUpdate {
  import ServerTopState._

  def update(state: ServerTopState, msg: Msg): (ServerTopState, List[Effect]) = msg match {
    case Msg.Refreshed(unordered, nowMs) =>
      // Newest first, by when the process started — the server you just caused is at the top, the ones that have outlived everything sink to the bottom.
      // Deterministic, so a line stays where it was from one second to the next (pid, then hash, break ties; servers with no start time go last). Ordering
      // here rather than in the view keeps the cursor and the screen in agreement — the arrows move through `live` in exactly the order it is drawn.
      val rows = unordered.sortBy(row => (row.startedAtEpochMs.map(-_).getOrElse(Long.MaxValue), -row.pid.getOrElse(0L), row.hash))
      // Servers come and go while you watch. Keep the selection on the same server rather than on the same index, so a row disappearing above the cursor does
      // not silently move it onto a different daemon — which would matter a great deal the next time `k` is pressed.
      val previouslySelected = state.selectedRow.map(_.hash)
      val live = rows.filter(_.isLive)
      val selected = previouslySelected.flatMap(hash => Option(live.indexWhere(_.hash == hash)).filter(_ >= 0)).getOrElse(clamp(state.selected, live.length))

      // Only pids still present are kept, so the maps do not grow with every fork that ever lived.
      val readings = rows.flatMap(_.processes.getOrElse(Nil)).collect { case p if p.cpuTimeMs.isDefined => p.pid -> CpuSample(p.cpuTimeMs.get, nowMs) }.toMap
      val rates = readings.flatMap { case (pid, now) =>
        state.cpuSamples.get(pid) match {
          case Some(before) if now.atMs > before.atMs && now.cpuTimeMs >= before.cpuTimeMs =>
            Some(pid -> (now.cpuTimeMs - before.cpuTimeMs).toDouble * 100.0 / (now.atMs - before.atMs).toDouble)
          // Same instant, or a reused pid whose clock went backwards: keep the last rate rather than invent one.
          case Some(_) => state.cpuPercent.get(pid).map(pid -> _)
          case None    => None
        }
      }

      (
        state.copy(
          rows = rows,
          selected = clamp(selected, live.length),
          deadScroll = clamp(state.deadScroll, rows.count(!_.isLive)),
          cpuSamples = readings,
          cpuPercent = rates,
          nowMs = nowMs
        ),
        Nil
      )

    case Msg.ActionFinished(message) =>
      (state.copy(message = Some(message), pending = None), Nil)

    case Msg.SelectRow(index) =>
      // A click while a confirmation is up dismisses it: the prompt names one server, and pointing at another plainly means "not that one". A different server
      // means a different log, so the view goes back to following.
      (
        state.copy(selected = clamp(index, state.live.length), message = None, pending = None, logScrollFromBottom = 0, startupScrollY = 0, startupScrollX = 0),
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
      (state.copy(startupScrollY = math.max(0, state.startupScrollY + dy), startupScrollX = clampX(state, state.startupScrollX + dx)), Nil)

    case Msg.StartupBounds(maxScrollX) =>
      (state.copy(startupMaxScrollX = maxScrollX, startupScrollX = math.min(state.startupScrollX, maxScrollX)), Nil)

    case Msg.ScrollDead(delta) =>
      (state.copy(deadScroll = clamp(state.deadScroll + delta, state.dead.length)), Nil)

    case Msg.ScrollLog(delta) =>
      (state.copy(logScrollFromBottom = clamp(state.logScrollFromBottom + delta, state.logTail.length)), Nil)

    case Msg.Key(key) =>
      state.pending match {
        case Some(confirm) =>
          key match {
            case KeyPress.Yes =>
              confirm match {
                case Confirm.OnServer(action, hash, _) =>
                  state.rows.find(_.hash == hash) match {
                    case Some(row) =>
                      (state.copy(pending = None, message = Some(s"${action.verb}ing ${row.name}…")), List(Effect.Perform(action, row)))
                    case None => (state.copy(pending = None, message = Some(s"$hash is gone")), Nil)
                  }
                case Confirm.PruneDead(_, _) =>
                  (state.copy(pending = None, message = Some("removing stopped servers…")), List(Effect.PruneDead))
              }
            case KeyPress.No | KeyPress.Quit | KeyPress.Back => (state.copy(pending = None, message = None), Nil)
            case _                                           => (state, Nil)
          }

        case None if state.screen == Screen.Dead =>
          key match {
            case KeyPress.Quit      => (state.copy(quit = true), Nil)
            case KeyPress.Back      => (state.copy(screen = Screen.Main, message = None), Nil)
            case KeyPress.ShowDead  => (state.copy(screen = Screen.Main, message = None), Nil)
            case KeyPress.Up        => (state.copy(deadScroll = clamp(state.deadScroll - 1, state.dead.length)), Nil)
            case KeyPress.Down      => (state.copy(deadScroll = clamp(state.deadScroll + 1, state.dead.length)), Nil)
            case KeyPress.PruneDead =>
              val dead = state.dead
              if (dead.isEmpty) (state.copy(message = Some("no stopped servers to remove")), Nil)
              else (state.copy(pending = Some(Confirm.PruneDead(dead.size, dead.map(_.info.sizeMb).sum)), message = None), Nil)
            case _ => (state, Nil)
          }

        case None =>
          key match {
            case KeyPress.Quit | KeyPress.Back => (state.copy(quit = true), Nil)
            case KeyPress.ShowDead             => (state.copy(screen = Screen.Dead, deadScroll = 0, message = None), Nil)
            case KeyPress.PruneDead            => (state, Nil)
            // On the Log tab the arrows scroll the log, which is what they are for when a log is what you are looking at. Rows stay selectable by clicking.
            case KeyPress.Up if state.tab == Tab.Log     => (state.copy(logScrollFromBottom = clamp(state.logScrollFromBottom + 1, state.logTail.length)), Nil)
            case KeyPress.Down if state.tab == Tab.Log   => (state.copy(logScrollFromBottom = clamp(state.logScrollFromBottom - 1, state.logTail.length)), Nil)
            case KeyPress.Up if state.tab == Tab.Startup => (state.copy(startupScrollY = math.max(0, state.startupScrollY - 1)), Nil)
            case KeyPress.Down if state.tab == Tab.Startup => (state.copy(startupScrollY = state.startupScrollY + 1), Nil)
            case KeyPress.Up                               => (state.copy(selected = clamp(state.selected - 1, state.live.length), message = None), Nil)
            case KeyPress.Down                             => (state.copy(selected = clamp(state.selected + 1, state.live.length), message = None), Nil)
            case KeyPress.NextTab                          => (state.copy(tab = shiftTab(state.tab, 1)), Nil)
            // Sideways scrolling on Startup, until the pane is at that edge — then the arrow does what it does everywhere else and changes tab. Otherwise
            // arriving here with → would leave no way back with ←.
            case KeyPress.Left if state.tab == Tab.Startup && state.startupScrollX > 0 =>
              (state.copy(startupScrollX = clampX(state, state.startupScrollX - 8)), Nil)
            case KeyPress.Right if state.tab == Tab.Startup && state.startupScrollX < state.startupMaxScrollX =>
              (state.copy(startupScrollX = clampX(state, state.startupScrollX + 8)), Nil)
            case KeyPress.Left              => (state.copy(tab = shiftTab(state.tab, -1)), Nil)
            case KeyPress.Right             => (state.copy(tab = shiftTab(state.tab, 1)), Nil)
            case KeyPress.Kill              => (confirming(state, Action.Kill), Nil)
            case KeyPress.Restart           => (confirming(state, Action.Restart), Nil)
            case KeyPress.Yes | KeyPress.No => (state, Nil)
          }
      }
  }

  /** Only a server with a process behind it can be stopped, and saying so beats a confirmation prompt for something that will do nothing. A wedged one can be
    * killed — that is what it is waiting for — but not restarted, which needs it to answer.
    */
  private def confirming(state: ServerTopState, action: Action): ServerTopState =
    state.selectedRow match {
      case None                                                         => state.copy(message = Some("no server selected"))
      case Some(row) if action == Action.Restart && !row.info.isRunning =>
        state.copy(message = Some(s"${row.name} is ${row.info.state.label} — kill it instead"))
      case Some(row) => state.copy(pending = Some(Confirm.OnServer(action, row.hash, row.name)), message = None)
    }

  /** Wraps in both directions, so ← from the first tab lands on the last rather than doing nothing. */
  private def shiftTab(tab: Tab, by: Int): Tab = {
    val all = Tab.all
    all(((all.indexOf(tab) + by) % all.length + all.length) % all.length)
  }

  private def clampX(state: ServerTopState, x: Int): Int = math.max(0, math.min(x, state.startupMaxScrollX))

  private def clamp(index: Int, size: Int): Int =
    if (size <= 0) 0 else math.max(0, math.min(index, size - 1))
}
