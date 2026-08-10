package bleep
package commands
package server
package tui

import bleep.bsp.{ServerAdminClient, ServerDirs}
import jatatui.core.terminal.Terminal
import jatatui.crossterm.{CrosstermBackend, Jatatui}
import jatatui.react.Renderer
import ryddig.Logger

import java.nio.file.Path

/** The only impure part of the dashboard: terminal setup, the poll, the event loop.
  *
  * Everything it decides is decided by [[ServerTopUpdate.update]] and everything it draws is drawn by [[ServerTopView]]; this owns the clock, the socket and
  * the keyboard, which is exactly the set of things a test should not have to own.
  */
/** Takes no logger on purpose: anything written to stderr lands on the screen this is drawing, so every command it runs gets a storing logger instead. */
class ServerTopLoop(userPaths: UserPaths, currentWorkspace: Option[Path]) {
  import ServerTopState._

  private val PollIntervalMs = 1000L
  private val InputPollNanos = 100_000_000 // 100ms, so a keystroke never waits on the data tick
  private val LogTailLines = 500
  private val LogPollIntervalMs = 250L

  def run(): Unit = {
    val terminal: Terminal[CrosstermBackend] = Jatatui.init()
    try {
      val jni = terminal.backend().writer()
      // Jatatui.init sets up the alternate screen and raw mode but not the mouse: without this the terminal keeps handling clicks itself and nothing reaches
      // the dashboard.
      jni.execute(new _root_.tui.crossterm.Command.EnableMouseCapture())
      val renderer = new Renderer

      var state = ServerTopUpdate.update(initialState(), Msg.Refreshed(scan(), System.currentTimeMillis()))._1
      state = ServerTopUpdate.update(state, Msg.LogTail(tailOfSelectedLog(state)))._1
      var lastPoll = System.currentTimeMillis()
      var lastLogPoll = lastPoll

      // Clicks arrive while the element tree is being built, so they land here and are applied on the next turn of the loop, exactly like keystrokes.
      val clicked = new java.util.concurrent.ConcurrentLinkedQueue[Msg]()
      val dispatch: Msg => Unit = msg => clicked.add(msg): Unit

      while (!state.quit) {
        terminal.draw(frame => renderer.render(frame, ServerTopView.render(state, dispatch))): Unit

        var pendingClick = clicked.poll()
        while (pendingClick != null) {
          val before = state.selectedRow.map(_.hash)
          state = applyMsg(state, pendingClick)
          // Selecting another server should show that server's log, not the previous one's until the next tick.
          if (state.selectedRow.map(_.hash) != before) state = ServerTopUpdate.update(state, Msg.LogTail(tailOfSelectedLog(state)))._1
          pendingClick = clicked.poll()
        }

        // Drain every pending event before drawing again. A mouse wheel emits events far faster than a frame takes to render, so handling one per frame makes
        // the pane fall further and further behind until it looks frozen — the build display learned the same lesson about scroll lag.
        var hasEvent = jni.poll(new _root_.tui.crossterm.Duration(0, InputPollNanos))
        while (hasEvent && !state.quit) {
          jni.read() match {
            case key: _root_.tui.crossterm.Event.Key =>
              keyPress(key).foreach { press =>
                val before = state.selectedRow.map(_.hash)
                state = applyMsg(state, Msg.Key(press))
                if (state.selectedRow.map(_.hash) != before) state = ServerTopUpdate.update(state, Msg.LogTail(tailOfSelectedLog(state)))._1
              }

            case mouse: _root_.tui.crossterm.Event.Mouse =>
              // Horizontal wheel never reaches the react layer — it has no kind for it — so it is turned into a message here. It is also the natural way to
              // read a classpath, which is the one pane wide enough to need it.
              mouse.mouseEvent.kind match {
                case _: _root_.tui.crossterm.MouseEventKind.ScrollLeft if state.tab == ServerTopState.Tab.Startup =>
                  state = applyMsg(state, Msg.ScrollStartup(0, -8))
                case _: _root_.tui.crossterm.MouseEventKind.ScrollRight if state.tab == ServerTopState.Tab.Startup =>
                  state = applyMsg(state, Msg.ScrollStartup(0, 8))
                case _ =>
                  // Handing the event to the renderer is what makes a click land on whichever element owns that cell; the element then dispatches its own Msg.
                  mouseEvent(mouse).foreach(event => renderer.dispatchMouse(event): Unit)
              }

            case _ => ()
          }

          var queued = clicked.poll()
          while (queued != null) {
            val before = state.selectedRow.map(_.hash)
            state = applyMsg(state, queued)
            if (state.selectedRow.map(_.hash) != before) state = ServerTopUpdate.update(state, Msg.LogTail(tailOfSelectedLog(state)))._1
            queued = clicked.poll()
          }

          hasEvent = jni.poll(new _root_.tui.crossterm.Duration(0, 0))
        }

        val now = System.currentTimeMillis()

        // The log gets its own, faster cadence: status is a round trip per server, the log is a seek to the end of one file, and a log you are watching should
        // move at something closer to the speed it is being written.
        if (state.tab == ServerTopState.Tab.Log && now - lastLogPoll >= LogPollIntervalMs) {
          state = ServerTopUpdate.update(state, Msg.LogTail(tailOfSelectedLog(state)))._1
          lastLogPoll = now
        }

        if (now - lastPoll >= PollIntervalMs) {
          state = ServerTopUpdate.update(state, Msg.Refreshed(scan(), now))._1
          state = ServerTopUpdate.update(state, Msg.LogTail(tailOfSelectedLog(state)))._1
          lastPoll = now
          lastLogPoll = now
        }
      }
    } finally Jatatui.restore()
  }

  private def initialState(): ServerTopState = ServerTopState.initial(System.currentTimeMillis())

  /** One path for everything the user does, mouse or keyboard: pure update, then run whatever effects it asked for. */
  private def applyMsg(state: ServerTopState, msg: Msg): ServerTopState = {
    val (next, effects) = ServerTopUpdate.update(state, msg)
    effects.foldLeft(next) { case (current, Effect.Perform(action, row)) =>
      // Run the same command the CLI runs. One implementation, so the TUI cannot drift into doing something subtly different.
      val message = perform(action, row.hash)
      val reported = ServerTopUpdate.update(current, Msg.ActionFinished(message))._1
      ServerTopUpdate.update(reported, Msg.Refreshed(scan(), System.currentTimeMillis()))._1
    }
  }

  /** The last few lines of the selected server's log.
    *
    * Read from the end of the file rather than by loading it: these logs run to tens of megabytes, and this happens once a second.
    */
  private def tailOfSelectedLog(state: ServerTopState): List[String] =
    state.selectedRow match {
      case None      => Nil
      case Some(row) =>
        val file = row.info.socketDir.resolve("output")
        if (!java.nio.file.Files.exists(file)) Nil
        else {
          val channel = java.nio.file.Files.newByteChannel(file)
          try {
            val size = channel.size()
            val window = math.min(size, 16384L)
            channel.position(size - window)
            val buffer = java.nio.ByteBuffer.allocate(window.toInt)
            channel.read(buffer): Unit
            val text = new String(buffer.array(), java.nio.charset.StandardCharsets.UTF_8)
            // The first line is usually cut in half by the window, so drop it unless we happened to read the whole file.
            val lines = text.linesIterator.toList
            val whole = if (window == size) lines else lines.drop(1)
            whole.takeRight(LogTailLines)
          } finally channel.close()
        }
    }

  private def mouseEvent(mouse: _root_.tui.crossterm.Event.Mouse): Option[jatatui.react.MouseEvent] = {
    val event = mouse.mouseEvent
    // Every kind the binding can produce is named here, horizontal scroll included. Beyond mapping them, naming them keeps them reachable: native-image drops
    // classes nothing references, and the crossterm jar's own JNI config missed ScrollLeft/ScrollRight — see the reachability metadata in src/resources.
    val kind = event.kind match {
      case _: _root_.tui.crossterm.MouseEventKind.Down       => Some(jatatui.react.MouseEvent.Kind.DOWN)
      case _: _root_.tui.crossterm.MouseEventKind.Up         => Some(jatatui.react.MouseEvent.Kind.UP)
      case _: _root_.tui.crossterm.MouseEventKind.Drag       => Some(jatatui.react.MouseEvent.Kind.DRAG)
      case _: _root_.tui.crossterm.MouseEventKind.Moved      => Some(jatatui.react.MouseEvent.Kind.MOVE)
      case _: _root_.tui.crossterm.MouseEventKind.ScrollUp   => Some(jatatui.react.MouseEvent.Kind.SCROLL_UP)
      case _: _root_.tui.crossterm.MouseEventKind.ScrollDown => Some(jatatui.react.MouseEvent.Kind.SCROLL_DOWN)
      // Horizontal scroll has nothing to move here, but it must still be matched rather than left to fall through as an unknown event.
      case _: _root_.tui.crossterm.MouseEventKind.ScrollLeft  => None
      case _: _root_.tui.crossterm.MouseEventKind.ScrollRight => None
      case _                                                  => None
    }
    kind.map(k => new jatatui.react.MouseEvent(event.column, event.row, event.modifiers, k))
  }

  private def keyPress(key: _root_.tui.crossterm.Event.Key): Option[KeyPress] =
    key.keyEvent.code match {
      case _: _root_.tui.crossterm.KeyCode.Up      => Some(KeyPress.Up)
      case _: _root_.tui.crossterm.KeyCode.Down    => Some(KeyPress.Down)
      case _: _root_.tui.crossterm.KeyCode.Tab     => Some(KeyPress.NextTab)
      case _: _root_.tui.crossterm.KeyCode.Right   => Some(KeyPress.Right)
      case _: _root_.tui.crossterm.KeyCode.Left    => Some(KeyPress.Left)
      case _: _root_.tui.crossterm.KeyCode.Esc     => Some(KeyPress.Quit)
      case char: _root_.tui.crossterm.KeyCode.Char =>
        char.c() match {
          case 'q' | 'Q' => Some(KeyPress.Quit)
          case 'k'       => Some(KeyPress.Kill)
          case 'r'       => Some(KeyPress.Restart)
          case 'y' | 'Y' => Some(KeyPress.Yes)
          case 'n' | 'N' => Some(KeyPress.No)
          case 'j'       => Some(KeyPress.Down)
          case _         => None
        }
      case _ => None
    }

  /** One status query per running server per tick. Observer connections, so watching never keeps a daemon alive or resets its idle clock. */
  private def scan(): List[ServerRow] = {
    val infos = ServerDirs.scan(userPaths)
    val mine = currentWorkspace.map(_.toAbsolutePath.normalize().toString)

    val queried = infos.map { info =>
      val (status, error) =
        if (!info.isRunning) (None, None)
        else
          ServerAdminClient.status(info.socketDir) match {
            case Right(status) => (Some(status), None)
            case Left(err)     => (None, Some(err))
          }
      (info, status, error)
    }

    // Several servers can hold the same workspace — every bleep version ever run here leaves one — but only one is the server this build talks to. Marking
    // more than one is worse than marking none, since that label is what a reader trusts when deciding which to kill.
    val holders = mine.toList.flatMap { path =>
      queried.collect { case (info, Some(status), _) if status.workspaces.exists(_.path == path) => (info.hash, info.identity.map(_.bleepVersion)) }
    }
    val currentHash = ServerDirs.currentAmong(holders, bleep.model.BleepVersion.current.value)

    queried
      .map { case (info, status, error) => ServerRow(info, status, error, isCurrent = currentHash.contains(info.hash)) }
      .sortBy(row => (!row.isCurrent, !row.info.isRunning, row.hash))
  }

  /** Run a command without letting it write to the terminal.
    *
    * The commands log their progress, and a logger writing to stderr goes straight to the screen the dashboard is drawing on — the line lands on top of
    * whatever was there, corrupting the frame until the next full repaint. Observed when stopping a server that turned out to be from an older bleep: its
    * "cannot report status" warning was painted across the server list.
    *
    * So the TUI hands the command a logger that stores instead of printing, and shows what it collected in the footer, where a message belongs.
    */
  private def perform(action: Action, hash: String): String = {
    val stored = ryddig.Loggers.storing()
    val command = commandFor(action, hash, stored)

    val outcome = command.run() match {
      case Right(())       => s"${action.verb} $hash"
      case Left(exception) => s"${action.verb} $hash failed: ${exception.message}"
    }

    // The last line the command logged is the informative one — "stopped", or why it could not be.
    val lastMessage = stored.underlying.lastOption.map(_.message.plainText.trim).filter(_.nonEmpty)
    lastMessage.fold(outcome)(message => s"$outcome: $message")
  }

  private def commandFor(action: Action, hash: String, logger: Logger): BleepCommand = {
    val command = action match {
      case Action.Kill =>
        ServerKill(logger, userPaths, List(hash), all = false, force = false, deleteDir = false, deprecatedAlias = None, currentWorkspace = currentWorkspace)
      case Action.Restart =>
        ServerRestart(logger, userPaths, List(hash), all = false, currentWorkspace = currentWorkspace)
    }
    command
  }
}
