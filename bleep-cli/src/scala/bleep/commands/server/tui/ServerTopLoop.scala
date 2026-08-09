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
class ServerTopLoop(logger: Logger, userPaths: UserPaths, currentWorkspace: Option[Path]) {
  import ServerTopState._

  private val PollIntervalMs = 1000L
  private val InputPollNanos = 100_000_000 // 100ms, so a keystroke never waits on the data tick

  def run(): Unit = {
    val terminal: Terminal[CrosstermBackend] = Jatatui.init()
    try {
      val jni = terminal.backend().writer()
      val renderer = new Renderer

      var state = ServerTopUpdate.update(initialState(), Msg.Refreshed(scan(), System.currentTimeMillis()))._1
      var lastPoll = System.currentTimeMillis()

      while (!state.quit) {
        terminal.draw(frame => renderer.render(frame, ServerTopView.render(state))): Unit

        if (jni.poll(new _root_.tui.crossterm.Duration(0, InputPollNanos)))
          jni.read() match {
            case key: _root_.tui.crossterm.Event.Key =>
              keyPress(key).foreach { press =>
                val (next, effects) = ServerTopUpdate.update(state, Msg.Key(press))
                state = next
                effects.foreach { case Effect.Perform(action, row) =>
                  // Run the same command the CLI runs. One implementation, so the TUI cannot drift into doing something subtly different.
                  val message = perform(action, row.hash)
                  state = ServerTopUpdate.update(state, Msg.ActionFinished(message))._1
                  state = ServerTopUpdate.update(state, Msg.Refreshed(scan(), System.currentTimeMillis()))._1
                }
              }
            case _ => ()
          }

        val now = System.currentTimeMillis()
        if (now - lastPoll >= PollIntervalMs) {
          state = ServerTopUpdate.update(state, Msg.Refreshed(scan(), now))._1
          lastPoll = now
        }
      }
    } finally Jatatui.restore()
  }

  private def initialState(): ServerTopState = ServerTopState.initial(System.currentTimeMillis())

  private def keyPress(key: _root_.tui.crossterm.Event.Key): Option[KeyPress] =
    key.keyEvent.code match {
      case _: _root_.tui.crossterm.KeyCode.Up      => Some(KeyPress.Up)
      case _: _root_.tui.crossterm.KeyCode.Down    => Some(KeyPress.Down)
      case _: _root_.tui.crossterm.KeyCode.Tab     => Some(KeyPress.NextTab)
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

    infos
      .map { info =>
        val (status, error) =
          if (!info.isRunning) (None, None)
          else
            ServerAdminClient.status(info.socketDir) match {
              case Right(status) => (Some(status), None)
              case Left(err)     => (None, Some(err))
            }
        ServerRow(info, status, error, isCurrent = mine.exists(path => status.exists(_.workspaces.exists(_.path == path))))
      }
      .sortBy(row => (!row.isCurrent, !row.info.isRunning, row.hash))
  }

  private def perform(action: Action, hash: String): String = {
    val command = action match {
      case Action.Kill =>
        ServerKill(logger, userPaths, List(hash), all = false, force = false, deleteDir = false, deprecatedAlias = None, currentWorkspace = currentWorkspace)
      case Action.Restart =>
        ServerRestart(logger, userPaths, List(hash), all = false, currentWorkspace = currentWorkspace)
    }
    command.run() match {
      case Right(())       => s"${action.verb} $hash: done"
      case Left(exception) => s"${action.verb} $hash failed: ${exception.message}"
    }
  }
}
