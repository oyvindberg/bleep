package bleep
package commands
package server

import bleep.testing.FancyBuildDisplay
import ryddig.Logger

/** `bleep server top` — the live dashboard, and what bare `bleep server` runs.
  *
  * Falls back to the `ls` listing when the terminal cannot support it, naming the reason rather than failing or silently printing something else. That is
  * mostly pipes and CI logs now: since the jatatui migration this works on Windows too.
  */
case class ServerTop(logger: Logger, userPaths: UserPaths, currentWorkspace: Option[java.nio.file.Path]) extends BleepCommand {

  override def run(): Either[BleepException, Unit] =
    FancyBuildDisplay.checkSupport match {
      case Right(()) =>
        new tui.ServerTopLoop(logger, userPaths, currentWorkspace).run()
        Right(())
      case Left(reason) =>
        logger.warn(s"$reason — showing the listing instead")
        ServerLs(logger, userPaths, OutputMode.Text, currentWorkspace).run()
    }
}
