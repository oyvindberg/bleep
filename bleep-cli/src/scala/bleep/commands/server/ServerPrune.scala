package bleep
package commands
package server

import bleep.bsp.{ServerDirInfo, ServerDirs, ServerState}
import bleep.internal.FileUtils
import ryddig.Logger

import java.nio.file.Files
import scala.jdk.StreamConverters.StreamHasToScala

/** `bleep server prune` — delete the socket directories of compile servers that are no longer running.
  *
  * Every bleep version and JVM configuration ever run on a machine leaves one behind, and nothing removes them except the spawn-time sweep, which waits days. A
  * hundred of them is normal after a few weeks of local snapshot deploys. This is the explicit "clear them out", and what the dashboard's dead-servers view
  * runs.
  *
  * Only dead and litter directories are touched: a running or wedged server still has a process behind it, which is `kill`'s business. Each directory is
  * classified again right before it is deleted, and one written to in the last [[RecentlyTouchedMs]] is left alone — that is a client in the middle of spawning
  * a fresh daemon into it, holding its spawn lock there before any pid exists.
  */
case class ServerPrune(logger: Logger, userPaths: UserPaths) extends BleepCommand {

  private val RecentlyTouchedMs = 60_000L

  override def run(): Either[BleepException, Unit] = {
    val stopped = ServerDirs.scan(userPaths).filter(isStopped)
    if (stopped.isEmpty) logger.info("no stopped compile servers to remove")
    else {
      val removed = stopped.flatMap(prune)
      val freedMb = removed.map(_.sizeMb).sum
      logger.info(s"removed ${removed.size} of ${stopped.size} stopped compile server(s), ${freedMb}MB freed")
    }
    Right(())
  }

  private def isStopped(info: ServerDirInfo): Boolean = info.state match {
    case ServerState.Dead(_) | ServerState.Litter => true
    case ServerState.Running | ServerState.Wedged => false
  }

  private def prune(seen: ServerDirInfo): Option[ServerDirInfo] = {
    val now = ServerDirs.classify(seen.socketDir)
    if (!isStopped(now)) {
      logger.info(s"${now.hash} is ${now.state.label} now — left alone")
      None
    } else if (System.currentTimeMillis() - newestModificationMs(now) < RecentlyTouchedMs) {
      logger.info(s"${now.hash} was written to in the last minute, probably a server starting — left alone")
      None
    } else {
      FileUtils.deleteDirectory(now.socketDir)
      logger.info(s"removed ${now.hash} (${now.state.label}, ${now.sizeMb}MB)")
      Some(now)
    }
  }

  private def newestModificationMs(info: ServerDirInfo): Long = {
    val stream = Files.walk(info.socketDir)
    try stream.toScala(List).map(Files.getLastModifiedTime(_).toMillis).max
    finally stream.close()
  }
}
