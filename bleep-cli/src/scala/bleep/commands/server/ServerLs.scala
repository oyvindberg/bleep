package bleep
package commands
package server

import bleep.bsp.{ServerAdminClient, ServerDirInfo, ServerDirs, ServerState}
import io.circe.syntax._
import ryddig.Logger

import java.time.Duration

/** `bleep server ls` — every compile server on this machine, whatever state it is in.
  *
  * A daemon per bleep version and JVM configuration is normal; a daemon left behind after an upgrade is not, and until now nothing showed you either. Running
  * servers are enriched with live numbers over `bleep/status`; dead ones are named from `server.json`. Nothing is filtered out.
  */
case class ServerLs(logger: Logger, userPaths: UserPaths, outputMode: OutputMode, currentWorkspace: Option[java.nio.file.Path]) extends BleepCommand {

  /** One row per socket directory, paired with whatever the daemon would say about itself. Queried once and reused for both the "is this mine" test and the
    * rendering, so `ls` opens at most one connection per running daemon.
    */
  private case class Row(info: ServerDirInfo, status: Option[bleep.bsp.protocol.DaemonStatus], error: Option[bleep.bsp.AdminError]) {

    /** Serves the build you are standing in. Answered from what the daemon reports it is serving rather than by re-deriving its JVM-key hash: the daemon's own
      * account is the truth, and it stays right even when config has drifted since it started.
      */
    def isCurrent: Boolean =
      currentWorkspace.exists(cwd => status.exists(_.workspaces.exists(_.path == cwd.toString)))
  }

  override def run(): Either[BleepException, Unit] = {
    val rows = ServerDirs.scan(userPaths).map { info =>
      if (!info.isRunning) Row(info, None, None)
      else
        ServerAdminClient.status(info.socketDir) match {
          case Right(status) => Row(info, Some(status), None)
          case Left(err)     => Row(info, None, Some(err))
        }
    }

    // Yours first — it is the one you almost always came to look at.
    val ordered = rows.sortBy(row => (!row.isCurrent, !row.info.isRunning, row.info.hash))

    outputMode match {
      case OutputMode.Json => renderJson(ordered)
      case _               => renderText(ordered)
    }
    Right(())
  }

  private def renderJson(rows: List[Row]): Unit = {
    val json = rows.map { row =>
      val info = row.info
      val status = row.status
      io.circe.Json.obj(
        "hash" -> info.hash.asJson,
        "current" -> row.isCurrent.asJson,
        "state" -> info.state.label.asJson,
        "pid" -> info.pid.asJson,
        "bleepVersion" -> info.bleepVersion.asJson,
        "jvm" -> info.jvm.asJson,
        "socketDir" -> info.socketDir.toString.asJson,
        "sizeMb" -> info.sizeMb.asJson,
        "uptimeMs" -> status.map(s => System.currentTimeMillis() - s.startedAtEpochMs).asJson,
        "heapUsedMb" -> status.map(_.jvm.heapUsedMb).asJson,
        "heapMaxMb" -> status.map(_.jvm.heapMaxMb).asJson,
        "connections" -> status.map(_.connections.size).asJson,
        "workspaces" -> status.map(_.workspaces.map(_.path)).asJson,
        "activeCompiles" -> status.map(_.machine.activeCompiles).asJson,
        "error" -> row.error.map(_.message).asJson
      )
    }
    println(io.circe.Json.arr(json*).spaces2)
  }

  private def renderText(rows: List[Row]): Unit =
    if (rows.isEmpty) logger.info("no compile servers — one starts on the next build")
    else
      rows.foreach { row =>
        val info = row.info
        val marker = info.state match {
          case ServerState.Running => "●"
          case ServerState.Wedged  => "!"
          case _                   => "◇"
        }
        // The one serving the directory you are standing in. Everything else on the machine is someone else's daemon, or a leftover.
        val current = if (row.isCurrent) " ← this build" else ""
        val head = s"$marker ${info.hash}  ${info.state.label}  ${info.bleepVersion}  ${info.jvm}$current"

        val detail = row.status match {
          case Some(status) =>
            val uptime = humanDuration(System.currentTimeMillis() - status.startedAtEpochMs)
            val workspaces = if (status.workspaces.isEmpty) "no workspaces" else s"${status.workspaces.size} workspaces"
            val busy = if (status.machine.activeCompiles > 0) s", ${status.machine.activeCompiles} compiling" else ""
            val queued = if (status.machine.waiting.nonEmpty) s", ${status.machine.waiting.size} queued" else ""
            ServerAdminClient.skewWarning(info.socketDir, status).foreach(warning => logger.warn(warning.message))
            s"  pid ${status.pid}  heap ${status.jvm.heapUsedMb}/${status.jvm.heapMaxMb}MB  up $uptime  " +
              s"${status.connections.size} clients  $workspaces$busy$queued"
          case None =>
            // An unreachable or too-old daemon is a row with a reason attached, never a row that disappears.
            row.error match {
              case Some(err) => s"  ${err.message}"
              case None      => s"  pid ${info.pid.map(_.toString).getOrElse("—")}  ${info.sizeMb}MB on disk"
            }
        }

        info.state match {
          case ServerState.Wedged => logger.warn(head); logger.warn(detail)
          case _                  => logger.info(head); logger.info(detail)
        }
      }

  private def humanDuration(ms: Long): String = {
    val d = Duration.ofMillis(ms)
    if (d.toHours > 0) s"${d.toHours}h${d.toMinutesPart}m"
    else if (d.toMinutes > 0) s"${d.toMinutes}m${d.toSecondsPart}s"
    else s"${d.toSeconds}s"
  }
}
