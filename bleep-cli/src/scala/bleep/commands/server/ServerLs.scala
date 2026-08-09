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
case class ServerLs(logger: Logger, userPaths: UserPaths, outputMode: OutputMode) extends BleepCommand {

  override def run(): Either[BleepException, Unit] = {
    val infos = ServerDirs.scan(userPaths)

    outputMode match {
      case OutputMode.Json => renderJson(infos)
      case _               => renderText(infos)
    }
    Right(())
  }

  private def renderJson(infos: List[ServerDirInfo]): Unit = {
    val rows = infos.map { info =>
      val status = if (info.isRunning) ServerAdminClient.status(info.socketDir).toOption else None
      io.circe.Json.obj(
        "hash" -> info.hash.asJson,
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
        "activeCompiles" -> status.map(_.machine.activeCompiles).asJson
      )
    }
    println(io.circe.Json.arr(rows*).spaces2)
  }

  private def renderText(infos: List[ServerDirInfo]): Unit =
    if (infos.isEmpty) logger.info("no compile servers — one starts on the next build")
    else
      infos.foreach { info =>
        val marker = info.state match {
          case ServerState.Running => "●"
          case ServerState.Wedged  => "!"
          case _                   => "◇"
        }
        val head = s"$marker ${info.hash}  ${info.state.label}  ${info.bleepVersion}  ${info.jvm}"

        val detail =
          if (info.isRunning)
            ServerAdminClient.status(info.socketDir) match {
              case Right(status) =>
                val uptime = humanDuration(System.currentTimeMillis() - status.startedAtEpochMs)
                val workspaces = if (status.workspaces.isEmpty) "no workspaces" else s"${status.workspaces.size} workspaces"
                val busy = if (status.machine.activeCompiles > 0) s", ${status.machine.activeCompiles} compiling" else ""
                val queued = if (status.machine.waiting.nonEmpty) s", ${status.machine.waiting.size} queued" else ""
                ServerAdminClient.skewWarning(info.socketDir, status).foreach(warning => logger.warn(warning.message))
                s"  pid ${status.pid}  heap ${status.jvm.heapUsedMb}/${status.jvm.heapMaxMb}MB  up $uptime  " +
                  s"${status.connections.size} clients  $workspaces$busy$queued"
              case Left(err) =>
                // An unreachable or too-old daemon is a row with a reason attached, never a row that disappears.
                s"  ${err.message}"
            }
          else s"  pid ${info.pid.map(_.toString).getOrElse("—")}  ${info.sizeMb}MB on disk"

        info.state match {
          case ServerState.Wedged  => logger.warn(head); logger.warn(detail)
          case ServerState.Running => logger.info(head); logger.info(detail)
          case _                   => logger.info(head); logger.info(detail)
        }
      }

  private def humanDuration(ms: Long): String = {
    val d = Duration.ofMillis(ms)
    if (d.toHours > 0) s"${d.toHours}h${d.toMinutesPart}m"
    else if (d.toMinutes > 0) s"${d.toMinutes}m${d.toSecondsPart}s"
    else s"${d.toSeconds}s"
  }
}
