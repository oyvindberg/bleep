package bleep
package commands
package server

import bleep.bsp.{ServerAdminClient, ServerDirInfo, ServerDirs}
import io.circe.syntax._
import ryddig.Logger

import java.time.Duration

/** `bleep server status [<id>]` — the deep, one-shot view of a single daemon. The scriptable twin of the `top` TUI.
  *
  * With no id and exactly one running server, that one is used. With several, it says which ids to choose from rather than picking one.
  */
case class ServerStatus(logger: Logger, userPaths: UserPaths, id: Option[String], outputMode: OutputMode, currentWorkspace: Option[java.nio.file.Path])
    extends BleepCommand {

  override def run(): Either[BleepException, Unit] = {
    val infos = ServerDirs.scan(userPaths)

    select(infos).flatMap { info =>
      ServerAdminClient.status(info.socketDir) match {
        case Left(err)     => Left(new BleepException.Text(err.message))
        case Right(status) =>
          ServerAdminClient.skewWarning(info.socketDir, status).foreach(warning => logger.warn(warning.message))
          outputMode match {
            case OutputMode.Json => println(status.asJson.spaces2)
            case _               => renderText(info, status)
          }
          Right(())
      }
    }
  }

  private def select(infos: List[ServerDirInfo]): Either[BleepException, ServerDirInfo] =
    ServerTarget.select(infos, id, currentWorkspace, allowStopped = false, what = "inspect")

  private def renderText(info: ServerDirInfo, status: bleep.bsp.protocol.DaemonStatus): Unit = {
    val uptime = Duration.ofMillis(System.currentTimeMillis() - status.startedAtEpochMs)
    logger.info(s"${info.hash}  pid ${status.pid}  bleep ${status.bleepVersion}  up ${uptime.toHours}h${uptime.toMinutesPart}m")
    logger.info(s"  socket   ${status.socketDir}")

    val live = if (status.jvm.heapLiveMb < 0) "n/a" else s"${status.jvm.heapLiveMb}MB"
    logger.info(s"  heap     ${status.jvm.heapUsedMb}/${status.jvm.heapMaxMb}MB (live $live, committed ${status.jvm.heapCommittedMb}MB)")
    logger.info(s"  threads  ${status.jvm.threads} (peak ${status.jvm.peakThreads}, daemon ${status.jvm.daemonThreads})")
    logger.info(s"  cpu      process ${pct(status.jvm.cpuProcess)}, system ${pct(status.jvm.cpuSystem)}")
    logger.info(s"  fds      ${status.jvm.openFileDescriptors.map(_.toString).getOrElse("n/a")}")
    status.jvm.gc.foreach(gc => logger.info(s"  gc       ${gc.name}: ${gc.count} collections, ${gc.timeMs}ms"))

    logger.info(
      s"  machine  cpu ${status.machine.usedCpu}/${status.machine.totalCpu}, " +
        s"fork mem ${status.machine.usedMemoryMb}/${status.machine.totalMemoryMb}MB, ${status.machine.activeCompiles} compiling"
    )
    status.machine.active.foreach(e => logger.info(s"    running  ${e.kind} ${e.label} (cpu ${e.cpu}, ${e.memoryMb}MB, ${e.ageMs / 1000}s)"))
    status.machine.waiting.foreach(e => logger.info(s"    queued   ${e.kind} ${e.label} (cpu ${e.cpu}, ${e.memoryMb}MB, waiting ${e.ageMs / 1000}s)"))

    logger.info(s"  clients  ${status.connections.size}")
    status.connections.foreach { c =>
      val who = c.clientName.getOrElse(if (c.observer) "observer" else "unidentified")
      val version = c.clientVersion.map(v => s" $v").getOrElse("")
      val workspace = c.workspace.map(w => s" — $w").getOrElse("")
      logger.info(s"    #${c.connId} $who$version$workspace")
    }

    logger.info(s"  builds   ${status.buildCache.cachedWorkspaces.size}/${status.buildCache.bound} cached")
    status.workspaces.foreach { ws =>
      val cached = if (ws.buildCached) "cached" else "not cached"
      logger.info(s"    ${ws.path} ($cached)")
      ws.activeOperations.foreach(op => logger.info(s"      ${op.operation} ${op.projects.mkString(", ")} (${op.startedAgoMs / 1000}s)"))
    }

    val analysis = status.analysisCache
    logger.info(s"  analysis ${analysis.entries} entries, ${analysis.fileBytes / (1024 * 1024)}MB, ${analysis.sharedAnalyses} shared")

    val config = status.config
    logger.info(s"  config   parallelism ${config.parallelism}, max cached workspaces ${config.maxCachedWorkspaces}")
    logger.info(
      s"           idle timeout ${config.compileServerIdleTimeoutMillis / 60000}m, read timeout ${config.bspReadTimeoutMillis / 60000}m, " +
        s"heap pressure ${config.heapPressureThreshold}"
    )
    config.compileServerMaxMemory.foreach(m => logger.info(s"           compile server max memory $m"))
    config.testRunnerMaxMemory.foreach(m => logger.info(s"           test runner max memory $m"))
    logger.info("           (as booted — edits on disk apply on restart)")
  }

  private def pct(value: Double): String =
    if (value < 0) "n/a" else f"${value * 100}%.0f%%"
}
