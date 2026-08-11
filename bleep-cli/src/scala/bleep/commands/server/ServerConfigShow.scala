package bleep
package commands
package server

import bleep.bsp.{ServerAdminClient, ServerDirs}
import bleep.bsp.protocol.DaemonStatus
import bleep.model.BspServerConfig
import io.circe.syntax._
import ryddig.Logger

/** `bleep server config show` — every knob, what it is set to, and what the running server actually booted with.
  *
  * The last column is the point. These settings are read once when a server starts, so editing the file changes nothing until the server is replaced — and a
  * config command that only ever showed you the file would let you believe otherwise. Where the two disagree, this says so and tells you how to apply it.
  */
case class ServerConfigShow(
    logger: Logger,
    userPaths: UserPaths,
    outputMode: OutputMode,
    currentWorkspace: Option[java.nio.file.Path]
) extends BleepCommand {

  private case class Row(knob: String, configured: Option[String], running: Option[String]) {

    /** Only a difference the user can act on: a knob they set that the server is not honouring. */
    def drifted: Boolean = (configured, running) match {
      case (Some(set), Some(live)) => set != live
      case _                       => false
    }
  }

  override def run(): Either[BleepException, Unit] = {
    val config = BleepConfigOps.loadOrDefault(userPaths).orThrow.bspServerConfigOrDefault

    // Best-effort: `show` is useful with no server running at all, so a missing or unreachable one just means no third column.
    val running: Option[(String, DaemonStatus)] =
      currentWorkspace
        .flatMap(workspace => ServerDirs.servingWorkspace(ServerDirs.scan(userPaths), workspace))
        .flatMap(info => ServerAdminClient.status(info.socketDir).toOption.map(status => (info.hash, status)))

    val rows = buildRows(config, running.map(_._2))

    outputMode match {
      case OutputMode.Json =>
        println(
          io.circe.Json
            .obj(
              "configFile" -> userPaths.configYaml.toString.asJson,
              "runningServer" -> running.map(_._1).asJson,
              "knobs" -> io.circe.Json.arr(
                rows.map(row =>
                  io.circe.Json.obj(
                    "knob" -> row.knob.asJson,
                    "configured" -> row.configured.asJson,
                    "running" -> row.running.asJson,
                    "drifted" -> row.drifted.asJson
                  )
                )*
              )
            )
            .spaces2
        )
      case _ =>
        logger.info(s"config file: ${userPaths.configYaml}")
        running match {
          case Some((hash, _)) => logger.info(s"running server: $hash (third column is what it booted with)")
          case None            => logger.info("no running server for this build — showing the file only")
        }

        val width = rows.map(_.knob.length).maxOption.getOrElse(0)
        rows.foreach { row =>
          val configured = row.configured.getOrElse("—")
          val live = row.running.map(value => s"  server: $value").getOrElse("")
          val line = s"  ${row.knob.padTo(width, ' ')}  ${configured.padTo(12, ' ')}$live"
          if (row.drifted) logger.warn(s"$line   ← differs; `bleep server restart` to apply") else logger.info(line)
        }

        if (rows.exists(_.drifted))
          logger.warn("some settings are not in effect on the running server")
    }
    Right(())
  }

  private def buildRows(config: BspServerConfig, status: Option[DaemonStatus]): List[Row] = {
    def row[A](knob: String, configured: Option[A], running: Option[A]): Row =
      Row(knob, configured.map(_.toString), running.map(_.toString))

    List(
      row("parallelism", config.parallelism, status.map(_.config.parallelism)),
      row("parallelism-ratio", config.parallelismRatio, None),
      row("max-memory", config.compileServerMaxMemory, status.flatMap(_.config.compileServerMaxMemory)),
      row("max-cached-workspaces", config.maxCachedWorkspaces, status.map(_.config.maxCachedWorkspaces)),
      row("read-timeout", config.bspReadTimeoutMinutes, status.map(_.config.bspReadTimeoutMillis / 60000)),
      row("idle-timeout", config.compileServerIdleTimeoutMinutes, status.map(_.config.compileServerIdleTimeoutMillis / 60000)),
      row("heap-pressure-threshold", config.heapPressureThreshold, status.map(_.config.heapPressureThreshold)),
      row("test-runner-heap", config.testRunnerHeap, status.flatMap(_.config.testRunnerHeap)),
      row("test-idle-timeout", config.testIdleTimeoutMinutes, status.map(_.config.testIdleTimeoutMinutes)),
      row("sourcegen-max-memory", config.sourcegenMaxMemory, None),
      row("ksp-runner-max-memory", config.kspRunnerMaxMemory, None)
    )
  }
}
