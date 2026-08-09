package bleep
package commands
package server

import bleep.bsp.{ServerAdminClient, ServerDirInfo, ServerDirs}
import ryddig.Logger

/** `bleep server restart` — stop a compile server so the next build starts a fresh one.
  *
  * Deliberately does not respawn the daemon itself, even though `server.json` records the exact argv that would make that easy. Replaying that argv would
  * resurrect the JVM options the dead daemon booted with — including `-Xmx` — which is the precise thing people restart to escape. `bleep server config` even
  * tells you to restart to apply a change; a restart that reinstated the old value would make that advice a lie.
  *
  * So this stops the server and gets out of the way. The next build spawns one through the normal path, reading the config as it is on disk now. What the old
  * daemon was running is printed first, so you can see what is about to change.
  */
case class ServerRestart(
    logger: Logger,
    userPaths: UserPaths,
    ids: List[String],
    all: Boolean,
    currentWorkspace: Option[java.nio.file.Path]
) extends BleepCommand {

  override def run(): Either[BleepException, Unit] = {
    val infos = ServerDirs.scan(userPaths)

    select(infos).flatMap { targets =>
      if (targets.isEmpty) {
        logger.info("no compile server to restart — the next build will start one")
        Right(())
      } else {
        targets.foreach(describe)
        ServerKill(
          logger = logger,
          userPaths = userPaths,
          ids = targets.map(_.hash),
          all = false,
          force = false,
          deleteDir = false,
          deprecatedAlias = None,
          currentWorkspace = currentWorkspace
        ).run()
          .map { _ =>
            logger.info("stopped — the next build starts a fresh server with the config on disk now")
          }
      }
    }
  }

  private def select(infos: List[ServerDirInfo]): Either[BleepException, List[ServerDirInfo]] =
    if (all) Right(infos.filter(_.isRunning))
    else if (ids.nonEmpty)
      ids
        .map(id => ServerDirs.resolve(infos, id))
        .foldLeft[Either[String, List[ServerDirInfo]]](Right(Nil)) {
          case (Left(err), _)           => Left(err)
          case (_, Left(err))           => Left(err)
          case (Right(acc), Right(one)) => Right(acc :+ one)
        }
        .left
        .map(msg => new BleepException.Text(msg))
    else if (infos.forall(!_.isRunning)) Right(Nil)
    else ServerTarget.select(infos, None, currentWorkspace, allowStopped = false, what = "restart").map(List(_))

  /** Say what is going away, and where its config differs from what a fresh daemon would pick up. */
  private def describe(info: ServerDirInfo): Unit = {
    logger.info(s"restarting ${info.hash} (${info.bleepVersion}, ${info.jvm})")
    info.identity.foreach(id => logger.info(s"  it booted with: ${id.javaOpts.mkString(" ")}"))
    ServerAdminClient.status(info.socketDir).foreach { status =>
      val config = status.config
      logger.info(s"  and config: parallelism ${config.parallelism}, max cached workspaces ${config.maxCachedWorkspaces}")
    }
  }
}
