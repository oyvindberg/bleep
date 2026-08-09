package bleep
package commands
package server

import bleep.bsp.{BspRifleConfig, BspServerOperations, ServerAdminClient, ServerDirInfo, ServerDirs, ServerState}
import bleep.internal.FileUtils
import cats.effect.unsafe.implicits.global
import ryddig.Logger

import scala.concurrent.duration._

/** `bleep server kill` — stop compile servers.
  *
  * Graceful by default, through a ladder that only escalates when the gentler step does not work:
  *
  *   1. `bleep/shutdown` over the protocol, which lets the daemon reply, close its accept loop, release its lock, remove its pid and socket files and flush its
  *      metrics. The next client then gets a clean refusal and simply spawns a fresh daemon.
  *   1. `ProcessHandle.destroy` (SIGTERM on Unix) if it is still alive after the grace period.
  *   1. `destroyForcibly`, plus descendants.
  *
  * `--force` starts at the last rung and also deletes the socket directory, which is what `stop-all` has always done.
  *
  * On Windows `destroy` is already forcible, which is precisely why the protocol rung matters there: it is the only genuinely graceful stop available.
  */
case class ServerKill(
    logger: Logger,
    userPaths: UserPaths,
    ids: List[String],
    all: Boolean,
    force: Boolean,
    deleteDir: Boolean,
    /** Set when reached through an old command path, naming it so the warning can quote what the user actually typed. */
    deprecatedAlias: Option[String]
) extends BleepCommand {

  private val GracePeriod = 10.seconds

  override def run(): Either[BleepException, Unit] = {
    deprecatedAlias.foreach(old => logger.warn(s"deprecated: `$old` — use `bleep server stop-all` instead"))

    val infos = ServerDirs.scan(userPaths)

    select(infos).flatMap { targets =>
      if (targets.isEmpty) {
        logger.info("no compile servers to stop")
        Right(())
      } else {
        targets.foreach(kill)
        Right(())
      }
    }
  }

  private def select(infos: List[ServerDirInfo]): Either[BleepException, List[ServerDirInfo]] =
    if (all) Right(infos)
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
    else
      // No argument and exactly one server: the intent is unambiguous. More than one, and guessing would be the wrong kind of helpful.
      infos.filter(_.isRunning) match {
        case one :: Nil => Right(List(one))
        case Nil        => Right(Nil)
        case many       =>
          Left(new BleepException.Text(s"${many.size} servers are running — name one, or pass --all: ${many.map(_.hash).mkString(", ")}"))
      }

  private def kill(info: ServerDirInfo): Unit = {
    val sizeMb = info.sizeMb

    // A daemon that can answer knows its own pid; the file only records what the client that spawned it believed. They can differ, and then signalling the file's
    // pid hits nothing. Ask first when asking is possible.
    val target =
      if (!info.isRunning) info
      else
        ServerAdminClient.status(info.socketDir) match {
          case Right(status) => info.copy(pid = Some(status.pid))
          case Left(_)       => info
        }

    target.state match {
      case ServerState.Running if !force =>
        logger.info(s"asking ${target.hash} (pid ${target.pid.map(_.toString).getOrElse("?")}) to shut down")
        ServerAdminClient.shutdown(target.socketDir) match {
          case Right(()) => awaitExit(target)
          case Left(err) =>
            // An older daemon has no bleep/shutdown. Signals still work on it, so say what happened and drop to the next rung rather than giving up.
            logger.info(s"${target.hash}: ${err.message}")
            signalDown(target)
        }

      case ServerState.Running | ServerState.Wedged =>
        signalDown(target)

      case _ =>
        logger.info(s"${target.hash} is already ${target.state.label}")
    }

    if (deleteDir) removeDir(target, sizeMb)
  }

  /** Whether the daemon is still there, asked the only way that is trustworthy: by trying to connect.
    *
    * "Is that pid gone" and "did the server stop" are different questions, and on a real machine they disagree. A socket directory can hold a pid file naming a
    * process that exited long ago while a live daemon still serves the socket — signalling that pid is then a no-op, and reporting "stopped" is a guess.
    * Observed exactly that: kill claimed success and the very next `ls` showed the daemon still running.
    */
  private def stillServing(info: ServerDirInfo): Boolean =
    BspServerOperations.check(BspRifleConfig.Address.DomainSocket(info.socketDir.resolve("socket"))).unsafeRunSync()

  private def awaitStopped(info: ServerDirInfo): Boolean = {
    val deadline = System.currentTimeMillis() + GracePeriod.toMillis
    while (stillServing(info) && System.currentTimeMillis() < deadline)
      Thread.sleep(100)
    !stillServing(info)
  }

  /** Wait out the grace period, then escalate. A daemon mid-compile can take a moment to unwind. */
  private def awaitExit(info: ServerDirInfo): Unit =
    if (awaitStopped(info)) logger.info(s"${info.hash} stopped")
    else {
      logger.warn(s"${info.hash} did not stop within ${GracePeriod.toSeconds}s — escalating")
      signalDown(info)
    }

  private def signalDown(info: ServerDirInfo): Unit = {
    if (force) {
      logger.info(s"force-killing ${info.hash}")
      BspServerOperations.forceKillAndCleanup(info.socketDir).unsafeRunSync()
    } else
      info.pid match {
        case None      => logger.info(s"${info.hash} has no pid file — nothing to signal")
        case Some(pid) =>
          ProcessHandle.of(pid).ifPresent { handle =>
            handle.destroy(): Unit
          }
          if (!awaitStopped(info)) {
            logger.warn(s"${info.hash} is still serving after SIGTERM — forcing")
            BspServerOperations.forceKillAndCleanup(info.socketDir).unsafeRunSync()
          }
      }

    // Report what is true, not what was attempted.
    if (stillServing(info))
      logger.error(
        s"${info.hash} is STILL RUNNING — its pid file says ${info.pid.map(_.toString).getOrElse("nothing")}, which is not the process holding the socket. " +
          s"Find the real one with `lsof ${info.socketDir.resolve("socket")}` and stop it by hand."
      )
    else
      logger.info(s"${info.hash} stopped")
  }

  /** Deleting gigabytes without a word once cost a user the evidence for an OOM report — say what went away, and stand out when it was big. */
  private def removeDir(info: ServerDirInfo, sizeMb: Long): Unit = {
    // The process may still be releasing file handles; on Windows locked files surface as FileSystemException rather than DirectoryNotEmptyException.
    Thread.sleep(200)
    try FileUtils.deleteDirectory(info.socketDir)
    catch {
      case _: java.nio.file.DirectoryNotEmptyException | _: java.nio.file.FileSystemException =>
        Thread.sleep(2000)
        FileUtils.deleteDirectory(info.socketDir)
    }
    val msg = s"deleted ${info.socketDir} (${sizeMb}MB)"
    if (sizeMb >= 1024) logger.warn(msg) else logger.info(msg)
  }
}
