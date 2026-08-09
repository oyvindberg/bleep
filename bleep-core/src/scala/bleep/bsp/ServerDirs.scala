package bleep
package bsp

import bleep.internal.FileUtils
import cats.effect.unsafe.implicits.global

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.jdk.StreamConverters.StreamHasToScala

/** What a socket directory currently is.
  *
  * Every subdirectory gets a state and every state gets shown. A daemon that is wedged or dead is precisely what you opened `bleep server ls` to find, so
  * nothing here filters — the alternative is a listing that looks clean while 400MB of dead daemon sits on disk.
  */
sealed trait ServerState {
  def label: String
}

object ServerState {

  /** The socket accepted a connection. */
  case object Running extends ServerState { val label = "running" }

  /** A process is alive but its socket refuses connections: it stopped listening without dying. This is the row `kill` exists for. */
  case object Wedged extends ServerState { val label = "wedged" }

  /** A pid file names a process that no longer exists. */
  case class Dead(crashed: Boolean) extends ServerState {
    val label = if (crashed) "dead (crashed)" else "dead"
  }

  /** No pid, no socket, just files left behind — logs and metrics outliving the daemon that wrote them. */
  case object Litter extends ServerState { val label = "litter" }
}

/** One socket directory, as seen from the client without talking to the daemon.
  *
  * @param identity
  *   from `server.json`. `None` for directories created before that file existed — reported as unknown rather than hidden, because those daemons are exactly
  *   the ones a new client cannot ask.
  */
case class ServerDirInfo(
    socketDir: Path,
    hash: String,
    state: ServerState,
    pid: Option[Long],
    identity: Option[ServerJson],
    sizeBytes: Long
) {
  def bleepVersion: String = identity.map(_.bleepVersion).getOrElse("unknown (pre-M11 daemon)")
  def jvm: String = identity.map(id => s"${id.jvmName}:${id.jvmVersion}").getOrElse("unknown")
  def sizeMb: Long = sizeBytes / (1024L * 1024L)
  def isRunning: Boolean = state == ServerState.Running
}

object ServerDirs {

  /** Every socket directory under the cache, classified. Sorted running-first so the interesting rows are at the top. */
  def scan(userPaths: UserPaths): List[ServerDirInfo] = {
    val dirs =
      if (FileUtils.exists(userPaths.bspSocketDir)) Files.list(userPaths.bspSocketDir).toScala(List).filter(Files.isDirectory(_))
      else Nil

    dirs.map(classify).sortBy(info => (!info.isRunning, info.socketDir.getFileName.toString))
  }

  def classify(socketDir: Path): ServerDirInfo = {
    val address = BspRifleConfig.Address.DomainSocket(socketDir.resolve("socket"))
    val pid = readPid(socketDir)

    // Liveness is a connect probe, never the pid file: a pid file can name a process that died, and a live process can have stopped listening. Only the socket
    // answers the question a client actually cares about.
    val connects = BspServerOperations.check(address).unsafeRunSync()

    val state =
      if (connects) ServerState.Running
      else
        pid match {
          case Some(p) if ProcessHandle.of(p).isPresent => ServerState.Wedged
          case Some(_)                                  => ServerState.Dead(crashed = crashed(socketDir))
          case None                                     => ServerState.Litter
        }

    ServerDirInfo(
      socketDir = socketDir,
      hash = socketDir.getFileName.toString,
      state = state,
      pid = pid,
      identity = ServerJson.read(socketDir),
      sizeBytes = dirSizeBytes(socketDir)
    )
  }

  /** Resolve a user-supplied id: a pid, a socket-dir hash, or an unambiguous prefix of the hash. */
  def resolve(infos: List[ServerDirInfo], id: String): Either[String, ServerDirInfo] = {
    val byPid = infos.filter(_.pid.map(_.toString).contains(id))
    val byHash = infos.filter(_.hash == id)
    val byPrefix = infos.filter(_.hash.startsWith(id))

    (byPid ++ byHash).distinct match {
      case one :: Nil => Right(one)
      case Nil        =>
        byPrefix match {
          case one :: Nil => Right(one)
          case Nil        => Left(s"no compile server matches '$id'. Run 'bleep server ls' to see them.")
          case many       => Left(s"'$id' is ambiguous — matches ${many.map(_.hash).mkString(", ")}")
        }
      case many => Left(s"'$id' is ambiguous — matches ${many.map(_.hash).mkString(", ")}")
    }
  }

  private def readPid(socketDir: Path): Option[Long] = {
    val pidFile = socketDir.resolve("pid")
    if (!Files.exists(pidFile)) None
    else Files.readString(pidFile).trim.toLongOption
  }

  /** The daemon's last words. `output` is the current generation; a crash usually leaves its evidence in the rotated ones. */
  private def crashed(socketDir: Path): Boolean =
    List("output", "output.1", "output.2")
      .map(socketDir.resolve)
      .filter(Files.exists(_))
      .exists(BspServerOperations.containsOomMarker)

  private def dirSizeBytes(dir: Path): Long = {
    val stream = Files.walk(dir)
    try stream.iterator().asScala.filter(Files.isRegularFile(_)).map(Files.size).sum
    finally stream.close()
  }
}
