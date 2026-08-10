package bleep
package bsp

import bleep.internal.FileUtils
import cats.effect.unsafe.implicits.global

import java.nio.file.{Files, Path, Paths}
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

  /** A compile server process with no socket directory left to find it by.
    *
    * @param socketDir
    *   the directory it was started with, from its own command line — gone from disk, or it would have been listed normally.
    */
  case class OrphanDaemon(pid: Long, socketDir: String, rssMb: Option[Long])

  /** Compile servers that outlived their socket directory.
    *
    * A daemon is normally found by scanning those directories, which makes one whose directory has been deleted invisible — while it goes on holding a
    * multi-gigabyte heap. That is not hypothetical: a force-stop deletes the directory after killing, and a process that survives the kill is orphaned by its
    * own cleanup. One was found two days old, holding 4.1GB, which nothing on the machine could name.
    *
    * Found by asking the OS for processes whose command line runs the daemon main class, and keeping the ones whose `--socket` directory no longer exists.
    */
  def orphanDaemons(known: List[ServerDirInfo]): List[OrphanDaemon] = {
    val knownDirs = known.map(_.socketDir.toString).toSet

    ProcessHandle
      .allProcesses()
      .iterator()
      .asScala
      .toList
      .flatMap { handle =>
        val commandLine = handle.info().commandLine()
        if (!commandLine.isPresent || !commandLine.get().contains(BspRifleConfig.ServerMainClass)) None
        else
          socketDirOf(commandLine.get()) match {
            case Some(dir) if !knownDirs.contains(dir) || !Files.exists(Paths.get(dir)) => Some(OrphanDaemon(handle.pid(), dir, None))
            case Some(_)                                                                => None
            // No `--socket` at all is stranger still, and worth showing rather than dropping.
            case None => Some(OrphanDaemon(handle.pid(), "unknown", None))
          }
      }
  }

  /** The `--socket` argument out of a daemon's command line. Separated out because the command line is the only handle on an orphan, and parsing it is the part
    * worth testing without needing a real orphaned process to hand.
    */
  private[bsp] def socketDirOf(commandLine: String): Option[String] =
    commandLine.split(' ').sliding(2).collectFirst { case Array("--socket", dir) => dir }

  /** The daemon serving a given workspace, if one is.
    *
    * Answered from what each daemon reports it is serving rather than by re-deriving its JVM-key hash: the daemon's own account is the truth, and it stays
    * right even when config on disk has drifted since it started. Costs one round trip per running daemon, which is why callers that already hold a status
    * should test against that instead.
    *
    * This is what makes `bleep server log`, `status` and `kill` default to *your* server rather than refusing to choose among several.
    */
  def servingWorkspace(infos: List[ServerDirInfo], workspace: Path): Option[ServerDirInfo] = {
    val wanted = workspace.toAbsolutePath.normalize().toString
    infos.filter(_.isRunning).find { info =>
      ServerAdminClient.status(info.socketDir) match {
        case Right(status) => status.workspaces.exists(_.path == wanted)
        case Left(_)       => false
      }
    }
  }

  /** Which of the servers holding this workspace is the one *this* build would actually talk to.
    *
    * More than one can have the same workspace loaded — every bleep version you have run here leaves a server that has seen it — but only one of them is yours,
    * and marking several "this build" is worse than marking none: it is the one label a reader trusts to decide which server to kill.
    *
    * The socket directory is named after a hash that includes the client's own bleep version, so a server recorded with a different version is definitionally
    * not the one this client connects to. Where that does not decide it, the first candidate wins rather than several being marked.
    *
    * @param candidates
    *   hash and recorded bleep version, for the servers that report holding the workspace, in display order.
    */
  def currentAmong(candidates: List[(String, Option[String])], clientVersion: String): Option[String] =
    candidates.collectFirst { case (hash, Some(version)) if version == clientVersion => hash }.orElse(candidates.headOption.map(_._1))

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
