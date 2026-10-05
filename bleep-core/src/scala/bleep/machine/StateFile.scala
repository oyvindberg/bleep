package bleep.machine

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption, StandardOpenOption}
import scala.jdk.StreamConverters.StreamHasToScala

/** `state.json`: one server's live scheduler state, in its socket directory next to `server.json` (design §6).
  *
  * Single writer per file, written as `state.json.tmp` + atomic rename in the same directory: a reader sees the previous version or the new one, never half of
  * either, and a crash mid-write leaves the previous version. Readers never lock; `bleep server top` reads the files the same way the scheduler does.
  *
  * A file is live iff the process it names exists and started when the file says it did (`ProcessHandle.Info.startInstant`). Anything else — the daemon exited,
  * the pid was reused — is ignored by the scheduler and removed by `bleep server prune`.
  */
object StateFile {
  val FileName = "state.json"
  private val TmpFileName = "state.json.tmp"

  def file(socketDir: Path): Path = socketDir.resolve(FileName)

  /** Who this process is, for the state it publishes. Throws if the JDK cannot report our own start instant: nothing could verify our liveness, so nothing we
    * publish would ever be counted.
    */
  def selfIdentity(bleepVersion: String): ServerIdentity = {
    val self = ProcessHandle.current()
    val start = self.info().startInstant()
    if (!start.isPresent)
      throw new IllegalStateException(s"the JDK reports no start instant for this process (pid ${self.pid()}); state.json cannot be verified")
    ServerIdentity(pid = self.pid(), startedAtEpochMs = start.get().toEpochMilli, bleepVersion = bleepVersion)
  }

  def write(socketDir: Path, state: StateJson): Unit = {
    val tmp = socketDir.resolve(TmpFileName)
    Files.write(
      tmp,
      state.asJson.noSpaces.getBytes(StandardCharsets.UTF_8),
      StandardOpenOption.CREATE,
      StandardOpenOption.TRUNCATE_EXISTING,
      StandardOpenOption.WRITE
    )
    Files.move(tmp, file(socketDir), StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING): Unit
  }

  /** `None` only when the file is absent — a server from before the scheduler, or one that has not ticked yet. Any other failure throws: a socket directory we
    * cannot make sense of is a bug to surface, not a row to drop.
    */
  def read(socketDir: Path): Option[StateJson] = {
    val path = file(socketDir)
    if (!Files.exists(path)) None
    else {
      val content = Files.readString(path, StandardCharsets.UTF_8)
      Some(
        io.circe.parser.decode[StateJson](content) match {
          case Right(value) => value
          case Left(err)    => throw new IllegalStateException(s"$path: ${err.getMessage}", err)
        }
      )
    }
  }

  /** Whether the process behind a state file is the one that wrote it. */
  def isLive(state: StateJson): Boolean = {
    val handle = ProcessHandle.of(state.pid)
    handle.isPresent && {
      val start = handle.get().info().startInstant()
      start.isPresent && start.get().toEpochMilli == state.startedAtEpochMs
    }
  }

  /** Every other live server's state, by the socket-directory layout `ServerDirs` uses: one subdirectory of `bspSocketDir` per server.
    *
    * Decision: not `ServerDirs.scan`, which connects to every socket and walks every directory for its size — milliseconds to seconds, where a claiming tick
    * has microseconds. Only the layout is shared.
    *
    * @param self
    *   excluded by pid and start instant, not by directory: a restarted server overwrites its predecessor's file, but until its first write that file names a
    *   dead process and is dropped by liveness like any other.
    */
  def discoverOthers(bspSocketDir: Path, self: ServerIdentity): List[StateJson] =
    readOthers(listSocketDirs(bspSocketDir), self)

  /** The socket directories as they are on disk now. */
  def listSocketDirs(bspSocketDir: Path): List[Path] =
    if (!Files.isDirectory(bspSocketDir)) Nil
    else {
      val stream = Files.list(bspSocketDir)
      try stream.toScala(List).filter(Files.isDirectory(_))
      finally stream.close()
    }

  /** The live state behind each of `dirs`, fresh from disk, this server left out. */
  def readOthers(dirs: List[Path], self: ServerIdentity): List[StateJson] =
    dirs
      .flatMap(read)
      .filterNot(s => s.pid == self.pid && s.startedAtEpochMs == self.startedAtEpochMs)
      .filter(isLive)

  // ---- json. Hand-written rather than derived: the file is read by every later bleep version, so its spelling is a contract, not a reflection of case
  // class field order.

  implicit val forkEncoder: Encoder[StateFork] = Encoder.instance { f =>
    val base = List(
      "id" -> Json.fromLong(f.id),
      "kind" -> Json.fromString(f.kind.json),
      "boundMb" -> Json.fromLong(f.boundMb),
      "startedAtEpochMs" -> Json.fromLong(f.startedAtEpochMs),
      "pids" -> Json.arr(f.pids.map(Json.fromLong)*)
    )
    val pid = Nil
    val state = f.state match {
      case StateForkState.Starting            => List("state" -> Json.fromString("starting"))
      case StateForkState.Measured(footprint) => List("state" -> Json.fromString("measured"), "footprintMb" -> Json.fromLong(footprint))
    }
    Json.obj((base ++ pid ++ state)*)
  }

  implicit val forkDecoder: Decoder[StateFork] = Decoder.instance { c =>
    for {
      id <- c.get[Long]("id")
      pids <- c.get[List[Long]]("pids")
      kind <- c.get[String]("kind").flatMap(s => ForkKind.all.find(_.json == s).toRight(io.circe.DecodingFailure(s"unknown fork kind '$s'", c.history)))
      boundMb <- c.get[Long]("boundMb")
      startedAt <- c.get[Long]("startedAtEpochMs")
      state <- c.get[String]("state").flatMap {
        case "starting" => Right(StateForkState.Starting: StateForkState)
        case "measured" => c.get[Long]("footprintMb").map(footprint => StateForkState.Measured(footprint): StateForkState)
        case other      => Left(io.circe.DecodingFailure(s"unknown fork state '$other'", c.history))
      }
    } yield StateFork(id, pids, kind, boundMb, state, startedAt)
  }

  implicit val stateEncoder: Encoder[StateJson] = Encoder.instance { s =>
    val fields = List(
      "version" -> Json.fromInt(s.version),
      "pid" -> Json.fromLong(s.pid),
      "startedAtEpochMs" -> Json.fromLong(s.startedAtEpochMs),
      "bleepVersion" -> Json.fromString(s.bleepVersion),
      "updatedAtEpochMs" -> Json.fromLong(s.updatedAtEpochMs),
      "requests" -> Json.fromInt(s.requests),
      "cpuInUse" -> Json.fromInt(s.cpuInUse),
      "wantsMore" -> Json.fromBoolean(s.wantsMore),
      "shuttingDown" -> Json.fromBoolean(s.shuttingDown),
      "forks" -> s.forks.asJson
    )
    // Only while idle, and last: a busy server's document is exactly §6.2's.
    Json.obj((fields ++ s.idleSinceEpochMs.map(since => "idleSinceEpochMs" -> Json.fromLong(since)))*)
  }

  /** Version 1 is read whole. A version this bleep does not know is read as the fields every version must keep — `pid`, `startedAtEpochMs` (liveness), `forks`,
    * `cpuInUse` — and a document without those fails. The fields it cannot know are marked as such rather than guessed.
    */
  implicit val stateDecoder: Decoder[StateJson] = Decoder.instance { c =>
    c.get[Int]("version").flatMap {
      case StateJson.CurrentVersion => decodeV1(c)
      case other                    =>
        for {
          pid <- c.get[Long]("pid")
          startedAt <- c.get[Long]("startedAtEpochMs")
          cpuInUse <- c.get[Int]("cpuInUse")
          forks <- c.get[List[StateFork]]("forks")
        } yield StateJson(
          version = other,
          pid = pid,
          startedAtEpochMs = startedAt,
          bleepVersion = s"unknown (state.json version $other)",
          updatedAtEpochMs = 0L,
          requests = 0,
          cpuInUse = cpuInUse,
          wantsMore = false,
          shuttingDown = false,
          forks = forks,
          idleSinceEpochMs = None
        )
    }
  }

  private def decodeV1(c: HCursor): Decoder.Result[StateJson] =
    for {
      pid <- c.get[Long]("pid")
      startedAt <- c.get[Long]("startedAtEpochMs")
      bleepVersion <- c.get[String]("bleepVersion")
      updatedAt <- c.get[Long]("updatedAtEpochMs")
      requests <- c.get[Int]("requests")
      cpuInUse <- c.get[Int]("cpuInUse")
      wantsMore <- c.get[Boolean]("wantsMore")
      shuttingDown <- c.get[Boolean]("shuttingDown")
      forks <- c.get[List[StateFork]]("forks")
      idleSince <- c.get[Option[Long]]("idleSinceEpochMs")
    } yield StateJson(StateJson.CurrentVersion, pid, startedAt, bleepVersion, updatedAt, requests, cpuInUse, wantsMore, shuttingDown, forks, idleSince)
}

/** [[StateFile.discoverOthers]] for the tick: the directory listing is cached for `listingTtlMs`, every known server's `state.json` is read fresh on every
  * call. The listing changes only when a server is spawned or pruned, and a new server takes longer than a second to be anything but idle — while the state
  * files are exactly the data the lock serialises, so they are never stale.
  *
  * @param clock
  *   epoch milliseconds, injected so a test can move it
  */
final class ServerDiscovery(bspSocketDir: Path, self: ServerIdentity, clock: () => Long, listingTtlMs: Long) {
  require(listingTtlMs >= 0L, s"listingTtlMs $listingTtlMs is negative")
  private var listing: Option[(Long, List[Path])] = None

  def others(): List[StateJson] = {
    val now = clock()
    val dirs = listing match {
      case Some((listedAt, dirs)) if now - listedAt < listingTtlMs => dirs
      case _                                                       =>
        val fresh = StateFile.listSocketDirs(bspSocketDir)
        listing = Some((now, fresh))
        fresh
    }
    StateFile.readOthers(dirs, self)
  }
}
