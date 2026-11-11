package bleep.mcp

import bleep.{BleepException, BuildPaths}
import bleep.history.DirectoryLock
import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.EncoderOps

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.jdk.CollectionConverters.*

/** One program run through `bleep.run` or `bleep.sourcegen`, as recorded in `run.json`. Written when the run starts and again when it ends.
  *
  * `script` is the `scripts:` entry the program was started as, if it was started by name as a script rather than as a project. `sourcegenFor` is non-empty for
  * a sourcegen script run by `bleep.sourcegen`: the projects it generated sources for.
  *
  * `ownerPid` is the MCP server that ran it. A run left in state `running` by a server that has since died is reported as `abandoned` by [[RunLogStore.read]] —
  * the file cannot be finished by a process that no longer exists, and must not claim to still be running.
  */
case class RunRecord(
    id: Long,
    workspace: String,
    project: String,
    script: Option[String],
    sourcegenFor: List[String],
    mainClass: String,
    args: List[String],
    command: String,
    timeoutSeconds: Int,
    ownerPid: Long,
    pid: Option[Long],
    startedAtMs: Long,
    state: String, // RunState.Running / Exited / TimedOut / Cancelled / Failed, and Abandoned when read back
    exitCode: Option[Int],
    durationMs: Option[Long]
)

object RunRecord {
  implicit val codec: Codec[RunRecord] = deriveCodec
}

object RunState {
  val Running = "running"
  val Exited = "exited"
  val TimedOut = "timedOut"
  val Cancelled = "cancelled"
  val Failed = "failed"
  val Abandoned = "abandoned"
}

/** Per-workspace run logs: `<workspace>/.bleep/builds/<variant>/run-logs/<id>/`, holding `run.json` and the program's output (see
  * [[bleep.logs.StreamRecorder]]).
  *
  * Only the MCP server writes here: a run gets its id under [[DirectoryLock]] before the program starts, so its output can be read while it is still running.
  * Ids increase per workspace+variant and are never reused. Retention is its own — at most [[MaxRuns]] runs and [[MaxTotalBytes]] on disk, oldest first — so
  * programs run by agents never push compile/test transcripts out of history. A run that is still going is never evicted.
  */
object RunLogStore {
  val MaxRuns = 64
  val MaxTotalBytes: Long = 256L * 1024 * 1024
  val RecordFile = "run.json"

  private val DirName = raw"(\d+)".r

  def dir(buildPaths: BuildPaths): Path = buildPaths.runLogsDir

  def runDir(buildPaths: BuildPaths, id: Long): Path = dir(buildPaths).resolve(id.toString)

  /** Every run id on disk, ascending. */
  def list(buildPaths: BuildPaths): List[Long] = {
    val d = dir(buildPaths)
    if (!Files.isDirectory(d)) Nil
    else {
      val stream = Files.list(d)
      try
        stream
          .iterator()
          .asScala
          .filter(Files.isDirectory(_))
          .flatMap(p => p.getFileName.toString match { case DirName(id) => Some(id.toLong); case _ => None })
          .toList
          .sorted
      finally stream.close()
    }
  }

  /** Mint the next id, create its directory and write the initial record (state running), then apply retention. The program has not started yet. */
  def create(
      buildPaths: BuildPaths,
      project: String,
      script: Option[String],
      sourcegenFor: List[String],
      mainClass: String,
      args: List[String],
      command: String,
      timeoutSeconds: Int,
      startedAtMs: Long
  ): RunRecord = {
    val d = dir(buildPaths)
    Files.createDirectories(d)
    DirectoryLock.withLock(d) {
      val id = list(buildPaths).lastOption.getOrElse(0L) + 1L
      Files.createDirectory(runDir(buildPaths, id))
      val record = RunRecord(
        id = id,
        workspace = buildPaths.buildDir.toString,
        project = project,
        script = script,
        sourcegenFor = sourcegenFor,
        mainClass = mainClass,
        args = args,
        command = command,
        timeoutSeconds = timeoutSeconds,
        ownerPid = ProcessHandle.current().pid(),
        pid = None,
        startedAtMs = startedAtMs,
        state = RunState.Running,
        exitCode = None,
        durationMs = None
      )
      writeRecord(buildPaths, record)
      applyRetention(buildPaths)
      record
    }
  }

  /** Replace a run's record atomically. Only the run's own writer calls this, so it needs no lock; it fails loudly if retention already removed the run. */
  def update(buildPaths: BuildPaths, record: RunRecord): Unit = {
    if (!Files.isDirectory(runDir(buildPaths, record.id)))
      throw new BleepException.Text(s"run #${record.id} no longer exists in ${dir(buildPaths)}")
    writeRecord(buildPaths, record)
  }

  /** The record as written, except that a run whose owning server has died reads as [[RunState.Abandoned]]. */
  def read(buildPaths: BuildPaths, id: Long): RunRecord = {
    val file = runDir(buildPaths, id).resolve(RecordFile)
    if (!Files.isRegularFile(file))
      throw new BleepException.Text(s"No run #$id in ${dir(buildPaths)}. Kept: the last $MaxRuns runs per workspace.")
    val record = io.circe.parser.decode[RunRecord](new String(Files.readAllBytes(file), StandardCharsets.UTF_8)) match {
      case Right(r)  => r
      case Left(err) => throw new BleepException.Text(s"Could not parse run record $file: ${err.getMessage}")
    }
    if (record.state == RunState.Running && !isAlive(record.ownerPid)) record.copy(state = RunState.Abandoned) else record
  }

  def readLatest(buildPaths: BuildPaths): RunRecord =
    list(buildPaths).lastOption match {
      case Some(id) => read(buildPaths, id)
      case None     => throw new BleepException.Text(s"No runs recorded in ${dir(buildPaths)}. Run a program with bleep.run first.")
    }

  private def isAlive(pid: Long): Boolean = ProcessHandle.of(pid).map[Boolean](_.isAlive).orElse(false)

  private def writeRecord(buildPaths: BuildPaths, record: RunRecord): Unit = {
    val d = runDir(buildPaths, record.id)
    val tmp = Files.createTempFile(d, ".run-", ".part")
    Files.write(tmp, record.asJson.noSpaces.getBytes(StandardCharsets.UTF_8))
    Files.move(tmp, d.resolve(RecordFile), StandardCopyOption.ATOMIC_MOVE): Unit
  }

  /** Delete the oldest runs beyond [[MaxRuns]] or once their cumulative size exceeds [[MaxTotalBytes]]. A running run is kept and counted. Called with the lock
    * held.
    */
  private def applyRetention(buildPaths: BuildPaths): Unit = {
    var kept = 0
    var bytes = 0L
    list(buildPaths).reverse.foreach { id =>
      val d = runDir(buildPaths, id)
      val size = sizeOf(d)
      // A directory without a record is what a crash between creating the directory and writing its record leaves: not running, so evictable.
      val running = Files.isRegularFile(d.resolve(RecordFile)) && read(buildPaths, id).state == RunState.Running
      if (running || (kept < MaxRuns && bytes + size <= MaxTotalBytes)) {
        kept += 1
        bytes += size
      } else deleteRecursively(d)
    }
  }

  private def sizeOf(d: Path): Long = {
    val stream = Files.list(d)
    try stream.iterator().asScala.map(Files.size).sum
    finally stream.close()
  }

  private def deleteRecursively(d: Path): Unit = {
    val stream = Files.list(d)
    try stream.iterator().asScala.foreach(Files.delete)
    finally stream.close()
    Files.delete(d)
  }
}
