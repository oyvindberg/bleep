package bleep.history

import bleep.{BleepException, BuildPaths}
import bleep.bsp.protocol.BleepBspProtocol
import io.circe.{Codec, Decoder, Encoder}
import io.circe.generic.semiauto.deriveCodec
import io.circe.syntax.EncoderOps

import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption, StandardOpenOption}
import scala.jdk.CollectionConverters.*

/** The record of one completed compile/test request: everything the daemon streamed while running it, plus enough header to interpret it anywhere.
  *
  * A transcript describes its own run, never current state — that is what makes it immune to staleness, safe to persist, and safe to diff against any other
  * transcript, including one from a different worktree. Paths inside `events` are stored ABSOLUTE, exactly as the compiler reported them: transcripts are
  * grepped and their paths clicked, and absolute + [[workspace]] can always derive relative while the reverse loses paths outside the workspace. Only
  * [[TranscriptDiff]]'s identity computation relativizes, ephemerally.
  */
case class Transcript(
    id: Long,
    timestampMs: Long,
    workspace: String,
    variant: String,
    mode: String, // "compile" or "test"
    targets: List[String],
    client: String, // which kind of client ran it: "cli", "mcp", an IDE name — display metadata only
    events: List[BleepBspProtocol.Event],
    testRunResult: Option[BleepBspProtocol.TestRunResult]
)

object Transcript {
  implicit val codec: Codec[Transcript] = deriveCodec
}

/** Per-workspace, per-variant transcript files: `<workspace>/.bleep/builds/<variant>/history/<id>.json`.
  *
  * The daemon writes; everyone reads. Ids are monotonically increasing per workspace+variant and never reused — eviction leaves gaps. Writes are atomic (tmp +
  * rename) so a reader never sees a torn file, and id assignment happens under a file lock so two daemons racing on one workspace cannot mint the same id.
  * Reads are lock-free.
  *
  * Retention is bounded twice over: at most [[MaxEntries]] transcripts and at most [[MaxTotalBytes]] on disk, oldest deleted first on every write. Noop
  * transcripts are kept deliberately — "everything up-to-date" is exactly what a copy-state verification diff wants to see.
  */
object TranscriptStore {
  val MaxEntries = 32
  val MaxTotalBytes: Long = 64L * 1024 * 1024

  private val FileName = raw"(\d+)\.json".r

  def dir(buildPaths: BuildPaths): Path = buildPaths.historyDir

  /** `Files.list` holds an open directory handle until closed — materialize inside try/finally, never leak the stream. */
  private def listEntries(d: Path): List[(Long, Path)] =
    if (!Files.isDirectory(d)) Nil
    else {
      val stream = Files.list(d)
      try
        stream
          .iterator()
          .asScala
          .flatMap(p => FileName.findFirstMatchIn(p.getFileName.toString).map(m => (m.group(1).toLong, p)))
          .toList
          .sortBy(_._1)
      finally stream.close()
    }

  def list(buildPaths: BuildPaths): List[Long] = listEntries(dir(buildPaths)).map(_._1)

  def latestId(buildPaths: BuildPaths): Option[Long] = list(buildPaths).lastOption

  def read(buildPaths: BuildPaths, id: Long): Transcript = {
    val file = dir(buildPaths).resolve(s"$id.json")
    if (!Files.isRegularFile(file))
      throw new BleepException.Text(s"No history entry #$id in ${dir(buildPaths)}. Kept: last $MaxEntries entries per workspace.")
    io.circe.parser.decode[Transcript](new String(Files.readAllBytes(file), StandardCharsets.UTF_8)) match {
      case Right(t)  => t
      case Left(err) => throw new BleepException.Text(s"Could not parse transcript $file: ${err.getMessage}")
    }
  }

  def readLatest(buildPaths: BuildPaths): Transcript =
    latestId(buildPaths) match {
      case Some(id) => read(buildPaths, id)
      case None     => throw new BleepException.Text(s"No history recorded in ${dir(buildPaths)}. Run a compile or test first.")
    }

  /** Assign the next id and persist the transcript atomically. Returns the stored transcript, its id filled in. */
  def write(
      buildPaths: BuildPaths,
      timestampMs: Long,
      mode: String,
      targets: List[String],
      client: String,
      events: List[BleepBspProtocol.Event],
      testRunResult: Option[BleepBspProtocol.TestRunResult]
  ): Transcript = {
    val d = dir(buildPaths)
    Files.createDirectories(d)
    withLock(d) {
      val id = list(buildPaths).lastOption.getOrElse(0L) + 1L
      val transcript = Transcript(
        id = id,
        timestampMs = timestampMs,
        workspace = buildPaths.buildDir.toString,
        variant = buildPaths.variant.name,
        mode = mode,
        targets = targets,
        client = client,
        events = events,
        testRunResult = testRunResult
      )
      val tmp = Files.createTempFile(d, s".tmp-$id-", ".part")
      Files.write(tmp, transcript.asJson.noSpaces.getBytes(StandardCharsets.UTF_8))
      Files.move(tmp, d.resolve(s"$id.json"), StandardCopyOption.ATOMIC_MOVE)
      applyRetention(d)
      transcript
    }
  }

  /** Delete oldest transcripts beyond [[MaxEntries]] or once cumulative size exceeds [[MaxTotalBytes]]. Called with the lock held. */
  private def applyRetention(d: Path): Unit = {
    val byIdDesc: List[(Long, Path)] = listEntries(d).reverse

    var kept = 0
    var bytes = 0L
    byIdDesc.foreach { case (_, p) =>
      val size = Files.size(p)
      if (kept < MaxEntries && bytes + size <= MaxTotalBytes) {
        kept += 1
        bytes += size
      } else {
        Files.deleteIfExists(p): Unit
      }
    }
  }

  /** Exclusive advisory lock on `<history>/.lock`, held only while assigning an id + renaming. Readers never take it. */
  private def withLock[A](d: Path)(body: => A): A = {
    val channel = FileChannel.open(d.resolve(".lock"), StandardOpenOption.CREATE, StandardOpenOption.WRITE)
    try {
      val lock = channel.lock()
      try body
      finally lock.release()
    } finally channel.close()
  }
}
