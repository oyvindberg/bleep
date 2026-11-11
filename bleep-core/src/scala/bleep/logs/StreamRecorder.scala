package bleep.logs

import java.io.{BufferedOutputStream, ByteArrayOutputStream, DataOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

/** Records one output stream of a running program into its run-log directory, as it arrives:
  *
  *   - `<stream>.log` — the bytes exactly as the program wrote them, colour codes included, so `cat` shows what a terminal would have. Colour is stripped only
  *     when the output is served to an agent.
  *   - `<stream>.idx` — one fixed-size entry per line: where the line ends in the log, when it arrived, and its [[LogShape]] pattern id. Line `n` is entry
  *     `n - 1`, so any line range is two seeks, and filtering by pattern never re-reads the log.
  *
  * The log stops growing at `maxBytes`; later output is counted as dropped rather than kept, so a runaway program cannot fill the disk. Both files are flushed
  * after every chunk, log before index, so a reader of a running program never sees an index entry pointing past the end of the log.
  *
  * Used from the stream's reader thread only, except [[close]] and the two counters the heartbeat reads.
  */
final class StreamRecorder(val stream: String, dir: Path, maxBytes: Long, silence: LogShape.Silence) {
  private val log = new BufferedOutputStream(Files.newOutputStream(dir.resolve(StreamRecorder.logFile(stream)), StandardOpenOption.CREATE_NEW), 1 << 16)
  private val index = new DataOutputStream(
    new BufferedOutputStream(Files.newOutputStream(dir.resolve(StreamRecorder.indexFile(stream)), StandardOpenOption.CREATE_NEW), 1 << 16)
  )
  private val shape = new LogShape.Builder(stream)
  private val pending = new ByteArrayOutputStream()

  private var written = 0L
  private var lineHasBytes = false
  private var capped = false
  private var midLineAtCap = false
  private var droppedLines = 0L
  private var droppedBytes = 0L
  private var closed = false

  @volatile private var lines = 0
  @volatile private var last: Option[String] = None

  def lineCount: Int = lines
  def lastLine: Option[String] = last

  /** Record a chunk read from the stream. Returns false once the recorder is closed: the run has been reported, so the reader should stop. */
  def consume(buf: Array[Byte], len: Int, atMs: Long): Boolean = synchronized {
    if (closed) false
    else {
      var i = 0
      while (i < len)
        if (capped) {
          var j = i
          while (j < len) {
            if (buf(j) == '\n') {
              if (midLineAtCap) midLineAtCap = false else droppedLines += 1
            }
            j += 1
          }
          droppedBytes += len - i
          i = len
        } else if (written >= maxBytes) {
          // The log is full. A line already started ends here, and the rest of it (up to its newline) is not counted again as a dropped line.
          capped = true
          midLineAtCap = lineHasBytes
          if (lineHasBytes) endLine(atMs)
        } else {
          val nl = indexOf(buf, '\n', i, len)
          val segmentEnd = if (nl >= 0) nl + 1 else len
          val take = math.min((segmentEnd - i).toLong, maxBytes - written).toInt
          log.write(buf, i, take)
          if (pending.size < StreamRecorder.MaxAnalyzedLineBytes) pending.write(buf, i, math.min(take, StreamRecorder.MaxAnalyzedLineBytes - pending.size))
          written += take
          lineHasBytes = true
          if (take == segmentEnd - i && nl >= 0) endLine(atMs)
          i += take
        }
      log.flush()
      index.flush()
      true
    }
  }

  /** The stream ended: a last line without a trailing newline is still a line. */
  def endOfInput(atMs: Long): Unit = synchronized {
    if (!closed && lineHasBytes) {
      endLine(atMs)
      log.flush()
      index.flush()
    }
  }

  /** Stop recording and summarize. Output arriving after this — from a grandchild still holding the pipe after the program itself ended — is not recorded. */
  def close(): StreamRecorder.Result = synchronized {
    if (!closed) {
      closed = true
      log.close()
      index.close()
    }
    StreamRecorder.Result(shape.result(), written, droppedLines, droppedBytes)
  }

  private def endLine(atMs: Long): Unit = {
    val raw = new String(pending.toByteArray, StandardCharsets.UTF_8)
    val text = stripAnsi(raw.stripSuffix("\n").stripSuffix("\r"))
    pending.reset()
    lineHasBytes = false
    val n = lines + 1
    val patternId = shape.add(text)
    index.writeLong(written)
    index.writeLong(atMs)
    index.writeInt(patternId)
    silence.onLine(stream, n, atMs)
    last = Some(text)
    lines = n
  }

  private def indexOf(buf: Array[Byte], b: Char, from: Int, until: Int): Int = {
    var j = from
    while (j < until && buf(j) != b) j += 1
    if (j < until) j else -1
  }
}

object StreamRecorder {

  /** Bytes per index entry: end offset (long), arrival time in ms (long), pattern id (int). */
  val IndexEntryBytes = 20

  /** How much of one line is decoded for the shape and the heartbeat. The log keeps the whole line. */
  val MaxAnalyzedLineBytes = 8192

  def logFile(stream: String): String = s"$stream.log"
  def indexFile(stream: String): String = s"$stream.idx"

  case class Result(shape: LogShape.StreamShape, bytes: Long, droppedLines: Long, droppedBytes: Long)
}
