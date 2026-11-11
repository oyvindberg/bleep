package bleep.logs

import bleep.BleepException

import java.io.RandomAccessFile
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** Serves lines of a recorded stream by line number, for `bleep.run.output` and `bleep history`. Works on a run that is still going: it reads only index
  * entries already flushed, and every one of those points inside the flushed log (see [[StreamRecorder]]).
  *
  * Lines are served without colour codes and cut at [[MaxLineChars]]; the log on disk keeps them as the program wrote them.
  */
object RunLogReader {

  /** At most this many lines per call; `nextFrom` says where to continue. */
  val MaxLinesPerCall = 200

  /** Lines served when the caller gives no range: the end of the stream. */
  val DefaultTailLines = 20

  val MaxLineChars = 2000

  case class Page(totalLines: Int, from: Int, to: Int, lines: List[(Int, String)], nextFrom: Option[Int])

  /** Lines `from` to `to` (1-based, inclusive; negative counts from the end, -1 being the last line). With `pattern`, only that pattern's lines in the range.
    */
  def read(runDir: Path, stream: String, from: Option[Int], to: Option[Int], pattern: Option[Int]): Page = {
    val logPath = runDir.resolve(StreamRecorder.logFile(stream))
    val indexPath = runDir.resolve(StreamRecorder.indexFile(stream))
    if (!Files.isRegularFile(indexPath) || !Files.isRegularFile(logPath))
      throw new BleepException.Text(s"no $stream recorded in $runDir: the program never started")

    val index = new RandomAccessFile(indexPath.toFile, "r")
    val log = new RandomAccessFile(logPath.toFile, "r")
    try {
      val total = (index.length() / StreamRecorder.IndexEntryBytes).toInt
      def resolve(n: Int): Int = if (n < 0) total + 1 + n else n
      val first = math.max(1, from.map(resolve).getOrElse(if (pattern.isDefined) 1 else total - DefaultTailLines + 1))
      val last = math.min(total, to.map(resolve).getOrElse(total))

      def entry(n: Int): (Long, Int) = {
        index.seek((n - 1).toLong * StreamRecorder.IndexEntryBytes)
        val end = index.readLong()
        index.readLong() // arrival time: used by the shape, not by paging
        (end, index.readInt())
      }
      def startOf(n: Int): Long = if (n == 1) 0L else entry(n - 1)._1

      def text(n: Int, end: Long): String = {
        val start = startOf(n)
        val len = math.min(end - start, (MaxLineChars * 4).toLong).toInt
        val bytes = new Array[Byte](len)
        log.seek(start)
        log.readFully(bytes)
        abbreviate(stripAnsi(new String(bytes, StandardCharsets.UTF_8).stripSuffix("\n").stripSuffix("\r")), MaxLineChars)
      }

      val lines = List.newBuilder[(Int, String)]
      var count = 0
      var n = first
      var nextFrom: Option[Int] = None
      while (n <= last && nextFrom.isEmpty) {
        val (end, patternId) = entry(n)
        if (pattern.forall(_ == patternId)) {
          if (count == MaxLinesPerCall) nextFrom = Some(n)
          else {
            lines += n -> text(n, end)
            count += 1
          }
        }
        n += 1
      }
      Page(total, first, if (nextFrom.isDefined) nextFrom.get - 1 else last, lines.result(), nextFrom)
    } finally {
      index.close()
      log.close()
    }
  }
}
