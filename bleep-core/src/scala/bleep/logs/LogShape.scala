package bleep.logs

import io.circe.Json

import scala.collection.mutable

/** What a program's output looks like, in a few hundred characters however much it printed: the line patterns it repeats, the few lines it printed only once or
  * twice, and when it went quiet.
  *
  * The raw output used to go into `bleep.run`'s result, and it was most of what bleep cost an agent's context: one source generator printed 4,000
  * near-identical warnings per run. The agent does not need those lines; it needs to know they are there, how many, where, and which few lines are different.
  * Every number in the shape is a line number or a pattern id the agent can page to with `bleep.run.output`.
  */
object LogShape {

  /** A line's pattern is its first [[KeyTokens]] whitespace tokens after [[mask]]. */
  val KeyTokens = 6

  /** Distinct patterns tracked per stream; lines of any further pattern are counted under pattern 0, "other". */
  val MaxPatterns = 500

  /** A stream this short is returned verbatim instead of summarized: its shape would be longer than the lines. */
  val InlineLines = 10

  /** Patterns listed per stream, most frequent first. Only patterns seen at least [[MinPatternCount]] times are listed: rarer ones are shown as rare lines. */
  val ShownPatterns = 5
  val MinPatternCount = 3

  /** Rare lines listed per run, across both streams. */
  val RareLines = 5

  val MaxPatternChars = 100
  val MaxShownLineChars = 200

  private val Quoted = "\"[^\"]*\"".r
  private val Uuid = "\\b[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\\b".r
  private val Hex = "\\b(?:0x[0-9a-fA-F]+|(?=[0-9a-fA-F]*[0-9])(?=[0-9a-fA-F]*[a-fA-F])[0-9a-fA-F]{8,})\\b".r
  private val Number = "\\d+(?:[.,:]\\d+)*".r
  private val RootedPath = "(?:[A-Za-z]:)?[\\\\/][\\w.@+#~-]+(?:[\\\\/][\\w.@+#~-]*)*".r
  private val RelativePath = "[\\w.@+#~-]*[A-Za-z][\\w.@+#~-]*(?:[\\\\/][\\w.@+#~-]+)+".r
  private val ErrorWords = "(?i)(error|exception|fatal|fail|panic|caused by)".r

  /** The parts of a line that vary between otherwise identical messages, replaced by placeholders: quoted strings, UUIDs and hashes, numbers, paths. */
  def mask(line: String): String = {
    val q = Quoted.replaceAllIn(line, "\"…\"")
    val u = Uuid.replaceAllIn(q, "#")
    val h = Hex.replaceAllIn(u, "#")
    val n = Number.replaceAllIn(h, "#")
    val r = RootedPath.replaceAllIn(n, "<path>")
    RelativePath.replaceAllIn(r, "<path>")
  }

  /** The pattern a line belongs to. */
  def key(line: String): String = {
    val tokens = mask(line).split("\\s+").iterator.filter(_.nonEmpty).take(KeyTokens + 1).toList
    if (tokens.isEmpty) "(blank)"
    else if (tokens.sizeIs > KeyTokens) tokens.take(KeyTokens).mkString(" ") + " …"
    else tokens.mkString(" ")
  }

  def isErrorLike(line: String): Boolean = ErrorWords.findFirstIn(line).isDefined

  case class Pattern(id: Int, text: String, count: Int, firstLine: Int, lastLine: Int)

  /** A line quoted in full, with where it was. */
  case class Quote(stream: String, line: Int, text: String)

  /** The finished shape of one stream. `inline` is the whole stream when it had at most [[InlineLines]] lines. */
  case class StreamShape(
      stream: String,
      lines: Int,
      inline: Option[List[String]],
      patterns: List[Pattern],
      patternsNotShown: Int,
      linesInOtherPattern: Int,
      rareCandidates: List[Quote]
  )

  /** Accumulates one stream's shape line by line. Used from that stream's reader thread only. */
  final class Builder(stream: String) {
    private final class Stats(val id: Int, val text: String, val firstLine: Int) {
      var count: Int = 0
      var lastLine: Int = 0
      val examples: mutable.ArrayBuffer[Quote] = mutable.ArrayBuffer.empty
    }

    private val byKey = mutable.HashMap.empty[String, Stats]
    private val firstLines = mutable.ArrayBuffer.empty[String]
    private var lines = 0
    private var other = 0

    /** Record the next line (already free of colour codes) and return its pattern id; 0 is "other", once [[MaxPatterns]] are tracked. */
    def add(line: String): Int = {
      lines += 1
      if (firstLines.sizeIs < InlineLines + 1) firstLines += line
      val k = key(line)
      val stats = byKey.get(k) match {
        case Some(s)                            => Some(s)
        case None if byKey.sizeIs < MaxPatterns =>
          val s = new Stats(byKey.size + 1, k, lines)
          byKey.update(k, s)
          Some(s)
        case None => None
      }
      stats match {
        case Some(s) =>
          s.count += 1
          s.lastLine = lines
          if (s.examples.sizeIs < MinPatternCount - 1) s.examples += Quote(stream, lines, abbreviate(line, MaxShownLineChars))
          s.id
        case None =>
          other += 1
          0
      }
    }

    def lineCount: Int = lines

    def result(): StreamShape =
      if (lines <= InlineLines) StreamShape(stream, lines, Some(firstLines.toList.map(abbreviate(_, MaxShownLineChars))), Nil, 0, 0, Nil)
      else {
        val all = byKey.values.toList
        val frequent = all.filter(_.count >= MinPatternCount).sortBy(s => (-s.count, s.id))
        val shown = frequent.take(ShownPatterns).map(s => Pattern(s.id, abbreviate(s.text, MaxPatternChars), s.count, s.firstLine, s.lastLine))
        val rare = all.filter(_.count < MinPatternCount).flatMap(_.examples)
        StreamShape(stream, lines, None, shown, frequent.size - shown.size, other, rare)
      }
  }

  /** The longest stretch without output, and how long the program had been quiet when the run ended. Shared by both streams' reader threads. */
  final class Silence(startedAtMs: Long) {
    private var lastOutputMs = startedAtMs
    private var longestMs = 0L
    private var longestBefore: Option[(String, Int)] = None
    private var anyOutput = false

    def onLine(stream: String, line: Int, atMs: Long): Unit = synchronized {
      val gap = atMs - lastOutputMs
      if (gap > longestMs) {
        longestMs = gap
        longestBefore = Some((stream, line))
      }
      lastOutputMs = math.max(lastOutputMs, atMs)
      anyOutput = true
    }

    def result(endedAtMs: Long): Timing = synchronized {
      Timing(longestMs, longestBefore, silentAtEndMs = endedAtMs - lastOutputMs, anyOutput)
    }
  }

  case class Timing(longestSilenceMs: Long, longestSilenceBefore: Option[(String, Int)], silentAtEndMs: Long, anyOutput: Boolean)

  /** Rare lines worth quoting, across both streams: anything that reads like an error first, then the most recent. */
  def pickRare(shapes: List[StreamShape]): List[Quote] =
    shapes
      .flatMap(_.rareCandidates)
      .sortBy(q => (if (isErrorLike(q.text)) 0 else 1, -q.line, q.stream))
      .take(RareLines)

  /** The shape of one stream as the run result shows it. */
  def streamJson(shape: StreamShape, droppedLines: Long): Json = {
    val fields = List.newBuilder[(String, Json)]
    fields += "lines" -> Json.fromInt(shape.lines)
    shape.inline match {
      case Some(lines) => fields += "text" -> Json.arr(lines.map(Json.fromString)*)
      case None        =>
        if (shape.patterns.nonEmpty)
          fields += "patterns" -> Json.arr(shape.patterns.map { p =>
            val where = if (p.firstLine == p.lastLine) s"L${p.firstLine}" else s"L${p.firstLine}–${p.lastLine}"
            Json.fromString(s"p${p.id} ×${p.count} $where: ${p.text}")
          }*)
        if (shape.patternsNotShown > 0) fields += "morePatterns" -> Json.fromInt(shape.patternsNotShown)
        if (shape.linesInOtherPattern > 0) fields += "unpatternedLines" -> Json.fromInt(shape.linesInOtherPattern)
    }
    if (droppedLines > 0) fields += "droppedLines" -> Json.fromLong(droppedLines)
    Json.obj(fields.result()*)
  }

  def rareJson(quotes: List[Quote]): Json =
    Json.arr(quotes.map(q => Json.fromString(s"${q.stream} L${q.line}: ${q.text}"))*)

  def timingJson(timing: Timing): Json = {
    val fields = List.newBuilder[(String, Json)]
    timing.longestSilenceBefore.foreach { case (stream, line) =>
      fields += "longestSilenceMs" -> Json.fromLong(timing.longestSilenceMs)
      fields += "longestSilenceBefore" -> Json.fromString(s"$stream L$line")
    }
    fields += (if (timing.anyOutput) "silentAtEndMs" else "silentThroughoutMs") -> Json.fromLong(timing.silentAtEndMs)
    Json.obj(fields.result()*)
  }
}
