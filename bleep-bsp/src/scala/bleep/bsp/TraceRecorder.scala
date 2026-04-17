package bleep.bsp

import cats.effect.{IO, Ref}
import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets

/** A trace event in Chrome Trace Event Format.
  *
  * @param name
  *   Display name (e.g., "compile java-lib")
  * @param category
  *   Category for grouping (e.g., "compile", "link", "test")
  * @param startUs
  *   Start time in microseconds since trace start
  * @param durationUs
  *   Duration in microseconds
  * @param tid
  *   Thread/lane ID for parallel display
  */
case class TraceEvent(
    name: String,
    category: TraceCategory,
    startUs: Long,
    durationUs: Long,
    tid: Int
)

/** Category for trace events — prevents typos in string-based category names. */
sealed trait TraceCategory {
  def value: String
  override def toString: String = value
}
object TraceCategory {
  case object Compile extends TraceCategory { val value = "compile" }
  case object Link extends TraceCategory { val value = "link" }
  case object Discover extends TraceCategory { val value = "discover" }
  case object Test extends TraceCategory { val value = "test" }
  case object Sourcegen extends TraceCategory { val value = "sourcegen" }
}

/** Records execution traces in Chrome Trace Event Format.
  *
  * Output can be viewed in:
  *   - `chrome://tracing` (built into Chrome)
  *   - https://ui.perfetto.dev/ (modern UI, better features)
  */
trait TraceRecorder {

  /** Record that a task has started */
  def recordStart(category: TraceCategory, name: String): IO[Unit]

  /** Record that a task has ended */
  def recordEnd(category: TraceCategory, name: String): IO[Unit]

  /** Write the trace to a JSON file in Chrome Trace Event Format */
  def writeTrace(outputPath: Path): IO[Unit]
}

object TraceRecorder {

  /** Create a new trace recorder that tracks events */
  def create: IO[TraceRecorder] =
    for {
      // Base time in microseconds (when recorder was created)
      baseTimeRef <- Ref.of[IO, Long](System.currentTimeMillis() * 1000)
      // In-flight tasks: key (cat:name) -> (startUs, tid)
      inFlightRef <- Ref.of[IO, Map[String, (Long, Int)]](Map.empty)
      // Next thread/lane ID for parallel display
      nextTidRef <- Ref.of[IO, Int](1)
      // Completed events
      eventsRef <- Ref.of[IO, List[TraceEvent]](Nil)
    } yield new Impl(baseTimeRef, inFlightRef, nextTidRef, eventsRef)

  /** No-op trace recorder (when --flamegraph is not specified) */
  val noop: TraceRecorder = new TraceRecorder {
    def recordStart(category: TraceCategory, name: String): IO[Unit] = IO.unit
    def recordEnd(category: TraceCategory, name: String): IO[Unit] = IO.unit
    def writeTrace(outputPath: Path): IO[Unit] = IO.unit
  }

  private class Impl(
      baseTimeRef: Ref[IO, Long],
      inFlightRef: Ref[IO, Map[String, (Long, Int)]],
      nextTidRef: Ref[IO, Int],
      eventsRef: Ref[IO, List[TraceEvent]]
  ) extends TraceRecorder {

    private def nowUs: IO[Long] = IO.delay(System.currentTimeMillis() * 1000)

    private def key(category: TraceCategory, name: String): String = s"${category.value}:$name"

    override def recordStart(category: TraceCategory, name: String): IO[Unit] =
      for {
        baseTime <- baseTimeRef.get
        currentUs <- nowUs
        relativeUs = currentUs - baseTime
        tid <- nextTidRef.getAndUpdate(_ + 1)
        _ <- inFlightRef.update(_ + (key(category, name) -> (relativeUs, tid)))
      } yield ()

    override def recordEnd(category: TraceCategory, name: String): IO[Unit] =
      for {
        baseTime <- baseTimeRef.get
        currentUs <- nowUs
        relativeUs = currentUs - baseTime
        inFlight <- inFlightRef.get
        _ <- inFlight.get(key(category, name)) match {
          case Some((startUs, tid)) =>
            val event = TraceEvent(
              name = s"${category.value} $name",
              category = category,
              startUs = startUs,
              durationUs = relativeUs - startUs,
              tid = tid
            )
            eventsRef.update(event :: _) >> inFlightRef.update(_ - key(category, name))
          case None =>
            // Task wasn't recorded as started, skip
            IO.unit
        }
      } yield ()

    override def writeTrace(outputPath: Path): IO[Unit] =
      for {
        events <- eventsRef.get
        json = formatAsJson(events.reverse)
        _ <- IO.blocking {
          Files.createDirectories(outputPath.getParent)
          Files.writeString(outputPath, json, StandardCharsets.UTF_8)
        }
      } yield ()

    /** Format events as Chrome Trace Event Format JSON.
      *
      * Format spec: https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU
      */
    private def formatAsJson(events: List[TraceEvent]): String = {
      val traceEvents = events.map { e =>
        // Using "X" (complete event) format which has name, cat, ph, ts, dur, pid, tid
        s"""    {"name": ${escapeJson(e.name)}, "cat": ${escapeJson(
            e.category.value
          )}, "ph": "X", "ts": ${e.startUs}, "dur": ${e.durationUs}, "pid": 1, "tid": ${e.tid}}"""
      }

      s"""{
  "traceEvents": [
${traceEvents.mkString(",\n")}
  ],
  "displayTimeUnit": "ms"
}"""
    }

    private def escapeJson(s: String): String = {
      val sb = new StringBuilder("\"")
      s.foreach {
        case '"'          => sb.append("\\\"")
        case '\\'         => sb.append("\\\\")
        case '\b'         => sb.append("\\b")
        case '\f'         => sb.append("\\f")
        case '\n'         => sb.append("\\n")
        case '\r'         => sb.append("\\r")
        case '\t'         => sb.append("\\t")
        case c if c < ' ' =>
          sb.append("\\u")
          sb.append(f"${c.toInt}%04x")
        case c => sb.append(c)
      }
      sb.append("\"")
      sb.toString()
    }
  }
}
