package bleep.logging

import fansi.Str
import io.circe.generic.semiauto
import io.circe.parser.decode
import io.circe.{Codec, Decoder, Encoder}
import sourcecode.{Enclosing, File, Line}

import java.io.{PrintStream, PrintWriter}
import java.time.Instant
import scala.util.control.NoStackTrace

private object jsonEvents {

  /** Meant for transferring log events between processes */
  case class JsonEvent(formatted: Str, throwable: Option[Th], metadata: Metadata, ctx: Ctx, path: List[String])

  object JsonEvent {
    implicit val strCodec: Codec[Str] =
      Codec.forProduct2[Str, Array[Char], Array[Long]]("chars", "colors") { case (chars, colors) => Str.fromArrays(chars, colors) } { str =>
        (str.getChars, str.getColors)
      }

    implicit val metadataCodec: Codec.AsObject[Metadata] =
      Codec.forProduct5[Metadata, Instant, Int, Int, String, String]("instant", "logLevel", "line", "file", "enclosing") {
        case (instant, logLevel, line, file, enclosing) =>
          new Metadata(instant, LogLevel.unsafeFrom(logLevel), new Line(line), new File(file), new Enclosing(enclosing))
      }(m => (m.instant, m.logLevel.level, m.line.value, m.file.value, m.enclosing.value))

    implicit val codec: Codec[JsonEvent] = semiauto.deriveCodec

    def from[T: Formatter](t: T, throwable: Option[Throwable], metadata: Metadata, ctx: Ctx, path: List[String]): JsonEvent =
      JsonEvent(Formatter[T](t), throwable.map(Th.from), metadata, ctx, path)
  }

  /** For used in called program, so it outputs all log events in json
    */
  final class SerializeLogEvents[U <: Appendable](val underlying: U, val context: Ctx, val path: List[String]) extends TypedLogger[U] {
    import io.circe.syntax._

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
      val json = JsonEvent.from(t, throwable, metadata, context, path).asJson
      underlying.append(json.noSpaces + "\n")
      ()
    }

    override def withContext[T: Formatter](key: String, value: T): SerializeLogEvents[U] =
      new SerializeLogEvents(underlying, context + (key -> Formatter(value)), path)

    override def progressMonitor: Option[LoggerFn] = None

    override def withPath(fragment: String): TypedLogger[U] =
      new SerializeLogEvents[U](underlying, context, fragment :: path)
  }

  final case class DeserializeLogEvents[U](next: TypedLogger[U]) extends TypedLogger[U] {
    override def log[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
      val str = implicitly[Formatter[T]].apply(t)
      if (str.plainText.startsWith("{")) {
        decode[jsonEvents.JsonEvent](str.plainText) match {
          case Left(_) => next.log(t, throwable, metadata)
          case Right(jsonEvent) =>
            val logger1 = jsonEvent.path.foldRight(next) { case (fragment, acc) => acc.withPath(fragment) }
            val logger2 = jsonEvent.ctx.foldRight(logger1) { case ((k, v), acc) => acc.withContext(k, v) }

            logger2(
              jsonEvent.metadata.logLevel,
              jsonEvent.formatted,
              jsonEvent.throwable.map(DeserializedThrowable.apply),
              jsonEvent.metadata.instant
            )(Formatter.StrFormatter, jsonEvent.metadata.line, jsonEvent.metadata.file, jsonEvent.metadata.enclosing)
        }
      } else
        next.log(t, throwable, metadata)
    }

    override def withContext[T: Formatter](key: String, value: T): DeserializeLogEvents[U] =
      DeserializeLogEvents(next.withContext(value))

    override def progressMonitor: Option[LoggerFn] = None

    override def withPath(fragment: String): DeserializeLogEvents[U] =
      DeserializeLogEvents(next.withPath(fragment))

    override def underlying: U = next.underlying
  }

  case class DeserializedThrowable(th: Th) extends Throwable with NoStackTrace {
    override def printStackTrace(s: PrintStream): Unit = s.println(th.stackTrace)
    override def printStackTrace(s: PrintWriter): Unit = s.println(th.stackTrace)
    override def getMessage: String = th.message.getOrElse("")
  }

  /** Wrap exceptions in something which is easier to transfer
    */
  case class Th(className: String, message: Option[String], cause: Option[Th], stackTrace: String, suppressed: Array[Th]) extends Throwable()

  object Th {
    implicit val encoder: Encoder[Th] = semiauto.deriveEncoder
    implicit val decoder: Decoder[Th] = semiauto.deriveDecoder

    def from(th: Throwable): Th =
      Th(
        className = th.getClass.getName,
        message = Option(th.getMessage),
        cause = Option(th.getCause).map(from),
        stackTrace = formatThrowable(th),
        suppressed = th.getSuppressed.map(from)
      )
  }
}
