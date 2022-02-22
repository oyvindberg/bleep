package bleep.logging

import sourcecode.{Enclosing, File, Line, Text}

import java.time.Instant

@FunctionalInterface
trait LoggerFn { self =>
  def log[T: Formatter](text: => Text[T], throwable: Option[Throwable], metadata: Metadata): Unit
}

object LoggerFn {
  implicit final class Syntax(private val fn: LoggerFn) extends AnyVal {
    @inline def apply[T: Formatter](logLevel: LogLevel, t: => Text[T], throwable: Option[Throwable] = None, instant: Instant = Instant.now)(implicit
        l: Line,
        f: File,
        e: Enclosing
    ): Unit =
      fn.log(t, throwable, new Metadata(instant, logLevel, l, f, e))

    @inline def debug[T: Formatter](t: => Text[T])(implicit l: Line, f: File, e: Enclosing): Unit =
      apply(LogLevel.debug, t)

    @inline def debug[T: Formatter](t: => Text[T], th: Throwable)(implicit l: Line, f: File, e: Enclosing): Unit =
      apply(LogLevel.debug, t, Some(th))

    @inline def info[T: Formatter](t: => Text[T])(implicit l: Line, f: File, e: Enclosing): Unit =
      apply(LogLevel.info, t)

    @inline def info[T: Formatter](t: => Text[T], th: Throwable)(implicit l: Line, f: File, e: Enclosing): Unit =
      apply(LogLevel.info, t, Some(th))

    @inline def warn[T: Formatter](t: => Text[T])(implicit l: Line, f: File, e: Enclosing): Unit =
      apply(LogLevel.warn, t)

    @inline def warn[T: Formatter](t: => Text[T], th: Throwable)(implicit l: Line, f: File, e: Enclosing): Unit =
      apply(LogLevel.warn, t, Some(th))

    @inline def error[T: Formatter](t: => Text[T])(implicit l: Line, f: File, e: Enclosing): Unit =
      apply(LogLevel.error, t)

    @inline def error[T: Formatter](t: => Text[T], th: Throwable)(implicit l: Line, f: File, e: Enclosing): Unit =
      apply(LogLevel.error, t, Some(th))

    @inline def and(other: LoggerFn): LoggerFn =
      new LoggerFn {
        override def log[T: Formatter](text: => Text[T], throwable: Option[Throwable], metadata: Metadata): Unit = {
          fn.log(text, throwable, metadata)
          other.log(text, throwable, metadata)
        }
      }
  }
}
