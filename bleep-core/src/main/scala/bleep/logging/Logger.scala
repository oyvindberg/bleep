package bleep.logging

import fansi.Str
import sourcecode.{Enclosing, File, Line, Text}

import java.io.Writer
import java.time.Instant

trait Logger { self =>
  type Underlying
  def underlying: Underlying

  def withContext[T: Formatter](key: String, value: T): Logger.Aux[Underlying]

  final def withContext[T: Formatter](value: Text[T]): Logger.Aux[Underlying] =
    self.withContext(value.source, value.value)

  def log[T: Formatter](text: => Text[T], throwable: Option[Throwable], metadata: Metadata): Unit
}

object Logger {
  type Aux[U] = Logger { type Underlying = U }

  case class Stored(message: Str, throwable: Option[Throwable], metadata: Metadata, ctx: Ctx)

  private[logging] class Store() {
    private var reversed: List[Stored] = Nil

    def store(s: Stored): Unit =
      reversed = s :: reversed

    def normal: Array[Stored] =
      reversed.to(Array).reverse
  }

  private[logging] final class StoringLogger(store: Store, val ctx: Ctx) extends Logger {
    override type Underlying = Array[Stored]

    override def log[T: Formatter](t: => Text[T], throwable: Option[Throwable], metadata: Metadata): Unit =
      store.store(Stored(Formatter(t.value), throwable, metadata, ctx))

    override def withContext[T: Formatter](key: String, value: T): StoringLogger =
      new StoringLogger(store, ctx + (Str(key) -> Formatter(value)))

    override def underlying: Array[Stored] =
      store.normal
  }

  private[logging] final class AppendableLogger[U <: Appendable](val underlying: U, pattern: Pattern, val context: Ctx) extends Logger {
    type Underlying = U
    override def log[T: Formatter](t: => Text[T], throwable: Option[Throwable], metadata: Metadata): Unit = {
      val formatted = pattern(t, throwable, metadata, context)
      underlying.append(formatted.render + "\n")
      ()
    }

    override def withContext[T: Formatter](key: String, value: T): AppendableLogger[U] =
      new AppendableLogger(underlying, pattern, context + (Str(key) -> Formatter(value)))
  }

  private[logging] final class WriterLogger[U <: Writer](appendable: AppendableLogger[U]) extends Logger {
    override type Underlying = U
    override def underlying: U = appendable.underlying

    override def log[T: Formatter](t: => Text[T], throwable: Option[Throwable], metadata: Metadata): Unit = {
      appendable.log(t, throwable, metadata)
      appendable.underlying.flush()
    }

    override def withContext[T: Formatter](key: String, value: T): WriterLogger[U] =
      new WriterLogger[U](appendable.withContext(key, value))

  }

  private[logging] final class Zipped[U1, U2](one: Logger.Aux[U1], two: Logger.Aux[U2]) extends Logger {
    override type Underlying = (U1, U2)
    override def underlying: (U1, U2) =
      (one.underlying, two.underlying)

    override def log[T: Formatter](t: => Text[T], throwable: Option[Throwable], metadata: Metadata): Unit = {
      one.log(t, throwable, metadata)
      two.log(t, throwable, metadata)
    }

    override def withContext[T: Formatter](key: String, value: T): Zipped[U1, U2] =
      new Zipped(one.withContext(key, value), two.withContext(key, value))
  }

  private[logging] final class WithFilter[U](wrapped: Logger.Aux[U], minLogLevel: LogLevel) extends Logger {
    override type Underlying = U
    override def underlying: U = wrapped.underlying

    override def log[T: Formatter](t: => Text[T], throwable: Option[Throwable], m: Metadata): Unit =
      if (m.logLevel.level >= minLogLevel.level) {
        wrapped.log(t, throwable, m)
      }

    override def withContext[T: Formatter](key: String, value: T) =
      new WithFilter[Underlying](wrapped.withContext(key, value), minLogLevel)
  }

  private[logging] final class Mapped[U, UU](wrapped: Logger.Aux[U], f: U => UU) extends Logger {
    override type Underlying = UU
    override def underlying: UU = f(wrapped.underlying)

    override def log[T: Formatter](t: => Text[T], throwable: Option[Throwable], m: Metadata): Unit =
      wrapped.log(t, throwable, m)

    override def withContext[T: Formatter](key: String, value: T): Logger.Aux[Underlying] =
      new Mapped(wrapped.withContext(key, value), f)
  }

  private[logging] final class Synchronized[U](wrapped: Logger.Aux[U]) extends Logger {
    override type Underlying = U
    override def underlying: U = wrapped.underlying

    override def log[T: Formatter](t: => Text[T], throwable: Option[Throwable], m: Metadata): Unit =
      this.synchronized(wrapped.log(t, throwable, m))

    override def withContext[T: Formatter](key: String, value: T): Synchronized[U] =
      new Synchronized(wrapped.withContext(key, value))
  }

  object DevNull extends Logger {
    override type Underlying = Unit
    override def underlying: Unit = ()
    override def withContext[T: Formatter](key: String, value: T): Logger.Aux[Unit] = this
    override def log[T: Formatter](text: => Text[T], throwable: Option[Throwable], metadata: Metadata): Unit = ()
  }

  implicit final class LoggerOps[U](private val self: Logger.Aux[U]) extends AnyVal {
    def zipWith[UU](other: Logger.Aux[UU]): Logger.Aux[(U, UU)] =
      new Zipped(self, other)

    def filter(minLogLevel: LogLevel): Logger.Aux[U] =
      new WithFilter(self, minLogLevel)

    def void: Logger.Aux[Unit] =
      new Mapped[U, Unit](self, _ => ())

    def syncAccess: Logger.Aux[U] =
      new Synchronized(self)
  }

  implicit final class LoggingOps(private val self: Logger) extends AnyVal {
    @inline def apply[T: Formatter](
        logLevel: LogLevel,
        t: => Text[T],
        throwable: Option[Throwable] = None,
        instant: Instant = Instant.now
    )(implicit l: Line, f: File, e: Enclosing): Unit =
      self.log(t, throwable, new Metadata(instant, logLevel, l, f, e))

    @inline def trace[T: Formatter](t: => Text[T])(implicit l: Line, f: File, e: Enclosing): Unit =
      apply(LogLevel.trace, t)

    @inline def trace[T: Formatter](t: => Text[T], th: Throwable)(implicit l: Line, f: File, e: Enclosing): Unit =
      apply(LogLevel.trace, t, Some(th))

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
  }
}
