package bleep.logging

import fansi.Str
import sourcecode.Text

import java.io.{Flushable, PrintStream}
import java.util.concurrent.atomic.AtomicBoolean

trait TypedLogger[Underlying] extends LoggerFn {

  def underlying: Underlying

  def withContext[T: Formatter](key: String, value: T): TypedLogger[Underlying]

  def withPath(fragment: String): TypedLogger[Underlying]

  final def withContext[T: Formatter](value: Text[T]): TypedLogger[Underlying] =
    withContext(value.source, value.value)

  final def withOptContext[T: Formatter](key: String, maybeValue: Option[T]): TypedLogger[Underlying] =
    maybeValue match {
      case Some(value) => withContext(key, value)
      case None        => this
    }

  def progressMonitor: Option[LoggerFn]
}

object TypedLogger {

  case class Stored(message: Str, throwable: Option[Throwable], metadata: Metadata, ctx: Ctx, path: List[String])

  private[logging] class Store() {
    private var reversed: List[Stored] = Nil

    def store(s: Stored): Unit =
      reversed = s :: reversed

    def normal: Array[Stored] =
      reversed.toArray.reverse
  }

  private[logging] final class StoringLogger(store: Store, val ctx: Ctx, path: List[String]) extends TypedLogger[Array[Stored]] {

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit =
      store.store(Stored(Formatter(t), throwable, metadata, ctx, path))

    override def withContext[T: Formatter](key: String, value: T): StoringLogger =
      new StoringLogger(store, ctx + (key -> Formatter(value)), path)

    override def withPath(fragment: String): TypedLogger[Array[Stored]] =
      new StoringLogger(store, ctx, fragment :: path)

    override def underlying: Array[Stored] =
      store.normal

    override def progressMonitor: Option[LoggerFn] = None
  }

  private[logging] final class AppendableLogger[U <: Appendable](val underlying: U, pattern: Pattern, val context: Ctx, val path: List[String])
      extends TypedLogger[U] { self =>

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
      val formatted = pattern(t, throwable, metadata, context, path)
      underlying.append(formatted.render + "\n")
      ()
    }

    override def withContext[T: Formatter](key: String, value: T): AppendableLogger[U] =
      new AppendableLogger(underlying, pattern, context + (key -> Formatter(value)), path)

    override def progressMonitor: Option[LoggerFn] = None

    override def withPath(fragment: String): AppendableLogger[U] =
      new AppendableLogger(underlying, pattern, context, fragment :: path)
  }

  private[logging] final class ConsoleLogger[U <: PrintStream](
      val underlying: U,
      pattern: Pattern,
      val context: Ctx,
      path: List[String],
      disableProgress: Boolean,
      lastWasProgress: AtomicBoolean = new AtomicBoolean(false) // need to share this across instances after `withContext`
  ) extends TypedLogger[U] {

    val CleanCurrentLine = "\u001b[K"

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
      val formatted = pattern(t, throwable, metadata, context, path)
      if (lastWasProgress.get()) {
        underlying.append(CleanCurrentLine + formatted.render + "\n")
      } else {
        underlying.append(formatted.render + "\n")
      }

      lastWasProgress.set(false)
      ()
    }

    override def withContext[T: Formatter](key: String, value: T): ConsoleLogger[U] =
      new ConsoleLogger(underlying, pattern, context + (key -> Formatter(value)), path, disableProgress, lastWasProgress)

    override def withPath(fragment: String): ConsoleLogger[U] =
      new ConsoleLogger(underlying, pattern, context, fragment :: path, disableProgress, lastWasProgress)

    // todo: this is only here until we have a proper thing to render UI like tui.
    override def progressMonitor: Option[LoggerFn] =
      if (disableProgress) None
      else
        Some {
          new LoggerFn {
            override def log[T: Formatter](text: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
              val formatted = pattern(text, throwable, metadata, context, path)
              if (lastWasProgress.get()) {
                underlying.append(CleanCurrentLine + formatted.render + "\r")
                ()
              } else {
                underlying.append(formatted.render + "\r")
                lastWasProgress.set(true)
              }
            }
          }
        }
  }

  private[logging] final class Flushing[U <: Flushable](wrapped: TypedLogger[U]) extends TypedLogger[U] {
    override def underlying: U = wrapped.underlying

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit = {
      wrapped.log(t, throwable, metadata)
      wrapped.underlying.flush()
    }

    override def withContext[T: Formatter](key: String, value: T): Flushing[U] =
      new Flushing[U](wrapped.withContext(key, value))

    override def withPath(fragment: String): Flushing[U] =
      new Flushing[U](wrapped.withPath(fragment))

    override def progressMonitor: Option[LoggerFn] = wrapped.progressMonitor
  }

  private[logging] final class Zipped[U1, U2](one: TypedLogger[U1], two: TypedLogger[U2]) extends TypedLogger[(U1, U2)] {
    override def underlying: (U1, U2) =
      (one.underlying, two.underlying)

    private val both = one.and(two)

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit =
      both.log(t, throwable, metadata)

    override def withContext[T: Formatter](key: String, value: T): Zipped[U1, U2] =
      new Zipped(one.withContext(key, value), two.withContext(key, value))

    override def withPath(fragment: String): Zipped[U1, U2] =
      new Zipped(one.withPath(fragment), two.withPath(fragment))

    override def progressMonitor: Option[LoggerFn] =
      List(one.progressMonitor, two.progressMonitor).flatten.reduceOption(_ and _)
  }

  private[logging] final class MinLogLevel[U](wrapped: TypedLogger[U], minLogLevel: LogLevel) extends TypedLogger[U] {
    override def underlying: U = wrapped.underlying

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], m: Metadata): Unit =
      if (m.logLevel.level >= minLogLevel.level) {
        wrapped.log(t, throwable, m)
      }

    override def withContext[T: Formatter](key: String, value: T): MinLogLevel[U] =
      new MinLogLevel[U](wrapped.withContext(key, value), minLogLevel)

    override def withPath(fragment: String): MinLogLevel[U] =
      new MinLogLevel[U](wrapped.withPath(fragment), minLogLevel)

    override def progressMonitor: Option[LoggerFn] =
      wrapped.progressMonitor
  }

  private[logging] final class Mapped[U, UU](wrapped: TypedLogger[U], f: U => UU) extends TypedLogger[UU] {
    override def underlying: UU = f(wrapped.underlying)

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], m: Metadata): Unit =
      wrapped.log(t, throwable, m)

    override def withContext[T: Formatter](key: String, value: T): Mapped[U, UU] =
      new Mapped(wrapped.withContext(key, value), f)

    override def withPath(fragment: String): Mapped[U, UU] =
      new Mapped[U, UU](wrapped.withPath(fragment), f)

    override def progressMonitor: Option[LoggerFn] =
      wrapped.progressMonitor
  }

  private[logging] final class Synchronized[U](wrapped: TypedLogger[U]) extends TypedLogger[U] {
    override def underlying: U = wrapped.underlying

    override def log[T: Formatter](t: => T, throwable: Option[Throwable], m: Metadata): Unit =
      this.synchronized(wrapped.log(t, throwable, m))

    override def withContext[T: Formatter](key: String, value: T): Synchronized[U] =
      new Synchronized(wrapped.withContext(key, value))

    override def withPath(fragment: String): Synchronized[U] =
      new Synchronized(wrapped.withPath(fragment))

    override def progressMonitor: Option[LoggerFn] =
      wrapped.progressMonitor
  }

  object DevNull extends TypedLogger[Unit] {
    override def underlying: Unit = ()
    override def withContext[T: Formatter](key: String, value: T): DevNull.type = this
    override def withPath(fragment: String): DevNull.type = this
    override def log[T: Formatter](text: => T, throwable: Option[Throwable], metadata: Metadata): Unit = ()
    override def progressMonitor: Option[LoggerFn] = None
  }

  implicit final class LoggerAuxSyntax[U](private val self: TypedLogger[U]) extends AnyVal {
    def zipWith[UU](other: TypedLogger[UU]): TypedLogger[(U, UU)] =
      new Zipped(self, other)

    def minLogLevel(minLogLevel: LogLevel): TypedLogger[U] =
      new MinLogLevel(self, minLogLevel)

    def untyped: TypedLogger[Unit] =
      new Mapped[U, Unit](self, _ => ())

    def syncAccess: TypedLogger[U] =
      new Synchronized(self)
  }

  implicit final class LoggerFlushableSyntax[U <: Flushable](private val self: TypedLogger[U]) extends AnyVal {
    def flushing: TypedLogger[U] =
      new Flushing[U](self)
  }
}
