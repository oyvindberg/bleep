package bleep.logging

import fansi.Str
import sourcecode.Text

import java.io.{Flushable, PrintStream}
import java.util.concurrent.atomic.AtomicBoolean

trait TypedLogger[Underlying] extends LoggerFn {

  def underlying: Underlying

  def withContext[T: Formatter](key: String, value: T): TypedLogger[Underlying]

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

  case class Stored(message: Str, throwable: Option[Throwable], metadata: Metadata, ctx: Ctx)

  private[logging] class Store() {
    private var reversed: List[Stored] = Nil

    def store(s: Stored): Unit =
      reversed = s :: reversed

    def normal: Array[Stored] =
      reversed.to(Array).reverse
  }

  private[logging] final class StoringLogger(store: Store, val ctx: Ctx) extends TypedLogger[Array[Stored]] {

    override def log[T: Formatter](t: => Text[T], throwable: Option[Throwable], metadata: Metadata): Unit =
      store.store(Stored(Formatter(t.value), throwable, metadata, ctx))

    override def withContext[T: Formatter](key: String, value: T): StoringLogger =
      new StoringLogger(store, ctx + (Str(key) -> Formatter(value)))

    override def underlying: Array[Stored] =
      store.normal

    override def progressMonitor: Option[LoggerFn] = None
  }

  private[logging] final class AppendableLogger[U <: Appendable](val underlying: U, pattern: Pattern, val context: Ctx) extends TypedLogger[U] { self =>

    override def log[T: Formatter](t: => Text[T], throwable: Option[Throwable], metadata: Metadata): Unit = {
      val formatted = pattern(t, throwable, metadata, context)
      underlying.append(formatted.render + "\n")
      ()
    }

    override def withContext[T: Formatter](key: String, value: T): AppendableLogger[U] =
      new AppendableLogger(underlying, pattern, context + (Str(key) -> Formatter(value)))

    override def progressMonitor: Option[LoggerFn] = None
  }

  private[logging] final class ConsoleLogger[U <: PrintStream](
      val underlying: U,
      pattern: Pattern,
      val context: Ctx,
      lastWasProgress: AtomicBoolean = new AtomicBoolean(false) // need to share this across instances after `withContext`
  ) extends TypedLogger[U] {

    val CleanCurrentLine = "\u001b[K"

    override def log[T: Formatter](t: => Text[T], throwable: Option[Throwable], metadata: Metadata): Unit = {
      val formatted = pattern(t, throwable, metadata, context)
      if (lastWasProgress.get()) {
        underlying.append(CleanCurrentLine + formatted.render + "\n")
      } else {
        underlying.append(formatted.render + "\n")
      }

      lastWasProgress.set(false)
      ()
    }

    override def withContext[T: Formatter](key: String, value: T): ConsoleLogger[U] =
      new ConsoleLogger(underlying, pattern, context + (Str(key) -> Formatter(value)), lastWasProgress)

    override def progressMonitor: Option[LoggerFn] = Some(new LoggerFn {
      override def log[T: Formatter](text: => Text[T], throwable: Option[Throwable], metadata: Metadata): Unit = {
        val formatted = pattern(text, throwable, metadata, context)
        if (lastWasProgress.get()) {
          underlying.append(CleanCurrentLine + formatted.render + "\r")
          ()
        } else {
          underlying.append(formatted.render + "\r")
          lastWasProgress.set(true)
        }
      }
    })
  }

  private[logging] final class Flushing[U <: Flushable](wrapped: TypedLogger[U]) extends TypedLogger[U] {
    override def underlying: U = wrapped.underlying

    override def log[T: Formatter](t: => Text[T], throwable: Option[Throwable], metadata: Metadata): Unit = {
      wrapped.log(t, throwable, metadata)
      wrapped.underlying.flush()
    }

    override def withContext[T: Formatter](key: String, value: T): Flushing[U] =
      new Flushing[U](wrapped.withContext(key, value))

    override def progressMonitor: Option[LoggerFn] = wrapped.progressMonitor
  }

  private[logging] final class Zipped[U1, U2](one: TypedLogger[U1], two: TypedLogger[U2]) extends TypedLogger[(U1, U2)] {
    override def underlying: (U1, U2) =
      (one.underlying, two.underlying)

    private val both = one.and(two)

    override def log[T: Formatter](t: => Text[T], throwable: Option[Throwable], metadata: Metadata): Unit =
      both.log(t, throwable, metadata)

    override def withContext[T: Formatter](key: String, value: T): Zipped[U1, U2] =
      new Zipped(one.withContext(key, value), two.withContext(key, value))

    override def progressMonitor: Option[LoggerFn] =
      List(one.progressMonitor, two.progressMonitor).flatten.reduceOption(_ and _)
  }

  private[logging] final class WithFilter[U](wrapped: TypedLogger[U], minLogLevel: LogLevel) extends TypedLogger[U] {
    override def underlying: U = wrapped.underlying

    override def log[T: Formatter](t: => Text[T], throwable: Option[Throwable], m: Metadata): Unit =
      if (m.logLevel.level >= minLogLevel.level) {
        wrapped.log(t, throwable, m)
      }

    override def withContext[T: Formatter](key: String, value: T) =
      new WithFilter[U](wrapped.withContext(key, value), minLogLevel)

    override def progressMonitor: Option[LoggerFn] =
      wrapped.progressMonitor
  }

  private[logging] final class Mapped[U, UU](wrapped: TypedLogger[U], f: U => UU) extends TypedLogger[UU] {
    override def underlying: UU = f(wrapped.underlying)

    override def log[T: Formatter](t: => Text[T], throwable: Option[Throwable], m: Metadata): Unit =
      wrapped.log(t, throwable, m)

    override def withContext[T: Formatter](key: String, value: T): TypedLogger[UU] =
      new Mapped(wrapped.withContext(key, value), f)

    override def progressMonitor: Option[LoggerFn] =
      wrapped.progressMonitor
  }

  private[logging] final class Synchronized[U](wrapped: TypedLogger[U]) extends TypedLogger[U] {
    override def underlying: U = wrapped.underlying

    override def log[T: Formatter](t: => Text[T], throwable: Option[Throwable], m: Metadata): Unit =
      this.synchronized(wrapped.log(t, throwable, m))

    override def withContext[T: Formatter](key: String, value: T): Synchronized[U] =
      new Synchronized(wrapped.withContext(key, value))

    override def progressMonitor: Option[LoggerFn] =
      wrapped.progressMonitor
  }

  object DevNull extends TypedLogger[Unit] {
    override def underlying: Unit = ()
    override def withContext[T: Formatter](key: String, value: T): TypedLogger[Unit] = this
    override def log[T: Formatter](text: => Text[T], throwable: Option[Throwable], metadata: Metadata): Unit = ()
    override def progressMonitor: Option[LoggerFn] = None
  }

  implicit final class LoggerAuxSyntax[U](private val self: TypedLogger[U]) extends AnyVal {
    def zipWith[UU](other: TypedLogger[UU]): TypedLogger[(U, UU)] =
      new Zipped(self, other)

    def filter(minLogLevel: LogLevel): TypedLogger[U] =
      new WithFilter(self, minLogLevel)

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
