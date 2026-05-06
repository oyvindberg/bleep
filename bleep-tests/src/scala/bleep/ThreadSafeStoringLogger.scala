package bleep

import fansi.Str
import ryddig.{Ctx, Formatter, LogLevel, LoggerFn, Metadata, Stored, TypedLogger}

import java.util.concurrent.ConcurrentLinkedDeque
import scala.jdk.CollectionConverters.*

/** Thread-safe drop-in replacement for `ryddig.Loggers.storing()`.
  *
  * Ryddig 0.0.6's `TypedLogger.Store` uses an unsynchronized `var reversed: List[Stored]` with a `reversed = s :: reversed` write — classic lost-update race
  * when multiple threads emit log lines concurrently (in-process BSP server fibers, subprocess output drain threads, the test thread itself). Under load, log
  * entries silently disappear, which is how `YourFirstProjectIT` flaked: the subprocess's "Hello, World!" line was printed to stdout but lost on the way into
  * the in-memory store, so the assertion failed even though the message had been emitted.
  *
  * Same external interface as the ryddig version (`TypedLogger[Array[Stored]]`); only difference is the underlying buffer is a `ConcurrentLinkedDeque`.
  * Children created via `withContext`/`withPath` share the same buffer, exactly like the ryddig version.
  */
object ThreadSafeStoringLogger {
  def apply(): TypedLogger[Array[Stored]] =
    new Impl(new ConcurrentLinkedDeque[Stored](), Map.empty[String, Str], Nil)

  private final class Impl(buffer: ConcurrentLinkedDeque[Stored], val context: Ctx, val path: List[String]) extends TypedLogger[Array[Stored]] {

    override def apply[T: Formatter](t: => T, throwable: Option[Throwable], metadata: Metadata): Unit =
      buffer.add(Stored(Formatter(t), throwable, metadata, context, path))

    override def withContext[T: Formatter](key: String, value: T): TypedLogger[Array[Stored]] =
      new Impl(buffer, context + (key -> Formatter(value)), path)

    override def withPath(fragment: String): TypedLogger[Array[Stored]] =
      new Impl(buffer, context, fragment :: path)

    override def underlying: Array[Stored] =
      buffer.iterator().asScala.toArray

    override def progressMonitor: Option[LoggerFn] = None

    override val minLogLevel: LogLevel = LogLevel.debug
  }
}
