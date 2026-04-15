package bleep.javaapi

final class JLogger(private val underlying: ryddig.Logger) extends bleepscript.Logger {
  override def debug(msg: String): Unit = underlying.debug(msg)
  override def debug(msg: String, t: Throwable): Unit = underlying.debug(msg, t)
  override def info(msg: String): Unit = underlying.info(msg)
  override def info(msg: String, t: Throwable): Unit = underlying.info(msg, t)
  override def warn(msg: String): Unit = underlying.warn(msg)
  override def warn(msg: String, t: Throwable): Unit = underlying.warn(msg, t)
  override def error(msg: String): Unit = underlying.error(msg)
  override def error(msg: String, t: Throwable): Unit = underlying.error(msg, t)

  override def withContext(key: String, value: String): bleepscript.Logger =
    new JLogger(underlying.withContext(key, value))

  override def withContext(key: String, value: Object): bleepscript.Logger =
    new JLogger(underlying.withContext(key, if (value == null) "null" else value.toString))

  override def withPath(fragment: String): bleepscript.Logger =
    new JLogger(underlying.withPath(fragment))
}
