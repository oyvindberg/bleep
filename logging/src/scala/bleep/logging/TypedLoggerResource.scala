package bleep.logging

import java.io.Flushable

trait TypedLoggerResource[U] {
  def use[T](f: TypedLogger[U] => T): T

  final def unsafeGet(): TypedLogger[U] = {
    var ret: TypedLogger[U] = null
    use(ret = _)
    ret
  }
}

object TypedLoggerResource {
  final implicit class Ops[U1](private val one: TypedLoggerResource[U1]) extends AnyVal {
    def map[U2](transform: TypedLogger[U1] => TypedLogger[U2]): TypedLoggerResource[U2] =
      new TypedLoggerResource[U2] {
        override def use[T](f: TypedLogger[U2] => T): T =
          one.use(logger => f(transform(logger)))
      }

    def zipWith[U2](other: TypedLoggerResource[U2]): TypedLoggerResource[(U1, U2)] =
      new TypedLoggerResource[(U1, U2)] {
        override def use[T](f: TypedLogger[(U1, U2)] => T): T =
          one.use(oneLogger => other.use(otherLogger => f(oneLogger.zipWith(otherLogger))))
      }

    def maybeZipWith[U2](other: Option[TypedLoggerResource[U2]]): TypedLoggerResource[(U1, Option[U2])] =
      new TypedLoggerResource[(U1, Option[U2])] {
        override def use[T](f: TypedLogger[(U1, Option[U2])] => T): T =
          one.use { oneLogger =>
            other match {
              case Some(other) => other.use(otherLogger => f(oneLogger.maybeZipWith(Some(otherLogger))))
              case None        => f(oneLogger.maybeZipWith(None))
            }
          }
      }

    def untyped: LoggerResource = new TypedLoggerResource[Unit] {
      override def use[T](f: TypedLogger[Unit] => T): T = one.use(l => f(l.untyped))
    }
  }

  def pure[U](logger: TypedLogger[U]): TypedLoggerResource[U] =
    new TypedLoggerResource[U] {
      override def use[T](f: TypedLogger[U] => T): T = f(logger)
    }

  def autoCloseable[U <: AutoCloseable](mkLogger: => TypedLogger[U]): TypedLoggerResource[U] =
    new TypedLoggerResource[U] {
      override def use[T](f: TypedLogger[U] => T): T = {
        var inited: TypedLogger[U] = null
        try {
          inited = mkLogger
          f(inited)
        } finally if (inited != null) inited.underlying.close()
      }
    }

  def flushable[U <: Flushable](mkLogger: => TypedLogger[U]): TypedLoggerResource[U] =
    new TypedLoggerResource[U] {
      override def use[T](f: TypedLogger[U] => T): T = {
        var inited: TypedLogger[U] = null
        try {
          inited = mkLogger
          f(inited)
        } finally if (inited != null) inited.underlying.flush()
      }
    }
}
