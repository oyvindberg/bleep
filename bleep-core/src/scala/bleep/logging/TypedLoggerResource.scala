package bleep.logging

trait TypedLoggerResource[U] {
  def use[T](f: TypedLogger[U] => T): T
}

object TypedLoggerResource {
  implicit class Ops[U1](private val one: TypedLoggerResource[U1]) extends AnyVal {
    final def map[U2](transform: TypedLogger[U1] => TypedLogger[U2]): TypedLoggerResource[U2] =
      new TypedLoggerResource[U2] {
        override def use[T](f: TypedLogger[U2] => T): T =
          one.use(logger => f(transform(logger)))
      }

    final def zipWith[U2](other: TypedLoggerResource[U2]): TypedLoggerResource[(U1, U2)] =
      new TypedLoggerResource[(U1, U2)] {
        override def use[T](f: TypedLogger[(U1, U2)] => T): T =
          one.use(oneLogger => other.use(otherLogger => f(oneLogger.zipWith(otherLogger))))
      }

    final def untyped: LoggerResource = new TypedLoggerResource[Unit] {
      override def use[T](f: TypedLogger[Unit] => T): T = one.use(l => f(l.untyped))
    }
  }

  def pure[U](logger: TypedLogger[U]): TypedLoggerResource[U] =
    new TypedLoggerResource[U] {
      override def use[T](f: TypedLogger[U] => T): T = f(logger)
    }
}
