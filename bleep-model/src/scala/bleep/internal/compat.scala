package bleep.internal

import java.util.Optional

// between 2.12 and 2.13
object compat {
  implicit class OptionalCompatOps[A](private val oa: Optional[A]) extends AnyVal {
    def toScalaCompat: Option[A] = if (oa.isPresent) Some(oa.get) else None
  }

  implicit class JavaListCompatOps[A](private val as: java.util.List[A]) extends AnyVal {
    def toScalaCompat: List[A] = {
      val b = List.newBuilder[A]
      as.forEach(a => b += a)
      b.result()
    }
  }

  implicit class OptionCompatOps[A](private val oa: Option[A]) extends AnyVal {
    def zipCompat[A2](oa2: Option[A2]): Option[(A, A2)] =
      for {
        a <- oa
        a2 <- oa2
      } yield (a, a2)
  }

  implicit class ListCompatOps[A](private val as: List[A]) extends AnyVal {
    def maxOptionCompat[B >: A](implicit ord: Ordering[B]): Option[A] =
      if (as.isEmpty) None else Some(as.max(ord))
  }

  implicit class IteratorCompatOps[A](private val as: Array[A]) extends AnyVal {
    def maxByOptionCompat[B](f: A => B)(implicit cmp: Ordering[B]): Option[A] =
      if (as.isEmpty) None else Some(as.maxBy(f))
  }
}
