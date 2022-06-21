package bleep

import coursier.Dependency
import io.circe.Encoder
import io.circe.syntax.EncoderOps

package object internal {

  def assertUsed(anies: Any*): Unit = ((), anies)._1

  implicit val dependencyOrdering: Ordering[Dependency] =
    Ordering.by(_.toString())

  implicit class IterableOps[I[t] <: Iterable[t], T](private val ts: I[T]) extends AnyVal {
    // surprisingly difficult to express with default collections
    def optReduce(op: (T, T) => Option[T]): Option[T] = {
      val it = ts.iterator
      var acc: Option[T] = None
      var first = true
      while (it.hasNext && (acc.nonEmpty || first)) {
        val x = it.next()
        if (first) {
          acc = Some(x)
          first = false
        } else {
          acc = op(acc.get, x)
        }
      }
      acc
    }

    def firstDefined[U](f: T => Option[U]): Option[U] = {
      val it = ts.iterator
      while (it.hasNext) {
        val ou = f(it.next())
        if (ou.isDefined) return ou
      }
      None
    }
  }

  def throwableMessages(th: Throwable): List[String] =
    th.getMessage :: Option(th.getCause).toList.flatMap(throwableMessages)

  private val printer = new io.circe.yaml24.Printer(
    preserveOrder = true,
    dropNullKeys = true
  )

  def asYamlString[T: Encoder](t: T): String =
    printer.pretty(t.asJson.foldWith(ShortenAndSortJson))
}
