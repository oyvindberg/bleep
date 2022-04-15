package bleep.internal

import scala.collection.immutable.SortedMap

// this exists to be able to lazily process things with dependencies
//  where processing one thing requires having processed its dependencies first
object rewriteDependentData {
  @FunctionalInterface
  trait Get[K, VV] {
    def apply(k: K): Lazy[VV]
  }

  def apply[K: Ordering, V](in: Map[K, V]) = new Api(in)

  final class Api[K: Ordering, V](in: Map[K, V]) {
    def eager[VV](f: (K, V, Get[K, VV]) => VV): SortedMap[K, VV] =
      apply(f).map { case (k, v) => (k, v.forceGet) }

    def startFrom[VV](include: K => Boolean)(f: (K, V, Get[K, VV]) => VV): SortedMap[K, VV] = {
      val lazyMap = apply(f)
      lazyMap.foreach {
        case (k, lazyV) if include(k) => lazyV.forceGet
        case _                        => ()
      }
      // this will include dependencies of what was chosen in `pred`
      lazyMap.flatMap { case (k, v) => v.getIfEvaluated.map(vv => (k, vv)) }
    }

    def apply[VV](f: (K, V, Get[K, VV]) => VV): SortedMap[K, Lazy[VV]] = {
      // sorted to ensure consistency
      val sortedIn = SortedMap.empty[K, V] ++ in

      lazy val get: Get[K, VV] = rewritten.apply

      lazy val rewritten: SortedMap[K, Lazy[VV]] =
        sortedIn.map { case (k, v) => k -> Lazy(f(k, v, get)) }

      rewritten
    }
  }
}
