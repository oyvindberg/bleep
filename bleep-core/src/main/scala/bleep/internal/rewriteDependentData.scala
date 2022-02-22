package bleep.internal

import scala.collection.immutable.SortedMap

// this exists to be able to lazily process things with dependencies
//  where processing one thing requires having processed its dependencies first
object rewriteDependentData {
  @FunctionalInterface
  trait Get[K, VV] {
    def apply(k: K): Lazy[VV]
  }

  // hide `Lazy` and out tparam
  def eager[K: Ordering, V](in: Map[K, V])(f: (K, V, Get[K, V]) => V): SortedMap[K, V] =
    apply[K, V, V](in)(f).map { case (k, v) => (k, v.forceGet) }

  def apply[K: Ordering, V, VV](in: Map[K, V])(f: (K, V, Get[K, VV]) => VV): SortedMap[K, Lazy[VV]] = {
    // sorted to ensure consistency
    val sortedIn = SortedMap.empty[K, V] ++ in

    lazy val get: Get[K, VV] = rewritten.apply

    lazy val rewritten: SortedMap[K, Lazy[VV]] =
      sortedIn.map { case (k, v) => k -> Lazy(f(k, v, get)) }

    rewritten
  }
}
