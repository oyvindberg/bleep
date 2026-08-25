package bleep.analysis

import bleep.bsp.SbtTestingBridge.ScalaColl
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** [[ScalaColl]] builds Scala collections reflectively so bleep can hand values to a platform test adapter loaded by a different classloader.
  *
  * Reflection over Scala's immutable collections is easy to get subtly wrong, because their *class* changes with their size: an immutable map is `EmptyMap$`,
  * then `Map1`, `Map2`, `Map3`, `Map4`, then `HashMap`. A `Method` resolved from one of those cannot be invoked on another, so anything that caches a `Method`
  * across a fold breaks at whichever size crosses the boundary — and works perfectly below it.
  *
  * That is exactly how the original bug escaped: `toMap` resolved `updated` once from the empty map and reused it, so zero entries and one entry were fine and
  * two were fatal. It reached users as a Scala.js test run that hung with no output.
  */
class ScalaCollReflectionTest extends AnyFunSuite with Matchers {

  private val loader: ClassLoader = getClass.getClassLoader

  private def roundTrip(entries: Map[String, String]): Map[String, String] = {
    val built = ScalaColl.toMap(entries, loader)
    built.asInstanceOf[Map[String, String]]
  }

  // Every size that changes the underlying class, plus one past the point where Scala switches to a HashMap.
  for (n <- 0 to 6)
    test(s"toMap builds a map of $n entries — the size at which its class changes is where reflection breaks") {
      val entries = (1 to n).map(i => s"key$i" -> s"value$i").toMap
      roundTrip(entries) shouldBe entries
    }

  test("toList and fromList round-trip") {
    val elems = List("a", "b", "c")
    ScalaColl.fromList[String](ScalaColl.toList(elems, loader), loader) shouldBe elems
  }
}
