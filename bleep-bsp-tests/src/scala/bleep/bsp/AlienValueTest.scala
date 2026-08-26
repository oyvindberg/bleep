package bleep.bsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Every test in this class passes bleep's own classloader. `underlying` is then a collection this JVM compares against a literal. A defect in these wrappers
  * appears as the wrong collection rather than as a `ClassCastException` from a second `scala-library`. The failure then points at the class that supplied a
  * `Method` rather than at the classloader.
  */
class AlienValueTest extends AnyFunSuite with Matchers {

  private val loader: ClassLoader = getClass.getClassLoader

  test("AlienMap.of builds an empty map") {
    AlienMap.of(Map.empty, loader).underlying shouldBe Map.empty
  }

  test("AlienMap.of builds a one-entry map") {
    AlienMap.of(Map("a" -> "1"), loader).underlying shouldBe Map("a" -> "1")
  }

  /** `Map.empty` is a `Map$EmptyMap$`. A one-entry map is a `Map$Map1`. A `Method` for `updated` that `Map$EmptyMap$` declares rejects a `Map$Map1` receiver.
    * The BSP server reaches that failure with any test environment with two or more variables.
    */
  test("AlienMap.of builds a six-entry map") {
    val entries = (1 to 6).map(i => s"k$i" -> s"v$i").toMap
    AlienMap.of(entries, loader).underlying shouldBe entries
  }

  test("AlienList.of and AlienList.elements round-trip a list of strings") {
    val alienList = AlienList.of(List("a", "b", "c"), loader)
    alienList.underlying shouldBe List("a", "b", "c")
    alienList.elements shouldBe List("a", "b", "c")
  }

  test("AlienList.elements extracts an empty list from Nil") {
    AlienList(Nil, loader).elements shouldBe Nil
  }

  /** `loadFrameworks` returns a `List[Option[Framework]]`. The elements of that alien list are themselves alien. */
  test("AlienOption casts an element of an alien list of options") {
    val alienList = AlienList.of(List(Option("a"), None), loader)
    alienList.elements.map(element => AlienOption(element, loader).as[String]) shouldBe List(Option("a"), None)
  }

  test("AlienOption.as extracts the value from Some") {
    AlienOption(Option("a"), loader).as[String] shouldBe Option("a")
  }

  test("AlienOption.as returns None for None") {
    AlienOption(None, loader).as[String] shouldBe None
  }
}
