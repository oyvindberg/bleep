package bleep.bsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Every test in this class passes bleep's own classloader to `ScalaCollectionReflection`. That object builds collections inside the classloader a `TestAdapter` brings.
  * A defect in that object comes from the class that supplied a `Method` rather than from the classloader.
  */
class ScalaCollectionReflectionTest extends AnyFunSuite with Matchers {

  private val loader: ClassLoader = getClass.getClassLoader

  test("toScalaMap builds an empty map") {
    ScalaCollectionReflection.toScalaMap(Map.empty, loader) shouldBe Map.empty
  }

  test("toScalaMap builds a one-entry map") {
    ScalaCollectionReflection.toScalaMap(Map("a" -> "1"), loader) shouldBe Map("a" -> "1")
  }

  /** `Map.empty` is a `Map$EmptyMap$`. A one-entry map is a `Map$Map1`. A `Method` for `updated` taken from `Map$EmptyMap$` rejects a `Map$Map1` receiver. The
    * BSP server reaches that failure with any test environment of two or more variables.
    */
  test("toScalaMap builds a map whose every intermediate size is a different class") {
    val entries = (1 to 6).map(i => s"k$i" -> s"v$i").toMap
    ScalaCollectionReflection.toScalaMap(entries, loader) shouldBe entries
  }

  test("toScalaList and fromScalaList round-trip") {
    val scalaList = ScalaCollectionReflection.toScalaList(List("a", "b", "c"), loader)
    scalaList shouldBe List("a", "b", "c")
    ScalaCollectionReflection.fromScalaList[String](scalaList, loader) shouldBe List("a", "b", "c")
  }

  test("fromScalaList reads the empty list") {
    ScalaCollectionReflection.fromScalaList[String](Nil, loader) shouldBe Nil
  }

  test("fromScalaOption reads Some and None") {
    ScalaCollectionReflection.fromScalaOption[String](Some("a"), loader) shouldBe Some("a")
    ScalaCollectionReflection.fromScalaOption[String](None, loader) shouldBe None
  }
}
