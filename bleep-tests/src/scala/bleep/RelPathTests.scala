package bleep

import org.scalactic.TripleEqualsSupport
import org.scalatest.funsuite.AnyFunSuite

class RelPathTests extends AnyFunSuite with TripleEqualsSupport {
  test("works") {
    val expected = "../../Foo"
    val actual = RelPath.force(expected)
    assertResult(expected)(actual.asString)
  }
}
