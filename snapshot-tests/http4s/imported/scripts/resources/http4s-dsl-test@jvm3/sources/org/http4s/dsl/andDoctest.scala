package org.http4s.dsl

import _root_.munit._

class `andDoctest` extends FunSuite {

  def sbtDoctestTypeEquals[A](a1: => A)(a2: => A): _root_.scala.Unit = {
    val _ = () => (a1, a2)
  }
  def sbtDoctestReplString(any: _root_.scala.Any): _root_.scala.Predef.String =
    _root_.com.github.tkawachi.doctest.DoctestRuntime.replStringOf(any)

  test("and.scala:19: &") {
    object Even { def unapply(i: Int) = (i % 2) == 0 }

    object Positive { def unapply(i: Int) = i > 0 }

    def describe(i: Int) = i match {
      case org.http4s.dsl.&(Even(), Positive()) => "even and positive"
      case Even() => "even but not positive"
      case Positive() => "positive but not even"
      case _ => "neither even nor positive"
    }

    // example at line 30: describe(-1)
    sbtDoctestTypeEquals(describe(-1))(describe(-1): String)
    assertEquals(sbtDoctestReplString(describe(-1)), "neither even nor positive")

    // example at line 32: describe(0)
    sbtDoctestTypeEquals(describe(0))(describe(0): String)
    assertEquals(sbtDoctestReplString(describe(0)), "even but not positive")

    // example at line 34: describe(1)
    sbtDoctestTypeEquals(describe(1))(describe(1): String)
    assertEquals(sbtDoctestReplString(describe(1)), "positive but not even")

    // example at line 36: describe(2)
    sbtDoctestTypeEquals(describe(2))(describe(2): String)
    assertEquals(sbtDoctestReplString(describe(2)), "even and positive")
  }

}
