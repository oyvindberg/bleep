package org.http4s

import _root_.munit._

class `QueryOpsDoctest` extends FunSuite {

  def sbtDoctestTypeEquals[A](a1: => A)(a2: => A): _root_.scala.Unit = {
    val _ = () => (a1, a2)
  }
  def sbtDoctestReplString(any: _root_.scala.Any): _root_.scala.Predef.String =
    _root_.com.github.tkawachi.doctest.DoctestRuntime.replStringOf(any)

  test("QueryOps.scala:56: ++?") {
    import org.http4s.implicits._

    // example at line 60: uri\"www.scala.com\".++?(\"key\" -> List(\"value1\", \"value2\", \"va ...
    sbtDoctestTypeEquals(uri"www.scala.com".++?("key" -> List("value1", "value2", "value3")))(
      uri"www.scala.com".++?("key" -> List("value1", "value2", "value3")): Uri
    )
    assertEquals(
      sbtDoctestReplString(uri"www.scala.com".++?("key" -> List("value1", "value2", "value3"))),
      "www.scala.com?key=value1&key=value2&key=value3",
    )
  }

}
