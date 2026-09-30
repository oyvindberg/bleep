package org.http4s

import _root_.munit._

class `HttpVersionDoctest` extends FunSuite {

  def sbtDoctestTypeEquals[A](a1: => A)(a2: => A): _root_.scala.Unit = {
    val _ = () => (a1, a2)
  }
  def sbtDoctestReplString(any: _root_.scala.Any): _root_.scala.Predef.String =
    _root_.com.github.tkawachi.doctest.DoctestRuntime.replStringOf(any)

  test("HttpVersion.scala:47: render") {
    // example at line 50: HttpVersion.`HTTP/1.1`.renderString

    assertEquals(sbtDoctestReplString(HttpVersion.`HTTP/1.1`.renderString), "HTTP/1.1")
  }

  test("HttpVersion.scala:56: compare") {
    // example at line 59: List(HttpVersion.`HTTP/1.0`, HttpVersion.`HTTP/1.1`, HttpVer ...

    assertEquals(
      sbtDoctestReplString(
        List(HttpVersion.`HTTP/1.0`, HttpVersion.`HTTP/1.1`, HttpVersion.`HTTP/0.9`).sorted
      ),
      "List(HTTP/0.9, HTTP/1.0, HTTP/1.1)",
    )
  }

  test("HttpVersion.scala:152: fromString") {
    // example at line 155: HttpVersion.fromString(\"HTTP/1.1\")

    assertEquals(sbtDoctestReplString(HttpVersion.fromString("HTTP/1.1")), "Right(HTTP/1.1)")
  }

  test("HttpVersion.scala:177: fromVersion") {
    // example at line 180: HttpVersion.fromVersion(1, 1)

    assertEquals(sbtDoctestReplString(HttpVersion.fromVersion(1, 1)), "Right(HTTP/1.1)")

    // example at line 183: HttpVersion.fromVersion(1, 10)

    assertEquals(
      sbtDoctestReplString(HttpVersion.fromVersion(1, 10)),
      "Left(org.http4s.ParseFailure: Invalid HTTP version: major must be <= 9: 10)",
    )
  }

}
