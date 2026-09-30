package org.http4s

import _root_.munit._

class `MessageDoctest` extends FunSuite {

  def sbtDoctestTypeEquals[A](a1: => A)(a2: => A): _root_.scala.Unit = {
    val _ = () => (a1, a2)
  }
  def sbtDoctestReplString(any: _root_.scala.Any): _root_.scala.Predef.String =
    _root_.com.github.tkawachi.doctest.DoctestRuntime.replStringOf(any)

  test("Message.scala:145: putHeaders") {
    import org.http4s.headers.Accept

    val req = Request().putHeaders(Accept(MediaRange.`application/*`))

    // example at line 152: req.headers.get[Accept]

    assertEquals(
      sbtDoctestReplString(req.headers.get[Accept]),
      "Some(Accept(NonEmptyList(application/*)))",
    )

    val req2 = req.putHeaders(Accept(MediaRange.`text/*`))

    // example at line 156: req2.headers.get[Accept]

    assertEquals(
      sbtDoctestReplString(req2.headers.get[Accept]),
      "Some(Accept(NonEmptyList(text/*)))",
    )
  }

  test("Message.scala:164: addHeader") {
    import org.http4s.headers.Accept

    val req = Request().addHeader(Accept(MediaRange.`application/*`))

    // example at line 172: req.headers.get[Accept]

    assertEquals(
      sbtDoctestReplString(req.headers.get[Accept]),
      "Some(Accept(NonEmptyList(application/*)))",
    )

    val req2 = req.addHeader(Accept(MediaRange.`text/*`))

    // example at line 176: req2.headers.get[Accept]

    assertEquals(
      sbtDoctestReplString(req2.headers.get[Accept]),
      "Some(Accept(NonEmptyList(application/*, text/*)))",
    )
  }

}
