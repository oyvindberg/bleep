package org.http4s.headers

import _root_.munit._

class `Transfer-EncodingDoctest` extends FunSuite {

  def sbtDoctestTypeEquals[A](a1: => A)(a2: => A): _root_.scala.Unit = {
    val _ = () => (a1, a2)
  }
  def sbtDoctestReplString(any: _root_.scala.Any): _root_.scala.Predef.String =
    _root_.com.github.tkawachi.doctest.DoctestRuntime.replStringOf(any)

  test("Transfer-Encoding.scala:53: filter") {
    import org.http4s._

    val te = `Transfer-Encoding`(TransferCoding.chunked, TransferCoding.gzip)

    // example at line 59: te.filter(_ != TransferCoding.chunked)
    sbtDoctestTypeEquals(te.filter(_ != TransferCoding.chunked))(
      te.filter(_ != TransferCoding.chunked): Option[`Transfer-Encoding`]
    )
    assertEquals(
      sbtDoctestReplString(te.filter(_ != TransferCoding.chunked)),
      "Some(Transfer-Encoding(NonEmptyList(TransferCoding(gzip))))",
    )

    // example at line 61: te.filter(_ => false)
    sbtDoctestTypeEquals(te.filter(_ => false))(te.filter(_ => false): Option[`Transfer-Encoding`])
    assertEquals(sbtDoctestReplString(te.filter(_ => false)), "None")
  }

}
