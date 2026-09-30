package org.http4s

import _root_.munit._

class `HeadersDoctest` extends FunSuite {

  def sbtDoctestTypeEquals[A](a1: => A)(a2: => A): _root_.scala.Unit = {
    val _ = () => (a1, a2)
  }
  def sbtDoctestReplString(any: _root_.scala.Any): _root_.scala.Predef.String =
    _root_.com.github.tkawachi.doctest.DoctestRuntime.replStringOf(any)

  test("Headers.scala:95: withContentLength") {
    import org.http4s.headers._

    val chunked =
      Headers(`Transfer-Encoding`(TransferCoding.chunked), `Content-Type`(MediaType.text.plain))

    // example at line 105: chunked.withContentLength(`Content-Length`.unsafeFromLong(10 ...
    sbtDoctestTypeEquals(chunked.withContentLength(`Content-Length`.unsafeFromLong(1024)))(
      chunked.withContentLength(`Content-Length`.unsafeFromLong(1024)): Headers
    )
    assertEquals(
      sbtDoctestReplString(chunked.withContentLength(`Content-Length`.unsafeFromLong(1024))),
      "Headers(Content-Length: 1024, Content-Type: text/plain)",
    )

    val chunkedGzipped = Headers(
      `Transfer-Encoding`(TransferCoding.chunked, TransferCoding.gzip),
      `Content-Type`(MediaType.text.plain),
    )

    // example at line 111: chunkedGzipped.withContentLength(`Content-Length`.unsafeFrom ...
    sbtDoctestTypeEquals(chunkedGzipped.withContentLength(`Content-Length`.unsafeFromLong(1024)))(
      chunkedGzipped.withContentLength(`Content-Length`.unsafeFromLong(1024)): Headers
    )
    assertEquals(
      sbtDoctestReplString(chunkedGzipped.withContentLength(`Content-Length`.unsafeFromLong(1024))),
      "Headers(Content-Length: 1024, Transfer-Encoding: gzip, Content-Type: text/plain)",
    )

    val const = Headers(`Content-Length`(2048), `Content-Type`(MediaType.text.plain))

    // example at line 117: const.withContentLength(`Content-Length`.unsafeFromLong(1024 ...
    sbtDoctestTypeEquals(const.withContentLength(`Content-Length`.unsafeFromLong(1024)))(
      const.withContentLength(`Content-Length`.unsafeFromLong(1024)): Headers
    )
    assertEquals(
      sbtDoctestReplString(const.withContentLength(`Content-Length`.unsafeFromLong(1024))),
      "Headers(Content-Length: 1024, Content-Type: text/plain)",
    )
  }

}
