package bleep

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RemoteCacheConfigTest extends AnyFunSuite with Matchers {
  // Deliberately not `Decoder[java.net.URI]`: resolved generically from outside `RemoteCacheConfig`'s own scope, that finds
  // `bleep.internal.codecs.codecURI` instead (no ${HOME_DIR} support) — a different instance than the one `deriveDecoder[RemoteCacheConfig]` actually
  // picks up from within its own companion object. Naming `RemoteCacheConfig.uriDecoder` explicitly tests the instance that's really in play.
  private def decodeUri(raw: String): java.net.URI =
    model.RemoteCacheConfig.uriDecoder.decodeJson(io.circe.Json.fromString(raw)).getOrElse(fail(s"could not decode '$raw' as a URI"))

  test("${HOME_DIR} expands to the running user's home directory") {
    val uri = decodeUri(s"file://${model.Replacements.known.HomeDir}/.cache/bleep-remote-cache")
    // Not the raw `user.home` system property: on Windows that's `C:\Users\x` (backslashes, no leading
    // slash), which the decoder itself normalizes into a valid `file://` URI path component before
    // substituting — see `RemoteCacheConfig.homeDirUriPath`. Asserting against the same normalization,
    // not the raw property, is what makes this test pass on every OS instead of only macOS/Linux.
    uri.toString shouldBe s"file://${model.RemoteCacheConfig.homeDirUriPath}/.cache/bleep-remote-cache"
  }

  test("a uri with no ${HOME_DIR} token is unaffected") {
    val uri = decodeUri("s3://my-bleep-cache/builds")
    uri.toString shouldBe "s3://my-bleep-cache/builds"
  }

  test("decoding a full RemoteCacheConfig, as bleep.yaml loading actually does, also expands ${HOME_DIR}") {
    val json = io.circe.Json.obj(
      "uri" -> io.circe.Json.fromString(s"file://${model.Replacements.known.HomeDir}/.cache/bleep-remote-cache"),
      "region" -> io.circe.Json.Null
    )
    val config = model.RemoteCacheConfig.decoder.decodeJson(json).getOrElse(fail("could not decode RemoteCacheConfig"))
    config.uri.toString shouldBe s"file://${model.RemoteCacheConfig.homeDirUriPath}/.cache/bleep-remote-cache"
  }
}
