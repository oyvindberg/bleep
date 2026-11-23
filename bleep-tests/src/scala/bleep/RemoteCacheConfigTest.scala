package bleep

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

class RemoteCacheConfigTest extends AnyFunSuite with Matchers {
  test("RemoteCacheConfig decoding does NOT touch ${HOME_DIR} — that stays the caller's job, parametrized via Replacements") {
    val json = io.circe.Json.obj(
      "uri" -> io.circe.Json.fromString(s"file://${model.Replacements.known.HomeDir}/.cache/bleep-remote-cache"),
      "region" -> io.circe.Json.Null
    )
    val config = model.RemoteCacheConfig.decoder.decodeJson(json).getOrElse(fail("could not decode RemoteCacheConfig"))
    // The literal token survives decoding unresolved: bleep-model holds data, not the logic (or the
    // JVM-environment dependency) that resolving it against a real home directory requires.
    config.uri shouldBe model.TemplatedUri(s"file://${model.Replacements.known.HomeDir}/.cache/bleep-remote-cache")
  }

  // `TemplatedUri.resolve` is parametrized on `Replacements` rather than reading `UserPaths.fromAppDirs` itself. Deliberately NOT a test with a hand-built
  // Windows-shaped path: `Path.of` is platform-native, not a cross-platform path simulator — `Path.of("C:\Users\x")` on macOS/Linux parses as a *relative*
  // POSIX path with literal backslashes in the filename, not an absolute Windows one, so faking the Windows case this way silently tests the wrong thing.
  // The actual platform-specific part — turning a home directory into a valid `file://` URI path component on whichever OS this runs on — is `Path.toUri()`
  // itself, delegated to the JDK rather than hand-rolled here (hand-rolling it is exactly what broke on Windows CI before this fix). So the expected value
  // below is computed the same way `resolve` computes it, not hardcoded to one OS's shape: this test covers `resolve`'s own substitution logic, which is
  // genuinely platform-agnostic, and real coverage of `Path.toUri()`'s Windows behavior is the Windows CI run itself exercising a real Windows `Path`.
  private def homeDirReplacements(homeDir: Path): model.Replacements =
    model.Replacements.ofReplacements(List(homeDir.toUri.getRawPath.stripSuffix("/") -> model.Replacements.known.HomeDir))

  test("${HOME_DIR} expands against this process's real home directory, on whichever OS runs the test") {
    val homeDir = Path.of(System.getProperty("user.home"))
    val uri = model.TemplatedUri(s"file://${model.Replacements.known.HomeDir}/.cache/bleep-remote-cache").resolve(homeDirReplacements(homeDir))
    uri.toString shouldBe s"file://${homeDir.toUri.getRawPath.stripSuffix("/")}/.cache/bleep-remote-cache"
  }

  test("a uri with no ${HOME_DIR} token is unaffected") {
    val uri = model.TemplatedUri("s3://my-bleep-cache/builds").resolve(homeDirReplacements(Path.of(System.getProperty("user.home"))))
    uri.toString shouldBe "s3://my-bleep-cache/builds"
  }
}
