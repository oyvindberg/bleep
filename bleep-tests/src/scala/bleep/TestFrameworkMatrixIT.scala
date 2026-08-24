package bleep

/** `bleep test` run end to end for every (platform, Scala version, framework) combination bleep claims to support.
  *
  * One suite per (platform, Scala version) rather than one per platform. Each test drives a complete inner build — resolve, compile, link, discover, run — and
  * the harness's in-process BSP transport (`InProcessBspServer`, client and server joined by `PipedInputStream`/`PipedOutputStream`) stops delivering requests
  * after roughly five of those in a single suite: the server parks in `PipedInputStream.read` waiting for a request while the client parks in
  * `CompletableFuture.get()` waiting for the reply to one it believes it sent. Three inner builds per suite stays well inside that, and the suites run in
  * parallel, which the single large suite could not. Production BSP speaks over a socket, so nothing here reflects how bleep behaves for a user.
  *
  * Every suite is named `*IT`, which puts it under the `slow` tag `bleep-tests` already declares for `**IT` — the `build` job runs them, the native-image jobs
  * skip them with `--exclude-tag slow`.
  */
abstract class TestFrameworkMatrixIT(platform: FixturePlatform) extends IntegrationTestHarness with PlatformFrameworkHarness {
  TestFrameworkFixture.forPlatform(platform.id).foreach { fixture =>
    integrationTest(s"${platform.describe} / ${fixture.name}") { ws =>
      checkFixture(ws, fixture, platform)
    }
  }
}

/** The JVM is the control: its runner is the one with real prior coverage, so a fixture that cannot pass here is a broken fixture, not a broken runner. That is
  * what makes the same fixture failing on Scala.js or Scala Native evidence about bleep.
  */
class JvmScala3TestFrameworkIT extends TestFrameworkMatrixIT(FixturePlatform.Jvm(model.Versions.Scala3))
class JvmScala213TestFrameworkIT extends TestFrameworkMatrixIT(FixturePlatform.Jvm(model.Versions.Scala213))

/** Issue #655: `bleep test` ran no Scala.js suite at all — munit failed to load, utest loaded and executed nothing. Neither was visible to the previous tests,
  * which fed `ScalaJsTestRunner` hand-written `.js` files printing a protocol only bleep's injected harness spoke, so no linked Scala.js program ever appeared.
  */
class ScalaJsScala3TestFrameworkIT extends TestFrameworkMatrixIT(FixturePlatform.Js(model.Versions.Scala3, model.Versions.ScalaJs1, model.Versions.Node))
class ScalaJsScala213TestFrameworkIT extends TestFrameworkMatrixIT(FixturePlatform.Js(model.Versions.Scala213, model.Versions.ScalaJs1, model.Versions.Node))

/** Scala Native production runs `runTestsViaAdapter`, whose only test was `ignore`d as a "known RPC protocol issue"; the green suites beside it drive
  * `runTests`, the stdout-parsing method production no longer calls, through hand-written `TestRunner` mains.
  */
class ScalaNativeScala3TestFrameworkIT extends TestFrameworkMatrixIT(FixturePlatform.Native(model.Versions.Scala3, model.Versions.ScalaNative05))
class ScalaNativeScala213TestFrameworkIT extends TestFrameworkMatrixIT(FixturePlatform.Native(model.Versions.Scala213, model.Versions.ScalaNative05))
