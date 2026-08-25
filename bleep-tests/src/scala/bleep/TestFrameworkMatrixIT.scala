package bleep

/** `bleep test` run end to end for every (platform, Scala version, framework) combination bleep claims to support.
  *
  * Each test drives a complete inner build — resolve, compile, link, discover, run — through the real `bleep test` entry point, and asserts on the JUnit XML
  * that came out. Nothing is stubbed. A runner that discovers a suite and then executes none of its tests fails here, which is what issue #655 was.
  *
  * These suites pin one version per framework: the current one. The other versions each fixture declares are swept by [[TestFrameworkVersionMatrixIT]], which
  * is not run automatically.
  *
  * Every suite is named `*IT`, which puts it under the `slow` tag `bleep-tests` declares for `**IT` — the `build` job runs them, the native-image jobs skip
  * them with `--exclude-tag slow`.
  */
abstract class TestFrameworkMatrixIT(platform: FixturePlatform) extends IntegrationTestHarness with PlatformFrameworkHarness {
  TestFrameworkFixture.pinnedFor(platform.id, platform.scalaBinaryVersion).foreach { case (fixture, version) =>
    integrationTest(s"${platform.describe} / ${fixture.name} $version") { ws =>
      checkFixture(ws, fixture, version, platform)
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

/** Scala Native production runs `runTestsViaAdapter`, whose only test was `ignore`d as a "known RPC protocol issue"; the green suites beside it drove
  * `runTests`, the stdout-parsing method production no longer calls, through hand-written `TestRunner` mains.
  */
class ScalaNativeScala3TestFrameworkIT extends TestFrameworkMatrixIT(FixturePlatform.Native(model.Versions.Scala3, model.Versions.ScalaNative05))
class ScalaNativeScala213TestFrameworkIT extends TestFrameworkMatrixIT(FixturePlatform.Native(model.Versions.Scala213, model.Versions.ScalaNative05))
