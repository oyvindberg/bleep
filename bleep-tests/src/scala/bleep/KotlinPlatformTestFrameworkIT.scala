package bleep

/** `bleep test` end to end on Kotlin/JS and Kotlin/Native.
  *
  * These two platforms had no framework coverage at all. `KotlinJsIntegrationTest` and `KotlinNativeIntegrationTest` test compiler configuration — resolver
  * caching, module kinds, output modes — and never run a suite, so `KotlinTestRunner` was reached only in production.
  *
  * That is the same gap issue #655 lived in, and the same shape of mechanism: bleep links the test artifact, then talks to it over a protocol of its own making
  * — QUnit stubs injected around the Kotlin/JS output, a `##kotlin-test##` line format parsed back on both platforms. A private protocol with no end-to-end
  * test is exactly what let Scala.js report success while executing nothing.
  */
abstract class KotlinPlatformTestFrameworkIT(platform: FixturePlatform) extends IntegrationTestHarness with PlatformFrameworkHarness {
  TestFrameworkFixture.pinnedFor(platform.id, platform.scalaBinaryVersion).foreach { case (fixture, version) =>
    integrationTest(s"${platform.describe} / ${fixture.name} $version") { ws =>
      checkFixture(ws, fixture, version, platform)
    }
  }
}

class KotlinJsTestFrameworkIT extends KotlinPlatformTestFrameworkIT(FixturePlatform.KotlinJs(model.Versions.Kotlin24, model.Versions.Node))
class KotlinNativeTestFrameworkIT extends KotlinPlatformTestFrameworkIT(FixturePlatform.KotlinNative(model.Versions.Kotlin24))
