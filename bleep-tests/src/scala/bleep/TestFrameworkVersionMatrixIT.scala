package bleep

/** The same end-to-end check as [[TestFrameworkMatrixIT]], swept across every framework version and Scala version each fixture declares.
  *
  * ==Why this is separate, and not run automatically==
  *
  * This is the coverage that catches the bugs nobody sees coming, because nobody upgrades a build tool and a five-year-old test framework on the same day. An
  * older release of a framework fingerprints its suites differently, names its tests differently, and in several cases predates the platform artifact bleep
  * injects alongside it — every one of those is a way for discovery or runner selection to be wrong for a real user while the pinned matrix stays green.
  *
  * It is also, unavoidably, an enormous amount of work: one full inner build per combination, resolved from Maven Central, compiled, linked where the platform
  * needs linking, and run. Putting that in front of every push would buy very little and cost a great deal, since the axes only move when someone edits this
  * file. So it carries its own `matrix` tag and CI excludes it. Run it deliberately, when changing anything about discovery, runner selection or the fork
  * protocol:
  *
  * {{{
  * bleep test bleep-tests --only-tag matrix
  * bleep test bleep-tests --only bleep.ScalaJsScala3TestFrameworkVersionIT   # or one leg of it
  * }}}
  *
  * ==Adding coverage==
  *
  * Add a version to a fixture's `versions` list, or a Scala binary version to its `scalaBinaryVersions`, and it appears here. A combination that does not exist
  * — a framework that never published for 2.12, say — must be excluded there rather than left to fail, because a resolution error in this matrix reads as a
  * bleep defect and trains people to ignore the red.
  */
abstract class TestFrameworkVersionMatrixIT(platform: FixturePlatform) extends IntegrationTestHarness with PlatformFrameworkHarness {
  TestFrameworkFixture.sweepFor(platform.id, platform.scalaBinaryVersion).foreach { case (fixture, version) =>
    integrationTest(s"${platform.describe} / ${fixture.name} $version") { ws =>
      checkFixture(ws, fixture, version, platform)
    }
  }
}

class JvmScala3TestFrameworkVersionIT extends TestFrameworkVersionMatrixIT(FixturePlatform.Jvm(model.Versions.Scala3))
class JvmScala213TestFrameworkVersionIT extends TestFrameworkVersionMatrixIT(FixturePlatform.Jvm(model.Versions.Scala213))
class JvmScala212TestFrameworkVersionIT extends TestFrameworkVersionMatrixIT(FixturePlatform.Jvm(model.Versions.Scala212))

class ScalaJsScala3TestFrameworkVersionIT
    extends TestFrameworkVersionMatrixIT(FixturePlatform.Js(model.Versions.Scala3, model.Versions.ScalaJs1, model.Versions.Node))
class ScalaJsScala213TestFrameworkVersionIT
    extends TestFrameworkVersionMatrixIT(FixturePlatform.Js(model.Versions.Scala213, model.Versions.ScalaJs1, model.Versions.Node))
class ScalaJsScala212TestFrameworkVersionIT
    extends TestFrameworkVersionMatrixIT(FixturePlatform.Js(model.Versions.Scala212, model.Versions.ScalaJs1, model.Versions.Node))

class ScalaNativeScala3TestFrameworkVersionIT extends TestFrameworkVersionMatrixIT(FixturePlatform.Native(model.Versions.Scala3, model.Versions.ScalaNative05))
class ScalaNativeScala213TestFrameworkVersionIT
    extends TestFrameworkVersionMatrixIT(FixturePlatform.Native(model.Versions.Scala213, model.Versions.ScalaNative05))
class ScalaNativeScala212TestFrameworkVersionIT
    extends TestFrameworkVersionMatrixIT(FixturePlatform.Native(model.Versions.Scala212, model.Versions.ScalaNative05))
