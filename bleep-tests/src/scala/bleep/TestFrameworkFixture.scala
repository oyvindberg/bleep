package bleep

/** Which language a fixture's source is written in, and therefore where it lives and how its project is configured. */
sealed abstract class FixtureLanguage(val sourceDir: String)
object FixtureLanguage {
  case object Scala extends FixtureLanguage("scala")
  case object Java extends FixtureLanguage("java")
  case object Kotlin extends FixtureLanguage("kotlin")
}

/** One test framework, as build dependencies plus a suite source.
  *
  * The same fixture runs on every platform that claims to support the framework, and the source is byte-identical across them — that identity is the assertion.
  * `bleep test` on a JVM project and on a Scala.js project compile the same file, so any difference in the reported result is bleep's, not the test's.
  *
  * Every fixture declares two passing tests and one failing one. A suite that only passes cannot distinguish "ran and passed" from "did not run": issue #655 is
  * precisely a runner that reported no individual results, and a green-only fixture would have been satisfied by a runner that executed nothing. The failing
  * test also proves the failure path reports a count rather than swallowing the assertion.
  */
case class TestFrameworkFixture(
    /** Short name used in test names. */
    name: String,
    /** Dependency lines for bleep.yaml, as a function of the framework version under test. `::` for Scala artifacts so the platform suffix (`_sjs1`,
      * `_native0.5`) is applied per project; `:` for Java ones.
      */
    deps: String => List[String],
    /** Framework versions to exercise, newest first.
      *
      * The head is the version [[TestFrameworkMatrixIT]] pins, and is what CI runs. The tail exists for [[TestFrameworkVersionMatrixIT]], which is not run
      * automatically: a framework's older releases fingerprint their suites differently, name their tests differently, and in several cases predate the
      * platform artifact bleep injects alongside them. Those are the combinations where bleep's discovery and runner selection are most likely to be wrong, and
      * the least likely to be noticed, because nobody upgrades a build tool and an old test framework on the same day.
      */
    versions: List[String],
    language: FixtureLanguage,
    /** Path under the source directory, e.g. `example/MunitFixture.scala`. */
    relPath: String,
    /** Fully qualified suite name, as `bleep test` reports it. */
    suiteFqn: String,
    source: String,
    /** Test names as written in the source. */
    testNames: List[String],
    /** The one test that must fail an assertion, as written in the source. */
    failingTestName: String,
    /** The one test that must throw an uncaught exception, as written in the source.
      *
      * Separate from [[failingTestName]] because the two travel different paths. sbt-testing has distinct `Status.Failure` and `Status.Error`, JUnit XML has
      * `<failure>` and `<error>`, and a runner can get one right while swallowing the other — an exception thrown outside an assertion is the one most likely
      * to escape a framework's own reporting and reach bleep as a dead suite rather than a failed test.
      */
    throwingTestName: String,
    /** How this framework renders a test name to sbt-testing.
      *
      * Frameworks disagree, and the difference is visible to users in `bleep test` output and in JUnit XML: munit prefixes the suite's fully qualified name,
      * ScalaTest and utest report the bare name, JUnit appends parentheses. Pinning it per framework keeps the assertion exact instead of loosening it to a
      * suffix match that a runner reporting one synthetic case could satisfy.
      */
    reportedName: String => String,
    /** Platform ids this framework publishes for. A framework absent from a platform is not a bleep defect, so it is simply not run there. */
    platforms: Set[String],
    /** Scala binary versions this framework publishes for, *at a given framework version*.
      *
      * A function of the version rather than a flat set, because support moves over a framework's life: scalatest 3.1.4 predates Scala 3 entirely and there is
      * no `scalatest_3:3.1.4` to resolve, while 3.2.15 beside it is fine. A combination that was never published has to be skipped rather than attempted — the
      * failure is a coursier `CantDownloadModule`, which reads exactly like a bleep defect and would train people to ignore red in this matrix.
      *
      * Java and Kotlin fixtures declare all of them: their artifacts carry no Scala suffix, so the project's Scala version cannot affect whether they resolve.
      */
    scalaBinaryVersions: String => Set[String]
) {

  /** The version CI pins. Every other version in [[versions]] is reached only by the manual matrix. */
  def currentVersion: String = versions.head

  def supports(platformId: String, scalaBinaryVersion: String, frameworkVersion: String): Boolean =
    platforms.contains(platformId) && scalaBinaryVersions(frameworkVersion).contains(scalaBinaryVersion)

  def passingTestNames: List[String] = testNames.filterNot(n => n == failingTestName || n == throwingTestName).map(reportedName)
  def reportedFailingName: String = reportedName(failingTestName)
  def reportedThrowingName: String = reportedName(throwingTestName)

  /** Both non-passing cases, as the report will name them. Not split by `failure` vs `error`: sbt-testing lets a framework choose, and several report an
    * uncaught exception as a Failure carrying the throwable. What must hold is that neither case is reported as passing and neither goes missing.
    */
  def reportedNotPassingNames: List[String] = List(reportedFailingName, reportedThrowingName).sorted
  def expectedPassed: Int = passingTestNames.size
  def expectedTotal: Int = testNames.size
}

object TestFrameworkFixture {
  private val AllPlatforms = Set("jvm", "js", "native")
  private val JvmOnly = Set("jvm")
  private val AllScalaBinaryVersions = Set("2.12", "2.13", "3")

  val munit: TestFrameworkFixture = TestFrameworkFixture(
    name = "munit",
    deps = v => List(s"org.scalameta::munit:$v"),
    versions = List(model.Versions.Munit, "1.0.0", "0.7.29"),
    language = FixtureLanguage.Scala,
    relPath = "example/MunitFixture.scala",
    suiteFqn = "example.MunitFixture",
    // munit's Framework declares SubclassFingerprint(superclassName = "munit.Suite", isModule = false), so a munit suite is a *class*. A runner that only ever
    // looks up module accessors cannot load it — that is the munit half of #655, and the reason this fixture must stay a class.
    source = """package example
               |
               |class MunitFixture extends munit.FunSuite {
               |  test("adds") { assertEquals(1 + 1, 2) }
               |  test("measures") { assertEquals("hello".length, 5) }
               |  test("fails on purpose") { assertEquals(1, 2) }
               |  test("throws on purpose") { throw new RuntimeException("boom") }
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    reportedName = name => s"example.MunitFixture.$name",
    platforms = AllPlatforms,
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  val scalatest: TestFrameworkFixture = TestFrameworkFixture(
    name = "scalatest",
    deps = v => List(s"org.scalatest::scalatest:$v"),
    versions = List("3.2.19", "3.2.15", "3.1.4"),
    language = FixtureLanguage.Scala,
    relPath = "example/ScalatestFixture.scala",
    suiteFqn = "example.ScalatestFixture",
    source = """package example
               |
               |import org.scalatest.funsuite.AnyFunSuite
               |
               |class ScalatestFixture extends AnyFunSuite {
               |  test("adds") { assert(1 + 1 == 2) }
               |  test("measures") { assert("hello".length == 5) }
               |  test("fails on purpose") { assert(1 == 2) }
               |  test("throws on purpose") { throw new RuntimeException("boom") }
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    reportedName = identity,
    platforms = AllPlatforms,
    // 3.1.4 predates Scala 3: there is no `scalatest_3:3.1.4` on Maven Central, so that row is skipped rather than left to fail resolution.
    scalaBinaryVersions = {
      case "3.1.4" => Set("2.12", "2.13")
      case _       => AllScalaBinaryVersions
    }
  )

  val utest: TestFrameworkFixture = TestFrameworkFixture(
    name = "utest",
    deps = v => List(s"com.lihaoyi::utest:$v"),
    versions = List("0.9.1", "0.8.5", "0.7.11"),
    language = FixtureLanguage.Scala,
    relPath = "example/UtestFixture.scala",
    suiteFqn = "example.UtestFixture",
    // A utest suite is an object, the opposite of munit's class. Both shapes have to work, and #655 reports the object shape failing too — it loaded and then
    // died inside `TestRunner.runAsync` — so this is not redundant with the munit fixture.
    source = """package example
               |
               |import utest._
               |
               |object UtestFixture extends TestSuite {
               |  val tests = Tests {
               |    test("adds") { assert(1 + 1 == 2) }
               |    test("measures") { assert("hello".length == 5) }
               |    test("fails on purpose") { assert(1 == 2) }
               |    test("throws on purpose") { throw new RuntimeException("boom") }
               |  }
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    reportedName = identity,
    platforms = AllPlatforms,
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  val scalacheck: TestFrameworkFixture = TestFrameworkFixture(
    name = "scalacheck",
    deps = v => List(s"org.scalacheck::scalacheck:$v"),
    versions = List("1.18.1", "1.17.1", "1.15.4"),
    language = FixtureLanguage.Scala,
    relPath = "example/ScalacheckFixture.scala",
    suiteFqn = "example.ScalacheckFixture",
    // ScalaCheck names its own suite: the string given to `Properties` is what reaches sbt-testing, not the class name. Spelling it as the fully qualified name
    // keeps the reported suite and the discovered class the same string, which every other framework gets for free.
    source = """package example
               |
               |import org.scalacheck.Properties
               |import org.scalacheck.Prop.forAll
               |
               |object ScalacheckFixture extends Properties("example.ScalacheckFixture") {
               |  property("adds") = forAll { (n: Int) => n + 0 == n }
               |  property("measures") = forAll { (s: String) => s.length >= 0 }
               |  property("fails on purpose") = forAll { (n: Int) => n != n }
               |  property("throws on purpose") = forAll { (n: Int) => throw new RuntimeException("boom") }
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    reportedName = name => s"example.ScalacheckFixture.$name",
    platforms = AllPlatforms,
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  val specs2: TestFrameworkFixture = TestFrameworkFixture(
    name = "specs2",
    deps = v => List(s"org.specs2::specs2-core:$v"),
    versions = List("4.20.9", "4.19.2"),
    language = FixtureLanguage.Scala,
    relPath = "example/Specs2Fixture.scala",
    suiteFqn = "example.Specs2Fixture",
    // The throwing case is spelled `if (...) throw ... else ok` rather than a bare `throw`: specs2's `>>` is overloaded, a bare `throw` types as `Nothing`,
    // and the compiler cannot pick an overload from it. The condition is always true, so the `ok` branch exists only to give the block a `Result` type.
    source = """package example
               |
               |import org.specs2.mutable.Specification
               |
               |class Specs2Fixture extends Specification {
               |  "adds" >> { (1 + 1) must beEqualTo(2) }
               |  "measures" >> { "hello".length must beEqualTo(5) }
               |  "fails on purpose" >> { 1 must beEqualTo(2) }
               |  "throws on purpose" >> { if (1 + 1 == 2) throw new RuntimeException("boom") else ok }
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    reportedName = identity,
    platforms = Set("jvm", "js"),
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  val minitest: TestFrameworkFixture = TestFrameworkFixture(
    name = "minitest",
    deps = v => List(s"io.monix::minitest:$v"),
    versions = List("2.9.6", "2.8.2"),
    language = FixtureLanguage.Scala,
    relPath = "example/MinitestFixture.scala",
    suiteFqn = "example.MinitestFixture",
    source = """package example
               |
               |import minitest.SimpleTestSuite
               |
               |object MinitestFixture extends SimpleTestSuite {
               |  test("adds") { assertEquals(1 + 1, 2) }
               |  test("measures") { assertEquals("hello".length, 5) }
               |  test("fails on purpose") { assertEquals(1, 2) }
               |  test("throws on purpose") { throw new RuntimeException("boom") }
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    // minitest reports no per-test name: its events echo back whatever selector they were handed (`minitest.runner.Task`'s event returns
    // `taskDef.selectors`), and bleep passes a `SuiteSelector`, exactly as sbt does. So every case arrives under the suite's own name, and the individual
    // names exist only in the framework's logger output. A framework limitation rather than a bleep defect — the counts are still exact, which is what this
    // fixture is really asserting.
    reportedName = _ => "example.MinitestFixture",
    // No `minitest_native0.5_3` is published, so there is nothing for bleep to run there.
    platforms = Set("jvm", "js"),
    // No `minitest_3:2.8.2` was ever published; 2.9.6 is the first of its line to carry a Scala 3 artifact.
    scalaBinaryVersions = {
      case "2.8.2" => Set("2.12", "2.13")
      case _       => AllScalaBinaryVersions
    }
  )

  val junit5: TestFrameworkFixture = TestFrameworkFixture(
    name = "junit5",
    // Only the API. bleep injects the sbt-testing bridge and the junit-platform launcher/engines itself, aligned to whatever junit-platform this classpath
    // carries — see `MultiWorkspaceBspServer.externalTestRunnerDeps`.
    deps = v => List(s"org.junit.jupiter:junit-jupiter:$v"),
    versions = List(model.Versions.JunitJupiter, "5.10.1", "5.7.2"),
    language = FixtureLanguage.Java,
    relPath = "example/Junit5Fixture.java",
    suiteFqn = "example.Junit5Fixture",
    source = """package example;
               |
               |import org.junit.jupiter.api.Test;
               |import static org.junit.jupiter.api.Assertions.assertEquals;
               |
               |public class Junit5Fixture {
               |  @Test
               |  public void adds() { assertEquals(2, 1 + 1); }
               |
               |  @Test
               |  public void measures() { assertEquals(5, "hello".length()); }
               |
               |  @Test
               |  public void failsOnPurpose() { assertEquals(2, 1); }
               |
               |  @Test
               |  public void throwsOnPurpose() { throw new RuntimeException("boom"); }
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "failsOnPurpose", "throwsOnPurpose"),
    failingTestName = "failsOnPurpose",
    throwingTestName = "throwsOnPurpose",
    reportedName = name => s"$name()",
    platforms = JvmOnly,
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  val junit4: TestFrameworkFixture = TestFrameworkFixture(
    name = "junit4",
    // The vintage engine that runs these is injected by bleep, same as for junit5.
    deps = v => List(s"junit:junit:$v"),
    versions = List("4.13.2", "4.12"),
    language = FixtureLanguage.Java,
    relPath = "example/Junit4Fixture.java",
    suiteFqn = "example.Junit4Fixture",
    source = """package example;
               |
               |import org.junit.Test;
               |import static org.junit.Assert.assertEquals;
               |
               |public class Junit4Fixture {
               |  @Test
               |  public void adds() { assertEquals(2, 1 + 1); }
               |
               |  @Test
               |  public void measures() { assertEquals(5, "hello".length()); }
               |
               |  @Test
               |  public void failsOnPurpose() { assertEquals(2, 1); }
               |
               |  @Test
               |  public void throwsOnPurpose() { throw new RuntimeException("boom"); }
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "failsOnPurpose", "throwsOnPurpose"),
    failingTestName = "failsOnPurpose",
    throwingTestName = "throwsOnPurpose",
    // Unlike junit5, whose platform launcher reports "adds()", the vintage path reports the bare method name.
    reportedName = identity,
    platforms = JvmOnly,
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  val zioTest: TestFrameworkFixture = TestFrameworkFixture(
    name = "zio-test",
    deps = v => List(s"dev.zio::zio-test:$v", s"dev.zio::zio-test-sbt:$v"),
    versions = List("2.1.14", "2.0.22"),
    language = FixtureLanguage.Scala,
    relPath = "example/ZioTestFixture.scala",
    suiteFqn = "example.ZioTestFixture",
    source = """package example
               |
               |import zio.test._
               |
               |object ZioTestFixture extends ZIOSpecDefault {
               |  def spec = suite("example.ZioTestFixture")(
               |    test("adds") { assertTrue(1 + 1 == 2) },
               |    test("measures") { assertTrue("hello".length == 5) },
               |    test("fails on purpose") { assertTrue(1 == 2) },
               |    test("throws on purpose") { throw new RuntimeException("boom") }
               |  )
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    // zio-test prefixes each case with the enclosing suite's label.
    reportedName = name => s"example.ZioTestFixture - $name",
    // JVM only, and not because zio-test lacks a Scala.js build — it has one, it links, its framework loads and a task runs. What comes back is a single
    // suite-level failure carrying no message and no output, which is not enough to say whose defect it is. Left off the JS and Native rows deliberately rather
    // than left failing: an unexplained red in this matrix would train people to ignore it. See the note in the PR.
    platforms = JvmOnly,
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  val weaver: TestFrameworkFixture = TestFrameworkFixture(
    name = "weaver",
    deps = v => List(s"com.disneystreaming::weaver-cats:$v"),
    versions = List("0.8.4", "0.8.3"),
    language = FixtureLanguage.Scala,
    relPath = "example/WeaverFixture.scala",
    suiteFqn = "example.WeaverFixture",
    source = """package example
               |
               |import weaver._
               |
               |object WeaverFixture extends SimpleIOSuite {
               |  pureTest("adds") { expect(1 + 1 == 2) }
               |  pureTest("measures") { expect("hello".length == 5) }
               |  pureTest("fails on purpose") { expect(1 == 2) }
               |  pureTest("throws on purpose") { throw new RuntimeException("boom") }
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    reportedName = identity,
    platforms = Set("jvm", "js"),
    scalaBinaryVersions = _ => Set("2.13", "3")
  )

  val hedgehog: TestFrameworkFixture = TestFrameworkFixture(
    name = "hedgehog",
    deps = v => List(s"qa.hedgehog::hedgehog-sbt:$v"),
    versions = List("0.10.1", "0.9.0"),
    language = FixtureLanguage.Scala,
    relPath = "example/HedgehogFixture.scala",
    suiteFqn = "example.HedgehogFixture",
    source = """package example
               |
               |import hedgehog._
               |import hedgehog.runner._
               |
               |object HedgehogFixture extends Properties {
               |  def tests: List[Test] = List(
               |    example("adds", Result.assert(1 + 1 == 2)),
               |    example("measures", Result.assert("hello".length == 5)),
               |    example("fails on purpose", Result.assert(1 == 2)),
               |    example("throws on purpose", Result.assert(throw new RuntimeException("boom")))
               |  )
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    reportedName = identity,
    // No `hedgehog-sbt_native0.5_3` is published.
    platforms = Set("jvm", "js"),
    scalaBinaryVersions = _ => Set("2.13", "3")
  )

  /** Kotlin, and driven by the JUnit Platform rather than by an sbt adapter.
    *
    * That combination is the reason it is here. bleep opens a `LauncherSession` and lets the platform find its engines through the `ServiceLoader`, so Kotest
    * exercises the path no Scala framework touches — and kotest 6 brings its own junit-platform line, which is exactly the alignment case the test runtime's
    * rule table exists to get right (see `MultiWorkspaceBspServer.testRuntimeRules`, and the `NoSuchMethodError: ReflectionUtils.returnsVoid` it was written
    * for).
    */
  val kotest: TestFrameworkFixture = TestFrameworkFixture(
    name = "kotest",
    deps = v => List(s"io.kotest:kotest-runner-junit5-jvm:$v"),
    // 6.x and 5.x sit on different junit-platform lines, which is the whole point of covering both.
    versions = List(model.Versions.Kotest, "5.9.1"),
    language = FixtureLanguage.Kotlin,
    relPath = "example/KotestFixture.kt",
    suiteFqn = "example.KotestFixture",
    source = """package example
               |
               |import io.kotest.core.spec.style.FunSpec
               |import io.kotest.matchers.shouldBe
               |
               |class KotestFixture : FunSpec({
               |  test("adds") { (1 + 1) shouldBe 2 }
               |  test("measures") { "hello".length shouldBe 5 }
               |  test("fails on purpose") { 1 shouldBe 2 }
               |  test("throws on purpose") { throw RuntimeException("boom") }
               |})
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    reportedName = identity,
    // `kotest-runner-junit5-jvm` is the JVM artifact by name. Kotest does publish for JS and Native, under different artifact ids and a different runner; that
    // is a separate fixture rather than a platform row on this one.
    platforms = JvmOnly,
    // A Kotlin fixture carries no Scala at all, so the project's Scala version never enters into whether it resolves.
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  /** TestNG ships no sbt test-interface implementation, so it only runs through Mill's bridge — which makes this fixture a test of the bridge as much as of
    * TestNG, and the only coverage of a framework bleep can discover but not run unaided.
    */
  val testng: TestFrameworkFixture = TestFrameworkFixture(
    name = "testng",
    // The bridge is spelled with a single colon and an explicit `_2.13` suffix, not `::`. This is a Java fixture with no `scala:` block at all, and `::` asks
    // bleep to append the project's Scala suffix — which fails outright with "You need to configure a scala version". The bridge is a fixed 2.13 artifact
    // whatever the consuming project is written in, so naming it in full is both what works and what is true.
    deps = v => List(s"org.testng:testng:$v", "com.lihaoyi:mill-contrib-testng_2.13:0.9.6"),
    versions = List("7.10.2", "7.5"),
    language = FixtureLanguage.Java,
    relPath = "example/TestNGFixture.java",
    suiteFqn = "example.TestNGFixture",
    source = """package example;
               |
               |import org.testng.annotations.Test;
               |import static org.testng.Assert.assertEquals;
               |
               |public class TestNGFixture {
               |  @Test
               |  public void adds() { assertEquals(1 + 1, 2); }
               |
               |  @Test
               |  public void measures() { assertEquals("hello".length(), 5); }
               |
               |  @Test
               |  public void failsOnPurpose() { assertEquals(1, 2); }
               |
               |  @Test
               |  public void throwsOnPurpose() { throw new RuntimeException("boom"); }
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "failsOnPurpose", "throwsOnPurpose"),
    failingTestName = "failsOnPurpose",
    throwingTestName = "throwsOnPurpose",
    // Every case arrives under the suite's own name, the same way minitest's does and for the same reason: Mill's bridge echoes back the selector it was handed
    // — bleep passes a `SuiteSelector`, exactly as sbt does — and never names the method. The per-method names exist only in TestNG's own output. The counts
    // stay exact, which is what this fixture actually asserts: four cases, two of them not passing.
    reportedName = _ => "example.TestNGFixture",
    platforms = JvmOnly,
    // Unconstrained, because nothing here is compiled against Scala: the fixture is Java and the bridge is named at a fixed suffix.
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  /** Every framework the matrix knows about. Each runs on the platforms and Scala versions it declares. */
  val all: List[TestFrameworkFixture] =
    List(munit, scalatest, utest, scalacheck, specs2, minitest, zioTest, weaver, hedgehog, junit5, junit4, kotest, testng)

  /** Fixtures that publish for this platform and Scala binary version, at the one version CI pins. */
  def pinnedFor(platformId: String, scalaBinaryVersion: String): List[(TestFrameworkFixture, String)] =
    all.filter(f => f.supports(platformId, scalaBinaryVersion, f.currentVersion)).map(f => (f, f.currentVersion))

  /** Every (fixture, version) pair that publishes for this platform and Scala binary version — the whole sweep, pinned version included.
    *
    * Deliberately not `pinnedFor`'s complement: a version that CI already covers on one platform is still worth covering on another, and excluding it would
    * make the sweep's results depend on which combinations CI happens to run today.
    */
  def sweepFor(platformId: String, scalaBinaryVersion: String): List[(TestFrameworkFixture, String)] =
    all.flatMap(f => f.versions.filter(v => f.supports(platformId, scalaBinaryVersion, v)).map(v => (f, v)))
}
