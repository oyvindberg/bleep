package bleep

/** Which language a fixture's source is written in, and therefore where it lives and how its project is configured. */
sealed abstract class FixtureLanguage(val sourceDir: String)
object FixtureLanguage {
  case object Scala extends FixtureLanguage("scala")
  case object Java extends FixtureLanguage("java")
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
    /** Dependency lines for bleep.yaml. `::` for Scala artifacts so the platform suffix (`_sjs1`, `_native0.5`) is applied per project; `:` for Java ones. */
    deps: List[String],
    language: FixtureLanguage,
    /** Path under the source directory, e.g. `example/MunitFixture.scala`. */
    relPath: String,
    /** Fully qualified suite name, as `bleep test` reports it. */
    suiteFqn: String,
    source: String,
    /** Test names as written in the source. */
    testNames: List[String],
    /** The one test that must fail, as written in the source. */
    failingTestName: String,
    /** How this framework renders a test name to sbt-testing.
      *
      * Frameworks disagree, and the difference is visible to users in `bleep test` output and in JUnit XML: munit prefixes the suite's fully qualified name,
      * ScalaTest and utest report the bare name, JUnit appends parentheses. Pinning it per framework keeps the assertion exact instead of loosening it to a
      * suffix match that a runner reporting one synthetic case could satisfy.
      */
    reportedName: String => String,
    /** Platform ids this framework publishes for. A framework absent from a platform is not a bleep defect, so it is simply not run there. */
    platforms: Set[String]
) {
  def passingTestNames: List[String] = testNames.filterNot(_ == failingTestName).map(reportedName)
  def reportedFailingName: String = reportedName(failingTestName)
  def expectedPassed: Int = passingTestNames.size
  def expectedTotal: Int = testNames.size
}

object TestFrameworkFixture {
  private val AllPlatforms = Set("jvm", "js", "native")
  private val JvmOnly = Set("jvm")

  val munit: TestFrameworkFixture = TestFrameworkFixture(
    name = "munit",
    deps = List(s"org.scalameta::munit:${model.Versions.Munit}"),
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
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose"),
    failingTestName = "fails on purpose",
    reportedName = name => s"example.MunitFixture.$name",
    platforms = AllPlatforms
  )

  val scalatest: TestFrameworkFixture = TestFrameworkFixture(
    name = "scalatest",
    deps = List("org.scalatest::scalatest:3.2.19"),
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
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose"),
    failingTestName = "fails on purpose",
    reportedName = identity,
    platforms = AllPlatforms
  )

  val utest: TestFrameworkFixture = TestFrameworkFixture(
    name = "utest",
    deps = List("com.lihaoyi::utest:0.9.1"),
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
               |  }
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose"),
    failingTestName = "fails on purpose",
    reportedName = identity,
    platforms = AllPlatforms
  )

  val scalacheck: TestFrameworkFixture = TestFrameworkFixture(
    name = "scalacheck",
    deps = List("org.scalacheck::scalacheck:1.18.1"),
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
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose"),
    failingTestName = "fails on purpose",
    reportedName = name => s"example.ScalacheckFixture.$name",
    platforms = AllPlatforms
  )

  val specs2: TestFrameworkFixture = TestFrameworkFixture(
    name = "specs2",
    deps = List("org.specs2::specs2-core:4.20.9"),
    language = FixtureLanguage.Scala,
    relPath = "example/Specs2Fixture.scala",
    suiteFqn = "example.Specs2Fixture",
    source = """package example
               |
               |import org.specs2.mutable.Specification
               |
               |class Specs2Fixture extends Specification {
               |  "adds" >> { (1 + 1) must beEqualTo(2) }
               |  "measures" >> { "hello".length must beEqualTo(5) }
               |  "fails on purpose" >> { 1 must beEqualTo(2) }
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose"),
    failingTestName = "fails on purpose",
    reportedName = identity,
    platforms = Set("jvm", "js")
  )

  val minitest: TestFrameworkFixture = TestFrameworkFixture(
    name = "minitest",
    deps = List("io.monix::minitest:2.9.6"),
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
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose"),
    failingTestName = "fails on purpose",
    // minitest reports no per-test name: its events echo back whatever selector they were handed (`minitest.runner.Task`'s event returns
    // `taskDef.selectors`), and bleep passes a `SuiteSelector`, exactly as sbt does. So every case arrives under the suite's own name, and the individual
    // names exist only in the framework's logger output. A framework limitation rather than a bleep defect — the counts are still exact, which is what this
    // fixture is really asserting.
    reportedName = _ => "example.MinitestFixture",
    // No `minitest_native0.5_3` is published, so there is nothing for bleep to run there.
    platforms = Set("jvm", "js")
  )

  val junit5: TestFrameworkFixture = TestFrameworkFixture(
    name = "junit5",
    // Only the API. bleep injects the sbt-testing bridge and the junit-platform launcher/engines itself, aligned to whatever junit-platform this classpath
    // carries — see `MultiWorkspaceBspServer.externalTestRunnerDeps`.
    deps = List(s"org.junit.jupiter:junit-jupiter:${model.Versions.JunitJupiter}"),
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
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "failsOnPurpose"),
    failingTestName = "failsOnPurpose",
    reportedName = name => s"$name()",
    platforms = JvmOnly
  )

  val junit4: TestFrameworkFixture = TestFrameworkFixture(
    name = "junit4",
    // The vintage engine that runs these is injected by bleep, same as for junit5.
    deps = List("junit:junit:4.13.2"),
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
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "failsOnPurpose"),
    failingTestName = "failsOnPurpose",
    // Unlike junit5, whose platform launcher reports "adds()", the vintage path reports the bare method name.
    reportedName = identity,
    platforms = JvmOnly
  )

  val zioTest: TestFrameworkFixture = TestFrameworkFixture(
    name = "zio-test",
    deps = List("dev.zio::zio-test:2.1.14", "dev.zio::zio-test-sbt:2.1.14"),
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
               |    test("fails on purpose") { assertTrue(1 == 2) }
               |  )
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose"),
    failingTestName = "fails on purpose",
    // zio-test prefixes each case with the enclosing suite's label.
    reportedName = name => s"example.ZioTestFixture - $name",
    // JVM only, and not because zio-test lacks a Scala.js build — it has one, it links, its framework loads and a task runs. What comes back is a single
    // suite-level failure carrying no message and no output, which is not enough to say whose defect it is. Left off the JS and Native rows deliberately rather
    // than left failing: an unexplained red in this matrix would train people to ignore it. See the note in the PR.
    platforms = JvmOnly
  )

  val weaver: TestFrameworkFixture = TestFrameworkFixture(
    name = "weaver",
    deps = List("com.disneystreaming::weaver-cats:0.8.4"),
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
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose"),
    failingTestName = "fails on purpose",
    reportedName = identity,
    platforms = Set("jvm", "js")
  )

  val hedgehog: TestFrameworkFixture = TestFrameworkFixture(
    name = "hedgehog",
    deps = List("qa.hedgehog::hedgehog-sbt:0.10.1"),
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
               |    example("fails on purpose", Result.assert(1 == 2))
               |  )
               |}
               |""".stripMargin,
    testNames = List("adds", "measures", "fails on purpose"),
    failingTestName = "fails on purpose",
    reportedName = identity,
    // No `hedgehog-sbt_native0.5_3` is published.
    platforms = Set("jvm", "js")
  )

  /** Every framework the matrix knows about. Each runs on the platforms it declares. */
  val all: List[TestFrameworkFixture] =
    List(munit, scalatest, utest, scalacheck, specs2, minitest, zioTest, weaver, hedgehog, junit5, junit4)

  def forPlatform(platformId: String): List[TestFrameworkFixture] =
    all.filter(_.platforms.contains(platformId))
}
