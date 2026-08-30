package bleep.bsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Parsing of what a Kotlin/Native test binary actually prints.
  *
  * The binary speaks GTest's format, not the `##kotlin-test##` protocol the Kotlin/JS side uses, and every line below is copied from a real run rather than
  * imagined. That distinction matters: both defects pinned here survived because the parser was written against a sketch of the format in a comment, and
  * nothing ever compared it to output.
  */
class KotlinNativeGTestParsingTest extends AnyFunSuite with Matchers {

  /** Verbatim from `mytest.kexe` — a four-test Kotlin/Native fixture. */
  private val realOutput = List(
    "[==========] Running 4 tests from 1 test cases.",
    "[----------] Global test environment set-up.",
    "[----------] 4 tests from example.KotlinTestFixture",
    "[ RUN      ] example.KotlinTestFixture.adds",
    "[       OK ] example.KotlinTestFixture.adds (0 ms)",
    "[----------] 4 tests from example.KotlinTestFixture (0 ms total)",
    "[==========] 4 tests from 1 test cases ran. (0 ms total)"
  )

  test("the closing summary line is not mistaken for a second suite") {
    // Both lines say "4 tests from example.KotlinTestFixture"; only the first opens a suite. Matching both opened a phantom suite named
    // "example.KotlinTestFixture (0 ms total)" which carried its own copy of every failure — a four-test fixture reported six failures.
    val opened = realOutput.collect { case KotlinTestRunner.Native.suiteStartPattern(suite) => suite }
    opened shouldBe List("example.KotlinTestFixture")
  }

  /** Verbatim from a run of a fixture with one `@Ignore`d test. Note there is no inline report for it at all — the closing listing is its only appearance, and
    * it arrives after `[==========]`.
    */
  private val skippedOutput = List(
    "[----------] 3 tests from example.KotlinTestFixture (0 ms total)",
    "[----------] Global test environment tear-down",
    "[==========] 3 tests from 1 test cases ran. (0 ms total)",
    "[  PASSED  ] 1 tests.",
    "[  SKIPPED ] 1 test, listed below:",
    "[  SKIPPED ] example.KotlinTestFixture.skippedOnPurpose",
    "[  FAILED  ] 1 test, listed below:",
    "[  FAILED  ] example.KotlinTestFixture.failsOnPurpose"
  )

  test("a skipped test is read from the closing listing, and its count line is not") {
    // Two lines start with `[  SKIPPED ]`. Only one names a test; the other is a count. Matching both would invent a test called "1" — and matching
    // neither, which is what the parser did before, dropped the case entirely: the report said 4 tests where the fixture has 5, with skipped=0.
    val skipped = skippedOutput.collect { case KotlinTestRunner.Native.testSkippedPattern(test) => test }
    skipped shouldBe List("example.KotlinTestFixture.skippedOnPurpose")
  }

  test("the closing FAILED listing is still not counted as a second failure") {
    // Guards the pairing: `testSkippedPattern` matches the summary section on purpose, so the neighbouring FAILED lines must keep being excluded by
    // `testFailedPattern`'s required timing suffix.
    val failed = skippedOutput.collect { case KotlinTestRunner.Native.testFailedPattern(test) => test }
    failed shouldBe empty
  }

  test("a suite name carries no timing suffix") {
    val opened = realOutput.collect { case KotlinTestRunner.Native.suiteStartPattern(suite) => suite }
    opened.foreach(_ should not include "ms total")
  }

  test("--ktest_list_tests output is read as a tree, not a list") {
    // Verbatim shape: the suite line ends with a dot, its tests are indented beneath it.
    val listing = List("example.KotlinTestFixture.", "  adds", "  measures", "  failsOnPurpose", "  throwsOnPurpose")
    val suites = KotlinTestRunner.Native.parseListedSuites(listing)
    suites.map(_.fullyQualifiedName) shouldBe List("example.KotlinTestFixture")
    suites.map(_.name) shouldBe List("KotlinTestFixture")
  }

  test("two suites in one listing are both found, and their tests are not") {
    val listing = List("example.First.", "  a", "  b", "example.Second.", "  c")
    KotlinTestRunner.Native.parseListedSuites(listing).map(_.fullyQualifiedName) shouldBe List("example.First", "example.Second")
  }

  test("a failure is counted once, though GTest reports it three times") {
    // Verbatim tail of a real run: inline as it happens, then a count line, then again in the closing summary.
    val tail = List(
      "[  FAILED  ] example.KotlinTestFixture.failsOnPurpose (0 ms)",
      "[  FAILED  ] example.KotlinTestFixture.throwsOnPurpose (0 ms)",
      "[  PASSED  ] 2 tests.",
      "[  FAILED  ] 2 tests, listed below:",
      "[  FAILED  ] example.KotlinTestFixture.failsOnPurpose",
      "[  FAILED  ] example.KotlinTestFixture.throwsOnPurpose"
    )
    val counted = tail.collect { case KotlinTestRunner.Native.testFailedPattern(test) => test }
    counted shouldBe List("example.KotlinTestFixture.failsOnPurpose", "example.KotlinTestFixture.throwsOnPurpose")
  }

  test("the failure count line is not read as a failing test") {
    val line = "[  FAILED  ] 2 tests, listed below:"
    line match {
      case KotlinTestRunner.Native.testFailedPattern(t) => fail(s"counted the summary line as a test named '$t'")
      case _                                            => succeed
    }
  }

  test("a binary that does not support listing yields nothing rather than nonsense") {
    KotlinTestRunner.Native.parseListedSuites(Nil) shouldBe empty
  }
}
