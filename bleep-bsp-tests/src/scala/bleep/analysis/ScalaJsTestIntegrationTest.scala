package bleep.analysis

import bleep.bsp.{ScalaJsTestRunner, TestRunnerTypes}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit-level checks around Scala.js test execution.
  *
  * Everything that used to live here drove `ScalaJsTestRunner.runTests` with `.js` files the test wrote itself, each one printing lines in a `##scalajs-test##`
  * protocol that only bleep's injected harness ever spoke. Those tests passed throughout the entire lifetime of issue #655 — a runner that ran no munit suite
  * and no utest suite — because no linked Scala.js program appeared anywhere in them, and the mangled-name lookups that actually broke were never reached. When
  * the harness was replaced by `org.scalajs.testing.adapter.TestAdapter`, the protocol they asserted on ceased to exist.
  *
  * Real coverage now lives in `bleep.ScalaJsTestFrameworkIT` (in bleep-tests), which compiles a suite, links it, runs it through `bleep test`, and asserts on
  * the per-test results that come back — for each framework in turn.
  */
class ScalaJsTestIntegrationTest extends AnyFunSuite with Matchers {

  test("TestResult: success when no failures") {
    val result = TestRunnerTypes.TestResult(
      passed = 5,
      failed = 0,
      skipped = 1,
      ignored = 0,
      terminationReason = TestRunnerTypes.TerminationReason.Completed
    )

    result.isSuccess shouldBe true
    result.passed shouldBe 5
    result.skipped shouldBe 1
  }

  test("TestResult: not success when failures") {
    val result = TestRunnerTypes.TestResult(
      passed = 4,
      failed = 1,
      skipped = 0,
      ignored = 0,
      terminationReason = TestRunnerTypes.TerminationReason.Completed
    )

    result.isSuccess shouldBe false
  }

  test("TestResult: not success when cancelled") {
    val result = TestRunnerTypes.TestResult(
      passed = 5,
      failed = 0,
      skipped = 0,
      ignored = 0,
      terminationReason = TestRunnerTypes.TerminationReason.Killed(bleep.bsp.protocol.KillReason.UserRequest)
    )

    result.isSuccess shouldBe false
    result.terminationReason shouldBe a[TestRunnerTypes.TerminationReason.Killed]
  }

  test("NodeEnvironment: Node and JSDOM") {
    ScalaJsTestRunner.NodeEnvironment.Node shouldBe a[ScalaJsTestRunner.NodeEnvironment]

    val jsdom = ScalaJsTestRunner.NodeEnvironment.JSDOM("http://localhost")
    jsdom.url shouldBe "http://localhost"
  }
}
