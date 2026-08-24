package bleep.analysis

import bleep.bsp.TestRunnerTypes
import bleep.bsp.protocol.{KillReason, TestStatus}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Unit tests for the types every non-JVM test runner reports through. */
class TestRunnerTypesTest extends AnyFunSuite with Matchers {

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
      terminationReason = TestRunnerTypes.TerminationReason.Killed(KillReason.UserRequest)
    )

    result.isSuccess shouldBe false
    result.terminationReason shouldBe a[TestRunnerTypes.TerminationReason.Killed]
  }

  test("TestStatus: all statuses") {
    TestStatus.Passed shouldBe a[TestStatus]
    TestStatus.Failed shouldBe a[TestStatus]
    TestStatus.Skipped shouldBe a[TestStatus]
    TestStatus.Ignored shouldBe a[TestStatus]
    TestStatus.Cancelled shouldBe a[TestStatus]
  }

  /** `ClasspathTestDiscovery` reports a framework's own `name()` for a fingerprint match and its base-class table's label otherwise. Those two spell the same
    * framework differently, and both have to reach the same `TestFramework`.
    */
  test("TestFramework.fromName: matches both spellings of every framework bleep knows") {
    TestRunnerTypes.TestFramework.fromName("munit") shouldBe TestRunnerTypes.TestFramework.MUnit
    TestRunnerTypes.TestFramework.fromName("MUnit") shouldBe TestRunnerTypes.TestFramework.MUnit
    TestRunnerTypes.TestFramework.fromName("utest") shouldBe TestRunnerTypes.TestFramework.UTest
    TestRunnerTypes.TestFramework.fromName("uTest") shouldBe TestRunnerTypes.TestFramework.UTest
    TestRunnerTypes.TestFramework.fromName("ScalaTest") shouldBe TestRunnerTypes.TestFramework.ScalaTest
  }

  /** `Unknown` lists every candidate class name, which asks a `TestAdapter` to try them all. */
  test("TestFramework.fromName: an unrecognized name reaches Unknown") {
    TestRunnerTypes.TestFramework.fromName("weaver") shouldBe TestRunnerTypes.TestFramework.Unknown
  }
}
