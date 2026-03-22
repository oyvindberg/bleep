package bleep.analysis

import bleep.bsp.protocol.TestStatus
import bleep.testing._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Pure unit tests for BuildStateReducer.reduce.
  *
  * No IO, no temp dirs, no toolchains. Tests the state machine that tracks build/test progress.
  */
class BuildStateReducerTest extends AnyFunSuite with Matchers {

  private val ts = System.currentTimeMillis()

  private def reduce(events: BuildEvent*): BuildState =
    events.foldLeft(BuildState.empty)(BuildStateReducer.reduce)

  // ==========================================================================
  // SuiteFinished synthetic failure tests
  // ==========================================================================

  test("SuiteFinished with failures creates synthetic TestFailure when no individual test events exist") {
    val state = reduce(
      BuildEvent.SuiteStarted("proj", "com.example.MySuite", ts),
      BuildEvent.Output("proj", "com.example.MySuite", "error line 1", isError = true, ts + 1),
      BuildEvent.Output("proj", "com.example.MySuite", "at com.example.MySuite.test(MySuite.scala:42)", isError = true, ts + 2),
      BuildEvent.SuiteFinished("proj", "com.example.MySuite", passed = 0, failed = 2, skipped = 0, ignored = 0, durationMs = 100, ts + 3)
    )

    state.failures should have size 1
    val failure = state.failures.head
    failure.category shouldBe FailureCategory.ProcessError
    failure.project shouldBe "proj"
    failure.suite shouldBe "com.example.MySuite"
    failure.test shouldBe "(suite failed)"
    failure.output should contain("error line 1")
    failure.output should contain("at com.example.MySuite.test(MySuite.scala:42)")
    state.suitesFailed shouldBe 1
  }

  test("SuiteFinished with failures does NOT create synthetic when real TestFailure exists") {
    val state = reduce(
      BuildEvent.SuiteStarted("proj", "com.example.MySuite", ts),
      BuildEvent.TestStarted("proj", "com.example.MySuite", "test should work", ts + 1),
      BuildEvent.TestFinished(
        "proj",
        "com.example.MySuite",
        "test should work",
        TestStatus.Failed,
        durationMs = 50,
        message = Some("assertion failed"),
        throwable = None,
        ts + 2
      ),
      BuildEvent.SuiteFinished("proj", "com.example.MySuite", passed = 0, failed = 1, skipped = 0, ignored = 0, durationMs = 100, ts + 3)
    )

    state.failures should have size 1
    val failure = state.failures.head
    failure.test should not be "(suite failed)"
    failure.test shouldBe "test should work"
    failure.category shouldBe FailureCategory.TestFailed
  }

  test("SuiteFinished with no failures creates no synthetic") {
    val state = reduce(
      BuildEvent.SuiteStarted("proj", "com.example.MySuite", ts),
      BuildEvent.TestStarted("proj", "com.example.MySuite", "test1", ts + 1),
      BuildEvent.TestFinished("proj", "com.example.MySuite", "test1", TestStatus.Passed, durationMs = 10, message = None, throwable = None, ts + 2),
      BuildEvent.TestStarted("proj", "com.example.MySuite", "test2", ts + 3),
      BuildEvent.TestFinished("proj", "com.example.MySuite", "test2", TestStatus.Passed, durationMs = 10, message = None, throwable = None, ts + 4),
      BuildEvent.TestStarted("proj", "com.example.MySuite", "test3", ts + 5),
      BuildEvent.TestFinished("proj", "com.example.MySuite", "test3", TestStatus.Passed, durationMs = 10, message = None, throwable = None, ts + 6),
      BuildEvent.SuiteFinished("proj", "com.example.MySuite", passed = 3, failed = 0, skipped = 0, ignored = 0, durationMs = 100, ts + 7)
    )

    state.failures shouldBe empty
    state.suitesFailed shouldBe 0
    state.testsPassed shouldBe 3
  }

  test("SuiteFinished clears pending output for suite") {
    val key = SuiteKey("proj", "com.example.MySuite")

    // After Output events, pending output should exist
    val stateWithOutput = reduce(
      BuildEvent.SuiteStarted("proj", "com.example.MySuite", ts),
      BuildEvent.Output("proj", "com.example.MySuite", "some output", isError = false, ts + 1)
    )
    stateWithOutput.pendingOutput should contain key key

    // After SuiteFinished, pending output should be cleared
    val stateAfterFinished = BuildStateReducer.reduce(
      stateWithOutput,
      BuildEvent.SuiteFinished("proj", "com.example.MySuite", passed = 1, failed = 0, skipped = 0, ignored = 0, durationMs = 100, ts + 2)
    )
    stateAfterFinished.pendingOutput.contains(key) shouldBe false
  }

  // ==========================================================================
  // ConnectionLost tests
  // ==========================================================================

  test("ConnectionLost cancels all running suites") {
    val state = reduce(
      BuildEvent.SuiteStarted("p1", "suite1", ts),
      BuildEvent.SuiteStarted("p2", "suite2", ts + 1),
      BuildEvent.ConnectionLost("", ts + 2)
    )

    state.runningSuites shouldBe empty
    state.runningTests shouldBe empty
    state.cancelledSuites should have size 2
    state.cancelledSuites.foreach { cs =>
      cs.reason shouldBe Some("BSP connection lost")
    }
    val cancelledKeys = state.cancelledSuites.map(cs => SuiteKey(cs.project, cs.suite)).toSet
    cancelledKeys should contain(SuiteKey("p1", "suite1"))
    cancelledKeys should contain(SuiteKey("p2", "suite2"))
  }

  // ==========================================================================
  // TestRunCompleted tests
  // ==========================================================================

  test("TestRunCompleted overrides accumulated counts") {
    // First accumulate some counts that may be wrong (e.g., from missed events)
    val stateBeforeComplete = reduce(
      BuildEvent.SuiteStarted("proj", "suite1", ts),
      BuildEvent.TestStarted("proj", "suite1", "test1", ts + 1),
      BuildEvent.TestFinished("proj", "suite1", "test1", TestStatus.Passed, durationMs = 10, message = None, throwable = None, ts + 2),
      BuildEvent.SuiteFinished("proj", "suite1", passed = 1, failed = 0, skipped = 0, ignored = 0, durationMs = 50, ts + 3)
    )

    // TestRunCompleted with authoritative counts that differ from accumulated
    val state = BuildStateReducer.reduce(
      stateBeforeComplete,
      BuildEvent.TestRunCompleted(
        project = "proj",
        totalPassed = 10,
        totalFailed = 2,
        totalSkipped = 3,
        totalIgnored = 1,
        suitesTotal = 5,
        suitesCompleted = 5,
        suitesFailed = 1,
        suitesCancelled = 0,
        durationMs = 5000,
        timestamp = ts + 4
      )
    )

    state.testsPassed shouldBe 10
    state.testsFailed shouldBe 2
    state.testsSkipped shouldBe 3
    state.testsIgnored shouldBe 1
    state.suitesTotal shouldBe 5
    state.suitesCompleted shouldBe 5
    state.suitesFailed shouldBe 1
    state.suitesCancelled shouldBe 0
    state.runningSuites shouldBe empty
    state.runningTests shouldBe empty
  }
}
