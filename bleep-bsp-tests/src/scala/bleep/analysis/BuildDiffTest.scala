package bleep.analysis

import bleep.bsp.protocol.{BleepBspProtocol, CompileStatus, DiagnosticSeverity, TestStatus}
import bleep.testing._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Pure unit tests for BuildDiff — per-project diff logic for compile and test results.
  *
  * No IO, no temp dirs, no toolchains. Tests the pure diff functions that compute terse per-project one-liners.
  */
class BuildDiffTest extends AnyFunSuite with Matchers {

  private val ts = System.currentTimeMillis()

  // ==========================================================================
  // Helper constructors
  // ==========================================================================

  private def diag(severity: DiagnosticSeverity, message: String, path: String): BleepBspProtocol.Diagnostic =
    BleepBspProtocol.Diagnostic(severity = severity, message = message, rendered = None, path = Some(path))

  private def errorDiag(message: String, path: String): BleepBspProtocol.Diagnostic =
    diag(DiagnosticSeverity.Error, message, path)

  private def warningDiag(message: String, path: String): BleepBspProtocol.Diagnostic =
    diag(DiagnosticSeverity.Warning, message, path)

  private def testFinished(project: String, suite: String, test: String, status: TestStatus): BuildEvent.TestFinished =
    BuildEvent.TestFinished(
      project = project,
      suite = suite,
      test = test,
      status = status,
      durationMs = 10,
      message = None,
      throwable = None,
      timestamp = ts
    )

  // ==========================================================================
  // PreviousRunState.fromEvents
  // ==========================================================================

  test("PreviousRunState.fromEvents extracts compile diagnostics per project") {
    val events = List(
      BuildEvent.CompileStarted("proj-a", ts),
      BuildEvent.CompileFinished(
        "proj-a",
        CompileStatus.Success,
        durationMs = 100,
        timestamp = ts + 1,
        diagnostics = List(warningDiag("unused import", "Foo.scala:10:5")),
        skippedBecause = None
      ),
      BuildEvent.CompileStarted("proj-b", ts + 2),
      BuildEvent.CompileFinished(
        "proj-b",
        CompileStatus.Failed,
        durationMs = 200,
        timestamp = ts + 3,
        diagnostics = List(errorDiag("type mismatch", "Bar.scala:20:3")),
        skippedBecause = None
      )
    )

    val state = PreviousRunState.fromEvents(events)
    state.compileDiagnostics should have size 2
    state.compileDiagnostics("proj-a") should have size 1
    state.compileDiagnostics("proj-a").head.severity shouldBe DiagnosticSeverity.Warning
    state.compileDiagnostics("proj-b") should have size 1
    state.compileDiagnostics("proj-b").head.severity shouldBe DiagnosticSeverity.Error
  }

  test("PreviousRunState.fromEvents extracts test results per (project, suite, test)") {
    val events = List(
      BuildEvent.SuiteStarted("proj", "MySuite", ts),
      testFinished("proj", "MySuite", "test1", TestStatus.Passed),
      testFinished("proj", "MySuite", "test2", TestStatus.Failed),
      BuildEvent.SuiteFinished("proj", "MySuite", passed = 1, failed = 1, skipped = 0, ignored = 0, durationMs = 100, ts + 1)
    )

    val state = PreviousRunState.fromEvents(events)
    state.testResults should have size 2
    state.testResults(TestKey("proj", "MySuite", "test1")) shouldBe TestStatus.Passed
    state.testResults(TestKey("proj", "MySuite", "test2")) shouldBe TestStatus.Failed
  }

  test("PreviousRunState.fromEvents ignores non-compile-finished and non-test-finished events") {
    val events = List(
      BuildEvent.CompileStarted("proj-a", ts),
      BuildEvent.CompileProgress("proj-a", 50, ts + 1),
      BuildEvent.SuiteStarted("proj-b", "MySuite", ts + 2),
      BuildEvent.Output("proj-b", "MySuite", "some output", isError = false, ts + 3)
    )

    val state = PreviousRunState.fromEvents(events)
    state.compileDiagnostics shouldBe empty
    state.testResults shouldBe empty
  }

  // ==========================================================================
  // PreviousRunState.fromProtocolEvents
  // ==========================================================================

  test("PreviousRunState.fromProtocolEvents extracts from protocol event types") {
    val events: List[BleepBspProtocol.Event] = List(
      BleepBspProtocol.Event.CompileFinished(
        project = "proj-a",
        status = CompileStatus.Success,
        durationMs = 100,
        diagnostics = List(warningDiag("unused", "A.scala:1:1")),
        skippedBecause = None,
        timestamp = ts
      ),
      BleepBspProtocol.Event.TestFinished(
        project = "proj-b",
        suite = "MySuite",
        test = "testFoo",
        status = TestStatus.Passed,
        durationMs = 50,
        message = None,
        throwable = None,
        timestamp = ts + 1
      )
    )

    val state = PreviousRunState.fromProtocolEvents(events)
    state.compileDiagnostics should have size 1
    state.compileDiagnostics("proj-a") should have size 1
    state.testResults should have size 1
    state.testResults(TestKey("proj-b", "MySuite", "testFoo")) shouldBe TestStatus.Passed
  }

  // ==========================================================================
  // BuildDiff.diffCompile — diagnostic diff logic
  // ==========================================================================

  test("diffCompile: clean project with no previous = all zeros") {
    val diff = BuildDiff.diffCompile(
      project = "proj",
      status = CompileStatus.Success,
      currentDiagnostics = Nil,
      previousDiagnostics = Nil,
      durationMs = 100
    )

    diff.totalErrors shouldBe 0
    diff.totalWarnings shouldBe 0
    diff.newErrors shouldBe 0
    diff.fixedErrors shouldBe 0
    diff.newWarnings shouldBe 0
    diff.fixedWarnings shouldBe 0
  }

  test("diffCompile: new errors where none existed before") {
    val current = List(
      errorDiag("type mismatch", "Foo.scala:10:5"),
      errorDiag("not found: value x", "Bar.scala:20:3")
    )

    val diff = BuildDiff.diffCompile("proj", CompileStatus.Failed, current, Nil, 200)

    diff.totalErrors shouldBe 2
    diff.newErrors shouldBe 2
    diff.fixedErrors shouldBe 0
  }

  test("diffCompile: all errors fixed") {
    val previous = List(
      errorDiag("type mismatch", "Foo.scala:10:5"),
      errorDiag("not found: value x", "Bar.scala:20:3")
    )

    val diff = BuildDiff.diffCompile("proj", CompileStatus.Success, Nil, previous, 100)

    diff.totalErrors shouldBe 0
    diff.newErrors shouldBe 0
    diff.fixedErrors shouldBe 2
  }

  test("diffCompile: some errors fixed, some new, some remaining") {
    val previous = List(
      errorDiag("type mismatch", "Foo.scala:10:5"),
      errorDiag("not found: value x", "Bar.scala:20:3"),
      errorDiag("missing argument", "Baz.scala:5:1")
    )
    val current = List(
      errorDiag("not found: value x", "Bar.scala:25:3"), // same file+message, different line — matches
      errorDiag("implicit not found", "Qux.scala:1:1") // new error
    )

    val diff = BuildDiff.diffCompile("proj", CompileStatus.Failed, current, previous, 300)

    diff.totalErrors shouldBe 2
    diff.newErrors shouldBe 1 // implicit not found is new
    diff.fixedErrors shouldBe 2 // type mismatch and missing argument were fixed
  }

  test("diffCompile: line number change does not affect matching") {
    val previous = List(errorDiag("type mismatch", "Foo.scala:10:5"))
    val current = List(errorDiag("type mismatch", "Foo.scala:42:8"))

    val diff = BuildDiff.diffCompile("proj", CompileStatus.Failed, current, previous, 100)

    // Same file + message = same error, line shift is ignored
    diff.newErrors shouldBe 0
    diff.fixedErrors shouldBe 0
    diff.totalErrors shouldBe 1
  }

  test("diffCompile: warnings are tracked separately from errors") {
    val previous = List(
      warningDiag("unused import", "A.scala:1:1"),
      errorDiag("type mismatch", "B.scala:2:1")
    )
    val current = List(
      warningDiag("unused import", "A.scala:1:1"),
      warningDiag("deprecation", "C.scala:3:1")
    )

    val diff = BuildDiff.diffCompile("proj", CompileStatus.Success, current, previous, 100)

    diff.totalErrors shouldBe 0
    diff.fixedErrors shouldBe 1 // type mismatch fixed
    diff.newErrors shouldBe 0
    diff.totalWarnings shouldBe 2
    diff.newWarnings shouldBe 1 // deprecation is new
    diff.fixedWarnings shouldBe 0 // unused import still present
  }

  test("diffCompile: diagnostics without path are matched by message only") {
    val noDiag = BleepBspProtocol.Diagnostic(severity = DiagnosticSeverity.Error, message = "build error", rendered = None, path = None)
    val previous = List(noDiag)
    val current = List(noDiag)

    val diff = BuildDiff.diffCompile("proj", CompileStatus.Failed, current, previous, 100)

    diff.totalErrors shouldBe 1
    diff.newErrors shouldBe 0 // same error
    diff.fixedErrors shouldBe 0
  }

  // ==========================================================================
  // BuildDiff.diffSuite — test result diff logic
  // ==========================================================================

  test("diffSuite: all passing, no previous = no diff details") {
    val currentTests = List(
      testFinished("proj", "MySuite", "test1", TestStatus.Passed),
      testFinished("proj", "MySuite", "test2", TestStatus.Passed)
    )

    val diff = BuildDiff.diffSuite("proj", "MySuite", currentTests, Map.empty, passed = 2, failed = 0, skipped = 0, ignored = 0, durationMs = 100)

    diff.passed shouldBe 2
    diff.failed shouldBe 0
    diff.newFailures shouldBe empty
    diff.fixedTests shouldBe empty
    diff.stillFailing shouldBe empty
  }

  test("diffSuite: test flips from pass to fail = new failure") {
    val previousResults = Map(
      TestKey("proj", "MySuite", "test1") -> TestStatus.Passed,
      TestKey("proj", "MySuite", "test2") -> TestStatus.Passed
    )
    val currentTests = List(
      testFinished("proj", "MySuite", "test1", TestStatus.Passed),
      testFinished("proj", "MySuite", "test2", TestStatus.Failed)
    )

    val diff = BuildDiff.diffSuite("proj", "MySuite", currentTests, previousResults, passed = 1, failed = 1, skipped = 0, ignored = 0, durationMs = 100)

    diff.newFailures shouldBe List("test2")
    diff.fixedTests shouldBe empty
    diff.stillFailing shouldBe empty
  }

  test("diffSuite: test flips from fail to pass = fixed") {
    val previousResults = Map(
      TestKey("proj", "MySuite", "test1") -> TestStatus.Passed,
      TestKey("proj", "MySuite", "test2") -> TestStatus.Failed
    )
    val currentTests = List(
      testFinished("proj", "MySuite", "test1", TestStatus.Passed),
      testFinished("proj", "MySuite", "test2", TestStatus.Passed)
    )

    val diff = BuildDiff.diffSuite("proj", "MySuite", currentTests, previousResults, passed = 2, failed = 0, skipped = 0, ignored = 0, durationMs = 100)

    diff.fixedTests shouldBe List("test2")
    diff.newFailures shouldBe empty
    diff.stillFailing shouldBe empty
  }

  test("diffSuite: test still failing = still failing") {
    val previousResults = Map(
      TestKey("proj", "MySuite", "testBad") -> TestStatus.Failed
    )
    val currentTests = List(
      testFinished("proj", "MySuite", "testBad", TestStatus.Failed)
    )

    val diff = BuildDiff.diffSuite("proj", "MySuite", currentTests, previousResults, passed = 0, failed = 1, skipped = 0, ignored = 0, durationMs = 100)

    diff.stillFailing shouldBe List("testBad")
    diff.newFailures shouldBe empty
    diff.fixedTests shouldBe empty
  }

  test("diffSuite: new test that immediately fails = new failure") {
    val previousResults = Map(
      TestKey("proj", "MySuite", "test1") -> TestStatus.Passed
    )
    val currentTests = List(
      testFinished("proj", "MySuite", "test1", TestStatus.Passed),
      testFinished("proj", "MySuite", "testNew", TestStatus.Failed) // new test, no previous
    )

    val diff = BuildDiff.diffSuite("proj", "MySuite", currentTests, previousResults, passed = 1, failed = 1, skipped = 0, ignored = 0, durationMs = 100)

    diff.newFailures shouldBe List("testNew")
  }

  test("diffSuite: new test that passes = no diff detail") {
    val previousResults = Map(
      TestKey("proj", "MySuite", "test1") -> TestStatus.Passed
    )
    val currentTests = List(
      testFinished("proj", "MySuite", "test1", TestStatus.Passed),
      testFinished("proj", "MySuite", "testNew", TestStatus.Passed) // new test, passes
    )

    val diff = BuildDiff.diffSuite("proj", "MySuite", currentTests, previousResults, passed = 2, failed = 0, skipped = 0, ignored = 0, durationMs = 100)

    diff.newFailures shouldBe empty
    diff.fixedTests shouldBe empty
    diff.stillFailing shouldBe empty
  }

  test("diffSuite: error and timeout statuses count as failures in previous run") {
    val previousResults = Map(
      TestKey("proj", "MySuite", "testErr") -> TestStatus.Error,
      TestKey("proj", "MySuite", "testTimeout") -> TestStatus.Timeout
    )
    val currentTests = List(
      testFinished("proj", "MySuite", "testErr", TestStatus.Passed),
      testFinished("proj", "MySuite", "testTimeout", TestStatus.Passed)
    )

    val diff = BuildDiff.diffSuite("proj", "MySuite", currentTests, previousResults, passed = 2, failed = 0, skipped = 0, ignored = 0, durationMs = 100)

    diff.fixedTests should contain("testErr")
    diff.fixedTests should contain("testTimeout")
  }

  test("diffSuite: mixed scenario — fix one, break another, one still failing") {
    val previousResults = Map(
      TestKey("proj", "MySuite", "test1") -> TestStatus.Passed,
      TestKey("proj", "MySuite", "test2") -> TestStatus.Failed,
      TestKey("proj", "MySuite", "test3") -> TestStatus.Failed
    )
    val currentTests = List(
      testFinished("proj", "MySuite", "test1", TestStatus.Failed), // was passing, now failing
      testFinished("proj", "MySuite", "test2", TestStatus.Passed), // was failing, now fixed
      testFinished("proj", "MySuite", "test3", TestStatus.Failed) // still failing
    )

    val diff = BuildDiff.diffSuite("proj", "MySuite", currentTests, previousResults, passed = 1, failed = 2, skipped = 0, ignored = 0, durationMs = 100)

    diff.newFailures shouldBe List("test1")
    diff.fixedTests shouldBe List("test2")
    diff.stillFailing shouldBe List("test3")
  }

  // ==========================================================================
  // BuildDiff.formatCompileDiff — terse one-liner formatting
  // ==========================================================================

  test("formatCompileDiff: clean project with no previous") {
    val diff = BuildDiff.CompileDiff("proj", CompileStatus.Success, 0, 0, 0, 0, 0, 0, 100)
    BuildDiff.formatCompileDiff(diff) shouldBe "proj: OK (100ms)"
  }

  test("formatCompileDiff: clean project after fixing errors") {
    val diff = BuildDiff.CompileDiff("proj", CompileStatus.Success, 0, 0, 0, 2, 0, 0, 100)
    BuildDiff.formatCompileDiff(diff) shouldBe "proj: OK (fixed 2 errors) (100ms)"
  }

  test("formatCompileDiff: clean project with warnings") {
    val diff = BuildDiff.CompileDiff("proj", CompileStatus.Success, 0, 3, 0, 0, 0, 0, 100)
    BuildDiff.formatCompileDiff(diff) shouldBe "proj: OK (3 warnings) (100ms)"
  }

  test("formatCompileDiff: clean project after fixing errors, with warnings") {
    val diff = BuildDiff.CompileDiff("proj", CompileStatus.Success, 0, 2, 0, 1, 0, 0, 100)
    BuildDiff.formatCompileDiff(diff) shouldBe "proj: OK (fixed 1 error, 2 warnings) (100ms)"
  }

  test("formatCompileDiff: failed with all new errors") {
    val diff = BuildDiff.CompileDiff("proj", CompileStatus.Failed, 3, 0, 3, 0, 0, 0, 200)
    BuildDiff.formatCompileDiff(diff) shouldBe "proj: 3 errors (all new) (200ms)"
  }

  test("formatCompileDiff: failed with mix of new and fixed") {
    val diff = BuildDiff.CompileDiff("proj", CompileStatus.Failed, 2, 0, 1, 1, 0, 0, 300)
    BuildDiff.formatCompileDiff(diff) shouldBe "proj: 2 errors (1 new, 1 fixed) (300ms)"
  }

  test("formatCompileDiff: failed with some new and remaining") {
    val diff = BuildDiff.CompileDiff("proj", CompileStatus.Failed, 3, 0, 1, 0, 0, 0, 150)
    BuildDiff.formatCompileDiff(diff) shouldBe "proj: 3 errors (1 new, 2 remaining) (150ms)"
  }

  test("formatCompileDiff: failed with only remaining errors (no new, no fixed)") {
    val diff = BuildDiff.CompileDiff("proj", CompileStatus.Failed, 2, 0, 0, 0, 0, 0, 100)
    BuildDiff.formatCompileDiff(diff) shouldBe "proj: 2 errors (2 remaining) (100ms)"
  }

  test("formatCompileDiff: failed with errors and warnings") {
    val diff = BuildDiff.CompileDiff("proj", CompileStatus.Failed, 1, 2, 1, 0, 0, 0, 100)
    BuildDiff.formatCompileDiff(diff) shouldBe "proj: 1 error (all new, 2 warnings) (100ms)"
  }

  test("formatCompileDiff: singular error uses no 's'") {
    val diff = BuildDiff.CompileDiff("proj", CompileStatus.Failed, 1, 0, 1, 0, 0, 0, 100)
    BuildDiff.formatCompileDiff(diff) shouldBe "proj: 1 error (all new) (100ms)"
  }

  test("formatCompileDiff: singular warning uses no 's'") {
    val diff = BuildDiff.CompileDiff("proj", CompileStatus.Success, 0, 1, 0, 0, 0, 0, 100)
    BuildDiff.formatCompileDiff(diff) shouldBe "proj: OK (1 warning) (100ms)"
  }

  // ==========================================================================
  // BuildDiff.formatSuiteDiff — terse one-liner formatting
  // ==========================================================================

  test("formatSuiteDiff: all passing, no diff") {
    val diff = BuildDiff.SuiteDiff("proj", "MySuite", 5, 0, 0, 0, Nil, Nil, Nil, 100)
    BuildDiff.formatSuiteDiff(diff) shouldBe "proj MySuite: 5 passed"
  }

  test("formatSuiteDiff: failures with no diff detail") {
    val diff = BuildDiff.SuiteDiff("proj", "MySuite", 3, 2, 0, 0, Nil, Nil, Nil, 100)
    BuildDiff.formatSuiteDiff(diff) shouldBe "proj MySuite: 3 passed, 2 failed"
  }

  test("formatSuiteDiff: with skipped and ignored") {
    val diff = BuildDiff.SuiteDiff("proj", "MySuite", 5, 0, 2, 1, Nil, Nil, Nil, 100)
    BuildDiff.formatSuiteDiff(diff) shouldBe "proj MySuite: 5 passed, 2 skipped, 1 ignored"
  }

  test("formatSuiteDiff: with fixed tests") {
    val diff = BuildDiff.SuiteDiff("proj", "MySuite", 5, 0, 0, 0, Nil, List("testFoo"), Nil, 100)
    BuildDiff.formatSuiteDiff(diff) shouldBe "proj MySuite: 5 passed (testFoo fixed)"
  }

  test("formatSuiteDiff: with new failures") {
    val diff = BuildDiff.SuiteDiff("proj", "MySuite", 4, 1, 0, 0, List("testBar"), Nil, Nil, 100)
    BuildDiff.formatSuiteDiff(diff) shouldBe "proj MySuite: 4 passed, 1 failed (testBar new failure)"
  }

  test("formatSuiteDiff: with still failing") {
    val diff = BuildDiff.SuiteDiff("proj", "MySuite", 4, 1, 0, 0, Nil, Nil, List("testBaz"), 100)
    BuildDiff.formatSuiteDiff(diff) shouldBe "proj MySuite: 4 passed, 1 failed (testBaz still failing)"
  }

  test("formatSuiteDiff: mixed — fixed, new failure, still failing") {
    val diff = BuildDiff.SuiteDiff("proj", "MySuite", 3, 2, 0, 0, List("testNew"), List("testFixed"), List("testStuck"), 100)
    BuildDiff.formatSuiteDiff(diff) shouldBe "proj MySuite: 3 passed, 2 failed (testFixed fixed, testNew new failure, testStuck still failing)"
  }

  // ==========================================================================
  // BuildDiff.formatCompileSummary — end-of-build summary
  // ==========================================================================

  test("formatCompileSummary: all clear, no history") {
    BuildDiff.formatCompileSummary(10, 0, 0, 0, 0) shouldBe "Build: 10 projects compiled, all clear"
  }

  test("formatCompileSummary: all clear after fixing errors") {
    BuildDiff.formatCompileSummary(10, 0, 0, 0, 3) shouldBe "Build: 10 projects compiled, all clear (fixed 3 errors)"
  }

  test("formatCompileSummary: errors with new and fixed and remaining") {
    BuildDiff.formatCompileSummary(10, 2, 0, 1, 1) shouldBe "Build: 10 projects compiled, 2 errors (1 fixed, 1 new, 1 remaining)"
  }

  test("formatCompileSummary: errors with new and remaining") {
    BuildDiff.formatCompileSummary(10, 3, 0, 2, 0) shouldBe "Build: 10 projects compiled, 3 errors (2 new, 1 remaining)"
  }

  test("formatCompileSummary: singular error") {
    BuildDiff.formatCompileSummary(5, 1, 0, 1, 0) shouldBe "Build: 5 projects compiled, 1 error (1 new)"
  }

  test("formatCompileSummary: fixed singular error") {
    BuildDiff.formatCompileSummary(5, 0, 0, 0, 1) shouldBe "Build: 5 projects compiled, all clear (fixed 1 error)"
  }

  // ==========================================================================
  // BuildDiff.formatTestSummary — end-of-build summary
  // ==========================================================================

  test("formatTestSummary: all passing, no history") {
    BuildDiff.formatTestSummary(20, 0, 0, 0) shouldBe "Tests: 20 passed"
  }

  test("formatTestSummary: all passing after fixing") {
    BuildDiff.formatTestSummary(20, 0, 0, 2) shouldBe "Tests: 20 passed (2 fixed)"
  }

  test("formatTestSummary: failures with new") {
    BuildDiff.formatTestSummary(18, 2, 2, 0) shouldBe "Tests: 18 passed, 2 failed (2 new failures)"
  }

  test("formatTestSummary: failures with fixed and new") {
    BuildDiff.formatTestSummary(19, 1, 1, 1) shouldBe "Tests: 19 passed, 1 failed (1 fixed, 1 new failure)"
  }

  test("formatTestSummary: singular new failure") {
    BuildDiff.formatTestSummary(9, 1, 1, 0) shouldBe "Tests: 9 passed, 1 failed (1 new failure)"
  }

  // ==========================================================================
  // DiagKey — line number stripping
  // ==========================================================================

  test("diffCompile: same file different line numbers treated as same diagnostic") {
    val prev = List(errorDiag("type mismatch", "src/Foo.scala:10:5"))
    val curr = List(errorDiag("type mismatch", "src/Foo.scala:99:1"))

    val diff = BuildDiff.diffCompile("proj", CompileStatus.Failed, curr, prev, 100)
    // Line stripped — same file + same message = same error
    diff.newErrors shouldBe 0
    diff.fixedErrors shouldBe 0
  }

  test("diffCompile: same message different file treated as different diagnostic") {
    val prev = List(errorDiag("type mismatch", "src/Foo.scala:10:5"))
    val curr = List(errorDiag("type mismatch", "src/Bar.scala:10:5"))

    val diff = BuildDiff.diffCompile("proj", CompileStatus.Failed, curr, prev, 100)
    diff.newErrors shouldBe 1 // Bar.scala error is new
    diff.fixedErrors shouldBe 1 // Foo.scala error was fixed
  }

  test("diffCompile: path without line:col is preserved as-is") {
    val prev = List(errorDiag("error", "Foo.scala"))
    val curr = List(errorDiag("error", "Foo.scala"))

    val diff = BuildDiff.diffCompile("proj", CompileStatus.Failed, curr, prev, 100)
    diff.newErrors shouldBe 0
    diff.fixedErrors shouldBe 0
  }

  // ==========================================================================
  // Edge cases
  // ==========================================================================

  test("diffCompile: two identical errors same file are both counted as new") {
    val current = List(
      errorDiag("Found: Int, Required: String", "Foo.scala:10:5"),
      errorDiag("Found: Int, Required: String", "Foo.scala:20:5") // same file+message — counted via multiset
    )

    val diff = BuildDiff.diffCompile("proj", CompileStatus.Failed, current, Nil, 100)
    diff.totalErrors shouldBe 2
    diff.newErrors shouldBe 2 // multiset: 0 previous, 2 current = 2 new
  }

  test("diffCompile: fixing one of two identical errors shows 1 fixed") {
    val previous = List(
      errorDiag("Found: Int, Required: String", "Foo.scala:10:5"),
      errorDiag("Found: Int, Required: String", "Foo.scala:20:5")
    )
    val current = List(
      errorDiag("Found: Int, Required: String", "Foo.scala:10:5")
    )

    val diff = BuildDiff.diffCompile("proj", CompileStatus.Failed, current, previous, 100)
    diff.totalErrors shouldBe 1
    diff.newErrors shouldBe 0
    diff.fixedErrors shouldBe 1 // had 2, now 1 = 1 fixed
  }

  test("diffCompile: adding a third identical error shows 1 new") {
    val previous = List(
      errorDiag("Found: Int, Required: String", "Foo.scala:10:5"),
      errorDiag("Found: Int, Required: String", "Foo.scala:20:5")
    )
    val current = List(
      errorDiag("Found: Int, Required: String", "Foo.scala:10:5"),
      errorDiag("Found: Int, Required: String", "Foo.scala:20:5"),
      errorDiag("Found: Int, Required: String", "Foo.scala:30:5")
    )

    val diff = BuildDiff.diffCompile("proj", CompileStatus.Failed, current, previous, 100)
    diff.totalErrors shouldBe 3
    diff.newErrors shouldBe 1 // had 2, now 3 = 1 new
    diff.fixedErrors shouldBe 0
  }

  test("diffCompile: different messages same file are distinct") {
    val current = List(
      errorDiag("Found: Int, Required: String", "Foo.scala:10:5"),
      errorDiag("Found: Boolean, Required: List[Int]", "Foo.scala:20:5")
    )

    val diff = BuildDiff.diffCompile("proj", CompileStatus.Failed, current, Nil, 100)
    diff.totalErrors shouldBe 2
    diff.newErrors shouldBe 2 // different messages = different keys
  }

  test("PreviousRunState.empty has no diagnostics and no test results") {
    val state = PreviousRunState.empty
    state.compileDiagnostics shouldBe Map.empty
    state.testResults shouldBe Map.empty
  }

  test("diffSuite: cancelled status in previous counts as failure for diff") {
    val previousResults = Map(
      TestKey("proj", "MySuite", "testCancel") -> TestStatus.Cancelled
    )
    val currentTests = List(
      testFinished("proj", "MySuite", "testCancel", TestStatus.Passed)
    )

    val diff = BuildDiff.diffSuite("proj", "MySuite", currentTests, previousResults, passed = 1, failed = 0, skipped = 0, ignored = 0, durationMs = 100)

    diff.fixedTests shouldBe List("testCancel")
  }

  test("diffSuite: skipped previous status is not treated as failure") {
    val previousResults = Map(
      TestKey("proj", "MySuite", "testSkip") -> TestStatus.Skipped
    )
    val currentTests = List(
      testFinished("proj", "MySuite", "testSkip", TestStatus.Passed)
    )

    val diff = BuildDiff.diffSuite("proj", "MySuite", currentTests, previousResults, passed = 1, failed = 0, skipped = 0, ignored = 0, durationMs = 100)

    // skipped -> passed is not a "fix" (skipped was never a failure)
    diff.fixedTests shouldBe empty
    diff.newFailures shouldBe empty
  }

  test("diffCompile: skipped status with fixed errors reports correctly") {
    val previous = List(errorDiag("missing import", "A.scala:1:1"))
    val diff = BuildDiff.diffCompile("proj", CompileStatus.Skipped, Nil, previous, 0)

    diff.status shouldBe CompileStatus.Skipped
    diff.fixedErrors shouldBe 1
    BuildDiff.formatCompileDiff(diff) shouldBe "proj: OK (fixed 1 error) (0ms)"
  }
}
