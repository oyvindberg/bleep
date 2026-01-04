package bleep.testing

import java.nio.file.Path

/** Events emitted during test execution for progress tracking and reporting */
sealed trait TestEvent {
  def timestamp: Long
  def project: String
}

/** Thread dump information from a timed-out test */
case class ThreadDumpInfo(
    /** Number of active (non-waiting) threads */
    activeThreadCount: Int,
    /** If single active thread, its stack trace (first N lines) */
    singleThreadStack: Option[String],
    /** If multiple threads, path to temp file with full dump */
    dumpFile: Option[Path]
)

object TestEvent {

  /** A project has started compiling */
  case class CompileStarted(
      project: String,
      timestamp: Long
  ) extends TestEvent

  /** A project has finished compiling */
  case class CompileFinished(
      project: String,
      success: Boolean,
      durationMs: Long,
      timestamp: Long,
      errors: List[String] = Nil
  ) extends TestEvent

  /** Compilation progress update */
  case class CompileProgress(
      project: String,
      percent: Int,
      timestamp: Long
  ) extends TestEvent

  /** A test suite has started execution */
  case class SuiteStarted(
      project: String,
      suite: String,
      timestamp: Long
  ) extends TestEvent

  /** An individual test has started */
  case class TestStarted(
      project: String,
      suite: String,
      test: String,
      timestamp: Long
  ) extends TestEvent

  /** An individual test has finished */
  case class TestFinished(
      project: String,
      suite: String,
      test: String,
      status: TestStatus,
      durationMs: Long,
      message: Option[String],
      throwable: Option[String],
      timestamp: Long
  ) extends TestEvent

  /** A test suite has finished execution */
  case class SuiteFinished(
      project: String,
      suite: String,
      passed: Int,
      failed: Int,
      skipped: Int,
      ignored: Int,
      durationMs: Long,
      timestamp: Long
  ) extends TestEvent

  /** A test suite timed out */
  case class SuiteTimedOut(
      project: String,
      suite: String,
      timeoutMs: Long,
      threadDump: Option[ThreadDumpInfo],
      timestamp: Long
  ) extends TestEvent

  /** Output from a test (stdout/stderr) */
  case class Output(
      project: String,
      suite: String,
      line: String,
      isError: Boolean,
      timestamp: Long
  ) extends TestEvent

  /** Test suites discovered (from pre-compilation scan of already-compiled classes) */
  case class SuitesDiscovered(
      count: Int,
      timestamp: Long
  ) extends TestEvent {
    val project: String = "" // Global event, not project-specific
  }

  /** A project was skipped because a dependency failed to compile */
  case class ProjectSkipped(
      project: String,
      reason: String,
      timestamp: Long
  ) extends TestEvent
}

/** Status of an individual test */
sealed trait TestStatus {
  def isFailure: Boolean
}

object TestStatus {
  case object Passed extends TestStatus {
    val isFailure: Boolean = false
  }
  case object Failed extends TestStatus {
    val isFailure: Boolean = true
  }
  case object Error extends TestStatus {
    val isFailure: Boolean = true
  }
  case object Skipped extends TestStatus {
    val isFailure: Boolean = false
  }
  case object Ignored extends TestStatus {
    val isFailure: Boolean = false
  }
  case object Cancelled extends TestStatus {
    val isFailure: Boolean = false
  }
  case object Pending extends TestStatus {
    val isFailure: Boolean = false
  }
  case object Timeout extends TestStatus {
    val isFailure: Boolean = true
  }

  def fromString(s: String): TestStatus = s.toLowerCase match {
    case "passed" | "success" => Passed
    case "failed" | "failure" => Failed
    case "error"              => Error
    case "skipped"            => Skipped
    case "ignored"            => Ignored
    case "cancelled"          => Cancelled
    case "pending"            => Pending
    case "timeout"            => Timeout
    case _                    => Failed
  }
}
