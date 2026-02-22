package bleep.testing

import bleep.bsp.protocol.BleepBspProtocol
import java.nio.file.Path

/** Events emitted during build execution for progress tracking and reporting */
sealed trait BuildEvent {
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

object BuildEvent {

  /** A project has started compiling */
  case class CompileStarted(
      project: String,
      timestamp: Long
  ) extends BuildEvent

  /** Why compilation is being triggered */
  case class CompilationReason(
      project: String,
      reason: String, // "clean-build", "empty-output", "incremental", "up-to-date"
      totalFiles: Int,
      invalidatedFiles: List[String],
      changedDependencies: List[String],
      timestamp: Long
  ) extends BuildEvent {

    /** Format the reason for display */
    def formatted: String = reason match {
      case "clean-build"  => s"$project: clean build (no previous analysis)"
      case "empty-output" => s"$project: clean build (output directory empty)"
      case "up-to-date"   => s"$project: up to date"
      case "incremental" =>
        val invalidatedCount = invalidatedFiles.size
        val depCount = changedDependencies.size

        val invalidatedStr =
          if (invalidatedFiles.isEmpty) ""
          else {
            val fileNames = invalidatedFiles.take(5)
            val suffix = if (invalidatedFiles.size > 5) s", ... (${invalidatedFiles.size - 5} more)" else ""
            s"$invalidatedCount/$totalFiles files invalidated (${fileNames.mkString(", ")}$suffix)"
          }

        val depStr =
          if (changedDependencies.isEmpty) ""
          else {
            val depNames = changedDependencies.take(3)
            val suffix = if (changedDependencies.size > 3) s", ... (${changedDependencies.size - 3} more)" else ""
            s"$depCount changed dependencies (${depNames.mkString(", ")}$suffix)"
          }

        val parts = List(invalidatedStr, depStr).filter(_.nonEmpty)
        if (parts.isEmpty) s"$project: incremental (changes detected)"
        else s"$project: ${parts.mkString("; ")}"
      case other => s"$project: $other"
    }
  }

  /** A project has finished compiling */
  case class CompileFinished(
      project: String,
      status: String, // "success", "failed", "error", "skipped", "cancelled"
      durationMs: Long,
      timestamp: Long,
      diagnostics: List[BleepBspProtocol.Diagnostic] = Nil,
      skippedBecause: Option[String] = None // CrossProjectName.value of failed dependency
  ) extends BuildEvent

  /** Compilation progress update */
  case class CompileProgress(
      project: String,
      percent: Int,
      timestamp: Long
  ) extends BuildEvent

  /** Compilation sub-phase transition (reading analysis, analyzing, compiling, saving) */
  case class CompilePhaseChanged(
      project: String,
      phase: String, // "reading-analysis", "analyzing", "compiling", "saving-analysis"
      trackedApis: Int,
      timestamp: Long
  ) extends BuildEvent

  /** Compilation is stalled due to heap pressure — waiting for GC to recover */
  case class CompileStalled(
      project: String,
      heapUsedMb: Long,
      heapMaxMb: Long,
      retryAtMs: Long,
      timestamp: Long
  ) extends BuildEvent

  /** Compilation resumed after heap pressure subsided */
  case class CompileResumed(
      project: String,
      heapUsedMb: Long,
      heapMaxMb: Long,
      stalledMs: Long,
      timestamp: Long
  ) extends BuildEvent

  /** A test suite has started execution */
  case class SuiteStarted(
      project: String,
      suite: String,
      timestamp: Long
  ) extends BuildEvent

  /** An individual test has started */
  case class TestStarted(
      project: String,
      suite: String,
      test: String,
      timestamp: Long
  ) extends BuildEvent

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
  ) extends BuildEvent

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
  ) extends BuildEvent

  /** A test suite timed out */
  case class SuiteTimedOut(
      project: String,
      suite: String,
      timeoutMs: Long,
      threadDump: Option[ThreadDumpInfo],
      timestamp: Long
  ) extends BuildEvent

  /** A test suite crashed or errored (process killed, OOM, etc.) - distinct from logical test failure */
  case class SuiteError(
      project: String,
      suite: String,
      error: String,
      exitCode: Option[Int],
      signal: Option[Int],
      durationMs: Long,
      timestamp: Long
  ) extends BuildEvent

  /** A test suite was cancelled (e.g., due to timeout or user cancellation) */
  case class SuiteCancelled(
      project: String,
      suite: String,
      reason: Option[String],
      timestamp: Long
  ) extends BuildEvent

  /** Output from a test (stdout/stderr) */
  case class Output(
      project: String,
      suite: String,
      line: String,
      isError: Boolean,
      timestamp: Long
  ) extends BuildEvent

  /** Test suites discovered for a project */
  case class SuitesDiscovered(
      project: String,
      suites: List[String],
      totalDiscovered: Int,
      timestamp: Long
  ) extends BuildEvent

  /** A project was skipped because a dependency failed to compile */
  case class ProjectSkipped(
      project: String,
      reason: String,
      timestamp: Long
  ) extends BuildEvent

  /** A project has started linking (Scala.js, Scala Native, etc.) */
  case class LinkStarted(
      project: String,
      platform: String,
      timestamp: Long
  ) extends BuildEvent

  /** A project has finished linking successfully */
  case class LinkSucceeded(
      project: String,
      platform: String,
      durationMs: Long,
      timestamp: Long
  ) extends BuildEvent

  /** A project has failed to link */
  case class LinkFailed(
      project: String,
      platform: String,
      durationMs: Long,
      error: String,
      timestamp: Long
  ) extends BuildEvent

  /** A sourcegen script has started running */
  case class SourcegenStarted(
      scriptMain: String,
      forProjects: List[String],
      timestamp: Long
  ) extends BuildEvent {
    def project: String = scriptMain // Use script main class as the "project" identifier
  }

  /** A sourcegen script has finished */
  case class SourcegenFinished(
      scriptMain: String,
      success: Boolean,
      durationMs: Long,
      error: Option[String],
      timestamp: Long
  ) extends BuildEvent {
    def project: String = scriptMain
  }

  /** An error occurred during test execution */
  case class Error(
      project: String,
      message: String,
      details: Option[String],
      timestamp: Long
  ) extends BuildEvent

  /** The workspace is busy — another connection is running a build operation */
  case class WorkspaceBusy(
      project: String, // empty string
      operation: String,
      waitingProjects: List[String],
      startedAgoMs: Long,
      timestamp: Long
  ) extends BuildEvent

  /** The workspace has become available after being busy */
  case class WorkspaceReady(
      project: String, // empty string
      timestamp: Long
  ) extends BuildEvent

  /** BSP connection was lost (server died, socket closed). All currently running suites should be marked as cancelled since their results will never arrive.
    */
  case class ConnectionLost(
      project: String,
      timestamp: Long
  ) extends BuildEvent

  /** Authoritative test run completion from BSP response (not notification).
    *
    * The BSP TestResult response carries this in its data field. Because it uses reliable request-response (not fire-and-forget notifications), the client can
    * use these counts as the definitive source of truth for the summary — even if individual SuiteFinished notifications were lost.
    */
  case class TestRunCompleted(
      project: String,
      totalPassed: Int,
      totalFailed: Int,
      totalSkipped: Int,
      totalIgnored: Int,
      suitesTotal: Int,
      suitesCompleted: Int,
      suitesFailed: Int,
      suitesCancelled: Int,
      durationMs: Long,
      timestamp: Long
  ) extends BuildEvent
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
    val isFailure: Boolean = true
  }
  case object AssumptionFailed extends TestStatus {
    val isFailure: Boolean = false
  }
  case object Pending extends TestStatus {
    val isFailure: Boolean = false // ScalaTest "pending" = not yet implemented, similar to ignored
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
    case "assumption-failed"  => AssumptionFailed
    case "pending"            => Pending
    case "timeout"            => Timeout
    case _                    => Failed
  }
}
