package bleep.testing

import bleep.bsp.protocol.{BleepBspProtocol, CompilePhase, CompileReason, CompileStatus, LinkPlatformName, OutputChannel, ProcessExit, TestStatus}
import bleep.model.{CrossProjectName, SuiteName, TestName}
import java.nio.file.Path

/** Events emitted during build execution for progress tracking and reporting */
sealed trait BuildEvent {
  def timestamp: Long
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
      project: CrossProjectName,
      timestamp: Long
  ) extends BuildEvent

  /** Why compilation is being triggered */
  case class CompilationReason(
      project: CrossProjectName,
      reason: CompileReason,
      totalFiles: Int,
      invalidatedFiles: List[String],
      changedDependencies: List[String],
      timestamp: Long
  ) extends BuildEvent {

    /** Format the reason for display */
    def formatted: String = reason match {
      case CompileReason.CleanBuild  => s"${project.value}: clean build (no previous analysis)"
      case CompileReason.EmptyOutput => s"${project.value}: clean build (output directory empty)"
      case CompileReason.UpToDate    => s"${project.value}: up to date"
      case CompileReason.Incremental =>
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
        if (parts.isEmpty) s"${project.value}: incremental (changes detected)"
        else s"${project.value}: ${parts.mkString("; ")}"
    }
  }

  /** A project has finished compiling */
  case class CompileFinished(
      project: CrossProjectName,
      status: CompileStatus,
      durationMs: Long,
      timestamp: Long,
      diagnostics: List[BleepBspProtocol.Diagnostic],
      skippedBecause: Option[CrossProjectName]
  ) extends BuildEvent

  /** Compilation progress update */
  case class CompileProgress(
      project: CrossProjectName,
      percent: Int,
      timestamp: Long
  ) extends BuildEvent

  /** Compilation sub-phase transition (reading analysis, analyzing, compiling, saving) */
  case class CompilePhaseChanged(
      project: CrossProjectName,
      phase: CompilePhase,
      trackedApis: Int,
      timestamp: Long
  ) extends BuildEvent

  /** Compilation is stalled due to heap pressure — waiting for GC to recover */
  case class CompileStalled(
      project: CrossProjectName,
      heapUsedMb: Long,
      heapMaxMb: Long,
      retryAtMs: Long,
      timestamp: Long
  ) extends BuildEvent

  /** Compilation resumed after heap pressure subsided */
  case class CompileResumed(
      project: CrossProjectName,
      heapUsedMb: Long,
      heapMaxMb: Long,
      stalledMs: Long,
      timestamp: Long
  ) extends BuildEvent

  /** A project is waiting to acquire its compile lock (another compile is holding it) */
  case class LockContention(
      project: CrossProjectName,
      waitingMs: Long,
      timestamp: Long
  ) extends BuildEvent

  /** A project acquired its compile lock after waiting */
  case class LockAcquired(
      project: CrossProjectName,
      waitedMs: Long,
      timestamp: Long
  ) extends BuildEvent

  /** A test suite has started execution */
  case class SuiteStarted(
      project: CrossProjectName,
      suite: SuiteName,
      timestamp: Long
  ) extends BuildEvent

  /** An individual test has started */
  case class TestStarted(
      project: CrossProjectName,
      suite: SuiteName,
      test: TestName,
      timestamp: Long
  ) extends BuildEvent

  /** An individual test has finished */
  case class TestFinished(
      project: CrossProjectName,
      suite: SuiteName,
      test: TestName,
      status: TestStatus,
      durationMs: Long,
      message: Option[String],
      throwable: Option[String],
      timestamp: Long
  ) extends BuildEvent

  /** A test suite has finished execution */
  case class SuiteFinished(
      project: CrossProjectName,
      suite: SuiteName,
      passed: Int,
      failed: Int,
      skipped: Int,
      ignored: Int,
      durationMs: Long,
      timestamp: Long
  ) extends BuildEvent

  /** A test suite timed out */
  case class SuiteTimedOut(
      project: CrossProjectName,
      suite: SuiteName,
      timeoutMs: Long,
      threadDump: Option[ThreadDumpInfo],
      timestamp: Long
  ) extends BuildEvent

  /** A test suite crashed or errored (process killed, OOM, etc.) - distinct from logical test failure */
  case class SuiteError(
      project: CrossProjectName,
      suite: SuiteName,
      error: String,
      processExit: ProcessExit,
      durationMs: Long,
      timestamp: Long
  ) extends BuildEvent

  /** A test suite was cancelled (e.g., due to timeout or user cancellation) */
  case class SuiteCancelled(
      project: CrossProjectName,
      suite: SuiteName,
      reason: Option[String],
      timestamp: Long
  ) extends BuildEvent

  /** Output from a test (stdout/stderr) */
  case class Output(
      project: CrossProjectName,
      suite: SuiteName,
      line: String,
      channel: OutputChannel,
      timestamp: Long
  ) extends BuildEvent

  /** Test suites discovered for a project */
  case class SuitesDiscovered(
      project: CrossProjectName,
      suites: List[SuiteName],
      totalDiscovered: Int,
      timestamp: Long
  ) extends BuildEvent

  /** A project was skipped because a dependency failed to compile */
  case class ProjectSkipped(
      project: CrossProjectName,
      reason: String,
      timestamp: Long
  ) extends BuildEvent

  /** A project has started linking (Scala.js, Scala Native, etc.) */
  case class LinkStarted(
      project: CrossProjectName,
      platform: LinkPlatformName,
      timestamp: Long
  ) extends BuildEvent

  /** A project has finished linking successfully */
  case class LinkSucceeded(
      project: CrossProjectName,
      platform: LinkPlatformName,
      durationMs: Long,
      timestamp: Long
  ) extends BuildEvent

  /** A project has failed to link */
  case class LinkFailed(
      project: CrossProjectName,
      platform: LinkPlatformName,
      durationMs: Long,
      error: String,
      timestamp: Long
  ) extends BuildEvent

  /** A sourcegen script has started running */
  case class SourcegenStarted(
      scriptMain: String,
      forProjects: List[CrossProjectName],
      timestamp: Long
  ) extends BuildEvent

  /** A sourcegen script has finished */
  case class SourcegenFinished(
      scriptMain: String,
      success: Boolean,
      durationMs: Long,
      error: Option[String],
      timestamp: Long
  ) extends BuildEvent

  /** Annotation-processor resolution finished for a project. `success=false` means the AP DAG handler errored (no-op opt-in or coursier resolution failure). */
  case class ResolveAnnotationProcessorsFinished(
      project: CrossProjectName,
      success: Boolean,
      durationMs: Long,
      error: Option[String],
      timestamp: Long
  ) extends BuildEvent

  /** An error occurred during test execution */
  case class Error(
      message: String,
      details: Option[String],
      timestamp: Long
  ) extends BuildEvent

  /** The workspace is busy — another connection is running a build operation */
  case class WorkspaceBusy(
      operation: String,
      waitingProjects: List[CrossProjectName],
      startedAgoMs: Long,
      timestamp: Long
  ) extends BuildEvent

  /** The workspace has become available after being busy */
  case class WorkspaceReady(
      timestamp: Long
  ) extends BuildEvent

  /** BSP connection was lost (server died, socket closed). All currently running suites should be marked as cancelled since their results will never arrive.
    */
  case class ConnectionLost(
      timestamp: Long
  ) extends BuildEvent

  /** Authoritative test run completion from BSP response (not notification).
    *
    * The BSP TestResult response carries this in its data field. Because it uses reliable request-response (not fire-and-forget notifications), the client can
    * use these counts as the definitive source of truth for the summary — even if individual SuiteFinished notifications were lost.
    */
  case class TestRunCompleted(
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
