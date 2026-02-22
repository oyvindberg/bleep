package bleep.bsp.protocol

import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._

/** Protocol for streaming rich build events over BSP.
  *
  * Events are sent via standard BSP notifications (build/taskProgress) with dataKind="bleep" and the event JSON in the data field.
  *
  * This allows FancyBuildDisplay to receive rich events whether running via bloop or bleep-bsp.
  */
object BleepBspProtocol {

  // ==========================================================================
  // Link Directory Suffix (shared between bleep-bsp and bleep-core)
  // ==========================================================================

  /** Compute config-aware output directory suffix based on link options.
    *
    * Different link configurations (debug vs release, optimized vs not) produce different binaries. Using separate directories prevents stale binary issues
    * where an optimized binary would incorrectly satisfy an up-to-date check for a debug build.
    *
    * @param isRelease
    *   whether release/optimized mode is enabled
    * @param hasDebugInfo
    *   whether debug info is enabled (for native platforms)
    * @param hasLto
    *   whether link-time optimization is enabled (for Scala Native)
    * @return
    *   directory suffix like "debug", "release", "debug-g", "release-lto"
    */
  def linkDirSuffix(isRelease: Boolean, hasDebugInfo: Boolean, hasLto: Boolean): String =
    (isRelease, hasDebugInfo, hasLto) match {
      case (false, false, false) => "debug"
      case (false, true, false)  => "debug-g"
      case (false, false, true)  => "debug-lto"
      case (false, true, true)   => "debug-g-lto"
      case (true, false, false)  => "release"
      case (true, true, false)   => "release-g"
      case (true, false, true)   => "release-lto"
      case (true, true, true)    => "release-g-lto"
    }

  /** The dataKind used in BSP events */
  val DataKind: String = "bleep"

  /** The dataKind used in BSP TestParams for test options */
  val TestOptionsDataKind: String = "bleep-test-options"

  /** The dataKind used in TestResult.data for reliable delivery of test run results */
  val TestRunResultDataKind: String = "bleep-test-run-result"

  // ==========================================================================
  // Response Extensions (server -> client, in TestResult.data)
  // ==========================================================================

  /** Complete test run result, included in TestResult.data for reliable delivery.
    *
    * Notifications (build/taskProgress) are fire-and-forget — if any are lost, the client's accumulated counts will be wrong. This result is returned in the
    * TestResult response (request-response, reliable) so the client can use it as the authoritative source of truth for the summary.
    */
  case class TestRunResult(
      totalPassed: Int,
      totalFailed: Int,
      totalSkipped: Int,
      totalIgnored: Int,
      suitesTotal: Int,
      suitesCompleted: Int,
      suitesFailed: Int,
      suitesCancelled: Int,
      durationMs: Long
  )

  object TestRunResult {
    implicit val codec: Codec[TestRunResult] = deriveCodec

    def encode(result: TestRunResult): String = result.asJson.noSpaces

    def decode(json: String): Either[io.circe.Error, TestRunResult] =
      io.circe.parser.decode[TestRunResult](json)
  }

  // ==========================================================================
  // Request Extensions (client -> server)
  // ==========================================================================

  /** Options passed via TestParams.data field */
  case class TestOptions(
      jvmOptions: List[String],
      testArgs: List[String],
      only: List[String],
      exclude: List[String],
      flamegraph: Boolean = false
  )

  object TestOptions {
    val empty: TestOptions = TestOptions(Nil, Nil, Nil, Nil, false)

    implicit val codec: Codec[TestOptions] = deriveCodec

    def encode(options: TestOptions): String = options.asJson.noSpaces

    def decode(json: String): Either[io.circe.Error, TestOptions] =
      io.circe.parser.decode[TestOptions](json)
  }

  // ==========================================================================
  // Build Mode
  // ==========================================================================

  /** The type of build operation being performed */
  sealed trait BuildMode

  object BuildMode {
    case object Compile extends BuildMode
    case class Link(releaseMode: Boolean) extends BuildMode
    case object Test extends BuildMode
    case class Run(mainClass: String, args: List[String]) extends BuildMode

    implicit val linkCodec: Codec[Link] = deriveCodec
    implicit val runCodec: Codec[Run] = deriveCodec

    implicit val encoder: Encoder[BuildMode] = Encoder.instance {
      case Compile => Json.obj("type" -> "Compile".asJson)
      case l: Link => Json.obj("type" -> "Link".asJson, "data" -> l.asJson)
      case Test    => Json.obj("type" -> "Test".asJson)
      case r: Run  => Json.obj("type" -> "Run".asJson, "data" -> r.asJson)
    }

    implicit val decoder: Decoder[BuildMode] = Decoder.instance { cursor =>
      cursor.downField("type").as[String].flatMap {
        case "Compile" => Right(Compile)
        case "Link"    => cursor.downField("data").as[Link]
        case "Test"    => Right(Test)
        case "Run"     => cursor.downField("data").as[Run]
        case other     => Left(DecodingFailure(s"Unknown BuildMode type: $other", cursor.history))
      }
    }
  }

  // ==========================================================================
  // Shared types
  // ==========================================================================

  /** A compiler diagnostic with severity */
  case class Diagnostic(
      severity: String, // "error", "warning", "info"
      message: String
  )

  object Diagnostic {
    implicit val codec: Codec[Diagnostic] = deriveCodec

    def error(message: String): Diagnostic = Diagnostic("error", message)
    def warning(message: String): Diagnostic = Diagnostic("warning", message)
    def info(message: String): Diagnostic = Diagnostic("info", message)
  }

  // ==========================================================================
  // Events (server -> client via build/taskProgress)
  // ==========================================================================

  /** Events sent from bleep-bsp to the client */
  sealed trait Event {
    def timestamp: Long
  }

  object Event {

    // === Compile events ===

    case class CompileStarted(
        project: String,
        timestamp: Long
    ) extends Event

    /** Why compilation is being triggered */
    case class CompilationReason(
        project: String,
        reason: String, // "clean-build", "empty-output", "incremental", "up-to-date"
        totalFiles: Int,
        invalidatedFiles: List[String], // file names (not full paths)
        changedDependencies: List[String], // dependency names
        timestamp: Long
    ) extends Event

    case class CompileProgress(
        project: String,
        percentage: Int,
        timestamp: Long
    ) extends Event

    /** Compilation sub-phase transition (reading analysis, analyzing, compiling, saving) */
    case class CompilePhaseChanged(
        project: String,
        phase: String, // "reading-analysis", "analyzing", "compiling", "saving-analysis"
        trackedApis: Int, // number of tracked API structures from analysis (non-zero only for "reading-analysis")
        timestamp: Long
    ) extends Event

    case class CompileFinished(
        project: String,
        status: String, // "success", "failed", "error", "skipped", "cancelled"
        durationMs: Long,
        diagnostics: List[BleepBspProtocol.Diagnostic],
        skippedBecause: Option[String], // CrossProjectName.value of the dependency whose failure caused this to be skipped
        timestamp: Long
    ) extends Event

    /** Compilation is stalled due to heap pressure — waiting for GC to recover */
    case class CompileStalled(
        project: String,
        heapUsedMb: Long,
        heapMaxMb: Long,
        retryAtMs: Long,
        timestamp: Long
    ) extends Event

    /** Compilation resumed after heap pressure subsided */
    case class CompileResumed(
        project: String,
        heapUsedMb: Long,
        heapMaxMb: Long,
        stalledMs: Long,
        timestamp: Long
    ) extends Event

    // === Link events (Scala.js, Scala Native, Kotlin/JS, Kotlin/Native) ===

    case class LinkStarted(
        project: String,
        platform: String, // "Scala.js", "Scala Native", "Kotlin/JS", "Kotlin/Native"
        timestamp: Long
    ) extends Event

    case class LinkProgress(
        project: String,
        phase: String,
        timestamp: Long
    ) extends Event

    case class LinkFinished(
        project: String,
        success: Boolean,
        durationMs: Long,
        outputPath: Option[String],
        timestamp: Long,
        platform: String,
        error: Option[String]
    ) extends Event

    // === Sourcegen events ===

    case class SourcegenStarted(
        scriptMain: String, // e.g., "bleep.scripts.GenSources"
        forProjects: List[String], // projects that will use the generated sources
        timestamp: Long
    ) extends Event

    case class SourcegenFinished(
        scriptMain: String,
        success: Boolean,
        durationMs: Long,
        error: Option[String],
        timestamp: Long
    ) extends Event

    // === Discovery events ===

    case class DiscoveryStarted(
        project: String,
        timestamp: Long
    ) extends Event

    case class SuitesDiscovered(
        project: String,
        suites: List[String],
        totalSuitesDiscovered: Int,
        timestamp: Long
    ) extends Event

    // === Test execution events ===

    case class SuiteStarted(
        project: String,
        suite: String,
        timestamp: Long
    ) extends Event

    case class TestStarted(
        project: String,
        suite: String,
        test: String,
        timestamp: Long
    ) extends Event

    case class TestFinished(
        project: String,
        suite: String,
        test: String,
        status: String, // passed, failed, error, skipped, ignored, pending, cancelled
        durationMs: Long,
        message: Option[String],
        throwable: Option[String],
        timestamp: Long
    ) extends Event

    case class SuiteFinished(
        project: String,
        suite: String,
        passed: Int,
        failed: Int,
        skipped: Int,
        ignored: Int,
        durationMs: Long,
        timestamp: Long
    ) extends Event

    case class SuiteTimedOut(
        project: String,
        suite: String,
        timeoutMs: Long,
        threadDump: Option[String],
        timestamp: Long
    ) extends Event

    /** A test suite crashed or errored (process killed, OOM, etc.) - distinct from logical test failure */
    case class SuiteError(
        project: String,
        suite: String,
        error: String,
        exitCode: Option[Int],
        signal: Option[Int],
        durationMs: Long,
        timestamp: Long
    ) extends Event

    /** A test suite was cancelled (e.g., due to timeout or external cancellation) */
    case class SuiteCancelled(
        project: String,
        suite: String,
        reason: Option[String],
        timestamp: Long
    ) extends Event

    // === Run events ===

    case class RunStarted(
        project: String,
        mainClass: String,
        timestamp: Long
    ) extends Event

    case class RunOutput(
        project: String,
        line: String,
        isError: Boolean,
        timestamp: Long
    ) extends Event

    case class RunFinished(
        project: String,
        exitCode: Int,
        durationMs: Long,
        timestamp: Long
    ) extends Event

    // === Output events ===

    case class Output(
        project: String,
        suite: String,
        line: String,
        isError: Boolean,
        timestamp: Long
    ) extends Event

    // === Log events (structured logging) ===

    case class LogMessage(
        project: Option[String],
        level: String, // "info", "warn", "error", "debug"
        message: String,
        timestamp: Long
    ) extends Event

    // === Overall progress ===

    /** Information about a project in the build */
    case class ProjectInfo(
        name: String,
        isTestProject: Boolean,
        dependencies: List[String] // Names of projects this depends on
    )

    /** Sent at the start of a build run to describe all projects involved */
    case class BuildInitialized(
        projects: List[ProjectInfo],
        mode: String, // "compile", "link", "test", "run"
        timestamp: Long
    ) extends Event

    /** Sent when a project's state changes */
    case class ProjectStateChanged(
        project: String,
        state: String, // pending, compiling, compiled, compile_failed, linking, linked, discovering, testing, completed, skipped
        details: Option[String], // e.g., "waiting for dependency X" or error message
        timestamp: Long
    ) extends Event

    case class ProjectSkipped(
        project: String,
        reason: String,
        timestamp: Long
    ) extends Event

    case class BuildFinished(
        success: Boolean,
        durationMs: Long,
        timestamp: Long
    ) extends Event

    // === Test run summary (backward compat with TestRunFinished) ===

    case class TestRunFinished(
        totalPassed: Int,
        totalFailed: Int,
        totalSkipped: Int,
        totalIgnored: Int,
        durationMs: Long,
        timestamp: Long
    ) extends Event

    // === Workspace coordination events ===

    /** Sent to a waiting client when the workspace they want is already busy with another operation */
    case class WorkspaceBusy(
        operation: String,
        projects: List[String],
        startedAgoMs: Long,
        timestamp: Long
    ) extends Event

    /** Sent to a waiting client when the workspace becomes available */
    case class WorkspaceReady(
        timestamp: Long
    ) extends Event

    // === Error events ===

    case class Error(
        message: String,
        details: Option[String],
        timestamp: Long
    ) extends Event

    // ==========================================================================
    // Codecs
    // ==========================================================================

    implicit val compileStartedCodec: Codec[CompileStarted] = deriveCodec
    implicit val compilationReasonCodec: Codec[CompilationReason] = deriveCodec
    implicit val compileProgressCodec: Codec[CompileProgress] = deriveCodec
    implicit val compilePhaseChangedCodec: Codec[CompilePhaseChanged] = deriveCodec
    implicit val compileFinishedCodec: Codec[CompileFinished] = deriveCodec
    implicit val compileStalledCodec: Codec[CompileStalled] = deriveCodec
    implicit val compileResumedCodec: Codec[CompileResumed] = deriveCodec
    implicit val linkStartedCodec: Codec[LinkStarted] = deriveCodec
    implicit val linkProgressCodec: Codec[LinkProgress] = deriveCodec
    implicit val linkFinishedCodec: Codec[LinkFinished] = deriveCodec
    implicit val sourcegenStartedCodec: Codec[SourcegenStarted] = deriveCodec
    implicit val sourcegenFinishedCodec: Codec[SourcegenFinished] = deriveCodec
    implicit val discoveryStartedCodec: Codec[DiscoveryStarted] = deriveCodec
    implicit val suitesDiscoveredCodec: Codec[SuitesDiscovered] = deriveCodec
    implicit val suiteStartedCodec: Codec[SuiteStarted] = deriveCodec
    implicit val testStartedCodec: Codec[TestStarted] = deriveCodec
    implicit val testFinishedCodec: Codec[TestFinished] = deriveCodec
    implicit val suiteFinishedCodec: Codec[SuiteFinished] = deriveCodec
    implicit val suiteTimedOutCodec: Codec[SuiteTimedOut] = deriveCodec
    implicit val suiteErrorCodec: Codec[SuiteError] = deriveCodec
    implicit val suiteCancelledCodec: Codec[SuiteCancelled] = deriveCodec
    implicit val runStartedCodec: Codec[RunStarted] = deriveCodec
    implicit val runOutputCodec: Codec[RunOutput] = deriveCodec
    implicit val runFinishedCodec: Codec[RunFinished] = deriveCodec
    implicit val outputCodec: Codec[Output] = deriveCodec
    implicit val logMessageCodec: Codec[LogMessage] = deriveCodec
    implicit val projectInfoCodec: Codec[ProjectInfo] = deriveCodec
    implicit val buildInitializedCodec: Codec[BuildInitialized] = deriveCodec
    implicit val projectStateChangedCodec: Codec[ProjectStateChanged] = deriveCodec
    implicit val projectSkippedCodec: Codec[ProjectSkipped] = deriveCodec
    implicit val buildFinishedCodec: Codec[BuildFinished] = deriveCodec
    implicit val testRunFinishedCodec: Codec[TestRunFinished] = deriveCodec
    implicit val workspaceBusyCodec: Codec[WorkspaceBusy] = deriveCodec
    implicit val workspaceReadyCodec: Codec[WorkspaceReady] = deriveCodec
    implicit val errorCodec: Codec[Error] = deriveCodec

    implicit val encoder: Encoder[Event] = Encoder.instance {
      case e: CompileStarted      => Json.obj("type" -> "CompileStarted".asJson, "data" -> e.asJson)
      case e: CompilationReason   => Json.obj("type" -> "CompilationReason".asJson, "data" -> e.asJson)
      case e: CompileProgress     => Json.obj("type" -> "CompileProgress".asJson, "data" -> e.asJson)
      case e: CompilePhaseChanged => Json.obj("type" -> "CompilePhaseChanged".asJson, "data" -> e.asJson)
      case e: CompileFinished     => Json.obj("type" -> "CompileFinished".asJson, "data" -> e.asJson)
      case e: CompileStalled      => Json.obj("type" -> "CompileStalled".asJson, "data" -> e.asJson)
      case e: CompileResumed      => Json.obj("type" -> "CompileResumed".asJson, "data" -> e.asJson)
      case e: LinkStarted         => Json.obj("type" -> "LinkStarted".asJson, "data" -> e.asJson)
      case e: LinkProgress        => Json.obj("type" -> "LinkProgress".asJson, "data" -> e.asJson)
      case e: LinkFinished        => Json.obj("type" -> "LinkFinished".asJson, "data" -> e.asJson)
      case e: SourcegenStarted    => Json.obj("type" -> "SourcegenStarted".asJson, "data" -> e.asJson)
      case e: SourcegenFinished   => Json.obj("type" -> "SourcegenFinished".asJson, "data" -> e.asJson)
      case e: DiscoveryStarted    => Json.obj("type" -> "DiscoveryStarted".asJson, "data" -> e.asJson)
      case e: SuitesDiscovered    => Json.obj("type" -> "SuitesDiscovered".asJson, "data" -> e.asJson)
      case e: SuiteStarted        => Json.obj("type" -> "SuiteStarted".asJson, "data" -> e.asJson)
      case e: TestStarted         => Json.obj("type" -> "TestStarted".asJson, "data" -> e.asJson)
      case e: TestFinished        => Json.obj("type" -> "TestFinished".asJson, "data" -> e.asJson)
      case e: SuiteFinished       => Json.obj("type" -> "SuiteFinished".asJson, "data" -> e.asJson)
      case e: SuiteTimedOut       => Json.obj("type" -> "SuiteTimedOut".asJson, "data" -> e.asJson)
      case e: SuiteError          => Json.obj("type" -> "SuiteError".asJson, "data" -> e.asJson)
      case e: SuiteCancelled      => Json.obj("type" -> "SuiteCancelled".asJson, "data" -> e.asJson)
      case e: RunStarted          => Json.obj("type" -> "RunStarted".asJson, "data" -> e.asJson)
      case e: RunOutput           => Json.obj("type" -> "RunOutput".asJson, "data" -> e.asJson)
      case e: RunFinished         => Json.obj("type" -> "RunFinished".asJson, "data" -> e.asJson)
      case e: Output              => Json.obj("type" -> "Output".asJson, "data" -> e.asJson)
      case e: LogMessage          => Json.obj("type" -> "LogMessage".asJson, "data" -> e.asJson)
      case e: BuildInitialized    => Json.obj("type" -> "BuildInitialized".asJson, "data" -> e.asJson)
      case e: ProjectStateChanged => Json.obj("type" -> "ProjectStateChanged".asJson, "data" -> e.asJson)
      case e: ProjectSkipped      => Json.obj("type" -> "ProjectSkipped".asJson, "data" -> e.asJson)
      case e: BuildFinished       => Json.obj("type" -> "BuildFinished".asJson, "data" -> e.asJson)
      case e: TestRunFinished     => Json.obj("type" -> "TestRunFinished".asJson, "data" -> e.asJson)
      case e: WorkspaceBusy       => Json.obj("type" -> "WorkspaceBusy".asJson, "data" -> e.asJson)
      case e: WorkspaceReady      => Json.obj("type" -> "WorkspaceReady".asJson, "data" -> e.asJson)
      case e: Error               => Json.obj("type" -> "Error".asJson, "data" -> e.asJson)
    }

    implicit val decoder: Decoder[Event] = Decoder.instance { cursor =>
      cursor.downField("type").as[String].flatMap {
        case "CompileStarted"      => cursor.downField("data").as[CompileStarted]
        case "CompilationReason"   => cursor.downField("data").as[CompilationReason]
        case "CompileProgress"     => cursor.downField("data").as[CompileProgress]
        case "CompilePhaseChanged" => cursor.downField("data").as[CompilePhaseChanged]
        case "CompileFinished"     => cursor.downField("data").as[CompileFinished]
        case "CompileStalled"      => cursor.downField("data").as[CompileStalled]
        case "CompileResumed"      => cursor.downField("data").as[CompileResumed]
        case "LinkStarted"         => cursor.downField("data").as[LinkStarted]
        case "LinkProgress"        => cursor.downField("data").as[LinkProgress]
        case "LinkFinished"        => cursor.downField("data").as[LinkFinished]
        case "SourcegenStarted"    => cursor.downField("data").as[SourcegenStarted]
        case "SourcegenFinished"   => cursor.downField("data").as[SourcegenFinished]
        case "DiscoveryStarted"    => cursor.downField("data").as[DiscoveryStarted]
        case "SuitesDiscovered"    => cursor.downField("data").as[SuitesDiscovered]
        case "SuiteStarted"        => cursor.downField("data").as[SuiteStarted]
        case "TestStarted"         => cursor.downField("data").as[TestStarted]
        case "TestFinished"        => cursor.downField("data").as[TestFinished]
        case "SuiteFinished"       => cursor.downField("data").as[SuiteFinished]
        case "SuiteTimedOut"       => cursor.downField("data").as[SuiteTimedOut]
        case "SuiteError"          => cursor.downField("data").as[SuiteError]
        case "SuiteCancelled"      => cursor.downField("data").as[SuiteCancelled]
        case "RunStarted"          => cursor.downField("data").as[RunStarted]
        case "RunOutput"           => cursor.downField("data").as[RunOutput]
        case "RunFinished"         => cursor.downField("data").as[RunFinished]
        case "Output"              => cursor.downField("data").as[Output]
        case "LogMessage"          => cursor.downField("data").as[LogMessage]
        case "BuildInitialized"    => cursor.downField("data").as[BuildInitialized]
        case "ProjectStateChanged" => cursor.downField("data").as[ProjectStateChanged]
        case "ProjectSkipped"      => cursor.downField("data").as[ProjectSkipped]
        case "BuildFinished"       => cursor.downField("data").as[BuildFinished]
        case "TestRunFinished"     => cursor.downField("data").as[TestRunFinished]
        case "WorkspaceBusy"       => cursor.downField("data").as[WorkspaceBusy]
        case "WorkspaceReady"      => cursor.downField("data").as[WorkspaceReady]
        case "Error"               => cursor.downField("data").as[Error]
        case other                 => Left(DecodingFailure(s"Unknown event type: $other", cursor.history))
      }
    }
  }

  /** Encode event to JSON string for BSP data field */
  def encode(event: Event): String =
    event.asJson.noSpaces

  /** Decode event from JSON string in BSP data field */
  def decode(json: String): Either[io.circe.Error, Event] =
    io.circe.parser.decode[Event](json)
}
