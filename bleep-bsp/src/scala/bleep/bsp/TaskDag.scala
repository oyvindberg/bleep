package bleep.bsp

import bleep.machine.{Demand, ForkDemand, ForkGrant, ForkId, ForkKey, ForkKind, InHeap, InHeapKind, RequestId}
import bleep.bsp.protocol.KillReason
import bleep.bsp.protocol.{BleepBspProtocol, LinkPlatformName, OutputChannel, ProcessExit, SuiteOutcome, TestStatus}
import bleep.bsp.protocol.BleepBspProtocol.BuildMode
import bleep.model.{CrossProjectName, KotlinJsModuleKind, KotlinJsSourceMapEmbedSources, ScriptDef, SuiteName, TestName}
import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._

import scala.collection.mutable

/** Task DAG for unified compile + test execution.
  *
  * Tasks are nodes with dependencies. A task can only execute after all its dependencies have completed successfully.
  *
  * Task types:
  *   - CompileTask: Compile a project using Zinc
  *   - DiscoverTask: Scan compiled classes for test suites
  *   - TestSuiteTask: Execute a test suite in forked JVM
  *
  * Dependencies:
  *   - DiscoverTask depends on CompileTask for same project
  *   - TestSuiteTask depends on DiscoverTask for same project
  *   - CompileTask may depend on other CompileTasks (project deps)
  *
  * Kill signals: The executor accepts a Deferred[IO, KillReason] which can be completed to request termination. Running tasks will receive this signal and
  * report Killed outcomes.
  */
object TaskDag {

  /** Typed task identifier — prevents confusion between task IDs and arbitrary strings. */
  sealed trait TaskId {
    def value: String
    override def toString: String = value
  }
  object TaskId {

    /** The project is built: its classes are what consumers read. For a project with a `postCompile` step that is the step's output, so this names the
      * [[PostCompileTask]], and the compile before it is [[CompilerOutput]].
      */
    case class Compile(project: CrossProjectName) extends TaskId {
      val value: String = s"compile:${project.value}"
    }

    /** The compile of a project with a `postCompile` step: what the compiler wrote, before the step rewrites it into the project's classes. */
    case class CompilerOutput(project: CrossProjectName) extends TaskId {
      val value: String = s"compiler-output:${project.value}"
    }
    case class Link(project: CrossProjectName) extends TaskId {
      val value: String = s"link:${project.value}"
    }
    case class Discover(project: CrossProjectName) extends TaskId {
      val value: String = s"discover:${project.value}"
    }
    case class Test(project: CrossProjectName, suiteName: SuiteName) extends TaskId {
      val value: String = s"test:${project.value}:${suiteName.value}"
    }

    /** A whole project's JUnit suites run as one batched execution — one task, not one per suite. */
    case class TestBatch(project: CrossProjectName) extends TaskId {
      val value: String = s"test-batch:${project.value}"
    }

    /** Identity for a sourcegen script in the DAG.
      *
      * Two `ScriptDef.Main` values collapse to the same task iff they share the same script project + main class. A single `SourcegenTask` runs the script once
      * for all target projects that declared it, regardless of how many projects list it.
      */
    case class Sourcegen(scriptProject: CrossProjectName, mainClass: String) extends TaskId {
      val value: String = s"sourcegen:${scriptProject.value}/$mainClass"
    }
    object Sourcegen {
      def apply(script: ScriptDef.Main): Sourcegen = Sourcegen(script.project, script.main)
    }

    /** Per-project annotation-processor resolution: fetch processor JARs (Coursier), scan resolved-`dependencies` JARs for `META-INF/services`, assemble the
      * javac flags. No useful cross-project dedup — each project has its own dep set and explicit list.
      */
    case class ResolveAnnotationProcessors(project: CrossProjectName) extends TaskId {
      val value: String = s"resolve-ap:${project.value}"
    }

    /** Per-project KSP run: resolve the KSP standalone-runner classpath and the user-listed processor JARs, then fork a JVM that runs
      * `com.google.devtools.ksp.cmdline.KSPJvmMain` against the project's sources. Generated `.kt`/`.java`/resources land under
      * `.bleep/projects/<cross>/generated-sources/ksp/`; KSP-emitted `.class`es and caches live per-variant under
      * `.bleep/projects/<cross>/builds/<variant>/ksp/`. The generated sources are picked up by the project's source set for the subsequent kotlinc compile.
      */
    case class RunSymbolProcessors(project: CrossProjectName) extends TaskId {
      val value: String = s"run-ksp:${project.value}"
    }
  }

  /** A task in the DAG */
  sealed trait Task {
    def id: TaskId
    def project: CrossProjectName
    def dependencies: Set[TaskId]

    /** Ordering-only predecessors: scheduling waits for these to finish (in any state) without propagating their failure. See TestSuiteTask. */
    def runAfter: Set[TaskId] = Set.empty
  }

  /** What a linked artifact asked to list its own suites is charged until the scheduler has measured it, a second after it starts: node loading a Kotlin/JS
    * bundle, or a Kotlin/Native test binary. Neither is a JVM, so no `-Xmx` sizes it. PROVISIONAL — a round number above what either has been seen to need; the
    * measurement replaces it almost at once, so what it costs is one second of over-charge, not a wrong steady state.
    */
  val ListingProcessBoundMb: Long = 256L

  /** What `task` asks the machine scheduler for, given what this machine gives each kind of fork — or nothing, for a test task.
    *
    * The rule (owner's): work in the server's own process counts like a compile — a cpu slot and, when it allocates heavily, the heap gate's consent; never
    * machine room, never the lock. Real processes are forks that report their pid and are measured. So:
    *   - compiles, annotation-processor resolution, and discovery by reflection are in-heap;
    *   - the Scala.js and Kotlin/JS linkers run in this JVM and are in-heap `Link`s;
    *   - Kotlin/Native links fork `konanc`, and Scala Native's linker spawns clang — both fork demands;
    *   - Kotlin/JS and Kotlin/Native discovery run the linked artifact (node, the binary) — fork demands;
    *   - sourcegen, KSP and post-compile each fork a JVM, charged the heap they are started with plus the non-heap a JVM also commits.
    * Test suites and batches ask for their fork themselves, from inside their handler, once the classpath — and so the fork's key — is known (design §11,
    * two-stage test admission); the executor starts them as soon as they are ready and submits nothing for them.
    *
    * A function rather than a field on the task: cost is a property of (what kind of work this is, how this machine is configured), not of the task's identity.
    */
  def demandFor(task: Task, forkHeaps: ForkHeaps, request: RequestId): Option[Demand] = {
    val id = bleep.machine.TaskId(task.id.value)
    def inHeap(kind: InHeapKind) = Some(InHeap(request, id, kind, cpu = 1))
    def fork(kind: ForkKind, boundMb: Long) = Some(ForkDemand(request, id, kind, ForkKey(task.id.value), boundMb, cpu = 1, shared = false))
    task match {
      case _: CompileTask                     => inHeap(InHeapKind.Compile)
      case _: ResolveAnnotationProcessorsTask => inHeap(InHeapKind.ResolveAnnotationProcessors)
      case dt: DiscoverTask                   =>
        dt.platform match {
          case Some(_: LinkPlatform.KotlinJs) | Some(_: LinkPlatform.KotlinNative) => fork(ForkKind.Discover, ListingProcessBoundMb)
          case Some(_: LinkPlatform.ScalaJs) | Some(_: LinkPlatform.ScalaNative) | Some(LinkPlatform.Jvm) | None => inHeap(InHeapKind.Discover)
        }
      case lt: LinkTask =>
        lt.platform match {
          case _: LinkPlatform.ScalaJs | _: LinkPlatform.KotlinJs | LinkPlatform.Jvm => inHeap(InHeapKind.Link)
          case _: LinkPlatform.ScalaNative | _: LinkPlatform.KotlinNative            => fork(ForkKind.Link, forkHeaps.linkMb)
        }
      case _: PostCompileTask                  => fork(ForkKind.PostCompile, forkHeaps.sourcegenMb)
      case _: SourcegenTask                    => fork(ForkKind.Sourcegen, forkHeaps.sourcegenMb)
      case _: RunSymbolProcessorsTask          => fork(ForkKind.Ksp, forkHeaps.kspMb)
      case _: TestSuiteTask | _: TestBatchTask => None
    }
  }

  /** The project a compile's scheduler task id names, for the heap-wait events; `None` for any other task. */
  def compileProjectOf(taskId: bleep.machine.TaskId): Option[String] =
    if (taskId.value.startsWith("compile:")) Some(taskId.value.stripPrefix("compile:")) else None

  /** The project an in-heap link's scheduler task id names — the heap gate holds those back too; `None` for any other task. */
  def linkProjectOf(taskId: bleep.machine.TaskId): Option[String] =
    if (taskId.value.startsWith("link:")) Some(taskId.value.stripPrefix("link:")) else None

  /** Compile a project.
    *
    * `dependencies` is supplied directly (rather than derived) so the DAG builder can combine project-level compile deps with sourcegen deps (SourcegenTask
    * edges) and any future pre-compile steps. `projectDependencies` remains separate because the compile handler needs the project-level form (to compute
    * dependency analysis file paths for Zinc).
    */
  case class CompileTask(
      project: CrossProjectName,
      projectDependencies: Set[CrossProjectName],
      dependencies: Set[TaskId],
      /** The project declares a `postCompile`, so this compile is followed by a [[PostCompileTask]], which consumers wait for instead. */
      postCompile: Boolean
  ) extends Task {
    val id: TaskId = if (postCompile) TaskId.CompilerOutput(project) else TaskId.Compile(project)
  }

  /** Run a project's `postCompile` script on what its compile wrote — see [[bleep.bsp.PostCompileRunner]].
    *
    * A task of its own so that what the script reads (`reads`: its script project and inputs) holds back only the script, not the compile before it. Takes the
    * project's [[TaskId.Compile]], so everything downstream waits for the script's output rather than the compiler's.
    */
  case class PostCompileTask(
      project: CrossProjectName,
      reads: Set[CrossProjectName]
  ) extends Task {
    val id: TaskId = TaskId.Compile(project)
    val dependencies: Set[TaskId] = reads.map(p => TaskId.Compile(p): TaskId) + TaskId.CompilerOutput(project)
  }

  /** Run a sourcegen script for a set of target projects.
    *
    * One `SourcegenTask` per unique script (identified by `TaskId.Sourcegen(scriptProject, main)`), shared across all target projects that declared it.
    *
    * Dependencies:
    *   - `CompileTask(scriptProject)` and compile tasks for its transitive deps — the script project must be built before it can be forked.
    *
    * Target projects' `CompileTask`s depend on this sourcegen task, so compilation of targets is blocked until sourcegen finishes (or short-circuits on
    * up-to-date outputs).
    */
  case class SourcegenTask(
      scriptProject: CrossProjectName,
      main: String,
      /** Each distinct declaration of the script, with the projects that declared it that way. Consumers of one script may declare it differently
        * (`sourceGlobs`, `inputs`); the task is still one, and runs the script once per declaration, each for its own consumers.
        */
      declarations: Map[ScriptDef.Main, Set[CrossProjectName]],
      scriptProjectDeps: Set[CrossProjectName]
  ) extends Task {
    val id: TaskId = TaskId.Sourcegen(scriptProject, main)
    val project: CrossProjectName = scriptProject
    val dependencies: Set[TaskId] = scriptProjectDeps.map(p => TaskId.Compile(p): TaskId)
    def forProjects: Set[CrossProjectName] = declarations.values.flatten.toSet
  }

  /** Resolve annotation processors for a project: fetch processor JARs from Coursier, scan resolved-`dependencies` JARs for `META-INF/services`, assemble the
    * javac flags. Has no DAG dependencies — runs as soon as the executor starts. The project's `CompileTask` depends on this when the project has any
    * annotation-processor configuration.
    */
  case class ResolveAnnotationProcessorsTask(
      project: CrossProjectName
  ) extends Task {
    val id: TaskId = TaskId.ResolveAnnotationProcessors(project)
    val dependencies: Set[TaskId] = Set.empty
    // Coursier resolution and jar scanning, in the server. I/O-bound rather than CPU-bound, but it
  }

  /** Run KSP for a project. Resolves the KSP standalone-runner classpath + user processors, then forks a JVM running `KSPJvmMain` which processes the project's
    * Kotlin/Java sources and emits generated code.
    *
    * Depends on the `CompileTask` of every transitive upstream project: KSP's `-libraries` argument needs those projects' compiled class dirs so type
    * references across projects resolve.
    *
    * The project's own `CompileTask` depends on this task (wired in `compileDeps`), so the generated sources are on disk before kotlinc runs.
    */
  case class RunSymbolProcessorsTask(
      project: CrossProjectName,
      upstreamCompileDeps: Set[CrossProjectName]
  ) extends Task {
    val id: TaskId = TaskId.RunSymbolProcessors(project)
    val dependencies: Set[TaskId] = upstreamCompileDeps.map(p => TaskId.Compile(p): TaskId)
  }

  /** Link a non-JVM project (Scala.js, Scala Native, Kotlin/JS, Kotlin/Native).
    *
    * Links compiled output to runnable/testable form. For JVM projects, this task is skipped (LinkPlatform.Jvm).
    */
  case class LinkTask(
      project: CrossProjectName,
      platform: LinkPlatform,
      releaseMode: Boolean,
      isTest: Boolean
  ) extends Task {
    val id: TaskId = TaskId.Link(project)
    val dependencies: Set[TaskId] = Set(TaskId.Compile(project))
  }

  /** Platform for linking non-JVM targets. */
  sealed trait LinkPlatform {

    /** Which platform this links for. Carry this rather than re-deriving it by matching on the case class at each use site — and never render it to a string to
      * compare it back.
      */
    def name: LinkPlatformName

    def isJs: Boolean = name == LinkPlatformName.ScalaJs || name == LinkPlatformName.KotlinJs
    def isNative: Boolean = name == LinkPlatformName.ScalaNative || name == LinkPlatformName.KotlinNative
  }
  object LinkPlatform {
    case class ScalaJs(
        version: String,
        scalaVersion: String,
        config: bleep.analysis.ScalaJsLinkConfig
    ) extends LinkPlatform { val name: LinkPlatformName = LinkPlatformName.ScalaJs }

    case class ScalaNative(
        version: String,
        scalaVersion: String,
        config: bleep.analysis.ScalaNativeLinkConfig
    ) extends LinkPlatform { val name: LinkPlatformName = LinkPlatformName.ScalaNative }

    case class KotlinJs(
        version: String,
        config: KotlinJsConfig
    ) extends LinkPlatform { val name: LinkPlatformName = LinkPlatformName.KotlinJs }

    case class KotlinNative(
        version: String,
        config: KotlinNativeConfig
    ) extends LinkPlatform { val name: LinkPlatformName = LinkPlatformName.KotlinNative }

    /** JVM platform - linking is a no-op */
    case object Jvm extends LinkPlatform { val name: LinkPlatformName = LinkPlatformName.Jvm }
  }

  /** Kotlin/JS configuration.
    *
    * No `outputDir`: the field that used to be here had no reader. `LinkExecutor.execute` computes the output directory for every platform the same way, from
    * the base directory it is handed plus the mode suffix, and Kotlin/JS was no exception — but the two callers filled this field with two *different*
    * directories, which made the compile path and the test path look like they disagreed about where a link lands when neither was being consulted.
    */
  case class KotlinJsConfig(
      moduleKind: KotlinJsModuleKind,
      moduleName: Option[String],
      sourceMap: Boolean,
      sourceMapPrefix: Option[String],
      sourceMapEmbedSources: KotlinJsSourceMapEmbedSources,
      generateDts: Boolean,
      dce: Boolean // Dead Code Elimination - true = smaller output
  )

  /** Kotlin/Native configuration */
  case class KotlinNativeConfig(
      target: String,
      debugInfo: Boolean,
      optimizations: Boolean,
      isTest: Boolean
  )

  /** Discover test suites in a compiled project.
    *
    * For non-JVM platforms, depends on LinkTask instead of CompileTask.
    */
  case class DiscoverTask(
      project: CrossProjectName,
      platform: Option[LinkPlatform]
  ) extends Task {
    val id: TaskId = TaskId.Discover(project)
    val dependencies: Set[TaskId] = platform match {
      case Some(LinkPlatform.Jvm) | None => Set(TaskId.Compile(project))
      case Some(_)                       => Set(TaskId.Link(project))
    }
  }

  /** What a [[DiscoverTask]] found.
    *
    * `suites` is what survived `--only` / `--exclude` / tag filters and is what the run goes on to execute. `discoveredBeforeFilters` is what the classpath
    * scan produced, and is kept separately because the two answer different questions: an empty `suites` may be the user narrowing the run, while an empty scan
    * is a test project whose compiled classes no framework claimed. Only the second is a broken build — see `BuildSummary.toEither`.
    */
  case class DiscoveryResult(
      suites: List[(String, bleep.testing.FrameworkSelection)],
      discoveredBeforeFilters: Int,
      /** The project's `maxConcurrentSuites`: how many of its suites may run in parallel forks. None = unbounded (the default). 1 = all suites run sequentially
        * through one warm fork, maven-style.
        */
      suiteParallelism: Option[Int],
      /** Per-project batches, one per framework: the suites to run through a single execution (JUnit Platform: one `launcher.execute()`; sbt test-interface:
        * one `Framework`/`Runner`, one `done()` — maven's one-execute-per-module), paired with the degree of parallelism bleep chose. Decided by the discover
        * handler from `testFork == per-project`, grouping JVM suites by framework. Empty = run suite-by-suite (per-suite mode, and any PlatformRunner suites).
        */
      batches: List[(List[(String, bleep.testing.FrameworkSelection)], Int)]
  )

  /** Execute a test suite */
  case class TestSuiteTask(
      project: CrossProjectName,
      suiteName: SuiteName,
      selection: bleep.testing.FrameworkSelection,
      /** Ordering-only predecessors: this suite waits for them to reach a terminal state but does NOT inherit their failure — a red suite must not skip the
        * rest of its project's chain, just as maven's surefire keeps going after a failing class. Used by `maxConcurrentSuites` to serialize a project's suites
        * through one warm fork.
        */
      override val runAfter: Set[TaskId]
  ) extends Task {
    val id: TaskId = TaskId.Test(project, suiteName)
    val dependencies: Set[TaskId] = Set(TaskId.Discover(project))
    // A core, but no fork memory declared here: acquiring a JVM from the pool may reuse a warm one
    // (free) or spawn a new one (measured RSS), and only the pool knows which. It holds that
    // reservation itself, from spawn until the process is destroyed — a lifetime this task does not
  }

  /** Run ALL of a project's JUnit suites as one batched execution in a single fork — maven surefire's one-execute-per-module, which is what keeps an
    * execution-scoped fixture (an application the framework boots for the run) built once and reused across the classes rather than rebuilt per class. junit's
    * engine runs `parallelism` classes at once inside the fork, a number bleep chose. One task, not one per suite: the resource cost is one fork doing that
    * much work.
    */
  case class TestBatchTask(
      project: CrossProjectName,
      suites: List[(SuiteName, bleep.testing.FrameworkSelection)],
      parallelism: Int
  ) extends Task {
    val id: TaskId = TaskId.TestBatch(project)
    val dependencies: Set[TaskId] = Set(TaskId.Discover(project))
  }

  /** Result of task execution.
    *
    * Semantics:
    *   - Success: Task completed successfully
    *   - Failure: Logical failure (test assertion failed, compilation error)
    *   - Error: Infrastructure failure (process crash, OOM, signal kill)
    *   - Skipped: Dependency failed (propagates failure)
    *   - Killed: User-initiated cancellation (Ctrl-C, $/cancelRequest)
    *   - TimedOut: Suite exceeded time limit (does NOT propagate - downstream tasks still run)
    */
  sealed trait TaskResult
  object TaskResult {
    case object Success extends TaskResult
    case class Failure(error: String, diagnostics: List[BleepBspProtocol.Diagnostic]) extends TaskResult
    case class Error(error: String, processExit: ProcessExit) extends TaskResult
    case class Skipped(failedDependency: Task) extends TaskResult
    case class Killed(reason: KillReason) extends TaskResult
    case class TimedOut(threadDump: Option[String]) extends TaskResult

    /** Backward compatibility alias - prefer Killed with explicit reason */
    val Cancelled: TaskResult = Killed(KillReason.UserRequest)
  }

  /** Result of linking operation */
  sealed trait LinkResult {

    /** Output directory (with config-aware suffix) for logging and downstream use */
    def outputDir: Option[java.nio.file.Path]
  }
  object LinkResult {

    /** Successful JS linking */
    case class JsSuccess(
        mainModule: java.nio.file.Path,
        sourceMap: Option[java.nio.file.Path],
        allFiles: Seq[java.nio.file.Path],
        wasUpToDate: Boolean
    ) extends LinkResult {
      def outputDir: Option[java.nio.file.Path] = Some(mainModule.getParent)
    }

    /** Successful native linking */
    case class NativeSuccess(
        binary: java.nio.file.Path,
        wasUpToDate: Boolean
    ) extends LinkResult {
      def outputDir: Option[java.nio.file.Path] = Some(binary.getParent)
    }

    /** Linking failed */
    case class Failure(
        error: String,
        diagnostics: List[String]
    ) extends LinkResult {
      def outputDir: Option[java.nio.file.Path] = None
    }

    /** Linking was killed */
    case class Killed(reason: KillReason) extends LinkResult {
      def outputDir: Option[java.nio.file.Path] = None
    }

    /** Linking not applicable (JVM platform) */
    case object NotApplicable extends LinkResult {
      def outputDir: Option[java.nio.file.Path] = None
    }

    /** Backward compatibility alias - prefer Killed with explicit reason */
    val Cancelled: LinkResult = Killed(KillReason.UserRequest)
  }

  /** Events emitted during DAG execution */
  sealed trait DagEvent
  object DagEvent {
    case class TaskStarted(task: Task, timestamp: Long) extends DagEvent
    case class TaskProgress(task: Task, percent: Int, timestamp: Long) extends DagEvent
    case class TaskFinished(task: Task, result: TaskResult, durationMs: Long, timestamp: Long) extends DagEvent

    // Link-specific events
    case class LinkStarted(project: CrossProjectName, platform: LinkPlatformName, timestamp: Long) extends DagEvent
    case class LinkProgress(project: CrossProjectName, phase: String, percent: Int, timestamp: Long) extends DagEvent
    case class LinkFinished(
        project: CrossProjectName,
        result: LinkResult,
        durationMs: Long,
        timestamp: Long,
        /** The platform actually linked, carried rather than inferred.
          *
          * [[LinkResult]] only distinguishes JS from Native, so reconstructing the name downstream reported every Kotlin/JS link as "Scala.js", every
          * Kotlin/Native link as "Scala Native", and every *failure* as "JVM" — `❌ link failed [JVM]` for a Scala.js project. The task knows which platform it
          * ran; [[LinkStarted]] already carries it, and now so does this.
          */
        platform: bleep.bsp.protocol.LinkPlatformName
    ) extends DagEvent

    // Test-specific events (nested within TestSuiteTask execution)
    case class TestStarted(project: CrossProjectName, suite: SuiteName, test: TestName, timestamp: Long) extends DagEvent
    case class TestFinished(
        project: CrossProjectName,
        suite: SuiteName,
        test: TestName,
        status: TestStatus,
        durationMs: Long,
        message: Option[String],
        throwable: Option[String],
        timestamp: Long,
        location: Option[bleep.bsp.protocol.BleepBspProtocol.SourceLocation]
    ) extends DagEvent

    // Discovery events
    case class SuitesDiscovered(
        project: CrossProjectName,
        suites: List[SuiteName],
        discoveredBeforeFilters: Int,
        timestamp: Long
    ) extends DagEvent

    // Output events
    case class Output(project: CrossProjectName, suite: SuiteName, line: String, channel: OutputChannel, timestamp: Long) extends DagEvent

    // Suite completion — outcome distinguishes executed-with-counts from empty/no-framework/errored
    case class SuiteFinished(
        project: CrossProjectName,
        suite: SuiteName,
        outcome: SuiteOutcome,
        durationMs: Long,
        timestamp: Long
    ) extends DagEvent

    // A suite the idle-timeout watchdog stopped before it reported a result. A per-project batch kills the whole fork on timeout, so its own TimedOut result
    // names no suite; each suite that had not reported is emitted here so the run counts it as a timeout (and the verdict says "timed out") instead of the
    // anonymous "N suites never reported a result". A TestSuiteTask reports its own timeout from the task result (see abnormalTaskEvent), so this is the
    // batch-only equivalent.
    case class SuiteTimedOut(
        project: CrossProjectName,
        suite: SuiteName,
        timeoutMs: Long,
        threadDump: Option[String],
        timestamp: Long
    ) extends DagEvent

    // Sourcegen events (mirror Link events: Started around handler, Finished with result)
    case class SourcegenStarted(
        scriptProject: CrossProjectName,
        scriptMain: String,
        forProjects: List[CrossProjectName],
        timestamp: Long
    ) extends DagEvent

    case class SourcegenFinished(
        scriptProject: CrossProjectName,
        scriptMain: String,
        success: Boolean,
        durationMs: Long,
        error: Option[String],
        timestamp: Long
    ) extends DagEvent

    // Annotation-processor resolution events (mirror Sourcegen events).
    case class ResolveAnnotationProcessorsStarted(
        project: CrossProjectName,
        timestamp: Long
    ) extends DagEvent

    case class ResolveAnnotationProcessorsFinished(
        project: CrossProjectName,
        success: Boolean,
        durationMs: Long,
        error: Option[String],
        discoveredJarCount: Int,
        timestamp: Long
    ) extends DagEvent

    // KSP run events (mirror AP events; the task itself runs the KSP standalone runner end-to-end).
    case class RunSymbolProcessorsStarted(
        project: CrossProjectName,
        timestamp: Long
    ) extends DagEvent

    case class RunSymbolProcessorsFinished(
        project: CrossProjectName,
        success: Boolean,
        durationMs: Long,
        error: Option[String],
        discoveredJarCount: Int,
        timestamp: Long
    ) extends DagEvent
  }

  /** The DAG itself - holds tasks and tracks execution state */
  case class Dag(
      tasks: Map[TaskId, Task],
      completed: Set[TaskId],
      failed: Set[TaskId],
      errored: Set[TaskId],
      skipped: Set[TaskId],
      killed: Set[TaskId],
      timedOut: Set[TaskId],
      linkResults: Map[TaskId, LinkResult],
      /** What each project's link actually produced, keyed so the tasks that run *after* it can ask.
        *
        * The linker is the only thing that knows where its output landed; every consumer used to rebuild that path from convention instead —
        * `targetDir / linkDirSuffix(...) / "js" / s"$moduleName.js"` in one place, a list of four candidate paths tried in order in another. Two derivations of
        * one fact, in code that has to agree with a third (the linker's own), and they did not: `bleep link` writes under `link-output/` while the test path
        * looks under `builds/<suffix>/`, so a linked artifact was invisible to the run that needed it.
        */
      linkOutputs: Map[CrossProjectName, LinkResult]
  ) {

    /** What this project's link produced, if it has been linked in this run. */
    def linkOutputFor(project: CrossProjectName): Option[LinkResult] = linkOutputs.get(project)

    /** All finished tasks (any terminal state) */
    def finished: Set[TaskId] = completed ++ failed ++ errored ++ skipped ++ killed ++ timedOut

    /** States that propagate failure to downstream tasks. Note: timedOut does NOT propagate - downstream tasks still run. */
    private def propagatesFailure(taskId: TaskId): Boolean =
      failed.contains(taskId) || errored.contains(taskId) || skipped.contains(taskId) || killed.contains(taskId)

    /** Get tasks that are ready to execute (all dependencies satisfied) */
    def ready: Set[Task] = {
      val notStarted = tasks.keySet -- finished
      notStarted.flatMap { taskId =>
        val task = tasks(taskId)
        // A task is complete if it's in any terminal state (including timedOut)
        val depsComplete = task.dependencies.forall(d => finished.contains(d))
        // runAfter is ordering only: wait for a terminal state, ignore what that state was
        val orderingComplete = task.runAfter.forall(d => finished.contains(d))
        val depsFailed = task.dependencies.exists(propagatesFailure)

        if (depsFailed) None // Will be skipped
        else if (depsComplete && orderingComplete) Some(task)
        else None
      }
    }

    /** Get tasks that should be skipped, with the failed dependency task that caused it */
    def toSkip: Map[Task, Task] = {
      val notStarted = tasks.keySet -- finished
      notStarted.flatMap { taskId =>
        val task = tasks(taskId)
        val failedDep = task.dependencies.find(propagatesFailure)
        failedDep.flatMap(depId => tasks.get(depId).map(dep => task -> dep))
      }.toMap
    }

    /** Mark a task as completed */
    def complete(taskId: TaskId): Dag =
      copy(completed = completed + taskId)

    /** Mark a task as failed (logical failure like test assertion) */
    def fail(taskId: TaskId): Dag =
      copy(failed = failed + taskId)

    /** Mark a task as errored (infrastructure failure like process crash) */
    def error(taskId: TaskId): Dag =
      copy(errored = errored + taskId)

    /** Mark a task as skipped */
    def skip(taskId: TaskId): Dag =
      copy(skipped = skipped + taskId)

    /** Mark a task as killed */
    def kill(taskId: TaskId): Dag =
      copy(killed = killed + taskId)

    /** Mark a task as timed out (does NOT propagate to downstream) */
    def timeout(taskId: TaskId): Dag =
      copy(timedOut = timedOut + taskId)

    /** Add a task to the DAG (used for dynamic task creation) */
    def addTask(task: Task): Dag =
      copy(tasks = tasks + (task.id -> task))

    /** Record a link result for a task */
    def recordLinkResult(taskId: TaskId, project: CrossProjectName, result: LinkResult): Dag =
      copy(linkResults = linkResults + (taskId -> result), linkOutputs = linkOutputs + (project -> result))

    /** Check if DAG execution is complete */
    def isComplete: Boolean = finished == tasks.keySet

    /** Get all tasks of a specific type */
    def tasksOfType[T <: Task](implicit ct: scala.reflect.ClassTag[T]): List[T] =
      tasks.values.collect { case t: T => t }.toList

    /** Get dependency count for each task (for topological order) */
    def inDegrees: Map[TaskId, Int] =
      tasks.map { case (id, task) =>
        id -> task.dependencies.count(tasks.contains)
      }

    /** Count of tasks (transitively) blocked by each task */
    def dependentsCount: Map[TaskId, Int] = {
      // Build reverse adjacency: taskId -> set of tasks that directly depend on it
      val reverseDeps = tasks.values.foldLeft(Map.empty[TaskId, Set[TaskId]]) { (acc, task) =>
        task.dependencies.foldLeft(acc) { (acc2, depId) =>
          acc2.updated(depId, acc2.getOrElse(depId, Set.empty) + task.id)
        }
      }
      // For each task, count transitive dependents (BFS)
      tasks.keys.map { taskId =>
        val visited = scala.collection.mutable.Set.empty[TaskId]
        val queue = scala.collection.mutable.Queue(taskId)
        while (queue.nonEmpty) {
          val current = queue.dequeue()
          reverseDeps.getOrElse(current, Set.empty).foreach { dep =>
            if (visited.add(dep)) queue.enqueue(dep)
          }
        }
        taskId -> visited.size
      }.toMap
    }
  }

  object Dag {
    def empty: Dag = Dag(Map.empty[TaskId, Task], Set.empty, Set.empty, Set.empty, Set.empty, Set.empty, Set.empty, Map.empty, Map.empty)

    /** Create DAG from a set of tasks */
    def fromTasks(tasks: Seq[Task]): Dag =
      Dag(tasks.map(t => t.id -> t).toMap, Set.empty, Set.empty, Set.empty, Set.empty, Set.empty, Set.empty, Map.empty, Map.empty)
  }

  /** Plan for sourcegen DAG integration.
    *
    *   - `perProject` — for each target project that declared sourcegen in its config, the set of scripts that must run before it compiles.
    *   - `scriptProjectDeps` — for each script, the script project and the projects it reads (`inputs`), with their transitive dependency projects. The DAG
    *     inserts `CompileTask`s for all of these, and the `SourcegenTask` depends on their compiles.
    *
    * When `perProject` is empty, the DAG falls back to the original compile-only topology.
    */
  case class SourcegenPlan(
      perProject: Map[CrossProjectName, Set[ScriptDef.Main]],
      scriptProjectDeps: Map[ScriptDef.Main, Set[CrossProjectName]]
  ) {
    def isEmpty: Boolean = perProject.isEmpty
    def allScripts: Set[ScriptDef.Main] = perProject.values.flatten.toSet
  }
  object SourcegenPlan {
    val empty: SourcegenPlan = SourcegenPlan(Map.empty, Map.empty)
  }

  /** Which projects need annotation-processor resolution as a DAG step. A project belongs in `projects` iff its `model.Java` declares any annotation-processor
    * configuration (scan opt-in or non-empty explicit list). Projects without AP configuration skip the DAG step entirely; their javac options simply get
    * `-proc:none` from `ResolveProjects` directly.
    */
  case class AnnotationProcessorPlan(projects: Set[CrossProjectName]) {
    def isEmpty: Boolean = projects.isEmpty
    def needsResolution(project: CrossProjectName): Boolean = projects.contains(project)
  }
  object AnnotationProcessorPlan {
    val empty: AnnotationProcessorPlan = AnnotationProcessorPlan(Set.empty)
  }

  /** Which projects need KSP processor resolution as a DAG step. A project belongs in `projects` iff its `model.Kotlin` declares any KSP configuration (scan
    * opt-in or non-empty explicit list). Projects without KSP configuration skip the DAG step entirely.
    */
  case class SymbolProcessorPlan(projects: Set[CrossProjectName]) {
    def isEmpty: Boolean = projects.isEmpty
    def needsResolution(project: CrossProjectName): Boolean = projects.contains(project)
  }
  object SymbolProcessorPlan {
    val empty: SymbolProcessorPlan = SymbolProcessorPlan(Set.empty)
  }

  /** Inputs to DAG construction. Bundles project-graph and per-task-type plans (sourcegen, AP, KSP) so that `buildDag` and friends take a single value instead
    * of a cascade of positional parameters that grows every time a new task type is added.
    *
    * Tests use [[BuildContext.empty]] and `.copy(...)` to populate just the fields they care about.
    */
  case class BuildContext(
      allProjectDeps: Map[CrossProjectName, Set[CrossProjectName]],
      platforms: Map[CrossProjectName, LinkPlatform],
      sourcegen: SourcegenPlan,
      apPlan: AnnotationProcessorPlan,
      kspPlan: SymbolProcessorPlan,
      /** Which of these projects declared `isTestProject: true`.
        *
        * Needed because linking a test project is a different operation from linking a library: Scala.js needs test module initializers, Scala Native and
        * Kotlin/Native need a generated test-runner entry point instead of a `main`. `bleep link` used to pass `isTest = false` unconditionally, so on a
        * test-only project it asked all four platforms to link a `main` that does not exist — Scala.js produced no output, Scala Native failed on "requires a
        * main class", and Kotlin/Native failed with "could not find '/main' function".
        */
      testProjects: Set[CrossProjectName],
      /** The projects that declare a `postCompile` script, each with what that script reads (its script project and inputs). `allProjectDeps` leaves these out:
        * they are built before the script runs, not before the compile.
        */
      postCompile: Map[CrossProjectName, Set[CrossProjectName]]
  ) {

    /** Everything that must be built for a project, its post-compile step included: what decides which projects a build covers. */
    lazy val buildOrderDeps: Map[CrossProjectName, Set[CrossProjectName]] =
      allProjectDeps.map { case (p, deps) => (p, deps ++ postCompile.getOrElse(p, Set.empty)) }
  }

  /** What each kind of forked JVM is charged: its heap plus the non-heap a JVM also commits (metaspace, code cache, stacks, GC structures). Resolved from
    * config once when the DAG is built, so the number a task declares is the number the fork is actually started with.
    */
  case class ForkHeaps(sourcegenMb: Long, kspMb: Long, linkMb: Long)
  object ForkHeaps {

    /** What bleep gives a fork that states no `-Xmx` of its own. */
    private val defaultFootprint: Long = bleep.MemorySizes.forkFootprintMb(bleep.MemorySizes.DefaultForkHeapMb)
    val default: ForkHeaps = ForkHeaps(sourcegenMb = defaultFootprint, kspMb = defaultFootprint, linkMb = defaultFootprint)
  }

  object BuildContext {
    val empty: BuildContext = BuildContext(
      testProjects = Set.empty,
      allProjectDeps = Map.empty,
      platforms = Map.empty,
      sourcegen = SourcegenPlan.empty,
      apPlan = AnnotationProcessorPlan.empty,
      kspPlan = SymbolProcessorPlan.empty,
      postCompile = Map.empty
    )
  }

  /** Build DAG based on build mode. */
  def buildDag(
      projects: Set[CrossProjectName],
      ctx: BuildContext,
      mode: BuildMode
  ): Dag = mode match {
    case BuildMode.Compile           => buildCompileDag(projects, ctx)
    case BuildMode.Link(releaseMode) => buildLinkDag(projects, ctx, releaseMode)
    case BuildMode.Test              => buildTestDag(projects, ctx)
    case BuildMode.Run(_, _)         =>
      // Run mode is similar to link mode - compile and optionally link
      buildLinkDag(projects, ctx, releaseMode = false)
  }

  /** Compute the CompileTask deps for a project: upstream-project compiles plus sourcegen tasks plus the project's annotation-processor and KSP resolution
    * tasks (when configured).
    */
  private def compileDeps(
      project: CrossProjectName,
      ctx: BuildContext,
      inScope: Set[CrossProjectName]
  ): (Set[CrossProjectName], Set[TaskId]) = {
    val projectDeps = ctx.allProjectDeps.getOrElse(project, Set.empty).filter(inScope.contains)
    val compileTaskDeps: Set[TaskId] = projectDeps.map(p => TaskId.Compile(p): TaskId)
    val sourcegenDeps: Set[TaskId] =
      ctx.sourcegen.perProject.getOrElse(project, Set.empty).map(s => TaskId.Sourcegen(s): TaskId)
    val apDeps: Set[TaskId] =
      if (ctx.apPlan.needsResolution(project)) Set(TaskId.ResolveAnnotationProcessors(project): TaskId)
      else Set.empty
    val kspDeps: Set[TaskId] =
      if (ctx.kspPlan.needsResolution(project)) Set(TaskId.RunSymbolProcessors(project): TaskId)
      else Set.empty
    (projectDeps, compileTaskDeps ++ sourcegenDeps ++ apDeps ++ kspDeps)
  }

  /** Build the per-project AP resolution tasks for projects in the plan that are also in scope. */
  private def annotationProcessorTasks(
      inScope: Set[CrossProjectName],
      apPlan: AnnotationProcessorPlan
  ): Seq[ResolveAnnotationProcessorsTask] =
    if (apPlan.isEmpty) Seq.empty
    else apPlan.projects.intersect(inScope).toSeq.map(ResolveAnnotationProcessorsTask.apply)

  /** Build the per-project KSP run tasks for projects in the plan that are also in scope. Each task's `upstreamCompileDeps` is the project's transitive
    * dependency set intersected with the in-scope set — KSP needs those compiled before it can resolve cross-project types.
    */
  private def symbolProcessorTasks(
      inScope: Set[CrossProjectName],
      kspPlan: SymbolProcessorPlan,
      allProjectDeps: Map[CrossProjectName, Set[CrossProjectName]]
  ): Seq[RunSymbolProcessorsTask] =
    if (kspPlan.isEmpty) Seq.empty
    else
      kspPlan.projects.intersect(inScope).toSeq.map { project =>
        val upstream = allProjectDeps.getOrElse(project, Set.empty).filter(inScope.contains)
        RunSymbolProcessorsTask(project, upstream)
      }

  /** Build SourcegenTasks from the plan plus the set of project compiles already in scope. Returns the sourcegen tasks plus any extra script-project compiles
    * that need to be included (if not already present).
    */
  private def sourcegenTasksAndScriptCompiles(
      inScope: Set[CrossProjectName],
      sourcegen: SourcegenPlan
  ): (Seq[SourcegenTask], Set[CrossProjectName]) =
    if (sourcegen.isEmpty) (Seq.empty, Set.empty)
    else {
      // Aggregate forProjects per declaration
      val scriptToTargets: Map[ScriptDef.Main, Set[CrossProjectName]] =
        sourcegen.perProject.toSeq
          .flatMap { case (target, scripts) => scripts.map(s => s -> target) }
          .groupMap(_._1)(_._2)
          .view
          .mapValues(_.toSet)
          .toMap

      // One task per task id. Grouping by the whole declaration made two tasks with one id whenever consumers declared the same script differently, and the
      // DAG kept only one of them: the other's consumers compiled without their sourcegen ever running.
      val tasks = scriptToTargets.toSeq.groupBy { case (script, _) => TaskId.Sourcegen(script) }.toSeq.map { case (id, declarations) =>
        val deps = declarations.flatMap { case (script, _) => sourcegen.scriptProjectDeps.getOrElse(script, Set(script.project)) }.toSet
        SourcegenTask(id.scriptProject, id.mainClass, declarations.toMap, deps)
      }
      val extraScriptProjects = tasks.flatMap(_.scriptProjectDeps).toSet -- inScope
      (tasks, extraScriptProjects)
    }

  /** What builds each of `allProjects`: its compile, and for a project with a `postCompile` step, the step after it. */
  private def buildProjectTasks(allProjects: Set[CrossProjectName], ctx: BuildContext): Set[Task] =
    allProjects.flatMap { project =>
      val (projectDeps, deps) = compileDeps(project, ctx, allProjects)
      ctx.postCompile.get(project) match {
        case Some(reads) => Set[Task](CompileTask(project, projectDeps, deps, postCompile = true), PostCompileTask(project, reads))
        case None        => Set[Task](CompileTask(project, projectDeps, deps, postCompile = false))
      }
    }

  /** Build DAG for compile-only (no linking, no tests). */
  def buildCompileDag(projects: Set[CrossProjectName], ctx: BuildContext): Dag = {
    val targetTransitive = transitiveDependencies(projects, ctx.buildOrderDeps)
    val (sourcegenTasks, extraScriptProjects) = sourcegenTasksAndScriptCompiles(targetTransitive, ctx.sourcegen)
    // Script projects' transitive compile tasks — we already have the dep closure from the plan, but they may themselves depend on others we haven't walked.
    val scriptTransitive = transitiveDependencies(extraScriptProjects, ctx.buildOrderDeps)
    val allProjects = targetTransitive ++ scriptTransitive

    val projectTasks = buildProjectTasks(allProjects, ctx)

    val apTasks = annotationProcessorTasks(allProjects, ctx.apPlan)
    val kspTasks = symbolProcessorTasks(allProjects, ctx.kspPlan, ctx.allProjectDeps)

    Dag.fromTasks(projectTasks.toSeq ++ sourcegenTasks ++ apTasks ++ kspTasks)
  }

  /** Build initial DAG for test execution.
    *
    * Every target and its transitive dependencies get a CompileTask. Only the targets that declared `isTestProject: true` get a DiscoverTask — and, on non-JVM
    * platforms, the LinkTask between compile and discover. TestSuiteTasks are added dynamically after discovery completes.
    *
    * The two sets are not the same, which is why `targets` and `ctx.testProjects` are both consulted. `bleep ci` hands this *every* project in the build so
    * that every project compiles in one pass; when `targets` alone decided where suites came from, a project carrying a test framework had its suites run by
    * `bleep ci` despite `isTestProject: false` — while `bleep test` skipped it. `isTestProject` is the single answer to "are there suites here", whichever
    * command is asking.
    */
  def buildTestDag(targets: Set[CrossProjectName], ctx: BuildContext): Dag = {
    val targetTransitive = transitiveDependencies(targets, ctx.buildOrderDeps)
    val (sourcegenTasks, extraScriptProjects) = sourcegenTasksAndScriptCompiles(targetTransitive, ctx.sourcegen)
    val scriptTransitive = transitiveDependencies(extraScriptProjects, ctx.buildOrderDeps)
    val allProjects = targetTransitive ++ scriptTransitive

    val projectTasks = buildProjectTasks(allProjects, ctx)

    val suiteBearing = targets.filter(ctx.testProjects)

    val linkTasks = suiteBearing.flatMap { project =>
      ctx.platforms.get(project) match {
        case Some(LinkPlatform.Jvm) | None => None
        case Some(platform)                =>
          Some(LinkTask(project, platform, releaseMode = false, isTest = true))
      }
    }

    val discoverTasks = suiteBearing.map { project =>
      DiscoverTask(project, ctx.platforms.get(project))
    }

    val apTasks = annotationProcessorTasks(allProjects, ctx.apPlan)
    val kspTasks = symbolProcessorTasks(allProjects, ctx.kspPlan, ctx.allProjectDeps)

    Dag.fromTasks((projectTasks ++ linkTasks ++ discoverTasks).toSeq ++ sourcegenTasks ++ apTasks ++ kspTasks)
  }

  /** Build DAG for linking (compile + link without tests). */
  def buildLinkDag(projects: Set[CrossProjectName], ctx: BuildContext, releaseMode: Boolean): Dag = {
    val targetTransitive = transitiveDependencies(projects, ctx.buildOrderDeps)
    val (sourcegenTasks, extraScriptProjects) = sourcegenTasksAndScriptCompiles(targetTransitive, ctx.sourcegen)
    val scriptTransitive = transitiveDependencies(extraScriptProjects, ctx.buildOrderDeps)
    val allProjects = targetTransitive ++ scriptTransitive

    val projectTasks = buildProjectTasks(allProjects, ctx)

    val linkTasks = projects.flatMap { project =>
      ctx.platforms.get(project) match {
        case Some(LinkPlatform.Jvm) | None => None
        case Some(platform)                =>
          Some(LinkTask(project, platform, releaseMode, isTest = ctx.testProjects(project)))
      }
    }

    val apTasks = annotationProcessorTasks(allProjects, ctx.apPlan)
    val kspTasks = symbolProcessorTasks(allProjects, ctx.kspPlan, ctx.allProjectDeps)

    Dag.fromTasks((projectTasks ++ linkTasks).toSeq ++ sourcegenTasks ++ apTasks ++ kspTasks)
  }

  /** Get transitive dependencies for a set of projects */
  private def transitiveDependencies(
      projects: Set[CrossProjectName],
      deps: Map[CrossProjectName, Set[CrossProjectName]]
  ): Set[CrossProjectName] = {
    val visited = mutable.Set[CrossProjectName]()
    val queue = mutable.Queue[CrossProjectName]()
    queue.enqueueAll(projects)

    while (queue.nonEmpty) {
      val p = queue.dequeue()
      if (!visited.contains(p)) {
        visited += p
        val projectDeps = deps.getOrElse(p, Set.empty)
        queue.enqueueAll(projectDeps.filterNot(visited.contains))
      }
    }

    visited.toSet
  }

  /** Parallel DAG executor */
  trait DagExecutor {

    /** Execute the DAG with explicit kill signal.
      *
      * @param dag
      *   The DAG to execute
      * @param channel
      *   This request's line to the machine scheduler: the executor submits what is ready, in priority order, and starts what it is granted.
      * @param eventQueue
      *   Queue for emitting events
      * @param killSignal
      *   Deferred that can be completed to kill all running tasks
      * @return
      *   The final DAG state
      */
    def execute(
        dag: Dag,
        channel: RequestChannel,
        forkHeaps: ForkHeaps,
        eventQueue: Queue[IO, Option[DagEvent]],
        killSignal: Deferred[IO, KillReason]
    ): IO[Dag]
  }

  /** Per-task-type handler functions bundled into a single value. New task types are added as fields here rather than as new positional parameters cascading
    * through the callsites. Every field is required — there are no default no-op handlers (a no-op default is just a default parameter in disguise; the call
    * site should know which task types it expects to see).
    */
  /** What the scheduler granted a task, for the handlers whose shape depends on the platform: a fork to run a process under, or a slot in the server's heap.
    * The handler checks it got the shape its platform needs and throws otherwise — a Kotlin/Native link handed an in-heap slot is a bug in `demandFor`, not a
    * case to work around.
    */
  sealed trait TaskGrant
  object TaskGrant {
    case class Fork(fork: GrantedFork) extends TaskGrant
    case object InHeap extends TaskGrant

    /** The fork, for a platform that runs a process; loud when the grant is in-heap. */
    def forkFor(grant: TaskGrant, what: String): GrantedFork = grant match {
      case Fork(fork) => fork
      case InHeap     => throw new IllegalStateException(s"$what runs a process but was granted an in-heap slot — demandFor and the handler disagree")
    }

    /** Checks that a platform working in the server's heap was not handed a fork it would leave unused and charged. */
    def requireInHeap(grant: TaskGrant, what: String): Unit = grant match {
      case InHeap  => ()
      case Fork(f) =>
        throw new IllegalStateException(s"$what runs in the server's heap but was granted fork #${f.id.value} — demandFor and the handler disagree")
    }
  }

  case class Handlers(
      compile: (CompileTask, Deferred[IO, KillReason]) => IO[TaskResult],
      /** Forks a JVM under the grant the scheduler allotted it, reporting each process it starts through the [[GrantedFork]]; the executor reports the fork
        * gone when the handler returns.
        */
      postCompile: (PostCompileTask, GrantedFork, Deferred[IO, KillReason]) => IO[TaskResult],
      /** In the server's heap for Scala.js and Kotlin/JS; under a fork for Kotlin/Native (`konanc`) and Scala Native — see [[demandFor]]. */
      link: (LinkTask, TaskGrant, Deferred[IO, KillReason]) => IO[(TaskResult, LinkResult)],
      /** Discovery reads the linked artifact on JS and Native — it asks the binary to enumerate its own suites — so it needs the same link output the run does.
        * Where that means running the artifact (Kotlin/JS, Kotlin/Native) the grant is a fork and the process is reported through it; by reflection (JVM,
        * Scala.js, Scala Native) it is in-heap.
        */
      discover: (DiscoverTask, Option[LinkResult], TaskGrant, Deferred[IO, KillReason]) => IO[(TaskResult, DiscoveryResult)],
      /** Given the suite to run and what its project's link produced, run it.
        *
        * The `LinkResult` is `None` on the JVM, where nothing links, and `Some` for every platform that does. Passing it beats letting the handler rebuild the
        * path from convention: the linker already knows where it wrote, and a second derivation is a second thing to keep in step with it.
        */
      test: (TestSuiteTask, Option[LinkResult], Deferred[IO, KillReason]) => IO[TaskResult],
      /** Run a whole project's JUnit suites as one batched execution. JVM-only (JUnit Platform has no non-JVM linked form), so no LinkResult. */
      testBatch: (TestBatchTask, Deferred[IO, KillReason]) => IO[TaskResult],
      sourcegen: (SourcegenTask, GrantedFork, Deferred[IO, KillReason]) => IO[TaskResult],
      annotationProcessor: (ResolveAnnotationProcessorsTask, Deferred[IO, KillReason]) => IO[(TaskResult, Int)],
      symbolProcessor: (RunSymbolProcessorsTask, GrantedFork, Deferred[IO, KillReason]) => IO[(TaskResult, Int)]
  )

  /** Create a DAG executor with the given handlers. */
  def executor(handlers: Handlers): DagExecutor = new DagExecutor {

    override def execute(
        initialDag: Dag,
        channel: RequestChannel,
        forkHeaps: ForkHeaps,
        eventQueue: Queue[IO, Option[DagEvent]],
        killSignal: Deferred[IO, KillReason]
    ): IO[Dag] = {
      def now: IO[Long] = IO.realTime.map(_.toMillis)

      def isTest(task: Task): Boolean = task match {
        case _: TestSuiteTask | _: TestBatchTask => true
        case _                                   => false
      }

      /** Test tasks not finished, per project: what keeps a warm fork alive between a project's suites (design §5 rule 3). */
      def pendingTestsByProject(dag: Dag): Map[String, Int] =
        (dag.tasks.keySet -- dag.finished).toList.map(dag.tasks).filter(isTest).groupBy(_.project.value).map { case (p, ts) => p -> ts.size }

      def emit(event: DagEvent): IO[Unit] = eventQueue.offer(Some(event))

      /** Check if kill has been requested (non-blocking) */
      def isKilled: IO[Option[KillReason]] = killSignal.tryGet

      /** Reduce a TaskResult to the `(success, errorMsg)` pair the finished-event constructors take. Shared by every per-task branch that emits a
        * `DagEvent.*Finished` to keep the success/failure mapping consistent across task kinds.
        */
      def resultSummary(result: TaskResult): (Boolean, Option[String]) = result match {
        case TaskResult.Success            => (true, None)
        case TaskResult.Failure(error, _)  => (false, Some(error))
        case TaskResult.Error(error, _)    => (false, Some(error))
        case TaskResult.Skipped(failedDep) => (false, Some(s"dependency ${failedDep.id.value} failed"))
        case TaskResult.Killed(reason)     => (false, Some(s"killed: $reason"))
        case TaskResult.TimedOut(_)        => (false, Some("timed out"))
      }

      def executeTask(task: Task, fork: Option[ForkId], dagRef: Ref[IO, Dag], taskKillSignals: Ref[IO, Map[TaskId, Deferred[IO, KillReason]]]): IO[Unit] = {
        val startTime = System.currentTimeMillis()
        val grant: Option[GrantedFork] = fork.map(id => channel.grantedFork(id, task.id.value, ForkKey(task.id.value)))
        def forkIdOrThrow: GrantedFork = grant.getOrElse(throw new IllegalStateException(s"${task.id} forks a JVM but was started without a fork grant"))
        val taskGrant: TaskGrant = grant match {
          case Some(f) => TaskGrant.Fork(f)
          case None    => TaskGrant.InHeap
        }

        // Per-task kill signal as a Resource so the propagation fiber + registration are both scoped to the task's lifetime. On release: the `.background`
        // cancels the propagation fiber (no leaked listener), and `taskKillSignals` is deregistered.
        val taskKillSignal: cats.effect.Resource[IO, Deferred[IO, KillReason]] = {
          val acquire = for {
            taskKill <- Deferred[IO, KillReason]
            _ <- taskKillSignals.update(_ + (task.id -> taskKill))
          } yield taskKill
          val release = taskKillSignals.update(_ - task.id)

          for {
            taskKill <- cats.effect.Resource.make(acquire)(_ => release)
            // .attempt handles "already completed" (task finished before global kill arrived).
            _ <- killSignal.get.flatMap(reason => taskKill.complete(reason).attempt.void).background
          } yield taskKill
        }

        /** Render a thrown exception as the TaskResult it semantically is.
          *
          * A THROWN exception is infrastructure failure, not a logical one: `TaskResult.Error`, not `Failure`. Handlers return `Failure` explicitly for logical
          * failures they already reported (compile diagnostics, `N test(s) failed` after a SuiteFinished). What reaches here is only exceptions nothing
          * reported — e.g. bleep-test-runner failing to resolve throws out of getTestClasspath BEFORE any suite runs. Mapping that to `Failure` made a
          * TestSuiteTask swallow it (its event mapping assumes a preceding SuiteFinished conveyed it), so it surfaced only as an uncategorized "detailed info
          * was not captured" count. As `Error` it becomes a SuiteError / CompileFinished(Error) carrying the actual message.
          */
        def errorResult(taskName: String, error: Throwable): TaskResult = {
          val lines = new scala.collection.mutable.ArrayBuffer[String]()
          lines += s"$taskName failed: ${error.getClass.getName}: ${error.getMessage}"
          var cause = error.getCause
          while (cause != null) {
            lines += s"  Caused by: ${cause.getClass.getName}: ${cause.getMessage}"
            cause = cause.getCause
          }
          val frames = error.getStackTrace.take(10)
          if (frames.nonEmpty) {
            lines += "  Stack trace:"
            frames.foreach(f => lines += s"    at $f")
          }
          TaskResult.Error(error = lines.mkString("\n"), processExit = ProcessExit.Unknown)
        }

        /** Convert ALL outcomes (success, thrown error, cancellation) into a value, via `fiber.join` + `embed` with a kill fallback.
          *
          * Generic in the payload so tasks whose handler returns more than a TaskResult (annotation processors and KSP also return a discovered-jar count) get
          * the same recovery instead of hand-rolling their own — `inject` says how to represent a recovered TaskResult in that payload.
          */
        def withRecoveryOf[A](taskName: String, taskKill: Deferred[IO, KillReason], inject: TaskResult => A)(io: IO[A]): IO[A] =
          io.start
            .flatMap(_.join)
            .flatMap { outcome =>
              outcome.embed(
                onCancel = taskKill.tryGet.map {
                  case Some(reason) => inject(TaskResult.Killed(reason))
                  case None         => inject(TaskResult.Killed(KillReason.UserRequest)) // Fallback
                }
              )
            }
            .handleErrorWith(error => IO.pure(inject(errorResult(taskName, error))))

        def withRecovery(taskName: String, taskKill: Deferred[IO, KillReason])(io: IO[TaskResult]): IO[TaskResult] =
          withRecoveryOf[TaskResult](taskName, taskKill, identity)(io)

        /** For handlers returning `(TaskResult, Int)`: a recovered failure discovered nothing. */
        def withRecoveryCounted(taskName: String, taskKill: Deferred[IO, KillReason])(io: IO[(TaskResult, Int)]): IO[(TaskResult, Int)] =
          withRecoveryOf[(TaskResult, Int)](taskName, taskKill, r => (r, 0))(io)

        for {
          maybeKilled <- isKilled
          timestamp <- now
          _ <- emit(DagEvent.TaskStarted(task, timestamp))
          result <- maybeKilled match {
            case Some(reason) =>
              // Task was killed before it started
              IO.pure(TaskResult.Killed(reason))
            case None =>
              taskKillSignal.use { taskKill =>
                task match {
                  case ct: CompileTask =>
                    withRecovery(s"Compile ${ct.project.value}", taskKill)(handlers.compile(ct, taskKill))

                  case pct: PostCompileTask =>
                    withRecovery(s"Post-compile ${pct.project.value}", taskKill)(handlers.postCompile(pct, forkIdOrThrow, taskKill))

                  case lt: LinkTask =>
                    withRecovery(s"Link ${lt.project.value}", taskKill) {
                      for {
                        linkStartTs <- now
                        _ <- emit(DagEvent.LinkStarted(lt.project, lt.platform.name, linkStartTs))
                        (result, linkResult) <- handlers.link(lt, taskGrant, taskKill)
                        linkEndTs <- now
                        _ <- emit(DagEvent.LinkFinished(lt.project, linkResult, linkEndTs - linkStartTs, linkEndTs, lt.platform.name))
                        _ <- dagRef.update(_.recordLinkResult(lt.id, lt.project, linkResult))
                      } yield result
                    }

                  case dt: DiscoverTask =>
                    withRecovery(s"Discover ${dt.project.value}", taskKill) {
                      for {
                        linkOutput <- dagRef.get.map(_.linkResults.get(TaskId.Link(dt.project)))
                        (result, discovery) <- handlers.discover(dt, linkOutput, taskGrant, taskKill)
                        _ <- result match {
                          case TaskResult.Success =>
                            // One batched execution per framework group (per-project mode) — an execution-scoped fixture / a framework's Runner is built once for
                            // all its classes. Whatever a batch does not cover (per-suite mode, or PlatformRunner suites) runs suite-by-suite.
                            val batchTasks: List[Task] =
                              discovery.batches.map { case (groupSuites, degree) =>
                                val ordered = groupSuites.sortBy(_._1).map { case (n, sel) => (SuiteName(n), sel) }
                                TestBatchTask(dt.project, ordered, degree)
                              }
                            val batchedNames: Set[String] = discovery.batches.flatMap(_._1.map(_._1)).toSet
                            // Suite-by-suite for the rest. With a suite-parallelism bound, a project's suites form that many round-robin chains, alphabetically
                            // ordered — surefire's usual class order, which schema-bootstrapping setups rely on. At bound 1 all suites run through one warm fork
                            // sequentially.
                            val perSuite = discovery.suites.filterNot { case (n, _) => batchedNames(n) }.sortBy(_._1)
                            val bound = discovery.suiteParallelism.getOrElse(Int.MaxValue)
                            val suiteTasks: List[Task] =
                              perSuite.zipWithIndex.map { case ((suiteName, selection), idx) =>
                                val after: Set[TaskId] =
                                  if (idx < bound) Set.empty
                                  else Set(TaskId.Test(dt.project, SuiteName(perSuite(idx - bound)._1)))
                                TestSuiteTask(dt.project, SuiteName(suiteName), selection, runAfter = after)
                              }
                            val newTasks: List[Task] = batchTasks ++ suiteTasks
                            dagRef.update(dag => newTasks.foldLeft(dag)(_.addTask(_))) >>
                              emit(
                                DagEvent.SuitesDiscovered(
                                  dt.project,
                                  discovery.suites.map(s => SuiteName(s._1)),
                                  discovery.discoveredBeforeFilters,
                                  timestamp
                                )
                              )
                          case _ => IO.unit
                        }
                      } yield result
                    }

                  case tt: TestSuiteTask =>
                    // Tests handle their own cancellation: the kill-signal Deferred (`taskKill`)
                    // is racked by handlers.test internally (e.g. TestRunner.runSuite races
                    // suite-execution vs idle-timeout vs killSignal.get). `withRecovery` catches
                    // fiber cancellation via outcome.embed and emits a Killed TaskResult, and the
                    // outer executeTask still runs the TaskFinished emit after — so a cancelled
                    // test reports a structured Killed status without us blocking cancellation
                    // entirely. Previously this branch wrapped the whole thing in IO.uncancelable
                    // "so status events always fire", but that meant a wedged test framework
                    // could pin the BSP fiber indefinitely and block server shutdown.
                    dagRef.get.flatMap { dag =>
                      val linkResult = dag.linkResults.get(TaskId.Link(tt.project))
                      withRecovery(s"Test ${tt.suiteName.value}", taskKill)(handlers.test(tt, linkResult, taskKill))
                    }

                  case bt: TestBatchTask =>
                    // Same cancellation story as TestSuiteTask: the handler races execution vs the kill signal internally.
                    withRecovery(s"Test batch ${bt.project.value}", taskKill)(handlers.testBatch(bt, taskKill))

                  // These three emit their own Started/Finished pair, and the Finished carries the
                  // error message. Recovery therefore wraps ONLY the handler call, with the emit
                  // driven by the recovered result — if withRecovery wrapped the whole
                  // for-comprehension instead, a thrown handler would skip straight past the
                  // Finished emit and the exception would reach the client as nothing at all (the
                  // TaskFinished mapping deliberately emits no protocol event for these tasks).
                  case sgt: SourcegenTask =>
                    for {
                      sourcegenStartTs <- now
                      forProjectsList = sgt.forProjects.toList.sortBy(_.value)
                      _ <- emit(DagEvent.SourcegenStarted(sgt.scriptProject, sgt.main, forProjectsList, sourcegenStartTs))
                      result <- withRecovery(s"Sourcegen ${sgt.main}", taskKill)(handlers.sourcegen(sgt, forkIdOrThrow, taskKill))
                      sourcegenEndTs <- now
                      (success, errorMsg) = resultSummary(result)
                      _ <- emit(
                        DagEvent.SourcegenFinished(sgt.scriptProject, sgt.main, success, sourcegenEndTs - sourcegenStartTs, errorMsg, sourcegenEndTs)
                      )
                    } yield result

                  case apt: ResolveAnnotationProcessorsTask =>
                    for {
                      apStartTs <- now
                      _ <- emit(DagEvent.ResolveAnnotationProcessorsStarted(apt.project, apStartTs))
                      resultAndCount <- withRecoveryCounted(s"ResolveAnnotationProcessors ${apt.project.value}", taskKill)(
                        handlers.annotationProcessor(apt, taskKill)
                      )
                      (result, discoveredJarCount) = resultAndCount
                      apEndTs <- now
                      (success, errorMsg) = resultSummary(result)
                      _ <- emit(
                        DagEvent.ResolveAnnotationProcessorsFinished(apt.project, success, apEndTs - apStartTs, errorMsg, discoveredJarCount, apEndTs)
                      )
                    } yield result

                  case kspt: RunSymbolProcessorsTask =>
                    for {
                      kspStartTs <- now
                      _ <- emit(DagEvent.RunSymbolProcessorsStarted(kspt.project, kspStartTs))
                      resultAndCount <- withRecoveryCounted(s"RunSymbolProcessors ${kspt.project.value}", taskKill)(
                        handlers.symbolProcessor(kspt, forkIdOrThrow, taskKill)
                      )
                      (result, discoveredJarCount) = resultAndCount
                      kspEndTs <- now
                      (success, errorMsg) = resultSummary(result)
                      _ <- emit(DagEvent.RunSymbolProcessorsFinished(kspt.project, success, kspEndTs - kspStartTs, errorMsg, discoveredJarCount, kspEndTs))
                    } yield result
                }
              }
          }
          endTimestamp <- now
          durationMs = endTimestamp - startTime
          _ <- emit(DagEvent.TaskFinished(task, result, durationMs, endTimestamp))
          // What the task held goes back: an in-heap slot, or a fork whose process is gone now that its handler has returned. A test task reports nothing
          // here — its fork is the pool's, and the pool told the scheduler when the suite let go of it.
          _ <- IO {
            demandFor(task, forkHeaps, channel.id) match {
              case Some(_: InHeap)     => channel.inHeapFinished(bleep.machine.TaskId(task.id.value))
              case Some(_: ForkDemand) =>
                grant.foreach(_.ended())
                fork.foreach(channel.forkExited)
              case None => channel.testFinished(task.project.value)
            }
          }
          _ <- result match {
            case TaskResult.Success       => dagRef.update(_.complete(task.id))
            case TaskResult.Failure(_, _) => dagRef.update(_.fail(task.id))
            case TaskResult.Error(_, _)   => dagRef.update(_.error(task.id))
            case TaskResult.Skipped(_)    => dagRef.update(_.skip(task.id))
            case TaskResult.Killed(_)     => dagRef.update(_.kill(task.id))
            case TaskResult.TimedOut(_)   => dagRef.update(_.timeout(task.id))
          }
        } yield ()
      }

      def skipTask(task: Task, failedDep: Task, dagRef: Ref[IO, Dag]): IO[Unit] =
        for {
          timestamp <- now
          _ <- emit(DagEvent.TaskStarted(task, timestamp))
          _ <- emit(DagEvent.TaskFinished(task, TaskResult.Skipped(failedDep), 0, timestamp))
          _ <- dagRef.update(_.skip(task.id))
        } yield ()

      def killTask(task: Task, reason: KillReason, dagRef: Ref[IO, Dag]): IO[Unit] =
        for {
          timestamp <- now
          _ <- emit(DagEvent.TaskStarted(task, timestamp))
          _ <- emit(DagEvent.TaskFinished(task, TaskResult.Killed(reason), 0, timestamp))
          _ <- dagRef.update(_.kill(task.id))
        } yield ()

      // Coalescing wakeup channel. Every task-completion does `wakeup.tryOffer(())` — non-blocking,
      // dropped if a wakeup is already pending (no point queuing N wakeups when the loop will
      // re-read everything anyway). The loop `take`s one wakeup per iteration. This replaces the
      // prior pattern of rotating a Deferred under a Ref, which conflated "wake the loop" with
      // "broadcast a signal", had a race window where completions could land between the rotate
      // and the next get, and made the deadlock-detection path fire spurious false positives on
      // very fast no-op tasks.
      def loop(
          dagRef: Ref[IO, Dag],
          runningRef: Ref[IO, Set[TaskId]],
          taskKillSignals: Ref[IO, Map[TaskId, Deferred[IO, KillReason]]],
          wakeup: Queue[IO, Unit],
          supervisor: cats.effect.std.Supervisor[IO]
      ): IO[Unit] =
        for {
          // Read `running` BEFORE `dag`. A completing task writes in the opposite order — finished
          // into dagRef (end of executeTask), then removed from runningRef (its guarantee) — so a
          // task that finishes between the two reads is visible in at least one snapshot. Read the
          // other way around, it is visible in neither: not finished in the stale dag, not running
          // in the fresh set — so `ready.filterNot(running)` admits it a second time and the task
          // runs twice. Caught by LinkDagIntegrationTest emitting two LinkStarted events 1ms apart.
          running <- runningRef.get
          dag <- dagRef.get
          maybeKilled <- isKilled
          _ <-
            if (dag.isComplete) {
              IO(
                System.err.println(
                  s"[DAG] Executor complete: ${dag.tasks.size} tasks, ${dag.completed.size} completed, ${dag.failed.size} failed, ${dag.errored.size} errored, ${dag.skipped.size} skipped, ${dag.killed.size} killed"
                )
              )
            } else if (maybeKilled.isDefined && running.isEmpty) {
              // Kill requested and no tasks running - kill all remaining tasks
              val remaining = dag.tasks.keySet -- dag.completed -- dag.failed -- dag.errored -- dag.skipped -- dag.killed -- dag.timedOut
              IO(
                System.err
                  .println(s"[DAG] Kill requested (${maybeKilled.get}), no tasks running. Killing ${remaining.size} remaining: ${remaining.mkString(", ")}")
              ) >>
                remaining.toList.traverse_ { taskId =>
                  killTask(dag.tasks(taskId), maybeKilled.get, dagRef)
                }
            } else if (maybeKilled.isDefined) {
              // Kill requested but tasks still running - wait for any to complete
              IO(System.err.println(s"[DAG] Kill requested (${maybeKilled.get}), waiting for ${running.size} running tasks: ${running.mkString(", ")}")) >>
                wakeup.take >> loop(dagRef, runningRef, taskKillSignals, wakeup, supervisor)
            } else {
              // Normal execution
              // Skip tasks with failed dependencies
              val toSkip = dag.toSkip
              for {
                _ <- toSkip.toList.traverse_ { case (task, failedDep) =>
                  skipTask(task, failedDep, dagRef)
                }
                // Ready tasks not already running, most-unblocking first. Test tasks start now and ask the scheduler for their fork from inside; everything
                // else is submitted to the scheduler as this request's ready set, in this order, and starts when granted.
                readyTasks = dag.ready.filterNot(t => running.contains(t.id))
                depCounts = dag.dependentsCount
                prioritized = readyTasks.toList.sortBy(t => -depCounts.getOrElse(t.id, 0))
                (tests, scheduled) = prioritized.partition(isTest)
                // Grants first, then the ready set: a task granted since the last look starts now and is not submitted again. The channel drops anything
                // whose grant lands between these two steps, so a demand is never in front of the scheduler twice.
                granted <- IO(channel.takeGrants())
                // A grant for a task that is no longer ready (killed, skipped) gives its resource straight back: a spawned fork as exited — nothing will start
                // it — and a reused one with the cpu the demand asked for, since the fork itself runs on for whoever else holds it.
                byId = dag.tasks.map { case (id, t) => (t.id.value, t) }
                startable = granted.flatMap { g =>
                  byId.get(g.taskId.value).filter(t => readyTasks.contains(t)) match {
                    case Some(t) => List((t, g))
                    case None    =>
                      g.grant match {
                        case Grant.Fork(ForkGrant.Spawn(fork)) => channel.forkExited(fork)
                        case Grant.Fork(ForkGrant.Reuse(fork)) => channel.forkWorkFinished(fork, g.cpu)
                        case Grant.InHeap                      => channel.inHeapFinished(g.taskId)
                      }
                      Nil
                  }
                }
                starting = startable.map(_._1.id).toSet
                demands = scheduled.filterNot(t => starting.contains(t.id)).flatMap(t => demandFor(t, forkHeaps, channel.id))
                _ <- IO(channel.setDagReady(demands, pendingTestsByProject(dag)))
                toStart = tests.map(t => (t, Option.empty[ForkId])) ++ startable.map { case (t, g) =>
                  (
                    t,
                    g.grant match {
                      case Grant.Fork(fg) => Some(fg.fork)
                      case Grant.InHeap   => None
                    }
                  )
                }
                // Start tasks. The guarantee cleans up runningRef and wakes the loop — and the wakeup is what re-runs this, so a completion is exactly when
                // the ready set is resubmitted.
                _ <- toStart.traverse_ { case (task, fork) =>
                  runningRef.update(_ + task.id) >>
                    supervisor
                      .supervise(
                        executeTask(task, fork, dagRef, taskKillSignals)
                          .guarantee(runningRef.update(_ - task.id) >> wakeup.tryOffer(()).void)
                      )
                      .void
                }
                // Re-read state. If nothing is running, the DAG is either complete, in a transient gap (skips just opened up new ready tasks), genuinely
                // stuck, or waiting for the scheduler's grant — which wakes this loop when it arrives.
                newRunning <- runningRef.get
                newDag <- dagRef.get
                _ <-
                  if (newDag.isComplete) IO.unit
                  else if (newRunning.isEmpty && newDag.ready.isEmpty && newDag.toSkip.isEmpty) {
                    val remaining = newDag.tasks.keySet -- newDag.finished
                    val stuckDetails = remaining.toList.map { taskId =>
                      val task = newDag.tasks(taskId)
                      val unsatisfied = task.dependencies.filterNot(newDag.finished.contains)
                      s"  $taskId (waiting for: ${unsatisfied.mkString(", ")})"
                    }
                    IO.raiseError(
                      new RuntimeException(
                        s"DAG deadlock: ${remaining.size} tasks stuck:\n${stuckDetails.mkString("\n")}"
                      )
                    )
                  } else if (newRunning.isEmpty && newDag.toSkip.nonEmpty) {
                    // No tasks running but skips opened up new ready tasks — re-evaluate without waiting.
                    loop(dagRef, runningRef, taskKillSignals, wakeup, supervisor)
                  } else {
                    wakeup.take >> loop(dagRef, runningRef, taskKillSignals, wakeup, supervisor)
                  }
              } yield ()
            }
        } yield ()

      // Supervisor scopes the per-task fibers: if the executor's parent fiber is cancelled mid-execution, the supervisor cancels every still-running supervised
      // task fiber on resource release. Previously each task was spawned via `.start.void`, which orphans them on parent cancellation — they'd keep running
      // until they self-noticed the kill signal (which is also raced against the same parent cancellation).
      cats.effect.std.Supervisor[IO](await = false).use { supervisor =>
        for {
          dagRef <- Ref.of[IO, Dag](initialDag)
          runningRef <- Ref.of[IO, Set[TaskId]](Set.empty)
          taskKillSignals <- Ref.of[IO, Map[TaskId, Deferred[IO, KillReason]]](Map.empty)
          wakeup <- Queue.bounded[IO, Unit](1)
          // A grant from the scheduler wakes the loop exactly as a task completion does; both mean "look at the ready set again".
          _ <- IO {
            import cats.effect.unsafe.implicits.global
            channel.onGrant(() => wakeup.tryOffer(()).void.unsafeRunAndForget())
          }
          _ <- loop(dagRef, runningRef, taskKillSignals, wakeup, supervisor)
          finalDag <- dagRef.get
        } yield finalDag
      }
    }
  }
}
