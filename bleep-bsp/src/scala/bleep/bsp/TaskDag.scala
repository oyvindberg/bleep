package bleep.bsp

import bleep.MachineResources
import bleep.bsp.protocol.KillReason
import bleep.bsp.protocol.{BleepBspProtocol, LinkPlatformName, OutputChannel, ProcessExit, SuiteOutcome, TestStatus}
import bleep.bsp.protocol.BleepBspProtocol.BuildMode
import bleep.model.{CrossProjectName, KotlinJsModuleKind, ScriptDef, SuiteName, TestName}
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
    case class Compile(project: CrossProjectName) extends TaskId {
      val value: String = s"compile:${project.value}"
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
  }

  /** A task's claim on the machine. `cpu` is in cores; `memoryMb` is off-heap memory for a forked process. */
  case class Cost(kind: MachineResources.ResourceKind, cpu: Int, memoryMb: Long)

  /** What running `task` costs, given what this machine gives each kind of fork.
    *
    * A function rather than a field on the task: cost is a property of (what kind of work this is, how this machine is configured), not of the task's identity.
    * Putting it on the case class would also make two otherwise-identical tasks unequal because a config value differed.
    *
    * Admission is done with this by the scheduler loop, in priority order, instead of inside each task's body. Reserving inside meant the loop dispatched blind
    * and the governor queued FIFO — so the DAG's dependents-first ordering stopped mattering the moment anything had to wait.
    */
  def costOf(task: Task, forkHeaps: ForkHeaps): Cost =
    task match {
      // In-server work: a core, and no fork memory. Compile heap is watched separately by HeapPressureGate.
      case _: CompileTask                     => Cost(MachineResources.ResourceKind.Compile, cpu = 1, memoryMb = 0L)
      case _: DiscoverTask                    => Cost(MachineResources.ResourceKind.Compile, cpu = 1, memoryMb = 0L)
      case _: ResolveAnnotationProcessorsTask => Cost(MachineResources.ResourceKind.Compile, cpu = 1, memoryMb = 0L)
      // Forks: charged the heap they are started with plus the non-heap a JVM also commits.
      case _: SourcegenTask           => Cost(MachineResources.ResourceKind.SourcegenFork, cpu = 1, memoryMb = forkHeaps.sourcegenMb)
      case _: RunSymbolProcessorsTask => Cost(MachineResources.ResourceKind.KspFork, cpu = 1, memoryMb = forkHeaps.kspMb)
      // Scala.js, Scala Native, Kotlin/JS and Kotlin/Native each fork a linker (and node, for JS).
      // These reserved nothing at all until now — counted as one task while being a whole JVM plus a
      // toolchain, which is the accounting hole the governor exists to close.
      case _: LinkTask => Cost(MachineResources.ResourceKind.SourcegenFork, cpu = 1, memoryMb = forkHeaps.linkMb)
      // A core, but no fork memory HERE: acquiring a JVM may reuse a warm one (free) or spawn one
      // (measured RSS), and only the pool knows which. It holds that reservation itself, for a
      // lifetime this task does not share — one JVM serves many suites.
      case _: TestSuiteTask => Cost(MachineResources.ResourceKind.TestFork, cpu = 1, memoryMb = 0L)
    }

  /** Compile a project.
    *
    * `dependencies` is supplied directly (rather than derived) so the DAG builder can combine project-level compile deps with sourcegen deps (SourcegenTask
    * edges) and any future pre-compile steps. `projectDependencies` remains separate because the compile handler needs the project-level form (to compute
    * dependency analysis file paths for Zinc).
    */
  case class CompileTask(
      project: CrossProjectName,
      projectDependencies: Set[CrossProjectName],
      dependencies: Set[TaskId]
  ) extends Task {
    val id: TaskId = TaskId.Compile(project)
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
      script: ScriptDef.Main,
      forProjects: Set[CrossProjectName],
      scriptProjectDeps: Set[CrossProjectName]
  ) extends Task {
    val id: TaskId = TaskId.Sourcegen(script)
    val project: CrossProjectName = script.project
    val dependencies: Set[TaskId] = scriptProjectDeps.map(p => TaskId.Compile(p): TaskId)
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
    // Scala.js, Scala Native, Kotlin/JS and Kotlin/Native all fork a linker (and node, for JS). Those
    // forks reserved nothing until now — counted as one task while actually being a whole JVM plus a
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

  /** Kotlin/JS configuration */
  case class KotlinJsConfig(
      moduleKind: KotlinJsModuleKind,
      sourceMap: Boolean,
      dce: Boolean, // Dead Code Elimination - true = smaller output
      outputDir: java.nio.file.Path
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

  /** Execute a test suite */
  case class TestSuiteTask(
      project: CrossProjectName,
      suiteName: SuiteName,
      framework: String
  ) extends Task {
    val id: TaskId = TaskId.Test(project, suiteName)
    val dependencies: Set[TaskId] = Set(TaskId.Discover(project))
    // A core, but no fork memory declared here: acquiring a JVM from the pool may reuse a warm one
    // (free) or spawn a new one (measured RSS), and only the pool knows which. It holds that
    // reservation itself, from spawn until the process is destroyed — a lifetime this task does not
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
        timestamp: Long
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
        timestamp: Long
    ) extends DagEvent

    // Discovery events
    case class SuitesDiscovered(project: CrossProjectName, suites: List[SuiteName], timestamp: Long) extends DagEvent

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
      linkResults: Map[TaskId, LinkResult]
  ) {

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
        val depsFailed = task.dependencies.exists(propagatesFailure)

        if (depsFailed) None // Will be skipped
        else if (depsComplete) Some(task)
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
    def recordLinkResult(taskId: TaskId, result: LinkResult): Dag =
      copy(linkResults = linkResults + (taskId -> result))

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
    def empty: Dag = Dag(Map.empty[TaskId, Task], Set.empty, Set.empty, Set.empty, Set.empty, Set.empty, Set.empty, Map.empty)

    /** Create DAG from a set of tasks */
    def fromTasks(tasks: Seq[Task]): Dag =
      Dag(tasks.map(t => t.id -> t).toMap, Set.empty, Set.empty, Set.empty, Set.empty, Set.empty, Set.empty, Map.empty)
  }

  /** Plan for sourcegen DAG integration.
    *
    *   - `perProject` — for each target project that declared sourcegen in its config, the set of scripts that must run before it compiles.
    *   - `scriptProjectDeps` — for each script, the script project + its transitive dependency projects. The DAG inserts `CompileTask`s for all of these, and
    *     the `SourcegenTask` depends on their compiles.
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
      kspPlan: SymbolProcessorPlan
  )

  /** What each kind of forked JVM is charged: its heap plus the non-heap a JVM also commits (metaspace, code cache, stacks, GC structures). Resolved from
    * config once when the DAG is built, so the number a task declares is the number the fork is actually started with.
    */
  case class ForkHeaps(sourcegenMb: Long, kspMb: Long, linkMb: Long)
  object ForkHeaps {

    /** What bleep gives a fork that states no `-Xmx` of its own. */
    private val defaultFootprint: Long = MachineResources.forkFootprintMb(MachineResources.DefaultForkHeapMb)
    val default: ForkHeaps = ForkHeaps(sourcegenMb = defaultFootprint, kspMb = defaultFootprint, linkMb = defaultFootprint)
  }

  object BuildContext {
    val empty: BuildContext = BuildContext(
      allProjectDeps = Map.empty,
      platforms = Map.empty,
      sourcegen = SourcegenPlan.empty,
      apPlan = AnnotationProcessorPlan.empty,
      kspPlan = SymbolProcessorPlan.empty
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
      // Aggregate forProjects per script
      val scriptToTargets: Map[ScriptDef.Main, Set[CrossProjectName]] =
        sourcegen.perProject.toSeq
          .flatMap { case (target, scripts) => scripts.map(s => s -> target) }
          .groupMap(_._1)(_._2)
          .view
          .mapValues(_.toSet)
          .toMap

      val tasks = scriptToTargets.toSeq.map { case (script, targets) =>
        val deps = sourcegen.scriptProjectDeps.getOrElse(script, Set(script.project))
        SourcegenTask(script, targets, deps)
      }
      val extraScriptProjects = tasks.flatMap(_.scriptProjectDeps).toSet -- inScope
      (tasks, extraScriptProjects)
    }

  /** Build DAG for compile-only (no linking, no tests). */
  def buildCompileDag(projects: Set[CrossProjectName], ctx: BuildContext): Dag = {
    val targetTransitive = transitiveDependencies(projects, ctx.allProjectDeps)
    val (sourcegenTasks, extraScriptProjects) = sourcegenTasksAndScriptCompiles(targetTransitive, ctx.sourcegen)
    // Script projects' transitive compile tasks — we already have the dep closure from the plan, but they may themselves depend on others we haven't walked.
    val scriptTransitive = transitiveDependencies(extraScriptProjects, ctx.allProjectDeps)
    val allProjects = targetTransitive ++ scriptTransitive

    val compileTasks = allProjects.map { project =>
      val (projectDeps, deps) = compileDeps(project, ctx, allProjects)
      CompileTask(project, projectDeps, deps)
    }

    val apTasks = annotationProcessorTasks(allProjects, ctx.apPlan)
    val kspTasks = symbolProcessorTasks(allProjects, ctx.kspPlan, ctx.allProjectDeps)

    Dag.fromTasks(compileTasks.toSeq ++ sourcegenTasks ++ apTasks ++ kspTasks)
  }

  /** Build initial DAG for test execution.
    *
    * Creates CompileTask and DiscoverTask for each test project. For non-JVM platforms, adds LinkTask between compile and discover. TestSuiteTasks are added
    * dynamically after discovery completes.
    */
  def buildTestDag(testProjects: Set[CrossProjectName], ctx: BuildContext): Dag = {
    val targetTransitive = transitiveDependencies(testProjects, ctx.allProjectDeps)
    val (sourcegenTasks, extraScriptProjects) = sourcegenTasksAndScriptCompiles(targetTransitive, ctx.sourcegen)
    val scriptTransitive = transitiveDependencies(extraScriptProjects, ctx.allProjectDeps)
    val allProjects = targetTransitive ++ scriptTransitive

    val compileTasks = allProjects.map { project =>
      val (projectDeps, deps) = compileDeps(project, ctx, allProjects)
      CompileTask(project, projectDeps, deps)
    }

    val linkTasks = testProjects.flatMap { project =>
      ctx.platforms.get(project) match {
        case Some(LinkPlatform.Jvm) | None => None
        case Some(platform)                =>
          Some(LinkTask(project, platform, releaseMode = false, isTest = true))
      }
    }

    val discoverTasks = testProjects.map { project =>
      DiscoverTask(project, ctx.platforms.get(project))
    }

    val apTasks = annotationProcessorTasks(allProjects, ctx.apPlan)
    val kspTasks = symbolProcessorTasks(allProjects, ctx.kspPlan, ctx.allProjectDeps)

    Dag.fromTasks((compileTasks ++ linkTasks ++ discoverTasks).toSeq ++ sourcegenTasks ++ apTasks ++ kspTasks)
  }

  /** Build DAG for linking (compile + link without tests). */
  def buildLinkDag(projects: Set[CrossProjectName], ctx: BuildContext, releaseMode: Boolean): Dag = {
    val targetTransitive = transitiveDependencies(projects, ctx.allProjectDeps)
    val (sourcegenTasks, extraScriptProjects) = sourcegenTasksAndScriptCompiles(targetTransitive, ctx.sourcegen)
    val scriptTransitive = transitiveDependencies(extraScriptProjects, ctx.allProjectDeps)
    val allProjects = targetTransitive ++ scriptTransitive

    val compileTasks = allProjects.map { project =>
      val (projectDeps, deps) = compileDeps(project, ctx, allProjects)
      CompileTask(project, projectDeps, deps)
    }

    val linkTasks = projects.flatMap { project =>
      ctx.platforms.get(project) match {
        case Some(LinkPlatform.Jvm) | None => None
        case Some(platform)                =>
          Some(LinkTask(project, platform, releaseMode, isTest = false))
      }
    }

    val apTasks = annotationProcessorTasks(allProjects, ctx.apPlan)
    val kspTasks = symbolProcessorTasks(allProjects, ctx.kspPlan, ctx.allProjectDeps)

    Dag.fromTasks((compileTasks ++ linkTasks).toSeq ++ sourcegenTasks ++ apTasks ++ kspTasks)
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
      * @param machine
      *   The machine's resources. Admission happens against this, in priority order, so how much runs at once is what the machine can currently afford rather
      *   than a number carried alongside it.
      * @param eventQueue
      *   Queue for emitting events
      * @param killSignal
      *   Deferred that can be completed to kill all running tasks
      * @return
      *   The final DAG state
      */
    def execute(
        dag: Dag,
        machine: MachineResources,
        forkHeaps: ForkHeaps,
        eventQueue: Queue[IO, Option[DagEvent]],
        killSignal: Deferred[IO, KillReason]
    ): IO[Dag]
  }

  /** Per-task-type handler functions bundled into a single value. New task types are added as fields here rather than as new positional parameters cascading
    * through the callsites. Every field is required — there are no default no-op handlers (a no-op default is just a default parameter in disguise; the call
    * site should know which task types it expects to see).
    */
  case class Handlers(
      compile: (CompileTask, Deferred[IO, KillReason]) => IO[TaskResult],
      link: (LinkTask, Deferred[IO, KillReason]) => IO[(TaskResult, LinkResult)],
      discover: (DiscoverTask, Deferred[IO, KillReason]) => IO[(TaskResult, List[(String, String)])],
      test: (TestSuiteTask, Deferred[IO, KillReason]) => IO[TaskResult],
      sourcegen: (SourcegenTask, Deferred[IO, KillReason]) => IO[TaskResult],
      annotationProcessor: (ResolveAnnotationProcessorsTask, Deferred[IO, KillReason]) => IO[(TaskResult, Int)],
      symbolProcessor: (RunSymbolProcessorsTask, Deferred[IO, KillReason]) => IO[(TaskResult, Int)],

      /** Consulted at ADMISSION, before a compile is given a reservation: false means "not now, reconsider on the next wakeup".
        *
        * This is the one resource question the machine governor cannot answer for itself. CPU and fork memory are known before a task starts; heap pressure is
        * a property of the daemon's own heap right now, and the thing that relieves it is another task finishing. It belongs here rather than inside the
        * compile handler because a gate below admission holds a machine-wide CPU permit while it waits — starving tests and links that could have run.
        *
        * Callers with no opinion pass `_ => IO.pure(true)`. Explicitly, not by default: a no-op default is a default parameter in disguise.
        */
      mayAdmitCompile: CompileTask => IO[Boolean]
  )

  /** Create a DAG executor with the given handlers. */
  def executor(handlers: Handlers): DagExecutor = new DagExecutor {

    override def execute(
        initialDag: Dag,
        machine: MachineResources,
        forkHeaps: ForkHeaps,
        eventQueue: Queue[IO, Option[DagEvent]],
        killSignal: Deferred[IO, KillReason]
    ): IO[Dag] = {
      def now: IO[Long] = IO.realTime.map(_.toMillis)

      /** Take as many of `candidates` as the machine can currently afford, in the order given.
        *
        * `tryReserve` rather than `reserve`: a scheduler with a priority order wants "does this fit now", not "queue me". Anything refused stays ready and gets
        * another chance on the next wakeup, which fires whenever a task completes — precisely when resources free.
        *
        * `idle` is the forward-progress guarantee. With nothing running, nothing will ever complete to wake this loop, so a task that does not fit would hang
        * the build rather than merely wait. When the machine is idle the first candidate is admitted by blocking reservation instead, which clamps its request
        * to the machine's totals — so a task larger than the whole machine waits for the whole machine and then runs, rather than never running.
        */
      /** Ask whether a compile may start, BEFORE spending a reservation on it.
        *
        * Heap pressure used to be handled below admission: the compile was admitted, took a machine-wide CPU permit, and only then slept on the gate. That
        * withholds capacity from every other kind of work — a test or a link that could have run right now waits behind a permit held by something doing
        * nothing. Deferring here instead leaves the permit available, and the compile is reconsidered on the next wakeup, which fires when a task completes:
        * exactly when heap is most likely to have been freed.
        *
        * `idle` bypasses the gate for the same reason it bypasses `tryReserve` below — with nothing running, nothing will complete to reconsider this, so
        * deferring would stall the build rather than delay a start.
        */
      def mayAdmit(task: Task, idle: Boolean): IO[Boolean] =
        task match {
          case c: CompileTask if !idle => handlers.mayAdmitCompile(c)
          case _                       => IO.pure(true)
        }

      def reserveFor(task: Task): IO[Option[(Task, IO[Unit])]] = {
        val c = costOf(task, forkHeaps)
        machine.tryReserve(c.kind, task.id.toString, c.cpu, c.memoryMb).map(_.map(release => (task, release)))
      }

      def admit(candidates: List[Task], idle: Boolean): IO[List[(Task, IO[Unit])]] =
        candidates match {
          case Nil           => IO.pure(Nil)
          case first :: rest =>
            val firstCost = costOf(first, forkHeaps)
            val firstAdmission: IO[Option[(Task, IO[Unit])]] =
              if (idle)
                machine
                  .reserveUntilReleased(firstCost.kind, first.id.toString, firstCost.cpu, firstCost.memoryMb)
                  .map(release => Some((first, release)))
              else
                mayAdmit(first, idle).flatMap {
                  case true  => reserveFor(first)
                  case false => IO.pure(None)
                }

            firstAdmission.flatMap { headResult =>
              rest
                .traverse { task =>
                  // Never `idle` here: if the head was admitted, something is running by definition.
                  mayAdmit(task, idle = false).flatMap {
                    case true  => reserveFor(task)
                    case false => IO.pure(None)
                  }
                }
                .map(tail => (headResult :: tail).flatten)
            }
        }

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

      def executeTask(task: Task, dagRef: Ref[IO, Dag], taskKillSignals: Ref[IO, Map[TaskId, Deferred[IO, KillReason]]]): IO[Unit] = {
        val startTime = System.currentTimeMillis()

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

                  case lt: LinkTask =>
                    withRecovery(s"Link ${lt.project.value}", taskKill) {
                      for {
                        linkStartTs <- now
                        _ <- emit(DagEvent.LinkStarted(lt.project, lt.platform.name, linkStartTs))
                        (result, linkResult) <- handlers.link(lt, taskKill)
                        linkEndTs <- now
                        _ <- emit(DagEvent.LinkFinished(lt.project, linkResult, linkEndTs - linkStartTs, linkEndTs))
                        _ <- dagRef.update(_.recordLinkResult(lt.id, linkResult))
                      } yield result
                    }

                  case dt: DiscoverTask =>
                    withRecovery(s"Discover ${dt.project.value}", taskKill) {
                      for {
                        (result, suites) <- handlers.discover(dt, taskKill)
                        _ <- result match {
                          case TaskResult.Success =>
                            // Add test tasks for discovered suites
                            val testTasks = suites.map { case (suiteName, framework) =>
                              TestSuiteTask(dt.project, SuiteName(suiteName), framework)
                            }
                            dagRef.update(dag => testTasks.foldLeft(dag)(_.addTask(_))) >>
                              emit(DagEvent.SuitesDiscovered(dt.project, suites.map(s => SuiteName(s._1)), timestamp))
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
                    withRecovery(s"Test ${tt.suiteName.value}", taskKill)(handlers.test(tt, taskKill))

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
                      _ <- emit(DagEvent.SourcegenStarted(sgt.script.project, sgt.script.main, forProjectsList, sourcegenStartTs))
                      result <- withRecovery(s"Sourcegen ${sgt.script.main}", taskKill)(handlers.sourcegen(sgt, taskKill))
                      sourcegenEndTs <- now
                      (success, errorMsg) = resultSummary(result)
                      _ <- emit(
                        DagEvent.SourcegenFinished(sgt.script.project, sgt.script.main, success, sourcegenEndTs - sourcegenStartTs, errorMsg, sourcegenEndTs)
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
                      resultAndCount <- withRecoveryCounted(s"RunSymbolProcessors ${kspt.project.value}", taskKill)(handlers.symbolProcessor(kspt, taskKill))
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
          dag <- dagRef.get
          running <- runningRef.get
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
                // Get ready tasks (not already running), prioritized by dependents count
                readyTasks = dag.ready.filterNot(t => running.contains(t.id))
                depCounts = dag.dependentsCount
                // Most-unblocking first. Admission then happens HERE, against the machine, instead of
                // inside each task — so the ordering survives contention. Dispatching first and
                // reserving inside meant everything queued FIFO in the governor and this sort was
                // decoration.
                prioritized = readyTasks.toList.sortBy(t => -depCounts.getOrElse(t.id, 0))
                admitted <- admit(prioritized, idle = running.isEmpty)
                // Start tasks. The guarantee releases the reservation, cleans up runningRef and wakes
                // the loop — and the wakeup is what re-runs admission, so a completion is exactly when
                // the next task gets its chance.
                _ <- admitted.parTraverse_ { case (task, release) =>
                  runningRef.update(_ + task.id) >>
                    supervisor
                      .supervise(
                        executeTask(task, dagRef, taskKillSignals)
                          .guarantee(
                            release >> runningRef.update(_ - task.id) >> wakeup.tryOffer(()).void
                          )
                      )
                      .void
                }
                // Re-read state. If nothing is running, the DAG is either complete, in a
                // transient gap (skips just opened up new ready tasks), or genuinely stuck.
                newDag <- dagRef.get
                newRunning <- runningRef.get
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
                  } else if (newRunning.isEmpty) {
                    // No tasks running but progress still possible — re-evaluate without waiting.
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
          _ <- loop(dagRef, runningRef, taskKillSignals, wakeup, supervisor)
          finalDag <- dagRef.get
        } yield finalDag
      }
    }
  }
}
