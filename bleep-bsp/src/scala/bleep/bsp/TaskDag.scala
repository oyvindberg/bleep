package bleep.bsp

import bleep.bsp.Outcome.KillReason
import bleep.bsp.protocol.{BleepBspProtocol, TestStatus}
import bleep.bsp.protocol.BleepBspProtocol.BuildMode
import bleep.model.CrossProjectName
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
    case class Test(project: CrossProjectName, suiteName: String) extends TaskId {
      val value: String = s"test:${project.value}:$suiteName"
    }
  }

  /** A task in the DAG */
  sealed trait Task {
    def id: TaskId
    def project: CrossProjectName
    def dependencies: Set[TaskId]
  }

  /** Compile a project */
  case class CompileTask(
      project: CrossProjectName,
      projectDependencies: Set[CrossProjectName]
  ) extends Task {
    val id: TaskId = TaskId.Compile(project)
    val dependencies: Set[TaskId] = projectDependencies.map(p => TaskId.Compile(p))
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
  sealed trait LinkPlatform
  object LinkPlatform {
    case class ScalaJs(
        version: String,
        scalaVersion: String,
        config: bleep.analysis.ScalaJsLinkConfig
    ) extends LinkPlatform

    case class ScalaNative(
        version: String,
        scalaVersion: String,
        config: bleep.analysis.ScalaNativeLinkConfig
    ) extends LinkPlatform

    case class KotlinJs(
        version: String,
        config: KotlinJsConfig
    ) extends LinkPlatform

    case class KotlinNative(
        version: String,
        config: KotlinNativeConfig
    ) extends LinkPlatform

    /** JVM platform - linking is a no-op */
    case object Jvm extends LinkPlatform
  }

  /** Kotlin/JS configuration */
  case class KotlinJsConfig(
      moduleKind: String,
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
      suiteName: String,
      framework: String
  ) extends Task {
    val id: TaskId = TaskId.Test(project, suiteName)
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
    case class Error(error: String, exitCode: Option[Int], signal: Option[Int]) extends TaskResult
    case class Skipped(failedDependency: Task) extends TaskResult
    case class Killed(reason: KillReason) extends TaskResult
    case object TimedOut extends TaskResult

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
    case class LinkStarted(project: String, platform: String, timestamp: Long) extends DagEvent
    case class LinkProgress(project: String, phase: String, percent: Int, timestamp: Long) extends DagEvent
    case class LinkFinished(
        project: String,
        result: LinkResult,
        durationMs: Long,
        timestamp: Long
    ) extends DagEvent

    // Test-specific events (nested within TestSuiteTask execution)
    case class TestStarted(project: String, suite: String, test: String, timestamp: Long) extends DagEvent
    case class TestFinished(
        project: String,
        suite: String,
        test: String,
        status: TestStatus,
        durationMs: Long,
        message: Option[String],
        throwable: Option[String],
        timestamp: Long
    ) extends DagEvent

    // Discovery events
    case class SuitesDiscovered(project: String, suites: List[String], timestamp: Long) extends DagEvent

    // Output events
    case class Output(project: String, suite: String, line: String, isError: Boolean, timestamp: Long) extends DagEvent

    // Suite completion with counts
    case class SuiteFinished(
        project: String,
        suite: String,
        passed: Int,
        failed: Int,
        skipped: Int,
        ignored: Int,
        durationMs: Long,
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

  /** Build DAG based on build mode.
    *
    * @param projects
    *   Target projects for the operation
    * @param allProjectDeps
    *   Map of project -> direct dependencies
    * @param platforms
    *   Map of project -> link platform (for non-JVM)
    * @param mode
    *   The build mode (Compile, Link, Test, Run)
    * @return
    *   A DAG with appropriate tasks for the mode
    */
  def buildDag(
      projects: Set[CrossProjectName],
      allProjectDeps: Map[CrossProjectName, Set[CrossProjectName]],
      platforms: Map[CrossProjectName, LinkPlatform],
      mode: BuildMode
  ): Dag = mode match {
    case BuildMode.Compile =>
      buildCompileDag(projects, allProjectDeps)
    case BuildMode.Link(releaseMode) =>
      buildLinkDag(projects, allProjectDeps, platforms, releaseMode)
    case BuildMode.Test =>
      buildTestDag(projects, allProjectDeps, platforms)
    case BuildMode.Run(_, _) =>
      // Run mode is similar to link mode - compile and optionally link
      buildLinkDag(projects, allProjectDeps, platforms, releaseMode = false)
  }

  /** Build DAG for compile-only (no linking, no tests). */
  def buildCompileDag(
      projects: Set[CrossProjectName],
      allProjectDeps: Map[CrossProjectName, Set[CrossProjectName]]
  ): Dag = {
    // Get transitive dependencies
    val allProjects = transitiveDependencies(projects, allProjectDeps)

    // Create compile tasks only
    val compileTasks = allProjects.map { project =>
      val deps = allProjectDeps.getOrElse(project, Set.empty).filter(allProjects.contains)
      CompileTask(project, deps)
    }

    Dag.fromTasks(compileTasks.toSeq)
  }

  /** Build initial DAG for test execution.
    *
    * Creates CompileTask and DiscoverTask for each test project. For non-JVM platforms, adds LinkTask between compile and discover. TestSuiteTasks are added
    * dynamically after discovery completes.
    */
  def buildTestDag(
      testProjects: Set[CrossProjectName],
      allProjectDeps: Map[CrossProjectName, Set[CrossProjectName]],
      platforms: Map[CrossProjectName, LinkPlatform]
  ): Dag = {
    // Get transitive dependencies for all test projects
    val allProjects = transitiveDependencies(testProjects, allProjectDeps)

    // Create compile tasks for all projects (dependencies + test projects)
    val compileTasks = allProjects.map { project =>
      val deps = allProjectDeps.getOrElse(project, Set.empty).filter(allProjects.contains)
      CompileTask(project, deps)
    }

    // Create link tasks for non-JVM test projects
    val linkTasks = testProjects.flatMap { project =>
      platforms.get(project) match {
        case Some(LinkPlatform.Jvm) | None => None
        case Some(platform) =>
          Some(LinkTask(project, platform, releaseMode = false, isTest = true))
      }
    }

    // Create discover tasks for test projects (with platform info for dependency tracking)
    val discoverTasks = testProjects.map { project =>
      DiscoverTask(project, platforms.get(project))
    }

    Dag.fromTasks((compileTasks ++ linkTasks ++ discoverTasks).toSeq)
  }

  /** Build initial DAG for test execution (JVM-only version for backward compatibility). */
  def buildTestDag(
      testProjects: Set[CrossProjectName],
      allProjectDeps: Map[CrossProjectName, Set[CrossProjectName]]
  ): Dag =
    buildTestDag(testProjects, allProjectDeps, Map.empty)

  /** Build DAG for linking (compile + link without tests). */
  def buildLinkDag(
      projects: Set[CrossProjectName],
      allProjectDeps: Map[CrossProjectName, Set[CrossProjectName]],
      platforms: Map[CrossProjectName, LinkPlatform],
      releaseMode: Boolean
  ): Dag = {
    // Get transitive dependencies
    val allProjects = transitiveDependencies(projects, allProjectDeps)

    // Create compile tasks
    val compileTasks = allProjects.map { project =>
      val deps = allProjectDeps.getOrElse(project, Set.empty).filter(allProjects.contains)
      CompileTask(project, deps)
    }

    // Create link tasks for target projects
    val linkTasks = projects.flatMap { project =>
      platforms.get(project) match {
        case Some(LinkPlatform.Jvm) | None => None
        case Some(platform) =>
          Some(LinkTask(project, platform, releaseMode, isTest = false))
      }
    }

    Dag.fromTasks((compileTasks ++ linkTasks).toSeq)
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
      * @param maxParallelism
      *   Maximum concurrent tasks
      * @param eventQueue
      *   Queue for emitting events
      * @param killSignal
      *   Deferred that can be completed to kill all running tasks
      * @return
      *   The final DAG state
      */
    def execute(
        dag: Dag,
        maxParallelism: Int,
        eventQueue: Queue[IO, Option[DagEvent]],
        killSignal: Deferred[IO, KillReason]
    ): IO[Dag]
  }

  /** Create a DAG executor with task handlers */
  def executor(
      compileHandler: (CompileTask, Deferred[IO, KillReason]) => IO[TaskResult],
      discoverHandler: (DiscoverTask, Deferred[IO, KillReason]) => IO[(TaskResult, List[(String, String)])],
      testHandler: (TestSuiteTask, Deferred[IO, KillReason]) => IO[TaskResult]
  ): DagExecutor = executor(
    compileHandler,
    (_, _) => IO.pure((TaskResult.Success, LinkResult.NotApplicable)),
    discoverHandler,
    testHandler
  )

  /** Backward compatibility: Create a DAG executor from handlers that don't use kill signal */
  def executorLegacy(
      compileHandler: CompileTask => IO[TaskResult],
      discoverHandler: DiscoverTask => IO[(TaskResult, List[(String, String)])],
      testHandler: TestSuiteTask => IO[TaskResult]
  ): DagExecutor = executor(
    (task, _) => compileHandler(task),
    (task, _) => discoverHandler(task),
    (task, _) => testHandler(task)
  )

  /** Create a DAG executor with task handlers including link support */
  def executor(
      compileHandler: (CompileTask, Deferred[IO, KillReason]) => IO[TaskResult],
      linkHandler: (LinkTask, Deferred[IO, KillReason]) => IO[(TaskResult, LinkResult)],
      discoverHandler: (DiscoverTask, Deferred[IO, KillReason]) => IO[(TaskResult, List[(String, String)])],
      testHandler: (TestSuiteTask, Deferred[IO, KillReason]) => IO[TaskResult]
  ): DagExecutor = new DagExecutor {

    override def execute(
        initialDag: Dag,
        maxParallelism: Int,
        eventQueue: Queue[IO, Option[DagEvent]],
        killSignal: Deferred[IO, KillReason]
    ): IO[Dag] = {
      def now: IO[Long] = IO.realTime.map(_.toMillis)

      def emit(event: DagEvent): IO[Unit] = eventQueue.offer(Some(event))

      /** Check if kill has been requested (non-blocking) */
      def isKilled: IO[Option[KillReason]] = killSignal.tryGet

      def executeTask(task: Task, dagRef: Ref[IO, Dag], taskKillSignals: Ref[IO, Map[TaskId, Deferred[IO, KillReason]]]): IO[Unit] = {
        val startTime = System.currentTimeMillis()

        // Create a per-task kill signal that can be completed either by the global signal or individually
        def createTaskKillSignal: IO[Deferred[IO, KillReason]] =
          for {
            taskKill <- Deferred[IO, KillReason]
            // Register this task's kill signal
            _ <- taskKillSignals.update(_ + (task.id -> taskKill))
            // Set up propagation from global kill signal to this task's signal
            _ <- killSignal.get.flatMap(reason => taskKill.complete(reason).attempt.void).start
          } yield taskKill

        // Helper to convert ALL outcomes (success, error, killed) to TaskResult
        // Uses fiber.join to get Outcome, then embed to convert back to IO with kill fallback
        def withRecovery(taskName: String, taskKill: Deferred[IO, KillReason])(io: IO[TaskResult]): IO[TaskResult] =
          io.start
            .flatMap(_.join)
            .flatMap { outcome =>
              outcome.embed(
                onCancel = taskKill.tryGet.map {
                  case Some(reason) => TaskResult.Killed(reason)
                  case None         => TaskResult.Killed(KillReason.UserRequest) // Fallback
                }
              )
            }
            .handleErrorWith { error =>
              val errorMsg = s"$taskName failed: ${error.getClass.getName}: ${error.getMessage}"
              // Build a detailed diagnostic with cause chain and brief stack trace
              val lines = new scala.collection.mutable.ArrayBuffer[String]()
              lines += errorMsg
              var cause = error.getCause
              while (cause != null) {
                lines += s"  Caused by: ${cause.getClass.getName}: ${cause.getMessage}"
                cause = cause.getCause
              }
              // Include first few relevant stack frames (skip internal framework frames)
              val frames = error.getStackTrace.take(10)
              if (frames.nonEmpty) {
                lines += "  Stack trace:"
                frames.foreach(f => lines += s"    at $f")
              }
              val diagMessage = lines.mkString("\n")
              IO.pure(
                TaskResult.Failure(
                  error = errorMsg,
                  diagnostics = List(BleepBspProtocol.Diagnostic.error(diagMessage))
                )
              )
            }

        for {
          maybeKilled <- isKilled
          timestamp <- now
          _ <- emit(DagEvent.TaskStarted(task, timestamp))
          result <- maybeKilled match {
            case Some(reason) =>
              // Task was killed before it started
              IO.pure(TaskResult.Killed(reason))
            case None =>
              for {
                taskKill <- createTaskKillSignal
                result <- task match {
                  case ct: CompileTask =>
                    withRecovery(s"Compile ${ct.project.value}", taskKill)(compileHandler(ct, taskKill))

                  case lt: LinkTask =>
                    withRecovery(s"Link ${lt.project.value}", taskKill) {
                      for {
                        linkStartTs <- now
                        platformName = lt.platform match {
                          case _: LinkPlatform.ScalaJs      => "Scala.js"
                          case _: LinkPlatform.ScalaNative  => "Scala Native"
                          case _: LinkPlatform.KotlinJs     => "Kotlin/JS"
                          case _: LinkPlatform.KotlinNative => "Kotlin/Native"
                          case LinkPlatform.Jvm             => "JVM"
                        }
                        _ <- emit(DagEvent.LinkStarted(lt.project.value, platformName, linkStartTs))
                        (result, linkResult) <- linkHandler(lt, taskKill)
                        linkEndTs <- now
                        _ <- emit(DagEvent.LinkFinished(lt.project.value, linkResult, linkEndTs - linkStartTs, linkEndTs))
                        _ <- dagRef.update(_.recordLinkResult(lt.id, linkResult))
                      } yield result
                    }

                  case dt: DiscoverTask =>
                    withRecovery(s"Discover ${dt.project.value}", taskKill) {
                      for {
                        (result, suites) <- discoverHandler(dt, taskKill)
                        _ <- result match {
                          case TaskResult.Success =>
                            // Add test tasks for discovered suites
                            val testTasks = suites.map { case (suiteName, framework) =>
                              TestSuiteTask(dt.project, suiteName, framework)
                            }
                            dagRef.update(dag => testTasks.foldLeft(dag)(_.addTask(_))) >>
                              emit(DagEvent.SuitesDiscovered(dt.project.value, suites.map(_._1), timestamp))
                          case _ => IO.unit
                        }
                      } yield result
                    }

                  case tt: TestSuiteTask =>
                    // Make test execution uncancelable so it always completes and reports status.
                    // Without this, fiber cancellation could abort mid-test and leave no status event.
                    IO.uncancelable { _ =>
                      withRecovery(s"Test ${tt.suiteName}", taskKill)(testHandler(tt, taskKill))
                    }
                }
                // Unregister this task's kill signal
                _ <- taskKillSignals.update(_ - task.id)
              } yield result
          }
          endTimestamp <- now
          durationMs = endTimestamp - startTime
          _ <- emit(DagEvent.TaskFinished(task, result, durationMs, endTimestamp))
          _ <- result match {
            case TaskResult.Success        => dagRef.update(_.complete(task.id))
            case TaskResult.Failure(_, _)  => dagRef.update(_.fail(task.id))
            case TaskResult.Error(_, _, _) => dagRef.update(_.error(task.id))
            case TaskResult.Skipped(_)     => dagRef.update(_.skip(task.id))
            case TaskResult.Killed(_)      => dagRef.update(_.kill(task.id))
            case TaskResult.TimedOut       => dagRef.update(_.timeout(task.id))
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

      // Use Deferred signaling instead of polling for task completion.
      // The signalRef holds the current Deferred so running tasks can always signal the latest one.
      def loop(
          dagRef: Ref[IO, Dag],
          runningRef: Ref[IO, Set[TaskId]],
          taskKillSignals: Ref[IO, Map[TaskId, Deferred[IO, KillReason]]],
          signalRef: Ref[IO, Deferred[IO, Unit]]
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
              // Kill requested but tasks still running - wait for signal
              IO(System.err.println(s"[DAG] Kill requested (${maybeKilled.get}), waiting for ${running.size} running tasks: ${running.mkString(", ")}")) >>
                signalRef.get.flatMap(_.get) >>
                Deferred[IO, Unit].flatMap { newSignal =>
                  signalRef.set(newSignal) >> loop(dagRef, runningRef, taskKillSignals, signalRef)
                }
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
                availableSlots = maxParallelism - running.size
                tasksToStart = readyTasks.toList.sortBy(t => -depCounts.getOrElse(t.id, 0)).take(availableSlots)
                // Start tasks - they will signal completion via the signalRef
                _ <- tasksToStart.toList.parTraverse_ { task =>
                  runningRef.update(_ + task.id) >>
                    executeTask(task, dagRef, taskKillSignals)
                      .guarantee(
                        runningRef.update(_ - task.id) >>
                          // Signal the CURRENT deferred (read from ref to avoid stale reference)
                          signalRef.get.flatMap(_.complete(()).attempt.void)
                      )
                      .start
                      .void
                }
                // If no tasks are running, we're done or in error state
                newRunning <- runningRef.get
                _ <-
                  if (newRunning.isEmpty) {
                    // Nothing running - check if we're complete or stuck
                    dagRef.get.flatMap { newDag =>
                      if (newDag.isComplete) IO.unit
                      else {
                        // Check if we're actually stuck: no ready tasks and nothing to skip
                        val stillReady = newDag.ready
                        val stillToSkip = newDag.toSkip
                        if (stillReady.isEmpty && stillToSkip.isEmpty) {
                          // Deadlock: nothing running, nothing ready, nothing to skip, but DAG not complete
                          val remaining = newDag.tasks.keySet -- newDag.finished
                          IO.raiseError(
                            new RuntimeException(
                              s"DAG deadlock: ${remaining.size} tasks stuck with unsatisfied dependencies: ${remaining.mkString(", ")}"
                            )
                          )
                        } else {
                          // Some tasks became ready (e.g., from skipping) — retry
                          Deferred[IO, Unit].flatMap { newSignal =>
                            signalRef.set(newSignal) >> loop(dagRef, runningRef, taskKillSignals, signalRef)
                          }
                        }
                      }
                    }
                  } else {
                    // Wait for any task to complete, then create fresh signal and continue
                    signalRef.get.flatMap(_.get) >>
                      Deferred[IO, Unit].flatMap { newSignal =>
                        signalRef.set(newSignal) >> loop(dagRef, runningRef, taskKillSignals, signalRef)
                      }
                  }
              } yield ()
            }
        } yield ()

      for {
        dagRef <- Ref.of[IO, Dag](initialDag)
        runningRef <- Ref.of[IO, Set[TaskId]](Set.empty)
        taskKillSignals <- Ref.of[IO, Map[TaskId, Deferred[IO, KillReason]]](Map.empty)
        initialSignal <- Deferred[IO, Unit]
        signalRef <- Ref.of[IO, Deferred[IO, Unit]](initialSignal)
        _ <- loop(dagRef, runningRef, taskKillSignals, signalRef)
        finalDag <- dagRef.get
      } yield finalDag
    }
  }
}
