package bleep.testing

import bleep.model

/** Wrapper types for type-safe data flow */
object TestTypes {

  /** Fully qualified class name of a test suite */
  case class SuiteClassName(value: String) extends AnyVal {
    override def toString: String = value
    def shortName: String = value.split('.').lastOption.getOrElse(value)
  }

  /** Name of an individual test within a suite */
  case class TestName(value: String) extends AnyVal {
    override def toString: String = value
  }

  /** Key for identifying a running suite (project + suite) */
  case class SuiteKey(project: model.CrossProjectName, suite: SuiteClassName) {
    def asString: String = s"${project.value}:${suite.value}"
  }

  /** Key for identifying a running test (project + suite + test) */
  case class TestKey(project: model.CrossProjectName, suite: SuiteClassName, test: TestName) {
    def asString: String = s"${project.value}:${suite.value}:${test.value}"
    def suiteKey: SuiteKey = SuiteKey(project, suite)
  }
}

import TestTypes._

/** State of a single project in the test execution pipeline */
sealed trait ProjectState {
  def project: model.CrossProjectName
  def isTestProject: Boolean

  /** Whether this project has completed (either success or failure) */
  def isCompleted: Boolean = this match {
    case _: ProjectState.CompileFailed  => true
    case _: ProjectState.Skipped        => true
    case _: ProjectState.TestsCompleted => true
    case _                              => false
  }

  /** Whether tests can be run for this project */
  def canRunTests: Boolean = this match {
    case _: ProjectState.SuitesDiscovered => true
    case _: ProjectState.TestsInProgress  => true
    case _                                => false
  }
}

object ProjectState {

  /** Initial state - project has not been processed yet */
  case class Initial(
      project: model.CrossProjectName,
      isTestProject: Boolean
  ) extends ProjectState

  /** Project is currently compiling */
  case class Compiling(
      project: model.CrossProjectName,
      isTestProject: Boolean,
      startedAt: Long,
      diagnostics: List[CompileDiagnostic],
      progressPercent: Option[Int] = None
  ) extends ProjectState {
    def addDiagnostic(d: CompileDiagnostic): Compiling =
      copy(diagnostics = diagnostics :+ d)
    def updateProgress(percent: Int): Compiling =
      copy(progressPercent = Some(percent))
  }

  /** Compilation failed */
  case class CompileFailed(
      project: model.CrossProjectName,
      isTestProject: Boolean,
      errors: List[String],
      durationMs: Long
  ) extends ProjectState

  /** Project was skipped because a dependency failed */
  case class Skipped(
      project: model.CrossProjectName,
      isTestProject: Boolean,
      reason: SkipReason
  ) extends ProjectState

  /** Compilation succeeded but tests not yet discovered */
  case class CompileSucceeded(
      project: model.CrossProjectName,
      isTestProject: Boolean,
      durationMs: Long
  ) extends ProjectState

  /** Discovering test suites in the project */
  case class DiscoveringSuites(
      project: model.CrossProjectName,
      isTestProject: Boolean
  ) extends ProjectState

  /** Test suites have been discovered */
  case class SuitesDiscovered(
      project: model.CrossProjectName,
      isTestProject: Boolean,
      suites: List[SuiteClassName]
  ) extends ProjectState {
    def suiteCount: Int = suites.size
  }

  /** Info about a running suite */
  case class RunningSuiteInfo(
      suite: SuiteClassName,
      jvmPid: Long,
      startedAt: Long
  )

  /** How tests are executed for a project - sum type for different platforms */
  sealed trait TestExecution {
    def isComplete: Boolean
    def hasFailed: Boolean
  }

  object TestExecution {

    /** JVM tests - granular suite/test tracking via reactive runner */
    case class Reactive(
        suites: List[SuiteClassName],
        runningSuites: Map[SuiteClassName, RunningSuiteInfo],
        completedSuites: Map[SuiteClassName, SuiteResult],
        runningTests: Map[TestKey, RunningTestInfo]
    ) extends TestExecution {
      def suiteCount: Int = suites.size
      def suitesCompleted: Int = completedSuites.size
      def suitesRunning: Int = runningSuites.size
      def failedSoFar: Int = completedSuites.values.map(_.failed).sum
      def activeJvmPids: Set[Long] = runningSuites.values.map(_.jvmPid).toSet

      def startSuite(suite: SuiteClassName, jvmPid: Long, timestamp: Long): Reactive =
        copy(runningSuites = runningSuites + (suite -> RunningSuiteInfo(suite, jvmPid, timestamp)))

      def completeSuite(suite: SuiteClassName, result: SuiteResult): Reactive =
        copy(
          runningSuites = runningSuites - suite,
          completedSuites = completedSuites + (suite -> result)
        )

      def startTest(key: TestKey, info: RunningTestInfo): Reactive =
        copy(runningTests = runningTests + (key -> info))

      def completeTest(key: TestKey): Reactive =
        copy(runningTests = runningTests - key)

      def isComplete: Boolean =
        completedSuites.size == suites.size && runningSuites.isEmpty

      def hasFailed: Boolean = completedSuites.values.exists(_.failed > 0)

      def totalPassed: Int = completedSuites.values.map(_.passed).sum
      def totalFailed: Int = completedSuites.values.map(_.failed).sum
      def totalSkipped: Int = completedSuites.values.map(_.skipped).sum
      def totalIgnored: Int = completedSuites.values.map(_.ignored).sum
    }

    object Reactive {
      def initial(suites: List[SuiteClassName]): Reactive =
        Reactive(suites, Map.empty, Map.empty, Map.empty)
    }

    /** Non-JVM tests (JS/Native) - project-level execution via BSP */
    case class Bsp(
        platform: model.PlatformId,
        status: BspStatus
    ) extends TestExecution {
      def isComplete: Boolean = status.isInstanceOf[BspStatus.Completed]
      def hasFailed: Boolean = status match {
        case BspStatus.Completed(success, _, _) => !success
        case _                                  => false
      }
    }

    object Bsp {
      def running(platform: model.PlatformId, startedAt: Long): Bsp =
        Bsp(platform, BspStatus.Running(startedAt))
    }

    /** Status of BSP test execution */
    sealed trait BspStatus
    object BspStatus {
      case class Running(startedAt: Long) extends BspStatus
      case class Completed(success: Boolean, durationMs: Long, output: List[String]) extends BspStatus
    }
  }

  /** Tests are in progress (running or partially complete) */
  case class TestsInProgress(
      project: model.CrossProjectName,
      isTestProject: Boolean,
      execution: TestExecution
  ) extends ProjectState {
    def isComplete: Boolean = execution.isComplete

    def suiteCount: Int = execution match {
      case r: TestExecution.Reactive => r.suiteCount
      case _: TestExecution.Bsp      => 1 // BSP is project-level
    }

    def suitesCompleted: Int = execution match {
      case r: TestExecution.Reactive => r.suitesCompleted
      case b: TestExecution.Bsp      => if (b.isComplete) 1 else 0
    }

    def failedSoFar: Int = execution match {
      case r: TestExecution.Reactive => r.failedSoFar
      case b: TestExecution.Bsp      => if (b.hasFailed) 1 else 0
    }
  }

  /** All tests have completed */
  case class TestsCompleted(
      project: model.CrossProjectName,
      isTestProject: Boolean,
      execution: TestExecution,
      durationMs: Long
  ) extends ProjectState {
    def hasFailed: Boolean = execution.hasFailed

    // Convenience accessors for reactive mode
    def reactiveResults: Option[Map[SuiteClassName, SuiteResult]] = execution match {
      case r: TestExecution.Reactive => Some(r.completedSuites)
      case _                         => None
    }

    def totalPassed: Int = execution match {
      case r: TestExecution.Reactive => r.totalPassed
      case _                         => 0
    }
    def totalFailed: Int = execution match {
      case r: TestExecution.Reactive => r.totalFailed
      case b: TestExecution.Bsp      => if (b.hasFailed) 1 else 0
    }
    def totalSkipped: Int = execution match {
      case r: TestExecution.Reactive => r.totalSkipped
      case _                         => 0
    }
    def totalIgnored: Int = execution match {
      case r: TestExecution.Reactive => r.totalIgnored
      case _                         => 0
    }
    def suiteCount: Int = execution match {
      case r: TestExecution.Reactive => r.suiteCount
      case _: TestExecution.Bsp      => 1 // BSP is project-level, count as 1
    }
  }
}

/** Reason why a project was skipped */
sealed trait SkipReason {
  def description: String
}

object SkipReason {
  case class DependencyFailed(failedProject: model.CrossProjectName) extends SkipReason {
    def description: String = s"Dependency ${failedProject.value} failed to compile"
  }
}

/** A compile diagnostic (error or warning) */
case class CompileDiagnostic(
    file: String,
    line: Int,
    message: String,
    severity: DiagnosticSeverity
)

sealed trait DiagnosticSeverity
object DiagnosticSeverity {
  case object Error extends DiagnosticSeverity
  case object Warning extends DiagnosticSeverity
  case object Info extends DiagnosticSeverity
}

/** Info about a currently running test */
case class RunningTestInfo(
    suite: SuiteClassName,
    test: TestName,
    startedAt: Long
)

/** Result of a completed test suite */
case class SuiteResult(
    suite: SuiteClassName,
    passed: Int,
    failed: Int,
    skipped: Int,
    ignored: Int,
    durationMs: Long,
    failures: List[TestFailureInfo]
) {
  def total: Int = passed + failed + skipped + ignored
  def hasFailed: Boolean = failed > 0
}

/** Information about a failed test */
case class TestFailureInfo(
    test: TestName,
    message: Option[String],
    throwable: Option[String]
)

/** Actions that can be applied to project state */
sealed trait ProjectAction {
  def project: model.CrossProjectName
}

object ProjectAction {
  case class StartCompile(project: model.CrossProjectName, timestamp: Long) extends ProjectAction
  case class UpdateCompileProgress(project: model.CrossProjectName, percent: Int) extends ProjectAction
  case class AddDiagnostic(project: model.CrossProjectName, diagnostic: CompileDiagnostic) extends ProjectAction
  case class FinishCompile(project: model.CrossProjectName, success: Boolean, errors: List[String], durationMs: Long) extends ProjectAction
  case class SkipProject(project: model.CrossProjectName, reason: SkipReason) extends ProjectAction
  case class StartDiscovery(project: model.CrossProjectName) extends ProjectAction
  case class FinishDiscovery(project: model.CrossProjectName, suites: List[SuiteClassName]) extends ProjectAction

  // Reactive (JVM) test actions
  case class StartSuite(project: model.CrossProjectName, suite: SuiteClassName, jvmPid: Long, timestamp: Long) extends ProjectAction
  case class FinishSuite(project: model.CrossProjectName, suite: SuiteClassName, result: SuiteResult) extends ProjectAction
  case class StartTest(project: model.CrossProjectName, suite: SuiteClassName, test: TestName, timestamp: Long) extends ProjectAction
  case class FinishTest(
      project: model.CrossProjectName,
      suite: SuiteClassName,
      test: TestName,
      status: TestStatus,
      durationMs: Long,
      message: Option[String],
      throwable: Option[String]
  ) extends ProjectAction

  // BSP (JS/Native) test actions
  case class StartBspTests(project: model.CrossProjectName, platform: model.PlatformId, timestamp: Long) extends ProjectAction
  case class FinishBspTests(project: model.CrossProjectName, success: Boolean, durationMs: Long, output: List[String]) extends ProjectAction
}

/** Overall state of the test run */
case class TestRunState(
    projects: Map[model.CrossProjectName, ProjectState],
    testProjects: Set[model.CrossProjectName],
    graph: ProjectGraph,
    startTime: Long
) {

  /** Reduce an action to produce new state. Returns (newState, skippedDownstream) where skippedDownstream is the set of projects that were marked as Skipped
    * due to a compile failure cascade.
    */
  def reduce(action: ProjectAction): (TestRunState, Set[model.CrossProjectName]) = {
    val currentState = projects.getOrElse(action.project, ProjectState.Initial(action.project, testProjects.contains(action.project)))

    // If project is already in a final state, log and ignore the action
    if (currentState.isCompleted) {
      System.err.println(
        s"[WARN] Action ${action.getClass.getSimpleName} received for project ${action.project.value} already in final state ${currentState.getClass.getSimpleName}"
      )
      (this, Set.empty)
    } else {
      val newState = ProjectStateReducer.reduce(currentState, action)
      val updatedProjects = projects + (action.project -> newState)

      // If this action resulted in a CompileFailed, cascade skip to downstream projects
      newState match {
        case _: ProjectState.CompileFailed =>
          val downstream = graph.transitiveDownstream(action.project)
          val skippedProjects = downstream.foldLeft(updatedProjects) { (projs, downstreamProject) =>
            val downstreamState = projs.getOrElse(downstreamProject, ProjectState.Initial(downstreamProject, testProjects.contains(downstreamProject)))
            // Only skip if not already in a final state
            if (!downstreamState.isCompleted) {
              projs + (downstreamProject -> ProjectState.Skipped(
                downstreamProject,
                testProjects.contains(downstreamProject),
                SkipReason.DependencyFailed(action.project)
              ))
            } else {
              projs
            }
          }
          (copy(projects = skippedProjects), downstream)
        case _ =>
          (copy(projects = updatedProjects), Set.empty)
      }
    }
  }

  /** Convenience method that discards the skipped set */
  def reduceSimple(action: ProjectAction): TestRunState = reduce(action)._1

  /** Check if a project has been queued (is no longer in Initial state) */
  def isQueued(project: model.CrossProjectName): Boolean =
    projects.get(project).exists {
      case _: ProjectState.Initial => false
      case _                       => true
    }

  /** Check if a project has failed to compile */
  def hasFailed(project: model.CrossProjectName): Boolean =
    projects.get(project).exists(_.isInstanceOf[ProjectState.CompileFailed])

  /** Count of test projects that have been processed (not in Initial state) */
  def testProjectsProcessed: Int =
    testProjects.count(p => isQueued(p) && projects.get(p).exists(_.isCompleted))

  // Derived state - lists
  def projectsCompiling: List[model.CrossProjectName] =
    projects.collect { case (p, _: ProjectState.Compiling) => p }.toList

  def projectsCompileFailed: Map[model.CrossProjectName, List[String]] =
    projects.collect { case (p, s: ProjectState.CompileFailed) => p -> s.errors }

  def projectsCompileFailedNames: Set[String] =
    projects.collect { case (p, _: ProjectState.CompileFailed) => p.value }.toSet

  def projectsSkipped: Map[model.CrossProjectName, SkipReason] =
    projects.collect { case (p, s: ProjectState.Skipped) => p -> s.reason }

  def projectsCompileSucceeded: List[model.CrossProjectName] =
    projects.collect { case (p, _: ProjectState.CompileSucceeded) => p }.toList

  def projectsDiscoveringSuites: List[model.CrossProjectName] =
    projects.collect { case (p, _: ProjectState.DiscoveringSuites) => p }.toList

  def projectsWithSuitesDiscovered: Map[model.CrossProjectName, List[SuiteClassName]] =
    projects.collect { case (p, s: ProjectState.SuitesDiscovered) => p -> s.suites }

  def projectsRunningTests: List[model.CrossProjectName] =
    projects.collect { case (p, _: ProjectState.TestsInProgress) => p }.toList

  def projectsTestsCompleted: Map[model.CrossProjectName, ProjectState.TestsCompleted] =
    projects.collect { case (p, s: ProjectState.TestsCompleted) => p -> s }

  // Derived state - counts
  def totalSuitesDiscovered: Int =
    projects.values.collect {
      case s: ProjectState.SuitesDiscovered => s.suiteCount
      case s: ProjectState.TestsInProgress  => s.suiteCount
      case s: ProjectState.TestsCompleted   => s.suiteCount
    }.sum

  def totalSuitesCompleted: Int =
    projects.values.collect {
      case s: ProjectState.TestsInProgress => s.suitesCompleted
      case s: ProjectState.TestsCompleted  => s.suiteCount
    }.sum

  def totalTestsPassed: Int =
    projects.values.collect { case s: ProjectState.TestsCompleted => s.totalPassed }.sum

  def totalTestsFailed: Int =
    projects.values.collect { case s: ProjectState.TestsCompleted => s.totalFailed }.sum

  def totalTestsSkipped: Int =
    projects.values.collect { case s: ProjectState.TestsCompleted => s.totalSkipped }.sum

  def totalTestsIgnored: Int =
    projects.values.collect { case s: ProjectState.TestsCompleted => s.totalIgnored }.sum

  def suitesFailed: Int =
    projects.values.collect { case s: ProjectState.TestsCompleted =>
      s.reactiveResults.map(_.count(_._2.hasFailed)).getOrElse(if (s.hasFailed) 1 else 0)
    }.sum

  /** Number of projects currently compiling */
  def activelyCompilingCount: Int =
    projects.values.count(_.isInstanceOf[ProjectState.Compiling])

  /** Number of projects currently running tests */
  def activelyTestingCount: Int =
    projects.values.count(_.isInstanceOf[ProjectState.TestsInProgress])

  /** All test projects have reached a terminal state */
  def allTestsCompleted: Boolean =
    testProjects.forall { p =>
      projects.get(p).exists(_.isCompleted)
    }

  /** Get all failures for summary display */
  def allFailures: List[(model.CrossProjectName, SuiteClassName, TestFailureInfo)] =
    projects.values
      .collect { case s: ProjectState.TestsCompleted =>
        s.reactiveResults.toList.flatMap { results =>
          results.values.flatMap { result =>
            result.failures.map(f => (s.project, result.suite, f))
          }
        }
      }
      .flatten
      .toList
}

object TestRunState {
  def initial(testProjects: Set[model.CrossProjectName], graph: ProjectGraph, startTime: Long): TestRunState =
    TestRunState(
      projects = testProjects.map(p => p -> ProjectState.Initial(p, isTestProject = true)).toMap,
      testProjects = testProjects,
      graph = graph,
      startTime = startTime
    )
}

/** Reducer for project state transitions */
object ProjectStateReducer {
  import ProjectState._
  import ProjectAction._

  def reduce(state: ProjectState, action: ProjectAction): ProjectState = (state, action) match {
    // Initial -> Compiling
    case (s: Initial, StartCompile(_, timestamp)) =>
      Compiling(s.project, s.isTestProject, timestamp, Nil)

    // Initial + finish(fail) -> CompileFailed (handles case where StartCompile wasn't received)
    case (s: Initial, FinishCompile(_, success, errors, durationMs)) if !success =>
      CompileFailed(s.project, s.isTestProject, errors, durationMs)

    // Initial + finish(success) -> CompileSucceeded (handles case where StartCompile wasn't received)
    case (s: Initial, FinishCompile(_, success, _, durationMs)) if success =>
      CompileSucceeded(s.project, s.isTestProject, durationMs)

    // Compiling + progress -> Compiling (update percentage)
    case (s: Compiling, UpdateCompileProgress(_, percent)) =>
      s.updateProgress(percent)

    // Compiling + diagnostic -> Compiling (accumulate)
    case (s: Compiling, AddDiagnostic(_, diagnostic)) =>
      s.addDiagnostic(diagnostic)

    // Compiling + finish(fail) -> CompileFailed
    case (s: Compiling, FinishCompile(_, success, errors, durationMs)) if !success =>
      CompileFailed(s.project, s.isTestProject, errors, durationMs)

    // Compiling + finish(success) -> CompileSucceeded
    case (s: Compiling, FinishCompile(_, success, _, durationMs)) if success =>
      CompileSucceeded(s.project, s.isTestProject, durationMs)

    // Any -> Skipped
    case (s, SkipProject(_, reason)) =>
      Skipped(s.project, s.isTestProject, reason)

    // CompileSucceeded -> DiscoveringSuites
    case (s: CompileSucceeded, StartDiscovery(_)) =>
      DiscoveringSuites(s.project, s.isTestProject)

    // DiscoveringSuites -> SuitesDiscovered (or TestsCompleted if no suites)
    case (s: DiscoveringSuites, FinishDiscovery(_, suites)) =>
      if (suites.isEmpty) {
        // No suites found - mark as completed immediately with empty reactive execution
        TestsCompleted(s.project, s.isTestProject, TestExecution.Reactive.initial(Nil), 0L)
      } else {
        SuitesDiscovered(s.project, s.isTestProject, suites)
      }

    // ==================== Reactive (JVM) test transitions ====================

    // SuitesDiscovered + StartSuite -> TestsInProgress (Reactive)
    case (s: SuitesDiscovered, StartSuite(_, suite, jvmPid, timestamp)) =>
      val reactive = TestExecution.Reactive.initial(s.suites).startSuite(suite, jvmPid, timestamp)
      TestsInProgress(s.project, s.isTestProject, reactive)

    // TestsInProgress (Reactive) + StartSuite -> TestsInProgress
    case (s: TestsInProgress, StartSuite(_, suite, jvmPid, timestamp)) =>
      s.execution match {
        case r: TestExecution.Reactive =>
          s.copy(execution = r.startSuite(suite, jvmPid, timestamp))
        case _ => s // Invalid: BSP mode doesn't have suites
      }

    // TestsInProgress (Reactive) + FinishSuite -> TestsInProgress or TestsCompleted
    case (s: TestsInProgress, FinishSuite(_, suite, result)) =>
      s.execution match {
        case r: TestExecution.Reactive =>
          val updated = r.completeSuite(suite, result)
          if (updated.isComplete) {
            val totalDuration = updated.completedSuites.values.map(_.durationMs).sum
            TestsCompleted(s.project, s.isTestProject, updated, totalDuration)
          } else {
            s.copy(execution = updated)
          }
        case _ => s // Invalid: BSP mode doesn't have suites
      }

    // TestsInProgress (Reactive) + StartTest -> TestsInProgress
    case (s: TestsInProgress, StartTest(_, suite, test, timestamp)) =>
      s.execution match {
        case r: TestExecution.Reactive =>
          val key = TestKey(s.project, suite, test)
          s.copy(execution = r.startTest(key, RunningTestInfo(suite, test, timestamp)))
        case _ => s // Invalid: BSP mode doesn't have individual tests
      }

    // TestsInProgress (Reactive) + FinishTest -> TestsInProgress
    case (s: TestsInProgress, FinishTest(_, suite, test, _, _, _, _)) =>
      s.execution match {
        case r: TestExecution.Reactive =>
          val key = TestKey(s.project, suite, test)
          s.copy(execution = r.completeTest(key))
        case _ => s // Invalid: BSP mode doesn't have individual tests
      }

    // ==================== BSP (JS/Native) test transitions ====================

    // SuitesDiscovered + StartBspTests -> TestsInProgress (Bsp)
    case (s: SuitesDiscovered, StartBspTests(_, platform, timestamp)) =>
      TestsInProgress(s.project, s.isTestProject, TestExecution.Bsp.running(platform, timestamp))

    // CompileSucceeded + StartBspTests -> TestsInProgress (Bsp) - skip discovery for BSP
    case (s: CompileSucceeded, StartBspTests(_, platform, timestamp)) =>
      TestsInProgress(s.project, s.isTestProject, TestExecution.Bsp.running(platform, timestamp))

    // TestsInProgress (Bsp) + FinishBspTests -> TestsCompleted
    case (s: TestsInProgress, FinishBspTests(_, success, durationMs, output)) =>
      s.execution match {
        case b: TestExecution.Bsp =>
          val completed = b.copy(status = TestExecution.BspStatus.Completed(success, durationMs, output))
          TestsCompleted(s.project, s.isTestProject, completed, durationMs)
        case _ => s // Invalid: Reactive mode doesn't use BSP finish
      }

    // Invalid transitions - keep current state
    case (s, _) =>
      s
  }
}

/** ADT for what to display in the "Running" section of the TUI. Derived from TestRunState for clean separation of concerns.
  */
sealed trait ProjectDisplayItem
object ProjectDisplayItem {

  /** Info about a test running on a JVM */
  case class RunningTest(
      suiteName: TestTypes.SuiteClassName,
      testName: TestTypes.TestName,
      jvmPid: Long,
      elapsedMs: Long
  )

  /** Project is compiling */
  case class Compiling(
      project: model.CrossProjectName,
      percent: Option[Int]
  ) extends ProjectDisplayItem

  /** Project is running tests - sum type for different execution modes */
  sealed trait Testing extends ProjectDisplayItem {
    def project: model.CrossProjectName
  }

  object Testing {

    /** Reactive/JVM mode - granular suite/test tracking */
    case class Reactive(
        project: model.CrossProjectName,
        suitesCompleted: Int,
        suitesTotal: Int,
        failures: Int,
        runningTests: List[RunningTest]
    ) extends Testing {
      def activeJvmCount: Int = runningTests.map(_.jvmPid).distinct.size
    }

    /** BSP mode (JS/Native) - project-level only */
    case class Bsp(
        project: model.CrossProjectName,
        platform: model.PlatformId,
        elapsedMs: Long
    ) extends Testing
  }

  /** Summary row showing waiting projects */
  case class Waiting(count: Int) extends ProjectDisplayItem

  /** Derive display items from state */
  def fromState(state: TestRunState, now: Long): List[ProjectDisplayItem] = {
    val compiling = state.projects.values
      .collect { case s: ProjectState.Compiling =>
        Compiling(s.project, s.progressPercent)
      }
      .toList
      .sortBy(_.project.value)

    val testing: List[Testing] = state.projects.values
      .collect { case s: ProjectState.TestsInProgress =>
        s.execution match {
          case r: ProjectState.TestExecution.Reactive =>
            // Show running tests; fall back to running suites if no test info
            val runningTests = if (r.runningTests.nonEmpty) {
              r.runningTests.values
                .map { info =>
                  val jvmPid = r.runningSuites.get(info.suite).map(_.jvmPid).getOrElse(0L)
                  RunningTest(info.suite, info.test, jvmPid, now - info.startedAt)
                }
                .toList
                .sortBy(t => (t.jvmPid, t.suiteName.value))
            } else {
              // Fallback: show suite as the "test" when we don't have test-level info
              r.runningSuites.values
                .map { info =>
                  RunningTest(info.suite, TestTypes.TestName(""), info.jvmPid, now - info.startedAt)
                }
                .toList
                .sortBy(_.jvmPid)
            }
            Testing.Reactive(s.project, r.suitesCompleted, r.suiteCount, r.failedSoFar, runningTests)

          case b: ProjectState.TestExecution.Bsp =>
            val elapsed = b.status match {
              case ProjectState.TestExecution.BspStatus.Running(startedAt) => now - startedAt
              case _                                                       => 0L
            }
            Testing.Bsp(s.project, b.platform, elapsed)
        }
      }
      .toList
      .sortBy(_.project.value)

    // Count waiting projects (Initial, CompileSucceeded, DiscoveringSuites, SuitesDiscovered)
    val waitingCount = state.testProjects.count { p =>
      state.projects.get(p) match {
        case Some(_: ProjectState.Initial)           => true
        case Some(_: ProjectState.CompileSucceeded)  => true
        case Some(_: ProjectState.DiscoveringSuites) => true
        case Some(_: ProjectState.SuitesDiscovered)  => true
        case _                                       => false
      }
    }

    val items = compiling ++ testing
    if (waitingCount > 0) items :+ Waiting(waitingCount)
    else items
  }
}
