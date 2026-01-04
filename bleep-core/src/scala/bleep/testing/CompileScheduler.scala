package bleep.testing

import bleep.model.CrossProjectName
import cats.effect.{IO, Ref}
import cats.effect.std.Queue
import ch.epfl.scala.bsp4j

/** State of compilation for a project */
sealed trait CompileState
object CompileState {
  case object Pending extends CompileState
  case object InProgress extends CompileState
  case object Completed extends CompileState
  case class Failed(error: String) extends CompileState
}

/** Statistics about scheduler progress */
case class SchedulerStats(
    pending: Int,
    inProgress: Int,
    completed: Int,
    failed: Int,
    testsReady: Int
)

/** Observes BSP compilation events and tracks when projects are ready for testing.
  *
  * Rather than issuing multiple compile commands, we issue ONE compile command for all targets and use BSP's TaskFinish events to detect when individual
  * projects complete. This is more efficient because:
  *   1. Bloop handles dependency ordering internally 2. Single compile command avoids startup overhead 3. Bloop deduplicates compilation of shared dependencies
  *
  * When a test project's compilation finishes successfully, it gets added to the testsReady queue.
  */
trait CompileScheduler {

  /** Called when BSP sends a TaskStart for a project */
  def onCompileStart(targetId: bsp4j.BuildTargetIdentifier): IO[Unit]

  /** Called when BSP sends a TaskFinish for a project */
  def onCompileFinish(targetId: bsp4j.BuildTargetIdentifier, status: bsp4j.StatusCode): IO[Unit]

  /** Take test projects that are ready to run */
  def takeReadyTests(maxTests: Int): IO[List[CrossProjectName]]

  /** Wait for a test project to be ready (blocks until one is available) */
  def awaitReadyTest: IO[Option[CrossProjectName]]

  /** Check if all compilation is done */
  def isAllDone: IO[Boolean]

  /** Get current state snapshot for display */
  def getStats: IO[SchedulerStats]

  /** Signal that compilation is complete (no more events will come) */
  def signalCompilationComplete: IO[Unit]
}

/** Internal state for the scheduler */
private case class SchedulerState(
    projectStates: Map[CrossProjectName, CompileState],
    testProjects: Set[CrossProjectName],
    targetToProject: Map[String, CrossProjectName],
    compilationComplete: Boolean
) {

  /** Mark a project as in-progress */
  def markStarted(targetUri: String): SchedulerState =
    targetToProject.get(targetUri) match {
      case Some(project) =>
        copy(projectStates = projectStates.updated(project, CompileState.InProgress))
      case None => this
    }

  /** Mark a project as completed, return the project if it's a test */
  def markCompleted(targetUri: String): (SchedulerState, Option[CrossProjectName]) =
    targetToProject.get(targetUri) match {
      case Some(project) =>
        val newState = copy(projectStates = projectStates.updated(project, CompileState.Completed))
        val testReady = if (testProjects.contains(project)) Some(project) else None
        (newState, testReady)
      case None => (this, None)
    }

  /** Mark a project as failed */
  def markFailed(targetUri: String, error: String): SchedulerState =
    targetToProject.get(targetUri) match {
      case Some(project) =>
        copy(projectStates = projectStates.updated(project, CompileState.Failed(error)))
      case None => this
    }

  /** Count of pending projects */
  def pendingCount: Int =
    projectStates.values.count(_ == CompileState.Pending)

  /** Count of in-progress projects */
  def inProgressCount: Int =
    projectStates.values.count(_ == CompileState.InProgress)

  /** Count of completed projects */
  def completedCount: Int =
    projectStates.values.count(_ == CompileState.Completed)

  /** Count of failed projects */
  def failedCount: Int =
    projectStates.values.count {
      case CompileState.Failed(_) => true
      case _                      => false
    }

  /** Check if all projects are done */
  def allDone: Boolean = compilationComplete && pendingCount == 0 && inProgressCount == 0
}

object CompileScheduler {

  /** Create a new scheduler for the given project graph.
    *
    * @param graph
    *   The project dependency graph
    * @param buildTargetFor
    *   Function to get the BSP build target URI for a project
    */
  def create(
      graph: ProjectGraph,
      buildTargetFor: CrossProjectName => String
  ): IO[CompileScheduler] = {
    val targetToProject = graph.allProjects.map(p => buildTargetFor(p) -> p).toMap
    val initialStates = graph.allProjects.map(p => p -> CompileState.Pending).toMap
    val initialState = SchedulerState(
      projectStates = initialStates,
      testProjects = graph.testProjects,
      targetToProject = targetToProject,
      compilationComplete = false
    )

    for {
      stateRef <- Ref.of[IO, SchedulerState](initialState)
      // Queue for tests ready to run - None signals completion
      testsReadyQueue <- Queue.unbounded[IO, Option[CrossProjectName]]
    } yield new CompileScheduler {

      override def onCompileStart(targetId: bsp4j.BuildTargetIdentifier): IO[Unit] =
        stateRef.update(_.markStarted(targetId.getUri))

      override def onCompileFinish(targetId: bsp4j.BuildTargetIdentifier, status: bsp4j.StatusCode): IO[Unit] =
        status match {
          case bsp4j.StatusCode.OK =>
            stateRef.modify(_.markCompleted(targetId.getUri)).flatMap {
              case Some(testProject) => testsReadyQueue.offer(Some(testProject))
              case None              => IO.unit
            }
          case bsp4j.StatusCode.ERROR =>
            stateRef.update(_.markFailed(targetId.getUri, "compilation failed"))
          case bsp4j.StatusCode.CANCELLED =>
            stateRef.update(_.markFailed(targetId.getUri, "compilation cancelled"))
        }

      override def takeReadyTests(maxTests: Int): IO[List[CrossProjectName]] =
        // Non-blocking take of up to maxTests from the queue
        (1 to maxTests).toList.foldLeft(IO.pure(List.empty[CrossProjectName])) { (acc, _) =>
          acc.flatMap { list =>
            testsReadyQueue.tryTake.map {
              case Some(Some(project)) => list :+ project
              case _                   => list
            }
          }
        }

      override def awaitReadyTest: IO[Option[CrossProjectName]] =
        testsReadyQueue.take

      override def isAllDone: IO[Boolean] =
        stateRef.get.map(_.allDone)

      override def getStats: IO[SchedulerStats] =
        for {
          state <- stateRef.get
          queueSize <- testsReadyQueue.size
        } yield SchedulerStats(
          pending = state.pendingCount,
          inProgress = state.inProgressCount,
          completed = state.completedCount,
          failed = state.failedCount,
          testsReady = queueSize
        )

      override def signalCompilationComplete: IO[Unit] =
        stateRef.update(_.copy(compilationComplete = true)) *>
          testsReadyQueue.offer(None) // Signal end of stream
    }
  }
}
