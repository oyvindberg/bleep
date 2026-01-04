package bleep.testing

import bleep.model

import java.nio.file.Path
import java.security.MessageDigest
import scala.collection.immutable.Queue

/** JVM identifier - wraps the process PID */
case class JvmId(pid: Long) extends AnyVal

/** A suite waiting to be scheduled */
case class SuiteJob(
    project: model.CrossProjectName,
    suite: DiscoveredSuite,
    classpath: List[Path],
    jvmCommand: Path,
    jvmOptions: List[String]
) {
  lazy val jvmKey: JvmKey = JvmKey.from(classpath, jvmOptions)
}

/** Key for pooling JVMs - JVMs with the same key can be reused */
case class JvmKey(classpathHash: String, optionsHash: String)

object JvmKey {
  def from(classpath: List[Path], options: List[String]): JvmKey = {
    val cpHash = hashStrings(classpath.map(_.toString))
    val optHash = hashStrings(options)
    JvmKey(cpHash, optHash)
  }

  private def hashStrings(strings: List[String]): String = {
    val md = MessageDigest.getInstance("SHA-256")
    strings.foreach(s => md.update(s.getBytes("UTF-8")))
    md.digest().take(8).map("%02x".format(_)).mkString
  }
}

/** GADT for scheduler actions - a simple monad
  *
  * Uses Pure and FlatMap for monadic composition. Primitive effects:
  *   - SpawnJvm: Start a JVM, returns JvmId
  *   - RunSuite: Run a suite on a JVM, returns SuiteResult
  *   - KillJvm: Kill a JVM
  *   - NotifyTimeout: Notify about a timeout
  *
  * Example:
  * {{{
  * // Spawn and run:
  * SpawnJvm(job).flatMap(jvmId => RunSuite(jvmId, job))
  *
  * // Reuse existing:
  * RunSuite(existingJvmId, job)
  * }}}
  */
sealed trait SchedulerAction[A] {
  def name: String

  def flatMap[B](f: A => SchedulerAction[B]): SchedulerAction[B] =
    SchedulerAction.FlatMap(this, f)

  def map[B](f: A => B): SchedulerAction[B] =
    flatMap(a => SchedulerAction.Pure(f(a)))

  def >>[B](next: SchedulerAction[B]): SchedulerAction[B] =
    flatMap(_ => next)
}

object SchedulerAction {

  /** Pure value - lifts a value into an action (no effect) */
  case class Pure[A](value: A) extends SchedulerAction[A] {
    def name: String = s"pure($value)"
  }

  /** Sequence two actions */
  case class FlatMap[A, B](action: SchedulerAction[A], f: A => SchedulerAction[B]) extends SchedulerAction[B] {
    def name: String = s"${action.name}.flatMap(...)"
  }

  /** Spawn a new JVM process - primitive effect Returns the PID-based JvmId once the JVM is ready
    */
  case class SpawnJvm(job: SuiteJob) extends SchedulerAction[JvmId] {
    def name: String = s"spawn-jvm(${job.project.value})"
  }

  /** Run a suite on a JVM - primitive effect */
  case class RunSuite(jvmId: JvmId, suite: SuiteJob) extends SchedulerAction[SuiteResult] {
    def name: String = s"run-suite(${suite.suite.className})"
  }

  /** Get thread dump from a JVM - primitive effect */
  case class GetThreadDump(jvmId: JvmId, job: SuiteJob) extends SchedulerAction[Option[ThreadDumpInfo]] {
    def name: String = s"get-thread-dump(${jvmId.pid})"
  }

  /** Kill a JVM - primitive effect */
  case class KillJvm(jvmId: JvmId, reason: String) extends SchedulerAction[Unit] {
    def name: String = s"kill-jvm(${jvmId.pid})"
  }

  /** Notify about a timeout with optional thread dump - primitive effect */
  case class NotifyTimeout(jvmId: JvmId, job: SuiteJob, reason: String, threadDump: Option[ThreadDumpInfo]) extends SchedulerAction[Unit] {
    def name: String = s"notify-timeout(${job.suite.className})"
  }

  /** Helper to spawn a JVM and immediately run a suite */
  def spawnAndRun(job: SuiteJob): SchedulerAction[SuiteResult] =
    SpawnJvm(job).flatMap(jvmId => RunSuite(jvmId, job))
}

/** Events that flow into the scheduler */
sealed trait SchedulerEvent

object SchedulerEvent {

  /** A project finished compiling and has suites ready to run */
  case class SuitesReady(
      project: model.CrossProjectName,
      suites: List[DiscoveredSuite],
      classpath: List[Path],
      jvmCommand: Path,
      jvmOptions: List[String]
  ) extends SchedulerEvent

  /** A JVM has started, is ready, and running a suite */
  case class JvmStartedSuite(jvmId: JvmId, job: SuiteJob, timestamp: Long) extends SchedulerEvent

  /** A suite finished (JVM is now idle) */
  case class SuiteFinished(jvmId: JvmId, result: SuiteResult) extends SchedulerEvent

  /** Thread dump received from a timed-out JVM */
  case class ThreadDumpReceived(jvmId: JvmId, dump: Option[ThreadDumpInfo]) extends SchedulerEvent

  /** A JVM died unexpectedly */
  case class JvmDied(jvmId: JvmId, error: Option[String]) extends SchedulerEvent

  /** A JVM was killed (by timeout or user cancel) */
  case class JvmKilled(jvmId: JvmId) extends SchedulerEvent

  /** Discovery phase is complete - no more suites will be sent */
  case object DiscoveryComplete extends SchedulerEvent
}

/** Timeout configuration */
case class TimeoutConfig(
    jvmStartupMs: Long,
    suiteExecutionMs: Long,
    idleJvmMaxMs: Long
)

object TimeoutConfig {
  val default: TimeoutConfig = TimeoutConfig(
    jvmStartupMs = 30 * 1000L,
    suiteExecutionMs = 2 * 60 * 1000L,
    idleJvmMaxMs = 60 * 1000L
  )

  def fromSeconds(jvmStartupSeconds: Int, suiteTimeoutMinutes: Int): TimeoutConfig =
    TimeoutConfig(
      jvmStartupMs = jvmStartupSeconds * 1000L,
      suiteExecutionMs = suiteTimeoutMinutes * 60 * 1000L,
      idleJvmMaxMs = 60 * 1000L
    )
}

/** State of a JVM in the scheduler (after it's ready) */
sealed trait JvmState {
  def jvmId: JvmId
  def key: JvmKey
}

object JvmState {

  /** JVM is currently running a suite */
  case class Running(jvmId: JvmId, key: JvmKey, job: SuiteJob, suiteStartedAt: Long) extends JvmState

  /** Suite timed out, requesting thread dump */
  case class GettingThreadDump(jvmId: JvmId, key: JvmKey, job: SuiteJob, timeoutAt: Long) extends JvmState

  /** Got thread dump, killing JVM */
  case class Killing(jvmId: JvmId, key: JvmKey, job: SuiteJob, threadDump: Option[ThreadDumpInfo]) extends JvmState

  /** JVM is idle and can be reused */
  case class Idle(jvmId: JvmId, key: JvmKey, idleSince: Long) extends JvmState
}

/** Immutable suite scheduler state */
case class SuiteSchedulerState(
    pendingSuites: Queue[SuiteJob],
    /** JVMs that are ready (running or idle) - keyed by PID */
    jvms: Map[JvmId, JvmState],
    /** Number of spawn actions emitted but not yet completed */
    pendingSpawns: Int,
    maxConcurrency: Int,
    tickCount: Long,
    timeoutConfig: TimeoutConfig,
    completedSuites: Map[(model.CrossProjectName, String), SuiteResult],
    startedProjects: Set[model.CrossProjectName],
    /** True when discovery phase is complete - no more suites will be sent */
    discoveryComplete: Boolean
) {

  /** Slots currently in use (running JVMs + pending spawns) */
  def slotsInUse: Int = {
    val runningCount = jvms.count { case (_, s) => s.isInstanceOf[JvmState.Running] }
    runningCount + pendingSpawns
  }

  def availableSlots: Int = maxConcurrency - slotsInUse

  def idleJvmsByKey: Map[JvmKey, List[JvmState.Idle]] =
    jvms.values.collect { case idle: JvmState.Idle => idle }.toList.groupBy(_.key)

  def isComplete: Boolean =
    discoveryComplete && pendingSuites.isEmpty && pendingSpawns == 0 &&
      jvms.forall { case (_, s) => s.isInstanceOf[JvmState.Idle] }

  def canScheduleWork: Boolean =
    pendingSuites.nonEmpty && availableSlots > 0

  def completedFor(project: model.CrossProjectName): Map[String, SuiteResult] =
    completedSuites.collect {
      case ((p, suite), result) if p == project => suite -> result
    }
}

object SuiteSchedulerState {
  def empty(maxConcurrency: Int, timeoutConfig: TimeoutConfig): SuiteSchedulerState =
    SuiteSchedulerState(
      pendingSuites = Queue.empty,
      jvms = Map.empty,
      pendingSpawns = 0,
      maxConcurrency = maxConcurrency,
      tickCount = 0,
      timeoutConfig = timeoutConfig,
      completedSuites = Map.empty,
      startedProjects = Set.empty,
      discoveryComplete = false
    )
}

/** Pure scheduler logic */
object SuiteScheduler {

  /** Result of a tick */
  case class TickResult(
      state: SuiteSchedulerState,
      actions: List[SchedulerAction[_]]
  )

  /** Apply a single event to the state */
  def applyEvent(state: SuiteSchedulerState, event: SchedulerEvent, nowMs: Long): SuiteSchedulerState = event match {

    case SchedulerEvent.SuitesReady(project, suites, classpath, jvmCommand, jvmOptions) =>
      val jobs = suites.map(s => SuiteJob(project, s, classpath, jvmCommand, jvmOptions))
      state.copy(
        pendingSuites = state.pendingSuites.enqueueAll(jobs),
        startedProjects = state.startedProjects + project
      )

    case SchedulerEvent.JvmStartedSuite(jvmId, job, timestamp) =>
      state.jvms.get(jvmId) match {
        case Some(_) =>
          // Reuse case - JVM already known (was Idle), just update to Running
          state.copy(jvms = state.jvms + (jvmId -> JvmState.Running(jvmId, job.jvmKey, job, timestamp)))
        case None =>
          // Fresh spawn case - add to map and decrement pendingSpawns
          state.copy(
            jvms = state.jvms + (jvmId -> JvmState.Running(jvmId, job.jvmKey, job, timestamp)),
            pendingSpawns = Math.max(0, state.pendingSpawns - 1)
          )
      }

    case SchedulerEvent.SuiteFinished(jvmId, result) =>
      state.jvms.get(jvmId) match {
        case Some(JvmState.Running(_, key, job, _)) =>
          state.copy(
            jvms = state.jvms + (jvmId -> JvmState.Idle(jvmId, key, nowMs)),
            completedSuites = state.completedSuites + ((job.project, job.suite.className) -> result)
          )
        case _ => state
      }

    case SchedulerEvent.ThreadDumpReceived(jvmId, dump) =>
      state.jvms.get(jvmId) match {
        case Some(JvmState.GettingThreadDump(_, key, job, _)) =>
          // Got dump, transition to Killing
          state.copy(jvms = state.jvms + (jvmId -> JvmState.Killing(jvmId, key, job, dump)))
        case _ => state
      }

    case SchedulerEvent.JvmDied(jvmId, errorOpt) =>
      state.jvms.get(jvmId) match {
        case Some(jvmState) =>
          val jobOpt = jvmState match {
            case JvmState.Running(_, _, job, _)           => Some(job)
            case JvmState.GettingThreadDump(_, _, job, _) => Some(job)
            case JvmState.Killing(_, _, job, _)           => Some(job)
            case JvmState.Idle(_, _, _)                   => None
          }
          jobOpt match {
            case Some(job) =>
              val failureResult = SuiteResult(
                suite = TestTypes.SuiteClassName(job.suite.className),
                passed = 0,
                failed = 1,
                skipped = 0,
                ignored = 0,
                durationMs = 0,
                failures = List(TestFailureInfo(TestTypes.TestName("(JVM died)"), errorOpt, None))
              )
              state.copy(
                jvms = state.jvms - jvmId,
                completedSuites = state.completedSuites + ((job.project, job.suite.className) -> failureResult)
              )
            case None =>
              state.copy(jvms = state.jvms - jvmId)
          }
        case None => state
      }

    case SchedulerEvent.JvmKilled(jvmId) =>
      state.jvms.get(jvmId) match {
        case Some(JvmState.Killing(_, _, job, _)) =>
          // Record as failed suite
          val failureResult = SuiteResult(
            suite = TestTypes.SuiteClassName(job.suite.className),
            passed = 0,
            failed = 1,
            skipped = 0,
            ignored = 0,
            durationMs = 0,
            failures = List(TestFailureInfo(TestTypes.TestName("(timeout)"), Some("Suite timed out"), None))
          )
          state.copy(
            jvms = state.jvms - jvmId,
            completedSuites = state.completedSuites + ((job.project, job.suite.className) -> failureResult)
          )
        case _ =>
          state.copy(jvms = state.jvms - jvmId)
      }

    case SchedulerEvent.DiscoveryComplete =>
      state.copy(discoveryComplete = true)
  }

  def applyEvents(state: SuiteSchedulerState, events: List[SchedulerEvent], nowMs: Long): SuiteSchedulerState =
    events.foldLeft(state)((s, e) => applyEvent(s, e, nowMs))

  /** Timeout for getting thread dump before giving up */
  private val ThreadDumpTimeoutMs = 5000L

  private def checkTimeouts(state: SuiteSchedulerState, nowMs: Long): (SuiteSchedulerState, List[SchedulerAction[_]]) = {
    var actions = List.empty[SchedulerAction[_]]
    var updatedJvms = state.jvms

    state.jvms.foreach { case (jvmId, jvmState) =>
      jvmState match {
        case JvmState.Running(_, key, job, suiteStartedAt) =>
          if (nowMs - suiteStartedAt > state.timeoutConfig.suiteExecutionMs) {
            // Timeout detected -> transition to GettingThreadDump
            updatedJvms = updatedJvms + (jvmId -> JvmState.GettingThreadDump(jvmId, key, job, nowMs))
            actions = actions :+ SchedulerAction.GetThreadDump(jvmId, job)
          }

        case JvmState.GettingThreadDump(_, key, job, timeoutAt) =>
          if (nowMs - timeoutAt > ThreadDumpTimeoutMs) {
            // Thread dump timed out -> transition to Killing without dump
            updatedJvms = updatedJvms + (jvmId -> JvmState.Killing(jvmId, key, job, None))
            actions = actions :+ SchedulerAction.KillJvm(jvmId, "Thread dump timeout")
            actions = actions :+ SchedulerAction.NotifyTimeout(jvmId, job, "Suite timed out", None)
          }

        case JvmState.Killing(_, _, job, threadDump) =>
          // Already in killing state - emit kill and notify actions
          // (This handles the case where we got the thread dump via event)
          actions = actions :+ SchedulerAction.KillJvm(jvmId, "Suite timed out")
          actions = actions :+ SchedulerAction.NotifyTimeout(jvmId, job, "Suite timed out", threadDump)
        // Will be cleaned up when JvmKilled event arrives

        case JvmState.Idle(_, _, idleSince) =>
          if (nowMs - idleSince > state.timeoutConfig.idleJvmMaxMs) {
            updatedJvms = updatedJvms - jvmId
            actions = actions :+ SchedulerAction.KillJvm(jvmId, "Idle timeout")
          }
      }
    }

    (state.copy(jvms = updatedJvms), actions)
  }

  private def scheduleWork(state: SuiteSchedulerState, nowMs: Long): (SuiteSchedulerState, List[SchedulerAction[_]]) = {
    if (!state.canScheduleWork) return (state, Nil)

    var actions = List.empty[SchedulerAction[_]]
    var updatedJvms = state.jvms
    var remainingQueue = state.pendingSuites
    var slotsUsed = 0
    var newPendingSpawns = state.pendingSpawns
    val maxToSchedule = state.availableSlots

    // First pass: match pending suites to idle JVMs (reuse)
    val idleByKey = state.idleJvmsByKey.map { case (k, v) => k -> v.toBuffer }.toMap

    val (matchedJobs, unmatchedJobs) = {
      val matched = List.newBuilder[(SuiteJob, JvmState.Idle)]
      val unmatched = Queue.newBuilder[SuiteJob]

      remainingQueue.foreach { job =>
        if (slotsUsed < maxToSchedule) {
          idleByKey.get(job.jvmKey).flatMap(_.headOption) match {
            case Some(idle) =>
              matched += ((job, idle))
              idleByKey(job.jvmKey).remove(0): Unit
              slotsUsed += 1
            case None =>
              unmatched += job
          }
        } else {
          unmatched += job
        }
      }
      (matched.result(), unmatched.result())
    }

    // Generate RunSuite actions for matched pairs (reusing idle JVMs)
    matchedJobs.foreach { case (job, idle) =>
      val action = SchedulerAction.RunSuite(idle.jvmId, job)
      actions = actions :+ action
      updatedJvms = updatedJvms + (idle.jvmId -> JvmState.Running(idle.jvmId, idle.key, job, nowMs))
    }

    remainingQueue = unmatchedJobs

    // Second pass: spawn new JVMs
    // Iteratively pick the best candidate, updating counts after each pick
    // Priority: (1) prefer keys without running JVMs, (2) fewer tests running per project, (3) alphabetical
    val remainingSlots = maxToSchedule - slotsUsed
    if (remainingSlots > 0 && remainingQueue.nonEmpty) {
      // Initial count of running tests per project
      val initialRunningPerProject = state.jvms.values
        .collect { case JvmState.Running(_, _, job, _) =>
          job.project
        }
        .groupBy(identity)
        .view
        .mapValues(_.size)
        .toMap
        .withDefaultValue(0)

      // Keys that already have running JVMs
      val runningKeys = state.jvms.values.collect { case JvmState.Running(_, key, _, _) =>
        key
      }.toSet

      // Iteratively pick best candidates, updating project counts as we go
      var pickedPerProject = Map.empty[model.CrossProjectName, Int].withDefaultValue(0)
      var picked = List.empty[SuiteJob]
      var available = remainingQueue.toList.sortBy(_.suite.className)

      while (picked.size < remainingSlots && available.nonEmpty) {
        // Find the best candidate
        val best = available.minBy { job =>
          val hasRunningJvm = runningKeys.contains(job.jvmKey)
          val totalForProject = initialRunningPerProject(job.project) + pickedPerProject(job.project)
          (hasRunningJvm, totalForProject, job.suite.className)
        }

        picked = picked :+ best
        pickedPerProject = pickedPerProject.updated(best.project, pickedPerProject(best.project) + 1)
        available = available.filterNot(_ eq best)
      }

      picked.foreach { job =>
        actions = actions :+ SchedulerAction.spawnAndRun(job)
        newPendingSpawns += 1
      }

      val spawnedSet = picked.toSet
      remainingQueue = remainingQueue.filterNot(spawnedSet.contains)
    }

    val newState = state.copy(
      pendingSuites = remainingQueue,
      jvms = updatedJvms,
      pendingSpawns = newPendingSpawns
    )

    (newState, actions)
  }

  /** Core tick function */
  def tick(state: SuiteSchedulerState, events: List[SchedulerEvent], nowMs: Long): TickResult = {
    val stateAfterEvents = applyEvents(state, events, nowMs)
    val (stateAfterTimeouts, timeoutActions) = checkTimeouts(stateAfterEvents, nowMs)
    val (finalState, scheduleActions) = scheduleWork(stateAfterTimeouts, nowMs)

    TickResult(
      state = finalState.copy(tickCount = finalState.tickCount + 1),
      actions = timeoutActions ++ scheduleActions
    )
  }
}
