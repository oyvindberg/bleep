package bleep.analysis

import bleep.bsp.{CompileCacheContext, Outcome, TaskDag}
import bleep.bsp.Outcome.KillReason
import bleep.bsp.TaskDag.{CachePullTask, CachePushTask, CompileTask, DagEvent, TaskId, TaskResult}
import bleep.bsp.protocol.BleepBspProtocol
import bleep.model.{CrossProjectName, ProjectName}
import cats.effect.{Deferred, IO}
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference
import scala.jdk.CollectionConverters.*

/** Integration tests for remote-cache DAG integration.
  *
  * Covers:
  *   - DAG shape when `withCache = true` / `false`: pull/push tasks inserted, correct deps
  *   - Lazy pull ordering: downstream pulls wait for upstream compiles
  *   - Executor orchestration: pull runs before compile, push runs after compile success, push skipped when compile fails
  *   - Cache-hit short-circuit: `wasHit == true` bypasses the real compile handler
  *   - `Disabled` context is a no-op
  *
  * Tests avoid S3 entirely — they exercise the DAG + executor + `CompileCacheContext` seam using in-memory test doubles. Full end-to-end tests against a real
  * S3-compatible backend belong elsewhere (not on every CI run).
  */
class RemoteCacheDagIntegrationTest extends AnyFunSuite with Matchers {

  private def projectName(name: String): CrossProjectName =
    CrossProjectName(ProjectName(name), None)

  private def drainQueue(queue: Queue[IO, Option[DagEvent]]): IO[List[DagEvent]] = {
    def loop(acc: List[DagEvent]): IO[List[DagEvent]] =
      queue.tryTake.flatMap {
        case Some(Some(event)) => loop(event :: acc)
        case Some(None)        => IO.pure(acc.reverse)
        case None              => IO.pure(acc.reverse)
      }
    loop(Nil)
  }

  /** In-memory cache context driven by a predefined hit-set. */
  private class FakeCache(hits: Set[CrossProjectName]) extends CompileCacheContext {
    val pulled = new ConcurrentLinkedQueue[CrossProjectName]()
    val pushed = new ConcurrentLinkedQueue[CrossProjectName]()

    def isEnabled: Boolean = true
    def tryPull(project: CrossProjectName): IO[Unit] =
      IO(pulled.add(project): Unit)
    def tryPush(project: CrossProjectName): IO[Unit] =
      IO(pushed.add(project): Unit)
    def wasHit(project: CrossProjectName): IO[Boolean] =
      IO.pure(hits.contains(project))
  }

  // ==========================================================================
  // DAG Construction Tests
  // ==========================================================================

  test("buildCompileDag with cache: adds CachePullTask and CachePushTask per project") {
    val a = projectName("a")
    val b = projectName("b")

    val dag = TaskDag.buildCompileDag(
      projects = Set(a, b),
      allProjectDeps = Map(a -> Set.empty, b -> Set(a)),
      withCache = true
    )

    dag.tasks.values.collect { case t: CachePullTask => t } should have size 2
    dag.tasks.values.collect { case t: CachePushTask => t } should have size 2
    dag.tasks.values.collect { case t: CompileTask => t } should have size 2
    dag.tasks should have size 6
  }

  test("buildCompileDag without cache: no cache tasks") {
    val a = projectName("a")

    val dag = TaskDag.buildCompileDag(
      projects = Set(a),
      allProjectDeps = Map.empty,
      withCache = false
    )

    dag.tasks.values.collect { case t: CachePullTask => t } shouldBe empty
    dag.tasks.values.collect { case t: CachePushTask => t } shouldBe empty
    dag.tasks.values.collect { case t: CompileTask => t } should have size 1
  }

  test("CompileTask depends on its own CachePullTask when cache is enabled") {
    val a = projectName("a")

    val dag = TaskDag.buildCompileDag(
      projects = Set(a),
      allProjectDeps = Map.empty,
      withCache = true
    )

    val compileTask = dag.tasks.values.collectFirst { case t: CompileTask => t }.get
    compileTask.cachePullDep shouldBe Some(TaskId.CachePull(a))
    compileTask.dependencies should contain(TaskId.CachePull(a))
  }

  test("CompileTask has no CachePull dependency when cache is disabled") {
    val a = projectName("a")

    val dag = TaskDag.buildCompileDag(
      projects = Set(a),
      allProjectDeps = Map.empty,
      withCache = false
    )

    val compileTask = dag.tasks.values.collectFirst { case t: CompileTask => t }.get
    compileTask.cachePullDep shouldBe None
    compileTask.dependencies shouldBe empty
  }

  test("CachePushTask depends on its own CompileTask") {
    val a = projectName("a")

    val dag = TaskDag.buildCompileDag(
      projects = Set(a),
      allProjectDeps = Map.empty,
      withCache = true
    )

    val pushTask = dag.tasks.values.collectFirst { case t: CachePushTask => t }.get
    pushTask.dependencies shouldBe Set(TaskId.Compile(a))
  }

  test("CachePullTask for a leaf project has no dependencies (pulls immediately)") {
    val a = projectName("a")

    val dag = TaskDag.buildCompileDag(
      projects = Set(a),
      allProjectDeps = Map.empty,
      withCache = true
    )

    val pullTask = dag.tasks.values.collectFirst { case t: CachePullTask => t }.get
    pullTask.dependencies shouldBe empty
  }

  test("CachePullTask is lazy: waits for upstream CompileTasks before firing") {
    val leaf = projectName("leaf")
    val mid = projectName("mid")
    val top = projectName("top")

    val dag = TaskDag.buildCompileDag(
      projects = Set(top),
      allProjectDeps = Map(
        leaf -> Set.empty,
        mid -> Set(leaf),
        top -> Set(mid)
      ),
      withCache = true
    )

    val pullByProject = dag.tasks.values.collect { case t: CachePullTask => t.project -> t }.toMap

    pullByProject(leaf).dependencies shouldBe empty
    pullByProject(mid).dependencies shouldBe Set(TaskId.Compile(leaf))
    pullByProject(top).dependencies shouldBe Set(TaskId.Compile(mid))
  }

  test("buildTestDag with cache: includes pull/push for every project transitively") {
    val leaf = projectName("leaf")
    val testProject = projectName("test-suite")

    val dag = TaskDag.buildTestDag(
      testProjects = Set(testProject),
      allProjectDeps = Map(
        leaf -> Set.empty,
        testProject -> Set(leaf)
      ),
      platforms = Map.empty,
      withCache = true
    )

    val pulls = dag.tasks.values.collect { case t: CachePullTask => t.project }.toSet
    val pushes = dag.tasks.values.collect { case t: CachePushTask => t.project }.toSet

    pulls shouldBe Set(leaf, testProject)
    pushes shouldBe Set(leaf, testProject)
  }

  test("buildLinkDag with cache: includes pull/push for every project transitively") {
    val dep = projectName("dep")
    val linked = projectName("linked")

    val dag = TaskDag.buildLinkDag(
      projects = Set(linked),
      allProjectDeps = Map(dep -> Set.empty, linked -> Set(dep)),
      platforms = Map.empty,
      releaseMode = false,
      withCache = true
    )

    val pulls = dag.tasks.values.collect { case t: CachePullTask => t.project }.toSet
    pulls shouldBe Set(dep, linked)
  }

  // ==========================================================================
  // Executor Orchestration Tests
  // ==========================================================================

  test("executor: cache pull runs before compile, cache push runs after compile success") {
    val project = projectName("a")
    val dag = TaskDag.buildCompileDag(Set(project), Map.empty, withCache = true)

    val order = new ConcurrentLinkedQueue[String]()

    val executor = TaskDag.executor(
      compileHandler = (_, _) => IO(order.add("compile"): Unit).as(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      cachePullHandler = (_, _) => IO(order.add("pull"): Unit).as(TaskResult.Success),
      cachePushHandler = (_, _) => IO(order.add("push"): Unit).as(TaskResult.Success)
    )

    (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      _          <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield ()).unsafeRunSync()

    order.asScala.toList shouldBe List("pull", "compile", "push")
  }

  test("executor: cache push does NOT run when compile fails") {
    val project = projectName("a")
    val dag = TaskDag.buildCompileDag(Set(project), Map.empty, withCache = true)

    val pushCalled = new java.util.concurrent.atomic.AtomicBoolean(false)

    val executor = TaskDag.executor(
      compileHandler = (_, _) => IO.pure(TaskResult.Failure("boom", Nil)),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      cachePullHandler = (_, _) => IO.pure(TaskResult.Success),
      cachePushHandler = (_, _) => IO(pushCalled.set(true)).as(TaskResult.Success)
    )

    val finalDag = (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      d          <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield d).unsafeRunSync()

    pushCalled.get() shouldBe false
    finalDag.failed should contain(TaskId.Compile(project))
    finalDag.skipped should contain(TaskId.CachePush(project))
  }

  test("executor: downstream cache pull waits for upstream compile to finish (lazy pull)") {
    val leaf = projectName("leaf")
    val downstream = projectName("down")
    val dag = TaskDag.buildCompileDag(
      projects = Set(downstream),
      allProjectDeps = Map(leaf -> Set.empty, downstream -> Set(leaf)),
      withCache = true
    )

    // Record timestamps so we can verify ordering.
    val events = new ConcurrentLinkedQueue[(String, Long)]()
    def record(tag: String): IO[Unit] = IO(events.add((tag, System.nanoTime())): Unit)

    val executor = TaskDag.executor(
      compileHandler = (task, _) => record(s"compile:${task.project.value}") >> IO.sleep(scala.concurrent.duration.DurationInt(50).millis).as(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      cachePullHandler = (task, _) => record(s"pull:${task.project.value}").as(TaskResult.Success),
      cachePushHandler = (task, _) => record(s"push:${task.project.value}").as(TaskResult.Success)
    )

    (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      _          <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield ()).unsafeRunSync()

    val timeline = events.asScala.toList
    val tagOrder = timeline.map(_._1)

    // Expected ordering (respecting dep graph):
    //   pull:leaf (no deps) → compile:leaf (needs pull:leaf) → pull:down (needs compile:leaf) → compile:down → push:*
    tagOrder.indexOf("pull:leaf") should be < tagOrder.indexOf("compile:leaf")
    tagOrder.indexOf("compile:leaf") should be < tagOrder.indexOf("pull:down")
    tagOrder.indexOf("pull:down") should be < tagOrder.indexOf("compile:down")
  }

  test("executor: disabled cache (empty DAG of cache tasks) still compiles") {
    val project = projectName("a")
    val dag = TaskDag.buildCompileDag(Set(project), Map.empty, withCache = false)

    val compileCalled = new java.util.concurrent.atomic.AtomicBoolean(false)

    val executor = TaskDag.executor(
      compileHandler = (_, _) => IO(compileCalled.set(true)).as(TaskResult.Success),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success)
    )

    val finalDag = (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      d          <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield d).unsafeRunSync()

    compileCalled.get() shouldBe true
    finalDag.completed should contain(TaskId.Compile(project))
    finalDag.tasks.values.collect { case t: CachePullTask => t } shouldBe empty
    finalDag.tasks.values.collect { case t: CachePushTask => t } shouldBe empty
  }

  // ==========================================================================
  // Cache-hit short-circuit tests
  // ==========================================================================

  test("compile handler wrapped with wasHit check: skips real compile on hit") {
    val project = projectName("a")
    val fakeCache = new FakeCache(hits = Set(project))

    val realCompileCalled = new java.util.concurrent.atomic.AtomicBoolean(false)
    val underlying: (CompileTask, Deferred[IO, KillReason]) => IO[TaskResult] =
      (_, _) => IO(realCompileCalled.set(true)).as(TaskResult.Success)

    val shortCircuiting: (CompileTask, Deferred[IO, KillReason]) => IO[TaskResult] =
      (task, killSignal) =>
        fakeCache.wasHit(task.project).flatMap { hit =>
          if (hit) IO.pure(TaskResult.Success)
          else underlying(task, killSignal)
        }

    val task = CompileTask(project, Set.empty, cachePullDep = Some(TaskId.CachePull(project)))
    val result = (for {
      kill <- Deferred[IO, KillReason]
      r    <- shortCircuiting(task, kill)
    } yield r).unsafeRunSync()

    result shouldBe TaskResult.Success
    realCompileCalled.get() shouldBe false
  }

  test("compile handler wrapped with wasHit check: runs real compile on miss") {
    val project = projectName("a")
    val fakeCache = new FakeCache(hits = Set.empty) // no hits

    val realCompileCalled = new java.util.concurrent.atomic.AtomicBoolean(false)
    val underlying: (CompileTask, Deferred[IO, KillReason]) => IO[TaskResult] =
      (_, _) => IO(realCompileCalled.set(true)).as(TaskResult.Success)

    val shortCircuiting: (CompileTask, Deferred[IO, KillReason]) => IO[TaskResult] =
      (task, killSignal) =>
        fakeCache.wasHit(task.project).flatMap { hit =>
          if (hit) IO.pure(TaskResult.Success)
          else underlying(task, killSignal)
        }

    val task = CompileTask(project, Set.empty, cachePullDep = Some(TaskId.CachePull(project)))
    (for {
      kill <- Deferred[IO, KillReason]
      _    <- shortCircuiting(task, kill)
    } yield ()).unsafeRunSync()

    realCompileCalled.get() shouldBe true
  }

  // ==========================================================================
  // CompileCacheContext.Disabled tests
  // ==========================================================================

  test("Disabled cache context: isEnabled false, wasHit false, tryPull/tryPush are no-ops") {
    val project = projectName("a")
    val ctx = CompileCacheContext.Disabled

    ctx.isEnabled shouldBe false
    ctx.wasHit(project).unsafeRunSync() shouldBe false
    ctx.tryPull(project).unsafeRunSync() // no-op, should not throw
    ctx.tryPush(project).unsafeRunSync() // no-op, should not throw
  }

  // ==========================================================================
  // EventSink tests
  // ==========================================================================

  test("EventSink.noop accepts all events without throwing") {
    val sink = CompileCacheContext.EventSink.noop
    val project = projectName("a")
    sink.pullStarted(project, 0L)
    sink.pullFinished(project, BleepBspProtocol.Event.CachePullStatus.Hit, 10L, 1024L, 0L)
    sink.pushStarted(project, 0L)
    sink.pushFinished(project, BleepBspProtocol.Event.CachePushStatus.Success, 10L, 1024L, 0L)
    succeed
  }

  test("Recording EventSink: captures pull/push lifecycle events in order") {
    val recorded = new AtomicReference[List[String]](Nil)
    def append(s: String): Unit =
      recorded.updateAndGet(list => list :+ s): Unit

    val sink = new CompileCacheContext.EventSink {
      def pullStarted(project: CrossProjectName, timestamp: Long): Unit =
        append(s"pull-started:${project.value}")
      def pullFinished(
          project: CrossProjectName,
          status: BleepBspProtocol.Event.CachePullStatus,
          durationMs: Long,
          bytes: Long,
          timestamp: Long
      ): Unit =
        append(s"pull-finished:${project.value}:${status.getClass.getSimpleName.stripSuffix("$")}")
      def pushStarted(project: CrossProjectName, timestamp: Long): Unit =
        append(s"push-started:${project.value}")
      def pushFinished(
          project: CrossProjectName,
          status: BleepBspProtocol.Event.CachePushStatus,
          durationMs: Long,
          bytes: Long,
          timestamp: Long
      ): Unit =
        append(s"push-finished:${project.value}:${status.getClass.getSimpleName.stripSuffix("$")}")
    }

    val project = projectName("a")
    sink.pullStarted(project, 0L)
    sink.pullFinished(project, BleepBspProtocol.Event.CachePullStatus.Hit, 5L, 1024L, 5L)
    sink.pushStarted(project, 10L)
    sink.pushFinished(project, BleepBspProtocol.Event.CachePushStatus.AlreadyCached, 2L, 0L, 12L)

    recorded.get() shouldBe List(
      "pull-started:a",
      "pull-finished:a:Hit",
      "push-started:a",
      "push-finished:a:AlreadyCached"
    )
  }
}
