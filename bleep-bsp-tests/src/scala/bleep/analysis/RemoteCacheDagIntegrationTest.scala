package bleep.analysis

import bleep.bsp.{Outcome, RemoteCacheContext, TaskDag}
import bleep.bsp.Outcome.KillReason
import bleep.bsp.TaskDag.{CachePullTask, CompileTask, DagEvent, TaskId, TaskResult}
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
  *   - DAG shape when `withCache = true` / `false`: pull task inserted, correct deps
  *   - Lazy pull ordering: downstream pulls wait for upstream compiles
  *   - Executor orchestration: pull runs before compile
  *   - Cache-hit short-circuit: `wasHit == true` bypasses the real compile handler
  *   - Fail-hard semantics: pull errors fail the build (no fallback)
  *   - Cancellation: kill signal terminates in-flight pulls
  *   - `Disabled` context is a no-op
  *
  * Push is NOT part of the DAG — `bleep remote-cache push` is a separate command. These tests don't exercise it.
  *
  * Tests avoid S3 entirely — they exercise the DAG + executor + `RemoteCacheContext` seam using in-memory test doubles. Full end-to-end tests against a real
  * S3-compatible backend belong elsewhere (not on every CI run).
  */
class RemoteCacheDagIntegrationTest extends AnyFunSuite with Matchers {

  private def projectName(name: String): CrossProjectName =
    CrossProjectName(ProjectName(name), None)

  /** In-memory cache context driven by a predefined hit-set. */
  private class FakeCache(hits: Set[CrossProjectName]) extends RemoteCacheContext {
    val pulled = new ConcurrentLinkedQueue[CrossProjectName]()

    def isEnabled: Boolean = true
    def tryPull(project: CrossProjectName): IO[Unit] =
      IO(pulled.add(project): Unit)
    def wasHit(project: CrossProjectName): IO[Boolean] =
      IO.pure(hits.contains(project))
  }

  // ==========================================================================
  // DAG Construction Tests
  // ==========================================================================

  test("buildCompileDag with cache: adds CachePullTask per project (no push task)") {
    val a = projectName("a")
    val b = projectName("b")

    val dag = TaskDag.buildCompileDag(
      projects = Set(a, b),
      allProjectDeps = Map(a -> Set.empty, b -> Set(a)),
      withCache = true
    )

    dag.tasks.values.collect { case t: CachePullTask => t } should have size 2
    dag.tasks.values.collect { case t: CompileTask => t } should have size 2
    dag.tasks should have size 4
  }

  test("buildCompileDag without cache: no cache tasks") {
    val a = projectName("a")

    val dag = TaskDag.buildCompileDag(
      projects = Set(a),
      allProjectDeps = Map.empty,
      withCache = false
    )

    dag.tasks.values.collect { case t: CachePullTask => t } shouldBe empty
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

  test("buildTestDag with cache: includes pull for every project transitively") {
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
    pulls shouldBe Set(leaf, testProject)
  }

  test("buildLinkDag with cache: includes pull for every project transitively") {
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

  test("executor: cache pull runs before compile") {
    val project = projectName("a")
    val dag = TaskDag.buildCompileDag(Set(project), Map.empty, withCache = true)

    val order = new ConcurrentLinkedQueue[String]()

    val executor = TaskDag.executor(
      compileHandler = (_, _) => IO(order.add("compile"): Unit).as(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      cachePullHandler = (_, _) => IO(order.add("pull"): Unit).as(TaskResult.Success)
    )

    (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      _ <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield ()).unsafeRunSync()

    order.asScala.toList shouldBe List("pull", "compile")
  }

  test("executor: downstream cache pull waits for upstream compile to finish (lazy pull)") {
    val leaf = projectName("leaf")
    val downstream = projectName("down")
    val dag = TaskDag.buildCompileDag(
      projects = Set(downstream),
      allProjectDeps = Map(leaf -> Set.empty, downstream -> Set(leaf)),
      withCache = true
    )

    val events = new ConcurrentLinkedQueue[(String, Long)]()
    def record(tag: String): IO[Unit] = IO(events.add((tag, System.nanoTime())): Unit)

    val executor = TaskDag.executor(
      compileHandler =
        (task, _) => record(s"compile:${task.project.value}") >> IO.sleep(scala.concurrent.duration.DurationInt(50).millis).as(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      cachePullHandler = (task, _) => record(s"pull:${task.project.value}").as(TaskResult.Success)
    )

    (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      _ <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield ()).unsafeRunSync()

    val tagOrder = events.asScala.toList.map(_._1)
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
      d <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield d).unsafeRunSync()

    compileCalled.get() shouldBe true
    finalDag.completed should contain(TaskId.Compile(project))
    finalDag.tasks.values.collect { case t: CachePullTask => t } shouldBe empty
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
      r <- shortCircuiting(task, kill)
    } yield r).unsafeRunSync()

    result shouldBe TaskResult.Success
    realCompileCalled.get() shouldBe false
  }

  test("compile handler wrapped with wasHit check: runs real compile on miss") {
    val project = projectName("a")
    val fakeCache = new FakeCache(hits = Set.empty)

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
      _ <- shortCircuiting(task, kill)
    } yield ()).unsafeRunSync()

    realCompileCalled.get() shouldBe true
  }

  // ==========================================================================
  // RemoteCacheContext.Disabled tests
  // ==========================================================================

  test("Disabled cache context: isEnabled false, wasHit false, tryPull is a no-op") {
    val project = projectName("a")
    val ctx = RemoteCacheContext.Disabled

    ctx.isEnabled shouldBe false
    ctx.wasHit(project).unsafeRunSync() shouldBe false
    ctx.tryPull(project).unsafeRunSync() // no-op, should not throw
  }

  // ==========================================================================
  // EventSink tests
  // ==========================================================================

  test("EventSink.noop accepts all events without throwing") {
    val sink = RemoteCacheContext.EventSink.noop
    val project = projectName("a")
    sink.pullStarted(project, 0L)
    sink.pullFinished(project, BleepBspProtocol.Event.CachePullStatus.Hit, 10L, 1024L, 0L)
    succeed
  }

  test("Recording EventSink: captures pull lifecycle events in order") {
    val recorded = new AtomicReference[List[String]](Nil)
    def append(s: String): Unit =
      recorded.updateAndGet(list => list :+ s): Unit

    val sink = new RemoteCacheContext.EventSink {
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
    }

    val project = projectName("a")
    sink.pullStarted(project, 0L)
    sink.pullFinished(project, BleepBspProtocol.Event.CachePullStatus.Hit, 5L, 1024L, 5L)

    recorded.get() shouldBe List(
      "pull-started:a",
      "pull-finished:a:Hit"
    )
  }

  // ==========================================================================
  // Cancellation tests (RemoteCacheContext.pullHandler)
  // ==========================================================================

  /** A cache context whose pull uses `IO.never`, so we can reliably race cancellation against it. */
  private class HangingCache extends RemoteCacheContext {
    def isEnabled: Boolean = true
    def tryPull(project: CrossProjectName): IO[Unit] = IO.never
    def wasHit(project: CrossProjectName): IO[Boolean] = IO.pure(false)
  }

  test("pullHandler: kill signal set BEFORE pull starts → returns Killed immediately") {
    val pull = RemoteCacheContext.pullHandler(new HangingCache)
    val project = projectName("a")

    val result = (for {
      kill <- Deferred[IO, KillReason]
      _ <- kill.complete(KillReason.UserRequest)
      r <- pull(CachePullTask(project, Set.empty), kill)
    } yield r).unsafeRunSync()

    result shouldBe TaskResult.Killed(KillReason.UserRequest)
  }

  test("pullHandler: kill signal fired DURING pull → reports Killed (race wins)") {
    val pull = RemoteCacheContext.pullHandler(new HangingCache)
    val project = projectName("a")

    val result = (for {
      kill <- Deferred[IO, KillReason]
      fib <- pull(CachePullTask(project, Set.empty), kill).start
      _ <- IO.sleep(scala.concurrent.duration.DurationInt(30).millis)
      _ <- kill.complete(KillReason.UserRequest)
      r <- fib.joinWithNever
    } yield r).unsafeRunSync()

    result shouldBe TaskResult.Killed(KillReason.UserRequest)
  }

  test("executor: kill during pull → compile does not run (pull and compile Killed)") {
    val project = projectName("a")
    val dag = TaskDag.buildCompileDag(Set(project), Map.empty, withCache = true)

    val compileCalled = new java.util.concurrent.atomic.AtomicBoolean(false)
    val pullHandler = RemoteCacheContext.pullHandler(new HangingCache)

    val executor = TaskDag.executor(
      compileHandler = (_, _) => IO(compileCalled.set(true)).as(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      cachePullHandler = pullHandler
    )

    val finalDag = (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Deferred[IO, KillReason]
      _ <- (IO.sleep(scala.concurrent.duration.DurationInt(50).millis) >> killSignal.complete(KillReason.UserRequest)).start
      d <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield d).unsafeRunSync()

    compileCalled.get() shouldBe false
    finalDag.killed should contain(TaskId.CachePull(project))
    finalDag.killed should contain(TaskId.Compile(project))
  }

  // ==========================================================================
  // Pull failure tests — fail-hard semantics (no fallback)
  // ==========================================================================

  /** A cache context whose pull raises an exception. */
  private class FailingCache extends RemoteCacheContext {
    def isEnabled: Boolean = true
    def tryPull(project: CrossProjectName): IO[Unit] =
      IO.raiseError(new java.io.IOException(s"network down for ${project.value}"))
    def wasHit(project: CrossProjectName): IO[Boolean] = IO.pure(false)
  }

  test("pullHandler: pull raising an exception propagates (no swallowing)") {
    val pull = RemoteCacheContext.pullHandler(new FailingCache)
    val project = projectName("a")

    // The handler does NOT catch the exception — it propagates to the executor's withRecovery,
    // which converts it to TaskResult.Failure.
    val attempt = (for {
      kill <- Deferred[IO, KillReason]
      r <- pull(CachePullTask(project, Set.empty), kill).attempt
    } yield r).unsafeRunSync()

    attempt.isLeft shouldBe true
    attempt.left.toOption.get shouldBe a[java.io.IOException]
  }

  test("executor: failing pull fails the build, compile is Skipped") {
    val project = projectName("a")
    val dag = TaskDag.buildCompileDag(Set(project), Map.empty, withCache = true)

    val compileCalled = new java.util.concurrent.atomic.AtomicBoolean(false)
    val pullHandler = RemoteCacheContext.pullHandler(new FailingCache)

    val executor = TaskDag.executor(
      compileHandler = (_, _) => IO(compileCalled.set(true)).as(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      cachePullHandler = pullHandler
    )

    val finalDag = (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      d <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield d).unsafeRunSync()

    compileCalled.get() shouldBe false
    finalDag.failed should contain(TaskId.CachePull(project))
    finalDag.skipped should contain(TaskId.Compile(project))
  }

  test("executor: one project's failing pull does NOT affect unrelated projects") {
    val bad = projectName("bad")
    val good = projectName("good")
    // Two independent projects — no deps between them.
    val dag = TaskDag.buildCompileDag(
      projects = Set(bad, good),
      allProjectDeps = Map(bad -> Set.empty, good -> Set.empty),
      withCache = true
    )

    val goodCompileCalled = new java.util.concurrent.atomic.AtomicBoolean(false)

    // Pull fails only for `bad`, succeeds for `good`.
    val selectiveCache = new RemoteCacheContext {
      def isEnabled: Boolean = true
      def tryPull(project: CrossProjectName): IO[Unit] =
        if (project == bad) IO.raiseError(new java.io.IOException("boom"))
        else IO.unit
      def wasHit(project: CrossProjectName): IO[Boolean] = IO.pure(false)
    }

    val executor = TaskDag.executor(
      compileHandler = (task, _) =>
        IO {
          if (task.project == good) goodCompileCalled.set(true)
        }.as(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      cachePullHandler = RemoteCacheContext.pullHandler(selectiveCache)
    )

    val finalDag = (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      d <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield d).unsafeRunSync()

    goodCompileCalled.get() shouldBe true
    finalDag.completed should contain(TaskId.Compile(good))
    finalDag.failed should contain(TaskId.CachePull(bad))
    finalDag.skipped should contain(TaskId.Compile(bad))
  }

  test("executor: simulated S3 transport exception inside IO.blocking fails the build") {
    val throwsTransport = new RemoteCacheContext {
      def isEnabled: Boolean = true
      def tryPull(project: CrossProjectName): IO[Unit] =
        IO.blocking(throw new java.net.SocketTimeoutException("connect timed out"))
      def wasHit(project: CrossProjectName): IO[Boolean] = IO.pure(false)
    }
    val project = projectName("a")
    val dag = TaskDag.buildCompileDag(Set(project), Map.empty, withCache = true)

    val compileCalled = new java.util.concurrent.atomic.AtomicBoolean(false)
    val executor = TaskDag.executor(
      compileHandler = (_, _) => IO(compileCalled.set(true)).as(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      cachePullHandler = RemoteCacheContext.pullHandler(throwsTransport)
    )

    val finalDag = (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      d <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield d).unsafeRunSync()

    compileCalled.get() shouldBe false
    finalDag.failed should contain(TaskId.CachePull(project))
    finalDag.skipped should contain(TaskId.Compile(project))
  }
}
