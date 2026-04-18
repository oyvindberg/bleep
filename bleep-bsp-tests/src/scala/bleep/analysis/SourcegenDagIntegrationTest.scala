package bleep.analysis

import bleep.bsp.{Outcome, TaskDag}
import bleep.bsp.Outcome.KillReason
import bleep.bsp.TaskDag.{CompileTask, DagEvent, SourcegenPlan, SourcegenTask, TaskId, TaskResult}
import bleep.model.{CrossProjectName, JsonSet, ProjectName, ScriptDef}
import cats.effect.{Deferred, IO}
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

/** Integration tests for sourcegen DAG integration.
  *
  * Covers:
  *   - DAG shape: SourcegenTask inserted per unique script, script-project compile + its transitive deps included, CompileTask dep edge to sourcegen
  *   - Executor orchestration: script-project compile → sourcegen → target compile ordering
  *   - Failure propagation: script-project compile fails → sourcegen Skipped → target compile Skipped (no more unsafeRunSync hang)
  *   - Sourcegen failure: target compile is Skipped
  *   - Cancellation: kill during sourcegen → task Killed, target compile Killed/Skipped
  *   - Up-to-date fast-path: sourcegen reports Success quickly without a real fork (tested via fake handler)
  *   - One script shared by many target projects: single SourcegenTask, fan-out via forProjects
  *
  * Tests avoid forking real JVMs — they exercise the DAG + executor seam using stub handlers.
  */
class SourcegenDagIntegrationTest extends AnyFunSuite with Matchers {

  private def projectName(name: String): CrossProjectName =
    CrossProjectName(ProjectName(name), None)

  private def script(scriptProject: CrossProjectName, main: String): ScriptDef.Main =
    ScriptDef.Main(scriptProject, main, JsonSet.empty)

  // ==========================================================================
  // DAG Construction Tests
  // ==========================================================================

  test("buildCompileDag without sourcegen: no SourcegenTasks") {
    val p = projectName("p")
    val dag = TaskDag.buildCompileDag(Set(p), Map.empty, SourcegenPlan.empty)
    dag.tasks.values.collect { case t: SourcegenTask => t } shouldBe empty
    dag.tasks.values.collect { case t: CompileTask => t } should have size 1
  }

  test("buildCompileDag with one sourcegen: task inserted, compile depends on it") {
    val target = projectName("target")
    val scriptsProject = projectName("scripts")
    val s = script(scriptsProject, "gen.Tool")

    val plan = SourcegenPlan(
      perProject = Map(target -> Set(s)),
      scriptProjectDeps = Map(s -> Set(scriptsProject))
    )

    val dag = TaskDag.buildCompileDag(
      projects = Set(target),
      allProjectDeps = Map.empty,
      sourcegen = plan
    )

    val sgTasks = dag.tasks.values.collect { case t: SourcegenTask => t }.toList
    sgTasks should have size 1
    sgTasks.head.script shouldBe s
    sgTasks.head.forProjects shouldBe Set(target)

    val compileTasks = dag.tasks.values.collect { case t: CompileTask => t.project -> t }.toMap
    // Both target and scripts project have CompileTasks
    compileTasks.keySet shouldBe Set(target, scriptsProject)
    // Target compile depends on sourcegen
    compileTasks(target).dependencies should contain(TaskId.Sourcegen(s))
    // Scripts compile has no deps
    compileTasks(scriptsProject).dependencies shouldBe empty
  }

  test("SourcegenTask depends on script-project Compile and its transitive deps") {
    val target = projectName("target")
    val scriptsProject = projectName("scripts")
    val scriptsLib = projectName("scripts-lib")
    val s = script(scriptsProject, "gen.Tool")

    val plan = SourcegenPlan(
      perProject = Map(target -> Set(s)),
      scriptProjectDeps = Map(s -> Set(scriptsProject, scriptsLib))
    )

    val dag = TaskDag.buildCompileDag(
      projects = Set(target),
      allProjectDeps = Map(scriptsProject -> Set(scriptsLib), scriptsLib -> Set.empty),
      sourcegen = plan
    )

    val sgTask = dag.tasks.values.collectFirst { case t: SourcegenTask => t }.get
    sgTask.dependencies shouldBe Set(
      TaskId.Compile(scriptsProject),
      TaskId.Compile(scriptsLib)
    )

    // Script-project compile tasks are in the DAG with their inter-deps
    val compileTasks = dag.tasks.values.collect { case t: CompileTask => t.project -> t }.toMap
    compileTasks.keySet should contain allOf (scriptsProject, scriptsLib, target)
    compileTasks(scriptsProject).dependencies should contain(TaskId.Compile(scriptsLib))
  }

  test("one script shared by many target projects: single SourcegenTask, fan-out via forProjects") {
    val a = projectName("a")
    val b = projectName("b")
    val scriptsProject = projectName("scripts")
    val s = script(scriptsProject, "gen.Shared")

    val plan = SourcegenPlan(
      perProject = Map(a -> Set(s), b -> Set(s)),
      scriptProjectDeps = Map(s -> Set(scriptsProject))
    )

    val dag = TaskDag.buildCompileDag(
      projects = Set(a, b),
      allProjectDeps = Map.empty,
      sourcegen = plan
    )

    val sgTasks = dag.tasks.values.collect { case t: SourcegenTask => t }.toList
    sgTasks should have size 1
    sgTasks.head.forProjects shouldBe Set(a, b)

    // Both target compiles depend on the same sourcegen task
    val compileTasks = dag.tasks.values.collect { case t: CompileTask => t.project -> t }.toMap
    compileTasks(a).dependencies should contain(TaskId.Sourcegen(s))
    compileTasks(b).dependencies should contain(TaskId.Sourcegen(s))
  }

  test("mixed DAG: project with sourcegen + project without — only the one with sourcegen gets a SourcegenTask dep") {
    val withSg = projectName("with-sg")
    val withoutSg = projectName("without-sg")
    val scriptsProject = projectName("scripts")
    val s = script(scriptsProject, "gen.Tool")

    val plan = SourcegenPlan(
      perProject = Map(withSg -> Set(s)), // withoutSg intentionally absent
      scriptProjectDeps = Map(s -> Set(scriptsProject))
    )

    val dag = TaskDag.buildCompileDag(
      projects = Set(withSg, withoutSg),
      allProjectDeps = Map.empty,
      sourcegen = plan
    )

    val sgTasks = dag.tasks.values.collect { case t: SourcegenTask => t }.toList
    sgTasks should have size 1
    sgTasks.head.forProjects shouldBe Set(withSg)

    val compileTasks = dag.tasks.values.collect { case t: CompileTask => t.project -> t }.toMap
    compileTasks(withSg).dependencies should contain(TaskId.Sourcegen(s))
    // The other project has NO sourcegen dep — its CompileTask looks exactly like a no-sourcegen DAG.
    compileTasks(withoutSg).dependencies shouldBe empty
  }

  test("one target with multiple scripts: one SourcegenTask per unique script") {
    val target = projectName("target")
    val scriptsProject = projectName("scripts")
    val s1 = script(scriptsProject, "gen.First")
    val s2 = script(scriptsProject, "gen.Second")

    val plan = SourcegenPlan(
      perProject = Map(target -> Set(s1, s2)),
      scriptProjectDeps = Map(s1 -> Set(scriptsProject), s2 -> Set(scriptsProject))
    )

    val dag = TaskDag.buildCompileDag(Set(target), Map.empty, plan)

    val sgIds = dag.tasks.values.collect { case t: SourcegenTask => t.id }.toSet
    sgIds shouldBe Set(TaskId.Sourcegen(s1), TaskId.Sourcegen(s2))

    val compileTasks = dag.tasks.values.collect { case t: CompileTask => t.project -> t }.toMap
    compileTasks(target).dependencies should contain allOf (TaskId.Sourcegen(s1), TaskId.Sourcegen(s2))
  }

  test("buildTestDag with sourcegen: script-project and sourcegen tasks included") {
    val testProject = projectName("test-suite")
    val scriptsProject = projectName("scripts")
    val s = script(scriptsProject, "gen.Tool")

    val plan = SourcegenPlan(
      perProject = Map(testProject -> Set(s)),
      scriptProjectDeps = Map(s -> Set(scriptsProject))
    )

    val dag = TaskDag.buildTestDag(
      testProjects = Set(testProject),
      allProjectDeps = Map.empty,
      platforms = Map.empty,
      sourcegen = plan
    )

    dag.tasks.values.collect { case t: SourcegenTask => t } should have size 1
    dag.tasks.values.collect { case t: CompileTask => t.project }.toSet shouldBe Set(testProject, scriptsProject)
  }

  test("buildLinkDag with sourcegen: includes script-project and sourcegen tasks") {
    val linked = projectName("linked")
    val scriptsProject = projectName("scripts")
    val s = script(scriptsProject, "gen.Tool")

    val plan = SourcegenPlan(
      perProject = Map(linked -> Set(s)),
      scriptProjectDeps = Map(s -> Set(scriptsProject))
    )

    val dag = TaskDag.buildLinkDag(
      projects = Set(linked),
      allProjectDeps = Map.empty,
      platforms = Map.empty,
      releaseMode = false,
      sourcegen = plan
    )

    dag.tasks.values.collect { case t: SourcegenTask => t } should have size 1
    dag.tasks.values.collect { case t: CompileTask => t.project }.toSet shouldBe Set(linked, scriptsProject)
  }

  // ==========================================================================
  // Executor Orchestration Tests
  // ==========================================================================

  test("executor: script-project compile runs before sourcegen, sourcegen runs before target compile") {
    val target = projectName("target")
    val scriptsProject = projectName("scripts")
    val s = script(scriptsProject, "gen.Tool")

    val plan = SourcegenPlan(
      perProject = Map(target -> Set(s)),
      scriptProjectDeps = Map(s -> Set(scriptsProject))
    )
    val dag = TaskDag.buildCompileDag(Set(target), Map.empty, plan)

    val order = new ConcurrentLinkedQueue[String]()

    val executor = TaskDag.executor(
      compileHandler = (t, _) => IO(order.add(s"compile:${t.project.value}"): Unit).as(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      sourcegenHandler = (t, _) => IO(order.add(s"sourcegen:${t.script.main}"): Unit).as(TaskResult.Success)
    )

    (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      _ <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield ()).unsafeRunSync()

    val events = order.asScala.toList
    events.indexOf("compile:scripts") should be < events.indexOf("sourcegen:gen.Tool")
    events.indexOf("sourcegen:gen.Tool") should be < events.indexOf("compile:target")
  }

  test("executor: script-project compile failure → sourcegen Skipped → target compile Skipped") {
    val target = projectName("target")
    val scriptsProject = projectName("scripts")
    val s = script(scriptsProject, "gen.Tool")

    val plan = SourcegenPlan(
      perProject = Map(target -> Set(s)),
      scriptProjectDeps = Map(s -> Set(scriptsProject))
    )
    val dag = TaskDag.buildCompileDag(Set(target), Map.empty, plan)

    val sourcegenCalled = new java.util.concurrent.atomic.AtomicBoolean(false)
    val targetCompileCalled = new java.util.concurrent.atomic.AtomicBoolean(false)

    val executor = TaskDag.executor(
      compileHandler = (t, _) =>
        if (t.project == scriptsProject) IO.pure(TaskResult.Failure("compile error", Nil))
        else if (t.project == target) IO(targetCompileCalled.set(true)).as(TaskResult.Success)
        else IO.pure(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      sourcegenHandler = (_, _) => IO(sourcegenCalled.set(true)).as(TaskResult.Success)
    )

    val finalDag = (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      d <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield d).unsafeRunSync()

    sourcegenCalled.get() shouldBe false
    targetCompileCalled.get() shouldBe false
    finalDag.failed should contain(TaskId.Compile(scriptsProject))
    finalDag.skipped should contain(TaskId.Sourcegen(s))
    finalDag.skipped should contain(TaskId.Compile(target))
  }

  test("executor: sourcegen failure → target compile Skipped (script-project compile still completed)") {
    val target = projectName("target")
    val scriptsProject = projectName("scripts")
    val s = script(scriptsProject, "gen.Tool")

    val plan = SourcegenPlan(
      perProject = Map(target -> Set(s)),
      scriptProjectDeps = Map(s -> Set(scriptsProject))
    )
    val dag = TaskDag.buildCompileDag(Set(target), Map.empty, plan)

    val targetCompileCalled = new java.util.concurrent.atomic.AtomicBoolean(false)

    val executor = TaskDag.executor(
      compileHandler = (t, _) =>
        if (t.project == target) IO(targetCompileCalled.set(true)).as(TaskResult.Success)
        else IO.pure(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      sourcegenHandler = (_, _) => IO.pure(TaskResult.Failure("script threw", Nil))
    )

    val finalDag = (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      d <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield d).unsafeRunSync()

    targetCompileCalled.get() shouldBe false
    finalDag.completed should contain(TaskId.Compile(scriptsProject))
    finalDag.failed should contain(TaskId.Sourcegen(s))
    finalDag.skipped should contain(TaskId.Compile(target))
  }

  test("executor: sourcegen up-to-date (handler returns Success immediately) → target compiles normally") {
    val target = projectName("target")
    val scriptsProject = projectName("scripts")
    val s = script(scriptsProject, "gen.Tool")

    val plan = SourcegenPlan(
      perProject = Map(target -> Set(s)),
      scriptProjectDeps = Map(s -> Set(scriptsProject))
    )
    val dag = TaskDag.buildCompileDag(Set(target), Map.empty, plan)

    val targetCompileCalled = new java.util.concurrent.atomic.AtomicBoolean(false)

    val executor = TaskDag.executor(
      compileHandler = (t, _) =>
        if (t.project == target) IO(targetCompileCalled.set(true)).as(TaskResult.Success)
        else IO.pure(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      // Handler returns Success without forking — simulates up-to-date fast path
      sourcegenHandler = (_, _) => IO.pure(TaskResult.Success)
    )

    val finalDag = (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      d <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield d).unsafeRunSync()

    targetCompileCalled.get() shouldBe true
    finalDag.completed should contain allOf (
      TaskId.Compile(scriptsProject),
      TaskId.Sourcegen(s),
      TaskId.Compile(target)
    )
  }

  test("executor: emits SourcegenStarted and SourcegenFinished DagEvents around handler call") {
    val target = projectName("target")
    val scriptsProject = projectName("scripts")
    val s = script(scriptsProject, "gen.Tool")

    val plan = SourcegenPlan(
      perProject = Map(target -> Set(s)),
      scriptProjectDeps = Map(s -> Set(scriptsProject))
    )
    val dag = TaskDag.buildCompileDag(Set(target), Map.empty, plan)

    val executor = TaskDag.executor(
      compileHandler = (_, _) => IO.pure(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      sourcegenHandler = (_, _) => IO.pure(TaskResult.Success)
    )

    val events = (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      _ <- executor.execute(dag, 4, eventQueue, killSignal)
      _ <- eventQueue.offer(None)
      drained <- drainQueue(eventQueue)
    } yield drained).unsafeRunSync()

    val started = events.collect { case e: DagEvent.SourcegenStarted => e }
    val finished = events.collect { case e: DagEvent.SourcegenFinished => e }

    started should have size 1
    started.head.scriptMain shouldBe "gen.Tool"
    started.head.forProjects shouldBe List(target)

    finished should have size 1
    finished.head.success shouldBe true
    finished.head.error shouldBe None
  }

  test("executor: sourcegen failure emits SourcegenFinished with success=false and error message") {
    val target = projectName("target")
    val scriptsProject = projectName("scripts")
    val s = script(scriptsProject, "gen.Tool")

    val plan = SourcegenPlan(
      perProject = Map(target -> Set(s)),
      scriptProjectDeps = Map(s -> Set(scriptsProject))
    )
    val dag = TaskDag.buildCompileDag(Set(target), Map.empty, plan)

    val executor = TaskDag.executor(
      compileHandler = (_, _) => IO.pure(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      sourcegenHandler = (_, _) => IO.pure(TaskResult.Failure("boom", Nil))
    )

    val events = (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      _ <- executor.execute(dag, 4, eventQueue, killSignal)
      _ <- eventQueue.offer(None)
      drained <- drainQueue(eventQueue)
    } yield drained).unsafeRunSync()

    val finished = events.collect { case e: DagEvent.SourcegenFinished => e }
    finished should have size 1
    finished.head.success shouldBe false
    finished.head.error shouldBe Some("boom")
  }

  test("executor: kill during sourcegen → sourcegen Killed and target compile does not run") {
    val target = projectName("target")
    val scriptsProject = projectName("scripts")
    val s = script(scriptsProject, "gen.Tool")

    val plan = SourcegenPlan(
      perProject = Map(target -> Set(s)),
      scriptProjectDeps = Map(s -> Set(scriptsProject))
    )
    val dag = TaskDag.buildCompileDag(Set(target), Map.empty, plan)

    val targetCompileCalled = new java.util.concurrent.atomic.AtomicBoolean(false)

    val executor = TaskDag.executor(
      compileHandler = (t, _) =>
        if (t.project == target) IO(targetCompileCalled.set(true)).as(TaskResult.Success)
        else IO.pure(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      sourcegenHandler = (_, taskKill) =>
        // Block until kill, then return Killed
        taskKill.get.map(reason => TaskResult.Killed(reason))
    )

    val finalDag = (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Deferred[IO, KillReason]
      _ <- (IO.sleep(scala.concurrent.duration.DurationInt(50).millis) >> killSignal.complete(KillReason.UserRequest)).start
      d <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield d).unsafeRunSync()

    targetCompileCalled.get() shouldBe false
    finalDag.killed should contain(TaskId.Sourcegen(s))
    // Target compile is Killed (not Skipped) because executor's kill path marks remaining tasks as Killed
    finalDag.killed should contain(TaskId.Compile(target))
  }

  test("executor: target compile does not start before sourcegen finishes (timeline)") {
    val target = projectName("target")
    val scriptsProject = projectName("scripts")
    val s = script(scriptsProject, "gen.Tool")

    val plan = SourcegenPlan(
      perProject = Map(target -> Set(s)),
      scriptProjectDeps = Map(s -> Set(scriptsProject))
    )
    val dag = TaskDag.buildCompileDag(Set(target), Map.empty, plan)

    val timeline = new ConcurrentLinkedQueue[(String, Long)]()
    def record(tag: String): IO[Unit] = IO(timeline.add((tag, System.nanoTime())): Unit)

    val executor = TaskDag.executor(
      compileHandler = (t, _) => record(s"compile:${t.project.value}").as(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Success, TaskDag.LinkResult.NotApplicable)),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success),
      sourcegenHandler = (_, _) =>
        record("sourcegen:start") >>
          IO.sleep(scala.concurrent.duration.DurationInt(100).millis) >>
          record("sourcegen:end").as(TaskResult.Success)
    )

    (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      _ <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield ()).unsafeRunSync()

    val tags = timeline.asScala.toList.map(_._1)
    tags.indexOf("compile:scripts") should be < tags.indexOf("sourcegen:start")
    tags.indexOf("sourcegen:end") should be < tags.indexOf("compile:target")
  }

  // ==========================================================================
  // SourcegenPlan tests
  // ==========================================================================

  test("SourcegenPlan.empty is empty") {
    SourcegenPlan.empty.isEmpty shouldBe true
    SourcegenPlan.empty.allScripts shouldBe empty
  }

  test("SourcegenPlan.allScripts aggregates across target projects") {
    val scriptsProject = projectName("scripts")
    val s1 = script(scriptsProject, "gen.A")
    val s2 = script(scriptsProject, "gen.B")
    val plan = SourcegenPlan(
      perProject = Map(projectName("p") -> Set(s1), projectName("q") -> Set(s1, s2)),
      scriptProjectDeps = Map(s1 -> Set(scriptsProject), s2 -> Set(scriptsProject))
    )
    plan.allScripts shouldBe Set(s1, s2)
  }

  // ==========================================================================
  // helpers
  // ==========================================================================

  private def drainQueue(queue: Queue[IO, Option[DagEvent]]): IO[List[DagEvent]] = {
    def loop(acc: List[DagEvent]): IO[List[DagEvent]] =
      queue.tryTake.flatMap {
        case Some(Some(event)) => loop(event :: acc)
        case Some(None)        => IO.pure(acc.reverse)
        case None              => IO.pure(acc.reverse)
      }
    loop(Nil)
  }
}
