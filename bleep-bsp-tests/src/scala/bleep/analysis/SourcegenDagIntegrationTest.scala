package bleep.analysis

import bleep.bsp.{Outcome, TaskDag}
import bleep.bsp.Outcome.KillReason
import bleep.bsp.TaskDag.{AnnotationProcessorPlan, BuildContext, CompileTask, DagEvent, Handlers, LinkResult, SourcegenPlan, SourcegenTask, TaskId, TaskResult}
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
  * Covers DAG shape, executor orchestration, failure propagation, cancellation, fast paths, and event emission. Tests stub out every task type's handler
  * explicitly — there is no `Handlers.noop` factory because it would let a test silently miss the case where the executor schedules a new kind of task it
  * didn't expect.
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
    val dag = TaskDag.buildCompileDag(
      Set(p),
      BuildContext(allProjectDeps = Map.empty, platforms = Map.empty, sourcegen = SourcegenPlan.empty, apPlan = AnnotationProcessorPlan.empty)
    )
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
      Set(target),
      BuildContext(allProjectDeps = Map.empty, platforms = Map.empty, sourcegen = plan, apPlan = AnnotationProcessorPlan.empty)
    )

    val sgTasks = dag.tasks.values.collect { case t: SourcegenTask => t }.toList
    sgTasks should have size 1
    sgTasks.head.script shouldBe s
    sgTasks.head.forProjects shouldBe Set(target)

    val compileTasks = dag.tasks.values.collect { case t: CompileTask => t.project -> t }.toMap
    compileTasks.keySet shouldBe Set(target, scriptsProject)
    compileTasks(target).dependencies should contain(TaskId.Sourcegen(s))
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
      Set(target),
      BuildContext(
        allProjectDeps = Map(scriptsProject -> Set(scriptsLib), scriptsLib -> Set.empty),
        platforms = Map.empty,
        sourcegen = plan,
        apPlan = AnnotationProcessorPlan.empty
      )
    )

    val sgTask = dag.tasks.values.collectFirst { case t: SourcegenTask => t }.get
    sgTask.dependencies shouldBe Set(
      TaskId.Compile(scriptsProject),
      TaskId.Compile(scriptsLib)
    )

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
      Set(a, b),
      BuildContext(allProjectDeps = Map.empty, platforms = Map.empty, sourcegen = plan, apPlan = AnnotationProcessorPlan.empty)
    )

    val sgTasks = dag.tasks.values.collect { case t: SourcegenTask => t }.toList
    sgTasks should have size 1
    sgTasks.head.forProjects shouldBe Set(a, b)

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
      Set(withSg, withoutSg),
      BuildContext(allProjectDeps = Map.empty, platforms = Map.empty, sourcegen = plan, apPlan = AnnotationProcessorPlan.empty)
    )

    val sgTasks = dag.tasks.values.collect { case t: SourcegenTask => t }.toList
    sgTasks should have size 1
    sgTasks.head.forProjects shouldBe Set(withSg)

    val compileTasks = dag.tasks.values.collect { case t: CompileTask => t.project -> t }.toMap
    compileTasks(withSg).dependencies should contain(TaskId.Sourcegen(s))
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

    val dag = TaskDag.buildCompileDag(
      Set(target),
      BuildContext(allProjectDeps = Map.empty, platforms = Map.empty, sourcegen = plan, apPlan = AnnotationProcessorPlan.empty)
    )

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
      Set(testProject),
      BuildContext(allProjectDeps = Map.empty, platforms = Map.empty, sourcegen = plan, apPlan = AnnotationProcessorPlan.empty)
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
      Set(linked),
      BuildContext(allProjectDeps = Map.empty, platforms = Map.empty, sourcegen = plan, apPlan = AnnotationProcessorPlan.empty),
      releaseMode = false
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
    val dag = TaskDag.buildCompileDag(
      Set(target),
      BuildContext(allProjectDeps = Map.empty, platforms = Map.empty, sourcegen = plan, apPlan = AnnotationProcessorPlan.empty)
    )

    val order = new ConcurrentLinkedQueue[String]()

    val executor = TaskDag.executor(
      Handlers(
        compile = (t, _) => IO(order.add(s"compile:${t.project.value}"): Unit).as(TaskResult.Success),
        link = (_, _) => sys.error("LinkTask should not appear here"),
        discover = (_, _) => sys.error("DiscoverTask should not appear here"),
        test = (_, _) => sys.error("TestSuiteTask should not appear here"),
        sourcegen = (t, _) => IO(order.add(s"sourcegen:${t.script.main}"): Unit).as(TaskResult.Success),
        annotationProcessor = (_, _) => sys.error("ResolveAnnotationProcessorsTask should not appear here")
      )
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
    val dag = TaskDag.buildCompileDag(
      Set(target),
      BuildContext(allProjectDeps = Map.empty, platforms = Map.empty, sourcegen = plan, apPlan = AnnotationProcessorPlan.empty)
    )

    val sourcegenCalled = new java.util.concurrent.atomic.AtomicBoolean(false)
    val targetCompileCalled = new java.util.concurrent.atomic.AtomicBoolean(false)

    val executor = TaskDag.executor(
      Handlers(
        compile = (t, _) =>
          if (t.project == scriptsProject) IO.pure(TaskResult.Failure("compile error", Nil))
          else if (t.project == target) IO(targetCompileCalled.set(true)).as(TaskResult.Success)
          else IO.pure(TaskResult.Success),
        link = (_, _) => sys.error("LinkTask should not appear here"),
        discover = (_, _) => sys.error("DiscoverTask should not appear here"),
        test = (_, _) => sys.error("TestSuiteTask should not appear here"),
        sourcegen = (_, _) => IO(sourcegenCalled.set(true)).as(TaskResult.Success),
        annotationProcessor = (_, _) => sys.error("ResolveAnnotationProcessorsTask should not appear here")
      )
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
    val dag = TaskDag.buildCompileDag(
      Set(target),
      BuildContext(allProjectDeps = Map.empty, platforms = Map.empty, sourcegen = plan, apPlan = AnnotationProcessorPlan.empty)
    )

    val targetCompileCalled = new java.util.concurrent.atomic.AtomicBoolean(false)

    val executor = TaskDag.executor(
      Handlers(
        compile = (t, _) =>
          if (t.project == target) IO(targetCompileCalled.set(true)).as(TaskResult.Success)
          else IO.pure(TaskResult.Success),
        link = (_, _) => sys.error("LinkTask should not appear here"),
        discover = (_, _) => sys.error("DiscoverTask should not appear here"),
        test = (_, _) => sys.error("TestSuiteTask should not appear here"),
        sourcegen = (_, _) => IO.pure(TaskResult.Failure("script threw", Nil)),
        annotationProcessor = (_, _) => sys.error("ResolveAnnotationProcessorsTask should not appear here")
      )
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
    val dag = TaskDag.buildCompileDag(
      Set(target),
      BuildContext(allProjectDeps = Map.empty, platforms = Map.empty, sourcegen = plan, apPlan = AnnotationProcessorPlan.empty)
    )

    val targetCompileCalled = new java.util.concurrent.atomic.AtomicBoolean(false)

    val executor = TaskDag.executor(
      Handlers(
        compile = (t, _) =>
          if (t.project == target) IO(targetCompileCalled.set(true)).as(TaskResult.Success)
          else IO.pure(TaskResult.Success),
        link = (_, _) => sys.error("LinkTask should not appear here"),
        discover = (_, _) => sys.error("DiscoverTask should not appear here"),
        test = (_, _) => sys.error("TestSuiteTask should not appear here"),
        sourcegen = (_, _) => IO.pure(TaskResult.Success), // up-to-date fast path
        annotationProcessor = (_, _) => sys.error("ResolveAnnotationProcessorsTask should not appear here")
      )
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
    val dag = TaskDag.buildCompileDag(
      Set(target),
      BuildContext(allProjectDeps = Map.empty, platforms = Map.empty, sourcegen = plan, apPlan = AnnotationProcessorPlan.empty)
    )

    val executor = TaskDag.executor(
      Handlers(
        compile = (_, _) => IO.pure(TaskResult.Success),
        link = (_, _) => sys.error("LinkTask should not appear here"),
        discover = (_, _) => sys.error("DiscoverTask should not appear here"),
        test = (_, _) => sys.error("TestSuiteTask should not appear here"),
        sourcegen = (_, _) => IO.pure(TaskResult.Success),
        annotationProcessor = (_, _) => sys.error("ResolveAnnotationProcessorsTask should not appear here")
      )
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
    val dag = TaskDag.buildCompileDag(
      Set(target),
      BuildContext(allProjectDeps = Map.empty, platforms = Map.empty, sourcegen = plan, apPlan = AnnotationProcessorPlan.empty)
    )

    val executor = TaskDag.executor(
      Handlers(
        compile = (_, _) => IO.pure(TaskResult.Success),
        link = (_, _) => sys.error("LinkTask should not appear here"),
        discover = (_, _) => sys.error("DiscoverTask should not appear here"),
        test = (_, _) => sys.error("TestSuiteTask should not appear here"),
        sourcegen = (_, _) => IO.pure(TaskResult.Failure("boom", Nil)),
        annotationProcessor = (_, _) => sys.error("ResolveAnnotationProcessorsTask should not appear here")
      )
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
    val dag = TaskDag.buildCompileDag(
      Set(target),
      BuildContext(allProjectDeps = Map.empty, platforms = Map.empty, sourcegen = plan, apPlan = AnnotationProcessorPlan.empty)
    )

    val targetCompileCalled = new java.util.concurrent.atomic.AtomicBoolean(false)

    val executor = TaskDag.executor(
      Handlers(
        compile = (t, _) =>
          if (t.project == target) IO(targetCompileCalled.set(true)).as(TaskResult.Success)
          else IO.pure(TaskResult.Success),
        link = (_, _) => sys.error("LinkTask should not appear here"),
        discover = (_, _) => sys.error("DiscoverTask should not appear here"),
        test = (_, _) => sys.error("TestSuiteTask should not appear here"),
        sourcegen = (_, taskKill) => taskKill.get.map(reason => TaskResult.Killed(reason)),
        annotationProcessor = (_, _) => sys.error("ResolveAnnotationProcessorsTask should not appear here")
      )
    )

    val finalDag = (for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Deferred[IO, KillReason]
      _ <- (IO.sleep(scala.concurrent.duration.DurationInt(50).millis) >> killSignal.complete(KillReason.UserRequest)).start
      d <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield d).unsafeRunSync()

    targetCompileCalled.get() shouldBe false
    finalDag.killed should contain(TaskId.Sourcegen(s))
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
    val dag = TaskDag.buildCompileDag(
      Set(target),
      BuildContext(allProjectDeps = Map.empty, platforms = Map.empty, sourcegen = plan, apPlan = AnnotationProcessorPlan.empty)
    )

    val timeline = new ConcurrentLinkedQueue[(String, Long)]()
    def record(tag: String): IO[Unit] = IO(timeline.add((tag, System.nanoTime())): Unit)

    val executor = TaskDag.executor(
      Handlers(
        compile = (t, _) => record(s"compile:${t.project.value}").as(TaskResult.Success),
        link = (_, _) => sys.error("LinkTask should not appear here"),
        discover = (_, _) => sys.error("DiscoverTask should not appear here"),
        test = (_, _) => sys.error("TestSuiteTask should not appear here"),
        sourcegen = (_, _) =>
          record("sourcegen:start") >>
            IO.sleep(scala.concurrent.duration.DurationInt(100).millis) >>
            record("sourcegen:end").as(TaskResult.Success),
        annotationProcessor = (_, _) => sys.error("ResolveAnnotationProcessorsTask should not appear here")
      )
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
