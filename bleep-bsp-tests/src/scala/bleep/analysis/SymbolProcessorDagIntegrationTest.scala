package bleep.analysis

import bleep.bsp.TaskDag
import bleep.bsp.TaskDag.{
  AnnotationProcessorPlan,
  BuildContext,
  CompileTask,
  Handlers,
  RunSymbolProcessorsTask,
  SourcegenPlan,
  SymbolProcessorPlan,
  TaskId,
  TaskResult
}
import bleep.bsp.protocol.KillReason
import bleep.bsp.TaskDag.LinkPlatform
import bleep.model.{CrossProjectName, ProjectName}
import cats.effect.{Deferred, IO}
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

/** DAG-shape and executor wiring tests for KSP processor resolution.
  *
  * Mirrors [[SourcegenDagIntegrationTest]] in spirit: no kotlinc, no Coursier, just the shape of the DAG and the routing through the executor's handlers.
  */
class SymbolProcessorDagIntegrationTest extends AnyFunSuite with Matchers {

  private def projectName(name: String): CrossProjectName =
    CrossProjectName(ProjectName(name), None)

  private def emptyBuildContext(projectDeps: Map[CrossProjectName, Set[CrossProjectName]] = Map.empty): BuildContext =
    BuildContext(
      allProjectDeps = projectDeps,
      platforms = Map.empty,
      sourcegen = SourcegenPlan.empty,
      apPlan = AnnotationProcessorPlan.empty,
      kspPlan = SymbolProcessorPlan.empty
    )

  test("buildCompileDag without KSP plan: no RunSymbolProcessorsTasks") {
    val p = projectName("p")
    val dag = TaskDag.buildCompileDag(Set(p), emptyBuildContext())
    dag.tasks.values.collect { case t: RunSymbolProcessorsTask => t } shouldBe empty
    dag.tasks.values.collect { case t: CompileTask => t } should have size 1
  }

  test("buildCompileDag with KSP plan: per-project task inserted and compile depends on it") {
    val target = projectName("target")
    val ctx = emptyBuildContext().copy(kspPlan = SymbolProcessorPlan(Set(target)))
    val dag = TaskDag.buildCompileDag(Set(target), ctx)

    val kspTasks = dag.tasks.values.collect { case t: RunSymbolProcessorsTask => t }.toList
    kspTasks should have size 1
    kspTasks.head.project shouldBe target
    kspTasks.head.dependencies shouldBe empty // no upstream projects → KSP task has no deps

    val compileTasks = dag.tasks.values.collect { case t: CompileTask => t.project -> t }.toMap
    compileTasks(target).dependencies should contain(TaskId.RunSymbolProcessors(target))
  }

  test("KSP plan only covers some projects: only those compiles depend on KSP resolution") {
    val withKsp = projectName("with-ksp")
    val withoutKsp = projectName("without-ksp")
    val ctx = emptyBuildContext().copy(kspPlan = SymbolProcessorPlan(Set(withKsp)))
    val dag = TaskDag.buildCompileDag(Set(withKsp, withoutKsp), ctx)

    val kspTasks = dag.tasks.values.collect { case t: RunSymbolProcessorsTask => t }.toList
    kspTasks.map(_.project) shouldBe List(withKsp)

    val compileTasks = dag.tasks.values.collect { case t: CompileTask => t.project -> t }.toMap
    compileTasks(withKsp).dependencies should contain(TaskId.RunSymbolProcessors(withKsp))
    compileTasks(withoutKsp).dependencies should not contain TaskId.RunSymbolProcessors(withoutKsp)
  }

  test("buildTestDag and buildLinkDag include KSP tasks") {
    val target = projectName("target")
    val ctx = emptyBuildContext().copy(
      kspPlan = SymbolProcessorPlan(Set(target)),
      platforms = Map(target -> LinkPlatform.Jvm)
    )

    val testDag = TaskDag.buildTestDag(Set(target), ctx)
    testDag.tasks.values.collect { case _: RunSymbolProcessorsTask => () } should have size 1

    val linkDag = TaskDag.buildLinkDag(Set(target), ctx, releaseMode = false)
    linkDag.tasks.values.collect { case _: RunSymbolProcessorsTask => () } should have size 1
  }

  test("executor: RunSymbolProcessorsTask is dispatched to handlers.symbolProcessor and Compile waits") {
    val target = projectName("target")
    val ctx = emptyBuildContext().copy(kspPlan = SymbolProcessorPlan(Set(target)))
    val dag = TaskDag.buildCompileDag(Set(target), ctx)

    val timeline = new ConcurrentLinkedQueue[String]()

    val program = for {
      eventQueue <- Queue.bounded[IO, Option[TaskDag.DagEvent]](1024)
      killSignal <- Deferred[IO, KillReason]
      handlers = Handlers(
        compile = (ct, _) => IO { timeline.add(s"compile:${ct.project.value}"); TaskResult.Success },
        link = (_, _) => sys.error("LinkTask should not appear here"),
        discover = (_, _) => sys.error("DiscoverTask should not appear here"),
        test = (_, _) => sys.error("TestSuiteTask should not appear here"),
        sourcegen = (_, _) => sys.error("SourcegenTask should not appear here"),
        annotationProcessor = (_, _) => sys.error("ResolveAnnotationProcessorsTask should not appear here"),
        symbolProcessor = (kspt, _) => IO { timeline.add(s"ksp:${kspt.project.value}"); (TaskResult.Success, 2) }
      )
      executor = TaskDag.executor(handlers)
      _ <- executor.execute(dag, maxParallelism = 4, eventQueue, killSignal).flatTap(_ => eventQueue.offer(None))
    } yield ()

    program.unsafeRunSync()

    val log = timeline.asScala.toList
    log should contain(s"ksp:${target.value}")
    log should contain(s"compile:${target.value}")
    log.indexOf(s"ksp:${target.value}") should be < log.indexOf(s"compile:${target.value}")
  }

  test("executor: KSP failure causes downstream Compile to be Skipped") {
    val target = projectName("target")
    val ctx = emptyBuildContext().copy(kspPlan = SymbolProcessorPlan(Set(target)))
    val dag = TaskDag.buildCompileDag(Set(target), ctx)

    val compileInvoked = new java.util.concurrent.atomic.AtomicBoolean(false)
    val finishedTasks = new ConcurrentLinkedQueue[(String, TaskResult)]()

    val program = for {
      eventQueue <- Queue.bounded[IO, Option[TaskDag.DagEvent]](1024)
      killSignal <- Deferred[IO, KillReason]
      handlers = Handlers(
        compile = (ct, _) => IO { compileInvoked.set(true); finishedTasks.add(ct.project.value -> TaskResult.Success); TaskResult.Success },
        link = (_, _) => sys.error("LinkTask should not appear here"),
        discover = (_, _) => sys.error("DiscoverTask should not appear here"),
        test = (_, _) => sys.error("TestSuiteTask should not appear here"),
        sourcegen = (_, _) => sys.error("SourcegenTask should not appear here"),
        annotationProcessor = (_, _) => sys.error("ResolveAnnotationProcessorsTask should not appear here"),
        symbolProcessor = (_, _) => IO((TaskResult.Failure("simulated KSP misconfig", Nil), 0))
      )
      executor = TaskDag.executor(handlers)
      finalDag <- executor.execute(dag, maxParallelism = 4, eventQueue, killSignal).flatTap(_ => eventQueue.offer(None))
    } yield finalDag

    val finalDag = program.unsafeRunSync()

    compileInvoked.get() shouldBe false
    finalDag.skipped should contain(TaskId.Compile(target))
    finalDag.failed should contain(TaskId.RunSymbolProcessors(target))
  }

  test("SymbolProcessorPlan.empty and isEmpty / needsResolution") {
    val p = projectName("p")
    SymbolProcessorPlan.empty.isEmpty shouldBe true
    SymbolProcessorPlan.empty.needsResolution(p) shouldBe false

    val populated = SymbolProcessorPlan(Set(p))
    populated.isEmpty shouldBe false
    populated.needsResolution(p) shouldBe true
    populated.needsResolution(projectName("other")) shouldBe false
  }
}
