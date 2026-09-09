package bleep.analysis

import bleep.bsp.{Outcome, TaskDag}
import bleep.bsp.TaskDag.{TaskId, _}
import bleep.model.{CrossProjectName, ProjectName, SuiteName}
import bleep.testing.FrameworkSelection
import cats.effect.{IO, Ref}
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** `maxConcurrentSuites` turns a project's discovered suites into round-robin chains of ordering-only `runAfter` edges. The properties that matter:
  *
  *   - at bound 1 the suites run strictly sequentially in alphabetical order — surefire's usual class order, which schema-bootstrapping test setups rely on
  *   - a failing suite does NOT skip the rest of its chain: `runAfter` is ordering, not failure propagation
  *   - without a bound no chains exist and every suite is schedulable at once
  */
class MaxConcurrentSuitesDagTest extends AnyFunSuite with Matchers {

  private def testMachine(cpu: Int): bleep.MachineResources =
    bleep.MachineResources.create(totalCpu = cpu, totalMemoryMb = 64 * 1024, logger = ryddig.TypedLogger.DevNull, longWaitWarnMs = 60000L)

  private def projectName(name: String): CrossProjectName =
    CrossProjectName(ProjectName(name), None)

  private val selection = FrameworkSelection.JUnitPlatform("junit")

  /** Runs a one-project test DAG whose discovery yields `suiteNames` (deliberately unsorted) under the given parallelism bound. Returns the interleaved
    * start/finish event log and the final dag.
    */
  private def run(
      project: CrossProjectName,
      suiteNames: List[String],
      parallelism: Option[Int],
      failingSuites: Set[String]
  ): (List[String], Dag) = {
    val dag = TaskDag.buildTestDag(
      Set(project),
      BuildContext(
        allProjectDeps = Map.empty,
        platforms = Map(project -> LinkPlatform.Jvm),
        sourcegen = SourcegenPlan.empty,
        apPlan = AnnotationProcessorPlan.empty,
        kspPlan = SymbolProcessorPlan.empty,
        testProjects = Set(project)
      )
    )

    (for {
      log <- Ref.of[IO, List[String]](Nil)
      executor = TaskDag.executor(
        Handlers(
          mayAdmitCompile = _ => IO.pure(true),
          compile = (_, _) => IO.pure(TaskResult.Success),
          link = (_, _) => sys.error("no link on JVM"),
          discover = (_, _, _) =>
            IO.pure(
              (
                TaskResult.Success,
                TaskDag.DiscoveryResult(
                  suiteNames.map(_ -> selection),
                  suiteNames.size,
                  isTestProject = true,
                  suiteParallelism = parallelism,
                  batches = Nil
                )
              )
            ),
          test = (task, _, _) =>
            for {
              _ <- log.update(_ :+ s"start:${task.suiteName.value}")
              // yield so that, were another suite schedulable, it could interleave between our start and finish
              _ <- IO.cede
              _ <- log.update(_ :+ s"finish:${task.suiteName.value}")
            } yield if (failingSuites(task.suiteName.value)) TaskResult.Failure(s"${task.suiteName.value} failed", Nil) else TaskResult.Success,
          testBatch = (_, _) => sys.error("no test batch here"),
          sourcegen = (_, _) => sys.error("no sourcegen here"),
          annotationProcessor = (_, _) => sys.error("no annotation processors here"),
          symbolProcessor = (_, _) => sys.error("no symbol processors here")
        )
      )
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      finalDag <- executor.execute(dag, testMachine(4), TaskDag.ForkHeaps.default, eventQueue, killSignal)
      events <- log.get
    } yield (events, finalDag)).unsafeRunSync()
  }

  test("suiteParallelism=1: suites run sequentially in alphabetical order") {
    val project = projectName("app-test")
    val (events, dag) = run(project, List("b.Suite", "c.Suite", "a.Suite"), parallelism = Some(1), failingSuites = Set.empty)

    events shouldBe List("start:a.Suite", "finish:a.Suite", "start:b.Suite", "finish:b.Suite", "start:c.Suite", "finish:c.Suite")
    dag.completed should contain allOf (
      TaskId.Test(project, SuiteName("a.Suite")),
      TaskId.Test(project, SuiteName("b.Suite")),
      TaskId.Test(
        project,
        SuiteName("c.Suite")
      )
    )
  }

  test("suiteParallelism=1: a failing suite does not skip the rest of the chain") {
    val project = projectName("app-test")
    val (events, dag) = run(project, List("a.Suite", "b.Suite", "c.Suite"), parallelism = Some(1), failingSuites = Set("a.Suite"))

    events shouldBe List("start:a.Suite", "finish:a.Suite", "start:b.Suite", "finish:b.Suite", "start:c.Suite", "finish:c.Suite")
    dag.failed should contain(TaskId.Test(project, SuiteName("a.Suite")))
    dag.skipped shouldBe empty
    dag.completed should contain allOf (TaskId.Test(project, SuiteName("b.Suite")), TaskId.Test(project, SuiteName("c.Suite")))
  }

  test("suiteParallelism=2: each suite waits only for the one two places ahead of it") {
    val project = projectName("app-test")
    val (events, dag) = run(project, List("a.Suite", "b.Suite", "c.Suite"), parallelism = Some(2), failingSuites = Set.empty)

    // c chains after a (index 2 waits for index 2-2=0); b is unchained
    events.indexOf("start:c.Suite") should be > events.indexOf("finish:a.Suite")
    dag.completed.count { case TaskId.Test(_, _) => true; case _ => false } shouldBe 3
  }

  test("no suiteParallelism: no ordering edges, all suites complete") {
    val project = projectName("app-test")
    val (events, dag) = run(project, List("b.Suite", "a.Suite"), parallelism = None, failingSuites = Set.empty)

    events should have size 4
    dag.completed should contain allOf (TaskId.Test(project, SuiteName("a.Suite")), TaskId.Test(project, SuiteName("b.Suite")))
    dag.tasks.values.collect { case t: TestSuiteTask => t }.foreach { t =>
      t.runAfter shouldBe empty
    }
  }
}
