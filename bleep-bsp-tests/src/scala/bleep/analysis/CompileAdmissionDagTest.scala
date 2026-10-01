package bleep.analysis

import bleep.bsp.{Outcome, TaskDag}
import bleep.bsp.TaskDag.{AnnotationProcessorPlan, BuildContext, CompileAdmission, DagEvent, Handlers, SourcegenPlan, SymbolProcessorPlan, TaskResult}
import bleep.model.{CrossProjectName, ProjectName}
import cats.effect.{Deferred, IO, Ref}
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*

/** A compile the heap gate defers is looked at again when its stagger is up.
  *
  * It used to be reconsidered only when some task completed. Two compiles ready at once had the second staggered by 200 ms, and it then waited for the first to
  * finish — 40 s, for a large library — so the two ran one after the other.
  */
class CompileAdmissionDagTest extends AnyFunSuite with Matchers {

  private def p(name: String): CrossProjectName = CrossProjectName(ProjectName(name), None)

  test("a deferred compile starts after its stagger, while the compile that was running keeps running") {
    val long = p("long")
    val staggered = p("staggered")
    val dag = TaskDag.buildCompileDag(
      Set(long, staggered),
      BuildContext(
        allProjectDeps = Map(long -> Set.empty, staggered -> Set.empty),
        platforms = Map.empty,
        sourcegen = SourcegenPlan.empty,
        apPlan = AnnotationProcessorPlan.empty,
        kspPlan = SymbolProcessorPlan.empty,
        testProjects = Set.empty,
        postCompile = Map.empty
      )
    )
    val machine = bleep.MachineResources.create(totalCpu = 4, totalMemoryMb = 64 * 1024, logger = ryddig.TypedLogger.DevNull, longWaitWarnMs = 60000L)

    val program = for {
      // `long` cannot finish before `staggered` has started: if `staggered` waited for a completion to be reconsidered, this would never end
      staggeredStarted <- Deferred[IO, Unit]
      deferredOnce <- Ref.of[IO, Boolean](false)
      executor = TaskDag.executor(
        Handlers(
          compile = (t, _) =>
            if (t.project == long) staggeredStarted.get.as(TaskResult.Success)
            else staggeredStarted.complete(()).as(TaskResult.Success),
          postCompile = (_, _) => IO.raiseError(new IllegalStateException("no post-compile step in this build")),
          link = (_, _) => sys.error("LinkTask should not appear here"),
          discover = (_, _, _) => sys.error("DiscoverTask should not appear here"),
          test = (_, _, _) => sys.error("TestSuiteTask should not appear here"),
          testBatch = (_, _) => sys.error("TestBatchTask should not appear here"),
          sourcegen = (_, _) => sys.error("SourcegenTask should not appear here"),
          annotationProcessor = (_, _) => sys.error("ResolveAnnotationProcessorsTask should not appear here"),
          symbolProcessor = (_, _) => sys.error("RunSymbolProcessorsTask should not appear here"),
          // the gate staggers `staggered` once, as it does the second of two compiles ready together
          mayAdmitCompile = t =>
            if (t.project == staggered) deferredOnce.getAndSet(true).map(already => if (already) CompileAdmission.Admit else CompileAdmission.Defer(50.millis))
            else IO.pure(CompileAdmission.Admit)
        )
      )
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      finalDag <- executor.execute(dag, machine, TaskDag.ForkHeaps.default, eventQueue, killSignal)
      wasDeferred <- deferredOnce.get
    } yield (finalDag, wasDeferred)

    val (finalDag, wasDeferred) = program.timeout(30.seconds).unsafeRunSync()
    wasDeferred shouldBe true
    finalDag.failed shouldBe empty
    finalDag.completed.size shouldBe 2
  }
}
