package bleep.analysis

import bleep.bsp.{Outcome, TaskDag}
import bleep.bsp.TaskDag.{
  AnnotationProcessorPlan,
  BuildContext,
  CompileTask,
  DagEvent,
  Handlers,
  PostCompileTask,
  SourcegenPlan,
  SymbolProcessorPlan,
  TaskId,
  TaskResult
}
import bleep.model.{CrossProjectName, ProjectName}
import cats.effect.{Deferred, IO}
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.ConcurrentLinkedQueue
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

/** A `postCompile` step is a task of its own: what it reads holds back the step, not the compile before it, and consumers wait for the step.
  *
  * Before, the step ran inside the compile task and its inputs were the compile's dependencies, so a library patched with another library's classes did not
  * start compiling until that other library had finished — though it only reads them after.
  */
class PostCompileDagTest extends AnyFunSuite with Matchers with org.scalatest.LoneElement {

  private def p(name: String): CrossProjectName = CrossProjectName(ProjectName(name), None)

  private val lib = p("lib") // patched after compiling
  private val input = p("input") // read by the patch
  private val script = p("script") // the patch
  private val app = p("app") // depends on lib

  private val ctx = BuildContext(
    allProjectDeps = Map(app -> Set(lib), lib -> Set.empty, input -> Set.empty, script -> Set.empty),
    platforms = Map.empty,
    sourcegen = SourcegenPlan.empty,
    apPlan = AnnotationProcessorPlan.empty,
    kspPlan = SymbolProcessorPlan.empty,
    testProjects = Set.empty,
    postCompile = Map(lib -> Set(input, script))
  )

  private def machine: bleep.MachineResources =
    bleep.MachineResources.create(totalCpu = 4, totalMemoryMb = 64 * 1024, logger = ryddig.TypedLogger.DevNull, longWaitWarnMs = 60000L)

  test("the compile waits only for its own dependencies; the step waits for the compile and what it reads; consumers wait for the step") {
    val dag = TaskDag.buildCompileDag(Set(app), ctx)

    // what the step reads is in the build even though nothing compiles against it
    dag.tasks.keySet should contain allOf (TaskId.Compile(input), TaskId.Compile(script))

    val compile = dag.tasks.values.collect { case t: CompileTask if t.project == lib => t }.loneElement
    compile.id shouldBe TaskId.CompilerOutput(lib)
    compile.dependencies shouldBe empty

    val step = dag.tasks(TaskId.Compile(lib)).asInstanceOf[PostCompileTask]
    step.dependencies shouldBe Set[TaskId](TaskId.CompilerOutput(lib), TaskId.Compile(input), TaskId.Compile(script))

    dag.tasks(TaskId.Compile(app)).dependencies shouldBe Set[TaskId](TaskId.Compile(lib))
  }

  test("the compile runs while what the step reads is still compiling") {
    val dag = TaskDag.buildCompileDag(Set(app), ctx)
    val order = new ConcurrentLinkedQueue[String]()

    val program = for {
      // `input` cannot finish before `lib` has compiled: if `lib`'s compile waited for `input`, this would never complete
      libCompiled <- Deferred[IO, Unit]
      executor = TaskDag.executor(
        Handlers(
          compile = (t, _) =>
            (if (t.project == input) libCompiled.get else IO.unit) >>
              IO(order.add(s"compile:${t.project.value}"): Unit) >>
              (if (t.project == lib) libCompiled.complete(()).void else IO.unit).as(TaskResult.Success),
          postCompile = (t, _) => IO(order.add(s"post-compile:${t.project.value}"): Unit).as(TaskResult.Success),
          link = (_, _) => sys.error("LinkTask should not appear here"),
          discover = (_, _, _) => sys.error("DiscoverTask should not appear here"),
          test = (_, _, _) => sys.error("TestSuiteTask should not appear here"),
          testBatch = (_, _) => sys.error("TestBatchTask should not appear here"),
          sourcegen = (_, _) => sys.error("SourcegenTask should not appear here"),
          annotationProcessor = (_, _) => sys.error("ResolveAnnotationProcessorsTask should not appear here"),
          symbolProcessor = (_, _) => sys.error("RunSymbolProcessorsTask should not appear here"),
          mayAdmitCompile = _ => IO.pure(true)
        )
      )
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      finalDag <- executor.execute(dag, machine, TaskDag.ForkHeaps.default, eventQueue, killSignal)
    } yield finalDag

    val finalDag = program.timeout(30.seconds).unsafeRunSync()
    finalDag.failed shouldBe empty

    val events = order.asScala.toList
    events.indexOf("compile:lib") should be < events.indexOf("compile:input")
    events.indexOf("compile:input") should be < events.indexOf("post-compile:lib")
    events.indexOf("post-compile:lib") should be < events.indexOf("compile:app")
  }
}
