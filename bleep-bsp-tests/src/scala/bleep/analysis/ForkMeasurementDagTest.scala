package bleep.analysis

import bleep.bsp.TaskDag.*
import bleep.bsp.protocol.KillReason
import bleep.bsp.{GrantedFork, Outcome, TaskDag}
import bleep.machine.{ForkState, SchedulerSnapshot}
import bleep.model.{CrossProjectName, JsonSet, ProjectName, ScriptDef}
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference

/** Every fork the scheduler grants to a DAG task — sourcegen, a Kotlin/Native link, KSP, post-compile, Kotlin/JS and Kotlin/Native test discovery — reports the
  * process it starts, so the scheduler sees it `Starting` with a pid, measures it a second later, and sees it gone when the task ends. Through a cooperative
  * scheduler with this machine's real probes, against temp directories. What runs in the server's own heap — annotation-processor resolution, the Scala.js
  * linker, discovery by reflection — forks nothing: the scheduler counts it as an in-heap task.
  */
class ForkMeasurementDagTest extends AnyFunSuite with Matchers {
  private def projectName(name: String): CrossProjectName = CrossProjectName(ProjectName(name), None)
  private def ctx(
      platforms: Map[CrossProjectName, LinkPlatform],
      sourcegen: SourcegenPlan,
      kspPlan: SymbolProcessorPlan,
      apPlan: AnnotationProcessorPlan,
      postCompile: Map[CrossProjectName, Set[CrossProjectName]],
      deps: Map[CrossProjectName, Set[CrossProjectName]]
  ) =
    BuildContext(
      allProjectDeps = deps,
      platforms = platforms,
      sourcegen = sourcegen,
      apPlan = apPlan,
      kspPlan = kspPlan,
      testProjects = Set.empty,
      postCompile = postCompile
    )

  /** A test DAG's context: one suite-bearing project on `platform` (`None` for the JVM), nothing else. */
  private def testCtx(project: CrossProjectName, platform: Option[LinkPlatform]) =
    BuildContext(
      allProjectDeps = Map(project -> Set.empty),
      platforms = platform.map(project -> _).toMap,
      sourcegen = SourcegenPlan.empty,
      apPlan = AnnotationProcessorPlan.empty,
      kspPlan = SymbolProcessorPlan.empty,
      testProjects = Set(project),
      postCompile = Map.empty
    )

  private val kotlinJs =
    LinkPlatform.KotlinJs(
      "2.0.0",
      KotlinJsConfig(bleep.model.KotlinJsModuleKind.CommonJS, None, true, None, bleep.model.KotlinJsSourceMapEmbedSources.Never, false, false)
    )
  private val kotlinNative = LinkPlatform.KotlinNative("2.0.0", KotlinNativeConfig("linux-x64", true, false, false))
  private val noSuites = DiscoveryResult(Nil, 0, suiteParallelism = None, batches = Nil)

  private def sleeper(ms: Long): Process =
    new ProcessBuilder(
      Path.of(System.getProperty("java.home"), "bin", "java").toString,
      "-cp",
      System.getProperty("java.class.path"),
      "bleep.machine.SleepMain",
      ms.toString
    )
      .redirectErrorStream(true)
      .start()

  /** Runs a real process under the grant for `ms`, as the runners do, reporting it the moment it exists. */
  private def runUnder(grant: GrantedFork, ms: Long): IO[Unit] = IO.blocking {
    val p = sleeper(ms)
    grant.started(p)
    p.waitFor(): Unit
  }

  private val quiet: (CompileTask, Deferred[IO, KillReason]) => IO[TaskResult] = (_, _) => IO.pure(TaskResult.Success)
  private def absent[A](what: String): A = sys.error(s"$what should not appear here")

  /** Runs the DAG while sampling the scheduler every 50 ms; returns the fork states seen, in order, and the final snapshot. */
  private def observe(dag: Dag, handlers: Handlers): (List[ForkState], SchedulerSnapshot) = {
    val (scheduling, channel) = TestScheduling.openCooperative(parallelism = 2)
    val seen = new AtomicReference[List[(Set[Long], ForkState)]](Nil)
    val program = for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      sampler <- (IO.sleep(scala.concurrent.duration.DurationInt(50).millis) >> IO {
        scheduling.snapshot.foreach(snap => snap.state.forks.foreach(f => seen.updateAndGet(acc => acc :+ (f.pids, f.state)): Unit))
      }).foreverM.start
      finalDag <- TaskDag.executor(handlers).execute(dag, channel, ForkHeaps.default, eventQueue, killSignal)
      _ <- sampler.cancel
    } yield finalDag
    val finalDag = program.timeout(scala.concurrent.duration.DurationInt(60).seconds).unsafeRunSync()
    finalDag.failed shouldBe empty
    finalDag.errored shouldBe empty
    // The fork is gone once its task is over: its exit was reported.
    val after = SchedulerFakesEventually.eventually(5000L)(scheduling.snapshot.exists(_.state.forks.isEmpty))
    after shouldBe true
    val states = seen.get()
    withClue(s"states seen: $states") {
      states.exists { case (pids, state) => pids.nonEmpty && state == ForkState.Starting } shouldBe true // reported with a pid, charged at the bound
      states.exists { case (_, state) => state.isInstanceOf[ForkState.Measured] } shouldBe true // measured a second after it started
      // A process measured in the instant it exits reads zero — its pages are gone, its pid not yet — so not every reading is positive, but one must be.
      states.collect { case (_, ForkState.Measured(footprint, _)) => footprint }.exists(_ > 0L) shouldBe true
    }
    channel.close()
    val snap = scheduling.snapshot.get
    scheduling.close()
    (states.map(_._2), snap)
  }

  test("a sourcegen fork is reported, measured and gone") {
    val target = projectName("target")
    val scripts = projectName("scripts")
    val s = ScriptDef.Main(scripts, "gen.Tool", JsonSet.empty, JsonSet.empty)
    val plan = SourcegenPlan(perProject = Map(target -> Set(s)), scriptProjectDeps = Map(s -> Set(scripts)))
    val dag = TaskDag.buildCompileDag(Set(target), ctx(Map.empty, plan, SymbolProcessorPlan.empty, AnnotationProcessorPlan.empty, Map.empty, Map.empty))
    observe(
      dag,
      Handlers(
        compile = quiet,
        postCompile = (_, _, _) => absent("PostCompileTask"),
        link = (_, _, _) => absent("LinkTask"),
        discover = (_, _, _, _) => absent("DiscoverTask"),
        test = (_, _, _) => absent("TestSuiteTask"),
        testBatch = (_, _) => absent("TestBatchTask"),
        sourcegen = (_, grant, _) => runUnder(grant, 2500L).as(TaskResult.Success),
        annotationProcessor = (_, _) => absent("ResolveAnnotationProcessorsTask"),
        symbolProcessor = (_, _, _) => absent("RunSymbolProcessorsTask")
      )
    ): Unit
  }

  test("a KSP fork is reported, measured and gone") {
    val target = projectName("target")
    val dag = TaskDag.buildCompileDag(
      Set(target),
      ctx(Map.empty, SourcegenPlan.empty, SymbolProcessorPlan(Set(target)), AnnotationProcessorPlan.empty, Map.empty, Map.empty)
    )
    observe(
      dag,
      Handlers(
        compile = quiet,
        postCompile = (_, _, _) => absent("PostCompileTask"),
        link = (_, _, _) => absent("LinkTask"),
        discover = (_, _, _, _) => absent("DiscoverTask"),
        test = (_, _, _) => absent("TestSuiteTask"),
        testBatch = (_, _) => absent("TestBatchTask"),
        sourcegen = (_, _, _) => absent("SourcegenTask"),
        annotationProcessor = (_, _) => absent("ResolveAnnotationProcessorsTask"),
        symbolProcessor = (_, grant, _) => runUnder(grant, 2500L).as((TaskResult.Success, 1))
      )
    ): Unit
  }

  test("a Kotlin/Native link fork is reported, measured and gone") {
    val project = projectName("myapp-native")
    val dag = TaskDag.buildLinkDag(
      Set(project),
      ctx(Map(project -> kotlinNative), SourcegenPlan.empty, SymbolProcessorPlan.empty, AnnotationProcessorPlan.empty, Map.empty, Map.empty),
      releaseMode = false
    )
    observe(
      dag,
      Handlers(
        compile = quiet,
        postCompile = (_, _, _) => absent("PostCompileTask"),
        link = (lt, grant, _) =>
          runUnder(TaskGrant.forkFor(grant, s"the Kotlin/Native link of ${lt.project.value}"), 2500L).as((TaskResult.Success, LinkResult.NotApplicable)),
        discover = (_, _, _, _) => absent("DiscoverTask"),
        test = (_, _, _) => absent("TestSuiteTask"),
        testBatch = (_, _) => absent("TestBatchTask"),
        sourcegen = (_, _, _) => absent("SourcegenTask"),
        annotationProcessor = (_, _) => absent("ResolveAnnotationProcessorsTask"),
        symbolProcessor = (_, _, _) => absent("RunSymbolProcessorsTask")
      )
    ): Unit
  }

  test("a post-compile fork is reported, measured and gone") {
    val lib = projectName("lib")
    val input = projectName("input")
    val script = projectName("script")
    val dag = TaskDag.buildCompileDag(
      Set(lib),
      ctx(
        Map.empty,
        SourcegenPlan.empty,
        SymbolProcessorPlan.empty,
        AnnotationProcessorPlan.empty,
        Map(lib -> Set(input, script)),
        Map(lib -> Set.empty, input -> Set.empty, script -> Set.empty)
      )
    )
    observe(
      dag,
      Handlers(
        compile = quiet,
        postCompile = (_, grant, _) => runUnder(grant, 2500L).as(TaskResult.Success),
        link = (_, _, _) => absent("LinkTask"),
        discover = (_, _, _, _) => absent("DiscoverTask"),
        test = (_, _, _) => absent("TestSuiteTask"),
        testBatch = (_, _) => absent("TestBatchTask"),
        sourcegen = (_, _, _) => absent("SourcegenTask"),
        annotationProcessor = (_, _) => absent("ResolveAnnotationProcessorsTask"),
        symbolProcessor = (_, _, _) => absent("RunSymbolProcessorsTask")
      )
    ): Unit
  }

  test("Kotlin/JS test discovery runs node: a fork, reported, measured and gone") {
    val project = projectName("myapp-kjs")
    val dag = TaskDag.buildTestDag(Set(project), testCtx(project, Some(kotlinJs)))
    observe(
      dag,
      Handlers(
        compile = quiet,
        postCompile = (_, _, _) => absent("PostCompileTask"),
        // The Kotlin/JS linker runs in the server's heap; the executor hands it an in-heap grant.
        link =
          (lt, grant, _) => IO(TaskGrant.requireInHeap(grant, s"the Kotlin/JS link of ${lt.project.value}")).as((TaskResult.Success, LinkResult.NotApplicable)),
        discover =
          (dt, _, grant, _) => runUnder(TaskGrant.forkFor(grant, s"Kotlin/JS discovery of ${dt.project.value}"), 2500L).as((TaskResult.Success, noSuites)),
        test = (_, _, _) => absent("TestSuiteTask"),
        testBatch = (_, _) => absent("TestBatchTask"),
        sourcegen = (_, _, _) => absent("SourcegenTask"),
        annotationProcessor = (_, _) => absent("ResolveAnnotationProcessorsTask"),
        symbolProcessor = (_, _, _) => absent("RunSymbolProcessorsTask")
      )
    ): Unit
  }

  test("Kotlin/Native test discovery runs the binary: a fork, reported, measured and gone") {
    val project = projectName("myapp-knative")
    val dag = TaskDag.buildTestDag(Set(project), testCtx(project, Some(kotlinNative)))
    observe(
      dag,
      Handlers(
        compile = quiet,
        postCompile = (_, _, _) => absent("PostCompileTask"),
        // Two forks in sequence, each its own grant: the link's konanc, then the binary listing its suites.
        link = (lt, grant, _) =>
          runUnder(TaskGrant.forkFor(grant, s"the Kotlin/Native link of ${lt.project.value}"), 1500L).as((TaskResult.Success, LinkResult.NotApplicable)),
        discover =
          (dt, _, grant, _) => runUnder(TaskGrant.forkFor(grant, s"Kotlin/Native discovery of ${dt.project.value}"), 2500L).as((TaskResult.Success, noSuites)),
        test = (_, _, _) => absent("TestSuiteTask"),
        testBatch = (_, _) => absent("TestBatchTask"),
        sourcegen = (_, _, _) => absent("SourcegenTask"),
        annotationProcessor = (_, _) => absent("ResolveAnnotationProcessorsTask"),
        symbolProcessor = (_, _, _) => absent("RunSymbolProcessorsTask")
      )
    ): Unit
  }

  /** Watches the scheduler while `taskPrefix` runs: whether it was counted in the heap, and whether any fork existed meanwhile. */
  private def watchInHeap(
      scheduling: bleep.bsp.DaemonScheduling,
      taskPrefix: String,
      inHeapSeen: AtomicReference[Boolean],
      forksSeen: AtomicReference[Boolean]
  ): Unit = {
    val deadline = System.nanoTime() + 3_000_000_000L
    while (System.nanoTime() < deadline && !inHeapSeen.get()) {
      scheduling.snapshot.foreach { snap =>
        if (snap.state.inHeap.exists(_.taskId.value.startsWith(taskPrefix))) inHeapSeen.set(true)
        if (snap.state.forks.nonEmpty) forksSeen.set(true)
      }
      Thread.sleep(20L)
    }
  }

  test("a Scala.js link is in-heap work: no fork, an in-heap slot while it runs") {
    val project = projectName("myapp-js")
    val platform = LinkPlatform.ScalaJs("1.16.0", "3.3.3", ScalaJsLinkConfig.Debug)
    val dag = TaskDag.buildLinkDag(
      Set(project),
      ctx(Map(project -> platform), SourcegenPlan.empty, SymbolProcessorPlan.empty, AnnotationProcessorPlan.empty, Map.empty, Map.empty),
      releaseMode = false
    )
    val (scheduling, channel) = TestScheduling.openCooperative(parallelism = 2)
    val inHeapSeen = new AtomicReference[Boolean](false)
    val forksSeen = new AtomicReference[Boolean](false)
    val handlers = Handlers(
      compile = quiet,
      postCompile = (_, _, _) => absent("PostCompileTask"),
      link = (lt, grant, _) =>
        IO.blocking {
          TaskGrant.requireInHeap(grant, s"the Scala.js link of ${lt.project.value}")
          watchInHeap(scheduling, "link:", inHeapSeen, forksSeen)
          (TaskResult.Success, LinkResult.NotApplicable)
        },
      discover = (_, _, _, _) => absent("DiscoverTask"),
      test = (_, _, _) => absent("TestSuiteTask"),
      testBatch = (_, _) => absent("TestBatchTask"),
      sourcegen = (_, _, _) => absent("SourcegenTask"),
      annotationProcessor = (_, _) => absent("ResolveAnnotationProcessorsTask"),
      symbolProcessor = (_, _, _) => absent("RunSymbolProcessorsTask")
    )
    val program = for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      finalDag <- TaskDag.executor(handlers).execute(dag, channel, ForkHeaps.default, eventQueue, killSignal)
    } yield finalDag
    val finalDag = program.timeout(scala.concurrent.duration.DurationInt(60).seconds).unsafeRunSync()
    finalDag.completed should contain(TaskId.Link(project))
    inHeapSeen.get() shouldBe true
    forksSeen.get() shouldBe false
    channel.close()
    scheduling.close()
  }

  test("JVM test discovery is in-heap work: no fork, an in-heap slot while it runs") {
    val project = projectName("myapp")
    val dag = TaskDag.buildTestDag(Set(project), testCtx(project, None))
    val (scheduling, channel) = TestScheduling.openCooperative(parallelism = 2)
    val inHeapSeen = new AtomicReference[Boolean](false)
    val forksSeen = new AtomicReference[Boolean](false)
    val handlers = Handlers(
      compile = quiet,
      postCompile = (_, _, _) => absent("PostCompileTask"),
      link = (_, _, _) => absent("LinkTask"),
      discover = (dt, _, grant, _) =>
        IO.blocking {
          TaskGrant.requireInHeap(grant, s"JVM discovery of ${dt.project.value}")
          watchInHeap(scheduling, "discover:", inHeapSeen, forksSeen)
          (TaskResult.Success, noSuites)
        },
      test = (_, _, _) => absent("TestSuiteTask"),
      testBatch = (_, _) => absent("TestBatchTask"),
      sourcegen = (_, _, _) => absent("SourcegenTask"),
      annotationProcessor = (_, _) => absent("ResolveAnnotationProcessorsTask"),
      symbolProcessor = (_, _, _) => absent("RunSymbolProcessorsTask")
    )
    val program = for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      finalDag <- TaskDag.executor(handlers).execute(dag, channel, ForkHeaps.default, eventQueue, killSignal)
    } yield finalDag
    val finalDag = program.timeout(scala.concurrent.duration.DurationInt(60).seconds).unsafeRunSync()
    finalDag.completed should contain(TaskId.Discover(project))
    inHeapSeen.get() shouldBe true
    forksSeen.get() shouldBe false
    channel.close()
    scheduling.close()
  }

  test("annotation-processor resolution is in-heap work: no fork, an in-heap slot while it runs") {
    val target = projectName("target")
    val dag = TaskDag.buildCompileDag(
      Set(target),
      ctx(Map.empty, SourcegenPlan.empty, SymbolProcessorPlan.empty, AnnotationProcessorPlan(Set(target)), Map.empty, Map.empty)
    )
    val (scheduling, channel) = TestScheduling.openCooperative(parallelism = 2)
    val inHeapSeen = new AtomicReference[Boolean](false)
    val forksSeen = new AtomicReference[Boolean](false)
    val handlers = Handlers(
      compile = quiet,
      postCompile = (_, _, _) => absent("PostCompileTask"),
      link = (_, _, _) => absent("LinkTask"),
      discover = (_, _, _, _) => absent("DiscoverTask"),
      test = (_, _, _) => absent("TestSuiteTask"),
      testBatch = (_, _) => absent("TestBatchTask"),
      sourcegen = (_, _, _) => absent("SourcegenTask"),
      annotationProcessor = (_, _) =>
        IO.blocking {
          // While this runs, the scheduler counts it as an in-heap task of this request — and has no fork for it.
          val deadline = System.nanoTime() + 3_000_000_000L
          while (System.nanoTime() < deadline && !inHeapSeen.get()) {
            scheduling.snapshot.foreach { snap =>
              if (snap.state.inHeap.exists(_.taskId.value.startsWith("resolve-ap:"))) inHeapSeen.set(true)
              if (snap.state.forks.nonEmpty) forksSeen.set(true)
            }
            Thread.sleep(20L)
          }
          (TaskResult.Success, 0)
        },
      symbolProcessor = (_, _, _) => absent("RunSymbolProcessorsTask")
    )
    val program = for {
      eventQueue <- Queue.unbounded[IO, Option[DagEvent]]
      killSignal <- Outcome.neverKillSignal
      finalDag <- TaskDag.executor(handlers).execute(dag, channel, ForkHeaps.default, eventQueue, killSignal)
    } yield finalDag
    val finalDag = program.timeout(scala.concurrent.duration.DurationInt(60).seconds).unsafeRunSync()
    finalDag.completed should contain(TaskId.ResolveAnnotationProcessors(target))
    inHeapSeen.get() shouldBe true
    forksSeen.get() shouldBe false
    channel.close()
    scheduling.close()
  }
}

/** Poll until a condition holds. */
private object SchedulerFakesEventually {
  def eventually(timeoutMs: Long)(condition: => Boolean): Boolean = {
    val deadline = System.nanoTime() + timeoutMs * 1_000_000L
    var ok = condition
    while (!ok && System.nanoTime() < deadline) {
      Thread.sleep(20L)
      ok = condition
    }
    ok
  }
}
