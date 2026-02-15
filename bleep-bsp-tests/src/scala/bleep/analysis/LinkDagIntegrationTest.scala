package bleep.analysis

import bleep.bsp.{Outcome, TaskDag}
import bleep.bsp.Outcome.KillReason
import bleep.bsp.TaskDag._
import bleep.model.{CrossProjectName, ProjectName}
import cats.effect.{Deferred, IO}
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Integration tests for the Link DAG functionality.
  *
  * Tests:
  *   - DAG construction with compile → link → discover → test dependencies
  *   - Link task insertion for non-JVM platforms
  *   - Task ordering and execution
  *   - JVM projects skip linking
  */
class LinkDagIntegrationTest extends AnyFunSuite with Matchers {

  /** Helper to create CrossProjectName from a simple string. */
  private def projectName(name: String): CrossProjectName =
    CrossProjectName(ProjectName(name), None)

  // ==========================================================================
  // DAG Construction Tests
  // ==========================================================================

  test("buildTestDag: JVM project has compile → discover dependency (no link)") {
    val project = projectName("myapp-jvm")
    val dag = TaskDag.buildTestDag(
      testProjects = Set(project),
      allProjectDeps = Map.empty,
      platforms = Map(project -> LinkPlatform.Jvm)
    )

    dag.tasks should have size 2 // CompileTask + DiscoverTask (no LinkTask)
    dag.tasks.values.collect { case ct: CompileTask => ct } should have size 1
    dag.tasks.values.collect { case dt: DiscoverTask => dt } should have size 1
    dag.tasks.values.collect { case lt: LinkTask => lt } shouldBe empty

    // DiscoverTask should depend on CompileTask
    val discoverTask = dag.tasks.values.collectFirst { case dt: DiscoverTask => dt }.get
    discoverTask.dependencies should contain(s"compile:${project.value}")
  }

  test("buildTestDag: Scala.js project has compile → link → discover dependency") {
    val project = projectName("myapp-js")
    val jsConfig = ScalaJsLinkConfig.Debug
    val platform = LinkPlatform.ScalaJs("1.16.0", "3.3.3", jsConfig)

    val dag = TaskDag.buildTestDag(
      testProjects = Set(project),
      allProjectDeps = Map.empty,
      platforms = Map(project -> platform)
    )

    dag.tasks should have size 3 // CompileTask + LinkTask + DiscoverTask
    dag.tasks.values.collect { case lt: LinkTask => lt } should have size 1

    // LinkTask should depend on CompileTask
    val linkTask = dag.tasks.values.collectFirst { case lt: LinkTask => lt }.get
    linkTask.dependencies should contain(s"compile:${project.value}")
    linkTask.platform shouldBe platform
    linkTask.isTest shouldBe true

    // DiscoverTask should depend on LinkTask
    val discoverTask = dag.tasks.values.collectFirst { case dt: DiscoverTask => dt }.get
    discoverTask.dependencies should contain(s"link:${project.value}")
  }

  test("buildTestDag: Scala Native project has compile → link → discover dependency") {
    val project = projectName("myapp-native")
    val nativeConfig = ScalaNativeLinkConfig.Debug
    val platform = LinkPlatform.ScalaNative("0.5.6", "3.3.3", nativeConfig)

    val dag = TaskDag.buildTestDag(
      testProjects = Set(project),
      allProjectDeps = Map.empty,
      platforms = Map(project -> platform)
    )

    dag.tasks should have size 3
    val linkTask = dag.tasks.values.collectFirst { case lt: LinkTask => lt }.get
    linkTask.platform shouldBe a[LinkPlatform.ScalaNative]
  }

  test("buildTestDag: includes transitive dependencies") {
    val core = projectName("core")
    val app = projectName("app-js")
    val platform = LinkPlatform.ScalaJs("1.16.0", "3.3.3", ScalaJsLinkConfig.Debug)

    val dag = TaskDag.buildTestDag(
      testProjects = Set(app),
      allProjectDeps = Map(
        app -> Set(core),
        core -> Set.empty
      ),
      platforms = Map(app -> platform)
    )

    // Should have compile tasks for both projects
    val compileTasks = dag.tasks.values.collect { case ct: CompileTask => ct }
    compileTasks should have size 2
    compileTasks.map(_.project.value).toSet shouldBe Set("core", "app-js")

    // Link task only for the test project
    val linkTasks = dag.tasks.values.collect { case lt: LinkTask => lt }
    linkTasks should have size 1
    linkTasks.head.project.value shouldBe "app-js"
  }

  test("buildTestDag: mixed platforms") {
    val jvmProject = projectName("app-jvm")
    val jsProject = projectName("app-js")
    val nativeProject = projectName("app-native")

    val dag = TaskDag.buildTestDag(
      testProjects = Set(jvmProject, jsProject, nativeProject),
      allProjectDeps = Map.empty,
      platforms = Map(
        jvmProject -> LinkPlatform.Jvm,
        jsProject -> LinkPlatform.ScalaJs("1.16.0", "3.3.3", ScalaJsLinkConfig.Debug),
        nativeProject -> LinkPlatform.ScalaNative("0.5.6", "3.3.3", ScalaNativeLinkConfig.Debug)
      )
    )

    // 3 compile + 2 link (JS, Native) + 3 discover = 8 tasks
    dag.tasks should have size 8
    dag.tasks.values.collect { case lt: LinkTask => lt } should have size 2
  }

  // ==========================================================================
  // buildLinkDag Tests
  // ==========================================================================

  test("buildLinkDag: creates compile + link tasks only") {
    val project = projectName("myapp-js")
    val platform = LinkPlatform.ScalaJs("1.16.0", "3.3.3", ScalaJsLinkConfig.Debug)

    val dag = TaskDag.buildLinkDag(
      projects = Set(project),
      allProjectDeps = Map.empty,
      platforms = Map(project -> platform),
      releaseMode = false
    )

    dag.tasks should have size 2 // CompileTask + LinkTask (no DiscoverTask)
    dag.tasks.values.collect { case lt: LinkTask => lt } should have size 1
    dag.tasks.values.collect { case dt: DiscoverTask => dt } shouldBe empty

    val linkTask = dag.tasks.values.collectFirst { case lt: LinkTask => lt }.get
    linkTask.releaseMode shouldBe false
  }

  test("buildLinkDag: release mode") {
    val project = projectName("myapp-js")
    val platform = LinkPlatform.ScalaJs("1.16.0", "3.3.3", ScalaJsLinkConfig.Release)

    val dag = TaskDag.buildLinkDag(
      projects = Set(project),
      allProjectDeps = Map.empty,
      platforms = Map(project -> platform),
      releaseMode = true
    )

    val linkTask = dag.tasks.values.collectFirst { case lt: LinkTask => lt }.get
    linkTask.releaseMode shouldBe true
  }

  test("buildLinkDag: JVM project has no link task") {
    val project = projectName("myapp-jvm")

    val dag = TaskDag.buildLinkDag(
      projects = Set(project),
      allProjectDeps = Map.empty,
      platforms = Map(project -> LinkPlatform.Jvm),
      releaseMode = false
    )

    // Only compile task
    dag.tasks should have size 1
    dag.tasks.values.collect { case lt: LinkTask => lt } shouldBe empty
  }

  // ==========================================================================
  // DAG Execution Tests
  // ==========================================================================

  test("executor: link task is called for non-JVM platforms") {
    val project = projectName("myapp-js")
    val platform = LinkPlatform.ScalaJs("1.16.0", "3.3.3", ScalaJsLinkConfig.Debug)

    val dag = TaskDag.buildLinkDag(
      projects = Set(project),
      allProjectDeps = Map.empty,
      platforms = Map(project -> platform),
      releaseMode = false
    )

    var linkCalled = false
    var linkPlatformReceived: Option[LinkPlatform] = None

    val executor = TaskDag.executor(
      compileHandler = (_, _) => IO.pure(TaskResult.Success),
      linkHandler = (lt, _) => {
        linkCalled = true
        linkPlatformReceived = Some(lt.platform)
        IO.pure(
          (
            TaskResult.Success,
            LinkResult.JsSuccess(
              java.nio.file.Path.of("out.js"),
              None,
              Seq.empty,
              wasUpToDate = false
            )
          )
        )
      },
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success)
    )

    val result = (for {
      eventQueue <- Queue.unbounded[IO, DagEvent]
      killSignal <- Outcome.neverKillSignal
      finalDag <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield finalDag).unsafeRunSync()

    linkCalled shouldBe true
    linkPlatformReceived shouldBe Some(platform)
    result.completed should contain(s"link:${project.value}")
  }

  test("executor: emits link events") {
    val project = projectName("myapp-js")
    val platform = LinkPlatform.ScalaJs("1.16.0", "3.3.3", ScalaJsLinkConfig.Debug)

    val dag = TaskDag.buildLinkDag(
      projects = Set(project),
      allProjectDeps = Map.empty,
      platforms = Map(project -> platform),
      releaseMode = false
    )

    val executor = TaskDag.executor(
      compileHandler = (_, _) => IO.pure(TaskResult.Success),
      linkHandler = (_, _) =>
        IO.pure(
          (
            TaskResult.Success,
            LinkResult.JsSuccess(
              java.nio.file.Path.of("out.js"),
              None,
              Seq.empty,
              wasUpToDate = false
            )
          )
        ),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success)
    )

    val events = (for {
      eventQueue <- Queue.unbounded[IO, DagEvent]
      killSignal <- Outcome.neverKillSignal
      _ <- executor.execute(dag, 4, eventQueue, killSignal)
      allEvents <- drainQueue(eventQueue)
    } yield allEvents).unsafeRunSync()

    val linkStarted = events.collect { case e: DagEvent.LinkStarted => e }
    val linkFinished = events.collect { case e: DagEvent.LinkFinished => e }

    linkStarted should have size 1
    linkStarted.head.project shouldBe project.value
    linkStarted.head.platform shouldBe "Scala.js"

    linkFinished should have size 1
    linkFinished.head.result shouldBe a[LinkResult.JsSuccess]
  }

  test("executor: link failure prevents discover") {
    val project = projectName("myapp-js")
    val platform = LinkPlatform.ScalaJs("1.16.0", "3.3.3", ScalaJsLinkConfig.Debug)

    val dag = TaskDag.buildTestDag(
      testProjects = Set(project),
      allProjectDeps = Map.empty,
      platforms = Map(project -> platform)
    )

    val executor = TaskDag.executor(
      compileHandler = (_, _) => IO.pure(TaskResult.Success),
      linkHandler = (_, _) => IO.pure((TaskResult.Failure("Link error", List.empty), LinkResult.Failure("Link error", List.empty))),
      discoverHandler = (_, _) => IO.pure((TaskResult.Success, List.empty)),
      testHandler = (_, _) => IO.pure(TaskResult.Success)
    )

    val result = (for {
      eventQueue <- Queue.unbounded[IO, DagEvent]
      killSignal <- Outcome.neverKillSignal
      finalDag <- executor.execute(dag, 4, eventQueue, killSignal)
    } yield finalDag).unsafeRunSync()

    result.failed should contain(s"link:${project.value}")
    result.skipped should contain(s"discover:${project.value}")
  }

  // ==========================================================================
  // Task ID Tests
  // ==========================================================================

  test("LinkTask: correct task ID format") {
    val task = LinkTask(
      projectName("myapp-js"),
      LinkPlatform.ScalaJs("1.16.0", "3.3.3", ScalaJsLinkConfig.Debug),
      releaseMode = false,
      isTest = false
    )

    task.id shouldBe "link:myapp-js"
  }

  test("LinkPlatform: all platforms have correct types") {
    LinkPlatform.Jvm shouldBe a[LinkPlatform]
    LinkPlatform.ScalaJs("1.16.0", "3.3.3", ScalaJsLinkConfig.Debug) shouldBe a[LinkPlatform.ScalaJs]
    LinkPlatform.ScalaNative("0.5.6", "3.3.3", ScalaNativeLinkConfig.Debug) shouldBe a[LinkPlatform.ScalaNative]
    LinkPlatform.KotlinJs("2.0.0", TaskDag.KotlinJsConfig("commonjs", true, false, java.nio.file.Path.of("."))) shouldBe a[LinkPlatform.KotlinJs]
    LinkPlatform.KotlinNative("2.0.0", TaskDag.KotlinNativeConfig("linux-x64", true, false, false)) shouldBe a[LinkPlatform.KotlinNative]
  }

  // ==========================================================================
  // LinkResult Tests
  // ==========================================================================

  test("LinkResult: all result types") {
    LinkResult
      .JsSuccess(java.nio.file.Path.of("main.js"), Some(java.nio.file.Path.of("main.js.map")), Seq.empty, wasUpToDate = false) shouldBe a[LinkResult.JsSuccess]
    LinkResult.NativeSuccess(java.nio.file.Path.of("app"), wasUpToDate = false) shouldBe a[LinkResult.NativeSuccess]
    LinkResult.Failure("error", List.empty) shouldBe a[LinkResult.Failure]
    LinkResult.Cancelled shouldBe LinkResult.Cancelled
    LinkResult.NotApplicable shouldBe LinkResult.NotApplicable
  }

  // ==========================================================================
  // Helpers
  // ==========================================================================

  private def drainQueue(queue: Queue[IO, DagEvent]): IO[List[DagEvent]] = {
    def loop(acc: List[DagEvent]): IO[List[DagEvent]] =
      queue.tryTake.flatMap {
        case Some(event) => loop(event :: acc)
        case None        => IO.pure(acc.reverse)
      }
    // Small delay to let events accumulate
    IO.sleep(scala.concurrent.duration.Duration(100, scala.concurrent.duration.MILLISECONDS)) >> loop(Nil)
  }
}
