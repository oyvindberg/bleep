package bleep.analysis

import bleep.bsp.TaskDag.*
import bleep.bsp.{LinkExecutor, Outcome, TaskDag}
import bleep.machine.ForkState
import bleep.model.{CrossProjectName, ProjectName}
import cats.effect.IO
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files
import java.util.concurrent.atomic.AtomicReference

/** A real Scala Native link through the DAG: the clang and lld its toolchain spawns are children of this JVM, attributed to the link's grant by the work
  * directory they name, reported, measured, and gone with the task (design §5.3). Through a cooperative scheduler with this machine's real probes.
  */
class ScalaNativeChildrenDagTest extends AnyFunSuite with Matchers with PlatformTestHelper {

  /** Enough of the Scala library — collections, strings, sorting, formatting — that the toolchain's final link holds still for a few seconds: a hello world
    * links in under a second, too briefly for the one-second measurement, with clang churn right up to the end.
    */
  private val scalaSource =
    """package example
      |
      |object Main {
      |  final case class Row(name: String, weight: Double, tags: List[String])
      |  def main(args: Array[String]): Unit = {
      |    val rows = (1 to 500).map(i => Row(s"row-$i", i * 1.5, List.fill(i % 7)(s"t$i"))).toVector
      |    val grouped = rows.groupBy(_.tags.size).view.mapValues(_.map(_.weight).sum).toMap
      |    val sorted = rows.sortBy(r => (-r.weight, r.name)).take(20).map(r => f"${r.name}%-10s ${r.weight}%8.2f").mkString("\n")
      |    val set = scala.collection.immutable.TreeSet.from(rows.flatMap(_.tags))
      |    println(s"ChildrenObserved ${grouped.size} ${sorted.length} ${set.size} ${rows.map(_.name).mkString(",").hashCode}")
      |  }
      |}
      |""".stripMargin

  test("a Scala Native link's clang and lld are observed under its grant, measured, and gone with the task") {
    withTempDir("sn-children") { tempDir =>
      val srcDir = tempDir.resolve("src")
      writeScalaSource(srcDir, "example", "Main.scala", scalaSource)
      val classpath = compileForScalaNative(srcDir, tempDir.resolve("classes"), DefaultScalaVersion, DefaultScalaNativeVersion)
      val outputDir = Files.createDirectories(tempDir.resolve("link-output"))

      val project = CrossProjectName(ProjectName("app"), None)
      val platform = LinkPlatform.ScalaNative(DefaultScalaNativeVersion, DefaultScalaVersion, ScalaNativeLinkConfig.ReleaseFull)
      val dag = TaskDag.buildLinkDag(
        Set(project),
        BuildContext(
          allProjectDeps = Map(project -> Set.empty),
          platforms = Map(project -> platform),
          sourcegen = SourcegenPlan.empty,
          apPlan = AnnotationProcessorPlan.empty,
          kspPlan = SymbolProcessorPlan.empty,
          testProjects = Set.empty,
          postCompile = Map.empty
        ),
        releaseMode = false
      )

      val (scheduling, channel) = TestScheduling.openCooperative(parallelism = 2)
      val seen = new AtomicReference[List[(Set[Long], ForkState)]](Nil)
      def absent[A](what: String): A = sys.error(s"$what should not appear here")
      val handlers = Handlers(
        compile = (_, _) => IO.pure(TaskResult.Success),
        postCompile = (_, _, _) => absent("PostCompileTask"),
        link = (lt, grant, kill) => LinkExecutor.execute(lt, classpath, Some("example.Main"), outputDir, LinkExecutor.LinkLogger.Silent, kill, grant),
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
        sampler <- (IO.sleep(scala.concurrent.duration.DurationInt(50).millis) >> IO {
          scheduling.snapshot.foreach(snap => snap.state.forks.foreach(f => seen.updateAndGet(acc => acc :+ (f.pids, f.state)): Unit))
        }).foreverM.start
        finalDag <- TaskDag.executor(handlers).execute(dag, channel, ForkHeaps.default, eventQueue, killSignal)
        _ <- sampler.cancel
      } yield finalDag
      val finalDag = program.timeout(scala.concurrent.duration.DurationInt(10).minutes).unsafeRunSync()
      try {
        finalDag.failed shouldBe empty
        finalDag.errored shouldBe empty
        finalDag.linkResults.get(TaskId.Link(project)) match {
          case Some(LinkResult.NativeSuccess(binary, _)) => Files.exists(binary) shouldBe true
          case other                                     => fail(s"expected a native binary, got $other")
        }
        val states = seen.get()
        withClue(s"states seen: ${states.map { case (pids, st) => s"${pids.size} pids/$st" }.distinct}: ") {
          // The toolchain's processes were attributed to the grant: the fork had pids it never started itself.
          states.exists { case (pids, _) => pids.nonEmpty } shouldBe true
          // And measured — the final clang++ link holds still long enough for the whole set to be measured.
          states.exists { case (pids, st) => pids.nonEmpty && st.isInstanceOf[ForkState.Measured] } shouldBe true
        }
        // Gone with the task: its exit was reported, the registry has no entry for it.
        SchedulerFakesEventually.eventually(5000L)(scheduling.snapshot.exists(_.state.forks.isEmpty)) shouldBe true
        scheduling.forks.size shouldBe 0
        scheduling.children.open shouldBe 0
      } finally {
        channel.close()
        scheduling.close()
      }
    }
  }
}
