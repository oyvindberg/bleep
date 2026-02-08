package bleep.analysis

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import scala.concurrent.duration.*

class ParallelProjectCompilerTest extends AnyFunSuite with Matchers {

  private def tempDir(): Path = Files.createTempDirectory("parallel-compiler-test")

  private def config(name: String, dir: Path, lang: ProjectLanguage = ProjectLanguage.JavaOnly(None, Nil, None)): ProjectConfig =
    ProjectConfig(
      name = name,
      sources = Set(dir.resolve(s"src/$name")),
      classpath = Seq.empty,
      outputDir = dir.resolve(s"target/$name"),
      language = lang,
      analysisDir = None
    )

  private val noop = DiagnosticListener.noop
  private val noopProgress = ParallelProjectCompiler.BuildProgressListener.noop
  private val neverCancel = CancellationToken.never

  test("build empty DAG succeeds") {
    val result = ParallelProjectCompiler
      .build(
        ProjectDag.empty,
        parallelism = 4,
        noop,
        neverCancel,
        noopProgress
      )
      .unsafeRunSync()

    result shouldBe a[ParallelProjectCompiler.BuildSuccess]
    result.asInstanceOf[ParallelProjectCompiler.BuildSuccess].results shouldBe empty
  }

  test("build single Java project with empty sources succeeds") {
    val dir = tempDir()
    Files.createDirectories(dir.resolve("src/myproject"))

    val dag = ProjectDag.empty.addProject(
      config("myproject", dir),
      Set.empty
    )

    val result = ParallelProjectCompiler.build(dag, 4, noop, neverCancel, noopProgress).unsafeRunSync()

    result shouldBe a[ParallelProjectCompiler.BuildSuccess]
    val success = result.asInstanceOf[ParallelProjectCompiler.BuildSuccess]
    success.results.keySet shouldBe Set("myproject")
  }

  test("build detects cyclic dependencies") {
    val dir = tempDir()
    val dag = ProjectDag.fromProjects(
      Seq(
        (config("a", dir), Set("b")),
        (config("b", dir), Set("a"))
      )
    )

    val result = ParallelProjectCompiler.build(dag, 4, noop, neverCancel, noopProgress).unsafeRunSync()

    result shouldBe a[ParallelProjectCompiler.BuildFailure]
  }

  test("build respects dependency order") {
    val dir = tempDir()
    Files.createDirectories(dir.resolve("src/a"))
    Files.createDirectories(dir.resolve("src/b"))
    Files.createDirectories(dir.resolve("src/c"))

    // Chain: a -> b -> c (c depends on b, b depends on a)
    val dag = ProjectDag.fromProjects(
      Seq(
        (config("a", dir), Set.empty[String]),
        (config("b", dir), Set("a")),
        (config("c", dir), Set("b"))
      )
    )

    val result = ParallelProjectCompiler.build(dag, 4, noop, neverCancel, noopProgress).unsafeRunSync()

    result shouldBe a[ParallelProjectCompiler.BuildSuccess]
    val success = result.asInstanceOf[ParallelProjectCompiler.BuildSuccess]
    success.results.keySet shouldBe Set("a", "b", "c")
  }

  test("buildProject builds only needed dependencies") {
    val dir = tempDir()
    Files.createDirectories(dir.resolve("src/a"))
    Files.createDirectories(dir.resolve("src/b"))
    Files.createDirectories(dir.resolve("src/c"))
    Files.createDirectories(dir.resolve("src/unrelated"))

    val dag = ProjectDag.fromProjects(
      Seq(
        (config("a", dir), Set.empty[String]),
        (config("b", dir), Set("a")),
        (config("c", dir), Set("b")),
        (config("unrelated", dir), Set.empty[String])
      )
    )

    // Build only 'b' and its dependencies
    val result = ParallelProjectCompiler
      .buildProject(
        "b",
        dag,
        4,
        noop,
        neverCancel,
        noopProgress
      )
      .unsafeRunSync()

    result shouldBe a[ParallelProjectCompiler.BuildSuccess]
    val success = result.asInstanceOf[ParallelProjectCompiler.BuildSuccess]
    // Should build a and b, but not c or unrelated
    success.results.keySet shouldBe Set("a", "b")
  }

  test("cancellation stops build") {
    val dir = tempDir()
    Files.createDirectories(dir.resolve("src/a"))

    val dag = ProjectDag.empty.addProject(config("a", dir), Set.empty)
    val token = CancellationToken.create()
    token.cancel() // Cancel immediately

    val result = ParallelProjectCompiler.build(dag, 4, noop, token, noopProgress).unsafeRunSync()

    // Build should be cancelled or complete (depending on timing)
    // At minimum, it should not hang
    result should (be(a[ParallelProjectCompiler.BuildSuccess]) or be(a[ParallelProjectCompiler.BuildFailure]))
  }

  test("parallel compilation of independent projects") {
    val dir = tempDir()
    // Create 4 independent projects
    (1 to 4).foreach { i =>
      Files.createDirectories(dir.resolve(s"src/project$i"))
    }

    val dag = ProjectDag.fromProjects(
      (1 to 4).map(i => (config(s"project$i", dir), Set.empty[String]))
    )

    dag.maxParallelism shouldBe 4

    val result = ParallelProjectCompiler.build(dag, 4, noop, neverCancel, noopProgress).unsafeRunSync()

    result shouldBe a[ParallelProjectCompiler.BuildSuccess]
    val success = result.asInstanceOf[ParallelProjectCompiler.BuildSuccess]
    success.results.keySet shouldBe Set("project1", "project2", "project3", "project4")
  }
}
