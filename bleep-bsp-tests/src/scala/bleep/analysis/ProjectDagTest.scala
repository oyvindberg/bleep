package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.Paths

class ProjectDagTest extends AnyFunSuite with Matchers {

  private def config(name: String): ProjectConfig = ProjectConfig(
    name = name,
    sources = Set(Paths.get(s"src/$name")),
    classpath = Seq.empty,
    outputDir = Paths.get(s"target/$name"),
    language = ProjectLanguage.ScalaJava("3.3.3", Nil, None),
    analysisDir = None,
    buildDir = Paths.get(".")
  )

  test("empty DAG") {
    val dag = ProjectDag.empty
    dag.projects shouldBe empty
    dag.roots shouldBe empty
    dag.topologicalOrder shouldBe Right(Nil)
  }

  test("single project with no dependencies") {
    val dag = ProjectDag.empty.addProject(config("a"), Set.empty)

    dag.projects.keySet shouldBe Set("a")
    dag.roots shouldBe Set("a")
    dag.leaves shouldBe Set("a")
    dag.dependenciesOf("a") shouldBe Set.empty
    dag.dependentsOf("a") shouldBe Set.empty
    dag.ready(Set.empty) shouldBe Set("a")
    dag.topologicalOrder shouldBe Right(List("a"))
  }

  test("linear dependency chain: a -> b -> c") {
    val dag = ProjectDag.fromProjects(
      Seq(
        (config("a"), Set.empty[String]),
        (config("b"), Set("a")),
        (config("c"), Set("b"))
      )
    )

    dag.roots shouldBe Set("a")
    dag.leaves shouldBe Set("c")
    dag.dependenciesOf("c") shouldBe Set("b")
    dag.dependentsOf("a") shouldBe Set("b")

    dag.ready(Set.empty) shouldBe Set("a")
    dag.ready(Set("a")) shouldBe Set("b")
    dag.ready(Set("a", "b")) shouldBe Set("c")

    dag.topologicalOrder match {
      case Right(order) =>
        order.indexOf("a") should be < order.indexOf("b")
        order.indexOf("b") should be < order.indexOf("c")
      case Left(_) => fail("Expected valid order")
    }
  }

  test("diamond dependency: d depends on b,c which depend on a") {
    val dag = ProjectDag.fromProjects(
      Seq(
        (config("a"), Set.empty[String]),
        (config("b"), Set("a")),
        (config("c"), Set("a")),
        (config("d"), Set("b", "c"))
      )
    )

    dag.roots shouldBe Set("a")
    dag.leaves shouldBe Set("d")

    dag.ready(Set.empty) shouldBe Set("a")
    dag.ready(Set("a")) shouldBe Set("b", "c")
    dag.ready(Set("a", "b")) shouldBe Set("c") // d not ready yet
    dag.ready(Set("a", "b", "c")) shouldBe Set("d")

    dag.maxParallelism shouldBe 2 // b and c can run in parallel
  }

  test("cycle detection") {
    val dag = ProjectDag.fromProjects(
      Seq(
        (config("a"), Set("c")),
        (config("b"), Set("a")),
        (config("c"), Set("b"))
      )
    )

    dag.validate match {
      case Left(CycleDetected(cycle)) =>
        cycle should contain allOf ("a", "b", "c")
      case Right(_)    => fail("Expected cycle to be detected")
      case Left(other) => fail(s"Expected CycleDetected, got $other")
    }
  }

  test("missing dependency detection") {
    val dag = ProjectDag.fromProjects(
      Seq(
        (config("a"), Set("nonexistent"))
      )
    )

    dag.validate match {
      case Left(MissingDependencies(missing)) =>
        missing should contain("a" -> "nonexistent")
      case Right(_)    => fail("Expected missing dependency error")
      case Left(other) => fail(s"Expected MissingDependencies, got $other")
    }
  }

  test("topological order respects all dependencies") {
    val dag = ProjectDag.fromProjects(
      Seq(
        (config("a"), Set.empty[String]),
        (config("b"), Set.empty[String]),
        (config("c"), Set("a", "b")),
        (config("d"), Set("c")),
        (config("e"), Set("a"))
      )
    )

    dag.topologicalOrder match {
      case Right(order) =>
        // a and b must come before c
        order.indexOf("a") should be < order.indexOf("c")
        order.indexOf("b") should be < order.indexOf("c")
        // c must come before d
        order.indexOf("c") should be < order.indexOf("d")
        // a must come before e
        order.indexOf("a") should be < order.indexOf("e")
      case Left(_) => fail("Expected valid order")
    }
  }

  test("removeProject updates edges") {
    val dag = ProjectDag.fromProjects(
      Seq(
        (config("a"), Set.empty[String]),
        (config("b"), Set("a")),
        (config("c"), Set("b"))
      )
    )

    val reduced = dag.removeProject("b")

    reduced.projects.keySet shouldBe Set("a", "c")
    reduced.dependenciesOf("c") shouldBe Set.empty // b removed from deps
  }
}
