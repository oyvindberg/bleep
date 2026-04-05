package bleep

import bleep.commands.BuildInvalidated
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedSet

class BuildInvalidatedTest extends AnyFunSuite with Matchers {

  private def cpn(name: String): model.CrossProjectName =
    model.CrossProjectName(model.ProjectName(name), None)

  private def projectWithDeps(deps: String*): model.Project =
    model.Project.empty.copy(
      dependsOn = model.JsonSet(SortedSet.from(deps.map(model.ProjectName.apply)))
    )

  private def makeBuild(projects: (String, model.Project)*): model.Build.Exploded =
    model.Build.Exploded(
      $version = model.BleepVersion("test"),
      explodedProjects = projects.map { case (name, p) => cpn(name) -> p }.toMap,
      resolvers = model.JsonList.empty,
      jvm = None,
      scripts = Map.empty
    )

  // ============================================================================
  // computeReverseDeps tests
  // ============================================================================

  test("computeReverseDeps: empty build") {
    val build = makeBuild()
    BuildInvalidated.computeReverseDeps(build) shouldBe empty
  }

  test("computeReverseDeps: simple chain A -> B -> C") {
    val build = makeBuild(
      "a" -> model.Project.empty,
      "b" -> projectWithDeps("a"),
      "c" -> projectWithDeps("b")
    )
    val reverse = BuildInvalidated.computeReverseDeps(build)
    reverse(cpn("a")) shouldBe Set(cpn("b"))
    reverse(cpn("b")) shouldBe Set(cpn("c"))
    reverse.get(cpn("c")) shouldBe None
  }

  test("computeReverseDeps: diamond dependency") {
    val build = makeBuild(
      "a" -> model.Project.empty,
      "b" -> projectWithDeps("a"),
      "c" -> projectWithDeps("a"),
      "d" -> projectWithDeps("b", "c")
    )
    val reverse = BuildInvalidated.computeReverseDeps(build)
    reverse(cpn("a")) shouldBe Set(cpn("b"), cpn("c"))
    reverse(cpn("b")) shouldBe Set(cpn("d"))
    reverse(cpn("c")) shouldBe Set(cpn("d"))
  }

  test("computeReverseDeps: includes sourcegen dependencies") {
    val scripts = cpn("scripts")
    val app = model.Project.empty.copy(
      dependsOn = model.JsonSet(SortedSet(model.ProjectName("lib"))),
      sourcegen = model.JsonSet(SortedSet(model.ScriptDef.Main(scripts, "my.Gen", model.JsonSet.empty): model.ScriptDef))
    )
    val build = makeBuild(
      "lib" -> model.Project.empty,
      "scripts" -> model.Project.empty,
      "app" -> app
    )
    val reverse = BuildInvalidated.computeReverseDeps(build)
    reverse(cpn("scripts")) shouldBe Set(cpn("app"))
    reverse(cpn("lib")) shouldBe Set(cpn("app"))
  }

  // ============================================================================
  // transitiveDependents tests
  // ============================================================================

  test("transitiveDependents: empty invalidated set") {
    val reverse = Map(cpn("a") -> Set(cpn("b")))
    BuildInvalidated.transitiveDependents(Set.empty, reverse) shouldBe empty
  }

  test("transitiveDependents: single project, no dependents") {
    val a = cpn("a")
    BuildInvalidated.transitiveDependents(Set(a), Map.empty) shouldBe Set(a)
  }

  test("transitiveDependents: propagates through chain") {
    val a = cpn("a")
    val b = cpn("b")
    val c = cpn("c")
    val reverse = Map(
      a -> Set(b),
      b -> Set(c)
    )
    BuildInvalidated.transitiveDependents(Set(a), reverse) shouldBe Set(a, b, c)
  }

  test("transitiveDependents: diamond propagation") {
    val a = cpn("a")
    val b = cpn("b")
    val c = cpn("c")
    val d = cpn("d")
    val reverse = Map(
      a -> Set(b, c),
      b -> Set(d),
      c -> Set(d)
    )
    BuildInvalidated.transitiveDependents(Set(a), reverse) shouldBe Set(a, b, c, d)
  }

  test("transitiveDependents: multiple seeds") {
    val a = cpn("a")
    val b = cpn("b")
    val c = cpn("c")
    val d = cpn("d")
    val reverse = Map(
      a -> Set(c),
      b -> Set(d)
    )
    BuildInvalidated.transitiveDependents(Set(a, b), reverse) shouldBe Set(a, b, c, d)
  }

  test("transitiveDependents: handles cycles gracefully") {
    val a = cpn("a")
    val b = cpn("b")
    val reverse = Map(
      a -> Set(b),
      b -> Set(a)
    )
    BuildInvalidated.transitiveDependents(Set(a), reverse) shouldBe Set(a, b)
  }

  test("transitiveDependents: isolated projects not included") {
    val a = cpn("a")
    val b = cpn("b")
    val reverse = Map(a -> Set(b))
    BuildInvalidated.transitiveDependents(Set(a), reverse) shouldBe Set(a, b)
  }
}
