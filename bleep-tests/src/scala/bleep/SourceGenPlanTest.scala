package bleep

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.SortedSet

/** Which sourcegen scripts `bleep sourcegen` runs, and with which arguments. The CLI and `bleep.sourcegen` over MCP both run exactly this plan; they differ
  * only in where the scripts' output goes.
  */
class SourceGenPlanTest extends AnyFunSuite with Matchers {

  private def cpn(name: String): model.CrossProjectName = model.CrossProjectName(model.ProjectName(name), None)

  private def script(project: String, main: String): model.ScriptDef.Main =
    model.ScriptDef.Main(cpn(project), main, model.JsonSet(SortedSet.empty[RelPath]), model.JsonSet.empty, None)

  private def consumer(scripts: model.ScriptDef*): model.Project =
    model.Project.empty.copy(sourcegen = model.JsonSet(SortedSet.from(scripts)))

  private def build(projects: (String, model.Project)*): model.Build.Exploded =
    model.Build.Exploded(
      $version = model.BleepVersion("test"),
      explodedProjects = projects.map { case (name, p) => cpn(name) -> p }.toMap,
      resolvers = model.JsonList.empty,
      jvm = None,
      scripts = Map.empty,
      remoteCache = None
    )

  test("a script shared by several projects runs once, told about all of them") {
    val gen = script("scripts", "gen.Main")
    val b = build("a" -> consumer(gen), "b" -> consumer(gen), "scripts" -> model.Project.empty)

    val plan = commands.SourceGen.plan(b, Array(cpn("a"), cpn("b")))

    plan.map(_.script) shouldBe List(gen)
    plan.head.args shouldBe List("--project", "a", "--project", "b")
  }

  test("projects without sourcegen contribute nothing, and the order is stable") {
    val one = script("scripts", "gen.One")
    val two = script("scripts", "gen.Two")
    val b = build("a" -> consumer(two), "b" -> consumer(one), "plain" -> model.Project.empty)

    commands.SourceGen.plan(b, Array(cpn("plain"))) shouldBe Nil
    commands.SourceGen.plan(b, Array(cpn("a"), cpn("b"), cpn("plain"))).map(_.script.main) shouldBe List("gen.One", "gen.Two")
  }
}
