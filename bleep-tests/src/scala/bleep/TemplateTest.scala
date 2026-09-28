package bleep

import bleep.internal.{BleepTemplateLogger, ShortenAndSortJson}
import bleep.templates.{mineTemplates, templatesInfer}
import bleep.testing.SnapshotTest
import io.circe.Decoder
import io.circe.syntax.EncoderOps
import org.scalactic.{source, Prettifier}
import org.scalatest.Assertion

import java.nio.file.{Files, Path, Paths}

/** Every build here goes through `templatesInfer`, which throws if the inferred build file does not explode back to exactly the projects that went in. So each
  * test also checks that inference lost nothing.
  */
class TemplateTest extends SnapshotTest {

  override val outFolder = Paths.get("snapshot-tests").resolve("templates").toAbsolutePath
  val p: model.Project = model.Project.empty

  private def opts(flags: String*) = model.Options(flags.map(model.Options.Opt.Flag.apply).toSet)
  private def dep(s: String) = model.Dep.parse(s).getOrElse(sys.error(s"bad dep $s"))
  private def scala(options: model.Options, plugins: model.Dep*) =
    model.Scala(Some(model.VersionScala.Scala213), options, None, model.JsonSet.fromIterable(plugins), None, None, None)

  private val sharedOptions = opts("-deprecation", "-feature", "-unchecked", "-Xlint")
  private val kindProjector = dep("org.typelevel:::kind-projector:0.13.3")
  private val depX = dep("com.example:libx:1.0.0")
  private val t = model.TemplateId.apply

  test("what every project shares goes into template-common") {
    val projects = List("a", "b", "c", "d").map(name => noCross(name) -> p.copy(scala = Some(scala(sharedOptions)))).toMap
    val build = run(projects, "common_template.yaml", Set.empty)
    val common = requireBuildHasTemplate(build, t("template-common"))
    assert(common.scala.flatMap(_.version).contains(model.VersionScala.Scala213), common.toString)
    requireProjectsHaveTemplate(build, t("template-common"), projects.keys.map(_.name).toList)
  }

  test("one project without a setting does not keep the others from sharing it") {
    // the case the fixed template groups could not handle: every main project has kind-projector and the flags that go with it, except one
    val pluginOptions = opts("-deprecation", "-feature", "-unchecked", "-Xlint", "-Xsource:3", "-language:higherKinds", "-Wconf:cat=unused-nowarn:s")
    val usesPlugin = List("a", "b", "c", "d").map(name => noCross(name) -> p.copy(scala = Some(scala(pluginOptions, kindProjector)))).toMap
    val outlier = noCross("outlier") -> p.copy(scala = Some(scala(sharedOptions)))
    val build = run(usesPlugin + outlier, "outlier.yaml", Set.empty)

    val withPlugin = build.templates.value.collect { case (id, tp) if tp.scala.exists(_.compilerPlugins.values.contains(kindProjector)) => id }.toList
    assert(withPlugin.size == 1, s"kind-projector should live in one template: ${build.templates.value.keys.mkString(", ")}")
    requireProjectsHaveTemplate(build, withPlugin.head, usesPlugin.keys.map(_.name).toList)
    assert(
      !build.projects.value(outlier._1.name).`extends`.values.contains(withPlugin.head),
      "the outlier must not get kind-projector"
    )
    usesPlugin.keys.foreach { name =>
      assert(build.projects.value(name.name).scala.forall(_.compilerPlugins.isEmpty), s"${name.value} still lists kind-projector itself")
    }
  }

  test("test projects share template-common-test") {
    val mains = List("a", "b", "c").map(name => noCross(name) -> p.copy(scala = Some(scala(sharedOptions)))).toMap
    val tests = List("a", "b", "c").map { name =>
      noCross(s"$name-test") -> p.copy(
        scala = Some(scala(sharedOptions)),
        isTestProject = Some(true),
        dependencies = model.JsonSet(dep("org.scalatest::scalatest:3.2.18"), dep("org.scalacheck::scalacheck:1.17.0"), dep("org.scalameta::munit:1.0.0")),
        testFrameworks = model.JsonSet(model.TestFrameworkName("munit.Framework"), model.TestFrameworkName("org.scalatest.tools.Framework")),
        dependsOn = model.JsonSet(model.ProjectRef(model.ProjectName(name)))
      )
    }.toMap
    val build = run(mains ++ tests, "common_test_template.yaml", Set.empty)
    requireProjectsHaveTemplate(build, t("template-common-test"), tests.keys.map(_.name).toList)
    // where a project lives and what it depends on stay on the project
    tests.keys.foreach(name => assert(build.projects.value(name.name).dependsOn.values.nonEmpty, s"${name.value} lost dependsOn to a template"))
  }

  test("projects ignored when inferring templates do not count as users") {
    val projects = List("a", "b").map(name => noCross(name) -> p.copy(scala = Some(scala(sharedOptions)))).toMap
    // one project is too few to share a template, and `b` does not count
    val build = run(projects, "template_ignore_b.yaml", Set(model.ProjectName("b")))
    assert(build.templates.value.isEmpty, build.templates.value.keys.mkString(", "))
  }

  test("bug") {
    val path = Path.of(getClass.getResource("/bug.yaml").toURI)
    val content = Files.readString(path)

    implicit val foo: Decoder[model.Project] =
      model.Project.decodes(using model.TemplateId.decoder(Nil), Decoder[String].map(model.ProjectName.apply))

    val Right(projects) = io.circe.parser.decode[Map[model.CrossProjectName, model.Project]](content): @unchecked
    run(projects, "bug.yaml", Set(model.ProjectName("b"))).discard()
    // should probably have some assertions, but let's be lazy and lean on the snapshots for now
  }

  test("a dependency shared by enough projects lands in a template and is not also listed on the projects") {
    val projects = List("a", "b", "c", "d").map(name => noCross(name) -> p.copy(dependencies = model.JsonSet(depX), scala = Some(scala(sharedOptions)))).toMap
    val build = run(projects, "shared_dependency.yaml", Set.empty)
    val common = requireBuildHasTemplate(build, t("template-common"))
    assert(common.dependencies.values.contains(depX), s"template deps = ${common.dependencies.values}")
    projects.keys.foreach(name => assert(build.projects.value(name.name).dependencies.values.isEmpty, s"${name.value} still lists the dependency"))
  }

  test("a dependency on a single project stays on that project") {
    val projects = Map(noCross("a") -> p.copy(dependencies = model.JsonSet(depX)), noCross("b") -> p.copy(scala = Some(scala(sharedOptions))))
    val build = run(projects, "single_dependency.yaml", Set.empty)
    assert(build.projects.value(model.ProjectName("a")).dependencies.values.contains(depX))
  }

  private def scalaV(v: String) = model.Scala(Some(model.VersionScala(v)), model.Options.empty, None, model.JsonSet.empty, None, None, None)
  private def crossName(name: String, id: String) = model.CrossProjectName(model.ProjectName(name), Some(model.CrossId(id)))

  private def crossBuilt(name: String) = Map(
    crossName(name, "jvm211") -> p.copy(scala = Some(scalaV("2.11.12")), dependencies = model.JsonSet(depX)),
    crossName(name, "jvm212") -> p.copy(scala = Some(scalaV("2.12.18")), dependencies = model.JsonSet(depX)),
    crossName(name, "jvm213") -> p.copy(scala = Some(scalaV("2.13.12")), dependencies = model.JsonSet(depX))
  )

  test("cross projects sharing a dependency collapse without empty cross entries and keep the dependency") {
    val build = run(crossBuilt("a"), "cross_shared_dependency.yaml", Set.empty)
    val ap = build.projects.value(model.ProjectName("a"))
    val emptyCross = ap.cross.value.collect { case (id, cp) if cp.isEmpty => id.value }
    assert(emptyCross.isEmpty, s"empty `cross: {}` entries leaked into the collapsed build: ${emptyCross.mkString(", ")}")
  }

  test("a cross template shared by several projects does not leak empty cross entries") {
    // the scalameta shape where empty `cross: {}` entries appeared
    val build = run(crossBuilt("a") ++ crossBuilt("b") ++ crossBuilt("c"), "cross_template.yaml", Set.empty)
    val leaks = build.projects.value.flatMap { case (name, proj) =>
      proj.cross.value.collect { case (id, cp) if cp.isEmpty => s"$name/${id.value}" }
    }
    assert(leaks.isEmpty, s"empty `cross: {}` entries leaked: ${leaks.mkString(", ")}")
  }

  def run(
      projects: Map[model.CrossProjectName, model.Project],
      testName: String,
      ignoreWhenInferringTemplates: Set[model.ProjectName]
  ): model.BuildFile = {
    val pre = model.Build.Exploded(model.BleepVersion.dev, projects, model.JsonList.empty, None, Map.empty, None)
    val logger = logger0.withContext("testName", testName)
    val buildFile = templatesInfer(new BleepTemplateLogger(logger), pre, ignoreWhenInferringTemplates, mineTemplates.Costs.default)
    writeAndCompare(
      outFolder.resolve(testName),
      Map(outFolder.resolve(testName) -> buildFile.asJson.foldWith(ShortenAndSortJson(Nil)).spaces2),
      logger
    ).discard()

    buildFile
  }

  def requireProjectsHaveTemplate(
      buildFile: model.BuildFile,
      templateId: model.TemplateId,
      projects: List[model.ProjectName]
  )(implicit prettifier: Prettifier, pos: source.Position): Assertion = {
    val ps = buildFile.projects.value.filter { case (k, _) => projects.contains(k) }
    assert(ps.size == projects.size, s"missing projects among ${buildFile.projects.value.keySet}")
    assert(
      ps.values.forall(_.`extends`.values.contains(templateId)),
      ps.map { case (k, v) => s"$k:${v.`extends`.values.mkString(", ")}" }.mkString("\n")
    )
  }

  def requireBuildHasTemplate(
      buildFile: model.BuildFile,
      templateId: model.TemplateId
  )(implicit prettifier: Prettifier, pos: source.Position): model.Project = {
    assert(
      buildFile.templates.value.contains(templateId),
      buildFile.templates.value.keySet.mkString(", ")
    ).discard()
    buildFile.templates.value(templateId)
  }

  def noCross(str: String): model.CrossProjectName =
    model.CrossProjectName(model.ProjectName(str), None)
}
