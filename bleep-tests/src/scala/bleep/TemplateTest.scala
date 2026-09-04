package bleep

import bleep.internal.{BleepTemplateLogger, ShortenAndSortJson}
import bleep.rewrites.Defaults
import bleep.templates.{templatesInfer, TemplateDef}
import bleep.testing.SnapshotTest
import io.circe.Decoder
import io.circe.syntax.EncoderOps
import org.scalactic.{source, Prettifier}
import org.scalatest.Assertion

import java.nio.file.{Files, Path, Paths}

class TemplateTest extends SnapshotTest {

  override val outFolder = Paths.get("snapshot-tests").resolve("templates").toAbsolutePath
  val scala = model.Scala(Some(model.VersionScala.Scala213), model.Options.empty, None, model.JsonSet.empty, None)
  val a = noCross("a")
  val aTest = noCross("aTest")
  val b = noCross("b")
  val bTest = noCross("bTest")
  val p: model.Project = model.Project.empty
  val fooOpt = model.Options(Set(model.Options.Opt.Flag("foo")))

  test("should extract common template") {
    val projects = Map(
      a -> p.copy(scala = Some(scala)),
      b -> p.copy(scala = Some(scala.copy(options = fooOpt)))
    )

    val build = run(projects, "common_template.yaml")
    requireBuildHasTemplate(build, TemplateDef.Common).discard()
    requireProjectsHaveTemplate(build, TemplateDef.Common, a.name, b.name)
  }

  test("should extract common template and a test template") {
    val scala = model.Scala(Some(model.VersionScala.Scala213), model.Options.empty, None, model.JsonSet.empty, None)
    val projects = Map(
      a -> p.copy(scala = Some(scala)),
      aTest -> p.copy(scala = Some(scala.copy(options = fooOpt)), isTestProject = Some(true), dependsOn = model.JsonSet(a.name)),
      bTest -> p.copy(scala = Some(scala.copy(options = fooOpt)), isTestProject = Some(true), dependsOn = model.JsonSet(a.name))
    )

    val build = run(projects, "common_test_template.yaml")
    val commonTest = TemplateDef.Test(TemplateDef.Common)
    requireBuildHasTemplate(build, commonTest).discard()
    requireTemplateHasParent(build, childTemplate = commonTest, parentTemplate = TemplateDef.Common).discard()
    requireProjectsHaveTemplate(build, commonTest, aTest.name, bTest.name)
  }

  test("should heed ignoreWhenInferringTemplates") {
    val scala = model.Scala(Some(model.VersionScala.Scala213), model.Options.empty, None, model.JsonSet.empty, None)
    val projects = Map(
      a -> p.copy(scala = Some(scala)),
      b -> p.copy(dependsOn = model.JsonSet(a.name)),
      aTest -> p.copy(scala = Some(scala.copy(options = fooOpt)), isTestProject = Some(true), dependsOn = model.JsonSet(a.name))
    )

    val build = run(projects, "template_ignore_b.yaml", ignoreWhenInferringTemplates = Set(b.name))
    requireBuildHasProject(build, b.name).discard()
    requireBuildHasTemplate(build, TemplateDef.Common).discard()
    requireProjectsHaveTemplate(build, TemplateDef.Common, aTest.name, bTest.name)
  }

  test("bug") {
    val path = Path.of(getClass.getResource("/bug.yaml").toURI)
    val content = Files.readString(path)

    implicit val foo: Decoder[model.Project] =
      model.Project.decodes(using model.TemplateId.decoder(Nil), Decoder[String].map(model.ProjectName.apply))

    val Right(projects) = io.circe.parser.decode[Map[model.CrossProjectName, model.Project]](content): @unchecked
    run(projects, "bug.yaml", ignoreWhenInferringTemplates = Set(b.name))
    // should probably have some assertions, but let's be lazy and lean on the snapshots for now
  }

  private val depX = model.Dep.parse("com.example:libx:1.0.0").getOrElse(sys.error("bad dep"))

  test("a dependency shared by two projects lands in a template, is not inlined, and survives the round-trip") {
    val projects = Map(
      a -> p.copy(dependencies = model.JsonSet(depX)),
      b -> p.copy(dependencies = model.JsonSet(depX))
    )
    // `run` fails if templating dropped or otherwise changed anything (its diffProjects round-trip check).
    val build = run(projects, "shared_dependency.yaml")
    val common = requireBuildHasTemplate(build, TemplateDef.Common)
    assert(
      common.dependencies.values.contains(depX),
      s"the shared dependency should live in the common template; template deps = ${common.dependencies.values}"
    )
    requireProjectsHaveTemplate(build, TemplateDef.Common, a.name, b.name).discard()
    assert(
      build.projects.value(a.name).dependencies.values.isEmpty,
      s"the dependency should be in the template, not also inlined in a: ${build.projects.value(a.name).dependencies.values}"
    )
  }

  test("a dependency on a single project stays on that project (nothing to templatize) and survives the round-trip") {
    val projects = Map(a -> p.copy(dependencies = model.JsonSet(depX)), b -> p.copy(scala = Some(scala)))
    val build = run(projects, "single_dependency.yaml")
    assert(build.projects.value(a.name).dependencies.values.contains(depX), s"a lost its dependency: ${build.projects.value(a.name).dependencies.values}")
  }

  private def scalaV(v: String) = model.Scala(Some(model.VersionScala(v)), model.Options.empty, None, model.JsonSet.empty, None)
  private def crossName(name: String, id: String) = model.CrossProjectName(model.ProjectName(name), Some(model.CrossId(id)))

  test("cross-projects sharing a dependency collapse without empty cross entries and keep the dependency") {
    val projects = Map(
      crossName("a", "jvm211") -> p.copy(scala = Some(scalaV("2.11.12")), dependencies = model.JsonSet(depX)),
      crossName("a", "jvm212") -> p.copy(scala = Some(scalaV("2.12.18")), dependencies = model.JsonSet(depX)),
      crossName("a", "jvm213") -> p.copy(scala = Some(scalaV("2.13.12")), dependencies = model.JsonSet(depX))
    )
    val build = run(projects, "cross_shared_dependency.yaml")
    val ap = build.projects.value(model.ProjectName("a"))
    val emptyCross = ap.cross.value.collect { case (id, cp) if cp.isEmpty => id.value }
    assert(emptyCross.isEmpty, s"empty `cross: {}` entries leaked into the collapsed build: ${emptyCross.mkString(", ")}")
  }

  test("a cross-template shared by multiple projects does not leak empty cross entries") {
    // Two projects with the same cross shape → a cross-template is inferred (step 2). This is the scalameta shape where empty `cross: {}` entries appeared.
    def crossBuilt(name: String) = Map(
      crossName(name, "jvm211") -> p.copy(scala = Some(scalaV("2.11.12")), dependencies = model.JsonSet(depX)),
      crossName(name, "jvm212") -> p.copy(scala = Some(scalaV("2.12.18")), dependencies = model.JsonSet(depX)),
      crossName(name, "jvm213") -> p.copy(scala = Some(scalaV("2.13.12")), dependencies = model.JsonSet(depX))
    )
    val projects = crossBuilt("a") ++ crossBuilt("b")
    val build = run(projects, "cross_template.yaml")
    val leaks = build.projects.value.flatMap { case (name, proj) =>
      proj.cross.value.collect { case (id, cp) if cp.isEmpty => s"$name/${id.value}" }
    }
    assert(leaks.isEmpty, s"empty `cross: {}` entries leaked: ${leaks.mkString(", ")}")
  }

  def run(
      projects: Map[model.CrossProjectName, model.Project],
      testName: String,
      ignoreWhenInferringTemplates: Set[model.ProjectName] = Set.empty
  ): model.BuildFile = {
    val pre = model.Build.Exploded(model.BleepVersion.dev, projects, model.JsonList.empty, None, Map.empty, None)
    val logger = logger0.withContext("testName", testName)
    val buildFile = templatesInfer(new BleepTemplateLogger(logger), pre, ignoreWhenInferringTemplates)
    writeAndCompare(
      outFolder.resolve(testName),
      Map(outFolder.resolve(testName) -> buildFile.asJson.foldWith(ShortenAndSortJson(Nil)).spaces2),
      logger
    ).discard()

    // complain if we have done illegal rewrites during templating
    val post = model.Build.FileBacked(buildFile).dropBuildFile.dropTemplates
    model.Build.diffProjects(Defaults.add(pre, null), post) match {
      case empty if empty.isEmpty => ()
      case diffs                  =>
        diffs.foreach { case (projectName, msg) => System.err.println(s"$projectName: $msg") }
        fail("Project templating did illegal rewrites. ")
    }

    buildFile
  }

  def requireProjectsHaveTemplate(
      buildFile: model.BuildFile,
      templateId: TemplateDef,
      firstProject: model.ProjectName,
      restProjects: model.ProjectName*
  )(implicit prettifier: Prettifier, pos: source.Position): Assertion = {
    val ps = buildFile.projects.value.filter { case (k, _) => firstProject == k || restProjects.contains(k) }
    assert(
      ps.values.forall(_.`extends`.values.contains(templateId.templateId)),
      ps.map { case (k, v) => s"$k:${v.`extends`.values.mkString(", ")}" }.mkString("\n")
    )
  }

  def requireTemplateHasParent(
      build: model.BuildFile,
      childTemplate: TemplateDef,
      parentTemplate: TemplateDef
  )(implicit prettifier: Prettifier, pos: source.Position): Assertion = {
    val child = build.templates.value(childTemplate.templateId)
    assert(
      child.`extends`.values.contains(parentTemplate.templateId),
      child.`extends`.values.mkString(", ")
    )
  }

  def requireBuildHasTemplate(buildFile: model.BuildFile, templateId: TemplateDef)(implicit prettifier: Prettifier, pos: source.Position): model.Project = {
    assert(
      buildFile.templates.value.contains(templateId.templateId),
      buildFile.templates.value.keySet.mkString(", ")
    ).discard()
    buildFile.templates.value(templateId.templateId)
  }
  def requireBuildHasProject(build: model.BuildFile, name: model.ProjectName)(implicit prettifier: Prettifier, pos: source.Position): model.Project = {
    assert(
      build.projects.value.contains(name),
      build.projects.value.keySet.mkString(", ")
    ).discard()
    build.projects.value(name)
  }

  def noCross(str: String): model.CrossProjectName =
    model.CrossProjectName(model.ProjectName(str), None)
}
