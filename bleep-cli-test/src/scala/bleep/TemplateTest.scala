package bleep

import bleep.internal.Templates.TemplateDef
import bleep.internal.{ShortenAndSortJson, Templates}
import bleep.rewrites.Defaults
import bleep.testing.SnapshotTest
import io.circe.Decoder
import io.circe.syntax.EncoderOps
import org.scalactic.{source, Prettifier}
import org.scalatest.Assertion

import java.nio.file.{Files, Path, Paths}
import java.time.Instant

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
    requireBuildHasTemplate(build, TemplateDef.Common)
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
    requireBuildHasTemplate(build, commonTest)
    requireTemplateHasParent(build, childTemplate = commonTest, parentTemplate = TemplateDef.Common)
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
    requireBuildHasProject(build, b.name)
    requireBuildHasTemplate(build, TemplateDef.Common)
    requireProjectsHaveTemplate(build, TemplateDef.Common, aTest.name, bTest.name)
  }

  test("bug") {
    val path = Path.of(getClass.getResource("/bug.yaml").toURI)
    val content = Files.readString(path)

    implicit val foo: Decoder[model.Project] =
      model.Project.decodes(model.TemplateId.decoder(Nil), Decoder[String].map(model.ProjectName.apply))

    val Right(projects) = io.circe.parser.decode[Map[model.CrossProjectName, model.Project]](content)
    run(projects, "bug.yaml", ignoreWhenInferringTemplates = Set(b.name))
    // should probably have some assertions, but let's be lazy and lean on the snapshots for now
  }

  def run(
      projects: Map[model.CrossProjectName, model.Project],
      testName: String,
      ignoreWhenInferringTemplates: Set[model.ProjectName] = Set.empty
  ): model.Build = {
    val pre = model.ExplodedBuild(model.Build.empty(model.Version("testing")), Map.empty, projects)
    val logger = bleep.logging.stdout(LogPatterns.interface(Instant.now, None, noColor = false)).withContext(testName).untyped
    val build = Templates(logger, pre, ignoreWhenInferringTemplates)
    writeAndCompareEarly(outFolder.resolve(testName), Map(outFolder.resolve(testName) -> build.asJson.foldWith(ShortenAndSortJson).spaces2))

    // complain if we have done illegal rewrites during templating
    val post = model.ExplodedBuild.of(build).dropTemplates
    model.ExplodedBuild.diffProjects(Defaults.add(pre), post) match {
      case empty if empty.isEmpty => ()
      case diffs =>
        diffs.foreach { case (projectName, msg) => System.err.println(s"$projectName: $msg") }
        fail("Project templating did illegal rewrites. ")
    }

    build
  }

  def requireProjectsHaveTemplate(
      build: model.Build,
      templateId: TemplateDef,
      firstProject: model.ProjectName,
      restProjects: model.ProjectName*
  )(implicit prettifier: Prettifier, pos: source.Position): Assertion = {
    val ps = build.projects.value.filter { case (k, _) => firstProject == k || restProjects.contains(k) }
    assert(
      ps.values.forall(_.`extends`.values.contains(templateId.templateId)),
      ps.map { case (k, v) => s"$k:${v.`extends`.values.mkString(", ")}" }.mkString("\n")
    )
  }

  def requireTemplateHasParent(
      build: model.Build,
      childTemplate: TemplateDef,
      parentTemplate: TemplateDef
  )(implicit prettifier: Prettifier, pos: source.Position): Assertion = {
    val child = build.templates.value(childTemplate.templateId)
    assert(
      child.`extends`.values.contains(parentTemplate.templateId),
      child.`extends`.values.mkString(", ")
    )
  }

  def requireBuildHasTemplate(build: model.Build, templateId: TemplateDef)(implicit prettifier: Prettifier, pos: source.Position): model.Project = {
    assert(
      build.templates.value.contains(templateId.templateId),
      build.templates.value.keySet.mkString(", ")
    )
    build.templates.value(templateId.templateId)
  }
  def requireBuildHasProject(build: model.Build, name: model.ProjectName)(implicit prettifier: Prettifier, pos: source.Position): model.Project = {
    assert(
      build.projects.value.contains(name),
      build.projects.value.keySet.mkString(", ")
    )
    build.projects.value(name)
  }

  def noCross(str: String): model.CrossProjectName =
    model.CrossProjectName(model.ProjectName(str), None)
}
