package bleep

import bleep.packaging.CoordinatesFor
import org.scalatest.funsuite.AnyFunSuite

class CoordinatesForTest extends AnyFunSuite {
  private val projectName = model.CrossProjectName(model.ProjectName("myartifact"), None)
  private val coords = CoordinatesFor.Default(groupId = "com.example", version = "1.0.0")

  test("project with no scala block publishes as Java artifact (single-colon repr)") {
    val project = model.Project.empty
    val dep = coords(projectName, project)

    assert(dep.isInstanceOf[model.Dep.JavaDependency])
    assert(dep.repr == "com.example:myartifact:1.0.0")
  }

  test("project with scala block AND version publishes as Scala artifact (double-colon repr)") {
    val scala = model.Scala(
      version = Some(model.VersionScala.Scala3),
      options = model.Options.empty,
      setup = None,
      compilerPlugins = model.JsonSet.empty,
      strict = None,
      skipStdlib = None,
      compilerProject = None,
      sbtPlugin = None
    )
    val project = model.Project.empty.copy(scala = Some(scala))
    val dep = coords(projectName, project)

    assert(dep.isInstanceOf[model.Dep.ScalaDependency])
    assert(dep.repr == "com.example::myartifact:1.0.0")
  }

  test("project with scala block but NO version publishes as Java artifact") {
    // The case that broke bleep-plugin-spring-boot: extending template-common pulls in
    // `scala.options`, `scala.setup`, `scala.strict` without a version. The project is still
    // a Java artifact and should publish with a single-colon coordinate, not be treated as
    // Scala-suffixed (which would fail to resolve without a known Scala binVersion).
    val scala = model.Scala(
      version = None,
      options = model.Options(Set(model.Options.Opt.Flag("-feature"))),
      setup = None,
      compilerPlugins = model.JsonSet.empty,
      strict = Some(true),
      skipStdlib = None,
      compilerProject = None,
      sbtPlugin = None
    )
    val project = model.Project.empty.copy(scala = Some(scala))
    val dep = coords(projectName, project)

    assert(
      dep.isInstanceOf[model.Dep.JavaDependency],
      s"expected Dep.JavaDependency, got ${dep.getClass.getSimpleName} with repr=${dep.repr}"
    )
    assert(dep.repr == "com.example:myartifact:1.0.0")
  }

  test("FromModel falls back to provided groupId when project has no publish.groupId") {
    val coords = CoordinatesFor.FromModel(version = "2.0.0", fallbackGroupId = "fallback.group")
    val project = model.Project.empty
    val dep = coords(projectName, project)

    assert(dep.repr == "fallback.group:myartifact:2.0.0")
  }

  test("FromModel uses publish.groupId when set on the project") {
    val coords = CoordinatesFor.FromModel(version = "2.0.0", fallbackGroupId = "fallback.group")
    val publish = model.PublishConfig(
      enabled = None,
      groupId = Some("real.group"),
      description = None,
      url = None,
      organization = None,
      developers = model.JsonSet.empty,
      licenses = model.JsonSet.empty,
      sonatypeProfileName = None,
      sonatypeCredentialHost = None
    )
    val project = model.Project.empty.copy(publish = Some(publish))
    val dep = coords(projectName, project)

    assert(dep.repr == "real.group:myartifact:2.0.0")
  }

  private def sbtPlugin(scalaVersion: model.VersionScala): model.Project =
    model.Project.empty
      .copy(scala = Some(model.Scala(Some(scalaVersion), model.Options.empty, None, model.JsonSet.empty, None, None, None, sbtPlugin = Some(true))))

  test("an sbt plugin for scala 2.12 publishes under sbt 1's name") {
    assert(coords(projectName, sbtPlugin(model.VersionScala.Scala212)).repr == "com.example:myartifact_2.12_1.0:1.0.0")
  }

  test("an sbt plugin for scala 3 publishes under sbt 2's name") {
    assert(coords(projectName, sbtPlugin(model.VersionScala.Scala3)).repr == "com.example:myartifact_sbt2_3:1.0.0")
  }

  test("an sbt plugin for another scala version is an error") {
    assertThrows[BleepException](coords(projectName, sbtPlugin(model.VersionScala.Scala213)))
  }

  test("a pom names an sbt 1 plugin dependency by its published name, an sbt 2 one already has it") {
    val dynver = model.Dep.ScalaDependency(
      coursier.core.Organization("com.github.sbt"),
      coursier.core.ModuleName("sbt-dynver"),
      "5.1.1",
      fullCrossVersion = false,
      isSbtPlugin = true
    )
    def artifactId(scalaVersion: model.VersionScala) =
      packaging.GenLayout.artifactId(dynver.asJava(model.VersionCombo.Jvm(scalaVersion)).orThrowText.dependency)
    assert(artifactId(model.VersionScala.Scala212) == "sbt-dynver_2.12_1.0")
    assert(artifactId(model.VersionScala.Scala3) == "sbt-dynver_sbt2_3")
  }
}
