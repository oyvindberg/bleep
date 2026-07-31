package bleep.model

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class BuildValidationTest extends AnyFunSuite with Matchers {

  // `Platform.Js.apply` takes a non-optional jsVersion, so the missing case has to be reached by copy. That the smart constructor cannot express it is a good
  // sign for the model and an awkward one for a test about exactly that state.
  private val jsPlatform: Platform = Platform.Js(VersionScalaJs.ScalaJs1, None, None, None, None, None, None)
  private val nativePlatform: Platform = Platform.Native(VersionScalaNative.ScalaNative05, None, None, None, None, None, None, None, None, None)

  private val scalaVersioned = Scala(version = Some(VersionScala.Scala3), options = Options.empty, setup = None, compilerPlugins = JsonSet.empty, strict = None)
  private val scalaUnversioned = scalaVersioned.copy(version = None)

  private def build(projects: (String, Project)*): Build.Exploded =
    Build.Exploded(
      $version = BleepVersion("1.0.0-M9"),
      explodedProjects = projects.map { case (n, p) => CrossProjectName(ProjectName(n), None) -> p }.toMap,
      resolvers = JsonList.empty,
      jvm = None,
      scripts = Map.empty,
      remoteCache = None
    )

  test("a fully specified scala.js project is fine") {
    val p = Project.empty.copy(platform = Some(jsPlatform), scala = Some(scalaVersioned))
    BuildValidation.missingVersions(build("app" -> p)) shouldBe Nil
  }

  test("a jvm project with no scala version is left alone — nothing downstream demands one") {
    val p = Project.empty.copy(scala = Some(scalaUnversioned))
    BuildValidation.missingVersions(build("app" -> p)) shouldBe Nil
  }

  test("scala.js without jsVersion is reported, naming the project and the field") {
    val p = Project.empty.copy(platform = Some(jsPlatform.copy(jsVersion = None)), scala = Some(scalaVersioned))
    val errs = BuildValidation.missingVersions(build("app" -> p))
    errs should have size 1
    errs.head should include("app")
    errs.head should include("platform.jsVersion")
  }

  test("scala native without nativeVersion is reported") {
    val p = Project.empty.copy(platform = Some(nativePlatform.copy(nativeVersion = None)), scala = Some(scalaVersioned))
    val errs = BuildValidation.missingVersions(build("app" -> p))
    errs should have size 1
    errs.head should include("platform.nativeVersion")
  }

  test("both missing versions are reported, rather than stopping at the first") {
    val p = Project.empty.copy(platform = Some(jsPlatform.copy(jsVersion = None)), scala = Some(scalaUnversioned))
    BuildValidation.missingVersions(build("app" -> p)) should have size 2
  }

  test("a kotlin/js project is not asked for scala versions") {
    val kotlin = Kotlin.empty.copy(version = Some(VersionKotlin.Kotlin24))
    val p = Project.empty.copy(platform = Some(jsPlatform.copy(jsVersion = None)), kotlin = Some(kotlin))
    BuildValidation.missingVersions(build("app" -> p)) shouldBe Nil
  }

  test("every offending project is reported, not just the first") {
    val bad = Project.empty.copy(platform = Some(jsPlatform.copy(jsVersion = None)), scala = Some(scalaVersioned))
    BuildValidation.missingVersions(build("b" -> bad, "a" -> bad)) should have size 2
  }
}
