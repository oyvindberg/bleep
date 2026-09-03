package bleep.bsp

import bleep.{BleepException, ResolvedProject}
import bleep.model
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/** The JUnit Platform is unforgiving about version skew across its own jars: `junit-jupiter-engine` 5.9/5.10 call
  * `org.junit.platform.commons.util.ReflectionUtils.returnsVoid`, which 1.13 removed in favour of `returnsPrimitiveVoid`. Pair either of those engines with a
  * 1.13 `junit-platform-commons` and every suite dies during discovery with a `NoSuchMethodError` that names neither jar.
  *
  * That is exactly what `bleep build new --lang kotlin` shipped: kotest 6.2.3 brings junit-platform 1.13.4, while `bleep-test-runner`'s and
  * `jupiter-interface`'s POMs both declare 1.9.1/5.9.1 — and those transitive jars landed on the fork classpath ahead of the aligned ones.
  *
  * These tests pin the two halves of the fix: the injected dependency set is aligned *and* intransitive, so no POM can smuggle a second junit line in, and the
  * assembled classpath is checked for coherence so a future regression fails loudly with coordinates instead of as a `NoSuchMethodError` blamed on a user's
  * test.
  */
class TestRunnerJunitAlignmentTest extends AnyFunSuite with Matchers {
  private val project = model.CrossProjectName(model.ProjectName("mytest"), None)

  private def rulesFor(modules: (String, String, String)*): Set[model.Dep] =
    MultiWorkspaceBspServer.testRuntimeDeps(project, resolvedWith(modules*))

  private def junitPlatform(version: String): Seq[(String, String, String)] =
    Seq(("org.junit.platform", "junit-platform-commons", version), ("org.junit.platform", "junit-platform-engine", version))

  private def versionOf(deps: Set[model.Dep], org: String, name: String): String =
    deps
      .collectFirst { case d if d.organization.value == org && d.baseModuleName.value == name => d.version }
      .getOrElse(fail(s"$org:$name not among ${deps.map(d => s"${d.organization.value}:${d.baseModuleName.value}").toList.sorted.mkString(", ")}"))

  private def cp(names: String*): List[Path] = names.map(n => Path.of("/cache", n)).toList

  /** A `ResolvedProject` carrying nothing but the resolved module graph, which is all `detectJunitPlatformVersion` reads. */
  private def resolvedWith(modules: (String, String, String)*): ResolvedProject =
    resolvedProject(
      Some(ResolvedProject.Resolution(modules.map { case (org, name, version) => ResolvedProject.ResolvedModule(org, name, version, Nil) }.toList))
    )

  private def resolvedProject(resolution: Option[ResolvedProject.Resolution]): ResolvedProject =
    ResolvedProject(
      name = "mytest",
      directory = Path.of("/ws/mytest"),
      workspaceDir = Path.of("/ws"),
      sources = Nil,
      classpath = Nil,
      classesDir = Path.of("/ws/mytest/classes"),
      resources = None,
      language = ResolvedProject.Language.Java(Nil),
      platform = None,
      isTestProject = true,
      dependencies = Nil,
      testFrameworks = Nil,
      resolution = resolution,
      boms = Nil
    )

  test("junit-platform 1.x maps to the jupiter/vintage 5.x of the same minor.patch") {
    MultiWorkspaceBspServer.junitEngineVersionFor("1.13.4") shouldBe "5.13.4"
    MultiWorkspaceBspServer.junitEngineVersionFor("1.8.2") shouldBe "5.8.2"
  }

  test("from junit 6 the platform and engine version lines are unified") {
    MultiWorkspaceBspServer.junitEngineVersionFor("6.0.1") shouldBe "6.0.1"
  }

  test("kotest 6 (junit-platform 1.13.4): launcher and both engines align to the project's line") {
    val deps = rulesFor(junitPlatform("1.13.4")*)
    versionOf(deps, "org.junit.platform", "junit-platform-launcher") shouldBe "1.13.4"
    versionOf(deps, "org.junit.jupiter", "junit-jupiter-engine") shouldBe "5.13.4"
    versionOf(deps, "org.junit.vintage", "junit-vintage-engine") shouldBe "5.13.4"
  }

  test("kotest 5 (junit-platform 1.8.2): launcher and both engines align to the project's line") {
    val deps = rulesFor(junitPlatform("1.8.2")*)
    versionOf(deps, "org.junit.platform", "junit-platform-launcher") shouldBe "1.8.2"
    versionOf(deps, "org.junit.jupiter", "junit-jupiter-engine") shouldBe "5.8.2"
    versionOf(deps, "org.junit.vintage", "junit-vintage-engine") shouldBe "5.8.2"
  }

  test("JUnit 4 without the platform gets the vintage engine, which the project cannot supply itself") {
    // Discovery reports the framework as the display name "JUnit", which `ForkedTestRunner.isJUnitPlatformFramework` matches, so even a plain JUnit 4 suite
    // runs through JUnitPlatformRunner and needs a launcher. Nothing in `com.github.sbt:junit-interface` provides one.
    val deps = rulesFor(("junit", "junit", "4.13.2"), ("com.github.sbt", "junit-interface", "0.13.3"))
    versionOf(deps, "org.junit.platform", "junit-platform-launcher") shouldBe model.Versions.JunitPlatformLauncher
    versionOf(deps, "org.junit.vintage", "junit-vintage-engine") shouldBe model.Versions.JunitVintageEngine
  }

  test("the JUnit 4 and JUnit Platform rows are mutually exclusive — bleep never supplies a version the project already has") {
    // The whole safety argument for the JUnit 4 row's hardcoded versions. junit:junit is on a JUnit 5 project's classpath too (vintage pulls it in), so the
    // rows would otherwise both fire and hand coursier two junit-platform versions to reconcile — the exact mechanism behind the kotest 6 breakage.
    val junit5Project = rulesFor((junitPlatform("1.13.4") :+ ("junit", "junit", "4.13.2"))*)
    versionOf(junit5Project, "org.junit.platform", "junit-platform-launcher") shouldBe "1.13.4"
    versionOf(junit5Project, "org.junit.vintage", "junit-vintage-engine") shouldBe "5.13.4"
    junit5Project.count(_.baseModuleName.value == "junit-platform-launcher") shouldBe 1
  }

  test("a project with no junit at all gets no junit — the whole alignment problem never arises") {
    // The row that used to inject bleep's own launcher/engine defaults into ScalaTest, munit and utest projects, giving them five junit artifacts they
    // never load and a conflict surface conjured out of nothing.
    val deps = rulesFor(("org.scalatest", "scalatest_3", "3.2.15"), ("org.scala-lang", "scala3-library_3", "3.3.3"))
    deps.map(d => s"${d.organization.value}:${d.baseModuleName.value}") shouldBe Set("org.scala-sbt:test-interface")
  }

  test("the sbt test interface is unconditional — every framework's Runner is loaded through it") {
    versionOf(rulesFor(), "org.scala-sbt", "test-interface") shouldBe model.Versions.TestInterface
  }

  test("no sbt junit adapter is injected — the platform runner never loads one") {
    // `jupiter-interface` used to be injected here, from when junit ran through `sbt.testing.Framework` like every other framework, and it needed a wildcard
    // junit exclusion because its own POM declares 1.9.1/5.9.1 and coursier reconciles to the highest version rather than to ours. It is gone:
    // `ForkedTestRunner` routes every junit framework name to `JUnitPlatformRunner`, which drives the Launcher directly, so `loadFramework`'s junit branch is
    // unreachable and the adapter was never loaded. Injecting it only widened the surface a version conflict could arrive from.
    val deps = rulesFor(junitPlatform("1.13.4")*)
    val adapters = deps.filter(d => Set("net.aichler", "com.github.sbt", "com.novocode").contains(d.organization.value))
    withClue(s"among ${deps.map(_.repr).toList.sorted.mkString(", ")}: ")(adapters shouldBe empty)
  }

  test("the junit artifacts themselves are NOT excluded — they must keep resolving commons and platform-engine") {
    val deps = rulesFor(junitPlatform("1.13.4")*)
    List("junit-platform-launcher", "junit-jupiter-engine", "junit-vintage-engine").foreach { name =>
      val dep = deps.collectFirst { case d if d.baseModuleName.value == name => d }.getOrElse(fail(s"$name missing"))
      withClue(s"$name must resolve transitively, or the fork loses junit-platform-commons: ") {
        dep.exclusions.value shouldBe empty
      }
    }
    succeed
  }

  private def modulesOf(modules: (String, String, String)*): List[bleep.ResolvedProject.ResolvedModule] =
    modules.map { case (org, name, version) => bleep.ResolvedProject.ResolvedModule(org, name, version, Nil) }.toList

  test("reads the junit-platform version from the project's resolved module graph") {
    val modules = modulesOf(
      ("io.kotest", "kotest-runner-junit5-jvm", "6.2.4"),
      ("org.junit.platform", "junit-platform-engine", "1.13.4"),
      ("org.junit.platform", "junit-platform-commons", "1.13.4")
    )
    MultiWorkspaceBspServer.singleResolvedVersionOf(project, "org.junit.platform", modules) shouldBe Some("1.13.4")
  }

  test("reads it when only junit-platform-commons is present") {
    // A project depending on junit-jupiter-api alone resolves commons and no engine, and commons is what the injected engine has to agree with.
    val modules = modulesOf(("org.junit.jupiter", "junit-jupiter-api", "5.13.4"), ("org.junit.platform", "junit-platform-commons", "1.13.4"))
    MultiWorkspaceBspServer.singleResolvedVersionOf(project, "org.junit.platform", modules) shouldBe Some("1.13.4")
  }

  test("no junit-platform at all reads as None") {
    MultiWorkspaceBspServer.singleResolvedVersionOf(project, "org.junit.platform", modulesOf(("org.scalatest", "scalatest_3", "3.2.15"))) shouldBe None
  }

  test("a project resolving two junit-platform versions fails with both named") {
    val modules = modulesOf(
      ("org.junit.platform", "junit-platform-commons", "1.9.1"),
      ("org.junit.platform", "junit-platform-engine", "1.13.4")
    )
    val e = intercept[BleepException] {
      MultiWorkspaceBspServer.singleResolvedVersionOf(project, "org.junit.platform", modules)
    }
    e.getMessage should include("org.junit.platform:junit-platform-commons:1.9.1")
    e.getMessage should include("org.junit.platform:junit-platform-engine:1.13.4")
  }

  test("the hard fail survives the table — a conflicting project throws instead of the rules picking one") {
    val resolved = resolvedWith(
      ("org.junit.platform", "junit-platform-commons", "1.9.1"),
      ("org.junit.platform", "junit-platform-engine", "1.13.4")
    )
    val e = intercept[BleepException](MultiWorkspaceBspServer.testRuntimeDeps(project, resolved))
    e.getMessage should include("1.9.1")
    e.getMessage should include("1.13.4")
  }

  test("a project resolved without a dependency graph fails rather than being guessed at") {
    val e = intercept[BleepException] {
      MultiWorkspaceBspServer.testRuntimeDeps(project, resolvedProject(None))
    }
    e.getMessage should include("resolved without a dependency graph")
  }

  test("a coherent assembled classpath passes") {
    val classpath = cp(
      "kotest-runner-junit5-jvm-6.2.3.jar",
      "junit-platform-commons-1.13.4.jar",
      "junit-platform-engine-1.13.4.jar",
      "junit-platform-launcher-1.13.4.jar",
      "junit-jupiter-api-5.13.4.jar",
      "junit-jupiter-engine-5.13.4.jar",
      "junit-vintage-engine-5.13.4.jar",
      "jupiter-interface-0.11.1.jar",
      "test-interface-1.0.jar"
    )
    MultiWorkspaceBspServer.assertCoherentJunitClasspath(project, classpath)
    succeed
  }

  test("the shipped kotest 6 classpath — aligned jars plus the runner POM's stale line — is rejected by coordinate") {
    // Verbatim shape of what `bleep build new --lang kotlin` forked before the fix: kotest's own 1.13.4 line first, then bleep-test-runner's transitive
    // 1.9.1/5.9.1, then the correctly aligned 1.13.4/5.13.4. The JVM took junit-platform-commons from the front and junit-jupiter-engine from the middle.
    val classpath = cp(
      "kotest-runner-junit5-jvm-6.2.3.jar",
      "junit-jupiter-api-5.13.4.jar",
      "junit-platform-commons-1.13.4.jar",
      "junit-platform-engine-1.13.4.jar",
      "junit-platform-launcher-1.13.4.jar",
      "jupiter-interface-0.11.1.jar",
      "junit-platform-launcher-1.9.1.jar",
      "junit-vintage-engine-5.9.1.jar",
      "junit-jupiter-engine-5.9.1.jar",
      "junit-platform-engine-1.9.1.jar",
      "junit-jupiter-api-5.9.1.jar",
      "junit-platform-commons-1.9.1.jar",
      "junit-vintage-engine-5.13.4.jar",
      "junit-jupiter-engine-5.13.4.jar"
    )
    val e = intercept[BleepException] {
      MultiWorkspaceBspServer.assertCoherentJunitClasspath(project, classpath)
    }
    // Versions are listed in classpath order, and the first one of each module is the one the JVM actually loads.
    e.getMessage should include("org.junit.jupiter:junit-jupiter-engine:5.9.1 —")
    e.getMessage should include("org.junit.jupiter:junit-jupiter-engine:5.13.4 —")
    e.getMessage should include("org.junit.platform:junit-platform-commons:1.13.4 —")
    e.getMessage should include("org.junit.platform:junit-platform-commons:1.9.1 —")
    e.getMessage should include("org.junit.vintage:junit-vintage-engine")
  }

  test("non-junit jars whose names start with junit are not mistaken for junit modules") {
    // `jupiter-interface` and the two sbt junit-interface adapters are on every test classpath and are versioned independently of the platform.
    MultiWorkspaceBspServer.assertCoherentJunitClasspath(project, cp("junit-interface-0.11.jar", "junit-interface-1.0.4.jar", "junit-4.13.2.jar"))
    succeed
  }
}
