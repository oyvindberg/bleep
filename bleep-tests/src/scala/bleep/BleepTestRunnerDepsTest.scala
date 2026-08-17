package bleep

import bleep.internal.FileUtils
import org.scalatest.funsuite.AnyFunSuite

/** `bleep-test-runner` is injected into every forked test JVM, so anything it declares transitively lands on a classpath next to the user's own test framework.
  * The junit runtime specifically is chosen per project by `MultiWorkspaceBspServer.testRuntimeRules`, at the version that project resolved — so the runner
  * must not carry a second opinion. Coursier reconciles to the highest version rather than to ours, so a junit line here does not lose quietly: it wins, and
  * the project runs discovery against an engine it never asked for (`NoSuchMethodError: ReflectionUtils.returnsVoid`, on kotest 6).
  *
  * `MultiWorkspaceBspServer.ExcludeTestRuntime` excludes these at the point of use as well. That is on purpose and this test is not made redundant by it: the
  * exclusion protects forks from POMs already published, and this protects the next one from being published wrong in the first place.
  */
class BleepTestRunnerDepsTest extends AnyFunSuite {

  private val testRuntimeOrgs = Set("org.junit.platform", "org.junit.jupiter", "org.junit.vintage", "net.aichler", "junit")

  private lazy val testRunnerDeps: List[model.Dep] = {
    val existing = BuildLoader.find(FileUtils.cwd).existing.orThrow
    val buildFile = existing.buildFile.forceGet("BleepTestRunnerDepsTest").orThrow
    val build = model.Build.FileBacked(buildFile)
    val (name, project) = build.explodedProjects
      .find { case (name, _) => name.name.value == "bleep-test-runner" }
      .getOrElse(sys.error("bleep-test-runner is not in this build"))
    assert(name.crossId.isEmpty, "bleep-test-runner is expected to be a single, non-cross project")
    project.dependencies.values.toList
  }

  test("no test runtime reaches a consumer's runtime classpath") {
    val leaking = testRunnerDeps.filter { dep =>
      testRuntimeOrgs.contains(dep.organization.value) && dep.configuration != coursier.core.Configuration.provided
    }
    assert(
      leaking.isEmpty,
      s"bleep-test-runner declares a test runtime that consumers inherit: ${leaking.map(_.repr).mkString(", ")}. " +
        "Either drop it, or mark it `configuration: provided` if bleep-test-runner compiles against it."
    )
  }

  test("the junit-platform-launcher version is the compatibility floor") {
    // `JUnitPlatformRunner` is compiled against this and executed against whatever the project resolved, because the rule table injects the launcher at the
    // project's own platform version rather than overriding a version the project picked. So this number is the OLDEST platform bleep can run, and raising it
    // strands projects silently — no resolution error, no diagnostic, just `NoSuchMethodError` inside the fork. 1.9.1 stranded every Spring Boot <= 2.5.
    //
    // Newer Launcher APIs are still reachable; they just have to be probed rather than imported, the way `openLauncher` handles `LauncherSession` (1.8+).
    // `JunitPlatformVersionRangeIT` runs both ends of the range for real; this only guards the declaration.
    val expected = "1.0.0"
    val launchers = testRunnerDeps.filter(_.baseModuleName.value == "junit-platform-launcher")
    assert(launchers.map(_.version) == List(expected), s"expected exactly one junit-platform-launcher at $expected, got ${launchers.map(_.repr)}")
  }

  test("only what it actually compiles against is declared at all") {
    // The two that were dropped were never imported — only named in `Class.forName` probe strings and in error messages — so their presence was pure POM
    // pollution. Named explicitly rather than derived, so re-adding either has to argue with this line.
    val neverImported = Set("jupiter-interface", "junit-vintage-engine")
    val found = testRunnerDeps.filter(dep => neverImported.contains(dep.baseModuleName.value))
    assert(
      found.isEmpty,
      s"bleep-test-runner declares ${found.map(_.repr).mkString(", ")}, which no source file imports. " +
        "ForkedTestRunner reaches jupiter-interface through Class.forName on a name string; vintage it only mentions in a diagnostic."
    )
  }
}
