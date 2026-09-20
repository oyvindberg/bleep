package bleep.testing

import bleep.model
import org.scalatest.funsuite.AnyFunSuite

/** What the "Tests (N running)" heading is allowed to count.
  *
  * The number is read as a statement about how hard the machine is being driven, so it being wrong is not cosmetic: a run over nine JVM projects with nineteen
  * suites in flight announced twenty-eight, on a machine whose governor admits eighteen — which reads as the scheduler over-subscribing, and sends you looking
  * at admission code that turns out to be correct.
  */
class RunningTestCountTest extends AnyFunSuite {

  private def project(name: String) = model.CrossProjectName(model.ProjectName(name), None)

  private def jvmProject(name: String) =
    ProjectDisplayItem.Testing.Reactive(
      project = project(name),
      suitesCompleted = 0,
      suitesTotal = 10,
      failures = 0,
      runningTests = Nil
    )

  private def platformProject(name: String) =
    ProjectDisplayItem.Testing.Bsp(project = project(name), platform = model.PlatformId.Js, elapsedMs = 1000L)

  test("a JVM project's suites are counted once, not once per suite plus once per project") {
    // The shape from the report: nine projects testing, nineteen suites actually running.
    val items = List.tabulate(9)(i => jvmProject(s"proj-$i"))
    assert(FancyBuildDisplay.runningTestCount(runningSuites = 19, displayItems = items) == 19)
  }

  test("a JS or Native project counts once, because it reports no suites of its own") {
    // These emit no per-suite events, so `runningSuites` knows nothing about them. Counting the project is the only way the heading reflects that they are
    // running at all.
    val items = List(platformProject("js-app"), platformProject("native-app"))
    assert(FancyBuildDisplay.runningTestCount(runningSuites = 0, displayItems = items) == 2)
  }

  test("a mixed run adds the platform projects to the suite count and nothing else") {
    val items = List(jvmProject("jvm-app"), platformProject("js-app"))
    assert(FancyBuildDisplay.runningTestCount(runningSuites = 5, displayItems = items) == 6)
  }

  test("nothing running is nothing running") {
    assert(FancyBuildDisplay.runningTestCount(runningSuites = 0, displayItems = Nil) == 0)
  }
}
