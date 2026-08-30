package bleep

/** A project may legitimately depend on bleep itself — build scripts, plugins, anything pulling `build.bleep::bleep-core`. That drags a *released*
  * `bleep-test-runner` onto the project's own classpath, alongside the one the server puts there to drive the fork.
  *
  * Which of the two the fork loads is decided by classpath order, and it used to be the project's: `getTestClasspath` appended the server's runner after the
  * dependencies, so first-match-wins classloading handed the fork the released copy. The two do not speak the same protocol — a released runner reads commands
  * from stdin, while the server waits for a connect-back on a loopback socket — so neither side ever moved and every suite in the project died on the 60-second
  * accept timeout, having run nothing.
  *
  * It presented as an infrastructure fault rather than a misconfiguration, which is what made it expensive to find: the fork was alive and healthy the whole
  * time, blocked in `readLine`, so nothing crashed and there was no stderr to explain it. It was also invisible to every existing test, because bleep's own
  * suites never put a second `bleep-test-runner` on a test classpath.
  *
  * The conflicting class comes from a sibling project rather than from a real released artifact. A dependency's classes reach the fork by the same route
  * whether they arrive as a jar or as another project's output — both land in `dependencyClasspath` — and this way the test states the collision outright
  * instead of resolving bleep's own release to imply it, and cannot rot when those releases age out.
  */
class ShadowedTestRunnerIT extends IntegrationTestHarness {

  private val Yaml =
    """projects:
      |  legacy-runner:
      |    platform:
      |      name: jvm
      |    scala:
      |      version: 3.3.3
      |  mytest:
      |    dependencies: org.scalatest::scalatest:3.2.15
      |    dependsOn: legacy-runner
      |    isTestProject: true
      |    platform:
      |      name: jvm
      |    scala:
      |      version: 3.3.3
      |""".stripMargin

  /** Stands in for a `bleep-test-runner` predating the protocol socket: it waits for orders on stdin, which this server will never send. If the fork loads this
    * class instead of the real runner, the spawn can only end in the accept timeout — which is the whole point, and what makes the assertion below meaningful.
    */
  private val LegacyRunner =
    """package bleep.testing.runner
      |
      |object ForkedTestRunner {
      |  def main(args: Array[String]): Unit = {
      |    val in = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
      |    while (in.readLine() != null) ()
      |    Thread.sleep(Long.MaxValue)
      |  }
      |}
      |""".stripMargin

  private val Source =
    """package example
      |
      |import org.scalatest.funsuite.AnyFunSuite
      |
      |class ShadowedTest extends AnyFunSuite {
      |  test("the suite runs even though a dependency supplies its own ForkedTestRunner") {
      |    assert(1 + 1 == 2)
      |  }
      |}
      |""".stripMargin

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  integrationTest("a dependency's bleep-test-runner does not shadow the server's") { ws =>
    ws.yaml(Yaml)
    ws.file("legacy-runner/src/scala/bleep/testing/runner/ForkedTestRunner.scala", LegacyRunner)
    ws.file("mytest/src/scala/ShadowedTest.scala", Source)
    val (_, commands, storingLogger) = ws.start()

    commands.test(
      projects = List(mytest),
      watch = false,
      only = None,
      exclude = None,
      includeTags = None,
      excludeTags = None
    )

    // Asserted on a passing count rather than on `commands.test` merely not throwing: a run that discovers a suite and executes none of it is the shape this
    // whole family of bugs takes, and only a count rules it out.
    assertSuitePassed(storingLogger, "example.ShadowedTest", tests = 1)
  }
}
