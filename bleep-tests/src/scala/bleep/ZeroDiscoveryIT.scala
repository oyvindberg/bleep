package bleep

import cats.data.NonEmptyList

/** End-to-end proof that a test project which discovers nothing fails the run.
  *
  * `BuildSummaryVerdictTest` pins the verdict given the events; this pins that the server actually produces those events. The two halves matter separately: the
  * whole bug was that a real build reported success, and a classifier that is right about hypothetical input cannot rule that out.
  */
class ZeroDiscoveryIT extends IntegrationTestHarness {

  /** `isTestProject: true` with no test framework anywhere on the classpath. It compiles; nothing claims it. */
  private val NoFrameworkYaml =
    """projects:
      |  mytest:
      |    isTestProject: true
      |    platform:
      |      name: jvm
      |    scala:
      |      version: 3.3.3
      |""".stripMargin

  private val WithFrameworkYaml =
    """projects:
      |  mytest:
      |    dependencies: org.scalatest::scalatest:3.2.15
      |    isTestProject: true
      |    platform:
      |      name: jvm
      |    scala:
      |      version: 3.3.3
      |""".stripMargin

  /** Looks like a test to a human and to nobody else — no framework on the classpath recognises it. */
  private val LooksLikeATest =
    """package example
      |
      |class NotReallyATest {
      |  def test(): Unit = ()
      |}
      |""".stripMargin

  private val RealTest =
    """package example
      |
      |import org.scalatest.funsuite.AnyFunSuite
      |
      |class RealTest extends AnyFunSuite {
      |  test("real") { assert(1 + 1 == 2) }
      |}
      |""".stripMargin

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  integrationTest("a test project with no framework on its classpath fails instead of reporting 0 tests") { ws =>
    ws.yaml(NoFrameworkYaml)
    ws.file("mytest/src/scala/NotReallyATest.scala", LooksLikeATest)
    val (_, commands, _) = ws.start()
    val thrown = intercept[BleepException] {
      commands.test(projects = List(mytest), watch = false, only = None, exclude = None, includeTags = None, excludeTags = None)
    }
    // The message has to name the project — "no tests ran" without a where is the same dead end as the silent pass.
    assert(thrown.getMessage.contains("No test suites found"), s"unexpected message: ${thrown.getMessage}")
    assert(thrown.getMessage.contains("mytest"), s"the failure must name the project, got: ${thrown.getMessage}")
  }

  integrationTest("a filter that matches nothing is still the user's choice, not a discovery failure") { ws =>
    // The scan finds RealTest; --exclude removes it. The run executes nothing and that is fine, because nothing is broken.
    ws.yaml(WithFrameworkYaml)
    ws.file("mytest/src/scala/RealTest.scala", RealTest)
    val (_, commands, _) = ws.start()
    commands.test(
      projects = List(mytest),
      watch = false,
      only = None,
      exclude = Some(NonEmptyList.of("RealTest")),
      includeTags = None,
      excludeTags = None
    )
    succeed
  }
}
