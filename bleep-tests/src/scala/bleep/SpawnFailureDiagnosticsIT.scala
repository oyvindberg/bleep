package bleep

/** When a test fork never connects back, the spawn failure has to say why — and the only witness is the fork itself.
  *
  * The message used to be "Accept timed out" and nothing else. That reads as a slow or overloaded machine, which sends anyone debugging it towards timeouts and
  * memory budgets, and it is the one thing the failure never is: the fork connects within milliseconds of reaching `main`, so a timeout means it did not get
  * there. The child's exit code and stderr say what stopped it, and both were being collected by the OS and dropped on the floor.
  *
  * Here the fork is given a JVM option no JVM accepts, so it dies during startup having written its complaint to stderr — the same shape as a bad `-Xmx`, a
  * missing class, or an unreadable jar. The other shape, a fork that starts fine and simply does not speak this protocol, is covered by
  * [[ShadowedTestRunnerIT]].
  */
class SpawnFailureDiagnosticsIT extends IntegrationTestHarness {

  private val Yaml =
    """projects:
      |  mytest:
      |    dependencies: org.scalatest::scalatest:3.2.15
      |    isTestProject: true
      |    platform:
      |      name: jvm
      |      jvmOptions: -XX:+ThisFlagDoesNotExist
      |    scala:
      |      version: 3.3.3
      |""".stripMargin

  private val Source =
    """package example
      |
      |import org.scalatest.funsuite.AnyFunSuite
      |
      |class DoomedTest extends AnyFunSuite {
      |  test("never runs, because its fork cannot start") {
      |    assert(true)
      |  }
      |}
      |""".stripMargin

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  integrationTest("a fork that dies on startup reports its exit code and its own stderr") { ws =>
    ws.yaml(Yaml)
    ws.file("mytest/src/scala/DoomedTest.scala", Source)
    val (_, commands, storingLogger) = ws.start()

    intercept[BleepException] {
      commands.test(
        projects = List(mytest),
        watch = false,
        only = None,
        exclude = None,
        includeTags = None,
        excludeTags = None
      )
    }

    val log = storingLogger.underlying.iterator.map(_.message.plainText).mkString("\n")
    assert(log.contains("without ever connecting"), s"the spawn failure did not describe the fork's fate:\n$log")
    // The JVM's own words. Quoting them is the entire point: this is the sentence that names the actual mistake, and no amount of detail about the timeout
    // could substitute for it.
    assert(log.contains("Unrecognized VM option"), s"the fork's stderr was not surfaced:\n$log")
  }
}
