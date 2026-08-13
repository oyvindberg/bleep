package bleep

import bleep.bsp.protocol.BleepBspProtocol

/** Environment variables reaching a forked test process.
  *
  * The value under test is deliberately a name no real environment sets, and every test asserts it is absent from THIS JVM before running. That guard is the
  * whole point: the test runner is forked, so a var that happened to be in the harness's own environment would be inherited by the child and the test would
  * pass without the BSP request carrying anything at all.
  */
class TestEnvPropagationIT extends IntegrationTestHarness {
  private val VarName = "BLEEP_ENV_PROPAGATION_IT"

  private val Yaml =
    """projects:
      |  mytest:
      |    dependencies: org.scalatest::scalatest:3.2.15
      |    isTestProject: true
      |    platform:
      |      name: jvm
      |    scala:
      |      version: 3.3.3
      |""".stripMargin

  /** Same yaml, but the build itself declares the variable. */
  private val YamlWithBuildEnv =
    s"""projects:
       |  mytest:
       |    dependencies: org.scalatest::scalatest:3.2.15
       |    isTestProject: true
       |    platform:
       |      name: jvm
       |      jvmEnvironment:
       |        $VarName: from-build
       |    scala:
       |      version: 3.3.3
       |""".stripMargin

  /** A suite that fails unless the forked JVM sees `expected` in its environment. */
  private def suiteExpecting(expected: String): String =
    s"""package example
       |
       |import org.scalatest.funsuite.AnyFunSuite
       |
       |class EnvTest extends AnyFunSuite {
       |  test("env var is visible to the forked test jvm") {
       |    assert(sys.env.get("$VarName") == Some("$expected"), s"got $${sys.env.get("$VarName")}")
       |  }
       |}
       |""".stripMargin

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  private def runTests(started: Started, clientEnv: Map[String, String]): Either[BleepException, Unit] =
    commands.ReactiveBsp
      .test(
        watch = false,
        projects = Array(mytest),
        displayMode = commands.DisplayMode.NoTui,
        jvmOptions = Nil,
        testArgs = Nil,
        only = Nil,
        exclude = Nil,
        includeTags = Nil,
        excludeTags = Nil,
        flamegraph = false,
        cancel = false,
        junitReportDir = None,
        diffBase = None,
        clientEnv = clientEnv
      )
      .run(started)

  integrationTest("client env reaches the forked test jvm") { ws =>
    assert(sys.env.get(VarName).isEmpty, s"$VarName must not be set in the harness JVM or this test proves nothing")
    ws.yaml(Yaml)
    ws.file("mytest/src/scala/EnvTest.scala", suiteExpecting("from-client"))
    val (started, _, _) = ws.start()
    runTests(started, Map(VarName -> "from-client")).orThrow
    succeed
  }

  integrationTest("without the client env the same suite fails (the assertion is load-bearing)") { ws =>
    assert(sys.env.get(VarName).isEmpty, s"$VarName must not be set in the harness JVM or this test proves nothing")
    ws.yaml(Yaml)
    ws.file("mytest/src/scala/EnvTest.scala", suiteExpecting("from-client"))
    val (started, _, _) = ws.start()
    assert(runTests(started, Map.empty).isLeft, "suite should fail when the env var is not forwarded")
    succeed
  }

  integrationTest("platform.jvmEnvironment outranks the ambient client env") { ws =>
    assert(sys.env.get(VarName).isEmpty, s"$VarName must not be set in the harness JVM or this test proves nothing")
    ws.yaml(YamlWithBuildEnv)
    ws.file("mytest/src/scala/EnvTest.scala", suiteExpecting("from-build"))
    val (started, _, _) = ws.start()
    // The client also sends a value. The build's deliberate declaration must win, so that a stray var in
    // someone's shell profile cannot silently override what the build states on purpose.
    runTests(started, Map(VarName -> "from-client")).orThrow
    succeed
  }

  integrationTest("client env does not disturb a build-declared var it never mentions") { ws =>
    assert(sys.env.get(VarName).isEmpty, s"$VarName must not be set in the harness JVM or this test proves nothing")
    ws.yaml(YamlWithBuildEnv)
    ws.file("mytest/src/scala/EnvTest.scala", suiteExpecting("from-build"))
    val (started, _, _) = ws.start()
    runTests(started, Map("SOMETHING_ELSE" -> "x")).orThrow
    succeed
  }

  integrationTest("CLASSPATH is never forwarded, so it cannot clobber the fork's own classpath") { ws =>
    // ClientEnv.capture drops it at the source; assert here as well as in the unit test because the
    // consequence of a regression is a test JVM that cannot see the classes it was asked to test.
    assert(BleepBspProtocol.ClientEnv.capture(Map("CLASSPATH" -> "/nope", "KEEP" -> "yes")) == Map("KEEP" -> "yes"))

    assert(sys.env.get(VarName).isEmpty, s"$VarName must not be set in the harness JVM or this test proves nothing")
    ws.yaml(Yaml)
    ws.file("mytest/src/scala/EnvTest.scala", suiteExpecting("from-client"))
    val (started, _, _) = ws.start()
    // A CLASSPATH in the client's shell must not reach the fork; if it did, the suite would not even load.
    runTests(started, BleepBspProtocol.ClientEnv.capture(Map(VarName -> "from-client", "CLASSPATH" -> "/nonexistent"))).orThrow
    succeed
  }
}
