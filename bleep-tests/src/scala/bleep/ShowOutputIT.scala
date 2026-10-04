package bleep

/** `bleep test --show-output`: a passing suite's stdout/stderr is printed only when asked for. Without the flag it is dropped, as it always was; failing suites
  * print theirs regardless.
  */
class ShowOutputIT extends IntegrationTestHarness {
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

  private val Suite =
    """package example
      |
      |import org.scalatest.funsuite.AnyFunSuite
      |
      |class ChattyTest extends AnyFunSuite {
      |  test("passes while talking") {
      |    println("chatty-stdout-line")
      |    System.err.println("chatty-stderr-line")
      |    assert(1 + 1 == 2)
      |  }
      |}
      |""".stripMargin

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  private def runTests(started: Started, showOutput: Boolean): Unit =
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
        showOutput = showOutput,
        diffBase = None,
        diffOutput = bleep.OutputMode.Text,
        clientEnv = Map.empty
      )
      .run(started)
      .orThrow

  integrationTest("a passing suite's stdout and stderr are printed with --show-output, and not without it") { ws =>
    ws.yaml(Yaml)
    ws.file("mytest/src/scala/ChattyTest.scala", Suite)
    val (started, _, storingLogger) = ws.start()
    def log: String = storingLogger.underlying.iterator.map(_.message.plainText).mkString("\n")

    runTests(started, showOutput = false)
    assert(!log.contains("chatty-stdout-line"), log)
    assert(!log.contains("Output from passing suites"), log)

    runTests(started, showOutput = true)
    assert(log.contains("Output from passing suites (1)"), log)
    assert(log.contains("mytest / example.ChattyTest"), log)
    // the stream each line was written to survives to the printout
    assert(log.contains("| chatty-stdout-line"), log)
    assert(log.contains("! chatty-stderr-line"), log)
    // and is logged at the matching level: stdout as info, stderr as a warning
    def levelOf(needle: String): List[ryddig.LogLevel] =
      storingLogger.underlying.iterator.filter(_.message.plainText.contains(needle)).map(_.metadata.logLevel).toList
    assert(levelOf("| chatty-stdout-line") == List(ryddig.LogLevel.info), levelOf("| chatty-stdout-line"))
    assert(levelOf("! chatty-stderr-line") == List(ryddig.LogLevel.warn), levelOf("! chatty-stderr-line"))
  }
}
