package bleep

class JvmRunIT extends IntegrationTestHarness {
  integrationTest("run prefer jvmRuntimeOptions") { ws =>
    ws.yaml(
      """projects:
        |  a:
        |    platform:
        |      name: jvm
        |      jvmRuntimeOptions: -Xmx512m -Xms64m -Dfoo=2
        |      jvmOptions: -Dfoo=1
        |      mainClass: test.Main
        |    scala:
        |      version: 3.4.2
        |""".stripMargin
    )
    ws.file(
      "a/src/scala/Main.scala",
      """package test
        |object Main {
        |  def main(args: Array[String]): Unit =
        |    println("foo was: " + sys.props("foo"))
        |}""".stripMargin
    )
    val (_, commands, storingLogger) = ws.start()
    commands.run(model.CrossProjectName(model.ProjectName("a"), None))
    assert(storingLogger.underlying.exists(_.message.plainText == "foo was: 2"))
  }

  integrationTest("run fallback to jvmOptions") { ws =>
    ws.yaml(
      """projects:
        |  a:
        |    platform:
        |      name: jvm
        |      jvmOptions: -Xmx512m -Xms64m -Dfoo=1
        |      mainClass: test.Main
        |    scala:
        |      version: 3.4.2
        |""".stripMargin
    )
    ws.file(
      "a/src/scala/Main.scala",
      """package test
        |object Main {
        |  def main(args: Array[String]): Unit =
        |    println("foo was: " + sys.props("foo"))
        |}""".stripMargin
    )
    val (_, commands, storingLogger) = ws.start()
    commands.run(model.CrossProjectName(model.ProjectName("a"), None))
    assert(storingLogger.underlying.exists(_.message.plainText == "foo was: 1"))
  }
}
