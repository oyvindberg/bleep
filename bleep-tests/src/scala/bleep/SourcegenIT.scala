package bleep

class SourcegenIT extends IntegrationTestHarness {
  integrationTest("resource generator") { ws =>
    // Forked JVMs (sourcegen script + `bleep run`) get bounded heaps via `jvmRuntimeOptions`
    // so the total (test JVM + in-process BSP + forks) fits within the 7GB CI runner budget.
    val Yaml = """projects:
                 |  a:
                 |    extends: common
                 |    platform:
                 |      mainClass: test.Main
                 |      jvmRuntimeOptions: -Xmx512m -Xms64m
                 |    sourcegen: scripts/testscripts.SourceGen
                 |  scripts:
                 |    extends: common
                 |    dependencies: build.bleep::bleep-core:${BLEEP_VERSION}
                 |    platform:
                 |      jvmRuntimeOptions: -Xmx512m -Xms64m
                 |templates:
                 |  common:
                 |    platform:
                 |      name: jvm
                 |    scala:
                 |      version: 3.8.3
                 |""".stripMargin

    val Main = """package test
                 |
                 |object Main {
                 |  def main(args: Array[String]): Unit =
                 |    println("result: " + testgenerated.GeneratedSource.result)
                 |}
                 |""".stripMargin

    val SourceGen = """package testscripts
                      |
                      |import bleep.*
                      |import java.nio.file.Files
                      |
                      |object SourceGen extends BleepCodegenScript("SourceGen") {
                      |  def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
                      |    targets.foreach { target =>
                      |      val targetFile = target.sources / "testgenerated" / "GeneratedSource.scala"
                      |      Files.createDirectories(targetFile.getParent)
                      |      Files.writeString(
                      |        targetFile,
                      |        "package testgenerated;\n" +
                      |        "\n" +
                      |        "object GeneratedSource {" +
                      |        "  val result = 100" +
                      |        "}"
                      |      )
                      |      started.logger.withContext("targetFile", targetFile).warn("Wrote ")
                      |    }
                      |  }
                      |}
                      |""".stripMargin

    ws.yaml(Yaml)
    ws.file("a/src/scala/test/Main.scala", Main)
    ws.file("scripts/src/scala/testscripts/SourceGen.scala", SourceGen)
    val (_, commands, storingLogger) = ws.start()
    commands.run(model.CrossProjectName(model.ProjectName("a"), None))
    assert(storingLogger.underlying.exists(_.message.plainText == "result: 100"))
  }

  integrationTest("sourcegen script project that fails to compile: build fails cleanly, does not hang") { ws =>
    // Same shape as "resource generator" above, but the scripts project pins Scala 3.3.3 while
    // bleep-core it depends on is built for a newer Scala — the script project won't compile.
    // The DAG should route the failure as: Compile(scripts) fails → Sourcegen(..) Skipped →
    // Compile(a) Skipped → build fails with a BleepException. Before the sourcegen-in-DAG fix,
    // this scenario would hang the BSP handler via unsafeRunSync.
    ws.yaml(
      """projects:
        |  a:
        |    extends: common
        |    platform:
        |      mainClass: test.Main
        |    sourcegen: scripts/testscripts.SourceGen
        |  scripts:
        |    dependencies: build.bleep::bleep-core:${BLEEP_VERSION}
        |    platform:
        |      name: jvm
        |    scala:
        |      version: 3.3.3
        |templates:
        |  common:
        |    platform:
        |      name: jvm
        |    scala:
        |      version: 3.8.3
        |""".stripMargin
    )
    ws.file(
      "a/src/scala/test/Main.scala",
      """package test
        |
        |object Main {
        |  def main(args: Array[String]): Unit = println(testgenerated.GeneratedSource.result)
        |}
        |""".stripMargin
    )
    ws.file(
      "scripts/src/scala/testscripts/SourceGen.scala",
      """package testscripts
        |
        |import bleep.*
        |
        |object SourceGen extends BleepCodegenScript("SourceGen") {
        |  def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit =
        |    started.logger.info("should never run — this script project must fail to compile first")
        |}
        |""".stripMargin
    )

    val (_, commands, storingLogger) = ws.start()
    val thrown = intercept[BleepException] {
      commands.compile(List(model.CrossProjectName(model.ProjectName("a"), None)))
    }
    val msg = thrown.getMessage
    assert(msg.contains("compile") || msg.contains("failed"), s"unexpected failure message: $msg")

    val loggedLines = storingLogger.underlying.map(_.message.plainText)
    val scriptsCompileFailed = loggedLines.exists(l => l.contains("scripts") && (l.contains("failed") || l.contains("❌")))
    val targetSkipped = loggedLines.exists(l => l.contains("a") && (l.contains("Skipped") || l.contains("skipped") || l.contains("⏭️")))
    assert(scriptsCompileFailed || targetSkipped, s"expected evidence of scripts compile failure or target skip in logs; got: ${loggedLines.mkString("\n")}")

    val noGeneratedSource = !loggedLines.exists(_.contains("testgenerated.GeneratedSource"))
    assert(noGeneratedSource, "target compile should not have proceeded; sourcegen output should be absent")

    succeed
  }
}
