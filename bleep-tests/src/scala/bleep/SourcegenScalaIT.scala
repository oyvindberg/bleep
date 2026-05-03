package bleep

class SourcegenScalaIT extends IntegrationTestHarness {
  integrationTest("sourcegen-scala workspace compiles") { ws =>
    ws.yaml(
      snippet = "sourcegen-scala/bleep.yaml",
      content = s"""projects:
                   |  myapp:
                   |    platform:
                   |      name: jvm
                   |      mainClass: com.example.Main
                   |    scala:
                   |      version: ${model.VersionScala.Scala3.scalaVersion}
                   |    sourcegen:
                   |      project: scripts
                   |      main: scripts.GenConstants
                   |  scripts:
                   |    dependencies:
                   |      - build.bleep:bleepscript:$${BLEEP_VERSION}
                   |    platform:
                   |      name: jvm
                   |    scala:
                   |      version: ${model.VersionScala.Scala3.scalaVersion}
                   |""".stripMargin
    )

    ws.file(
      "myapp/src/scala/com/example/Main.scala",
      snippet = "sourcegen-scala/Main.scala",
      content = """package com.example
                  |
                  |import generated.Constants
                  |
                  |@main def main(): Unit =
                  |  println(s"answer: ${Constants.ANSWER}")
                  |""".stripMargin
    )

    ws.file(
      "scripts/src/scala/scripts/GenConstants.scala",
      snippet = "sourcegen-scala/GenConstants.scala",
      content = """package scripts
                  |
                  |import bleepscript.{BleepCodegenScript, CodegenTarget, Commands, Started}
                  |
                  |import java.nio.file.Files
                  |import scala.jdk.CollectionConverters.*
                  |
                  |class GenConstants extends BleepCodegenScript("GenConstants") {
                  |  override def run(started: Started, commands: Commands, targets: java.util.List[CodegenTarget], args: java.util.List[String]): Unit =
                  |    targets.asScala.foreach { target =>
                  |      val file = target.sources.resolve("generated/Constants.scala")
                  |      Files.createDirectories(file.getParent)
                  |      Files.writeString(file,
                  |        "package generated\n" +
                  |        "\n" +
                  |        "object Constants:\n" +
                  |        "  val ANSWER: Int = 42\n"
                  |      )
                  |    }
                  |}
                  |""".stripMargin
    )

    ws.compileAll()
    succeed
  }
}
