package bleep

class SourcegenKotlinIT extends IntegrationTestHarness {
  integrationTest("sourcegen-kotlin workspace compiles") { ws =>
    ws.yaml(
      snippet = "sourcegen-kotlin/bleep.yaml",
      content = s"""projects:
                   |  myapp:
                   |    kotlin:
                   |      jvmTarget: "${model.Jvm.graalvm.majorVersion}"
                   |      version: ${model.VersionKotlin.Kotlin23.kotlinVersion}
                   |    platform:
                   |      name: jvm
                   |      mainClass: com.example.MainKt
                   |    sourcegen:
                   |      project: scripts
                   |      main: scripts.GenConstants
                   |  scripts:
                   |    dependencies:
                   |      - build.bleep:bleepscript:$${BLEEP_VERSION}
                   |    kotlin:
                   |      jvmTarget: "${model.Jvm.graalvm.majorVersion}"
                   |      version: ${model.VersionKotlin.Kotlin23.kotlinVersion}
                   |    platform:
                   |      name: jvm
                   |""".stripMargin
    )

    ws.file(
      "myapp/src/kotlin/com/example/Main.kt",
      snippet = "sourcegen-kotlin/Main.kt",
      content = """package com.example
                  |
                  |import generated.Constants
                  |
                  |fun main() {
                  |  println("answer: ${Constants.ANSWER}")
                  |}
                  |""".stripMargin
    )

    ws.file(
      "scripts/src/kotlin/scripts/GenConstants.kt",
      snippet = "sourcegen-kotlin/GenConstants.kt",
      content = """package scripts
                  |
                  |import bleepscript.BleepCodegenScript
                  |import bleepscript.CodegenTarget
                  |import bleepscript.Commands
                  |import bleepscript.Started
                  |import java.nio.file.Files
                  |
                  |class GenConstants : BleepCodegenScript("GenConstants") {
                  |  override fun run(started: Started, commands: Commands, targets: List<CodegenTarget>, args: List<String>) {
                  |    for (target in targets) {
                  |      val file = target.sources().resolve("generated/Constants.kt")
                  |      Files.createDirectories(file.parent)
                  |      Files.writeString(file,
                  |        "package generated\n\n" +
                  |        "object Constants {\n" +
                  |        "  const val ANSWER: Int = 42\n" +
                  |        "}\n"
                  |      )
                  |    }
                  |  }
                  |}
                  |""".stripMargin
    )

    ws.compileAll()
    succeed
  }
}
