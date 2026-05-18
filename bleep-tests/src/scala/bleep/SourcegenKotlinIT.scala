package bleep

class SourcegenKotlinIT extends IntegrationTestHarness {
  // Split workspace setup from compile so each phase gets its own suite-idle-timer reset window. Kotlin compilation pulls a fresh kotlinc into memory plus the
  // bleepscript dep tree, which under parallel-suite CPU contention easily exceeds 2 minutes; splitting (a) compiling just the `scripts` codegen project and
  // (b) compiling the rest of the workspace gives the timer two reset events instead of one.
  private def writeFiles(ws: Workspace): Unit = {
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
  }

  integrationTest("sourcegen-kotlin: scripts project compiles (codegen runtime ready)") { ws =>
    writeFiles(ws)
    val (_, commands, _) = ws.start()
    commands.compile(List(model.CrossProjectName(model.ProjectName("scripts"), None)))
    succeed
  }

  integrationTest("sourcegen-kotlin: full workspace compiles (sourcegen runs, myapp builds)") { ws =>
    writeFiles(ws)
    ws.compileAll()
    succeed
  }
}
