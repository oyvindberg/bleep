package bleep

class FirstScriptKotlinIT extends IntegrationTestHarness {
  integrationTest("first-script-kotlin workspace compiles") { ws =>
    ws.yaml(
      snippet = "first-script-kotlin/bleep.yaml",
      content = s"""projects:
                   |  myapp:
                   |    kotlin:
                   |      jvmTarget: "${model.Jvm.graalvm.majorVersion}"
                   |      version: ${model.VersionKotlin.Kotlin23.kotlinVersion}
                   |    platform:
                   |      name: jvm
                   |      mainClass: com.example.MainKt
                   |  scripts:
                   |    dependencies:
                   |      - build.bleep:bleepscript:$${BLEEP_VERSION}
                   |    kotlin:
                   |      jvmTarget: "${model.Jvm.graalvm.majorVersion}"
                   |      version: ${model.VersionKotlin.Kotlin23.kotlinVersion}
                   |    platform:
                   |      name: jvm
                   |scripts:
                   |  hello:
                   |    main: scripts.HelloScript
                   |    project: scripts
                   |""".stripMargin
    )

    ws.file(
      "myapp/src/kotlin/com/example/Main.kt",
      snippet = "first-script-kotlin/Main.kt",
      content = """package com.example
                  |
                  |fun main() {
                  |  println("Hello from bleep!")
                  |}
                  |""".stripMargin
    )

    ws.file(
      "scripts/src/kotlin/scripts/HelloScript.kt",
      snippet = "first-script-kotlin/HelloScript.kt",
      content = """package scripts
                  |
                  |import bleepscript.BleepScript
                  |import bleepscript.Commands
                  |import bleepscript.Started
                  |
                  |class HelloScript : BleepScript("hello") {
                  |  override fun run(started: Started, commands: Commands, args: List<String>) {
                  |    val projectCount = started.build().explodedProjects().size
                  |    started.logger().info("This build has $projectCount projects")
                  |    if (args.isEmpty()) {
                  |      started.logger().info("Hello, world!")
                  |    } else {
                  |      started.logger().info("Hello, ${args.joinToString(" ")}!")
                  |    }
                  |  }
                  |}
                  |""".stripMargin
    )

    ws.compileAll()
    succeed
  }
}
