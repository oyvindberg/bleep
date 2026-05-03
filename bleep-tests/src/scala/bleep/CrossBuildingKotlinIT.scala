package bleep

class CrossBuildingKotlinIT extends IntegrationTestHarness {
  integrationTest("kotlin multiplatform jvm and js") { ws =>
    ws.yaml(
      snippet = "cross-building-kotlin/bleep.yaml",
      content = s"""projects:
                   |  app:
                   |    kotlin:
                   |      version: ${model.VersionKotlin.Kotlin23.kotlinVersion}
                   |    cross:
                   |      jvm:
                   |        kotlin:
                   |          jvmTarget: "${model.Jvm.graalvm.majorVersion}"
                   |        platform:
                   |          mainClass: com.example.MainKt
                   |          name: jvm
                   |      js:
                   |        kotlin:
                   |          js:
                   |            outputMode: js
                   |            target: nodejs
                   |        platform:
                   |          name: js
                   |""".stripMargin
    )

    ws.file(
      "app/src/kotlin/com/example/Main.kt",
      snippet = "cross-building-kotlin/Main.kt",
      content = """package com.example
                  |
                  |fun main() {
                  |  val green = "\u001B[32m"
                  |  val cyan = "\u001B[36m"
                  |  val reset = "\u001B[0m"
                  |  println("${green}Hello${reset} from ${cyan}bleep${reset}!")
                  |}
                  |""".stripMargin
    )

    ws.compileAll()
    val (started, _, _) = ws.start()
    val crossIds = started.build.explodedProjects.keys.toList.flatMap(_.crossId).map(_.value).distinct.sorted
    assert(crossIds == List("js", "jvm"))
  }
}
