package bleep

/** `jsRuntimeClassNameMapper`: what `getClass.getName` reports in the linked Scala.js program. */
class ScalaJsRuntimeClassNameMapperIT extends IntegrationTestHarness {

  private val app = model.CrossProjectName(model.ProjectName("app"), None)

  integrationTest("renames apply in order to the names the program reports; a name no rename matches is kept") { ws =>
    ws.yaml(
      s"""projects:
         |  app:
         |    platform:
         |      name: js
         |      jsVersion: ${model.Versions.ScalaJs1}
         |      jsNodeVersion: ${model.Versions.Node}
         |      mainClass: example.Main
         |      jsRuntimeClassNameMapper:
         |        - regex: ^example\\.Renamed$$
         |          replacement: renamed.First
         |        - regex: ^renamed\\.First$$
         |          replacement: renamed.Second
         |    scala:
         |      version: ${model.Versions.Scala3}
         |""".stripMargin
    )
    ws.file(
      "app/src/scala/example/Main.scala",
      """package example
        |
        |class Renamed
        |class Kept
        |
        |object Main {
        |  def main(args: Array[String]): Unit = {
        |    println("name: " + new Renamed().getClass.getName)
        |    println("name: " + new Kept().getClass.getName)
        |  }
        |}
        |""".stripMargin
    )

    val (_, commands, storingLogger) = ws.start()
    commands.run(app)

    val printed = storingLogger.underlying.map(_.message.plainText).filter(_.startsWith("name: ")).toList
    assert(printed == List("name: renamed.Second", "name: example.Kept"), printed)
  }
}
