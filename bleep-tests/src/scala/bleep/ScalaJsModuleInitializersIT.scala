package bleep

/** `jsModuleInitializers`: static methods the linked Scala.js program calls on start, in order, after bleep's own initializer. */
class ScalaJsModuleInitializersIT extends IntegrationTestHarness {

  private val app = model.CrossProjectName(model.ProjectName("app"), None)

  integrationTest("the program calls each initializer, in order, after the main method, with or without arguments") { ws =>
    ws.yaml(
      s"""projects:
         |  app:
         |    platform:
         |      name: js
         |      jsVersion: ${model.Versions.ScalaJs1}
         |      jsNodeVersion: ${model.Versions.Node}
         |      mainClass: example.Main
         |      jsModuleInitializers:
         |        - className: example.Inits
         |          method: noArgs
         |        - className: example.Inits
         |          method: withArgs
         |          args: [foo, bar]
         |        - className: example.Inits
         |          method: withArgs
         |          args: []
         |    scala:
         |      version: ${model.Versions.Scala3}
         |""".stripMargin
    )
    ws.file(
      "app/src/scala/example/Main.scala",
      """package example
        |
        |object Main {
        |  def main(args: Array[String]): Unit = println("init: main")
        |}
        |
        |object Inits {
        |  def noArgs(): Unit = println("init: noArgs")
        |  def withArgs(args: Array[String]): Unit = println("init: withArgs(" + args.mkString(",") + ")")
        |}
        |""".stripMargin
    )

    val (_, commands, storingLogger) = ws.start()
    commands.run(app)

    val printed = storingLogger.underlying.map(_.message.plainText).filter(_.startsWith("init: ")).toList
    assert(printed == List("init: main", "init: noArgs", "init: withArgs(foo,bar)", "init: withArgs()"), printed)
  }
}
