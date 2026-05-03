package bleep

class FirstScriptScalaIT extends IntegrationTestHarness {
  integrationTest("first-script-scala workspace compiles") { ws =>
    ws.yaml(
      snippet = "first-script-scala/bleep.yaml",
      content = s"""projects:
                   |  myapp:
                   |    platform:
                   |      name: jvm
                   |      mainClass: myapp.Main
                   |    scala:
                   |      version: ${model.VersionScala.Scala3.scalaVersion}
                   |  scripts:
                   |    dependencies:
                   |      - build.bleep:bleepscript:$${BLEEP_VERSION}
                   |    platform:
                   |      name: jvm
                   |    scala:
                   |      version: ${model.VersionScala.Scala3.scalaVersion}
                   |scripts:
                   |  hello:
                   |    main: scripts.HelloScript
                   |    project: scripts
                   |""".stripMargin
    )

    ws.file(
      "myapp/src/scala/myapp/Main.scala",
      snippet = "first-script-scala/Main.scala",
      content = """package myapp
                  |
                  |@main def helloMain(): Unit =
                  |  println("Hello from bleep!")
                  |""".stripMargin
    )

    ws.file(
      "scripts/src/scala/scripts/HelloScript.scala",
      snippet = "first-script-scala/HelloScript.scala",
      content = """package scripts
                  |
                  |import bleepscript.{BleepScript, Commands, Started}
                  |
                  |import scala.jdk.CollectionConverters.*
                  |
                  |class HelloScript extends BleepScript("hello") {
                  |  override def run(started: Started, commands: Commands, args: java.util.List[String]): Unit = {
                  |    val projectCount = started.build.explodedProjects.size
                  |    started.logger.info(s"This build has $projectCount projects")
                  |    args.asScala.toList match {
                  |      case Nil => started.logger.info("Hello, world!")
                  |      case xs  => started.logger.info(s"Hello, ${xs.mkString(" ")}!")
                  |    }
                  |  }
                  |}
                  |""".stripMargin
    )

    ws.compileAll()
    succeed
  }
}
