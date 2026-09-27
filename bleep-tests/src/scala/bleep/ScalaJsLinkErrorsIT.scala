package bleep

import bleep.commands.{DisplayMode, LinkOptions, ReactiveBsp}

/** A failed Scala.js link says why. The linker reports its errors through its logger and then fails with only "There were linking errors". */
class ScalaJsLinkErrorsIT extends IntegrationTestHarness {

  private val app = model.CrossProjectName(model.ProjectName("app"), None)

  integrationTest("a link that fails names what the linker could not find") { ws =>
    ws.yaml(
      s"""projects:
         |  app:
         |    platform:
         |      name: js
         |      jsVersion: ${model.Versions.ScalaJs1}
         |      jsNodeVersion: ${model.Versions.Node}
         |      mainClass: example.DoesNotExist
         |    scala:
         |      version: ${model.Versions.Scala3}
         |""".stripMargin
    )
    ws.file("app/src/scala/example/Main.scala", "package example\n\nobject Main {\n  def main(args: Array[String]): Unit = println(\"hi\")\n}\n")

    val (started, _, storingLogger) = ws.start()
    val result = ReactiveBsp
      .link(
        watch = false,
        projects = Array(app),
        displayMode = DisplayMode.NoTui,
        options = LinkOptions(releaseMode = false, sourceMaps = None, minify = None, moduleKind = None, lto = None, optimize = None, debugInfo = None),
        flamegraph = false,
        cancel = false
      )
      .run(started)
    assert(result.isLeft, "the link should fail")
    val output = storingLogger.underlying.map(_.message.plainText).toList
    assert(output.exists(_.contains("Referring to non-existent class example.DoesNotExist")), output.mkString("\n"))
  }
}
