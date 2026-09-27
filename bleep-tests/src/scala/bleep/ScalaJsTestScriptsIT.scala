package bleep

/** `jsTestScripts`: JavaScript files a Scala.js test project loads before its linked test program, found among its resources. */
class ScalaJsTestScriptsIT extends IntegrationTestHarness {

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  private def yaml(scripts: String): String =
    s"""projects:
       |  mytest:
       |    dependencies: org.scalameta::munit:${model.Versions.Munit}
       |    isTestProject: true
       |    platform:
       |      name: js
       |      jsVersion: ${model.Versions.ScalaJs1}
       |      jsNodeVersion: ${model.Versions.Node}
       |      jsTestScripts: $scripts
       |    scala:
       |      version: ${model.Versions.Scala3}
       |""".stripMargin

  private val Suite =
    """package example
      |
      |import scala.scalajs.js
      |
      |class NativesSuite extends munit.FunSuite {
      |  test("a global the preloaded script defined") {
      |    assertEquals(js.Dynamic.global.preloadedByBleep.asInstanceOf[String], "yes")
      |  }
      |}
      |""".stripMargin

  private def runTests(ws: Workspace): testing.BuildSummary = {
    val (_, commands, _) = ws.start()
    commands.test(List(mytest), watch = false, only = None, exclude = None, includeTags = None, excludeTags = None)
  }

  integrationTest("a script named under jsTestScripts is loaded before the tests run") { ws =>
    ws.yaml(yaml("natives.js"))
    ws.file("mytest/src/resources/natives.js", "(function() { this.preloadedByBleep = \"yes\"; }).call(typeof global !== \"undefined\" ? global : this);\n")
    ws.file("mytest/src/scala/example/NativesSuite.scala", Suite)

    val summary = runTests(ws)
    assert(summary.testsPassed == 1, summary)
  }

  integrationTest("a script that is not among the project's resources fails the run, naming it") { ws =>
    ws.yaml(yaml("missing.js"))
    ws.file("mytest/src/scala/example/NativesSuite.scala", Suite)

    val (_, commands, storingLogger) = ws.start()
    intercept[BleepException](commands.test(List(mytest), watch = false, only = None, exclude = None, includeTags = None, excludeTags = None))
    // reported on the suite that could not run, where a reader looks
    val output = storingLogger.underlying.map(_.message.plainText).toList
    assert(output.exists(_.contains("jsTestScripts names missing.js")), output.mkString("\n"))
  }
}
