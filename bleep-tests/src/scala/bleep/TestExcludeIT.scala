package bleep

import cats.data.NonEmptyList

/** `testExclude`: suites a project's `bleep test` never runs. */
class TestExcludeIT extends IntegrationTestHarness {

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  private def writeFiles(ws: Workspace): Unit = {
    ws.yaml(
      s"""projects:
         |  mytest:
         |    dependencies: org.scalameta::munit:${model.Versions.Munit}
         |    isTestProject: true
         |    platform:
         |      name: jvm
         |    scala:
         |      version: ${model.Versions.Scala3}
         |    testExclude: example.broken.*
         |""".stripMargin
    )
    ws.file("mytest/src/scala/example/KeptSuite.scala", "package example\n\nclass KeptSuite extends munit.FunSuite {\n  test(\"runs\") {}\n}\n")
    ws.file(
      "mytest/src/scala/example/broken/BrokenSuite.scala",
      "package example.broken\n\nclass BrokenSuite extends munit.FunSuite {\n  test(\"fails\") { fail(\"excluded, so never run\") }\n}\n"
    )
  }

  integrationTest("an excluded suite is not run") { ws =>
    writeFiles(ws)
    val (_, commands, _) = ws.start()
    val summary = commands.test(List(mytest), watch = false, only = None, exclude = None, includeTags = None, excludeTags = None)
    assert(summary.suitesTotal == 1, summary)
    assert(summary.testsPassed == 1, summary)
  }

  integrationTest("naming an excluded suite with --only does not bring it back") { ws =>
    writeFiles(ws)
    val (_, commands, storingLogger) = ws.start()
    intercept[BleepException](
      commands.test(List(mytest), watch = false, only = Some(NonEmptyList.of("BrokenSuite")), exclude = None, includeTags = None, excludeTags = None)
    )
    val output = storingLogger.underlying.map(_.message.plainText).toList
    assert(output.exists(_.contains("--only matched no test suites")), output.mkString("\n"))
    assert(!output.exists(_.contains("excluded, so never run")), output.mkString("\n"))
  }
}
