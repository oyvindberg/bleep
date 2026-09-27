package bleep

/** JUnit on Scala.js: `scalajs-junit-test-runtime`, whose suites the compiler registers through generated bootstrapper modules.
  *
  * Neither discovery route that finds JVM JUnit applies: `org.junit.Test` is a Scala annotation on Scala.js, invisible to reflection, and the fingerprint the
  * framework declares is never on the class. So these suites were compiled, linked, and then reported as "no test framework claimed them".
  */
class ScalaJsJUnitIT extends IntegrationTestHarness {

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  private def yaml(extraDeps: List[String]): String =
    s"""projects:
       |  mytest:
       |    dependencies:
       |      - org.scala-js:scalajs-junit-test-runtime_2.13:${model.Versions.ScalaJs1}
       |${extraDeps.map(d => s"      - $d\n").mkString}    isTestProject: true
       |    platform:
       |      name: js
       |      jsVersion: ${model.Versions.ScalaJs1}
       |      jsNodeVersion: ${model.Versions.Node}
       |    scala:
       |      version: ${model.Versions.Scala3}
       |""".stripMargin

  private val JUnitSuite =
    """package example
      |
      |import org.junit.{Ignore, Test}
      |import org.junit.Assert.assertEquals
      |
      |class JUnitSuite {
      |  @Test def adds(): Unit = assertEquals(2, 1 + 1)
      |  @Test def measures(): Unit = assertEquals(5, "hello".length)
      |  @Ignore("skipped on purpose") @Test def skipped(): Unit = assertEquals(1, 1)
      |}
      |""".stripMargin

  private def runTests(ws: Workspace): testing.BuildSummary = {
    val (_, commands, _) = ws.start()
    commands.test(List(mytest), watch = false, only = None, exclude = None, includeTags = None, excludeTags = None)
  }

  integrationTest("JUnit suites on Scala.js are discovered and run") { ws =>
    ws.yaml(yaml(Nil))
    ws.file("mytest/src/scala/example/JUnitSuite.scala", JUnitSuite)

    val summary = runTests(ws)
    assert(summary.suitesTotal == 1, summary)
    assert(summary.testsPassed == 2, summary)
    assert(summary.testsSkipped + summary.testsIgnored == 1, summary)
  }

  integrationTest("a failing JUnit test on Scala.js fails the run") { ws =>
    ws.yaml(yaml(Nil))
    ws.file(
      "mytest/src/scala/example/FailingSuite.scala",
      """package example
        |
        |import org.junit.Test
        |import org.junit.Assert.assertEquals
        |
        |class FailingSuite {
        |  @Test def failsOnPurpose(): Unit = assertEquals(2, 1)
        |}
        |""".stripMargin
    )

    val e = intercept[BleepException](runTests(ws))
    assert(e.getMessage.contains("fail"), e.getMessage)
  }

  integrationTest("next to another framework, each suite runs under the framework it was discovered with") { ws =>
    // munit's Scala.js artifact is on the classpath too, so the linked program holds two frameworks; each suite must be handed to its own
    ws.yaml(yaml(List(s"org.scalameta::munit:${model.Versions.Munit}")))
    ws.file("mytest/src/scala/example/JUnitSuite.scala", JUnitSuite)
    ws.file(
      "mytest/src/scala/example/MunitSuite.scala",
      """package example
        |
        |class MunitSuite extends munit.FunSuite {
        |  test("greets") { assertEquals("hi".length, 2) }
        |}
        |""".stripMargin
    )

    val summary = runTests(ws)
    assert(summary.suitesTotal == 2, summary)
    assert(summary.testsPassed == 3, summary)
  }
}
