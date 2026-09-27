package bleep

/** Which classes are JUnit 4 suites when `@RunWith` sits on a base class.
  *
  * `@RunWith` is `@Inherited`, and junit-interface fingerprints it, so reading the fingerprint through `getAnnotations` made every concrete subclass of a
  * `@RunWith` base a suite, helpers with no tests included, and JUnit failed each of them with "No runnable methods". Like sbt, only a class's own annotations
  * count; a class is still a suite when its test methods are inherited.
  */
class InheritedRunWithDiscoveryIT extends IntegrationTestHarness {

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  integrationTest("a helper under a @RunWith base is not a suite; a class with its own or inherited tests is") { ws =>
    ws.yaml(
      s"""projects:
         |  mytest:
         |    dependencies:
         |      - com.github.sbt:junit-interface:0.13.3
         |    isTestProject: true
         |    platform:
         |      name: jvm
         |    scala:
         |      version: ${model.Versions.Scala3}
         |""".stripMargin
    )
    ws.file(
      "mytest/src/scala/example/Suites.scala",
      """package example
        |
        |import org.junit.Test
        |import org.junit.runner.RunWith
        |import org.junit.runners.BlockJUnit4ClassRunner
        |
        |@RunWith(classOf[BlockJUnit4ClassRunner])
        |abstract class Base
        |
        |// a fixture base for other suites, with no tests of its own
        |class Helper extends Base
        |
        |class OwnTests extends Base {
        |  @Test def own(): Unit = ()
        |}
        |
        |abstract class TestsInBase {
        |  @Test def inherited(): Unit = ()
        |}
        |
        |class InheritsTests extends TestsInBase
        |""".stripMargin
    )

    val (_, commands, _) = ws.start()
    val summary = commands.test(List(mytest), watch = false, only = None, exclude = None, includeTags = None, excludeTags = None)
    assert(summary.suitesTotal == 2, summary)
    assert(summary.testsPassed == 2, summary)
  }
}
