package bleep

import java.nio.file.Files

/** A Scala Native test project links once, for the project, and every suite runs the binary the linker produced.
  *
  * It used to link once per suite, on top of the link the dependency graph had already run — so a three-suite project linked four times. Worse than wasteful:
  * every suite computed the same output path, so the three suite links wrote one file, concurrently, while suites were starting to execute it.
  *
  * Nothing tied the two computations together. The graph's link resolved its output under `targetDir` while the suite path rebuilt a path of its own from
  * `classes.getParent` — which is the same directory, reached by different arithmetic, and neither knew about the other.
  */
class ScalaNativeSharedBinaryIT extends IntegrationTestHarness {

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  private def suite(n: Int): String =
    s"""package example
       |
       |class Suite$n extends munit.FunSuite {
       |  test("suite $n runs") {
       |    assertEquals(1 + $n, ${1 + n})
       |  }
       |}
       |""".stripMargin

  integrationTest("every suite runs the one binary the project's link produced") { ws =>
    ws.yaml(
      s"""projects:
         |  mytest:
         |    dependencies: org.scalameta::munit:1.0.4
         |    isTestProject: true
         |    platform:
         |      name: native
         |      nativeVersion: ${model.Versions.ScalaNative05}
         |      nativeGc: immix
         |      nativeLto: none
         |      nativeMode: debug
         |    scala:
         |      version: ${model.Versions.Scala3}
         |""".stripMargin
    )
    // Three, because one suite cannot show the defect: the damage was suites racing each other for a single file.
    (1 to 3).foreach(n => ws.file(s"mytest/src/scala/Suite$n.scala", suite(n)))
    val (started, commands, storingLogger) = ws.start()

    commands.test(projects = List(mytest), watch = false, only = None, exclude = None, includeTags = None, excludeTags = None)

    (1 to 3).foreach(n => assertSuitePassed(storingLogger, s"example.Suite$n", tests = 1))

    // The binary the per-suite link used to write, at the path all three suites computed for themselves. Asserted on rather than counting links, because the
    // count is not observable from here while the stray path is: if anything links per suite again, this file comes back.
    val strayBinary = started.projectPaths(mytest).classes.getParent.resolve("link-output").resolve("mytest-test")
    assert(
      !Files.exists(strayBinary),
      s"a per-suite link wrote its own binary at $strayBinary — the suites should be running the one the project's link produced"
    )
    succeed
  }
}
