package bleep

/** A build may depend on bleep itself, and in an integration-test workspace those `build.bleep:*` deps are rewritten to bleep's own projects so the tests
  * exercise the bleep being built rather than a published one. That rewrite copies each bleep project's dependencies into the consumer's own set — and it used
  * to copy `provided` ones too, which Maven's rule says never travel to a consumer.
  *
  * `bleep-test-runner` is where that bites. It declares `provided org.junit.platform:junit-platform-launcher:1.0.0`, deliberately pinned to the oldest platform
  * bleep can run, so the runner compiles against a floor and executes against whatever the project resolved. Copied into a consumer, that floor stops being a
  * compile-time detail and becomes the junit-platform the project appears to have resolved — and `testRuntimeRules` pairs the junit engines to exactly that
  * version. The workspace then asked for `junit-vintage-engine:5.0.0`, which has never existed, and the run died in resolution naming a version nobody wrote.
  */
class BleepDepProvidedScopeIT extends IntegrationTestHarness {

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  integrationTest("a bleep dependency does not bring its provided deps with it") { ws =>
    ws.yaml(
      """projects:
        |  mytest:
        |    dependencies:
        |      - org.scalatest::scalatest:3.2.15
        |      - build.bleep:bleep-test-runner:1.0.0-M11
        |    isTestProject: true
        |    platform:
        |      name: jvm
        |    scala:
        |      version: 3.3.3
        |""".stripMargin
    )
    ws.file(
      "mytest/src/scala/T.scala",
      """package example
        |
        |import org.scalatest.funsuite.AnyFunSuite
        |
        |class T extends AnyFunSuite {
        |  test("runs despite depending on bleep") {
        |    assert(1 + 1 == 2)
        |  }
        |}
        |""".stripMargin
    )
    val (started, commands, storingLogger) = ws.start()

    // Asserted on the classpath rather than only on the run succeeding, because this is a statement about what a dependency drags along. The floor is what to
    // look for: any `org.junit.platform` here should be the project's own, and this project declares none.
    val floor = started.resolvedProject(mytest).classpath.map(_.toString).filter(_.contains("junit-platform")).toList
    assert(
      floor.isEmpty,
      s"bleep-test-runner's provided junit-platform floor reached a consumer's classpath:\n${floor.mkString("\n")}"
    )

    // And the run itself, since the version that floor produced was rejected during resolution — before any suite could start.
    commands.test(projects = List(mytest), watch = false, only = None, exclude = None, includeTags = None, excludeTags = None)
    assertSuitePassed(storingLogger, "example.T", tests = 1)
  }
}
