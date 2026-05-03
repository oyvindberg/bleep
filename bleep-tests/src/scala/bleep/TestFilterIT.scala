package bleep

import cats.data.NonEmptyList

class TestFilterIT extends IntegrationTestHarness {
  private val SingleSuiteYaml =
    """projects:
      |  mytest:
      |    dependencies: org.scalatest::scalatest:3.2.15
      |    isTestProject: true
      |    platform:
      |      name: jvm
      |    scala:
      |      version: 3.3.3
      |""".stripMargin

  private val MultiSuiteYaml = SingleSuiteYaml

  private val SingleSuiteSource =
    """package example
      |
      |import org.scalatest.funsuite.AnyFunSuite
      |
      |class MyTest extends AnyFunSuite {
      |  test("dummy test") {
      |    assert(1 + 1 == 2)
      |  }
      |}
      |""".stripMargin

  private val PassingSuite =
    """package example
      |
      |import org.scalatest.funsuite.AnyFunSuite
      |
      |class PassingTest extends AnyFunSuite {
      |  test("this passes") {
      |    assert(true)
      |  }
      |}
      |""".stripMargin

  private val FailingSuite =
    """package example
      |
      |import org.scalatest.funsuite.AnyFunSuite
      |
      |class FailingTest extends AnyFunSuite {
      |  test("this fails") {
      |    assert(false)
      |  }
      |}
      |""".stripMargin

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  integrationTest("test --only works before compilation") { ws =>
    ws.yaml(SingleSuiteYaml)
    ws.file("mytest/src/scala/MyTest.scala", SingleSuiteSource)
    val (_, commands, _) = ws.start()
    commands.test(
      projects = List(mytest),
      watch = false,
      only = Some(NonEmptyList.of("MyTest")),
      exclude = None
    )
    succeed
  }

  integrationTest("test --only with fully qualified class name") { ws =>
    ws.yaml(SingleSuiteYaml)
    ws.file("mytest/src/scala/MyTest.scala", SingleSuiteSource)
    val (_, commands, _) = ws.start()
    commands.test(
      projects = List(mytest),
      watch = false,
      only = Some(NonEmptyList.of("example.MyTest")),
      exclude = None
    )
    succeed
  }

  integrationTest("test --only filters to matching suite among multiple") { ws =>
    ws.yaml(MultiSuiteYaml)
    ws.file("mytest/src/scala/PassingTest.scala", PassingSuite)
    ws.file("mytest/src/scala/FailingTest.scala", FailingSuite)
    val (_, commands, _) = ws.start()
    commands.test(
      projects = List(mytest),
      watch = false,
      only = Some(NonEmptyList.of("PassingTest")),
      exclude = None
    )
    succeed
  }

  integrationTest("test --exclude skips matching suite") { ws =>
    ws.yaml(MultiSuiteYaml)
    ws.file("mytest/src/scala/PassingTest.scala", PassingSuite)
    ws.file("mytest/src/scala/FailingTest.scala", FailingSuite)
    val (_, commands, _) = ws.start()
    commands.test(
      projects = List(mytest),
      watch = false,
      only = None,
      exclude = Some(NonEmptyList.of("FailingTest"))
    )
    succeed
  }

  integrationTest("test --only with nonexistent suite fails") { ws =>
    ws.yaml(SingleSuiteYaml)
    ws.file("mytest/src/scala/MyTest.scala", SingleSuiteSource)
    val (_, commands, _) = ws.start()
    assertThrows[BleepException] {
      commands.test(
        projects = List(mytest),
        watch = false,
        only = Some(NonEmptyList.of("NonExistentTest")),
        exclude = None
      )
    }
  }
}
