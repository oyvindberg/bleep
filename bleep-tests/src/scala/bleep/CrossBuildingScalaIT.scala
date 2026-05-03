package bleep

class CrossBuildingScalaIT extends IntegrationTestHarness {
  integrationTest("scala cross-version on jvm") { ws =>
    ws.yaml(
      snippet = "cross-building-scala/bleep.yaml",
      content = s"""projects:
                   |  mylib:
                   |    extends: template-cross
                   |  mylib-test:
                   |    dependencies:
                   |      - org.scalameta::munit:1.0.0
                   |    dependsOn: mylib
                   |    extends: template-cross
                   |    isTestProject: true
                   |templates:
                   |  template-cross:
                   |    cross:
                   |      jvm213:
                   |        scala:
                   |          version: ${model.VersionScala.Scala213.scalaVersion}
                   |      jvm3:
                   |        scala:
                   |          version: ${model.VersionScala.Scala3.scalaVersion}
                   |    platform:
                   |      name: jvm
                   |""".stripMargin
    )

    ws.file(
      "mylib/src/scala/mylib/Greeting.scala",
      snippet = "cross-building-scala/Greeting.scala",
      content = """package mylib
                  |
                  |object Greeting {
                  |  def hello(name: String): String = s"Hello, $name!"
                  |}
                  |""".stripMargin
    )

    ws.file(
      "mylib-test/src/scala/mylib/GreetingTest.scala",
      snippet = "cross-building-scala/GreetingTest.scala",
      content = """package mylib
                  |
                  |class GreetingTest extends munit.FunSuite {
                  |  test("hello") {
                  |    assertEquals(Greeting.hello("world"), "Hello, world!")
                  |  }
                  |}
                  |""".stripMargin
    )

    ws.compileAll()
    val (started, _, _) = ws.start()
    val crossIds = started.build.explodedProjects.keys.toList.flatMap(_.crossId).map(_.value).distinct.sorted
    assert(crossIds == List("jvm213", "jvm3"))
  }
}
