package bleep

class TestProjectsIT extends IntegrationTestHarness {
  integrationTest("test-projects workspace compiles") { ws =>
    ws.yaml(
      snippet = "test-projects/bleep.yaml",
      content = """projects:
                  |  mylib:
                  |    extends: template-jvm
                  |  mylib-test:
                  |    extends: template-jvm
                  |    isTestProject: true
                  |    dependsOn: mylib
                  |    dependencies:
                  |      - org.junit.jupiter:junit-jupiter:5.10.1
                  |  mylib-it:
                  |    extends: template-jvm
                  |    isTestProject: true
                  |    dependsOn: mylib
                  |    dependencies:
                  |      - org.junit.jupiter:junit-jupiter:5.10.1
                  |      - org.testcontainers:testcontainers:1.20.4
                  |templates:
                  |  template-jvm:
                  |    platform:
                  |      name: jvm
                  |""".stripMargin
    )

    ws.file(
      "mylib/src/java/com/example/Greeter.java",
      snippet = "test-projects/Greeter.java",
      content = """package com.example;
                  |
                  |public final class Greeter {
                  |  private Greeter() {}
                  |
                  |  public static String hello(String name) {
                  |    return "Hello, " + name + "!";
                  |  }
                  |}
                  |""".stripMargin
    )

    ws.file(
      "mylib-test/src/java/com/example/GreeterTest.java",
      snippet = "test-projects/GreeterTest.java",
      content = """package com.example;
                  |
                  |import org.junit.jupiter.api.Test;
                  |import static org.junit.jupiter.api.Assertions.assertEquals;
                  |
                  |class GreeterTest {
                  |  @Test
                  |  void greetsByName() {
                  |    assertEquals("Hello, world!", Greeter.hello("world"));
                  |  }
                  |}
                  |""".stripMargin
    )

    ws.file(
      "mylib-it/src/java/com/example/GreeterIT.java",
      snippet = "test-projects/GreeterIT.java",
      content = """package com.example;
                  |
                  |import org.junit.jupiter.api.Test;
                  |import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;
                  |import static org.junit.jupiter.api.Assertions.assertEquals;
                  |
                  |class GreeterIT {
                  |  @Test
                  |  @EnabledIfEnvironmentVariable(named = "RUN_INTEGRATION", matches = "true")
                  |  void greetsAcrossLocales() {
                  |    // Pretend this hits a Testcontainers Postgres or similar.
                  |    assertEquals("Hello, world!", Greeter.hello("world"));
                  |  }
                  |}
                  |""".stripMargin
    )

    ws.compileAll()
    val (started, _, _) = ws.start()
    val testProjects = started.build.explodedProjects.values.count(_.isTestProject.contains(true))
    assert(testProjects == 2)
  }
}
