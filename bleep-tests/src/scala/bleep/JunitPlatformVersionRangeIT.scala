package bleep

/** Pins the range of JUnit Platform versions a forked test JVM can actually run.
  *
  * `JUnitPlatformRunner` drives the Platform's `Launcher` directly rather than going through an sbt test-interface bridge, so it is compiled against one
  * launcher version and executed against whatever version the project resolved — `MultiWorkspaceBspServer.testRuntimeRules` injects the launcher at the
  * project's own platform version, deliberately, so bleep never overrides a version the project chose.
  *
  * That makes the runner's compile-time API surface a *compatibility floor* rather than a preference, and nothing about it is visible at the call site. It was
  * wrong once: compiled against 1.9.1, it called `LauncherFactory.openSession()`, which does not exist before 1.8 — so every project on Jupiter 5.7 or older
  * died in the fork with `NoSuchMethodError` and no diagnostic. That band is not exotic. Spring Boot 2.4 and 2.5 pin junit-jupiter 5.7.2; 2.6 was the release
  * that moved to 5.8.
  *
  * Hence both ends and a middle. The old end is the regression that motivated the test, and is what fails if someone reaches for a newer Launcher API. The far
  * end is JUnit 6, which abandoned the `1.x` platform line — it catches both a `junitEngineVersionFor` that mangles the version and an API the runner leans on
  * being dropped upstream, which is the same failure as the original with the ends swapped.
  */
class JunitPlatformVersionRangeIT extends IntegrationTestHarness {

  /** Jupiter 5.7.2 -> platform 1.7.2, i.e. no `LauncherSession`. Spring Boot 2.4/2.5's pin. */
  private val oldJupiter = "5.7.2"

  /** A current-era 5.x pin, where the full session lifecycle is available. */
  private val newJupiter = "5.10.1"

  /** JUnit 6, where the platform stopped being versioned `1.x` and shares the `6.x` line with jupiter — so `junitEngineVersionFor`'s `1.x -> 5.x` mapping has
    * to fall through rather than mangle it. Also the far end of what one compiled runner has to reach.
    */
  private val junit6Jupiter = "6.1.3"

  integrationTest("junit platform, oldest and newest supported") { ws =>
    ws.yaml(
      content = s"""projects:
                   |  lib:
                   |    platform:
                   |      name: jvm
                   |  lib-old-test:
                   |    platform:
                   |      name: jvm
                   |    isTestProject: true
                   |    dependsOn: lib
                   |    dependencies:
                   |      - org.junit.jupiter:junit-jupiter:$oldJupiter
                   |  lib-new-test:
                   |    platform:
                   |      name: jvm
                   |    isTestProject: true
                   |    dependsOn: lib
                   |    dependencies:
                   |      - org.junit.jupiter:junit-jupiter:$newJupiter
                   |  lib-junit6-test:
                   |    platform:
                   |      name: jvm
                   |    isTestProject: true
                   |    dependsOn: lib
                   |    dependencies:
                   |      - org.junit.jupiter:junit-jupiter:$junit6Jupiter
                   |""".stripMargin
    )

    ws.file(
      "lib/src/java/com/example/Greeter.java",
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

    // One source, three projects. Only the resolved platform version differs, which is the whole point.
    List("lib-old-test" -> "OldPlatformTest", "lib-new-test" -> "NewPlatformTest", "lib-junit6-test" -> "Junit6PlatformTest").foreach { case (project, cls) =>
      ws.file(
        s"$project/src/java/com/example/$cls.java",
        content = s"""package com.example;
                     |
                     |import static org.junit.jupiter.api.Assertions.assertEquals;
                     |
                     |import org.junit.jupiter.api.Test;
                     |
                     |class $cls {
                     |  @Test
                     |  void greetsByName() {
                     |    assertEquals("Hello, world!", Greeter.hello("world"));
                     |  }
                     |
                     |  @Test
                     |  void greetsSomeoneElse() {
                     |    assertEquals("Hello, bleep!", Greeter.hello("bleep"));
                     |  }
                     |}
                     |""".stripMargin
      )
    }

    val (_, commands, storingLogger) = ws.start()
    commands.test(
      projects = List(
        model.CrossProjectName(model.ProjectName("lib-old-test"), None),
        model.CrossProjectName(model.ProjectName("lib-new-test"), None),
        model.CrossProjectName(model.ProjectName("lib-junit6-test"), None)
      ),
      watch = false,
      only = None,
      exclude = None,
      includeTags = None,
      excludeTags = None
    )

    assertSuitePassed(storingLogger, "com.example.OldPlatformTest", tests = 2)
    assertSuitePassed(storingLogger, "com.example.NewPlatformTest", tests = 2)
    assertSuitePassed(storingLogger, "com.example.Junit6PlatformTest", tests = 2)
  }
}
