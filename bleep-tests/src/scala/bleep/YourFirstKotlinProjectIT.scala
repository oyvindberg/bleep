package bleep

import bleep.commands.BuildCreateNew

class YourFirstKotlinProjectIT extends IntegrationTestHarness {
  private val myappTest = model.CrossProjectName(model.ProjectName("myapp-test"), None)

  private def runTests(commands: Commands): Unit =
    commands.test(
      projects = List(myappTest),
      watch = false,
      only = None,
      exclude = None,
      includeTags = None,
      excludeTags = None
    )

  /** A kotest project, parameterised on the kotest version. Deliberately not `bleep build new`'s output — the scaffold pins one version, and the point of the
    * two tests below is that both JUnit Platform lines work: kotest 5 brings platform 1.8.2, kotest 6 brings 1.13.4.
    */
  private def kotestYaml(kotestVersion: String): String =
    s"""projects:
       |  myapp:
       |    kotlin:
       |      version: ${model.Versions.Kotlin24}
       |    platform:
       |      name: jvm
       |  myapp-test:
       |    dependencies: io.kotest:kotest-runner-junit5-jvm:$kotestVersion
       |    dependsOn: myapp
       |    isTestProject: true
       |    kotlin:
       |      version: ${model.Versions.Kotlin24}
       |    platform:
       |      name: jvm
       |""".stripMargin

  private val mainSrc =
    """package com.example
      |
      |object Main {
      |  fun greet(name: String): String = "Hello, $name!"
      |}
      |""".stripMargin

  private val testSrc =
    """package com.example
      |
      |import io.kotest.core.spec.style.FunSpec
      |import io.kotest.matchers.shouldBe
      |
      |class MainTest : FunSpec({
      |  test("greets by name") {
      |    Main.greet("World") shouldBe "Hello, World!"
      |  }
      |})
      |""".stripMargin

  integrationTest("bleep new myapp --lang kotlin") { ws =>
    ws.bleepNew(BuildCreateNew.Language.Kotlin, "myapp")
    ws.attachSnippet("bleep.yaml", "your-first-kotlin-project/bleep.yaml")
    ws.attachSnippet("myapp/src/kotlin/com/example/Main.kt", "your-first-kotlin-project/Main.kt")
    ws.attachSnippet("myapp-test/src/kotlin/com/example/MainTest.kt", "your-first-kotlin-project/MainTest.kt")

    val (_, commands, storingLogger) = ws.start()
    commands.run(model.CrossProjectName(model.ProjectName("myapp"), None))
    assert(storingLogger.underlying.exists(_.message.plainText == "Hello, World!"))

    runTests(commands)
    // Not `succeed`. The scaffold shipped a release whose tests could not start at all — every suite died in JUnit Platform discovery with
    // `NoSuchMethodError: ReflectionUtils.returnsVoid` — and this test stayed green throughout, because it only ever checked that `commands.test` did not throw.
    assertSuitePassed(storingLogger, "com.example.MainTest", tests = 1)
  }

  /** The version the scaffold pins. `bleep build new --lang kotlin` is the first thing a new user runs, so the exact version it writes gets its own test. */
  integrationTest(s"kotest ${model.Versions.Kotest} (the scaffold's version) runs its tests") { ws =>
    ws.yaml(kotestYaml(model.Versions.Kotest))
    ws.file("myapp/src/kotlin/com/example/Main.kt", mainSrc)
    ws.file("myapp-test/src/kotlin/com/example/MainTest.kt", testSrc)

    val (_, commands, storingLogger) = ws.start()
    runTests(commands)
    assertSuitePassed(storingLogger, "com.example.MainTest", tests = 1)
  }

  /** kotest 5 brings junit-platform 1.8.2, whose `junit-platform-commons` still has `ReflectionUtils.returnsVoid`. It happened to survive the misalignment that
    * killed kotest 6, which makes it the case a fix could quietly break: bleep injects engines matching *this* line, not a newer one.
    */
  integrationTest("kotest 5.9.1 (junit-platform 1.8.2) still runs its tests") { ws =>
    ws.yaml(kotestYaml("5.9.1"))
    ws.file("myapp/src/kotlin/com/example/Main.kt", mainSrc)
    ws.file("myapp-test/src/kotlin/com/example/MainTest.kt", testSrc)

    val (_, commands, storingLogger) = ws.start()
    runTests(commands)
    assertSuitePassed(storingLogger, "com.example.MainTest", tests = 1)
  }
}
