package bleep

import bleep.commands.BuildCreateNew

class YourFirstKotlinProjectIT extends IntegrationTestHarness {
  integrationTest("bleep new myapp --lang kotlin") { ws =>
    ws.bleepNew(BuildCreateNew.Language.Kotlin, "myapp")
    ws.attachSnippet("bleep.yaml", "your-first-kotlin-project/bleep.yaml")
    ws.attachSnippet("myapp/src/kotlin/com/example/Main.kt", "your-first-kotlin-project/Main.kt")
    ws.attachSnippet("myapp-test/src/kotlin/com/example/MainTest.kt", "your-first-kotlin-project/MainTest.kt")

    val (_, commands, storingLogger) = ws.start()
    commands.run(model.CrossProjectName(model.ProjectName("myapp"), None))
    assert(storingLogger.underlying.exists(_.message.plainText == "Hello, World!"))

    commands.test(
      projects = List(model.CrossProjectName(model.ProjectName("myapp-test"), None)),
      watch = false,
      only = None,
      exclude = None
    )
    succeed
  }
}
