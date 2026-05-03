package bleep

import bleep.commands.BuildCreateNew

class YourFirstProjectIT extends IntegrationTestHarness {
  integrationTest("bleep new myapp --lang java") { ws =>
    ws.bleepNew(BuildCreateNew.Language.Java, "myapp")
    ws.attachSnippet("bleep.yaml", "your-first-project/bleep.yaml")
    ws.attachSnippet("myapp/src/java/com/example/Main.java", "your-first-project/Main.java")
    ws.attachSnippet("myapp-test/src/java/com/example/MainTest.java", "your-first-project/MainTest.java")

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
