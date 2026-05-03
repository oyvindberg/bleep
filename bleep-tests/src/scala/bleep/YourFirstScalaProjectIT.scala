package bleep

import bleep.commands.BuildCreateNew

class YourFirstScalaProjectIT extends IntegrationTestHarness {
  integrationTest("bleep new myapp --lang scala") { ws =>
    ws.bleepNew(BuildCreateNew.Language.Scala, "myapp")
    ws.attachSnippet("bleep.yaml", "your-first-scala-project/bleep.yaml")
    ws.attachSnippet("myapp/src/scala/com/example/Main.scala", "your-first-scala-project/Main.scala")
    ws.attachSnippet("myapp-test/src/scala/com/example/MainTest.scala", "your-first-scala-project/MainTest.scala")

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
