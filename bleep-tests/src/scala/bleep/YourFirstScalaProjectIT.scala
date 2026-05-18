package bleep

import bleep.commands.BuildCreateNew

class YourFirstScalaProjectIT extends IntegrationTestHarness {
  // Split into two `integrationTest` blocks: each gets its own workspace + setup, but more importantly each gets its own suite-idle-timer reset window. The
  // combined run+test version was one slow test method and routinely tripped the 2-minute idle timeout under parallel-suite CPU contention even though the
  // underlying work itself runs in ~5s standalone.
  integrationTest("bleep new myapp --lang scala (run the app)") { ws =>
    ws.bleepNew(BuildCreateNew.Language.Scala, "myapp")
    ws.attachSnippet("bleep.yaml", "your-first-scala-project/bleep.yaml")
    ws.attachSnippet("myapp/src/scala/com/example/Main.scala", "your-first-scala-project/Main.scala")
    ws.attachSnippet("myapp-test/src/scala/com/example/MainTest.scala", "your-first-scala-project/MainTest.scala")

    val (_, commands, storingLogger) = ws.start()
    commands.run(model.CrossProjectName(model.ProjectName("myapp"), None))
    assert(storingLogger.underlying.exists(_.message.plainText == "Hello, World!"))
  }

  integrationTest("bleep new myapp --lang scala (run the tests)") { ws =>
    ws.bleepNew(BuildCreateNew.Language.Scala, "myapp")
    ws.attachSnippet("bleep.yaml", "your-first-scala-project/bleep.yaml")
    ws.attachSnippet("myapp/src/scala/com/example/Main.scala", "your-first-scala-project/Main.scala")
    ws.attachSnippet("myapp-test/src/scala/com/example/MainTest.scala", "your-first-scala-project/MainTest.scala")

    val (_, commands, _) = ws.start()
    commands.test(
      projects = List(model.CrossProjectName(model.ProjectName("myapp-test"), None)),
      watch = false,
      only = None,
      exclude = None,
      includeTags = None,
      excludeTags = None
    )
    succeed
  }
}
