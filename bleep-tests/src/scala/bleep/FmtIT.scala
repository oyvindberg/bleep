package bleep

import java.nio.file.Files

class FmtIT extends IntegrationTestHarness {

  private val unformattedScala =
    "package test\n\n  object Hello   {\n    def greet(name:String):String=\"Hello, \"+name\n  }\n"

  private val unformattedJava =
    "package test;\n\npublic class Hello {\npublic static String greet(String name){return \"Hello, \"+name;}}\n"

  private val unformattedKotlin =
    "package test\n\nfun greet(  name :  String ) :  String  = \"Hello, \"  +  name\n"

  integrationTest("scala formatting via scalafmt reformats unformatted source") { ws =>
    ws.yaml(
      """projects:
        |  myapp:
        |    platform:
        |      name: jvm
        |    scala:
        |      version: 2.13.14
        |""".stripMargin
    )
    // Pin scalafmt config explicitly so the test does not depend on Fmt.ScalaFmt.defaultConfig.
    ws.file(
      ".scalafmt.conf",
      """version = 3.11.1
        |maxColumn = 120
        |runner.dialect = scala213
        |""".stripMargin
    )
    ws.file("myapp/src/scala/test/Hello.scala", unformattedScala)

    val (started, _, _) = ws.start()
    commands.Fmt(check = false, projects = Array.empty).run(started).orThrow

    val after = Files.readString(ws.root.resolve("myapp/src/scala/test/Hello.scala"))
    assert(after !== unformattedScala, "scalafmt should have reformatted the source")
    assert(after.contains("object Hello {"), s"unexpected reformat output:\n$after")
  }

  integrationTest("java formatting via google-java-format reformats unformatted source") { ws =>
    ws.yaml(
      """projects:
        |  myapp:
        |    platform:
        |      name: jvm
        |    java: {}
        |""".stripMargin
    )
    ws.file("myapp/src/java/test/Hello.java", unformattedJava)

    val (started, _, _) = ws.start()
    commands.Fmt(check = false, projects = Array.empty).run(started).orThrow

    val after = Files.readString(ws.root.resolve("myapp/src/java/test/Hello.java"))
    assert(after !== unformattedJava, "google-java-format should have reformatted the source")
    assert(after.contains("public static String greet(String name)"), s"unexpected reformat output:\n$after")
  }

  integrationTest("kotlin formatting via ktfmt reformats unformatted source") { ws =>
    ws.yaml(
      """projects:
        |  myapp:
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |    platform:
        |      name: jvm
        |""".stripMargin
    )
    ws.file("myapp/src/kotlin/test/Hello.kt", unformattedKotlin)

    val (started, _, _) = ws.start()
    commands.Fmt(check = false, projects = Array.empty).run(started).orThrow

    val after = Files.readString(ws.root.resolve("myapp/src/kotlin/test/Hello.kt"))
    assert(after !== unformattedKotlin, "ktfmt should have reformatted the source")
    assert(after.contains("fun greet(name: String): String"), s"unexpected reformat output:\n$after")
  }

  integrationTest("only formatter for present language is invoked") { ws =>
    // Kotlin-only project: scalafmt should never run, so .scalafmt.conf must not be auto-created.
    ws.yaml(
      """projects:
        |  myapp:
        |    source-layout: kotlin
        |    kotlin:
        |      version: 2.1.20
        |    platform:
        |      name: jvm
        |""".stripMargin
    )
    ws.file("myapp/src/kotlin/test/Hello.kt", unformattedKotlin)

    val (started, _, _) = ws.start()
    commands.Fmt(check = false, projects = Array.empty).run(started).orThrow

    val scalafmtConf = ws.root.resolve(".scalafmt.conf")
    assert(!Files.exists(scalafmtConf), s".scalafmt.conf must not be created for a kotlin-only project — found at $scalafmtConf")

    val ktAfter = Files.readString(ws.root.resolve("myapp/src/kotlin/test/Hello.kt"))
    assert(ktAfter !== unformattedKotlin, "ktfmt should still have reformatted the kotlin file")
  }
}
