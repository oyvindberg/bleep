package bleep

import bleep.commands.LinkOptions

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** The surface a build script reaches through [[bleep.Commands]].
  *
  * Two gaps, both reported against a real build: there was no `link` at all, so a script that packages linked JavaScript into a jar had to shell out to the
  * `bleep` command line and pay the start-up cost a second time; and `compile` returned `Unit`, so a script could not tell a compile that rebuilt everything
  * from one that found nothing to do — a distinction bleep already knows and had nowhere to put.
  */
class ScriptCommandsIT extends IntegrationTestHarness {

  private val a = model.CrossProjectName(model.ProjectName("a"), None)
  private val myapp = model.CrossProjectName(model.ProjectName("myapp"), None)

  integrationTest("compile reports whether anything actually recompiled") { ws =>
    ws.yaml(
      """projects:
        |  a:
        |    platform:
        |      name: jvm
        |    scala:
        |      version: 3.4.2
        |""".stripMargin
    )
    ws.file("a/src/scala/A.scala", "package test\nobject A { def one = 1 }\n")
    val (_, commands, _) = ws.start()

    val first = commands.compile(List(a))
    assert(!first.noOp, s"the first compile of a fresh project cannot be a no-op: upToDate=${first.upToDateProjects}")
    assert(first.compilesCompleted > 0, "the first compile reported no completed compiles at all")

    val second = commands.compile(List(a))
    assert(second.noOp, s"a repeat compile with nothing changed should be a no-op: upToDate=${second.upToDateProjects}")
    assert(second.upToDateProjects.contains(a), s"the up-to-date project was not named: ${second.upToDateProjects}")

    // And back again, so the flag is shown to track the source rather than just the call count.
    ws.file("a/src/scala/A.scala", "package test\nobject A { def one = 1; def two = 2 }\n")
    val third = commands.compile(List(a))
    assert(!third.noOp, s"a compile after an edit should not be a no-op: upToDate=${third.upToDateProjects}")
    succeed
  }

  integrationTest("link is reachable from a script") { ws =>
    ws.yaml(
      s"""projects:
         |  myapp:
         |    platform:
         |      name: js
         |      jsVersion: ${model.Versions.ScalaJs1}
         |      jsNodeVersion: ${model.Versions.Node}
         |      jsKind: none
         |      mainClass: example.Greeter
         |    scala:
         |      version: ${model.Versions.Scala3}
         |""".stripMargin
    )
    ws.file(
      "myapp/src/scala/example/Greeter.scala",
      """package example
        |
        |object Greeter {
        |  def main(args: Array[String]): Unit = println("hello")
        |}
        |""".stripMargin
    )
    val (_, commands, _) = ws.start()

    val summary = commands.link(List(myapp), LinkOptions.Debug)

    // Asserted on the linked file, not on the call returning: `link` throws on failure, so "it did not throw" is also what a `link` that linked nothing would
    // look like.
    val onDisk = Files
      .walk(ws.root.resolve(".bleep"))
      .iterator()
      .asScala
      .filter(p => p.getFileName.toString == "main.js")
      .toList
    assert(onDisk.nonEmpty, s"commands.link produced no main.js under ${ws.root}")
    assert(Files.size(onDisk.head) > 0, s"commands.link produced an empty ${onDisk.head}")

    // And the caller is told where it went. The whole reason for reporting this is that the alternative is rebuilding `link-output/<mode>/js/main.js` from a
    // layout bleep owns and has already renamed once, so the assertion is that the reported path is the file that is really there — not merely that some path
    // came back.
    val reported = summary.linkedOutputs
    assert(reported.map(_.project) == List(myapp), s"expected one linked output for myapp, got ${reported.map(_.project.value)}")
    assert(
      reported.head.mainArtifact.toAbsolutePath == onDisk.head.toAbsolutePath,
      s"the reported main artifact ${reported.head.mainArtifact} is not the file the link wrote, ${onDisk.head}"
    )
    assert(Files.exists(reported.head.mainArtifact), s"the reported main artifact does not exist: ${reported.head.mainArtifact}")
    succeed
  }
}
