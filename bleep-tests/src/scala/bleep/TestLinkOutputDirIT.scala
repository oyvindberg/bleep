package bleep

import bleep.commands.LinkOptions

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** `bleep link` and `bleep test` link a project into the same place.
  *
  * They did not. The compile/link path handed `LinkExecutor` a base of `targetDir/link-output`, the test path handed it `targetDir`, and the executor appended
  * the mode suffix to whichever it got — so one project linked twice landed in two trees, each with an up-to-date check that could not see the other's output.
  * `bleep run` reads the first of those, which is why the split was invisible: nothing ever looked in both.
  *
  * This is #673's shape surviving in the one spot #673's fix did not reach. That fix stopped every *suite* from linking its own copy; this stops the two
  * *commands* from doing the same thing one level up.
  */
class TestLinkOutputDirIT extends IntegrationTestHarness {

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  private val Yaml =
    s"""projects:
       |  mytest:
       |    dependencies:
       |      - org.scalameta::munit:${model.Versions.Munit}
       |    isTestProject: true
       |    platform:
       |      name: js
       |      jsVersion: ${model.Versions.ScalaJs1}
       |      jsNodeVersion: ${model.Versions.Node}
       |      jsKind: commonjs
       |    scala:
       |      version: ${model.Versions.Scala3}
       |""".stripMargin

  private val Source =
    """package example
      |
      |class OutputDirSuite extends munit.FunSuite {
      |  test("runs") { assertEquals(1 + 1, 2) }
      |}
      |""".stripMargin

  private def linkedModules(ws: Workspace): List[Path] =
    Files
      .walk(ws.root.resolve(".bleep"))
      .iterator()
      .asScala
      .filter(p => p.getFileName.toString == "main.js")
      .toList

  integrationTest("linking a test project and testing it produce one output, not two") { ws =>
    ws.yaml(Yaml)
    ws.file("mytest/src/scala/example/OutputDirSuite.scala", Source)
    val (_, commands, _) = ws.start()

    commands.link(List(mytest), LinkOptions.Debug)
    val afterLink = linkedModules(ws)
    assert(afterLink.size == 1, s"expected one linked module after link, found ${afterLink.size}: ${afterLink.mkString(", ")}")

    commands.test(List(mytest), watch = false, only = None, exclude = None, includeTags = None, excludeTags = None)
    val afterTest = linkedModules(ws)

    // The assertion that matters. With the two paths disagreeing this is 2 — one under `link-output/debug/js`, one under `debug/js` — and neither run can tell
    // that the other already did the work.
    assert(
      afterTest.size == 1,
      s"link and test wrote separate outputs:\n${afterTest.map(p => ws.root.relativize(p).toString).sorted.mkString("\n")}"
    )
    assert(
      afterTest.head.toString.contains("link-output"),
      s"the test link did not land under link-output, where `bleep link` and `bleep run` look: ${ws.root.relativize(afterTest.head)}"
    )
    succeed
  }
}
