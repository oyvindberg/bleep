package bleep

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** A Scala.js test link has to emit the module kind the build declared.
  *
  * The link `bleep test` runs and the link `bleep link` runs used to disagree about that. The main path read `--module-kind`, then the project's `jsKind`; the
  * test path declared `ScalaJsLinkConfig.Debug` and took its `CommonJSModule` along with the debug semantics it actually wanted. So a build saying `jsKind:
  * esmodule` had its tests linked as CommonJS, with no flag able to change it and nothing reporting the substitution.
  *
  * Asserted against the emitted JavaScript rather than the configuration, for the same reason [[ScalaJsReleaseLinkIT]] is: a test that checks which constant
  * was passed around would have gone on passing throughout, because the constant was consistent — consistently wrong.
  */
class ScalaJsTestModuleKindIT extends IntegrationTestHarness {

  private val mytest = model.CrossProjectName(model.ProjectName("mytest"), None)

  private def yamlFor(jsKind: String): String =
    s"""projects:
       |  mytest:
       |    dependencies:
       |      - org.scalameta::munit:${model.Versions.Munit}
       |    isTestProject: true
       |    platform:
       |      name: js
       |      jsVersion: ${model.Versions.ScalaJs1}
       |      jsNodeVersion: ${model.Versions.Node}
       |      jsKind: $jsKind
       |    scala:
       |      version: ${model.Versions.Scala3}
       |""".stripMargin

  /** A passing suite, plus a top-level export.
    *
    * The export is what makes the module kind visible at all. Scala.js emits a program with no exported members almost identically under CommonJS and under
    * ESModule — the difference is in how exports leave the module, so a fixture with none has nothing to tell them apart by, and the issue reporting this said
    * as much.
    */
  private val Source =
    """package example
      |
      |import scala.scalajs.js.annotation.JSExportTopLevel
      |
      |object Exports {
      |  @JSExportTopLevel("greet")
      |  def greet(name: String): String = s"Hello, $name!"
      |}
      |
      |class ModuleKindSuite extends munit.FunSuite {
      |  test("greets") { assertEquals(Exports.greet("world"), "Hello, world!") }
      |}
      |""".stripMargin

  /** The linked test program, wherever under `.bleep` the test run put it. Walked rather than reconstructed from the path convention, so a change to the layout
    * surfaces as a different file instead of as this test quietly finding nothing.
    */
  private def linkedTestJs(ws: Workspace): Path = {
    val root = ws.root.resolve(".bleep")
    val candidates =
      Files
        .walk(root)
        .iterator()
        .asScala
        .filter(p => p.getFileName.toString == "main.js")
        .toList
    candidates match {
      case one :: Nil => one
      case Nil        => fail(s"no linked main.js under ${ws.root}")
      case many       => fail(s"expected one linked main.js, found ${many.size}: ${many.mkString(", ")}")
    }
  }

  private def runTests(ws: Workspace): Unit = {
    val (_, commands, _) = ws.start()
    commands.test(List(mytest), watch = false, only = None, exclude = None, includeTags = None, excludeTags = None)
  }

  integrationTest("a test link emits the ES module the build declared") { ws =>
    ws.yaml(yamlFor("esmodule"))
    ws.file("mytest/src/scala/example/ModuleKindSuite.scala", Source)

    runTests(ws)

    val js = Files.readString(linkedTestJs(ws))
    // An ES module says goodbye to its exports with an `export` statement; CommonJS assigns them onto `exports`. Both markers are checked, because asserting
    // only the absence of the wrong one would also pass for a linked file that exported nothing at all.
    assert(
      js.contains("export {") || js.linesIterator.exists(_.trim.startsWith("export ")),
      s"the test link did not emit an ES module, so `jsKind: esmodule` never reached it. Tail:\n${js.takeRight(400)}"
    )
    assert(
      !js.contains("exports.greet"),
      s"the test link emitted CommonJS exports despite `jsKind: esmodule`. Tail:\n${js.takeRight(400)}"
    )
    succeed
  }

  integrationTest("a test link emits CommonJS when the build declares it") { ws =>
    ws.yaml(yamlFor("commonjs"))
    ws.file("mytest/src/scala/example/ModuleKindSuite.scala", Source)

    runTests(ws)

    val js = Files.readString(linkedTestJs(ws))
    // The other direction, and the reason this pair exists: `commonjs` was what the test link produced no matter what, so a test asserting only the ESModule
    // case would leave "does it still honour the declaration when the declaration is the old constant" unchecked.
    assert(
      js.contains("exports.greet"),
      s"the test link did not emit a CommonJS module despite `jsKind: commonjs`. Tail:\n${js.takeRight(400)}"
    )
    succeed
  }
}
