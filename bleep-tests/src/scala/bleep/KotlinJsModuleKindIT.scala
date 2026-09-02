package bleep

import bleep.commands.LinkOptions

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** A Kotlin/JS link has to emit the module kind the build declared.
  *
  * This is the Kotlin half of #664. `model.KotlinJs.moduleKind` has been in the build model, and decodable from `bleep.yaml`, with no reader anywhere in the
  * server — the link path read the `--module-kind` flag and otherwise used a hardcoded CommonJS. A build declaring `moduleKind: es` got CommonJS and nothing
  * said otherwise.
  *
  * Asserted on the emitted JavaScript, for the same reason [[ScalaJsTestModuleKindIT]] is: the constant was consistent, and everything downstream of it agreed.
  */
class KotlinJsModuleKindIT extends IntegrationTestHarness {

  private val myapp = model.CrossProjectName(model.ProjectName("myapp"), None)

  private def yamlFor(moduleKind: Option[String]): String = {
    val jsBlock = moduleKind.map(kind => s"      js:\n        moduleKind: $kind\n").getOrElse("")
    s"""projects:
       |  myapp:
       |    platform:
       |      name: js
       |      jsNodeVersion: ${model.Versions.Node}
       |      mainClass: example.MainKt
       |    kotlin:
       |      version: ${model.Versions.Kotlin24}
       |$jsBlock""".stripMargin
  }

  private val Source =
    """package example
      |
      |fun greet(name: String): String = "Hello, " + name + "!"
      |
      |fun main() {
      |  println(greet("world"))
      |}
      |""".stripMargin

  private def linkedJs(ws: Workspace): Path = {
    val candidates = Files
      .walk(ws.root.resolve(".bleep"))
      .iterator()
      .asScala
      .filter(p => p.getFileName.toString.endsWith(".js") || p.getFileName.toString.endsWith(".mjs"))
      .filter(p => Files.size(p) > 0)
      .toList
    if (candidates.isEmpty) fail(s"no linked JavaScript under ${ws.root}")
    // The main module is the largest output; Kotlin/JS also emits small per-file chunks next to it.
    val picked = candidates.maxBy(Files.size)
    info(s"linked: ${candidates.map(p => s"${p.getFileName}=${Files.size(p)}B").mkString(", ")} -> picked ${picked.getFileName}")
    picked
  }

  private def link(ws: Workspace): Path = {
    val (_, commands, _) = ws.start()
    commands.link(List(myapp), LinkOptions.Debug)
    linkedJs(ws)
  }

  integrationTest("a Kotlin/JS link emits the ES module the build declared") { ws =>
    ws.yaml(yamlFor(Some("es")))
    ws.file("myapp/src/kotlin/example/Main.kt", Source)

    val linked = link(ws)

    // Asserted on the extension because that is Kotlin's own signal: it names ES output `<module>.mjs` and every other module kind `<module>.js`. Grepping the
    // body for `export` is the weaker check — half a megabyte of generated JavaScript says many things — and the extension is what node reads to decide how to
    // load the file.
    assert(
      linked.getFileName.toString.endsWith(".mjs"),
      s"the link produced ${linked.getFileName}, not an .mjs module, so `kotlin.js.moduleKind: es` never reached it"
    )
    assert(
      !Files.readString(linked).contains("module.exports"),
      s"${linked.getFileName} still assigns to `module.exports`, so it is not really an ES module"
    )
    succeed
  }

  integrationTest("a Kotlin/JS link defaults to CommonJS when the build declares nothing") { ws =>
    ws.yaml(yamlFor(None))
    ws.file("myapp/src/kotlin/example/Main.kt", Source)

    // The other direction, so the first test cannot pass by the linker having simply changed its default: with no declaration the output must still be the
    // CommonJS `.js` it always was.
    val linked = link(ws)
    assert(linked.getFileName.toString.endsWith(".js"), s"the default link produced ${linked.getFileName}, expected a .js module")
    assert(
      Files.readString(linked).contains("module.exports"),
      s"${linked.getFileName} does not assign to `module.exports`, so the default is no longer CommonJS"
    )
    succeed
  }
}
