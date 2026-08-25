package bleep

import bleep.commands.{DisplayMode, LinkOptions, ReactiveBsp}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** `bleep link --release` on a Scala.js project has to produce a genuinely optimized program, not a debug one with dead code removed.
  *
  * Asserted against the bytes the linker wrote, never against the flags it was given. The defect this covers was invisible to any flag-level check:
  * `ScalaJsLinkConfig.Release` declared `minify = true` and the bridge simply never called `withMinify`, so every setting read as correct while the output kept
  * fastLinkJS's long mangled names and a size that overran a 10MB deployment limit. A test that confirmed the configuration would have passed throughout.
  *
  * The release overlay mirrors mill's `ScalaJSConfigModule.fullOptConfig`: optimized semantics, the Closure compiler where the module kind allows it, and
  * minification from Scala.js 1.16 on.
  */
class ScalaJsReleaseLinkIT extends IntegrationTestHarness {

  private val myapp = model.CrossProjectName(model.ProjectName("myapp"), None)

  /** `jsKind: none` produces a NoModule program, which is what lets the Closure compiler run — Scala.js refuses to pair it with ESModule output. */
  private val Yaml =
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

  /** Deliberately free of exports and reflection: every name here is internal, so all of them are the minifier's to rename. */
  private val Source =
    """package example
      |
      |object Greeter {
      |  def greet(name: String): String = s"Hello, $name!"
      |  def shout(name: String): String = greet(name).toUpperCase
      |  def main(args: Array[String]): Unit = println(shout("world"))
      |}
      |""".stripMargin

  private def link(started: Started, release: Boolean): Either[BleepException, Unit] =
    ReactiveBsp
      .link(
        watch = false,
        projects = Array(myapp),
        displayMode = DisplayMode.NoTui,
        options = LinkOptions(
          releaseMode = release,
          sourceMaps = None,
          minify = None,
          moduleKind = None,
          lto = None,
          optimize = None,
          debugInfo = None
        ),
        flamegraph = false,
        cancel = false
      )
      .run(started)

  /** The linked program, wherever under `.bleep` the link put it. Located by walking rather than by rebuilding the path convention, so a change to the layout
    * shows up as a different file rather than as this test quietly finding nothing.
    */
  private def linkedJs(ws: Workspace, dirSegment: String): Path = {
    val root = ws.root.resolve(".bleep")
    val candidates =
      Files
        .walk(root)
        .iterator()
        .asScala
        .filter(p => p.getFileName.toString == "main.js")
        // Matched on the directory segment, not on the whole path: the workspace is a temp directory named after the test, so a substring check for "release"
        // matched every path under a test whose name mentions it — including the debug output.
        .filter { p =>
          val mode = Option(p.getParent).flatMap(js => Option(js.getParent)).map(_.getFileName.toString)
          mode.contains(dirSegment)
        }
        .toList
    candidates match {
      case one :: Nil => one
      case Nil        => fail(s"no linked main.js under a '$dirSegment' directory in ${ws.root}")
      case many       => fail(s"expected one linked main.js under '$dirSegment', found ${many.size}: ${many.mkString(", ")}")
    }
  }

  integrationTest("release link minifies: names are shortened and the program shrinks") { ws =>
    ws.yaml(Yaml)
    ws.file("myapp/src/scala/example/Greeter.scala", Source)
    val (started, _, _) = ws.start()

    link(started, release = false).orThrow
    val debugJs = Files.readString(linkedJs(ws, "debug"))

    link(started, release = true).orThrow
    val releaseJs = Files.readString(linkedJs(ws, "release"))

    // The mangled form of `example.Greeter`. A fast link names every internal symbol after its fully qualified Scala name; minification is precisely what
    // replaces those with short ones, so its presence or absence is a direct read on whether minification ran.
    val mangled = "example_Greeter"
    val inDebug = mangled.r.findAllIn(debugJs).size
    val inRelease = mangled.r.findAllIn(releaseJs).size

    info(f"debug ${debugJs.length}%,dB with $inDebug occurrences of '$mangled'; release ${releaseJs.length}%,dB with $inRelease")

    assert(
      inDebug > 0,
      s"the debug link should carry the unminified name '$mangled' — if it does not, this test can no longer tell minified output from unminified"
    )
    assert(
      inRelease == 0,
      s"release output still contains '$mangled' $inRelease time(s): the linker was not minifying. debug=${debugJs.length}B release=${releaseJs.length}B"
    )
    assert(
      releaseJs.length < debugJs.length,
      s"release output (${releaseJs.length}B) is not smaller than debug (${debugJs.length}B)"
    )
    succeed
  }
}
