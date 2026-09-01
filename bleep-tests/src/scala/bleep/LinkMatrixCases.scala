package bleep

import bleep.commands.LinkOptions

/** The concrete link matrix: one suite per target, one case per (mode, module kind) that target supports. */
object LinkMatrixCases {

  private def scalaJsProject(jsKind: String): String =
    s"""  myapp:
       |    platform:
       |      name: js
       |      jsVersion: ${model.Versions.ScalaJs1}
       |      jsNodeVersion: ${model.Versions.Node}
       |      jsKind: $jsKind
       |      mainClass: example.Main
       |    scala:
       |      version: ${model.Versions.Scala3}
       |""".stripMargin

  private def kotlinJsProject(moduleKind: Option[String]): String =
    kotlinJsProjectWith(moduleKind.map(kind => s"        moduleKind: $kind").toList)

  /** A Kotlin/JS project carrying arbitrary `kotlin.js` settings, one per line. */
  private def kotlinJsProjectWith(jsSettings: List[String]): String = {
    val js = if (jsSettings.isEmpty) "" else s"      js:\n${jsSettings.mkString("\n")}\n"
    s"""  myapp:
       |    platform:
       |      name: js
       |      jsNodeVersion: ${model.Versions.Node}
       |      mainClass: example.MainKt
       |    kotlin:
       |      version: ${model.Versions.Kotlin24}
       |$js""".stripMargin
  }

  private val scalaNativeProject: String =
    s"""  myapp:
       |    platform:
       |      name: native
       |      nativeVersion: ${model.Versions.ScalaNative05}
       |      nativeGc: immix
       |      mainClass: example.Main
       |    scala:
       |      version: ${model.Versions.Scala3}
       |""".stripMargin

  // `mainClass` is not decoration here: bleep derives Kotlin/Native's entry point from it (`example.MainKt` -> `example.main`), and without one the linker
  // looks for `/main` in the root package and fails with "could not find '/main' function".
  private val kotlinNativeProject: String =
    s"""  myapp:
       |    platform:
       |      name: native
       |      mainClass: example.MainKt
       |    kotlin:
       |      version: ${model.Versions.Kotlin24}
       |""".stripMargin

  private val isJs: String => Boolean = _.endsWith(".js")
  private val isMjs: String => Boolean = _.endsWith(".mjs")
  private val isBinary: String => Boolean = name => !name.endsWith(".js") && !name.endsWith(".mjs")

  /** Scala.js. `jsKind` decides how the program leaves the module, and `--release` decides whether Closure and the minifier ran. */
  val scalaJs: List[LinkCase] = List(
    LinkCase(
      name = "debug / jsKind none",
      projectYaml = scalaJsProject("none"),
      options = LinkOptions.Debug,
      expectedFileName = isJs,
      fileNameDescription = "main.js",
      // A NoModule program is wrapped in `(function(){ ... }).call(this)`; the other kinds are a bare sequence of top-level statements.
      mustContain = List(").call(this)"),
      mustNotContain = Nil,
      siblings = Nil,
      siblingMustContain = Nil,
      runnable = true
    ),
    LinkCase(
      name = "debug / jsKind commonjs",
      projectYaml = scalaJsProject("commonjs"),
      options = LinkOptions.Debug,
      expectedFileName = isJs,
      fileNameDescription = "main.js",
      mustContain = Nil,
      mustNotContain = List(").call(this)"),
      siblings = Nil,
      siblingMustContain = Nil,
      runnable = true
    ),
    LinkCase(
      name = "debug / jsKind esmodule",
      projectYaml = scalaJsProject("esmodule"),
      options = LinkOptions.Debug,
      expectedFileName = isJs,
      fileNameDescription = "main.js",
      mustContain = Nil,
      mustNotContain = List(").call(this)"),
      siblings = Nil,
      siblingMustContain = Nil,
      runnable = true
    ),
    LinkCase(
      name = "release / jsKind commonjs",
      projectYaml = scalaJsProject("commonjs"),
      options = LinkOptions.Release,
      expectedFileName = isJs,
      fileNameDescription = "main.js",
      mustContain = Nil,
      // A release link minifies: the fully qualified Scala name survives a fast link and not an optimized one.
      mustNotContain = List("example_Main"),
      siblings = Nil,
      siblingMustContain = Nil,
      runnable = true
    )
  )

  val scalaJsSource: (String, String) = (
    "myapp/src/scala/example/Main.scala",
    """package example
      |
      |object Main {
      |  def main(args: Array[String]): Unit = println("linked-and-ran")
      |}
      |""".stripMargin
  )

  /** Kotlin/JS. Kotlin names ES output `.mjs` and everything else `.js`, which is the toolchain stating the module kind in the file name. */
  val kotlinJs: List[LinkCase] = List(
    LinkCase(
      name = "debug / no declaration defaults to commonjs",
      projectYaml = kotlinJsProject(None),
      options = LinkOptions.Debug,
      expectedFileName = isJs,
      fileNameDescription = "a .js module",
      mustContain = List("module.exports"),
      mustNotContain = Nil,
      siblings = Nil,
      siblingMustContain = Nil,
      runnable = true
    ),
    LinkCase(
      name = "debug / moduleKind umd",
      projectYaml = kotlinJsProject(Some("umd")),
      options = LinkOptions.Debug,
      expectedFileName = isJs,
      fileNameDescription = "a .js module",
      // UMD probes for both loaders before falling back to a global; that probe is the shape that distinguishes it from plain CommonJS.
      mustContain = List("define.amd"),
      mustNotContain = Nil,
      siblings = Nil,
      siblingMustContain = Nil,
      runnable = true
    ),
    LinkCase(
      name = "debug / moduleKind plain",
      projectYaml = kotlinJsProject(Some("plain")),
      options = LinkOptions.Debug,
      expectedFileName = isJs,
      fileNameDescription = "a .js module",
      mustContain = Nil,
      mustNotContain = List("module.exports"),
      siblings = Nil,
      siblingMustContain = Nil,
      runnable = true
    ),
    LinkCase(
      name = "debug / moduleKind es",
      projectYaml = kotlinJsProject(Some("es")),
      options = LinkOptions.Debug,
      expectedFileName = isMjs,
      fileNameDescription = "an .mjs module",
      mustContain = Nil,
      mustNotContain = List("module.exports"),
      siblings = Nil,
      siblingMustContain = Nil,
      runnable = true
    ),
    LinkCase(
      name = "debug / moduleKind amd",
      projectYaml = kotlinJsProject(Some("amd")),
      options = LinkOptions.Debug,
      expectedFileName = isJs,
      fileNameDescription = "a .js module",
      mustContain = List("define("),
      mustNotContain = Nil,
      siblings = Nil,
      siblingMustContain = Nil,
      // An AMD module defines itself into a loader that node does not have.
      runnable = false
    ),
    // The rest of the `kotlin.js` block. Each of these was hardcoded until now — bleep set a value on the compiler arguments either way, so the question was
    // never whether to support them but which constant to pick. One case per field, each asserting on something the compiler produced rather than on the
    // configuration it was handed.
    LinkCase(
      name = "debug / moduleName names the artifact",
      projectYaml = kotlinJsProjectWith(List("        moduleName: custom_name")),
      options = LinkOptions.Debug,
      expectedFileName = _ == "custom_name.js",
      fileNameDescription = "custom_name.js",
      mustContain = Nil,
      mustNotContain = Nil,
      siblings = Nil,
      siblingMustContain = Nil,
      runnable = true
    ),
    LinkCase(
      name = "debug / sourceMap emits a map",
      projectYaml = kotlinJsProjectWith(List("        sourceMap: true")),
      options = LinkOptions.Debug,
      expectedFileName = isJs,
      fileNameDescription = "a .js module",
      mustContain = Nil,
      mustNotContain = Nil,
      siblings = List(".js.map"),
      siblingMustContain = Nil,
      runnable = true
    ),
    LinkCase(
      name = "debug / sourceMapEmbedSources always embeds the sources",
      projectYaml = kotlinJsProjectWith(List("        sourceMap: true", "        sourceMapEmbedSources: always")),
      options = LinkOptions.Debug,
      expectedFileName = isJs,
      fileNameDescription = "a .js module",
      mustContain = Nil,
      mustNotContain = Nil,
      siblings = List(".js.map"),
      // `sourcesContent` is the map field that carries the sources themselves, and it is absent under the `never` every link used to get.
      siblingMustContain = List(".js.map" -> "sourcesContent"),
      runnable = true
    ),
    LinkCase(
      name = "debug / sourceMapPrefix prefixes the map's sources",
      projectYaml = kotlinJsProjectWith(List("        sourceMap: true", "        sourceMapPrefix: bleep-prefix/")),
      options = LinkOptions.Debug,
      expectedFileName = isJs,
      fileNameDescription = "a .js module",
      mustContain = Nil,
      mustNotContain = Nil,
      siblings = List(".js.map"),
      siblingMustContain = List(".js.map" -> "bleep-prefix/"),
      runnable = true
    ),
    LinkCase(
      name = "debug / generateDts emits TypeScript declarations",
      projectYaml = kotlinJsProjectWith(List("        generateDts: true")),
      options = LinkOptions.Debug,
      expectedFileName = isJs,
      fileNameDescription = "a .js module",
      mustContain = Nil,
      mustNotContain = Nil,
      siblings = List(".d.ts"),
      siblingMustContain = Nil,
      runnable = true
    )
  )

  val kotlinJsSource: (String, String) = (
    "myapp/src/kotlin/example/Main.kt",
    """package example
      |
      |fun main() {
      |  println("linked-and-ran")
      |}
      |""".stripMargin
  )

  val scalaNative: List[LinkCase] = List(
    LinkCase(
      name = "debug",
      projectYaml = scalaNativeProject,
      options = LinkOptions.Debug,
      expectedFileName = isBinary,
      fileNameDescription = "a native binary",
      mustContain = Nil,
      mustNotContain = Nil,
      siblings = Nil,
      siblingMustContain = Nil,
      runnable = true
    ),
    LinkCase(
      name = "release",
      projectYaml = scalaNativeProject,
      options = LinkOptions.Release,
      expectedFileName = isBinary,
      fileNameDescription = "a native binary",
      mustContain = Nil,
      mustNotContain = Nil,
      siblings = Nil,
      siblingMustContain = Nil,
      runnable = true
    )
  )

  val scalaNativeSource: (String, String) = (
    "myapp/src/scala/example/Main.scala",
    """package example
      |
      |object Main {
      |  def main(args: Array[String]): Unit = println("linked-and-ran")
      |}
      |""".stripMargin
  )

  val kotlinNative: List[LinkCase] = List(
    LinkCase(
      name = "debug",
      projectYaml = kotlinNativeProject,
      options = LinkOptions.Debug,
      expectedFileName = isBinary,
      fileNameDescription = "a native binary",
      mustContain = Nil,
      mustNotContain = Nil,
      siblings = Nil,
      siblingMustContain = Nil,
      runnable = true
    ),
    LinkCase(
      name = "release",
      projectYaml = kotlinNativeProject,
      options = LinkOptions.Release,
      expectedFileName = isBinary,
      fileNameDescription = "a native binary",
      mustContain = Nil,
      mustNotContain = Nil,
      siblings = Nil,
      siblingMustContain = Nil,
      runnable = true
    )
  )

  val kotlinNativeSource: (String, String) = kotlinJsSource
}

class ScalaJsLinkMatrixIT extends LinkMatrixIT("scala.js", LinkMatrixCases.scalaJs, LinkMatrixCases.scalaJsSource._1, LinkMatrixCases.scalaJsSource._2)

class KotlinJsLinkMatrixIT extends LinkMatrixIT("kotlin/js", LinkMatrixCases.kotlinJs, LinkMatrixCases.kotlinJsSource._1, LinkMatrixCases.kotlinJsSource._2)

class ScalaNativeLinkMatrixIT
    extends LinkMatrixIT("scala native", LinkMatrixCases.scalaNative, LinkMatrixCases.scalaNativeSource._1, LinkMatrixCases.scalaNativeSource._2)

class KotlinNativeLinkMatrixIT
    extends LinkMatrixIT("kotlin/native", LinkMatrixCases.kotlinNative, LinkMatrixCases.kotlinNativeSource._1, LinkMatrixCases.kotlinNativeSource._2)
