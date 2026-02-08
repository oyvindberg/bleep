package bleep
package commands

/** Options for controlling link behavior.
  *
  * These options control how Scala.js, Scala Native, Kotlin/JS, and Kotlin/Native projects are linked. Options that don't apply to a particular platform are
  * ignored.
  */
case class LinkOptions(
    /** Release mode - enables optimizations, disables debug info */
    releaseMode: Boolean,
    /** Generate source maps (JS platforms) */
    sourceMaps: Option[Boolean],
    /** Minify output (Scala.js only) */
    minify: Option[Boolean],
    /** Module kind for JS output */
    moduleKind: Option[LinkOptions.ModuleKind],
    /** Link-time optimization level (Scala Native only) */
    lto: Option[LinkOptions.LTO],
    /** Enable optimizations/DCE (all non-JVM platforms) */
    optimize: Option[Boolean],
    /** Include debug info (native platforms only) */
    debugInfo: Option[Boolean]
) {

  /** Convert to argument list for BSP protocol */
  def toArgs: List[String] = {
    val args = List.newBuilder[String]
    args += "--link"
    if (releaseMode) args += "--release"
    sourceMaps.foreach { v =>
      args += (if (v) "--source-maps" else "--no-source-maps")
    }
    minify.foreach { v =>
      args += (if (v) "--minify" else "--no-minify")
    }
    moduleKind.foreach { mk =>
      args += s"--module-kind=${mk.name}"
    }
    lto.foreach { l =>
      args += s"--lto=${l.name}"
    }
    optimize.foreach { v =>
      args += (if (v) "--optimize" else "--no-optimize")
    }
    debugInfo.foreach { v =>
      args += (if (v) "--debug-info" else "--no-debug-info")
    }
    args.result()
  }
}

object LinkOptions {

  /** Module kind for JavaScript output */
  sealed trait ModuleKind {
    def name: String
  }
  object ModuleKind {
    case object NoModule extends ModuleKind { val name = "nomodule" }
    case object CommonJS extends ModuleKind { val name = "commonjs" }
    case object ESModule extends ModuleKind { val name = "esmodule" }

    def fromString(s: String): Option[ModuleKind] = s.toLowerCase match {
      case "nomodule" => Some(NoModule)
      case "commonjs" => Some(CommonJS)
      case "esmodule" => Some(ESModule)
      case _          => None
    }

    val all: List[ModuleKind] = List(NoModule, CommonJS, ESModule)
  }

  /** Link-time optimization level (Scala Native) */
  sealed trait LTO {
    def name: String
  }
  object LTO {
    case object None extends LTO { val name = "none" }
    case object Thin extends LTO { val name = "thin" }
    case object Full extends LTO { val name = "full" }

    def fromString(s: String): Option[LTO] = s.toLowerCase match {
      case "none" => Some(None)
      case "thin" => Some(Thin)
      case "full" => Some(Full)
      case _      => scala.None
    }

    val all: List[LTO] = List(None, Thin, Full)
  }

  /** Default options (debug mode) */
  val Debug: LinkOptions = LinkOptions(
    releaseMode = false,
    sourceMaps = scala.None,
    minify = scala.None,
    moduleKind = scala.None,
    lto = scala.None,
    optimize = scala.None,
    debugInfo = scala.None
  )

  /** Release mode options */
  val Release: LinkOptions = LinkOptions(
    releaseMode = true,
    sourceMaps = scala.None,
    minify = scala.None,
    moduleKind = scala.None,
    lto = scala.None,
    optimize = scala.None,
    debugInfo = scala.None
  )
}
