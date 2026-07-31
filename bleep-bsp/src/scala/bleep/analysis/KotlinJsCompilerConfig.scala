package bleep.analysis

import java.nio.file.Path

/** Configuration for Kotlin/JS compilation.
  *
  * Contains all options needed to configure the K2JSCompiler.
  */
case class KotlinJsCompilerConfig(
    kotlinVersion: String,
    moduleKind: KotlinJsCompilerConfig.ModuleKind,
    moduleName: String,
    outputMode: KotlinJsCompilerConfig.OutputMode,
    sourceMap: Boolean,
    sourceMapPrefix: Option[String],
    sourceMapEmbedSources: KotlinJsCompilerConfig.SourceMapEmbedSources,
    target: KotlinJsCompilerConfig.Target,
    developmentMode: Boolean,
    generateDts: Boolean,
    additionalOptions: Seq[String]
)

object KotlinJsCompilerConfig {

  /** JS module kind. */
  sealed trait ModuleKind {
    def name: String
  }
  object ModuleKind {
    case object Plain extends ModuleKind { val name = "plain" }
    case object AMD extends ModuleKind { val name = "amd" }
    case object CommonJS extends ModuleKind { val name = "commonjs" }
    case object UMD extends ModuleKind { val name = "umd" }
    case object ESModule extends ModuleKind { val name = "es" }
  }

  /** Output mode. */
  sealed trait OutputMode {
    def name: String
  }
  object OutputMode {
    case object Klib extends OutputMode { val name = "klib" }
    case object JsExecutable extends OutputMode { val name = "js" }
  }

  /** Source map embedding. */
  sealed trait SourceMapEmbedSources {
    def name: String
  }
  object SourceMapEmbedSources {
    case object Never extends SourceMapEmbedSources { val name = "never" }
    case object Always extends SourceMapEmbedSources { val name = "always" }
    case object Inlining extends SourceMapEmbedSources { val name = "inlining" }
  }

  /** Target environment. */
  sealed trait Target {
    def name: String
  }
  object Target {
    case object Browser extends Target { val name = "browser" }
    case object Node extends Target { val name = "nodejs" }
  }

  /** A ready-made config for tests, not a default anything reads at build time. Production builds this explicitly in `ProjectCompiler` from the project's own
    * `kotlin.version`; nothing here is consulted. It was called `Default` and sat next to real defaults, which reads as "what a project gets when it says
    * nothing" — it is not, and that misreading is easy to act on.
    */
  val ForTests: KotlinJsCompilerConfig = KotlinJsCompilerConfig(
    kotlinVersion = bleep.model.Versions.Kotlin23,
    moduleKind = ModuleKind.CommonJS,
    moduleName = "main",
    outputMode = OutputMode.JsExecutable,
    sourceMap = true,
    sourceMapPrefix = None,
    sourceMapEmbedSources = SourceMapEmbedSources.Never,
    target = Target.Node,
    developmentMode = true,
    generateDts = false,
    additionalOptions = Seq.empty
  )
}

/** Result of Kotlin/JS compilation.
  */
case class KotlinJsCompileResult(
    outputDir: Path,
    outputFile: Option[Path],
    klibFile: Option[Path],
    exitCode: Int
) {
  def isSuccess: Boolean = exitCode == 0
}
