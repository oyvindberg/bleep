package bleep.model

import bleep.internal.EnumCodec
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Kotlin/JS configuration for a project.
  *
  * @param moduleKind
  *   The JS module kind (CommonJS, ESModule, UMD, Plain, AMD)
  * @param moduleName
  *   Optional custom module name
  * @param outputMode
  *   Output mode (Klib for library, JsExecutable for runnable)
  * @param sourceMap
  *   Whether to generate source maps
  * @param sourceMapPrefix
  *   Optional prefix for source map paths
  * @param sourceMapEmbedSources
  *   Whether/how to embed sources in source maps
  * @param target
  *   Target environment (Browser, Node, Both)
  * @param developmentMode
  *   If false, enables production optimizations
  * @param generateDts
  *   Whether to generate TypeScript declaration files
  */
case class KotlinJs(
    moduleKind: Option[KotlinJsModuleKind],
    moduleName: Option[String],
    outputMode: Option[KotlinJsOutputMode],
    sourceMap: Option[Boolean],
    sourceMapPrefix: Option[String],
    sourceMapEmbedSources: Option[KotlinJsSourceMapEmbedSources],
    target: Option[KotlinJsTarget],
    developmentMode: Option[Boolean],
    generateDts: Option[Boolean]
) extends SetLike[KotlinJs] {

  override def intersect(other: KotlinJs): KotlinJs =
    KotlinJs(
      moduleKind = if (moduleKind == other.moduleKind) moduleKind else None,
      moduleName = if (moduleName == other.moduleName) moduleName else None,
      outputMode = if (outputMode == other.outputMode) outputMode else None,
      sourceMap = if (sourceMap == other.sourceMap) sourceMap else None,
      sourceMapPrefix = if (sourceMapPrefix == other.sourceMapPrefix) sourceMapPrefix else None,
      sourceMapEmbedSources = if (sourceMapEmbedSources == other.sourceMapEmbedSources) sourceMapEmbedSources else None,
      target = if (target == other.target) target else None,
      developmentMode = if (developmentMode == other.developmentMode) developmentMode else None,
      generateDts = if (generateDts == other.generateDts) generateDts else None
    )

  override def removeAll(other: KotlinJs): KotlinJs =
    KotlinJs(
      moduleKind = if (moduleKind == other.moduleKind) None else moduleKind,
      moduleName = if (moduleName == other.moduleName) None else moduleName,
      outputMode = if (outputMode == other.outputMode) None else outputMode,
      sourceMap = if (sourceMap == other.sourceMap) None else sourceMap,
      sourceMapPrefix = if (sourceMapPrefix == other.sourceMapPrefix) None else sourceMapPrefix,
      sourceMapEmbedSources = if (sourceMapEmbedSources == other.sourceMapEmbedSources) None else sourceMapEmbedSources,
      target = if (target == other.target) None else target,
      developmentMode = if (developmentMode == other.developmentMode) None else developmentMode,
      generateDts = if (generateDts == other.generateDts) None else generateDts
    )

  override def union(other: KotlinJs): KotlinJs =
    KotlinJs(
      moduleKind = moduleKind.orElse(other.moduleKind),
      moduleName = moduleName.orElse(other.moduleName),
      outputMode = outputMode.orElse(other.outputMode),
      sourceMap = sourceMap.orElse(other.sourceMap),
      sourceMapPrefix = sourceMapPrefix.orElse(other.sourceMapPrefix),
      sourceMapEmbedSources = sourceMapEmbedSources.orElse(other.sourceMapEmbedSources),
      target = target.orElse(other.target),
      developmentMode = developmentMode.orElse(other.developmentMode),
      generateDts = generateDts.orElse(other.generateDts)
    )

  override def isEmpty: Boolean =
    moduleKind.isEmpty && moduleName.isEmpty && outputMode.isEmpty &&
      sourceMap.isEmpty && sourceMapPrefix.isEmpty && sourceMapEmbedSources.isEmpty &&
      target.isEmpty && developmentMode.isEmpty && generateDts.isEmpty
}

object KotlinJs {
  val empty: KotlinJs = KotlinJs(
    moduleKind = None,
    moduleName = None,
    outputMode = None,
    sourceMap = None,
    sourceMapPrefix = None,
    sourceMapEmbedSources = None,
    target = None,
    developmentMode = None,
    generateDts = None
  )

  implicit val decodes: Decoder[KotlinJs] = deriveDecoder
  implicit val encodes: Encoder[KotlinJs] = deriveEncoder
}

/** Kotlin/JS module kind. */
sealed abstract class KotlinJsModuleKind(val value: String)

object KotlinJsModuleKind {
  case object Plain extends KotlinJsModuleKind("plain")
  case object AMD extends KotlinJsModuleKind("amd")
  case object CommonJS extends KotlinJsModuleKind("commonjs")
  case object UMD extends KotlinJsModuleKind("umd")
  case object ESModule extends KotlinJsModuleKind("es")

  val All: List[KotlinJsModuleKind] = List(Plain, AMD, CommonJS, UMD, ESModule)

  implicit val codec: io.circe.Codec[KotlinJsModuleKind] =
    EnumCodec.codec(All.map(x => (x.value, x)).toMap)
}

/** Kotlin/JS output mode. */
sealed abstract class KotlinJsOutputMode(val value: String)

object KotlinJsOutputMode {
  case object Klib extends KotlinJsOutputMode("klib")
  case object JsExecutable extends KotlinJsOutputMode("js")

  val All: List[KotlinJsOutputMode] = List(Klib, JsExecutable)

  implicit val codec: io.circe.Codec[KotlinJsOutputMode] =
    EnumCodec.codec(All.map(x => (x.value, x)).toMap)
}

/** Kotlin/JS source map embedding mode. */
sealed abstract class KotlinJsSourceMapEmbedSources(val value: String)

object KotlinJsSourceMapEmbedSources {
  case object Never extends KotlinJsSourceMapEmbedSources("never")
  case object Always extends KotlinJsSourceMapEmbedSources("always")
  case object Inlining extends KotlinJsSourceMapEmbedSources("inlining")

  val All: List[KotlinJsSourceMapEmbedSources] = List(Never, Always, Inlining)

  implicit val codec: io.circe.Codec[KotlinJsSourceMapEmbedSources] =
    EnumCodec.codec(All.map(x => (x.value, x)).toMap)
}

/** Kotlin/JS target environment. */
sealed abstract class KotlinJsTarget(val value: String)

object KotlinJsTarget {
  case object Browser extends KotlinJsTarget("browser")
  case object Node extends KotlinJsTarget("nodejs")
  case object Both extends KotlinJsTarget("both")

  val All: List[KotlinJsTarget] = List(Browser, Node, Both)

  implicit val codec: io.circe.Codec[KotlinJsTarget] =
    EnumCodec.codec(All.map(x => (x.value, x)).toMap)
}
