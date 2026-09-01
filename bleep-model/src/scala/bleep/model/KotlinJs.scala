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
  * @param sourceMap
  *   Whether to generate source maps
  * @param sourceMapPrefix
  *   Optional prefix for source map paths
  * @param sourceMapEmbedSources
  *   Whether/how to embed sources in source maps
  * @param generateDts
  *   Whether to generate TypeScript declaration files
  */
case class KotlinJs(
    moduleKind: Option[KotlinJsModuleKind],
    moduleName: Option[String],
    sourceMap: Option[Boolean],
    sourceMapPrefix: Option[String],
    sourceMapEmbedSources: Option[KotlinJsSourceMapEmbedSources],
    generateDts: Option[Boolean]
) extends SetLike[KotlinJs] {

  override def intersect(other: KotlinJs): KotlinJs =
    KotlinJs(
      moduleKind = if (moduleKind == other.moduleKind) moduleKind else None,
      moduleName = if (moduleName == other.moduleName) moduleName else None,
      sourceMap = if (sourceMap == other.sourceMap) sourceMap else None,
      sourceMapPrefix = if (sourceMapPrefix == other.sourceMapPrefix) sourceMapPrefix else None,
      sourceMapEmbedSources = if (sourceMapEmbedSources == other.sourceMapEmbedSources) sourceMapEmbedSources else None,
      generateDts = if (generateDts == other.generateDts) generateDts else None
    )

  override def removeAll(other: KotlinJs): KotlinJs =
    KotlinJs(
      moduleKind = if (moduleKind == other.moduleKind) None else moduleKind,
      moduleName = if (moduleName == other.moduleName) None else moduleName,
      sourceMap = if (sourceMap == other.sourceMap) None else sourceMap,
      sourceMapPrefix = if (sourceMapPrefix == other.sourceMapPrefix) None else sourceMapPrefix,
      sourceMapEmbedSources = if (sourceMapEmbedSources == other.sourceMapEmbedSources) None else sourceMapEmbedSources,
      generateDts = if (generateDts == other.generateDts) None else generateDts
    )

  override def union(other: KotlinJs): KotlinJs =
    KotlinJs(
      moduleKind = moduleKind.orElse(other.moduleKind),
      moduleName = moduleName.orElse(other.moduleName),
      sourceMap = sourceMap.orElse(other.sourceMap),
      sourceMapPrefix = sourceMapPrefix.orElse(other.sourceMapPrefix),
      sourceMapEmbedSources = sourceMapEmbedSources.orElse(other.sourceMapEmbedSources),
      generateDts = generateDts.orElse(other.generateDts)
    )

  override def isEmpty: Boolean =
    moduleKind.isEmpty && moduleName.isEmpty && sourceMap.isEmpty &&
      sourceMapPrefix.isEmpty && sourceMapEmbedSources.isEmpty && generateDts.isEmpty
}

object KotlinJs {
  val empty: KotlinJs = KotlinJs(
    moduleKind = None,
    moduleName = None,
    sourceMap = None,
    sourceMapPrefix = None,
    sourceMapEmbedSources = None,
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
