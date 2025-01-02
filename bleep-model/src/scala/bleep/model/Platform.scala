package bleep.model

import io.circe.*
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

case class Platform(
    name: Option[PlatformId],
    mainClass: Option[String],
    jsVersion: Option[VersionScalaJs],
    jsKind: Option[ModuleKindJS],
    jsSplitStyle: Option[ModuleSplitStyleJS],
    jsEmitSourceMaps: Option[Boolean],
    jsJsdom: Option[Boolean],
    jsNodeVersion: Option[String],
    //      output: Option[Path],
    //      nodePath: Option[Path],
    //      toolchain: List[Path]
    //        mapSourceURI: Option[URI]
    //      home: Option[Path],
    jvmOptions: Options,
    //      runtimeHome: Option[Path],
    jvmRuntimeOptions: Options,
    //        classpath: Option[List[Path]],
    //        resources: Option[List[Path]]
    nativeVersion: Option[VersionScalaNative],
    nativeGc: Option[String]
    //      targetTriple: Option[String],
    //      clang: Path,
    //      clangpp: Path,
    //      toolchain: List[Path],
    //      linker: Option[List[String]],
    //      compiler: Option[List[String]],
    //      linkStubs: Option[Boolean],
    //      check: Option[Boolean],
    //      dump: Option[Boolean],
    //      output: Option[Path]
) extends SetLike[Platform] {

  override def intersect(other: Platform): Platform =
    new Platform(
      name = if (name == other.name) name else None,
      mainClass = if (mainClass == other.mainClass) mainClass else None,
      jsVersion = if (jsVersion == other.jsVersion) jsVersion else None,
      jsKind = if (jsKind == other.jsKind) jsKind else None,
      jsSplitStyle = if (jsSplitStyle == other.jsSplitStyle) jsSplitStyle else None,
      jsEmitSourceMaps = if (jsEmitSourceMaps == other.jsEmitSourceMaps) jsEmitSourceMaps else None,
      jsJsdom = if (jsJsdom == other.jsJsdom) jsJsdom else None,
      jsNodeVersion = if (jsNodeVersion == other.jsNodeVersion) jsNodeVersion else None,
      //          mapSourceURI = if (mapSourceURI == other.mapSourceURI) mapSourceURI else None
      jvmOptions = jvmOptions.intersect(other.jvmOptions),
      jvmRuntimeOptions = jvmRuntimeOptions.intersect(other.jvmRuntimeOptions),
      nativeVersion = if (nativeVersion == other.nativeVersion) nativeVersion else None,
      nativeGc = if (nativeGc == other.nativeGc) nativeGc else None
    )

  override def removeAll(other: Platform): Platform =
    new Platform(
      name = if (name == other.name) None else name,
      mainClass = if (mainClass == other.mainClass) None else mainClass,
      jsVersion = if (jsVersion == other.jsVersion) None else jsVersion,
      jsKind = if (jsKind == other.jsKind) None else jsKind,
      jsSplitStyle = if (jsSplitStyle == other.jsSplitStyle) None else jsSplitStyle,
      jsEmitSourceMaps = if (jsEmitSourceMaps == other.jsEmitSourceMaps) None else jsEmitSourceMaps,
      jsJsdom = if (jsJsdom == other.jsJsdom) None else jsJsdom,
      jsNodeVersion = if (jsNodeVersion == other.jsNodeVersion) None else jsNodeVersion,
      //          mapSourceURI = if (mapSourceURI == other.mapSourceURI) None else mapSourceURI
      jvmOptions = jvmOptions.removeAll(other.jvmOptions),
      jvmRuntimeOptions = jvmRuntimeOptions.removeAll(other.jvmRuntimeOptions),
      nativeVersion = if (nativeVersion == other.nativeVersion) None else nativeVersion,
      nativeGc = if (nativeGc == other.nativeGc) None else nativeGc
    )

  override def union(other: Platform): Platform =
    new Platform(
      name = name.orElse(other.name),
      mainClass = mainClass.orElse(other.mainClass),
      jsVersion = jsVersion.orElse(other.jsVersion),
      jsKind = jsKind.orElse(other.jsKind),
      jsSplitStyle = jsSplitStyle.orElse(other.jsSplitStyle),
      jsEmitSourceMaps = jsEmitSourceMaps.orElse(other.jsEmitSourceMaps),
      jsJsdom = jsJsdom.orElse(other.jsJsdom),
      jsNodeVersion = jsNodeVersion.orElse(other.jsNodeVersion),
      //          mapSourceURI = mapSourceURI.orElse(other.mapSourceURI)
      jvmOptions = jvmOptions.union(other.jvmOptions),
      jvmRuntimeOptions = jvmRuntimeOptions.union(other.jvmRuntimeOptions),
      nativeVersion = nativeVersion.orElse(other.nativeVersion),
      nativeGc = nativeGc.orElse(other.nativeGc)
    )

  override def isEmpty: Boolean =
    name.isEmpty && mainClass.isEmpty && jsVersion.isEmpty && jsKind.isEmpty && jsEmitSourceMaps.isEmpty && jsJsdom.isEmpty && jsNodeVersion.isEmpty &&
      jvmOptions.isEmpty && jvmRuntimeOptions.isEmpty &&
      nativeVersion.isEmpty && nativeGc.isEmpty
}

object Platform {
  object Jvm {
    def apply(jvmOptions: Options, jvmMainClass: Option[String], jvmRuntimeOptions: Options) =
      new Platform(
        name = Some(PlatformId.Jvm),
        mainClass = jvmMainClass,
        jsVersion = None,
        jsKind = None,
        jsSplitStyle = None,
        jsEmitSourceMaps = None,
        jsJsdom = None,
        jsNodeVersion = None,
        jvmOptions = jvmOptions,
        jvmRuntimeOptions = jvmRuntimeOptions,
        nativeVersion = None,
        nativeGc = None
      )

    def unapply(x: Platform): Option[Platform] =
      x.name.flatMap {
        case PlatformId.Jvm => Some(x)
        case _              => None
      }

  }

  object Js {
    def apply(
        jsVersion: VersionScalaJs,
        jsKind: Option[ModuleKindJS],
        jsSplitStyle: Option[ModuleSplitStyleJS],
        jsEmitSourceMaps: Option[Boolean],
        jsJsdom: Option[Boolean],
        jsNodeVersion: Option[String],
        jsMainClass: Option[String]
    ) =
      new Platform(
        name = Some(PlatformId.Js),
        mainClass = jsMainClass,
        jsVersion = Some(jsVersion),
        jsKind = jsKind,
        jsSplitStyle = jsSplitStyle,
        jsEmitSourceMaps = jsEmitSourceMaps,
        jsJsdom = jsJsdom,
        jsNodeVersion = jsNodeVersion,
        jvmOptions = Options.empty,
        jvmRuntimeOptions = Options.empty,
        nativeVersion = None,
        nativeGc = None
      )

    def unapply(x: Platform): Option[Platform] =
      x.name.flatMap {
        case PlatformId.Js => Some(x)
        case _             => None
      }
  }

  object Native {
    def apply(nativeVersion: VersionScalaNative, nativeGc: Option[String], nativeMainClass: Option[String]) =
      new Platform(
        name = Some(PlatformId.Native),
        mainClass = nativeMainClass,
        jsVersion = None,
        jsKind = None,
        jsSplitStyle = None,
        jsEmitSourceMaps = None,
        jsJsdom = None,
        jsNodeVersion = None,
        jvmOptions = Options.empty,
        jvmRuntimeOptions = Options.empty,
        nativeVersion = Some(nativeVersion),
        nativeGc = nativeGc
      )

    def unapply(x: Platform): Option[Platform] =
      x.name.flatMap {
        case PlatformId.Native => Some(x)
        case _                 => None
      }
  }

  implicit val decodes: Decoder[Platform] = deriveDecoder
  implicit val encodes: Encoder[Platform] = deriveEncoder
}
