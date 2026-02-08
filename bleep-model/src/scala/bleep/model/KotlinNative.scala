package bleep.model

import bleep.internal.EnumCodec
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

/** Kotlin/Native configuration for a project.
  *
  * @param target
  *   Target platform (macosX64, macosArm64, linuxX64, mingwX64, etc.)
  * @param outputKind
  *   Output type (Executable, Klib, StaticLibrary, DynamicLibrary, Framework)
  * @param debuggable
  *   Whether to include debug info
  * @param optimized
  *   Whether to enable optimizations
  * @param baseName
  *   Optional base name for the output binary
  * @param binaryOptions
  *   Additional binary options
  * @param linkerOpts
  *   Linker options
  * @param freeCompilerArgs
  *   Additional compiler arguments
  * @param embedBitcode
  *   Apple bitcode embedding mode (for iOS/macOS)
  * @param isStatic
  *   For frameworks, whether to create a static framework
  */
case class KotlinNative(
    target: Option[KotlinNativeTarget],
    outputKind: Option[KotlinNativeOutputKind],
    debuggable: Option[Boolean],
    optimized: Option[Boolean],
    baseName: Option[String],
    binaryOptions: Option[Map[String, String]],
    linkerOpts: Option[List[String]],
    freeCompilerArgs: Option[List[String]],
    embedBitcode: Option[KotlinNativeEmbedBitcode],
    isStatic: Option[Boolean]
) extends SetLike[KotlinNative] {

  override def intersect(other: KotlinNative): KotlinNative =
    KotlinNative(
      target = if (target == other.target) target else None,
      outputKind = if (outputKind == other.outputKind) outputKind else None,
      debuggable = if (debuggable == other.debuggable) debuggable else None,
      optimized = if (optimized == other.optimized) optimized else None,
      baseName = if (baseName == other.baseName) baseName else None,
      binaryOptions = if (binaryOptions == other.binaryOptions) binaryOptions else None,
      linkerOpts = if (linkerOpts == other.linkerOpts) linkerOpts else None,
      freeCompilerArgs = if (freeCompilerArgs == other.freeCompilerArgs) freeCompilerArgs else None,
      embedBitcode = if (embedBitcode == other.embedBitcode) embedBitcode else None,
      isStatic = if (isStatic == other.isStatic) isStatic else None
    )

  override def removeAll(other: KotlinNative): KotlinNative =
    KotlinNative(
      target = if (target == other.target) None else target,
      outputKind = if (outputKind == other.outputKind) None else outputKind,
      debuggable = if (debuggable == other.debuggable) None else debuggable,
      optimized = if (optimized == other.optimized) None else optimized,
      baseName = if (baseName == other.baseName) None else baseName,
      binaryOptions = if (binaryOptions == other.binaryOptions) None else binaryOptions,
      linkerOpts = if (linkerOpts == other.linkerOpts) None else linkerOpts,
      freeCompilerArgs = if (freeCompilerArgs == other.freeCompilerArgs) None else freeCompilerArgs,
      embedBitcode = if (embedBitcode == other.embedBitcode) None else embedBitcode,
      isStatic = if (isStatic == other.isStatic) None else isStatic
    )

  override def union(other: KotlinNative): KotlinNative =
    KotlinNative(
      target = target.orElse(other.target),
      outputKind = outputKind.orElse(other.outputKind),
      debuggable = debuggable.orElse(other.debuggable),
      optimized = optimized.orElse(other.optimized),
      baseName = baseName.orElse(other.baseName),
      binaryOptions = binaryOptions.orElse(other.binaryOptions),
      linkerOpts = linkerOpts.orElse(other.linkerOpts),
      freeCompilerArgs = freeCompilerArgs.orElse(other.freeCompilerArgs),
      embedBitcode = embedBitcode.orElse(other.embedBitcode),
      isStatic = isStatic.orElse(other.isStatic)
    )

  override def isEmpty: Boolean =
    target.isEmpty && outputKind.isEmpty && debuggable.isEmpty && optimized.isEmpty &&
      baseName.isEmpty && binaryOptions.isEmpty && linkerOpts.isEmpty &&
      freeCompilerArgs.isEmpty && embedBitcode.isEmpty && isStatic.isEmpty
}

object KotlinNative {
  val empty: KotlinNative = KotlinNative(
    target = None,
    outputKind = None,
    debuggable = None,
    optimized = None,
    baseName = None,
    binaryOptions = None,
    linkerOpts = None,
    freeCompilerArgs = None,
    embedBitcode = None,
    isStatic = None
  )

  implicit val decodes: Decoder[KotlinNative] = deriveDecoder
  implicit val encodes: Encoder[KotlinNative] = deriveEncoder
}

/** Kotlin/Native target platform. */
sealed abstract class KotlinNativeTarget(val value: String) {

  /** The konan target name used by the Kotlin/Native compiler. */
  def konanName: String = value.replace("-", "_")
}

object KotlinNativeTarget {
  // macOS
  case object MacosX64 extends KotlinNativeTarget("macos-x64")
  case object MacosArm64 extends KotlinNativeTarget("macos-arm64")

  // Linux
  case object LinuxX64 extends KotlinNativeTarget("linux-x64")
  case object LinuxArm64 extends KotlinNativeTarget("linux-arm64")

  // Windows
  case object MingwX64 extends KotlinNativeTarget("mingw-x64")

  // iOS
  case object IosArm64 extends KotlinNativeTarget("ios-arm64")
  case object IosX64 extends KotlinNativeTarget("ios-x64")
  case object IosSimulatorArm64 extends KotlinNativeTarget("ios-simulator-arm64")

  // watchOS
  case object WatchosArm32 extends KotlinNativeTarget("watchos-arm32")
  case object WatchosArm64 extends KotlinNativeTarget("watchos-arm64")
  case object WatchosX64 extends KotlinNativeTarget("watchos-x64")
  case object WatchosSimulatorArm64 extends KotlinNativeTarget("watchos-simulator-arm64")

  // tvOS
  case object TvosArm64 extends KotlinNativeTarget("tvos-arm64")
  case object TvosX64 extends KotlinNativeTarget("tvos-x64")
  case object TvosSimulatorArm64 extends KotlinNativeTarget("tvos-simulator-arm64")

  // Android Native
  case object AndroidNativeArm32 extends KotlinNativeTarget("android-native-arm32")
  case object AndroidNativeArm64 extends KotlinNativeTarget("android-native-arm64")
  case object AndroidNativeX64 extends KotlinNativeTarget("android-native-x64")
  case object AndroidNativeX86 extends KotlinNativeTarget("android-native-x86")

  val All: List[KotlinNativeTarget] = List(
    MacosX64,
    MacosArm64,
    LinuxX64,
    LinuxArm64,
    MingwX64,
    IosArm64,
    IosX64,
    IosSimulatorArm64,
    WatchosArm32,
    WatchosArm64,
    WatchosX64,
    WatchosSimulatorArm64,
    TvosArm64,
    TvosX64,
    TvosSimulatorArm64,
    AndroidNativeArm32,
    AndroidNativeArm64,
    AndroidNativeX64,
    AndroidNativeX86
  )

  /** Detect the host target based on the current system. */
  def hostTarget: KotlinNativeTarget = {
    val os = System.getProperty("os.name").toLowerCase
    val arch = System.getProperty("os.arch").toLowerCase

    (os, arch) match {
      case (o, "aarch64") if o.contains("mac")   => MacosArm64
      case (o, _) if o.contains("mac")           => MacosX64
      case (o, "aarch64") if o.contains("linux") => LinuxArm64
      case (o, _) if o.contains("linux")         => LinuxX64
      case (o, _) if o.contains("windows")       => MingwX64
      case _                                     => LinuxX64
    }
  }

  implicit val codec: io.circe.Codec[KotlinNativeTarget] =
    EnumCodec.codec(All.map(x => (x.value, x)).toMap)
}

/** Kotlin/Native output kind. */
sealed abstract class KotlinNativeOutputKind(val value: String)

object KotlinNativeOutputKind {
  case object Executable extends KotlinNativeOutputKind("executable")
  case object Klib extends KotlinNativeOutputKind("klib")
  case object StaticLibrary extends KotlinNativeOutputKind("static")
  case object DynamicLibrary extends KotlinNativeOutputKind("dynamic")
  case object Framework extends KotlinNativeOutputKind("framework")
  case object FatFramework extends KotlinNativeOutputKind("fat-framework")
  case object XCFramework extends KotlinNativeOutputKind("xcframework")

  val All: List[KotlinNativeOutputKind] = List(
    Executable,
    Klib,
    StaticLibrary,
    DynamicLibrary,
    Framework,
    FatFramework,
    XCFramework
  )

  implicit val codec: io.circe.Codec[KotlinNativeOutputKind] =
    EnumCodec.codec(All.map(x => (x.value, x)).toMap)
}

/** Kotlin/Native Apple bitcode embedding mode. */
sealed abstract class KotlinNativeEmbedBitcode(val value: String)

object KotlinNativeEmbedBitcode {
  case object Disable extends KotlinNativeEmbedBitcode("disable")
  case object Bitcode extends KotlinNativeEmbedBitcode("bitcode")
  case object Marker extends KotlinNativeEmbedBitcode("marker")

  val All: List[KotlinNativeEmbedBitcode] = List(Disable, Bitcode, Marker)

  implicit val codec: io.circe.Codec[KotlinNativeEmbedBitcode] =
    EnumCodec.codec(All.map(x => (x.value, x)).toMap)
}
