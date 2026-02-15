package bleep.analysis

import java.nio.file.Path

/** Configuration for Scala Native linking.
  *
  * Contains all options needed to configure the Scala Native linker.
  */
case class ScalaNativeLinkConfig(
    mode: ScalaNativeLinkConfig.NativeMode,
    gc: ScalaNativeLinkConfig.NativeGC,
    lto: ScalaNativeLinkConfig.NativeLTO,
    buildTarget: ScalaNativeLinkConfig.NativeBuildTarget,
    clang: Option[Path],
    clangpp: Option[Path],
    compileOptions: Seq[String],
    linkingOptions: Seq[String],
    targetTriple: Option[String],
    embedResources: Boolean,
    incrementalCompilation: Boolean,
    multithreading: Option[Boolean],
    sanitizer: Option[ScalaNativeLinkConfig.Sanitizer],
    optimize: Boolean,
    dump: Boolean,
    check: Boolean
)

object ScalaNativeLinkConfig {

  /** Linker mode - determines optimization level.
    */
  sealed trait NativeMode {
    def name: String
  }
  object NativeMode {
    case object Debug extends NativeMode { val name = "debug" }
    case object ReleaseFast extends NativeMode { val name = "release-fast" }
    case object ReleaseFull extends NativeMode { val name = "release-full" }
    case object ReleaseSize extends NativeMode { val name = "release-size" }
  }

  /** Garbage collector to use.
    */
  sealed trait NativeGC {
    def name: String
  }
  object NativeGC {
    case object Immix extends NativeGC { val name = "immix" }
    case object Commix extends NativeGC { val name = "commix" }
    case object Boehm extends NativeGC { val name = "boehm" }
    case object NoGC extends NativeGC { val name = "none" }
    case object Experimental extends NativeGC { val name = "experimental" }
  }

  /** Link-Time Optimization level.
    */
  sealed trait NativeLTO {
    def name: String
  }
  object NativeLTO {
    case object None extends NativeLTO { val name = "none" }
    case object Thin extends NativeLTO { val name = "thin" }
    case object Full extends NativeLTO { val name = "full" }
  }

  /** Build target type.
    */
  sealed trait NativeBuildTarget {
    def name: String
  }
  object NativeBuildTarget {
    case object Application extends NativeBuildTarget { val name = "application" }
    case object LibraryDynamic extends NativeBuildTarget { val name = "library-dynamic" }
    case object LibraryStatic extends NativeBuildTarget { val name = "library-static" }
  }

  /** Sanitizer for debugging.
    */
  sealed trait Sanitizer {
    def name: String
  }
  object Sanitizer {
    case object AddressSanitizer extends Sanitizer { val name = "address" }
    case object ThreadSanitizer extends Sanitizer { val name = "thread" }
    case object UndefinedBehavior extends Sanitizer { val name = "undefined" }
  }

  /** Default debug configuration. */
  val Debug: ScalaNativeLinkConfig = ScalaNativeLinkConfig(
    mode = NativeMode.Debug,
    gc = NativeGC.Immix,
    lto = NativeLTO.None,
    buildTarget = NativeBuildTarget.Application,
    clang = None,
    clangpp = None,
    compileOptions = Seq.empty,
    linkingOptions = Seq.empty,
    targetTriple = None,
    embedResources = false,
    incrementalCompilation = true,
    multithreading = None,
    sanitizer = None,
    optimize = false,
    dump = false,
    check = false
  )

  /** Default release configuration. */
  val ReleaseFast: ScalaNativeLinkConfig = ScalaNativeLinkConfig(
    mode = NativeMode.ReleaseFast,
    gc = NativeGC.Immix,
    lto = NativeLTO.Thin,
    buildTarget = NativeBuildTarget.Application,
    clang = None,
    clangpp = None,
    compileOptions = Seq.empty,
    linkingOptions = Seq.empty,
    targetTriple = None,
    embedResources = true,
    incrementalCompilation = false,
    multithreading = None,
    sanitizer = None,
    optimize = true,
    dump = false,
    check = false
  )

  /** Default release-full configuration (maximum optimization). */
  val ReleaseFull: ScalaNativeLinkConfig = ScalaNativeLinkConfig(
    mode = NativeMode.ReleaseFull,
    gc = NativeGC.Immix,
    lto = NativeLTO.Full,
    buildTarget = NativeBuildTarget.Application,
    clang = None,
    clangpp = None,
    compileOptions = Seq.empty,
    linkingOptions = Seq.empty,
    targetTriple = None,
    embedResources = true,
    incrementalCompilation = false,
    multithreading = None,
    sanitizer = None,
    optimize = true,
    dump = false,
    check = false
  )
}

/** Result of Scala Native linking.
  */
case class ScalaNativeLinkResult(
    binary: Path,
    exitCode: Int
) {
  def isSuccess: Boolean = exitCode == 0
}
