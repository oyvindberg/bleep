package bleep.analysis

import java.nio.file.Path

/** Configuration for Kotlin/Native compilation.
  *
  * Contains all options needed to configure the K2NativeCompiler.
  */
case class KotlinNativeCompilerConfig(
    kotlinVersion: String,
    target: KotlinNativeCompilerConfig.Target,
    outputKind: KotlinNativeCompilerConfig.OutputKind,
    debuggable: Boolean,
    optimized: Boolean,
    baseName: Option[String],
    linkerOpts: Seq[String],
    freeCompilerArgs: Seq[String],
    entryPoint: Option[String],
    additionalOptions: Seq[String]
)

object KotlinNativeCompilerConfig {

  /** Target platform. */
  sealed trait Target {
    def value: String
    def konanName: String = value.replace("-", "_")
  }
  object Target {
    // macOS
    case object MacosX64 extends Target { val value = "macos_x64" }
    case object MacosArm64 extends Target { val value = "macos_arm64" }

    // Linux
    case object LinuxX64 extends Target { val value = "linux_x64" }
    case object LinuxArm64 extends Target { val value = "linux_arm64" }

    // Windows
    case object MingwX64 extends Target { val value = "mingw_x64" }

    // iOS
    case object IosArm64 extends Target { val value = "ios_arm64" }
    case object IosX64 extends Target { val value = "ios_x64" }
    case object IosSimulatorArm64 extends Target { val value = "ios_simulator_arm64" }

    /** Detect the host target based on the current system. */
    def hostTarget: Target = {
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
  }

  /** Output kind. */
  sealed trait OutputKind {
    def name: String
    def produce: String
  }
  object OutputKind {
    case object Executable extends OutputKind {
      val name = "executable"
      val produce = "program"
    }
    case object Klib extends OutputKind {
      val name = "klib"
      val produce = "library"
    }
    case object StaticLibrary extends OutputKind {
      val name = "static"
      val produce = "static"
    }
    case object DynamicLibrary extends OutputKind {
      val name = "dynamic"
      val produce = "dynamic"
    }
    case object Framework extends OutputKind {
      val name = "framework"
      val produce = "framework"
    }
  }

  /** Default configuration. */
  val Default: KotlinNativeCompilerConfig = KotlinNativeCompilerConfig(
    kotlinVersion = "2.3.0",
    target = Target.hostTarget,
    outputKind = OutputKind.Executable,
    debuggable = true,
    optimized = false,
    baseName = None,
    linkerOpts = Seq.empty,
    freeCompilerArgs = Seq.empty,
    entryPoint = None,
    additionalOptions = Seq.empty
  )
}

/** Result of Kotlin/Native compilation.
  */
case class KotlinNativeCompileResult(
    outputPath: Path,
    exitCode: Int
) {
  def isSuccess: Boolean = exitCode == 0
}
