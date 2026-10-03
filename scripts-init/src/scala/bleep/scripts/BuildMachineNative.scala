package bleep
package scripts

import java.nio.file.{Files, Path, StandardCopyOption}
import scala.jdk.CollectionConverters.*

/** Builds the JNI library behind bleep's machine probes (`bleep-machine-probes/src/c/bleep_machine.c`) into bleep-machine-probes's resources, under
  * `bleep/machine/native/<platform>/`, where `bleep.machine.MachineNative` finds it.
  *
  * A machine can only build its own platform's library, with the C compiler it has: `clang` on macOS (Xcode command line tools) and Windows (LLVM, which finds
  * the MSVC libraries itself). Linux needs no library, and Intel macOS is not supported, so on those this builds nothing. That is enough for every developer:
  * the bleep they build runs on the machine they built it on.
  *
  * A bleep-machine-probes jar that is published has to carry every platform's library, so CI builds each one on its own OS and hands them to the job that
  * publishes, which puts them under `native-prebuilt/<platform>/` before compiling. This copies whatever is there for the platforms it cannot build itself.
  * `native-prebuilt` is declared under bleep-machine-probes' `sourceGlobs`, so dropping a library there re-runs this script.
  */
object BuildMachineNative extends BleepCodegenScript("BuildMachineNative") {
  case class NativeTarget(platform: String, fileName: String)
  val MacOsArm64: NativeTarget = NativeTarget("darwin-arm64", "libbleep-machine.dylib")
  val WindowsX64: NativeTarget = NativeTarget("windows-x86_64", "bleep-machine.dll")
  val All: List[NativeTarget] = List(MacOsArm64, WindowsX64)

  /** The library this machine builds; `None` where the probes need no library (Linux) or bleep has none (any other platform). */
  def hostTarget(osName: String, osArch: String): Option[NativeTarget] = {
    val os = osName.toLowerCase(java.util.Locale.ROOT)
    if (os.startsWith("mac") && osArch == "aarch64") Some(MacOsArm64)
    else if (os.startsWith("windows") && (osArch == "amd64" || osArch == "x86_64")) Some(WindowsX64)
    else None
  }

  override def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
    val buildDir = started.buildPaths.buildDir
    val source = buildDir.resolve("bleep-machine-probes/src/c/bleep_machine.c")
    val prebuiltDir = buildDir.resolve("native-prebuilt")
    val host = hostTarget(System.getProperty("os.name"), System.getProperty("os.arch"))
    val logger = started.logger

    targets.foreach { target =>
      target.project.name.value match {
        case "bleep-machine-probes" =>
          val outDir = target.resources.resolve("bleep/machine/native")
          host match {
            case Some(t) =>
              if (Files.exists(prebuiltDir.resolve(t.platform)))
                sys.error(
                  s"${prebuiltDir.resolve(t.platform)} holds a prebuilt library for ${t.platform}, which this machine builds itself. Remove one of them."
                )
              compile(t, source, outDir.resolve(t.platform).resolve(t.fileName), started)
            case None =>
              logger.info(s"No machine-probe library to build on ${System.getProperty("os.name")}/${System.getProperty("os.arch")}")
          }
          All.filterNot(host.contains).foreach { t =>
            val prebuilt = prebuiltDir.resolve(t.platform).resolve(t.fileName)
            if (Files.exists(prebuilt)) {
              val to = outDir.resolve(t.platform).resolve(t.fileName)
              Files.createDirectories(to.getParent)
              Files.copy(prebuilt, to, StandardCopyOption.REPLACE_EXISTING)
              logger.info(s"Using prebuilt $prebuilt")
            }
          }
        case other =>
          sys.error(s"BuildMachineNative builds bleep-machine-probes' native library; it has nothing for '$other'")
      }
    }
  }

  def compile(t: NativeTarget, source: Path, to: Path, started: Started): Unit = {
    val javaHome = Path.of(System.getProperty("java.home"))
    val include = javaHome.resolve("include")
    if (!Files.isRegularFile(include.resolve("jni.h")))
      sys.error(s"$javaHome has no include/jni.h, so the machine-probe library cannot be built with it. Use a full JDK as the build's JVM.")

    // Built in a scratch directory: on Windows the linker writes an import library and an export file next to the DLL, and only the DLL is wanted.
    val scratch = Files.createTempDirectory("bleep-machine-native")
    val out = scratch.resolve(t.fileName)
    val cmd: List[String] = t match {
      case MacOsArm64 =>
        // A fixed install name: by default it is the output path, a random scratch directory, which would make every build a different file — and the loader
        // unpacks the library under its content hash, so each rebuild would leave another copy in the user's cache directory.
        List("clang", "-arch", "arm64", "-mmacosx-version-min=11.0", "-dynamiclib", "-install_name", "@rpath/libbleep-machine.dylib") ++
          List("-O2", "-Wall", "-Wextra", "-Werror") ++
          List("-I", include.toString, "-I", include.resolve("darwin").toString, "-o", out.toString, source.toString)
      case WindowsX64 =>
        // The C runtime linked statically, so the DLL depends on nothing but kernel32 — no assumption that the user's JDK ships a matching vcruntime.
        // `/Brepro` leaves the link timestamp out, for the same reason the macOS build fixes its install name: one source, one file.
        List("clang", "-shared", "-O2", "-Wall", "-Werror", "-fms-runtime-lib=static", "-Wl,/Brepro") ++
          List("-I", include.toString, "-I", include.resolve("win32").toString, "-o", out.toString, source.toString)
      case other =>
        sys.error(s"no compile command for $other")
    }
    started.logger.info(s"Building ${t.platform} machine-probe library: ${cmd.mkString(" ")}")
    val process =
      try new ProcessBuilder(cmd.asJava).redirectErrorStream(true).start()
      catch {
        case e: java.io.IOException =>
          throw new RuntimeException(
            s"Could not start `clang` to build bleep's machine-probe library for ${t.platform}. Install a C compiler (${
                if (t == MacOsArm64) "xcode-select --install" else "LLVM, with the Visual Studio C++ build tools"
              }).",
            e
          )
      }
    val output = new String(process.getInputStream.readAllBytes(), "UTF-8")
    val exit = process.waitFor()
    if (output.nonEmpty) started.logger.info(output)
    if (exit != 0) sys.error(s"clang failed with exit code $exit building ${t.platform} machine-probe library:\n$output")

    Files.createDirectories(to.getParent)
    Files.copy(out, to, StandardCopyOption.REPLACE_EXISTING)
    Files.list(scratch).iterator().asScala.foreach(Files.delete)
    Files.delete(scratch)
  }
}
