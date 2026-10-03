package bleep
package scripts

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.security.MessageDigest
import scala.jdk.CollectionConverters.*

/** Builds the JNI library behind bleep's machine probes (`bleep-machine-probes/src/c/bleep_machine.c`), or checks that the checked-in builds are of the current
  * source. Run it as `bleep build-machine-native` after editing the C file.
  *
  * The libraries are checked in, under `bleep-machine-probes/src/resources/bleep/machine/native/<platform>/`, so packaging bleep needs no C toolchain and no
  * job waits for a native build. Next to each library, `<file>.source-sha256` records the SHA-256 of the C source it was built from.
  *
  *   - no arguments: builds the libraries this machine can build, into the checked-in location, and records the source hash next to each. `clang` on macOS
  *     (Xcode command line tools) builds one universal dylib for arm64 and x86_64; `clang` on Windows x64 (LLVM, which finds the MSVC libraries itself) builds
  *     the x64 DLL and cross-compiles the arm64 one. Linux needs no library and builds nothing.
  *   - `--check`: fails unless every library is checked in with a recorded hash equal to the current C source's. Needs no compiler; CI runs it.
  *
  * A machine can only build its own OS's libraries, so after editing the C file a developer builds theirs, pushes, and commits the other OS's libraries from
  * the artifact the `native-libs` CI job uploads — that job rebuilds every library, tests the probes against the fresh build on each OS, and fails while any
  * recorded hash is stale.
  *
  * Intel macOS (the x86_64 slice) and Windows on arm64 are built but untested: bleep does not support them and CI has no runner for them.
  */
object BuildMachineNative extends BleepScript("BuildMachineNative") {
  case class NativeTarget(platform: String, fileName: String)
  val DarwinUniversal: NativeTarget = NativeTarget("darwin-universal", "libbleep-machine.dylib")
  val WindowsX64: NativeTarget = NativeTarget("windows-x86_64", "bleep-machine.dll")
  val WindowsArm64: NativeTarget = NativeTarget("windows-arm64", "bleep-machine.dll")
  val All: List[NativeTarget] = List(DarwinUniversal, WindowsX64, WindowsArm64)

  /** The libraries this machine builds; none where the probes need no library (Linux) or this machine's compiler cannot build them. */
  def hostTargets(osName: String, osArch: String): List[NativeTarget] = {
    val os = osName.toLowerCase(java.util.Locale.ROOT)
    if (os.startsWith("mac")) List(DarwinUniversal)
    else if (os.startsWith("windows") && (osArch == "amd64" || osArch == "x86_64")) List(WindowsX64, WindowsArm64)
    else Nil
  }

  /** SHA-256 of the C source, with line endings normalised so a Windows checkout with `core.autocrlf` hashes the same as everyone else's. */
  def sourceHash(source: Path): String = {
    val normalised = new String(Files.readAllBytes(source), StandardCharsets.UTF_8).replace("\r\n", "\n")
    MessageDigest.getInstance("SHA-256").digest(normalised.getBytes(StandardCharsets.UTF_8)).map(b => f"${b & 0xff}%02x").mkString
  }

  def sidecar(lib: Path): Path = lib.resolveSibling(lib.getFileName.toString + ".source-sha256")

  override def run(started: Started, commands: Commands, args: List[String]): Unit = {
    val module = started.buildPaths.buildDir.resolve("bleep-machine-probes")
    val source = module.resolve("src/c/bleep_machine.c")
    val nativeDir = module.resolve("src/resources/bleep/machine/native")
    val hash = sourceHash(source)
    def lib(t: NativeTarget): Path = nativeDir.resolve(t.platform).resolve(t.fileName)

    args match {
      case List("--check") =>
        val problems = All.flatMap { t =>
          val l = lib(t)
          if (!Files.isRegularFile(l)) List(s"$l is missing")
          else if (!Files.isRegularFile(sidecar(l))) List(s"${sidecar(l)} is missing")
          else {
            val recorded = Files.readString(sidecar(l)).trim
            if (recorded != hash) List(s"$l was built from source $recorded, but $source is now $hash") else Nil
          }
        }
        if (problems.nonEmpty)
          sys.error(
            (s"The checked-in machine-probe libraries are not built from the current $source:" :: problems).mkString("\n  ") +
              "\nRebuild with `bleep build-machine-native` on macOS and Windows, or commit the `machine-probe-*` artifacts of the native-libs CI job."
          )
        started.logger.info(s"All ${All.size} machine-probe libraries are built from the current source ($hash)")

      case Nil =>
        val host = hostTargets(System.getProperty("os.name"), System.getProperty("os.arch"))
        if (host.isEmpty) started.logger.info(s"No machine-probe library to build on ${System.getProperty("os.name")}/${System.getProperty("os.arch")}")
        host.foreach { t =>
          compile(t, source, lib(t), started)
          Files.writeString(sidecar(lib(t)), hash + "\n")
          started.logger.info(s"Built ${lib(t)} from source $hash")
        }

      case other =>
        sys.error(s"usage: bleep build-machine-native [--check], got: ${other.mkString(" ")}")
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
      case DarwinUniversal =>
        // One file for both architectures, so the loader needs no choice between them. A fixed install name: by default it is the output path, a random
        // scratch directory, which would make every build a different file.
        List("clang", "-arch", "arm64", "-arch", "x86_64", "-mmacosx-version-min=11.0", "-dynamiclib", "-install_name", "@rpath/libbleep-machine.dylib") ++
          List("-O2", "-Wall", "-Wextra", "-Werror") ++
          List("-I", include.toString, "-I", include.resolve("darwin").toString, "-o", out.toString, source.toString)
      case WindowsX64 | WindowsArm64 =>
        // The C runtime linked statically, so the DLL depends on nothing but kernel32 — no assumption that the user's JDK ships a matching vcruntime.
        // `/Brepro` leaves the link timestamp out, for the same reason the macOS build fixes its install name: one source, one file. arm64 is a
        // cross-compile, which needs the Visual Studio ARM64 build tools; JNI's win32 headers are the same for both.
        val triple = if (t == WindowsArm64) "aarch64-pc-windows-msvc" else "x86_64-pc-windows-msvc"
        List("clang", s"--target=$triple", "-shared", "-O2", "-Wall", "-Werror", "-fms-runtime-lib=static", "-Wl,/Brepro") ++
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
                if (t == DarwinUniversal) "xcode-select --install" else "LLVM, with the Visual Studio C++ build tools including ARM64"
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
