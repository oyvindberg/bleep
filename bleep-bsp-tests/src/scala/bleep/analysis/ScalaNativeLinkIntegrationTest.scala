package bleep.analysis

import bleep.bsp.{LinkExecutor, Outcome, TaskDag}
import bleep.bsp.Outcome.{KillReason, RunOutcome}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

/** Integration tests for Scala Native linking.
  *
  * Tests:
  *   - Linking configurations (Debug, ReleaseFast, ReleaseFull)
  *   - Different GC options
  *   - Native binary execution
  */
class ScalaNativeLinkIntegrationTest extends AnyFunSuite with Matchers {

  def createTempDir(prefix: String): Path =
    Files.createTempDirectory(prefix)

  def deleteRecursively(path: Path): Unit =
    if (Files.exists(path)) {
      if (Files.isDirectory(path)) {
        import scala.jdk.StreamConverters._
        Files.list(path).toScala(List).foreach(deleteRecursively)
      }
      Files.delete(path)
    }

  // ==========================================================================
  // ScalaNativeLinkConfig Tests
  // ==========================================================================

  test("ScalaNativeLinkConfig: Debug configuration defaults") {
    val config = ScalaNativeLinkConfig.Debug

    config.mode shouldBe ScalaNativeLinkConfig.NativeMode.Debug
    config.gc shouldBe ScalaNativeLinkConfig.NativeGC.Immix
    config.lto shouldBe ScalaNativeLinkConfig.NativeLTO.None
    config.buildTarget shouldBe ScalaNativeLinkConfig.NativeBuildTarget.Application
    config.incrementalCompilation shouldBe true
    config.optimize shouldBe false
  }

  test("ScalaNativeLinkConfig: ReleaseFast configuration defaults") {
    val config = ScalaNativeLinkConfig.ReleaseFast

    config.mode shouldBe ScalaNativeLinkConfig.NativeMode.ReleaseFast
    config.lto shouldBe ScalaNativeLinkConfig.NativeLTO.Thin
    config.embedResources shouldBe true
    config.incrementalCompilation shouldBe false
    config.optimize shouldBe true
  }

  test("ScalaNativeLinkConfig: ReleaseFull configuration defaults") {
    val config = ScalaNativeLinkConfig.ReleaseFull

    config.mode shouldBe ScalaNativeLinkConfig.NativeMode.ReleaseFull
    config.lto shouldBe ScalaNativeLinkConfig.NativeLTO.Full
    config.optimize shouldBe true
  }

  test("ScalaNativeLinkConfig: GC options") {
    ScalaNativeLinkConfig.NativeGC.Immix.name shouldBe "immix"
    ScalaNativeLinkConfig.NativeGC.Commix.name shouldBe "commix"
    ScalaNativeLinkConfig.NativeGC.Boehm.name shouldBe "boehm"
    ScalaNativeLinkConfig.NativeGC.NoGC.name shouldBe "none"
  }

  test("ScalaNativeLinkConfig: LTO options") {
    ScalaNativeLinkConfig.NativeLTO.None.name shouldBe "none"
    ScalaNativeLinkConfig.NativeLTO.Thin.name shouldBe "thin"
    ScalaNativeLinkConfig.NativeLTO.Full.name shouldBe "full"
  }

  test("ScalaNativeLinkConfig: build targets") {
    ScalaNativeLinkConfig.NativeBuildTarget.Application.name shouldBe "application"
    ScalaNativeLinkConfig.NativeBuildTarget.LibraryDynamic.name shouldBe "library-dynamic"
    ScalaNativeLinkConfig.NativeBuildTarget.LibraryStatic.name shouldBe "library-static"
  }

  test("ScalaNativeLinkConfig: sanitizers") {
    ScalaNativeLinkConfig.Sanitizer.AddressSanitizer.name shouldBe "address"
    ScalaNativeLinkConfig.Sanitizer.ThreadSanitizer.name shouldBe "thread"
    ScalaNativeLinkConfig.Sanitizer.UndefinedBehavior.name shouldBe "undefined"
  }

  test("ScalaNativeLinkConfig: custom configuration") {
    val config = ScalaNativeLinkConfig.Debug.copy(
      gc = ScalaNativeLinkConfig.NativeGC.Boehm,
      multithreading = Some(true),
      sanitizer = Some(ScalaNativeLinkConfig.Sanitizer.AddressSanitizer)
    )

    config.gc shouldBe ScalaNativeLinkConfig.NativeGC.Boehm
    config.multithreading shouldBe Some(true)
    config.sanitizer shouldBe Some(ScalaNativeLinkConfig.Sanitizer.AddressSanitizer)
  }

  // ==========================================================================
  // ScalaNativeToolchain Factory Tests
  // ==========================================================================

  test("ScalaNativeToolchain.forVersion: creates toolchain for 0.4.x") {
    val toolchain = ScalaNativeToolchain.forVersion("0.4.17", "2.13.15")
    toolchain shouldBe a[ScalaNative04Bridge]
  }

  test("ScalaNativeToolchain.forVersion: creates toolchain for 0.5.x") {
    val toolchain = ScalaNativeToolchain.forVersion("0.5.6", "3.3.3")
    toolchain shouldBe a[ScalaNative05Bridge]
  }

  test("ScalaNativeToolchain.forVersion: creates toolchain for 1.0.x") {
    val toolchain = ScalaNativeToolchain.forVersion("1.0.0", "3.3.3")
    toolchain shouldBe a[ScalaNative05Bridge] // 1.0 uses 0.5 API
  }

  test("ScalaNativeToolchain.forVersion: rejects unsupported versions") {
    assertThrows[IllegalArgumentException] {
      ScalaNativeToolchain.forVersion("0.3.0", "2.13.15")
    }
  }

  // ==========================================================================
  // LinkPlatform Tests
  // ==========================================================================

  test("LinkPlatform.ScalaNative: stores version and config") {
    val config = ScalaNativeLinkConfig.Debug
    val platform = TaskDag.LinkPlatform.ScalaNative("0.5.6", "3.3.3", config)

    platform.version shouldBe "0.5.6"
    platform.scalaVersion shouldBe "3.3.3"
    platform.config shouldBe config
  }

  // ==========================================================================
  // LinkResult Tests
  // ==========================================================================

  test("LinkResult.NativeSuccess: stores binary path") {
    val binary = Path.of("out/myapp")
    val result = TaskDag.LinkResult.NativeSuccess(binary, wasUpToDate = false)

    result.binary shouldBe binary
    result.wasUpToDate shouldBe false
  }

  test("ScalaNativeLinkResult: isSuccess based on exit code") {
    val success = ScalaNativeLinkResult(Path.of("out/myapp"), 0)
    val failure = ScalaNativeLinkResult(Path.of("out/myapp"), 1)

    success.isSuccess shouldBe true
    failure.isSuccess shouldBe false
  }

  // ==========================================================================
  // CompilerResolver Tests for Scala Native
  // ==========================================================================

  test("CompilerResolver: resolves Scala Native tools JARs") {
    val jars = CompilerResolver.resolveScalaNativeTools("0.5.6", "3")
    jars should not be empty
    info(s"Resolved ${jars.size} JARs for Scala Native 0.5.6")
    jars.exists(_.getFileName.toString.contains("tools")) shouldBe true
  }

  test("CompilerResolver: caches Scala Native tools instances") {
    val instance1 = CompilerResolver.getScalaNativeTools("0.5.6", "3.3.3")
    val instance2 = CompilerResolver.getScalaNativeTools("0.5.6", "3.3.3")

    instance1.loader shouldBe instance2.loader
    info("Scala Native tools instances are cached correctly")
  }

  // ==========================================================================
  // Clang Detection Tests
  // ==========================================================================

  test("detect clang availability") {
    val clangAvailable = isClangAvailable()
    if (clangAvailable) {
      info("clang is available")
    } else {
      info("clang is not available, Scala Native linking tests will be skipped")
    }
  }

  private def isClangAvailable(): Boolean =
    try {
      val pb = new ProcessBuilder("clang", "--version")
        .redirectOutput(ProcessBuilder.Redirect.DISCARD)
        .redirectError(ProcessBuilder.Redirect.DISCARD)
      pb.start().waitFor() == 0
    } catch {
      case _: Exception => false
    }
}

/** Advanced Scala Native linking tests that compile and link actual .nir files.
  */
class ScalaNativeAdvancedLinkIntegrationTest extends AnyFunSuite with Matchers with PlatformTestHelper {

  /** Homebrew LLVM clang path — needed for LTO because Apple clang lacks the LTO plugin for Scala Native. */
  private lazy val homebrewClang: Option[Path] = {
    val candidate = Path.of("/opt/homebrew/opt/llvm/bin/clang")
    if (Files.isExecutable(candidate)) Some(candidate) else Option.empty
  }

  private lazy val homebrewClangPP: Option[Path] = {
    val candidate = Path.of("/opt/homebrew/opt/llvm/bin/clang++")
    if (Files.isExecutable(candidate)) Some(candidate) else Option.empty
  }

  /** macOS SDK root — Homebrew LLVM needs this to find system libraries. */
  private lazy val sdkRoot: String = runCommand("xcrun", "--show-sdk-path")

  private lazy val ltoAvailable: Boolean = {
    // Scala Native LTO needs full LLVM (not Apple clang). Test full LTO since thin is unstable on macOS.
    val clang = homebrewClang.map(_.toString).getOrElse("clang")
    val sysrootFlag = if (sdkRoot.nonEmpty) s"-isysroot $sdkRoot" else ""
    val pb = new ProcessBuilder("sh", "-c", s"echo 'int main(){return 0;}' | $clang $sysrootFlag -flto=full -x c - -o /dev/null 2>/dev/null")
    pb.redirectErrorStream(true)
    try {
      val p = pb.start()
      p.waitFor() == 0
    } catch { case _: Exception => false }
  }

  private lazy val boehmGcAvailable: Boolean = {
    // Use pkg-config which is how Scala Native actually discovers bdw-gc at link time.
    // On macOS with Homebrew, gc.h is not on clang's default include path but pkg-config knows about it.
    val pb = new ProcessBuilder("pkg-config", "--exists", "bdw-gc")
    pb.redirectErrorStream(true)
    try {
      val p = pb.start()
      p.waitFor() == 0
    } catch { case _: Exception => false }
  }

  /** Get compile flags for Boehm GC from pkg-config */
  private lazy val boehmGcCompileFlags: Seq[String] = runCommand("pkg-config", "--cflags", "bdw-gc").split("\\s+").filter(_.nonEmpty).toSeq
  private lazy val boehmGcLinkFlags: Seq[String] = runCommand("pkg-config", "--libs", "bdw-gc").split("\\s+").filter(_.nonEmpty).toSeq

  private def runCommand(cmd: String*): String = {
    val pb = new ProcessBuilder(cmd*)
    pb.redirectErrorStream(true)
    try {
      val p = pb.start()
      val output = new String(p.getInputStream.readAllBytes()).trim
      if (p.waitFor() == 0) output else ""
    } catch { case _: Exception => "" }
  }

  private val scalaSource =
    """package example
      |
      |object Main {
      |  def main(args: Array[String]): Unit = {
      |    println("LinkOutput77")
      |  }
      |}
      |""".stripMargin

  private def compileAndGetClasspath(tempDir: Path): Seq[Path] = {
    val srcDir = tempDir.resolve("src")
    writeScalaSource(srcDir, "example", "Main.scala", scalaSource)
    val outDir = tempDir.resolve("classes")
    compileForScalaNative(srcDir, outDir, DefaultScalaVersion, DefaultScalaNativeVersion)
  }

  test("ScalaNative05Bridge: link project in debug mode") {
    withTempDir("sn-link-debug") { tempDir =>
      val classpath = compileAndGetClasspath(tempDir)
      val binaryPath = tempDir.resolve("app" + ScalaNativeRunner.binaryExtension)
      val workDir = tempDir.resolve("work")
      Files.createDirectories(workDir)

      val toolchain = ScalaNativeToolchain.forVersion(DefaultScalaNativeVersion, DefaultScalaVersion)
      val result = toolchain
        .link(ScalaNativeLinkConfig.Debug, classpath, "example.Main", binaryPath, workDir, ScalaNativeToolchain.Logger.Silent, CancellationToken.never)
        .unsafeRunSync()

      assert(result.isSuccess, s"Debug link failed: exit code ${result.exitCode}")
      assert(Files.exists(binaryPath))
      info(s"Debug binary: $binaryPath")
    }
  }

  test("ScalaNative05Bridge: link project in release mode with LTO") {
    assume(ltoAvailable, "Full LTO support not available (requires Homebrew LLVM)")
    withTempDir("sn-link-releasefast") { tempDir =>
      val classpath = compileAndGetClasspath(tempDir)
      val binaryPath = tempDir.resolve("app" + ScalaNativeRunner.binaryExtension)
      val workDir = tempDir.resolve("work")
      Files.createDirectories(workDir)

      val toolchain = ScalaNativeToolchain.forVersion(DefaultScalaNativeVersion, DefaultScalaVersion)
      val sysrootFlags = if (sdkRoot.nonEmpty) Seq(s"-isysroot", sdkRoot) else Seq.empty
      // Use LTO.full instead of thin — thin LTO on macOS is unstable with Scala Native's
      // vendored libunwind (symbols get lost during LTO optimization).
      val config = ScalaNativeLinkConfig.ReleaseFast.copy(
        clang = homebrewClang,
        clangpp = homebrewClangPP,
        lto = ScalaNativeLinkConfig.NativeLTO.Full,
        compileOptions = sysrootFlags,
        linkingOptions = sysrootFlags
      )
      val result = toolchain
        .link(config, classpath, "example.Main", binaryPath, workDir, ScalaNativeToolchain.Logger.Silent, CancellationToken.never)
        .unsafeRunSync()

      assert(result.isSuccess, s"ReleaseFast link failed: exit code ${result.exitCode}")
      assert(Files.exists(binaryPath))
      info(s"ReleaseFast binary: $binaryPath")
    }
  }

  test("ScalaNative05Bridge: link project with different GC") {
    assume(boehmGcAvailable, "Boehm GC (libgc) not installed")
    withTempDir("sn-link-gc") { tempDir =>
      val classpath = compileAndGetClasspath(tempDir)
      val binaryPath = tempDir.resolve("app" + ScalaNativeRunner.binaryExtension)
      val workDir = tempDir.resolve("work")
      Files.createDirectories(workDir)

      val toolchain = ScalaNativeToolchain.forVersion(DefaultScalaNativeVersion, DefaultScalaVersion)
      val config = ScalaNativeLinkConfig.Debug.copy(
        gc = ScalaNativeLinkConfig.NativeGC.Boehm,
        compileOptions = boehmGcCompileFlags,
        linkingOptions = boehmGcLinkFlags
      )
      val result = toolchain
        .link(config, classpath, "example.Main", binaryPath, workDir, ScalaNativeToolchain.Logger.Silent, CancellationToken.never)
        .unsafeRunSync()

      assert(result.isSuccess, s"Boehm GC link failed: exit code ${result.exitCode}")
      assert(Files.exists(binaryPath))
      info(s"Boehm GC binary: $binaryPath")
    }
  }

  test("ScalaNative05Bridge: link and run native binary") {
    withTempDir("sn-link-run") { tempDir =>
      val classpath = compileAndGetClasspath(tempDir)
      val binaryPath = tempDir.resolve("app" + ScalaNativeRunner.binaryExtension)
      val workDir = tempDir.resolve("work")
      Files.createDirectories(workDir)

      val toolchain = ScalaNativeToolchain.forVersion(DefaultScalaNativeVersion, DefaultScalaVersion)
      val linkResult = toolchain
        .link(ScalaNativeLinkConfig.Debug, classpath, "example.Main", binaryPath, workDir, ScalaNativeToolchain.Logger.Silent, CancellationToken.never)
        .unsafeRunSync()

      assert(linkResult.isSuccess, "Link failed")

      val runResult = (for {
        killSignal <- Outcome.neverKillSignal
        outcome <- ScalaNativeRunner.run(binaryPath, Seq.empty, tempDir, Map.empty, killSignal)
      } yield outcome).unsafeRunSync()

      runResult match {
        case RunOutcome.Completed(exitCode, stdout, _) =>
          exitCode shouldBe 0
          stdout should include("LinkOutput77")
          info(s"Ran linked binary, stdout: ${stdout.trim}")
        case other =>
          fail(s"Unexpected outcome: $other")
      }
    }
  }
}
