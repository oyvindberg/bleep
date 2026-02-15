package bleep.analysis

import bleep.bsp.Outcome
import bleep.bsp.Outcome.{KillReason, RunOutcome}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import cats.effect.unsafe.implicits.global

/** Integration tests for Scala Native toolchain support.
  *
  * Tests linking and running Scala Native projects with different versions and configurations.
  */
class ScalaNativeIntegrationTest extends AnyFunSuite with Matchers {

  def createTempDir(prefix: String): Path =
    Files.createTempDirectory(prefix)

  def deleteRecursively(path: Path): Unit =
    if (Files.exists(path)) {
      if (Files.isDirectory(path)) {
        import scala.jdk.StreamConverters.*
        Files.list(path).toScala(List).foreach(deleteRecursively)
      }
      Files.delete(path)
    }

  // ============================================================================
  // Compiler Resolution Tests
  // ============================================================================

  test("CompilerResolver: resolves Scala Native tools for 0.4.17") {
    val jars = CompilerResolver.resolveScalaNativeTools("0.4.17", "2.13")
    jars should not be empty
    info(s"Resolved ${jars.size} JARs for Scala Native 0.4.17")
    jars.exists(_.getFileName.toString.contains("tools")) shouldBe true
  }

  test("CompilerResolver: resolves Scala Native tools for 0.5.6") {
    val jars = CompilerResolver.resolveScalaNativeTools("0.5.6", "3")
    jars should not be empty
    info(s"Resolved ${jars.size} JARs for Scala Native 0.5.6")
    jars.exists(_.getFileName.toString.contains("tools")) shouldBe true
  }

  test("CompilerResolver: different Scala Native versions have different classloaders") {
    val instance04 = CompilerResolver.getScalaNativeTools("0.4.17", "2.13")
    val instance05 = CompilerResolver.getScalaNativeTools("0.5.6", "3")

    instance04.loader should not be instance05.loader
    info("Different Scala Native versions have isolated classloaders")
  }

  test("CompilerResolver: caches Scala Native tools instances") {
    val instance1 = CompilerResolver.getScalaNativeTools("0.5.6", "3")
    val instance2 = CompilerResolver.getScalaNativeTools("0.5.6", "3")

    instance1.loader shouldBe instance2.loader
    info("Scala Native tools instances are cached correctly")
  }

  // ============================================================================
  // ScalaNativeToolchain Factory Tests
  // ============================================================================

  test("ScalaNativeToolchain.forVersion: creates 0.4 bridge for 0.4.x") {
    val toolchain = ScalaNativeToolchain.forVersion("0.4.17", "2.13.15")
    toolchain shouldBe a[ScalaNative04Bridge]
  }

  test("ScalaNativeToolchain.forVersion: creates 0.5 bridge for 0.5.x") {
    val toolchain = ScalaNativeToolchain.forVersion("0.5.6", "3.3.3")
    toolchain shouldBe a[ScalaNative05Bridge]
  }

  test("ScalaNativeToolchain.forVersion: creates 0.5 bridge for 1.0.x") {
    val toolchain = ScalaNativeToolchain.forVersion("1.0.0", "3.3.3")
    toolchain shouldBe a[ScalaNative05Bridge]
  }

  // ============================================================================
  // ScalaNativeRunner Tests
  // ============================================================================

  test("ScalaNativeRunner: isWindows/isMacOS/isLinux detection") {
    val os = System.getProperty("os.name").toLowerCase
    if (os.contains("windows")) {
      ScalaNativeRunner.isWindows shouldBe true
      ScalaNativeRunner.binaryExtension shouldBe ".exe"
    } else if (os.contains("mac")) {
      ScalaNativeRunner.isMacOS shouldBe true
      ScalaNativeRunner.dynamicLibraryExtension shouldBe ".dylib"
    } else if (os.contains("linux")) {
      ScalaNativeRunner.isLinux shouldBe true
      ScalaNativeRunner.dynamicLibraryExtension shouldBe ".so"
    }
    info(s"Detected OS: $os")
  }

  test("ScalaNativeRunner: static library extension") {
    if (ScalaNativeRunner.isWindows) {
      ScalaNativeRunner.staticLibraryExtension shouldBe ".lib"
    } else {
      ScalaNativeRunner.staticLibraryExtension shouldBe ".a"
    }
  }

  // ============================================================================
  // Configuration Tests
  // ============================================================================

  test("ScalaNativeLinkConfig: default debug configuration") {
    val config = ScalaNativeLinkConfig.Debug
    config.mode shouldBe ScalaNativeLinkConfig.NativeMode.Debug
    config.gc shouldBe ScalaNativeLinkConfig.NativeGC.Immix
    config.lto shouldBe ScalaNativeLinkConfig.NativeLTO.None
    config.incrementalCompilation shouldBe true
    config.optimize shouldBe false
  }

  test("ScalaNativeLinkConfig: default release-fast configuration") {
    val config = ScalaNativeLinkConfig.ReleaseFast
    config.mode shouldBe ScalaNativeLinkConfig.NativeMode.ReleaseFast
    config.lto shouldBe ScalaNativeLinkConfig.NativeLTO.Thin
    config.optimize shouldBe true
    config.embedResources shouldBe true
  }

  test("ScalaNativeLinkConfig: default release-full configuration") {
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
    ScalaNativeLinkConfig.NativeGC.Experimental.name shouldBe "experimental"
  }

  test("ScalaNativeLinkConfig: LTO options") {
    ScalaNativeLinkConfig.NativeLTO.None.name shouldBe "none"
    ScalaNativeLinkConfig.NativeLTO.Thin.name shouldBe "thin"
    ScalaNativeLinkConfig.NativeLTO.Full.name shouldBe "full"
  }

  test("ScalaNativeLinkConfig: build target options") {
    ScalaNativeLinkConfig.NativeBuildTarget.Application.name shouldBe "application"
    ScalaNativeLinkConfig.NativeBuildTarget.LibraryDynamic.name shouldBe "library-dynamic"
    ScalaNativeLinkConfig.NativeBuildTarget.LibraryStatic.name shouldBe "library-static"
  }

  test("ScalaNativeLinkConfig: sanitizer options") {
    ScalaNativeLinkConfig.Sanitizer.AddressSanitizer.name shouldBe "address"
    ScalaNativeLinkConfig.Sanitizer.ThreadSanitizer.name shouldBe "thread"
    ScalaNativeLinkConfig.Sanitizer.UndefinedBehavior.name shouldBe "undefined"
  }

  // ============================================================================
  // ScalaNativeLinkResult Tests
  // ============================================================================

  test("ScalaNativeLinkResult: isSuccess when exitCode is 0") {
    val result = ScalaNativeLinkResult(
      binary = Path.of("app"),
      exitCode = 0
    )
    result.isSuccess shouldBe true
  }

  test("ScalaNativeLinkResult: not success when exitCode is non-zero") {
    val result = ScalaNativeLinkResult(
      binary = Path.of("app"),
      exitCode = 1
    )
    result.isSuccess shouldBe false
  }

  // ============================================================================
  // Logger Tests
  // ============================================================================

  test("ScalaNativeToolchain.Logger.Silent: does not output") {
    val logger = ScalaNativeToolchain.Logger.Silent
    // These should not throw
    logger.trace("test")
    logger.debug("test")
    logger.info("test")
    logger.warn("test")
    logger.error("test")
    logger.running(Seq("test", "command"))
    // If we got here without exception, the test passes
  }
}

/** Advanced Scala Native integration tests that compile and link to native binaries.
  */
class ScalaNativeAdvancedIntegrationTest extends AnyFunSuite with Matchers with PlatformTestHelper {

  private val scalaSource =
    """package example
      |
      |object Main {
      |  def main(args: Array[String]): Unit = {
      |    println("NativeOutput99")
      |  }
      |}
      |""".stripMargin

  test("ScalaNative05Bridge: link simple project with Debug mode") {
    withTempDir("sn-debug") { tempDir =>
      val srcDir = tempDir.resolve("src")
      writeScalaSource(srcDir, "example", "Main.scala", scalaSource)
      val outDir = tempDir.resolve("classes")
      val classpath = compileForScalaNative(srcDir, outDir, DefaultScalaVersion, DefaultScalaNativeVersion)

      val binaryPath = tempDir.resolve("app" + ScalaNativeRunner.binaryExtension)
      val workDir = tempDir.resolve("work")
      Files.createDirectories(workDir)

      val toolchain = ScalaNativeToolchain.forVersion(DefaultScalaNativeVersion, DefaultScalaVersion)
      val result = toolchain
        .link(ScalaNativeLinkConfig.Debug, classpath, "example.Main", binaryPath, workDir, ScalaNativeToolchain.Logger.Silent, CancellationToken.never)
        .unsafeRunSync()

      assert(result.isSuccess, s"Debug linking failed with exit code ${result.exitCode}")
      assert(Files.exists(binaryPath), s"Binary not found at $binaryPath")
      info(s"Successfully linked Debug binary: $binaryPath")
    }
  }

  test("ScalaNative05Bridge: link simple project with ReleaseFast mode") {
    withTempDir("sn-releasefast") { tempDir =>
      val srcDir = tempDir.resolve("src")
      writeScalaSource(srcDir, "example", "Main.scala", scalaSource)
      val outDir = tempDir.resolve("classes")
      val classpath = compileForScalaNative(srcDir, outDir, DefaultScalaVersion, DefaultScalaNativeVersion)

      val binaryPath = tempDir.resolve("app" + ScalaNativeRunner.binaryExtension)
      val workDir = tempDir.resolve("work")
      Files.createDirectories(workDir)

      // Use LTO.None to avoid known ARM64 LTO issue with libunwind in SN 0.5.x
      // (ld: symbol(s) defined in LTO objects are referenced but missing in compiled objects)
      val config = ScalaNativeLinkConfig.ReleaseFast.copy(lto = ScalaNativeLinkConfig.NativeLTO.None)
      val toolchain = ScalaNativeToolchain.forVersion(DefaultScalaNativeVersion, DefaultScalaVersion)
      val result = toolchain
        .link(config, classpath, "example.Main", binaryPath, workDir, ScalaNativeToolchain.Logger.Silent, CancellationToken.never)
        .unsafeRunSync()

      assert(result.isSuccess, s"ReleaseFast linking failed with exit code ${result.exitCode}")
      assert(Files.exists(binaryPath), s"Binary not found at $binaryPath")
      info(s"Successfully linked ReleaseFast binary: $binaryPath")
    }
  }

  test("ScalaNative05Bridge: link and run binary") {
    withTempDir("sn-run") { tempDir =>
      val srcDir = tempDir.resolve("src")
      writeScalaSource(srcDir, "example", "Main.scala", scalaSource)
      val outDir = tempDir.resolve("classes")
      val classpath = compileForScalaNative(srcDir, outDir, DefaultScalaVersion, DefaultScalaNativeVersion)

      val binaryPath = tempDir.resolve("app" + ScalaNativeRunner.binaryExtension)
      val workDir = tempDir.resolve("work")
      Files.createDirectories(workDir)

      val toolchain = ScalaNativeToolchain.forVersion(DefaultScalaNativeVersion, DefaultScalaVersion)
      val linkResult = toolchain
        .link(ScalaNativeLinkConfig.Debug, classpath, "example.Main", binaryPath, workDir, ScalaNativeToolchain.Logger.Silent, CancellationToken.never)
        .unsafeRunSync()

      assert(linkResult.isSuccess, "Linking failed")

      val runResult = (for {
        killSignal <- Outcome.neverKillSignal
        outcome <- ScalaNativeRunner.run(binaryPath, Seq.empty, tempDir, Map.empty, killSignal)
      } yield outcome).unsafeRunSync()

      runResult match {
        case RunOutcome.Completed(exitCode, stdout, _) =>
          exitCode shouldBe 0
          stdout should include("NativeOutput99")
          info(s"Successfully ran native binary, stdout: ${stdout.trim}")
        case other =>
          fail(s"Unexpected outcome: $other")
      }
    }
  }

  test("ScalaNative04Bridge: link simple project (0.4.x API)") {
    withTempDir("sn04") { tempDir =>
      val srcDir = tempDir.resolve("src")
      writeScalaSource(srcDir, "example", "Main.scala", scalaSource)
      val outDir = tempDir.resolve("classes")
      val classpath = compileForScalaNative(srcDir, outDir, DefaultScala213Version, DefaultScalaNative04Version)

      val binaryPath = tempDir.resolve("app" + ScalaNativeRunner.binaryExtension)
      val workDir = tempDir.resolve("work")
      Files.createDirectories(workDir)

      val toolchain = ScalaNativeToolchain.forVersion(DefaultScalaNative04Version, DefaultScala213Version)
      val result = toolchain
        .link(ScalaNativeLinkConfig.Debug, classpath, "example.Main", binaryPath, workDir, ScalaNativeToolchain.Logger.Silent, CancellationToken.never)
        .unsafeRunSync()

      assert(result.isSuccess, s"0.4.x linking failed with exit code ${result.exitCode}")
      assert(Files.exists(binaryPath), s"Binary not found at $binaryPath")
      info(s"Successfully linked 0.4.x binary: $binaryPath")
    }
  }
}
