package bleep.analysis

import bleep.bsp.Outcome
import bleep.bsp.Outcome.RunOutcome
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import cats.effect.unsafe.implicits.global

/** Integration tests for Scala.js toolchain support.
  *
  * Tests linking and running Scala.js projects with different versions and configurations.
  */
class ScalaJsIntegrationTest extends AnyFunSuite with Matchers {

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

  test("CompilerResolver: resolves Scala.js linker JARs for 1.16.0") {
    val jars = CompilerResolver.resolveScalaJsLinker("1.16.0", "2.13")
    jars should not be empty
    info(s"Resolved ${jars.size} JARs for Scala.js 1.16.0")
    jars.exists(_.getFileName.toString.contains("scalajs-linker")) shouldBe true
  }

  test("CompilerResolver: resolves Scala.js linker JARs for 1.19.0") {
    val jars = CompilerResolver.resolveScalaJsLinker("1.19.0", "2.13")
    jars should not be empty
    info(s"Resolved ${jars.size} JARs for Scala.js 1.19.0")
    jars.exists(_.getFileName.toString.contains("scalajs-linker")) shouldBe true
  }

  test("CompilerResolver: different Scala.js versions have different classloaders") {
    val instance116 = CompilerResolver.getScalaJsLinker("1.16.0", "2.13")
    val instance119 = CompilerResolver.getScalaJsLinker("1.19.0", "2.13")

    instance116.loader should not be instance119.loader
    info("Different Scala.js versions have isolated classloaders")
  }

  test("CompilerResolver: caches Scala.js linker instances") {
    val instance1 = CompilerResolver.getScalaJsLinker("1.16.0", "2.13")
    val instance2 = CompilerResolver.getScalaJsLinker("1.16.0", "2.13")

    instance1.loader shouldBe instance2.loader
    info("Scala.js linker instances are cached correctly")
  }

  // ============================================================================
  // ScalaJsToolchain Factory Tests
  // ============================================================================

  test("ScalaJsToolchain.forVersion: creates toolchain for 1.x") {
    val toolchain = ScalaJsToolchain.forVersion("1.16.0", "2.13.15")
    toolchain shouldBe a[ScalaJs1Bridge]
  }

  test("ScalaJsToolchain.forVersion: rejects unsupported versions") {
    assertThrows[IllegalArgumentException] {
      ScalaJsToolchain.forVersion("0.6.33", "2.13.15")
    }
  }

  // ============================================================================
  // ScalaJsRunner Tests
  // ============================================================================

  test("ScalaJsRunner: check Node.js availability") {
    val isAvailable = ScalaJsRunner.isNodeAvailable(PlatformTestHelper.nodeBinary).unsafeRunSync()
    if (isAvailable) {
      info("Node.js is available")
      val version = ScalaJsRunner.nodeVersion(PlatformTestHelper.nodeBinary).unsafeRunSync()
      version.foreach(v => info(s"Node.js version: $v"))
    } else {
      info("Node.js is not available, skipping Node.js-dependent tests")
    }
  }

  // ============================================================================
  // Configuration Tests
  // ============================================================================

  test("ScalaJsLinkConfig: default debug configuration") {
    val config = ScalaJsLinkConfig.Debug
    config.mode shouldBe ScalaJsLinkConfig.LinkerMode.Debug
    config.emitSourceMaps shouldBe true
    config.optimizer shouldBe false
    config.minify shouldBe false
  }

  test("ScalaJsLinkConfig: default release configuration") {
    val config = ScalaJsLinkConfig.Release
    config.mode shouldBe ScalaJsLinkConfig.LinkerMode.Release
    config.emitSourceMaps shouldBe false
    config.optimizer shouldBe true
    config.minify shouldBe true
  }

  test("ScalaJsLinkConfig: module kinds") {
    ScalaJsLinkConfig.ModuleKind.NoModule.name shouldBe "NoModule"
    ScalaJsLinkConfig.ModuleKind.CommonJSModule.name shouldBe "CommonJSModule"
    ScalaJsLinkConfig.ModuleKind.ESModule.name shouldBe "ESModule"
  }

  test("ScalaJsLinkConfig: module split styles") {
    ScalaJsLinkConfig.ModuleSplitStyle.FewestModules.name shouldBe "FewestModules"
    ScalaJsLinkConfig.ModuleSplitStyle.SmallestModules.name shouldBe "SmallestModules"
    ScalaJsLinkConfig.ModuleSplitStyle.SmallModulesFor(List("com.example")).name shouldBe "SmallModulesFor"
  }

  test("ScalaJsLinkConfig: ES features") {
    val defaults = ScalaJsLinkConfig.EsFeatures.Defaults
    defaults.esVersion shouldBe ScalaJsLinkConfig.EsVersion.ES2015
    defaults.useECMAScript2015Semantics shouldBe true
  }

  // ============================================================================
  // ScalaJsLinkResult Tests
  // ============================================================================

  test("ScalaJsLinkResult: isSuccess when outputFiles is non-empty") {
    val result = ScalaJsLinkResult(
      outputFiles = Seq(Path.of("main.js")),
      mainModule = Path.of("main.js"),
      publicModules = Seq(Path.of("main.js"))
    )
    result.isSuccess shouldBe true
  }

  test("ScalaJsLinkResult: not success when outputFiles is empty") {
    val result = ScalaJsLinkResult(
      outputFiles = Seq.empty,
      mainModule = Path.of("main.js"),
      publicModules = Seq.empty
    )
    result.isSuccess shouldBe false
  }
}

/** Advanced Scala.js integration tests that require Scala.js libraries.
  *
  * These tests verify actual linking behavior by compiling Scala sources to .sjsir and then linking.
  */
class ScalaJsAdvancedIntegrationTest extends AnyFunSuite with Matchers with PlatformTestHelper {

  private val scalaSource =
    """package example
      |
      |object Main {
      |  def main(args: Array[String]): Unit = {
      |    println("HelloFromScalaJs")
      |  }
      |}
      |""".stripMargin

  test("ScalaJs1Bridge: link simple project with CommonJS") {
    withTempDir("sjs-cjs") { tempDir =>
      val srcDir = tempDir.resolve("src")
      writeScalaSource(srcDir, "example", "Main.scala", scalaSource)
      val outDir = tempDir.resolve("classes")
      val classpath = compileForScalaJs(srcDir, outDir, DefaultScalaVersion, DefaultScalaJsVersion)

      val linkDir = tempDir.resolve("linked")
      Files.createDirectories(linkDir)

      val toolchain = ScalaJsToolchain.forVersion(DefaultScalaJsVersion, DefaultScalaVersion)
      val config = ScalaJsLinkConfig.Debug.copy(moduleKind = ScalaJsLinkConfig.ModuleKind.CommonJSModule)

      val result = toolchain
        .link(config, classpath, Some("example.Main"), linkDir, "main", ScalaJsToolchain.Logger.Silent, CancellationToken.never)
        .unsafeRunSync()

      assert(result.isSuccess, "Linking failed")
      assert(result.outputFiles.nonEmpty, "No output files")
      info(s"Linked ${result.outputFiles.size} files with CommonJS")
    }
  }

  test("ScalaJs1Bridge: link simple project with ESModule") {
    withTempDir("sjs-esm") { tempDir =>
      val srcDir = tempDir.resolve("src")
      writeScalaSource(srcDir, "example", "Main.scala", scalaSource)
      val outDir = tempDir.resolve("classes")
      val classpath = compileForScalaJs(srcDir, outDir, DefaultScalaVersion, DefaultScalaJsVersion)

      val linkDir = tempDir.resolve("linked")
      Files.createDirectories(linkDir)

      val toolchain = ScalaJsToolchain.forVersion(DefaultScalaJsVersion, DefaultScalaVersion)
      val config = ScalaJsLinkConfig.Debug.copy(moduleKind = ScalaJsLinkConfig.ModuleKind.ESModule)

      val result = toolchain
        .link(config, classpath, Some("example.Main"), linkDir, "main", ScalaJsToolchain.Logger.Silent, CancellationToken.never)
        .unsafeRunSync()

      assert(result.isSuccess, "Linking failed")
      assert(result.outputFiles.nonEmpty, "No output files")
      info(s"Linked ${result.outputFiles.size} files with ESModule")
    }
  }

  test("ScalaJs1Bridge: link and run with Node.js") {
    withTempDir("sjs-run") { tempDir =>
      val srcDir = tempDir.resolve("src")
      writeScalaSource(srcDir, "example", "Main.scala", scalaSource)
      val outDir = tempDir.resolve("classes")
      val classpath = compileForScalaJs(srcDir, outDir, DefaultScalaVersion, DefaultScalaJsVersion)

      val linkDir = tempDir.resolve("linked")
      Files.createDirectories(linkDir)

      val toolchain = ScalaJsToolchain.forVersion(DefaultScalaJsVersion, DefaultScalaVersion)
      val config = ScalaJsLinkConfig.Debug.copy(moduleKind = ScalaJsLinkConfig.ModuleKind.CommonJSModule)

      val linkResult = toolchain
        .link(config, classpath, Some("example.Main"), linkDir, "main", ScalaJsToolchain.Logger.Silent, CancellationToken.never)
        .unsafeRunSync()

      assert(linkResult.isSuccess, "Linking failed")

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        outcome <- ScalaJsRunner.run(linkResult.mainModule, Seq.empty, ScalaJsLinkConfig.ModuleKind.CommonJSModule, linkDir, Map.empty, nodeBinary, killSignal)
      } yield outcome).unsafeRunSync()

      result match {
        case RunOutcome.Completed(exitCode, stdout, _) =>
          exitCode shouldBe 0
          stdout should include("HelloFromScalaJs")
          info(s"Successfully ran Scala.js output: ${stdout.trim}")
        case other =>
          fail(s"Expected Completed, got: $other")
      }
    }
  }
}
