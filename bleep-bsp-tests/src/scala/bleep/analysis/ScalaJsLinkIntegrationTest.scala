package bleep.analysis

import bleep.bsp.{LinkExecutor, Outcome, TaskDag}
import bleep.bsp.Outcome.{KillReason, RunOutcome}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

/** Integration tests for Scala.js linking.
  *
  * Tests:
  *   - Linking configurations (CommonJS, ESModule, NoModule)
  *   - Debug vs Release modes
  *   - Source maps
  *   - Module split styles
  */
class ScalaJsLinkIntegrationTest extends AnyFunSuite with Matchers {

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
  // ScalaJsLinkConfig Tests
  // ==========================================================================

  test("ScalaJsLinkConfig: Debug configuration defaults") {
    val config = ScalaJsLinkConfig.Debug

    config.mode shouldBe ScalaJsLinkConfig.LinkerMode.Debug
    config.moduleKind shouldBe ScalaJsLinkConfig.ModuleKind.CommonJSModule
    config.moduleSplitStyle shouldBe ScalaJsLinkConfig.ModuleSplitStyle.FewestModules
    config.emitSourceMaps shouldBe true
    config.minify shouldBe false
    config.prettyPrint shouldBe false
    config.optimizer shouldBe false
  }

  test("ScalaJsLinkConfig: Release configuration defaults") {
    val config = ScalaJsLinkConfig.Release

    config.mode shouldBe ScalaJsLinkConfig.LinkerMode.Release
    config.emitSourceMaps shouldBe false
    config.minify shouldBe true
    config.optimizer shouldBe true
  }

  test("ScalaJsLinkConfig: ESModule configuration") {
    val config = ScalaJsLinkConfig.Debug.copy(
      moduleKind = ScalaJsLinkConfig.ModuleKind.ESModule
    )

    config.moduleKind shouldBe ScalaJsLinkConfig.ModuleKind.ESModule
  }

  test("ScalaJsLinkConfig: NoModule configuration") {
    val config = ScalaJsLinkConfig.Debug.copy(
      moduleKind = ScalaJsLinkConfig.ModuleKind.NoModule
    )

    config.moduleKind shouldBe ScalaJsLinkConfig.ModuleKind.NoModule
  }

  test("ScalaJsLinkConfig: SmallModulesFor split style") {
    val config = ScalaJsLinkConfig.Debug.copy(
      moduleSplitStyle = ScalaJsLinkConfig.ModuleSplitStyle.SmallModulesFor(List("com.example"))
    )

    config.moduleSplitStyle match {
      case ScalaJsLinkConfig.ModuleSplitStyle.SmallModulesFor(packages) =>
        packages shouldBe List("com.example")
      case other =>
        fail(s"Expected SmallModulesFor, got $other")
    }
  }

  // ==========================================================================
  // ScalaJsToolchain Factory Tests
  // ==========================================================================

  test("ScalaJsToolchain.forVersion: creates toolchain for 1.16.x") {
    val toolchain = ScalaJsToolchain.forVersion("1.16.0", "3.3.3")
    toolchain shouldBe a[ScalaJs1Bridge]
  }

  test("ScalaJsToolchain.forVersion: creates toolchain for 1.19.x") {
    val toolchain = ScalaJsToolchain.forVersion("1.19.0", "3.3.3")
    toolchain shouldBe a[ScalaJs1Bridge]
  }

  test("ScalaJsToolchain.forVersion: rejects unsupported versions") {
    assertThrows[IllegalArgumentException] {
      ScalaJsToolchain.forVersion("0.6.33", "2.13.15")
    }
  }

  // ==========================================================================
  // LinkPlatform Tests
  // ==========================================================================

  test("LinkPlatform.ScalaJs: stores version and config") {
    val config = ScalaJsLinkConfig.Debug
    val platform = TaskDag.LinkPlatform.ScalaJs("1.16.0", "3.3.3", config)

    platform.version shouldBe "1.16.0"
    platform.scalaVersion shouldBe "3.3.3"
    platform.config shouldBe config
  }

  // ==========================================================================
  // LinkResult Tests
  // ==========================================================================

  test("LinkResult.JsSuccess: stores output paths") {
    val mainModule = Path.of("out/main.js")
    val sourceMap = Some(Path.of("out/main.js.map"))
    val allFiles = Seq(mainModule, Path.of("out/main.js.map"))

    val result = TaskDag.LinkResult.JsSuccess(mainModule, sourceMap, allFiles, wasUpToDate = false)

    result.mainModule shouldBe mainModule
    result.sourceMap shouldBe sourceMap
    result.allFiles shouldBe allFiles
    result.wasUpToDate shouldBe false
  }

  // ==========================================================================
  // LinkExecutor Tests (Unit level)
  // ==========================================================================

  test("LinkExecutor: JVM platform returns NotApplicable") {
    val linkTask = TaskDag.LinkTask(
      project = bleep.model.CrossProjectName(bleep.model.ProjectName("app-jvm"), None),
      platform = TaskDag.LinkPlatform.Jvm,
      releaseMode = false,
      isTest = false
    )

    val result = (for {
      killSignal <- Outcome.neverKillSignal
      outcome <- LinkExecutor.execute(
        linkTask,
        classpath = Seq.empty,
        mainClass = None,
        baseOutputDir = Path.of("."),
        logger = LinkExecutor.LinkLogger.Silent,
        killSignal = killSignal
      )
    } yield outcome).unsafeRunSync()

    result._1 shouldBe TaskDag.TaskResult.Success
    result._2 shouldBe TaskDag.LinkResult.NotApplicable
  }

  // ==========================================================================
  // ScalaJsRunner Tests
  // ==========================================================================

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

  test("ScalaJsRunner: run simple CommonJS module") {
    val tempDir = createTempDir("scalajs-run-test")
    try {
      // Create a simple JS file
      val jsFile = tempDir.resolve("test.js")
      Files.writeString(
        jsFile,
        """
        |console.log("Hello from Scala.js!");
        |process.exit(0);
        |""".stripMargin
      )

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        outcome <- ScalaJsRunner.run(
          jsFile,
          args = Seq.empty,
          moduleKind = ScalaJsLinkConfig.ModuleKind.CommonJSModule,
          workingDir = tempDir,
          env = Map.empty,
          nodeBinary = PlatformTestHelper.nodeBinary,
          killSignal = killSignal
        )
      } yield outcome).unsafeRunSync()

      result match {
        case RunOutcome.Completed(exitCode, stdout, _) =>
          exitCode shouldBe 0
          stdout should include("Hello from Scala.js!")
        case other =>
          fail(s"Unexpected outcome: $other")
      }
    } finally deleteRecursively(tempDir)
  }

  // ==========================================================================
  // CompilerResolver Tests for Scala.js
  // ==========================================================================

  test("CompilerResolver: resolves Scala.js linker JARs") {
    val jars = CompilerResolver.resolveScalaJsLinker("1.16.0", "2.13")
    jars should not be empty
    info(s"Resolved ${jars.size} JARs for Scala.js 1.16.0")
    jars.exists(_.getFileName.toString.contains("scalajs-linker")) shouldBe true
  }

  test("CompilerResolver: caches Scala.js linker instances") {
    val instance1 = CompilerResolver.getScalaJsLinker("1.16.0", "2.13")
    val instance2 = CompilerResolver.getScalaJsLinker("1.16.0", "2.13")

    instance1.loader shouldBe instance2.loader
    info("Scala.js linker instances are cached correctly")
  }

  test("CompilerResolver: different Scala.js versions have different classloaders") {
    val instance116 = CompilerResolver.getScalaJsLinker("1.16.0", "2.13")
    val instance117 = CompilerResolver.getScalaJsLinker("1.17.0", "2.13")

    instance116.loader should not be instance117.loader
    info("Different Scala.js versions have isolated classloaders")
  }
}

/** Advanced Scala.js linking tests that compile and link actual .sjsir files.
  */
class ScalaJsAdvancedLinkIntegrationTest extends AnyFunSuite with Matchers with PlatformTestHelper {

  private val scalaSource =
    """package example
      |
      |object Main {
      |  def main(args: Array[String]): Unit = {
      |    println("LinkedOutput")
      |  }
      |}
      |""".stripMargin

  private def compileAndGetClasspath(tempDir: Path): Seq[Path] = {
    val srcDir = tempDir.resolve("src")
    writeScalaSource(srcDir, "example", "Main.scala", scalaSource)
    val outDir = tempDir.resolve("classes")
    compileForScalaJs(srcDir, outDir, DefaultScalaVersion, DefaultScalaJsVersion)
  }

  test("ScalaJs1Bridge: link project with CommonJS output") {
    withTempDir("sjs-link-cjs") { tempDir =>
      val classpath = compileAndGetClasspath(tempDir)
      val linkDir = tempDir.resolve("linked")
      Files.createDirectories(linkDir)

      val toolchain = ScalaJsToolchain.forVersion(DefaultScalaJsVersion, DefaultScalaVersion)
      val config = ScalaJsLinkConfig.Debug.copy(moduleKind = ScalaJsLinkConfig.ModuleKind.CommonJSModule)

      val result = toolchain
        .link(config, classpath, Some("example.Main"), linkDir, "main", ScalaJsToolchain.Logger.Silent, CancellationToken.never)
        .unsafeRunSync()

      assert(result.isSuccess, "CommonJS linking failed")
      result.outputFiles.exists(_.getFileName.toString.endsWith(".js")) shouldBe true
      info(s"CommonJS output: ${result.outputFiles.map(_.getFileName)}")
    }
  }

  test("ScalaJs1Bridge: link project with ESModule output") {
    withTempDir("sjs-link-esm") { tempDir =>
      val classpath = compileAndGetClasspath(tempDir)
      val linkDir = tempDir.resolve("linked")
      Files.createDirectories(linkDir)

      val toolchain = ScalaJsToolchain.forVersion(DefaultScalaJsVersion, DefaultScalaVersion)
      val config = ScalaJsLinkConfig.Debug.copy(moduleKind = ScalaJsLinkConfig.ModuleKind.ESModule)

      val result = toolchain
        .link(config, classpath, Some("example.Main"), linkDir, "main", ScalaJsToolchain.Logger.Silent, CancellationToken.never)
        .unsafeRunSync()

      assert(result.isSuccess, "ESModule linking failed")
      result.outputFiles.nonEmpty shouldBe true
      info(s"ESModule output: ${result.outputFiles.map(_.getFileName)}")
    }
  }

  test("ScalaJs1Bridge: link project with source maps") {
    withTempDir("sjs-link-srcmap") { tempDir =>
      val classpath = compileAndGetClasspath(tempDir)
      val linkDir = tempDir.resolve("linked")
      Files.createDirectories(linkDir)

      val toolchain = ScalaJsToolchain.forVersion(DefaultScalaJsVersion, DefaultScalaVersion)
      val config = ScalaJsLinkConfig.Debug.copy(
        moduleKind = ScalaJsLinkConfig.ModuleKind.CommonJSModule,
        emitSourceMaps = true
      )

      val result = toolchain
        .link(config, classpath, Some("example.Main"), linkDir, "main", ScalaJsToolchain.Logger.Silent, CancellationToken.never)
        .unsafeRunSync()

      assert(result.isSuccess, "Linking with source maps failed")
      val hasSourceMap = result.outputFiles.exists(_.getFileName.toString.endsWith(".js.map"))
      info(s"Source map present: $hasSourceMap, output files: ${result.outputFiles.map(_.getFileName)}")
    }
  }

  test("ScalaJs1Bridge: link and run with Node.js") {
    withTempDir("sjs-link-run") { tempDir =>
      val classpath = compileAndGetClasspath(tempDir)
      val linkDir = tempDir.resolve("linked")
      Files.createDirectories(linkDir)

      val toolchain = ScalaJsToolchain.forVersion(DefaultScalaJsVersion, DefaultScalaVersion)
      val config = ScalaJsLinkConfig.Debug.copy(moduleKind = ScalaJsLinkConfig.ModuleKind.CommonJSModule)

      val linkResult = toolchain
        .link(config, classpath, Some("example.Main"), linkDir, "main", ScalaJsToolchain.Logger.Silent, CancellationToken.never)
        .unsafeRunSync()

      assert(linkResult.isSuccess, "Linking failed")

      val runResult = (for {
        killSignal <- Outcome.neverKillSignal
        outcome <- ScalaJsRunner.run(linkResult.mainModule, Seq.empty, ScalaJsLinkConfig.ModuleKind.CommonJSModule, linkDir, Map.empty, nodeBinary, killSignal)
      } yield outcome).unsafeRunSync()

      runResult match {
        case RunOutcome.Completed(exitCode, stdout, _) =>
          exitCode shouldBe 0
          stdout should include("LinkedOutput")
          info(s"Ran linked JS, stdout: ${stdout.trim}")
        case other =>
          fail(s"Unexpected outcome: $other")
      }
    }
  }
}
