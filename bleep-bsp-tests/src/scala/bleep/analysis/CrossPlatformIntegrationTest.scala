package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import cats.effect.unsafe.implicits.global

/** Cross-platform integration tests.
  *
  * Tests that verify toolchain isolation and cross-platform compilation scenarios.
  */
class CrossPlatformIntegrationTest extends AnyFunSuite with Matchers {

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
  // Toolchain Coexistence Tests
  // ============================================================================

  test("Multiple Scala.js versions can coexist") {
    val instance116 = CompilerResolver.getScalaJsLinker("1.16.0", "2.13")
    val instance117 = CompilerResolver.getScalaJsLinker("1.17.0", "2.13")
    val instance119 = CompilerResolver.getScalaJsLinker("1.19.0", "2.13")

    // All should be resolved
    instance116.allJars should not be empty
    instance117.allJars should not be empty
    instance119.allJars should not be empty

    // All should have different classloaders
    instance116.loader should not be instance117.loader
    instance117.loader should not be instance119.loader
    instance116.loader should not be instance119.loader

    info("Multiple Scala.js versions loaded in isolated classloaders")
  }

  test("Multiple Scala Native versions can coexist") {
    val instance04 = CompilerResolver.getScalaNativeTools("0.4.17", "2.13")
    val instance05 = CompilerResolver.getScalaNativeTools("0.5.6", "3")

    // All should be resolved
    instance04.allJars should not be empty
    instance05.allJars should not be empty

    // All should have different classloaders
    instance04.loader should not be instance05.loader

    info("Multiple Scala Native versions loaded in isolated classloaders")
  }

  test("Multiple Kotlin versions can coexist") {
    val instance200 = CompilerResolver.getKotlinCompiler("2.0.0")
    val instance230 = CompilerResolver.getKotlinCompiler("2.3.0")

    // All should be resolved
    instance200.allJars should not be empty
    instance230.allJars should not be empty

    // All should have different classloaders
    instance200.loader should not be instance230.loader

    info("Multiple Kotlin versions loaded in isolated classloaders")
  }

  test("Scala.js, Scala Native, and Kotlin compilers can coexist") {
    val scalaJs = CompilerResolver.getScalaJsLinker("1.19.0", "2.13")
    val scalaNative = CompilerResolver.getScalaNativeTools("0.5.6", "3")
    val kotlinJvm = CompilerResolver.getKotlinCompiler("2.3.0")
    val kotlinJs = CompilerResolver.getKotlinJsCompiler("2.3.0")
    val kotlinNative = CompilerResolver.getKotlinNativeCompiler("2.3.0")

    // All should be resolved
    scalaJs.allJars should not be empty
    scalaNative.allJars should not be empty
    kotlinJvm.allJars should not be empty
    kotlinJs.allJars should not be empty
    kotlinNative.allJars should not be empty

    // All should have different classloaders (except Kotlin variants which may share)
    scalaJs.loader should not be scalaNative.loader
    scalaJs.loader should not be kotlinJvm.loader

    info("All platform compilers loaded in isolated classloaders")
  }

  // ============================================================================
  // Toolchain Factory Tests
  // ============================================================================

  test("ScalaJsToolchain factory creates correct bridges") {
    val toolchain116 = ScalaJsToolchain.forVersion("1.16.0", "2.13.15")
    val toolchain119 = ScalaJsToolchain.forVersion("1.19.0", "3.3.3")

    toolchain116 shouldBe a[ScalaJs1Bridge]
    toolchain119 shouldBe a[ScalaJs1Bridge]
  }

  test("ScalaNativeToolchain factory creates correct bridges") {
    val toolchain04 = ScalaNativeToolchain.forVersion("0.4.17", "2.13.15")
    val toolchain05 = ScalaNativeToolchain.forVersion("0.5.6", "3.3.3")

    toolchain04 shouldBe a[ScalaNative04Bridge]
    toolchain05 shouldBe a[ScalaNative05Bridge]
  }

  // ============================================================================
  // Configuration Compatibility Tests
  // ============================================================================

  test("Scala.js module kinds map correctly") {
    ScalaJsLinkConfig.ModuleKind.NoModule.name shouldBe "NoModule"
    ScalaJsLinkConfig.ModuleKind.CommonJSModule.name shouldBe "CommonJSModule"
    ScalaJsLinkConfig.ModuleKind.ESModule.name shouldBe "ESModule"
  }

  test("Kotlin/JS module kinds map correctly") {
    KotlinJsCompilerConfig.ModuleKind.Plain.name shouldBe "plain"
    KotlinJsCompilerConfig.ModuleKind.CommonJS.name shouldBe "commonjs"
    KotlinJsCompilerConfig.ModuleKind.ESModule.name shouldBe "es"
    KotlinJsCompilerConfig.ModuleKind.UMD.name shouldBe "umd"
    KotlinJsCompilerConfig.ModuleKind.AMD.name shouldBe "amd"
  }

  test("Scala Native build modes map correctly") {
    ScalaNativeLinkConfig.NativeMode.Debug.name shouldBe "debug"
    ScalaNativeLinkConfig.NativeMode.ReleaseFast.name shouldBe "release-fast"
    ScalaNativeLinkConfig.NativeMode.ReleaseFull.name shouldBe "release-full"
    ScalaNativeLinkConfig.NativeMode.ReleaseSize.name shouldBe "release-size"
  }

  test("Kotlin/Native output kinds map correctly") {
    KotlinNativeCompilerConfig.OutputKind.Executable.produce shouldBe "program"
    KotlinNativeCompilerConfig.OutputKind.Klib.produce shouldBe "library"
    KotlinNativeCompilerConfig.OutputKind.StaticLibrary.produce shouldBe "static"
    KotlinNativeCompilerConfig.OutputKind.DynamicLibrary.produce shouldBe "dynamic"
  }

  // ============================================================================
  // Runner Compatibility Tests
  // ============================================================================

  test("All JS runners can check Node.js availability") {
    val isAvailable = ScalaJsRunner.isNodeAvailable(PlatformTestHelper.nodeBinary).unsafeRunSync()
    // Both should report the same thing
    info(s"Node.js available: $isAvailable")

    if (isAvailable) {
      val version = ScalaJsRunner.nodeVersion(PlatformTestHelper.nodeBinary).unsafeRunSync()
      version should not be empty
      info(s"Node.js version: ${version.get}")
    }
  }

  test("Native runners can detect platform") {
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

    // Kotlin Native target should match
    val kotlinTarget = KotlinNativeCompilerConfig.Target.hostTarget
    info(s"Kotlin/Native host target: $kotlinTarget")
  }

  // ============================================================================
  // Model Integration Tests
  // ============================================================================

  test("Kotlin model supports JS configuration") {
    import bleep.model.*

    val kotlinWithJs = Kotlin(
      version = Some(VersionKotlin("2.3.0")),
      options = Options.empty,
      jvmTarget = None,
      js = Some(
        KotlinJs(
          moduleKind = Some(KotlinJsModuleKind.ESModule),
          moduleName = Some("mymodule"),
          outputMode = Some(KotlinJsOutputMode.JsExecutable),
          sourceMap = Some(true),
          sourceMapPrefix = None,
          sourceMapEmbedSources = None,
          target = Some(KotlinJsTarget.Node),
          developmentMode = Some(false),
          generateDts = Some(true)
        )
      ),
      native = None
    )

    kotlinWithJs.js should not be empty
    kotlinWithJs.js.get.moduleKind shouldBe Some(KotlinJsModuleKind.ESModule)
    kotlinWithJs.isEmpty shouldBe false
  }

  test("Kotlin model supports Native configuration") {
    import bleep.model.*

    val kotlinWithNative = Kotlin(
      version = Some(VersionKotlin("2.3.0")),
      options = Options.empty,
      jvmTarget = None,
      js = None,
      native = Some(
        KotlinNative(
          target = Some(KotlinNativeTarget.MacosArm64),
          outputKind = Some(KotlinNativeOutputKind.Executable),
          debuggable = Some(true),
          optimized = Some(false),
          baseName = Some("myapp"),
          binaryOptions = None,
          linkerOpts = Some(List("-L/usr/local/lib")),
          freeCompilerArgs = None,
          embedBitcode = None,
          isStatic = None
        )
      )
    )

    kotlinWithNative.native should not be empty
    kotlinWithNative.native.get.target shouldBe Some(KotlinNativeTarget.MacosArm64)
    kotlinWithNative.isEmpty shouldBe false
  }

  test("Kotlin model SetLike operations work with JS/Native") {
    import bleep.model.*

    val kotlin1 = Kotlin(
      version = Some(VersionKotlin("2.3.0")),
      options = Options.empty,
      jvmTarget = Some("17"),
      js = Some(KotlinJs.empty.copy(moduleKind = Some(KotlinJsModuleKind.CommonJS))),
      native = None
    )

    val kotlin2 = Kotlin(
      version = Some(VersionKotlin("2.3.0")),
      options = Options.empty,
      jvmTarget = Some("21"),
      js = Some(KotlinJs.empty.copy(moduleKind = Some(KotlinJsModuleKind.ESModule))),
      native = None
    )

    val intersection = kotlin1.intersect(kotlin2)
    intersection.version shouldBe Some(VersionKotlin("2.3.0"))
    intersection.jvmTarget shouldBe None // Different values

    val union = kotlin1.union(kotlin2)
    union.version shouldBe Some(VersionKotlin("2.3.0"))
    union.jvmTarget shouldBe Some("17") // First value wins
  }
}

/** Performance tests for toolchain resolution.
  *
  * These tests verify that caching works correctly for repeated resolutions.
  */
class ToolchainCachingTest extends AnyFunSuite with Matchers {

  test("Scala.js linker resolution is cached") {
    // First resolution (may be slow due to download)
    val start1 = System.currentTimeMillis()
    val instance1 = CompilerResolver.getScalaJsLinker("1.16.0", "2.13")
    val time1 = System.currentTimeMillis() - start1

    // Second resolution (should be fast - cached)
    val start2 = System.currentTimeMillis()
    val instance2 = CompilerResolver.getScalaJsLinker("1.16.0", "2.13")
    val time2 = System.currentTimeMillis() - start2

    // Same instance
    instance1 shouldBe instance2

    // Second should be much faster (unless first was already cached)
    info(s"First resolution: ${time1}ms, Second resolution: ${time2}ms")
    if (time1 > 100) { // Only check if first was slow (not pre-cached)
      time2 should be < (time1 / 2)
    }
  }

  test("Scala Native tools resolution is cached") {
    val start1 = System.currentTimeMillis()
    val instance1 = CompilerResolver.getScalaNativeTools("0.5.6", "3")
    val time1 = System.currentTimeMillis() - start1

    val start2 = System.currentTimeMillis()
    val instance2 = CompilerResolver.getScalaNativeTools("0.5.6", "3")
    val time2 = System.currentTimeMillis() - start2

    instance1 shouldBe instance2
    info(s"First resolution: ${time1}ms, Second resolution: ${time2}ms")
  }

  test("Kotlin compiler resolution is cached") {
    val start1 = System.currentTimeMillis()
    val instance1 = CompilerResolver.getKotlinCompiler("2.3.0")
    val time1 = System.currentTimeMillis() - start1

    val start2 = System.currentTimeMillis()
    val instance2 = CompilerResolver.getKotlinCompiler("2.3.0")
    val time2 = System.currentTimeMillis() - start2

    instance1 shouldBe instance2
    info(s"First resolution: ${time1}ms, Second resolution: ${time2}ms")
  }
}
