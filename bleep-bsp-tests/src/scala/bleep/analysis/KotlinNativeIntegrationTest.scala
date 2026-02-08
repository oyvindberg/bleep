package bleep.analysis

import bleep.bsp.Outcome
import bleep.bsp.Outcome.{KillReason, RunOutcome}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import cats.effect.unsafe.implicits.global

/** Integration tests for Kotlin/Native compiler support.
  *
  * Tests compiling and running Kotlin/Native projects with different configurations.
  */
class KotlinNativeIntegrationTest extends AnyFunSuite with Matchers {

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

  test("CompilerResolver: resolves Kotlin/Native compiler embeddable for 2.3.0") {
    val jars = CompilerResolver.resolveKotlinNativeCompilerEmbeddable(bleep.model.VersionKotlin("2.3.0"))
    jars should not be empty
    info(s"Resolved ${jars.size} JARs for Kotlin/Native 2.3.0")
  }

  test("CompilerResolver: resolves Kotlin/Native compiler embeddable for 2.0.0") {
    val jars = CompilerResolver.resolveKotlinNativeCompilerEmbeddable(bleep.model.VersionKotlin("2.0.0"))
    jars should not be empty
    info(s"Resolved ${jars.size} JARs for Kotlin/Native 2.0.0")
  }

  test("CompilerResolver: creates Kotlin/Native compiler instance") {
    val instance = CompilerResolver.getKotlinNativeCompiler("2.3.0")
    instance.allJars should not be empty
    instance.language shouldBe "kotlin-native"
    instance.version shouldBe "2.3.0"
    info(s"Created Kotlin/Native compiler instance with ${instance.allJars.size} JARs")
  }

  test("CompilerResolver: different Kotlin/Native versions have different classloaders") {
    val instance230 = CompilerResolver.getKotlinNativeCompiler("2.3.0")
    val instance200 = CompilerResolver.getKotlinNativeCompiler("2.0.0")

    instance230.loader should not be instance200.loader
    info("Different Kotlin/Native versions have isolated classloaders")
  }

  test("CompilerResolver: caches Kotlin/Native compiler instances") {
    val instance1 = CompilerResolver.getKotlinNativeCompiler("2.3.0")
    val instance2 = CompilerResolver.getKotlinNativeCompiler("2.3.0")

    instance1.loader shouldBe instance2.loader
    info("Kotlin/Native compiler instances are cached correctly")
  }

  // ============================================================================
  // Configuration Tests
  // ============================================================================

  test("KotlinNativeCompilerConfig: default configuration") {
    val config = KotlinNativeCompilerConfig.Default
    config.kotlinVersion shouldBe "2.3.0"
    config.outputKind shouldBe KotlinNativeCompilerConfig.OutputKind.Executable
    config.debuggable shouldBe true
    config.optimized shouldBe false
  }

  test("KotlinNativeCompilerConfig: host target detection") {
    val hostTarget = KotlinNativeCompilerConfig.Target.hostTarget
    val os = System.getProperty("os.name").toLowerCase
    val arch = System.getProperty("os.arch").toLowerCase

    info(s"Detected host target: $hostTarget (OS: $os, arch: $arch)")

    if (os.contains("mac")) {
      if (arch == "aarch64") {
        hostTarget shouldBe KotlinNativeCompilerConfig.Target.MacosArm64
      } else {
        hostTarget shouldBe KotlinNativeCompilerConfig.Target.MacosX64
      }
    } else if (os.contains("linux")) {
      if (arch == "aarch64") {
        hostTarget shouldBe KotlinNativeCompilerConfig.Target.LinuxArm64
      } else {
        hostTarget shouldBe KotlinNativeCompilerConfig.Target.LinuxX64
      }
    } else if (os.contains("windows")) {
      hostTarget shouldBe KotlinNativeCompilerConfig.Target.MingwX64
    }
  }

  test("KotlinNativeCompilerConfig: target konan names") {
    KotlinNativeCompilerConfig.Target.MacosX64.konanName shouldBe "macos_x64"
    KotlinNativeCompilerConfig.Target.MacosArm64.konanName shouldBe "macos_arm64"
    KotlinNativeCompilerConfig.Target.LinuxX64.konanName shouldBe "linux_x64"
    KotlinNativeCompilerConfig.Target.LinuxArm64.konanName shouldBe "linux_arm64"
    KotlinNativeCompilerConfig.Target.MingwX64.konanName shouldBe "mingw_x64"
    KotlinNativeCompilerConfig.Target.IosArm64.konanName shouldBe "ios_arm64"
    KotlinNativeCompilerConfig.Target.IosSimulatorArm64.konanName shouldBe "ios_simulator_arm64"
  }

  test("KotlinNativeCompilerConfig: output kinds") {
    KotlinNativeCompilerConfig.OutputKind.Executable.produce shouldBe "program"
    KotlinNativeCompilerConfig.OutputKind.Klib.produce shouldBe "library"
    KotlinNativeCompilerConfig.OutputKind.StaticLibrary.produce shouldBe "static"
    KotlinNativeCompilerConfig.OutputKind.DynamicLibrary.produce shouldBe "dynamic"
    KotlinNativeCompilerConfig.OutputKind.Framework.produce shouldBe "framework"
  }

  // ============================================================================
  // KotlinNativeCompileResult Tests
  // ============================================================================

  test("KotlinNativeCompileResult: isSuccess when exitCode is 0") {
    val result = KotlinNativeCompileResult(
      outputPath = Path.of("app"),
      exitCode = 0
    )
    result.isSuccess shouldBe true
  }

  test("KotlinNativeCompileResult: not success when exitCode is non-zero") {
    val result = KotlinNativeCompileResult(
      outputPath = Path.of("app"),
      exitCode = 1
    )
    result.isSuccess shouldBe false
  }

  // ============================================================================
  // KotlinNativeRunner Tests
  // ============================================================================

  test("KotlinNativeRunner: isExecutable returns false for non-existent path") {
    KotlinNativeRunner.isExecutable(Path.of("/non/existent/path")) shouldBe false
  }

  test("KotlinNativeRunner: can run simple binary") {
    val tempDir = createTempDir("kotlin-native-runner-test")
    try {
      // Create a simple script to test the runner
      val script = if (ScalaNativeRunner.isWindows) {
        val scriptFile = tempDir.resolve("test.bat")
        Files.writeString(scriptFile, "@echo Hello from test script")
        scriptFile
      } else {
        val scriptFile = tempDir.resolve("test.sh")
        Files.writeString(scriptFile, "#!/bin/bash\necho 'Hello from test script'")
        import java.nio.file.attribute.PosixFilePermissions
        Files.setPosixFilePermissions(scriptFile, PosixFilePermissions.fromString("rwxr-xr-x"))
        scriptFile
      }

      if (KotlinNativeRunner.isExecutable(script)) {
        val result = (for {
          killSignal <- Outcome.neverKillSignal
          outcome <- KotlinNativeRunner.run(
            binary = script,
            args = Seq.empty,
            workingDir = tempDir,
            env = Map.empty,
            killSignal = killSignal
          )
        } yield outcome).unsafeRunSync()

        result match {
          case RunOutcome.Completed(exitCode, stdout, _) =>
            exitCode shouldBe 0
            stdout should include("Hello from test script")
            info("KotlinNativeRunner executed script successfully")
          case other =>
            fail(s"Unexpected outcome: $other")
        }
      }
    } finally deleteRecursively(tempDir)
  }
}

/** Advanced Kotlin/Native integration tests.
  *
  * These tests require the Kotlin/Native toolchain (konanc) to be installed.
  */
class KotlinNativeAdvancedIntegrationTest extends AnyFunSuite with Matchers with PlatformTestHelper {

  test("KotlinNativeCompiler: compile simple Kotlin file to native binary") {
    withTempDir("kt-native-exe") { tempDir =>
      val srcDir = tempDir.resolve("src")
      writeKotlinSource(srcDir, "Main.kt", "fun main() { println(\"Hello from Kotlin/Native\") }")

      val outputPath = tempDir.resolve("app" + ScalaNativeRunner.binaryExtension)
      val config = KotlinNativeCompilerConfig.Default.copy(
        outputKind = KotlinNativeCompilerConfig.OutputKind.Executable
      )

      val result = KotlinNativeCompiler
        .compile(
          sources = Seq(srcDir.resolve("Main.kt")),
          libraries = Seq.empty,
          outputPath = outputPath,
          config = config,
          diagnosticListener = DiagnosticListener.noop,
          cancellation = CancellationToken.never
        )
        .unsafeRunSync()

      assert(result.isSuccess, s"Compilation failed with exit code ${result.exitCode}")
      assert(Files.exists(result.outputPath), s"Output binary not found at ${result.outputPath}")
      info(s"Successfully compiled Kotlin/Native binary: ${result.outputPath}")
    }
  }

  test("KotlinNativeCompiler: compile to KLIB") {
    withTempDir("kt-native-klib") { tempDir =>
      val srcDir = tempDir.resolve("src")
      writeKotlinSource(srcDir, "Lib.kt", "fun greet(name: String): String = \"Hello, $name\"")

      val outputPath = tempDir.resolve("mylib.klib")
      val config = KotlinNativeCompilerConfig.Default.copy(
        outputKind = KotlinNativeCompilerConfig.OutputKind.Klib
      )

      val result = KotlinNativeCompiler
        .compile(
          sources = Seq(srcDir.resolve("Lib.kt")),
          libraries = Seq.empty,
          outputPath = outputPath,
          config = config,
          diagnosticListener = DiagnosticListener.noop,
          cancellation = CancellationToken.never
        )
        .unsafeRunSync()

      assert(result.isSuccess, s"Compilation failed with exit code ${result.exitCode}")
      assert(Files.exists(outputPath), s"KLIB not found at $outputPath")
      info(s"Successfully compiled KLIB: $outputPath")
    }
  }

  test("KotlinNativeCompiler: compile and run binary") {
    withTempDir("kt-native-run") { tempDir =>
      val srcDir = tempDir.resolve("src")
      writeKotlinSource(srcDir, "Main.kt", "fun main() { println(\"KotlinNativeOutput42\") }")

      val outputPath = tempDir.resolve("app" + ScalaNativeRunner.binaryExtension)
      val config = KotlinNativeCompilerConfig.Default.copy(
        outputKind = KotlinNativeCompilerConfig.OutputKind.Executable
      )

      val result = KotlinNativeCompiler
        .compile(
          sources = Seq(srcDir.resolve("Main.kt")),
          libraries = Seq.empty,
          outputPath = outputPath,
          config = config,
          diagnosticListener = DiagnosticListener.noop,
          cancellation = CancellationToken.never
        )
        .unsafeRunSync()

      assert(result.isSuccess, s"Compilation failed with exit code ${result.exitCode}")

      val runResult = (for {
        killSignal <- Outcome.neverKillSignal
        outcome <- KotlinNativeRunner.run(
          binary = result.outputPath,
          args = Seq.empty,
          workingDir = tempDir,
          env = Map.empty,
          killSignal = killSignal
        )
      } yield outcome).unsafeRunSync()

      runResult match {
        case RunOutcome.Completed(exitCode, stdout, _) =>
          exitCode shouldBe 0
          stdout should include("KotlinNativeOutput42")
          info(s"Successfully ran Kotlin/Native binary, stdout: ${stdout.trim}")
        case other =>
          fail(s"Unexpected outcome: $other")
      }
    }
  }
}
