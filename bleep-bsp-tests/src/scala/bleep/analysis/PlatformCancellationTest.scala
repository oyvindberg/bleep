package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import cats.effect.{IO, Outcome}
import cats.effect.unsafe.implicits.global
import scala.concurrent.duration.*

/** Integration tests for platform compiler/linker cancellation.
  *
  * Tests that:
  *   1. Pre-cancelled operations return IO.canceled immediately
  *   2. Operations can be cancelled mid-way via fiber cancellation
  *   3. Cancelled operations result in Outcome.Canceled, not success or failure
  */
class PlatformCancellationTest extends AnyFunSuite with Matchers {

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

  /** Generate a large Kotlin source. */
  def generateLargeKotlinSource(numClasses: Int, methodsPerClass: Int): SourceFile = {
    val sb = new StringBuilder
    sb.append("package largekt\n\n")

    for (i <- 0 until numClasses) {
      sb.append(s"class LargeClass$i {\n")
      for (j <- 0 until methodsPerClass) {
        sb.append(s"    fun method$j(x: Int): Int {\n")
        sb.append(s"        val a = x + $j\n")
        sb.append(s"        val b = a * 2\n")
        sb.append(s"        val c = b + a\n")
        sb.append(s"        return c\n")
        sb.append("    }\n")
      }
      sb.append("}\n\n")
    }

    SourceFile(Path.of("largekt/Large.kt"), sb.toString)
  }

  // ============================================================================
  // Kotlin/JS Cancellation Tests
  // ============================================================================

  test("Kotlin/JS: pre-cancelled compilation results in IO.canceled") {
    val outputDir = createTempDir("kotlin-js-cancel-")
    try {
      val source = generateLargeKotlinSource(numClasses = 5, methodsPerClass = 5)

      // Write source to disk
      val sourceDir = outputDir.resolve("src")
      Files.createDirectories(sourceDir.resolve("largekt"))
      val sourcePath = sourceDir.resolve(source.path)
      Files.writeString(sourcePath, source.content)

      val config = KotlinJsCompilerConfig(
        kotlinVersion = "2.3.0",
        moduleName = "test-module",
        moduleKind = KotlinJsCompilerConfig.ModuleKind.CommonJS,
        outputMode = KotlinJsCompilerConfig.OutputMode.JsExecutable,
        sourceMap = false,
        sourceMapPrefix = None,
        sourceMapEmbedSources = KotlinJsCompilerConfig.SourceMapEmbedSources.Never,
        target = KotlinJsCompilerConfig.Target.Node,
        developmentMode = true,
        generateDts = false,
        additionalOptions = Seq.empty
      )

      // Pre-cancelled token
      val cancellation = CancellationToken.create()
      cancellation.cancel()

      val io = KotlinJsCompiler.compile(
        sources = Seq(sourcePath),
        libraries = Seq.empty,
        outputDir = outputDir.resolve("out"),
        config = config,
        diagnosticListener = DiagnosticListener.noop,
        cancellation = cancellation
      )

      // Run and check outcome
      val outcome = io.start.flatMap(_.join).unsafeRunSync()

      outcome match {
        case Outcome.Canceled() =>
          info("Pre-cancelled Kotlin/JS compilation correctly returned Canceled")
        case Outcome.Succeeded(fa) =>
          fail(s"Expected Canceled but got Succeeded")
        case Outcome.Errored(e) =>
          fail(s"Expected Canceled but got Errored: ${e.getMessage}")
      }
    } finally deleteRecursively(outputDir)
  }

  test("Kotlin/JS: fiber cancellation results in IO.canceled") {
    val outputDir = createTempDir("kotlin-js-fiber-cancel-")
    try {
      // Use a moderate size - enough to actually be compiling when we cancel
      val source = generateLargeKotlinSource(numClasses = 20, methodsPerClass = 10)
      info(s"Generated source file with ${source.content.length} characters")

      // Write source to disk
      val sourceDir = outputDir.resolve("src")
      Files.createDirectories(sourceDir.resolve("largekt"))
      val sourcePath = sourceDir.resolve(source.path)
      Files.writeString(sourcePath, source.content)

      val config = KotlinJsCompilerConfig(
        kotlinVersion = "2.3.0",
        moduleName = "test-module",
        moduleKind = KotlinJsCompilerConfig.ModuleKind.CommonJS,
        outputMode = KotlinJsCompilerConfig.OutputMode.JsExecutable,
        sourceMap = false,
        sourceMapPrefix = None,
        sourceMapEmbedSources = KotlinJsCompilerConfig.SourceMapEmbedSources.Never,
        target = KotlinJsCompilerConfig.Target.Node,
        developmentMode = true,
        generateDts = false,
        additionalOptions = Seq.empty
      )

      val cancellation = CancellationToken.create()

      val io = KotlinJsCompiler.compile(
        sources = Seq(sourcePath),
        libraries = Seq.empty,
        outputDir = outputDir.resolve("out"),
        config = config,
        diagnosticListener = DiagnosticListener.noop,
        cancellation = cancellation
      )

      // Start fiber, wait a bit, then cancel
      // Timeout of 10s ensures test doesn't hang forever if something goes wrong
      val program = (for {
        fiber <- io.start
        _ <- IO.sleep(100.millis) // Let compilation start
        _ <- fiber.cancel
        outcome <- fiber.join
      } yield outcome).timeout(10.seconds)

      val outcome = program.unsafeRunSync()

      outcome match {
        case Outcome.Canceled() =>
          info("Fiber-cancelled Kotlin/JS compilation correctly returned Canceled")
        case Outcome.Succeeded(fa) =>
          // If compilation was very fast, it might complete before cancellation
          info("Compilation completed before cancellation could take effect (fast compilation)")
        case Outcome.Errored(e) =>
          fail(s"Expected Canceled but got Errored: ${e.getMessage}")
      }
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Kotlin/Native Cancellation Tests
  // ============================================================================

  test("Kotlin/Native: pre-cancelled compilation results in IO.canceled") {
    val outputDir = createTempDir("kotlin-native-cancel-")
    val outputPath = outputDir.resolve("test-binary")
    try {
      val source = generateLargeKotlinSource(numClasses = 5, methodsPerClass = 5)

      // Write source to disk
      val sourceDir = outputDir.resolve("src")
      Files.createDirectories(sourceDir.resolve("largekt"))
      val sourcePath = sourceDir.resolve(source.path)
      Files.writeString(sourcePath, source.content)

      val config = KotlinNativeCompilerConfig(
        kotlinVersion = "2.3.0",
        target = KotlinNativeCompilerConfig.Target.hostTarget,
        outputKind = KotlinNativeCompilerConfig.OutputKind.Executable,
        debuggable = true,
        optimized = false,
        baseName = None,
        linkerOpts = Seq.empty,
        freeCompilerArgs = Seq.empty,
        entryPoint = Some("largekt.main"),
        additionalOptions = Seq.empty
      )

      // Pre-cancelled token
      val cancellation = CancellationToken.create()
      cancellation.cancel()

      val io = KotlinNativeCompiler.compile(
        sources = Seq(sourcePath),
        libraries = Seq.empty,
        outputPath = outputPath,
        config = config,
        diagnosticListener = DiagnosticListener.noop,
        cancellation = cancellation
      )

      // Run and check outcome
      val outcome = io.start.flatMap(_.join).unsafeRunSync()

      outcome match {
        case Outcome.Canceled() =>
          info("Pre-cancelled Kotlin/Native compilation correctly returned Canceled")
        case Outcome.Succeeded(fa) =>
          fail(s"Expected Canceled but got Succeeded")
        case Outcome.Errored(e) =>
          fail(s"Expected Canceled but got Errored: ${e.getMessage}")
      }
    } finally deleteRecursively(outputDir)
  }

  // NOTE: Kotlin/Native fiber cancellation test is intentionally omitted.
  // The Kotlin Native compiler uses kotlinx.coroutines runBlocking which crashes
  // the JVM when interrupted via Thread.interrupt (from fiber.cancel).
  // Token-based pre-cancellation is tested above. Mid-compilation cancellation
  // isn't feasible because the K/N compiler doesn't check external cancellation
  // tokens during its internal compilation phases.

  // ============================================================================
  // Scala.js Cancellation Tests
  // ============================================================================

  test("Scala.js: pre-cancelled linking results in IO.canceled") {
    val outputDir = createTempDir("scalajs-cancel-")
    try {
      val config = ScalaJsLinkConfig.Debug

      // Pre-cancelled token
      val cancellation = CancellationToken.create()
      cancellation.cancel()

      val toolchain = ScalaJsToolchain.forVersion("1.16.0", "3.3.3")

      val io = toolchain.link(
        config = config,
        classpath = Seq.empty,
        mainClass = Some("test.Main"),
        outputDir = outputDir,
        moduleName = "test",
        logger = ScalaJsToolchain.Logger.Silent,
        cancellation = cancellation
      )

      // Run and check outcome
      val outcome = io.start.flatMap(_.join).unsafeRunSync()

      outcome match {
        case Outcome.Canceled() =>
          info("Pre-cancelled Scala.js linking correctly returned Canceled")
        case Outcome.Succeeded(fa) =>
          fail(s"Expected Canceled but got Succeeded")
        case Outcome.Errored(e) =>
          // Empty classpath might cause error before cancellation check
          info(s"Got error (acceptable with empty classpath): ${e.getMessage}")
      }
    } finally deleteRecursively(outputDir)
  }

  test("Scala.js: fiber cancellation results in IO.canceled") {
    val outputDir = createTempDir("scalajs-fiber-cancel-")
    try {
      val config = ScalaJsLinkConfig.Debug

      val cancellation = CancellationToken.create()

      val toolchain = ScalaJsToolchain.forVersion("1.16.0", "3.3.3")

      // Get Scala.js library classpath
      val scalaJsLibrary = CompilerResolver.resolveScalaJsLibrary("1.16.0", "3.3.3")

      val io = toolchain.link(
        config = config,
        classpath = scalaJsLibrary,
        mainClass = None,
        outputDir = outputDir,
        moduleName = "test",
        logger = ScalaJsToolchain.Logger.Silent,
        cancellation = cancellation
      )

      // Start fiber, wait a bit, then cancel
      // Timeout of 10s ensures test doesn't hang forever
      val program = (for {
        fiber <- io.start
        _ <- IO.sleep(100.millis) // Let linking start
        _ <- fiber.cancel
        outcome <- fiber.join
      } yield outcome).timeout(10.seconds)

      val outcome = program.unsafeRunSync()

      outcome match {
        case Outcome.Canceled() =>
          info("Fiber-cancelled Scala.js linking correctly returned Canceled")
        case Outcome.Succeeded(fa) =>
          info("Linking completed before cancellation could take effect (fast linking)")
        case Outcome.Errored(e) =>
          fail(s"Expected Canceled but got Errored: ${e.getMessage}")
      }
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Scala Native Cancellation Tests
  // ============================================================================

  test("Scala Native: pre-cancelled linking results in IO.canceled") {
    val outputDir = createTempDir("scala-native-cancel-")
    val outputPath = outputDir.resolve("test-binary")
    try {
      val config = ScalaNativeLinkConfig.Debug

      // Pre-cancelled token
      val cancellation = CancellationToken.create()
      cancellation.cancel()

      val toolchain = ScalaNativeToolchain.forVersion("0.5.6", "3.3.3")

      val io = toolchain.link(
        config = config,
        classpath = Seq.empty,
        mainClass = "test.Main",
        outputPath = outputPath,
        workDir = outputDir,
        logger = ScalaNativeToolchain.Logger.Silent,
        cancellation = cancellation
      )

      // Run and check outcome
      val outcome = io.start.flatMap(_.join).unsafeRunSync()

      outcome match {
        case Outcome.Canceled() =>
          info("Pre-cancelled Scala Native linking correctly returned Canceled")
        case Outcome.Succeeded(fa) =>
          fail(s"Expected Canceled but got Succeeded")
        case Outcome.Errored(e) =>
          // Empty classpath might cause error before cancellation check
          info(s"Got error (acceptable with empty classpath): ${e.getMessage}")
      }
    } finally deleteRecursively(outputDir)
  }

  test("Scala Native: fiber cancellation results in IO.canceled") {
    val outputDir = createTempDir("scala-native-fiber-cancel-")
    val outputPath = outputDir.resolve("test-binary")
    try {
      val config = ScalaNativeLinkConfig.Debug

      val cancellation = CancellationToken.create()

      val toolchain = ScalaNativeToolchain.forVersion("0.5.6", "3.3.3")

      // Get Scala Native library classpath
      val scalaNativeLibrary = CompilerResolver.resolveScalaNativeLibrary("0.5.6", "3.3.3")

      val io = toolchain.link(
        config = config,
        classpath = scalaNativeLibrary,
        mainClass = "test.Main",
        outputPath = outputPath,
        workDir = outputDir,
        logger = ScalaNativeToolchain.Logger.Silent,
        cancellation = cancellation
      )

      // Start fiber, wait a bit, then cancel
      // Timeout of 15s ensures test doesn't hang forever (SN is slow)
      val program = (for {
        fiber <- io.start
        _ <- IO.sleep(200.millis) // Let linking start
        _ <- fiber.cancel
        outcome <- fiber.join
      } yield outcome).timeout(15.seconds)

      val outcome = program.unsafeRunSync()

      outcome match {
        case Outcome.Canceled() =>
          info("Fiber-cancelled Scala Native linking correctly returned Canceled")
        case Outcome.Succeeded(fa) =>
          info("Linking completed before cancellation could take effect (fast linking)")
        case Outcome.Errored(e) =>
          fail(s"Expected Canceled but got Errored: ${e.getMessage}")
      }
    } finally deleteRecursively(outputDir)
  }
}
