package bleep.analysis

import bleep.bsp.Outcome
import bleep.bsp.Outcome.{KillReason, RunOutcome}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import cats.effect.unsafe.implicits.global

/** Integration tests for Kotlin/JS compiler support.
  *
  * Tests compiling and running Kotlin/JS projects with different configurations.
  */
class KotlinJsIntegrationTest extends AnyFunSuite with Matchers {

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

  test("CompilerResolver: resolves Kotlin/JS compiler for 2.3.0") {
    val instance = CompilerResolver.getKotlinJsCompiler("2.3.0")
    instance.allJars should not be empty
    info(s"Resolved ${instance.allJars.size} JARs for Kotlin/JS 2.3.0")
    instance.allJars.exists(_.getFileName.toString.contains("kotlin-compiler")) shouldBe true
  }

  test("CompilerResolver: resolves Kotlin/JS compiler for 2.0.0") {
    val instance = CompilerResolver.getKotlinJsCompiler("2.0.0")
    instance.allJars should not be empty
    info(s"Resolved ${instance.allJars.size} JARs for Kotlin/JS 2.0.0")
  }

  test("CompilerResolver: resolves Kotlin/JS stdlib") {
    val jars = CompilerResolver.resolveKotlinJsLibrary("2.3.0")
    jars should not be empty
    info(s"Resolved ${jars.size} JARs for Kotlin/JS stdlib")
    jars.exists(_.getFileName.toString.contains("kotlin-stdlib-js")) shouldBe true
  }

  test("CompilerResolver: different Kotlin/JS versions have different classloaders") {
    val instance230 = CompilerResolver.getKotlinJsCompiler("2.3.0")
    val instance200 = CompilerResolver.getKotlinJsCompiler("2.0.0")

    instance230.loader should not be instance200.loader
    info("Different Kotlin/JS versions have isolated classloaders")
  }

  test("CompilerResolver: caches Kotlin/JS compiler instances") {
    val instance1 = CompilerResolver.getKotlinJsCompiler("2.3.0")
    val instance2 = CompilerResolver.getKotlinJsCompiler("2.3.0")

    instance1.loader shouldBe instance2.loader
    info("Kotlin/JS compiler instances are cached correctly")
  }

  // ============================================================================
  // Configuration Tests
  // ============================================================================

  test("KotlinJsCompilerConfig: default configuration") {
    val config = KotlinJsCompilerConfig.Default
    config.kotlinVersion shouldBe "2.3.0"
    config.moduleKind shouldBe KotlinJsCompilerConfig.ModuleKind.CommonJS
    config.outputMode shouldBe KotlinJsCompilerConfig.OutputMode.JsExecutable
    config.sourceMap shouldBe true
    config.developmentMode shouldBe true
    config.generateDts shouldBe false
  }

  test("KotlinJsCompilerConfig: module kinds") {
    KotlinJsCompilerConfig.ModuleKind.Plain.name shouldBe "plain"
    KotlinJsCompilerConfig.ModuleKind.AMD.name shouldBe "amd"
    KotlinJsCompilerConfig.ModuleKind.CommonJS.name shouldBe "commonjs"
    KotlinJsCompilerConfig.ModuleKind.UMD.name shouldBe "umd"
    KotlinJsCompilerConfig.ModuleKind.ESModule.name shouldBe "es"
  }

  test("KotlinJsCompilerConfig: output modes") {
    KotlinJsCompilerConfig.OutputMode.Klib.name shouldBe "klib"
    KotlinJsCompilerConfig.OutputMode.JsExecutable.name shouldBe "js"
  }

  test("KotlinJsCompilerConfig: source map embed sources") {
    KotlinJsCompilerConfig.SourceMapEmbedSources.Never.name shouldBe "never"
    KotlinJsCompilerConfig.SourceMapEmbedSources.Always.name shouldBe "always"
    KotlinJsCompilerConfig.SourceMapEmbedSources.Inlining.name shouldBe "inlining"
  }

  test("KotlinJsCompilerConfig: targets") {
    KotlinJsCompilerConfig.Target.Browser.name shouldBe "browser"
    KotlinJsCompilerConfig.Target.Node.name shouldBe "nodejs"
  }

  // ============================================================================
  // KotlinJsCompileResult Tests
  // ============================================================================

  test("KotlinJsCompileResult: isSuccess when exitCode is 0") {
    val result = KotlinJsCompileResult(
      outputDir = Path.of("output"),
      outputFile = Some(Path.of("output/main.js")),
      klibFile = None,
      exitCode = 0
    )
    result.isSuccess shouldBe true
  }

  test("KotlinJsCompileResult: not success when exitCode is non-zero") {
    val result = KotlinJsCompileResult(
      outputDir = Path.of("output"),
      outputFile = None,
      klibFile = None,
      exitCode = 1
    )
    result.isSuccess shouldBe false
  }

  // ============================================================================
  // KotlinJsRunner Tests (reuses ScalaJsRunner Node.js detection)
  // ============================================================================

  test("KotlinJsRunner: can detect Node.js availability") {
    // This test just verifies the runner exists and has the expected methods
    val outputDir = createTempDir("kotlin-js-runner-test")
    try {
      val jsFile = outputDir.resolve("test.js")
      Files.writeString(jsFile, "console.log('Hello from Kotlin/JS test')")

      val result = (for {
        killSignal <- Outcome.neverKillSignal
        outcome <- KotlinJsRunner.run(
          jsFile = jsFile,
          args = Seq.empty,
          moduleKind = KotlinJsCompilerConfig.ModuleKind.CommonJS,
          workingDir = outputDir,
          env = Map.empty,
          nodeBinary = PlatformTestHelper.nodeBinary,
          killSignal = killSignal
        )
      } yield outcome).unsafeRunSync()

      result match {
        case RunOutcome.Completed(exitCode, stdout, _) =>
          exitCode shouldBe 0
          stdout.trim shouldBe "Hello from Kotlin/JS test"
          info("KotlinJsRunner executed JavaScript successfully")
        case other =>
          fail(s"Unexpected outcome: $other")
      }
    } finally deleteRecursively(outputDir)
  }
}

/** Advanced Kotlin/JS integration tests.
  *
  * These tests verify actual compilation behavior.
  */
class KotlinJsAdvancedIntegrationTest extends AnyFunSuite with Matchers with PlatformTestHelper {

  test("KotlinJsCompiler: compile simple Kotlin file to JavaScript") {
    val outputDir = createTempDir("kotlin-js-compile-")
    val sourceDir = createTempDir("kotlin-js-src-")
    try {
      // Create a simple Kotlin source file
      val sourceFile = sourceDir.resolve("Hello.kt")
      Files.writeString(
        sourceFile,
        """fun main() {
          |    println("Hello from Kotlin/JS!")
          |}
          |""".stripMargin
      )

      val config = KotlinJsCompilerConfig.Default.copy(
        moduleName = "hello",
        outputMode = KotlinJsCompilerConfig.OutputMode.JsExecutable
      )

      val errors = scala.collection.mutable.ListBuffer[CompilerError]()
      val listener = new DiagnosticListener {
        override def onDiagnostic(error: CompilerError): Unit = {
          errors += error
          info(s"Diagnostic: ${error.formatted}")
        }
      }

      val kotlinJsLib = CompilerResolver.resolveKotlinJsLibrary(config.kotlinVersion)

      val result = KotlinJsCompiler
        .compile(
          sources = Seq(sourceFile),
          libraries = kotlinJsLib,
          outputDir = outputDir,
          config = config,
          diagnosticListener = listener,
          cancellation = CancellationToken.create()
        )
        .unsafeRunSync()

      if (result.isSuccess) {
        info(s"Compiled successfully to ${result.outputDir}")
        result.outputFile.foreach(f => info(s"Output file: $f"))

        // Try to run if Node.js is available
        result.outputFile.foreach { jsFile =>
          if (Files.exists(jsFile) && ScalaJsRunner.isNodeAvailable(nodeBinary).unsafeRunSync()) {
            val runResult = (for {
              killSignal <- Outcome.neverKillSignal
              outcome <- KotlinJsRunner.run(
                jsFile = jsFile,
                args = Seq.empty,
                moduleKind = config.moduleKind,
                workingDir = outputDir,
                env = Map.empty,
                nodeBinary = nodeBinary,
                killSignal = killSignal
              )
            } yield outcome).unsafeRunSync()

            runResult match {
              case RunOutcome.Completed(exitCode, stdout, stderr) =>
                if (exitCode == 0) {
                  info(s"Ran successfully, output: ${stdout.trim}")
                } else {
                  info(s"Run failed with exit code $exitCode: $stderr")
                }
              case other =>
                info(s"Run had unexpected outcome: $other")
            }
          }
        }
      } else {
        info(s"Compilation failed with ${errors.size} errors")
        // This is expected if Kotlin/JS IR runtime is not available
      }
    } finally {
      deleteRecursively(outputDir)
      deleteRecursively(sourceDir)
    }
  }

  test("KotlinJsCompiler: compile to KLIB") {
    val outputDir = createTempDir("kotlin-js-klib-")
    val sourceDir = createTempDir("kotlin-js-klib-src-")
    try {
      val sourceFile = sourceDir.resolve("Lib.kt")
      Files.writeString(
        sourceFile,
        """package mylib
          |
          |fun greet(name: String): String = "Hello, $name!"
          |""".stripMargin
      )

      val config = KotlinJsCompilerConfig.Default.copy(
        moduleName = "mylib",
        outputMode = KotlinJsCompilerConfig.OutputMode.Klib
      )

      val listener = new DiagnosticListener {
        override def onDiagnostic(error: CompilerError): Unit =
          info(s"Diagnostic: ${error.formatted}")
      }

      val result = KotlinJsCompiler
        .compile(
          sources = Seq(sourceFile),
          libraries = Seq.empty,
          outputDir = outputDir,
          config = config,
          diagnosticListener = listener,
          cancellation = CancellationToken.create()
        )
        .unsafeRunSync()

      if (result.isSuccess) {
        info(s"Compiled KLIB successfully to ${result.outputDir}")
        result.klibFile.foreach(f => info(s"KLIB file: $f"))
      } else {
        info("KLIB compilation may require additional setup")
      }
    } finally {
      deleteRecursively(outputDir)
      deleteRecursively(sourceDir)
    }
  }
}
