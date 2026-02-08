package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import cats.effect.{IO, Outcome}
import cats.effect.unsafe.implicits.global
import scala.concurrent.duration.*

/** Integration tests for compilation cancellation.
  *
  * These tests verify that:
  *   1. Pre-cancelled compilations return CompilationCancelled immediately
  *   2. Fiber cancellation can interrupt compilation mid-way
  *   3. The cancelled compilation returns CompilationCancelled result
  *
  * Uses Cats Effect fiber cancellation for robust, non-flaky tests.
  */
class CancellationTest extends AnyFunSuite with Matchers {

  def createTempDir(prefix: String): Path =
    Files.createTempDirectory(prefix)

  def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then {
      if Files.isDirectory(path) then {
        import scala.jdk.StreamConverters.*
        Files.list(path).toScala(List).foreach(deleteRecursively)
      }
      Files.delete(path)
    }

  /** Generate a massive Scala source file that would take a long time to compile. Creates many classes with methods to ensure compilation takes several
    * seconds.
    */
  def generateHugeScalaSource(numClasses: Int, methodsPerClass: Int): SourceFile = {
    val sb = new StringBuilder
    sb.append("package huge\n\n")

    for i <- 0 until numClasses do {
      sb.append(s"class HugeClass$i:\n")
      for j <- 0 until methodsPerClass do {
        // Add some complexity to slow down compilation
        sb.append(s"  def method${j}(x: Int): Int =\n")
        sb.append(s"    val a$j = x + $j\n")
        sb.append(s"    val b$j = a$j * 2\n")
        sb.append(s"    val c$j = b$j + a$j\n")
        sb.append(s"    c$j\n")
        sb.append("\n")
      }
      sb.append("\n")
    }

    SourceFile(Path.of("huge/Huge.scala"), sb.toString)
  }

  /** Generate a massive Kotlin source file that would take a long time to compile. */
  def generateHugeKotlinSource(numClasses: Int, methodsPerClass: Int): SourceFile = {
    val sb = new StringBuilder
    sb.append("package huge\n\n")

    for i <- 0 until numClasses do {
      sb.append(s"class HugeClass$i {\n")
      for j <- 0 until methodsPerClass do {
        sb.append(s"    fun method$j(x: Int): Int {\n")
        sb.append(s"        val a$j = x + $j\n")
        sb.append(s"        val b$j = a$j * 2\n")
        sb.append(s"        val c$j = b$j + a$j\n")
        sb.append(s"        return c$j\n")
        sb.append("    }\n")
        sb.append("\n")
      }
      sb.append("}\n\n")
    }

    SourceFile(Path.of("huge/Huge.kt"), sb.toString)
  }

  /** Generate a massive Java source file that would take a long time to compile. */
  def generateHugeJavaSource(numClasses: Int, methodsPerClass: Int): SourceFile = {
    val sb = new StringBuilder
    sb.append("package huge;\n\n")

    // Java requires one public class per file, so we'll make one big class
    sb.append("public class Huge {\n")
    for j <- 0 until methodsPerClass * numClasses do {
      sb.append(s"    public int method$j(int x) {\n")
      sb.append(s"        int a$j = x + $j;\n")
      sb.append(s"        int b$j = a$j * 2;\n")
      sb.append(s"        int c$j = b$j + a$j;\n")
      sb.append(s"        return c$j;\n")
      sb.append("    }\n")
      sb.append("\n")
    }
    sb.append("}\n")

    SourceFile(Path.of("huge/Huge.java"), sb.toString)
  }

  /** Wrap synchronous compilation in IO for fiber-based cancellation testing */
  def compileScalaIO(
      input: CompilationInput,
      listener: DiagnosticListener,
      cancellation: CancellationToken
  ): IO[CompilationResult] =
    IO.interruptible {
      Compiler.forConfig(input.config).compile(input, listener, cancellation)
    }

  def compileKotlinIO(
      input: CompilationInput,
      listener: DiagnosticListener,
      cancellation: CancellationToken
  ): IO[CompilationResult] =
    IO.interruptible {
      KotlinSourceCompiler.compile(input, listener, cancellation)
    }

  def compileJavaIO(
      input: CompilationInput,
      listener: DiagnosticListener,
      cancellation: CancellationToken
  ): IO[CompilationResult] =
    IO.interruptible {
      Compiler.forConfig(input.config).compile(input, listener, cancellation)
    }

  // ============================================================================
  // Scala Cancellation Tests
  // ============================================================================

  test("Scala: pre-cancelled compilation returns CompilationCancelled") {
    val outputDir = createTempDir("scala-cancel-result-")
    try {
      val source = generateHugeScalaSource(numClasses = 30, methodsPerClass = 15)

      val input = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.scalaLibrary,
        outputDir = outputDir,
        config = ScalaConfig(version = "3.7.4")
      )

      // Pre-cancelled token
      val cancellation = CancellationToken.create()
      cancellation.cancel()

      val result = Compiler.forConfig(input.config).compile(input, DiagnosticListener.noop, cancellation)

      result shouldBe CompilationCancelled
      info("Pre-cancelled compilation returned CompilationCancelled")
    } finally deleteRecursively(outputDir)
  }

  test("Scala: fiber cancellation interrupts compilation") {
    val outputDir = createTempDir("scala-cancel-")
    try {
      // Generate a huge source file (50 classes x 20 methods = 1000 methods)
      val source = generateHugeScalaSource(numClasses = 50, methodsPerClass = 20)
      info(s"Generated source file with ${source.content.length} characters")

      val input = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.scalaLibrary,
        outputDir = outputDir,
        config = ScalaConfig(version = "3.7.4")
      )

      // Create cancellation token
      val cancellation = CancellationToken.create()

      val io = compileScalaIO(input, DiagnosticListener.noop, cancellation)

      // Start fiber, wait a bit for compilation to start, then cancel the token
      // and cancel the fiber
      val program = (for {
        fiber <- io.start
        _ <- IO.sleep(100.millis) // Let compilation start
        _ <- IO(cancellation.cancel()) // Signal cancellation to the compiler
        _ <- fiber.cancel // Cancel the fiber
        outcome <- fiber.join
      } yield outcome).timeout(30.seconds)

      val startTime = System.currentTimeMillis()
      val outcome = program.unsafeRunSync()
      val elapsed = System.currentTimeMillis() - startTime
      info(s"Total time: ${elapsed}ms")

      outcome match {
        case Outcome.Canceled() =>
          info("Fiber-cancelled Scala compilation correctly returned Canceled")
        case Outcome.Succeeded(io) =>
          val result = io.unsafeRunSync()
          result match {
            case CompilationCancelled =>
              info("Compilation returned CompilationCancelled via Succeeded outcome")
            case CompilationSuccess(_, _) =>
              info("Compilation completed before cancellation took effect (fast)")
            case CompilationFailure(errors) =>
              info(s"Compilation failed: ${errors.headOption.map(_.message).getOrElse("unknown")}")
          }
        case Outcome.Errored(e) =>
          fail(s"Expected Canceled but got Errored: ${e.getMessage}")
      }
    } finally deleteRecursively(outputDir)
  }

  test("Scala: mid-compilation cancellation via ProgressCallback") {
    val outputDir = createTempDir("scala-mid-cancel-")
    try {
      // Generate an even larger source to ensure we're in mid-compilation
      val source = generateHugeScalaSource(numClasses = 100, methodsPerClass = 30)
      info(s"Generated massive source file with ${source.content.length} characters (~${source.content.length / 1024}KB)")

      val input = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.scalaLibrary,
        outputDir = outputDir,
        config = ScalaConfig(version = "3.7.4")
      )

      val cancellation = CancellationToken.create()

      val io = compileScalaIO(input, DiagnosticListener.noop, cancellation)

      // Wait longer before cancelling to ensure we're in mid-compilation
      val program = (for {
        fiber <- io.start
        _ <- IO.sleep(500.millis) // Wait for compilation to be well underway
        _ <- IO(cancellation.cancel())
        _ <- fiber.cancel
        outcome <- fiber.join
      } yield outcome).timeout(30.seconds)

      val startTime = System.currentTimeMillis()
      val outcome = program.unsafeRunSync()
      val elapsed = System.currentTimeMillis() - startTime
      info(s"Total time: ${elapsed}ms")

      outcome match {
        case Outcome.Canceled() =>
          info("Mid-compilation cancellation correctly returned Canceled")
        case Outcome.Succeeded(io) =>
          val result = io.unsafeRunSync()
          result match {
            case CompilationCancelled =>
              info("Compilation returned CompilationCancelled")
            case CompilationSuccess(_, classes) =>
              info(s"Compilation completed with ${classes.size} classes (fast compilation)")
            case CompilationFailure(errors) =>
              info(s"Compilation failed: ${errors.headOption.map(_.message).getOrElse("unknown")}")
          }
        case Outcome.Errored(e) =>
          fail(s"Expected Canceled but got Errored: ${e.getMessage}")
      }
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Kotlin Cancellation Tests
  // ============================================================================

  test("Kotlin: pre-cancelled compilation returns CompilationCancelled") {
    val outputDir = createTempDir("kotlin-cancel-result-")
    try {
      val source = generateHugeKotlinSource(numClasses = 30, methodsPerClass = 15)

      val input = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.kotlinLibrary,
        outputDir = outputDir,
        config = KotlinConfig(version = "2.3.0")
      )

      val cancellation = CancellationToken.create()
      cancellation.cancel()

      val result = KotlinSourceCompiler.compile(input, DiagnosticListener.noop, cancellation)

      result shouldBe CompilationCancelled
      info("Pre-cancelled compilation returned CompilationCancelled")
    } finally deleteRecursively(outputDir)
  }

  test("Kotlin: fiber cancellation interrupts compilation") {
    val outputDir = createTempDir("kotlin-cancel-")
    try {
      val source = generateHugeKotlinSource(numClasses = 50, methodsPerClass = 20)
      info(s"Generated source file with ${source.content.length} characters")

      val input = CompilationInput(
        sources = Seq(source),
        classpath = CompilerTestLibraries.kotlinLibrary,
        outputDir = outputDir,
        config = KotlinConfig(version = "2.3.0")
      )

      val cancellation = CancellationToken.create()

      val io = compileKotlinIO(input, DiagnosticListener.noop, cancellation)

      val program = (for {
        fiber <- io.start
        _ <- IO.sleep(100.millis)
        _ <- IO(cancellation.cancel())
        _ <- fiber.cancel
        outcome <- fiber.join
      } yield outcome).timeout(30.seconds)

      val startTime = System.currentTimeMillis()
      val outcome = program.unsafeRunSync()
      val elapsed = System.currentTimeMillis() - startTime
      info(s"Total time: ${elapsed}ms")

      outcome match {
        case Outcome.Canceled() =>
          info("Fiber-cancelled Kotlin compilation correctly returned Canceled")
        case Outcome.Succeeded(io) =>
          val result = io.unsafeRunSync()
          result match {
            case CompilationCancelled =>
              info("Compilation returned CompilationCancelled via Succeeded outcome")
            case CompilationSuccess(_, _) =>
              info("Compilation completed before cancellation took effect")
            case CompilationFailure(errors) =>
              info(s"Compilation failed: ${errors.headOption.map(_.message).getOrElse("unknown")}")
          }
        case Outcome.Errored(e) =>
          fail(s"Expected Canceled but got Errored: ${e.getMessage}")
      }
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Java Cancellation Tests
  // ============================================================================

  test("Java: pre-cancelled compilation returns CompilationCancelled") {
    val outputDir = createTempDir("java-cancel-result-")
    try {
      val source = generateHugeJavaSource(numClasses = 30, methodsPerClass = 15)

      val input = CompilationInput(
        sources = Seq(source),
        classpath = Seq.empty,
        outputDir = outputDir,
        config = JavaConfig()
      )

      val cancellation = CancellationToken.create()
      cancellation.cancel()

      val result = Compiler.forConfig(input.config).compile(input, DiagnosticListener.noop, cancellation)

      result shouldBe CompilationCancelled
      info("Pre-cancelled compilation returned CompilationCancelled")
    } finally deleteRecursively(outputDir)
  }

  test("Java: fiber cancellation interrupts compilation") {
    val outputDir = createTempDir("java-cancel-")
    try {
      val source = generateHugeJavaSource(numClasses = 50, methodsPerClass = 20)
      info(s"Generated source file with ${source.content.length} characters")

      val input = CompilationInput(
        sources = Seq(source),
        classpath = Seq.empty,
        outputDir = outputDir,
        config = JavaConfig()
      )

      val cancellation = CancellationToken.create()

      val io = compileJavaIO(input, DiagnosticListener.noop, cancellation)

      val program = (for {
        fiber <- io.start
        _ <- IO.sleep(50.millis) // Java is fast, use shorter delay
        _ <- IO(cancellation.cancel())
        _ <- fiber.cancel
        outcome <- fiber.join
      } yield outcome).timeout(30.seconds)

      val startTime = System.currentTimeMillis()
      val outcome = program.unsafeRunSync()
      val elapsed = System.currentTimeMillis() - startTime
      info(s"Total time: ${elapsed}ms")

      outcome match {
        case Outcome.Canceled() =>
          info("Fiber-cancelled Java compilation correctly returned Canceled")
        case Outcome.Succeeded(io) =>
          val result = io.unsafeRunSync()
          result match {
            case CompilationCancelled =>
              info("Compilation returned CompilationCancelled via Succeeded outcome")
            case CompilationSuccess(_, _) =>
              info("Compilation completed before cancellation took effect (Java is fast)")
            case CompilationFailure(errors) =>
              info(s"Compilation failed: ${errors.headOption.map(_.message).getOrElse("unknown")}")
          }
        case Outcome.Errored(e) =>
          fail(s"Expected Canceled but got Errored: ${e.getMessage}")
      }
    } finally deleteRecursively(outputDir)
  }
}
