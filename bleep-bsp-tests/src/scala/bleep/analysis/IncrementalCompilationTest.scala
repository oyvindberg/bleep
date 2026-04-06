package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}
import scala.collection.mutable

/** Integration tests for incremental compilation using diagnostic tracking.
  *
  * These tests verify that compilers only recompile files that have changed by tracking which files are reported to DiagnosticListener.onCompileFile.
  *
  * Unlike IncrementalCompilationTest which tests cross-file dependencies, these tests focus on tracking which specific files are compiled.
  */
class IncrementalTrackingTest extends AnyFunSuite with Matchers {

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

  /** Create a tracking listener that records compiled files */
  def trackingListener(): (DiagnosticListener, mutable.Set[Path], mutable.Buffer[CompilerError]) = {
    val compiledFiles = mutable.Set[Path]()
    val diagnostics = mutable.Buffer[CompilerError]()
    val listener = DiagnosticListener.tracking(compiledFiles, diagnostics)
    (listener, compiledFiles, diagnostics)
  }

  // ============================================================================
  // Scala Source Compiler Incremental Tests
  // ============================================================================

  test("ScalaSourceCompiler: first compile reports all files") {
    val outputDir = createTempDir("scala-ic-test-")
    try {
      val source1 = SourceFile(
        Path.of("Foo.scala"),
        """object Foo {
          |  def hello: String = "Hello"
          |}
          |""".stripMargin
      )
      val source2 = SourceFile(
        Path.of("Bar.scala"),
        """object Bar {
          |  def world: String = "World"
          |}
          |""".stripMargin
      )

      val scalaVersion = "3.3.3"
      val config = ScalaConfig(version = scalaVersion)
      val input = CompilationInput(
        sources = Seq(source1, source2),
        classpath = CompilerResolver.getScalaCompiler(scalaVersion).allJars,
        outputDir = outputDir,
        config = config
      )

      val (listener, compiledFiles, diagnostics) = trackingListener()
      val result = Compiler.forConfig(input.config).compile(input, listener)

      result.isSuccess shouldBe true
      info(s"First compile reported ${compiledFiles.size} files: ${compiledFiles.map(_.getFileName).mkString(", ")}")

      // Both files should be reported on first compile
      compiledFiles.size should be >= 2
      compiledFiles.exists(_.toString.contains("Foo")) shouldBe true
      compiledFiles.exists(_.toString.contains("Bar")) shouldBe true
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Java Source Compiler Tests
  // ============================================================================

  test("JavaSourceCompiler: compile reports all files") {
    val outputDir = createTempDir("java-ic-test-")
    try {
      val source1 = SourceFile(
        Path.of("Foo.java"),
        """public class Foo {
          |    public static String hello() { return "Hello"; }
          |}
          |""".stripMargin
      )
      val source2 = SourceFile(
        Path.of("Bar.java"),
        """public class Bar {
          |    public static String world() { return "World"; }
          |}
          |""".stripMargin
      )

      val config = JavaConfig()
      val input = CompilationInput(
        sources = Seq(source1, source2),
        classpath = Seq.empty,
        outputDir = outputDir,
        config = config
      )

      val (listener, compiledFiles, diagnostics) = trackingListener()
      val result = Compiler.forConfig(input.config).compile(input, listener)

      result.isSuccess shouldBe true
      info(s"Java compile reported ${compiledFiles.size} files: ${compiledFiles.map(_.getFileName).mkString(", ")}")

      // Both files should be reported
      compiledFiles.size shouldBe 2
      compiledFiles.exists(_.toString.contains("Foo")) shouldBe true
      compiledFiles.exists(_.toString.contains("Bar")) shouldBe true
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Kotlin Source Compiler Incremental Tests
  // ============================================================================

  // Kotlin 2.3.0 adds JDK 25 support
  test("KotlinSourceCompiler: first compile reports all files") {
    val outputDir = createTempDir("kotlin-ic-test-")
    try {
      val source1 = SourceFile(
        Path.of("Foo.kt"),
        """object Foo {
          |    fun hello(): String = "Hello"
          |}
          |""".stripMargin
      )
      val source2 = SourceFile(
        Path.of("Bar.kt"),
        """object Bar {
          |    fun world(): String = "World"
          |}
          |""".stripMargin
      )

      val config = KotlinConfig(version = "2.3.0")
      val input = CompilationInput(
        sources = Seq(source1, source2),
        classpath = CompilerTestLibraries.kotlinLibrary,
        outputDir = outputDir,
        config = config
      )

      val (listener, compiledFiles, diagnostics) = trackingListener()
      val result = KotlinSourceCompiler.compile(input, listener)

      result.isSuccess shouldBe true
      info(s"Kotlin first compile reported ${compiledFiles.size} files: ${compiledFiles.map(_.getFileName).mkString(", ")}")

      // Both files should be reported on first compile
      compiledFiles.size shouldBe 2
      compiledFiles.exists(_.toString.contains("Foo")) shouldBe true
      compiledFiles.exists(_.toString.contains("Bar")) shouldBe true
    } finally deleteRecursively(outputDir)
  }

  // ============================================================================
  // Zinc Incremental Compilation Tests
  // ============================================================================

  test("Zinc: first compile reports all files via startUnit") {
    val outputDir = createTempDir("zinc-ic-test-")
    val sourceDir = createTempDir("zinc-ic-src-")
    try {
      // Write source files to disk
      val fooPath = sourceDir.resolve("Foo.scala")
      Files.writeString(fooPath, """object Foo { def hello: String = "Hello" }""")

      val barPath = sourceDir.resolve("Bar.scala")
      Files.writeString(barPath, """object Bar { def world: String = "World" }""")

      val language: ProjectLanguage.ScalaJava = ProjectLanguage.ScalaJava(
        scalaVersion = "3.3.3",
        scalaOptions = Nil,
        javaRelease = None,
        javaOptions = Nil
      )

      val config = ProjectConfig(
        name = "test-project",
        sources = Set(sourceDir),
        classpath = CompilerResolver.getScalaCompiler("3.3.3").allJars,
        outputDir = outputDir,
        language = language,
        analysisDir = Some(outputDir.resolve(".zinc")),
        buildDir = outputDir.getParent
      )

      val (listener, compiledFiles, diagnostics) = trackingListener()

      import cats.effect.unsafe.implicits.global
      val result = ZincBridge
        .compile(
          config = config,
          language = language,
          diagnosticListener = listener,
          cancellationToken = CancellationToken.never,
          dependencyAnalyses = Map.empty,
          progressListener = ProgressListener.noop
        )
        .unsafeRunSync()

      result match {
        case ProjectCompileSuccess(_, _, _) =>
          info(s"Zinc first compile reported ${compiledFiles.size} files")
          compiledFiles.foreach(p => info(s"  - ${p.getFileName}"))

          // Both files should be reported
          compiledFiles.size should be >= 2
          compiledFiles.exists(_.toString.contains("Foo")) shouldBe true
          compiledFiles.exists(_.toString.contains("Bar")) shouldBe true

        case ProjectCompileFailure(errors) =>
          fail(s"Compilation failed: ${errors.map(_.formatted).mkString(", ")}")
      }
    } finally {
      deleteRecursively(outputDir)
      deleteRecursively(sourceDir)
    }
  }

  test("Zinc: no files reported when nothing changed") {
    val outputDir = createTempDir("zinc-ic-noop-test-")
    val sourceDir = createTempDir("zinc-ic-noop-src-")
    try {
      // Write source files
      val fooPath = sourceDir.resolve("Foo.scala")
      Files.writeString(fooPath, """object Foo { def hello: String = "Hello" }""")

      val language: ProjectLanguage.ScalaJava = ProjectLanguage.ScalaJava(
        scalaVersion = "3.3.3",
        scalaOptions = Nil,
        javaRelease = None,
        javaOptions = Nil
      )

      val config = ProjectConfig(
        name = "test-project",
        sources = Set(sourceDir),
        classpath = CompilerResolver.getScalaCompiler("3.3.3").allJars,
        outputDir = outputDir,
        language = language,
        analysisDir = Some(outputDir.resolve(".zinc")),
        buildDir = outputDir.getParent
      )

      import cats.effect.unsafe.implicits.global

      // First compile
      val (listener1, files1, _) = trackingListener()
      val result1 = ZincBridge
        .compile(
          config = config,
          language = language,
          diagnosticListener = listener1,
          cancellationToken = CancellationToken.never,
          dependencyAnalyses = Map.empty,
          progressListener = ProgressListener.noop
        )
        .unsafeRunSync()

      result1 match {
        case ProjectCompileSuccess(_, _, _) =>
          info(s"First compile reported ${files1.size} files")
        case ProjectCompileFailure(errors) =>
          fail(s"First compilation failed: ${errors.map(_.formatted).mkString(", ")}")
      }

      // Second compile without changes
      val (listener2, files2, _) = trackingListener()
      val result2 = ZincBridge
        .compile(
          config = config,
          language = language,
          diagnosticListener = listener2,
          cancellationToken = CancellationToken.never,
          dependencyAnalyses = Map.empty,
          progressListener = ProgressListener.noop
        )
        .unsafeRunSync()

      result2 match {
        case ProjectCompileSuccess(_, _, _) =>
          info(s"Second compile (no changes) reported ${files2.size} files")

          // No files should be recompiled
          files2.size shouldBe 0

        case ProjectCompileFailure(errors) =>
          fail(s"Second compilation failed: ${errors.map(_.formatted).mkString(", ")}")
      }
    } finally {
      deleteRecursively(outputDir)
      deleteRecursively(sourceDir)
    }
  }

}
