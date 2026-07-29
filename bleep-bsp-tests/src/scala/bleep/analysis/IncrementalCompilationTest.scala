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

  /** Kotlin's incremental compiler only writes these once it has actually run incrementally — a non-incremental round leaves `caches-jvm/jvm/kotlin` empty.
    *
    * Regression guard: bleep used to construct the IC runner with a constructor signature no shipping Kotlin has, and pass `ChangedFiles.Unknown` (which makes
    * the runner bail out to a rebuild immediately), and never set `incrementalCompilation` on the compiler arguments (which makes the compiler drop the IC
    * trackers). All three failed silently, so every Kotlin build was a full compile.
    */
  test("KotlinSourceCompiler: incremental compilation actually engages and persists caches") {
    val outputDir = createTempDir("kotlin-ic-engages-")
    try {
      val fooV1 = SourceFile(Path.of("Foo.kt"), "object Foo {\n    fun hello(): String = \"Hello\"\n}\n")
      val bar = SourceFile(Path.of("Bar.kt"), "object Bar {\n    fun world(): String = \"World\"\n}\n")

      def compile(sources: Seq[SourceFile]): CompilationResult =
        KotlinSourceCompiler.compile(
          CompilationInput(sources = sources, classpath = CompilerTestLibraries.kotlinLibrary, outputDir = outputDir, config = KotlinConfig(version = "2.3.0")),
          DiagnosticListener.noop
        )

      compile(Seq(fooV1, bar)).isSuccess shouldBe true

      // The class -> source index Kotlin maintains for IC. Its presence means the compiler ran with IC wired up and flushed its caches.
      val jvmCaches = outputDir.resolve(".kotlin-ic/caches-jvm/jvm/kotlin")
      Files.isDirectory(jvmCaches) shouldBe true
      val tabs = {
        import scala.jdk.StreamConverters.*
        Files.list(jvmCaches).toScala(List).map(_.getFileName.toString)
      }
      info(s"Kotlin IC caches: ${tabs.filter(_.endsWith(".tab")).sorted.mkString(", ")}")
      tabs should contain("class-fq-name-to-source.tab")
      tabs should contain("source-to-classes.tab")

      // A second round must reuse those caches rather than wiping the output directory, which is what a rebuild does.
      val marker = outputDir.resolve("marker-must-survive-incremental-round")
      Files.writeString(marker, "x")

      // "Farewell" rather than "Hello again": the constant-pool check below needs the two versions to be disjoint strings, or `contains` cannot tell them apart.
      val fooV2 = SourceFile(Path.of("Foo.kt"), "object Foo {\n    fun hello(): String = \"Farewell\"\n}\n")
      compile(Seq(fooV2, bar)).isSuccess shouldBe true

      withClue("output directory was wiped, so Kotlin took the non-incremental rebuild path: ") {
        Files.exists(marker) shouldBe true
      }

      // Reusing caches is only worth anything if the result is still correct. The failure mode this guards is the expensive one: an incremental round that
      // decides Foo.kt needs no work and leaves the previous bytecode in place, so the build goes green while the class file disagrees with the source.
      // Checking the emitted constant pool directly, because every cheaper signal (timestamps, "compiled N files") is reported by the same machinery under test.
      // Latin-1 so every byte maps to a char and the scan is over the raw class file, not a lossy UTF-8 decode of it.
      val fooBytes = new String(Files.readAllBytes(outputDir.resolve("Foo.class")), java.nio.charset.StandardCharsets.ISO_8859_1)
      withClue("Foo.class does not contain the constant from the source it was just compiled from: ") {
        fooBytes.contains("Farewell") shouldBe true
      }
      withClue("Foo.class still carries the superseded constant, i.e. a stale class survived the incremental round: ") {
        fooBytes.contains("Hello") shouldBe false
      }
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
          progressListener = ProgressListener.noop,
          ecjVersion = None,
          analyses = AnalysisCache.standalone(config.buildDir)
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
        case ProjectCompileCancelled(reason) =>
          fail(s"Unexpected cancellation: $reason")
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
          progressListener = ProgressListener.noop,
          ecjVersion = None,
          analyses = AnalysisCache.standalone(config.buildDir)
        )
        .unsafeRunSync()

      result1 match {
        case ProjectCompileSuccess(_, _, _) =>
          info(s"First compile reported ${files1.size} files")
        case ProjectCompileFailure(errors) =>
          fail(s"First compilation failed: ${errors.map(_.formatted).mkString(", ")}")
        case ProjectCompileCancelled(reason) =>
          fail(s"Unexpected cancellation: $reason")
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
          progressListener = ProgressListener.noop,
          ecjVersion = None,
          analyses = AnalysisCache.standalone(config.buildDir)
        )
        .unsafeRunSync()

      result2 match {
        case ProjectCompileSuccess(_, _, _) =>
          info(s"Second compile (no changes) reported ${files2.size} files")

          // No files should be recompiled
          files2.size shouldBe 0

        case ProjectCompileFailure(errors) =>
          fail(s"Second compilation failed: ${errors.map(_.formatted).mkString(", ")}")
        case ProjectCompileCancelled(reason) =>
          fail(s"Unexpected cancellation: $reason")
      }
    } finally {
      deleteRecursively(outputDir)
      deleteRecursively(sourceDir)
    }
  }

}
