package bleep.analysis

import cats.effect.unsafe.implicits.global
import coursier.cache.CacheLogger
import java.nio.file.{Files, Path}

/** Shared helpers for advanced platform integration tests.
  *
  * Provides compilation helpers that invoke ZincBridge with appropriate compiler plugins to produce platform-specific IR (.sjsir, .nir) and then link/run.
  */
object PlatformTestHelper {
  val DefaultNodeVersion = bleep.constants.Node

  /** Managed Node.js binary path, fetched via Coursier. Cached globally. */
  lazy val nodeBinary: String = {
    val fetchNode = new bleep.FetchNode(CacheLogger.nop, scala.concurrent.ExecutionContext.global)
    fetchNode(DefaultNodeVersion).toAbsolutePath.toString
  }

  /** Unwrap a `ThreadOutcome.Completed` in tests, failing loudly on `Cancelled` or `Crashed`. Lets the rest of a test read the result type's fields directly
    * without inline pattern matching at every call site.
    */
  extension [A](o: bleep.bsp.Outcome.ThreadOutcome[A]) {
    def assertCompleted: A = o match {
      case bleep.bsp.Outcome.ThreadOutcome.Completed(r) => r
      case bleep.bsp.Outcome.ThreadOutcome.Cancelled(r) => throw new AssertionError(s"Expected Completed but got Cancelled($r)")
      case bleep.bsp.Outcome.ThreadOutcome.Crashed(t)   => throw new AssertionError(s"Expected Completed but got Crashed: ${t.getMessage}", t)
    }
  }
}

trait PlatformTestHelper {

  val DefaultScalaVersion = "3.3.3"
  val DefaultScala213Version = "2.13.15"
  val DefaultScalaJsVersion = "1.16.0"
  val DefaultScalaNativeVersion = "0.5.6"
  val DefaultScalaNative04Version = "0.4.17"
  val DefaultKotlinVersion = "2.0.0"
  val DefaultNodeVersion: String = PlatformTestHelper.DefaultNodeVersion

  /** Managed Node.js binary path, fetched via Coursier. */
  def nodeBinary: String = PlatformTestHelper.nodeBinary

  /** Cancel the surrounding test when Kotlin Native cannot run on this host. Uses `assume` so ScalaTest reports the test as canceled rather than passed — the
    * coverage loss stays visible in the run summary instead of masquerading as a green test.
    *
    * Two hosts are excluded, both because the Konan toolchain itself does not work there, never because a test is inconvenient:
    *
    *   - **linux-aarch64**: JetBrains does not publish a `kotlin-native-prebuilt-linux-aarch64-*` artifact (verified up to 2.3.21 / 2.4.0-RC).
    *     `KotlinNativeCompiler.scala` falls back to the `linux-x86_64` distribution, which the JVM then fails to load with `UnsatisfiedLinkError` inside
    *     `kotlinx.cinterop.JvmCallbacksKt.<clinit>`.
    *   - **Windows**: the Konan driver dies during its own static initialization — `error: compilation failed: null` followed by `ExceptionInInitializerError`
    *     at `org.jetbrains.kotlin.backend.konan.driver.NativeCompilerDriver.run`. It does not throw something catchable; it takes the whole forked test JVM
    *     down with exit code 2, which is why `KotlinNativeAdvancedIntegrationTest` and `LinkExecutorIntegrationTest` reported as "did not finish" rather than
    *     as failures. Note this cancels only the Konan-driving tests: Scala Native linking works on Windows and keeps running.
    */
  def assumeKotlinNativeAvailable(): Unit = {
    org.scalatest.Assertions
      .assume(bleep.OsArch.current != bleep.OsArch.LinuxArm64, "Kotlin Native does not publish a linux-aarch64 prebuilt; test cannot run on this host")
    org.scalatest.Assertions
      .assume(!scala.util.Properties.isWin, "Kotlin Native's compiler driver crashes in its static initializer on Windows and kills the test JVM")
  }

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

  def withTempDir[A](prefix: String)(f: Path => A): A = {
    val dir = createTempDir(prefix)
    try f(dir)
    finally deleteRecursively(dir)
  }

  def writeScalaSource(dir: Path, pkg: String, fileName: String, code: String): Path = {
    val pkgDir = pkg.split('.').foldLeft(dir)((d, p) => d.resolve(p))
    Files.createDirectories(pkgDir)
    val file = pkgDir.resolve(fileName)
    Files.writeString(file, code)
    file
  }

  def writeKotlinSource(dir: Path, fileName: String, code: String): Path = {
    Files.createDirectories(dir)
    val file = dir.resolve(fileName)
    Files.writeString(file, code)
    file
  }

  /** Compile Scala sources for Scala.js, producing .sjsir files.
    *
    * @return
    *   classpath including the output dir and scalajs-library
    */
  def compileForScalaJs(
      srcDir: Path,
      outDir: Path,
      scalaVersion: String,
      sjsVersion: String
  ): Seq[Path] = {
    Files.createDirectories(outDir)

    val scalaLibJars = CompilerResolver.resolveScalaLibrary(scalaVersion)
    val sjsLibJars = CompilerResolver.resolveScalaJsLibrary(sjsVersion, scalaVersion)

    val scalaOptions: List[String] = if (scalaVersion.startsWith("3.")) {
      List("-scalajs")
    } else {
      val pluginJars = CompilerResolver.resolveScalaJsCompilerPlugin(sjsVersion, scalaVersion)
      val pluginJar = pluginJars
        .find(_.getFileName.toString.contains("scalajs-compiler"))
        .getOrElse(pluginJars.head)
      List(s"-Xplugin:${pluginJar.toAbsolutePath}")
    }

    val language: ProjectLanguage.ScalaJava = ProjectLanguage.ScalaJava(
      scalaVersion = scalaVersion,
      scalaOptions = scalaOptions,
      javaOptions = Nil
    )

    val config = ProjectConfig(
      name = "scalajs-compile",
      sources = Set(srcDir),
      classpath = scalaLibJars ++ sjsLibJars,
      outputDir = outDir,
      language = language,
      analysisDir = None,
      buildDir = outDir.getParent
    )

    val result = ZincBridge
      .compile(
        config,
        language,
        DiagnosticListener.noop,
        CancellationToken.never,
        Map.empty,
        ProgressListener.noop
      )
      .unsafeRunSync()

    result match {
      case ProjectCompileSuccess(_, _, _)  => Seq(outDir) ++ scalaLibJars ++ sjsLibJars
      case f: ProjectCompileFailure        => throw new RuntimeException(s"Scala.js compilation failed: ${f.errors.map(_.formatted).mkString("\n")}")
      case ProjectCompileCancelled(reason) => throw new RuntimeException(s"Scala.js compilation cancelled: $reason")
    }
  }

  /** Compile Scala sources for Scala Native, producing .nir files.
    *
    * @return
    *   classpath including the output dir and scala-native nativelib
    */
  def compileForScalaNative(
      srcDir: Path,
      outDir: Path,
      scalaVersion: String,
      snVersion: String
  ): Seq[Path] =
    compileForScalaNativeWithDeps(srcDir, outDir, scalaVersion, snVersion, Seq.empty)

  /** Compile Scala sources for Scala Native with extra dependencies on classpath. */
  def compileForScalaNativeWithDeps(
      srcDir: Path,
      outDir: Path,
      scalaVersion: String,
      snVersion: String,
      extraDeps: Seq[Path]
  ): Seq[Path] = {
    Files.createDirectories(outDir)

    val scalaLibJars = CompilerResolver.resolveScalaLibrary(scalaVersion)
    val snLibJars = CompilerResolver.resolveScalaNativeLibrary(snVersion, scalaVersion)
    val pluginJars = CompilerResolver.resolveScalaNativeCompilerPlugin(snVersion, scalaVersion)

    val pluginJar = pluginJars
      .find(_.getFileName.toString.contains("nscplugin"))
      .getOrElse(pluginJars.head)

    val scalaOptions: List[String] = List(s"-Xplugin:${pluginJar.toAbsolutePath}")

    val language: ProjectLanguage.ScalaJava = ProjectLanguage.ScalaJava(
      scalaVersion = scalaVersion,
      scalaOptions = scalaOptions,
      javaOptions = Nil
    )

    val fullClasspath = (scalaLibJars ++ snLibJars ++ extraDeps).distinct

    val config = ProjectConfig(
      name = "scala-native-compile",
      sources = Set(srcDir),
      classpath = fullClasspath,
      outputDir = outDir,
      language = language,
      analysisDir = None,
      buildDir = outDir.getParent
    )

    val result = ZincBridge
      .compile(
        config,
        language,
        DiagnosticListener.noop,
        CancellationToken.never,
        Map.empty,
        ProgressListener.noop
      )
      .unsafeRunSync()

    result match {
      case ProjectCompileSuccess(_, _, _)  => Seq(outDir) ++ fullClasspath
      case f: ProjectCompileFailure        => throw new RuntimeException(s"Scala Native compilation failed: ${f.errors.map(_.formatted).mkString("\n")}")
      case ProjectCompileCancelled(reason) => throw new RuntimeException(s"Scala Native compilation cancelled: $reason")
    }
  }
}
