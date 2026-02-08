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
      javaRelease = None
    )

    val config = ProjectConfig(
      name = "scalajs-compile",
      sources = Set(srcDir),
      classpath = scalaLibJars ++ sjsLibJars,
      outputDir = outDir,
      language = language,
      analysisDir = None
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
      case ProjectCompileSuccess(_, _, _) => Seq(outDir) ++ scalaLibJars ++ sjsLibJars
      case f: ProjectCompileFailure       => throw new RuntimeException(s"Scala.js compilation failed: ${f.errors.map(_.formatted).mkString("\n")}")
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
      javaRelease = None
    )

    val fullClasspath = (scalaLibJars ++ snLibJars ++ extraDeps).distinct

    val config = ProjectConfig(
      name = "scala-native-compile",
      sources = Set(srcDir),
      classpath = fullClasspath,
      outputDir = outDir,
      language = language,
      analysisDir = None
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
      case ProjectCompileSuccess(_, _, _) => Seq(outDir) ++ fullClasspath
      case f: ProjectCompileFailure       => throw new RuntimeException(s"Scala Native compilation failed: ${f.errors.map(_.formatted).mkString("\n")}")
    }
  }
}
