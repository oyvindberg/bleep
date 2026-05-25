package bleep
package commands

import bleep.internal.FileUtils
import com.typesafe.config.ConfigFactory

import java.nio.file.{FileSystems, Files, Path}
import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters.StreamHasToScala

object Fmt {

  def findSourceFiles(dirs: Set[Path], extension: String): List[Path] =
    dirs.toList.flatMap { dir =>
      if (Files.exists(dir)) {
        Files.walk(dir).toScala(List).filter { path =>
          Files.isRegularFile(path) && path.toString.endsWith(extension)
        }
      } else Nil
    }

  /** Scala formatting support via scalafmt */
  object ScalaFmt {
    val defaultConfig: String =
      """version=3.11.1
        |maxColumn = 160
        |runner.dialect = scala213
        |""".stripMargin

    def getVersion(configStr: String): Option[String] =
      configStr
        .lines()
        .map(_.split("=").map(_.trim))
        .toScala(List)
        .collectFirst { case Array("version", version) => version.stripPrefix("\"").stripSuffix("\"") }
  }

  /** Kotlin formatting support via ktfmt */
  object KotlinFmt {

    /** Configuration for Kotlin code formatting via ktfmt.
      *
      * @param enabled
      *   Whether Kotlin formatting is enabled
      * @param version
      *   Version of ktfmt to use
      * @param style
      *   Formatting style: "kotlinlang" (official Kotlin coding conventions), "google" (Android/Google), or "meta" (Facebook/ktfmt default)
      * @param excludePaths
      *   Glob patterns for files to exclude from formatting
      */
    case class KotlinFmtConfig(
        enabled: Boolean,
        version: String,
        style: String,
        excludePaths: List[String]
    )

    object KotlinFmtConfig {
      val default: KotlinFmtConfig = KotlinFmtConfig(
        enabled = true,
        version = FetchKtfmt.DefaultVersion,
        style = "kotlinlang",
        excludePaths = Nil
      )
    }

    private val defaultHocon: String =
      s"""|enabled = true
          |version = "${FetchKtfmt.DefaultVersion}"
          |style = "kotlinlang"
          |excludePaths = []
          |""".stripMargin

    /** Parses a .kotlinfmt.conf file using HOCON and returns a KotlinFmtConfig. */
    def parseConfig(configStr: String): KotlinFmtConfig = {
      val hocon = ConfigFactory.parseString(configStr).withFallback(ConfigFactory.parseString(defaultHocon)).resolve()

      KotlinFmtConfig(
        enabled = hocon.getBoolean("enabled"),
        version = hocon.getString("version"),
        style = hocon.getString("style"),
        excludePaths = hocon.getStringList("excludePaths").asScala.toList
      )
    }

    /** Reads and parses .kotlinfmt.conf from the build directory, returning default config if not found. */
    def getConfig(buildDir: Path): KotlinFmtConfig = {
      val configPath = buildDir.resolve(".kotlinfmt.conf")
      if (FileUtils.exists(configPath)) {
        parseConfig(Files.readString(configPath))
      } else {
        KotlinFmtConfig.default
      }
    }

    /** Builds CLI flags for ktfmt based on config. */
    def buildFlags(config: KotlinFmtConfig): List[String] =
      config.style.toLowerCase match {
        case "kotlinlang" => List("--kotlinlang-style")
        case "google"     => List("--google-style")
        case "meta"       => Nil
        case other        => throw new BleepException.Text(s"Unknown ktfmt style '$other'. Valid: kotlinlang, google, meta")
      }
  }

  /** Java formatting support via google-java-format */
  object JavaFmt {

    /** Configuration for Java code formatting via google-java-format.
      *
      * @param enabled
      *   Whether Java formatting is enabled
      * @param version
      *   Version of google-java-format to use
      * @param style
      *   Formatting style: "google" (2-space indent) or "aosp" (4-space indent)
      * @param skipSortingImports
      *   Don't sort import statements
      * @param skipRemovingUnusedImports
      *   Keep unused import statements
      * @param fixImportsOnly
      *   Only fix imports, don't reformat code
      * @param skipReflowingLongStrings
      *   Don't reformat long strings
      * @param skipJavadocFormatting
      *   Don't reformat Javadoc comments
      * @param excludePaths
      *   Glob patterns for files to exclude from formatting
      */
    case class JavaFmtConfig(
        enabled: Boolean,
        version: String,
        style: String,
        skipSortingImports: Boolean,
        skipRemovingUnusedImports: Boolean,
        fixImportsOnly: Boolean,
        skipReflowingLongStrings: Boolean,
        skipJavadocFormatting: Boolean,
        excludePaths: List[String]
    )

    object JavaFmtConfig {
      val default: JavaFmtConfig = JavaFmtConfig(
        enabled = true,
        version = FetchGoogleJavaFormat.DefaultVersion,
        style = "google",
        skipSortingImports = false,
        skipRemovingUnusedImports = false,
        fixImportsOnly = false,
        skipReflowingLongStrings = false,
        skipJavadocFormatting = false,
        excludePaths = Nil
      )
    }

    private val defaultHocon: String =
      s"""|enabled = true
          |version = "${FetchGoogleJavaFormat.DefaultVersion}"
          |style = "google"
          |skipSortingImports = false
          |skipRemovingUnusedImports = false
          |fixImportsOnly = false
          |skipReflowingLongStrings = false
          |skipJavadocFormatting = false
          |excludePaths = []
          |""".stripMargin

    /** Parses a .javafmt.conf file using HOCON and returns a JavaFmtConfig. */
    def parseConfig(configStr: String): JavaFmtConfig = {
      val hocon = ConfigFactory.parseString(configStr).withFallback(ConfigFactory.parseString(defaultHocon)).resolve()

      JavaFmtConfig(
        enabled = hocon.getBoolean("enabled"),
        version = hocon.getString("version"),
        style = hocon.getString("style"),
        skipSortingImports = hocon.getBoolean("skipSortingImports"),
        skipRemovingUnusedImports = hocon.getBoolean("skipRemovingUnusedImports"),
        fixImportsOnly = hocon.getBoolean("fixImportsOnly"),
        skipReflowingLongStrings = hocon.getBoolean("skipReflowingLongStrings"),
        skipJavadocFormatting = hocon.getBoolean("skipJavadocFormatting"),
        excludePaths = hocon.getStringList("excludePaths").asScala.toList
      )
    }

    /** Reads and parses .javafmt.conf from the build directory, returning default config if not found. */
    def getConfig(buildDir: Path): JavaFmtConfig = {
      val configPath = buildDir.resolve(".javafmt.conf")
      if (FileUtils.exists(configPath)) {
        parseConfig(Files.readString(configPath))
      } else {
        JavaFmtConfig.default
      }
    }

    /** Checks if a path matches any of the exclusion glob patterns. */
    def matchesExcludePattern(path: Path, buildDir: Path, patterns: List[String]): Boolean = {
      val fs = FileSystems.getDefault
      val relativePath = buildDir.relativize(path)
      patterns.exists { pattern =>
        val globPattern = pattern.stripPrefix("glob:")
        val matcher = fs.getPathMatcher(s"glob:$globPattern")
        matcher.matches(relativePath) || matcher.matches(path)
      }
    }

    /** Builds CLI flags for google-java-format based on config. */
    def buildFlags(config: JavaFmtConfig): List[String] = {
      val flags = List.newBuilder[String]

      if (config.style.toLowerCase == "aosp") {
        flags += "--aosp"
      }
      if (config.skipSortingImports) {
        flags += "--skip-sorting-imports"
      }
      if (config.skipRemovingUnusedImports) {
        flags += "--skip-removing-unused-imports"
      }
      if (config.fixImportsOnly) {
        flags += "--fix-imports-only"
      }
      if (config.skipReflowingLongStrings) {
        flags += "--skip-reflowing-long-strings"
      }
      if (config.skipJavadocFormatting) {
        flags += "--skip-javadoc-formatting"
      }

      flags.result()
    }
  }
}

case class Fmt(check: Boolean, projects: Array[model.CrossProjectName]) extends BleepBuildCommand {
  override def run(started: Started): Either[BleepException, Unit] = {
    val selectedProjects =
      if (projects.isEmpty) started.build.explodedProjects.keys
      else projects.toSeq

    val sourcesDirs: Set[Path] =
      selectedProjects
        .flatMap { crossName =>
          val sourcesDirs = started.projectPaths(crossName).sourcesDirs
          sourcesDirs.fromSourceLayout ++ sourcesDirs.fromJson.values
        }
        .toSet
        .filter(Files.exists(_))

    val scalaFiles = Fmt.findSourceFiles(sourcesDirs, ".scala")
    val javaFiles = Fmt.findSourceFiles(sourcesDirs, ".java")
    val kotlinFiles = Fmt.findSourceFiles(sourcesDirs, ".kt")

    def tryFormat(name: String, run: () => Unit): Option[BleepException] =
      scala.util.Try(run()).failed.toOption.map {
        case e: BleepException => e
        case e                 => new BleepException.Text(s"$name: ${e.getMessage}")
      }

    val scalaResult = if (scalaFiles.nonEmpty) tryFormat("Scala", () => formatScala(started, sourcesDirs)) else None
    val javaResult = if (javaFiles.nonEmpty) tryFormat("Java", () => formatJava(started, javaFiles)) else None
    val kotlinResult = if (kotlinFiles.nonEmpty) tryFormat("Kotlin", () => formatKotlin(started, kotlinFiles)) else None

    val errors = List(scalaResult, javaResult, kotlinResult).flatten
    errors match {
      case Nil        => Right(())
      case err :: Nil => Left(err)
      case multiple   => Left(new BleepException.Text(s"Formatting failed:\n${multiple.map(e => s"- ${e.getMessage}").mkString("\n")}"))
    }
  }

  private def formatScala(started: Started, sourcesDirs: Set[Path]): Unit = {
    val configPath = started.buildPaths.buildDir / ".scalafmt.conf"

    val configStr: String =
      if (FileUtils.exists(configPath)) {
        Files.readString(configPath)
      } else {
        FileUtils.writeString(started.logger, Some("Creating example scalafmt configuration"), configPath, Fmt.ScalaFmt.defaultConfig)
        Fmt.ScalaFmt.defaultConfig
      }

    val version = Fmt.ScalaFmt
      .getVersion(configStr)
      .getOrElse(throw new BleepException.Text(s"Couldn't naively extract scalafmt version from $configPath"))

    val scalafmt = FetchScalafmt(started.pre.cacheLogger, started.executionContext, version)

    started.logger.withContext("scalafmt", scalafmt).debug("Using scalafmt")

    val cmd =
      List(scalafmt.toString, "-c", configPath.toString, "--non-interactive") ++
        (if (check) List("--test") else Nil) ++
        sourcesDirs.map(_.toString)

    cli(
      "scalafmt",
      started.buildPaths.cwd,
      cmd,
      logger = started.logger,
      out = cli.Out.ViaLogger(started.logger)
    ).discard()
  }

  private def formatJava(started: Started, javaFiles: List[Path]): Unit = {
    val config = Fmt.JavaFmt.getConfig(started.buildPaths.buildDir)

    if (!config.enabled) {
      started.logger.debug("Java formatting disabled via .javafmt.conf")
    } else {
      // Filter files based on exclude patterns from config
      val filteredFiles = if (config.excludePaths.nonEmpty) {
        javaFiles.filterNot(path => Fmt.JavaFmt.matchesExcludePattern(path, started.buildPaths.buildDir, config.excludePaths))
      } else {
        javaFiles
      }

      if (filteredFiles.isEmpty) {
        started.logger.info("No Java files to format after applying exclusions")
      } else {
        started.logger.info(s"Using google-java-format version ${config.version}")

        val googleJavaFormat = FetchGoogleJavaFormat(started.pre.cacheLogger, started.executionContext, config.version)

        started.logger
          .withContext("google-java-format", googleJavaFormat)
          .withContext("style", config.style)
          .debug("Using google-java-format")

        val javaCmd = started.jvmCommand.toString

        val jvmFlags = List(
          "--add-exports=jdk.compiler/com.sun.tools.javac.api=ALL-UNNAMED",
          "--add-exports=jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED",
          "--add-exports=jdk.compiler/com.sun.tools.javac.file=ALL-UNNAMED",
          "--add-exports=jdk.compiler/com.sun.tools.javac.parser=ALL-UNNAMED",
          "--add-exports=jdk.compiler/com.sun.tools.javac.tree=ALL-UNNAMED",
          "--add-exports=jdk.compiler/com.sun.tools.javac.util=ALL-UNNAMED"
        )

        val configFlags = Fmt.JavaFmt.buildFlags(config)
        val modeFlags = if (check) List("--dry-run", "--set-exit-if-changed") else List("--replace")

        val cmd =
          List(javaCmd) ++ jvmFlags ++ List("-jar", googleJavaFormat.toString) ++
            configFlags ++ modeFlags ++ filteredFiles.map(_.toString)

        cli(
          "google-java-format",
          started.buildPaths.cwd,
          cmd,
          logger = started.logger,
          out = cli.Out.ViaLogger(started.logger)
        ).discard()
      }
    }
  }

  private def formatKotlin(started: Started, kotlinFiles: List[Path]): Unit = {
    val config = Fmt.KotlinFmt.getConfig(started.buildPaths.buildDir)

    if (!config.enabled) {
      started.logger.debug("Kotlin formatting disabled via .kotlinfmt.conf")
    } else {
      val filteredFiles = if (config.excludePaths.nonEmpty) {
        kotlinFiles.filterNot(path => Fmt.JavaFmt.matchesExcludePattern(path, started.buildPaths.buildDir, config.excludePaths))
      } else {
        kotlinFiles
      }

      if (filteredFiles.isEmpty) {
        started.logger.info("No Kotlin files to format after applying exclusions")
      } else {
        started.logger.info(s"Using ktfmt version ${config.version}")

        val ktfmt = FetchKtfmt(started.pre.cacheLogger, started.executionContext, config.version)

        started.logger
          .withContext("ktfmt", ktfmt)
          .withContext("style", config.style)
          .debug("Using ktfmt")

        val javaCmd = started.jvmCommand.toString

        val configFlags = Fmt.KotlinFmt.buildFlags(config)
        val modeFlags = if (check) List("--dry-run", "--set-exit-if-changed") else Nil

        val cmd =
          List(javaCmd, "-jar", ktfmt.toString) ++
            configFlags ++ modeFlags ++ filteredFiles.map(_.toString)

        cli(
          "ktfmt",
          started.buildPaths.cwd,
          cmd,
          logger = started.logger,
          out = cli.Out.ViaLogger(started.logger)
        ).discard()
      }
    }
  }
}
