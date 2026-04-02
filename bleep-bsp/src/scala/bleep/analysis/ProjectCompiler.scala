package bleep.analysis

import cats.effect.IO
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Project-level compiler abstraction.
  *
  * Unlike the previous file-level approach, this operates at the project granularity:
  *   - A project is compiled as a unit (all sources together)
  *   - Zinc handles Scala/Java incremental compilation internally
  *   - Kotlin compiler handles Kotlin incremental compilation internally
  *   - Cross-language dependencies happen only at project boundaries (via classpath)
  */

/** Language mode for a project */
enum ProjectLanguage {

  /** Scala and Java compiled together by Zinc */
  case ScalaJava(scalaVersion: String, scalaOptions: List[String], javaRelease: Option[Int], javaOptions: List[String] = Nil)

  /** Kotlin/JVM compiled by K2JVMCompiler */
  case Kotlin(kotlinVersion: String, jvmTarget: String, kotlinOptions: List[String])

  /** Kotlin/JS compiled by K2JSCompiler
    *
    * @param kotlinVersion
    *   Kotlin compiler version
    * @param kotlinOptions
    *   additional compiler options
    * @param isTest
    *   whether this is a test project (affects library resolution)
    */
  case KotlinJs(kotlinVersion: String, kotlinOptions: List[String], isTest: Boolean)

  /** Kotlin/Native compiled by K2Native
    *
    * @param kotlinVersion
    *   Kotlin compiler version
    * @param kotlinOptions
    *   additional compiler options
    * @param isTest
    *   whether this is a test project (affects library resolution)
    */
  case KotlinNative(kotlinVersion: String, kotlinOptions: List[String], isTest: Boolean)

  /** Java-only compiled by javac or ECJ.
    * @param release
    *   Java release version (--release flag)
    * @param javaOptions
    *   additional javac options
    * @param ecjVersion
    *   if set, use ECJ (Eclipse Compiler for Java) instead of javac
    */
  case JavaOnly(release: Option[Int], javaOptions: List[String], ecjVersion: Option[String])
}

/** Configuration for compiling a project */
case class ProjectConfig(
    name: String,
    sources: Set[Path],
    classpath: Seq[Path],
    outputDir: Path,
    language: ProjectLanguage,
    analysisDir: Option[Path]
)

/** Result of compiling a project */
sealed trait ProjectCompileResult {
  def isSuccess: Boolean
}

case class ProjectCompileSuccess(
    outputDir: Path,
    classFiles: Set[Path],
    analysisFile: Option[Path]
) extends ProjectCompileResult {
  def isSuccess: Boolean = true
}

case class ProjectCompileFailure(
    errors: List[CompilerError]
) extends ProjectCompileResult {
  def isSuccess: Boolean = false
}

/** Trait for compiling a single project.
  *
  * Implementations:
  *   - ZincProjectCompiler for Scala/Java
  *   - KotlinProjectCompiler for Kotlin
  *   - JavacProjectCompiler for Java-only
  */
trait ProjectCompiler {

  /** Compile a project with the given configuration.
    *
    * @param config
    *   project configuration
    * @param diagnosticListener
    *   receives compilation diagnostics
    * @param cancellationToken
    *   for cancellation support
    * @param dependencyAnalyses
    *   analysis files from dependent projects (keyed by output dir)
    * @param progressListener
    *   receives compilation progress updates
    * @return
    *   compilation result (success or failure with errors)
    */
  def compile(
      config: ProjectConfig,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken,
      dependencyAnalyses: Map[Path, Path],
      progressListener: ProgressListener
  ): IO[ProjectCompileResult]

  /** Check if this compiler can handle the given language mode */
  def supports(language: ProjectLanguage): Boolean
}

object ProjectCompiler {

  /** Create a compiler appropriate for the project's language */
  def forLanguage(language: ProjectLanguage): ProjectCompiler = language match {
    case _: ProjectLanguage.ScalaJava    => ZincProjectCompiler
    case _: ProjectLanguage.Kotlin       => KotlinProjectCompiler
    case _: ProjectLanguage.KotlinJs     => KotlinJsProjectCompiler
    case _: ProjectLanguage.KotlinNative => KotlinNativeProjectCompiler
    case _: ProjectLanguage.JavaOnly     => JavacProjectCompiler
  }
}

/** Zinc-based compiler for Scala/Java projects.
  *
  * Uses sbt's Zinc for incremental compilation with these features:
  *   - Name hashing for precise invalidation
  *   - API change tracking
  *   - Analysis persistence across sessions
  */
object ZincProjectCompiler extends ProjectCompiler {
  def supports(language: ProjectLanguage): Boolean = language match {
    case _: ProjectLanguage.ScalaJava => true
    case _                            => false
  }

  def compile(
      config: ProjectConfig,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken,
      dependencyAnalyses: Map[Path, Path],
      progressListener: ProgressListener
  ): IO[ProjectCompileResult] =
    config.language match {
      case sl: ProjectLanguage.ScalaJava =>
        ZincBridge.compile(config, sl, diagnosticListener, cancellationToken, dependencyAnalyses, progressListener)
      case other =>
        IO.raiseError(new IllegalArgumentException(s"ZincProjectCompiler cannot compile $other"))
    }
}

/** Kotlin compiler for Kotlin projects.
  *
  * Uses JetBrains Kotlin compiler with:
  *   - Built-in incremental compilation
  *   - JVM target configuration
  */
object KotlinProjectCompiler extends ProjectCompiler {
  def supports(language: ProjectLanguage): Boolean = language match {
    case _: ProjectLanguage.Kotlin => true
    case _                         => false
  }

  def compile(
      config: ProjectConfig,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken,
      dependencyAnalyses: Map[Path, Path],
      progressListener: ProgressListener
  ): IO[ProjectCompileResult] =
    // Kotlin compiler doesn't have progress callbacks - ignore progressListener
    config.language match {
      case kt: ProjectLanguage.Kotlin =>
        compileKotlin(config, kt, diagnosticListener, cancellationToken)
      case other =>
        IO.raiseError(new IllegalArgumentException(s"KotlinProjectCompiler cannot compile $other"))
    }

  private def compileKotlin(
      config: ProjectConfig,
      kt: ProjectLanguage.Kotlin,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken
  ): IO[ProjectCompileResult] = IO.blocking {
    Files.createDirectories(config.outputDir)

    val kotlinConfig = KotlinConfig(kt.kotlinVersion, kt.jvmTarget, kt.kotlinOptions)

    val sourceFiles = ZincBridge.removeNestedDirs(config.sources).toSeq.flatMap { srcDir =>
      if (Files.isDirectory(srcDir)) {
        scala.util
          .Using(Files.walk(srcDir)) { stream =>
            stream
              .iterator()
              .asScala
              .filter(p => p.toString.endsWith(".kt") || p.toString.endsWith(".kts") || p.toString.endsWith(".java"))
              .map(p => SourceFile(p, Files.readString(p)))
              .toSeq
          }
          .getOrElse(Seq.empty)
      } else if (Files.exists(srcDir)) {
        Seq(SourceFile(srcDir, Files.readString(srcDir)))
      } else {
        Seq.empty
      }
    }

    if (sourceFiles.isEmpty) {
      ProjectCompileSuccess(config.outputDir, Set.empty, None)
    } else {
      // === FAST PATH: Check if up-to-date before expensive compilation ===
      // Quick check: any .class file exists?
      val hasClassFiles = scala.util
        .Using(Files.walk(config.outputDir)) { stream =>
          stream.iterator().asScala.exists(_.toString.endsWith(".class"))
        }
        .getOrElse(false)

      if (hasClassFiles) {
        // Now collect class files for timestamp comparison
        val classFiles = scala.util
          .Using(Files.walk(config.outputDir)) { stream =>
            stream.iterator().asScala.filter(_.toString.endsWith(".class")).toSeq
          }
          .getOrElse(Seq.empty)

        val outputModTime = classFiles.map(f => Files.getLastModifiedTime(f).toMillis).max

        // Check if any source is newer than output
        val newerSources = sourceFiles
          .filter { sf =>
            Files.getLastModifiedTime(sf.path).toMillis > outputModTime
          }
          .map(_.path)

        // Check if any classpath entry is newer than output
        val newerDeps = config.classpath.filter { dep =>
          try
            Files.exists(dep) && Files.getLastModifiedTime(dep).toMillis > outputModTime
          catch {
            case _: Exception => true
          }
        }.toSeq

        if (newerSources.isEmpty && newerDeps.isEmpty) {
          // Up to date!
          System.err.println(
            s"[KotlinFastPath] ${config.name}: UpToDate (${classFiles.size} class files, outputModTime=${java.time.Instant.ofEpochMilli(outputModTime)})"
          )
          diagnosticListener.onCompilationReason(config.name, CompilationReason.UpToDate)
          ProjectCompileSuccess(config.outputDir, classFiles.toSet, None)
        } else {
          System.err.println(s"[KotlinFastPath] ${config.name}: Incremental — ${newerSources.size} newer sources, ${newerDeps.size} newer deps")
          newerSources.take(5).foreach(s => System.err.println(s"  source: $s (mtime=${Files.getLastModifiedTime(s)})"))
          newerDeps.take(5).foreach(d => System.err.println(s"  dep: $d (mtime=${Files.getLastModifiedTime(d)})"))
          System.err.println(s"  outputModTime=${java.time.Instant.ofEpochMilli(outputModTime)}")
          val reason = CompilationReason.Incremental(sourceFiles.length, newerSources, newerDeps)
          diagnosticListener.onCompilationReason(config.name, reason)
          doKotlinCompile(sourceFiles, config, kotlinConfig, diagnosticListener, cancellationToken)
        }
      } else {
        System.err.println(s"[KotlinFastPath] ${config.name}: CleanBuild (no class files in ${config.outputDir})")
        val reason = CompilationReason.CleanBuild
        diagnosticListener.onCompilationReason(config.name, reason)
        doKotlinCompile(sourceFiles, config, kotlinConfig, diagnosticListener, cancellationToken)
      }
    }
  }

  /** Compile a Kotlin project with Java-first order.
    *
    * kotlinc does NOT compile .java files — it only uses them for type resolution. So for mixed Java+Kotlin projects we use Java-first compilation order:
    *   1. Compile .java files with javac (output to project output dir)
    *   2. Compile .kt/.kts files with kotlinc (with Java class output on classpath)
    *
    * This matches the default compile order for Scala projects (JavaThenScala).
    */
  private def doKotlinCompile(
      sourceFiles: Seq[SourceFile],
      config: ProjectConfig,
      kotlinConfig: KotlinConfig,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken
  ): ProjectCompileResult = {
    val javaSources = sourceFiles.filter(_.path.toString.endsWith(".java"))
    val kotlinSources = sourceFiles.filterNot(_.path.toString.endsWith(".java"))

    // Phase 1: Compile Java sources with javac to a separate directory.
    // We use a separate temp directory (NOT under outputDir) because the Kotlin incremental
    // compiler may clean the output directory on full compiles.
    val javaClassesDir = if (javaSources.nonEmpty) {
      val javaDir = Files.createTempDirectory("kotlin-project-java-classes-")
      val javaConfig = config.copy(outputDir = javaDir)
      val javaResult = compileJavaSources(javaSources, javaConfig, diagnosticListener)
      javaResult match {
        case failure: ProjectCompileFailure => return failure
        case _                              => // continue to Kotlin
      }
      Some(javaDir)
    } else None

    if (kotlinSources.isEmpty) {
      // Copy Java classes to main output dir
      javaClassesDir.foreach(jd => copyClassFiles(jd, config.outputDir))
      val allClasses = Compiler.collectClassFilesStatic(config.outputDir)
      return ProjectCompileSuccess(config.outputDir, allClasses, None)
    }

    // Phase 2: Compile Kotlin sources with kotlinc (Java classes dir on classpath)
    val kotlinClasspath = javaClassesDir.fold(config.classpath)(jd => config.classpath :+ jd)
    val input = CompilationInput(kotlinSources, kotlinClasspath, config.outputDir, kotlinConfig)
    KotlinSourceCompiler.compile(input, diagnosticListener, cancellationToken) match {
      case CompilationSuccess(outDir, _) =>
        // Copy Java classes into the main output dir alongside Kotlin classes
        javaClassesDir.foreach(jd => copyClassFiles(jd, outDir))
        val allClasses = Compiler.collectClassFilesStatic(outDir)
        ProjectCompileSuccess(outDir, allClasses, None)
      case CompilationFailure(errs) =>
        ProjectCompileFailure(errs)
      case CompilationCancelled =>
        ProjectCompileFailure(List(CompilerError(None, 0, 0, "Compilation cancelled", None, CompilerError.Severity.Error)))
    }
  }

  /** Compile .java files using javac, outputting to the project's output directory. */
  private def compileJavaSources(
      javaSources: Seq[SourceFile],
      config: ProjectConfig,
      diagnosticListener: DiagnosticListener
  ): ProjectCompileResult = {
    val javac = javax.tools.ToolProvider.getSystemJavaCompiler
    val diagnosticCollector = new javax.tools.DiagnosticCollector[javax.tools.JavaFileObject]()
    val fileManager = javac.getStandardFileManager(diagnosticCollector, null, null)

    try {
      val tempDir = Files.createTempDirectory("kotlin-java-compile-")
      try {
        val javaFiles = javaSources.map { sf =>
          val target = tempDir.resolve(sf.path)
          Files.createDirectories(target.getParent)
          Files.writeString(target, sf.content)
          target
        }

        Files.createDirectories(config.outputDir)
        val compilationUnits = fileManager.getJavaFileObjectsFromPaths(javaFiles.asJava)
        val fullClasspath = config.classpath.map(_.toString).mkString(java.io.File.pathSeparator)
        val options = java.util.List.of("-d", config.outputDir.toString, "-classpath", fullClasspath)

        val task = javac.getTask(null, fileManager, diagnosticCollector, options, null, compilationUnits)
        val success = task.call()

        if (success) {
          val classFiles = Compiler.collectClassFilesStatic(config.outputDir)
          ProjectCompileSuccess(config.outputDir, classFiles, None)
        } else {
          val errors = diagnosticCollector.getDiagnostics.asScala.toList
            .filter(_.getKind == javax.tools.Diagnostic.Kind.ERROR)
            .map { d =>
              val path = Option(d.getSource).map(s => Path.of(s.toUri))
              CompilerError(
                path,
                d.getLineNumber.toInt,
                d.getColumnNumber.toInt,
                d.getMessage(null),
                None,
                CompilerError.Severity.Error
              )
            }
          errors.foreach(diagnosticListener.onDiagnostic)
          ProjectCompileFailure(errors)
        }
      } finally {
        import scala.util.Using
        import scala.jdk.StreamConverters.*
        Using(Files.walk(tempDir)) { stream =>
          stream
            .toScala(LazyList)
            .sorted(Ordering[String].reverse.on[Path](_.toString))
            .foreach(p => scala.util.Try(Files.delete(p)))
        }
      }
    } finally fileManager.close()
  }

  /** Copy .class files from source dir to target dir, preserving package directory structure. */
  private def copyClassFiles(sourceDir: Path, targetDir: Path): Unit = {
    import scala.jdk.StreamConverters.*
    import scala.util.Using
    Using(Files.walk(sourceDir)) { stream =>
      stream
        .toScala(LazyList)
        .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".class"))
        .foreach { classFile =>
          val relativePath = sourceDir.relativize(classFile)
          val target = targetDir.resolve(relativePath)
          Files.createDirectories(target.getParent)
          Files.copy(classFile, target, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
        }
    }
  }
}

/** Java-only compiler using Zinc for incremental compilation.
  *
  * Even for pure Java projects, Zinc provides incremental compilation benefits:
  *   - Dependency tracking between Java files
  *   - Only recompiles files affected by changes
  *   - Analysis persistence across sessions
  *
  * This uses a minimal Scala 3 instance just for Zinc infrastructure, but all actual compilation is done by javac (or ECJ if specified).
  */
object JavacProjectCompiler extends ProjectCompiler {
  // Use Scala 3.8.3 to match bleep-bsp (from template-scala-3)
  private val zincScalaVersion = "3.8.3"

  def supports(language: ProjectLanguage): Boolean = language match {
    case _: ProjectLanguage.JavaOnly => true
    case _                           => false
  }

  def compile(
      config: ProjectConfig,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken,
      dependencyAnalyses: Map[Path, Path],
      progressListener: ProgressListener
  ): IO[ProjectCompileResult] =
    config.language match {
      case javaLang: ProjectLanguage.JavaOnly =>
        // Convert to ScalaJava with empty Scala options - Zinc will only compile .java files
        val scalaJavaLang: ProjectLanguage.ScalaJava = ProjectLanguage.ScalaJava(
          scalaVersion = zincScalaVersion,
          scalaOptions = List.empty,
          javaRelease = javaLang.release,
          javaOptions = javaLang.javaOptions
        )
        val javaConfig = config.copy(language = scalaJavaLang)
        ZincBridge.compile(
          javaConfig,
          scalaJavaLang,
          diagnosticListener,
          cancellationToken,
          dependencyAnalyses,
          progressListener,
          ecjVersion = javaLang.ecjVersion
        )
      case other =>
        IO.raiseError(new IllegalArgumentException(s"JavacProjectCompiler cannot compile $other"))
    }
}

/** Kotlin/JS compiler for Kotlin projects targeting JavaScript.
  *
  * Uses the K2JSCompiler to produce JavaScript output. For test projects, includes kotlin-test-js library automatically.
  */
object KotlinJsProjectCompiler extends ProjectCompiler {
  def supports(language: ProjectLanguage): Boolean = language match {
    case _: ProjectLanguage.KotlinJs => true
    case _                           => false
  }

  def compile(
      config: ProjectConfig,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken,
      dependencyAnalyses: Map[Path, Path],
      progressListener: ProgressListener
  ): IO[ProjectCompileResult] =
    config.language match {
      case kt: ProjectLanguage.KotlinJs =>
        compileKotlinJs(config, kt, diagnosticListener, cancellationToken)
      case other =>
        IO.raiseError(new IllegalArgumentException(s"KotlinJsProjectCompiler cannot compile $other"))
    }

  private def compileKotlinJs(
      config: ProjectConfig,
      kt: ProjectLanguage.KotlinJs,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken
  ): IO[ProjectCompileResult] = {
    // Collect source files
    val sourceFiles = config.sources.toSeq.flatMap { srcDir =>
      if (Files.isDirectory(srcDir)) {
        scala.util
          .Using(Files.walk(srcDir)) { stream =>
            stream
              .iterator()
              .asScala
              .filter(p => p.toString.endsWith(".kt") || p.toString.endsWith(".kts"))
              .toSeq
          }
          .getOrElse(Seq.empty)
      } else if (Files.exists(srcDir)) {
        Seq(srcDir)
      } else {
        Seq.empty
      }
    }

    if (sourceFiles.isEmpty) {
      IO.pure(ProjectCompileSuccess(config.outputDir, Set.empty, None))
    } else {
      // === FAST PATH: Check if up-to-date before expensive compilation ===
      // Quick check: any .klib or .js file exists?
      val hasOutputFiles = Files.isDirectory(config.outputDir) && scala.util
        .Using(Files.list(config.outputDir)) { stream =>
          stream.iterator().asScala.exists(p => p.toString.endsWith(".klib") || p.toString.endsWith(".js"))
        }
        .getOrElse(false)

      if (hasOutputFiles) {
        // Now collect output files for timestamp comparison
        val outputFiles = scala.util
          .Using(Files.list(config.outputDir)) { stream =>
            stream
              .iterator()
              .asScala
              .filter(p => p.toString.endsWith(".klib") || p.toString.endsWith(".js"))
              .toSeq
          }
          .getOrElse(Seq.empty)

        val outputModTime = outputFiles.map(f => Files.getLastModifiedTime(f).toMillis).max

        // Check if any source is newer than output
        val newerSources = sourceFiles.filter { src =>
          try
            Files.getLastModifiedTime(src).toMillis > outputModTime
          catch {
            case _: Exception => true
          }
        }

        // Check if any classpath entry is newer than output
        val newerDeps = config.classpath.filter { dep =>
          try
            Files.exists(dep) && Files.getLastModifiedTime(dep).toMillis > outputModTime
          catch {
            case _: Exception => true
          }
        }.toSeq

        if (newerSources.isEmpty && newerDeps.isEmpty) {
          // Up to date!
          diagnosticListener.onCompilationReason(config.name, CompilationReason.UpToDate)
          IO.pure(ProjectCompileSuccess(config.outputDir, outputFiles.toSet, None))
        } else {
          val reason = CompilationReason.Incremental(sourceFiles.length, newerSources, newerDeps)
          diagnosticListener.onCompilationReason(config.name, reason)
          doKotlinJsCompile(sourceFiles, config, kt, diagnosticListener, cancellationToken)
        }
      } else {
        val reason = if (Files.isDirectory(config.outputDir)) CompilationReason.EmptyOutput else CompilationReason.CleanBuild
        diagnosticListener.onCompilationReason(config.name, reason)
        doKotlinJsCompile(sourceFiles, config, kt, diagnosticListener, cancellationToken)
      }
    }
  }

  private def doKotlinJsCompile(
      sourceFiles: Seq[Path],
      config: ProjectConfig,
      kt: ProjectLanguage.KotlinJs,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken
  ): IO[ProjectCompileResult] = {
    // Resolve Kotlin/JS stdlib library
    val stdlibLibs = CompilerResolver.resolveKotlinJsLibrary(kt.kotlinVersion)

    // For test projects, also resolve kotlin-test-js
    val testLibs =
      if (kt.isTest) CompilerResolver.resolveKotlinTestJs(kt.kotlinVersion)
      else Seq.empty

    // For Kotlin/JS, find KLIB files from classpath entries
    // KLIB files are the native format for Kotlin/JS and Kotlin/Native
    // The classpath may contain:
    // - Direct KLIB files (external dependencies)
    // - Directories containing KLIB files (project dependencies)
    // - JVM JARs (which we need to filter out)
    val klibsFromClasspath = config.classpath.flatMap { p =>
      val name = p.getFileName.toString
      if (name.endsWith(".klib")) {
        // Direct KLIB file
        Seq(p)
      } else if (name.contains("-js-") && name.endsWith(".jar")) {
        // JS-specific JAR (might be acceptable for some libraries)
        Seq(p)
      } else if (Files.isDirectory(p)) {
        // Check if directory contains KLIB files (project dependency outputs)
        try
          scala.util
            .Using(Files.list(p)) { stream =>
              stream
                .iterator()
                .asScala
                .filter(_.toString.endsWith(".klib"))
                .toSeq
            }
            .getOrElse(Seq.empty)
        catch {
          case _: Exception => Seq.empty
        }
      } else {
        // Other files (JVM JARs etc) - filter out
        Seq.empty
      }
    }

    // Combined libraries: klib deps + stdlib + test libs (if test project)
    val allLibraries = klibsFromClasspath ++ stdlibLibs ++ testLibs

    val jsConfig = KotlinJsCompilerConfig(
      kotlinVersion = kt.kotlinVersion,
      moduleKind = KotlinJsCompilerConfig.ModuleKind.CommonJS,
      moduleName = config.name.replace("-", "_"),
      outputMode = KotlinJsCompilerConfig.OutputMode.JsExecutable,
      sourceMap = true,
      sourceMapPrefix = None,
      sourceMapEmbedSources = KotlinJsCompilerConfig.SourceMapEmbedSources.Never,
      target = KotlinJsCompilerConfig.Target.Node,
      developmentMode = true,
      generateDts = false,
      additionalOptions = kt.kotlinOptions
    )

    KotlinJsCompiler
      .compile(
        sources = sourceFiles,
        libraries = allLibraries,
        outputDir = config.outputDir,
        config = jsConfig,
        diagnosticListener = diagnosticListener,
        cancellation = cancellationToken
      )
      .map { result =>
        if (result.isSuccess) {
          val outputFiles = result.outputFile.toSet ++ result.klibFile.toSet
          ProjectCompileSuccess(result.outputDir, outputFiles, None)
        } else {
          ProjectCompileFailure(
            List(CompilerError(None, 0, 0, s"Kotlin/JS compilation failed with exit code ${result.exitCode}", None, CompilerError.Severity.Error))
          )
        }
      }
      .handleErrorWith { case e: Exception =>
        IO.pure(ProjectCompileFailure(List(CompilerError(None, 0, 0, s"Kotlin/JS compilation error: ${e.getMessage}", None, CompilerError.Severity.Error))))
      }
  }
}

/** Kotlin/Native compiler for Kotlin projects targeting native binaries.
  *
  * Uses the K2Native compiler to produce native executables. For test projects, includes kotlin-test library automatically.
  */
object KotlinNativeProjectCompiler extends ProjectCompiler {
  def supports(language: ProjectLanguage): Boolean = language match {
    case _: ProjectLanguage.KotlinNative => true
    case _                               => false
  }

  def compile(
      config: ProjectConfig,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken,
      dependencyAnalyses: Map[Path, Path],
      progressListener: ProgressListener
  ): IO[ProjectCompileResult] =
    config.language match {
      case kt: ProjectLanguage.KotlinNative =>
        compileKotlinNative(config, kt, diagnosticListener, cancellationToken)
      case other =>
        IO.raiseError(new IllegalArgumentException(s"KotlinNativeProjectCompiler cannot compile $other"))
    }

  private def compileKotlinNative(
      config: ProjectConfig,
      kt: ProjectLanguage.KotlinNative,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken
  ): IO[ProjectCompileResult] = {
    // Collect source files
    val sourceFiles = config.sources.toSeq.flatMap { srcDir =>
      if (Files.isDirectory(srcDir)) {
        scala.util
          .Using(Files.walk(srcDir)) { stream =>
            stream
              .iterator()
              .asScala
              .filter(p => p.toString.endsWith(".kt") || p.toString.endsWith(".kts"))
              .toSeq
          }
          .getOrElse(Seq.empty)
      } else if (Files.exists(srcDir)) {
        Seq(srcDir)
      } else {
        Seq.empty
      }
    }

    if (sourceFiles.isEmpty) {
      IO.pure(ProjectCompileSuccess(config.outputDir, Set.empty, None))
    } else {
      // Compilation ALWAYS produces a KLIB (library). The link step handles producing executables.
      val outputPath = config.outputDir.resolve(s"${config.name}.klib")

      // === FAST PATH: Check if up-to-date before expensive compilation ===
      val outputExists = Files.exists(outputPath)

      if (outputExists) {
        val outputModTime =
          try
            Files.getLastModifiedTime(outputPath).toMillis
          catch {
            case _: Exception => 0L
          }

        // Check if any source is newer than output
        val newerSources = sourceFiles.filter { src =>
          try
            Files.exists(src) && Files.getLastModifiedTime(src).toMillis > outputModTime
          catch {
            case _: Exception => true
          }
        }

        // Check if any classpath entry is newer than output
        val newerDeps = config.classpath.filter { dep =>
          try
            Files.exists(dep) && Files.getLastModifiedTime(dep).toMillis > outputModTime
          catch {
            case _: Exception => true
          }
        }.toSeq

        if (newerSources.isEmpty && newerDeps.isEmpty) {
          // Up to date! Skip compilation
          diagnosticListener.onCompilationReason(config.name, CompilationReason.UpToDate)
          IO.pure(ProjectCompileSuccess(config.outputDir, Set(outputPath), None))
        } else {
          // Need to recompile - report reason
          val reason = CompilationReason.Incremental(sourceFiles.length, newerSources, newerDeps)
          diagnosticListener.onCompilationReason(config.name, reason)
          doKotlinNativeCompile(sourceFiles, config, kt, outputPath, diagnosticListener, cancellationToken)
        }
      } else {
        // No output exists - clean build
        val reason =
          if (Files.exists(config.outputDir)) CompilationReason.EmptyOutput
          else CompilationReason.CleanBuild
        diagnosticListener.onCompilationReason(config.name, reason)
        doKotlinNativeCompile(sourceFiles, config, kt, outputPath, diagnosticListener, cancellationToken)
      }
    }
  }

  private def doKotlinNativeCompile(
      sourceFiles: Seq[Path],
      config: ProjectConfig,
      kt: ProjectLanguage.KotlinNative,
      outputPath: Path,
      diagnosticListener: DiagnosticListener,
      cancellationToken: CancellationToken
  ): IO[ProjectCompileResult] = {
    // For test projects, resolve kotlin-test native library
    val testLibs =
      if (kt.isTest) CompilerResolver.resolveKotlinTestNative(kt.kotlinVersion)
      else Seq.empty

    // Find KLIB files from classpath (Kotlin/Native libraries)
    // K/N compiler only accepts .klib files or directories with manifest, not JARs
    // Classpath may contain: .klib files directly, directories with manifest, or directories containing .klib files
    val klibsFromClasspath = config.classpath.toSeq.flatMap { path =>
      val pathStr = path.toString
      if (pathStr.endsWith(".klib") && Files.exists(path)) {
        // Direct .klib file
        Seq(path)
      } else if (Files.isDirectory(path) && Files.exists(path.resolve("manifest"))) {
        // Directory-based KLIB (unzipped)
        Seq(path)
      } else if (Files.isDirectory(path)) {
        // Search for .klib files inside the directory (e.g., compiled native library output)
        import scala.jdk.CollectionConverters._
        try
          Files
            .list(path)
            .iterator()
            .asScala
            .filter(_.toString.endsWith(".klib"))
            .toSeq
        catch {
          case _: Exception => Seq.empty
        }
      } else {
        Seq.empty
      }
    }

    // Combined libraries: filtered KLIB classpath + test libs (if test project)
    // Note: Kotlin/Native stdlib is built into the K/N distribution
    val allLibraries = klibsFromClasspath ++ testLibs

    val target = KotlinNativeCompilerConfig.Target.hostTarget
    // Compilation ALWAYS produces a KLIB. The link step handles producing executables.
    val outputKind = KotlinNativeCompilerConfig.OutputKind.Klib

    // Note: -generate-test-runner flag is added during linking (LinkExecutor), not during compilation

    val nativeConfig = KotlinNativeCompilerConfig(
      kotlinVersion = kt.kotlinVersion,
      target = target,
      outputKind = outputKind,
      debuggable = true,
      optimized = false,
      baseName = Some(config.name),
      linkerOpts = Nil,
      freeCompilerArgs = Seq.empty,
      entryPoint = None,
      additionalOptions = kt.kotlinOptions
    )

    KotlinNativeCompiler
      .compile(
        sources = sourceFiles,
        libraries = allLibraries,
        outputPath = outputPath,
        config = nativeConfig,
        diagnosticListener = diagnosticListener,
        cancellation = cancellationToken
      )
      .map { result =>
        if (result.isSuccess) {
          ProjectCompileSuccess(config.outputDir, Set(result.outputPath), None)
        } else {
          ProjectCompileFailure(
            List(CompilerError(None, 0, 0, s"Kotlin/Native compilation failed with exit code ${result.exitCode}", None, CompilerError.Severity.Error))
          )
        }
      }
      .handleErrorWith { case e: Exception =>
        IO.pure(ProjectCompileFailure(List(CompilerError(None, 0, 0, s"Kotlin/Native compilation error: ${e.getMessage}", None, CompilerError.Severity.Error))))
      }
  }
}
