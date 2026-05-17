package bleep.analysis

import bleep.BleepFileCache
import cats.effect.IO
import coursier.cache.{ArchiveCache, CacheLogger}
import coursier.util.{Artifact, Task}

import java.nio.file.{Files, Path}
import java.lang.reflect.InvocationTargetException
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext}
import scala.jdk.CollectionConverters.*
import scala.collection.mutable

/** Kotlin/Native compiler.
  *
  * Uses reflection to invoke the K2Native compiler from the Kotlin Native compiler, allowing different Kotlin versions to be loaded in isolated classloaders.
  */
object KotlinNativeCompiler {

  /** Compile Kotlin sources to native binary.
    *
    * @param sources
    *   the source files to compile
    * @param libraries
    *   the library dependencies (KLIB files)
    * @param outputPath
    *   the output path for the binary
    * @param config
    *   the compiler configuration
    * @param diagnosticListener
    *   listener for compilation diagnostics
    * @param cancellation
    *   token for cancelling compilation
    * @return
    *   the compilation result, or IO.canceled if cancelled
    */
  def compile(
      sources: Seq[Path],
      libraries: Seq[Path],
      outputPath: Path,
      config: KotlinNativeCompilerConfig,
      diagnosticListener: DiagnosticListener,
      cancellation: CancellationToken
  ): IO[KotlinNativeCompileResult] =
    // Check cancellation before starting
    if (cancellation.isCancelled) {
      IO.canceled.asInstanceOf[IO[KotlinNativeCompileResult]]
    } else {
      // Use IO.blocking (NOT IO.interruptible) because the Kotlin Native compiler
      // uses kotlinx.coroutines runBlocking which crashes on Thread.interrupt.
      // Cancellation is handled via the CancellationToken instead.
      IO.blocking {
        compileBlocking(sources, libraries, outputPath, config, diagnosticListener, cancellation)
      }.onCancel {
        IO.delay(cancellation.cancel())
      }.flatMap { result =>
        if (cancellation.isCancelled) IO.canceled.asInstanceOf[IO[KotlinNativeCompileResult]]
        else IO.pure(result)
      }.handleErrorWith {
        case _: InterruptedException =>
          IO.canceled.asInstanceOf[IO[KotlinNativeCompileResult]]
        case _: CompilationCancelledException =>
          IO.canceled.asInstanceOf[IO[KotlinNativeCompileResult]]
        case e =>
          IO.raiseError(e)
      }
    }

  /** Resolve the Kotlin/Native prebuilt distribution home directory.
    *
    * The K/N compiler needs a distribution directory containing platform libraries, LLVM, etc. We route the download/extract through Coursier's
    * [[ArchiveCache]] so the ~200MB tarball lands under `~/.cache/coursier/arc/...` — the same path GitHub Actions' `coursier/cache-action@v8` already caches.
    * Cuts ~90s off first-test cost on warm runners and removes a network dep mid-test on cold ones. Returns the path Konan should be pointed at via
    * `konan.home`: `<extracted>/kotlin-native-prebuilt-<platform>-<version>/`.
    */
  private def resolveKonanHome(kotlinVersion: String): Path = {
    val os = System.getProperty("os.name").toLowerCase
    val arch = System.getProperty("os.arch").toLowerCase

    val platform = (os, arch) match {
      case (o, "aarch64") if o.contains("mac")   => "macos-aarch64"
      case (o, _) if o.contains("mac")           => "macos-x86_64"
      case (o, "aarch64") if o.contains("linux") => "linux-x86_64" // K/N doesn't have linux-aarch64 prebuilt
      case (o, _) if o.contains("linux")         => "linux-x86_64"
      case _                                     => "linux-x86_64"
    }

    // Two distinct names: Maven Central names the artifact `kotlin-native-prebuilt-<version>-<platform>.tar.gz` (classifier convention), but the directory the
    // tarball extracts to is `kotlin-native-prebuilt-<platform>-<version>` (Kotlin's own naming). Get either one wrong → either 404 from Coursier or "Konan
    // distribution not found after extraction".
    val artifactFileName = s"kotlin-native-prebuilt-$kotlinVersion-$platform"
    val extractedFolderName = s"kotlin-native-prebuilt-$platform-$kotlinVersion"
    val url = s"https://repo1.maven.org/maven2/org/jetbrains/kotlin/kotlin-native-prebuilt/$kotlinVersion/$artifactFileName.tar.gz"
    val fileCache = BleepFileCache().withLogger(CacheLogger.nop)
    val cache = ArchiveCache[Task]().withCache(fileCache)
    val extractedRoot = Await.result(cache.get(Artifact(url)).value(ExecutionContext.global), Duration.Inf) match {
      case Left(err)     => throw new RuntimeException(s"Failed to fetch Kotlin/Native prebuilt $kotlinVersion ($platform): $err", err)
      case Right(folder) => folder.toPath
    }

    // Coursier's ArchiveCache extracts into `<arc-cache>/<hash>/`, and the tarball's top-level entry is `kotlin-native-prebuilt-<platform>-<version>/`. That
    // subfolder is what Konan calls "konan.home" — points at `bin/`, `klib/`, `konan/lib/`, etc.
    val konanHome = extractedRoot.resolve(extractedFolderName)
    if (!Files.isDirectory(konanHome))
      throw new RuntimeException(
        s"Kotlin/Native distribution not found after extraction. Expected directory at $konanHome (extracted root: $extractedRoot)"
      )
    konanHome
  }

  private def compileBlocking(
      sources: Seq[Path],
      libraries: Seq[Path],
      outputPath: Path,
      config: KotlinNativeCompilerConfig,
      diagnosticListener: DiagnosticListener,
      cancellation: CancellationToken
  ): KotlinNativeCompileResult = {
    val instance = CompilerResolver.getKotlinNativeCompiler(config.kotlinVersion)
    val loader = instance.loader

    // Create output directory
    Files.createDirectories(outputPath.getParent)

    // Set konan.home so the compiler can find platform libraries, LLVM, etc.
    val konanHome = resolveKonanHome(config.kotlinVersion)
    val oldKonanHome = System.getProperty("konan.home")
    System.setProperty("konan.home", konanHome.toString)

    try {
      // Check cancellation
      checkCancellation(cancellation)

      // Build argument list
      val argList = mutable.ListBuffer[String]()

      // Sources
      sources.foreach(s => argList += s.toAbsolutePath.toString)

      // Output
      argList += "-output"
      argList += outputPath.toAbsolutePath.toString

      // Target
      argList += "-target"
      argList += config.target.konanName

      // Output kind
      argList += "-produce"
      argList += config.outputKind.produce

      // Libraries
      libraries.foreach { lib =>
        argList += "-library"
        argList += lib.toAbsolutePath.toString
      }

      // Optimization
      if (config.optimized) argList += "-opt"
      if (config.debuggable) argList += "-g"

      // Entry point
      config.entryPoint.foreach { ep =>
        argList += "-entry"
        argList += ep
      }

      // Linker options
      config.linkerOpts.foreach { opt =>
        argList += "-linker-option"
        argList += opt
      }

      // Free args
      argList ++= config.freeCompilerArgs

      // Additional options
      argList ++= config.additionalOptions

      // Check cancellation before compiling
      checkCancellation(cancellation)

      // Load K2Native compiler class
      val compilerClass = loader.loadClass("org.jetbrains.kotlin.cli.bc.K2Native")

      // Invoke main method
      val mainMethod = compilerClass.getMethod("main", classOf[Array[String]])
      mainMethod.invoke(null, argList.toArray)

      // Check cancellation after compilation
      checkCancellation(cancellation)

      // Check if output was created.
      // K/N may output in different locations depending on version:
      // - directly at outputPath
      // - with .kexe extension (macOS/Linux executables)
      // - inside a classes/ subdirectory (newer versions)
      val filename = outputPath.getFileName.toString
      val parentDir = outputPath.getParent
      val possiblePaths = Seq(
        outputPath,
        outputPath.resolveSibling(filename + ".kexe"),
        parentDir.resolve("classes").resolve(filename),
        parentDir.resolve("classes").resolve(filename + ".kexe")
      )
      val actualOutput = possiblePaths.find(Files.exists(_)).getOrElse(outputPath)
      val exitCode = if (Files.exists(actualOutput)) 0 else 1

      KotlinNativeCompileResult(actualOutput, exitCode)
    } catch {
      case _: InterruptedException =>
        Thread.currentThread().interrupt()
        throw new InterruptedException("Compilation interrupted")
      case e: CompilationCancelledException =>
        throw e
      case e: InvocationTargetException =>
        val cause = e.getCause
        if (
          cause != null && (cause.getClass.getName.contains("CompilationCanceled") ||
            cause.isInstanceOf[InterruptedException])
        ) {
          throw new CompilationCancelledException("Compilation cancelled")
        }
        diagnosticListener.onDiagnostic(
          CompilerError(
            path = None,
            line = 0,
            column = 0,
            message = s"Kotlin/Native compilation failed: ${if (cause != null) cause.getMessage else e.getMessage}",
            rendered = None,
            severity = CompilerError.Severity.Error
          )
        )
        KotlinNativeCompileResult(outputPath, 1)
      case e: ClassNotFoundException =>
        // K2Native may not be available in all distributions
        // Try alternative approach using konanc if available
        compileWithKonanc(sources, libraries, outputPath, config, diagnosticListener, cancellation)
      case e: Exception =>
        diagnosticListener.onDiagnostic(
          CompilerError(
            path = None,
            line = 0,
            column = 0,
            message = s"Kotlin/Native compilation failed: ${e.getMessage}",
            rendered = None,
            severity = CompilerError.Severity.Error
          )
        )
        KotlinNativeCompileResult(outputPath, 1)
    } finally
      // Restore previous konan.home
      if (oldKonanHome != null) System.setProperty("konan.home", oldKonanHome): Unit
      else System.clearProperty("konan.home"): Unit
  }

  /** Check cancellation and throw if cancelled. Also checks thread interrupt. */
  private def checkCancellation(cancellation: CancellationToken): Unit = {
    if (Thread.interrupted()) {
      throw new InterruptedException("Compilation interrupted")
    }
    if (cancellation.isCancelled) {
      throw new CompilationCancelledException("Compilation cancelled")
    }
  }

  /** Fallback compilation using konanc command-line tool if available.
    *
    * This is used when the embeddable compiler is not available.
    */
  private def compileWithKonanc(
      sources: Seq[Path],
      libraries: Seq[Path],
      outputPath: Path,
      config: KotlinNativeCompilerConfig,
      diagnosticListener: DiagnosticListener,
      cancellation: CancellationToken
  ): KotlinNativeCompileResult = {
    val argList = mutable.ListBuffer[String]()

    // konanc command
    val konanc = findKonanc()

    if (konanc.isEmpty) {
      diagnosticListener.onDiagnostic(
        CompilerError(
          path = None,
          line = 0,
          column = 0,
          message = "Kotlin/Native compiler (konanc) not found. Please install Kotlin/Native.",
          rendered = None,
          severity = CompilerError.Severity.Error
        )
      )
      return KotlinNativeCompileResult(outputPath, 1)
    }

    argList += konanc.get

    // Sources
    sources.foreach(s => argList += s.toAbsolutePath.toString)

    // Output
    argList += "-output"
    argList += outputPath.toAbsolutePath.toString

    // Target
    argList += "-target"
    argList += config.target.konanName

    // Output kind
    argList += "-produce"
    argList += config.outputKind.produce

    // Libraries
    libraries.foreach { lib =>
      argList += "-library"
      argList += lib.toAbsolutePath.toString
    }

    // Optimization
    if (config.optimized) argList += "-opt"
    if (config.debuggable) argList += "-g"

    // Entry point
    config.entryPoint.foreach { ep =>
      argList += "-entry"
      argList += ep
    }

    // Linker options
    config.linkerOpts.foreach { opt =>
      argList += "-linker-option"
      argList += opt
    }

    // Free args
    argList ++= config.freeCompilerArgs

    try {
      // Check cancellation
      checkCancellation(cancellation)

      val pb = new ProcessBuilder(argList.asJava)
        .inheritIO()

      val process = pb.start()
      cancellation.onCancel { () => process.destroyForcibly(); () }

      val exitCode = process.waitFor()
      if (cancellation.isCancelled) {
        throw new CompilationCancelledException("Compilation cancelled")
      }
      KotlinNativeCompileResult(outputPath, exitCode)
    } catch {
      case _: InterruptedException =>
        throw new InterruptedException("Compilation interrupted")
      case e: CompilationCancelledException =>
        throw e
      case e: Exception =>
        diagnosticListener.onDiagnostic(
          CompilerError(
            path = None,
            line = 0,
            column = 0,
            message = s"Kotlin/Native compilation failed: ${e.getMessage}",
            rendered = None,
            severity = CompilerError.Severity.Error
          )
        )
        KotlinNativeCompileResult(outputPath, 1)
    }
  }

  /** Find konanc command in PATH or Kotlin installation. */
  private def findKonanc(): Option[String] = {
    // Check if konanc is in PATH
    try {
      val pb = new ProcessBuilder("konanc", "--version")
        .redirectOutput(ProcessBuilder.Redirect.DISCARD)
        .redirectError(ProcessBuilder.Redirect.DISCARD)
      val process = pb.start()
      if (process.waitFor() == 0) {
        return Some("konanc")
      }
    } catch {
      case _: Exception => // Not in PATH
    }

    // Check common installation locations
    val kotlinHome = sys.env.get("KOTLIN_HOME")
    kotlinHome.flatMap { home =>
      val konanc = java.nio.file.Paths.get(home, "bin", "konanc")
      if (Files.exists(konanc)) Some(konanc.toString) else None
    }
  }
}
