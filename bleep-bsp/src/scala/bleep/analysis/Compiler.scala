package bleep.analysis

import java.nio.file.{Files, Path}
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicBoolean

/** Cancellation token for aborting long-running operations.
  *
  * Supports both polling via `isCancelled` and reactive notification via `onCancel`. Listeners registered after cancellation are fired immediately.
  */
trait CancellationToken {

  /** Check if cancellation has been requested */
  def isCancelled: Boolean

  /** Request cancellation */
  def cancel(): Unit

  /** Register a callback to be invoked when cancellation is requested. If already cancelled, the callback fires immediately on the calling thread. */
  def onCancel(callback: () => Unit): Unit
}

object CancellationToken {

  /** A token that is never cancelled */
  val never: CancellationToken = new CancellationToken {
    def isCancelled: Boolean = false
    def cancel(): Unit = ()
    def onCancel(callback: () => Unit): Unit = ()
  }

  /** Create a new cancellable token */
  def create(): CancellationToken = new CancellationToken {
    private val cancelled = AtomicBoolean(false)
    private val listeners = new CopyOnWriteArrayList[() => Unit]()
    def isCancelled: Boolean = cancelled.get()
    def cancel(): Unit =
      if (cancelled.compareAndSet(false, true)) {
        listeners.forEach(cb => cb())
      }
    def onCancel(callback: () => Unit): Unit = {
      listeners.add(callback)
      // If already cancelled, fire immediately (handles race between add and cancel)
      if (cancelled.get()) callback()
    }
  }
}

/** Language configuration for compilation.
  *
  * Each language has version and options. For Scala/Kotlin, the version determines which compiler JARs to resolve. For Java, we use the JVM's bundled compiler
  * with --release for targeting older versions.
  */
sealed trait LanguageConfig {
  def version: String
  def options: List[String]
}

case class ScalaConfig(
    version: String, // e.g., "3.3.3", "3.7.4"
    options: List[String] = Nil
) extends LanguageConfig

case class KotlinConfig(
    version: String, // e.g., "2.0.0", "2.3.0"
    jvmTarget: String = "11",
    options: List[String] = Nil
) extends LanguageConfig

case class JavaConfig(
    release: Option[Int] = None, // --release flag (None = use JVM default)
    options: List[String] = Nil,
    ecjVersion: Option[String] = None // ECJ version (None = use javac)
) extends LanguageConfig {
  def version: String = release.map(_.toString).getOrElse("current")
}

/** A source file to compile */
case class SourceFile(path: Path, content: String)

/** Input to compilation */
case class CompilationInput(
    sources: Seq[SourceFile],
    classpath: Seq[Path],
    outputDir: Path,
    config: LanguageConfig
)

/** Result of compilation */
sealed trait CompilationResult {
  def isSuccess: Boolean
}

case class CompilationSuccess(
    outputDir: Path,
    compiledClasses: Set[Path]
) extends CompilationResult {
  def isSuccess: Boolean = true
}

case class CompilationFailure(
    errors: List[CompilerError]
) extends CompilationResult {
  def isSuccess: Boolean = false
}

case object CompilationCancelled extends CompilationResult {
  def isSuccess: Boolean = false
}

/** A compiler error with location information */
case class CompilerError(
    path: Option[Path],
    line: Int,
    column: Int,
    message: String,
    rendered: Option[String],
    severity: CompilerError.Severity
) {
  def formatted: String = {
    val loc = path match {
      case Some(p) =>
        val locPart = (line, column) match {
          case (0, 0) => ""
          case (l, 0) => s":$l"
          case (l, c) => s":$l:$c"
        }
        s"${p.getFileName}$locPart"
      case None => "<unknown>"
    }
    s"$loc: $message"
  }
}

object CompilerError {
  enum Severity {
    case Error, Warning, Info
  }
}

/** Why compilation is triggered */
sealed trait CompilationReason {

  /** Format the reason as a human-readable string */
  def formatted(projectName: String): String
}

object CompilationReason {

  /** No previous analysis - first compile or clean build */
  case object CleanBuild extends CompilationReason {
    def formatted(projectName: String): String =
      s"$projectName: clean build (no previous analysis)"
  }

  /** Output directory is empty or missing */
  case object EmptyOutput extends CompilationReason {
    def formatted(projectName: String): String =
      s"$projectName: clean build (output directory empty)"
  }

  /** Incremental compilation with specific invalidated files */
  case class Incremental(
      totalFiles: Int,
      invalidatedFiles: Seq[Path],
      changedDependencies: Seq[Path]
  ) extends CompilationReason {
    def formatted(projectName: String): String = {
      val invalidatedCount = invalidatedFiles.size
      val depCount = changedDependencies.size

      val invalidatedStr =
        if (invalidatedFiles.isEmpty) ""
        else {
          val fileNames = invalidatedFiles.take(5).map(_.getFileName.toString)
          val suffix = if (invalidatedFiles.size > 5) s", ... (${invalidatedFiles.size - 5} more)" else ""
          s"$invalidatedCount/$totalFiles files invalidated (${fileNames.mkString(", ")}$suffix)"
        }

      val depStr =
        if (changedDependencies.isEmpty) ""
        else {
          val depNames = changedDependencies.take(3).map(_.getFileName.toString)
          val suffix = if (changedDependencies.size > 3) s", ... (${changedDependencies.size - 3} more)" else ""
          s"${depCount} changed dependencies (${depNames.mkString(", ")}$suffix)"
        }

      val parts = List(invalidatedStr, depStr).filter(_.nonEmpty)
      if (parts.isEmpty) s"$projectName: incremental (changes detected)"
      else s"$projectName: ${parts.mkString("; ")}"
    }
  }

  /** No changes detected - everything is up to date */
  case object UpToDate extends CompilationReason {
    def formatted(projectName: String): String =
      s"$projectName: up to date"
  }
}

/** Sub-phases within a single project compilation.
  *
  * These phases are signalled by ZincBridge.compileOnce() to provide visibility into what zinc is doing between "started" and "done". The trackedApis count in
  * ReadingAnalysis correlates with heap usage — zinc loads all API structures into memory, which can explode to 24GB for large codegen projects.
  */
sealed trait CompilePhase {
  def name: String
}

object CompilePhase {
  case class ReadingAnalysis(trackedApis: Int) extends CompilePhase { def name: String = "reading-analysis" }
  case object Analyzing extends CompilePhase { def name: String = "analyzing" }
  case object Compiling extends CompilePhase { def name: String = "compiling" }
  case object SavingAnalysis extends CompilePhase { def name: String = "saving-analysis" }
}

/** Listener for streaming diagnostics during compilation */
trait DiagnosticListener {

  /** Called when a diagnostic is emitted during compilation */
  def onDiagnostic(error: CompilerError): Unit

  /** Called when a source file starts being compiled. This is used to track which files are actually being compiled, which is essential for testing incremental
    * compilation.
    *
    * @param path
    *   the source file being compiled
    * @param phase
    *   optional compilation phase name (e.g., "typer", "parser")
    */
  def onCompileFile(path: Path, phase: Option[String]): Unit = ()

  /** Called before compilation starts to report WHY compilation is happening.
    *
    * This provides visibility into what triggered recompilation - whether it's a clean build, which files were invalidated, or if the project is up to date.
    *
    * @param projectName
    *   the name of the project being compiled
    * @param reason
    *   the reason for compilation
    */
  def onCompilationReason(projectName: String, reason: CompilationReason): Unit = ()

  /** Called when compilation transitions between sub-phases (reading analysis, analyzing, compiling, saving).
    *
    * @param projectName
    *   the name of the project being compiled
    * @param phase
    *   the sub-phase being entered
    */
  def onCompilePhase(projectName: String, phase: CompilePhase): Unit = ()
}

object DiagnosticListener {

  /** No-op listener that discards diagnostics */
  val noop: DiagnosticListener = new DiagnosticListener {
    def onDiagnostic(error: CompilerError): Unit = ()
  }

  /** Listener that tracks which files are compiled */
  def tracking(
      files: scala.collection.mutable.Set[Path],
      diagnostics: scala.collection.mutable.Buffer[CompilerError]
  ): DiagnosticListener = new DiagnosticListener {
    def onDiagnostic(error: CompilerError): Unit = diagnostics += error
    override def onCompileFile(path: Path, phase: Option[String]): Unit = files += path
  }
}

/** Trait for language compilers */
trait Compiler {

  /** Compile the given sources with streaming diagnostics and cancellation support.
    *
    * Note: Currently cancellation is cooperative - it's checked before compilation starts but cannot interrupt mid-compilation. For true mid-compilation
    * cancellation, we'd need to use Zinc (which has CompileProgress callbacks) or run compilers in separate processes.
    */
  def compile(
      input: CompilationInput,
      listener: DiagnosticListener,
      cancellation: CancellationToken
  ): CompilationResult

  /** Compile with cancellation but no diagnostics */
  def compile(input: CompilationInput, cancellation: CancellationToken): CompilationResult =
    compile(input, DiagnosticListener.noop, cancellation)

  /** Compile without cancellation or diagnostics */
  def compile(input: CompilationInput, listener: DiagnosticListener): CompilationResult =
    compile(input, listener, CancellationToken.never)

  /** Compile without cancellation or diagnostics */
  def compile(input: CompilationInput): CompilationResult =
    compile(input, DiagnosticListener.noop, CancellationToken.never)

  /** Write sources to a directory (helper for implementations) */
  protected def writeSourcesToDir(sources: Seq[SourceFile], dir: Path): Seq[Path] =
    sources.map { sf =>
      val targetPath = dir.resolve(sf.path)
      Files.createDirectories(targetPath.getParent)
      Files.writeString(targetPath, sf.content)
      targetPath
    }

  /** Collect .class files from output directory */
  protected def collectClassFiles(dir: Path): Set[Path] =
    if !Files.exists(dir) then Set.empty
    else {
      import scala.jdk.StreamConverters.*
      import scala.util.Using
      // Use Using to ensure Files.walk stream is properly closed
      Using(Files.walk(dir)) { stream =>
        stream
          .toScala(Set)
          .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".class"))
      }.getOrElse(Set.empty)
    }
}

/** Factory for creating compilers based on language config.
  *
  * For Scala and Java, this uses ZincBridge for proper incremental compilation. For Kotlin, it uses KotlinSourceCompiler directly.
  */
object Compiler {
  import cats.effect.unsafe.implicits.global

  /** Collect .class files from a directory */
  def collectClassFilesStatic(dir: Path): Set[Path] =
    if !Files.exists(dir) then Set.empty
    else {
      import scala.jdk.StreamConverters.*
      import scala.util.Using
      Using(Files.walk(dir)) { stream =>
        stream
          .toScala(Set)
          .filter(p => Files.isRegularFile(p) && p.toString.endsWith(".class"))
      }.getOrElse(Set.empty)
    }

  def forConfig(config: LanguageConfig): Compiler = config match {
    case c: JavaConfig   => new ZincAdapterCompiler(toProjectLanguageJava(c))
    case c: ScalaConfig  => new ZincAdapterCompiler(toProjectLanguageScala(c))
    case _: KotlinConfig => KotlinSourceCompiler
  }

  private def toProjectLanguageScala(c: ScalaConfig): ProjectLanguage.ScalaJava =
    ProjectLanguage.ScalaJava(
      scalaVersion = c.version,
      scalaOptions = c.options,
      javaRelease = None,
      javaOptions = Nil
    )

  private def toProjectLanguageJava(c: JavaConfig): ProjectLanguage.JavaOnly =
    ProjectLanguage.JavaOnly(
      release = c.release,
      javaOptions = c.options,
      ecjVersion = c.ecjVersion
    )

  /** Adapter that wraps ProjectCompiler (Zinc-based) to implement the synchronous Compiler trait.
    *
    * This provides proper incremental compilation support via Zinc while maintaining backward compatibility with code using the Compiler trait.
    */
  private class ZincAdapterCompiler(language: ProjectLanguage) extends Compiler {

    private val projectCompiler: ProjectCompiler = ProjectCompiler.forLanguage(language)

    override def compile(
        input: CompilationInput,
        listener: DiagnosticListener,
        cancellation: CancellationToken
    ): CompilationResult = {
      // Check cancellation before starting
      if cancellation.isCancelled then return CompilationCancelled

      try {
        // Write source content to temp directory (matching old compiler behavior)
        val tempDir = Files.createTempDirectory("zinc-compile-")
        try {
          val writtenPaths = writeSourcesToDir(input.sources, tempDir)

          // Check cancellation after setup
          if cancellation.isCancelled then return CompilationCancelled

          // Create ProjectConfig from CompilationInput
          val projectConfig = ProjectConfig(
            name = "compile",
            sources = writtenPaths.toSet,
            classpath = input.classpath,
            outputDir = input.outputDir,
            language = language,
            analysisDir = None // No analysis persistence for single compilations
          )

          // Wrap DiagnosticListener in the adapter
          val progressListener: ProgressListener = (_, _, _) => !cancellation.isCancelled

          // Run the IO-based compiler synchronously
          val result = projectCompiler
            .compile(
              projectConfig,
              listener,
              cancellation,
              Map.empty, // No dependency analyses
              progressListener
            )
            .unsafeRunSync()

          // Convert ProjectCompileResult to CompilationResult
          result match {
            case ProjectCompileSuccess(outDir, classFiles, _) =>
              CompilationSuccess(outDir, classFiles)
            case ProjectCompileFailure(errors) =>
              CompilationFailure(errors)
          }
        } finally deleteRecursively(tempDir)
      } catch {
        case e: Exception if cancellation.isCancelled =>
          CompilationCancelled
        case e: Exception =>
          val err = CompilerError(None, 0, 0, s"Compilation failed: ${e.getMessage}", None, CompilerError.Severity.Error)
          listener.onDiagnostic(err)
          CompilationFailure(List(err))
      }
    }

    private def deleteRecursively(path: Path): Unit =
      if Files.exists(path) then {
        if Files.isDirectory(path) then {
          import scala.jdk.StreamConverters.*
          scala.util.Using(Files.list(path)) { stream =>
            stream.toScala(List).foreach(deleteRecursively)
          }
        }
        Files.delete(path)
      }
  }
}
