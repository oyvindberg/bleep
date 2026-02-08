package bleep.analysis

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import java.nio.file.Path
import scala.concurrent.duration.*

// =============================================================================
// IO-based compiler trait for testability
// =============================================================================

/** IO-based compiler trait for testability.
  *
  * Returns `IO` which allows:
  *   - Controlled timing in tests (via Deferred)
  *   - Better composition with the parallel build system
  *   - Native cancellation via IO
  */
trait CompilerIO {

  /** Compile sources to output directory */
  def compile(
      sources: Map[Path, String],
      classpath: Seq[Path],
      outputDir: Path,
      config: LanguageConfig
  ): IO[CompilationResult]
}

object CompilerIO {

  /** Wrap a synchronous Compiler factory as CompilerIO */
  val default: CompilerIO = new CompilerIO {
    def compile(
        sources: Map[Path, String],
        classpath: Seq[Path],
        outputDir: Path,
        config: LanguageConfig
    ): IO[CompilationResult] =
      IO.blocking {
        val compiler = Compiler.forConfig(config)
        val sourceFiles = sources.map { case (path, content) => SourceFile(path, content) }.toSeq
        val input = CompilationInput(sourceFiles, classpath, outputDir, config)
        compiler.compile(input, DiagnosticListener.noop, CancellationToken.never)
      }
  }
}

// =============================================================================
// Test Harness - Controllable implementations for testing
// =============================================================================

/** Handle to a running compilation operation in tests.
  *
  * Allows completing the compilation with success or failure.
  */
trait CompilationHandle {
  def sources: Map[Path, String]
  def config: LanguageConfig
  def complete(result: CompilationResult): IO[Unit]
  def completeSuccess(outputDir: Path, classes: Set[Path]): IO[Unit] =
    complete(CompilationSuccess(outputDir, classes))
  def completeFailure(errors: List[CompilerError]): IO[Unit] =
    complete(CompilationFailure(errors))
}

/** State snapshot of the test harness */
case class TestHarnessState(
    compilingSources: Set[Map[Path, String]],
    completedCompilations: List[(Map[Path, String], CompilationResult)]
)

/** Test harness for controllable compilation.
  *
  * Provides:
  *   - CompilerIO implementation that can be controlled
  *   - Visibility into what's currently running
  *   - Ability to complete operations with specific results
  *
  * Usage:
  * ```scala
  * for
  *   harness <- TestHarness.create
  * // Start a build with the harness's compiler
  *   buildFiber <- someCompilation(harness.compiler).start
  * // Wait for compilation to start
  *   _ <- harness.awaitAnyCompiling
  * // Get the handle and complete it
  *   handles <- harness.getAllCompilationHandles
  *   _ <- handles.head.completeSuccess(outputDir, Set.empty)
  * // Continue...
  * yield ()
  * ```
  */
class TestHarness private (
    compilationHandles: Ref[IO, Map[Int, (Deferred[IO, CompilationResult], CompilationHandle)]],
    completedCompilations: Ref[IO, List[(Map[Path, String], CompilationResult)]],
    nextCompilationId: Ref[IO, Int]
) {

  /** Get the compiler that can be controlled by this harness */
  val compiler: CompilerIO = new CompilerIO {
    def compile(
        sources: Map[Path, String],
        classpath: Seq[Path],
        outputDir: Path,
        config: LanguageConfig
    ): IO[CompilationResult] = {
      val theSources = sources
      val theConfig = config
      for {
        deferred <- Deferred[IO, CompilationResult]
        id <- nextCompilationId.getAndUpdate(_ + 1)
        handle = new CompilationHandle {
          def sources: Map[Path, String] = theSources
          def config: LanguageConfig = theConfig
          def complete(result: CompilationResult): IO[Unit] =
            for {
              _ <- deferred.complete(result)
              _ <- completedCompilations.update((theSources, result) :: _)
              _ <- compilationHandles.update(_ - id)
            } yield ()
        }
        _ <- compilationHandles.update(_ + (id -> (deferred, handle)))
        result <- deferred.get
      } yield result
    }
  }

  /** Get current state snapshot */
  def getState: IO[TestHarnessState] =
    for {
      compilations <- compilationHandles.get
      compiledList <- completedCompilations.get
    } yield TestHarnessState(
      compilingSources = compilations.values.map(_._2.sources).toSet,
      completedCompilations = compiledList.reverse
    )

  /** Get sources currently being compiled */
  def getCompilingSources: IO[Set[Map[Path, String]]] =
    compilationHandles.get.map(_.values.map(_._2.sources).toSet)

  /** Wait until any compilation starts */
  def awaitAnyCompiling: IO[Map[Path, String]] = {
    def poll: IO[Map[Path, String]] =
      compilationHandles.get.flatMap { handles =>
        handles.headOption match {
          case Some((_, (_, handle))) => IO.pure(handle.sources)
          case None                   => IO.cede >> poll
        }
      }
    poll
  }

  /** Wait until at least n compilations are running */
  def awaitCompilingCount(n: Int): IO[Unit] = {
    def poll: IO[Unit] =
      compilationHandles.get.flatMap { handles =>
        if (handles.size >= n) IO.unit
        else IO.cede >> poll
      }
    poll
  }

  /** Get all current compilation handles */
  def getAllCompilationHandles: IO[List[CompilationHandle]] =
    compilationHandles.get.map(_.values.map(_._2).toList)

  /** Complete all currently running compilations with success */
  def completeAllCompilations(outputDir: Path): IO[Unit] =
    for {
      handles <- getAllCompilationHandles
      _ <- handles.traverse_(h => h.completeSuccess(outputDir, Set.empty))
    } yield ()

  /** Complete compilations as they arrive until `count` have been completed. This handles the case where compilations start at different times.
    */
  def completeCompilationsUntil(count: Int, outputDir: Path): IO[Unit] = {
    def loop(completed: Int): IO[Unit] =
      if (completed >= count) IO.unit
      else
        for {
          handles <- getAllCompilationHandles
          newlyCompleted <- handles.headOption match {
            case Some(h) =>
              h.completeSuccess(outputDir, Set.empty) >> IO.cede.as(1)
            case None =>
              IO.cede.as(0)
          }
          _ <- loop(completed + newlyCompleted)
        } yield ()
    loop(0)
  }

  /** Fail all currently running compilations */
  def failAllCompilations(errors: List[CompilerError]): IO[Unit] =
    for {
      handles <- getAllCompilationHandles
      _ <- handles.traverse_(_.completeFailure(errors))
    } yield ()
}

object TestHarness {

  /** Create a new test harness */
  def create: IO[TestHarness] =
    for {
      compilationHandles <- Ref.of[IO, Map[Int, (Deferred[IO, CompilationResult], CompilationHandle)]](Map.empty)
      completedCompilations <- Ref.of[IO, List[(Map[Path, String], CompilationResult)]](Nil)
      nextId <- Ref.of[IO, Int](0)
    } yield new TestHarness(compilationHandles, completedCompilations, nextId)
}

// =============================================================================
// Simple mock implementations for quick testing
// =============================================================================

/** A mock compiler that returns pre-configured results immediately */
class MockCompilerIO(result: CompilationResult) extends CompilerIO {
  def compile(
      sources: Map[Path, String],
      classpath: Seq[Path],
      outputDir: Path,
      config: LanguageConfig
  ): IO[CompilationResult] =
    IO.pure(result)
}

/** A delayed compiler for testing timing */
class DelayedCompilerIO(delay: scala.concurrent.duration.FiniteDuration, underlying: CompilerIO) extends CompilerIO {
  def compile(
      sources: Map[Path, String],
      classpath: Seq[Path],
      outputDir: Path,
      config: LanguageConfig
  ): IO[CompilationResult] =
    IO.sleep(delay) >> underlying.compile(sources, classpath, outputDir, config)
}
