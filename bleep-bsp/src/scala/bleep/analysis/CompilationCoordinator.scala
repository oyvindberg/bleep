package bleep.analysis

import cats.effect.{Deferred, IO}
import cats.syntax.all.*
import java.nio.file.{Files, Path, StandardOpenOption}
import java.nio.channels.{FileChannel, FileLock as JFileLock}
import java.security.MessageDigest
import java.time.Instant
import scala.concurrent.duration.*
import scala.collection.concurrent.TrieMap

/** Content-addressed compilation key.
  *
  * A CompileKey uniquely identifies a compilation based on its inputs:
  *   - Source file contents (not paths - content matters)
  *   - Classpath contents
  *   - Compiler configuration
  *
  * Same key = same compilation result (deterministic).
  */
case class CompileKey(value: String)

object CompileKey {

  /** Compute a compile key from inputs */
  def compute(
      sources: Map[Path, String],
      classpath: Seq[Path],
      config: LanguageConfig
  ): CompileKey = {
    val digest = MessageDigest.getInstance("SHA-256")

    // Hash source contents (sorted by path for determinism)
    sources.toSeq.sortBy(_._1.toString).foreach { case (path, content) =>
      digest.update(path.toString.getBytes("UTF-8"))
      digest.update(0.toByte) // separator
      digest.update(content.getBytes("UTF-8"))
      digest.update(0.toByte)
    }

    // Hash classpath (by modification time and size for speed)
    classpath.sorted.foreach { cp =>
      digest.update(cp.toString.getBytes("UTF-8"))
      if Files.exists(cp) then {
        val mtime = Files.getLastModifiedTime(cp).toMillis
        val size = if Files.isRegularFile(cp) then Files.size(cp) else 0L
        digest.update(s":$mtime:$size".getBytes("UTF-8"))
      }
      digest.update(0.toByte)
    }

    // Hash config
    val configStr = config match {
      case ScalaConfig(version, options)             => s"scala:$version:${options.mkString(",")}"
      case KotlinConfig(version, jvmTarget, options) => s"kotlin:$version:$jvmTarget:${options.mkString(",")}"
      case JavaConfig(release, options, ecjVersion)  => s"java:${release.getOrElse("default")}:${options.mkString(",")}:ecj=${ecjVersion.getOrElse("javac")}"
    }
    digest.update(configStr.getBytes("UTF-8"))

    val hash = digest.digest().map("%02x".format(_)).mkString
    CompileKey(hash)
  }
}

/** Cross-process file lock with stale detection.
  *
  * Uses Java's FileChannel.lock() for cross-process coordination. Acts as a mutex only - no result sharing across processes. Detects stale locks from crashed
  * processes by checking PID and timeout.
  */
object CrossProcessLock {
  private val lockTimeout = 30.minutes // Max time a compile can hold a lock

  /** Try to acquire a lock, with stale lock recovery */
  def tryAcquire(lockPath: Path): IO[Option[AcquiredLock]] =
    for {
      _ <- IO.blocking(Files.createDirectories(lockPath.getParent))
      channelAndLock <- IO.blocking {
        val channel = FileChannel.open(
          lockPath,
          StandardOpenOption.CREATE,
          StandardOpenOption.WRITE
        )
        val lock = channel.tryLock()
        (channel, Option(lock))
      }
      result <- channelAndLock match {
        case (channel, Some(lock)) =>
          // Got the lock - write our PID
          for {
            _ <- writeLockInfo(lockPath, ProcessHandle.current().pid(), Instant.now())
          } yield Some(AcquiredLock(lock, channel, lockPath))

        case (channel, None) =>
          // Lock held by another process - check if stale
          for {
            _ <- IO.blocking(channel.close())
            stale <- isLockStale(lockPath)
            result <-
              if stale then
                // Break stale lock and retry
                breakLock(lockPath) >> tryAcquire(lockPath)
              else IO.pure(None)
          } yield result
      }
    } yield result

  /** Acquire a lock, waiting if necessary */
  def acquire(lockPath: Path, timeout: FiniteDuration): IO[Option[AcquiredLock]] = {
    def loop(remaining: FiniteDuration): IO[Option[AcquiredLock]] =
      if remaining <= 0.millis then IO.pure(None)
      else
        tryAcquire(lockPath).flatMap {
          case Some(lock) => IO.pure(Some(lock))
          case None =>
            IO.sleep(100.millis) >> loop(remaining - 100.millis)
        }
    loop(timeout)
  }

  private def writeLockInfo(lockPath: Path, pid: Long, started: Instant): IO[Unit] = {
    val infoPath = lockPath.resolveSibling("lock.info")
    IO.blocking {
      val _ = Files.writeString(infoPath, s"$pid\n${started.toEpochMilli}")
      ()
    }
  }

  private def readLockInfo(lockPath: Path): IO[Option[(Long, Instant)]] = {
    val infoPath = lockPath.resolveSibling("lock.info")
    IO.blocking {
      if Files.exists(infoPath) then {
        val lines = Files.readString(infoPath).split("\n")
        if lines.length >= 2 then Some((lines(0).toLong, Instant.ofEpochMilli(lines(1).toLong)))
        else None
      } else None
    }.handleError(_ => None)
  }

  private def isLockStale(lockPath: Path): IO[Boolean] =
    readLockInfo(lockPath).flatMap {
      case None => IO.pure(true) // No info = stale
      case Some((pid, started)) =>
        for {
          processAlive <- IO.blocking {
            ProcessHandle.of(pid).map(_.isAlive).orElse(false)
          }
          timedOut = started.plusMillis(lockTimeout.toMillis).isBefore(Instant.now())
        } yield !processAlive || timedOut
    }

  private def breakLock(lockPath: Path): IO[Unit] = {
    val infoPath = lockPath.resolveSibling("lock.info")
    IO.blocking {
      Files.deleteIfExists(lockPath)
      Files.deleteIfExists(infoPath)
    }.void
  }

  case class AcquiredLock(lock: JFileLock, channel: FileChannel, path: Path) {
    def release: IO[Unit] = IO
      .blocking {
        try lock.release()
        finally channel.close()
        val _ = Files.deleteIfExists(path.resolveSibling("lock.info"))
        ()
      }
      .handleError(_ => ())
  }
}

/** In-process compilation state for sharing results between fibers.
  *
  * Tracks in-flight compilations within this process using Deferred to share results. Multiple fibers requesting the same compile (by CompileKey) will share
  * the result.
  *
  * Note: This is process-local. Cross-process coordination uses file locks but does NOT share results - each process compiles independently.
  */
class InProcessCompilationState {
  // Map of CompileKey -> Deferred result (for waiting)
  private val inFlight = TrieMap[String, Deferred[IO, CompilationResult]]()

  /** Check if a compilation is already in flight in this process */
  def getInFlight(key: CompileKey): IO[Option[Deferred[IO, CompilationResult]]] =
    IO.pure(inFlight.get(key.value))

  /** Try to register an in-flight compilation. Returns Left(existing) if already registered, Right(new) if we registered.
    */
  def tryRegister(key: CompileKey): IO[Either[Deferred[IO, CompilationResult], Deferred[IO, CompilationResult]]] =
    for {
      deferred <- Deferred[IO, CompilationResult]
      result <- IO {
        inFlight.putIfAbsent(key.value, deferred) match {
          case Some(existing) => Left(existing)
          case None           => Right(deferred)
        }
      }
    } yield result

  /** Complete an in-flight compilation and remove it */
  def complete(key: CompileKey, result: CompilationResult): IO[Unit] =
    for {
      maybeDeferred <- IO(inFlight.remove(key.value))
      _ <- maybeDeferred.traverse_(_.complete(result))
    } yield ()
}

/** Compilation coordinator that handles in-process deduplication.
  *
  * Provides the following guarantees:
  *   1. Same inputs (CompileKey) will not be compiled twice within a process
  *   2. Multiple fibers requesting same compile share the result via Deferred
  *   3. Cross-process: File lock prevents concurrent writes, but no result sharing
  *   4. Changed files get new CompileKeys (no interference with in-flight compiles)
  *
  * Usage:
  * ```scala
  * val coordinator = CompilationCoordinator.create(lockDir)
  * coordinator.compile(sources, classpath, outputDir, config)
  * ```
  */
class CompilationCoordinator(
    lockDir: Path,
    inProcessState: InProcessCompilationState
) {

  /** Compile with in-process deduplication.
    *
    * @param sources
    *   source files (path -> content)
    * @param classpath
    *   compilation classpath
    * @param outputDir
    *   where to write results
    * @param config
    *   compiler configuration
    * @return
    *   compilation result
    */
  def compile(
      sources: Map[Path, String],
      classpath: Seq[Path],
      outputDir: Path,
      config: LanguageConfig
  ): IO[CompilationResult] = {
    val key = CompileKey.compute(sources, classpath, config)
    compileWithKey(key, sources, classpath, outputDir, config)
  }

  private def compileWithKey(
      key: CompileKey,
      sources: Map[Path, String],
      classpath: Seq[Path],
      outputDir: Path,
      config: LanguageConfig
  ): IO[CompilationResult] =
    // Try to register as the compiler for this key
    inProcessState.tryRegister(key).flatMap {
      case Left(existing) =>
        // Another fiber is already compiling - wait for their result
        existing.get

      case Right(deferred) =>
        // We're responsible for compiling
        // Use guaranteeCase to ensure deferred is completed even on error/cancellation
        doCompile(key, sources, classpath, outputDir, config)
          .guaranteeCase {
            case cats.effect.Outcome.Succeeded(fa) =>
              fa.flatMap(result => inProcessState.complete(key, result))
            case cats.effect.Outcome.Errored(e) =>
              // On error, complete with failure result so waiting fibers don't hang
              inProcessState.complete(
                key,
                CompilationFailure(
                  List(
                    CompilerError(None, 0, 0, s"Internal error: ${e.getMessage}", None, CompilerError.Severity.Error)
                  )
                )
              )
            case cats.effect.Outcome.Canceled() =>
              // On cancellation, complete with cancelled result
              inProcessState.complete(key, CompilationCancelled)
          }
    }

  private def doCompile(
      key: CompileKey,
      sources: Map[Path, String],
      classpath: Seq[Path],
      outputDir: Path,
      config: LanguageConfig
  ): IO[CompilationResult] = {
    // Use output directory for lock to prevent concurrent writes to same location
    // Use SHA-256 hash to avoid collision issues with hashCode()
    val digest = MessageDigest.getInstance("SHA-256")
    val pathHash = digest
      .digest(outputDir.toString.getBytes("UTF-8"))
      .take(16)
      .map("%02x".format(_))
      .mkString
    val lockPath = lockDir.resolve(s"$pathHash.lock")

    CrossProcessLock.acquire(lockPath, 10.minutes).flatMap {
      case Some(lock) =>
        performCompilation(sources, classpath, outputDir, config)
          .guarantee(lock.release)

      case None =>
        // Couldn't acquire lock after timeout - compile anyway (might fail)
        // This is a fallback for edge cases
        performCompilation(sources, classpath, outputDir, config)
    }
  }

  private def performCompilation(
      sources: Map[Path, String],
      classpath: Seq[Path],
      outputDir: Path,
      config: LanguageConfig
  ): IO[CompilationResult] =
    IO.blocking {
      Files.createDirectories(outputDir)

      config match {
        case kotlinConfig: KotlinConfig =>
          compileMixedKotlinJava(sources, classpath, outputDir, kotlinConfig)
        case _ =>
          val sourceFiles = sources.map { case (path, content) => SourceFile(path, content) }.toSeq
          val input = CompilationInput(sourceFiles, classpath, outputDir, config)
          val compiler = Compiler.forConfig(config)
          compiler.compile(input, DiagnosticListener.noop, CancellationToken.never)
      }
    }
  /** Mixed Kotlin+Java compilation: Java first, then Kotlin.
    *
    * Java is compiled first so that Kotlin can resolve Java types from the output directory. This handles the common case of generated Java sources (Avro,
    * OpenAPI) in Kotlin projects. For the reverse case (Java depending on Kotlin), a compile order flag could be added to the model later.
    */
  private def compileMixedKotlinJava(
      sources: Map[Path, String],
      classpath: Seq[Path],
      outputDir: Path,
      kotlinConfig: KotlinConfig
  ): CompilationResult = {
    val kotlinSources = sources.filter { case (p, _) => p.toString.endsWith(".kt") || p.toString.endsWith(".kts") }
    val javaSources = sources.filter { case (p, _) => p.toString.endsWith(".java") }

    if (javaSources.isEmpty) {
      // Pure Kotlin — no split needed
      val sourceFiles = sources.map { case (path, content) => SourceFile(path, content) }.toSeq
      val input = CompilationInput(sourceFiles, classpath, outputDir, kotlinConfig)
      return KotlinSourceCompiler.compile(input, DiagnosticListener.noop, CancellationToken.never)
    }

    // Step 1: Compile Java with javac
    val javaSourceFiles = javaSources.map { case (path, content) => SourceFile(path, content) }.toSeq
    val javaConfig = JavaConfig(release = Some(kotlinConfig.jvmTarget.toIntOption.getOrElse(11)))
    val javaInput = CompilationInput(javaSourceFiles, classpath, outputDir, javaConfig)
    val javaResult = Compiler.forConfig(javaConfig).compile(javaInput, DiagnosticListener.noop, CancellationToken.never)

    javaResult match {
      case _: CompilationSuccess =>
        if (kotlinSources.nonEmpty) {
          // Step 2: Compile Kotlin — only .kt files, Java .class files already in outputDir
          val kotlinSourceFiles = kotlinSources.map { case (path, content) => SourceFile(path, content) }.toSeq
          val kotlinClasspath = classpath :+ outputDir
          val kotlinInput = CompilationInput(kotlinSourceFiles, kotlinClasspath, outputDir, kotlinConfig)
          KotlinSourceCompiler.compile(kotlinInput, DiagnosticListener.noop, CancellationToken.never)
        } else javaResult
      case failed => failed
    }
  }
}

object CompilationCoordinator {

  /** Create a new compilation coordinator with shared in-process state */
  def create(lockDir: Path): IO[CompilationCoordinator] =
    IO {
      val inProcessState = new InProcessCompilationState()
      new CompilationCoordinator(lockDir, inProcessState)
    }

  /** Create with default lock directory */
  def create(workspaceRoot: Path, name: String): IO[CompilationCoordinator] =
    create(workspaceRoot.resolve(".bleep").resolve("locks").resolve(name))

  /** Shared global instance for the process (singleton pattern) */
  private val globalState = new InProcessCompilationState()

  /** Get a coordinator using shared global in-process state */
  def withGlobalState(lockDir: Path): CompilationCoordinator =
    new CompilationCoordinator(lockDir, globalState)
}
