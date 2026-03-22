package bleep.bsp

import bleep.analysis._
import bleep.bsp.Outcome.KillReason
import bleep.bsp.TaskDag._
import cats.effect.{Deferred, IO}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
import scala.util.Using

/** Executes linking for non-JVM platforms (Scala.js, Scala Native, Kotlin/JS, Kotlin/Native).
  *
  * Orchestrates the toolchain bridges to produce runnable/testable output from compiled artifacts.
  */
object LinkExecutor {

  /** Compute config-aware output directory suffix based on platform config.
    *
    * Uses the shared method from BleepBspProtocol for consistency with bleep-core.
    */
  def linkDirSuffix(platform: LinkPlatform): String = {
    import bleep.bsp.protocol.BleepBspProtocol

    platform match {
      case p: LinkPlatform.ScalaJs =>
        BleepBspProtocol.linkDirSuffix(isRelease = p.config.optimizer, hasDebugInfo = false, hasLto = false)

      case p: LinkPlatform.ScalaNative =>
        val hasLto = p.config.lto != ScalaNativeLinkConfig.NativeLTO.None
        BleepBspProtocol.linkDirSuffix(isRelease = p.config.optimize, hasDebugInfo = false, hasLto = hasLto)

      case p: LinkPlatform.KotlinJs =>
        BleepBspProtocol.linkDirSuffix(isRelease = p.config.dce, hasDebugInfo = false, hasLto = false)

      case p: LinkPlatform.KotlinNative =>
        BleepBspProtocol.linkDirSuffix(isRelease = p.config.optimizations, hasDebugInfo = p.config.debugInfo, hasLto = false)

      case LinkPlatform.Jvm =>
        "jvm" // JVM doesn't link, but provide a value anyway
    }
  }

  /** Check if output file is up-to-date relative to input files.
    *
    * Short-circuits as soon as any input is found to be newer than the output. Uses iterators to avoid allocating intermediate collections.
    *
    * @param outputFile
    *   the linked output file
    * @param inputPaths
    *   classpath entries (directories or JARs)
    * @param logger
    *   for logging
    * @return
    *   true if output exists and is newer than all inputs
    */
  private def isUpToDate(outputFile: Path, inputPaths: Seq[Path], logger: LinkLogger): Boolean =
    if (!Files.exists(outputFile)) {
      false
    } else {
      val outputTime = Files.getLastModifiedTime(outputFile).toMillis

      // Check if any input is newer than output - short-circuit on first match
      val hasNewerInput = inputPaths.iterator.exists { inputPath =>
        if (!Files.exists(inputPath)) {
          false
        } else if (Files.isDirectory(inputPath)) {
          // Walk directory, short-circuit if any file is newer
          Using(Files.walk(inputPath)) { stream =>
            stream
              .filter(Files.isRegularFile(_))
              .anyMatch(p => Files.getLastModifiedTime(p).toMillis > outputTime)
          }.getOrElse(false)
        } else {
          // Single file (JAR, KLIB, etc.)
          Files.getLastModifiedTime(inputPath).toMillis > outputTime
        }
      }

      val upToDate = !hasNewerInput
      if (upToDate) {
        logger.info(s"[LINK] Up-to-date: $outputFile")
      }
      upToDate
    }

  /** Find all JS output files in a directory. */
  private def findJsOutputFiles(jsOutputDir: Path): Seq[Path] =
    if (!Files.exists(jsOutputDir)) Seq.empty
    else
      Using(Files.walk(jsOutputDir)) { stream =>
        stream
          .iterator()
          .asScala
          .filter { p =>
            val name = p.getFileName.toString
            Files.isRegularFile(p) && (name.endsWith(".js") || name.endsWith(".js.map"))
          }
          .toSeq
      }.getOrElse(Seq.empty)

  /** Execute a link task.
    *
    * @param task
    *   the link task to execute
    * @param classpath
    *   the classpath including compiled output and dependencies
    * @param mainClass
    *   optional main class (for applications)
    * @param outputDir
    *   base output directory for linked output
    * @param logger
    *   logger for output
    * @param killSignal
    *   Deferred to complete to request termination
    * @return
    *   task result and link result
    */
  def execute(
      task: LinkTask,
      classpath: Seq[Path],
      mainClass: Option[String],
      baseOutputDir: Path,
      logger: LinkLogger,
      killSignal: Deferred[IO, KillReason]
  ): IO[(TaskResult, LinkResult)] =
    killSignal.tryGet.flatMap {
      case Some(reason) => IO.pure((TaskResult.Killed(reason), LinkResult.Cancelled))
      case None         =>
        // Compute config-aware output directory to avoid stale binary issues
        val outputDir = baseOutputDir.resolve(linkDirSuffix(task.platform))

        task.platform match {
          case platform: LinkPlatform.ScalaJs =>
            executeScalaJs(task.project.value, platform, classpath, mainClass, outputDir, logger, killSignal, task.isTest)

          case platform: LinkPlatform.ScalaNative =>
            val resolvedMainClass = mainClass.getOrElse {
              if (task.isTest) ScalaNativeTestRunner.TestMainClass
              else throw new IllegalArgumentException("Scala Native requires a main class")
            }
            executeScalaNative(
              task.project.value,
              platform,
              classpath,
              resolvedMainClass,
              outputDir,
              logger,
              killSignal
            )

          case platform: LinkPlatform.KotlinJs =>
            executeKotlinJs(task.project.value, platform, classpath, outputDir, logger, killSignal)

          case platform: LinkPlatform.KotlinNative =>
            executeKotlinNative(task.project.value, platform, classpath, mainClass, outputDir, logger, killSignal)

          case LinkPlatform.Jvm =>
            // JVM doesn't need linking
            IO.pure((TaskResult.Success, LinkResult.NotApplicable))
        }
    }

  /** Bridge a Deferred kill signal to CancellationToken for toolchains that still use CancellationToken. */
  private def bridgeKillSignal(killSignal: Deferred[IO, KillReason]): IO[CancellationToken] = {
    import java.util.concurrent.atomic.AtomicBoolean
    import scala.collection.mutable.ListBuffer

    IO.delay {
      val cancelled = new AtomicBoolean(false)
      val callbacks = ListBuffer[() => Unit]()

      new CancellationToken {
        def isCancelled: Boolean = cancelled.get()
        def cancel(): Unit =
          if (cancelled.compareAndSet(false, true)) {
            callbacks.synchronized {
              callbacks.foreach(cb => cb())
            }
          }
        def onCancel(callback: () => Unit): Unit =
          callbacks.synchronized {
            if (cancelled.get()) callback()
            else callbacks += callback
          }
      }
    }.flatTap { token =>
      // Start a fiber that watches the kill signal and triggers the token when killed
      killSignal.get.flatMap(_ => IO.delay(token.cancel())).start.void
    }
  }

  /** Execute Scala.js linking. */
  private def executeScalaJs(
      projectName: String,
      platform: LinkPlatform.ScalaJs,
      classpath: Seq[Path],
      mainClass: Option[String],
      outputDir: Path,
      logger: LinkLogger,
      killSignal: Deferred[IO, KillReason],
      isTest: Boolean
  ): IO[(TaskResult, LinkResult)] = {
    val jsOutputDir = outputDir.resolve("js")
    val moduleName = projectName.replace("-", "_")

    // Check if up-to-date by finding existing JS output
    val existingOutputFiles = findJsOutputFiles(jsOutputDir)
    val existingMainJs = existingOutputFiles.find(p => p.toString.endsWith(".js") && !p.toString.endsWith(".map"))

    existingMainJs match {
      case Some(mainJs) if isUpToDate(mainJs, classpath, logger) =>
        // Up-to-date - return cached result
        val sourceMap = existingOutputFiles.find(_.toString.endsWith(".map"))
        IO.pure((TaskResult.Success, LinkResult.JsSuccess(mainJs, sourceMap, existingOutputFiles, wasUpToDate = true)))

      case _ =>
        // Need to link
        doScalaJsLink(platform, classpath, mainClass, jsOutputDir, moduleName, logger, killSignal, isTest)
    }
  }

  private def doScalaJsLink(
      platform: LinkPlatform.ScalaJs,
      classpath: Seq[Path],
      mainClass: Option[String],
      jsOutputDir: Path,
      moduleName: String,
      logger: LinkLogger,
      killSignal: Deferred[IO, KillReason],
      isTest: Boolean
  ): IO[(TaskResult, LinkResult)] =
    bridgeKillSignal(killSignal).flatMap { cancellation =>
      val toolchain = ScalaJsToolchain.forVersion(platform.version, platform.scalaVersion)

      val scalaJsLogger = new ScalaJsToolchain.Logger {
        def trace(message: => String): Unit = logger.trace(message)
        def debug(message: => String): Unit = logger.debug(message)
        def info(message: => String): Unit = logger.info(message)
        def warn(message: => String): Unit = logger.warn(message)
        def error(message: => String): Unit = logger.error(message)
      }

      logger.info(s"[LINK] Starting Scala.js link: classpath=${classpath.size} files, outputDir=$jsOutputDir, isTest=$isTest")

      val work = toolchain
        .link(
          platform.config,
          classpath,
          mainClass,
          jsOutputDir,
          moduleName,
          scalaJsLogger,
          cancellation,
          isTest
        )
        .map { result =>
          logger.info(s"[LINK] Scala.js link result: isSuccess=${result.isSuccess}, outputFiles=${result.outputFiles.size}, mainModule=${result.mainModule}")
          if (result.isSuccess) {
            val sourceMap = result.outputFiles.find(_.toString.endsWith(".map"))
            (TaskResult.Success, LinkResult.JsSuccess(result.mainModule, sourceMap, result.outputFiles, wasUpToDate = false))
          } else {
            logger.error(s"[LINK] Scala.js linking produced no output files in $jsOutputDir")
            (TaskResult.Failure("Scala.js linking produced no output", List.empty), LinkResult.Failure("No output files", List.empty))
          }
        }

      Outcome
        .raceKill(killSignal)(work)
        .map {
          case Left(result)  => result
          case Right(reason) => (TaskResult.Killed(reason), LinkResult.Cancelled)
        }
        .handleErrorWith {
          case _: java.util.concurrent.CancellationException =>
            killSignal.tryGet.map {
              case Some(reason) => (TaskResult.Killed(reason), LinkResult.Cancelled)
              case None         => (TaskResult.Killed(KillReason.UserRequest), LinkResult.Cancelled)
            }
          case ex =>
            // Process-level crash (not a linker error)
            IO.pure((TaskResult.Error(s"Scala.js linker crashed: ${ex.getMessage}", None, None), LinkResult.Failure(ex.getMessage, List.empty)))
        }
    }

  /** Execute Scala Native linking. */
  private def executeScalaNative(
      projectName: String,
      platform: LinkPlatform.ScalaNative,
      classpath: Seq[Path],
      mainClass: String,
      outputDir: Path,
      logger: LinkLogger,
      killSignal: Deferred[IO, KillReason]
  ): IO[(TaskResult, LinkResult)] = {
    val binaryPath = outputDir.resolve(projectName)

    // Check if up-to-date
    if (isUpToDate(binaryPath, classpath, logger)) {
      IO.pure((TaskResult.Success, LinkResult.NativeSuccess(binaryPath, wasUpToDate = true)))
    } else {
      doScalaNativeLink(projectName, platform, classpath, mainClass, binaryPath, outputDir, logger, killSignal)
    }
  }

  private def doScalaNativeLink(
      projectName: String,
      platform: LinkPlatform.ScalaNative,
      classpath: Seq[Path],
      mainClass: String,
      binaryPath: Path,
      outputDir: Path,
      logger: LinkLogger,
      killSignal: Deferred[IO, KillReason]
  ): IO[(TaskResult, LinkResult)] =
    bridgeKillSignal(killSignal).flatMap { cancellation =>
      val toolchain = ScalaNativeToolchain.forVersion(platform.version, platform.scalaVersion)
      val workDir = outputDir.resolve("native-work")

      IO.blocking(Files.createDirectories(workDir)) >> {
        val nativeLogger = new ScalaNativeToolchain.Logger {
          def trace(message: => String): Unit = logger.trace(message)
          def debug(message: => String): Unit = logger.debug(message)
          def info(message: => String): Unit = logger.info(message)
          def warn(message: => String): Unit = logger.warn(message)
          def error(message: => String): Unit = logger.error(message)
          def running(command: Seq[String]): Unit = logger.info(s"Running: ${command.mkString(" ")}")
        }

        val work = toolchain
          .link(
            platform.config,
            classpath,
            mainClass,
            binaryPath,
            workDir,
            nativeLogger,
            cancellation
          )
          .map { result =>
            if (result.isSuccess) {
              (TaskResult.Success, LinkResult.NativeSuccess(result.binary, wasUpToDate = false))
            } else {
              // Non-zero exit code from linker - this is an infrastructure error (process crashed or linker bug)
              val exitCode = result.exitCode
              val signal = if (exitCode > 128) Some(exitCode - 128) else None
              (
                TaskResult.Error(s"Scala Native linking failed with exit code $exitCode", Some(exitCode), signal),
                LinkResult.Failure(s"Exit code: $exitCode", List.empty)
              )
            }
          }

        Outcome
          .raceKill(killSignal)(work)
          .map {
            case Left(result)  => result
            case Right(reason) => (TaskResult.Killed(reason), LinkResult.Cancelled)
          }
          .handleErrorWith {
            case _: java.util.concurrent.CancellationException =>
              killSignal.tryGet.map {
                case Some(reason) => (TaskResult.Killed(reason), LinkResult.Cancelled)
                case None         => (TaskResult.Killed(KillReason.UserRequest), LinkResult.Cancelled)
              }
            case ex =>
              IO.pure((TaskResult.Error(s"Scala Native linker crashed: ${ex.getMessage}", None, None), LinkResult.Failure(ex.getMessage, List.empty)))
          }
      }
    }

  /** Execute Kotlin/JS linking: KLIB → JS.
    *
    * This phase takes KLIB files produced during compilation and links them to JavaScript.
    */
  private def executeKotlinJs(
      projectName: String,
      platform: LinkPlatform.KotlinJs,
      classpath: Seq[Path],
      outputDir: Path,
      logger: LinkLogger,
      killSignal: Deferred[IO, KillReason]
  ): IO[(TaskResult, LinkResult)] = {
    val jsOutputDir = outputDir.resolve("js")
    val moduleName = projectName.replace("-", "_")

    // Check if up-to-date by finding existing JS output
    val existingOutputFiles = findJsOutputFiles(jsOutputDir)
    val existingMainJs = existingOutputFiles.find(p => p.toString.endsWith(".js") && !p.toString.endsWith(".map"))

    existingMainJs match {
      case Some(mainJs) if isUpToDate(mainJs, classpath, logger) =>
        // Up-to-date - return cached result
        val sourceMap = existingOutputFiles.find(_.toString.endsWith(".map"))
        IO.pure((TaskResult.Success, LinkResult.JsSuccess(mainJs, sourceMap, existingOutputFiles, wasUpToDate = true)))

      case _ =>
        // Need to link
        doKotlinJsLink(projectName, platform, classpath, jsOutputDir, moduleName, logger, killSignal)
    }
  }

  private def doKotlinJsLink(
      projectName: String,
      platform: LinkPlatform.KotlinJs,
      classpath: Seq[Path],
      jsOutputDir: Path,
      moduleName: String,
      logger: LinkLogger,
      killSignal: Deferred[IO, KillReason]
  ): IO[(TaskResult, LinkResult)] =
    bridgeKillSignal(killSignal).flatMap { cancellation =>
      // Find KLIB files from classpath (project output + dependencies)
      val klibs = classpath.flatMap { p =>
        val name = p.getFileName.toString.toLowerCase
        if (name.endsWith(".klib")) {
          Seq(p)
        } else if (Files.isDirectory(p)) {
          // Check directory for KLIB files
          scala.util
            .Using(Files.list(p)) { stream =>
              import scala.jdk.CollectionConverters._
              stream.iterator().asScala.filter(_.toString.endsWith(".klib")).toSeq
            }
            .get
        } else {
          Seq.empty
        }
      }

      if (klibs.isEmpty) {
        IO.pure(
          (TaskResult.Failure(s"No KLIB files found for linking: classpath=$classpath", List.empty), LinkResult.Failure("No KLIB files found", List.empty))
        )
      } else {
        logger.info(s"[LINK] Starting Kotlin/JS link: ${klibs.size} KLIBs, outputDir=$jsOutputDir")

        // Create compiler config for linking
        val moduleKind = platform.config.moduleKind match {
          case "umd"             => KotlinJsCompilerConfig.ModuleKind.UMD
          case "commonjs"        => KotlinJsCompilerConfig.ModuleKind.CommonJS
          case "amd"             => KotlinJsCompilerConfig.ModuleKind.AMD
          case "es" | "esmodule" => KotlinJsCompilerConfig.ModuleKind.ESModule
          case _                 => KotlinJsCompilerConfig.ModuleKind.Plain
        }

        val config = KotlinJsCompilerConfig(
          kotlinVersion = platform.version,
          moduleName = moduleName,
          moduleKind = moduleKind,
          outputMode = KotlinJsCompilerConfig.OutputMode.JsExecutable,
          sourceMap = platform.config.sourceMap,
          sourceMapPrefix = None,
          sourceMapEmbedSources = KotlinJsCompilerConfig.SourceMapEmbedSources.Never,
          target = KotlinJsCompilerConfig.Target.Node,
          developmentMode = !platform.config.dce, // DCE requires production mode
          generateDts = false,
          additionalOptions = Seq.empty
        )

        val diagnosticListener = new DiagnosticListener {
          def onDiagnostic(error: CompilerError): Unit =
            if (error.severity == CompilerError.Severity.Error) {
              logger.error(s"[LINK] ${error.message}")
            } else {
              logger.warn(s"[LINK] ${error.message}")
            }
        }

        val work = KotlinJsLinker
          .link(klibs, jsOutputDir, config, diagnosticListener, cancellation)
          .map { result =>
            logger.info(s"[LINK] Kotlin/JS link result: isSuccess=${result.isSuccess}, jsFile=${result.jsFile}")
            if (result.isSuccess && result.jsFile.isDefined) {
              val allFiles =
                try
                  scala.util
                    .Using(Files.list(jsOutputDir)) { stream =>
                      import scala.jdk.CollectionConverters._
                      stream.iterator().asScala.toSeq
                    }
                    .getOrElse(Seq.empty)
                catch {
                  case _: Exception => Seq.empty
                }
              val sourceMap = allFiles.find(_.toString.endsWith(".map"))
              (TaskResult.Success, LinkResult.JsSuccess(result.jsFile.get, sourceMap, allFiles, wasUpToDate = false))
            } else {
              logger.error(s"[LINK] Kotlin/JS linking failed")
              (TaskResult.Failure("Kotlin/JS linking failed", List.empty), LinkResult.Failure("Linking failed", List.empty))
            }
          }

        Outcome
          .raceKill(killSignal)(work)
          .map {
            case Left(result)  => result
            case Right(reason) => (TaskResult.Killed(reason), LinkResult.Cancelled)
          }
          .handleErrorWith {
            case _: java.util.concurrent.CancellationException =>
              killSignal.tryGet.map {
                case Some(reason) => (TaskResult.Killed(reason), LinkResult.Cancelled)
                case None         => (TaskResult.Killed(KillReason.UserRequest), LinkResult.Cancelled)
              }
            case ex =>
              IO.pure((TaskResult.Error(s"Kotlin/JS linker crashed: ${ex.getMessage}", None, None), LinkResult.Failure(ex.getMessage, List.empty)))
          }
      }
    }

  /** Execute Kotlin/Native linking to produce binary.
    *
    * For test projects, uses -generate-test-runner to generate a test main function. For app projects, links the KLIB into an executable with the specified
    * entry point.
    */
  private def executeKotlinNative(
      projectName: String,
      platform: LinkPlatform.KotlinNative,
      classpath: Seq[Path],
      mainClass: Option[String],
      outputDir: Path,
      logger: LinkLogger,
      killSignal: Deferred[IO, KillReason]
  ): IO[(TaskResult, LinkResult)] = {
    val binaryPath = outputDir.resolve(projectName)
    val possiblePaths = Seq(
      binaryPath,
      binaryPath.resolveSibling(projectName + ".kexe")
    )

    // Check if binary already exists and is up-to-date
    possiblePaths.find(p => Files.exists(p) && isUpToDate(p, classpath, logger)) match {
      case Some(existingPath) =>
        // Up-to-date - return cached result
        IO.blocking {
          if (!Files.isExecutable(existingPath)) {
            existingPath.toFile.setExecutable(true)
          }
          (TaskResult.Success, LinkResult.NativeSuccess(existingPath, wasUpToDate = true))
        }

      case None =>
        // Need to link
        doKotlinNativeLink(projectName, platform, classpath, mainClass, binaryPath, logger, killSignal)
    }
  }

  private def doKotlinNativeLink(
      projectName: String,
      platform: LinkPlatform.KotlinNative,
      classpath: Seq[Path],
      mainClass: Option[String],
      binaryPath: Path,
      logger: LinkLogger,
      killSignal: Deferred[IO, KillReason]
  ): IO[(TaskResult, LinkResult)] =
    killSignal.tryGet.flatMap {
      case Some(reason) => IO.pure((TaskResult.Killed(reason), LinkResult.Cancelled))
      case None         =>
        // Binary doesn't exist - we need to link the KLIB
        // Find KLIB files from classpath (including the project's own KLIB)
        val klibFiles = classpath.flatMap { path =>
          val pathStr = path.toString
          if (pathStr.endsWith(".klib") && Files.exists(path)) {
            Seq(path)
          } else if (Files.isDirectory(path)) {
            // Search for .klib files inside the directory
            if (Files.exists(path.resolve("manifest"))) {
              // This is an unpacked KLIB directory
              Seq(path)
            } else {
              scala.util
                .Try {
                  Files.list(path).iterator().asScala.filter(_.toString.endsWith(".klib")).toSeq
                }
                .getOrElse(Seq.empty)
            }
          } else {
            Seq.empty
          }
        }

        if (klibFiles.isEmpty) {
          IO.pure(
            (TaskResult.Failure(s"No KLIB files found in classpath for linking", List.empty), LinkResult.Failure(s"No KLIB files to link", List.empty))
          )
        } else {
          // Link KLIBs into executable using KotlinNativeCompiler
          bridgeKillSignal(killSignal).flatMap { cancellation =>
            val diagnosticListener = new DiagnosticListener {
              def onDiagnostic(error: CompilerError): Unit =
                error.severity match {
                  case CompilerError.Severity.Error   => logger.error(error.message)
                  case CompilerError.Severity.Warning => logger.warn(error.message)
                  case CompilerError.Severity.Info    => logger.info(error.message)
                }
            }

            Files.createDirectories(binaryPath.getParent)

            // Entry point for Kotlin/Native
            // For top-level main functions, the format is: package.main
            // Given mainClass like "example.KotlinNativeAppKt", extract package "example" and use "example.main"
            val entryPoint = mainClass match {
              case Some(mc) if mc.endsWith("Kt") =>
                // Extract package (everything before the last dot, which is the class name)
                val lastDot = mc.lastIndexOf('.')
                if (lastDot > 0) {
                  mc.substring(0, lastDot) + ".main"
                } else {
                  "main"
                }
              case Some(mc) =>
                // Use as-is if it doesn't end with Kt (might be a class with main method)
                mc
              case None =>
                "main"
            }

            // For test projects, use -generate-test-runner instead of entry point.
            // The project's own KLIB must be passed with -Xinclude so the test runner
            // can discover and register test classes. Dependencies use -library.
            val isTest = platform.config.isTest
            val testRunnerFlags = if (isTest) Seq("-generate-test-runner") else Seq.empty
            val entryPointOpt = if (isTest) None else Some(entryPoint)

            // For test projects, separate the project's own KLIB (needs -Xinclude) from dependencies (use -library)
            val (includeFlags, linkLibraries) = if (isTest) {
              val projectKlib = klibFiles.filter { p =>
                val name = p.getFileName.toString.stripSuffix(".klib")
                name == projectName
              }
              val depLibs = klibFiles.filterNot(projectKlib.contains)
              val flags = projectKlib.map(p => s"-Xinclude=${p.toAbsolutePath}")
              (flags, depLibs)
            } else {
              (Seq.empty[String], klibFiles)
            }

            val nativeConfig = KotlinNativeCompilerConfig(
              kotlinVersion = platform.version,
              target = KotlinNativeCompilerConfig.Target.hostTarget,
              outputKind = KotlinNativeCompilerConfig.OutputKind.Executable,
              debuggable = platform.config.debugInfo,
              optimized = platform.config.optimizations,
              baseName = None,
              linkerOpts = Seq.empty,
              freeCompilerArgs = testRunnerFlags ++ includeFlags,
              entryPoint = entryPointOpt,
              additionalOptions = Seq.empty
            )

            logger.info(s"Linking Kotlin/Native: ${klibFiles.size} KLIBs -> $binaryPath (includes: ${includeFlags.size}, libraries: ${linkLibraries.size})")

            KotlinNativeCompiler
              .compile(
                sources = Seq.empty, // No sources - we're linking KLIBs
                libraries = linkLibraries,
                outputPath = binaryPath,
                config = nativeConfig,
                diagnosticListener = diagnosticListener,
                cancellation = cancellation
              )
              .map { result =>
                if (result.isSuccess) {
                  if (!Files.isExecutable(binaryPath)) {
                    binaryPath.toFile.setExecutable(true)
                  }
                  (TaskResult.Success, LinkResult.NativeSuccess(binaryPath, wasUpToDate = false))
                } else {
                  (
                    TaskResult.Failure(s"Kotlin/Native linking failed with exit code ${result.exitCode}", List.empty),
                    LinkResult.Failure(s"Linking failed with exit code ${result.exitCode}", List.empty)
                  )
                }
              }
              .handleError { e =>
                (
                  TaskResult.Failure(s"Kotlin/Native linking error: ${e.getMessage}", List.empty),
                  LinkResult.Failure(s"Linking error: ${e.getMessage}", List.empty)
                )
              }
          }
        }
    }

  /** Logger interface for link output. */
  trait LinkLogger {
    def trace(message: String): Unit
    def debug(message: String): Unit
    def info(message: String): Unit
    def warn(message: String): Unit
    def error(message: String): Unit
  }

  object LinkLogger {
    val Silent: LinkLogger = new LinkLogger {
      def trace(message: String): Unit = ()
      def debug(message: String): Unit = ()
      def info(message: String): Unit = ()
      def warn(message: String): Unit = ()
      def error(message: String): Unit = ()
    }

    val Console: LinkLogger = new LinkLogger {
      def trace(message: String): Unit = System.err.println(s"[trace] $message")
      def debug(message: String): Unit = System.err.println(s"[debug] $message")
      def info(message: String): Unit = System.err.println(s"[info] $message")
      def warn(message: String): Unit = System.err.println(s"[warn] $message")
      def error(message: String): Unit = System.err.println(s"[error] $message")
    }

    /** Create a logger that sends output to BSP */
    def forBsp(server: BspServer): LinkLogger = new LinkLogger {
      import ch.epfl.scala.bsp.MessageType
      def trace(message: String): Unit = () // Don't flood BSP with trace
      def debug(message: String): Unit = () // Don't flood BSP with debug
      def info(message: String): Unit = server.sendLogMessage(message, MessageType.Info)
      def warn(message: String): Unit = server.sendLogMessage(message, MessageType.Warning)
      def error(message: String): Unit = server.sendLogMessage(message, MessageType.Error)
    }

  }
}
