package bleep.analysis

import java.io.File
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** Kotlin compiler for bleep-bsp with incremental compilation support.
  *
  * This compiler runs as part of the bleep-bsp daemon and provides:
  *   - Cached compiler instances and reflection setup (via CompilerResolver)
  *   - Incremental compilation using Kotlin's IncrementalJvmCompilerRunner
  *   - Cross-language incremental compilation support (detects Scala/Java changes)
  *   - Falls back to full compilation if incremental setup fails
  *   - Diagnostic streaming via MessageCollector proxy
  *   - Cancellation support via Services
  *
  * The incremental compilation uses Kotlin's built-in IC infrastructure, caching analysis in the output directory for fast recompilation.
  *
  * ==Cross-Language Incremental Compilation==
  *
  * When a Kotlin project depends on Scala/Java projects, we need to detect when those dependencies change and trigger a recompilation. Kotlin's native IC
  * doesn't track classpath changes by default (ClasspathSnapshotDisabled).
  *
  * We implement classpath change detection by:
  *   1. Computing a hash of the classpath (paths + modification times)
  *   2. Storing the hash in the IC cache directory
  *   3. On each compilation, comparing current hash with stored hash
  *   4. If different, invalidating the IC cache to force full recompilation
  *
  * This ensures that when upstream Scala/Java projects are recompiled, dependent Kotlin projects will also be recompiled correctly.
  */
object KotlinSourceCompiler extends Compiler {

  private val debugEnabled = System.getProperty("bleep.kotlin.debug", "false").toBoolean

  private def debug(msg: String): Unit =
    if (debugEnabled) System.err.println(s"[KotlinSourceCompiler] $msg")

  /** Cached reflection setup per compiler version */
  private case class CachedCompilerSetup(
      loader: ClassLoader,
      // Core compiler classes
      k2CompilerClass: Class[?],
      compilerInstance: Any,
      argumentsClass: Class[?],
      messageCollectorClass: Class[?],
      severityClass: Class[?],
      locationClass: Class[?],
      servicesClass: Class[?],
      servicesBuilderClass: Class[?],
      exitCodeClass: Class[?],
      canceledStatusClass: Class[?],
      canceledExceptionClass: Class[?],
      exitCodeOK: Any,
      severityError: Any,
      severityException: Any,
      execMethod: java.lang.reflect.Method,
      // Incremental compilation classes (may be null if not available)
      incrementalRunnerClass: Class[?],
      buildReporterInstance: Any,
      classpathChangesInstance: Any,
      changedFilesUnknownInstance: Any,
      icFeaturesClass: Class[?],
      fileLocationsClass: Class[?]
  )

  private val setupCache = new ConcurrentHashMap[String, CachedCompilerSetup]()

  // ==========================================================================
  // Classpath Change Detection
  // ==========================================================================

  /** Compute a hash of the classpath based on paths and modification times.
    *
    * This allows us to detect when dependencies have changed and invalidate the Kotlin IC cache accordingly. Changes in upstream Scala/Java projects will be
    * reflected in their output directory modification times.
    */
  private def computeClasspathHash(classpath: Seq[Path]): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    classpath.sorted.foreach { path =>
      // Include the path itself
      digest.update(path.toAbsolutePath.toString.getBytes("UTF-8"))
      digest.update(0.toByte) // separator

      // Include modification time if path exists
      val mtime = Try(Files.getLastModifiedTime(path).toMillis).getOrElse(0L)
      digest.update(java.lang.Long.toString(mtime).getBytes("UTF-8"))
      digest.update(0.toByte) // separator

      // For directories, also check the most recent class file modification
      if (Files.isDirectory(path)) {
        val latestClassMtime = Try {
          import scala.jdk.StreamConverters.*
          import scala.util.Using
          // Use Using to ensure Files.walk stream is properly closed
          Using(Files.walk(path)) { stream =>
            stream
              .toScala(LazyList)
              .filter(p => p.toString.endsWith(".class"))
              .map(p => Files.getLastModifiedTime(p).toMillis)
              .maxOption
              .getOrElse(0L)
          }.getOrElse(0L)
        }.getOrElse(0L)
        digest.update(java.lang.Long.toString(latestClassMtime).getBytes("UTF-8"))
        digest.update(0.toByte)
      }
    }
    digest.digest().map("%02x".format(_)).mkString
  }

  /** Check if classpath has changed since last compilation.
    *
    * @param cacheDir
    *   the Kotlin IC cache directory
    * @param classpath
    *   the current classpath
    * @return
    *   true if classpath changed (or no previous hash), false if unchanged
    */
  private def classpathChanged(cacheDir: Path, classpath: Seq[Path]): Boolean = {
    val hashFile = cacheDir.resolve("classpath-hash")
    val currentHash = computeClasspathHash(classpath)

    val previousHash = Try(Files.readString(hashFile).trim).getOrElse("")

    if (currentHash != previousHash) {
      // Save the new hash
      Files.createDirectories(cacheDir)
      Files.writeString(hashFile, currentHash)
      true
    } else {
      false
    }
  }

  /** Invalidate the Kotlin IC cache by deleting all files except the hash file.
    *
    * This forces a full recompilation on the next build.
    */
  private def invalidateCache(cacheDir: Path): Unit =
    if (Files.exists(cacheDir)) {
      import scala.jdk.StreamConverters.*
      import scala.util.Using
      val hashFile = cacheDir.resolve("classpath-hash")

      // Use Using to ensure Files.walk stream is properly closed
      Using(Files.walk(cacheDir)) { stream =>
        stream
          .toScala(LazyList)
          .filter(p => Files.isRegularFile(p) && p != hashFile)
          .foreach { p =>
            Try(Files.delete(p))
          }
      }

      // Also delete subdirectories (but not the cache dir itself)
      Using(Files.walk(cacheDir)) { stream =>
        stream
          .toScala(LazyList)
          .filter(p => Files.isDirectory(p) && p != cacheDir)
          .toList
          .sortBy(_.toString)(Ordering[String].reverse) // Delete deepest first
          .foreach { p =>
            Try(Files.delete(p))
          }
      }

      debug("Invalidated IC cache due to classpath change")
    }

  // ==========================================================================
  // Compilation
  // ==========================================================================

  override def compile(
      input: CompilationInput,
      listener: DiagnosticListener,
      cancellation: CancellationToken
  ): CompilationResult = {
    if cancellation.isCancelled then return CompilationCancelled

    val config = input.config match {
      case c: KotlinConfig => c
      case other =>
        val err = CompilerError(None, 0, 0, s"KotlinSourceCompiler requires KotlinConfig, got ${other.getClass.getSimpleName}")
        listener.onDiagnostic(err)
        return CompilationFailure(List(err))
    }

    // Write sources to temp directory (like Java and Scala compilers do)
    val tempDir = Files.createTempDirectory("kotlin-compile-")

    try {
      Files.createDirectories(input.outputDir)
      if cancellation.isCancelled then return CompilationCancelled

      // Write source content to temp directory and get the paths
      val sourcePaths = writeSourcesToDir(input.sources, tempDir)
      if (sourcePaths.isEmpty) {
        return CompilationSuccess(input.outputDir, Set.empty)
      }

      if cancellation.isCancelled then return CompilationCancelled

      // Get or create cached compiler setup
      val setup = getOrCreateSetup(config.version)

      // Try incremental compilation first, fall back to full compilation
      val result = if setup.incrementalRunnerClass != null then {
        debug(s"Compiling ${sourcePaths.size} Kotlin files (incremental)")
        compileIncremental(setup, config, sourcePaths, input, listener, cancellation)
      } else {
        debug(s"Compiling ${sourcePaths.size} Kotlin files (full)")
        compileWithReflection(setup, config, sourcePaths, input, listener, cancellation)
      }

      result match {
        case CompilationSuccess(_, classes) =>
          debug(s"Success: ${classes.size} class files")
        case CompilationFailure(errs) =>
          debug(s"Compilation failed with ${errs.size} error(s)")
          errs.take(5).foreach(e => debug(s"  ${e.formatted}"))
          if errs.size > 5 then debug(s"  ... and ${errs.size - 5} more")
        case CompilationCancelled =>
          debug("Compilation cancelled")
      }

      if cancellation.isCancelled then CompilationCancelled else result
    } catch {
      case e: Exception =>
        debug(s"Exception: ${e.getClass.getName}: ${e.getMessage}")
        e.printStackTrace(System.err)
        val err = CompilerError(None, 0, 0, s"Compilation failed: ${e.getMessage}")
        listener.onDiagnostic(err)
        CompilationFailure(List(err))
    } finally
      // Clean up temp directory
      deleteTempDir(tempDir)
  }

  /** Recursively delete a directory */
  private def deleteTempDir(dir: Path): Unit =
    if (Files.exists(dir)) {
      import scala.jdk.StreamConverters.*
      import scala.util.Using
      try
        // Use Using to ensure Files.walk stream is properly closed
        Using(Files.walk(dir)) { stream =>
          stream
            .toScala(LazyList)
            .sorted(Ordering[String].reverse.on[Path](_.toString)) // Delete deepest first
            .foreach(p => Try(Files.delete(p)))
        }
      catch {
        case _: Exception => () // Ignore cleanup errors
      }
    }

  /** Get or create cached compiler setup for a version */
  private def getOrCreateSetup(version: String): CachedCompilerSetup =
    setupCache.computeIfAbsent(
      version,
      _ => {
        val instance = CompilerResolver.getKotlinCompiler(version)
        val loader = instance.loader

        // Load core classes
        val k2CompilerClass = loader.loadClass("org.jetbrains.kotlin.cli.jvm.K2JVMCompiler")
        val argumentsClass = loader.loadClass("org.jetbrains.kotlin.cli.common.arguments.K2JVMCompilerArguments")
        val messageCollectorClass = loader.loadClass("org.jetbrains.kotlin.cli.common.messages.MessageCollector")
        val severityClass = loader.loadClass("org.jetbrains.kotlin.cli.common.messages.CompilerMessageSeverity")
        val locationClass = loader.loadClass("org.jetbrains.kotlin.cli.common.messages.CompilerMessageSourceLocation")
        val servicesClass = loader.loadClass("org.jetbrains.kotlin.config.Services")
        val servicesBuilderClass = loader.loadClass("org.jetbrains.kotlin.config.Services$Builder")
        val exitCodeClass = loader.loadClass("org.jetbrains.kotlin.cli.common.ExitCode")
        val canceledStatusClass = loader.loadClass("org.jetbrains.kotlin.progress.CompilationCanceledStatus")
        val canceledExceptionClass = loader.loadClass("org.jetbrains.kotlin.progress.CompilationCanceledException")

        val exitCodeOK = exitCodeClass.getField("OK").get(null)
        val severityError = severityClass.getField("ERROR").get(null)
        val severityException = severityClass.getField("EXCEPTION").get(null)

        // Note: We don't cache the compiler instance because K2JVMCompiler has internal state
        // that doesn't reset properly between invocations, causing issues when compiling
        // multiple files or switching between incremental/non-incremental modes.
        // Instead, we create a new instance for each compilation (see compileWithReflection).
        val compilerInstance: Any = null // Placeholder - created fresh in compileWithReflection

        val execMethod = k2CompilerClass.getMethods
          .find { m =>
            m.getName == "exec" &&
            m.getParameterCount == 3 &&
            m.getParameterTypes()(0).getName == "org.jetbrains.kotlin.cli.common.messages.MessageCollector" &&
            m.getParameterTypes()(1).getName == "org.jetbrains.kotlin.config.Services"
          }
          .getOrElse(throw new NoSuchMethodException("Could not find K2JVMCompiler.exec method"))

        // Try to load incremental compilation support
        val (incrementalRunnerClass, buildReporterInstance, classpathChangesInstance, changedFilesUnknownInstance, icFeaturesClass, fileLocationsClass) =
          try {
            val runnerClass = loader.loadClass("org.jetbrains.kotlin.incremental.IncrementalJvmCompilerRunner")

            // DoNothingBuildReporter.INSTANCE
            val buildReporterClass = loader.loadClass("org.jetbrains.kotlin.build.report.DoNothingBuildReporter")
            val buildReporter = buildReporterClass.getField("INSTANCE").get(null)

            // ClasspathChanges.ClasspathSnapshotDisabled.INSTANCE
            val classpathChangesClass = loader.loadClass("org.jetbrains.kotlin.incremental.ClasspathChanges$ClasspathSnapshotDisabled")
            val classpathChanges = classpathChangesClass.getField("INSTANCE").get(null)

            // ChangedFiles.Unknown.INSTANCE
            val changedFilesClass = loader.loadClass("org.jetbrains.kotlin.incremental.ChangedFiles$Unknown")
            val changedFilesUnknown = changedFilesClass.getField("INSTANCE").get(null)

            // IncrementalCompilationFeatures class (has default constructor)
            val featuresClass = loader.loadClass("org.jetbrains.kotlin.incremental.IncrementalCompilationFeatures")

            // FileLocations class
            val locationsClass = loader.loadClass("org.jetbrains.kotlin.incremental.storage.FileLocations")

            debug(s"Loaded IncrementalJvmCompilerRunner for Kotlin $version")
            (runnerClass, buildReporter, classpathChanges, changedFilesUnknown, featuresClass, locationsClass)
          } catch {
            case e: ClassNotFoundException =>
              debug(s"Incremental compilation not available: ${e.getMessage}")
              (null, null, null, null, null, null)
            case e: Exception =>
              debug(s"Failed to load incremental classes: ${e.getClass.getName}: ${e.getMessage}")
              (null, null, null, null, null, null)
          }

        CachedCompilerSetup(
          loader = loader,
          k2CompilerClass = k2CompilerClass,
          compilerInstance = compilerInstance,
          argumentsClass = argumentsClass,
          messageCollectorClass = messageCollectorClass,
          severityClass = severityClass,
          locationClass = locationClass,
          servicesClass = servicesClass,
          servicesBuilderClass = servicesBuilderClass,
          exitCodeClass = exitCodeClass,
          canceledStatusClass = canceledStatusClass,
          canceledExceptionClass = canceledExceptionClass,
          exitCodeOK = exitCodeOK,
          severityError = severityError,
          severityException = severityException,
          execMethod = execMethod,
          incrementalRunnerClass = incrementalRunnerClass,
          buildReporterInstance = buildReporterInstance,
          classpathChangesInstance = classpathChangesInstance,
          changedFilesUnknownInstance = changedFilesUnknownInstance,
          icFeaturesClass = icFeaturesClass,
          fileLocationsClass = fileLocationsClass
        )
      }
    )

  /** Compile using Kotlin's IncrementalJvmCompilerRunner */
  private def compileIncremental(
      setup: CachedCompilerSetup,
      config: KotlinConfig,
      sourceFiles: Seq[Path],
      input: CompilationInput,
      listener: DiagnosticListener,
      cancellation: CancellationToken
  ): CompilationResult = {
    val loader = setup.loader
    val collectedErrors = mutable.ListBuffer[CompilerError]()

    try {
      // Cache directory for incremental compilation state
      val cacheDir = input.outputDir.resolve(".kotlin-ic")
      Files.createDirectories(cacheDir)

      // Check if classpath has changed since last compilation
      // If so, invalidate the IC cache to ensure correct recompilation
      if (classpathChanged(cacheDir, input.classpath)) {
        invalidateCache(cacheDir)
      }

      val cacheDirFile = cacheDir.toFile

      // Source files as java.io.File list
      val sourceFilesList = sourceFiles.map(_.toAbsolutePath.toFile).asJava

      // Kotlin source file extensions
      val kotlinExtensions = Set("kt", "kts").asJava

      // Create IncrementalCompilationFeatures with default constructor
      val icFeatures = setup.icFeaturesClass.getDeclaredConstructor().newInstance()

      // Create FileLocations(rootProjectDir, buildDir)
      // Use output dir's parent as root, output dir as build dir
      val rootDir = input.outputDir.getParent.toFile
      val buildDir = input.outputDir.toFile
      val fileLocations = setup.fileLocationsClass
        .getDeclaredConstructor(classOf[File], classOf[File])
        .newInstance(rootDir, buildDir)

      // Report all files being compiled
      // (Kotlin incremental compiler doesn't expose per-file callbacks, so report all at start)
      sourceFiles.foreach(path => listener.onCompileFile(path, Some("kotlinc-incremental")))

      // Create IncrementalJvmCompilerRunner
      // Constructor: (workingDir, reporter, buildHistoryFile, outputDirs, classpathChanges, kotlinExtensions, icFeatures)
      // Note: outputDirs must contain both classesDir (destination) and workingDir
      val outputDirs: java.util.Collection[File] = Set(input.outputDir.toFile, cacheDirFile).asJava

      val runnerConstructor = setup.incrementalRunnerClass.getDeclaredConstructors
        .find(_.getParameterCount == 7)
        .orElse(setup.incrementalRunnerClass.getDeclaredConstructors.find(_.getParameterCount == 6))
        .getOrElse(throw new NoSuchMethodException("Could not find IncrementalJvmCompilerRunner constructor"))

      val runner = if runnerConstructor.getParameterCount == 7 then {
        // Kotlin 2.x constructor: (workingDir, reporter, buildHistoryFile, outputDirs, classpathChanges, extensions, features)
        val buildHistoryFile = cacheDir.resolve("build-history.bin").toFile
        runnerConstructor.newInstance(
          cacheDirFile,
          setup.buildReporterInstance,
          buildHistoryFile,
          outputDirs,
          setup.classpathChangesInstance,
          kotlinExtensions,
          icFeatures
        )
      } else {
        // Older constructor: (workingDir, reporter, outputDirs, classpathChanges, extensions, features)
        runnerConstructor.newInstance(
          cacheDirFile,
          setup.buildReporterInstance,
          outputDirs,
          setup.classpathChangesInstance,
          kotlinExtensions,
          icFeatures
        )
      }

      // Create K2JVMCompilerArguments
      val arguments = setup.argumentsClass.getDeclaredConstructor().newInstance()

      // Set module name (required for incremental compilation)
      val setModuleName = setup.argumentsClass.getMethod("setModuleName", classOf[String])
      val moduleName = input.outputDir.getFileName.toString.replaceAll("[^a-zA-Z0-9_]", "_")
      setModuleName.invoke(arguments, moduleName)

      // Set destination
      val setDestination = setup.argumentsClass.getMethod("setDestination", classOf[String])
      setDestination.invoke(arguments, input.outputDir.toString)

      // Set classpath
      if input.classpath.nonEmpty then {
        val setClasspath = setup.argumentsClass.getMethod("setClasspath", classOf[String])
        setClasspath.invoke(arguments, input.classpath.map(_.toString).mkString(java.io.File.pathSeparator))
      }

      // Set JVM target
      val setJvmTarget = setup.argumentsClass.getMethod("setJvmTarget", classOf[String])
      setJvmTarget.invoke(arguments, config.jvmTarget)

      // Set source files via freeArgs
      val setFreeArgs = setup.argumentsClass.getMethod("setFreeArgs", classOf[java.util.List[?]])
      setFreeArgs.invoke(arguments, sourceFiles.map(_.toAbsolutePath.toString).asJava)

      // Suppress warnings
      val setSuppressWarnings = setup.argumentsClass.getMethod("setSuppressWarnings", classOf[Boolean])
      setSuppressWarnings.invoke(arguments, java.lang.Boolean.TRUE)

      // Create MessageCollector proxy
      val messageCollectorProxy = createMessageCollectorProxy(setup, listener, collectedErrors, cancellation)

      // Find the compile method
      // compile(sources: List<File>, args: Args, messageCollector: MessageCollector, changedFiles: ChangedFiles, fileLocations: FileLocations)
      val compileMethod = setup.incrementalRunnerClass.getMethods
        .find { m =>
          m.getName == "compile" && m.getParameterCount == 5
        }
        .getOrElse(throw new NoSuchMethodException("Could not find IncrementalJvmCompilerRunner.compile method"))

      // Redirect System.out/err during compile — the Kotlin compiler writes directly to these
      // streams on errors, which can corrupt BSP JSON-RPC or cause pipe deadlocks in tests.
      val savedOut = System.out
      val savedErr = System.err
      val capturedOutput = new java.io.ByteArrayOutputStream()
      val capturedStream = new java.io.PrintStream(capturedOutput, true)
      val exitCode =
        try {
          System.setOut(capturedStream)
          System.setErr(capturedStream)

          compileMethod.invoke(
            runner,
            sourceFilesList,
            arguments,
            messageCollectorProxy,
            setup.changedFilesUnknownInstance,
            fileLocations
          )
        } finally {
          System.setOut(savedOut)
          System.setErr(savedErr)

          val captured = capturedOutput.toString.trim
          if (captured.nonEmpty) {
            debug(s"Kotlin compiler output: $captured")
          }
        }

      if cancellation.isCancelled then return CompilationCancelled

      if exitCode == setup.exitCodeOK then {
        val classFiles = collectClassFiles(input.outputDir)
        CompilationSuccess(input.outputDir, classFiles)
      } else {
        if collectedErrors.isEmpty then {
          val err = CompilerError(None, 0, 0, s"Kotlin incremental compilation failed with exit code: $exitCode")
          listener.onDiagnostic(err)
          collectedErrors += err
        }
        CompilationFailure(collectedErrors.toList)
      }
    } catch {
      case e: java.lang.reflect.InvocationTargetException =>
        val cause = Option(e.getCause).getOrElse(e)
        if cause.getClass.getName.contains("CompilationCanceledException") || cancellation.isCancelled then CompilationCancelled
        else {
          val msg = s"Kotlin compilation failed: ${cause.getClass.getName}: ${cause.getMessage}"
          if debugEnabled then cause.printStackTrace(System.err)
          val err = CompilerError(None, 0, 0, msg)
          listener.onDiagnostic(err)
          CompilationFailure(List(err))
        }
      case e: Exception =>
        val msg = s"Kotlin compilation failed: ${e.getClass.getName}: ${e.getMessage}"
        if debugEnabled then e.printStackTrace(System.err)
        val err = CompilerError(None, 0, 0, msg)
        listener.onDiagnostic(err)
        CompilationFailure(List(err))
    }
  }

  /** Compile using direct K2JVMCompiler invocation (fallback) */
  private def compileWithReflection(
      setup: CachedCompilerSetup,
      config: KotlinConfig,
      sourceFiles: Seq[Path],
      input: CompilationInput,
      listener: DiagnosticListener,
      cancellation: CancellationToken
  ): CompilationResult = {
    // Run compilation in a separate thread to avoid thread-local leaks from the Kotlin compiler
    // See: https://youtrack.jetbrains.com/issue/KT-28037
    val resultHolder = new java.util.concurrent.atomic.AtomicReference[CompilationResult]()
    val compileThread =
      new Thread(() => resultHolder.set(compileWithReflectionImpl(setup, config, sourceFiles, input, listener, cancellation)), "kotlin-compiler")
    compileThread.setContextClassLoader(setup.loader)
    compileThread.start()
    compileThread.join(5 * 60 * 1000L) // 5 minute timeout
    if (compileThread.isAlive) {
      compileThread.interrupt()
      val err = CompilerError(None, 0, 0, "Kotlin compilation timed out after 5 minutes")
      listener.onDiagnostic(err)
      CompilationFailure(List(err))
    } else {
      resultHolder.get()
    }
  }

  /** Internal implementation of reflection-based compilation */
  private def compileWithReflectionImpl(
      setup: CachedCompilerSetup,
      config: KotlinConfig,
      sourceFiles: Seq[Path],
      input: CompilationInput,
      listener: DiagnosticListener,
      cancellation: CancellationToken
  ): CompilationResult = {
    val loader = setup.loader
    val collectedErrors = mutable.ListBuffer[CompilerError]()

    try {
      // Create CompilationCanceledStatus proxy
      val canceledStatusProxy = java.lang.reflect.Proxy.newProxyInstance(
        loader,
        Array(setup.canceledStatusClass),
        new java.lang.reflect.InvocationHandler {
          override def invoke(proxy: Any, method: java.lang.reflect.Method, methodArgs: Array[AnyRef]): AnyRef =
            method.getName match {
              case "checkCanceled" =>
                if cancellation.isCancelled then throw setup.canceledExceptionClass.getDeclaredConstructor().newInstance().asInstanceOf[Throwable]
                null
              case _ => null
            }
        }
      )

      // Build Services with cancellation status
      val servicesBuilder = setup.servicesBuilderClass.getDeclaredConstructor().newInstance()
      val registerMethod = setup.servicesBuilderClass.getMethod("register", classOf[Class[?]], classOf[Any])
      registerMethod.invoke(servicesBuilder, setup.canceledStatusClass, canceledStatusProxy)
      val buildMethod = setup.servicesBuilderClass.getMethod("build")
      val services = buildMethod.invoke(servicesBuilder)

      // Create K2JVMCompilerArguments
      val arguments = setup.argumentsClass.getDeclaredConstructor().newInstance()

      // Set module name (required for compilation)
      val setModuleName = setup.argumentsClass.getMethod("setModuleName", classOf[String])
      val moduleName = input.outputDir.getFileName.toString.replaceAll("[^a-zA-Z0-9_]", "_")
      setModuleName.invoke(arguments, moduleName)

      // Set destination
      val setDestination = setup.argumentsClass.getMethod("setDestination", classOf[String])
      setDestination.invoke(arguments, input.outputDir.toString)

      // Set classpath
      if input.classpath.nonEmpty then {
        val setClasspath = setup.argumentsClass.getMethod("setClasspath", classOf[String])
        setClasspath.invoke(arguments, input.classpath.map(_.toString).mkString(java.io.File.pathSeparator))
      }

      // Set JVM target
      val setJvmTarget = setup.argumentsClass.getMethod("setJvmTarget", classOf[String])
      setJvmTarget.invoke(arguments, config.jvmTarget)

      // Set source files via freeArgs
      val setFreeArgs = setup.argumentsClass.getMethod("setFreeArgs", classOf[java.util.List[?]])
      setFreeArgs.invoke(arguments, sourceFiles.map(_.toAbsolutePath.toString).asJava)

      // Suppress warnings
      val setSuppressWarnings = setup.argumentsClass.getMethod("setSuppressWarnings", classOf[Boolean])
      setSuppressWarnings.invoke(arguments, java.lang.Boolean.TRUE)

      // Create MessageCollector proxy
      val messageCollectorProxy = createMessageCollectorProxy(setup, listener, collectedErrors, cancellation)

      // Report all files being compiled
      sourceFiles.foreach(path => listener.onCompileFile(path, Some("kotlinc")))

      // Create a fresh compiler instance for each compilation
      // K2JVMCompiler has internal state that doesn't reset properly between invocations
      val compilerInstance = setup.k2CompilerClass.getDeclaredConstructor().newInstance()

      // Redirect System.out/err during exec() — the Kotlin compiler writes directly to these
      // streams on errors, which can corrupt BSP JSON-RPC or cause pipe deadlocks in tests.
      val savedOut = System.out
      val savedErr = System.err
      val capturedOutput = new java.io.ByteArrayOutputStream()
      val capturedStream = new java.io.PrintStream(capturedOutput, true)
      val exitCode =
        try {
          System.setOut(capturedStream)
          System.setErr(capturedStream)

          // Call exec() on the fresh compiler instance
          setup.execMethod.invoke(compilerInstance, messageCollectorProxy, services, arguments)
        } finally {
          System.setOut(savedOut)
          System.setErr(savedErr)

          // Log captured output if non-empty (for debugging)
          val captured = capturedOutput.toString.trim
          if (captured.nonEmpty) {
            debug(s"Kotlin compiler output: $captured")
          }
        }

      if cancellation.isCancelled then return CompilationCancelled

      if exitCode == setup.exitCodeOK then {
        val classFiles = collectClassFiles(input.outputDir)
        CompilationSuccess(input.outputDir, classFiles)
      } else {
        if collectedErrors.isEmpty then {
          val err = CompilerError(None, 0, 0, s"Kotlin compilation failed with exit code: $exitCode")
          listener.onDiagnostic(err)
          collectedErrors += err
        }
        CompilationFailure(collectedErrors.toList)
      }
    } catch {
      case e: java.lang.reflect.InvocationTargetException =>
        val cause = Option(e.getCause).getOrElse(e)
        if cause.getClass.getName.contains("CompilationCanceledException") || cancellation.isCancelled then CompilationCancelled
        else {
          val err = CompilerError(None, 0, 0, s"Compiler invocation failed: ${cause.getMessage}")
          listener.onDiagnostic(err)
          CompilationFailure(List(err))
        }
      case e: Exception =>
        val err = CompilerError(None, 0, 0, s"Reflection failed: ${e.getClass.getName}: ${e.getMessage}")
        listener.onDiagnostic(err)
        CompilationFailure(List(err))
    }
  }

  /** Create a MessageCollector proxy that streams diagnostics */
  private def createMessageCollectorProxy(
      setup: CachedCompilerSetup,
      listener: DiagnosticListener,
      collectedErrors: mutable.ListBuffer[CompilerError],
      cancellation: CancellationToken
  ): Any =
    java.lang.reflect.Proxy.newProxyInstance(
      setup.loader,
      Array(setup.messageCollectorClass),
      new java.lang.reflect.InvocationHandler {
        override def invoke(proxy: Any, method: java.lang.reflect.Method, methodArgs: Array[AnyRef]): AnyRef =
          method.getName match {
            case "clear" => null
            case "hasErrors" =>
              if cancellation.isCancelled then throw setup.canceledExceptionClass.getDeclaredConstructor().newInstance().asInstanceOf[Throwable]
              java.lang.Boolean.valueOf(collectedErrors.exists(_.severity == CompilerError.Severity.Error))
            case "report" if methodArgs != null && methodArgs.length == 3 =>
              if cancellation.isCancelled then throw setup.canceledExceptionClass.getDeclaredConstructor().newInstance().asInstanceOf[Throwable]
              val severity = methodArgs(0)
              val message = methodArgs(1).asInstanceOf[String]
              val location = methodArgs(2)

              val isError = severity == setup.severityError || severity == setup.severityException

              if isError then {
                val (path, line, col) =
                  if location != null then
                    try {
                      val getPath = setup.locationClass.getMethod("getPath")
                      val getLine = setup.locationClass.getMethod("getLine")
                      val getColumn = setup.locationClass.getMethod("getColumn")

                      val pathStr = getPath.invoke(location).asInstanceOf[String]
                      val lineNum = getLine.invoke(location).asInstanceOf[Int]
                      val colNum = getColumn.invoke(location).asInstanceOf[Int]

                      (Some(Path.of(pathStr)), lineNum, colNum)
                    } catch {
                      case _: Exception => (None, 0, 0)
                    }
                  else (None, 0, 0)

                val error = CompilerError(path, line, col, message, CompilerError.Severity.Error)
                listener.onDiagnostic(error)
                collectedErrors += error
              }
              null
            case _ => null
          }
      }
    )
}
