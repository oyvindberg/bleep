package bleep.analysis

import bleep.model.VersionKotlin
import cats.effect.IO
import java.nio.file.{Files, Path}
import java.lang.reflect.InvocationTargetException
import scala.jdk.CollectionConverters.*
import scala.collection.mutable

/** Kotlin/JS compiler.
  *
  * Uses reflection to invoke the K2JSCompiler from the Kotlin compiler, allowing different Kotlin versions to be loaded in isolated classloaders.
  */
object KotlinJsCompiler {

  /** Compile Kotlin sources to JavaScript.
    *
    * @param sources
    *   the source files to compile
    * @param libraries
    *   the library dependencies (KLIB files)
    * @param outputDir
    *   the output directory
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
      outputDir: Path,
      config: KotlinJsCompilerConfig,
      diagnosticListener: DiagnosticListener,
      cancellation: CancellationToken
  ): IO[KotlinJsCompileResult] =
    // Check cancellation before starting
    if (cancellation.isCancelled) {
      IO.canceled.asInstanceOf[IO[KotlinJsCompileResult]]
    } else {
      // Run blocking compilation on a dedicated thread to avoid cats-effect
      // prepareForBlocking deadlock (LinkedTransferQueue.transfer can hang).
      IO.async[KotlinJsCompileResult] { cb =>
        IO.delay {
          val thread = new Thread(() =>
            try {
              val result = compileBlocking(sources, libraries, outputDir, config, diagnosticListener, cancellation)
              if (cancellation.isCancelled) cb(Left(new CompilationCancelledException("Compilation cancelled")))
              else cb(Right(result))
            } catch {
              case _: InterruptedException =>
                cb(Left(new CompilationCancelledException("Compilation interrupted")))
              case e: CompilationCancelledException =>
                cb(Left(e))
              case e: Throwable =>
                cb(Left(e))
            }
          )
          thread.setDaemon(true)
          thread.setName("kotlin-js-compiler")
          thread.start()
          Some(IO.delay {
            cancellation.cancel()
            thread.interrupt()
          })
        }
      }.flatMap { result =>
        if (cancellation.isCancelled) IO.canceled.asInstanceOf[IO[KotlinJsCompileResult]]
        else IO.pure(result)
      }.handleErrorWith {
        case _: InterruptedException =>
          IO.canceled.asInstanceOf[IO[KotlinJsCompileResult]]
        case _: CompilationCancelledException =>
          IO.canceled.asInstanceOf[IO[KotlinJsCompileResult]]
        case e =>
          IO.raiseError(e)
      }
    }

  private def compileBlocking(
      sources: Seq[Path],
      libraries: Seq[Path],
      outputDir: Path,
      config: KotlinJsCompilerConfig,
      diagnosticListener: DiagnosticListener,
      cancellation: CancellationToken
  ): KotlinJsCompileResult = {
    val instance = CompilerResolver.getKotlinJsCompiler(config.kotlinVersion)
    val loader = instance.loader

    Files.createDirectories(outputDir)

    // Compile directly - the K2 compiler can produce JS from sources in one step
    compileDirect(sources, libraries, outputDir, config, diagnosticListener, cancellation, loader)
  }

  /** Compile sources directly to JS or KLIB. */
  private def compileDirect(
      sources: Seq[Path],
      libraries: Seq[Path],
      outputDir: Path,
      config: KotlinJsCompilerConfig,
      diagnosticListener: DiagnosticListener,
      cancellation: CancellationToken,
      loader: ClassLoader
  ): KotlinJsCompileResult = {
    val capturedOutput = new java.io.ByteArrayOutputStream()
    val capturedStream = new java.io.PrintStream(capturedOutput, true)

    try {
      checkCancellation(cancellation)

      val compilerClass = loader.loadClass("org.jetbrains.kotlin.cli.js.K2JSCompiler")
      val argsClass = loader.loadClass("org.jetbrains.kotlin.cli.common.arguments.K2JSCompilerArguments")
      val exitCodeClass = loader.loadClass("org.jetbrains.kotlin.cli.common.ExitCode")
      val messageCollectorClass = loader.loadClass("org.jetbrains.kotlin.cli.common.messages.MessageCollector")

      val compiler = compilerClass.getDeclaredConstructor().newInstance()
      val args = argsClass.getDeclaredConstructor().newInstance()

      // Set source files in freeArgs
      val freeArgs = new java.util.ArrayList[String]()
      sources.foreach(s => freeArgs.add(s.toAbsolutePath.toString))
      config.additionalOptions.foreach(freeArgs.add)
      setField(args, "freeArgs", freeArgs)

      // Set output directory and module name as fields
      setField(args, "outputDir", outputDir.toAbsolutePath.toString)
      setField(args, "moduleName", config.moduleName)

      // Always produce KLIB - this is the primary output for Kotlin/JS compilation
      // KLIB is needed for:
      // 1. Dependent projects to compile against
      // 2. Linking to JS at test/run time
      //
      // Note: Do NOT set irProduceJs=true here. With source files as input and
      // irProduceJs=true, the K2 compiler enters "link mode" and expects KLIB input,
      // causing "jsFrontEndResult has not been initialized" errors.
      // JS generation happens separately at link time using the KLIB.
      setField(args, "irProduceKlibFile", true)

      // Set module kind
      setField(args, "moduleKind", config.moduleKind.name)

      // Source map options (always set for JS mode, ignored for KLIB-only)
      setField(args, "sourceMap", config.sourceMap)
      config.sourceMapPrefix.foreach(prefix => setField(args, "sourceMapPrefix", prefix))
      setField(args, "sourceMapEmbedSources", config.sourceMapEmbedSources.name)

      // DCE options for production mode
      setField(args, "irDce", !config.developmentMode)
      setField(args, "irMinimizedMemberNames", !config.developmentMode)

      // TypeScript declarations
      setField(args, "generateDts", config.generateDts)

      // Set libraries
      if (libraries.nonEmpty) {
        val librariesStr = libraries.map(_.toAbsolutePath.toString).mkString(java.io.File.pathSeparator)
        setField(args, "libraries", librariesStr)
      }

      val messageCollector = createMessageCollector(diagnosticListener, loader, messageCollectorClass)
      val services = createServicesWithCancellation(loader, cancellation)

      checkCancellation(cancellation)

      val execMethod = compilerClass.getMethods
        .find { m =>
          m.getName == "exec" &&
          m.getParameterCount == 3 &&
          m.getParameterTypes()(0).getName == "org.jetbrains.kotlin.cli.common.messages.MessageCollector" &&
          m.getParameterTypes()(1).getName == "org.jetbrains.kotlin.config.Services"
        }
        .getOrElse(throw new NoSuchMethodException(s"Could not find K2JSCompiler.exec method"))

      val savedOut = System.out
      val savedErr = System.err
      val result =
        try {
          System.setOut(capturedStream)
          System.setErr(capturedStream)
          execMethod.invoke(compiler, messageCollector, services, args)
        } finally {
          System.setOut(savedOut)
          System.setErr(savedErr)
        }

      checkCancellation(cancellation)

      val okField = exitCodeClass.getField("OK")
      val okValue = okField.get(null)
      val exitCode = if (result == okValue) 0 else 1

      if (exitCode != 0) {
        val output = capturedOutput.toString("UTF-8").trim
        if (output.nonEmpty) {
          diagnosticListener.onDiagnostic(
            CompilerError(
              path = None,
              line = 0,
              column = 0,
              message = s"Kotlin/JS compilation output:\n$output",
              rendered = None,
              severity = CompilerError.Severity.Error
            )
          )
        }
        return KotlinJsCompileResult(outputDir, None, None, exitCode)
      }

      // Find output files
      val jsFile = outputDir.resolve(s"${config.moduleName}.js")
      val outputFile = if (Files.exists(jsFile)) Some(jsFile) else None

      val klibFile = outputDir.resolve(s"${config.moduleName}.klib")
      val klibOpt = if (Files.exists(klibFile)) Some(klibFile) else None

      KotlinJsCompileResult(outputDir, outputFile, klibOpt, exitCode)
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
        val output = capturedOutput.toString("UTF-8").trim
        val causeMsg = if (cause != null) {
          val sw = new java.io.StringWriter()
          cause.printStackTrace(new java.io.PrintWriter(sw))
          s"${cause.getClass.getName}: ${cause.getMessage}\n${sw.toString}"
        } else e.getMessage
        val fullMsg = if (output.nonEmpty) s"$causeMsg\nCompiler output:\n$output" else causeMsg
        diagnosticListener.onDiagnostic(
          CompilerError(path = None, line = 0, column = 0, message = s"Kotlin/JS compilation failed: $fullMsg", rendered = None, severity = CompilerError.Severity.Error)
        )
        KotlinJsCompileResult(outputDir, None, None, 1)
      case e: Exception =>
        val output = capturedOutput.toString("UTF-8").trim
        val sw = new java.io.StringWriter()
        e.printStackTrace(new java.io.PrintWriter(sw))
        val fullMsg =
          if (output.nonEmpty) s"${e.getClass.getName}: ${e.getMessage}\n${sw.toString}\nCompiler output:\n$output"
          else s"${e.getClass.getName}: ${e.getMessage}\n${sw.toString}"
        diagnosticListener.onDiagnostic(
          CompilerError(path = None, line = 0, column = 0, message = s"Kotlin/JS compilation failed: $fullMsg", rendered = None, severity = CompilerError.Severity.Error)
        )
        KotlinJsCompileResult(outputDir, None, None, 1)
    }
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

  /** Create Services instance with CompilationCanceledStatus. */
  private def createServicesWithCancellation(loader: ClassLoader, cancellation: CancellationToken): Any =
    try {
      val servicesClass = loader.loadClass("org.jetbrains.kotlin.config.Services")
      val servicesBuilderClass = loader.loadClass("org.jetbrains.kotlin.config.Services$Builder")
      val canceledStatusClass = loader.loadClass("org.jetbrains.kotlin.progress.CompilationCanceledStatus")

      // Create a proxy for CompilationCanceledStatus
      val canceledStatusProxy = java.lang.reflect.Proxy.newProxyInstance(
        loader,
        Array(canceledStatusClass),
        new java.lang.reflect.InvocationHandler {
          override def invoke(proxy: Any, method: java.lang.reflect.Method, args: Array[AnyRef]): AnyRef =
            method.getName match {
              case "checkCanceled" =>
                // Also check thread interrupt
                if (Thread.interrupted()) {
                  val exClass = loader.loadClass("org.jetbrains.kotlin.progress.CompilationCanceledException")
                  throw exClass.getDeclaredConstructor().newInstance().asInstanceOf[Throwable]
                }
                if (cancellation.isCancelled) {
                  val exClass = loader.loadClass("org.jetbrains.kotlin.progress.CompilationCanceledException")
                  throw exClass.getDeclaredConstructor().newInstance().asInstanceOf[Throwable]
                }
                null // void return
              case _ =>
                null
            }
        }
      )

      // Build Services with our cancellation status
      val builderConstructor = servicesBuilderClass.getDeclaredConstructor()
      val builder = builderConstructor.newInstance()
      val registerMethod = servicesBuilderClass.getMethod("register", classOf[Class[?]], classOf[Any])
      registerMethod.invoke(builder, canceledStatusClass, canceledStatusProxy)
      val buildMethod = servicesBuilderClass.getMethod("build")
      buildMethod.invoke(builder)
    } catch {
      case _: ClassNotFoundException =>
        // Fallback to empty services if CompilationCanceledStatus not available
        createEmptyServices(loader)
      case _: NoSuchMethodException =>
        createEmptyServices(loader)
      case e: Exception =>
        createEmptyServices(loader)
    }

  private def createEmptyServices(loader: ClassLoader): Any = {
    val servicesClass = loader.loadClass("org.jetbrains.kotlin.config.Services")
    val servicesCompanion = loader.loadClass("org.jetbrains.kotlin.config.Services$Companion")
    val companionField = servicesClass.getField("Companion")
    val companion = companionField.get(null)
    val emptyMethod = companion.getClass.getMethod("getEMPTY")
    emptyMethod.invoke(companion)
  }

  private def setField(obj: Any, fieldName: String, value: Any): Unit =
    try {
      val field = obj.getClass.getDeclaredField(fieldName)
      field.setAccessible(true)
      field.set(obj, value)
    } catch {
      case _: NoSuchFieldException =>
        // Try setter method
        val setterName = s"set${fieldName.capitalize}"
        val methods = obj.getClass.getMethods.filter(_.getName == setterName)
        methods.headOption.foreach { method =>
          try
            method.invoke(obj, value.asInstanceOf[AnyRef])
          catch {
            case _: Exception => // Ignore if setter fails
          }
        }
    }

  private def getField[T](obj: Any, fieldName: String): T = {
    val field = obj.getClass.getDeclaredField(fieldName)
    field.setAccessible(true)
    field.get(obj).asInstanceOf[T]
  }

  private def createMessageCollector(
      listener: DiagnosticListener,
      loader: ClassLoader,
      messageCollectorClass: Class[?]
  ): Any = {
    // Track if errors were reported
    val hasErrorsRef = new java.util.concurrent.atomic.AtomicBoolean(false)

    // Create a message collector that reports to our diagnostic listener
    java.lang.reflect.Proxy.newProxyInstance(
      loader,
      Array(messageCollectorClass),
      new java.lang.reflect.InvocationHandler {
        override def invoke(proxy: Any, method: java.lang.reflect.Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "hasErrors" =>
              java.lang.Boolean.valueOf(hasErrorsRef.get())
            case "clear" =>
              hasErrorsRef.set(false)
              null
            case "report" if args != null && args.length >= 2 =>
              // report(CompilerMessageSeverity severity, String message, CompilerMessageSourceLocation location)
              val severity = args(0)
              val message = args(1).asInstanceOf[String]
              val location = if (args.length > 2 && args(2) != null) args(2) else null

              // Extract location if available
              val (path, line, column) = if (location != null) {
                try {
                  val pathMethod = location.getClass.getMethod("getPath")
                  val lineMethod = location.getClass.getMethod("getLine")
                  val columnMethod = location.getClass.getMethod("getColumn")
                  val p = pathMethod.invoke(location).asInstanceOf[String]
                  val l = lineMethod.invoke(location).asInstanceOf[Int]
                  val c = columnMethod.invoke(location).asInstanceOf[Int]
                  (Option(p).map(java.nio.file.Paths.get(_)), l, c)
                } catch {
                  case _: Exception => (None, 0, 0)
                }
              } else (None, 0, 0)

              // Check severity
              val severityName = severity.toString
              val isError = severityName.contains("ERROR") || severityName.contains("EXCEPTION")
              val isWarning = severityName.contains("WARNING")

              if (isError) {
                hasErrorsRef.set(true)
                listener.onDiagnostic(CompilerError(path, line, column, message, None, CompilerError.Severity.Error))
              } else if (isWarning) {
                listener.onDiagnostic(CompilerError(path, line, column, message, None, CompilerError.Severity.Warning))
              }
              // Ignore INFO, LOGGING, OUTPUT etc.
              null
            case _ =>
              null
          }
      }
    )
  }
}

/** Result of Kotlin/JS linking. */
case class KotlinJsLinkResult(
    outputDir: Path,
    jsFile: Option[Path],
    exitCode: Int
) {
  def isSuccess: Boolean = exitCode == 0 && jsFile.isDefined
}

object KotlinJsLinker {

  /** Link Kotlin/JS KLIBs to JavaScript.
    *
    * This is the second phase of Kotlin/JS compilation:
    *   1. Compile: sources → KLIB (KotlinJsCompiler.compile)
    *   2. Link: KLIBs → JS (this method)
    *
    * @param klibs
    *   the KLIB files to link (project KLIB + dependency KLIBs)
    * @param outputDir
    *   the output directory for JS files
    * @param config
    *   the compiler configuration
    * @param diagnosticListener
    *   listener for linking diagnostics
    * @param cancellation
    *   token for cancelling linking
    * @return
    *   the link result, or IO.canceled if cancelled
    */
  def link(
      klibs: Seq[Path],
      outputDir: Path,
      config: KotlinJsCompilerConfig,
      diagnosticListener: DiagnosticListener,
      cancellation: CancellationToken
  ): IO[KotlinJsLinkResult] =
    if (cancellation.isCancelled) {
      IO.canceled.asInstanceOf[IO[KotlinJsLinkResult]]
    } else {
      IO.async[KotlinJsLinkResult] { cb =>
        IO.delay {
          val thread = new Thread(() =>
            try {
              val result = linkBlocking(klibs, outputDir, config, diagnosticListener, cancellation)
              if (cancellation.isCancelled) cb(Left(new CompilationCancelledException("Linking cancelled")))
              else cb(Right(result))
            } catch {
              case _: InterruptedException =>
                cb(Left(new CompilationCancelledException("Linking interrupted")))
              case e: CompilationCancelledException =>
                cb(Left(e))
              case e: Throwable =>
                cb(Left(e))
            }
          )
          thread.setDaemon(true)
          thread.setName("kotlin-js-linker")
          thread.start()
          Some(IO.delay {
            cancellation.cancel()
            thread.interrupt()
          })
        }
      }.flatMap { result =>
        if (cancellation.isCancelled) IO.canceled.asInstanceOf[IO[KotlinJsLinkResult]]
        else IO.pure(result)
      }.handleErrorWith {
        case _: InterruptedException =>
          IO.canceled.asInstanceOf[IO[KotlinJsLinkResult]]
        case _: CompilationCancelledException =>
          IO.canceled.asInstanceOf[IO[KotlinJsLinkResult]]
        case e =>
          IO.raiseError(e)
      }
    }

  private def linkBlocking(
      klibs: Seq[Path],
      outputDir: Path,
      config: KotlinJsCompilerConfig,
      diagnosticListener: DiagnosticListener,
      cancellation: CancellationToken
  ): KotlinJsLinkResult = {
    val instance = CompilerResolver.getKotlinJsCompiler(config.kotlinVersion)
    val loader = instance.loader

    Files.createDirectories(outputDir)

    val capturedOutput = new java.io.ByteArrayOutputStream()
    val capturedStream = new java.io.PrintStream(capturedOutput, true)

    try {
      checkCancellation(cancellation)

      val compilerClass = loader.loadClass("org.jetbrains.kotlin.cli.js.K2JSCompiler")
      val argsClass = loader.loadClass("org.jetbrains.kotlin.cli.common.arguments.K2JSCompilerArguments")
      val exitCodeClass = loader.loadClass("org.jetbrains.kotlin.cli.common.ExitCode")
      val messageCollectorClass = loader.loadClass("org.jetbrains.kotlin.cli.common.messages.MessageCollector")

      val compiler = compilerClass.getDeclaredConstructor().newInstance()
      val args = argsClass.getDeclaredConstructor().newInstance()

      // For linking, we pass KLIBs via includes field (the main KLIB to link) and libraries (dependencies)
      // The first KLIB is the main one to link, others are dependencies
      val (mainKlib, depKlibs) = klibs.partition { p =>
        // The main KLIB is typically the one matching the module name
        p.getFileName.toString.contains(config.moduleName.replace("_", "-")) ||
        p.getFileName.toString.contains(config.moduleName)
      }

      // Set main KLIB via includes field (not freeArgs which is interpreted as sources)
      // Note: includes is a single String path, not an array
      if (mainKlib.nonEmpty) {
        val includesPath = mainKlib.head.toAbsolutePath.toString
        setField(args, "includes", includesPath)
      }

      // Set any additional options in freeArgs (but NOT includes)
      val freeArgs = new java.util.ArrayList[String]()
      config.additionalOptions.foreach(freeArgs.add)
      setField(args, "freeArgs", freeArgs)

      // Set output directory and module name
      setField(args, "outputDir", outputDir.toAbsolutePath.toString)
      setField(args, "moduleName", config.moduleName)

      // For linking: produce JS, not KLIB
      setField(args, "irProduceJs", true)
      setField(args, "irProduceKlibFile", false)

      // Module kind
      setField(args, "moduleKind", config.moduleKind.name)

      // Source maps
      setField(args, "sourceMap", config.sourceMap)
      config.sourceMapPrefix.foreach(prefix => setField(args, "sourceMapPrefix", prefix))
      setField(args, "sourceMapEmbedSources", config.sourceMapEmbedSources.name)

      // DCE for production mode
      setField(args, "irDce", !config.developmentMode)
      setField(args, "irMinimizedMemberNames", !config.developmentMode)

      // TypeScript declarations
      setField(args, "generateDts", config.generateDts)

      // Set dependency libraries (other KLIBs + kotlin-stdlib-js + kotlin-test-js)
      // The linker needs kotlin-stdlib-js to find built-in classes like kotlin.Any
      // Also include kotlin-test-js for test projects (it won't hurt non-test projects)
      val versionKotlin = VersionKotlin(config.kotlinVersion)
      val stdlibKlibs = CompilerResolver.resolveKotlinJsLibrary(versionKotlin)
      val testKlibs = CompilerResolver.resolveKotlinTestJs(versionKotlin)
      val allLibraries = (depKlibs ++ stdlibKlibs ++ testKlibs).distinct
      if (allLibraries.nonEmpty) {
        val librariesStr = allLibraries.map(_.toAbsolutePath.toString).mkString(java.io.File.pathSeparator)
        setField(args, "libraries", librariesStr)
      }

      val messageCollector = createMessageCollector(diagnosticListener, loader, messageCollectorClass)
      val services = createServicesWithCancellation(loader, cancellation)

      checkCancellation(cancellation)

      val execMethod = compilerClass.getMethods
        .find { m =>
          m.getName == "exec" &&
          m.getParameterCount == 3 &&
          m.getParameterTypes()(0).getName == "org.jetbrains.kotlin.cli.common.messages.MessageCollector" &&
          m.getParameterTypes()(1).getName == "org.jetbrains.kotlin.config.Services"
        }
        .getOrElse(throw new NoSuchMethodException(s"Could not find K2JSCompiler.exec method"))

      val savedOut = System.out
      val savedErr = System.err
      val result =
        try {
          System.setOut(capturedStream)
          System.setErr(capturedStream)
          execMethod.invoke(compiler, messageCollector, services, args)
        } finally {
          System.setOut(savedOut)
          System.setErr(savedErr)
        }

      checkCancellation(cancellation)

      val okField = exitCodeClass.getField("OK")
      val okValue = okField.get(null)
      val exitCode = if (result == okValue) 0 else 1

      if (exitCode != 0) {
        val output = capturedOutput.toString("UTF-8").trim
        if (output.nonEmpty) {
          diagnosticListener.onDiagnostic(
            CompilerError(
              path = None,
              line = 0,
              column = 0,
              message = s"Kotlin/JS linking output:\n$output",
              rendered = None,
              severity = CompilerError.Severity.Error
            )
          )
        }
        return KotlinJsLinkResult(outputDir, None, exitCode)
      }

      // Find the JS output file
      val jsFile = outputDir.resolve(s"${config.moduleName}.js")
      val jsFileOpt = if (Files.exists(jsFile)) Some(jsFile) else None

      KotlinJsLinkResult(outputDir, jsFileOpt, exitCode)
    } catch {
      case _: InterruptedException =>
        Thread.currentThread().interrupt()
        throw new InterruptedException("Linking interrupted")
      case e: CompilationCancelledException =>
        throw e
      case e: java.lang.reflect.InvocationTargetException =>
        val cause = e.getCause
        if (
          cause != null && (cause.getClass.getName.contains("CompilationCanceled") ||
            cause.isInstanceOf[InterruptedException])
        ) {
          throw new CompilationCancelledException("Linking cancelled")
        }
        val output = capturedOutput.toString("UTF-8").trim
        val causeMsg = if (cause != null) {
          val sw = new java.io.StringWriter()
          cause.printStackTrace(new java.io.PrintWriter(sw))
          s"${cause.getClass.getName}: ${cause.getMessage}\n${sw.toString}"
        } else e.getMessage
        val fullMsg = if (output.nonEmpty) s"$causeMsg\nLinker output:\n$output" else causeMsg
        diagnosticListener.onDiagnostic(
          CompilerError(path = None, line = 0, column = 0, message = s"Kotlin/JS linking failed: $fullMsg", rendered = None, severity = CompilerError.Severity.Error)
        )
        KotlinJsLinkResult(outputDir, None, 1)
      case e: Exception =>
        val output = capturedOutput.toString("UTF-8").trim
        val sw = new java.io.StringWriter()
        e.printStackTrace(new java.io.PrintWriter(sw))
        val fullMsg =
          if (output.nonEmpty) s"${e.getClass.getName}: ${e.getMessage}\n${sw.toString}\nLinker output:\n$output"
          else s"${e.getClass.getName}: ${e.getMessage}\n${sw.toString}"
        diagnosticListener.onDiagnostic(
          CompilerError(path = None, line = 0, column = 0, message = s"Kotlin/JS linking failed: $fullMsg", rendered = None, severity = CompilerError.Severity.Error)
        )
        KotlinJsLinkResult(outputDir, None, 1)
    }
  }

  private def checkCancellation(cancellation: CancellationToken): Unit = {
    if (Thread.interrupted()) {
      throw new InterruptedException("Linking interrupted")
    }
    if (cancellation.isCancelled) {
      throw new CompilationCancelledException("Linking cancelled")
    }
  }

  private def setField(obj: Any, fieldName: String, value: Any): Unit =
    try {
      val field = obj.getClass.getDeclaredField(fieldName)
      field.setAccessible(true)
      field.set(obj, value)
    } catch {
      case _: NoSuchFieldException =>
        val setterName = s"set${fieldName.capitalize}"
        val methods = obj.getClass.getMethods.filter(_.getName == setterName)
        methods.headOption.foreach { method =>
          try
            method.invoke(obj, value.asInstanceOf[AnyRef])
          catch {
            case _: Exception => ()
          }
        }
    }

  private def createServicesWithCancellation(loader: ClassLoader, cancellation: CancellationToken): Any =
    try {
      val servicesClass = loader.loadClass("org.jetbrains.kotlin.config.Services")
      val servicesBuilderClass = loader.loadClass("org.jetbrains.kotlin.config.Services$Builder")
      val canceledStatusClass = loader.loadClass("org.jetbrains.kotlin.progress.CompilationCanceledStatus")

      val canceledStatusProxy = java.lang.reflect.Proxy.newProxyInstance(
        loader,
        Array(canceledStatusClass),
        new java.lang.reflect.InvocationHandler {
          override def invoke(proxy: Any, method: java.lang.reflect.Method, args: Array[AnyRef]): AnyRef =
            method.getName match {
              case "checkCanceled" =>
                if (Thread.interrupted()) {
                  val exClass = loader.loadClass("org.jetbrains.kotlin.progress.CompilationCanceledException")
                  throw exClass.getDeclaredConstructor().newInstance().asInstanceOf[Throwable]
                }
                if (cancellation.isCancelled) {
                  val exClass = loader.loadClass("org.jetbrains.kotlin.progress.CompilationCanceledException")
                  throw exClass.getDeclaredConstructor().newInstance().asInstanceOf[Throwable]
                }
                null
              case _ =>
                null
            }
        }
      )

      val builderConstructor = servicesBuilderClass.getDeclaredConstructor()
      val builder = builderConstructor.newInstance()
      val registerMethod = servicesBuilderClass.getMethod("register", classOf[Class[?]], classOf[Any])
      registerMethod.invoke(builder, canceledStatusClass, canceledStatusProxy)
      val buildMethod = servicesBuilderClass.getMethod("build")
      buildMethod.invoke(builder)
    } catch {
      case _: ClassNotFoundException =>
        createEmptyServices(loader)
      case _: NoSuchMethodException =>
        createEmptyServices(loader)
      case _: Exception =>
        createEmptyServices(loader)
    }

  private def createEmptyServices(loader: ClassLoader): Any = {
    val servicesClass = loader.loadClass("org.jetbrains.kotlin.config.Services")
    val servicesCompanion = loader.loadClass("org.jetbrains.kotlin.config.Services$Companion")
    val companionField = servicesClass.getField("Companion")
    val companion = companionField.get(null)
    val emptyMethod = companion.getClass.getMethod("getEMPTY")
    emptyMethod.invoke(companion)
  }

  private def createMessageCollector(
      listener: DiagnosticListener,
      loader: ClassLoader,
      messageCollectorClass: Class[?]
  ): Any = {
    val hasErrorsRef = new java.util.concurrent.atomic.AtomicBoolean(false)

    java.lang.reflect.Proxy.newProxyInstance(
      loader,
      Array(messageCollectorClass),
      new java.lang.reflect.InvocationHandler {
        override def invoke(proxy: Any, method: java.lang.reflect.Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "hasErrors" =>
              java.lang.Boolean.valueOf(hasErrorsRef.get())
            case "clear" =>
              hasErrorsRef.set(false)
              null
            case "report" if args != null && args.length >= 2 =>
              val severity = args(0)
              val message = args(1).asInstanceOf[String]
              val location = if (args.length > 2 && args(2) != null) args(2) else null

              val (path, line, column) = if (location != null) {
                try {
                  val pathMethod = location.getClass.getMethod("getPath")
                  val lineMethod = location.getClass.getMethod("getLine")
                  val columnMethod = location.getClass.getMethod("getColumn")
                  val p = pathMethod.invoke(location).asInstanceOf[String]
                  val l = lineMethod.invoke(location).asInstanceOf[Int]
                  val c = columnMethod.invoke(location).asInstanceOf[Int]
                  (Option(p).map(java.nio.file.Paths.get(_)), l, c)
                } catch {
                  case _: Exception => (None, 0, 0)
                }
              } else (None, 0, 0)

              val severityName = severity.toString
              val isError = severityName.contains("ERROR") || severityName.contains("EXCEPTION")
              val isWarning = severityName.contains("WARNING")

              if (isError) {
                hasErrorsRef.set(true)
                listener.onDiagnostic(CompilerError(path, line, column, message, None, CompilerError.Severity.Error))
              } else if (isWarning) {
                listener.onDiagnostic(CompilerError(path, line, column, message, None, CompilerError.Severity.Warning))
              }
              null
            case _ =>
              null
          }
      }
    )
  }
}

/** Exception thrown when compilation is cancelled. */
class CompilationCancelledException(message: String) extends Exception(message)
