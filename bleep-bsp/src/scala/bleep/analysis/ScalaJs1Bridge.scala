package bleep.analysis

import cats.effect.IO
import java.nio.file.{Files, Path}
import java.lang.reflect.{InvocationTargetException, Method}
import scala.jdk.CollectionConverters.*

/** Scala.js 1.x linker bridge.
  *
  * Uses reflection to invoke the Scala.js linker APIs, allowing different Scala.js versions to be loaded in isolated classloaders.
  */
class ScalaJs1Bridge(scalaJsVersion: String, scalaVersion: String) extends ScalaJsToolchain {
  import ScalaJs1Bridge.*

  override def link(
      config: ScalaJsLinkConfig,
      classpath: Seq[Path],
      mainClass: Option[String],
      outputDir: Path,
      moduleName: String,
      logger: ScalaJsToolchain.Logger,
      cancellation: CancellationToken,
      isTest: Boolean = false
  ): IO[ScalaJsLinkResult] =
    // Check cancellation before starting
    if (cancellation.isCancelled) {
      IO.canceled.asInstanceOf[IO[ScalaJsLinkResult]]
    } else {
      // Run blocking link on a dedicated thread to avoid cats-effect prepareForBlocking deadlock.
      // IO.interruptible/IO.blocking use LinkedTransferQueue.transfer internally which can
      // deadlock the work-stealing pool. Using IO.async with a plain thread avoids this.
      IO.async[ScalaJsLinkResult] { cb =>
        IO.delay {
          val thread = new Thread(() =>
            try {
              val result = linkBlocking(config, classpath, mainClass, outputDir, moduleName, logger, cancellation, isTest)
              if (cancellation.isCancelled) cb(Left(new LinkingCancelledException("Linking cancelled")))
              else cb(Right(result))
            } catch {
              case _: InterruptedException =>
                cb(Left(new LinkingCancelledException("Linking interrupted")))
              case e: LinkingCancelledException =>
                cb(Left(e))
              case e: Throwable =>
                cb(Left(e))
            }
          )
          thread.setDaemon(true)
          thread.setName("scalajs-linker")
          thread.start()
          // Return finalizer that interrupts the thread on cancel
          Some(IO.delay {
            cancellation.cancel()
            thread.interrupt()
          })
        }
      }.flatMap { result =>
        if (cancellation.isCancelled) IO.canceled.asInstanceOf[IO[ScalaJsLinkResult]]
        else IO.pure(result)
      }.handleErrorWith {
        case _: InterruptedException =>
          IO.canceled.asInstanceOf[IO[ScalaJsLinkResult]]
        case _: LinkingCancelledException =>
          IO.canceled.asInstanceOf[IO[ScalaJsLinkResult]]
        case e =>
          IO.raiseError(new RuntimeException(s"Scala.js linking failed: ${e.getMessage}", e))
      }
    }

  private def linkBlocking(
      config: ScalaJsLinkConfig,
      classpath: Seq[Path],
      mainClass: Option[String],
      outputDir: Path,
      moduleName: String,
      logger: ScalaJsToolchain.Logger,
      cancellation: CancellationToken,
      isTest: Boolean
  ): ScalaJsLinkResult = {
    val instance = CompilerResolver.getScalaJsLinker(scalaJsVersion, scalaVersion)
    val loader = instance.loader

    // Create output directory
    Files.createDirectories(outputDir)

    // Check cancellation
    checkCancellation(cancellation)

    // Get linker classes
    val standardConfigClass = loader.loadClass("org.scalajs.linker.interface.StandardConfig")
    val standardConfigCompanion = loader.loadClass("org.scalajs.linker.interface.StandardConfig$")
    val linkerClass = loader.loadClass("org.scalajs.linker.StandardImpl$")
    val moduleInitializerClass = loader.loadClass("org.scalajs.linker.interface.ModuleInitializer")
    val moduleInitializerCompanion = loader.loadClass("org.scalajs.linker.interface.ModuleInitializer$")
    val pathIRContainerClass = loader.loadClass("org.scalajs.linker.PathIRContainer$")
    val pathOutputDirectoryClass = loader.loadClass("org.scalajs.linker.PathOutputDirectory$")
    val loggerClass = loader.loadClass("org.scalajs.logging.Logger")
    val levelClass = loader.loadClass("org.scalajs.logging.Level")

    // Get companion objects
    val standardConfigObj = standardConfigCompanion.getField("MODULE$").get(null)
    val linkerObj = linkerClass.getField("MODULE$").get(null)
    val moduleInitializerObj = moduleInitializerCompanion.getField("MODULE$").get(null)
    val pathIRContainerObj = pathIRContainerClass.getField("MODULE$").get(null)
    val pathOutputDirectoryObj = pathOutputDirectoryClass.getField("MODULE$").get(null)

    // Create linker config
    val linkerConfig = createLinkerConfig(
      standardConfigObj,
      standardConfigClass,
      config,
      loader
    )

    // Create the linker
    val linkerMethod = linkerClass.getMethod("linker", standardConfigClass)
    val linker = linkerMethod.invoke(linkerObj, linkerConfig)

    // Create logger adapter
    val scalaJsLogger = createLoggerAdapter(logger, loader, loggerClass, levelClass)

    // Check cancellation
    checkCancellation(cancellation)

    // Get IR files from classpath (use linkerObj to get IRFileCache via StandardImpl.irFileCache())
    logger.info(s"[ScalaJs1Bridge] Collecting IR files from ${classpath.size} classpath entries")
    val irFilesSeq = collectIRFiles(classpath, pathIRContainerObj, linkerObj, loader)
    logger.info(s"[ScalaJs1Bridge] Collected IR files: ${irFilesSeq.getClass.getName}")

    // Check cancellation
    checkCancellation(cancellation)

    // Create module initializers
    logger.info(s"[ScalaJs1Bridge] Creating module initializers for mainClass=$mainClass, isTest=$isTest")
    val moduleInitializers = createModuleInitializers(mainClass, moduleInitializerObj, loader, isTest)

    // Create output directory handler
    val outputDirectory = createOutputDirectory(outputDir, pathOutputDirectoryObj, loader)
    logger.info(s"[ScalaJs1Bridge] Output directory: $outputDir")

    // Check cancellation before linking
    checkCancellation(cancellation)

    // Link
    logger.info(s"[ScalaJs1Bridge] Starting linker...")
    val linkedFiles = runLinker(linker, irFilesSeq, moduleInitializers, outputDirectory, scalaJsLogger, loader, cancellation)
    logger.info(s"[ScalaJs1Bridge] Linker completed")

    // Check cancellation after linking
    checkCancellation(cancellation)

    // Find output files
    val outputFiles = Files
      .list(outputDir)
      .iterator()
      .asScala
      .filter { p =>
        val name = p.getFileName.toString
        name.endsWith(".js") || name.endsWith(".js.map")
      }
      .toSeq
    logger.info(s"[ScalaJs1Bridge] Found ${outputFiles.size} output files in $outputDir")

    val mainJs = outputDir.resolve(s"$moduleName.js")
    val actualMainJs = if (Files.exists(mainJs)) mainJs else outputFiles.find(_.toString.endsWith(".js")).getOrElse(mainJs)

    ScalaJsLinkResult(
      outputFiles = outputFiles,
      mainModule = actualMainJs,
      publicModules = outputFiles.filter(_.toString.endsWith(".js")).filterNot(_.toString.endsWith(".map"))
    )
  }

  /** Check cancellation and throw if cancelled. Also checks thread interrupt. */
  private def checkCancellation(cancellation: CancellationToken): Unit = {
    if (Thread.interrupted()) {
      throw new InterruptedException("Linking interrupted")
    }
    if (cancellation.isCancelled) {
      throw new LinkingCancelledException("Linking cancelled")
    }
  }

  private def createLinkerConfig(
      standardConfigObj: Any,
      standardConfigClass: Class[?],
      config: ScalaJsLinkConfig,
      loader: ClassLoader
  ): Any = {
    // Start with default config
    // Get default config
    val defaultMethod = standardConfigObj.getClass.getMethod("apply")
    var linkerConfig = defaultMethod.invoke(standardConfigObj)

    // Apply settings using withXxx methods
    val moduleKindClass = loader.loadClass("org.scalajs.linker.interface.ModuleKind")
    val moduleKindCompanion = loader.loadClass("org.scalajs.linker.interface.ModuleKind$")
    val moduleKindObj = moduleKindCompanion.getField("MODULE$").get(null)

    val moduleKindName = config.moduleKind match {
      case ScalaJsLinkConfig.ModuleKind.NoModule       => "NoModule$"
      case ScalaJsLinkConfig.ModuleKind.CommonJSModule => "CommonJSModule$"
      case ScalaJsLinkConfig.ModuleKind.ESModule       => "ESModule$"
    }
    val moduleKind = {
      val nestedClass = loader.loadClass(s"org.scalajs.linker.interface.ModuleKind$$$moduleKindName")
      nestedClass.getField("MODULE$").get(null)
    }
    linkerConfig = invokeWithMethod(linkerConfig, "withModuleKind", moduleKindClass, moduleKind)
    linkerConfig = invokeWithMethod(linkerConfig, "withSourceMap", classOf[Boolean], config.emitSourceMaps)
    linkerConfig = invokeWithMethod(linkerConfig, "withOptimizer", classOf[Boolean], config.optimizer)
    linkerConfig = invokeWithMethod(linkerConfig, "withCheckIR", classOf[Boolean], config.checkIR)
    linkerConfig = invokeWithMethod(linkerConfig, "withPrettyPrint", classOf[Boolean], config.prettyPrint)

    // Apply release mode semantics if in release mode
    if (config.mode == ScalaJsLinkConfig.LinkerMode.Release) {
      val semanticsClass = loader.loadClass("org.scalajs.linker.interface.Semantics")
      val semanticsCompanion = loader.loadClass("org.scalajs.linker.interface.Semantics$")
      val semanticsObj = semanticsCompanion.getField("MODULE$").get(null)
      val optimizedMethod = semanticsCompanion.getMethod("Defaults").invoke(semanticsObj)
      try
        linkerConfig = invokeWithMethod(linkerConfig, "withSemantics", semanticsClass, optimizedMethod)
      catch {
        case _: NoSuchMethodException => // Older version, skip
      }
    }

    // Apply module split style
    try {
      val splitStyleClass = loader.loadClass("org.scalajs.linker.interface.ModuleSplitStyle")
      val splitStyleCompanion = loader.loadClass("org.scalajs.linker.interface.ModuleSplitStyle$")
      val splitStyleObj = splitStyleCompanion.getField("MODULE$").get(null)

      val splitStyle = config.moduleSplitStyle match {
        case ScalaJsLinkConfig.ModuleSplitStyle.FewestModules =>
          splitStyleCompanion.getMethod("FewestModules").invoke(splitStyleObj)
        case ScalaJsLinkConfig.ModuleSplitStyle.SmallestModules =>
          splitStyleCompanion.getMethod("SmallestModules").invoke(splitStyleObj)
        case ScalaJsLinkConfig.ModuleSplitStyle.SmallModulesFor(packages) =>
          // SmallModulesFor requires a Seq parameter
          val seqPackages = packages.asJava
          val method = splitStyleCompanion.getMethods.find(_.getName == "SmallModulesFor").get
          method.invoke(splitStyleObj, seqPackages)
      }
      linkerConfig = invokeWithMethod(linkerConfig, "withModuleSplitStyle", splitStyleClass, splitStyle)
    } catch {
      case _: ClassNotFoundException => // Older version without split style support
      case _: NoSuchMethodException  => // Older version
    }

    linkerConfig
  }

  private def invokeWithMethod(obj: Any, methodName: String, paramType: Class[?], value: Any): Any = {
    val method = obj.getClass.getMethod(methodName, paramType)
    method.invoke(obj, value.asInstanceOf[AnyRef])
  }

  private def createLoggerAdapter(
      logger: ScalaJsToolchain.Logger,
      loader: ClassLoader,
      loggerClass: Class[?],
      levelClass: Class[?]
  ): Any = {
    // Create ScalaConsoleLogger with Level.Debug (Debug is a nested case object)
    val scalaLogger = loader.loadClass("org.scalajs.logging.ScalaConsoleLogger")
    val debugLevel = loader.loadClass("org.scalajs.logging.Level$Debug$").getField("MODULE$").get(null)
    scalaLogger.getDeclaredConstructor(levelClass).newInstance(debugLevel)
  }

  private def collectIRFiles(
      classpath: Seq[Path],
      pathIRContainerObj: Any,
      linkerObj: Any,
      loader: ClassLoader
  ): Any = {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    import scala.concurrent.duration.*

    // PathIRContainer.fromClasspath
    val fromClasspathMethod = pathIRContainerObj.getClass.getMethods.find(m => m.getName == "fromClasspath" && m.getParameterCount == 2).get

    // Convert classpath to a Scala Seq from the linker's classloader
    // We need to use the linker's Scala library, not ours, due to classloader isolation
    val scalaSeq = {
      // Start with Nil from the linker's classloader
      val nilClass = loader.loadClass("scala.collection.immutable.Nil$")
      var list: Any = nilClass.getField("MODULE$").get(null)
      // Build up the list in reverse using :: (prepend is O(1))
      val consMethod = list.getClass.getMethod("$colon$colon", classOf[Object])
      // Reverse so the final list is in the original order
      classpath.map(_.toAbsolutePath).reverse.foreach { path =>
        list = consMethod.invoke(list, path)
      }
      list
    }

    // Get the IR containers
    val futureClass = loader.loadClass("scala.concurrent.Future")
    val ecClass = loader.loadClass("scala.concurrent.ExecutionContext")
    val globalEC = loader.loadClass("scala.concurrent.ExecutionContext$").getField("MODULE$").get(null)
    val ecGlobal = globalEC.getClass.getMethod("global").invoke(globalEC)

    // Get IRFileCache from StandardImpl.irFileCache(), then call newCache on the instance
    val irFileCacheMethod = linkerObj.getClass.getMethod("irFileCache")
    val irFileCache = irFileCacheMethod.invoke(linkerObj)
    val newCacheMethod = irFileCache.getClass.getMethod("newCache")
    val cache = newCacheMethod.invoke(irFileCache)

    // Collect IR files from classpath
    try {
      val containers = fromClasspathMethod.invoke(pathIRContainerObj, scalaSeq, ecGlobal)
      // The result is a Future[(Seq[IRContainer], Seq[Path])]
      // We need to wait for it
      val awaitClass = loader.loadClass("scala.concurrent.Await$")
      val awaitObj = awaitClass.getField("MODULE$").get(null)
      val resultMethod = awaitClass.getMethods.find(m => m.getName == "result" && m.getParameterCount == 2).getOrElse {
        throw new RuntimeException(
          s"Could not find Await.result with 2 params. Available: ${awaitClass.getMethods.map(m => s"${m.getName}(${m.getParameterCount})").mkString(", ")}"
        )
      }
      val durationClass = loader.loadClass("scala.concurrent.duration.Duration$")
      val durationObj = durationClass.getField("MODULE$").get(null)
      val infMethod = durationClass.getMethod("Inf")
      val infDuration = infMethod.invoke(durationObj)

      val result = resultMethod.invoke(awaitObj, containers, infDuration)

      // Result is a tuple (Seq[IRContainer], Seq[Path])
      // We need the first element
      val tuple2Class = loader.loadClass("scala.Tuple2")
      val _1Method = tuple2Class.getMethod("_1")
      val irContainers = _1Method.invoke(result)

      // Now cache the IR files - cached takes (containers, ec) parameters
      val cachedMethod = cache.getClass.getMethods.find(m => m.getName == "cached" && m.getParameterCount == 2).getOrElse {
        throw new RuntimeException(
          s"Could not find cached with 2 params. Available: ${cache.getClass.getMethods.map(m => s"${m.getName}(${m.getParameterCount})").mkString(", ")}"
        )
      }
      val cachedFuture = cachedMethod.invoke(cache, irContainers, ecGlobal)
      // Wait for the cache result
      resultMethod.invoke(awaitObj, cachedFuture, infDuration)
    } catch {
      case e: InvocationTargetException =>
        throw e.getCause
      case e: Exception =>
        // Re-throw with context instead of silently returning Nil
        throw new RuntimeException(s"Failed to collect IR files from classpath: ${e.getMessage}", e)
    }
  }

  private def createModuleInitializers(
      mainClass: Option[String],
      moduleInitializerObj: Any,
      loader: ClassLoader,
      isTest: Boolean
  ): Any = {
    val nilClass = loader.loadClass("scala.collection.immutable.Nil$")
    val nil = nilClass.getField("MODULE$").get(null)
    val consMethod = nil.getClass.getMethod("$colon$colon", classOf[Object])

    mainClass match {
      case Some(mc) =>
        // ModuleInitializer.mainMethodWithArgs(mainClass, "main")
        val mainMethodWithArgs = moduleInitializerObj.getClass.getMethods.find(m => m.getName == "mainMethodWithArgs" && m.getParameterCount == 2).get
        val initializer = mainMethodWithArgs.invoke(moduleInitializerObj, mc, "main")
        consMethod.invoke(nil, initializer)

      case None if isTest =>
        // For test projects, use TestAdapterInitializer constants to create the test entry point
        // These are well-known values from org.scalajs.testing.adapter.TestAdapterInitializer
        // The test runner must provide the scalajsCom interface for communication
        val moduleClassName = "org.scalajs.testing.bridge.Bridge"
        val mainMethodName = "start"

        // ModuleInitializer.mainMethod(moduleClassName, mainMethodName)
        val mainMethod = moduleInitializerObj.getClass.getMethods.find(m => m.getName == "mainMethod" && m.getParameterCount == 2).get
        val initializer = mainMethod.invoke(moduleInitializerObj, moduleClassName, mainMethodName)
        consMethod.invoke(nil, initializer)

      case None =>
        // Return Nil - no module initializers (will produce empty output)
        nil
    }
  }

  private def createOutputDirectory(
      outputDir: Path,
      pathOutputDirectoryObj: Any,
      loader: ClassLoader
  ): Any = {
    val applyMethod = pathOutputDirectoryObj.getClass.getMethods.find(m => m.getName == "apply" && m.getParameterCount == 1).get
    applyMethod.invoke(pathOutputDirectoryObj, outputDir)
  }

  private def runLinker(
      linker: Any,
      irFiles: Any,
      moduleInitializers: Any,
      outputDirectory: Any,
      logger: Any,
      loader: ClassLoader,
      cancellation: CancellationToken
  ): Any = {
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    import scala.concurrent.duration.*

    // Get the link method
    val irFileClass = loader.loadClass("org.scalajs.linker.interface.IRFile")
    val moduleInitializerClass = loader.loadClass("org.scalajs.linker.interface.ModuleInitializer")
    val outputDirectoryClass = loader.loadClass("org.scalajs.linker.interface.OutputDirectory")
    val loggerClass = loader.loadClass("org.scalajs.logging.Logger")
    val ecClass = loader.loadClass("scala.concurrent.ExecutionContext")

    val globalEC = loader.loadClass("scala.concurrent.ExecutionContext$").getField("MODULE$").get(null)
    val ecGlobal = globalEC.getClass.getMethod("global").invoke(globalEC)

    val linkMethod = linker.getClass.getMethods.find { m =>
      m.getName == "link" && m.getParameterCount == 5
    }.get

    val future = linkMethod.invoke(linker, irFiles, moduleInitializers, outputDirectory, logger, ecGlobal)

    // Wait for the result
    val awaitClass = loader.loadClass("scala.concurrent.Await$")
    val awaitObj = awaitClass.getField("MODULE$").get(null)
    val resultMethod = awaitClass.getMethods.find(m => m.getName == "result" && m.getParameterCount == 2).get
    val durationClass = loader.loadClass("scala.concurrent.duration.Duration$")
    val durationObj = durationClass.getField("MODULE$").get(null)
    val infMethod = durationClass.getMethod("Inf")
    val infDuration = infMethod.invoke(durationObj)

    try {
      // Check cancellation before waiting
      checkCancellation(cancellation)

      // Unfortunately Scala.js doesn't have a cooperative cancellation mechanism,
      // so we wait for the result. If the thread is interrupted during the wait,
      // the InterruptedException will propagate up.
      resultMethod.invoke(awaitObj, future, infDuration)
    } catch {
      case e: InvocationTargetException =>
        val cause = e.getCause
        if (cause != null && cause.isInstanceOf[InterruptedException]) {
          throw new InterruptedException("Linking interrupted")
        }
        throw cause
      case _: InterruptedException =>
        throw new InterruptedException("Linking interrupted")
    }
  }
}

object ScalaJs1Bridge {
  // Companion object for any shared utilities
}

/** Exception thrown when linking is cancelled. */
class LinkingCancelledException(message: String) extends Exception(message)
