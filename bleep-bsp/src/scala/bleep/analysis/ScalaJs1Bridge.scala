package bleep.analysis

import bleep.bsp.Outcome
import cats.effect.IO
import java.nio.file.{Files, Path}
import java.lang.reflect.InvocationTargetException
import scala.jdk.CollectionConverters.*

/** Scala.js 1.x linker bridge.
  *
  * Uses reflection to invoke the Scala.js linker APIs, allowing different Scala.js versions to be loaded in isolated classloaders.
  */
class ScalaJs1Bridge(scalaJsVersion: String, scalaVersion: String) extends ScalaJsToolchain {

  override def link(
      config: ScalaJsLinkConfig,
      classpath: Seq[Path],
      mainClass: Option[String],
      outputDir: Path,
      moduleName: String,
      logger: ScalaJsToolchain.Logger,
      cancellation: CancellationToken,
      isTest: Boolean = false
  ): IO[Outcome.ThreadOutcome[ScalaJsLinkResult]] =
    Outcome.runInFreshThread[ScalaJsLinkResult](name = "scalajs-linker", contextClassLoader = None, cancellation = cancellation) {
      linkBlocking(config, classpath, mainClass, outputDir, moduleName, logger, cancellation, isTest)
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
    val moduleInitializerCompanion = loader.loadClass("org.scalajs.linker.interface.ModuleInitializer$")
    val pathIRContainerClass = loader.loadClass("org.scalajs.linker.PathIRContainer$")
    val pathOutputDirectoryClass = loader.loadClass("org.scalajs.linker.PathOutputDirectory$")

    // Get companion objects
    val standardConfigObj = standardConfigCompanion.getField("MODULE$").get(null)
    val linkerObj = linkerClass.getField("MODULE$").get(null)
    val moduleInitializerObj = moduleInitializerCompanion.getField("MODULE$").get(null)
    val pathIRContainerObj = pathIRContainerClass.getField("MODULE$").get(null)
    val pathOutputDirectoryObj = pathOutputDirectoryClass.getField("MODULE$").get(null)

    // Create linker config
    val linkerConfig = createLinkerConfig(
      standardConfigObj,
      config,
      loader
    )

    // Create the linker
    val linkerMethod = linkerClass.getMethod("linker", standardConfigClass)
    val linker = linkerMethod.invoke(linkerObj, linkerConfig)

    // Create logger adapter
    val linkerErrors = scala.collection.mutable.ListBuffer.empty[String]
    val scalaJsLogger = createLoggerAdapter(loader, logger, linkerErrors)

    // Check cancellation
    checkCancellation(cancellation)

    // Get IR files from classpath (use linkerObj to get IRFileCache via StandardImpl.irFileCache())
    logger.debug(s"[ScalaJs1Bridge] Collecting IR files from ${classpath.size} classpath entries")
    val irFilesSeq = collectIRFiles(classpath, pathIRContainerObj, linkerObj, loader)
    logger.debug(s"[ScalaJs1Bridge] Collected IR files: ${irFilesSeq.getClass.getName}")

    // Check cancellation
    checkCancellation(cancellation)

    // Create module initializers
    logger.debug(s"[ScalaJs1Bridge] Creating module initializers for mainClass=$mainClass, isTest=$isTest")
    val moduleInitializers = createModuleInitializers(mainClass, config.moduleInitializers, moduleInitializerObj, loader, isTest)

    // Create output directory handler
    val outputDirectory = createOutputDirectory(outputDir, pathOutputDirectoryObj)
    logger.debug(s"[ScalaJs1Bridge] Output directory: $outputDir")

    // Check cancellation before linking
    checkCancellation(cancellation)

    // Link
    logger.debug(s"[ScalaJs1Bridge] Starting linker...")
    try runLinker(linker, irFilesSeq, moduleInitializers, outputDirectory, scalaJsLogger, loader, cancellation)
    catch {
      // The linker reports what is wrong through its logger and then fails with only "There were linking errors"; the reasons belong in the failure
      case e: Exception if linkerErrors.nonEmpty => throw new RuntimeException(s"${e.getMessage}:\n${linkerErrors.mkString("\n")}", e)
    }
    logger.debug(s"[ScalaJs1Bridge] Linker completed")

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
    logger.debug(s"[ScalaJs1Bridge] Found ${outputFiles.size} output files in $outputDir")

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
      config: ScalaJsLinkConfig,
      loader: ClassLoader
  ): Any = {
    // Start with default config
    // Get default config
    val defaultMethod = standardConfigObj.getClass.getMethod("apply")
    var linkerConfig = defaultMethod.invoke(standardConfigObj)

    // Apply settings using withXxx methods
    val moduleKindClass = loader.loadClass("org.scalajs.linker.interface.ModuleKind")

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

    // The release overlay, mirroring mill's `ScalaJSConfigModule.fullOptConfig` line for line: optimized semantics, the Closure compiler where the module kind
    // permits it, and minification from Scala.js 1.16 on. Together these are what makes a full link a full link — without them a release build is a fast link
    // that has merely had dead code eliminated, which is why `bleep link --release` produced output still carrying fastLinkJS's long names and a payload too
    // large for a 10MB budget.
    if (config.mode == ScalaJsLinkConfig.LinkerMode.Release) {
      val semanticsClass = loader.loadClass("org.scalajs.linker.interface.Semantics")
      val semanticsCompanion = loader.loadClass("org.scalajs.linker.interface.Semantics$")
      val semanticsObj = semanticsCompanion.getField("MODULE$").get(null)
      val defaults = semanticsCompanion.getMethod("Defaults").invoke(semanticsObj)
      // `Defaults.optimized`, not `Defaults`. The plain defaults keep every runtime check the optimized ones drop, so passing them made this branch a no-op
      // that looked deliberate.
      val optimized = defaults.getClass.getMethod("optimized").invoke(defaults)
      linkerConfig = invokeWithMethod(linkerConfig, "withSemantics", semanticsClass, optimized)

      // Scala.js rejects the Closure compiler for ESModule output, so the module kind decides rather than the user (mill encodes the same exclusion; see its
      // issue #1392). `IfAvailable` because Closure is an optional artifact on the linker classpath — absent, the linker skips it rather than failing.
      val closureAllowed = config.moduleKind != ScalaJsLinkConfig.ModuleKind.ESModule
      linkerConfig = invokeWithMethod(linkerConfig, "withClosureCompilerIfAvailable", classOf[Boolean], closureAllowed)
    }

    // The project's run-time class names, over whichever semantics the mode chose: keep every name, then each rename in order, as sbt's
    // `RuntimeClassNameMapper.keepAll().andThen(regexReplace(...))...` does.
    if (config.runtimeClassNameRenames.nonEmpty) {
      val mapperClass = loader.loadClass("org.scalajs.linker.interface.Semantics$RuntimeClassNameMapper")
      val mapperCompanion = loader.loadClass("org.scalajs.linker.interface.Semantics$RuntimeClassNameMapper$")
      val mapperObj = mapperCompanion.getField("MODULE$").get(null)
      val regexReplace = mapperCompanion.getMethod("regexReplace", classOf[java.util.regex.Pattern], classOf[String])
      val andThen = mapperClass.getMethod("andThen", mapperClass)
      val mapper = config.runtimeClassNameRenames.foldLeft(mapperCompanion.getMethod("keepAll").invoke(mapperObj)) { case (acc, (regex, replacement)) =>
        andThen.invoke(acc, regexReplace.invoke(mapperObj, java.util.regex.Pattern.compile(regex), replacement))
      }
      val semanticsClass = loader.loadClass("org.scalajs.linker.interface.Semantics")
      val semantics = linkerConfig.getClass.getMethod("semantics").invoke(linkerConfig)
      val withMapper = semanticsClass.getMethod("withRuntimeClassNameMapper", mapperClass).invoke(semantics, mapper)
      linkerConfig = invokeWithMethod(linkerConfig, "withSemantics", semanticsClass, withMapper)
    }

    // Minification renames; it is the half of a release build that shortens the identifiers, and `ScalaJsLinkConfig.Release` has always declared it. The linker
    // only gained `withMinify` in 1.16, so the version decides whether it can be applied — checked rather than probed with an exception, the way mill checks it.
    if (config.minify && scalaJsMinorVersion.exists(_ >= 16))
      linkerConfig = invokeWithMethod(linkerConfig, "withMinify", classOf[Boolean], true)

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

  /** Minor version of the project's Scala.js, when it reads as `1.<minor>.<patch>`. Decides which linker options exist. */
  private lazy val scalaJsMinorVersion: Option[Int] =
    scalaJsVersion.split('.') match {
      case Array(_, minor, _*) => minor.toIntOption
      case _                   => None
    }

  private def invokeWithMethod(obj: Any, methodName: String, paramType: Class[?], value: Any): Any = {
    val method = obj.getClass.getMethod(methodName, paramType)
    method.invoke(obj, value.asInstanceOf[AnyRef])
  }

  /** An `org.scalajs.logging.Logger` that reports to bleep's link logger, collecting error-level messages into `errors`.
    *
    * It used to be a `ScalaConsoleLogger`, printing to the stdout of the process the linker runs in — a detached compile server, whose output nobody reads — so
    * a failed link said only "There were linking errors". Only the interface's two abstract methods are implemented; its default ones (`error`, `time`, ...)
    * run as the interface defines them and end up here.
    */
  private def createLoggerAdapter(loader: ClassLoader, logger: ScalaJsToolchain.Logger, errors: scala.collection.mutable.ListBuffer[String]): Any = {
    val loggerClass = loader.loadClass("org.scalajs.logging.Logger")
    // On the `Function0` interface, not the thunk's own (synthetic, inaccessible) class
    val applyMethod = loader.loadClass("scala.Function0").getMethod("apply")
    def force(thunk: AnyRef): AnyRef = applyMethod.invoke(thunk)
    val handler = new java.lang.reflect.InvocationHandler {
      def invoke(proxy: Any, method: java.lang.reflect.Method, rawArgs: Array[AnyRef]): AnyRef = {
        val args = if (rawArgs == null) Array.empty[AnyRef] else rawArgs
        if (method.isDefault) java.lang.reflect.InvocationHandler.invokeDefault(proxy, method, args*)
        else
          method.getName match {
            case "log" =>
              val message = String.valueOf(force(args(1)))
              // Level is a sealed object hierarchy whose `toString` is the level's name
              args(0).toString match {
                case "Error" => errors += message; logger.error(message)
                case "Warn"  => logger.warn(message)
                case "Info"  => logger.info(message)
                case _       => logger.debug(message)
              }
              null
            case "trace" =>
              val t = force(args(0)).asInstanceOf[Throwable]
              val out = new java.io.StringWriter()
              t.printStackTrace(new java.io.PrintWriter(out))
              logger.debug(out.toString)
              null
            case "toString" => "bleep-scalajs-linker-logger"
            case "hashCode" => Integer.valueOf(System.identityHashCode(proxy))
            case "equals"   => java.lang.Boolean.valueOf(proxy.asInstanceOf[AnyRef] eq args(0))
            case other      =>
              throw new UnsupportedOperationException(s"org.scalajs.logging.Logger.$other is abstract and not implemented by bleep's linker logger")
          }
      }
    }
    java.lang.reflect.Proxy.newProxyInstance(loader, Array(loggerClass), handler)
  }

  private def collectIRFiles(
      classpath: Seq[Path],
      pathIRContainerObj: Any,
      linkerObj: Any,
      loader: ClassLoader
  ): Any = {

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
      extra: List[ScalaJsLinkConfig.ModuleInitializer],
      moduleInitializerObj: Any,
      loader: ClassLoader,
      isTest: Boolean
  ): Any = {
    val nil = loader.loadClass("scala.collection.immutable.Nil$").getField("MODULE$").get(null)
    // A Scala `List` in the linker's own class loader
    def scalaList(xs: List[Any]): Any = xs.foldRight(nil)((x, acc) => acc.getClass.getMethod("$colon$colon", classOf[Object]).invoke(acc, x))
    def method(name: String, arity: Int) =
      moduleInitializerObj.getClass.getMethods
        .find(m => m.getName == name && m.getParameterCount == arity)
        .getOrElse(throw new IllegalStateException(s"this Scala.js linker has no ModuleInitializer.$name with $arity parameters"))

    val own: List[Any] = mainClass match {
      case Some(mc) =>
        // ModuleInitializer.mainMethodWithArgs(mainClass, "main")
        List(method("mainMethodWithArgs", 2).invoke(moduleInitializerObj, mc, "main"))

      case None if isTest =>
        // For test projects, use TestAdapterInitializer constants to create the test entry point
        // These are well-known values from org.scalajs.testing.adapter.TestAdapterInitializer
        // The test runner must provide the scalajsCom interface for communication
        // ModuleInitializer.mainMethod("org.scalajs.testing.bridge.Bridge", "start")
        List(method("mainMethod", 2).invoke(moduleInitializerObj, "org.scalajs.testing.bridge.Bridge", "start"))

      case None =>
        // no module initializers (will produce empty output)
        Nil
    }

    // The project's own, after bleep's, as sbt's `scalaJSModuleInitializers ++= ...` orders them
    val projects: List[Any] = extra.map {
      case ScalaJsLinkConfig.ModuleInitializer(className, name, None)       => method("mainMethod", 2).invoke(moduleInitializerObj, className, name)
      case ScalaJsLinkConfig.ModuleInitializer(className, name, Some(args)) =>
        method("mainMethodWithArgs", 3).invoke(moduleInitializerObj, className, name, scalaList(args))
    }

    scalaList(own ++ projects)
  }

  private def createOutputDirectory(
      outputDir: Path,
      pathOutputDirectoryObj: Any
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

/** Exception thrown when linking is cancelled. Extends `InterruptedException` so `Outcome.runInFreshThread` classifies it as `Cancelled`, not `Crashed`. */
class LinkingCancelledException(message: String) extends InterruptedException(message)
