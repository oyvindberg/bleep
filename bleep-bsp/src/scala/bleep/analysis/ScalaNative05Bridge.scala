package bleep.analysis

import cats.effect.IO
import java.nio.file.{Files, Path}
import java.lang.reflect.InvocationTargetException
import scala.jdk.CollectionConverters.*

/** Scala Native 0.5.x linker bridge.
  *
  * Uses reflection to invoke the Scala Native 0.5.x build APIs, allowing different Scala Native versions to be loaded in isolated classloaders.
  *
  * Note: Scala Native 0.5.x has an async build API returning Future[Path].
  */
class ScalaNative05Bridge(scalaNativeVersion: String, scalaVersion: String) extends ScalaNativeToolchain {

  override def link(
      config: ScalaNativeLinkConfig,
      classpath: Seq[Path],
      mainClass: String,
      outputPath: Path,
      workDir: Path,
      logger: ScalaNativeToolchain.Logger,
      cancellation: CancellationToken
  ): IO[ScalaNativeLinkResult] =
    // Check cancellation before starting
    if (cancellation.isCancelled) {
      IO.canceled.asInstanceOf[IO[ScalaNativeLinkResult]]
    } else {
      // Run blocking link on a dedicated thread to avoid cats-effect prepareForBlocking deadlock.
      // IO.interruptible/IO.blocking use LinkedTransferQueue.transfer internally which can
      // deadlock the work-stealing pool.
      IO.async[ScalaNativeLinkResult] { cb =>
        IO.delay {
          val thread = new Thread(() =>
            try {
              val result = linkBlocking(config, classpath, mainClass, outputPath, workDir, logger, cancellation)
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
          thread.setName("scala-native-linker")
          thread.start()
          Some(IO.delay {
            cancellation.cancel()
            thread.interrupt()
          })
        }
      }.flatMap { result =>
        if (cancellation.isCancelled) IO.canceled.asInstanceOf[IO[ScalaNativeLinkResult]]
        else IO.pure(result)
      }.handleErrorWith {
        case _: InterruptedException =>
          IO.canceled.asInstanceOf[IO[ScalaNativeLinkResult]]
        case _: LinkingCancelledException =>
          IO.canceled.asInstanceOf[IO[ScalaNativeLinkResult]]
        case e =>
          IO.raiseError(e)
      }
    }

  private def linkBlocking(
      config: ScalaNativeLinkConfig,
      classpath: Seq[Path],
      mainClass: String,
      outputPath: Path,
      workDir: Path,
      logger: ScalaNativeToolchain.Logger,
      cancellation: CancellationToken
  ): ScalaNativeLinkResult = {
    val instance = CompilerResolver.getScalaNativeTools(scalaNativeVersion, scalaVersion)
    val loader = instance.loader

    // Create work directory
    Files.createDirectories(workDir)

    // Check cancellation
    checkCancellation(cancellation)

    try {
      // Load classes
      val configClass = loader.loadClass("scala.scalanative.build.Config")
      val configCompanion = loader.loadClass("scala.scalanative.build.Config$")
      val nativeConfigClass = loader.loadClass("scala.scalanative.build.NativeConfig")
      val nativeConfigCompanion = loader.loadClass("scala.scalanative.build.NativeConfig$")
      val buildClass = loader.loadClass("scala.scalanative.build.Build$")
      val discoverClass = loader.loadClass("scala.scalanative.build.Discover$")
      val gcClass = loader.loadClass("scala.scalanative.build.GC$")
      val modeClass = loader.loadClass("scala.scalanative.build.Mode$")
      val ltoClass = loader.loadClass("scala.scalanative.build.LTO$")
      val buildTargetClass = loader.loadClass("scala.scalanative.build.BuildTarget$")
      val loggerClass = loader.loadClass("scala.scalanative.build.Logger")

      // Get companion objects
      val configObj = configCompanion.getField("MODULE$").get(null)
      val nativeConfigObj = nativeConfigCompanion.getField("MODULE$").get(null)
      val buildObj = buildClass.getField("MODULE$").get(null)
      val discoverObj = discoverClass.getField("MODULE$").get(null)
      val gcObj = gcClass.getField("MODULE$").get(null)
      val modeObj = modeClass.getField("MODULE$").get(null)
      val ltoObj = ltoClass.getField("MODULE$").get(null)
      val buildTargetObj = buildTargetClass.getField("MODULE$").get(null)

      // Discover clang
      val clang = config.clang.getOrElse {
        val clangMethod = discoverObj.getClass.getMethod("clang")
        clangMethod.invoke(discoverObj).asInstanceOf[Path]
      }
      val clangpp = config.clangpp.getOrElse {
        val clangppMethod = discoverObj.getClass.getMethod("clangpp")
        clangppMethod.invoke(discoverObj).asInstanceOf[Path]
      }

      // Create NativeConfig
      val nativeConfigEmpty = nativeConfigCompanion.getMethod("empty").invoke(nativeConfigObj)

      // Set GC
      val gc = mapGC(config.gc, gcObj)
      var nativeConfig = invokeWithMethod(nativeConfigEmpty, "withGC", loader.loadClass("scala.scalanative.build.GC"), gc)

      // Set Mode
      val mode = mapMode(config.mode, modeObj)
      nativeConfig = invokeWithMethod(nativeConfig, "withMode", loader.loadClass("scala.scalanative.build.Mode"), mode)

      // Set LTO
      val lto = mapLTO(config.lto, ltoObj)
      nativeConfig = invokeWithMethod(nativeConfig, "withLTO", loader.loadClass("scala.scalanative.build.LTO"), lto)

      // Set BuildTarget
      val buildTarget = mapBuildTarget(config.buildTarget, buildTargetObj)
      nativeConfig = invokeWithMethod(
        nativeConfig,
        "withBuildTarget",
        loader.loadClass("scala.scalanative.build.BuildTarget"),
        buildTarget
      )

      // Set clang paths
      nativeConfig = invokeWithMethod(nativeConfig, "withClang", classOf[Path], clang)
      nativeConfig = invokeWithMethod(nativeConfig, "withClangPP", classOf[Path], clangpp)

      // Set compile options
      if (config.compileOptions.nonEmpty) {
        val seqClass = loader.loadClass("scala.collection.immutable.Seq")
        val scalaSeq = toScalaSeq(config.compileOptions.asJava, loader)
        nativeConfig = invokeWithMethod(nativeConfig, "withCompileOptions", seqClass, scalaSeq)
      }

      // Set linking options
      if (config.linkingOptions.nonEmpty) {
        val seqClass = loader.loadClass("scala.collection.immutable.Seq")
        val scalaSeq = toScalaSeq(config.linkingOptions.asJava, loader)
        nativeConfig = invokeWithMethod(nativeConfig, "withLinkingOptions", seqClass, scalaSeq)
      }

      // Set embed resources (0.5.x feature)
      try
        nativeConfig = invokeWithMethod(nativeConfig, "withEmbedResources", classOf[Boolean], config.embedResources)
      catch {
        case _: NoSuchMethodException => // Older version
      }

      // Set incremental compilation (0.5.x feature)
      try
        nativeConfig = invokeWithMethod(
          nativeConfig,
          "withIncrementalCompilation",
          classOf[Boolean],
          config.incrementalCompilation
        )
      catch {
        case _: NoSuchMethodException => // Older version
      }

      // Set multithreading (0.5.x feature)
      config.multithreading.foreach { mt =>
        try {
          // Multithreading is Option[Boolean]
          val someClass = loader.loadClass("scala.Some")
          val someConstructor = someClass.getConstructor(classOf[Object])
          val someValue = someConstructor.newInstance(java.lang.Boolean.valueOf(mt))
          nativeConfig = invokeWithMethod(nativeConfig, "withMultithreading", loader.loadClass("scala.Option"), someValue)
        } catch {
          case _: NoSuchMethodException => // Older version
        }
      }

      // Set optimize
      try
        nativeConfig = invokeWithMethod(nativeConfig, "withOptimize", classOf[Boolean], config.optimize)
      catch {
        case _: NoSuchMethodException => // Older version
      }

      // Create Config
      val configEmpty = configCompanion.getMethod("empty").invoke(configObj)

      // Set classpath
      val classpathSeq = toScalaSeq(classpath.asJava, loader)
      var buildConfig = invokeWithMethod(
        configEmpty,
        "withClassPath",
        loader.loadClass("scala.collection.immutable.Seq"),
        classpathSeq
      )

      // Set main class (0.5.x takes Option[String])
      val someClass = loader.loadClass("scala.Some")
      val someConstructor = someClass.getConstructor(classOf[Object])
      val someMainClass = someConstructor.newInstance(mainClass)
      buildConfig = invokeWithMethod(buildConfig, "withMainClass", loader.loadClass("scala.Option"), someMainClass)

      // Set base dir (0.5.x uses baseDir for both working and output directories)
      buildConfig = invokeWithMethod(buildConfig, "withBaseDir", classOf[Path], workDir)

      // Set module name (required in 0.5.x — baseName is derived from it)
      val moduleName = {
        val fileName = outputPath.getFileName.toString
        val dotIdx = fileName.lastIndexOf('.')
        if (dotIdx > 0) fileName.substring(0, dotIdx) else fileName
      }
      buildConfig = invokeWithMethod(buildConfig, "withModuleName", classOf[String], moduleName)

      // Set native config
      buildConfig = invokeWithMethod(buildConfig, "withCompilerConfig", nativeConfigClass, nativeConfig)

      // Create logger
      val buildLogger = createBuildLogger(logger, loader, loggerClass)
      buildConfig = invokeWithMethod(buildConfig, "withLogger", loggerClass, buildLogger)

      // Check cancellation before build
      checkCancellation(cancellation)

      // 0.5.6 signature: build(config: Config)(implicit scope: Scope, ec: ExecutionContext): Future[Path]
      logger.info(s"Linking Scala Native binary to $outputPath")

      // Find build method — prefer the overload that returns Future (not void)
      val buildMethods = buildObj.getClass.getMethods.filter(_.getName == "build")
      val buildMethod = buildMethods
        .find(m => m.getReturnType.getName.contains("Future"))
        .orElse(buildMethods.sortBy(_.getParameterCount).headOption)
        .getOrElse {
          val available =
            buildObj.getClass.getMethods
              .map(m => s"${m.getName}(${m.getParameterTypes.map(_.getName).mkString(", ")}): ${m.getReturnType.getName}")
              .sorted
              .distinct
              .mkString("; ")
          throw new RuntimeException(s"No 'build' method on ${buildObj.getClass.getName}. Available: $available")
        }

      // Build args by matching each parameter type
      val n = buildMethod.getParameterCount
      val paramTypes = buildMethod.getParameterTypes
      val args = new Array[AnyRef](n)
      var scope: Any = null
      for (i <- 0 until n) {
        val paramName = paramTypes(i).getName
        args(i) = paramName match {
          case name if name.contains("Config") =>
            buildConfig.asInstanceOf[AnyRef]
          case "java.nio.file.Path" =>
            outputPath.asInstanceOf[AnyRef]
          case name if name.contains("Scope") =>
            // Create Scope.unsafe() for resource management
            val scopeCompanion = loader.loadClass("scala.scalanative.util.Scope$")
            val scopeObj = scopeCompanion.getField("MODULE$").get(null)
            scope = scopeCompanion.getMethod("unsafe").invoke(scopeObj)
            scope.asInstanceOf[AnyRef]
          case name if name.contains("ExecutionContext") =>
            // Use the JVM's shared ForkJoinPool rather than creating a classloader-specific pool
            val ecCompanion = loader.loadClass("scala.concurrent.ExecutionContext$")
            val ecObj = ecCompanion.getField("MODULE$").get(null)
            val fromExec = ecCompanion.getMethod("fromExecutor", classOf[java.util.concurrent.Executor])
            fromExec.invoke(ecObj, java.util.concurrent.ForkJoinPool.commonPool()).asInstanceOf[AnyRef]
          case name if name.contains("Logger") =>
            createBuildLogger(logger, loader, loggerClass).asInstanceOf[AnyRef]
          case _ =>
            throw new RuntimeException(s"Build.build has $n params, cannot determine arg $i of type $paramName")
        }
      }

      val future =
        try buildMethod.invoke(buildObj, args: _*)
        catch {
          case e: Throwable =>
            // Close scope on failure before rethrowing
            if (scope != null) {
              try scope.getClass.getMethod("close").invoke(scope)
              catch { case _: Exception => () }
            }
            throw e
        }

      if (future == null) {
        val sig = s"${buildMethod.getName}(${buildMethod.getParameterTypes.map(_.getName).mkString(", ")}): ${buildMethod.getReturnType.getName}"
        throw new RuntimeException(s"Build.build returned null — wrong overload selected? Method: $sig")
      }

      // Wait for the result with cancellation checking, then close scope
      val result =
        try awaitResult(future, loader, cancellation)
        finally
          if (scope != null) {
            try scope.getClass.getMethod("close").invoke(scope)
            catch { case _: Exception => () }
          }

      // Check cancellation after build
      checkCancellation(cancellation)

      // Copy result to output path if needed
      val resultPath = result.asInstanceOf[Path]
      if (resultPath != outputPath && Files.exists(resultPath)) {
        Files.copy(resultPath, outputPath, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      }

      ScalaNativeLinkResult(outputPath, 0)
    } catch {
      case e: InvocationTargetException =>
        logger.error(s"Scala Native linking failed: ${e.getCause.getMessage}")
        throw e.getCause
      case e: Exception =>
        logger.error(s"Scala Native linking failed: ${e.getMessage}")
        throw e
    }
  }

  private def mapGC(gc: ScalaNativeLinkConfig.NativeGC, gcObj: Any): Any = {
    val method = gc match {
      case ScalaNativeLinkConfig.NativeGC.Immix        => "immix"
      case ScalaNativeLinkConfig.NativeGC.Commix       => "commix"
      case ScalaNativeLinkConfig.NativeGC.Boehm        => "boehm"
      case ScalaNativeLinkConfig.NativeGC.NoGC         => "none"
      case ScalaNativeLinkConfig.NativeGC.Experimental => "experimental"
    }
    gcObj.getClass.getMethod(method).invoke(gcObj)
  }

  private def mapMode(mode: ScalaNativeLinkConfig.NativeMode, modeObj: Any): Any = {
    val method = mode match {
      case ScalaNativeLinkConfig.NativeMode.Debug       => "debug"
      case ScalaNativeLinkConfig.NativeMode.ReleaseFast => "releaseFast"
      case ScalaNativeLinkConfig.NativeMode.ReleaseFull => "releaseFull"
      case ScalaNativeLinkConfig.NativeMode.ReleaseSize => "releaseSize"
    }
    try
      modeObj.getClass.getMethod(method).invoke(modeObj)
    catch {
      case _: NoSuchMethodException =>
        // Fallback for older versions
        mode match {
          case ScalaNativeLinkConfig.NativeMode.Debug       => modeObj.getClass.getMethod("debug").invoke(modeObj)
          case ScalaNativeLinkConfig.NativeMode.ReleaseFast => modeObj.getClass.getMethod("release").invoke(modeObj)
          case ScalaNativeLinkConfig.NativeMode.ReleaseFull => modeObj.getClass.getMethod("release").invoke(modeObj)
          case ScalaNativeLinkConfig.NativeMode.ReleaseSize => modeObj.getClass.getMethod("release").invoke(modeObj)
        }
    }
  }

  private def mapLTO(lto: ScalaNativeLinkConfig.NativeLTO, ltoObj: Any): Any = {
    val method = lto match {
      case ScalaNativeLinkConfig.NativeLTO.None => "none"
      case ScalaNativeLinkConfig.NativeLTO.Thin => "thin"
      case ScalaNativeLinkConfig.NativeLTO.Full => "full"
    }
    ltoObj.getClass.getMethod(method).invoke(ltoObj)
  }

  private def mapBuildTarget(target: ScalaNativeLinkConfig.NativeBuildTarget, targetObj: Any): Any = {
    val method = target match {
      case ScalaNativeLinkConfig.NativeBuildTarget.Application    => "application"
      case ScalaNativeLinkConfig.NativeBuildTarget.LibraryDynamic => "libraryDynamic"
      case ScalaNativeLinkConfig.NativeBuildTarget.LibraryStatic  => "libraryStatic"
    }
    try
      targetObj.getClass.getMethod(method).invoke(targetObj)
    catch {
      case _: NoSuchMethodException =>
        // Fallback - just use application
        targetObj.getClass.getMethod("application").invoke(targetObj)
    }
  }

  private def invokeWithMethod(obj: Any, methodName: String, paramType: Class[?], value: Any): Any = {
    val method = obj.getClass.getMethod(methodName, paramType)
    method.invoke(obj, value.asInstanceOf[AnyRef])
  }

  private def toScalaSeq(javaList: java.util.List[?], loader: ClassLoader): Any = {
    val convertersClass = loader.loadClass("scala.jdk.javaapi.CollectionConverters$")
    val convertersObj = convertersClass.getField("MODULE$").get(null)
    val asScalaMethod = convertersClass.getMethods
      .find(m => m.getName == "asScala" && m.getParameterCount == 1 && m.getParameterTypes()(0) == classOf[java.util.List[?]])
      .get
    val scalaBuffer = asScalaMethod.invoke(convertersObj, javaList)
    val toListMethod = scalaBuffer.getClass.getMethod("toList")
    toListMethod.invoke(scalaBuffer)
  }

  private def createBuildLogger(logger: ScalaNativeToolchain.Logger, loader: ClassLoader, loggerClass: Class[?]): Any =
    // Create a proxy that delegates to our logger, suppressing trace/debug noise
    java.lang.reflect.Proxy.newProxyInstance(
      loader,
      Array(loggerClass),
      new java.lang.reflect.InvocationHandler {
        def invoke(proxy: AnyRef, method: java.lang.reflect.Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "trace" | "debug" => null // suppress verbose Scala Native output
            case "info"            => evalLogArg(args).foreach(logger.info(_)); null
            case "warn"            => evalLogArg(args).foreach(logger.warn(_)); null
            case "error"           => evalLogArg(args).foreach(logger.error(_)); null
            case "toString"        => "BleepLogger"
            case "hashCode"        => Int.box(System.identityHashCode(proxy))
            case "equals"          => Boolean.box(proxy eq args(0))
            case _                 =>
              // Concrete trait methods (timeAsync, time, running) are default methods
              // on the interface — delegate to the actual implementation so they work.
              if (method.isDefault)
                java.lang.reflect.InvocationHandler.invokeDefault(proxy, method, args: _*)
              else {
                val rt = method.getReturnType
                if (rt == java.lang.Boolean.TYPE) Boolean.box(false)
                else if (rt == java.lang.Integer.TYPE) Int.box(0)
                else null
              }
          }

        private def evalLogArg(args: Array[AnyRef]): Option[String] =
          if (args == null || args.length < 1) None
          else
            args(0) match {
              case f: Function0[?] => Some(f.asInstanceOf[Function0[String]]())
              case t: Throwable    => Some(t.toString)
              case other           => Some(String.valueOf(other))
            }
      }
    )

  private def awaitResult(future: Any, loader: ClassLoader, cancellation: CancellationToken): Any = {
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

      // Unfortunately Scala Native doesn't have a cooperative cancellation mechanism,
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

  /** Check cancellation and throw if cancelled. Also checks thread interrupt. */
  private def checkCancellation(cancellation: CancellationToken): Unit = {
    if (Thread.interrupted()) {
      throw new InterruptedException("Linking interrupted")
    }
    if (cancellation.isCancelled) {
      throw new LinkingCancelledException("Linking cancelled")
    }
  }
}
