package bleep.bsp

import bleep.analysis.*
import bleep.bsp.Outcome.KillReason
import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import ch.epfl.scala.bsp.*
import com.github.plokhotnyuk.jsoniter_scala.core.*

import java.io.{InputStream, OutputStream}
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.jdk.CollectionConverters.*

/** BSP server implementation for cross-language incremental compilation.
  *
  * Communicates over stdin/stdout using JSON-RPC 2.0 with Content-Length framing. Supports a single active build at a time.
  *
  * This server supports:
  *   - Scala build targets (dataKind = "scala")
  *   - Kotlin build targets (dataKind = "kotlin")
  *   - Java build targets (dataKind = "jvm")
  */
class BspServer(
    in: InputStream,
    out: OutputStream,
    workspaceRoot: Path
) {

  private val transport = new JsonRpcTransport(in, out)
  private val initialized = AtomicBoolean(false)
  private val shutdownRequested = AtomicBoolean(false)
  private val clientCapabilities = AtomicReference[Option[BuildClientCapabilities]](None)

  /** The build loader for managing project configuration */
  private val buildLoader = BuildLoader.forWorkspace(workspaceRoot)

  /** The build state - only one build active at a time */
  private val buildState = AtomicReference[Option[BuildState]](None)

  /** Active requests and their cancellation tokens for cooperative cancellation. Key is the JSON-RPC request id (as string), value is the cancellation token.
    */
  private val activeRequests = ConcurrentHashMap[String, CancellationToken]()

  /** Set the build state (called after loading build configuration) */
  def setBuildState(state: BuildState): Unit =
    buildState.set(Some(state))

  /** Get the build loader for configuration */
  def getBuildLoader: BuildLoader = buildLoader

  /** Run the server message loop */
  def run(): Unit = {
    // Ensure child processes are killed on JVM shutdown (e.g. Ctrl-C)
    val shutdownHook = new Thread(() => cancelAllActiveRequests(), "bsp-shutdown-cancel")
    Runtime.getRuntime.addShutdownHook(shutdownHook)
    try
      while !shutdownRequested.get() do
        transport.readMessage() match {
          case Some(request) =>
            handleRequest(request)
          case None =>
            // Stream closed
            return
        }
    finally
      try Runtime.getRuntime.removeShutdownHook(shutdownHook)
      catch { case _: IllegalStateException => () } // JVM already shutting down
  }

  /** Methods that run on a CE blocking fiber so the main loop can process cancellation. */
  private val asyncMethods = Set("buildTarget/run")

  /** Handle an incoming JSON-RPC request */
  private def handleRequest(request: JsonRpcRequest): Unit = {
    // Create cancellation token for this request using atomic putIfAbsent
    // This prevents race condition where two threads try to register the same ID
    val cancellationToken = request.id match {
      case Some(id) =>
        val idStr = id.key
        val token = CancellationToken.create()
        val existing = activeRequests.putIfAbsent(idStr, token)
        if (existing != null) existing else token
      case None =>
        CancellationToken.never
    }

    if (asyncMethods.contains(request.method))
      // Fork to a CE blocking fiber so the main loop can still process $/cancelRequest
      IO.blocking(dispatchAndRespond(request, cancellationToken)).start.unsafeRunAndForget()
    else
      dispatchAndRespond(request, cancellationToken)
  }

  private def dispatchAndRespond(request: JsonRpcRequest, cancellationToken: CancellationToken): Unit =
    try {
      val result = dispatch(request.method, request.params, cancellationToken)

      request.id match {
        case Some(id) =>
          // Clean up tracking
          activeRequests.remove(id.key)
          // Request - send response
          val response = JsonRpcResponse(
            jsonrpc = "2.0",
            id = id,
            result = result,
            error = None
          )
          transport.sendResponse(response)
        case None =>
          // Notification - no response needed
          ()
      }
    } catch {
      case e: BspException =>
        request.id.foreach { id =>
          activeRequests.remove(id.key)
          trySendResponse(id, None, Some(JsonRpcError(e.code, e.getMessage, None)))
        }
      case e: Exception =>
        System.err.println(s"[BSP] Error handling ${request.method}: ${e.getMessage}")
        e.printStackTrace(System.err)
        request.id.foreach { id =>
          activeRequests.remove(id.key)
          trySendResponse(
            id,
            None,
            Some(JsonRpcError(JsonRpcErrorCodes.InternalError, e.getMessage, None))
          )
        }
    }

  /** Try to send a JSON-RPC response. If the transport is dead, log and move on. */
  private def trySendResponse(id: RpcId, result: Option[RawJson], error: Option[JsonRpcError]): Unit =
    try
      transport.sendResponse(JsonRpcResponse(jsonrpc = "2.0", id = id, result = result, error = error))
    catch {
      case e: java.io.IOException =>
        System.err.println(s"[BSP] Failed to send response (client disconnected): ${e.getMessage}")
      case e: Exception =>
        System.err.println(s"[BSP] Failed to send response: ${e.getMessage}")
    }

  /** Dispatch a method call to the appropriate handler */
  private def dispatch(method: String, params: Option[RawJson], cancellation: CancellationToken): Option[RawJson] = {
    if !initialized.get() && method != "build/initialize" then
      throw BspException(
        JsonRpcErrorCodes.ServerNotInitialized,
        "Server not initialized"
      )

    method match {
      // Lifecycle
      case "build/initialize" =>
        val p = parseParams[InitializeBuildParams](params)
        Some(toRaw(handleInitialize(p)))

      case "build/initialized" =>
        handleInitialized()
        None

      case "build/shutdown" =>
        handleShutdown()
        None

      case "build/exit" =>
        handleExit()
        None

      // Workspace
      case "workspace/buildTargets" =>
        Some(toRaw(handleBuildTargets()))

      case "workspace/reload" =>
        handleReload()
        None

      // Build target info
      case "buildTarget/sources" =>
        val p = parseParams[SourcesParams](params)
        Some(toRaw(handleSources(p)))

      case "buildTarget/inverseSources" =>
        val p = parseParams[InverseSourcesParams](params)
        Some(toRaw(handleInverseSources(p)))

      case "buildTarget/dependencySources" =>
        val p = parseParams[DependencySourcesParams](params)
        Some(toRaw(handleDependencySources(p)))

      case "buildTarget/dependencyModules" =>
        val p = parseParams[DependencyModulesParams](params)
        Some(toRaw(handleDependencyModules(p)))

      case "buildTarget/resources" =>
        val p = parseParams[ResourcesParams](params)
        Some(toRaw(handleResources(p)))

      case "buildTarget/outputPaths" =>
        val p = parseParams[OutputPathsParams](params)
        Some(toRaw(handleOutputPaths(p)))

      // Compilation
      case "buildTarget/compile" =>
        val p = parseParams[CompileParams](params)
        Some(toRaw(handleCompile(p, cancellation)))

      case "buildTarget/cleanCache" =>
        val p = parseParams[CleanCacheParams](params)
        Some(toRaw(handleCleanCache(p)))

      // Run/Test
      case "buildTarget/run" =>
        val p = parseParams[RunParams](params)
        Some(toRaw(handleRun(p, cancellation)))

      case "buildTarget/test" =>
        val p = parseParams[TestParams](params)
        Some(toRaw(handleTest(p)))

      // JVM-specific
      case "buildTarget/jvmRunEnvironment" =>
        val p = parseParams[JvmRunEnvironmentParams](params)
        Some(toRaw(handleJvmRunEnvironment(p)))

      case "buildTarget/jvmTestEnvironment" =>
        val p = parseParams[JvmTestEnvironmentParams](params)
        Some(toRaw(handleJvmTestEnvironment(p)))

      case "buildTarget/jvmCompileClasspath" =>
        val p = parseParams[JvmCompileClasspathParams](params)
        Some(toRaw(handleJvmCompileClasspath(p)))

      // Scala-specific
      case "buildTarget/scalacOptions" =>
        val p = parseParams[ScalacOptionsParams](params)
        Some(toRaw(handleScalacOptions(p)))

      // Java-specific
      case "buildTarget/javacOptions" =>
        val p = parseParams[JavacOptionsParams](params)
        Some(toRaw(handleJavacOptions(p)))

      // Cancellation
      case "$/cancelRequest" =>
        val p = parseParams[CancelRequestParams](params)
        handleCancelRequest(p)
        None

      case _ =>
        throw BspException(
          JsonRpcErrorCodes.MethodNotFound,
          s"Method not found: $method"
        )
    }
  }

  // ==========================================================================
  // Lifecycle handlers
  // ==========================================================================

  private def handleInitialize(params: InitializeBuildParams): InitializeBuildResult = {
    clientCapabilities.set(Some(params.capabilities))

    initialized.set(true)

    InitializeBuildResult(
      displayName = "Bleep BSP Server",
      version = "0.1.0",
      bspVersion = Bsp4s.ProtocolVersion,
      capabilities = BuildServerCapabilities(
        compileProvider = Some(CompileProvider(List("scala", "java", "kotlin"))),
        testProvider = Some(TestProvider(List("scala", "java", "kotlin"))),
        runProvider = Some(RunProvider(List("scala", "java", "kotlin"))),
        debugProvider = None,
        inverseSourcesProvider = Some(true),
        dependencySourcesProvider = Some(true),
        dependencyModulesProvider = Some(true),
        resourcesProvider = Some(true),
        outputPathsProvider = Some(true),
        buildTargetChangedProvider = Some(false),
        jvmRunEnvironmentProvider = Some(true),
        jvmTestEnvironmentProvider = Some(true),
        cargoFeaturesProvider = None,
        canReload = Some(true),
        jvmCompileClasspathProvider = Some(true)
      ),
      dataKind = None,
      data = None
    )
  }

  private def handleInitialized(): Unit =
    // Client has finished initialization
    // We can start any background work here
    ()

  private def handleShutdown(): Unit = {
    shutdownRequested.set(true)
    cancelAllActiveRequests()
  }

  /** Cancel all in-flight requests, killing any child processes via onCancel callbacks. */
  private def cancelAllActiveRequests(): Unit =
    activeRequests.values().forEach(_.cancel())

  private def handleExit(): Unit = {
    // Exit the process
    val exitCode = if shutdownRequested.get() then 0 else 1
    System.exit(exitCode)
  }

  /** Handle cancellation request from client */
  private def handleCancelRequest(params: CancelRequestParams): Unit = {
    val idStr = params.id match {
      case Left(s)  => s
      case Right(i) => i.toString
    }
    Option(activeRequests.remove(idStr)).foreach { token =>
      token.cancel()
      System.err.println(s"[BSP] Cancelled request $idStr")
    }
  }

  // ==========================================================================
  // Workspace handlers
  // ==========================================================================

  private def handleBuildTargets(): WorkspaceBuildTargetsResult =
    buildState.get() match {
      case Some(state) =>
        // Apply Java-only target fix for IntelliJ compatibility
        val fixedTargets = state.targets.map(fixJavaOnlyTarget)
        WorkspaceBuildTargetsResult(fixedTargets)
      case None =>
        WorkspaceBuildTargetsResult(List.empty)
    }

  /** Fix Java-only targets for IntelliJ compatibility.
    *
    * Bloop doesn't set dataKind for Java-only targets, but IntelliJ requires dataKind=jvm to request javacOptions. This fixes up the target to add the missing
    * dataKind.
    *
    * Fixes up Java-only targets to have proper dataKind=jvm
    */
  private def fixJavaOnlyTarget(target: BuildTarget): BuildTarget = {
    val isJavaOnly = target.languageIds.contains("java") && !target.languageIds.contains("scala")
    val hasNoDataKind = target.dataKind.isEmpty

    if isJavaOnly && hasNoDataKind then {
      // Create JvmBuildTarget data if none exists
      val jvmTarget = JvmBuildTarget(
        javaHome = Some(Uri(java.nio.file.Path.of(System.getProperty("java.home")).toUri)),
        javaVersion = Some(System.getProperty("java.version"))
      )
      target.copy(
        dataKind = Some(BuildTargetDataKind.Jvm),
        data = Some(RawJson(writeToArray(jvmTarget)(using JvmBuildTarget.codec)))
      )
    } else target
  }

  private def handleReload(): Unit = {
    // Reload the build configuration
    System.err.println("[BSP] workspace/reload requested")

    // Send buildTargetsChanged notification to inform the client
    // The client should then re-query build targets to get the updated configuration
    sendBuildTargetsChanged()
  }

  /** Send a build/targetDidChange notification to inform the client that build targets have changed */
  private def sendBuildTargetsChanged(): Unit = {
    // In a full implementation, this would include the specific changed targets
    // For now, we signal that targets may have changed without specifics (empty list)
    val notification = JsonRpcNotification(
      jsonrpc = "2.0",
      method = "build/targetDidChange",
      params = Some(
        RawJson(
          writeToArray(
            DidChangeBuildTarget(changes = List.empty)
          )
        )
      )
    )
    transport.sendNotification(notification)
  }

  // ==========================================================================
  // Build target info handlers
  // ==========================================================================

  private def handleSources(params: SourcesParams): SourcesResult = {
    val items = params.targets.map { targetId =>
      val sources = buildLoader.getSources(targetId)
      val roots = buildLoader.getProject(targetId).map { project =>
        project.sources.filter(Files.isDirectory(_)).map(p => Uri(p.toUri)).toList
      }
      SourcesItem(
        target = targetId,
        sources = sources,
        roots = roots
      )
    }
    SourcesResult(items)
  }

  private def handleInverseSources(params: InverseSourcesParams): InverseSourcesResult = {
    // Find which build targets contain this source file
    val docPath = Path.of(java.net.URI.create(params.textDocument.uri.value))
    val targets = buildLoader.findTargetForSource(docPath).toList
    InverseSourcesResult(targets)
  }

  private def handleDependencySources(params: DependencySourcesParams): DependencySourcesResult = {
    // For now, we don't have source JARs - could be added later via coursier
    val items = params.targets.map { targetId =>
      DependencySourcesItem(
        target = targetId,
        sources = List.empty
      )
    }
    DependencySourcesResult(items)
  }

  private def handleDependencyModules(params: DependencyModulesParams): DependencyModulesResult = {
    // Build dependency module info from classpath
    val items = params.targets.map { targetId =>
      val modules = buildLoader.getProject(targetId).toList.flatMap { project =>
        project.classpath.flatMap { jarPath =>
          // Parse Maven artifact info from JAR path
          parseMavenArtifact(jarPath).map { case (org, name, version) =>
            DependencyModule(
              name = s"$org:$name",
              version = version,
              dataKind = Some(DependencyModuleDataKind.Maven),
              data = Some(
                RawJson(
                  writeToArray(
                    MavenDependencyModule(
                      organization = org,
                      name = name,
                      version = version,
                      artifacts = List(
                        MavenDependencyModuleArtifact(
                          uri = Uri(jarPath.toUri),
                          classifier = None
                        )
                      ),
                      scope = None
                    )
                  )(using MavenDependencyModule.codec)
                )
              )
            )
          }
        }
      }
      DependencyModulesItem(
        target = targetId,
        modules = modules
      )
    }
    DependencyModulesResult(items)
  }

  private def handleResources(params: ResourcesParams): ResourcesResult = {
    val items = params.targets.map { targetId =>
      ResourcesItem(
        target = targetId,
        resources = buildLoader.getResources(targetId)
      )
    }
    ResourcesResult(items)
  }

  private def handleOutputPaths(params: OutputPathsParams): OutputPathsResult = {
    val items = params.targets.map { targetId =>
      OutputPathsItem(
        target = targetId,
        outputPaths = buildLoader.getOutputPaths(targetId)
      )
    }
    OutputPathsResult(items)
  }

  // ==========================================================================
  // Compilation handlers
  // ==========================================================================

  private def handleCompile(params: CompileParams, cancellation: CancellationToken): CompileResult = {
    // Check for --link and --release flags in arguments
    val args = params.arguments.getOrElse(List.empty)
    val isLink = args.contains("--link")
    val isRelease = args.contains("--release")

    if (isLink) {
      handleCompileAndLink(params, isRelease, cancellation)
    } else {
      // Use parallel compiler which respects dependency order
      handleCompileParallel(params, cancellation)
    }
  }

  /** Handle compile + link for non-JVM platforms. */
  private def handleCompileAndLink(
      params: CompileParams,
      releaseMode: Boolean,
      cancellation: CancellationToken
  ): CompileResult = {
    import cats.effect.unsafe.implicits.global

    val taskId = "compile-and-link"
    sendTaskStart(params.originId, taskId, s"Compiling and linking ${params.targets.size} target(s)...")

    var hasErrors = false
    var wasCancelled = false

    for (targetId <- params.targets if !cancellation.isCancelled)
      buildLoader.getProject(targetId) match {
        case Some(project) =>
          // First compile
          val compileTaskId = s"compile-${project.name}"
          sendTaskStart(params.originId, compileTaskId, s"Compiling ${project.name}...")

          val sourceFiles = project.sources.toSeq.flatMap { srcPath =>
            if Files.isDirectory(srcPath) then collectSourceFiles(srcPath, project.languageConfig)
            else if Files.isRegularFile(srcPath) then {
              val content = Files.readString(srcPath)
              val relPath = workspaceRoot.relativize(srcPath)
              Seq(SourceFile(relPath, content))
            } else Seq.empty
          }

          if (sourceFiles.nonEmpty) {
            val input = CompilationInput(
              sources = sourceFiles,
              classpath = project.classpath,
              outputDir = project.outputDir,
              config = project.languageConfig
            )

            val compiler = Compiler.forConfig(project.languageConfig)
            val listener = createDiagnosticListener(targetId, params.originId)
            val compileResult = compiler.compile(input, listener, cancellation)

            compileResult match {
              case CompilationSuccess(_, _) =>
                sendTaskFinish(params.originId, compileTaskId, StatusCode.Ok)

                // Now link if this is a non-JVM platform
                val linkResult = linkProject(project, releaseMode, cancellation)
                linkResult match {
                  case TaskDag.LinkResult.JsSuccess(_, _, _, _) | TaskDag.LinkResult.NativeSuccess(_, _) | TaskDag.LinkResult.NotApplicable =>
                  // Success
                  case TaskDag.LinkResult.Failure(error, _) =>
                    hasErrors = true
                    sendLogMessage(s"Link failed: $error", MessageType.Error)
                  case TaskDag.LinkResult.Killed(reason) =>
                    wasCancelled = true
                  case TaskDag.LinkResult.Cancelled =>
                    wasCancelled = true
                }

              case CompilationFailure(_) =>
                hasErrors = true
                sendTaskFinish(params.originId, compileTaskId, StatusCode.Error)

              case CompilationCancelled =>
                wasCancelled = true
                sendTaskFinish(params.originId, compileTaskId, StatusCode.Cancelled)
            }
          } else {
            sendTaskFinish(params.originId, compileTaskId, StatusCode.Ok)
          }

        case None =>
          hasErrors = true
      }

    val statusCode =
      if wasCancelled || cancellation.isCancelled then StatusCode.Cancelled
      else if hasErrors then StatusCode.Error
      else StatusCode.Ok

    sendTaskFinish(params.originId, taskId, statusCode)

    CompileResult(
      originId = params.originId,
      statusCode = statusCode,
      dataKind = None,
      data = None
    )
  }

  /** Link a project for non-JVM platforms. */
  private def linkProject(
      project: buildLoader.ProjectInfo,
      releaseMode: Boolean,
      cancellation: CancellationToken
  ): TaskDag.LinkResult = {
    import cats.effect.unsafe.implicits.global

    // Detect platform from project configuration
    val platform = detectPlatform(project)

    platform match {
      case TaskDag.LinkPlatform.Jvm =>
        TaskDag.LinkResult.NotApplicable

      case _ =>
        val linkTaskId = s"link-${project.name}"
        sendTaskStart(None, linkTaskId, s"Linking ${project.name}...")

        val linkTask = TaskDag.LinkTask(
          project = bleep.model.CrossProjectName(bleep.model.ProjectName(project.name), None),
          platform = platform,
          releaseMode = releaseMode,
          isTest = false
        )

        val logger = LinkExecutor.LinkLogger.forBsp(this)
        val outputDir = project.outputDir.getParent.resolve("link-output")

        val mainClass = project.languageConfig match {
          case sc: ScalaConfig => None // Could be extracted from project config
          case _               => None
        }

        val classpath = project.outputDir +: project.classpath

        val program = for {
          killSignal <- Outcome.fromCancellationToken(cancellation)
          result <- LinkExecutor.execute(
            linkTask,
            classpath,
            mainClass,
            outputDir,
            logger,
            killSignal
          )
        } yield result

        val result = program.unsafeRunSync()

        val statusCode = result._1 match {
          case TaskDag.TaskResult.Success   => StatusCode.Ok
          case TaskDag.TaskResult.Cancelled => StatusCode.Cancelled
          case _                            => StatusCode.Error
        }

        sendTaskFinish(None, linkTaskId, statusCode)
        result._2
    }
  }

  /** Detect the platform from project configuration. */
  private def detectPlatform(project: buildLoader.ProjectInfo): TaskDag.LinkPlatform = {
    // Check classpath for platform indicators
    val classpathStr = project.classpath.map(_.toString).mkString
    val outputStr = project.outputDir.toString

    if (classpathStr.contains("scalajs") || outputStr.contains("js")) {
      // Scala.js project
      val version = extractScalaJsVersion(project.classpath).getOrElse {
        throw new IllegalStateException(s"Could not determine Scala.js version for project ${project.name}")
      }
      val scalaVersion = project.languageConfig match {
        case sc: ScalaConfig => sc.version
        case other =>
          throw new IllegalStateException(s"Scala.js project ${project.name} has non-Scala language config: $other")
      }
      val config = if (project.outputDir.toString.contains("release")) {
        ScalaJsLinkConfig.Release
      } else {
        ScalaJsLinkConfig.Debug
      }
      TaskDag.LinkPlatform.ScalaJs(version, scalaVersion, config)

    } else if (classpathStr.contains("scala-native") || outputStr.contains("native")) {
      // Scala Native project
      val version = extractScalaNativeVersion(project.classpath).getOrElse {
        throw new IllegalStateException(s"Could not determine Scala Native version for project ${project.name}")
      }
      val scalaVersion = project.languageConfig match {
        case sc: ScalaConfig => sc.version
        case other =>
          throw new IllegalStateException(s"Scala Native project ${project.name} has non-Scala language config: $other")
      }
      TaskDag.LinkPlatform.ScalaNative(version, scalaVersion, ScalaNativeLinkConfig.Debug)

    } else if (classpathStr.contains("kotlin") && classpathStr.contains("-js")) {
      // Kotlin/JS project
      val version = extractKotlinVersion(project.classpath).getOrElse {
        throw new IllegalStateException(s"Could not determine Kotlin version for project ${project.name}")
      }
      TaskDag.LinkPlatform.KotlinJs(
        version,
        TaskDag.KotlinJsConfig(
          moduleKind = bleep.model.KotlinJsModuleKind.CommonJS,
          sourceMap = true,
          dce = false, // Default without DCE
          outputDir = project.outputDir.resolve("js")
        )
      )

    } else if (classpathStr.contains("kotlin") && classpathStr.contains("-native")) {
      // Kotlin/Native project
      val version = extractKotlinVersion(project.classpath).getOrElse {
        throw new IllegalStateException(s"Could not determine Kotlin version for project ${project.name}")
      }
      TaskDag.LinkPlatform.KotlinNative(
        version,
        TaskDag.KotlinNativeConfig(
          target = detectNativeTarget(),
          debugInfo = false,
          optimizations = false,
          isTest = false
        )
      )

    } else {
      // JVM project
      TaskDag.LinkPlatform.Jvm
    }
  }

  /** Extract a version string from classpath JARs matching a regex pattern. */
  private def extractVersionFromClasspath(classpath: List[Path], pattern: scala.util.matching.Regex): Option[String] =
    classpath.flatMap { p =>
      pattern.findFirstMatchIn(p.getFileName.toString).map(_.group(1))
    }.headOption

  /** Extract Scala.js version from classpath. */
  private def extractScalaJsVersion(classpath: List[Path]): Option[String] =
    extractVersionFromClasspath(classpath, """scalajs-library[_-](\d+\.\d+\.\d+)""".r)

  /** Extract Scala Native version from classpath. */
  private def extractScalaNativeVersion(classpath: List[Path]): Option[String] =
    extractVersionFromClasspath(classpath, """nativelib[_-].*?[_-](\d+\.\d+\.\d+)""".r)

  /** Extract Kotlin version from classpath. */
  private def extractKotlinVersion(classpath: List[Path]): Option[String] =
    extractVersionFromClasspath(classpath, """kotlin-stdlib[_-](\d+\.\d+\.\d+)""".r)

  /** Detect the native target for the current platform. */
  private def detectNativeTarget(): String = {
    val os = System.getProperty("os.name").toLowerCase
    val arch = System.getProperty("os.arch").toLowerCase

    val osTarget =
      if (os.contains("mac")) "macos"
      else if (os.contains("linux")) "linux"
      else if (os.contains("win")) "mingw"
      else "linux"

    val archTarget =
      if (arch.contains("aarch64") || arch.contains("arm64")) "arm64"
      else "x64"

    s"$osTarget-$archTarget"
  }

  private def handleCleanCache(params: CleanCacheParams): CleanCacheResult = {
    var cleaned = false
    params.targets.foreach { targetId =>
      buildLoader.getProject(targetId).foreach { project =>
        val classesDir = project.outputDir
        if (Files.exists(classesDir)) {
          bleep.internal.FileUtils.deleteDirectory(classesDir)
          cleaned = true
        }
        // Also clean zinc analysis directory
        val analysisDir = classesDir.getParent.resolve(".zinc")
        if (Files.exists(analysisDir)) {
          bleep.internal.FileUtils.deleteDirectory(analysisDir)
          cleaned = true
        }
      }
    }
    CleanCacheResult(
      message = if (cleaned) Some("Cache cleaned") else Some("Nothing to clean"),
      cleaned = cleaned
    )
  }

  // ==========================================================================
  // Parallel compilation support
  // ==========================================================================

  /** Compile using parallel analysis and compilation with Cats Effect.
    *
    * This version uses maximum parallelism for both analysis and compilation, respecting file-level dependencies within each target.
    */
  private def handleCompileParallel(params: CompileParams, cancellation: CancellationToken): CompileResult =
    // Run the IO-based parallel compilation synchronously
    handleCompileParallelIO(params, cancellation).unsafeRunSync()

  private def handleCompileParallelIO(params: CompileParams, cancellation: CancellationToken): IO[CompileResult] = {
    import cats.syntax.all.*

    // Build ProjectDag from requested targets
    val targetProjects = params.targets.flatMap { targetId =>
      buildLoader.getProject(targetId).map(targetId -> _)
    }.toMap

    if targetProjects.isEmpty then
      IO.pure(
        CompileResult(
          originId = params.originId,
          statusCode = StatusCode.Ok,
          dataKind = None,
          data = None
        )
      )
    else {
      // Convert BuildLoader.ProjectInfo to ProjectConfig
      val projectConfigs: Seq[(ProjectConfig, Set[String])] = targetProjects.values.map { project =>
        val language = convertToProjectLanguage(project.languageConfig)
        val config = ProjectConfig(
          name = project.name,
          sources = project.sources,
          classpath = project.classpath,
          outputDir = project.outputDir,
          language = language,
          analysisDir = Some(project.outputDir.resolve(".zinc"))
        )
        // Get dependency names from target IDs
        val deps = project.dependsOn.flatMap { depId =>
          buildLoader.getProject(depId).map(_.name)
        }
        (config, deps)
      }.toSeq

      // Build DAG
      val dag = ProjectDag.fromProjects(projectConfigs)

      // Create diagnostic listener that streams to BSP
      val targetIdByName = targetProjects.map { case (targetId, project) => project.name -> targetId }
      val listener = createProjectDiagnosticListener(targetIdByName, params.originId)

      // Send task start for overall compilation
      val taskId = "compile-all"

      // Create progress listener that sends BSP task events
      val progressListener = new ParallelProjectCompiler.BuildProgressListener {
        def onProjectStarted(projectName: String): IO[Unit] = IO {
          targetIdByName.get(projectName).foreach { targetId =>
            val projectTaskId = s"compile-$projectName"
            sendTaskStartWithTarget(params.originId, projectTaskId, s"Compiling $projectName...", targetId)
          }
        }
        def onProjectFinished(projectName: String, result: ProjectCompileResult): IO[Unit] = IO {
          targetIdByName.get(projectName).foreach { targetId =>
            val projectTaskId = s"compile-$projectName"
            val status = if (result.isSuccess) StatusCode.Ok else StatusCode.Error
            sendTaskFinishWithTarget(params.originId, projectTaskId, status, targetId)
          }
        }
      }

      for {
        _ <- IO(sendTaskStart(params.originId, taskId, s"Compiling ${projectConfigs.size} project(s)..."))

        // Use ParallelProjectCompiler
        buildResult <- ParallelProjectCompiler.build(
          dag,
          parallelism = Runtime.getRuntime.availableProcessors.max(4),
          listener,
          cancellation,
          progressListener
        )

        statusCode = buildResult match {
          case ParallelProjectCompiler.BuildSuccess(_) =>
            StatusCode.Ok
          case ParallelProjectCompiler.BuildFailure(_, failures, notStarted) =>
            // Report errors
            failures.foreach { case (projectName, failure) =>
              failure.errors.foreach { error =>
                sendLogMessage(s"[$projectName] ${error.formatted}", MessageType.Error)
              }
            }
            if notStarted.nonEmpty then sendLogMessage(s"Not started due to failures: ${notStarted.mkString(", ")}", MessageType.Warning)

            if cancellation.isCancelled then StatusCode.Cancelled
            else StatusCode.Error
        }

        _ <- IO(sendTaskFinish(params.originId, taskId, statusCode))
      } yield CompileResult(
        originId = params.originId,
        statusCode = statusCode,
        dataKind = None,
        data = None
      )
    }
  }

  /** Convert old LanguageConfig to new ProjectLanguage */
  private def convertToProjectLanguage(config: LanguageConfig): ProjectLanguage = config match {
    case sc: ScalaConfig =>
      ProjectLanguage.ScalaJava(sc.version, sc.options, None) // ScalaConfig doesn't have Java release
    case kc: KotlinConfig =>
      ProjectLanguage.Kotlin(kc.version, kc.jvmTarget, kc.options)
    case jc: JavaConfig =>
      ProjectLanguage.JavaOnly(jc.release, jc.options, jc.ecjVersion)
  }

  /** Create a diagnostic listener for project-level compilation */
  private def createProjectDiagnosticListener(
      targetIdByName: Map[String, BuildTargetIdentifier],
      originId: Option[String]
  ): DiagnosticListener =
    new DiagnosticListener {
      override def onDiagnostic(error: CompilerError): Unit = {
        val severity = error.severity match {
          case CompilerError.Severity.Error   => DiagnosticSeverity.Error
          case CompilerError.Severity.Warning => DiagnosticSeverity.Warning
          case CompilerError.Severity.Info    => DiagnosticSeverity.Information
        }

        val diagnostic = Diagnostic(
          range = Range(
            start = Position(line = math.max(0, error.line - 1), character = math.max(0, error.column - 1)),
            end = Position(line = math.max(0, error.line - 1), character = error.column)
          ),
          severity = Some(severity),
          code = None,
          codeDescription = None,
          source = Some("bsp"),
          message = error.message,
          tags = None,
          relatedInformation = None,
          dataKind = None,
          data = None
        )

        // Find target ID from source path
        val targetId = error.path
          .flatMap { srcPath =>
            buildLoader.findTargetForSource(srcPath)
          }
          .orElse(targetIdByName.values.headOption)

        val docUri = error.path match {
          case Some(p) => Uri(workspaceRoot.resolve(p).toUri)
          case None    => Uri(java.net.URI.create("file:///unknown"))
        }

        targetId.foreach { tId =>
          sendDiagnostics(
            TextDocumentIdentifier(uri = docUri),
            tId,
            List(diagnostic),
            reset = false
          )
        }
      }

      override def onCompilationReason(projectName: String, reason: CompilationReason): Unit = {
        val message = reason.formatted(projectName)
        val messageType = reason match {
          case CompilationReason.UpToDate => MessageType.Info
          case _                          => MessageType.Log
        }
        sendLogMessage(message, messageType)
      }
    }

  // ==========================================================================
  // Run/Test handlers
  // ==========================================================================

  private def handleRun(params: RunParams, cancellation: CancellationToken): RunResult = {
    val targetId = params.target
    val taskId = s"run-${extractTargetName(targetId)}"

    sendTaskStart(params.originId, taskId, s"Running ${extractTargetName(targetId)}")

    if (cancellation.isCancelled) {
      sendTaskFinish(params.originId, taskId, StatusCode.Cancelled)
      return RunResult(originId = params.originId, statusCode = StatusCode.Cancelled)
    }

    try
      buildLoader.getProject(targetId) match {
        case Some(project) =>
          // Build classpath including output directory
          val classpath = (project.outputDir :: project.classpath).map(_.toString).mkString(java.io.File.pathSeparator)

          // Get main class from params or project config
          val mainClass = params.dataKind match {
            case Some("scala-main-class") =>
              params.data.flatMap { raw =>
                try {
                  val scalaMainClass = readFromArray[ScalaMainClass](raw.value)(using ScalaMainClass.codec)
                  Some(scalaMainClass.className)
                } catch case _: Exception => None
              }
            case _ => None
          }

          mainClass match {
            case Some(main) =>
              val javaHome = System.getProperty("java.home")
              val javaBin = Path.of(javaHome, "bin", "java").toString
              val args = params.arguments.getOrElse(List.empty)
              val command: List[String] = List(javaBin, "-cp", classpath, main) ++ args

              val processBuilder = new ProcessBuilder(command.asJava)
              processBuilder.directory(workspaceRoot.toFile)
              processBuilder.redirectErrorStream(true)

              val process = processBuilder.start()

              // Kill the process when cancellation is requested — destroyForcibly
              // closes the streams, so readLine returns null and waitFor returns immediately.
              cancellation.onCancel { () =>
                process.destroyForcibly()
              }

              // Stream output to log messages with proper resource cleanup
              val reader = new java.io.BufferedReader(new java.io.InputStreamReader(process.getInputStream))
              try {
                var line: String = null
                while { line = reader.readLine(); line != null } do sendLogMessage(line, MessageType.Log)
              } finally
                try reader.close()
                catch { case _: Exception => () }

              val exitCode = process.waitFor()

              val status =
                if (cancellation.isCancelled) StatusCode.Cancelled
                else if (exitCode == 0) StatusCode.Ok
                else StatusCode.Error
              sendTaskFinish(params.originId, taskId, status)
              RunResult(originId = params.originId, statusCode = status)

            case None =>
              sendLogMessage("No main class specified", MessageType.Error)
              sendTaskFinish(params.originId, taskId, StatusCode.Error)
              RunResult(originId = params.originId, statusCode = StatusCode.Error)
          }

        case None =>
          sendLogMessage(s"Unknown target: $targetId", MessageType.Error)
          sendTaskFinish(params.originId, taskId, StatusCode.Error)
          RunResult(originId = params.originId, statusCode = StatusCode.Error)
      }
    catch {
      case e: Exception =>
        sendLogMessage(s"Run failed: ${e.getMessage}", MessageType.Error)
        sendTaskFinish(params.originId, taskId, StatusCode.Error)
        RunResult(originId = params.originId, statusCode = StatusCode.Error)
    }
  }

  private def handleTest(params: TestParams): TestResult = {
    var hasErrors = false
    val taskId = "test-all"

    sendTaskStart(params.originId, taskId, "Running tests")

    try {
      params.targets.foreach { targetId =>
        buildLoader.getProject(targetId) match {
          case Some(project) if project.isTest =>
            val projectTaskId = s"test-${project.name}"
            sendTaskStart(params.originId, projectTaskId, s"Testing ${project.name}")

            val cancellation = activeRequests.get(taskId) match {
              case null => CancellationToken.never
              case ct   => ct
            }

            val testSucceeded = project.platform match {
              case BuildLoader.Platform.Jvm =>
                runJvmTests(project, params, cancellation)
              case BuildLoader.Platform.ScalaJs(sjsVersion, scalaVersion) =>
                runScalaJsTests(project, sjsVersion, scalaVersion, cancellation)
              case BuildLoader.Platform.ScalaNative(snVersion, scalaVersion) =>
                runScalaNativeTests(project, snVersion, scalaVersion, cancellation)
              case BuildLoader.Platform.KotlinJs(kotlinVersion) =>
                runKotlinJsTests(project, cancellation)
              case BuildLoader.Platform.KotlinNative(kotlinVersion) =>
                runKotlinNativeTests(project, cancellation)
            }

            if (!testSucceeded) hasErrors = true
            sendTaskFinish(params.originId, projectTaskId, if testSucceeded then StatusCode.Ok else StatusCode.Error)

          case Some(_) =>
            // Not a test project, skip
            ()

          case None =>
            sendLogMessage(s"Unknown target: $targetId", MessageType.Warning)
        }
      }

      sendTaskFinish(params.originId, taskId, if hasErrors then StatusCode.Error else StatusCode.Ok)
      TestResult(
        originId = params.originId,
        statusCode = if hasErrors then StatusCode.Error else StatusCode.Ok,
        dataKind = None,
        data = None
      )
    } catch {
      case e: Exception =>
        sendLogMessage(s"Test failed: ${e.getMessage}", MessageType.Error)
        sendTaskFinish(params.originId, taskId, StatusCode.Error)
        TestResult(
          originId = params.originId,
          statusCode = StatusCode.Error,
          dataKind = None,
          data = None
        )
    }
  }

  /** Run JVM tests using detected test framework. */
  private def runJvmTests(
      project: buildLoader.ProjectInfo,
      params: TestParams,
      cancellation: CancellationToken
  ): Boolean = {
    val classpath = (project.outputDir :: project.classpath).map(_.toString).mkString(java.io.File.pathSeparator)

    // Get test classes from params or discover them
    val testClasses: List[String] = params.dataKind match {
      case Some("scala-test") =>
        params.data
          .flatMap { raw =>
            try {
              val scalaTestParams = readFromArray[ScalaTestParams](raw.value)(using ScalaTestParams.codec)
              Some(scalaTestParams.testClasses.toList.flatten.flatMap(_.classes))
            } catch case _: Exception => None
          }
          .getOrElse(List.empty)
      case _ => List.empty
    }

    // Detect test framework from classpath
    val frameworks = ClasspathTestDiscovery.detectFrameworks(project.classpath)
    val (testRunnerClass, testArgs) = if (frameworks.contains("MUnit")) {
      ("munit.internal.jvm.MUnitRunner", List("-R", project.outputDir.toString))
    } else if (frameworks.contains("ScalaTest")) {
      val args =
        if (testClasses.nonEmpty) testClasses.flatMap(c => List("-s", c))
        else List("-R", project.outputDir.toString)
      ("org.scalatest.tools.Runner", args ++ List("-oD"))
    } else if (frameworks.contains("JUnit Jupiter") || frameworks.contains("JUnit 4") || frameworks.contains("JUnit Platform")) {
      ("org.junit.runner.JUnitCore", testClasses)
    } else if (frameworks.contains("uTest")) {
      ("utest.runner.Framework", List(project.outputDir.toString))
    } else {
      // Fallback: try ScalaTest runner
      val args =
        if (testClasses.nonEmpty) testClasses.flatMap(c => List("-s", c))
        else List("-R", project.outputDir.toString)
      ("org.scalatest.tools.Runner", args ++ List("-oD"))
    }

    val javaHome = System.getProperty("java.home")
    val javaBin = Path.of(javaHome, "bin", "java").toString
    val command: List[String] = List(javaBin, "-cp", classpath, testRunnerClass) ++ testArgs
    val processBuilder = new ProcessBuilder(command.asJava)
    processBuilder.directory(workspaceRoot.toFile)
    processBuilder.redirectErrorStream(true)

    try {
      val process = processBuilder.start()
      val reader = new java.io.BufferedReader(new java.io.InputStreamReader(process.getInputStream))
      try {
        var line: String = null
        while { line = reader.readLine(); line != null } do sendLogMessage(line, MessageType.Log)
      } finally
        try reader.close()
        catch { case _: Exception => () }

      process.waitFor() == 0
    } catch {
      case e: Exception =>
        sendLogMessage(s"JVM test run failed: ${e.getMessage}", MessageType.Error)
        false
    }
  }

  /** Run Scala.js tests: link → discover → run via Node.js. */
  private def runScalaJsTests(
      project: buildLoader.ProjectInfo,
      sjsVersion: String,
      scalaVersion: String,
      cancellation: CancellationToken
  ): Boolean = {
    val logger = LinkExecutor.LinkLogger.forBsp(this)
    val outputDir = project.outputDir.getParent.resolve("link-output")
    val classpath = project.outputDir +: project.classpath

    // Link
    val linkConfig = ScalaJsLinkConfig.Debug
    val linkTask = TaskDag.LinkTask(
      project = bleep.model.CrossProjectName(bleep.model.ProjectName(project.name), None),
      platform = TaskDag.LinkPlatform.ScalaJs(sjsVersion, scalaVersion, linkConfig),
      releaseMode = false,
      isTest = true
    )

    val program = for {
      killSignal <- Outcome.fromCancellationToken(cancellation)
      linkResult <- LinkExecutor.execute(linkTask, classpath, None, outputDir, logger, killSignal)
      (_, linkOutput) = linkResult
      result <- linkOutput match {
        case TaskDag.LinkResult.JsSuccess(mainModule, _, _, _) =>
          // Discover and run tests
          val frameworks = ClasspathTestDiscovery.detectFrameworks(project.classpath)
          val frameworkNames =
            if (frameworks.contains("MUnit")) Seq("munit.Framework")
            else if (frameworks.contains("ScalaTest")) Seq("org.scalatest.tools.Framework")
            else if (frameworks.contains("uTest")) Seq("utest.runner.Framework")
            else Seq("munit.Framework", "org.scalatest.tools.Framework")

          ScalaJsTestRunner
            .discoverSuites(mainModule, frameworkNames, ScalaJsTestRunner.NodeEnvironment.Node, killSignal)
            .flatMap {
              case ProcessRunner.DiscoveryResult.Failed(message) =>
                IO.delay(sendLogMessage(message, MessageType.Error)).as(false)
              case ProcessRunner.DiscoveryResult.Killed(_) =>
                IO.pure(true)
              case ProcessRunner.DiscoveryResult.Found(suites) =>
                val allSuites = suites.flatMap(_.suites)
                if (allSuites.isEmpty) {
                  IO.delay(sendLogMessage("No test suites discovered", MessageType.Warning)).as(true)
                } else {
                  val eventHandler = new TestRunnerTypes.TestEventHandler {
                    def onTestStarted(suite: String, test: String): Unit =
                      sendLogMessage(s"  $suite > $test STARTED", MessageType.Log)
                    def onTestFinished(suite: String, test: String, status: bleep.bsp.protocol.TestStatus, durationMs: Long, message: Option[String]): Unit =
                      sendLogMessage(s"  $suite > $test ${status} (${durationMs}ms)${message.map(m => s": $m").getOrElse("")}", MessageType.Log)
                    def onSuiteStarted(suite: String): Unit =
                      sendLogMessage(s"Suite: $suite", MessageType.Log)
                    def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit =
                      sendLogMessage(s"  $suite: $passed passed, $failed failed, $skipped skipped", MessageType.Log)
                    def onOutput(suite: String, line: String, channel: bleep.bsp.protocol.OutputChannel): Unit =
                      sendLogMessage(line, if channel.isStderr then MessageType.Error else MessageType.Log)
                  }

                  ScalaJsTestRunner
                    .runTests(mainModule, linkConfig.moduleKind, allSuites, eventHandler, ScalaJsTestRunner.NodeEnvironment.Node, Map.empty, killSignal)
                    .map(_.isSuccess)
                }
            }

        case _ =>
          IO.delay(sendLogMessage("Scala.js linking failed", MessageType.Error)).as(false)
      }
    } yield result

    program.unsafeRunSync()
  }

  /** Run Scala Native tests: link test binary → discover → run. */
  private def runScalaNativeTests(
      project: buildLoader.ProjectInfo,
      snVersion: String,
      scalaVersion: String,
      cancellation: CancellationToken
  ): Boolean = {
    val logger = LinkExecutor.LinkLogger.forBsp(this)
    val outputDir = project.outputDir.getParent.resolve("link-output")
    val classpath = project.outputDir +: project.classpath

    // Detect framework and get test main class
    val framework = ScalaNativeTestRunner.detectFramework(classpath)
    val testMainClass = ScalaNativeTestRunner.getTestMainClass(framework)

    // Link test binary
    val toolchain = ScalaNativeToolchain.forVersion(snVersion, scalaVersion)
    val binaryPath = outputDir.resolve(s"${project.name}-test")
    val workDir = outputDir.resolve("native-work")

    val nativeLogger = new ScalaNativeToolchain.Logger {
      def trace(message: => String): Unit = logger.trace(message)
      def debug(message: => String): Unit = logger.debug(message)
      def info(message: => String): Unit = logger.info(message)
      def warn(message: => String): Unit = logger.warn(message)
      def error(message: => String): Unit = logger.error(message)
      def running(command: Seq[String]): Unit = logger.info(s"Running: ${command.mkString(" ")}")
    }

    val program = for {
      killSignal <- Outcome.fromCancellationToken(cancellation)
      linkResult <- ScalaNativeTestRunner.linkTestBinary(
        toolchain,
        classpath,
        testMainClass,
        ScalaNativeLinkConfig.Debug,
        binaryPath,
        workDir,
        nativeLogger,
        killSignal
      )
      result <- linkResult match {
        case TaskDag.LinkResult.NativeSuccess(binary, _) =>
          val eventHandler = new TestRunnerTypes.TestEventHandler {
            def onTestStarted(suite: String, test: String): Unit =
              sendLogMessage(s"  $suite > $test STARTED", MessageType.Log)
            def onTestFinished(suite: String, test: String, status: bleep.bsp.protocol.TestStatus, durationMs: Long, message: Option[String]): Unit =
              sendLogMessage(s"  $suite > $test ${status} (${durationMs}ms)${message.map(m => s": $m").getOrElse("")}", MessageType.Log)
            def onSuiteStarted(suite: String): Unit =
              sendLogMessage(s"Suite: $suite", MessageType.Log)
            def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit =
              sendLogMessage(s"  $suite: $passed passed, $failed failed, $skipped skipped", MessageType.Log)
            def onOutput(suite: String, line: String, channel: bleep.bsp.protocol.OutputChannel): Unit =
              sendLogMessage(line, if channel.isStderr then MessageType.Error else MessageType.Log)
          }

          // Use TestAdapter protocol (socket-based RPC) for proper communication with the native binary.
          // The TestAdapter from scala-native:test-runner handles framework discovery and test execution.
          ScalaNativeTestRunner
            .runTestsViaAdapter(
              binary,
              List.empty,
              framework,
              eventHandler,
              Map.empty,
              workspaceRoot,
              snVersion,
              killSignal
            )
            .map(_.isSuccess)

        case _ =>
          IO.delay(sendLogMessage("Scala Native test binary linking failed", MessageType.Error)).as(false)
      }
    } yield result

    program.unsafeRunSync()
  }

  /** Run Kotlin/JS tests via Node.js. */
  private def runKotlinJsTests(
      project: buildLoader.ProjectInfo,
      cancellation: CancellationToken
  ): Boolean = {
    val jsOutput = project.outputDir.resolve("js").resolve(s"${project.name}.js")

    if (!Files.exists(jsOutput)) {
      sendLogMessage(s"Kotlin/JS output not found: $jsOutput", MessageType.Error)
      return false
    }

    val program = for {
      killSignal <- Outcome.fromCancellationToken(cancellation)
      result <- KotlinTestRunner.Js.discoverSuites(jsOutput, killSignal).flatMap {
        case ProcessRunner.DiscoveryResult.Failed(message) =>
          IO.delay(sendLogMessage(message, MessageType.Error)).as(false)
        case ProcessRunner.DiscoveryResult.Killed(_) =>
          IO.pure(true)
        case ProcessRunner.DiscoveryResult.Found(suites) =>
          if (suites.isEmpty) {
            IO.delay(sendLogMessage("No Kotlin/JS test suites discovered", MessageType.Warning)).as(true)
          } else {
            val eventHandler = new TestRunnerTypes.TestEventHandler {
              def onTestStarted(suite: String, test: String): Unit =
                sendLogMessage(s"  $suite > $test STARTED", MessageType.Log)
              def onTestFinished(suite: String, test: String, status: bleep.bsp.protocol.TestStatus, durationMs: Long, message: Option[String]): Unit =
                sendLogMessage(s"  $suite > $test ${status} (${durationMs}ms)${message.map(m => s": $m").getOrElse("")}", MessageType.Log)
              def onSuiteStarted(suite: String): Unit =
                sendLogMessage(s"Suite: $suite", MessageType.Log)
              def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit =
                sendLogMessage(s"  $suite: $passed passed, $failed failed, $skipped skipped", MessageType.Log)
              def onOutput(suite: String, line: String, channel: bleep.bsp.protocol.OutputChannel): Unit =
                sendLogMessage(line, if channel.isStderr then MessageType.Error else MessageType.Log)
            }

            KotlinTestRunner.Js.runTests(jsOutput, suites, eventHandler, Map.empty, killSignal).map(_.isSuccess)
          }
      }
    } yield result

    program.unsafeRunSync()
  }

  /** Run Kotlin/Native tests via native binary. */
  private def runKotlinNativeTests(
      project: buildLoader.ProjectInfo,
      cancellation: CancellationToken
  ): Boolean = {
    val binary = project.outputDir.resolve(project.name)

    if (!Files.exists(binary)) {
      sendLogMessage(s"Kotlin/Native binary not found: $binary", MessageType.Error)
      return false
    }

    val program = for {
      killSignal <- Outcome.fromCancellationToken(cancellation)
      result <- KotlinTestRunner.Native.discoverSuites(binary, killSignal).flatMap {
        case ProcessRunner.DiscoveryResult.Failed(message) =>
          IO.delay(sendLogMessage(message, MessageType.Error)).as(false)
        case ProcessRunner.DiscoveryResult.Killed(_) =>
          IO.pure(true)
        case ProcessRunner.DiscoveryResult.Found(suites) =>
          if (suites.isEmpty) {
            IO.delay(sendLogMessage("No Kotlin/Native test suites discovered", MessageType.Warning)).as(true)
          } else {
            val eventHandler = new TestRunnerTypes.TestEventHandler {
              def onTestStarted(suite: String, test: String): Unit =
                sendLogMessage(s"  $suite > $test STARTED", MessageType.Log)
              def onTestFinished(suite: String, test: String, status: bleep.bsp.protocol.TestStatus, durationMs: Long, message: Option[String]): Unit =
                sendLogMessage(s"  $suite > $test ${status} (${durationMs}ms)${message.map(m => s": $m").getOrElse("")}", MessageType.Log)
              def onSuiteStarted(suite: String): Unit =
                sendLogMessage(s"Suite: $suite", MessageType.Log)
              def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit =
                sendLogMessage(s"  $suite: $passed passed, $failed failed, $skipped skipped", MessageType.Log)
              def onOutput(suite: String, line: String, channel: bleep.bsp.protocol.OutputChannel): Unit =
                sendLogMessage(line, if channel.isStderr then MessageType.Error else MessageType.Log)
            }

            KotlinTestRunner.Native.runTests(binary, suites, eventHandler, Map.empty, workspaceRoot, killSignal).map(_.isSuccess)
          }
      }
    } yield result

    program.unsafeRunSync()
  }

  private def extractTargetName(targetId: BuildTargetIdentifier): String =
    targetId.uri.value.split("\\?id=").lastOption.getOrElse("unknown")

  // ==========================================================================
  // JVM handlers
  // ==========================================================================

  private def handleJvmRunEnvironment(params: JvmRunEnvironmentParams): JvmRunEnvironmentResult = {
    val items = params.targets.map { targetId =>
      val classpath = buildLoader.getClasspathStrings(targetId)
      val outputPath = buildLoader.getProject(targetId).map(_.outputDir.toUri.toString).toList
      JvmEnvironmentItem(
        target = targetId,
        classpath = outputPath ++ classpath,
        jvmOptions = List.empty,
        workingDirectory = workspaceRoot.toString,
        environmentVariables = Map.empty,
        mainClasses = None
      )
    }
    JvmRunEnvironmentResult(items)
  }

  private def handleJvmTestEnvironment(params: JvmTestEnvironmentParams): JvmTestEnvironmentResult = {
    val items = params.targets.map { targetId =>
      val classpath = buildLoader.getClasspathStrings(targetId)
      val outputPath = buildLoader.getProject(targetId).map(_.outputDir.toUri.toString).toList
      JvmEnvironmentItem(
        target = targetId,
        classpath = outputPath ++ classpath,
        jvmOptions = List.empty,
        workingDirectory = workspaceRoot.toString,
        environmentVariables = Map.empty,
        mainClasses = None
      )
    }
    JvmTestEnvironmentResult(items)
  }

  private def handleJvmCompileClasspath(params: JvmCompileClasspathParams): JvmCompileClasspathResult = {
    val items = params.targets.map { targetId =>
      JvmCompileClasspathItem(
        target = targetId,
        classpath = buildLoader.getClasspathStrings(targetId)
      )
    }
    JvmCompileClasspathResult(items)
  }

  // ==========================================================================
  // Language-specific handlers
  // ==========================================================================

  private def handleScalacOptions(params: ScalacOptionsParams): ScalacOptionsResult = {
    val items = params.targets.map { targetId =>
      buildLoader.getProject(targetId) match {
        case Some(project) =>
          val options = project.languageConfig match {
            case sc: ScalaConfig => sc.options
            case _               => List.empty
          }
          ScalacOptionsItem(
            target = targetId,
            options = options,
            classpath = project.classpath.map(_.toUri.toString),
            classDirectory = project.outputDir.toUri.toString
          )
        case None =>
          ScalacOptionsItem(
            target = targetId,
            options = List.empty,
            classpath = List.empty,
            classDirectory = ""
          )
      }
    }
    ScalacOptionsResult(items)
  }

  private def handleJavacOptions(params: JavacOptionsParams): JavacOptionsResult = {
    val items = params.targets.map { targetId =>
      buildLoader.getProject(targetId) match {
        case Some(project) =>
          val options = project.languageConfig match {
            case jc: JavaConfig => jc.options ++ jc.release.map(r => List("--release", r.toString)).getOrElse(Nil)
            case _              => List.empty
          }
          JavacOptionsItem(
            target = targetId,
            options = options,
            classpath = project.classpath.map(_.toUri.toString),
            classDirectory = project.outputDir.toUri.toString
          )
        case None =>
          JavacOptionsItem(
            target = targetId,
            options = List.empty,
            classpath = List.empty,
            classDirectory = ""
          )
      }
    }
    JavacOptionsResult(items)
  }

  // ==========================================================================
  // Notification helpers
  // ==========================================================================

  /** Send a task start notification */
  private def sendTaskStart(originId: Option[String], taskId: String, message: String): Unit = {
    val params = TaskStartParams(
      taskId = TaskId(taskId, None),
      originId = originId,
      eventTime = Some(System.currentTimeMillis()),
      message = Some(message),
      dataKind = None,
      data = None
    )
    sendNotification("build/taskStart", params)(using TaskStartParams.codec)
  }

  /** Send a task start notification with build target */
  private def sendTaskStartWithTarget(originId: Option[String], taskId: String, message: String, targetId: BuildTargetIdentifier): Unit = {
    // Create compile-task data with target info (what bloop sends)
    val data = RawJson(s"""{"target":{"uri":"${targetId.uri}"}}""")
    val params = TaskStartParams(
      taskId = TaskId(taskId, None),
      originId = originId,
      eventTime = Some(System.currentTimeMillis()),
      message = Some(message),
      dataKind = Some("compile-task"),
      data = Some(data)
    )
    sendNotification("build/taskStart", params)(using TaskStartParams.codec)
  }

  /** Send a task finish notification */
  private def sendTaskFinish(originId: Option[String], taskId: String, status: StatusCode): Unit = {
    val params = TaskFinishParams(
      taskId = TaskId(taskId, None),
      originId = originId,
      eventTime = Some(System.currentTimeMillis()),
      message = None,
      status = status,
      dataKind = None,
      data = None
    )
    sendNotification("build/taskFinish", params)(using TaskFinishParams.codec)
  }

  /** Send a task finish notification with build target */
  private def sendTaskFinishWithTarget(originId: Option[String], taskId: String, status: StatusCode, targetId: BuildTargetIdentifier): Unit = {
    // Create compile-report data with target info (what bloop sends)
    val data = RawJson(s"""{"target":{"uri":"${targetId.uri}"}}""")
    val params = TaskFinishParams(
      taskId = TaskId(taskId, None),
      originId = originId,
      eventTime = Some(System.currentTimeMillis()),
      message = None,
      status = status,
      dataKind = Some("compile-report"),
      data = Some(data)
    )
    sendNotification("build/taskFinish", params)(using TaskFinishParams.codec)
  }

  /** Send a log message notification */
  def sendLogMessage(message: String, messageType: MessageType): Unit = {
    val params = LogMessageParams(
      `type` = messageType,
      task = None,
      originId = None,
      message = message
    )
    sendNotification("build/logMessage", params)(using LogMessageParams.codec)
  }

  /** Send a publish diagnostics notification */
  def sendDiagnostics(
      textDocument: TextDocumentIdentifier,
      buildTarget: BuildTargetIdentifier,
      diagnostics: List[Diagnostic],
      reset: Boolean
  ): Unit = {
    val params = PublishDiagnosticsParams(
      textDocument = textDocument,
      buildTarget = buildTarget,
      originId = None,
      diagnostics = diagnostics,
      reset = reset
    )
    sendNotification("build/publishDiagnostics", params)(using PublishDiagnosticsParams.codec)
  }

  private def sendNotification[T](method: String, params: T)(using codec: JsonValueCodec[T]): Unit = {
    val notification = JsonRpcNotification(
      jsonrpc = "2.0",
      method = method,
      params = Some(RawJson(writeToArray(params)))
    )
    transport.sendNotification(notification)
  }

  // ==========================================================================
  // JSON helpers
  // ==========================================================================

  private def parseParams[T](params: Option[RawJson])(using codec: JsonValueCodec[T]): T =
    params match {
      case Some(raw) => readFromArray[T](raw.value)
      case None      => throw BspException(JsonRpcErrorCodes.InvalidParams, "Missing params")
    }

  private def toRaw[T](value: T)(using codec: JsonValueCodec[T]): RawJson =
    RawJson(writeToArray(value))

  // ==========================================================================
  // Compilation helpers
  // ==========================================================================

  /** Collect source files from a directory */
  private def collectSourceFiles(dir: Path, config: LanguageConfig): Seq[SourceFile] = {
    if !Files.isDirectory(dir) then return Seq.empty

    import scala.jdk.StreamConverters.*
    import scala.util.Using
    val extensions = config match {
      case _: ScalaConfig  => Set(".scala", ".java")
      case _: KotlinConfig => Set(".kt", ".kts", ".java")
      case _: JavaConfig   => Set(".java")
    }

    // Use Using to ensure Files.walk stream is properly closed
    Using(Files.walk(dir)) { stream =>
      stream
        .toScala(Seq)
        .filter { path =>
          Files.isRegularFile(path) && extensions.exists(path.toString.endsWith)
        }
        .map { path =>
          val content = Files.readString(path)
          val relPath = workspaceRoot.relativize(path)
          SourceFile(relPath, content)
        }
    }.getOrElse(Seq.empty)
  }

  /** Create a diagnostic listener that streams to BSP */
  private def createDiagnosticListener(
      targetId: BuildTargetIdentifier,
      originId: Option[String]
  ): DiagnosticListener =
    new DiagnosticListener {
      override def onDiagnostic(error: CompilerError): Unit = {
        val severity = error.severity match {
          case CompilerError.Severity.Error   => DiagnosticSeverity.Error
          case CompilerError.Severity.Warning => DiagnosticSeverity.Warning
          case CompilerError.Severity.Info    => DiagnosticSeverity.Information
        }

        val diagnostic = Diagnostic(
          range = Range(
            start = Position(line = math.max(0, error.line - 1), character = math.max(0, error.column - 1)),
            end = Position(line = math.max(0, error.line - 1), character = error.column)
          ),
          severity = Some(severity),
          code = None,
          codeDescription = None,
          source = Some("bsp"),
          message = error.message,
          tags = None,
          relatedInformation = None,
          dataKind = None,
          data = None
        )

        val docUri = error.path match {
          case Some(p) => Uri(workspaceRoot.resolve(p).toUri)
          case None    => Uri(java.net.URI.create("file:///unknown"))
        }

        sendDiagnostics(
          TextDocumentIdentifier(uri = docUri),
          targetId,
          List(diagnostic),
          reset = false
        )
      }
    }

  /** Parse Maven artifact coordinates from JAR path */
  private def parseMavenArtifact(jarPath: Path): Option[(String, String, String)] = {
    // Try to parse from typical Maven/Ivy cache path structure
    // e.g., ~/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.3.3/scala3-library_3-3.3.3.jar
    val pathStr = jarPath.toString
    val jarName = jarPath.getFileName.toString

    // Simple heuristic: parse from JAR filename pattern name-version.jar
    val pattern = "^(.+)-([0-9][^-]*)\\.jar$".r
    jarName match {
      case pattern(name, version) =>
        // Try to extract org from path
        val org = pathStr.split("/").reverse.drop(3).headOption.getOrElse("unknown")
        Some((org, name, version))
      case _ =>
        None
    }
  }
}

/** Exception for BSP errors */
class BspException(val code: Int, message: String) extends RuntimeException(message)

/** Build state for a single active build */
final case class BuildState(
    targets: List[BuildTarget]
)

object BspServer {

  /** Run a BSP server on stdin/stdout */
  def runStdio(workspaceRoot: Path): Unit = {
    val server = new BspServer(System.in, System.out, workspaceRoot)
    server.run()
  }

  /** Run a BSP server on stdin/stdout, detecting workspace from current directory */
  def runStdio(): Unit =
    runStdio(Path.of(".").toAbsolutePath.normalize())
}
