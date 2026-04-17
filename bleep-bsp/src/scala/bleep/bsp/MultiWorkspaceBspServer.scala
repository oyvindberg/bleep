package bleep.bsp

import bleep.*
import bleep.analysis.{
  CancellationToken,
  CompilationReason,
  CompilePhase,
  CompilerError,
  DiagnosticListener,
  ParallelProjectCompiler,
  ProgressListener,
  ProjectCompileFailure,
  ProjectCompileSuccess,
  ProjectCompiler,
  ProjectLanguage,
  ScalaJsLinkConfig,
  ScalaNativeLinkConfig,
  ScalaNativeToolchain,
  ZincBridge
}
import bleep.bsp.Outcome.KillReason
import bleep.bsp.protocol.{BleepBspProtocol, CompileReason, CompileStatus, LinkPlatformName, OutputChannel, ProcessExit}
import bleep.bsp.TraceCategory
import bleep.model.{CrossProjectName, SuiteName, TestName}
import bleep.testing.JvmPool
import cats.effect.{Deferred, FiberIO, IO, Ref}
import cats.effect.std.{Dispatcher, Queue}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import ch.epfl.scala.bsp.*
import com.github.plokhotnyuk.jsoniter_scala.core.*
import io.circe.parser.{decode => circeDecode}
import ryddig.Logger
import scala.collection.immutable.SortedSet

import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

import java.io.{InputStream, OutputStream}
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}
import scala.jdk.CollectionConverters.*

/** Multi-workspace BSP server that can handle requests for multiple workspaces.
  *
  * Workspaces are identified by their rootUri in the initialize request. Uses bleep-core's bootstrap infrastructure for build loading.
  */
class MultiWorkspaceBspServer(
    in: InputStream,
    out: OutputStream,
    logger: Logger,
    socketDir: Option[Path],
    compileSemaphore: java.util.concurrent.Semaphore,
    heapMonitor: HeapMonitor
) {
  import MultiWorkspaceBspServer.DebugLogging

  private val transport = new JsonRpcTransport(in, out)
  private val initialized = AtomicBoolean(false)
  private val shutdownRequested = AtomicBoolean(false)

  /** Track active compile count so heap pressure back-pressure can skip stalling when we're the only compile. */
  private val activeCompileCount = new java.util.concurrent.atomic.AtomicInteger(0)

  /** Pre-allocated emergency memory that gets released during OOM to allow sending error response. */
  @volatile private var emergencyMemory: Array[Byte] = new Array[Byte](1024 * 1024) // 1MB reserve
  private val clientCapabilities = AtomicReference[Option[BuildClientCapabilities]](None)

  /** The active workspace for this connection (set during initialize) */
  private val activeWorkspace = AtomicReference[Option[Path]](None)

  /** The active build variant (set during initialize) */
  private val activeVariant = AtomicReference[model.BuildVariant](model.BuildVariant.Normal)

  /** Parsed build data from initialize, if provided by bleep client */
  private val providedBuild = AtomicReference[Option[model.Build.Exploded]](None)

  /** Classpath overrides from client (used by ReplaceBleepDependencies) */
  private val providedClasspathOverrides = AtomicReference[Map[model.CrossProjectName, List[Path]]](Map.empty)

  /** Active build rewrites (set during initialize, applied on build load/reload) */
  private val activeRewrites = AtomicReference[List[bleep.rewrites.BuildRewrite]](Nil)

  /** Whether the connected client is an IDE (Metals, IntelliJ) — set during initialize */
  private val ideClient = AtomicBoolean(false)

  /** Resolved path to com.sourcegraph:semanticdb-javac JAR for Java semanticdb support (set during initialize for IDE clients) */
  private val javaSemanticdbPlugin = AtomicReference[Option[Path]](None)

  /** Per-workspace loaded builds (using bleep-core's Started) */
  private val loadedBuilds = ConcurrentHashMap[Path, Started]()

  /** Build load error (set during initialize if build fails to load) */
  private val buildLoadError = AtomicReference[Option[String]](None)

  /** Active requests and their cancellation tokens */
  private val activeRequests = ConcurrentHashMap[String, CancellationToken]()

  /** Active request fibers for cancellation */
  private val activeFibers = ConcurrentHashMap[String, FiberIO[Unit]]()

  /** Lock timeout for write operations */
  private val lockTimeout = 5.minutes

  /** Operation IDs registered by this connection (for cleanup on disconnect) */
  private val myOperationIds = ConcurrentHashMap.newKeySet[String]()

  /** Tracks diagnostic state across compilation cycles for the BSP reset protocol (issue #526). */
  private val diagnosticTracker = BspDiagnosticTracker()

  /** Run the server message loop with concurrent request handling.
    *
    * Notifications (like $/cancelRequest) are processed immediately. Requests are spawned in background fibers so the main loop stays responsive.
    */
  def run(): Unit = {
    val program = runConcurrent
      .onError { err =>
        IO.delay(logger.withContext("error", err.getClass.getName).error(s"Message loop failed: ${err.getMessage}", err))
      }
      .guarantee(
        // CRITICAL: Use uncancelable to ensure cleanup completes
        IO.uncancelable { _ =>
          // Cleanup on exit - cancel all active requests (kills child processes) then cancel fibers
          IO.delay(logger.warn("Server run() exiting - cleaning up")) >>
            // Unregister operations belonging to this connection
            IO.delay {
              activeWorkspace.get().foreach { ws =>
                SharedWorkspaceState.unregisterAll(ws, myOperationIds.asScala)
                myOperationIds.clear()
              }
            } >>
            IO.delay(cancelAllActiveRequests()) >>
            IO.blocking {
              val fibers = activeFibers.values().asScala.toList
              activeFibers.clear()
              fibers
            }.flatMap { fibers =>
              // Cancel all fibers and wait for them to complete (this ensures Resource finalizers run)
              fibers.traverse_(_.cancel)
            } >> ProjectLock.releaseAll()
        }
      )
    program.unsafeRunSync()
  }

  private def runConcurrent: IO[Unit] = {
    def loop: IO[Unit] =
      IO.blocking(transport.readMessage()).flatMap {
        case Some(request) =>
          processMessage(request) >> loop
        case None =>
          // Stream closed
          IO.delay(logger.info("Transport stream closed (client disconnected)"))
      }

    loop
  }

  /** Process a single message - notifications immediately, requests in background */
  private def processMessage(request: JsonRpcRequest): IO[Unit] = {
    // Check if this is a notification (no id) or a request
    val isNotification = request.id.isEmpty

    // These methods should be handled immediately (not spawned)
    val immediatelyHandled = Set(
      "$/cancelRequest",
      "bleep/cancelBlockingWork",
      "build/initialize",
      "build/initialized",
      "build/shutdown",
      "build/exit"
    )

    if (isNotification || immediatelyHandled.contains(request.method)) {
      // Handle immediately in the main loop
      IO.blocking(handleRequestSync(request))
    } else {
      // Spawn in background fiber
      spawnRequest(request)
    }
  }

  /** Spawn a request in a background fiber. The fiber is fire-and-forget: we do NOT join it here, so the main loop stays responsive and can read subsequent
    * messages (e.g. $/cancelRequest, build/shutdown) while the request runs. The fiber self-cleans from activeFibers via guarantee.
    */
  private def spawnRequest(request: JsonRpcRequest): IO[Unit] = {
    val requestId = request.id.map(_.key).getOrElse("unknown")

    Deferred[IO, Unit].flatMap { registered =>
      val handler: IO[Unit] =
        registered.get >>
          IO.interruptible(handleRequestSync(request))
            .onError { err =>
              IO.delay(logger.withContext("request", requestId).error(s"Request handler failed: ${err.getClass.getName}: ${err.getMessage}", err))
            }
            .guarantee(IO.blocking(activeFibers.remove(requestId)).void)

      handler.start.flatMap { fiber =>
        IO.blocking(activeFibers.put(requestId, fiber)) >> registered.complete(()).void
      }
    }
  }

  /** Handle a request synchronously (called from fiber or main loop) */
  private def handleRequestSync(request: JsonRpcRequest): Unit = {
    val cancellationToken = request.id match {
      case Some(id) =>
        val token = CancellationToken.create()
        activeRequests.put(id.key, token)
        token
      case None =>
        CancellationToken.never
    }

    try {
      val result = dispatch(request.method, request.params, cancellationToken)

      request.id match {
        case Some(id) =>
          activeRequests.remove(id.key)
          val response = JsonRpcResponse(
            jsonrpc = "2.0",
            id = id,
            result = result,
            error = None
          )
          transport.sendResponse(response)
        case None =>
          ()
      }
    } catch {
      case _: InterruptedException =>
        // Fiber was cancelled (client disconnected). Re-set interrupt flag so CE detects it,
        // clean up the request token, and return — no error response needed since the client is gone.
        Thread.currentThread().interrupt()
        request.id.foreach(id => activeRequests.remove(id.key))
      case oom: OutOfMemoryError =>
        handleOutOfMemory(request, oom)
      case e: BspException =>
        request.id.foreach { id =>
          activeRequests.remove(id.key)
          trySendResponse(id, None, Some(JsonRpcError(e.code, e.getMessage, None)))
        }
      case e: Exception =>
        val msg = Option(e.getMessage).getOrElse(e.getClass.getName)
        System.err.println(s"[BSP] Error handling ${request.method}: $msg")
        e.printStackTrace(System.err)
        request.id.foreach { id =>
          activeRequests.remove(id.key)
          trySendResponse(
            id,
            None,
            Some(JsonRpcError(JsonRpcErrorCodes.InternalError, msg, None))
          )
        }
    }
  }

  /** Try to send a JSON-RPC response. If the transport is dead (broken pipe), log and move on.
    *
    * This prevents cascading failures where a dead client connection takes down the server because sending the error response also throws.
    */
  private def trySendResponse(id: RpcId, result: Option[RawJson], error: Option[JsonRpcError]): Unit =
    try
      transport.sendResponse(JsonRpcResponse(jsonrpc = "2.0", id = id, result = result, error = error))
    catch {
      case e: java.io.IOException =>
        logger.withContext("error", e.getMessage).error("Failed to send response (client disconnected)")
      case e: Exception =>
        logger.withContext("error", e.getMessage).error("Failed to send response", e)
    }

  /** Handle OutOfMemoryError by sending an error response and requesting shutdown.
    *
    * Releases emergency memory to ensure we have enough heap to send the error response, then waits briefly before requesting shutdown.
    */
  private def handleOutOfMemory(request: JsonRpcRequest, oom: OutOfMemoryError): Unit = {
    // Release emergency memory to allow sending error response
    emergencyMemory = null
    System.gc() // Hint to GC to reclaim the emergency memory

    val errorMessage = "BSP server ran out of memory. Server will shutdown."

    // Try to send error response
    try {
      request.id.foreach { id =>
        activeRequests.remove(id.key)
        val response = JsonRpcResponse(
          jsonrpc = "2.0",
          id = id,
          result = None,
          error = Some(
            JsonRpcError(
              JsonRpcErrorCodes.InternalError,
              errorMessage,
              None
            )
          )
        )
        transport.sendResponse(response)
      }

      // Also send a build/showMessage notification to inform the client
      try {
        val showMessageParams = ShowMessageParams(
          `type` = MessageType.Error,
          task = None,
          originId = None,
          message = errorMessage
        )
        val notification = JsonRpcNotification(
          jsonrpc = "2.0",
          method = "build/showMessage",
          params = Some(toRaw(showMessageParams))
        )
        transport.sendNotification(notification)
      } catch {
        case _: Throwable => () // Best effort
      }
    } catch {
      case _: Throwable =>
        // If we can't even send the error, just log and continue to shutdown
        System.err.println(s"OOM: Failed to send error response: $errorMessage")
    }

    // Wait briefly to let the message be sent
    try Thread.sleep(50)
    catch { case _: InterruptedException => () }

    // Request shutdown - this will cause the message loop to exit
    shutdownRequested.set(true)

    // Also exit the JVM since OOM is a fatal condition
    // Use a separate thread to allow this method to return
    new Thread("oom-shutdown") {
      override def run(): Unit = {
        try Thread.sleep(100)
        catch { case _: InterruptedException => () }
        System.err.println("BSP server exiting due to OutOfMemoryError")
        System.exit(1)
      }
    }.start()
  }

  /** Dispatch a method call to the appropriate handler */
  private def dispatch(method: String, params: Option[RawJson], cancellation: CancellationToken): Option[RawJson] = {
    logger.withContext("method", method).withContext("thread", Thread.currentThread().getName).warn("dispatch")
    if !initialized.get() && method != "build/initialize" then
      throw BspException(
        JsonRpcErrorCodes.ServerNotInitialized,
        "Server not initialized"
      )

    // Gate all workspace/buildTarget requests behind a valid, loaded build.
    // Lifecycle methods (initialize, initialized, shutdown, exit, cancel) are exempt.
    val requiresBuild = method.startsWith("workspace/") || method.startsWith("buildTarget/")
    if requiresBuild then {
      getActiveBuild match {
        case Left(msg) =>
          bspError(msg)
          throw BspException(JsonRpcErrorCodes.InternalError, msg)
        case Right(_) => ()
      }
    }

    method match {
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

      case "workspace/buildTargets" =>
        Some(toRaw(handleBuildTargets()))

      case "workspace/reload" =>
        handleReload()
        None

      case "buildTarget/sources" =>
        val p = parseParams[SourcesParams](params)
        Some(toRaw(handleSources(p)))

      case "buildTarget/dependencySources" =>
        val p = parseParams[DependencySourcesParams](params)
        Some(toRaw(handleDependencySources(p)))

      case "buildTarget/compile" =>
        val p = parseParams[CompileParams](params)
        Some(toRaw(handleCompile(p, cancellation)))

      case "buildTarget/scalacOptions" =>
        val p = parseParams[ScalacOptionsParams](params)
        Some(toRaw(handleScalacOptions(p)))

      case "buildTarget/javacOptions" =>
        val p = parseParams[JavacOptionsParams](params)
        Some(toRaw(handleJavacOptions(p)))

      case "buildTarget/jvmRunEnvironment" =>
        val p = parseParams[JvmRunEnvironmentParams](params)
        Some(toRaw(handleJvmRunEnvironment(p)))

      case "buildTarget/jvmTestEnvironment" =>
        val p = parseParams[JvmTestEnvironmentParams](params)
        Some(toRaw(handleJvmTestEnvironment(p)))

      case "buildTarget/resources" =>
        val p = parseParams[ResourcesParams](params)
        Some(toRaw(handleResources(p)))

      case "buildTarget/outputPaths" =>
        val p = parseParams[OutputPathsParams](params)
        Some(toRaw(handleOutputPaths(p)))

      case "buildTarget/inverseSources" =>
        val p = parseParams[InverseSourcesParams](params)
        Some(toRaw(handleInverseSources(p)))

      case "buildTarget/dependencyModules" =>
        val p = parseParams[DependencyModulesParams](params)
        Some(toRaw(handleDependencyModules(p)))

      case "buildTarget/jvmCompileClasspath" =>
        val p = parseParams[JvmCompileClasspathParams](params)
        Some(toRaw(handleJvmCompileClasspath(p)))

      case "buildTarget/cleanCache" =>
        val p = parseParams[CleanCacheParams](params)
        Some(toRaw(handleCleanCache(p)))

      case "buildTarget/scalaMainClasses" =>
        val p = parseParams[ScalaMainClassesParams](params)
        Some(toRaw(handleScalaMainClasses(p)))

      case "buildTarget/scalaTestClasses" =>
        val p = parseParams[ScalaTestClassesParams](params)
        Some(toRaw(handleScalaTestClasses(p)))

      case "buildTarget/run" =>
        throw BspException(
          JsonRpcErrorCodes.MethodNotFound,
          "buildTarget/run is not supported by bleep-bsp"
        )

      case "buildTarget/test" =>
        val p = parseParams[TestParams](params)
        Some(toRaw(handleTest(p, cancellation)))

      case "$/cancelRequest" =>
        // Client may send $/cancelRequest with empty params {} when the
        // CompletableFuture is cancelled after the connection is closing.
        // Tolerate missing id — connection cleanup handles cancellation.
        try {
          val p = parseParams[CancelRequestParams](params)
          handleCancelRequest(p)
        } catch { case _: Exception => () }
        None

      case "bleep/cancelBlockingWork" =>
        activeWorkspace.get().foreach(SharedWorkspaceState.cancelAll)
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

    // Extract workspace root from rootUri (where bleep.yaml is)
    val buildRoot = params.rootUri.toPath

    // Try to parse build data from initialize params (sent by bleep client)
    debugLog(s"Initialize params - dataKind: ${params.dataKind}, data present: ${params.data.isDefined}")
    val parsedPayload: Option[BspBuildData.Payload] =
      params.dataKind.filter(_.contains(BspBuildData.DataKind)).flatMap { dataKind =>
        debugLog(s"DataKind matches: $dataKind")
        try
          params.data match {
            case None =>
              debugLog("params.data is None")
              None
            case Some(rawJson) =>
              debugLog(s"params.data is Some, rawJson class: ${rawJson.getClass.getName}")
              try {
                val jsonStr = new String(rawJson.value, "UTF-8")
                debugLog(s"Raw JSON (first 200 chars): ${jsonStr.take(200)}")
                circeDecode[BspBuildData.Payload](jsonStr) match {
                  case Right(payload) =>
                    debugLog(
                      s"Received rewritten build from client (variant: ${payload.variantName}, classpathOverrides: ${payload.classpathOverrides.size} projects)"
                    )
                    Some(payload)
                  case Left(err) =>
                    bspWarn(s"Failed to parse build data from client: ${err.getMessage}")
                    None
                }
              } catch {
                case e: Throwable =>
                  bspError(s"Exception accessing rawJson.value: ${e.getClass.getName}: ${e.getMessage}")
                  None
              }
          }
        catch {
          case e: Throwable =>
            bspError(s"Exception in data parsing: ${e.getClass.getName}: ${e.getMessage}")
            None
        }
      }

    // Detect IDE clients by name (Metals, IntelliJ, etc.) — NOT by parsedPayload emptiness,
    // since old bleep CLI versions also don't send BspBuildData.Payload.
    val knownIdeClients = Set("Metals", "IntelliJ", "IntelliJ-BSP")
    val isIdeClient = knownIdeClients.contains(params.displayName)

    // For IDE clients (e.g. Metals), extract semanticdbVersion and javaSemanticdbVersion from init data
    val parsedInitData: Option[io.circe.Json] = if (isIdeClient) {
      params.data.flatMap { rawJson =>
        try {
          val jsonStr = new String(rawJson.value, "UTF-8")
          io.circe.parser.parse(jsonStr).toOption
        } catch {
          case e: Throwable =>
            debugLog(s"Failed to parse IDE build params: ${e.getMessage}")
            None
        }
      }
    } else None

    val semanticDbVersionFromIde: Option[String] =
      parsedInitData.flatMap(_.hcursor.get[String]("semanticdbVersion").toOption)
    val javaSemanticDbVersionFromIde: Option[String] =
      parsedInitData.flatMap(_.hcursor.get[String]("javaSemanticdbVersion").toOption)

    ideClient.set(isIdeClient)

    val variant = parsedPayload
      .map(_.variantName)
      .map(model.BuildVariant.fromName)
      .getOrElse(
        if (isIdeClient) model.BuildVariant.BSP else model.BuildVariant.Normal
      )
    providedBuild.set(parsedPayload.map(_.build))
    providedClasspathOverrides.set(parsedPayload.map(_.classpathOverrides).getOrElse(Map.empty))

    // Set up rewrites for IDE clients (SemanticDB support for goto-definition, find-references, etc.)
    val rewrites: List[bleep.rewrites.BuildRewrite] = if (isIdeClient) {
      val sdVersion = semanticDbVersionFromIde.getOrElse("4.15.2")
      logger.info(s"IDE client '${params.displayName}' detected, applying semanticDb rewrite with version $sdVersion")
      List(new bleep.rewrites.semanticDb(sdVersion))
    } else Nil
    activeRewrites.set(rewrites)

    // Resolve Java semanticdb plugin for IDE clients
    if (isIdeClient) {
      val javaSDVersion = javaSemanticDbVersionFromIde.getOrElse("0.10.0")
      logger.info(s"Resolving Java semanticdb plugin: com.sourcegraph:semanticdb-javac:$javaSDVersion")
      resolveJavaSemanticdbPlugin(javaSDVersion)
    }

    activeWorkspace.set(Some(buildRoot))
    activeVariant.set(variant)

    // Load or use provided build
    val buildResult = providedBuild.get() match {
      case Some(exploded) =>
        // Use the provided build directly - no need to load from disk
        createStartedFromExplodedBuild(buildRoot, variant, exploded)
      case None =>
        // Fallback: load from disk (for IDE clients that don't pass build data)
        loadBuild(buildRoot, variant)
    }

    buildResult match {
      case Right(started) =>
        buildLoadError.set(None)
        logger
          .withContext("projects", started.build.explodedProjects.size)
          .withContext("workspace", buildRoot.toString)
          .withContext("variant", variant.toString)
          .info("Build loaded")
      case Left(err) =>
        val msg = s"Failed to load build: ${err.getMessage}"
        buildLoadError.set(Some(msg))
        logger.withContext("workspace", buildRoot).withContext("error", err.getMessage).error("Failed to load build")
        bspError(msg)
    }

    initialized.set(true)

    debugLog(s"Initialized for workspace: $buildRoot (variant: $variant)")

    InitializeBuildResult(
      displayName = "Bleep BSP Server",
      version = model.BleepVersion.current.value,
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

  private def handleInitialized(): Unit = ()

  private def handleShutdown(): Unit = {
    logger.warn("build/shutdown received - cancelling all active requests")
    shutdownRequested.set(true)
    cancelAllActiveRequests()
  }

  /** Register an operation for visibility. Non-blocking — multiple operations can run concurrently.
    *
    * Sends WorkspaceBusy events for any concurrent operations (informational, not blocking).
    */
  private def registerOperation(
      workspace: Path,
      operationId: String,
      operation: String,
      projects: Set[String],
      cancellation: CancellationToken,
      originId: Option[String]
  ): Unit = {
    // Notify client about concurrent operations (informational)
    val concurrent = SharedWorkspaceState.getActiveOperations(workspace)
    concurrent.foreach { active =>
      sendEvent(
        originId,
        operationId,
        BleepBspProtocol.Event.WorkspaceBusy(
          operation = active.operation,
          projects = active.projects.toList.sorted.map(s => CrossProjectName.fromString(s).get),
          startedAgoMs = System.currentTimeMillis() - active.startTimeMs,
          timestamp = System.currentTimeMillis()
        )
      )
    }

    val kill: Runnable = () => cancelAllActiveRequests()
    val work = SharedWorkspaceState.ActiveWork(operationId, operation, projects, cancellation, System.currentTimeMillis(), kill)
    SharedWorkspaceState.register(workspace, work)
    myOperationIds.add(operationId)
  }

  /** Unregister an operation after it completes. */
  private def unregisterOperation(workspace: Path, operationId: String): Unit = {
    SharedWorkspaceState.unregister(workspace, operationId)
    myOperationIds.remove(operationId)
  }

  /** Cancel all in-flight requests and kill child processes. */
  private def cancelAllActiveRequests(): Unit = {
    val activeCount = activeRequests.size()
    if (activeCount > 0) {
      logger.withContext("count", activeCount).withContext("requests", activeRequests.keySet().asScala.mkString(", ")).warn("Cancelling all active requests")
    }
    // Cancel all active request tokens (triggers cancellation flow in running IOs)
    activeRequests.values().forEach(_.cancel())

    // Kill any child processes of this JVM (belt and suspenders)
    killAllChildProcesses()
  }

  /** Kill all child processes of this JVM using ProcessHandle API */
  private def killAllChildProcesses(): Unit =
    try
      ProcessHandle.current().children().forEach { child =>
        try {
          debugLog(s"Killing child process: ${child.pid()}")
          child.destroyForcibly()
        } catch { case _: Exception => }
      }
    catch { case _: Exception => }

  private def handleExit(): Unit = {
    // Don't exit the daemon - just close this connection
  }

  private def handleCancelRequest(params: CancelRequestParams): Unit = {
    val idStr = params.id match {
      case Left(s)  => s
      case Right(i) => i.toString
    }
    val token = Option(activeRequests.remove(idStr))
    logger.withContext("id", idStr).withContext("tokenPresent", token.isDefined.toString).warn("Received cancelRequest")
    token.foreach(_.cancel())
  }

  // ==========================================================================
  // Build loading using bleep-core
  // ==========================================================================

  private def loadBuild(workspaceRoot: Path, variant: model.BuildVariant): Either[BleepException, Started] =
    Option(loadedBuilds.get(workspaceRoot)) match {
      case Some(existing) =>
        // Always set buildDir — PlainVirtualFile needs it for marker-prefixed IDs
        bleep.analysis.PlainVirtualFile.setBuildDir(existing.buildPaths.buildDir)
        Right(existing)
      case None =>
        val userPaths = UserPaths.fromAppDirs
        val buildLoader = BuildLoader.inDirectory(workspaceRoot)
        val buildPaths = BuildPaths(workspaceRoot, buildLoader, variant)

        for {
          bleepConfig <- BleepConfigOps.loadOrDefault(userPaths)
          existingBuild <- buildLoader.existing
          pre = Prebootstrapped(
            logger = logger,
            userPaths = userPaths,
            buildPaths = buildPaths,
            existingBuild = existingBuild,
            ec = scala.concurrent.ExecutionContext.global
          )
          started <- bootstrap.from(
            pre = pre,
            resolveProjects = ResolveProjects.InMemory,
            rewrites = activeRewrites.get(),
            config = bleepConfig,
            resolverFactory = CoursierResolver.Factory.default
          )
        } yield {
          loadedBuilds.put(workspaceRoot, started)
          // Configure PlainVirtualFile with build dir for portable zinc analysis IDs
          bleep.analysis.PlainVirtualFile.setBuildDir(started.buildPaths.buildDir)
          started
        }
    }

  /** Create Started from a pre-exploded build (when passed via init data from bleep client). This allows the BSP server to use the exact same rewritten build
    * that the client has, including any build rewrites like ReplaceBleepDependencies.
    *
    * Strategy:
    *   1. Resolve projects normally using the provided Build.Exploded (which has build.bleep:* dependencies already removed by ReplaceBleepDependencies
    *      rewrite)
    *   2. Apply classpath overrides from the client (which contain the full resolved classpath including class directories that replace the build.bleep:*
    *      dependencies)
    */
  private def createStartedFromExplodedBuild(
      buildRoot: Path,
      variant: model.BuildVariant,
      exploded: model.Build.Exploded
  ): Either[BleepException, Started] =
    // Check if already loaded
    Option(loadedBuilds.get(buildRoot)) match {
      case Some(existing) =>
        bleep.analysis.PlainVirtualFile.setBuildDir(existing.buildPaths.buildDir)
        Right(existing)
      case None =>
        val userPaths = UserPaths.fromAppDirs
        val buildLoader = BuildLoader.inDirectory(buildRoot)
        val buildPaths = BuildPaths(buildRoot, buildLoader, variant)
        val classpathOverrides = providedClasspathOverrides.get()

        for {
          bleepConfig <- BleepConfigOps.loadOrDefault(userPaths)
          existingBuild <- buildLoader.existing
          pre = Prebootstrapped(
            logger = logger,
            userPaths = userPaths,
            buildPaths = buildPaths,
            existingBuild = existingBuild,
            ec = scala.concurrent.ExecutionContext.global
          )
          // Use bootstrap.from but then replace the build with the provided exploded build
          baseStarted <- bootstrap.from(
            pre = pre,
            resolveProjects = ResolveProjects.InMemory,
            rewrites = Nil,
            config = bleepConfig,
            resolverFactory = CoursierResolver.Factory.default
          )
        } yield {
          val resolver = baseStarted.resolver

          // Resolve projects using the provided Build.Exploded.
          // The build.bleep:* dependencies have already been removed by ReplaceBleepDependencies
          // on the client side, so this should work without needing to resolve those deps.
          val resolveResult = ResolveProjects.InMemory(pre, resolver, exploded)

          // Apply classpath overrides if provided.
          // The client sends the full resolved classpath for each project, which includes
          // class directories that replace the build.bleep:* dependencies.
          val resolvedProjects: scala.collection.immutable.SortedMap[model.CrossProjectName, Lazy[ResolvedProject]] =
            if (classpathOverrides.nonEmpty) {
              debugLog(s"Applying classpath overrides from client for ${classpathOverrides.size} projects")
              scala.collection.immutable.SortedMap.from(
                resolveResult.projects.map { case (crossName, lazyResolved) =>
                  classpathOverrides.get(crossName) match {
                    case Some(overrideClasspath) =>
                      // Replace the classpath with the one from the client
                      crossName -> Lazy {
                        val resolved = lazyResolved.forceGet
                        resolved.copy(classpath = overrideClasspath)
                      }
                    case None =>
                      // No override, keep as-is
                      crossName -> lazyResolved
                  }
                }
              )
            } else {
              resolveResult.projects
            }

          lazy val started: Started = Started(
            pre = pre,
            rewrites = Nil,
            build = exploded,
            resolvedProjects = resolvedProjects,
            activeProjectsFromPath = baseStarted.activeProjectsFromPath,
            config = bleepConfig,
            resolver = resolver,
            bleepExecutable = baseStarted.bleepExecutable,
            bspServerClasspathSource = BspServerClasspathSource.FromCoursier(resolver)
          )((_, _, _) => Right(started)) // Reload returns the same build
          loadedBuilds.put(buildRoot, started)
          // Configure PlainVirtualFile with build dir for portable zinc analysis IDs
          bleep.analysis.PlainVirtualFile.setBuildDir(started.buildPaths.buildDir)
          started
        }
    }

  private def getActiveBuild: Either[String, Started] =
    activeWorkspace.get() match {
      case None =>
        Left("No workspace set. Call build/initialize first.")
      case Some(ws) =>
        buildLoadError.get() match {
          case Some(err) => Left(err)
          case None      =>
            Option(loadedBuilds.get(ws)) match {
              case Some(started) => Right(started)
              case None          => Left(s"Build not yet loaded for workspace $ws")
            }
        }
    }

  private def crossNameFromTargetId(started: Started, targetId: BuildTargetIdentifier): Option[CrossProjectName] = {
    val uri = targetId.uri.value
    val idPart = uri.split("\\?id=").lastOption
    idPart.flatMap { id =>
      started.build.explodedProjects.keys.find(_.value == id)
    }
  }

  private def buildTargetId(buildPaths: BuildPaths, crossName: CrossProjectName): BuildTargetIdentifier = {
    val baseUri = buildPaths.buildVariantDir.toUri.toASCIIString.stripSuffix("/")
    val uri = s"$baseUri?id=${crossName.value}"
    BuildTargetIdentifier(Uri(java.net.URI.create(uri)))
  }

  // ==========================================================================
  // Workspace handlers
  // ==========================================================================

  private def handleBuildTargets(): WorkspaceBuildTargetsResult = {
    // Gate already checked in dispatch, so this is always Right
    val started = getActiveBuild.fold(msg => throw BspException(JsonRpcErrorCodes.InternalError, msg), identity)
    val targets = started.build.explodedProjects.map { case (crossName, project) =>
      projectToBuildTarget(started, crossName, project)
    }.toList
    WorkspaceBuildTargetsResult(targets)
  }

  private def projectToBuildTarget(started: Started, crossName: CrossProjectName, project: model.Project): BuildTarget = {
    val targetId = buildTargetId(started.buildPaths, crossName)
    val projectPaths = started.projectPaths(crossName)
    val resolved = started.resolvedProjects.get(crossName).map(_.forceGet)
    val resolvedJvm = started.resolvedJvm.forceGet
    val javaHome = resolvedJvm.javaBin.getParent.getParent
    // jvm.name is e.g. "graalvm-community:25.0.1" — extract version after the colon.
    // For "system" JVM there's no version in the name, so fall back to the running JVM's version.
    // javaVersion is optional/informational in BSP (used by Metals doctor), so None is fine as last resort.
    val jvmVersion: Option[String] =
      if (model.Jvm.isSystem(resolvedJvm.jvm)) Option(System.getProperty("java.version"))
      else
        resolvedJvm.jvm.name.split(':') match {
          case Array(_, version) => Some(version)
          case _                 => None
        }

    val jvmTarget = JvmBuildTarget(
      javaHome = Some(Uri(javaHome.toUri)),
      javaVersion = jvmVersion
    )

    val (languages, dataKind, data) = resolved.map(_.language) match {
      case Some(sc: ResolvedProject.Language.Scala) =>
        val scalaBuildTarget = ScalaBuildTarget(
          scalaOrganization = sc.organization,
          scalaVersion = sc.version,
          scalaBinaryVersion = scalaBinaryVersion(sc.version),
          platform = ScalaPlatform.Jvm,
          jars = sc.compilerJars.map(p => Uri(p.toUri)),
          jvmBuildTarget = Some(jvmTarget)
        )
        (List("scala", "java"), BuildTargetDataKind.Scala, RawJson(writeToArray(scalaBuildTarget)(using ScalaBuildTarget.codec)))

      case Some(kt: ResolvedProject.Language.Kotlin) =>
        val kotlinBuildTarget = KotlinBuildTarget(
          kotlinVersion = kt.version,
          jvmTarget = jvmVersion.getOrElse(""),
          kotlincOptions = kt.options,
          isK2 = kt.version.split('.').headOption.flatMap(_.toIntOption).exists(_ >= 2)
        )
        (List("kotlin", "java"), KotlinBuildTargetDataKind.Kotlin, RawJson(writeToArray(kotlinBuildTarget)(using KotlinBuildTarget.codec)))

      case _ =>
        (List("java"), BuildTargetDataKind.Jvm, RawJson(writeToArray(jvmTarget)(using JvmBuildTarget.codec)))
    }

    val isTest = project.isTestProject.getOrElse(false)
    val hasMain = project.platform.flatMap(_.mainClass).isDefined
    val tags = if (isTest) List(BuildTargetTag.Test) else if (hasMain) List(BuildTargetTag.Application) else List(BuildTargetTag.Library)

    val dependencies = started.build
      .resolvedDependsOn(crossName)
      .toList
      .map(dep => buildTargetId(started.buildPaths, dep))

    BuildTarget(
      id = targetId,
      displayName = Some(crossName.value),
      baseDirectory = Some(Uri(projectPaths.dir.toUri)),
      tags = tags,
      languageIds = languages,
      dependencies = dependencies,
      capabilities = BuildTargetCapabilities(
        canCompile = Some(true),
        canTest = Some(true),
        canRun = Some(true),
        canDebug = Some(true)
      ),
      dataKind = Some(dataKind),
      data = Some(data)
    )
  }

  private def scalaBinaryVersion(version: String): String =
    if (version.startsWith("3.")) "3"
    else if (version.startsWith("2.13.")) "2.13"
    else if (version.startsWith("2.12.")) "2.12"
    else version

  /** Resolve the com.sourcegraph:semanticdb-javac plugin JAR via coursier */
  private def resolveJavaSemanticdbPlugin(version: String): Unit =
    try {
      import coursier.*
      val dep = Dependency(Module(Organization("com.sourcegraph"), ModuleName("semanticdb-javac")), version)
      val fetch = Fetch().addDependencies(dep)
      val files = fetch.run()
      files.find(_.getName.startsWith("semanticdb-javac")) match {
        case Some(jar) =>
          val path = jar.toPath
          logger.info(s"Resolved Java semanticdb plugin: $path")
          javaSemanticdbPlugin.set(Some(path))
        case None =>
          logger.warn(s"Could not find semanticdb-javac JAR in resolved files: ${files.map(_.getName)}")
      }
    } catch {
      case e: Throwable =>
        logger.warn(s"Failed to resolve com.sourcegraph:semanticdb-javac: ${e.getMessage}")
    }

  /** Compute Java semanticdb javac options for IDE clients */
  private def javaSemanticdbOptions(pluginPath: Path, workspaceDir: Path, classesDir: Path): List[String] = {
    val baseOptions = List(
      s"-Xplugin:semanticdb -sourceroot:$workspaceDir -targetroot:$classesDir",
      "-processorpath",
      pluginPath.toString
    )
    // Java 17+ needs --add-exports for javac internals
    val addExports = List(
      "-J--add-exports",
      "-Jjdk.compiler/com.sun.tools.javac.api=ALL-UNNAMED",
      "-J--add-exports",
      "-Jjdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED",
      "-J--add-exports",
      "-Jjdk.compiler/com.sun.tools.javac.model=ALL-UNNAMED",
      "-J--add-exports",
      "-Jjdk.compiler/com.sun.tools.javac.tree=ALL-UNNAMED",
      "-J--add-exports",
      "-Jjdk.compiler/com.sun.tools.javac.util=ALL-UNNAMED"
    )
    addExports ::: baseOptions
  }

  private def handleReload(): Unit =
    activeWorkspace.get().foreach { ws =>
      debugLog(s"Reloading workspace: $ws")
      loadedBuilds.remove(ws)
      BspMetrics.recordCacheEvict("loadedBuilds", ws.toString)
      val variant = activeVariant.get()
      loadBuild(ws, variant): Unit
    }

  private def handleSources(params: SourcesParams): SourcesResult = {
    val items = params.targets.map { targetId =>
      val sources = (for {
        started <- getActiveBuild.toOption
        crossName <- crossNameFromTargetId(started, targetId)
        resolved <- started.resolvedProjects.get(crossName)
      } yield resolved.forceGet.sources.map { src =>
        SourceItem(
          uri = Uri(src.toUri),
          kind = SourceItemKind.Directory,
          generated = false,
          dataKind = None,
          data = None
        )
      }.toList).getOrElse(List.empty)

      SourcesItem(target = targetId, sources = sources, roots = None)
    }
    SourcesResult(items)
  }

  private def handleDependencySources(params: DependencySourcesParams): DependencySourcesResult = {
    val items = params.targets.map { targetId =>
      val sourceJars = (for {
        started <- getActiveBuild.toOption
        crossName <- crossNameFromTargetId(started, targetId)
        resolved <- started.resolvedProjects.get(crossName)
      } yield {
        val p = resolved.forceGet
        p.resolution match {
          case Some(res) =>
            res.modules.flatMap { m =>
              m.artifacts
                .filter(a => a.classifier.contains("sources"))
                .map(a => Uri(a.path.toUri))
            }.distinct
          case None => List.empty
        }
      }).getOrElse(List.empty)
      DependencySourcesItem(target = targetId, sources = sourceJars)
    }
    DependencySourcesResult(items)
  }

  /** Parsed link options from compile arguments */
  private case class ParsedLinkOptions(
      isLink: Boolean,
      isRelease: Boolean,
      sourceMaps: Option[Boolean],
      minify: Option[Boolean],
      moduleKind: Option[String],
      lto: Option[String],
      optimize: Option[Boolean],
      debugInfo: Option[Boolean],
      flamegraph: Boolean
  )

  private def parseLinkOptions(args: List[String]): ParsedLinkOptions = {
    def findOption(prefix: String): Option[String] =
      args.find(_.startsWith(s"$prefix=")).map(_.stripPrefix(s"$prefix="))

    ParsedLinkOptions(
      isLink = args.contains("--link"),
      isRelease = args.contains("--release"),
      sourceMaps =
        if (args.contains("--source-maps")) Some(true)
        else if (args.contains("--no-source-maps")) Some(false)
        else None,
      minify =
        if (args.contains("--minify")) Some(true)
        else if (args.contains("--no-minify")) Some(false)
        else None,
      moduleKind = findOption("--module-kind"),
      lto = findOption("--lto"),
      optimize =
        if (args.contains("--optimize")) Some(true)
        else if (args.contains("--no-optimize")) Some(false)
        else None,
      debugInfo =
        if (args.contains("--debug-info")) Some(true)
        else if (args.contains("--no-debug-info")) Some(false)
        else None,
      flamegraph = args.contains("--flamegraph")
    )
  }

  /** Run sourcegen scripts for the given projects if any have them configured. Returns Right(summary message) on success or Left(error message) on failure.
    */
  private def runSourcegenIfNeeded(
      started: Started,
      projects: Set[CrossProjectName],
      originId: Option[String],
      maxParallelism: Int,
      cancellation: CancellationToken
  ): Either[String, Option[String]] = {
    val sourcegenScripts = SourceGenRunner.findScripts(started, projects)
    if (sourcegenScripts.isEmpty) return Right(None)

    debugLog(s"Found ${sourcegenScripts.size} sourcegen scripts")

    val compileScriptProjects: Set[CrossProjectName] => IO[Boolean] = { scriptProjects =>
      val scriptDag = BleepBuildConverter.toProjectDag(started, Some(scriptProjects))
      val scriptDiagnostics = new DiagnosticListener {
        def onDiagnostic(error: CompilerError): Unit =
          debugLog(s"Sourcegen compile: ${error.message}")
      }
      ParallelProjectCompiler
        .build(
          dag = scriptDag,
          parallelism = maxParallelism,
          diagnosticListener = scriptDiagnostics,
          cancellationToken = cancellation,
          progressListener = ParallelProjectCompiler.BuildProgressListener.noop
        )
        .map(_.isSuccess)
    }

    val sourcegenKillSignal = Deferred.unsafe[IO, KillReason]

    val sourcegenListener = new SourceGenRunner.SourceGenListener {
      def onScriptStarted(scriptMain: String, forProjects: List[String]): Unit = {
        BspMetrics.recordSourcegenStart(scriptMain)
        sendTestEvent(
          originId,
          s"sourcegen-$scriptMain",
          BleepBspProtocol.Event.SourcegenStarted(scriptMain, forProjects.map(s => CrossProjectName.fromString(s).get), System.currentTimeMillis())
        )
      }

      def onScriptFinished(scriptMain: String, success: Boolean, durationMs: Long, error: Option[String]): Unit = {
        BspMetrics.recordSourcegenEnd(scriptMain, durationMs, success)
        sendTestEvent(
          originId,
          s"sourcegen-$scriptMain",
          BleepBspProtocol.Event.SourcegenFinished(scriptMain, success, durationMs, error, System.currentTimeMillis())
        )
      }

      def onLog(message: String, isError: Boolean): Unit =
        if (isError) bspError(message)
        else bspInfo(message)
    }

    val sourcegenResult = SourceGenRunner
      .runScripts(
        started = started,
        scripts = sourcegenScripts,
        compileProjects = compileScriptProjects,
        killSignal = sourcegenKillSignal,
        listener = sourcegenListener
      )
      .unsafeRunSync()

    if (!sourcegenResult.isSuccess) {
      Left(s"Sourcegen failed: ${sourcegenResult.failures.mkString("; ")}")
    } else {
      val msg = s"Sourcegen complete: ${sourcegenResult.scriptsRun} run, ${sourcegenResult.scriptsSkipped} skipped"
      bspInfo(msg)
      Right(Some(msg))
    }
  }

  private def handleCompile(params: CompileParams, cancellation: CancellationToken): CompileResult = {
    val started = getActiveBuild.fold(msg => throw BspException(JsonRpcErrorCodes.InternalError, msg), identity)

    diagnosticTracker.startCycle()

    // Parse link options from arguments
    val args = params.arguments.getOrElse(List.empty)
    val linkOpts = parseLinkOptions(args)
    val isLink = linkOpts.isLink
    val isRelease = linkOpts.isRelease

    val projectsToCompile = params.targets.flatMap { targetId =>
      crossNameFromTargetId(started, targetId)
    }.toSet

    debugLog(s"Compile request for: ${projectsToCompile.map(_.value).mkString(", ")}, isLink=$isLink, isRelease=$isRelease, opts=$linkOpts")

    val opLabel = if (args.exists(_.contains("link"))) "link" else "compile"
    val taskId = java.util.UUID.randomUUID().toString
    val workspace = activeWorkspace.get().getOrElse(started.buildPaths.buildDir)
    registerOperation(workspace, taskId, opLabel, projectsToCompile.map(_.value), cancellation, params.originId)
    try {
      // Re-read user config fresh before starting (allows runtime config changes)
      val userPaths = UserPaths.fromAppDirs
      val freshConfig = BleepConfigOps.loadOrDefault(userPaths).getOrElse(model.BleepConfig.default)
      val serverConfig = freshConfig.bspServerConfigOrDefault
      val maxParallelism = serverConfig.effectiveParallelism
      debugLog(s"BSP config: parallelism=$maxParallelism")

      // Include transitive dependencies
      val allProjects = BleepBuildConverter.transitiveDependencies(projectsToCompile, started)
      debugLog(s"Compiling ${allProjects.size} projects (including dependencies)")

      // Run sourcegen scripts if any projects have them
      runSourcegenIfNeeded(started, allProjects, params.originId, maxParallelism, cancellation) match {
        case Left(err) =>
          bspError(err)
          return CompileResult(
            originId = params.originId,
            statusCode = StatusCode.Error,
            dataKind = None,
            data = None
          )
        case Right(_) => ()
      }

      // Get all project dependencies (for TaskDag)
      val allProjectDeps: Map[CrossProjectName, Set[CrossProjectName]] =
        started.build.resolvedDependsOn.map { case (crossName, deps) =>
          crossName -> deps.toSet
        }

      // Determine platforms for target projects (needed for link tasks)
      val platforms: Map[CrossProjectName, TaskDag.LinkPlatform] = if (isLink) {
        projectsToCompile.flatMap { crossName =>
          val project = started.build.explodedProjects(crossName)
          val platformOpt = project.platform.flatMap(_.name)
          val isKotlin = project.kotlin.flatMap(_.version).isDefined
          val kotlinVersion = project.kotlin.flatMap(_.version).map(_.kotlinVersion).getOrElse(model.VersionKotlin.Kotlin21.kotlinVersion)

          (platformOpt, isKotlin) match {
            case (Some(model.PlatformId.Js), true) =>
              // Kotlin/JS
              val projectPaths = started.projectPaths(crossName)
              val outputDir = projectPaths.targetDir.resolve("link-output").resolve("js")
              val moduleKind = linkOpts.moduleKind
                .map {
                  case "esmodule" => model.KotlinJsModuleKind.ESModule
                  case "nomodule" => model.KotlinJsModuleKind.Plain
                  case _          => model.KotlinJsModuleKind.CommonJS
                }
                .getOrElse(model.KotlinJsModuleKind.CommonJS)
              val config = TaskDag.KotlinJsConfig(
                moduleKind = moduleKind,
                sourceMap = linkOpts.sourceMaps.getOrElse(!isRelease),
                dce = linkOpts.optimize.getOrElse(isRelease),
                outputDir = outputDir
              )
              Some(crossName -> TaskDag.LinkPlatform.KotlinJs(kotlinVersion, config))

            case (Some(model.PlatformId.Js), false) =>
              // Scala.js
              val sjsVersion = project.platform.flatMap(_.jsVersion).map(_.scalaJsVersion).getOrElse(model.VersionScalaJs.ScalaJs1.scalaJsVersion)
              val scalaVersion = project.scala.flatMap(_.version).map(_.scalaVersion).getOrElse(model.VersionScala.Scala3.scalaVersion)
              val baseConfig = if (isRelease) bleep.analysis.ScalaJsLinkConfig.Release else bleep.analysis.ScalaJsLinkConfig.Debug
              val config = baseConfig.copy(
                emitSourceMaps = linkOpts.sourceMaps.getOrElse(baseConfig.emitSourceMaps),
                minify = linkOpts.minify.getOrElse(baseConfig.minify),
                optimizer = linkOpts.optimize.getOrElse(baseConfig.optimizer),
                moduleKind = linkOpts.moduleKind
                  .map {
                    case "nomodule" => ScalaJsLinkConfig.ModuleKind.NoModule
                    case "esmodule" => ScalaJsLinkConfig.ModuleKind.ESModule
                    case _          => ScalaJsLinkConfig.ModuleKind.CommonJSModule
                  }
                  .getOrElse(baseConfig.moduleKind)
              )
              Some(crossName -> TaskDag.LinkPlatform.ScalaJs(sjsVersion, scalaVersion, config))

            case (Some(model.PlatformId.Native), true) =>
              // Kotlin/Native
              val config = TaskDag.KotlinNativeConfig(
                target = "host",
                debugInfo = linkOpts.debugInfo.getOrElse(false),
                optimizations = linkOpts.optimize.getOrElse(isRelease),
                isTest = false
              )
              Some(crossName -> TaskDag.LinkPlatform.KotlinNative(kotlinVersion, config))

            case (Some(model.PlatformId.Native), false) =>
              // Scala Native
              val snVersion =
                project.platform.flatMap(_.nativeVersion).map(_.scalaNativeVersion).getOrElse(model.VersionScalaNative.ScalaNative05.scalaNativeVersion)
              val scalaVersion = project.scala.flatMap(_.version).map(_.scalaVersion).getOrElse(model.VersionScala.Scala3.scalaVersion)
              val baseConfig = if (isRelease) bleep.analysis.ScalaNativeLinkConfig.ReleaseFast else bleep.analysis.ScalaNativeLinkConfig.Debug
              val configWithLto = linkOpts.lto match {
                case Some("thin") => baseConfig.copy(lto = bleep.analysis.ScalaNativeLinkConfig.NativeLTO.Thin)
                case Some("full") => baseConfig.copy(lto = bleep.analysis.ScalaNativeLinkConfig.NativeLTO.Full)
                case Some("none") => baseConfig.copy(lto = bleep.analysis.ScalaNativeLinkConfig.NativeLTO.None)
                case _            => baseConfig
              }
              val config = configWithLto.copy(
                optimize = linkOpts.optimize.getOrElse(configWithLto.optimize)
              )
              Some(crossName -> TaskDag.LinkPlatform.ScalaNative(snVersion, scalaVersion, config))

            case _ =>
              // JVM - no linking needed
              None
          }
        }.toMap
      } else {
        Map.empty
      }

      // Validate link options if --link was specified
      if (isLink) {
        val platformsPresent = platforms.values.collect {
          case _: TaskDag.LinkPlatform.KotlinJs     => "Kotlin/JS"
          case _: TaskDag.LinkPlatform.ScalaJs      => "Scala.js"
          case _: TaskDag.LinkPlatform.KotlinNative => "Kotlin/Native"
          case _: TaskDag.LinkPlatform.ScalaNative  => "Scala Native"
        }.toSet

        val validationErrors = List.newBuilder[String]

        if (platformsPresent.isEmpty) {
          validationErrors += "No linkable projects found (only JVM projects can be compiled without linking)"
        }

        if (linkOpts.sourceMaps.isDefined && !platformsPresent.exists(p => p == "Scala.js" || p == "Kotlin/JS")) {
          validationErrors += s"--source-maps/--no-source-maps only applies to JS platforms (Scala.js, Kotlin/JS), but linking: ${platformsPresent.mkString(", ")}"
        }
        if (linkOpts.minify.isDefined && !platformsPresent.contains("Scala.js")) {
          validationErrors += s"--minify/--no-minify only applies to Scala.js, but linking: ${platformsPresent.mkString(", ")}"
        }
        if (linkOpts.moduleKind.isDefined && !platformsPresent.exists(p => p == "Scala.js" || p == "Kotlin/JS")) {
          validationErrors += s"--module-kind only applies to JS platforms (Scala.js, Kotlin/JS), but linking: ${platformsPresent.mkString(", ")}"
        }
        if (linkOpts.lto.isDefined && !platformsPresent.contains("Scala Native")) {
          validationErrors += s"--lto only applies to Scala Native, but linking: ${platformsPresent.mkString(", ")}"
        }
        if (linkOpts.optimize.isDefined && platformsPresent.isEmpty) {
          validationErrors += s"--optimize/--no-optimize only applies to non-JVM platforms, but no linkable projects found"
        }
        if (linkOpts.debugInfo.isDefined && !platformsPresent.exists(p => p == "Scala Native" || p == "Kotlin/Native")) {
          validationErrors += s"--debug-info/--no-debug-info only applies to native platforms (Scala Native, Kotlin/Native), but linking: ${platformsPresent.mkString(", ")}"
        }

        val errors = validationErrors.result()
        if (errors.nonEmpty) {
          errors.foreach(bspError)
          return CompileResult(
            originId = params.originId,
            statusCode = StatusCode.Error,
            dataKind = None,
            data = None
          )
        }
      }

      // Build the appropriate DAG based on mode
      val buildMode = if (isLink) {
        BleepBspProtocol.BuildMode.Link(isRelease)
      } else {
        BleepBspProtocol.BuildMode.Compile
      }

      val noCache = args.contains("--no-cache")
      val cacheContext = RemoteCacheContext.create(started, noCache, makeCacheEventSink(params.originId))
      val initialDag = TaskDag.buildDag(projectsToCompile, allProjectDeps, platforms, buildMode, withCache = cacheContext.isEnabled)
      debugLog(s"Built compile DAG with ${initialDag.tasks.size} tasks (mode=$buildMode, cache=${cacheContext.isEnabled})")

      val startTime = System.currentTimeMillis()
      BspMetrics.recordBuildStart(workspace.toString, allProjects.size)

      val compileHandler = makeCompileHandler(started, workspace, params.originId, serverConfig.effectiveHeapPressureThreshold, cacheContext)
      val (cachePullHandler, cachePushHandler) = RemoteCacheContext.handlers(cacheContext)

      // Create link handler
      val linkHandler: (TaskDag.LinkTask, Deferred[IO, KillReason]) => IO[(TaskDag.TaskResult, TaskDag.LinkResult)] =
        (linkTask, taskKillSignal) => {
          val projectPaths = started.projectPaths(linkTask.project)
          val project = started.build.explodedProjects(linkTask.project)
          val resolved = started.resolvedProject(linkTask.project)
          val classpath = projectPaths.classes :: resolved.classpath.map(p => Path.of(p.toString)).toList
          val linkLogger = createLinkLogger()
          val outputDir = projectPaths.targetDir.resolve("link-output")
          LinkExecutor.execute(linkTask, classpath.map(_.toAbsolutePath), project.platform.flatMap(_.mainClass), outputDir, linkLogger, taskKillSignal)
        }

      // No-op handlers for discover/test (won't be called for compile/link DAGs)
      val discoverHandler: (TaskDag.DiscoverTask, Deferred[IO, KillReason]) => IO[(TaskDag.TaskResult, List[(String, String)])] =
        (_, _) => IO.pure((TaskDag.TaskResult.Success, List.empty))

      val testHandler: (TaskDag.TestSuiteTask, Deferred[IO, KillReason]) => IO[TaskDag.TaskResult] =
        (_, _) => IO.pure(TaskDag.TaskResult.Success)

      // Create executor
      val executor = TaskDag.executor(compileHandler, linkHandler, discoverHandler, testHandler, cachePullHandler, cachePushHandler)

      // Create trace recorder (noop if not enabled)
      val traceRecorder = if (linkOpts.flamegraph) TraceRecorder.create.unsafeRunSync() else TraceRecorder.noop

      val ioProgram = for {
        eventQueue <- Queue.bounded[IO, Option[TaskDag.DagEvent]](100000)
        killSignal <- Outcome.fromCancellationToken(cancellation)

        // Start event consumer fiber - use guarantee to ensure cleanup on cancellation/error
        consumerErrorRef <- Ref.of[IO, Option[Throwable]](None)
        eventConsumerFiber <- consumeCompileEvents(eventQueue, params.originId, killSignal, traceRecorder).compile.drain.handleErrorWith { e =>
          // Capture consumer error for later inspection
          IO(logger.withContext("error", e.getMessage).error("Compile event consumer error")) >>
            consumerErrorRef.set(Some(e))
        }.start

        // Run executor with guarantee to cancel consumer fiber on completion/error/cancellation
        dag <- executor
          .execute(initialDag, maxParallelism, eventQueue, killSignal)
          .flatTap(_ => eventQueue.offer(None) >> eventConsumerFiber.joinWithNever)
          .guarantee(eventQueue.offer(None).attempt >> eventConsumerFiber.cancel)

        // Log consumer errors but don't fail the build — compilation results are still valid
        // even if progress notifications couldn't be sent (e.g., client disconnected mid-build)
        consumerError <- consumerErrorRef.get
        _ <- consumerError match {
          case Some(e) =>
            IO(logger.withContext("error", e.getMessage).warn("Event consumer failed (build results still valid)"))
          case None => IO.unit
        }
      } yield dag

      val ioResult = Try(ioProgram.unsafeRunSync())

      // Write trace file if flamegraph is enabled
      if (linkOpts.flamegraph) {
        val tracePath = started.buildPaths.dotBleepDir.resolve("trace.json")
        traceRecorder.writeTrace(tracePath).unsafeRunSync()
      }

      // Clear stale diagnostics for files that had errors last cycle but not this one
      clearStaleDiagnostics()

      ioResult match {
        case Success(dag) =>
          val durationMs = System.currentTimeMillis() - startTime
          val isSuccess = dag.failed.isEmpty && dag.errored.isEmpty && !cancellation.isCancelled
          BspMetrics.recordBuildEnd(workspace.toString, durationMs, isSuccess)
          val compileTasks = dag.tasks.values.collect { case ct: TaskDag.CompileTask => ct.id }.toSet
          val linkTasks = dag.tasks.values.collect { case lt: TaskDag.LinkTask => lt.id }.toSet
          val compileCompleted = compileTasks.count(dag.completed.contains)
          val compileFailed = compileTasks.count(id => dag.failed.contains(id) || dag.errored.contains(id))
          val linkCompleted = linkTasks.count(dag.completed.contains)
          val linkFailed = linkTasks.count(id => dag.failed.contains(id) || dag.errored.contains(id))
          // Count how many links were actually executed vs up-to-date
          val linksUpToDate = dag.linkResults.values.count {
            case TaskDag.LinkResult.JsSuccess(_, _, _, wasUpToDate) => wasUpToDate
            case TaskDag.LinkResult.NativeSuccess(_, wasUpToDate)   => wasUpToDate
            case _                                                  => false
          }
          val linksActuallyLinked = linkCompleted - linksUpToDate

          if (cancellation.isCancelled) {
            bspWarn(s"Compilation cancelled (${durationMs}ms)")
            CompileResult(
              originId = params.originId,
              statusCode = StatusCode.Cancelled,
              dataKind = None,
              data = None
            )
          } else if (dag.failed.nonEmpty || dag.errored.nonEmpty) {
            val failedIds = (dag.failed ++ dag.errored).mkString(", ")
            bspError(s"Compilation failed: $compileFailed compile tasks failed, $linkFailed link tasks failed (${durationMs}ms)")
            debugLog(s"Failed tasks: $failedIds")
            CompileResult(
              originId = params.originId,
              statusCode = StatusCode.Error,
              dataKind = None,
              data = None
            )
          } else {
            val linkSummary = if (linksUpToDate > 0 && linksActuallyLinked > 0) {
              s"$linksActuallyLinked linked, $linksUpToDate up-to-date"
            } else if (linksUpToDate > 0) {
              s"$linksUpToDate up-to-date"
            } else if (linksActuallyLinked > 0) {
              s"$linksActuallyLinked linked"
            } else if (linkCompleted > 0) {
              s"$linkCompleted linked"
            } else {
              ""
            }
            val fullSummary = if (linkSummary.nonEmpty) {
              s"$compileCompleted compiled, $linkSummary"
            } else {
              s"$compileCompleted compiled"
            }
            bspInfo(s"Compilation succeeded: $fullSummary (${durationMs}ms)")
            CompileResult(
              originId = params.originId,
              statusCode = StatusCode.Ok,
              dataKind = None,
              data = None
            )
          }

        case Failure(ex) =>
          val durationMs = System.currentTimeMillis() - startTime
          BspMetrics.recordBuildEnd(workspace.toString, durationMs, false)
          bspError(s"Compilation failed: ${ex.getMessage} (${durationMs}ms)")
          CompileResult(
            originId = params.originId,
            statusCode = StatusCode.Error,
            dataKind = None,
            data = None
          )
      }
    } finally unregisterOperation(workspace, taskId)
  }

  /** Create a HeapPressureGate.Listener that sends BSP events and logs */
  private def makeHeapPressureListener(originId: Option[String]): HeapPressureGate.Listener =
    new HeapPressureGate.Listener {
      def onWait(project: String, used: HeapMb, max: HeapMb, delayMs: Long, now: EpochMs): Unit = {
        val retryAt = EpochMs(now.value + delayMs)
        sendEvent(
          originId,
          s"compile:$project",
          BleepBspProtocol.Event.CompileStalled(CrossProjectName.fromString(project).get, used.value, max.value, retryAt.value, now.value)
        )
        logger
          .withContext("project", project)
          .warn(
            s"waiting to ensure sufficient memory (heap: ${used.value}MB/${max.value}MB) — retrying in ${delayMs}ms"
          )
      }
      def onResume(project: String, used: HeapMb, max: HeapMb, waitedFor: DurationMs, now: EpochMs): Unit = {
        sendEvent(
          originId,
          s"compile:$project",
          BleepBspProtocol.Event.CompileResumed(CrossProjectName.fromString(project).get, used.value, max.value, waitedFor.value, now.value)
        )
        logger.withContext("project", project).info(s"resuming after ${waitedFor.value}ms wait (heap: ${used.value}MB/${max.value}MB)")
      }
    }

  /** Wait until heap pressure is below threshold before starting compilation.
    *
    * Delegates to HeapPressureGate for testable logic. Cancellation-safe: IO.sleep is cancelable.
    */
  private def waitForHeapPressure(
      projectName: String,
      originId: Option[String],
      threshold: Double
  ): IO[Unit] =
    HeapPressureGate.waitForHeapPressure(
      heapMonitor = heapMonitor,
      activeCompileCount = activeCompileCount,
      threshold = threshold,
      retryMs = HeapPressureGate.DefaultRetryMs,
      projectName = projectName,
      listener = makeHeapPressureListener(originId)
    )

  /** Event sink for cache operations that forwards events over BSP using the same channel as compile/test events. */
  private def makeCacheEventSink(originId: Option[String]): RemoteCacheContext.EventSink =
    new RemoteCacheContext.EventSink {
      def pullStarted(project: CrossProjectName, timestamp: Long): Unit =
        sendEvent(originId, s"cache-pull:${project.value}", BleepBspProtocol.Event.CachePullStarted(project, timestamp))

      def pullFinished(
          project: CrossProjectName,
          status: BleepBspProtocol.Event.CachePullStatus,
          durationMs: Long,
          bytes: Long,
          timestamp: Long
      ): Unit =
        sendEvent(originId, s"cache-pull:${project.value}", BleepBspProtocol.Event.CachePullFinished(project, status, durationMs, bytes, timestamp))

      def pushStarted(project: CrossProjectName, timestamp: Long): Unit =
        sendEvent(originId, s"cache-push:${project.value}", BleepBspProtocol.Event.CachePushStarted(project, timestamp))

      def pushFinished(
          project: CrossProjectName,
          status: BleepBspProtocol.Event.CachePushStatus,
          durationMs: Long,
          bytes: Long,
          timestamp: Long
      ): Unit =
        sendEvent(originId, s"cache-push:${project.value}", BleepBspProtocol.Event.CachePushFinished(project, status, durationMs, bytes, timestamp))
    }

  /** Send a structured event via BSP notification. Used for compile, link, and test events. */
  private def sendEvent(originId: Option[String], taskId: String, event: BleepBspProtocol.Event): Unit = {
    val eventJson = BleepBspProtocol.encode(event)
    sendNotification(
      "build/taskProgress",
      TaskProgressParams(
        taskId = TaskId(taskId, None),
        originId = originId,
        eventTime = Some(event.timestamp),
        message = None,
        total = None,
        progress = None,
        unit = None,
        dataKind = Some(BleepBspProtocol.DataKind),
        data = Some(RawJson(eventJson.getBytes("UTF-8")))
      )
    )
  }

  /** Handle buildTarget/test request.
    *
    * This implements a unified compile+discover+test pipeline using TaskDag. Tests start running as soon as their dependencies compile, providing maximum
    * parallelism.
    */
  private def handleTest(params: TestParams, cancellation: CancellationToken): TestResult = {
    val started = getActiveBuild.fold(msg => throw BspException(JsonRpcErrorCodes.InternalError, msg), identity)

    val testProjects = params.targets.flatMap { targetId =>
      crossNameFromTargetId(started, targetId)
    }.toSet

    debugLog(s"Test request for: ${testProjects.map(_.value).mkString(", ")}")

    val taskId = java.util.UUID.randomUUID().toString
    val workspace = activeWorkspace.get().getOrElse(started.buildPaths.buildDir)
    registerOperation(workspace, taskId, "test", testProjects.map(_.value), cancellation, params.originId)
    try {
      // Re-read user config fresh before starting (allows runtime config changes)
      val userPaths = UserPaths.fromAppDirs
      val freshConfig = BleepConfigOps.loadOrDefault(userPaths).getOrElse(model.BleepConfig.default)
      val serverConfig = freshConfig.bspServerConfigOrDefault
      val maxParallelism = serverConfig.effectiveParallelism

      // Run sourcegen scripts if any test projects (or their dependencies) have them
      val allTestAndDeps = BleepBuildConverter.transitiveDependencies(testProjects, started)
      runSourcegenIfNeeded(started, allTestAndDeps, params.originId, maxParallelism, cancellation) match {
        case Left(err) =>
          bspError(err)
          return TestResult(
            originId = params.originId,
            statusCode = StatusCode.Error,
            dataKind = None,
            data = None
          )
        case Right(_) => ()
      }

      // Get all project dependencies (for compile tasks)
      val allProjectDeps: Map[CrossProjectName, Set[CrossProjectName]] =
        started.build.resolvedDependsOn.map { case (crossName, deps) =>
          crossName -> deps.toSet
        }

      // Determine platforms for test projects (needed for link tasks)
      val platforms: Map[model.CrossProjectName, TaskDag.LinkPlatform] = testProjects.flatMap { crossName =>
        val project = started.build.explodedProjects(crossName)
        val platformOpt = project.platform.flatMap(_.name)
        val isKotlin = project.kotlin.flatMap(_.version).isDefined
        val kotlinVersion = project.kotlin.flatMap(_.version).map(_.kotlinVersion).getOrElse(model.VersionKotlin.Kotlin21.kotlinVersion)

        (platformOpt, isKotlin) match {
          case (Some(model.PlatformId.Js), true) =>
            // Kotlin/JS - don't add "js" here; executeKotlinJs adds it
            val projectPaths = started.projectPaths(crossName)
            val outputDir = projectPaths.targetDir
            val config = TaskDag.KotlinJsConfig(
              moduleKind = model.KotlinJsModuleKind.UMD,
              sourceMap = false,
              dce = false, // Tests run without DCE
              outputDir = outputDir
            )
            Some(crossName -> TaskDag.LinkPlatform.KotlinJs(kotlinVersion, config))

          case (Some(model.PlatformId.Js), false) =>
            // Scala.js
            val sjsVersion = project.platform.flatMap(_.jsVersion).map(_.scalaJsVersion).getOrElse(model.VersionScalaJs.ScalaJs1.scalaJsVersion)
            val scalaVersion = project.scala.flatMap(_.version).map(_.scalaVersion).getOrElse(model.VersionScala.Scala3.scalaVersion)
            val config = bleep.analysis.ScalaJsLinkConfig.Debug
            Some(crossName -> TaskDag.LinkPlatform.ScalaJs(sjsVersion, scalaVersion, config))

          case (Some(model.PlatformId.Native), true) =>
            // Kotlin/Native - test project
            val config = TaskDag.KotlinNativeConfig(
              target = "host",
              debugInfo = false,
              optimizations = false,
              isTest = true
            )
            Some(crossName -> TaskDag.LinkPlatform.KotlinNative(kotlinVersion, config))

          case (Some(model.PlatformId.Native), false) =>
            // Scala Native - test project
            val snVersion =
              project.platform.flatMap(_.nativeVersion).map(_.scalaNativeVersion).getOrElse(model.VersionScalaNative.ScalaNative05.scalaNativeVersion)
            val scalaVersion = project.scala.flatMap(_.version).map(_.scalaVersion).getOrElse(model.VersionScala.Scala3.scalaVersion)
            val config = bleep.analysis.ScalaNativeLinkConfig.Debug
            Some(crossName -> TaskDag.LinkPlatform.ScalaNative(snVersion, scalaVersion, config))

          case _ =>
            // JVM - no linking needed
            None
        }
      }.toMap

      val cliArgs = params.arguments.getOrElse(List.empty)
      val noCache = cliArgs.contains("--no-cache")
      val cacheContext = RemoteCacheContext.create(started, noCache, makeCacheEventSink(params.originId))

      // Build the unified DAG with platforms
      val initialDag = TaskDag.buildTestDag(testProjects, allProjectDeps, platforms, withCache = cacheContext.isEnabled)
      debugLog(
        s"Built test DAG with ${initialDag.tasks.size} tasks, platforms: ${platforms.keys.map(_.value).mkString(", ")}, cache=${cacheContext.isEnabled}"
      )

      // Parse test options from params
      val testOptions = (params.dataKind, params.data) match {
        case (Some(BleepBspProtocol.TestOptionsDataKind), Some(data)) =>
          // data.toString gives raw JSON bytes as string.
          // If data was sent as a JSON string (double-encoded), unwrap it first.
          val raw = data.toString.trim
          val json =
            if (raw.startsWith("\""))
              io.circe.parser.parse(raw).flatMap(_.as[String]).getOrElse(raw)
            else
              raw
          BleepBspProtocol.TestOptions.decode(json) match {
            case Right(opts) => opts
            case Left(err)   =>
              logger.withContext("error", err.getMessage).withContext("raw", raw.take(200)).warn("Failed to decode TestOptions")
              BleepBspProtocol.TestOptions.empty
          }
        case _ =>
          BleepBspProtocol.TestOptions.empty
      }

      // Create event queue for streaming test events
      val idleTimeout = serverConfig.effectiveTestIdleTimeoutMinutes.minutes
      debugLog(s"BSP config: parallelism=$maxParallelism, idleTimeout=${idleTimeout.toMinutes}m")
      if (testOptions.jvmOptions.nonEmpty || testOptions.testArgs.nonEmpty) {
        debugLog(s"Test options: jvmOptions=${testOptions.jvmOptions}, testArgs=${testOptions.testArgs}")
      }
      if (testOptions.only.nonEmpty) {
        debugLog(s"Test filter --only: ${testOptions.only.mkString(", ")}")
      }
      if (testOptions.exclude.nonEmpty) {
        debugLog(s"Test filter --exclude: ${testOptions.exclude.mkString(", ")}")
      }

      // Create trace recorder (noop if not enabled)
      val traceRecorder = if (testOptions.flamegraph) TraceRecorder.create.unsafeRunSync() else TraceRecorder.noop

      val startTime = System.currentTimeMillis()

      val ioProgram = for {
        eventQueue <- Queue.bounded[IO, Option[TaskDag.DagEvent]](100000)
        totalSuitesRef <- Ref.of[IO, Int](0)
        totalPassedRef <- Ref.of[IO, Int](0)
        totalFailedRef <- Ref.of[IO, Int](0)
        totalSkippedRef <- Ref.of[IO, Int](0)
        totalIgnoredRef <- Ref.of[IO, Int](0)

        // Create kill signal from cancellation token
        killSignal <- Outcome.fromCancellationToken(cancellation)

        // Create JVM pool for test execution
        testResult <- JvmPool.create(maxParallelism, started.jvmCommand, started.buildPaths.buildDir).use { jvmPool =>
          val compileHandler = makeCompileHandler(started, workspace, params.originId, serverConfig.effectiveHeapPressureThreshold, cacheContext)
          val (cachePullHandler, cachePushHandler) = RemoteCacheContext.handlers(cacheContext)

          val discoverHandler: (TaskDag.DiscoverTask, Deferred[IO, Outcome.KillReason]) => IO[(TaskDag.TaskResult, List[(String, String)])] =
            (discoverTask, _) =>
              discoverTestSuites(started, discoverTask.project).map { case (result, suites) =>
                val filtered = filterSuites(suites, testOptions.only, testOptions.exclude)
                if (testOptions.only.nonEmpty && filtered.isEmpty) {
                  val msg = if (suites.nonEmpty) {
                    val available = suites.map(_._1).mkString(", ")
                    s"--only ${testOptions.only.mkString(", ")} matched no test suites in ${discoverTask.project.value}. Available suites: $available"
                  } else {
                    s"--only ${testOptions.only.mkString(", ")} matched no test suites in ${discoverTask.project.value}. No test suites were discovered"
                  }
                  (TaskDag.TaskResult.Failure(msg, Nil), Nil)
                } else {
                  (result, filtered)
                }
              }

          val testHandler: (TaskDag.TestSuiteTask, Deferred[IO, Outcome.KillReason]) => IO[TaskDag.TaskResult] = (testTask, taskKillSignal) => {
            val classpath = getTestClasspath(started, testTask.project)
            val project = started.build.explodedProjects(testTask.project)
            val projectPlatform = project.platform.flatMap(_.name)
            val isKotlin = project.kotlin.flatMap(_.version).isDefined

            (projectPlatform, isKotlin) match {
              case (Some(model.PlatformId.Js), true) =>
                runKotlinJsTestSuite(started, testTask, classpath, eventQueue, taskKillSignal)
              case (Some(model.PlatformId.Js), false) =>
                runScalaJsTestSuite(started, testTask, classpath, eventQueue, taskKillSignal)
              case (Some(model.PlatformId.Native), true) =>
                runKotlinNativeTestSuite(started, testTask, classpath, eventQueue, taskKillSignal)
              case (Some(model.PlatformId.Native), false) =>
                runScalaNativeTestSuite(started, testTask, classpath, eventQueue, taskKillSignal)
              case _ =>
                // JVM (default) - use JvmPool
                val testEnv = computeTestEnvironment(started, testTask.project)
                val projectDir =
                  started.build.explodedProjects.get(testTask.project).flatMap(_.folder).map(rp => started.buildPaths.buildDir.resolve(rp.toString))
                // Project-level JVM options from platform config (e.g. -Djava.util.logging.manager for Quarkus)
                val projectJvmOptions = started.resolvedProject(testTask.project).platform match {
                  case Some(p: ResolvedProject.Platform.Jvm) => p.options
                  case _                                     => Nil
                }
                TestRunner.runSuite(
                  project = testTask.project,
                  suiteName = testTask.suiteName.value,
                  framework = testTask.framework,
                  classpath = classpath,
                  pool = jvmPool,
                  eventQueue = eventQueue,
                  options = TestRunner.Options(
                    jvmOptions = serverConfig.testRunnerMaxMemory.map(m => s"-Xmx$m").toList ++ projectJvmOptions ++ testOptions.jvmOptions,
                    testArgs = testOptions.testArgs,
                    idleTimeout = idleTimeout,
                    environment = testEnv,
                    workingDirectory = projectDir
                  ),
                  killSignal = taskKillSignal
                )
            }
          }

          // Link handler for non-JVM platforms (Scala.js, Scala Native, Kotlin/JS, Kotlin/Native)
          val linkHandler: (TaskDag.LinkTask, Deferred[IO, Outcome.KillReason]) => IO[(TaskDag.TaskResult, TaskDag.LinkResult)] =
            (linkTask, killSignal) => {
              val projectPaths = started.projectPaths(linkTask.project)
              val classpath = getTestClasspath(started, linkTask.project)
              val logger = createLinkLogger()
              val outputDir = projectPaths.targetDir
              LinkExecutor.execute(linkTask, classpath.map(_.toAbsolutePath), None, outputDir, logger, killSignal)
            }

          // Create executor with link support
          val executor = TaskDag.executor(compileHandler, linkHandler, discoverHandler, testHandler, cachePullHandler, cachePushHandler)

          // Run event consumer in background (auto-cancels when scope exits)
          // This ensures the fiber is cleaned up even if the request is cancelled
          for {
            _ <- IO {
              logger.withContext("sendEventCounter", sendEventCounter.get()).warn("Starting event consumer and task executor")
            }
            // Start event consumer fiber - use guarantee to ensure cleanup on cancellation/error
            consumerErrorRef <- Ref.of[IO, Option[Throwable]](None)
            eventConsumerFiber <- consumeEvents(
              eventQueue,
              params.originId,
              totalSuitesRef,
              totalPassedRef,
              totalFailedRef,
              totalSkippedRef,
              totalIgnoredRef,
              killSignal,
              traceRecorder
            ).compile.drain.handleErrorWith { e =>
              // Capture consumer error for later inspection
              IO(logger.withContext("error", e.getMessage).error("Test event consumer error")) >>
                consumerErrorRef.set(Some(e))
            }.start

            // Run executor with guarantee to cancel consumer fiber on completion/error/cancellation
            dag <- executor
              .execute(initialDag, maxParallelism, eventQueue, killSignal)
              .flatMap { result =>
                IO {
                  val total = result.tasks.size
                  val completed = result.completed.size
                  val failed = result.failed.size
                  val errored = result.errored.size
                  val skipped = result.skipped.size
                  val killed = result.killed.size
                  val timedOut = result.timedOut.size
                  logger
                    .withContext("total", total)
                    .withContext("completed", completed)
                    .withContext("failed", failed)
                    .withContext("errored", errored)
                    .withContext("skipped", skipped)
                    .withContext("killed", killed)
                    .withContext("timedOut", timedOut)
                    .info("Task executor completed")
                  if (result.killed.nonEmpty) {
                    logger.withContext("tasks", result.killed.mkString(", ")).warn("Killed tasks")
                  }
                  if (result.failed.nonEmpty) {
                    logger.withContext("tasks", result.failed.mkString(", ")).warn("Failed tasks")
                  }
                  if (result.errored.nonEmpty) {
                    logger.withContext("tasks", result.errored.mkString(", ")).warn("Errored tasks")
                  }
                  if (result.skipped.nonEmpty) {
                    logger.withContext("tasks", result.skipped.mkString(", ")).warn("Skipped tasks")
                  }
                  if (cancellation.isCancelled) {
                    logger.warn("Cancellation token was triggered during test execution!")
                  }
                } >> IO {
                  logger
                    .withContext("sendEventCounter", sendEventCounter.get())
                    .withContext("cancelled", cancellation.isCancelled.toString)
                    .warn("Executor done, signalling consumer to terminate")
                } >>
                  eventQueue.offer(None) >>
                  eventConsumerFiber.joinWithNever >>
                  IO.pure(result)
              }
              .guarantee(eventQueue.offer(None).attempt >> eventConsumerFiber.cancel)

            // Log consumer errors but don't fail the build — test/compile results are still valid
            consumerError <- consumerErrorRef.get
            _ <- consumerError match {
              case Some(e) =>
                IO(logger.withContext("error", e.getMessage).warn("Event consumer failed (build results still valid)"))
              case None => IO.unit
            }
          } yield dag
        }
        passed <- totalPassedRef.get
        failed <- totalFailedRef.get
        skipped <- totalSkippedRef.get
        ignored <- totalIgnoredRef.get
        suites <- totalSuitesRef.get
      } yield (testResult, passed, failed, skipped, ignored, suites)

      logger.withContext("projects", testProjects.map(_.value).mkString(", ")).warn("handleTest: starting ioProgram")
      val ioResult = Try(ioProgram.unsafeRunSync())
      logger.withContext("sendEventCounter", sendEventCounter.get()).warn("handleTest: ioProgram returned")

      // Write trace file if flamegraph is enabled
      if (testOptions.flamegraph) {
        val tracePath = started.buildPaths.dotBleepDir.resolve("trace.json")
        traceRecorder.writeTrace(tracePath).unsafeRunSync()
      }

      ioResult match {
        case Success((result, totalPassed, totalFailed, totalSkipped, totalIgnored, totalSuites)) =>
          // Send TestRunFinished event
          val durationMs = System.currentTimeMillis() - startTime
          val timestamp = System.currentTimeMillis()
          sendTestEvent(
            params.originId,
            "test-run",
            BleepBspProtocol.Event.TestRunFinished(totalPassed, totalFailed, totalSkipped, totalIgnored, durationMs, timestamp)
          )

          // Determine final status
          val statusCode =
            if (cancellation.isCancelled) StatusCode.Cancelled
            else if (result.failed.nonEmpty) StatusCode.Error
            else StatusCode.Ok

          // Compute suite-level counts from DAG result
          val suiteTaskIds = result.tasks.collect { case (id, _: TaskDag.TestSuiteTask) => id }.toSet
          val suitesCompleted = suiteTaskIds.count(id => result.completed.contains(id) || result.failed.contains(id) || result.timedOut.contains(id))
          val suitesFailed = suiteTaskIds.count(id => result.failed.contains(id) || result.errored.contains(id))
          val suitesCancelled = suiteTaskIds.count(id => result.killed.contains(id) || result.skipped.contains(id))

          bspInfo(s"Test completed: $totalPassed passed, $totalFailed failed, $totalSkipped skipped (${durationMs}ms)")

          // Include authoritative test results in TestResult.data for reliable delivery
          val runResult = BleepBspProtocol.TestRunResult(
            totalPassed = totalPassed,
            totalFailed = totalFailed,
            totalSkipped = totalSkipped,
            totalIgnored = totalIgnored,
            suitesTotal = totalSuites,
            suitesCompleted = suitesCompleted,
            suitesFailed = suitesFailed,
            suitesCancelled = suitesCancelled,
            durationMs = durationMs
          )

          TestResult(
            originId = params.originId,
            statusCode = statusCode,
            dataKind = Some(BleepBspProtocol.TestRunResultDataKind),
            data = Some(RawJson(BleepBspProtocol.TestRunResult.encode(runResult).getBytes("UTF-8")))
          )

        case Failure(ex) =>
          val durationMs = System.currentTimeMillis() - startTime
          val timestamp = System.currentTimeMillis()
          System.err.println(s"[BSP] Test execution failed: ${ex.getMessage}")
          ex.printStackTrace(System.err)

          // Try to notify the client about the failure.
          // If the connection is dead (which may have caused the failure), these sends will
          // fail - that's fine, we just return the error TestResult.
          try {
            sendTestEvent(
              params.originId,
              "error",
              BleepBspProtocol.Event.Error(
                message = s"Test execution failed: ${ex.getMessage}",
                details = Some(ex.getStackTrace.take(10).mkString("\n")),
                timestamp = timestamp
              )
            )
            sendTestEvent(
              params.originId,
              "test-run",
              BleepBspProtocol.Event.TestRunFinished(0, 0, 0, 0, durationMs, timestamp)
            )
          } catch {
            case _: java.io.IOException =>
              System.err.println("[BSP] Client disconnected, cannot send test failure notification")
            case e: Exception =>
              System.err.println(s"[BSP] Failed to send test failure notification: ${e.getMessage}")
          }

          // Include zero-valued authoritative result even on failure
          val failRunResult = BleepBspProtocol.TestRunResult(
            totalPassed = 0,
            totalFailed = 0,
            totalSkipped = 0,
            totalIgnored = 0,
            suitesTotal = 0,
            suitesCompleted = 0,
            suitesFailed = 0,
            suitesCancelled = 0,
            durationMs = durationMs
          )

          TestResult(
            originId = params.originId,
            statusCode = StatusCode.Error,
            dataKind = Some(BleepBspProtocol.TestRunResultDataKind),
            data = Some(RawJson(BleepBspProtocol.TestRunResult.encode(failRunResult).getBytes("UTF-8")))
          )
      }
    } finally unregisterOperation(workspace, taskId)
  }

  /** Compute dependency analysis file paths for a project's compile-time dependencies.
    *
    * Returns a map from each dependency's output directory to its Zinc analysis file. This is needed for Zinc to detect API changes in upstream projects and
    * invalidate downstream classes accordingly. Without this, Zinc treats each project as having no dependencies, missing cross-project invalidation entirely.
    */
  private def computeDependencyAnalyses(started: Started, projectDeps: Set[CrossProjectName]): Map[Path, Path] =
    projectDeps.flatMap { dep =>
      val depOutputDir = java.nio.file.Paths.get(started.resolvedProject(dep).classesDir.toString)
      val depTargetDir = started.buildPaths.bleepBloopDir.resolve(dep.name.value).resolve(dep.crossId.fold("")(_.value))
      val depAnalysisFile = depTargetDir.resolve(".zinc").resolve("analysis.zip")
      if (java.nio.file.Files.exists(depAnalysisFile)) Some(depOutputDir -> depAnalysisFile)
      else None
    }.toMap

  /** Create a compile handler for use in DAG executors.
    *
    * Uses IO.race to race compilation against the kill signal. When the kill signal wins, IO.race cancels the compile fiber. Since ZincBridge uses
    * IO.interruptible, CE interrupts the compilation thread immediately.
    *
    * Shared between handleCompile and handleTest to eliminate duplicate compile handler definitions.
    */
  private def makeCompileHandler(
      started: Started,
      workspace: Path,
      originId: Option[String],
      heapPressureThreshold: Double,
      cacheContext: RemoteCacheContext
  ): (TaskDag.CompileTask, Deferred[IO, Outcome.KillReason]) => IO[TaskDag.TaskResult] =
    (compileTask, taskKillSignal) => {
      val projectName = compileTask.project.value
      val wsStr = workspace.toString
      val token = CancellationToken.create()
      taskKillSignal.tryGet.flatMap {
        case Some(_) => IO.pure(TaskDag.TaskResult.Killed(Outcome.KillReason.UserRequest))
        case None    =>
          cacheContext.wasHit(compileTask.project).flatMap { hit =>
            if (hit) {
              // The preceding CachePullTask extracted classes + Zinc analysis into targetDir. Skip compilation entirely.
              IO.pure(TaskDag.TaskResult.Success)
            } else {
              // Fast path: check noop manifest BEFORE acquiring semaphore / heap gate.
              // Noop projects skip all waiting and don't consume concurrency slots.
              val config = BleepBuildConverter.toProjectConfig(compileTask.project, started.resolvedProject(compileTask.project), started)
              val depAnalyses = computeDependencyAnalyses(started, compileTask.projectDependencies)
              val noopResult = config.language match {
                case sl: ProjectLanguage.ScalaJava => ZincBridge.isNoop(config, sl, depAnalyses, None)
                case _                             => None
              }
              if (noopResult.isDefined) {
                IO.pure(TaskDag.TaskResult.Success)
              } else {
                // Cooperative cancellation: set CancellationToken so advance() returns false
                val cooperativeCancelIO = taskKillSignal.get.flatMap(_ => IO(token.cancel())).start

                // Gate on server-wide semaphore to limit total concurrent compiles across all connections
                val gatedCompile = IO
                  .interruptible(compileSemaphore.acquire())
                  .bracket { _ =>
                    IO(activeCompileCount.incrementAndGet()) >>
                      waitForHeapPressure(projectName, originId, heapPressureThreshold) >> {
                        val compileStartTime = System.currentTimeMillis()
                        IO(BspMetrics.recordCompileStart(projectName, wsStr)) >>
                          compileProject(started, compileTask.project, originId, token, depAnalyses)
                            .guaranteeCase {
                              case cats.effect.Outcome.Succeeded(resultIO) =>
                                resultIO.flatMap { result =>
                                  val dur = System.currentTimeMillis() - compileStartTime
                                  val ok = result == TaskDag.TaskResult.Success
                                  IO(BspMetrics.recordCompileEnd(projectName, wsStr, dur, ok))
                                }
                              case _ =>
                                IO(BspMetrics.recordCompileEnd(projectName, wsStr, System.currentTimeMillis() - compileStartTime, false))
                            }
                      }
                  }(_ => IO(activeCompileCount.decrementAndGet()) >> IO(compileSemaphore.release()))
                val waitForKill = taskKillSignal.get.map(reason => TaskDag.TaskResult.Killed(reason))

                cooperativeCancelIO.flatMap { cancelFiber =>
                  IO.race(gatedCompile, waitForKill).map(_.merge).guarantee(cancelFiber.cancel)
                }
              }
            }
          }
      }
    }

  // Cache handler construction lives in RemoteCacheContext.handlers.

  /** Compile a single project (dependencies handled by TaskDag ordering).
    *
    * Calls the compiler directly (no ParallelProjectCompiler) so that CE fiber cancellation propagates through IO.interruptible in ZincBridge. This enables
    * IO.race in the compile handler to immediately interrupt compilation when the kill signal fires.
    */
  private def compileProject(
      started: Started,
      project: CrossProjectName,
      originId: Option[String],
      cancellation: CancellationToken,
      dependencyAnalyses: Map[Path, Path]
  ): IO[TaskDag.TaskResult] = {
    val config = BleepBuildConverter.toProjectConfig(project, started.resolvedProject(project), started)
    val compiler = ProjectCompiler.forLanguage(config.language)

    val diagnosticListener = new DiagnosticListener {
      def onDiagnostic(error: CompilerError): Unit = {
        // Suppress error diagnostics after cancellation — Zinc may emit spurious errors
        // (e.g. "duplicate class") when other compilations were cancelled mid-flight
        if (cancellation.isCancelled && error.severity == CompilerError.Severity.Error) return

        val diagnostic = Diagnostic(
          range = Range(
            start = Position(line = math.max(0, error.line - 1), character = math.max(0, error.column - 1)),
            end = Position(line = math.max(0, error.line - 1), character = error.column)
          ),
          severity = Some(error.severity match {
            case CompilerError.Severity.Error   => DiagnosticSeverity.Error
            case CompilerError.Severity.Warning => DiagnosticSeverity.Warning
            case CompilerError.Severity.Info    => DiagnosticSeverity.Information
          }),
          code = None,
          codeDescription = None,
          source = Some("bleep"),
          message = error.message,
          tags = None,
          relatedInformation = None,
          dataKind = None,
          data = None
        )

        val targetId = buildTargetId(started.buildPaths, project)
        val textDocument = error.path.map(p => TextDocumentIdentifier(Uri(p.toUri)))

        val docId = textDocument.getOrElse(TextDocumentIdentifier(Uri(java.net.URI.create("unknown"))))
        val reset = diagnosticTracker.recordDiagnostic(docId.uri.value, targetId.uri.value)
        val publishParams = PublishDiagnosticsParams(
          textDocument = docId,
          buildTarget = targetId,
          originId = originId,
          diagnostics = List(diagnostic),
          reset = reset
        )

        sendNotification("build/publishDiagnostics", publishParams)
      }

      override def onCompilePhase(projectName: String, phase: CompilePhase): Unit = {
        val trackedApis = phase match {
          case CompilePhase.ReadingAnalysis(n) => n
          case _                               => 0
        }
        BspMetrics.recordCompilePhase(projectName, phase.name, trackedApis)
        sendEvent(
          originId,
          s"compile:$projectName",
          BleepBspProtocol.Event
            .CompilePhaseChanged(
              CrossProjectName.fromString(projectName).get,
              bleep.bsp.protocol.CompilePhase.fromString(phase.name),
              trackedApis,
              System.currentTimeMillis()
            )
        )
      }

      override def onCompilationReason(projectName: String, reason: CompilationReason): Unit = {
        def depName(path: java.nio.file.Path): String = {
          val fileName = path.getFileName.toString
          if (fileName == "classes" || fileName == "test-classes") {
            val parent = path.getParent
            if (parent != null) parent.getFileName.toString else fileName
          } else {
            fileName
          }
        }

        val (reasonType, totalFiles, invalidatedFiles, changedDeps) = reason match {
          case CompilationReason.CleanBuild =>
            (bleep.bsp.protocol.CompileReason.CleanBuild, 0, Nil, Nil)
          case CompilationReason.EmptyOutput =>
            (bleep.bsp.protocol.CompileReason.EmptyOutput, 0, Nil, Nil)
          case CompilationReason.UpToDate =>
            (bleep.bsp.protocol.CompileReason.UpToDate, 0, Nil, Nil)
          case CompilationReason.Incremental(total, invalidated, changed) =>
            (bleep.bsp.protocol.CompileReason.Incremental, total, invalidated.map(_.getFileName.toString).toList, changed.map(depName).toList)
        }
        val now = System.currentTimeMillis()
        sendEvent(
          originId,
          s"compile:$projectName",
          BleepBspProtocol.Event.CompilationReason(
            project = CrossProjectName.fromString(projectName).get,
            reason = reasonType,
            totalFiles = totalFiles,
            invalidatedFiles = invalidatedFiles,
            changedDependencies = changedDeps,
            timestamp = now
          )
        )
      }
    }

    // Real progress listener that sends CompileProgress events, rate-limited to 1/sec.
    // Progress is monotonic: zinc calls advance() per compiler phase (typer, patmat, erasure, etc.)
    // and current/total reset between phases. We track maxPercent to avoid jumping backwards.
    val progressListener = new ProgressListener {
      @volatile private var lastEmitTime = 0L
      @volatile private var maxPercent = -1
      def onProgress(current: Int, total: Int, phase: String): Boolean = {
        val now = System.currentTimeMillis()
        if (total > 0) {
          val percent = (current.toDouble / total * 100).toInt
          if (percent > maxPercent && now - lastEmitTime > 1000) {
            maxPercent = percent
            lastEmitTime = now
            sendEvent(
              originId,
              s"compile:${project.value}",
              BleepBspProtocol.Event.CompileProgress(project, percent, now)
            )
          }
        }
        !cancellation.isCancelled
      }
    }

    val outputDir = started.projectPaths(project).targetDir / "classes"
    val lockStart = System.currentTimeMillis()
    ProjectLock
      .acquire(
        project,
        outputDir,
        lockTimeout,
        onContention = () =>
          sendEvent(
            originId,
            s"compile:${project.value}",
            BleepBspProtocol.Event.LockContention(project, 0, System.currentTimeMillis())
          )
      )
      .evalTap { hadContention =>
        IO {
          if (hadContention) {
            val waited = System.currentTimeMillis() - lockStart
            sendEvent(
              originId,
              s"compile:${project.value}",
              BleepBspProtocol.Event.LockAcquired(project, waited, System.currentTimeMillis())
            )
          }
        }
      }
      .use { _ =>
        compiler.compile(config, diagnosticListener, cancellation, dependencyAnalyses, progressListener)
      }
      .map {
        case _ if cancellation.isCancelled =>
          TaskDag.TaskResult.Killed(KillReason.UserRequest)
        case _: ProjectCompileSuccess =>
          TaskDag.TaskResult.Success
        case f: ProjectCompileFailure =>
          val errors = f.errors.map(toDiagnostic)
          TaskDag.TaskResult.Failure("Compilation failed", errors)
      }
  }

  /** Discover test suites in a compiled project */
  /** Filter discovered suites by --only and --exclude patterns.
    *
    * Patterns match against either the fully qualified class name or the simple class name (last segment after '.'). --exclude takes precedence over --only.
    */
  private def filterSuites(
      suites: List[(String, String)],
      only: List[String],
      exclude: List[String]
  ): List[(String, String)] = {
    if (only.isEmpty && exclude.isEmpty) return suites

    def simpleName(fqcn: String): String = fqcn.split('.').last

    def matches(suiteName: String, pattern: String): Boolean = {
      val simple = simpleName(suiteName)
      suiteName == pattern || simple == pattern ||
      suiteName.contains(pattern) || simple.contains(pattern)
    }

    suites.filter { case (suiteName, _) =>
      val included = only.isEmpty || only.exists(p => matches(suiteName, p))
      val excluded = exclude.exists(p => matches(suiteName, p))
      included && !excluded
    }
  }

  private def discoverTestSuites(started: Started, project: CrossProjectName): IO[(TaskDag.TaskResult, List[(String, String)])] = IO {
    val projectConfig = started.build.explodedProjects(project)
    val platformOpt = projectConfig.platform.flatMap(_.name)
    val isKotlin = projectConfig.kotlin.flatMap(_.version).isDefined

    // For Kotlin/JS and Kotlin/Native, use synthetic test suite (runtime discovery)
    // For JVM, use classpath scanning
    (platformOpt, isKotlin) match {
      case (Some(model.PlatformId.Js), true) =>
        // Kotlin/JS: return a synthetic suite that will run all tests via Node.js
        val suiteName = s"${project.value}:KotlinJsTests"
        debugLog(s"Discovered Kotlin/JS test project: ${project.value}")
        (TaskDag.TaskResult.Success, List((suiteName, "kotlin-test-js")))

      case (Some(model.PlatformId.Native), true) =>
        // Kotlin/Native: return a synthetic suite that will run all tests via binary
        val suiteName = s"${project.value}:KotlinNativeTests"
        debugLog(s"Discovered Kotlin/Native test project: ${project.value}")
        (TaskDag.TaskResult.Success, List((suiteName, "kotlin-test-native")))

      case _ =>
        // JVM: use classpath scanning
        val projectPaths = started.projectPaths(project)
        val classesDir = projectPaths.classes
        val resolved = started.resolvedProject(project)
        val classpath = resolved.classpath.map(p => Path.of(p.toString)).toList

        val suites = ClasspathTestDiscovery.discover(project, classesDir, classpath)

        if (suites.isEmpty) {
          debugLog(s"No test suites discovered in ${project.value}")
          (TaskDag.TaskResult.Success, Nil)
        } else {
          debugLog(s"Discovered ${suites.size} test suites in ${project.value}: ${suites.map(_.className).mkString(", ")}")
          (TaskDag.TaskResult.Success, suites.map(s => (s.className, s.framework)))
        }
    }
  }

  /** Get classpath for running tests */
  private def getTestClasspath(started: Started, project: CrossProjectName): List[Path] = {
    val projectPaths = started.projectPaths(project)
    val classesDir = projectPaths.classes

    val resolved = started.resolvedProject(project)
    val resourceDirs = resolved.resources.getOrElse(Nil)
    val dependencyClasspath = resolved.classpath.map(p => Path.of(p.toString)).toList

    // Try to find bleep-test-runner in the current build (when running bleep's own tests),
    // otherwise fetch via coursier
    val testRunnerFromBuild = started.build.explodedProjects.keys
      .find(p => p.name.value == "bleep-test-runner")
      .map(p => started.projectPaths(p).classes)

    val testRunnerClasses = testRunnerFromBuild match {
      case Some(path) => List(path)
      case None       => fetchTestRunnerViaCoursier(started)
    }

    (classesDir :: resourceDirs) ++ dependencyClasspath ++ testRunnerClasses
  }

  /** Fetch bleep-test-runner and its dependencies.
    *
    * Uses `${BLEEP_VERSION}` so the TemplatedVersions resolver handles version rewriting: for dev builds this resolves the test-runner class dir from
    * BleepDevDeps, for release builds it resolves the published JAR from Coursier.
    *
    * For dev builds, BleepDevDeps only returns class dirs (no transitive external deps like test-interface). We always resolve the external deps separately so
    * the forked test JVM has everything it needs.
    */
  @volatile private var cachedTestRunnerJars: List[Path] = _

  private def fetchTestRunnerViaCoursier(started: Started): List[Path] = {
    val cached = cachedTestRunnerJars
    if (cached != null) return cached

    val testRunnerDep = model.Dep.Java("build.bleep", "bleep-test-runner", model.Replacements.known.BleepVersion)
    val externalDeps = Set[model.Dep](
      model.Dep.Java("org.scala-sbt", "test-interface", "1.0"),
      model.Dep.Java("net.aichler", "jupiter-interface", "0.11.1"),
      model.Dep.Java("org.junit.platform", "junit-platform-launcher", "1.9.1"),
      model.Dep.Java("org.junit.vintage", "junit-vintage-engine", "5.9.1")
    )
    val allDeps = externalDeps + testRunnerDep

    val result = started.resolver.force(
      allDeps,
      model.VersionCombo.Jvm(model.VersionScala.Scala3),
      libraryVersionSchemes = SortedSet.empty[model.LibraryVersionScheme],
      context = "resolving bleep-test-runner",
      model.IgnoreEvictionErrors.No
    )

    if (result.jars.isEmpty) {
      throw new RuntimeException("bleep-test-runner resolution returned no jars")
    }

    cachedTestRunnerJars = result.jars
    result.jars
  }

  /** Get environment variables for a test JVM from the model. */
  private def computeTestEnvironment(started: Started, project: CrossProjectName): Map[String, String] =
    started.build.explodedProjects.get(project).flatMap(_.platform).map(_.jvmEnvironment.toMap).getOrElse(Map.empty)

  /** Create a TestEventHandler that offers events to the DAG queue via a Dispatcher.
    *
    * Using a Dispatcher avoids the overhead of `unsafeRunSync()` on every callback (test started, test finished, output line). The Dispatcher amortizes CE3
    * runtime setup across all calls. `unsafeRunSync` is safe here because the queue is unbounded (offer never suspends).
    */
  private def makeTestEventHandler(
      dispatcher: Dispatcher[IO],
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      project: CrossProjectName
  ): TestRunnerTypes.TestEventHandler =
    new TestRunnerTypes.TestEventHandler {
      def onTestStarted(suite: String, test: String): Unit =
        dispatcher.unsafeRunSync(eventQueue.offer(Some(TaskDag.DagEvent.TestStarted(project, SuiteName(suite), TestName(test), System.currentTimeMillis()))))
      def onTestFinished(suite: String, test: String, status: bleep.bsp.protocol.TestStatus, durationMs: Long, message: Option[String]): Unit =
        dispatcher.unsafeRunSync(
          eventQueue.offer(
            Some(TaskDag.DagEvent.TestFinished(project, SuiteName(suite), TestName(test), status, durationMs, message, None, System.currentTimeMillis()))
          )
        )
      def onSuiteStarted(suite: String): Unit = ()
      def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = ()
      def onOutput(suite: String, line: String, channel: OutputChannel): Unit =
        dispatcher.unsafeRunSync(eventQueue.offer(Some(TaskDag.DagEvent.Output(project, SuiteName(suite), line, channel, System.currentTimeMillis()))))
    }

  /** Run a Scala.js test suite: link → run via Node.js, emit events to DAG queue. */
  private def runScalaJsTestSuite(
      started: Started,
      testTask: TaskDag.TestSuiteTask,
      classpath: List[Path],
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      killSignal: Deferred[IO, Outcome.KillReason]
  ): IO[TaskDag.TaskResult] = {
    val project = started.build.explodedProjects(testTask.project)
    val sjsVersion = project.platform.flatMap(_.jsVersion).getOrElse {
      throw new IllegalStateException(s"Scala.js version not found for ${testTask.project.value}")
    }
    val scalaVersion = project.scala.flatMap(_.version).getOrElse {
      throw new IllegalStateException(s"Scala version not found for ${testTask.project.value}")
    }

    val projectPaths = started.projectPaths(testTask.project)
    val outputDir = projectPaths.classes.getParent.resolve("link-output")
    val linkConfig = bleep.analysis.ScalaJsLinkConfig.Debug
    val logger = createLinkLogger()

    val linkTask = TaskDag.LinkTask(
      project = testTask.project,
      platform = TaskDag.LinkPlatform.ScalaJs(sjsVersion.scalaJsVersion, scalaVersion.scalaVersion, linkConfig),
      releaseMode = false,
      isTest = true
    )

    for {
      startTs <- IO.realTime.map(_.toMillis)
      // Note: TaskStarted is already emitted by DAG executor - don't duplicate it here
      linkResult <- LinkExecutor.execute(linkTask, classpath.map(_.toAbsolutePath), None, outputDir, logger, killSignal)
      taskResult <- linkResult match {
        case (TaskDag.TaskResult.Success, TaskDag.LinkResult.JsSuccess(mainModule, _, _, _)) =>
          // Run the specific test suite via Node.js
          Dispatcher.sequential[IO].use { dispatcher =>
            val eventHandler = makeTestEventHandler(dispatcher, eventQueue, testTask.project)
            val suites = List(TestRunnerTypes.TestSuite(testTask.suiteName.value, testTask.suiteName.value))
            ScalaJsTestRunner
              .runTests(mainModule, linkConfig.moduleKind, suites, eventHandler, ScalaJsTestRunner.NodeEnvironment.Node, Map.empty, killSignal)
              .flatMap { result =>
                val endTs = System.currentTimeMillis()
                val durationMs = endTs - startTs
                eventQueue
                  .offer(
                    Some(
                      TaskDag.DagEvent
                        .SuiteFinished(
                          testTask.project,
                          testTask.suiteName,
                          result.passed,
                          result.failed,
                          result.skipped,
                          result.ignored,
                          durationMs,
                          endTs
                        )
                    )
                  )
                  .as(classifyTestResult(result))
              }
          }
        case (result, _) =>
          val endTs = System.currentTimeMillis()
          val durationMs = endTs - startTs
          eventQueue.offer(Some(TaskDag.DagEvent.SuiteFinished(testTask.project, testTask.suiteName, 0, 1, 0, 0, durationMs, endTs))).void >>
            IO.pure(result)
      }
    } yield taskResult
  }

  /** Run a Scala Native test suite: link → run binary, emit events to DAG queue. */
  private def runScalaNativeTestSuite(
      started: Started,
      testTask: TaskDag.TestSuiteTask,
      classpath: List[Path],
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      killSignal: Deferred[IO, Outcome.KillReason]
  ): IO[TaskDag.TaskResult] = {
    val project = started.build.explodedProjects(testTask.project)
    val snVersion = project.platform.flatMap(_.nativeVersion).getOrElse {
      throw new IllegalStateException(s"Scala Native version not found for ${testTask.project.value}")
    }
    val scalaVersion = project.scala.flatMap(_.version).getOrElse {
      throw new IllegalStateException(s"Scala version not found for ${testTask.project.value}")
    }

    val projectPaths = started.projectPaths(testTask.project)
    val outputDir = projectPaths.classes.getParent.resolve("link-output")
    val logger = createLinkLogger()

    val framework = ScalaNativeTestRunner.detectFramework(classpath)
    val testMainClass = ScalaNativeTestRunner.getTestMainClass(framework)
    val toolchain = bleep.analysis.ScalaNativeToolchain.forVersion(snVersion.scalaNativeVersion, scalaVersion.scalaVersion)
    val binaryPath = outputDir.resolve(s"${testTask.project.value}-test")
    val workDir = outputDir.resolve("native-work")

    val nativeLogger = new bleep.analysis.ScalaNativeToolchain.Logger {
      def trace(message: => String): Unit = logger.trace(message)
      def debug(message: => String): Unit = logger.debug(message)
      def info(message: => String): Unit = logger.info(message)
      def warn(message: => String): Unit = logger.warn(message)
      def error(message: => String): Unit = logger.error(message)
      def running(command: Seq[String]): Unit = logger.info(s"Running: ${command.mkString(" ")}")
    }

    for {
      startTs <- IO.realTime.map(_.toMillis)
      // Note: TaskStarted is already emitted by DAG executor - don't duplicate it here
      linkResult <- ScalaNativeTestRunner.linkTestBinary(
        toolchain,
        classpath.map(_.toAbsolutePath),
        testMainClass,
        bleep.analysis.ScalaNativeLinkConfig.Debug,
        binaryPath,
        workDir,
        nativeLogger,
        killSignal
      )
      taskResult <- linkResult match {
        case TaskDag.LinkResult.NativeSuccess(binary, _) =>
          Dispatcher.sequential[IO].use { dispatcher =>
            val eventHandler = makeTestEventHandler(dispatcher, eventQueue, testTask.project)
            val suites = List(TestRunnerTypes.TestSuite(testTask.suiteName.value, testTask.suiteName.value))
            ScalaNativeTestRunner
              .runTestsViaAdapter(binary, suites, framework, eventHandler, Map.empty, started.buildPaths.cwd, snVersion.scalaNativeVersion, killSignal)
              .flatMap { result =>
                val endTs = System.currentTimeMillis()
                val durationMs = endTs - startTs
                // Emit error/crash details as Output so they appear in failure details
                val terminationEvent: IO[Unit] = result.terminationReason match {
                  case TestRunnerTypes.TerminationReason.Error(msg) =>
                    eventQueue.offer(Some(TaskDag.DagEvent.Output(testTask.project, testTask.suiteName, msg, OutputChannel.Stderr, endTs)))
                  case TestRunnerTypes.TerminationReason.Crashed(signal) =>
                    eventQueue.offer(
                      Some(
                        TaskDag.DagEvent.Output(testTask.project, testTask.suiteName, s"Process crashed (signal $signal)", OutputChannel.Stderr, endTs)
                      )
                    )
                  case TestRunnerTypes.TerminationReason.TruncatedOutput(suite) =>
                    eventQueue.offer(
                      Some(
                        TaskDag.DagEvent
                          .Output(
                            testTask.project,
                            testTask.suiteName,
                            s"Process exited with truncated output (suite '$suite')",
                            OutputChannel.Stderr,
                            endTs
                          )
                      )
                    )
                  case TestRunnerTypes.TerminationReason.ExitCode(code) =>
                    eventQueue.offer(
                      Some(TaskDag.DagEvent.Output(testTask.project, testTask.suiteName, s"Process exited with code $code", OutputChannel.Stderr, endTs))
                    )
                  case _ => IO.unit
                }
                terminationEvent >>
                  eventQueue
                    .offer(
                      Some(
                        TaskDag.DagEvent
                          .SuiteFinished(
                            testTask.project,
                            testTask.suiteName,
                            result.passed,
                            result.failed,
                            result.skipped,
                            result.ignored,
                            durationMs,
                            endTs
                          )
                      )
                    )
                    .as(classifyTestResult(result))
              }
          }
        case _ =>
          val endTs = System.currentTimeMillis()
          val durationMs = endTs - startTs
          eventQueue.offer(Some(TaskDag.DagEvent.SuiteFinished(testTask.project, testTask.suiteName, 0, 1, 0, 0, durationMs, endTs))).void >>
            IO.pure(TaskDag.TaskResult.Failure("Native linking failed", List.empty))
      }
    } yield taskResult
  }

  /** Run a Kotlin/JS test suite: discover + run via Node.js, emit events to DAG queue. */
  private def runKotlinJsTestSuite(
      started: Started,
      testTask: TaskDag.TestSuiteTask,
      classpath: List[Path],
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      killSignal: Deferred[IO, Outcome.KillReason]
  ): IO[TaskDag.TaskResult] = {
    val projectPaths = started.projectPaths(testTask.project)
    // Note: JS file uses underscores (linker converts hyphens to underscores in module name)
    val moduleName = testTask.project.value.replace("-", "_")
    // Linked JS output is at targetDir / linkDirSuffix / "js" / moduleName.js
    // Test mode always uses dce=false → linkDirSuffix = "debug"
    val linkSuffix = bleep.bsp.protocol.BleepBspProtocol.linkDirSuffix(isRelease = false, hasDebugInfo = false, hasLto = false)
    val jsOutput = projectPaths.targetDir.resolve(linkSuffix).resolve("js").resolve(s"$moduleName.js")
    val logger = createLinkLogger()

    for {
      startTs <- IO.realTime.map(_.toMillis)
      // Note: TaskStarted is already emitted by DAG executor - don't duplicate it here
      taskResult <-
        if (!Files.exists(jsOutput)) {
          val endTs = System.currentTimeMillis()
          val durationMs = endTs - startTs
          eventQueue.offer(Some(TaskDag.DagEvent.SuiteFinished(testTask.project, testTask.suiteName, 0, 1, 0, 0, durationMs, endTs))).void >>
            IO.pure(TaskDag.TaskResult.Failure(s"Kotlin/JS output not found: $jsOutput", List.empty))
        } else {
          Dispatcher.sequential[IO].use { dispatcher =>
            val eventHandler = makeTestEventHandler(dispatcher, eventQueue, testTask.project)
            val suites = List(TestRunnerTypes.TestSuite(testTask.suiteName.value, testTask.suiteName.value))
            KotlinTestRunner.Js.runTests(jsOutput, suites, eventHandler, Map.empty, killSignal).flatMap { result =>
              val endTs = System.currentTimeMillis()
              val durationMs = endTs - startTs
              eventQueue
                .offer(
                  Some(
                    TaskDag.DagEvent
                      .SuiteFinished(
                        testTask.project,
                        testTask.suiteName,
                        result.passed,
                        result.failed,
                        result.skipped,
                        result.ignored,
                        durationMs,
                        endTs
                      )
                  )
                )
                .as(classifyTestResult(result))
            }
          }
        }
    } yield taskResult
  }

  /** Run a Kotlin/Native test suite: run binary, emit events to DAG queue. */
  private def runKotlinNativeTestSuite(
      started: Started,
      testTask: TaskDag.TestSuiteTask,
      classpath: List[Path],
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      killSignal: Deferred[IO, Outcome.KillReason]
  ): IO[TaskDag.TaskResult] = {
    val projectPaths = started.projectPaths(testTask.project)
    // K/N binary is at targetDir / linkDirSuffix / projectName (or .kexe extension)
    // Test mode always uses optimizations=false, debugInfo=false → linkDirSuffix = "debug"
    val linkSuffix = bleep.bsp.protocol.BleepBspProtocol.linkDirSuffix(isRelease = false, hasDebugInfo = false, hasLto = false)
    val projectName = testTask.project.value
    val linkDir = projectPaths.targetDir.resolve(linkSuffix)
    val possiblePaths = Seq(
      linkDir.resolve(projectName),
      linkDir.resolve(projectName + ".kexe"),
      // Also check legacy paths without linkDirSuffix
      projectPaths.targetDir.resolve(projectName),
      projectPaths.targetDir.resolve(projectName + ".kexe")
    )
    val binary = possiblePaths.find(Files.exists(_)).getOrElse(linkDir.resolve(projectName))
    val logger = createLinkLogger()

    for {
      startTs <- IO.realTime.map(_.toMillis)
      // Note: TaskStarted is already emitted by DAG executor - don't duplicate it here
      taskResult <-
        if (!Files.exists(binary)) {
          val endTs = System.currentTimeMillis()
          val durationMs = endTs - startTs
          eventQueue.offer(Some(TaskDag.DagEvent.SuiteFinished(testTask.project, testTask.suiteName, 0, 1, 0, 0, durationMs, endTs))).void >>
            IO.pure(TaskDag.TaskResult.Failure(s"Kotlin/Native binary not found: $binary", List.empty))
        } else {
          Dispatcher.sequential[IO].use { dispatcher =>
            val eventHandler = makeTestEventHandler(dispatcher, eventQueue, testTask.project)
            // Run all tests in the binary - passing an empty list means no filter
            // This is safer than trying to match synthetic suite names to actual test classes
            KotlinTestRunner.Native.runTests(binary, List.empty, eventHandler, Map.empty, started.buildPaths.cwd, killSignal).flatMap { result =>
              val endTs = System.currentTimeMillis()
              val durationMs = endTs - startTs
              eventQueue
                .offer(
                  Some(
                    TaskDag.DagEvent
                      .SuiteFinished(
                        testTask.project,
                        testTask.suiteName,
                        result.passed,
                        result.failed,
                        result.skipped,
                        result.ignored,
                        durationMs,
                        endTs
                      )
                  )
                )
                .as(classifyTestResult(result))
            }
          }
        }
    } yield taskResult
  }

  /** Get trace category and name for a task. */
  private def taskCatName(task: TaskDag.Task): (TraceCategory, String) = task match {
    case ct: TaskDag.CompileTask    => (TraceCategory.Compile, ct.project.value)
    case lt: TaskDag.LinkTask       => (TraceCategory.Link, lt.project.value)
    case dt: TaskDag.DiscoverTask   => (TraceCategory.Discover, dt.project.value)
    case tt: TaskDag.TestSuiteTask  => (TraceCategory.Test, s"${tt.project.value}:${tt.suiteName.value}")
    case cpt: TaskDag.CachePullTask => (TraceCategory.CachePull, cpt.project.value)
    case cpt: TaskDag.CachePushTask => (TraceCategory.CachePush, cpt.project.value)
  }

  /** Convert a compile TaskResult to a CompileFinished protocol event. */
  private def compileTaskFinishedEvent(
      project: CrossProjectName,
      result: TaskDag.TaskResult,
      durationMs: Long,
      timestamp: Long
  ): BleepBspProtocol.Event.CompileFinished =
    result match {
      case TaskDag.TaskResult.Success =>
        BleepBspProtocol.Event.CompileFinished(project, CompileStatus.Success, durationMs, Nil, skippedBecause = None, timestamp)
      case TaskDag.TaskResult.Failure(errorMsg, diags) =>
        val effectiveDiags =
          if (diags.exists(d => d.severity == bleep.bsp.protocol.DiagnosticSeverity.Error && d.message.nonEmpty)) diags
          else List(BleepBspProtocol.Diagnostic.error(errorMsg))
        BleepBspProtocol.Event.CompileFinished(project, CompileStatus.Failed, durationMs, effectiveDiags, skippedBecause = None, timestamp)
      case TaskDag.TaskResult.Error(error, _) =>
        BleepBspProtocol.Event.CompileFinished(
          project,
          CompileStatus.Error,
          durationMs,
          List(BleepBspProtocol.Diagnostic.error(error)),
          skippedBecause = None,
          timestamp
        )
      case TaskDag.TaskResult.Skipped(failedDep) =>
        BleepBspProtocol.Event.CompileFinished(project, CompileStatus.Skipped, durationMs, Nil, skippedBecause = Some(failedDep.project), timestamp)
      case TaskDag.TaskResult.Killed(_) | TaskDag.TaskResult.Cancelled | TaskDag.TaskResult.TimedOut =>
        BleepBspProtocol.Event.CompileFinished(project, CompileStatus.Cancelled, durationMs, Nil, skippedBecause = None, timestamp)
    }

  /** Convert a LinkResult to a LinkFinished protocol event. */
  private def linkFinishedEvent(
      project: CrossProjectName,
      result: TaskDag.LinkResult,
      durationMs: Long,
      timestamp: Long
  ): BleepBspProtocol.Event.LinkFinished = {
    val (success, outputPath, platform, error) = result match {
      case TaskDag.LinkResult.JsSuccess(mainModule, _, _, _) => (true, Some(mainModule.toString), LinkPlatformName.ScalaJs, None)
      case TaskDag.LinkResult.NativeSuccess(binary, _)       => (true, Some(binary.toString), LinkPlatformName.ScalaNative, None)
      case TaskDag.LinkResult.Failure(err, _)                => (false, None, LinkPlatformName.Jvm, Some(err))
      case TaskDag.LinkResult.Killed(reason)                 => (false, None, LinkPlatformName.Jvm, Some(s"Killed: $reason"))
      case TaskDag.LinkResult.NotApplicable                  => (true, None, LinkPlatformName.Jvm, None)
    }
    BleepBspProtocol.Event.LinkFinished(project, success, durationMs, outputPath, timestamp, platform, error)
  }

  /** Process link-specific DagEvents shared between consumeEvents and consumeCompileEvents. */
  private def processLinkEvent(
      event: TaskDag.DagEvent,
      originId: Option[String],
      traceRecorder: TraceRecorder
  ): IO[Unit] = event match {
    case TaskDag.DagEvent.LinkStarted(project, platform, timestamp) =>
      val protocolEvent = BleepBspProtocol.Event.LinkStarted(project, platform, timestamp)
      traceRecorder.recordStart(TraceCategory.Link, project.value) >>
        IO(sendEvent(originId, s"link:${project.value}", protocolEvent))
    case TaskDag.DagEvent.LinkProgress(project, phase, _, timestamp) =>
      val protocolEvent = BleepBspProtocol.Event.LinkProgress(project, phase, timestamp)
      IO(sendEvent(originId, s"link:${project.value}", protocolEvent))
    case TaskDag.DagEvent.LinkFinished(project, result, durationMs, timestamp) =>
      val protocolEvent = linkFinishedEvent(project, result, durationMs, timestamp)
      traceRecorder.recordEnd(TraceCategory.Link, project.value) >>
        IO(sendEvent(originId, s"link:${project.value}", protocolEvent))
    case _ => IO.unit
  }

  /** Wrap event processing with dead-client detection and kill signal propagation. */
  private def withDeadClientDetection(
      killSignal: Deferred[IO, Outcome.KillReason],
      contextLabel: String
  )(processEvent: IO[Unit]): IO[Unit] =
    IO.uncancelable { _ =>
      processEvent >> IO.whenA(clientDisconnected.get()) {
        IO.raiseError(new java.io.IOException("Client disconnected (detected via sendNotification)"))
      }
    }.handleErrorWith {
      case error: java.io.IOException =>
        IO(logger.withContext("error", error.getMessage).error(s"$contextLabel event send failed (connection dead)")) >>
          killSignal.complete(Outcome.KillReason.DeadClient).attempt >> IO.raiseError(error)
      case error =>
        IO(logger.withContext("error", error.getMessage).error(s"$contextLabel event processing failed")) >>
          IO.raiseError(error)
    }

  /** Consume events from the queue and send BSP notifications with rich test data.
    *
    * Uses BleepBspProtocol to send structured events in the BSP data field, allowing TestReactive to reconstruct rich TestEvents for FancyBuildDisplay.
    *
    * If a notification fails to send (connection dead), completes the killSignal to trigger cancellation of all running tasks, then re-raises the error.
    */
  private def consumeEvents(
      queue: Queue[IO, Option[TaskDag.DagEvent]],
      originId: Option[String],
      totalSuitesRef: Ref[IO, Int],
      totalPassedRef: Ref[IO, Int],
      totalFailedRef: Ref[IO, Int],
      totalSkippedRef: Ref[IO, Int],
      totalIgnoredRef: Ref[IO, Int],
      killSignal: Deferred[IO, Outcome.KillReason],
      traceRecorder: TraceRecorder
  ): fs2.Stream[IO, Unit] =
    fs2.Stream.fromQueueNoneTerminated(queue).evalMap { event =>
      def processEvent: IO[Unit] =
        event match {
          case TaskDag.DagEvent.TaskStarted(task, timestamp) =>
            val (cat, name) = taskCatName(task)
            val protocolEvent: Option[BleepBspProtocol.Event] = task match {
              case ct: TaskDag.CompileTask =>
                Some(BleepBspProtocol.Event.CompileStarted(ct.project, timestamp))
              case _: TaskDag.LinkTask =>
                None // Link tasks are not exposed via test protocol
              case dt: TaskDag.DiscoverTask =>
                Some(BleepBspProtocol.Event.DiscoveryStarted(dt.project, timestamp))
              case tt: TaskDag.TestSuiteTask =>
                Some(BleepBspProtocol.Event.SuiteStarted(tt.project, tt.suiteName, timestamp))
              case _: TaskDag.CachePullTask | _: TaskDag.CachePushTask =>
                None // Cache events emitted directly via the event sink in RemoteCacheContext
            }
            traceRecorder.recordStart(cat, name) >>
              IO(protocolEvent.foreach(e => sendTestEvent(originId, task.id.value, e)))

          case TaskDag.DagEvent.TaskFinished(task, result, durationMs, timestamp) =>
            val (cat, name) = taskCatName(task)
            val protocolEvent: Option[BleepBspProtocol.Event] = task match {
              case ct: TaskDag.CompileTask =>
                Some(compileTaskFinishedEvent(ct.project, result, durationMs, timestamp))

              case _: TaskDag.LinkTask =>
                None // Link tasks are not exposed via test protocol

              case _: TaskDag.CachePullTask | _: TaskDag.CachePushTask =>
                None // Cache events emitted directly via the event sink in RemoteCacheContext

              case dt: TaskDag.DiscoverTask =>
                result match {
                  case TaskDag.TaskResult.Failure(msg, _) =>
                    Some(BleepBspProtocol.Event.Error(msg, None, timestamp))
                  case _ =>
                    None // Discovery success is handled by SuitesDiscovered event
                }

              case tt: TaskDag.TestSuiteTask =>
                result match {
                  case TaskDag.TaskResult.Success =>
                    None // SuiteFinished already emitted by TestRunner
                  case TaskDag.TaskResult.Failure(errorMsg, _) =>
                    Some(BleepBspProtocol.Event.SuiteError(tt.project, tt.suiteName, errorMsg, ProcessExit.Unknown, durationMs, timestamp))
                  case TaskDag.TaskResult.Error(error, processExit) =>
                    val desc = processExit match {
                      case ProcessExit.Signal(sig)    => s"Process crashed (signal $sig)"
                      case ProcessExit.ExitCode(code) => s"Process exited with code $code"
                      case ProcessExit.Unknown        => error
                    }
                    Some(BleepBspProtocol.Event.SuiteError(tt.project, tt.suiteName, desc, processExit, durationMs, timestamp))
                  case TaskDag.TaskResult.Skipped(failedDep) =>
                    Some(
                      BleepBspProtocol.Event.SuiteCancelled(tt.project, tt.suiteName, Some(s"dependency ${failedDep.project.value} failed"), timestamp)
                    )
                  case TaskDag.TaskResult.Killed(_) =>
                    Some(BleepBspProtocol.Event.SuiteCancelled(tt.project, tt.suiteName, Some("killed"), timestamp))
                  case TaskDag.TaskResult.Cancelled =>
                    Some(BleepBspProtocol.Event.SuiteCancelled(tt.project, tt.suiteName, Some("cancelled"), timestamp))
                  case TaskDag.TaskResult.TimedOut =>
                    Some(BleepBspProtocol.Event.SuiteTimedOut(tt.project, tt.suiteName, durationMs, None, timestamp))
                }
            }
            val failureRefUpdate = (task, result) match {
              case (_: TaskDag.TestSuiteTask, _: TaskDag.TaskResult.Failure) =>
                totalFailedRef.update(n => math.max(n, 1))
              case _ =>
                IO.unit
            }
            traceRecorder.recordEnd(cat, name) >> failureRefUpdate >>
              IO(protocolEvent.foreach(e => sendTestEvent(originId, task.id.value, e)))

          case TaskDag.DagEvent.TestStarted(project, suite, test, timestamp) =>
            val protocolEvent = BleepBspProtocol.Event.TestStarted(project, suite, test, timestamp)
            IO(sendTestEvent(originId, s"test:$project:$suite", protocolEvent))

          case TaskDag.DagEvent.TestFinished(project, suite, test, status, durationMs, message, throwable, timestamp) =>
            IO(
              sendTestEvent(
                originId,
                s"test:$project:$suite",
                BleepBspProtocol.Event.TestFinished(project, suite, test, status, durationMs, message, throwable, timestamp)
              )
            )

          case TaskDag.DagEvent.SuitesDiscovered(project, suites, timestamp) =>
            for {
              total <- totalSuitesRef.updateAndGet(_ + suites.size)
              _ <- IO(sendTestEvent(originId, s"discover:$project", BleepBspProtocol.Event.SuitesDiscovered(project, suites, total, timestamp)))
            } yield ()

          case TaskDag.DagEvent.TaskProgress(task, percent, timestamp) =>
            task match {
              case ct: TaskDag.CompileTask =>
                IO(sendTestEvent(originId, task.id.value, BleepBspProtocol.Event.CompileProgress(ct.project, percent, timestamp)))
              case _ =>
                IO.unit
            }

          case TaskDag.DagEvent.Output(project, suite, line, channel, timestamp) =>
            IO(sendTestEvent(originId, s"output:$project:$suite", BleepBspProtocol.Event.Output(project, suite, line, channel, timestamp)))

          case TaskDag.DagEvent.SuiteFinished(project, suite, passed, failed, skipped, ignored, durationMs, timestamp) =>
            val protocolEvent = BleepBspProtocol.Event.SuiteFinished(project, suite, passed, failed, skipped, ignored, durationMs, timestamp)
            totalPassedRef.update(_ + passed) >>
              totalFailedRef.update(_ + failed) >>
              totalSkippedRef.update(_ + skipped) >>
              totalIgnoredRef.update(_ + ignored) >>
              IO(sendTestEvent(originId, s"suite:$project:$suite", protocolEvent))

          case linkEvent: TaskDag.DagEvent.LinkStarted  => processLinkEvent(linkEvent, originId, traceRecorder)
          case linkEvent: TaskDag.DagEvent.LinkProgress => processLinkEvent(linkEvent, originId, traceRecorder)
          case linkEvent: TaskDag.DagEvent.LinkFinished => processLinkEvent(linkEvent, originId, traceRecorder)
        }

      withDeadClientDetection(killSignal, "Test")(processEvent)
    }

  /** Consume compile/link events only (no test suite tracking).
    *
    * This is a simpler version of consumeEvents for compile-only and compile+link operations. It handles TaskStarted/TaskFinished for CompileTask and LinkTask,
    * as well as Link-specific events.
    */
  private def consumeCompileEvents(
      queue: Queue[IO, Option[TaskDag.DagEvent]],
      originId: Option[String],
      killSignal: Deferred[IO, Outcome.KillReason],
      traceRecorder: TraceRecorder
  ): fs2.Stream[IO, Unit] =
    fs2.Stream.fromQueueNoneTerminated(queue).evalMap { event =>
      def processEvent: IO[Unit] = event match {
        case TaskDag.DagEvent.TaskStarted(task, timestamp) =>
          val (cat, name) = taskCatName(task)
          val protocolEvent: Option[BleepBspProtocol.Event] = task match {
            case ct: TaskDag.CompileTask =>
              logger.withContext("project", ct.project.value).info("Compile starting")
              Some(BleepBspProtocol.Event.CompileStarted(ct.project, timestamp))
            case _ => None
          }
          traceRecorder.recordStart(cat, name) >>
            IO(protocolEvent.foreach(e => sendEvent(originId, task.id.value, e)))

        case TaskDag.DagEvent.TaskFinished(task, result, durationMs, timestamp) =>
          val (cat, name) = taskCatName(task)
          val protocolEvent: Option[BleepBspProtocol.Event] = task match {
            case ct: TaskDag.CompileTask =>
              logger.withContext("project", ct.project.value).withContext("durationMs", durationMs).info("Compile finished")
              Some(compileTaskFinishedEvent(ct.project, result, durationMs, timestamp))
            case _ => None
          }
          traceRecorder.recordEnd(cat, name) >>
            IO(protocolEvent.foreach(e => sendEvent(originId, task.id.value, e)))

        case TaskDag.DagEvent.TaskProgress(task, percent, timestamp) =>
          task match {
            case ct: TaskDag.CompileTask =>
              IO(sendEvent(originId, task.id.value, BleepBspProtocol.Event.CompileProgress(ct.project, percent, timestamp)))
            case _ => IO.unit
          }

        case linkEvent: TaskDag.DagEvent.LinkStarted  => processLinkEvent(linkEvent, originId, traceRecorder)
        case linkEvent: TaskDag.DagEvent.LinkProgress => processLinkEvent(linkEvent, originId, traceRecorder)
        case linkEvent: TaskDag.DagEvent.LinkFinished => processLinkEvent(linkEvent, originId, traceRecorder)

        case _ => IO.unit
      }

      withDeadClientDetection(killSignal, "Compile")(processEvent)
    }

  /** Convert a compiler error to a protocol Diagnostic preserving severity */
  private def toDiagnostic(error: CompilerError): BleepBspProtocol.Diagnostic = {
    val pathStr = error.path.map { p =>
      val locPart = (error.line, error.column) match {
        case (0, 0) => ""
        case (l, 0) => s":$l"
        case (l, c) => s":$l:$c"
      }
      s"$p$locPart"
    }
    val severity = error.severity match {
      case CompilerError.Severity.Error   => bleep.bsp.protocol.DiagnosticSeverity.Error
      case CompilerError.Severity.Warning => bleep.bsp.protocol.DiagnosticSeverity.Warning
      case CompilerError.Severity.Info    => bleep.bsp.protocol.DiagnosticSeverity.Info
    }
    BleepBspProtocol.Diagnostic(severity, error.message, error.rendered, pathStr)
  }

  private val sendEventCounter = new java.util.concurrent.atomic.AtomicInteger(0)

  /** Send a test event via BSP notification with structured data */
  private def sendTestEvent(originId: Option[String], taskId: String, event: BleepBspProtocol.Event): Unit = {
    import BleepBspProtocol.{Event => E}
    val n = sendEventCounter.incrementAndGet()
    event match {
      case e: E.CompileFinished =>
        logger
          .withContext("n", n)
          .withContext("taskId", taskId)
          .withContext("status", e.status.wireValue)
          .withContext("project", e.project.value)
          .withContext("durationMs", e.durationMs)
          .warn("sendTestEvent: CompileFinished")
      case e: E.CompileStarted =>
        logger.withContext("n", n).withContext("taskId", taskId).withContext("project", e.project.value).warn("sendTestEvent: CompileStarted")
      case e: E.TestFinished =>
        logger
          .withContext("n", n)
          .withContext("taskId", taskId)
          .withContext("status", e.status.wireValue)
          .withContext("project", e.project.value)
          .withContext("suite", e.suite.value)
          .withContext("test", e.test.value)
          .warn("sendTestEvent: TestFinished")
      case e: E.SuiteFinished =>
        logger
          .withContext("n", n)
          .withContext("taskId", taskId)
          .withContext("project", e.project.value)
          .withContext("suite", e.suite.value)
          .withContext("passed", e.passed)
          .withContext("failed", e.failed)
          .warn("sendTestEvent: SuiteFinished")
      case e: E.SuiteError =>
        logger
          .withContext("n", n)
          .withContext("taskId", taskId)
          .withContext("project", e.project.value)
          .withContext("suite", e.suite.value)
          .withContext("error", e.error)
          .warn("sendTestEvent: SuiteError")
      case e: E.SuiteCancelled =>
        logger
          .withContext("n", n)
          .withContext("taskId", taskId)
          .withContext("project", e.project.value)
          .withContext("suite", e.suite.value)
          .warn("sendTestEvent: SuiteCancelled")
      case e: E.SuiteTimedOut =>
        logger
          .withContext("n", n)
          .withContext("taskId", taskId)
          .withContext("project", e.project.value)
          .withContext("suite", e.suite.value)
          .withContext("timeoutMs", e.timeoutMs)
          .warn("sendTestEvent: SuiteTimedOut")
      case _: E.CompileProgress => () // too noisy
      case _                    =>
        logger.withContext("n", n).withContext("taskId", taskId).withContext("event", event.getClass.getSimpleName).warn("sendTestEvent")
    }
    sendEvent(originId, taskId, event)
  }

  /** Send a notification to the client.
    *
    * Notifications are best-effort — a disconnected client should not crash the server or abort compilation. Log the error and continue.
    */
  /** Set to true when a notification send fails with IOException, indicating the client has disconnected. Checked by event consumers to trigger kill signal.
    */
  private val clientDisconnected = new java.util.concurrent.atomic.AtomicBoolean(false)

  private def sendNotification[T](method: String, params: T)(using codec: JsonValueCodec[T]): Unit = {
    val notification = JsonRpcNotification(
      jsonrpc = "2.0",
      method = method,
      params = Some(RawJson(writeToArray(params)))
    )
    try transport.sendNotification(notification)
    catch {
      case e: java.io.IOException =>
        clientDisconnected.set(true)
        logger.withContext("method", method).withContext("error", e.getMessage).error("Failed to send notification (client disconnected)")
      case e: Exception =>
        logger.withContext("method", method).withContext("error", e.getMessage).error("Failed to send notification", e)
    }
  }

  /** Send empty diagnostics with reset=true for files that had errors in the previous compilation but are now clean. */
  private def clearStaleDiagnostics(): Unit =
    diagnosticTracker.filesToClear().foreach { case (docUri, targetUri) =>
      val publishParams = PublishDiagnosticsParams(
        textDocument = TextDocumentIdentifier(Uri(java.net.URI.create(docUri))),
        buildTarget = BuildTargetIdentifier(uri = Uri(java.net.URI.create(targetUri))),
        originId = None,
        diagnostics = Nil,
        reset = true
      )
      sendNotification("build/publishDiagnostics", publishParams)
    }

  /** Create a project-scoped logger that sends BSP log messages */
  private def projectLogger(project: CrossProjectName): BspProjectLogger = new BspProjectLogger {
    def info(message: String): Unit = sendProjectLogMessage(project, message, MessageType.Info)
    def warn(message: String): Unit = sendProjectLogMessage(project, message, MessageType.Warning)
    def error(message: String): Unit = sendProjectLogMessage(project, message, MessageType.Error)
  }

  /** Send a project-scoped log message notification to the client */
  private def sendProjectLogMessage(project: CrossProjectName, message: String, messageType: MessageType): Unit = {
    val params = LogMessageParams(
      `type` = messageType,
      task = Some(TaskId(s"project:${project.value}", None)),
      originId = None,
      message = message
    )
    sendNotification("build/logMessage", params)
  }

  /** Send a log message without project scope (for rare error cases only) */
  private def createLinkLogger(): LinkExecutor.LinkLogger = new LinkExecutor.LinkLogger {
    def trace(message: String): Unit = ()
    def debug(message: String): Unit = ()
    def info(message: String): Unit = sendLogMessage(message, MessageType.Info)
    def warn(message: String): Unit = sendLogMessage(message, MessageType.Warning)
    def error(message: String): Unit = sendLogMessage(message, MessageType.Error)
  }

  /** Classify non-JVM test result into TaskResult, distinguishing test failures from process crashes. */
  private def classifyTestResult(result: TestRunnerTypes.TestResult): TaskDag.TaskResult =
    result.terminationReason match {
      case TestRunnerTypes.TerminationReason.Completed =>
        if (result.failed > 0) TaskDag.TaskResult.Failure(s"${result.failed} test(s) failed", List.empty)
        else TaskDag.TaskResult.Success
      case TestRunnerTypes.TerminationReason.Killed(reason) =>
        TaskDag.TaskResult.Killed(reason)
      case TestRunnerTypes.TerminationReason.Crashed(signal) =>
        TaskDag.TaskResult.Error(s"Process crashed (signal $signal)", ProcessExit.Signal(signal))
      case TestRunnerTypes.TerminationReason.ExitCode(code) =>
        TaskDag.TaskResult.Error(s"Process exited with code $code", ProcessExit.ExitCode(code))
      case TestRunnerTypes.TerminationReason.Error(message) =>
        TaskDag.TaskResult.Error(message, ProcessExit.Unknown)
      case TestRunnerTypes.TerminationReason.TruncatedOutput(suite) =>
        TaskDag.TaskResult.Error(s"Process exited with truncated output (suite '$suite' started but never finished)", ProcessExit.Unknown)
    }

  private def sendLogMessage(message: String, messageType: MessageType): Unit = {
    val params = LogMessageParams(
      `type` = messageType,
      task = None,
      originId = None,
      message = message
    )
    sendNotification("build/logMessage", params)
  }

  private def handleScalacOptions(params: ScalacOptionsParams): ScalacOptionsResult = {
    val items = params.targets.map { targetId =>
      (for {
        started <- getActiveBuild.toOption
        crossName <- crossNameFromTargetId(started, targetId)
        resolved <- started.resolvedProjects.get(crossName)
      } yield {
        val p = resolved.forceGet
        val options = p.language match {
          case s: ResolvedProject.Language.Scala => s.options
          case _                                 => Nil
        }
        val classpath = p.classpath.map(_.toUri.toString)
        val classDir = p.classesDir.toUri.toString
        ScalacOptionsItem(target = targetId, options = options, classpath = classpath, classDirectory = classDir)
      }).getOrElse(
        ScalacOptionsItem(target = targetId, options = List.empty, classpath = List.empty, classDirectory = "")
      )
    }
    ScalacOptionsResult(items)
  }

  private def handleJavacOptions(params: JavacOptionsParams): JavacOptionsResult = {
    val maybePlugin = javaSemanticdbPlugin.get()
    val items = params.targets.map { targetId =>
      (for {
        started <- getActiveBuild.toOption
        crossName <- crossNameFromTargetId(started, targetId)
        resolved <- started.resolvedProjects.get(crossName)
      } yield {
        val p = resolved.forceGet
        val baseOptions = p.language.javaOptions
        val options = maybePlugin match {
          case Some(pluginPath) =>
            val sdOpts = javaSemanticdbOptions(pluginPath, started.buildPaths.buildDir, p.classesDir)
            sdOpts ::: baseOptions
          case None => baseOptions
        }
        val classpath = maybePlugin match {
          case Some(pluginPath) =>
            val pluginUri = pluginPath.toUri.toString
            if (p.classpath.exists(_.toString == pluginPath.toString)) p.classpath.map(_.toUri.toString)
            else pluginUri :: p.classpath.map(_.toUri.toString)
          case None => p.classpath.map(_.toUri.toString)
        }
        val classDir = p.classesDir.toUri.toString
        JavacOptionsItem(target = targetId, options = options, classpath = classpath, classDirectory = classDir)
      }).getOrElse(
        JavacOptionsItem(target = targetId, options = List.empty, classpath = List.empty, classDirectory = "")
      )
    }
    JavacOptionsResult(items)
  }

  private def handleJvmRunEnvironment(params: JvmRunEnvironmentParams): JvmRunEnvironmentResult = {
    val workspace = activeWorkspace.get().getOrElse(throw BspException(JsonRpcErrorCodes.ServerNotInitialized, "No active workspace"))
    val items = params.targets.map { targetId =>
      val classpath = (for {
        started <- getActiveBuild.toOption
        crossName <- crossNameFromTargetId(started, targetId)
        resolved <- started.resolvedProjects.get(crossName)
      } yield resolved.forceGet.classpath.map(_.toUri.toString).toList).getOrElse(List.empty)

      JvmEnvironmentItem(
        target = targetId,
        classpath = classpath,
        jvmOptions = List.empty,
        workingDirectory = workspace.toString,
        environmentVariables = Map.empty,
        mainClasses = None
      )
    }
    JvmRunEnvironmentResult(items)
  }

  private def handleJvmTestEnvironment(params: JvmTestEnvironmentParams): JvmTestEnvironmentResult = {
    val workspace = activeWorkspace.get().getOrElse(throw BspException(JsonRpcErrorCodes.ServerNotInitialized, "No active workspace"))
    val items = params.targets.map { targetId =>
      val classpath = (for {
        started <- getActiveBuild.toOption
        crossName <- crossNameFromTargetId(started, targetId)
        resolved <- started.resolvedProjects.get(crossName)
      } yield resolved.forceGet.classpath.map(_.toUri.toString).toList).getOrElse(List.empty)

      JvmEnvironmentItem(
        target = targetId,
        classpath = classpath,
        jvmOptions = List.empty,
        workingDirectory = workspace.toString,
        environmentVariables = Map.empty,
        mainClasses = None
      )
    }
    JvmTestEnvironmentResult(items)
  }

  private def handleResources(params: ResourcesParams): ResourcesResult = {
    val items = params.targets.map { targetId =>
      val resources = (for {
        started <- getActiveBuild.toOption
        crossName <- crossNameFromTargetId(started, targetId)
        resolved <- started.resolvedProjects.get(crossName)
      } yield resolved.forceGet.resources
        .getOrElse(Nil)
        .map { res =>
          Uri(Paths.get(res.toString).toUri)
        }
        .toList).getOrElse(List.empty)

      ResourcesItem(target = targetId, resources = resources)
    }
    ResourcesResult(items)
  }

  private def handleOutputPaths(params: OutputPathsParams): OutputPathsResult = {
    val items = params.targets.map { targetId =>
      val outputPaths = (for {
        started <- getActiveBuild.toOption
        crossName <- crossNameFromTargetId(started, targetId)
        resolved <- started.resolvedProjects.get(crossName)
      } yield {
        val classesDir = resolved.forceGet.classesDir
        List(
          OutputPathItem(
            uri = Uri(Paths.get(classesDir.toString).toUri),
            kind = OutputPathItemKind.Directory
          )
        )
      }).getOrElse(List.empty)

      OutputPathsItem(target = targetId, outputPaths = outputPaths)
    }
    OutputPathsResult(items)
  }

  private def handleInverseSources(params: InverseSourcesParams): InverseSourcesResult = {
    val textDocumentPath = params.textDocument.uri.toPath

    val targets = getActiveBuild
      .map { started =>
        started.build.explodedProjects.keys.flatMap { crossName =>
          started.resolvedProjects.get(crossName).flatMap { resolved =>
            val sources = resolved.forceGet.sources
            val containsFile = sources.exists { src =>
              val srcPath = Paths.get(src.toString)
              textDocumentPath.startsWith(srcPath)
            }
            if (containsFile) Some(buildTargetId(started.buildPaths, crossName))
            else None
          }
        }.toList
      }
      .fold(_ => List.empty, identity)

    InverseSourcesResult(targets)
  }

  private def handleDependencyModules(params: DependencyModulesParams): DependencyModulesResult = {
    val items = params.targets.map { targetId =>
      val modules = (for {
        started <- getActiveBuild.toOption
        crossName <- crossNameFromTargetId(started, targetId)
        resolved <- started.resolvedProjects.get(crossName)
      } yield
        // Extract module info from classpath JARs
        resolved.forceGet.classpath.flatMap { cp =>
          val path = Paths.get(cp.toString)
          val fileName = path.getFileName.toString
          if (fileName.endsWith(".jar")) {
            // Try to parse artifact info from filename (e.g., cats-core_3-2.9.0.jar)
            val nameWithoutExt = fileName.stripSuffix(".jar")
            Some(
              DependencyModule(
                name = nameWithoutExt,
                version = "",
                dataKind = Some(DependencyModuleDataKind.Maven),
                data = None
              )
            )
          } else None
        }.toList).getOrElse(List.empty)

      DependencyModulesItem(target = targetId, modules = modules)
    }
    DependencyModulesResult(items)
  }

  private def handleJvmCompileClasspath(params: JvmCompileClasspathParams): JvmCompileClasspathResult = {
    val items = params.targets.map { targetId =>
      val classpath = (for {
        started <- getActiveBuild.toOption
        crossName <- crossNameFromTargetId(started, targetId)
        resolved <- started.resolvedProjects.get(crossName)
      } yield resolved.forceGet.classpath.map(_.toUri.toString).toList).getOrElse(List.empty)

      JvmCompileClasspathItem(target = targetId, classpath = classpath)
    }
    JvmCompileClasspathResult(items)
  }

  private def handleCleanCache(params: CleanCacheParams): CleanCacheResult = {
    var cleaned = false
    params.targets.foreach { targetId =>
      for {
        started <- getActiveBuild.toOption
        crossName <- crossNameFromTargetId(started, targetId)
        resolved <- started.resolvedProjects.get(crossName)
      } {
        BspMetrics.recordCleanCache(crossName.value)
        val classesDir = Paths.get(resolved.forceGet.classesDir.toString)
        if (Files.exists(classesDir)) {
          bleep.internal.FileUtils.deleteDirectory(classesDir)
          cleaned = true
        }
        // Also clean analysis dir - use same path structure as BuildPaths.targetDir
        val targetDir = started.buildPaths.bleepBloopDir.resolve(crossName.name.value).resolve(crossName.crossId.fold("")(_.value))
        val analysisDir = targetDir.resolve(".zinc")
        if (Files.exists(analysisDir)) {
          bleep.internal.FileUtils.deleteDirectory(analysisDir)
          cleaned = true
        }
      }
    }
    CleanCacheResult(message = if (cleaned) Some("Cache cleaned") else Some("Nothing to clean"), cleaned = cleaned)
  }

  private def handleScalaMainClasses(params: ScalaMainClassesParams): ScalaMainClassesResult = {
    val items = params.targets.map { targetId =>
      val mainClasses = (for {
        started <- getActiveBuild.toOption
        crossName <- crossNameFromTargetId(started, targetId)
        project <- started.build.explodedProjects.get(crossName)
      } yield project.platform
        .flatMap(_.mainClass)
        .map { mainClass =>
          ScalaMainClass(
            className = mainClass,
            arguments = List.empty,
            jvmOptions = List.empty,
            environmentVariables = None
          )
        }
        .toList).getOrElse(List.empty)

      ScalaMainClassesItem(target = targetId, classes = mainClasses)
    }
    ScalaMainClassesResult(items, originId = params.originId)
  }

  private def handleScalaTestClasses(params: ScalaTestClassesParams): ScalaTestClassesResult = {
    val items = params.targets.map { targetId =>
      (for {
        started <- getActiveBuild.toOption
        crossName <- crossNameFromTargetId(started, targetId)
      } yield {
        val projectPaths = started.projectPaths(crossName)
        val classesDir = projectPaths.classes
        val resolved = started.resolvedProject(crossName)
        val classpath = resolved.classpath.map(p => Path.of(p.toString)).toList

        val suites = ClasspathTestDiscovery.discover(crossName, classesDir, classpath)

        debugLog(s"handleScalaTestClasses: project=${crossName.value}, classesDir=$classesDir, found ${suites.size} test classes")

        // Group by framework - BSP expects one item per target with all classes
        val classes = suites.map(_.className)
        val framework = suites.headOption.map(_.framework)
        ScalaTestClassesItem(target = targetId, framework = framework, classes = classes)
      }).getOrElse(
        ScalaTestClassesItem(target = targetId, framework = None, classes = List.empty)
      )
    }
    ScalaTestClassesResult(items)
  }

  // ==========================================================================
  // Helpers
  // ==========================================================================

  private def parseParams[T](params: Option[RawJson])(using codec: JsonValueCodec[T]): T =
    params match {
      case Some(raw) => readFromArray[T](raw.value)
      case None      => throw BspException(JsonRpcErrorCodes.InvalidParams, "Missing params")
    }

  private def toRaw[T](value: T)(using codec: JsonValueCodec[T]): RawJson =
    RawJson(writeToArray(value))

  /** Debug logging helper - only logs if DebugLogging is enabled */
  private inline def debugLog(message: => String): Unit =
    if (DebugLogging) logger.info(message)

  /** Info logging via BSP protocol */
  private def bspInfo(message: String): Unit =
    sendLogMessage(message, MessageType.Info)

  /** Warning logging via BSP protocol */
  private def bspWarn(message: String): Unit =
    sendLogMessage(message, MessageType.Warning)

  /** Error logging via BSP protocol */
  private def bspError(message: String): Unit =
    sendLogMessage(message, MessageType.Error)
}

object MultiWorkspaceBspServer {

  /** Enable debug logging to stderr (for development only) */
  val DebugLogging: Boolean = sys.env.get("BLEEP_BSP_DEBUG").contains("true")
}
