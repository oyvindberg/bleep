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
  ProjectCompileResult,
  ProjectCompileSuccess,
  ProjectCompiler,
  ScalaJsLinkConfig,
  ScalaNativeLinkConfig,
  ScalaNativeToolchain
}
import bleep.bsp.Outcome.KillReason
import bleep.bsp.protocol.BleepBspProtocol
import bleep.model.CrossProjectName
import bleep.testing.JvmPool
import cats.effect.{Deferred, FiberIO, IO, Ref}
import cats.effect.std.Queue
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

  /** Per-workspace loaded builds (using bleep-core's Started) */
  private val loadedBuilds = ConcurrentHashMap[Path, Started]()

  /** Build load error (set during initialize if build fails to load) */
  private val buildLoadError = AtomicReference[Option[String]](None)

  /** Active requests and their cancellation tokens */
  private val activeRequests = ConcurrentHashMap[String, CancellationToken]()

  /** Active request fibers for cancellation */
  private val activeFibers = ConcurrentHashMap[String, FiberIO[Unit]]()

  /** Track all spawned child processes for cleanup on shutdown */
  private val childProcesses = ConcurrentHashMap.newKeySet[Process]()

  /** Register a child process for tracking */
  def registerChildProcess(p: Process): Unit = childProcesses.add(p)

  /** Unregister a child process (after it exits normally) */
  def unregisterChildProcess(p: Process): Unit = childProcesses.remove(p)

  /** Lock timeout for write operations */
  private val lockTimeout = 5.minutes

  /** Run the server message loop with concurrent request handling.
    *
    * Notifications (like $/cancelRequest) are processed immediately. Requests are spawned in background fibers so the main loop stays responsive.
    */
  def run(): Unit = {
    val program = runConcurrent
      .handleErrorWith { err =>
        IO.delay(logger.error(s"Message loop failed: ${err.getClass.getName}: ${err.getMessage}", err))
      }
      .guarantee(
        // CRITICAL: Use uncancelable to ensure cleanup completes
        IO.uncancelable { _ =>
          // Cleanup on exit - cancel all active requests (kills child processes) then cancel fibers
          IO.delay(logger.warn("Server run() exiting - cleaning up")) >>
            // Release any workspace held by this connection
            IO.delay(activeWorkspace.get().foreach(SharedWorkspaceState.clearActiveUnconditional)) >>
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

    val handler: IO[Unit] = IO
      .interruptible(handleRequestSync(request))
      .guarantee(IO.blocking(activeFibers.remove(requestId)).void)

    handler.start.flatMap { fiber =>
      IO.blocking(activeFibers.put(requestId, fiber))
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
        logger.error(s"Failed to send response (client disconnected): ${e.getMessage}")
      case e: Exception =>
        logger.error(s"Failed to send response: ${e.getMessage}", e)
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
    logger.warn(s"[DISPATCH] method=$method thread=${Thread.currentThread().getName}")
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
        activeWorkspace.get().foreach(SharedWorkspaceState.cancelActive)
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

    val variant = parsedPayload.map(_.variantName).map(model.BuildVariant.fromName).getOrElse(model.BuildVariant.Normal)
    providedBuild.set(parsedPayload.map(_.build))
    providedClasspathOverrides.set(parsedPayload.map(_.classpathOverrides).getOrElse(Map.empty))

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
        logger.info(s"Build loaded: ${started.build.explodedProjects.size} projects for workspace $buildRoot (variant: $variant)")
      case Left(err) =>
        val msg = s"Failed to load build for workspace $buildRoot: ${err.getMessage}"
        buildLoadError.set(Some(msg))
        logger.error(msg)
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

  /** Acquire workspace for an operation, waiting if another connection holds it.
    *
    * If the workspace is busy, sends a WorkspaceBusy event to the client and blocks until the active work completes (or is cancelled). Then retries.
    *
    * @return
    *   Some(ActiveWork) if workspace was acquired (caller must call releaseWorkspace in finally), None if interrupted
    */
  @annotation.tailrec
  private def acquireWorkspace(
      workspace: Path,
      operation: String,
      projects: Set[String],
      cancellation: CancellationToken,
      originId: Option[String],
      taskId: String,
      sentBusyEvent: Boolean = false
  ): Option[SharedWorkspaceState.ActiveWork] =
    SharedWorkspaceState.getActive(workspace) match {
      case Some(active) =>
        // Workspace busy — notify client and wait
        sendCompileEvent(
          originId,
          taskId,
          BleepBspProtocol.Event.WorkspaceBusy(
            operation = active.operation,
            projects = active.projects.toList.sorted,
            startedAgoMs = System.currentTimeMillis() - active.startTimeMs,
            timestamp = System.currentTimeMillis()
          )
        )
        // Block until active work completes
        try active.completion.get()
        catch {
          case _: java.util.concurrent.CancellationException => ()
          case _: InterruptedException =>
            Thread.currentThread().interrupt()
            return None
        }
        // Retry — another waiter may have grabbed the slot
        acquireWorkspace(workspace, operation, projects, cancellation, originId, taskId, sentBusyEvent = true)

      case None =>
        val completion = new java.util.concurrent.CompletableFuture[Unit]()
        val kill: Runnable = () => cancelAllActiveRequests()
        val work = SharedWorkspaceState.ActiveWork(operation, projects, cancellation, completion, System.currentTimeMillis(), kill)
        if (SharedWorkspaceState.trySetActive(workspace, work)) {
          // Send WorkspaceReady if we had previously sent WorkspaceBusy
          if (sentBusyEvent) {
            sendCompileEvent(
              originId,
              taskId,
              BleepBspProtocol.Event.WorkspaceReady(timestamp = System.currentTimeMillis())
            )
          }
          Some(work)
        } else {
          // Race lost — retry
          acquireWorkspace(workspace, operation, projects, cancellation, originId, taskId, sentBusyEvent)
        }
    }

  /** Release workspace after operation completes. Only removes if this connection's ActiveWork still holds the lock. */
  private def releaseWorkspace(workspace: Path, work: SharedWorkspaceState.ActiveWork): Unit =
    SharedWorkspaceState.clearActive(workspace, work)

  /** Cancel all in-flight requests and kill child processes. */
  private def cancelAllActiveRequests(): Unit = {
    val activeCount = activeRequests.size()
    if (activeCount > 0) {
      logger.warn(s"cancelAllActiveRequests: cancelling $activeCount active requests: ${activeRequests.keySet().asScala.mkString(", ")}")
    }
    // Cancel all active request tokens (triggers cancellation flow in running IOs)
    activeRequests.values().forEach(_.cancel())

    // Force kill all tracked child processes (in case cancellation didn't clean them up)
    childProcesses.forEach { p =>
      try
        if (p.isAlive) {
          p.destroyForcibly()
        }
      catch { case _: Exception => }
    }
    childProcesses.clear()

    // Also kill any child processes of this JVM (belt and suspenders)
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
    logger.warn(s"Received cancelRequest for id=$idStr, token present=${token.isDefined}")
    token.foreach(_.cancel())
  }

  // ==========================================================================
  // Build loading using bleep-core
  // ==========================================================================

  private def loadBuild(workspaceRoot: Path, variant: model.BuildVariant): Either[BleepException, Started] =
    Option(loadedBuilds.get(workspaceRoot)) match {
      case Some(existing) => Right(existing)
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
            rewrites = Nil,
            config = bleepConfig,
            resolverFactory = CoursierResolver.Factory.default
          )
        } yield {
          loadedBuilds.put(workspaceRoot, started)
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
      case Some(existing) => Right(existing)
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
          case None =>
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

    val hasScala = project.scala.flatMap(_.version).isDefined
    val languages = if (hasScala) List("scala", "java") else List("java")

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
        canTest = Some(isTest),
        canRun = Some(hasMain),
        canDebug = Some(false)
      ),
      dataKind = if (hasScala) Some(BuildTargetDataKind.Scala) else Some(BuildTargetDataKind.Jvm),
      data = None
    )
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
      DependencySourcesItem(target = targetId, sources = List.empty)
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
          BleepBspProtocol.Event.SourcegenStarted(scriptMain, forProjects, System.currentTimeMillis())
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
    val activeWork = acquireWorkspace(workspace, opLabel, projectsToCompile.map(_.value), cancellation, params.originId, taskId) match {
      case Some(work) => work
      case None =>
        return CompileResult(originId = params.originId, statusCode = StatusCode.Cancelled, dataKind = None, data = None)
    }
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
          val kotlinVersion = project.kotlin.flatMap(_.version).map(_.kotlinVersion).getOrElse("2.1.0")

          (platformOpt, isKotlin) match {
            case (Some(model.PlatformId.Js), true) =>
              // Kotlin/JS
              val projectPaths = started.projectPaths(crossName)
              val outputDir = projectPaths.targetDir.resolve("link-output").resolve("js")
              val moduleKind = linkOpts.moduleKind
                .map {
                  case "esmodule" => "es"
                  case "nomodule" => "plain"
                  case _          => "commonjs"
                }
                .getOrElse("commonjs")
              val config = TaskDag.KotlinJsConfig(
                moduleKind = moduleKind,
                sourceMap = linkOpts.sourceMaps.getOrElse(!isRelease),
                dce = linkOpts.optimize.getOrElse(isRelease),
                outputDir = outputDir
              )
              Some(crossName -> TaskDag.LinkPlatform.KotlinJs(kotlinVersion, config))

            case (Some(model.PlatformId.Js), false) =>
              // Scala.js
              val sjsVersion = project.platform.flatMap(_.jsVersion).map(_.scalaJsVersion).getOrElse("1.19.0")
              val scalaVersion = project.scala.flatMap(_.version).map(_.scalaVersion).getOrElse("3.3.3")
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
              val snVersion = project.platform.flatMap(_.nativeVersion).map(_.scalaNativeVersion).getOrElse("0.5.6")
              val scalaVersion = project.scala.flatMap(_.version).map(_.scalaVersion).getOrElse("3.3.3")
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
      val initialDag = TaskDag.buildDag(projectsToCompile, allProjectDeps, platforms, buildMode)
      debugLog(s"Built compile DAG with ${initialDag.tasks.size} tasks (mode=$buildMode)")

      val startTime = System.currentTimeMillis()
      BspMetrics.recordBuildStart(workspace.toString, allProjects.size)

      // Create compile handler.
      // Uses IO.race to race compilation against the kill signal. When the kill
      // signal wins, IO.race cancels the compile fiber. Since ZincBridge uses
      // IO.interruptible, CE interrupts the compilation thread immediately.
      val compileHandler: (TaskDag.CompileTask, Deferred[IO, KillReason]) => IO[TaskDag.TaskResult] =
        (compileTask, taskKillSignal) => {
          val projectName = compileTask.project.value
          val wsStr = workspace.toString
          val token = CancellationToken.create()
          taskKillSignal.tryGet.flatMap {
            case Some(_) => IO.pure(TaskDag.TaskResult.Killed(KillReason.UserRequest))
            case None    =>
              // Cooperative cancellation: set CancellationToken so advance() returns false
              val cooperativeCancel = taskKillSignal.get.flatMap(_ => IO(token.cancel())).start

              // Gate on server-wide semaphore to limit total concurrent compiles across all connections
              val gatedCompile = IO
                .interruptible(compileSemaphore.acquire())
                .bracket { _ =>
                  IO(activeCompileCount.incrementAndGet()) >>
                    waitForHeapPressure(projectName, params.originId) >> {
                      val compileStartTime = System.currentTimeMillis()
                      IO(BspMetrics.recordCompileStart(projectName, wsStr)) >>
                        compileProject(started, compileTask.project, params.originId, token).flatMap { result =>
                          val dur = System.currentTimeMillis() - compileStartTime
                          val ok = result == TaskDag.TaskResult.Success
                          IO(BspMetrics.recordCompileEnd(projectName, wsStr, dur, ok)).as(result)
                        }
                    }
                }(_ => IO(activeCompileCount.decrementAndGet()) >> IO(compileSemaphore.release()))
              val waitForKill = taskKillSignal.get.map(reason => TaskDag.TaskResult.Killed(reason))

              cooperativeCancel >> IO.race(gatedCompile, waitForKill).map(_.merge)
          }
        }

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
      val executor = TaskDag.executor(compileHandler, linkHandler, discoverHandler, testHandler)

      // Create trace recorder (noop if not enabled)
      val traceRecorder = if (linkOpts.flamegraph) TraceRecorder.create.unsafeRunSync() else TraceRecorder.noop

      val ioProgram = for {
        eventQueue <- Queue.unbounded[IO, TaskDag.DagEvent]
        killSignal <- Outcome.fromCancellationToken(cancellation)

        // Start event consumer fiber - use guarantee to ensure cleanup on cancellation/error
        consumerErrorRef <- Ref.of[IO, Option[Throwable]](None)
        eventConsumerFiber <- consumeCompileEvents(eventQueue, params.originId, killSignal, traceRecorder).compile.drain.handleErrorWith { e =>
          // Capture consumer error for later inspection
          IO(logger.error(s"Event consumer error: ${e.getMessage}")) >>
            consumerErrorRef.set(Some(e))
        }.start

        // Run executor with guarantee to cancel consumer fiber on completion/error/cancellation
        dag <- executor
          .execute(initialDag, maxParallelism, eventQueue, killSignal)
          .flatTap(_ => IO.sleep(100.millis)) // Give consumer time to process remaining events
          .guarantee(eventConsumerFiber.cancel) // Always cancel consumer fiber

        // Log consumer errors but don't fail the build — compilation results are still valid
        // even if progress notifications couldn't be sent (e.g., client disconnected mid-build)
        consumerError <- consumerErrorRef.get
        _ <- consumerError match {
          case Some(e) =>
            IO(logger.warn(s"Event consumer failed (build results still valid): ${e.getMessage}"))
          case None => IO.unit
        }
      } yield dag

      val ioResult = Try(ioProgram.unsafeRunSync())

      // Write trace file if flamegraph is enabled
      if (linkOpts.flamegraph) {
        val tracePath = started.buildPaths.dotBleepDir.resolve("trace.json")
        traceRecorder.writeTrace(tracePath).unsafeRunSync()
      }

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
    } finally releaseWorkspace(workspace, activeWork)
  }

  /** Create a HeapPressureGate.Listener that sends BSP events and logs */
  private def makeHeapPressureListener(originId: Option[String]): HeapPressureGate.Listener =
    new HeapPressureGate.Listener {
      def onStall(project: String, used: HeapMb, max: HeapMb, retryAt: EpochMs, now: EpochMs): Unit = {
        sendCompileEvent(
          originId,
          s"compile:$project",
          BleepBspProtocol.Event.CompileStalled(project, used.value, max.value, retryAt.value, now.value)
        )
        logger.warn(
          s"[HEAP] $project: stalled (heap: ${used.value}MB/${max.value}MB) — retrying in ${HeapPressureGate.DefaultRetryMs.value}ms"
        )
      }
      def onResume(project: String, used: HeapMb, max: HeapMb, stalledFor: DurationMs, now: EpochMs): Unit = {
        sendCompileEvent(
          originId,
          s"compile:$project",
          BleepBspProtocol.Event.CompileResumed(project, used.value, max.value, stalledFor.value, now.value)
        )
        logger.warn(s"[HEAP] $project: resumed after ${stalledFor.value}ms stall (heap: ${used.value}MB/${max.value}MB)")
      }
      def onSkipBecauseAlone(project: String, used: HeapMb, max: HeapMb): Unit =
        logger.warn(
          s"[HEAP] $project: heap at ${used.value}MB/${max.value}MB but proceeding — no other compiles running"
        )
    }

  /** Wait until heap pressure is below threshold before starting compilation.
    *
    * Delegates to HeapPressureGate for testable logic. Cancellation-safe: IO.sleep is cancelable.
    */
  private def waitForHeapPressure(
      projectName: String,
      originId: Option[String]
  ): IO[Unit] =
    HeapPressureGate.waitForHeapPressure(
      heapMonitor = heapMonitor,
      activeCompileCount = activeCompileCount,
      threshold = HeapPressureGate.DefaultThreshold,
      retryMs = HeapPressureGate.DefaultRetryMs,
      projectName = projectName,
      listener = makeHeapPressureListener(originId)
    )

  /** Send a compile event via BSP notification with structured data */
  private def sendCompileEvent(originId: Option[String], taskId: String, event: BleepBspProtocol.Event): Unit = {
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

  /** Send a link event via BSP notification with structured data */
  private def sendLinkEvent(originId: Option[String], taskId: String, event: BleepBspProtocol.Event): Unit = {
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
    val activeWork = acquireWorkspace(workspace, "test", testProjects.map(_.value), cancellation, params.originId, taskId) match {
      case Some(work) => work
      case None =>
        return TestResult(originId = params.originId, statusCode = StatusCode.Cancelled, dataKind = None, data = None)
    }
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
        val kotlinVersion = project.kotlin.flatMap(_.version).map(_.kotlinVersion).getOrElse("2.1.0")

        (platformOpt, isKotlin) match {
          case (Some(model.PlatformId.Js), true) =>
            // Kotlin/JS - don't add "js" here; executeKotlinJs adds it
            val projectPaths = started.projectPaths(crossName)
            val outputDir = projectPaths.targetDir
            val config = TaskDag.KotlinJsConfig(
              moduleKind = "umd",
              sourceMap = false,
              dce = false, // Tests run without DCE
              outputDir = outputDir
            )
            Some(crossName -> TaskDag.LinkPlatform.KotlinJs(kotlinVersion, config))

          case (Some(model.PlatformId.Js), false) =>
            // Scala.js
            val sjsVersion = project.platform.flatMap(_.jsVersion).map(_.scalaJsVersion).getOrElse("1.19.0")
            val scalaVersion = project.scala.flatMap(_.version).map(_.scalaVersion).getOrElse("3.3.3")
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
            val snVersion = project.platform.flatMap(_.nativeVersion).map(_.scalaNativeVersion).getOrElse("0.5.6")
            val scalaVersion = project.scala.flatMap(_.version).map(_.scalaVersion).getOrElse("3.3.3")
            val config = bleep.analysis.ScalaNativeLinkConfig.Debug
            Some(crossName -> TaskDag.LinkPlatform.ScalaNative(snVersion, scalaVersion, config))

          case _ =>
            // JVM - no linking needed
            None
        }
      }.toMap

      // Build the unified DAG with platforms
      val initialDag = TaskDag.buildTestDag(testProjects, allProjectDeps, platforms)
      debugLog(s"Built test DAG with ${initialDag.tasks.size} tasks, platforms: ${platforms.keys.map(_.value).mkString(", ")}")

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
            case Left(err) =>
              logger.warn(s"Failed to decode TestOptions: $err (raw: ${raw.take(200)})")
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
        eventQueue <- Queue.unbounded[IO, TaskDag.DagEvent]
        totalSuitesRef <- Ref.of[IO, Int](0)
        totalPassedRef <- Ref.of[IO, Int](0)
        totalFailedRef <- Ref.of[IO, Int](0)
        totalSkippedRef <- Ref.of[IO, Int](0)
        totalIgnoredRef <- Ref.of[IO, Int](0)

        // Create kill signal from cancellation token
        killSignal <- Outcome.fromCancellationToken(cancellation)

        // Create JVM pool for test execution
        testResult <- JvmPool.create(maxParallelism, started.jvmCommand, started.buildPaths.buildDir).use { jvmPool =>
          // Create task handlers that accept kill signal
          val compileHandler: (TaskDag.CompileTask, Deferred[IO, Outcome.KillReason]) => IO[TaskDag.TaskResult] =
            (compileTask, taskKillSignal) => {
              val projectName = compileTask.project.value
              val wsStr = workspace.toString
              val token = CancellationToken.create()
              taskKillSignal.tryGet.flatMap {
                case Some(_) => IO.pure(TaskDag.TaskResult.Killed(Outcome.KillReason.UserRequest))
                case None =>
                  val cooperativeCancel = taskKillSignal.get.flatMap(_ => IO(token.cancel())).start
                  // Gate on server-wide semaphore to limit total concurrent compiles across all connections
                  val gatedCompile = IO
                    .interruptible(compileSemaphore.acquire())
                    .bracket { _ =>
                      IO(activeCompileCount.incrementAndGet()) >>
                        waitForHeapPressure(projectName, params.originId) >> {
                          val compileStartTime = System.currentTimeMillis()
                          IO(BspMetrics.recordCompileStart(projectName, wsStr)) >>
                            compileProject(started, compileTask.project, params.originId, token).flatMap { result =>
                              val dur = System.currentTimeMillis() - compileStartTime
                              val ok = result == TaskDag.TaskResult.Success
                              IO(BspMetrics.recordCompileEnd(projectName, wsStr, dur, ok)).as(result)
                            }
                        }
                    }(_ => IO(activeCompileCount.decrementAndGet()) >> IO(compileSemaphore.release()))
                  val waitForKill = taskKillSignal.get.map(reason => TaskDag.TaskResult.Killed(reason))
                  cooperativeCancel >> IO.race(gatedCompile, waitForKill).map(_.merge)
              }
            }

          val discoverHandler: (TaskDag.DiscoverTask, Deferred[IO, Outcome.KillReason]) => IO[(TaskDag.TaskResult, List[(String, String)])] =
            (discoverTask, _) =>
              discoverTestSuites(started, discoverTask.project).map { case (result, suites) =>
                val filtered = filterSuites(suites, testOptions.only, testOptions.exclude)
                (result, filtered)
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
                TestRunner.runSuite(
                  project = testTask.project,
                  suiteName = testTask.suiteName,
                  framework = testTask.framework,
                  classpath = classpath,
                  pool = jvmPool,
                  eventQueue = eventQueue,
                  options = TestRunner.Options(
                    jvmOptions = serverConfig.testRunnerMaxMemory.map(m => s"-Xmx$m").toList ++ testOptions.jvmOptions,
                    testArgs = testOptions.testArgs,
                    idleTimeout = idleTimeout
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
          val executor = TaskDag.executor(compileHandler, linkHandler, discoverHandler, testHandler)

          // Run event consumer in background (auto-cancels when scope exits)
          // This ensures the fiber is cleaned up even if the request is cancelled
          for {
            _ <- IO {
              logger.warn(s"[BSP-SERVER] Starting event consumer and task executor, sendEventCounter=${sendEventCounter.get()}")
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
              IO(logger.error(s"Event consumer error: ${e.getMessage}")) >>
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
                  logger.info(
                    s"Task executor completed: $total tasks total, $completed completed, $failed failed, $errored errored, $skipped skipped, $killed killed, $timedOut timedOut"
                  )
                  if (result.killed.nonEmpty) {
                    logger.warn(s"Killed tasks: ${result.killed.mkString(", ")}")
                  }
                  if (result.failed.nonEmpty) {
                    logger.warn(s"Failed tasks: ${result.failed.mkString(", ")}")
                  }
                  if (result.errored.nonEmpty) {
                    logger.warn(s"Errored tasks: ${result.errored.mkString(", ")}")
                  }
                  if (result.skipped.nonEmpty) {
                    logger.warn(s"Skipped tasks: ${result.skipped.mkString(", ")}")
                  }
                  if (cancellation.isCancelled) {
                    logger.warn("Cancellation token was triggered during test execution!")
                  }
                } >> IO {
                  logger.warn(
                    s"[BSP-SERVER] Executor done, draining remaining events. sendEventCounter=${sendEventCounter.get()}, cancellation.isCancelled=${cancellation.isCancelled}"
                  )
                } >>
                  IO.sleep(100.millis) >>
                  drainRemainingEvents(eventQueue, params.originId, totalSuitesRef, totalPassedRef, totalFailedRef, totalSkippedRef, totalIgnoredRef) >>
                  IO.pure(result)
              }
              .guarantee(eventConsumerFiber.cancel) // Always cancel consumer fiber

            // Log consumer errors but don't fail the build — test/compile results are still valid
            consumerError <- consumerErrorRef.get
            _ <- consumerError match {
              case Some(e) =>
                IO(logger.warn(s"Event consumer failed (build results still valid): ${e.getMessage}"))
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

      logger.warn(s"[BSP-SERVER] handleTest: starting ioProgram.unsafeRunSync() for ${testProjects.map(_.value).mkString(", ")}")
      val ioResult = Try(ioProgram.unsafeRunSync())
      logger.warn(s"[BSP-SERVER] handleTest: ioProgram.unsafeRunSync() returned, sendEventCounter=${sendEventCounter.get()}")

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
    } finally releaseWorkspace(workspace, activeWork)
  }

  /** Compile a single project (dependencies handled by TaskDag ordering).
    *
    * Calls the compiler directly (no ParallelProjectCompiler) so that CE fiber cancellation propagates through IO.interruptible in ZincBridge. This enables
    * IO.race in the compile handler to immediately interrupt compilation when the kill signal fires.
    */
  private def compileProject(started: Started, project: CrossProjectName, originId: Option[String], cancellation: CancellationToken): IO[TaskDag.TaskResult] = {
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

        val publishParams = PublishDiagnosticsParams(
          textDocument = textDocument.getOrElse(TextDocumentIdentifier(Uri(java.net.URI.create("unknown")))),
          buildTarget = targetId,
          originId = originId,
          diagnostics = List(diagnostic),
          reset = false
        )

        sendNotification("build/publishDiagnostics", publishParams)
      }

      override def onCompilePhase(projectName: String, phase: CompilePhase): Unit = {
        val trackedApis = phase match {
          case CompilePhase.ReadingAnalysis(n) => n
          case _                               => 0
        }
        BspMetrics.recordCompilePhase(projectName, phase.name, trackedApis)
        sendCompileEvent(
          originId,
          s"compile:$projectName",
          BleepBspProtocol.Event.CompilePhaseChanged(projectName, phase.name, trackedApis, System.currentTimeMillis())
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
            ("clean-build", 0, Nil, Nil)
          case CompilationReason.EmptyOutput =>
            ("empty-output", 0, Nil, Nil)
          case CompilationReason.UpToDate =>
            ("up-to-date", 0, Nil, Nil)
          case CompilationReason.Incremental(total, invalidated, changed) =>
            ("incremental", total, invalidated.map(_.getFileName.toString).toList, changed.map(depName).toList)
        }
        val now = System.currentTimeMillis()
        sendCompileEvent(
          originId,
          s"compile:$projectName",
          BleepBspProtocol.Event.CompilationReason(
            project = projectName,
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
            sendCompileEvent(
              originId,
              s"compile:${project.value}",
              BleepBspProtocol.Event.CompileProgress(project.value, percent, now)
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
          sendCompileEvent(
            originId,
            s"compile:${project.value}",
            BleepBspProtocol.Event.LockContention(project.value, 0, System.currentTimeMillis())
          )
      )
      .evalTap { hadContention =>
        IO {
          if (hadContention) {
            val waited = System.currentTimeMillis() - lockStart
            sendCompileEvent(
              originId,
              s"compile:${project.value}",
              BleepBspProtocol.Event.LockAcquired(project.value, waited, System.currentTimeMillis())
            )
          }
        }
      }
      .use { _ =>
        compiler.compile(config, diagnosticListener, cancellation, Map.empty, progressListener)
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

  /** Fetch bleep-test-runner via coursier using bleep's resolver */
  private def fetchTestRunnerViaCoursier(started: Started): List[Path] = {
    val version = model.BleepVersion.current.value
    val dep = model.Dep.Java("build.bleep", "bleep-test-runner", version)

    val result = started.resolver.force(
      Set(dep),
      model.VersionCombo.Jvm(model.VersionScala.Scala3),
      libraryVersionSchemes = SortedSet.empty[model.LibraryVersionScheme],
      context = s"resolving bleep-test-runner:$version",
      model.IgnoreEvictionErrors.No
    )

    // Filter to only bleep-test-runner.jar and test-interface.jar
    val neededJars = result.jars.filter { p =>
      val name = p.getFileName.toString
      name.contains("bleep-test-runner") || name.contains("test-interface")
    }

    if (neededJars.isEmpty) {
      throw new RuntimeException(
        s"bleep-test-runner jar not found in resolved files: ${result.jars.map(_.getFileName).mkString(", ")}"
      )
    }

    neededJars
  }

  /** Run a Scala.js test suite: link → run via Node.js, emit events to DAG queue. */
  private def runScalaJsTestSuite(
      started: Started,
      testTask: TaskDag.TestSuiteTask,
      classpath: List[Path],
      eventQueue: Queue[IO, TaskDag.DagEvent],
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
          val eventHandler = new ScalaJsTestRunner.TestEventHandler {
            def onTestStarted(suite: String, test: String): Unit =
              eventQueue.offer(TaskDag.DagEvent.TestStarted(testTask.project.value, suite, test, System.currentTimeMillis())).unsafeRunSync()
            def onTestFinished(suite: String, test: String, status: ScalaJsTestRunner.TestStatus, durationMs: Long, message: Option[String]): Unit = {
              val statusStr = status match {
                case ScalaJsTestRunner.TestStatus.Passed    => "passed"
                case ScalaJsTestRunner.TestStatus.Failed    => "failed"
                case ScalaJsTestRunner.TestStatus.Skipped   => "skipped"
                case ScalaJsTestRunner.TestStatus.Ignored   => "ignored"
                case ScalaJsTestRunner.TestStatus.Cancelled => "cancelled"
              }
              eventQueue
                .offer(TaskDag.DagEvent.TestFinished(testTask.project.value, suite, test, statusStr, durationMs, message, None, System.currentTimeMillis()))
                .unsafeRunSync()
            }
            def onSuiteStarted(suite: String): Unit = ()
            def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = ()
            def onOutput(suite: String, line: String, isError: Boolean): Unit =
              eventQueue.offer(TaskDag.DagEvent.Output(testTask.project.value, suite, line, isError, System.currentTimeMillis())).unsafeRunSync()
          }

          val suites = List(ScalaJsTestRunner.TestSuite(testTask.suiteName, testTask.suiteName))
          ScalaJsTestRunner
            .runTests(mainModule, linkConfig.moduleKind, suites, eventHandler, ScalaJsTestRunner.NodeEnvironment.Node, Map.empty, killSignal)
            .map { result =>
              val endTs = System.currentTimeMillis()
              val durationMs = endTs - startTs
              eventQueue
                .offer(
                  TaskDag.DagEvent
                    .SuiteFinished(testTask.project.value, testTask.suiteName, result.passed, result.failed, result.skipped, result.ignored, durationMs, endTs)
                )
                .unsafeRunSync()
              classifyJsTestResult(result)
            }
        case (result, _) =>
          val endTs = System.currentTimeMillis()
          val durationMs = endTs - startTs
          eventQueue.offer(TaskDag.DagEvent.SuiteFinished(testTask.project.value, testTask.suiteName, 0, 1, 0, 0, durationMs, endTs)).void >>
            IO.pure(result)
      }
    } yield taskResult
  }

  /** Run a Scala Native test suite: link → run binary, emit events to DAG queue. */
  private def runScalaNativeTestSuite(
      started: Started,
      testTask: TaskDag.TestSuiteTask,
      classpath: List[Path],
      eventQueue: Queue[IO, TaskDag.DagEvent],
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
          val eventHandler = new ScalaNativeTestRunner.TestEventHandler {
            def onTestStarted(suite: String, test: String): Unit =
              eventQueue.offer(TaskDag.DagEvent.TestStarted(testTask.project.value, suite, test, System.currentTimeMillis())).unsafeRunSync()
            def onTestFinished(suite: String, test: String, status: ScalaNativeTestRunner.TestStatus, durationMs: Long, message: Option[String]): Unit = {
              val statusStr = status match {
                case ScalaNativeTestRunner.TestStatus.Passed    => "passed"
                case ScalaNativeTestRunner.TestStatus.Failed    => "failed"
                case ScalaNativeTestRunner.TestStatus.Skipped   => "skipped"
                case ScalaNativeTestRunner.TestStatus.Ignored   => "ignored"
                case ScalaNativeTestRunner.TestStatus.Cancelled => "cancelled"
              }
              eventQueue
                .offer(TaskDag.DagEvent.TestFinished(testTask.project.value, suite, test, statusStr, durationMs, message, None, System.currentTimeMillis()))
                .unsafeRunSync()
            }
            def onSuiteStarted(suite: String): Unit = ()
            def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = ()
            def onOutput(suite: String, line: String, isError: Boolean): Unit =
              eventQueue.offer(TaskDag.DagEvent.Output(testTask.project.value, suite, line, isError, System.currentTimeMillis())).unsafeRunSync()
          }

          val suites = List(ScalaNativeTestRunner.TestSuite(testTask.suiteName, testTask.suiteName))
          ScalaNativeTestRunner
            .runTestsViaAdapter(binary, suites, framework, eventHandler, Map.empty, started.buildPaths.cwd, snVersion.scalaNativeVersion, killSignal)
            .map { result =>
              val endTs = System.currentTimeMillis()
              val durationMs = endTs - startTs
              // Emit error/crash details as Output so they appear in failure details
              result.terminationReason match {
                case ScalaNativeTestRunner.TerminationReason.Error(msg) =>
                  eventQueue.offer(TaskDag.DagEvent.Output(testTask.project.value, testTask.suiteName, msg, true, endTs)).unsafeRunSync()
                case ScalaNativeTestRunner.TerminationReason.Crashed(signal) =>
                  eventQueue
                    .offer(TaskDag.DagEvent.Output(testTask.project.value, testTask.suiteName, s"Process crashed (signal $signal)", true, endTs))
                    .unsafeRunSync()
                case ScalaNativeTestRunner.TerminationReason.TruncatedOutput(suite) =>
                  eventQueue
                    .offer(
                      TaskDag.DagEvent.Output(testTask.project.value, testTask.suiteName, s"Process exited with truncated output (suite '$suite')", true, endTs)
                    )
                    .unsafeRunSync()
                case ScalaNativeTestRunner.TerminationReason.ExitCode(code) =>
                  eventQueue
                    .offer(TaskDag.DagEvent.Output(testTask.project.value, testTask.suiteName, s"Process exited with code $code", true, endTs))
                    .unsafeRunSync()
                case _ => ()
              }
              eventQueue
                .offer(
                  TaskDag.DagEvent
                    .SuiteFinished(testTask.project.value, testTask.suiteName, result.passed, result.failed, result.skipped, result.ignored, durationMs, endTs)
                )
                .unsafeRunSync()
              classifyNativeTestResult(result)
            }
        case _ =>
          val endTs = System.currentTimeMillis()
          val durationMs = endTs - startTs
          eventQueue.offer(TaskDag.DagEvent.SuiteFinished(testTask.project.value, testTask.suiteName, 0, 1, 0, 0, durationMs, endTs)).void >>
            IO.pure(TaskDag.TaskResult.Failure("Native linking failed", List.empty))
      }
    } yield taskResult
  }

  /** Run a Kotlin/JS test suite: discover + run via Node.js, emit events to DAG queue. */
  private def runKotlinJsTestSuite(
      started: Started,
      testTask: TaskDag.TestSuiteTask,
      classpath: List[Path],
      eventQueue: Queue[IO, TaskDag.DagEvent],
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
          eventQueue.offer(TaskDag.DagEvent.SuiteFinished(testTask.project.value, testTask.suiteName, 0, 1, 0, 0, durationMs, endTs)).void >>
            IO.pure(TaskDag.TaskResult.Failure(s"Kotlin/JS output not found: $jsOutput", List.empty))
        } else {
          val eventHandler = new KotlinTestRunner.TestEventHandler {
            def onTestStarted(suite: String, test: String): Unit =
              eventQueue.offer(TaskDag.DagEvent.TestStarted(testTask.project.value, suite, test, System.currentTimeMillis())).unsafeRunSync()
            def onTestFinished(suite: String, test: String, status: KotlinTestRunner.TestStatus, durationMs: Long, message: Option[String]): Unit = {
              val statusStr = status match {
                case KotlinTestRunner.TestStatus.Passed    => "passed"
                case KotlinTestRunner.TestStatus.Failed    => "failed"
                case KotlinTestRunner.TestStatus.Skipped   => "skipped"
                case KotlinTestRunner.TestStatus.Ignored   => "ignored"
                case KotlinTestRunner.TestStatus.Cancelled => "cancelled"
              }
              eventQueue
                .offer(TaskDag.DagEvent.TestFinished(testTask.project.value, suite, test, statusStr, durationMs, message, None, System.currentTimeMillis()))
                .unsafeRunSync()
            }
            def onSuiteStarted(suite: String): Unit = ()
            def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = ()
            def onOutput(suite: String, line: String, isError: Boolean): Unit =
              eventQueue.offer(TaskDag.DagEvent.Output(testTask.project.value, suite, line, isError, System.currentTimeMillis())).unsafeRunSync()
          }

          val suites = List(KotlinTestRunner.TestSuite(testTask.suiteName, testTask.suiteName))
          KotlinTestRunner.Js.runTests(jsOutput, suites, eventHandler, Map.empty, killSignal).map { result =>
            val endTs = System.currentTimeMillis()
            val durationMs = endTs - startTs
            eventQueue
              .offer(
                TaskDag.DagEvent
                  .SuiteFinished(testTask.project.value, testTask.suiteName, result.passed, result.failed, result.skipped, result.ignored, durationMs, endTs)
              )
              .unsafeRunSync()
            classifyKotlinTestResult(result)
          }
        }
    } yield taskResult
  }

  /** Run a Kotlin/Native test suite: run binary, emit events to DAG queue. */
  private def runKotlinNativeTestSuite(
      started: Started,
      testTask: TaskDag.TestSuiteTask,
      classpath: List[Path],
      eventQueue: Queue[IO, TaskDag.DagEvent],
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
          eventQueue.offer(TaskDag.DagEvent.SuiteFinished(testTask.project.value, testTask.suiteName, 0, 1, 0, 0, durationMs, endTs)).void >>
            IO.pure(TaskDag.TaskResult.Failure(s"Kotlin/Native binary not found: $binary", List.empty))
        } else {
          val eventHandler = new KotlinTestRunner.TestEventHandler {
            def onTestStarted(suite: String, test: String): Unit =
              eventQueue.offer(TaskDag.DagEvent.TestStarted(testTask.project.value, suite, test, System.currentTimeMillis())).unsafeRunSync()
            def onTestFinished(suite: String, test: String, status: KotlinTestRunner.TestStatus, durationMs: Long, message: Option[String]): Unit = {
              val statusStr = status match {
                case KotlinTestRunner.TestStatus.Passed    => "passed"
                case KotlinTestRunner.TestStatus.Failed    => "failed"
                case KotlinTestRunner.TestStatus.Skipped   => "skipped"
                case KotlinTestRunner.TestStatus.Ignored   => "ignored"
                case KotlinTestRunner.TestStatus.Cancelled => "cancelled"
              }
              eventQueue
                .offer(TaskDag.DagEvent.TestFinished(testTask.project.value, suite, test, statusStr, durationMs, message, None, System.currentTimeMillis()))
                .unsafeRunSync()
            }
            def onSuiteStarted(suite: String): Unit = ()
            def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = ()
            def onOutput(suite: String, line: String, isError: Boolean): Unit =
              eventQueue.offer(TaskDag.DagEvent.Output(testTask.project.value, suite, line, isError, System.currentTimeMillis())).unsafeRunSync()
          }

          // Run all tests in the binary - passing an empty list means no filter
          // This is safer than trying to match synthetic suite names to actual test classes
          KotlinTestRunner.Native.runTests(binary, List.empty, eventHandler, Map.empty, started.buildPaths.cwd, killSignal).map { result =>
            val endTs = System.currentTimeMillis()
            val durationMs = endTs - startTs
            eventQueue
              .offer(
                TaskDag.DagEvent
                  .SuiteFinished(testTask.project.value, testTask.suiteName, result.passed, result.failed, result.skipped, result.ignored, durationMs, endTs)
              )
              .unsafeRunSync()
            classifyKotlinTestResult(result)
          }
        }
    } yield taskResult
  }

  /** Consume events from the queue and send BSP notifications with rich test data.
    *
    * Uses BleepBspProtocol to send structured events in the BSP data field, allowing TestReactive to reconstruct rich TestEvents for FancyBuildDisplay.
    *
    * If a notification fails to send (connection dead), completes the killSignal to trigger cancellation of all running tasks, then re-raises the error.
    */
  private def consumeEvents(
      queue: Queue[IO, TaskDag.DagEvent],
      originId: Option[String],
      totalSuitesRef: Ref[IO, Int],
      totalPassedRef: Ref[IO, Int],
      totalFailedRef: Ref[IO, Int],
      totalSkippedRef: Ref[IO, Int],
      totalIgnoredRef: Ref[IO, Int],
      killSignal: Deferred[IO, Outcome.KillReason],
      traceRecorder: TraceRecorder
  ): fs2.Stream[IO, Unit] =
    fs2.Stream.fromQueueUnterminated(queue).evalMap { event =>
      // Helper to get category and name for trace recording
      def taskCatName(task: TaskDag.Task): (String, String) = task match {
        case ct: TaskDag.CompileTask   => ("compile", ct.project.value)
        case lt: TaskDag.LinkTask      => ("link", lt.project.value)
        case dt: TaskDag.DiscoverTask  => ("discover", dt.project.value)
        case tt: TaskDag.TestSuiteTask => ("test", s"${tt.project.value}:${tt.suiteName}")
      }

      // Wrap event processing - if notification fails (connection dead),
      // trigger kill signal to cancel all tasks, then re-raise
      def processEvent: IO[Unit] =
        event match {
          case TaskDag.DagEvent.TaskStarted(task, timestamp) =>
            val (cat, name) = taskCatName(task)
            val protocolEvent = task match {
              case ct: TaskDag.CompileTask =>
                BleepBspProtocol.Event.CompileStarted(ct.project.value, timestamp)
              case _: TaskDag.LinkTask =>
                null // Link tasks are not exposed via test protocol
              case dt: TaskDag.DiscoverTask =>
                BleepBspProtocol.Event.DiscoveryStarted(dt.project.value, timestamp)
              case tt: TaskDag.TestSuiteTask =>
                BleepBspProtocol.Event.SuiteStarted(tt.project.value, tt.suiteName, timestamp)
            }
            traceRecorder.recordStart(cat, name) >>
              IO(if (protocolEvent != null) sendTestEvent(originId, task.id, protocolEvent))

          case TaskDag.DagEvent.TaskFinished(task, result, durationMs, timestamp) =>
            val (cat, name) = taskCatName(task)
            val protocolEvent = task match {
              case ct: TaskDag.CompileTask =>
                result match {
                  case TaskDag.TaskResult.Success =>
                    BleepBspProtocol.Event.CompileFinished(ct.project.value, "success", durationMs, Nil, skippedBecause = None, timestamp)
                  case TaskDag.TaskResult.Failure(errorMsg, diags) =>
                    // Ensure diagnostics always contain the error message — the errorMsg field
                    // has the human-readable description but was previously discarded
                    val effectiveDiags =
                      if (diags.exists(d => d.severity == "error" && d.message.nonEmpty)) diags
                      else List(BleepBspProtocol.Diagnostic.error(errorMsg))
                    BleepBspProtocol.Event.CompileFinished(ct.project.value, "failed", durationMs, effectiveDiags, skippedBecause = None, timestamp)
                  case TaskDag.TaskResult.Error(error, _, _) =>
                    BleepBspProtocol.Event.CompileFinished(
                      ct.project.value,
                      "error",
                      durationMs,
                      List(BleepBspProtocol.Diagnostic.error(error)),
                      skippedBecause = None,
                      timestamp
                    )
                  case TaskDag.TaskResult.Skipped(failedDep) =>
                    BleepBspProtocol.Event.CompileFinished(
                      ct.project.value,
                      "skipped",
                      durationMs,
                      Nil,
                      skippedBecause = Some(failedDep.project.value),
                      timestamp
                    )
                  case TaskDag.TaskResult.Killed(_) =>
                    BleepBspProtocol.Event.CompileFinished(ct.project.value, "cancelled", durationMs, Nil, skippedBecause = None, timestamp)
                  case TaskDag.TaskResult.Cancelled =>
                    BleepBspProtocol.Event.CompileFinished(ct.project.value, "cancelled", durationMs, Nil, skippedBecause = None, timestamp)
                  case TaskDag.TaskResult.TimedOut =>
                    BleepBspProtocol.Event.CompileFinished(ct.project.value, "timedout", durationMs, Nil, skippedBecause = None, timestamp)
                }

              case _: TaskDag.LinkTask =>
                null // Link tasks are not exposed via test protocol

              case dt: TaskDag.DiscoverTask =>
                // Discovery finished is handled by SuitesDiscovered event
                null

              case tt: TaskDag.TestSuiteTask =>
                // SuiteFinished is emitted by TestRunner with actual counts
                // Only handle timeout, skipped, error, and cancelled cases here
                result match {
                  case TaskDag.TaskResult.Success =>
                    null // SuiteFinished already emitted by TestRunner
                  case TaskDag.TaskResult.Failure(_, _) =>
                    null // SuiteFinished already emitted by TestRunner
                  case TaskDag.TaskResult.Error(error, exitCode, signal) =>
                    val desc = signal match {
                      case Some(sig) => s"Process crashed (signal $sig)"
                      case None =>
                        exitCode match {
                          case Some(code) => s"Process exited with code $code"
                          case None       => error
                        }
                    }
                    BleepBspProtocol.Event.SuiteError(tt.project.value, tt.suiteName, desc, exitCode, signal, durationMs, timestamp)
                  case TaskDag.TaskResult.Skipped(failedDep) =>
                    BleepBspProtocol.Event.SuiteCancelled(tt.project.value, tt.suiteName, Some(s"dependency ${failedDep.project.value} failed"), timestamp)
                  case TaskDag.TaskResult.Killed(_) =>
                    BleepBspProtocol.Event.SuiteCancelled(tt.project.value, tt.suiteName, Some("killed"), timestamp)
                  case TaskDag.TaskResult.Cancelled =>
                    BleepBspProtocol.Event.SuiteCancelled(tt.project.value, tt.suiteName, Some("cancelled"), timestamp)
                  case TaskDag.TaskResult.TimedOut =>
                    BleepBspProtocol.Event.SuiteTimedOut(tt.project.value, tt.suiteName, durationMs, None, timestamp)
                }
            }
            traceRecorder.recordEnd(cat, name) >>
              IO(if (protocolEvent != null) sendTestEvent(originId, task.id, protocolEvent))

          case TaskDag.DagEvent.TestStarted(project, suite, test, timestamp) =>
            val protocolEvent = BleepBspProtocol.Event.TestStarted(project, suite, test, timestamp)
            IO(sendTestEvent(originId, s"test:$project:$suite", protocolEvent))

          case TaskDag.DagEvent.TestFinished(project, suite, test, status, durationMs, message, throwable, timestamp) =>
            val protocolEvent = BleepBspProtocol.Event.TestFinished(
              project,
              suite,
              test,
              status,
              durationMs,
              message,
              throwable,
              timestamp
            )
            IO(sendTestEvent(originId, s"test:$project:$suite", protocolEvent))

          case TaskDag.DagEvent.SuitesDiscovered(project, suites, timestamp) =>
            for {
              total <- totalSuitesRef.updateAndGet(_ + suites.size)
              _ <- IO(
                sendTestEvent(
                  originId,
                  s"discover:$project",
                  BleepBspProtocol.Event.SuitesDiscovered(project, suites, total, timestamp)
                )
              )
            } yield ()

          case TaskDag.DagEvent.TaskProgress(task, percent, timestamp) =>
            task match {
              case ct: TaskDag.CompileTask =>
                val protocolEvent = BleepBspProtocol.Event.CompileProgress(ct.project.value, percent, timestamp)
                IO(sendTestEvent(originId, task.id, protocolEvent))
              case _ =>
                IO.unit
            }

          case TaskDag.DagEvent.Output(project, suite, line, isError, timestamp) =>
            val protocolEvent = BleepBspProtocol.Event.Output(project, suite, line, isError, timestamp)
            IO(sendTestEvent(originId, s"output:$project:$suite", protocolEvent))

          case TaskDag.DagEvent.SuiteFinished(project, suite, passed, failed, skipped, ignored, durationMs, timestamp) =>
            val protocolEvent = BleepBspProtocol.Event.SuiteFinished(project, suite, passed, failed, skipped, ignored, durationMs, timestamp)
            totalPassedRef.update(_ + passed) >>
              totalFailedRef.update(_ + failed) >>
              totalSkippedRef.update(_ + skipped) >>
              totalIgnoredRef.update(_ + ignored) >>
              IO(sendTestEvent(originId, s"suite:$project:$suite", protocolEvent))

          case TaskDag.DagEvent.LinkStarted(project, platform, timestamp) =>
            val protocolEvent = BleepBspProtocol.Event.LinkStarted(project, platform, timestamp)
            traceRecorder.recordStart("link", project) >>
              IO(sendTestEvent(originId, s"link:$project", protocolEvent))

          case TaskDag.DagEvent.LinkProgress(project, phase, _, timestamp) =>
            val protocolEvent = BleepBspProtocol.Event.LinkProgress(project, phase, timestamp)
            IO(sendTestEvent(originId, s"link:$project", protocolEvent))

          case TaskDag.DagEvent.LinkFinished(project, result, durationMs, timestamp) =>
            val (success, outputPath, platform, error) = result match {
              case TaskDag.LinkResult.JsSuccess(mainModule, _, _, _) => (true, Some(mainModule.toString), "Scala.js", None)
              case TaskDag.LinkResult.NativeSuccess(binary, _)       => (true, Some(binary.toString), "Scala Native", None)
              case TaskDag.LinkResult.Failure(err, _)                => (false, None, "", Some(err))
              case TaskDag.LinkResult.Killed(reason)                 => (false, None, "", Some(s"Killed: $reason"))
              case TaskDag.LinkResult.NotApplicable                  => (true, None, "JVM", None)
            }
            val protocolEvent = BleepBspProtocol.Event.LinkFinished(project, success, durationMs, outputPath, timestamp, platform, error)
            traceRecorder.recordEnd("link", project) >>
              IO(sendTestEvent(originId, s"link:$project", protocolEvent))
        }

      // If notification fails due to dead connection, trigger kill signal to stop work.
      // For other errors (e.g. serialization), log and continue.
      processEvent.handleErrorWith {
        case error: java.io.IOException =>
          IO(logger.error(s"Event send failed (connection dead): ${error.getMessage}")) >>
            killSignal.complete(Outcome.KillReason.DeadClient).attempt >> IO.raiseError(error)
        case error =>
          IO(logger.error(s"Event processing failed (continuing): ${error.getMessage}"))
      }
    }

  /** Consume compile/link events only (no test suite tracking).
    *
    * This is a simpler version of consumeEvents for compile-only and compile+link operations. It handles TaskStarted/TaskFinished for CompileTask and LinkTask,
    * as well as Link-specific events.
    */
  private def consumeCompileEvents(
      queue: Queue[IO, TaskDag.DagEvent],
      originId: Option[String],
      killSignal: Deferred[IO, Outcome.KillReason],
      traceRecorder: TraceRecorder
  ): fs2.Stream[IO, Unit] =
    fs2.Stream.fromQueueUnterminated(queue).evalMap { event =>
      // Helper to get category and name for trace recording
      def taskCatName(task: TaskDag.Task): (String, String) = task match {
        case ct: TaskDag.CompileTask   => ("compile", ct.project.value)
        case lt: TaskDag.LinkTask      => ("link", lt.project.value)
        case dt: TaskDag.DiscoverTask  => ("discover", dt.project.value)
        case tt: TaskDag.TestSuiteTask => ("test", s"${tt.project.value}:${tt.suiteName}")
      }

      def processEvent: IO[Unit] = event match {
        case TaskDag.DagEvent.TaskStarted(task, timestamp) =>
          val (cat, name) = taskCatName(task)
          val protocolEvent = task match {
            case ct: TaskDag.CompileTask =>
              logger.info(s"[COMPILE] Starting: ${ct.project.value}")
              BleepBspProtocol.Event.CompileStarted(ct.project.value, timestamp)
            case _: TaskDag.LinkTask =>
              null // Link started is emitted by LinkStarted event
            case _ =>
              null // Ignore discover/test tasks (shouldn't appear in compile DAGs)
          }
          traceRecorder.recordStart(cat, name) >>
            IO(if (protocolEvent != null) sendCompileEvent(originId, task.id, protocolEvent))

        case TaskDag.DagEvent.TaskFinished(task, result, durationMs, timestamp) =>
          val (cat, name) = taskCatName(task)
          val protocolEvent = task match {
            case ct: TaskDag.CompileTask =>
              val status = result match {
                case TaskDag.TaskResult.Success        => "success"
                case TaskDag.TaskResult.Failure(_, _)  => "failed"
                case TaskDag.TaskResult.Error(_, _, _) => "error"
                case TaskDag.TaskResult.Skipped(_)     => "skipped"
                case TaskDag.TaskResult.Killed(_)      => "cancelled"
                case TaskDag.TaskResult.Cancelled      => "cancelled"
                case TaskDag.TaskResult.TimedOut       => "timedout"
              }
              logger.info(s"[COMPILE] ${ct.project.value}: $status (${durationMs}ms)")
              result match {
                case TaskDag.TaskResult.Success =>
                  BleepBspProtocol.Event.CompileFinished(ct.project.value, "success", durationMs, Nil, skippedBecause = None, timestamp)
                case TaskDag.TaskResult.Failure(_, diags) =>
                  BleepBspProtocol.Event.CompileFinished(ct.project.value, "failed", durationMs, diags, skippedBecause = None, timestamp)
                case TaskDag.TaskResult.Error(error, _, _) =>
                  BleepBspProtocol.Event.CompileFinished(
                    ct.project.value,
                    "error",
                    durationMs,
                    List(BleepBspProtocol.Diagnostic.error(error)),
                    skippedBecause = None,
                    timestamp
                  )
                case TaskDag.TaskResult.Skipped(failedDep) =>
                  BleepBspProtocol.Event.CompileFinished(
                    ct.project.value,
                    "skipped",
                    durationMs,
                    Nil,
                    skippedBecause = Some(failedDep.project.value),
                    timestamp
                  )
                case TaskDag.TaskResult.Killed(_) =>
                  BleepBspProtocol.Event.CompileFinished(ct.project.value, "cancelled", durationMs, Nil, skippedBecause = None, timestamp)
                case TaskDag.TaskResult.Cancelled =>
                  BleepBspProtocol.Event.CompileFinished(ct.project.value, "cancelled", durationMs, Nil, skippedBecause = None, timestamp)
                case TaskDag.TaskResult.TimedOut =>
                  BleepBspProtocol.Event.CompileFinished(ct.project.value, "timedout", durationMs, Nil, skippedBecause = None, timestamp)
              }
            case _: TaskDag.LinkTask =>
              null // Link finished is emitted by LinkFinished event
            case _ =>
              null // Ignore discover/test tasks
          }
          traceRecorder.recordEnd(cat, name) >>
            IO(if (protocolEvent != null) sendCompileEvent(originId, task.id, protocolEvent))

        case TaskDag.DagEvent.TaskProgress(task, percent, timestamp) =>
          task match {
            case ct: TaskDag.CompileTask =>
              val protocolEvent = BleepBspProtocol.Event.CompileProgress(ct.project.value, percent, timestamp)
              IO(sendCompileEvent(originId, task.id, protocolEvent))
            case _ =>
              IO.unit
          }

        case TaskDag.DagEvent.LinkStarted(project, platform, timestamp) =>
          val protocolEvent = BleepBspProtocol.Event.LinkStarted(project, platform, timestamp)
          traceRecorder.recordStart("link", project) >>
            IO(sendLinkEvent(originId, s"link:$project", protocolEvent))

        case TaskDag.DagEvent.LinkProgress(project, phase, _, timestamp) =>
          val protocolEvent = BleepBspProtocol.Event.LinkProgress(project, phase, timestamp)
          IO(sendLinkEvent(originId, s"link:$project", protocolEvent))

        case TaskDag.DagEvent.LinkFinished(project, result, durationMs, timestamp) =>
          val (success, outputPath, platform, error) = result match {
            case TaskDag.LinkResult.JsSuccess(mainModule, _, _, _) => (true, Some(mainModule.toString), "Scala.js", None)
            case TaskDag.LinkResult.NativeSuccess(binary, _)       => (true, Some(binary.toString), "Scala Native", None)
            case TaskDag.LinkResult.Failure(err, _)                => (false, None, "", Some(err))
            case TaskDag.LinkResult.Killed(reason)                 => (false, None, "", Some(s"Killed: $reason"))
            case TaskDag.LinkResult.NotApplicable                  => (true, None, "JVM", None)
          }
          val protocolEvent = BleepBspProtocol.Event.LinkFinished(project, success, durationMs, outputPath, timestamp, platform, error)
          traceRecorder.recordEnd("link", project) >>
            IO(sendLinkEvent(originId, s"link:$project", protocolEvent))

        case _ =>
          // Ignore test-specific events (TestStarted, TestFinished, SuiteFinished, etc.)
          IO.unit
      }

      // If notification fails due to dead connection, trigger kill signal to stop compiling.
      // For other errors (e.g. serialization), log and continue — compilation is still valid.
      processEvent.handleErrorWith {
        case error: java.io.IOException =>
          IO(logger.error(s"Compile event send failed (connection dead): ${error.getMessage}")) >>
            killSignal.complete(Outcome.KillReason.DeadClient).attempt >> IO.raiseError(error)
        case error =>
          IO(logger.error(s"Compile event processing failed (continuing): ${error.getMessage}"))
      }
    }

  /** Drain any remaining events from the queue after executor completes */
  private def drainRemainingEvents(
      queue: Queue[IO, TaskDag.DagEvent],
      originId: Option[String],
      totalSuitesRef: Ref[IO, Int],
      totalPassedRef: Ref[IO, Int],
      totalFailedRef: Ref[IO, Int],
      totalSkippedRef: Ref[IO, Int],
      totalIgnoredRef: Ref[IO, Int]
  ): IO[Unit] = {
    def drainOne: IO[Boolean] = queue.tryTake.flatMap {
      case Some(event) =>
        processEvent(event, originId, totalSuitesRef, totalPassedRef, totalFailedRef, totalSkippedRef, totalIgnoredRef) >> IO.pure(true)
      case None =>
        IO.pure(false)
    }

    def loop: IO[Unit] = drainOne.flatMap {
      case true  => loop
      case false => IO.unit
    }

    loop
  }

  /** Process a single DAG event - shared between stream consumer and drain */
  private def processEvent(
      event: TaskDag.DagEvent,
      originId: Option[String],
      totalSuitesRef: Ref[IO, Int],
      totalPassedRef: Ref[IO, Int],
      totalFailedRef: Ref[IO, Int],
      totalSkippedRef: Ref[IO, Int],
      totalIgnoredRef: Ref[IO, Int]
  ): IO[Unit] = event match {
    case TaskDag.DagEvent.TaskStarted(task, timestamp) =>
      val protocolEvent = task match {
        case ct: TaskDag.CompileTask =>
          BleepBspProtocol.Event.CompileStarted(ct.project.value, timestamp)
        case _: TaskDag.LinkTask =>
          null // Link tasks are not exposed via test protocol
        case dt: TaskDag.DiscoverTask =>
          BleepBspProtocol.Event.DiscoveryStarted(dt.project.value, timestamp)
        case tt: TaskDag.TestSuiteTask =>
          BleepBspProtocol.Event.SuiteStarted(tt.project.value, tt.suiteName, timestamp)
      }
      IO(if (protocolEvent != null) sendTestEvent(originId, task.id, protocolEvent))

    case TaskDag.DagEvent.TaskFinished(task, result, durationMs, timestamp) =>
      val protocolEvent = task match {
        case ct: TaskDag.CompileTask =>
          result match {
            case TaskDag.TaskResult.Success =>
              BleepBspProtocol.Event.CompileFinished(ct.project.value, "success", durationMs, Nil, skippedBecause = None, timestamp)
            case TaskDag.TaskResult.Failure(_, diags) =>
              BleepBspProtocol.Event.CompileFinished(ct.project.value, "failed", durationMs, diags, skippedBecause = None, timestamp)
            case TaskDag.TaskResult.Error(error, _, _) =>
              BleepBspProtocol.Event.CompileFinished(
                ct.project.value,
                "error",
                durationMs,
                List(BleepBspProtocol.Diagnostic.error(error)),
                skippedBecause = None,
                timestamp
              )
            case TaskDag.TaskResult.Skipped(failedDep) =>
              BleepBspProtocol.Event.CompileFinished(ct.project.value, "skipped", durationMs, Nil, skippedBecause = Some(failedDep.project.value), timestamp)
            case TaskDag.TaskResult.Killed(_) =>
              BleepBspProtocol.Event.CompileFinished(ct.project.value, "cancelled", durationMs, Nil, skippedBecause = None, timestamp)
            case TaskDag.TaskResult.Cancelled =>
              BleepBspProtocol.Event.CompileFinished(ct.project.value, "cancelled", durationMs, Nil, skippedBecause = None, timestamp)
            case TaskDag.TaskResult.TimedOut =>
              BleepBspProtocol.Event.CompileFinished(ct.project.value, "timedout", durationMs, Nil, skippedBecause = None, timestamp)
          }
        case _: TaskDag.LinkTask =>
          null // Link tasks are not exposed via test protocol
        case _: TaskDag.DiscoverTask =>
          null
        case tt: TaskDag.TestSuiteTask =>
          result match {
            case TaskDag.TaskResult.TimedOut =>
              BleepBspProtocol.Event.SuiteTimedOut(tt.project.value, tt.suiteName, durationMs, None, timestamp)
            case TaskDag.TaskResult.Error(error, exitCode, signal) =>
              val desc = signal match {
                case Some(sig) => s"Process crashed (signal $sig)"
                case None =>
                  exitCode match {
                    case Some(code) => s"Process exited with code $code"
                    case None       => error
                  }
              }
              BleepBspProtocol.Event.SuiteError(tt.project.value, tt.suiteName, desc, exitCode, signal, durationMs, timestamp)
            case TaskDag.TaskResult.Skipped(failedDep) =>
              BleepBspProtocol.Event.SuiteCancelled(tt.project.value, tt.suiteName, Some(s"dependency ${failedDep.project.value} failed"), timestamp)
            case TaskDag.TaskResult.Killed(_) =>
              BleepBspProtocol.Event.SuiteCancelled(tt.project.value, tt.suiteName, Some("killed"), timestamp)
            case TaskDag.TaskResult.Cancelled =>
              BleepBspProtocol.Event.SuiteCancelled(tt.project.value, tt.suiteName, Some("cancelled"), timestamp)
            case _ =>
              null
          }
      }
      IO(if (protocolEvent != null) sendTestEvent(originId, task.id, protocolEvent))

    case TaskDag.DagEvent.TestStarted(project, suite, test, timestamp) =>
      val protocolEvent = BleepBspProtocol.Event.TestStarted(project, suite, test, timestamp)
      IO(sendTestEvent(originId, s"test:$project:$suite", protocolEvent))

    case TaskDag.DagEvent.TestFinished(project, suite, test, status, durationMs, message, throwable, timestamp) =>
      val protocolEvent = BleepBspProtocol.Event.TestFinished(project, suite, test, status, durationMs, message, throwable, timestamp)
      IO(sendTestEvent(originId, s"test:$project:$suite", protocolEvent))

    case TaskDag.DagEvent.SuitesDiscovered(project, suites, timestamp) =>
      for {
        total <- totalSuitesRef.updateAndGet(_ + suites.size)
        _ <- IO(sendTestEvent(originId, s"discover:$project", BleepBspProtocol.Event.SuitesDiscovered(project, suites, total, timestamp)))
      } yield ()

    case TaskDag.DagEvent.TaskProgress(task, percent, timestamp) =>
      task match {
        case ct: TaskDag.CompileTask =>
          val protocolEvent = BleepBspProtocol.Event.CompileProgress(ct.project.value, percent, timestamp)
          IO(sendTestEvent(originId, task.id, protocolEvent))
        case _ =>
          IO.unit
      }

    case TaskDag.DagEvent.Output(project, suite, line, isError, timestamp) =>
      val protocolEvent = BleepBspProtocol.Event.Output(project, suite, line, isError, timestamp)
      IO(sendTestEvent(originId, s"output:$project:$suite", protocolEvent))

    case TaskDag.DagEvent.SuiteFinished(project, suite, passed, failed, skipped, ignored, durationMs, timestamp) =>
      val protocolEvent = BleepBspProtocol.Event.SuiteFinished(project, suite, passed, failed, skipped, ignored, durationMs, timestamp)
      totalPassedRef.update(_ + passed) >>
        totalFailedRef.update(_ + failed) >>
        totalSkippedRef.update(_ + skipped) >>
        totalIgnoredRef.update(_ + ignored) >>
        IO(sendTestEvent(originId, s"suite:$project:$suite", protocolEvent))

    case TaskDag.DagEvent.LinkStarted(project, platform, timestamp) =>
      val protocolEvent = BleepBspProtocol.Event.LinkStarted(project, platform, timestamp)
      IO(sendTestEvent(originId, s"link:$project", protocolEvent))

    case TaskDag.DagEvent.LinkProgress(project, phase, _, timestamp) =>
      val protocolEvent = BleepBspProtocol.Event.LinkProgress(project, phase, timestamp)
      IO(sendTestEvent(originId, s"link:$project", protocolEvent))

    case TaskDag.DagEvent.LinkFinished(project, result, durationMs, timestamp) =>
      val (success, outputPath, platform, error) = result match {
        case TaskDag.LinkResult.JsSuccess(mainModule, _, _, _) => (true, Some(mainModule.toString), "Scala.js", None)
        case TaskDag.LinkResult.NativeSuccess(binary, _)       => (true, Some(binary.toString), "Scala Native", None)
        case TaskDag.LinkResult.Failure(err, _)                => (false, None, "", Some(err))
        case TaskDag.LinkResult.Killed(reason)                 => (false, None, "", Some(s"Killed: $reason"))
        case TaskDag.LinkResult.NotApplicable                  => (true, None, "JVM", None)
      }
      val protocolEvent = BleepBspProtocol.Event.LinkFinished(project, success, durationMs, outputPath, timestamp, platform, error)
      IO(sendTestEvent(originId, s"link:$project", protocolEvent))
  }

  /** Convert a compiler error to a protocol Diagnostic preserving severity */
  private def toDiagnostic(error: CompilerError): BleepBspProtocol.Diagnostic = {
    val location = error.path match {
      case Some(path) => s"${path.getFileName}:${error.line}:${error.column}: "
      case None       => ""
    }
    val severity = error.severity match {
      case CompilerError.Severity.Error   => "error"
      case CompilerError.Severity.Warning => "warning"
      case CompilerError.Severity.Info    => "info"
    }
    BleepBspProtocol.Diagnostic(severity, s"$location${error.message}")
  }

  private val sendEventCounter = new java.util.concurrent.atomic.AtomicInteger(0)

  /** Send a test event via BSP notification with structured data */
  private def sendTestEvent(originId: Option[String], taskId: String, event: BleepBspProtocol.Event): Unit = {
    val n = sendEventCounter.incrementAndGet()
    val eventType = event.getClass.getSimpleName
    // Log first 20, then every 100th, and all suite-level events
    val isSuiteEvent = eventType.startsWith("Suite") || eventType == "SuitesDiscovered"
    if (n <= 20 || n % 100 == 0 || isSuiteEvent)
      logger.warn(s"[BSP-SERVER] sendTestEvent #$n: $eventType taskId=$taskId thread=${Thread.currentThread().getName}")
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

  /** Send a notification to the client.
    *
    * Notifications are best-effort — a disconnected client should not crash the server or abort compilation. Log the error and continue.
    */
  private def sendNotification[T](method: String, params: T)(using codec: JsonValueCodec[T]): Unit = {
    val notification = JsonRpcNotification(
      jsonrpc = "2.0",
      method = method,
      params = Some(RawJson(writeToArray(params)))
    )
    try transport.sendNotification(notification)
    catch {
      case e: java.io.IOException =>
        logger.error(s"Failed to send notification $method (client disconnected): ${e.getMessage}")
      case e: Exception =>
        logger.error(s"Failed to send notification $method: ${e.getMessage}", e)
    }
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

  /** Classify Scala.js test result into TaskResult, distinguishing test failures from process crashes. */
  private def classifyJsTestResult(result: ScalaJsTestRunner.TestResult): TaskDag.TaskResult =
    result.terminationReason match {
      case ScalaJsTestRunner.TerminationReason.Completed =>
        if (result.failed > 0) TaskDag.TaskResult.Failure(s"${result.failed} test(s) failed", List.empty)
        else TaskDag.TaskResult.Success
      case ScalaJsTestRunner.TerminationReason.Killed(reason) =>
        TaskDag.TaskResult.Killed(reason)
      case ScalaJsTestRunner.TerminationReason.Crashed(signal) =>
        TaskDag.TaskResult.Error(s"Process crashed (signal $signal)", Some(128 + signal), Some(signal))
      case ScalaJsTestRunner.TerminationReason.ExitCode(code) =>
        TaskDag.TaskResult.Error(s"Process exited with code $code", Some(code), None)
      case ScalaJsTestRunner.TerminationReason.Error(message) =>
        TaskDag.TaskResult.Error(message, None, None)
      case ScalaJsTestRunner.TerminationReason.TruncatedOutput(suite) =>
        TaskDag.TaskResult.Error(s"Process exited with truncated output (suite '$suite' started but never finished)", None, None)
    }

  /** Classify Kotlin test result into TaskResult, distinguishing test failures from process crashes. */
  private def classifyKotlinTestResult(result: KotlinTestRunner.TestResult): TaskDag.TaskResult =
    result.terminationReason match {
      case KotlinTestRunner.TerminationReason.Completed =>
        if (result.failed > 0) TaskDag.TaskResult.Failure(s"${result.failed} test(s) failed", List.empty)
        else TaskDag.TaskResult.Success
      case KotlinTestRunner.TerminationReason.Killed(reason) =>
        TaskDag.TaskResult.Killed(reason)
      case KotlinTestRunner.TerminationReason.Crashed(signal) =>
        TaskDag.TaskResult.Error(s"Process crashed (signal $signal)", Some(128 + signal), Some(signal))
      case KotlinTestRunner.TerminationReason.ExitCode(code) =>
        TaskDag.TaskResult.Error(s"Process exited with code $code", Some(code), None)
      case KotlinTestRunner.TerminationReason.Error(message) =>
        TaskDag.TaskResult.Error(message, None, None)
      case KotlinTestRunner.TerminationReason.TruncatedOutput(suite) =>
        TaskDag.TaskResult.Error(s"Process exited with truncated output (suite '$suite' started but never finished)", None, None)
    }

  /** Classify Scala Native test result into TaskResult, distinguishing test failures from process crashes. */
  private def classifyNativeTestResult(result: ScalaNativeTestRunner.TestResult): TaskDag.TaskResult =
    result.terminationReason match {
      case ScalaNativeTestRunner.TerminationReason.Completed =>
        if (result.failed > 0) TaskDag.TaskResult.Failure(s"${result.failed} test(s) failed", List.empty)
        else TaskDag.TaskResult.Success
      case ScalaNativeTestRunner.TerminationReason.Killed(reason) =>
        TaskDag.TaskResult.Killed(reason)
      case ScalaNativeTestRunner.TerminationReason.Crashed(signal) =>
        TaskDag.TaskResult.Error(s"Process crashed (signal $signal)", Some(128 + signal), Some(signal))
      case ScalaNativeTestRunner.TerminationReason.ExitCode(code) =>
        TaskDag.TaskResult.Error(s"Process exited with code $code", Some(code), None)
      case ScalaNativeTestRunner.TerminationReason.Error(message) =>
        TaskDag.TaskResult.Error(message, None, None)
      case ScalaNativeTestRunner.TerminationReason.TruncatedOutput(suite) =>
        TaskDag.TaskResult.Error(s"Process exited with truncated output (suite '$suite' started but never finished)", None, None)
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
    val items = params.targets.map { targetId =>
      (for {
        started <- getActiveBuild.toOption
        crossName <- crossNameFromTargetId(started, targetId)
        resolved <- started.resolvedProjects.get(crossName)
      } yield {
        val p = resolved.forceGet
        val options = p.language.javaOptions
        val classpath = p.classpath.map(_.toUri.toString)
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
