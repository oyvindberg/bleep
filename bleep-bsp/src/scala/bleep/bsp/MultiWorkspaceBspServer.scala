package bleep.bsp

import bleep.*
import bleep.analysis.{
  CancellationToken,
  CompilationReason,
  CompilePhase,
  CompilerError,
  DiagnosticListener,
  ProgressListener,
  ProjectCompileCancelled,
  ProjectCompileFailure,
  ProjectCompileSuccess,
  ProjectCompiler,
  ProjectLanguage,
  ScalaJsLinkConfig,
  ZincBridge
}
import bleep.bsp.protocol.KillReason
import bleep.bsp.protocol.{
  AnalysisCacheDto,
  AnalysisWorkspaceDto,
  BleepServerAdmin,
  BuildCacheDto,
  CopyStateRequest,
  CopyStateResponse,
  DaemonStatus,
  MachineEntryDto,
  MachineSnapshotDto,
  OperationDto,
  ServerConfigDto,
  StatusRequest,
  WorkspaceDto
}
import bleep.bsp.protocol.{BleepBspProtocol, CompileStatus, LinkPlatformName, OutputChannel, ProcessExit, SuiteOutcome}
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
import io.circe.syntax.*
import ryddig.Logger
import scala.collection.immutable.SortedSet

import scala.concurrent.duration.*

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
    machine: MachineResources,
    heapMonitor: HeapMonitor,
    kspMutexes: KspMutexes,
    buildCache: BuildCache,
    analysisCache: bleep.analysis.AnalysisCache,
    daemonInfo: DaemonInfo,
    connId: Int
) {
  import MultiWorkspaceBspServer.DebugLogging

  private val transport = new JsonRpcTransport(in, out)
  private val initialized = AtomicBoolean(false)
  private val shutdownRequested = AtomicBoolean(false)

  private val clientCapabilities = AtomicReference[Option[BuildClientCapabilities]](None)

  /** The active workspace for this connection (set during initialize) */
  private val activeWorkspace = AtomicReference[Option[Path]](None)

  /** The active build variant (set during initialize) */
  private val activeVariant = AtomicReference[model.BuildVariant](model.BuildVariant.Normal)

  /** Parsed build data from initialize, if provided by bleep client */
  private val providedBuild = AtomicReference[Option[model.Build.Exploded]](None)

  /** Fully resolved projects from the client. When present these are used as-is — no coursier, no re-resolution. */
  private val providedResolvedProjects = AtomicReference[Map[model.CrossProjectName, ResolvedProject]](Map.empty)

  /** Active build rewrites (set during initialize, applied on build load/reload) */
  private val activeRewrites = AtomicReference[List[bleep.rewrites.BuildRewrite]](Nil)

  /** Serializes KSP runs for a project across all connections in this daemon. See [[KspMutexes]]. */
  private def kspMutexFor(cn: model.CrossProjectName): IO[cats.effect.std.Mutex[IO]] =
    activeWorkspace.get() match {
      case Some(ws) => kspMutexes.forProject(ws, cn)
      case None     => IO.raiseError(BspException(JsonRpcErrorCodes.ServerNotInitialized, "KSP mutex requested before initialize set the active workspace"))
    }

  /** Whether the connected client is an IDE (Metals, IntelliJ) — set during initialize */
  private val ideClient = AtomicBoolean(false)

  /** The connected client's self-declared name ("bleep", "bleep-mcp", "Metals", ...) from the initialize handshake. Recorded in transcripts as display
    * metadata. Requests cannot run before initialize (the dispatcher rejects them), so this is always set by the time a transcript is written.
    */
  private val clientDisplayName = AtomicReference[Option[String]](None)

  /** Resolved path to com.sourcegraph:semanticdb-javac JAR for Java semanticdb support (set during initialize for IDE clients) */
  private val javaSemanticdbPlugin = AtomicReference[Option[Path]](None)

  /** This connection's view of the active build: the daemon-cached `Started`, rebound to this connection's logger.
    *
    * The cache itself is shared across connections (see [[BuildCache]]); only the logger binding is per connection.
    */
  private val activeStarted = AtomicReference[Option[Started]](None)

  /** The build id this connection asked for, so `workspace/reload` can reload the same one. */
  private val activeBuildId = AtomicReference[Option[BuildId]](None)

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

  /** What diagnostics this connection's client currently has on screen.
    *
    * Trackers are scoped per build operation (handleCompile / handleTest) — sharing one across concurrent operations would let one wipe the other's in-flight
    * state — but the memory of what was already published has to outlive the operation, since the compile that clears an error is a different operation from
    * the one that reported it.
    */
  private val diagnosticMemory = new BspDiagnosticMemory

  /** Run the server message loop with concurrent request handling.
    *
    * Notifications (like $/cancelRequest) are processed immediately. Requests are spawned in background fibers so the main loop stays responsive.
    *
    * Returns when the transport closes, or when this thread is interrupted — see [[runProgram]] for why the latter needs saying.
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
              // Cancelling the fibers runs their Resource finalizers, which is what releases this
              // connection's ProjectLocks. Do NOT call ProjectLock.releaseAllOnDaemonShutdown() here:
              // that state is process-global, so one client disconnecting would release locks still
              // held by compiles running on other connections.
              fibers.traverse_(_.cancel)
            }
        }
      )
    MultiWorkspaceBspServer.runToCompletion(program)
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
      handleRequest(request)
    } else {
      // Spawn in background fiber
      spawnRequest(request)
    }
  }

  /** Spawn a request in a background fiber. The fiber is fire-and-forget: we do NOT join it here, so the main loop stays responsive and can read subsequent
    * messages (e.g. $/cancelRequest, build/shutdown) while the request runs. The fiber self-cleans from activeFibers via guarantee.
    */
  private def spawnRequest(request: JsonRpcRequest): IO[Unit] = {
    val rpcId = request.id.map(_.key).getOrElse("unknown")

    Deferred[IO, Unit].flatMap { registered =>
      val handler: IO[Unit] =
        registered.get >>
          handleRequest(request)
            .onError { err =>
              IO.delay(logger.withContext("request", rpcId).error(s"Request handler failed: ${err.getClass.getName}: ${err.getMessage}", err))
            }
            .guarantee(IO.blocking(activeFibers.remove(rpcId)).void)

      handler.start.flatMap { fiber =>
        IO.blocking(activeFibers.put(rpcId, fiber)) >> registered.complete(()).void
      }
    }
  }

  /** Handle one request and send exactly one response.
    *
    * `dispatch(...).attempt` is what guarantees exactly-one-response: success sends the result, failure sends an error, and there is no path that sends both or
    * neither. The `activeRequests` entry is removed in a `guarantee` so it goes away even if the fiber is cancelled — the one case that reaches neither branch,
    * and deliberately sends nothing, because a cancelled request must produce no response.
    */
  private def handleRequest(request: JsonRpcRequest): IO[Unit] = IO.defer {
    val cancellationToken = request.id match {
      case Some(id) =>
        val token = CancellationToken.create()
        // `compute` rather than `put`, because a $/cancelRequest for this id may already have run:
        // it is handled inline on the reader thread while this runs on the request's own fiber, so
        // the cancel genuinely can arrive first. When it does it leaves a cancelled token behind,
        // and we adopt its state instead of overwriting it. Overwriting is how a cancel used to get
        // dropped, leaving the client with Ok for a build it had cancelled.
        activeRequests.compute(
          id.key,
          (_, alreadyCancelled) => {
            if (alreadyCancelled != null && alreadyCancelled.isCancelled) token.cancel()
            token
          }
        )
      case None =>
        CancellationToken.never
    }

    dispatch(request.method, request.params, cancellationToken).attempt
      .flatMap {
        case Right(result) =>
          request.id match {
            case Some(id) => IO(transport.sendResponse(JsonRpcResponse(jsonrpc = "2.0", id = id, result = result, error = None)))
            case None     => IO.unit
          }

        case Left(e: BspException) =>
          IO(request.id.foreach(id => trySendResponse(id, None, Some(JsonRpcError(e.code, e.getMessage, None)))))

        case Left(e) =>
          val msg = Option(e.getMessage).getOrElse(e.getClass.getName)
          IO {
            System.err.println(s"[BSP] Error handling ${request.method}: $msg")
            e.printStackTrace(System.err)
            request.id.foreach(id => trySendResponse(id, None, Some(JsonRpcError(JsonRpcErrorCodes.InternalError, msg, None))))
          }
      }
      .guarantee(IO(request.id.foreach(id => activeRequests.remove(id.key))).void)
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

  /** Dispatch a method call to the appropriate handler.
    *
    * Compile, test and run return their `IO` directly so cancellation reaches them as CE cancellation rather than a thread interrupt; the rest are still
    * synchronous and get wrapped in `IO.blocking`. The guards run inside the returned `IO` so a rejection surfaces as a failed `IO` — same error response as
    * before, no exception escaping the call.
    */
  private def dispatch(method: String, params: Option[RawJson], cancellation: CancellationToken): IO[Option[RawJson]] = IO.defer {
    logger.withContext("method", method).withContext("thread", Thread.currentThread().getName).warn("dispatch")
    // The admin methods are deliberately outside the initialize handshake: `bleep server ls` runs from any directory, including one with no build at all, and
    // must be able to connect, ask, and leave without pretending to be a BSP client.
    if !initialized.get() && method != "build/initialize" && !BleepServerAdmin.Methods.contains(method) then
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

    dispatchMethod(method, params, cancellation)
  }

  private def dispatchMethod(method: String, params: Option[RawJson], cancellation: CancellationToken): IO[Option[RawJson]] = {

    /** A handler that is still synchronous: run it on the blocking pool and wrap its result. */
    def sync(result: => Option[RawJson]): IO[Option[RawJson]] = IO.blocking(result)

    method match {
      case BleepServerAdmin.StatusMethod =>
        sync(Some(circeRaw(handleAdminStatus(parseAdminRequest(method, params)))))

      case BleepServerAdmin.ShutdownMethod =>
        sync { handleAdminShutdown(); Some(circeRaw(io.circe.Json.obj())) }

      case BleepServerAdmin.CopyStateMethod =>
        // async on purpose: takes Resource-scoped project locks, so $/cancelRequest must be able to cancel the fiber and run the finalizers
        handleCopyState(parseCopyStateRequest(params)).map(r => Some(circeRaw(r)))

      case "build/initialize" =>
        sync(Some(toRaw(handleInitialize(parseParams[InitializeBuildParams](params)))))

      case "build/initialized" =>
        sync { handleInitialized(); None }

      case "build/shutdown" =>
        sync { handleShutdown(); None }

      case "build/exit" =>
        sync { handleExit(); None }

      case "workspace/buildTargets" =>
        sync(Some(toRaw(handleBuildTargets())))

      case "workspace/reload" =>
        sync { handleReload(); None }

      case BleepBspProtocol.BuildChanged =>
        sync { handleBuildChanged(params); None }

      case "buildTarget/sources" =>
        sync(Some(toRaw(handleSources(parseParams[SourcesParams](params)))))

      case "buildTarget/dependencySources" =>
        sync(Some(toRaw(handleDependencySources(parseParams[DependencySourcesParams](params)))))

      // The three long-running operations. They return IO directly, so a $/cancelRequest cancels
      // the fiber rather than interrupting a thread parked inside unsafeRunSync.
      case "buildTarget/compile" =>
        handleCompile(parseParams[CompileParams](params), cancellation).map(r => Some(toRaw(r)))

      case "buildTarget/test" =>
        handleTest(parseParams[TestParams](params), cancellation).map(r => Some(toRaw(r)))

      case "buildTarget/run" =>
        sync(Some(toRaw(handleRun(parseParams[RunParams](params), cancellation))))

      case "buildTarget/scalacOptions" =>
        sync(Some(toRaw(handleScalacOptions(parseParams[ScalacOptionsParams](params)))))

      case "buildTarget/javacOptions" =>
        sync(Some(toRaw(handleJavacOptions(parseParams[JavacOptionsParams](params)))))

      case "buildTarget/jvmRunEnvironment" =>
        sync(Some(toRaw(handleJvmRunEnvironment(parseParams[JvmRunEnvironmentParams](params)))))

      case "buildTarget/jvmTestEnvironment" =>
        sync(Some(toRaw(handleJvmTestEnvironment(parseParams[JvmTestEnvironmentParams](params)))))

      case "buildTarget/resources" =>
        sync(Some(toRaw(handleResources(parseParams[ResourcesParams](params)))))

      case "buildTarget/outputPaths" =>
        sync(Some(toRaw(handleOutputPaths(parseParams[OutputPathsParams](params)))))

      case "buildTarget/inverseSources" =>
        sync(Some(toRaw(handleInverseSources(parseParams[InverseSourcesParams](params)))))

      case "buildTarget/dependencyModules" =>
        sync(Some(toRaw(handleDependencyModules(parseParams[DependencyModulesParams](params)))))

      case "buildTarget/jvmCompileClasspath" =>
        sync(Some(toRaw(handleJvmCompileClasspath(parseParams[JvmCompileClasspathParams](params)))))

      case "buildTarget/cleanCache" =>
        sync(Some(toRaw(handleCleanCache(parseParams[CleanCacheParams](params)))))

      case "buildTarget/scalaMainClasses" =>
        sync(Some(toRaw(handleScalaMainClasses(parseParams[ScalaMainClassesParams](params)))))

      case "buildTarget/scalaTestClasses" =>
        sync(Some(toRaw(handleScalaTestClasses(parseParams[ScalaTestClassesParams](params)))))

      case "$/cancelRequest" =>
        // Client may send $/cancelRequest with empty params {} when the
        // CompletableFuture is cancelled after the connection is closing.
        // Tolerate missing id — connection cleanup handles cancellation.
        sync {
          try handleCancelRequest(parseParams[CancelRequestParams](params))
          catch { case _: Exception => () }
          None
        }

      case "bleep/cancelBlockingWork" =>
        sync {
          activeWorkspace.get().foreach(SharedWorkspaceState.cancelAll)
          None
        }

      case _ =>
        IO.raiseError(
          BspException(
            JsonRpcErrorCodes.MethodNotFound,
            s"Method not found: $method"
          )
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
                // Nested under DataField because IDEs keep their own settings in `data` alongside it.
                io.circe.parser
                  .parse(jsonStr)
                  .flatMap(_.hcursor.get[BspBuildData.Payload](BspBuildData.DataField)) match {
                  case Right(payload) =>
                    debugLog(
                      s"Received resolved build from client (variant: ${payload.variantName}, id: ${payload.buildId.short}, ${payload.resolvedProjects.size} projects)"
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
    clientDisplayName.set(Some(params.displayName))

    val variant = parsedPayload
      .map(_.variantName)
      .map(model.BuildVariant.fromName)
      .getOrElse(
        if (isIdeClient) model.BuildVariant.BSP else model.BuildVariant.Normal
      )
    providedBuild.set(parsedPayload.map(_.build))
    providedResolvedProjects.set(parsedPayload.map(_.resolvedProjects).getOrElse(Map.empty))

    // Set up rewrites for IDE clients (SemanticDB support for goto-definition, find-references, etc.)
    val rewrites: List[bleep.rewrites.BuildRewrite] = if (isIdeClient) {
      val sdVersion = semanticDbVersionFromIde.getOrElse(model.Versions.SemanticdbScalac)
      logger.info(s"IDE client '${params.displayName}' detected, applying semanticDb rewrite with version $sdVersion")
      List(new bleep.rewrites.semanticDb(sdVersion))
    } else Nil
    activeRewrites.set(rewrites)

    // Resolve Java semanticdb plugin for IDE clients
    if (isIdeClient) {
      val javaSDVersion = javaSemanticDbVersionFromIde.getOrElse(model.Versions.SemanticdbJavac)
      logger.info(s"Resolving Java semanticdb plugin: com.sourcegraph:semanticdb-javac:$javaSDVersion")
      resolveJavaSemanticdbPlugin(javaSDVersion)
    }

    activeWorkspace.set(Some(buildRoot))
    activeVariant.set(variant)

    parsedPayload.foreach(payload => activeBuildId.set(Some(payload.buildId)))

    // The server does not load builds. Every route here goes through a bleep process that already
    // has one resolved — `bleep compile`/`test`, the MCP server, and `bleep bsp` for IDEs — so a
    // missing payload means something is misconfigured, not that we should go read bleep.yaml and
    // hope we arrive at the same build the client has.
    val buildResult = parsedPayload match {
      case Some(payload) =>
        createStartedFromExplodedBuild(buildRoot, variant, payload.build, payload.buildId)
      case None =>
        Left(
          new BleepException.Text(
            s"build/initialize from '${params.displayName}' carried no bleep build. " +
              s"bleep-bsp compiles the build its client resolves and never loads one itself. " +
              s"IDEs should launch `bleep bsp` (see .bsp/bleep.json), which supplies it."
          )
        )
    }

    buildResult match {
      case Right(started) =>
        // The cached Started is shared across connections and carries whichever logger loaded it.
        // Rebind ours so this connection's log lines carry its own context. The Lazy resolved
        // projects are shared by reference, so this keeps the resolution cache.
        activeStarted.set(Some(started.withLogger(logger)))
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
        // We emit buildTarget/didChange when a client hands us an updated build via bleep/buildChanged.
        buildTargetChangedProvider = Some(true),
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

  /** Copy compiled state from workspace `from` into a freshly created worktree `to`, under the same per-project locks compiles take.
    *
    * Per project, holding a Shared lock on the source (which blocks writers but not other readers): the classes directories, the zinc analysis, and the
    * generated sources/resources are cloned. What is deliberately NOT copied: `noop-manifest.bin` — its keys are absolute paths into `from`, and a copied
    * manifest validates successfully against those paths, yielding a false noop that points `to` at `from`'s classes. The target's first compile does one zinc
    * round-trip per project against the copied analysis (fast — the analysis is byte-identical to one already resident, see AnalysisCache) and writes its own
    * manifest. `.zinc/cache`, `ksp/` and `.bleep-lock` are also skipped: caches regenerate, and a lock file must never be inherited.
    *
    * Projects are enumerated from disk, not from a resolved build — the daemon needs neither workspace's build to be loaded, and `to` has typically never
    * connected. Lock acquisition is sorted by project name, the same global order compiles use.
    */
  private def handleCopyState(request: CopyStateRequest): IO[CopyStateResponse] = {
    import java.nio.file.{Files, Path, Paths}
    import scala.jdk.CollectionConverters.*

    val startMs = System.currentTimeMillis()

    def workspacePaths(dirStr: String, what: String): BuildPaths = {
      val dir = Paths.get(dirStr)
      if (!dir.isAbsolute || !Files.isDirectory(dir)) throw BspException(JsonRpcErrorCodes.InvalidParams, s"$what is not an existing absolute directory: $dir")
      val buildLoader = BuildLoader.find(dir)
      val buildPaths = BuildPaths(dir, buildLoader, model.BuildVariant.Normal)
      if (buildPaths.buildDir.normalize() != dir.normalize())
        throw BspException(JsonRpcErrorCodes.InvalidParams, s"$what is not a workspace root: the build for $dir lives at ${buildPaths.buildDir}")
      buildPaths
    }

    def validated: (BuildPaths, BuildPaths, List[CrossProjectName]) = {
      request.variant.foreach { v =>
        if (v != model.BuildVariant.Normal.name)
          throw BspException(JsonRpcErrorCodes.InvalidParams, s"unsupported build variant for copy-state: $v")
      }
      val fromPaths = workspacePaths(request.from, "from")
      val toPaths = workspacePaths(request.to, "to")
      if (fromPaths.buildDir == toPaths.buildDir) throw BspException(JsonRpcErrorCodes.InvalidParams, s"from and to are the same workspace: ${request.from}")
      if (!Files.isDirectory(fromPaths.projectsDir))
        throw BspException(JsonRpcErrorCodes.InvalidParams, s"${request.from} has no compiled state to copy (${fromPaths.projectsDir} does not exist)")
      if (Files.exists(toPaths.projectsDir))
        throw BspException(
          JsonRpcErrorCodes.InvalidParams,
          s"${request.to} already has state at ${toPaths.projectsDir} — copy-state is for freshly created worktrees"
        )

      // project state dirs are `.bleep/projects/<crossName.value>/builds/<variant>` where crossName.value may contain
      // slashes (`dfmt/main`) and therefore nest — a flat listing silently skips every hierarchical project
      val projects: List[CrossProjectName] = Files
        .walk(fromPaths.projectsDir, 8)
        .iterator()
        .asScala
        .filter(dir => Files.isDirectory(dir.resolve("builds").resolve(model.BuildVariant.Normal.name)))
        .map { dir =>
          val rel = fromPaths.projectsDir.relativize(dir).toString.replace(java.io.File.separatorChar, '/')
          CrossProjectName
            .fromString(rel)
            .getOrElse(throw BspException(JsonRpcErrorCodes.InvalidParams, s"not a valid project directory name: $rel"))
        }
        .toList
        .sortBy(_.value)
      (fromPaths, toPaths, projects)
    }

    /** Copies one project's state and returns the logical bytes that landed (apparent file sizes of the cloned trees — a metadata-only walk, the clone itself
      * shares blocks on APFS). Counted on the destination, so the number reports what the new worktree actually starts with.
      */
    def copyProject(fromPaths: BuildPaths, toPaths: BuildPaths, crossName: CrossProjectName): Long = {
      def dirBytes(dir: Path): Long =
        if (!Files.isDirectory(dir)) 0L
        else {
          val stream = Files.walk(dir)
          try stream.iterator().asScala.foldLeft(0L)((acc, p) => if (Files.isRegularFile(p)) acc + Files.size(p) else acc)
          finally stream.close()
        }

      def cloneIfPresent(src: Path, dest: Path): Long =
        if (Files.isDirectory(src)) { CloneDir.clone(src, dest); dirBytes(dest) }
        else 0L

      val srcVariantDir = fromPaths.variantBuildDir(crossName)
      val destVariantDir = toPaths.variantBuildDir(crossName)
      // What crosses the workspace boundary is decided by bleep.StateSharing — the same allow-list the remote cache packs by — never a local list that can
      // drift out of sync with it. (Both dir names apply unconditionally: the daemon has no resolved build here, so it cannot know which of the two a
      // project uses.)
      val variantBytes = bleep.StateSharing.variantDirEntries.foldLeft(0L) {
        case (acc, bleep.StateSharing.SharedDir(rel)) =>
          acc + cloneIfPresent(srcVariantDir.resolve(rel), destVariantDir.resolve(rel))
        case (acc, bleep.StateSharing.SharedFile(rel)) =>
          val src = srcVariantDir.resolve(rel)
          if (Files.isRegularFile(src)) {
            val dest = destVariantDir.resolve(rel)
            Files.createDirectories(dest.getParent)
            Files.copy(src, dest)
            acc + Files.size(dest)
          } else acc
      }

      variantBytes +
        cloneIfPresent(fromPaths.generatedSourcesBaseDir(crossName), toPaths.generatedSourcesBaseDir(crossName)) +
        cloneIfPresent(fromPaths.generatedResourcesBaseDir(crossName), toPaths.generatedResourcesBaseDir(crossName))
    }

    IO.blocking(validated).flatMap { case (fromPaths, toPaths, projects) =>
      projects
        .foldLeft(IO.pure((List.empty[String], 0L))) { case (acc, crossName) =>
          acc.flatMap { case (copied, bytes) =>
            ProjectLock
              .acquire(
                project = crossName,
                outputDir = fromPaths.variantBuildDir(crossName).resolve("classes"),
                mode = ProjectLock.LockMode.Shared,
                timeout = scala.concurrent.duration.FiniteDuration(60, "seconds"),
                onContention = () => logger.info(s"copy-state waiting for in-flight compile of ${crossName.value}")
              )
              .use(_ => IO.blocking(copyProject(fromPaths, toPaths, crossName)))
              .map(projectBytes => (crossName.value :: copied, bytes + projectBytes))
          }
        }
        .map { case (copied, bytes) =>
          CopyStateResponse(projects = copied.reverse, durationMs = System.currentTimeMillis() - startMs, bytesCopied = bytes)
        }
    }
  }

  /** Assemble everything `bleep server status` / `top` show. Pure reads — nothing here can disturb a compile running on another connection.
    *
    * The numbers all existed before; none of them could leave the process. `machine.snapshot` and the two caches were already constructor params, the JVM
    * vitals only ever reached `metrics.jsonl`, and uptime was not recorded anywhere until [[DaemonInfo]].
    */
  private def handleAdminStatus(request: StatusRequest): DaemonStatus = {
    if (request.observer) daemonInfo.connectionRegistry.markObserver(connId)

    val machineSnapshot = machine.snapshot.unsafeRunSync()
    val cachedWorkspaces = buildCache.cachedWorkspaces
    val analysisStats = analysisCache.stats
    val jvm = JvmSampler.sample()
    val config = daemonInfo.bootedConfig
    val nowMs = System.currentTimeMillis()

    def entry(e: MachineResources.Entry): MachineEntryDto =
      MachineEntryDto(kind = e.kind.toString, label = e.label, cpu = e.cpu, memoryMb = e.memoryMb, ageMs = e.ageMs)

    // Union of two views that genuinely disagree, and a daemon serving a workspace should appear either way: `workspaces` holds what was explicitly registered
    // (daemon args, bleep/registerWorkspace), while the build cache holds whatever a client has actually shipped a build for. Listing only the former showed
    // "no workspaces" on a daemon that had just compiled one.
    val registered = BspServerDaemon.getWorkspaces.map(_.toString).toList
    val allWorkspaces = (registered ++ cachedWorkspaces).distinct.sorted

    val workspaces = allWorkspaces.map { path =>
      val operations = SharedWorkspaceState.getActiveOperations(java.nio.file.Paths.get(path)).map { work =>
        OperationDto(
          operationId = work.operationId,
          operation = work.operation,
          projects = work.projects.toList.sorted,
          startedAgoMs = nowMs - work.startTimeMs
        )
      }
      WorkspaceDto(path = path, buildCached = cachedWorkspaces.contains(path), activeOperations = operations)
    }

    DaemonStatus(
      adminProtocolVersion = BleepServerAdmin.ProtocolVersion,
      bleepVersion = daemonInfo.bleepVersion,
      pid = daemonInfo.pid,
      startedAtEpochMs = daemonInfo.startedAtEpochMs,
      socketDir = daemonInfo.socketDir.toString,
      jvm = jvm,
      machine = MachineSnapshotDto(
        totalCpu = machineSnapshot.totalCpu,
        usedCpu = machineSnapshot.usedCpu,
        totalMemoryMb = machineSnapshot.totalMemoryMb,
        usedMemoryMb = machineSnapshot.usedMemoryMb,
        activeCompiles = machineSnapshot.activeCompiles,
        active = machineSnapshot.active.map(entry),
        waiting = machineSnapshot.waiting.map(entry)
      ),
      connections = daemonInfo.connectionRegistry.snapshot,
      workspaces = workspaces,
      buildCache = BuildCacheDto(cachedWorkspaces = cachedWorkspaces, bound = buildCache.bound),
      analysisCache = AnalysisCacheDto(
        entries = analysisStats.entries,
        fileBytes = analysisStats.fileBytes,
        internedClasses = analysisStats.internedClasses,
        sharedAnalyses = analysisStats.sharedAnalyses,
        contentHits = analysisStats.contentHits,
        perWorkspace = analysisStats.perWorkspace.map(w => AnalysisWorkspaceDto(w.key.toString, w.entries, w.fileBytes))
      ),
      config = ServerConfigDto(
        parallelism = config.effectiveParallelism,
        compileServerMaxMemory = config.compileServerMaxMemory,
        testRunnerHeap = config.testRunnerHeap,
        maxCachedWorkspaces = config.maxCachedWorkspacesFor(Runtime.getRuntime.maxMemory()),
        bspReadTimeoutMillis = config.effectiveBspReadTimeoutMillis.toLong,
        compileServerIdleTimeoutMillis = config.effectiveCompileServerIdleTimeoutMillis,
        testIdleTimeoutMinutes = config.effectiveTestIdleTimeoutMinutes,
        heapPressureThreshold = config.effectiveHeapPressureThreshold
      ),
      idleMs = Some(daemonInfo.connectionRegistry.idleMs)
    )
  }

  /** Daemon-wide shutdown, as distinct from `build/shutdown` which only ends this connection.
    *
    * Logged as a chosen exit naming the connection that asked, so it never reads as a crash in the server log afterwards.
    */
  private def handleAdminShutdown(): Unit = {
    logger.warn(s"bleep/shutdown requested by connection #$connId — shutting the daemon down")
    daemonInfo.requestDaemonShutdown()
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
      originId: Option[String],
      recorder: TranscriptRecorder
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
        ),
        recorder
      )
    }

    val kill: Runnable = () => cancelAllActiveRequests()
    val work = SharedWorkspaceState.ActiveWork(operationId, operation, projects, cancellation, System.currentTimeMillis(), kill)
    SharedWorkspaceState.register(workspace, work)
    myOperationIds.add(operationId): Unit
  }

  /** Unregister an operation after it completes. */
  private def unregisterOperation(workspace: Path, operationId: String): Unit = {
    SharedWorkspaceState.unregister(workspace, operationId)
    myOperationIds.remove(operationId): Unit
  }

  /** Cancel this connection's in-flight requests.
    *
    * Scope is the point. `activeRequests` is per-connection, and cancelling those tokens propagates into the running IOs, whose `Resource` releases then
    * `destroyForcibly` the processes THOSE requests started. That is precise and sufficient.
    *
    * There used to be a `killAllChildProcesses()` here as "belt and suspenders", walking `ProcessHandle.current().children()`. But `current()` is the DAEMON,
    * which is shared by every workspace connected to it — so a cleanup path scoped to one connection reached across all of them and SIGKILLed other clients'
    * perfectly healthy forks. One client disconnecting or sending `build/shutdown` killed another client's in-flight sourcegen and test JVMs.
    *
    * It was invisible because `destroyForcibly` here bypassed the kill-reason tracking, so the victim reported "killed by SIGKILL, not by bleep" and looked
    * exactly like an OS memory kill. Measured on a fork that died this way: RSS flat at 160MB, 8GB free, memory pressure reporting 67% free — nothing to do
    * with memory. Only forks living longer than a few seconds died, because that is how long another client needs to disconnect.
    */
  private def cancelAllActiveRequests(): Unit = {
    val activeCount = activeRequests.size()
    if (activeCount > 0) {
      logger.withContext("count", activeCount).withContext("requests", activeRequests.keySet().asScala.mkString(", ")).warn("Cancelling all active requests")
    }
    // Cancel this connection's request tokens (triggers cancellation flow in running IOs, which
    // release their Resources and kill the processes they own).
    activeRequests.values().forEach(_.cancel())
  }

  private def handleExit(): Unit = {
    // Don't exit the daemon - just close this connection
  }

  private def handleCancelRequest(params: CancelRequestParams): Unit = {
    val idStr = params.id match {
      case Left(s)  => s
      case Right(i) => i.toString
    }
    // Cancel the request's token if it has registered, and leave a cancelled token behind if it
    // hasn't — `handleRequest` adopts that state when it registers, so a cancel that beats its own
    // request is honoured rather than lost. The entry is removed by `handleRequest`'s guarantee once
    // the request runs; an id that never arrives leaves one token behind, which is why this is keyed
    // by request id rather than accumulating a separate set.
    var wasRegistered = false
    activeRequests.compute(
      idStr,
      (_, existing) =>
        if (existing != null) {
          wasRegistered = true
          existing.cancel()
          existing
        } else {
          val preCancelled = CancellationToken.create()
          preCancelled.cancel()
          preCancelled
        }
    )
    logger.withContext("id", idStr).withContext("tokenPresent", wasRegistered.toString).warn("Received cancelRequest")
  }

  // ==========================================================================
  // Build handling. The client resolves builds; we only execute them.
  // ==========================================================================

  /** Build a `Started` purely from what the client sent — no `bleep.yaml`, no `bootstrap.from`, no coursier on the compile path.
    *
    * The client has already loaded the build, applied its rewrites, and resolved every project. Repeating any of that here would at best waste time and at
    * worst produce a build that differs from the one the client believes it asked for, which is the whole failure mode this protocol exists to prevent.
    *
    * `Prebootstrapped` still wants a `BuildLoader.Existing`, and two things downstream read through it: `resolvedJvm` (which the compile path forces) and the
    * `CoursierResolver` factory. Both are derived from a `model.BuildFile`. So we synthesize one from the exploded build — every field that matters
    * (`$version`, `jvm`, `resolvers`, `scripts`, `remote-cache`) is carried on `Build.Exploded`; `projects`/`templates` are already expanded into
    * `explodedProjects` and nothing reads them from here. Handing over an empty `Existing` instead would silently fall back to the system JVM and ignore the
    * build's `jvm` setting.
    *
    * The resolver is still constructed, because `buildTarget/dependencySources` and `dependencyModules` resolve on demand — but nothing on the compile path
    * touches it.
    */
  private def createStartedFromExplodedBuild(
      buildRoot: Path,
      variant: model.BuildVariant,
      exploded: model.Build.Exploded,
      buildId: BuildId
  ): Either[BleepException, Started] =
    // Before anything is cached or compiled. The compile path throws on a missing language or platform version, which is right but arrives mid-build as an
    // IllegalStateException about one project; this reports every offender at once, at load, through the channel the client already renders (`buildLoadError`).
    model.BuildValidation.missingVersions(exploded) match {
      case Nil    => createStartedFromValidatedBuild(buildRoot, variant, exploded, buildId)
      case errors => Left(new BleepException.Text(errors.mkString("\n")))
    }

  private def createStartedFromValidatedBuild(
      buildRoot: Path,
      variant: model.BuildVariant,
      exploded: model.Build.Exploded,
      buildId: BuildId
  ): Either[BleepException, Started] =
    buildCache
      .getOrLoad(buildRoot, variant, buildId, logger) {
        val userPaths = UserPaths.fromAppDirs
        val resolvedFromClient = providedResolvedProjects.get()
        val bleepYaml = buildRoot.resolve(BuildLoader.BuildFileName)

        val syntheticBuildFile = model.BuildFile(
          $schema = model.$schema,
          $version = exploded.$version,
          templates = model.JsonMap.empty,
          scripts = model.JsonMap(exploded.scripts),
          resolvers = exploded.resolvers,
          projects = model.JsonMap.empty,
          jvm = exploded.jvm,
          `remote-cache` = exploded.remoteCache
        )

        // Round-tripped through JSON so `Existing`'s own derived members (json, wantedVersion,
        // buildFile) all agree with each other and with what we pass to the resolver factory.
        val existingBuild = BuildLoader.Existing(bleepYaml, Lazy(Right(syntheticBuildFile.asJson.noSpaces)))
        val buildPaths = BuildPaths(buildRoot, bleepYaml, variant, Some(exploded.$version))

        val missing = exploded.explodedProjects.keySet -- resolvedFromClient.keySet

        for {
          _ <-
            if (missing.isEmpty) Right(())
            else
              Left(
                new BleepException.Text(
                  s"Client sent a build for $buildRoot without resolved projects for ${missing.toList.map(_.value).sorted.mkString(", ")}. " +
                    "The server does not resolve builds itself, so there is nothing to compile these from."
                )
              )
          bleepConfig <- BleepConfigOps.loadOrDefault(userPaths)
        } yield {
          val pre = Prebootstrapped(
            logger = logger,
            userPaths = userPaths,
            buildPaths = buildPaths,
            existingBuild = existingBuild,
            ec = scala.concurrent.ExecutionContext.global
          )
          val resolver = CoursierResolver.Factory.default(pre, bleepConfig, syntheticBuildFile)

          val resolvedProjects: scala.collection.immutable.SortedMap[model.CrossProjectName, Lazy[ResolvedProject]] =
            scala.collection.immutable.SortedMap.from(
              resolvedFromClient.map { case (crossName, resolved) => crossName -> Lazy.const(resolved) }
            )

          lazy val started: Started = Started(
            pre = pre,
            rewrites = Nil,
            build = exploded,
            resolvedProjects = resolvedProjects,
            // cwd == buildDir here, which is the case bootstrap.from also answers `None` for
            activeProjectsFromPath = None,
            config = bleepConfig,
            resolver = resolver,
            bleepExecutable = Lazy(BleepExecutable.getCommand(resolver, pre, forceJvm = false)),
            bspServerClasspathSource = BspServerClasspathSource.FromCoursier(resolver),
            jvmRunner = JvmRunner.Forked
          )((_, _, _) => Right(started)) // the client owns the build; reload is its call to make
          started
        }
      }
      .map { started =>
        // Configure PlainVirtualFile with build dir for portable zinc analysis IDs
        bleep.analysis.PlainVirtualFile.setBuildDir(started.buildPaths.buildDir)
        started
      }

  private def getActiveBuild: Either[String, Started] =
    activeWorkspace.get() match {
      case None =>
        Left("No workspace set. Call build/initialize first.")
      case Some(ws) =>
        buildLoadError.get() match {
          case Some(err) => Left(err)
          case None      =>
            activeStarted.get() match {
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
    val baseUri = buildPaths.workspaceVariantDir.toUri.toASCIIString.stripSuffix("/")
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
  private def resolveJavaSemanticdbPlugin(pluginVersion: String): Unit =
    try {
      import coursier.*
      // `pluginVersion` rather than `version`: coursier 2.1.25 added a `coursier.version` package, and `import coursier.*` makes a local `version` ambiguous.
      val dep = Dependency(Module(Organization("com.sourcegraph"), ModuleName("semanticdb-javac")), coursier.version.VersionConstraint(pluginVersion))
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

  /** The client re-resolved its build and is handing us the new one.
    *
    * This is a notification, so there is nobody to return an error to: a failure is recorded in `buildLoadError`, which the next request will surface.
    */
  /** Run a main class for a target.
    *
    * The user's program is a separate OS process, forked from the daemon — the same thing already done for the forked test runner. Its output cannot go to our
    * own stdout: for a connection over the daemon socket there is nowhere useful for it to land, and for the IDE it has to arrive as protocol messages. So both
    * streams are captured and republished as `build/logMessage`, which is what BSP provides for this and what Bloop does.
    *
    * The child gets no stdin. BSP has no channel for feeding input to a running program — Bloop has the same hole, and the accepted answers are a debug session
    * or the client's own terminal. A program that reads stdin therefore sees EOF rather than blocking forever.
    *
    * Compilation is the caller's business: `bleep run` compiles through this server and then forks locally, and an IDE issues its own compile first.
    */
  private def handleRun(params: RunParams, cancellation: CancellationToken): RunResult = {
    val started = getActiveBuild.fold(msg => throw BspException(JsonRpcErrorCodes.InternalError, msg), identity)
    val crossName = crossNameFromTargetId(started, params.target)
      .getOrElse(throw BspException(JsonRpcErrorCodes.InvalidParams, s"Unknown target: ${params.target.uri.value}"))

    val scalaMainClass: Option[ScalaMainClass] =
      params.dataKind match {
        case Some("scala-main-class") =>
          params.data.map(raw => readFromArray[ScalaMainClass](raw.value)(using ScalaMainClass.codec))
        case _ => None
      }

    val mainClass = scalaMainClass
      .map(_.className)
      .orElse(started.build.explodedProjects.get(crossName).flatMap(_.platform).flatMap(_.mainClass))
      .getOrElse(throw BspException(JsonRpcErrorCodes.InvalidParams, s"No main class for ${crossName.value}"))

    val resolved = started.resolvedProject(crossName)
    val classpath = started.projectPaths(crossName).classes :: resolved.classpath.map(p => Path.of(p.toString)).toList
    val jvmOptions = scalaMainClass.map(_.jvmOptions).getOrElse(Nil)
    // Bloop reads program arguments only from the ScalaMainClass payload and spends `arguments` on
    // compile flags; we accept either, since the field is named for this. Clients commonly send both
    // (our own test client does), so prefer the payload rather than concatenating and running the
    // program with every argument twice.
    val programArgs = scalaMainClass.map(_.arguments).filter(_.nonEmpty).orElse(params.arguments).getOrElse(Nil)

    val command =
      started.jvmCommand.toString ::
        jvmOptions ::: List("-cp", classpath.map(_.toString).mkString(java.io.File.pathSeparator), mainClass) ::: programArgs

    val builder = new ProcessBuilder(command.asJava)
    builder.directory(started.buildPaths.buildDir.toFile)
    builder.redirectErrorStream(true)
    builder.redirectInput(ProcessBuilder.Redirect.from(new java.io.File(if (scala.util.Properties.isWin) "NUL" else "/dev/null")))
    // Weakest first, matching computeTestEnvironment: the daemon's inherited env is the base, then the build's declaration, then the two channels through
    // which a client states env explicitly for this one run. `params.environmentVariables` is the standard BSP field and was previously ignored outright.
    val projectEnv = started.build.explodedProjects.get(crossName).flatMap(_.platform).map(_.jvmEnvironment.toMap).getOrElse(Map.empty)
    val requestEnv = params.environmentVariables.getOrElse(Map.empty)
    val mainClassEnv = scalaMainClass
      .flatMap(_.environmentVariables)
      .getOrElse(Nil)
      .flatMap { entry =>
        val idx = entry.indexOf('=')
        if (idx > 0) Some(entry.substring(0, idx) -> entry.substring(idx + 1)) else None
      }
      .toMap
    (projectEnv ++ requestEnv ++ mainClassEnv).foreach { case (k, v) => builder.environment().put(k, v): Unit }

    val process = builder.start()

    // Cancellation escalates rather than going straight to SIGKILL, so a program with a shutdown
    // hook gets a chance to run it.
    cancellation.onCancel { () =>
      process.destroy()
      if (!process.waitFor(200, java.util.concurrent.TimeUnit.MILLISECONDS)) {
        process.destroyForcibly()
        process.waitFor(200, java.util.concurrent.TimeUnit.MILLISECONDS): Unit
      }
    }

    val reader = new java.io.BufferedReader(new java.io.InputStreamReader(process.getInputStream, java.nio.charset.StandardCharsets.UTF_8))
    try {
      var line = reader.readLine()
      while (line != null) {
        sendLogMessage(line, MessageType.Log)
        line = reader.readLine()
      }
    } catch {
      // `Process.destroy()` closes the parent's ends of the pipes. On Linux/BSD/AIX `ProcessImpl.destroy(boolean)` calls `stdout.close()` outright, right
      // after signalling — and `BufferedInputStream.close()` nulls `buf`, so a thread parked in `readLine()` on that same stream wakes up in
      // `getBufIfOpen` and throws `IOException("Stream closed")` rather than seeing EOF.
      //
      // Which happens here is a race. `$/cancelRequest` is in `immediatelyHandled`, so the `onCancel` above runs inline on the message-loop thread while
      // this fiber is still parked in the read below. If the blocked read completes before `close()` nulls the buffer, the loop ends on a clean EOF; if
      // `close()` wins, it throws. Both are the same event — the run was cancelled — reported two different ways.
      //
      // Without this catch the throwing half escaped `handleRun` before the status below could be computed, so `dispatch(...).attempt` saw `Left(...)` and
      // answered the client `-32603 Stream closed` instead of `StatusCode.Cancelled`. That is why
      // `BspRunIntegrationTest / BSP: async run can be cancelled` looked flaky rather than broken: same run, ubuntu-22.04 green and ubuntu-22.04-arm red.
      //
      // Guarded on the token rather than swallowed: an IOException on a run nobody cancelled still propagates and still fails loudly. The guard cannot
      // race, because `CancellationToken.create()` flips `cancelled` before it fires any listener, so the flag is already set by the time `destroy()` runs.
      case _: java.io.IOException if cancellation.isCancelled => ()
    } finally
      try reader.close()
      catch { case _: Exception => () }

    val exitCode = process.waitFor()
    val status =
      if (cancellation.isCancelled) StatusCode.Cancelled
      else if (exitCode == 0) StatusCode.Ok
      else StatusCode.Error

    RunResult(originId = params.originId, statusCode = status)
  }

  private def handleBuildChanged(params: Option[RawJson]): Unit = {
    val payload = params match {
      case None      => throw BspException(JsonRpcErrorCodes.InvalidParams, s"${BleepBspProtocol.BuildChanged} requires params")
      case Some(raw) =>
        circeDecode[BspBuildData.Payload](new String(raw.value, "UTF-8")) match {
          case Right(p)  => p
          case Left(err) => throw BspException(JsonRpcErrorCodes.InvalidParams, s"Could not parse ${BleepBspProtocol.BuildChanged}: ${err.getMessage}")
        }
    }

    val ws = activeWorkspace.get().getOrElse(throw BspException(JsonRpcErrorCodes.ServerNotInitialized, "No active workspace"))

    if (activeBuildId.get().contains(payload.buildId) && activeStarted.get().isDefined) {
      // The client watches coarsely — a touched build file that parses to the same build lands here.
      debugLog(s"Ignoring ${BleepBspProtocol.BuildChanged}: build ${payload.buildId.short} is already active")
    } else {
      val variant = model.BuildVariant.fromName(payload.variantName)
      providedBuild.set(Some(payload.build))
      providedResolvedProjects.set(payload.resolvedProjects)
      activeBuildId.set(Some(payload.buildId))
      activeVariant.set(variant)

      createStartedFromExplodedBuild(ws, variant, payload.build, payload.buildId) match {
        case Right(started) =>
          activeStarted.set(Some(started.withLogger(logger)))
          buildLoadError.set(None)
          logger
            .withContext("workspace", ws.toString)
            .withContext("buildId", payload.buildId.short)
            .withContext("projects", started.build.explodedProjects.size)
            .info("Client sent an updated build")

          // Tell the client every target may have changed. We do not diff against the previous
          // build: a changed classpath or scalac option is just as significant as an added project,
          // and the client re-queries what it cares about anyway.
          val changes = started.build.explodedProjects.keys.toList.map { crossName =>
            BuildTargetEvent(
              target = buildTargetId(started.buildPaths, crossName),
              kind = Some(BuildTargetEventKind.Changed),
              dataKind = None,
              data = None
            )
          }
          sendNotification("buildTarget/didChange", DidChangeBuildTarget(changes = changes))

        case Left(err) =>
          buildLoadError.set(Some(err.getMessage))
          logger.withContext("buildId", payload.buildId.short).error(s"Failed to adopt build sent by client: ${err.getMessage}", err)
      }
    }
  }

  private def handleReload(): Unit =
    activeWorkspace.get().foreach { ws =>
      debugLog(s"Reloading workspace: $ws")
      val variant = activeVariant.get()
      buildCache.evict(ws, variant)
      BspMetrics.recordCacheEvict("buildCache", ws.toString)

      // Re-adopt the build this connection was given. A client that wants the server on a *newer*
      // build re-resolves and sends bleep/buildChanged — `bleep bsp` does exactly that when it sees
      // workspace/reload go past, before forwarding it here.
      (providedBuild.get(), activeBuildId.get()) match {
        case (Some(exploded), Some(buildId)) =>
          createStartedFromExplodedBuild(ws, variant, exploded, buildId)
            .foreach(started => activeStarted.set(Some(started.withLogger(logger))))
        case _ =>
          logger.warn("Ignoring workspace/reload: this connection never sent a build")
      }
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

  /** Build the sourcegen plan for a set of in-scope projects.
    *
    * Walks the build model to discover every sourcegen script declared by any project in scope, and for each script collects its script-project plus that
    * script project's transitive compile deps. Returns `SourcegenPlan.empty` when no scripts are declared.
    */
  /** Walk projects in scope and collect those whose `model.Java` configures annotation processing — either by setting `scanForAnnotationProcessors: true` or by
    * listing entries in `annotationProcessors`. Projects without any AP configuration skip the DAG step entirely; their javac options receive `-proc:none` from
    * `ResolveProjects` directly without ever scheduling a `ResolveAnnotationProcessorsTask`.
    */
  private def buildAnnotationProcessorPlan(started: Started, projects: Set[CrossProjectName]): TaskDag.AnnotationProcessorPlan = {
    val configured = projects.filter { projectName =>
      started.build.explodedProjects.get(projectName).flatMap(_.java).exists { java =>
        java.scanForAnnotationProcessors.contains(true) || java.annotationProcessors.values.nonEmpty
      }
    }
    TaskDag.AnnotationProcessorPlan(configured)
  }

  /** Walk projects in scope and collect those whose `model.Kotlin` configures KSP — either by setting `scanForSymbolProcessors: true` or by listing entries in
    * `symbolProcessors`. Projects without any KSP configuration skip the DAG step entirely.
    *
    * Loud-fails when a project sets KSP but targets Kotlin/JS or Kotlin/Native. See `ksp-design.md` §25: `symbol-processing-aa-embeddable` ships only
    * `KSPJvmMain` today; there is no `KSPJsMain` / `KSPNativeMain` in KSP2. Silently dropping these projects from the plan would produce "code compiles but my @JsonClass
    * annotations did nothing" confusion. The right experience is a fast, explicit error so the user can flip the platform or drop the processor list.
    */
  private def buildSymbolProcessorPlan(started: Started, projects: Set[CrossProjectName]): TaskDag.SymbolProcessorPlan = {
    val configured = projects.filter { projectName =>
      started.build.explodedProjects.get(projectName).flatMap(_.kotlin).exists(_.hasSymbolProcessing)
    }
    configured.foreach { projectName =>
      val project = started.build.explodedProjects(projectName)
      project.platform.flatMap(_.name).foreach { platformId =>
        if (platformId != bleep.model.PlatformId.Jvm) {
          sys.error(
            s"project ${projectName.value}: kotlin.symbolProcessors / kotlin.scanForSymbolProcessors is set but platform='${platformId.value}'. " +
              "KSP2 (symbol-processing-aa-embeddable) currently ships only a JVM runner — Kotlin/JS and Kotlin/Native are not supported. " +
              "See ksp-design.md §25 for the upstream blocker."
          )
        }
      }
    }
    TaskDag.SymbolProcessorPlan(configured)
  }

  private def buildSourcegenPlan(started: Started, projects: Set[CrossProjectName]): TaskDag.SourcegenPlan = {
    val perProject: Map[CrossProjectName, Set[bleep.model.ScriptDef.Main]] =
      projects.iterator.flatMap { projectName =>
        started.build.explodedProjects.get(projectName).toList.flatMap { project =>
          val scripts: Set[bleep.model.ScriptDef.Main] =
            project.sourcegen.values.iterator.collect { case s: bleep.model.ScriptDef.Main => s }.toSet
          if (scripts.isEmpty) None else Some(projectName -> scripts)
        }
      }.toMap
    if (perProject.isEmpty) TaskDag.SourcegenPlan.empty
    else {
      val allScripts = perProject.values.flatten.toSet
      val scriptProjectDeps: Map[bleep.model.ScriptDef.Main, Set[CrossProjectName]] =
        allScripts.iterator.map { script =>
          val transitive = started.build.transitiveDependenciesFor(script.project).keySet + script.project
          script -> transitive
        }.toMap
      TaskDag.SourcegenPlan(perProject, scriptProjectDeps)
    }
  }

  /** Annotation-processor handler factory. Resolves processor JARs (Coursier `force` over each `annotationProcessors` Dep) and scans resolved-`dependencies`
    * JARs for `META-INF/services/javax.annotation.processing.Processor` entries when `scanForAnnotationProcessors` is set. Stores the resulting
    * `AnnotationProcessorResult` keyed by project in `apResults` for the compile handler to read.
    */
  private def makeAnnotationProcessorHandler(
      started: Started,
      originId: Option[String],
      apResults: java.util.concurrent.ConcurrentHashMap[CrossProjectName, AnnotationProcessorResult]
  ): (TaskDag.ResolveAnnotationProcessorsTask, Deferred[IO, KillReason]) => IO[(TaskDag.TaskResult, Int)] = {
    val _ = originId // events are emitted by the executor; this factory closes over `started` only
    (task, _) =>
      // Wrap with `attempt` so the resolver's `sys.error` for misconfig (no-op opt-in, conflicting flags) lands as
      // a TaskResult.Failure instead of an unhandled exception. This matters because the executor's
      // ResolveAnnotationProcessorsFinished event is only emitted when the handler returns a value — if the IO
      // throws, the executor's `withRecovery` catches it but the surrounding for-comprehension that emits Finished
      // never reaches that point, and the build summary's `apResolutionFailed` counter stays at zero.
      IO.blocking {
        val crossName = task.project
        val explodedProject = started.build.explodedProjects(crossName)
        val javaCfg = explodedProject.java.getOrElse {
          sys.error(s"project ${crossName.value}: scheduled for AP resolution but has no java configuration")
        }
        if (AnnotationProcessorResolver.userOptsOut(javaCfg)) {
          // Escape hatch — user opted out via -proc:none in java.options. Leave apResults empty for this project.
          (TaskDag.TaskResult.Success, 0)
        } else {
          val resolvedProject = started.resolvedProject(crossName)
          val depJars: List[Path] = resolvedProject.classpath.filter(_.toString.endsWith(".jar"))
          val versionCombo = bleep.model.VersionCombo.fromExplodedProject(explodedProject).orThrowTextWithContext(crossName)
          val genSourcesDir = started.buildPaths.generatedSourcesDir(crossName, "annotations")
          val result = AnnotationProcessorResolver.resolve(
            crossName = crossName,
            java = javaCfg,
            resolvedDependencyJars = depJars,
            versionCombo = versionCombo,
            libraryVersionSchemes = explodedProject.libraryVersionSchemes.values,
            resolver = started.resolver,
            genSourcesDir = genSourcesDir,
            logger = logger
          )
          apResults.put(crossName, result)
          (TaskDag.TaskResult.Success, result.processorJars.size)
        }
      }.attempt
        .map {
          case Right(value) => value
          case Left(error)  =>
            val msg = Option(error.getMessage).getOrElse(error.getClass.getName)
            (TaskDag.TaskResult.Failure(msg, Nil), 0)
        }
  }

  private type RunSymbolProcessorsHandler =
    (TaskDag.RunSymbolProcessorsTask, Deferred[IO, KillReason]) => IO[(TaskDag.TaskResult, Int)]

  /** Per-project KSP handler: resolves the runner classpath + processor jars, computes the incremental decision against a per-variant `inputs-manifest.json`,
    * forks `KSPJvmMain`. Generated `.kt`/`.java`/resources land under `.bleep/projects/<cross>/generated-sources/ksp/`; KSP caches + emitted `.class`es under
    * `.bleep/projects/<cross>/builds/<variant>/ksp/`.
    */
  private def makeSymbolProcessorHandler(s: Started, originId: Option[String]): RunSymbolProcessorsHandler = {
    val _ = originId
    (task, kill) =>
      val cn = task.project
      // Bridge the DAG kill signal to a CancellationToken via a lifecycle-managed `background.surround` so the watcher fiber is cancelled when the work
      // completes. Using `Outcome.bridgeKillSignal` directly would leak the watcher fiber — it `.start.void`s a `kill.get` listener with no cancellation hook,
      // so every completed task leaves behind a fiber blocked on a Deferred that never fires. Across many tests those fibers accumulate and starve the runtime.
      val cancellation = bleep.analysis.CancellationToken.create()
      val watcher = kill.get *> IO(cancellation.cancel())

      // Setup is synchronous but lives on the blocking pool. KspRunner.run is IO and handles process cancellation via Outcome.fromCancellationToken internally.
      val setupIO: IO[(SymbolProcessorResult, KspIncrementalState.Decision, KspIncrementalState, java.nio.file.Path)] = IO.blocking {
        val proj = s.build.explodedProjects(cn)
        val kot = proj.kotlin.getOrElse(sys.error(s"project ${cn.value}: scheduled for KSP but has no kotlin configuration"))
        val rp = s.resolvedProject(cn)
        val paths = s.projectPaths(cn)
        // Exclude KSP-generated dirs from the input set — otherwise each run reads its own previous output.
        val sourceRoots = (paths.sourcesDirs.fromSourceLayout.toList ++ paths.sourcesDirs.fromJson.values.toList)
          .filterNot(_.toString.contains(s"/generated-sources/${cn.value}/ksp/"))
        val ksp = SymbolProcessorResolver.resolve(
          crossName = cn,
          kotlin = kot,
          resolvedDependencyJars = rp.classpath.filter(_.toString.endsWith(".jar")),
          librariesClasspath = rp.classpath.toList,
          sourceRoots = sourceRoots,
          javaSourceRoots = sourceRoots.filter(_.getFileName.toString.matches("java|java\\..*")),
          moduleName = s"${cn.name.value}${cn.crossId.fold("")(c => s"_${c.value}")}",
          jvmTarget = kot.jvmTarget.getOrElse("11"),
          // `jvmCommand` is `<jdk>/bin/java`; KSP wants `<jdk>`. Use `toRealPath` first so a JDK reached via a symlink chain (e.g.
          // `/usr/bin/java -> /etc/alternatives/java -> /usr/lib/jvm/.../bin/java` on Linux distros) resolves to the actual JDK root, not `/usr`.
          jdkHome = s.jvmCommand.toRealPath().getParent.getParent,
          versionCombo = bleep.model.VersionCombo.fromExplodedProject(proj).orThrowTextWithContext(cn),
          libraryVersionSchemes = proj.libraryVersionSchemes.values,
          resolver = s.resolver,
          projectBaseDir = paths.dir,
          kspSharedOutputBaseDir = s.buildPaths.generatedSourcesDir(cn, "ksp"),
          kspVariantStateDir = s.buildPaths.variantBuildDir(cn).resolve("ksp"),
          resolveKspPlugin = bleep.analysis.CompilerResolver.resolveKspPlugin,
          logger = logger
        )
        val stateFile = s.buildPaths.variantBuildDir(cn).resolve("ksp/inputs-manifest.json")
        val currentInputs = KspIncrementalState.CurrentInputs(
          kspVersion = kot.kspVersion.getOrElse(""),
          kotlinVersion = kot.version.map(_.kotlinVersion).getOrElse(""),
          jdkHome = ksp.jdkHome.toString,
          jvmTarget = ksp.jvmTarget,
          languageVersion = ksp.languageVersion,
          apiVersion = ksp.apiVersion,
          processorOptions = ksp.processorOptions,
          processorJars = ksp.processorJars,
          libraries = ksp.librariesClasspath,
          sources = KspIncrementalState.listSources(ksp.sourceRoots)
        )
        // decideWithSnapshot hashes sources once and hands us back the manifest we'll persist on success — no re-hashing in save.
        val (decision, snap) = KspIncrementalState.decideWithSnapshot(stateFile, currentInputs)
        if (decision == KspIncrementalState.Decision.CacheBust && Files.exists(ksp.cachesDir))
          bleep.internal.FileUtils.deleteDirectory(ksp.cachesDir)
        logger
          .withContext("project", cn.value)
          .withContext("decision", decision.getClass.getSimpleName.stripSuffix("$"))
          .debug("KSP incremental decision")
        (ksp, decision, snap, stateFile)
      }

      val workIO: IO[(TaskDag.TaskResult, Int)] = watcher.background.surround {
        setupIO.flatMap { case (ksp, decision, snap, stateFile) =>
          // Serialize KSP runs for the same cross-project across BSP server connections (e.g. a Normal-variant compile racing a BSP-variant compile of the same
          // project). The DAG already serializes within one build, but two builds for the same project share the daemon JVM + the shared sources dir.
          // One number decides both what the fork may use and what it is charged, so the two can't
          // drift. KspRunner only emits -Xmx when this is Some, so passing it explicitly is also what
          // bounds the fork at all rather than letting HotSpot hand it a quarter of the machine.
          val kspHeapMb = MachineResources.forkHeapMb(s.config.bspServerConfigOrDefault.kspRunnerMaxMemory)
          val kspForkMemMb = MachineResources.forkFootprintMb(kspHeapMb)
          kspMutexFor(cn).flatMap(_.lock.surround {
            machine
              .reserve(MachineResources.ResourceKind.KspFork, s"ksp ${cn.value}", cpu = 1, memoryMb = kspForkMemMb)
              .use(_ => bleep.analysis.KspRunner.run(ksp, decision, s.jvmCommand, Some(s"${kspHeapMb}m"), cancellation, logger))
              .flatMap {
                case bleep.analysis.KspRunner.RunResult.Success =>
                  // Save the manifest only on success; a failed run leaves the prior manifest intact so the next try sees the same deltas and can retry.
                  IO.blocking(KspIncrementalState.save(stateFile, snap)).as((TaskDag.TaskResult.Success: TaskDag.TaskResult, ksp.processorJars.size))
                case bleep.analysis.KspRunner.RunResult.Cancelled =>
                  // KSP doesn't write atomically; a kill mid-emit can leave a half-written `.kt` in the shared sources tree that would poison the next kotlinc
                  // invocation. Wipe outputs + caches; the manifest wasn't saved either, so the next decide forces a clean FullRebuild.
                  IO.blocking {
                    List(ksp.kotlinOutputDir, ksp.javaOutputDir, ksp.resourceOutputDir, ksp.classOutputDir, ksp.cachesDir).foreach { d =>
                      if (Files.exists(d)) bleep.internal.FileUtils.deleteDirectory(d)
                    }
                  }.as((TaskDag.TaskResult.Killed(KillReason.UserRequest): TaskDag.TaskResult, 0))
                case bleep.analysis.KspRunner.RunResult.Failure(ec, msg) =>
                  val short = if (msg.length > 4000) msg.substring(0, 4000) + "\n... [truncated]" else msg
                  IO.pure((TaskDag.TaskResult.Failure(s"KSP runner exited with code $ec\n$short", Nil): TaskDag.TaskResult, 0))
              }
          })
        }
      }

      workIO.attempt.map {
        case Right(v)    => v
        case Left(error) => (TaskDag.TaskResult.Failure(Option(error.getMessage).getOrElse(error.getClass.getName), Nil), 0)
      }
  }

  /** Sourcegen handler factory. Runs a single script via `SourceGenRunner.runOne`, emits BSP progress/log events, and translates the result to a `TaskResult`.
    * A failed sourcegen returns `TaskResult.Failure` → the DAG skips downstream `CompileTask`s.
    */
  private def makeSourcegenHandler(
      started: Started,
      originId: Option[String]
  ): (TaskDag.SourcegenTask, Deferred[IO, KillReason]) => IO[TaskDag.TaskResult] = {
    val _ = originId
    val listener = new SourceGenRunner.SourceGenListener {
      def onScriptStarted(scriptMain: String, forProjects: List[String]): Unit =
        BspMetrics.recordSourcegenStart(scriptMain)

      def onScriptFinished(scriptMain: String, success: Boolean, durationMs: Long, error: Option[String]): Unit = {
        BspMetrics.recordSourcegenEnd(scriptMain, durationMs, success)
        // Record a failure in the daemon's OWN log, not only as a client notification. `bspError`
        // sends a BSP logMessage to whoever is connected and nothing more, so a failed sourcegen left
        // no trace in the server log — the one place you look when a build fails intermittently and
        // the client has moved on. Reported: grepping the server log for the failing script found
        // zero hits.
        if (!success) logger.warn(s"Sourcegen $scriptMain failed${error.fold("")(e => s": $e")}")
      }

      def onLog(message: String, isError: Boolean): Unit =
        if (isError) bspError(message) else bspInfo(message)
    }
    (sgt, killSignal) =>
      killSignal.tryGet.flatMap {
        case Some(reason) => IO.pure(TaskDag.TaskResult.Killed(reason))
        case None         =>
          // Sourcegen forks a JVM — reserve machine resources like any other fork.
          // No reservation here: the DAG admitted this task against its declared cost before starting
          // it, so reserving again would charge the machine twice for one fork.
          IO.unit.flatMap { _ =>
            SourceGenRunner
              .runOne(started, sgt.script, sgt.forProjects, killSignal, listener)
              .map {
                case None        => TaskDag.TaskResult.Success
                case Some(error) => TaskDag.TaskResult.Failure(error, Nil)
              }
          }
      }
  }

  private def handleCompile(params: CompileParams, cancellation: CancellationToken): IO[CompileResult] = IO.defer {
    val started = getActiveBuild.fold(msg => throw BspException(JsonRpcErrorCodes.InternalError, msg), identity)

    // Per-operation tracker over the connection-wide memory — see the comment on `diagnosticMemory`.
    val diagnosticTracker = new BspDiagnosticTracker(diagnosticMemory)

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
    // Accumulates every event this request streams to the client, so completion can persist the transcript. Strictly request-scoped.
    val recorder = new TranscriptRecorder
    registerOperation(workspace, taskId, opLabel, projectsToCompile.map(_.value), cancellation, params.originId, recorder)
    IO.defer {
      // Re-read user config fresh before starting (allows runtime config changes)
      val userPaths = UserPaths.fromAppDirs
      val freshConfig = BleepConfigOps.loadOrDefault(userPaths).getOrElse(model.BleepConfig.default)
      val serverConfig = freshConfig.bspServerConfigOrDefault
      // Sizes are resolved here, once, so a task can declare the same heap the fork is started with.
      val forkHeaps = TaskDag.ForkHeaps(
        sourcegenMb = MachineResources.forkFootprintMb(MachineResources.forkHeapMb(serverConfig.sourcegenMaxMemory)),
        kspMb = MachineResources.forkFootprintMb(MachineResources.forkHeapMb(serverConfig.kspRunnerMaxMemory)),
        linkMb = MachineResources.forkFootprintMb(MachineResources.forkHeapMb(None))
      )
      debugLog(s"BSP config: parallelism=${serverConfig.effectiveParallelism}")

      // Include transitive dependencies
      val allProjects = BleepBuildConverter.transitiveDependencies(projectsToCompile, started)
      debugLog(s"Compiling ${allProjects.size} projects (including dependencies)")

      // Sourcegen plan — each target project's scripts, plus script-project dep closures.
      // Tasks are added to the DAG below; failures propagate via normal dep semantics.
      val sourcegenPlan = buildSourcegenPlan(started, allProjects)

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
          // `lazy` and no default: only the isKotlin arms below read this, and there it is defined by construction. A version is not something to invent here —
          // the exploded build is every project spelled out in full, and nothing in elaboration fills one in, so a missing one is a broken build, not a
          // prompt to guess. Versions.Kotlin* are for creating and importing projects, which is a different question than compiling one.
          lazy val kotlinVersion = project.kotlin
            .flatMap(_.version)
            .map(_.kotlinVersion)
            .getOrElse(throw new IllegalStateException(s"Kotlin version not found for ${crossName.value}"))

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
              val sjsVersion = project.platform
                .flatMap(_.jsVersion)
                .map(_.scalaJsVersion)
                .getOrElse(throw new IllegalStateException(s"Scala.js version not found for ${crossName.value}"))
              val scalaVersion = project.scala
                .flatMap(_.version)
                .map(_.scalaVersion)
                .getOrElse(throw new IllegalStateException(s"Scala version not found for ${crossName.value}"))
              val baseConfig = if (isRelease) bleep.analysis.ScalaJsLinkConfig.Release else bleep.analysis.ScalaJsLinkConfig.Debug
              val config = baseConfig.copy(
                emitSourceMaps = linkOpts.sourceMaps.getOrElse(baseConfig.emitSourceMaps),
                minify = linkOpts.minify.getOrElse(baseConfig.minify),
                optimizer = linkOpts.optimize.getOrElse(baseConfig.optimizer),
                // `--module-kind` first, then the project's own `jsKind`, and only then the constant.
                //
                // The project was never consulted: a build declaring `jsKind: esmodule` got a CommonJS link and no flag was needed to cause it, because the
                // fallback was a hardcoded `CommonJSModule`. That also quietly disarmed the Closure rule below, which skips Closure for ESModule output because
                // Scala.js rejects the pairing — a yaml-declared ESModule project reached it looking like CommonJS.
                moduleKind = linkOpts.moduleKind
                  .map {
                    case "nomodule" => ScalaJsLinkConfig.ModuleKind.NoModule
                    case "esmodule" => ScalaJsLinkConfig.ModuleKind.ESModule
                    case _          => ScalaJsLinkConfig.ModuleKind.CommonJSModule
                  }
                  .orElse(project.platform.flatMap(_.jsKind).map {
                    case model.ModuleKindJS.NoModule       => ScalaJsLinkConfig.ModuleKind.NoModule
                    case model.ModuleKindJS.CommonJSModule => ScalaJsLinkConfig.ModuleKind.CommonJSModule
                    case model.ModuleKindJS.ESModule       => ScalaJsLinkConfig.ModuleKind.ESModule
                  })
                  .getOrElse(baseConfig.moduleKind)
              )
              Some(crossName -> TaskDag.LinkPlatform.ScalaJs(sjsVersion, scalaVersion, config))

            case (Some(model.PlatformId.Native), true) =>
              // Kotlin/Native
              val config = TaskDag.KotlinNativeConfig(
                target = "host",
                debugInfo = linkOpts.debugInfo.getOrElse(false),
                optimizations = linkOpts.optimize.getOrElse(isRelease),
                // From the project, not hardcoded. A Kotlin/Native test project has no `main`, so linking it as a plain binary fails in the compiler with
                // "could not find '/main' function" — and, because K2Native used to be invoked through its `main`, took the daemon down with it.
                isTest = project.isTestProject.getOrElse(false)
              )
              Some(crossName -> TaskDag.LinkPlatform.KotlinNative(kotlinVersion, config))

            case (Some(model.PlatformId.Native), false) =>
              // Scala Native
              val snVersion =
                project.platform
                  .flatMap(_.nativeVersion)
                  .map(_.scalaNativeVersion)
                  .getOrElse(throw new IllegalStateException(s"Scala Native version not found for ${crossName.value}"))
              val scalaVersion = project.scala
                .flatMap(_.version)
                .map(_.scalaVersion)
                .getOrElse(throw new IllegalStateException(s"Scala version not found for ${crossName.value}"))
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
      val linkValidationErrors = if (isLink) validateLinkOptions(linkOpts, platforms) else Nil

      if (linkValidationErrors.nonEmpty) {
        linkValidationErrors.foreach(bspError)
        IO.pure(CompileResult(originId = params.originId, statusCode = StatusCode.Error, dataKind = None, data = None))
      } else {

        // Build the appropriate DAG based on mode
        val buildMode = if (isLink) {
          BleepBspProtocol.BuildMode.Link(isRelease)
        } else {
          BleepBspProtocol.BuildMode.Compile
        }
        val apPlan = buildAnnotationProcessorPlan(started, projectsToCompile)
        val kspPlan = buildSymbolProcessorPlan(started, projectsToCompile)
        val buildCtx = TaskDag.BuildContext(
          allProjectDeps,
          platforms,
          sourcegenPlan,
          apPlan,
          kspPlan,
          testProjects = allProjectDeps.keySet.filter(p => started.build.explodedProjects(p).isTestProject.getOrElse(false))
        )
        val initialDag = TaskDag.buildDag(projectsToCompile, buildCtx, buildMode)
        debugLog(
          s"Built compile DAG with ${initialDag.tasks.size} tasks (mode=$buildMode, sourcegen-scripts=${sourcegenPlan.allScripts.size}, ap-projects=${apPlan.projects.size}, ksp-projects=${kspPlan.projects.size})"
        )

        val startTime = System.currentTimeMillis()
        BspMetrics.recordBuildStart(workspace.toString, allProjects.size)

        // Per-build map populated by the AP DAG handler and read by the compile handler.
        // ConcurrentHashMap rather than `Ref[IO, Map[...]]` because both handler factories are called synchronously here, before the IO program starts.
        // KSP doesn't need an equivalent map: the runner emits files to disk that the project's source set picks up directly; no compile-time data flow.
        val apResults = new java.util.concurrent.ConcurrentHashMap[CrossProjectName, AnnotationProcessorResult]()

        val compileHandler = makeCompileHandler(started, workspace, params.originId, apResults, diagnosticTracker, recorder)
        val sourcegenHandler = makeSourcegenHandler(started, params.originId)

        // Create link handler
        val linkHandler: (TaskDag.LinkTask, Deferred[IO, KillReason]) => IO[(TaskDag.TaskResult, TaskDag.LinkResult)] = { (linkTask, taskKillSignal) =>
          val projectPaths = started.projectPaths(linkTask.project)
          val project = started.build.explodedProjects(linkTask.project)
          val resolved = started.resolvedProject(linkTask.project)
          val classpath = projectPaths.classes :: resolved.classpath.map(p => Path.of(p.toString)).toList
          val linkLogger = createLinkLogger()
          val outputDir = projectPaths.targetDir.resolve("link-output")
          withLinkMetrics(linkTask, started.buildPaths.buildDir.toString) {
            LinkExecutor.execute(linkTask, classpath.map(_.toAbsolutePath), project.platform.flatMap(_.mainClass), outputDir, linkLogger, taskKillSignal)
          }
        }

        // No-op handlers for task types absent from compile/link DAGs (no DiscoverTasks, TestSuiteTasks here).
        val discoverHandler: (TaskDag.DiscoverTask, Option[TaskDag.LinkResult], Deferred[IO, KillReason]) => IO[(TaskDag.TaskResult, TaskDag.DiscoveryResult)] =
          (_, _, _) => sys.error("DiscoverTask should not appear in compile/link DAG")

        val testHandler: (TaskDag.TestSuiteTask, Option[TaskDag.LinkResult], Deferred[IO, KillReason]) => IO[TaskDag.TaskResult] =
          (_, _, _) => sys.error("TestSuiteTask should not appear in compile/link DAG")

        val apHandler = makeAnnotationProcessorHandler(started, params.originId, apResults)
        val kspHandler = makeSymbolProcessorHandler(started, params.originId)

        // Create executor
        val executor = TaskDag.executor(
          TaskDag.Handlers(
            compile = compileHandler,
            link = linkHandler,
            discover = discoverHandler,
            test = testHandler,
            sourcegen = sourcegenHandler,
            annotationProcessor = apHandler,
            symbolProcessor = kspHandler,
            mayAdmitCompile = makeCompileAdmission(params.originId, serverConfig.effectiveHeapPressureThreshold, recorder)
          )
        )

        def ioProgram(traceRecorder: TraceRecorder) = for {
          eventQueue <- Queue.bounded[IO, Option[TaskDag.DagEvent]](100000)
          killSignal <- Outcome.fromCancellationToken(cancellation)

          // Start event consumer fiber - use guarantee to ensure cleanup on cancellation/error
          consumerErrorRef <- Ref.of[IO, Option[Throwable]](None)
          eventConsumerFiber <- consumeCompileEvents(eventQueue, params.originId, killSignal, traceRecorder, recorder).compile.drain.handleErrorWith { e =>
            // Capture consumer error for later inspection
            IO(logger.withContext("error", e.getMessage).error("Compile event consumer error")) >>
              consumerErrorRef.set(Some(e))
          }.start

          // Run executor with guarantee to cancel consumer fiber on completion/error/cancellation
          dag <- executor
            .execute(initialDag, machine, forkHeaps, eventQueue, killSignal)
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

        for {
          // Create trace recorder (noop if not enabled)
          traceRecorder <- if (linkOpts.flamegraph) TraceRecorder.create else IO.pure(TraceRecorder.noop)
          ioResult <- ioProgram(traceRecorder).attempt

          // Write trace file if flamegraph is enabled
          _ <-
            if (linkOpts.flamegraph) traceRecorder.writeTrace(started.buildPaths.dotBleepDir.resolve("trace.json"))
            else IO.unit

          // Clear stale diagnostics for files that had errors last cycle but not this one
          _ <- IO(clearStaleDiagnostics(diagnosticTracker))

          result <- IO {
            // Persist the transcript before building the response, whatever the outcome — a failed or cancelled run's transcript is exactly what a diff wants
            // to see. The consumer fiber has been joined by now, so the recorder holds the complete event stream. `None` means the write failed (logged, never
            // fails the build) and the response carries no id.
            val historyId: Option[Long] =
              recordTranscript(started, mode = "compile", targets = projectsToCompile.map(_.value).toList.sorted, recorder = recorder, testRunResult = None)
            val (responseDataKind, responseData) = historyId match {
              case Some(id) =>
                (
                  Some(BleepBspProtocol.HistoryIdDataKind),
                  Some(RawJson(BleepBspProtocol.HistoryIdPayload.encode(BleepBspProtocol.HistoryIdPayload(id)).getBytes("UTF-8")))
                )
              case None => (None, None)
            }

            ioResult match {
              case Right(dag) =>
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
                    dataKind = responseDataKind,
                    data = responseData
                  )
                } else if (dag.failed.nonEmpty || dag.errored.nonEmpty) {
                  val failedIds = (dag.failed ++ dag.errored).mkString(", ")
                  bspError(s"Compilation failed: $compileFailed compile tasks failed, $linkFailed link tasks failed (${durationMs}ms)")
                  debugLog(s"Failed tasks: $failedIds")
                  CompileResult(
                    originId = params.originId,
                    statusCode = StatusCode.Error,
                    dataKind = responseDataKind,
                    data = responseData
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
                    dataKind = responseDataKind,
                    data = responseData
                  )
                }

              case Left(ex) =>
                val durationMs = System.currentTimeMillis() - startTime
                BspMetrics.recordBuildEnd(workspace.toString, durationMs, false)
                bspError(s"Compilation failed: ${ex.getMessage} (${durationMs}ms)")
                CompileResult(
                  originId = params.originId,
                  statusCode = StatusCode.Error,
                  dataKind = responseDataKind,
                  data = responseData
                )
            }
          }
        } yield result
      }
    }.guarantee(IO(unregisterOperation(workspace, taskId)))
  }

  /** Check `--link` options against the platforms actually being linked. Returns one message per misuse, empty if the request is coherent. */
  private def validateLinkOptions(linkOpts: ParsedLinkOptions, platforms: Map[CrossProjectName, TaskDag.LinkPlatform]): List[String] = {
    // Jvm is not a link target, so it doesn't count as a platform present for linking.
    val present: Set[TaskDag.LinkPlatform] = platforms.values.toSet - TaskDag.LinkPlatform.Jvm
    val linking = present.map(_.name.wireValue).mkString(", ")

    val validationErrors = List.newBuilder[String]

    if (present.isEmpty) {
      validationErrors += "No linkable projects found (only JVM projects can be compiled without linking)"
    }

    if (linkOpts.sourceMaps.isDefined && !present.exists(_.isJs)) {
      validationErrors += s"--source-maps/--no-source-maps only applies to JS platforms (Scala.js, Kotlin/JS), but linking: $linking"
    }
    if (linkOpts.minify.isDefined && !present.exists(_.name == LinkPlatformName.ScalaJs)) {
      validationErrors += s"--minify/--no-minify only applies to Scala.js, but linking: $linking"
    }
    if (linkOpts.moduleKind.isDefined && !present.exists(_.isJs)) {
      validationErrors += s"--module-kind only applies to JS platforms (Scala.js, Kotlin/JS), but linking: $linking"
    }
    if (linkOpts.lto.isDefined && !present.exists(_.name == LinkPlatformName.ScalaNative)) {
      validationErrors += s"--lto only applies to Scala Native, but linking: $linking"
    }
    if (linkOpts.optimize.isDefined && present.isEmpty) {
      validationErrors += s"--optimize/--no-optimize only applies to non-JVM platforms, but no linkable projects found"
    }
    // Refused rather than obeyed: it is worse on both counts a user could want. Measured on a stdlib-using program, `--release --no-optimize` produced 288,209
    // bytes against release's 152,896 and took longer doing it — the optimizer shrinks the program before the Closure compiler has to read it, so dropping the
    // optimizer hands Closure more work and yields more output. Scoped to Scala.js because that is where Closure runs and where this was measured.
    if (linkOpts.isRelease && linkOpts.optimize.contains(false) && present.exists(_.name == LinkPlatformName.ScalaJs)) {
      validationErrors +=
        "--no-optimize with --release produces larger Scala.js output (~1.9x) and takes longer, because the optimizer reduces the work the Closure compiler " +
          "then has to do. Drop --no-optimize for a deployable build, or drop --release for a fast one."
    }
    if (linkOpts.debugInfo.isDefined && !present.exists(_.isNative)) {
      validationErrors += s"--debug-info/--no-debug-info only applies to native platforms (Scala Native, Kotlin/Native), but linking: $linking"
    }

    validationErrors.result()
  }

  /** Create a HeapPressureGate.Listener that sends BSP events and logs */
  private def makeHeapPressureListener(originId: Option[String], recorder: TranscriptRecorder): HeapPressureGate.Listener =
    new HeapPressureGate.Listener {
      def onWait(project: String, used: HeapMb, max: HeapMb, delayMs: Long, now: EpochMs): Unit = {
        val retryAt = EpochMs(now.value + delayMs)
        sendEvent(
          originId,
          s"compile:$project",
          BleepBspProtocol.Event.CompileStalled(CrossProjectName.fromString(project).get, used.value, max.value, retryAt.value, now.value),
          recorder
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
          BleepBspProtocol.Event.CompileResumed(CrossProjectName.fromString(project).get, used.value, max.value, waitedFor.value, now.value),
          recorder
        )
        logger.withContext("project", project).info(s"resuming after ${waitedFor.value}ms wait (heap: ${used.value}MB/${max.value}MB)")
      }
    }

  /** Heap pressure as an ADMISSION decision, for [[TaskDag.Handlers.mayAdmitCompile]].
    *
    * This replaced an `IO.sleep` loop that ran inside the compile task. The task had already been admitted by then, so it sat on a machine-wide CPU permit
    * while waiting — withholding capacity from tests and links that could have run. Refusing admission instead leaves the permit available, and the compile is
    * reconsidered on the next wakeup, which fires whenever a task completes: exactly when heap is most likely to have been freed.
    *
    * The refusal-time map is per DAG run and is what makes [[HeapPressureGate.MaxWaitMs]] enforceable at all now that there is no sleep to measure against: it
    * remembers when each project was first deferred, across separate admission attempts.
    *
    * `othersCompiling` is `> 0`, not `> 1` as the old in-task gate used: this runs BEFORE the reservation, so this compile is not in the count yet.
    */
  private def makeCompileAdmission(originId: Option[String], threshold: Double, recorder: TranscriptRecorder): TaskDag.CompileTask => IO[Boolean] = {
    val listener = makeHeapPressureListener(originId, recorder)
    val firstRefusedAt = Ref.unsafe[IO, Map[String, EpochMs]](Map.empty)

    compileTask => {
      val projectName = compileTask.project.value
      for {
        usage <- IO(heapMonitor.heapUsage())
        compiling <- machine.activeCompiles
        nowMs <- IO.realTime.map(d => EpochMs(d.toMillis))
        refusedAt <- firstRefusedAt.get.map(_.get(projectName))
        admit <- HeapPressureGate.decide(
          usage = usage,
          othersCompiling = compiling > 0,
          threshold = threshold,
          retryMs = HeapPressureGate.DefaultRetryMs,
          firstRefusedAt = refusedAt,
          now = nowMs
        ) match {
          case HeapPressureGate.Decision.Admit =>
            refusedAt match {
              case None        => IO.pure(true)
              case Some(start) =>
                firstRefusedAt.update(_ - projectName) >>
                  IO(listener.onResume(projectName, usage.usedMb, usage.maxMb, DurationMs(nowMs.value - start.value), nowMs)).as(true)
            }
          case HeapPressureGate.Decision.Defer(delayMs) =>
            firstRefusedAt.update(m => m.updated(projectName, m.getOrElse(projectName, nowMs))) >>
              IO(listener.onWait(projectName, usage.usedMb, usage.maxMb, delayMs, nowMs)) >>
              IO(
                BspMetrics.recordAdmissionDefer(
                  project = projectName,
                  // The gate defers for two unrelated reasons and only one of them is memory. Recording which is the difference between reading this data and
                  // misreading it.
                  reason = if (usage.fraction >= threshold) "heap_pressure" else "stagger",
                  heapUsedMb = usage.usedMb.value,
                  heapMaxMb = usage.maxMb.value,
                  delayMs = delayMs,
                  othersCompiling = compiling
                )
              ).as(false)
        }
      } yield admit
    }
  }

  /** Persist the transcript of a completed compile/test request to `<workspace>/.bleep/builds/<variant>/history/` and return the assigned id.
    *
    * SANCTIONED EXCEPTION to fail-loudly: a transcript-write failure must never fail a build that already ran — the compile output and test results are real
    * regardless of whether their record made it to disk. Log the error, return None, and the response simply carries no id.
    */
  private def recordTranscript(
      started: Started,
      mode: String,
      targets: List[String],
      recorder: TranscriptRecorder,
      testRunResult: Option[BleepBspProtocol.TestRunResult]
  ): Option[Long] =
    try {
      val client = clientDisplayName.get().getOrElse(sys.error("client displayName not set — build/initialize has not run"))
      val transcript = bleep.history.TranscriptStore.write(
        buildPaths = started.buildPaths,
        timestampMs = System.currentTimeMillis(),
        mode = mode,
        targets = targets,
        client = client,
        events = recorder.events,
        testRunResult = testRunResult
      )
      logger
        .withContext("historyId", transcript.id)
        .withContext("mode", mode)
        .withContext("events", transcript.events.size)
        .debug("History transcript written")
      Some(transcript.id)
    } catch {
      case scala.util.control.NonFatal(e) =>
        logger.withContext("error", e.getMessage).withContext("mode", mode).warn("Failed to write request transcript (build result unaffected)")
        None
    }

  /** Send a structured event via BSP notification. Used for compile, link, and test events.
    *
    * This is the choke point every request event passes through, so it also feeds the request's [[TranscriptRecorder]] — what the client sees and what the
    * transcript stores are the same stream by construction.
    */
  private def sendEvent(originId: Option[String], taskId: String, event: BleepBspProtocol.Event, recorder: TranscriptRecorder): Unit = {
    recorder.record(event)
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
  private def handleTest(params: TestParams, cancellation: CancellationToken): IO[TestResult] = IO.defer {
    val started = getActiveBuild.fold(msg => throw BspException(JsonRpcErrorCodes.InternalError, msg), identity)

    // Per-operation diagnostic tracker — keeps test-pipeline compiles' diagnostic state isolated
    // from any concurrent handleCompile, which would otherwise race on it.
    val diagnosticTracker = new BspDiagnosticTracker(diagnosticMemory)

    val testProjects = params.targets.flatMap { targetId =>
      crossNameFromTargetId(started, targetId)
    }.toSet

    debugLog(s"Test request for: ${testProjects.map(_.value).mkString(", ")}")

    val taskId = java.util.UUID.randomUUID().toString
    val workspace = activeWorkspace.get().getOrElse(started.buildPaths.buildDir)
    // Accumulates every event this request streams to the client, so completion can persist the transcript. Strictly request-scoped.
    val recorder = new TranscriptRecorder
    registerOperation(workspace, taskId, "test", testProjects.map(_.value), cancellation, params.originId, recorder)
    IO.defer {
      // Re-read user config fresh before starting (allows runtime config changes)
      val userPaths = UserPaths.fromAppDirs
      val freshConfig = BleepConfigOps.loadOrDefault(userPaths).getOrElse(model.BleepConfig.default)
      val serverConfig = freshConfig.bspServerConfigOrDefault
      val maxParallelism = serverConfig.effectiveParallelism
      val forkHeaps = TaskDag.ForkHeaps(
        sourcegenMb = MachineResources.forkFootprintMb(MachineResources.forkHeapMb(serverConfig.sourcegenMaxMemory)),
        kspMb = MachineResources.forkFootprintMb(MachineResources.forkHeapMb(serverConfig.kspRunnerMaxMemory)),
        linkMb = MachineResources.forkFootprintMb(MachineResources.forkHeapMb(None))
      )

      // Sourcegen plan — scripts for test projects and their transitive deps.
      val allTestAndDeps = BleepBuildConverter.transitiveDependencies(testProjects, started)
      val sourcegenPlan = buildSourcegenPlan(started, allTestAndDeps)

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
        // See the note on the same lookup in the compile path: lazy, and a missing version is a broken build rather than a default.
        lazy val kotlinVersion = project.kotlin
          .flatMap(_.version)
          .map(_.kotlinVersion)
          .getOrElse(throw new IllegalStateException(s"Kotlin version not found for ${crossName.value}"))

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
            val sjsVersion = project.platform
              .flatMap(_.jsVersion)
              .map(_.scalaJsVersion)
              .getOrElse(throw new IllegalStateException(s"Scala.js version not found for ${crossName.value}"))
            val scalaVersion =
              project.scala.flatMap(_.version).map(_.scalaVersion).getOrElse(throw new IllegalStateException(s"Scala version not found for ${crossName.value}"))
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
              project.platform
                .flatMap(_.nativeVersion)
                .map(_.scalaNativeVersion)
                .getOrElse(throw new IllegalStateException(s"Scala Native version not found for ${crossName.value}"))
            val scalaVersion =
              project.scala.flatMap(_.version).map(_.scalaVersion).getOrElse(throw new IllegalStateException(s"Scala version not found for ${crossName.value}"))
            val config = bleep.analysis.ScalaNativeLinkConfig.Debug
            Some(crossName -> TaskDag.LinkPlatform.ScalaNative(snVersion, scalaVersion, config))

          case _ =>
            // JVM - no linking needed
            None
        }
      }.toMap

      // Build the unified DAG with platforms (includes sourcegen tasks if any)
      val apPlan = buildAnnotationProcessorPlan(started, testProjects)
      val kspPlan = buildSymbolProcessorPlan(started, testProjects)
      val buildCtx = TaskDag.BuildContext(
        allProjectDeps,
        platforms,
        sourcegenPlan,
        apPlan,
        kspPlan,
        testProjects = allProjectDeps.keySet.filter(p => started.build.explodedProjects(p).isTestProject.getOrElse(false))
      )
      val initialDag = TaskDag.buildTestDag(testProjects, buildCtx)
      debugLog(
        s"Built test DAG with ${initialDag.tasks.size} tasks, platforms: ${platforms.keys.map(_.value).mkString(", ")}, sourcegen-scripts=${sourcegenPlan.allScripts.size}, ap-projects=${apPlan.projects.size}, ksp-projects=${kspPlan.projects.size}"
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
      // Names only — the values are the client's shell environment and routinely hold credentials.
      if (testOptions.env.nonEmpty) {
        debugLog(s"Client env forwarded (${testOptions.env.size}): ${testOptions.env.keys.toList.sorted.mkString(", ")}")
      }

      val startTime = System.currentTimeMillis()

      def ioProgram(traceRecorder: TraceRecorder) = for {
        eventQueue <- Queue.bounded[IO, Option[TaskDag.DagEvent]](100000)
        totalSuitesRef <- Ref.of[IO, Int](0)
        totalPassedRef <- Ref.of[IO, Int](0)
        totalFailedRef <- Ref.of[IO, Int](0)
        totalSkippedRef <- Ref.of[IO, Int](0)
        totalIgnoredRef <- Ref.of[IO, Int](0)

        // Create kill signal from cancellation token
        killSignal <- Outcome.fromCancellationToken(cancellation)

        // Create JVM pool for test execution. The machine governor caps concurrent forks (cores +
        // fork-memory budget) across ALL clients — the per-pool maxParallelism only bounds this run.
        testResult <- JvmPool.create(maxParallelism, started.jvmCommand, started.buildPaths.buildDir, machine, BspMetrics.jvmPoolListener).use {
          jvmPool =>
            // Per-test-run map populated by the AP DAG handler and read by the compile handler. KSP runs as a separate process and emits files directly; no
            // intermediate compile-time data flow, so no equivalent map.
            val apResults = new java.util.concurrent.ConcurrentHashMap[CrossProjectName, AnnotationProcessorResult]()

          val compileHandler =
            makeCompileHandler(started, workspace, params.originId, apResults, diagnosticTracker, recorder)
          val sourcegenHandler = makeSourcegenHandler(started, params.originId)

          val includeTagsSet = testOptions.includeTags.toSet
          val excludeTagsSet = testOptions.excludeTags.toSet
          val tagsActive = includeTagsSet.nonEmpty || excludeTagsSet.nonEmpty
          val regexActive = testOptions.only.nonEmpty || testOptions.exclude.nonEmpty

          val discoverHandler
              : (TaskDag.DiscoverTask, Option[TaskDag.LinkResult], Deferred[IO, KillReason]) => IO[(TaskDag.TaskResult, TaskDag.DiscoveryResult)] =
            (discoverTask, linkOutput, discoverKill) =>
              discoverTestSuites(started, discoverTask.project, linkOutput, discoverKill).map { case (result, suites) =>
                val projectName = discoverTask.project.value
                val regexFiltered = filterSuites(suites, testOptions.only, testOptions.exclude)
                val manifest: Map[String, Set[String]] =
                  started.build.explodedProjects(discoverTask.project).testTags.value.view.mapValues(_.values.toSet).toMap
                // Discovery runs on every target the client named, libraries included. Only a project that declared itself a test project is claiming there
                // are suites here, so only that project's empty scan is a contradiction worth failing the run over.
                val isTestProject = started.build.explodedProjects(discoverTask.project).isTestProject.getOrElse(false)
                val tagFiltered =
                  if (!tagsActive) regexFiltered
                  else {
                    val fqdns = regexFiltered.map(_._1)
                    val (keptFqdns, _) = bleep.testing.TestTagFilter.filter(fqdns, manifest, includeTagsSet, excludeTagsSet)
                    val keptSet = keptFqdns.toSet
                    regexFiltered.filter { case (fqdn, _) => keptSet(fqdn) }
                  }

                // Only treat an empty result as an error when the user asked to *include* something (--only or --only-tag).
                // A pure --exclude / --exclude-tag emptying the set is the user explicitly skipping, not a misconfiguration.
                val emptyIncludesError = tagFiltered.isEmpty && (testOptions.only.nonEmpty || includeTagsSet.nonEmpty)
                if (emptyIncludesError) {
                  // Stage-by-stage diagnostic so the user can tell which filter emptied the set.
                  // Format: "<N discovered> → <M after --only/--exclude> → <K after tag filter> → 0 in scope".
                  val pipeline = {
                    val parts = scala.collection.mutable.ListBuffer.empty[String]
                    parts += s"${suites.size} discovered"
                    if (regexActive) parts += s"${regexFiltered.size} after --only/--exclude"
                    if (tagsActive) parts += s"${tagFiltered.size} after tag filter"
                    parts.mkString(" → ")
                  }
                  val whichFilters = {
                    val parts = scala.collection.mutable.ListBuffer.empty[String]
                    if (testOptions.only.nonEmpty) parts += s"--only ${testOptions.only.mkString(",")}"
                    if (testOptions.exclude.nonEmpty) parts += s"--exclude ${testOptions.exclude.mkString(",")}"
                    if (includeTagsSet.nonEmpty) parts += s"--only-tag ${includeTagsSet.mkString(",")}"
                    if (excludeTagsSet.nonEmpty) parts += s"--exclude-tag ${excludeTagsSet.mkString(",")}"
                    parts.mkString(" ")
                  }
                  val hints = scala.collection.mutable.ListBuffer.empty[String]
                  if (suites.isEmpty) hints += "No test suites were discovered in this project."
                  else if (regexActive && regexFiltered.isEmpty)
                    hints += s"Available suites: ${suites.map(_._1).mkString(", ")}"
                  else if (tagsActive && tagFiltered.isEmpty) {
                    if (manifest.isEmpty)
                      hints += s"Project ${projectName} declares no testTags; --only-tag will never match here. (Did you mean to declare tags in bleep.yaml?)"
                    else {
                      val knownTags = manifest.keys.toList.sorted.mkString(", ")
                      val sample = regexFiltered.map(_._1).take(5).mkString(", ")
                      hints += s"Tags declared in ${projectName}: $knownTags"
                      if (regexFiltered.nonEmpty)
                        hints += s"Suites that survived --only/--exclude (none of these matched the tag filter): $sample${
                            if (regexFiltered.size > 5) ", …" else ""
                          }"
                    }
                  }

                  val triggered =
                    if (testOptions.only.nonEmpty) "--only"
                    else if (includeTagsSet.nonEmpty) "--only-tag"
                    else "filter"
                  val msg =
                    s"$triggered matched no test suites in $projectName ($whichFilters): $pipeline. " + hints.mkString(" ")
                  (TaskDag.TaskResult.Failure(msg, Nil), TaskDag.DiscoveryResult(Nil, suites.size, isTestProject))
                } else {
                  (result, TaskDag.DiscoveryResult(tagFiltered, suites.size, isTestProject))
                }
              }

          val testHandler: (TaskDag.TestSuiteTask, Option[TaskDag.LinkResult], Deferred[IO, KillReason]) => IO[TaskDag.TaskResult] =
            (testTask, linkResult, taskKillSignal) =>
              // getTestClasspath ends up in CoursierResolver.Direct.go which calls Fetch.eitherResult → Await.result(future, Duration.Inf).
              // Without IO.blocking that runs synchronously on the IOFiber's compute thread, holding it for the entire resolve while
              // every other fiber on the runtime — including the BSP pipe reader on the other end of an in-process server — has to
              // queue behind it. Routing it through the blocker pool lets cats-effect grow a helper thread instead of starving compute.
              IO.blocking(getTestClasspath(started, testTask.project)).flatMap { classpath =>
                val project = started.build.explodedProjects(testTask.project)
                val projectPlatform = project.platform.flatMap(_.name)
                val isKotlin = project.kotlin.flatMap(_.version).isDefined

                // Same env on every platform: a test that reads a var should not care whether it runs on the JVM, Node or a native binary.
                val testEnv = computeTestEnvironment(started, testTask.project, testOptions.env)

                (projectPlatform, isKotlin) match {
                  case (Some(model.PlatformId.Js), true) =>
                    runKotlinJsTestSuite(started, testTask, linkedArtifactOf(testTask.project, linkResult), testEnv, eventQueue, taskKillSignal)
                  case (Some(model.PlatformId.Js), false) =>
                    runScalaJsTestSuite(started, testTask, classpath, testEnv, linkResult, eventQueue, taskKillSignal)
                  case (Some(model.PlatformId.Native), true) =>
                    runKotlinNativeTestSuite(started, testTask, linkedArtifactOf(testTask.project, linkResult), testEnv, eventQueue, taskKillSignal)
                  case (Some(model.PlatformId.Native), false) =>
                    runScalaNativeTestSuite(started, testTask, classpath, testEnv, linkResult, eventQueue, taskKillSignal)
                  case _ =>
                    // JVM (default) - use JvmPool
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
                      selection = testTask.selection,
                      classpath = classpath,
                      pool = jvmPool,
                      eventQueue = eventQueue,
                      options = TestRunner.Options(
                        // Only what someone asked for, in precedence order: the project's own options, then this run's `--jvm-opt`. The configured heap is NOT
                        // prepended here — it goes in as the default the pool falls back to, so a fork carries exactly one `-Xmx` and it is the one that
                        // decided the heap. See MachineResources.withHeapBound.
                        jvmOptions = projectJvmOptions ++ testOptions.jvmOptions,
                        defaultHeapMb = MachineResources.forkHeapMb(serverConfig.testRunnerHeap),
                        testArgs = testOptions.testArgs,
                        idleTimeout = idleTimeout,
                        environment = testEnv,
                        workingDirectory = projectDir
                      ),
                      resolveSourcePath = className =>
                        bleep.analysis.ZincSourceLookup.relativeSourceForProject(
                          bleep.analysis.AnalysisCache.Ref(analysisCache, started.buildPaths.workspaceKey),
                          started.buildPaths.variantBuildDir(testTask.project).resolve(".zinc").resolve("analysis.zip"),
                          className
                        ),
                      killSignal = taskKillSignal
                    )
                }
              }

          // Link handler for non-JVM platforms (Scala.js, Scala Native, Kotlin/JS, Kotlin/Native)
          val linkHandler: (TaskDag.LinkTask, Deferred[IO, KillReason]) => IO[(TaskDag.TaskResult, TaskDag.LinkResult)] =
            (linkTask, killSignal) =>
              // Same reasoning as testHandler — getTestClasspath synchronously Awaits a coursier resolve.
              IO.blocking(getTestClasspath(started, linkTask.project)).flatMap { classpath =>
                val projectPaths = started.projectPaths(linkTask.project)
                val logger = createLinkLogger()
                val outputDir = projectPaths.targetDir
                withLinkMetrics(linkTask, started.buildPaths.buildDir.toString) {
                  LinkExecutor.execute(linkTask, classpath.map(_.toAbsolutePath), None, outputDir, logger, killSignal)
                }
              }

          val apHandler = makeAnnotationProcessorHandler(started, params.originId, apResults)
          val kspHandler = makeSymbolProcessorHandler(started, params.originId)

          // Create executor with link + sourcegen + annotation-processor + KSP support
          val executor = TaskDag.executor(
            TaskDag.Handlers(
              compile = compileHandler,
              link = linkHandler,
              discover = discoverHandler,
              test = testHandler,
              sourcegen = sourcegenHandler,
              annotationProcessor = apHandler,
              symbolProcessor = kspHandler,
              mayAdmitCompile = makeCompileAdmission(params.originId, serverConfig.effectiveHeapPressureThreshold, recorder)
            )
          )

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
              traceRecorder,
              recorder
            ).compile.drain.handleErrorWith { e =>
              // Capture consumer error for later inspection
              IO(logger.withContext("error", e.getMessage).error("Test event consumer error")) >>
                consumerErrorRef.set(Some(e))
            }.start

            // Run executor with guarantee to cancel consumer fiber on completion/error/cancellation
            dag <- executor
              .execute(initialDag, machine, forkHeaps, eventQueue, killSignal)
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

      for {
        // Create trace recorder (noop if not enabled)
        traceRecorder <- if (testOptions.flamegraph) TraceRecorder.create else IO.pure(TraceRecorder.noop)

        _ <- IO(logger.withContext("projects", testProjects.map(_.value).mkString(", ")).warn("handleTest: starting ioProgram"))
        ioResult <- ioProgram(traceRecorder).attempt
        _ <- IO(logger.withContext("sendEventCounter", sendEventCounter.get()).warn("handleTest: ioProgram returned"))

        // Write trace file if flamegraph is enabled
        _ <-
          if (testOptions.flamegraph) traceRecorder.writeTrace(started.buildPaths.dotBleepDir.resolve("trace.json"))
          else IO.unit

        // The test pipeline compiles too, so it owes the client the same stale-diagnostic clearing a plain compile does
        _ <- IO(clearStaleDiagnostics(diagnosticTracker))

        testResult <- IO(ioResult match {
          case Right((result, totalPassed, totalFailed, totalSkipped, totalIgnored, totalSuites)) =>
            // Send TestRunFinished event
            val durationMs = System.currentTimeMillis() - startTime
            val timestamp = System.currentTimeMillis()
            sendTestEvent(
              params.originId,
              "test-run",
              BleepBspProtocol.Event.TestRunFinished(totalPassed, totalFailed, totalSkipped, totalIgnored, durationMs, timestamp),
              recorder
            )

            // Determine final status. `failed` alone is not the whole story: a task that threw (errored), hung past its
            // timeout (timedOut) or was killed also means the run did not complete cleanly — only deliberate
            // cancellation gets its own code. Skipped tasks need no clause: whatever dependency failure caused the
            // skip is itself in one of these sets.
            val statusCode =
              if (cancellation.isCancelled) StatusCode.Cancelled
              else if (result.failed.nonEmpty || result.errored.nonEmpty || result.timedOut.nonEmpty || result.killed.nonEmpty) StatusCode.Error
              else StatusCode.Ok

            // Compute suite-level counts from DAG result
            val suiteTaskIds = result.tasks.collect { case (id, _: TaskDag.TestSuiteTask) => id }.toSet
            val suitesCompleted = suiteTaskIds.count(id => result.completed.contains(id) || result.failed.contains(id) || result.timedOut.contains(id))
            val suitesFailed = suiteTaskIds.count(id => result.failed.contains(id) || result.errored.contains(id))
            val suitesCancelled = suiteTaskIds.count(id => result.killed.contains(id) || result.skipped.contains(id))

            bspInfo(s"Test completed: $totalPassed passed, $totalFailed failed, $totalSkipped skipped (${durationMs}ms)")

            // Include authoritative test results in TestResult.data for reliable delivery. The copy stored in the transcript carries historyId=None — the
            // transcript's own id is authoritative there; the copy returned to the client carries the freshly assigned id (None iff the write failed).
            val storedRunResult = BleepBspProtocol.TestRunResult(
              totalPassed = totalPassed,
              totalFailed = totalFailed,
              totalSkipped = totalSkipped,
              totalIgnored = totalIgnored,
              suitesTotal = totalSuites,
              suitesCompleted = suitesCompleted,
              suitesFailed = suitesFailed,
              suitesCancelled = suitesCancelled,
              durationMs = durationMs,
              historyId = None
            )
            val historyId: Option[Long] =
              recordTranscript(
                started,
                mode = "test",
                targets = testProjects.map(_.value).toList.sorted,
                recorder = recorder,
                testRunResult = Some(storedRunResult)
              )
            val runResult = storedRunResult.copy(historyId = historyId)

            TestResult(
              originId = params.originId,
              statusCode = statusCode,
              dataKind = Some(BleepBspProtocol.TestRunResultDataKind),
              data = Some(RawJson(BleepBspProtocol.TestRunResult.encode(runResult).getBytes("UTF-8")))
            )

          case Left(ex) =>
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
                ),
                recorder
              )
              sendTestEvent(
                params.originId,
                "test-run",
                BleepBspProtocol.Event.TestRunFinished(0, 0, 0, 0, durationMs, timestamp),
                recorder
              )
            } catch {
              case _: java.io.IOException =>
                System.err.println("[BSP] Client disconnected, cannot send test failure notification")
              case e: Exception =>
                System.err.println(s"[BSP] Failed to send test failure notification: ${e.getMessage}")
            }

            // Include zero-valued authoritative result even on failure
            val storedFailRunResult = BleepBspProtocol.TestRunResult(
              totalPassed = 0,
              totalFailed = 0,
              totalSkipped = 0,
              totalIgnored = 0,
              suitesTotal = 0,
              suitesCompleted = 0,
              suitesFailed = 0,
              suitesCancelled = 0,
              durationMs = durationMs,
              historyId = None
            )
            val historyId: Option[Long] =
              recordTranscript(
                started,
                mode = "test",
                targets = testProjects.map(_.value).toList.sorted,
                recorder = recorder,
                testRunResult = Some(storedFailRunResult)
              )
            val failRunResult = storedFailRunResult.copy(historyId = historyId)

            TestResult(
              originId = params.originId,
              statusCode = StatusCode.Error,
              dataKind = Some(BleepBspProtocol.TestRunResultDataKind),
              data = Some(RawJson(BleepBspProtocol.TestRunResult.encode(failRunResult).getBytes("UTF-8")))
            )
        })
      } yield testResult
    }.guarantee(IO(unregisterOperation(workspace, taskId)))
  }

  /** Compute dependency analysis file paths for a project's compile-time dependencies.
    *
    * Returns a map from each dependency's output directory to its Zinc analysis file. This is needed for Zinc to detect API changes in upstream projects and
    * invalidate downstream classes accordingly. Without this, Zinc treats each project as having no dependencies, missing cross-project invalidation entirely.
    */
  private def computeDependencyAnalyses(started: Started, projectDeps: Set[CrossProjectName]): Map[Path, Path] =
    projectDeps.flatMap { dep =>
      val depOutputDir = started.projectPaths(dep).classes
      val depTargetDir = started.buildPaths.variantBuildDir(dep)
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
      apResults: java.util.concurrent.ConcurrentHashMap[CrossProjectName, AnnotationProcessorResult],
      diagnosticTracker: BspDiagnosticTracker,
      recorder: TranscriptRecorder
  ): (TaskDag.CompileTask, Deferred[IO, KillReason]) => IO[TaskDag.TaskResult] =
    (compileTask, taskKillSignal) => {
      val projectName = compileTask.project.value
      val wsStr = workspace.toString
      val token = CancellationToken.create()
      taskKillSignal.tryGet.flatMap {
        case Some(_) => IO.pure(TaskDag.TaskResult.Killed(KillReason.UserRequest))
        case None    =>
          // Fast path: check noop manifest BEFORE acquiring semaphore / heap gate.
          // Noop projects skip all waiting and don't consume concurrency slots.
          val apFlags: List[String] = Option(apResults.get(compileTask.project)).fold(List.empty[String])(_.javacFlags)
          val config = BleepBuildConverter.toProjectConfig(compileTask.project, started.resolvedProject(compileTask.project), started, apFlags)
          // Transitive, not `compileTask.projectDependencies` (direct edges only): the compile
          // classpath is transitive, so an API change two hops upstream is just as breaking as
          // one hop. It is also invisible via the intermediate project's analysis mtime, because
          // an intermediate that is itself a noop never rewrites its analysis.zip.
          val depAnalyses = computeDependencyAnalyses(started, started.build.transitiveDependenciesFor(compileTask.project).keySet)
          val noopResult = config.language match {
            case sl: ProjectLanguage.ScalaJava => ZincBridge.isNoop(config, sl, depAnalyses, None)
            case _                             => None
          }
          if (noopResult.isDefined) {
            // Say WHY nothing happened: without this event a noop's transcript is just Started/Finished, indistinguishable from a compile whose reason was
            // lost. With it, two noop runs of the same project carry identical logical facts — which is what lets a mechanical diff of two noop transcripts
            // report `identical` (the copy-state verification flow depends on exactly that).
            sendEvent(
              originId,
              s"compile:$projectName",
              BleepBspProtocol.Event.CompilationReason(
                project = compileTask.project,
                reason = bleep.bsp.protocol.CompileReason.UpToDate,
                totalFiles = 0,
                invalidatedFiles = Nil,
                changedDependencies = Nil,
                timestamp = System.currentTimeMillis()
              ),
              recorder
            )
            IO.pure(TaskDag.TaskResult.Success)
          } else {
            // Cooperative cancellation: a background fiber waits for the task-level kill signal and trips the CancellationToken so the inner compile's
            // `advance()` polling sees it. `.background` gives us a Resource — fiber is spawned on acquire, cancelled on release; whether the surrounded race
            // completes via gatedCompile or waitForKill, the listener is always cleaned up. Replaces the prior `.start` + manual `.guarantee(_.cancel)` pattern.
            val cooperativeCancelFiber = taskKillSignal.get.flatMap(_ => IO(token.cancel())).background

            // Reserve one core from the machine governor for this compile — the same governor test
            // forks reserve against, so compiles and forks can't oversubscribe the CPU. A compile
            // runs in the server heap (not a forked process), so it reserves no fork memory; server
            // heap pressure is handled at admission — see Handlers.mayAdmitCompile.
            val gatedCompile =
              // Admitted by the DAG before this ran — see TaskDag.admit.
              IO.unit.flatMap { _ =>
                // The reservation IS the count of compiles in flight — held for exactly this scope,
                // across every connection, and readable via `machine.activeCompiles`. The connection-
                // local tally that used to be maintained here counted only this client's compiles,
                // which is not the quantity anything wants to know.
                val compileStartTime = System.currentTimeMillis()
                IO(BspMetrics.recordCompileStart(projectName, wsStr)) >>
                  compileProject(started, compileTask.project, originId, token, depAnalyses, apFlags, diagnosticTracker, recorder)
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
            val waitForKill = taskKillSignal.get.map(reason => TaskDag.TaskResult.Killed(reason))

            cooperativeCancelFiber.surround(IO.race(gatedCompile, waitForKill).map(_.merge))
          }
      }
    }

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
      dependencyAnalyses: Map[Path, Path],
      additionalJavaOptions: List[String],
      diagnosticTracker: BspDiagnosticTracker,
      recorder: TranscriptRecorder
  ): IO[TaskDag.TaskResult] = {
    val config = BleepBuildConverter.toProjectConfig(project, started.resolvedProject(project), started, additionalJavaOptions)
    val compiler = ProjectCompiler.forLanguage(config.language)

    // We're actually compiling this target, so this cycle owns its diagnostics — including the case where it compiles clean and publishes nothing, which is
    // precisely when the previous cycle's errors need clearing.
    diagnosticTracker.beginTarget(buildTargetId(started.buildPaths, project).uri.value)

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
            ),
          recorder
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
          ),
          recorder
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
              BleepBspProtocol.Event.CompileProgress(project, percent, now),
              recorder
            )
          }
        }
        !cancellation.isCancelled
      }
    }

    val outputDir = started.projectPaths(project).targetDir / "classes"
    val lockStart = System.currentTimeMillis()

    // Acquire locks before compiling: Exclusive on own project, Shared on each transitive dep.
    // The dep's classes dir is read by javac/Zinc during this compile (classpath, JavaAnalyze
    // class loading), so we must block any concurrent writer on those deps for the duration.
    // Sort by project name to enforce a global lock order and prevent deadlock between concurrent
    // compiles whose project sets overlap.
    val transitiveDeps = started.build.transitiveDependenciesFor(project).keySet
    val ownSpec: (CrossProjectName, Path, ProjectLock.LockMode) =
      (project, outputDir, ProjectLock.LockMode.Exclusive)
    val depSpecs: List[(CrossProjectName, Path, ProjectLock.LockMode)] =
      transitiveDeps.toList.map { d =>
        val depDir = started.projectPaths(d).targetDir / "classes"
        (d, depDir, ProjectLock.LockMode.Shared)
      }
    val orderedSpecs = (ownSpec :: depSpecs).sortBy(_._1.value)

    val locksResource: cats.effect.Resource[IO, Unit] =
      orderedSpecs.foldLeft(cats.effect.Resource.pure[IO, Unit](())) { case (acc, (proj, dir, mode)) =>
        val onContention: () => Unit = if (proj == project) { () =>
          sendEvent(
            originId,
            s"compile:${project.value}",
            BleepBspProtocol.Event.LockContention(project, 0, System.currentTimeMillis()),
            recorder
          )
        } else { () => () }

        val one = ProjectLock
          .acquire(proj, dir, mode, lockTimeout, onContention)
          .evalTap { hadContention =>
            IO {
              if (hadContention && proj == project) {
                val waited = System.currentTimeMillis() - lockStart
                sendEvent(
                  originId,
                  s"compile:${project.value}",
                  BleepBspProtocol.Event.LockAcquired(project, waited, System.currentTimeMillis()),
                  recorder
                )
              }
            }
          }
          .void
        acc.flatMap(_ => one)
      }

    locksResource
      .use { _ =>
        compiler.compile(
          config,
          diagnosticListener,
          cancellation,
          dependencyAnalyses,
          progressListener,
          // Bound to THIS build, so a compile can only ever read or charge analyses belonging to
          // the workspace it is compiling.
          bleep.analysis.AnalysisCache.Ref(analysisCache, started.buildPaths.workspaceKey)
        )
      }
      .map {
        case _ if cancellation.isCancelled =>
          TaskDag.TaskResult.Killed(KillReason.UserRequest)
        case _: ProjectCompileSuccess =>
          TaskDag.TaskResult.Success
        case f: ProjectCompileFailure =>
          val errors = f.errors.map(toDiagnostic)
          TaskDag.TaskResult.Failure("Compilation failed", errors)
        case ProjectCompileCancelled(reason) =>
          TaskDag.TaskResult.Killed(reason)
      }
  }

  /** Discover test suites in a compiled project */
  /** Filter discovered suites by --only and --exclude patterns.
    *
    * Patterns match against either the fully qualified class name or the simple class name (last segment after '.'). --exclude takes precedence over --only.
    */
  private def filterSuites[A](
      suites: List[(String, A)],
      only: List[String],
      exclude: List[String]
  ): List[(String, A)] = {
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

  /** The artifact this project's link actually produced.
    *
    * Not derived, asked. Both the Kotlin/JS and Kotlin/Native paths used to rebuild this from convention — `targetDir / linkDirSuffix(...) / "js" /
    * s"$moduleName.js"` on one side, and on the other a list of four candidate paths tried until one existed. That is the same fact expressed three times (the
    * linker's, discovery's, the run's), and the copies had already drifted: `bleep link` writes under `link-output/`, so an artifact linked by it was invisible
    * to code looking under `builds/<suffix>/`, and the "try four paths" version silently fell back to a path that did not exist.
    *
    * Throws rather than guessing when there is no link result. Every JS or Native test task has a `LinkTask` ahead of it in the DAG, so `None` here is a broken
    * graph, not a case to paper over with a default path — and a default path is precisely how a run ends up executing a stale binary from a previous build and
    * reporting it as this one's result.
    */
  private def linkedArtifactOf(project: CrossProjectName, linkOutput: Option[TaskDag.LinkResult]): Path =
    linkOutput match {
      case Some(TaskDag.LinkResult.JsSuccess(mainModule, _, _, _)) => mainModule
      case Some(TaskDag.LinkResult.NativeSuccess(binary, _))       => binary
      case Some(other)                                             =>
        throw new IllegalStateException(s"${project.value}: expected a linked artifact before running its tests, but linking reported $other")
      case None =>
        throw new IllegalStateException(
          s"${project.value}: no link result available. A JS or Native test task must run after its project's LinkTask; this means the DAG did not put one there."
        )
    }

  private def discoverTestSuites(
      started: Started,
      project: CrossProjectName,
      linkOutput: Option[TaskDag.LinkResult],
      killSignal: Deferred[IO, KillReason]
  ): IO[(TaskDag.TaskResult, List[(String, bleep.testing.FrameworkSelection)])] = IO.defer {
    val projectConfig = started.build.explodedProjects(project)
    val platformOpt = projectConfig.platform.flatMap(_.name)
    val isKotlin = projectConfig.kotlin.flatMap(_.version).isDefined

    // For Kotlin/JS and Kotlin/Native, use synthetic test suite (runtime discovery)
    // For JVM, use classpath scanning
    (platformOpt, isKotlin) match {
      // Kotlin/JS and Kotlin/Native ask the linked artifact what is in it, rather than scanning classes: there are no class files to scan. Both runners have
      // always been able to do this — `KotlinTestRunner.Js.discoverSuites` and `.Native.discoverSuites` — and neither was called, so every project got one
      // synthetic suite named after itself. The per-test events then arrived under the real suite name, matched nothing, and the reducer synthesised an extra
      // failure for a suite it thought had reported none. Real names make the two agree.
      case (Some(model.PlatformId.Js), true) =>
        kotlinDiscovered(kotlinJsSuites(started, project, linkedArtifactOf(project, linkOutput), killSignal), "kotlin-test-js", project, "KotlinJsTests")
      case (Some(model.PlatformId.Native), true) =>
        kotlinDiscovered(
          KotlinTestRunner.Native.discoverSuites(linkedArtifactOf(project, linkOutput), killSignal),
          "kotlin-test-native",
          project,
          "KotlinNativeTests"
        )

      case _ =>
        IO {
          // JVM: use classpath scanning
          val projectPaths = started.projectPaths(project)
          val classesDir = projectPaths.classes
          val resolved = started.resolvedProject(project)
          val classpath = resolved.classpath.map(p => Path.of(p.toString)).toList

          val suites = ClasspathTestDiscovery.discover(project, classesDir, classpath, resolved.testFrameworks)

          if (suites.isEmpty) {
            debugLog(s"No test suites discovered in ${project.value}")
            (TaskDag.TaskResult.Success, Nil)
          } else {
            debugLog(s"Discovered ${suites.size} test suites in ${project.value}: ${suites.map(_.className).mkString(", ")}")
            (TaskDag.TaskResult.Success, suites.map(s => (s.className, s.selection)))
          }
        }
    }
  }

  private def kotlinJsSuites(
      started: Started,
      project: CrossProjectName,
      jsOutput: Path,
      killSignal: Deferred[IO, KillReason]
  ): IO[ProcessRunner.DiscoveryResult[List[TestRunnerTypes.TestSuite]]] =
    KotlinTestRunner.Js.discoverSuites(
      jsOutput,
      nodeBinaryFor(started, started.build.explodedProjects(project)),
      killSignal
    )

  /** Turn a Kotlin platform's runtime discovery into suites for the DAG.
    *
    * An empty result is not "no tests". Both runners answer that way when the artifact cannot enumerate itself — a Kotlin/Native binary built without
    * `--ktest_list_tests` support, for instance — and the honest reading is "I cannot tell you what is in here, run the whole thing". That is what the single
    * synthetic suite has always meant, so it stays for exactly that case, and nothing else.
    */
  private def kotlinDiscovered(
      discovery: IO[ProcessRunner.DiscoveryResult[List[TestRunnerTypes.TestSuite]]],
      runnerName: String,
      project: CrossProjectName,
      syntheticSuffix: String
  ): IO[(TaskDag.TaskResult, List[(String, bleep.testing.FrameworkSelection)])] = {
    val selection = bleep.testing.FrameworkSelection.PlatformRunner(runnerName)
    discovery.map {
      case ProcessRunner.DiscoveryResult.Found(Nil) =>
        debugLog(s"${project.value}: artifact cannot enumerate its suites; running it whole")
        (TaskDag.TaskResult.Success, List((s"${project.value}:$syntheticSuffix", selection)))
      case ProcessRunner.DiscoveryResult.Found(suites) =>
        debugLog(s"Discovered ${suites.size} test suites in ${project.value}: ${suites.map(_.fullyQualifiedName).mkString(", ")}")
        (TaskDag.TaskResult.Success, suites.map(s => (s.fullyQualifiedName, selection)))
      case ProcessRunner.DiscoveryResult.Failed(message) =>
        (TaskDag.TaskResult.Failure(s"${project.value}: $message", Nil), Nil)
      case ProcessRunner.DiscoveryResult.Killed(reason) =>
        (TaskDag.TaskResult.Killed(reason), Nil)
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

    val (testRunnerClasses, testRuntimeJars) = testRunnerFromBuild match {
      case Some(path) => (List(path), Nil)
      case None       => fetchTestRunnerViaCoursier(started, project, resolved)
    }

    // `bleep-test-runner` goes ahead of the project's own dependencies; the test runtime stays behind them.
    //
    // A build that depends on bleep itself — scripts, plugins, anything pulling `build.bleep::bleep-core` — drags a *released* `bleep-test-runner` in
    // transitively. Appended last, that copy came first on the classpath, and first-match-wins classloading handed the fork the released runner instead of the
    // one this server speaks to. The two disagree about the protocol: the released runner reads commands from stdin, while this server waits for a connect-back
    // on a loopback socket. Neither side ever moves, and the spawn dies 60 seconds later on the accept timeout — with the fork still alive and blocked in
    // `readLine`, which is why it never looked like a crash.
    //
    // Only the runner moves. It is safe to put first because its jar contains nothing but `bleep/testing/runner`, so it can shadow only itself, and it is the
    // only thing here whose identity must come from the server rather than from the project. The junit runtime is the opposite case: those versions are chosen
    // to match what the project itself resolved, [[assertCoherentJunitClasspath]] checks that they do, and promoting them over the project's own jars would
    // quietly make this server the arbiter of a version it deliberately follows.
    val classpath = (classesDir :: resourceDirs) ++ testRunnerClasses ++ dependencyClasspath ++ testRuntimeJars
    MultiWorkspaceBspServer.assertCoherentJunitClasspath(project, classpath)
    classpath
  }

  /** Fetch bleep-test-runner and whatever test runtime the project's dependencies call for.
    *
    * Two parts:
    *
    *   1. `bleep-test-runner` itself, and *only* itself — see [[fetchBleepTestRunnerOnly]] for how its version is chosen (`${BLEEP_VERSION}` for a `dev` build
    *      so it short-circuits to BleepDevDeps class dirs, otherwise pinned to this server's version). Not expressible as a rule in
    *      `MultiWorkspaceBspServer.testRuntimeRules` precisely because that version comes from the server rather than from the project's dependency graph.
    *   2. Whatever `MultiWorkspaceBspServer.testRuntimeRules` says this project needs — the sbt test interface always, the junit launcher and engines only when
    *      the project resolved a junit-platform of its own, at that version. Cached per (resolver, evaluated deps); without the cache, every inner-bleep
    *      `commands.test` re-runs Coursier for the same deps, and under CI's contention that's enough to trip the suite-idle timeout in #580.
    *
    * The split only works if part 1 contributes no junit of its own. It is enforced twice, on purpose. At the source, `bleep-test-runner` no longer declares a
    * junit runtime at all: `junit-platform-launcher` is `provided` there (it compiles against the Launcher API and nothing more), and `jupiter-interface` and
    * `junit-vintage-engine` are gone. At the point of use, [[ExcludeTestRuntime]] excludes them anyway, so a POM that reacquires one cannot silently change
    * what a fork runs — the failure would be a resolution error here rather than a wrong engine on a classpath.
    *
    * What that costs when it is wrong: the runner used to declare 1.9.1/5.9.1, coursier reconciles to the *highest* version rather than to ours, and a stale
    * `junit-jupiter-engine` landed ahead of the aligned one — a kotest 6 project then died in discovery with `NoSuchMethodError: ReflectionUtils.returnsVoid`.
    */
  private def fetchTestRunnerViaCoursier(started: Started, project: CrossProjectName, resolved: ResolvedProject): (List[Path], List[Path]) = {
    val testRuntimeJars = MultiWorkspaceBspServer.fetchTestRuntimeDeps(started, project, resolved)
    val testRunnerJars = fetchBleepTestRunnerOnly(started)
    if (testRunnerJars.isEmpty)
      throw new RuntimeException("bleep-test-runner resolution returned no jars")
    // Kept apart because they land in different places on the fork's classpath — the runner ahead of the project's dependencies, the runtime behind them; see
    // [[getTestClasspath]]. The runtime side drops anything the runner already contributes, because `test-interface` is in both: rule 1 always contributes it,
    // and a *published* `bleep-test-runner` also declares it at compile scope. Exact-path equality, so two genuinely different jars that share a filename both
    // survive.
    (testRunnerJars.distinct, testRuntimeJars.distinct.filterNot(testRunnerJars.contains))
  }

  /** The forked runner speaks this server's `TestProtocol` over the fork's stdin/stdout, so it must be built from the same code as the server. Two ways to get
    * there, and picking the wrong one breaks a different workflow:
    *
    *   - `$version: dev` (integration tests, `bleep --dev`): keep the `${BLEEP_VERSION}` template. `TemplatedVersions` rewrites it to `dev:<buildDir>`, which
    *     `BleepDevDeps` short-circuits to the *live class dirs* — literally the code this server was built from, so the protocol matches by construction.
    *     Pinning to `BleepVersion.current` here would bypass the short-circuit and resolve a published jar instead: it fails outright when nothing was
    *     published at that version, and — worse — silently runs stale runner code when a same-versioned jar happens to be in ivy local.
    *   - anything else: pin to the *server's* `BleepVersion.current`, NOT the build's `$version`. The two diverge exactly when a snapshot server serves a build
    *     pinning an older release (bleep deliberately ignores the build's `$version` for a snapshot — see Main.scala), and the template would then resolve the
    *     OLD released runner, whose `SuiteDone` this server can't decode.
    */
  private def fetchBleepTestRunnerOnly(started: Started): List[Path] = {
    val version =
      if (started.build.$version == model.BleepVersion.dev) model.Replacements.known.BleepVersion
      else model.BleepVersion.current.value
    // The test runtime is `testRuntimeRules`' job, at the version the project calls for, so the runner is resolved for itself alone. Its POM agrees today —
    // it declares only `test-interface` and a `provided` launcher — and this exclusion keeps that true for POMs published before that was so, and for any
    // future one that regains a junit line: those versions would win the coursier reconciliation and land ahead of the aligned jars.
    val testRunnerDep = model.Dep.Java("build.bleep", "bleep-test-runner", version).copy(exclusions = MultiWorkspaceBspServer.ExcludeTestRuntime)
    val result = started.resolver.force(
      Set(testRunnerDep),
      model.VersionCombo.Jvm(model.VersionScala.Scala3),
      libraryVersionSchemes = SortedSet.empty[model.LibraryVersionScheme],
      context = "resolving bleep-test-runner",
      model.IgnoreEvictionErrors.No
    )
    result.jars
  }

  /** Env vars passed to every forked test process, on every platform (JVM, Scala.js, Scala Native, Kotlin/JS, Kotlin/Native).
    *
    * Three layers, weakest first:
    *
    *   1. `NO_COLOR=1` — the no-color.org standard, honored by ScalaTest 3.2.16+, JUnit, JUnit 5, sbt, gradle and many others, so test output captured in CI
    *      logs / dashboards is plain text instead of ANSI-decorated. Devs in an interactive terminal see plain ScalaTest output too, barely distinguishable
    *      from the colored variant and worth avoiding the per-CI fix-up loop for.
    *   2. `requestEnv` — the invoking client's shell environment, forwarded on the BSP request (see `BleepBspProtocol.ClientEnv`). This is what makes
    *      `FOO=bar bleep test` work; the daemon's own environment belongs to whichever shell cold-started it and is never consulted for this.
    *   3. `platform.jvmEnvironment` — the build's own declaration.
    *
    * Build config deliberately outranks the ambient shell rather than the other way round. The client env is a bulk dump of whatever happened to be exported,
    * so letting it win would mean a stray `AWS_REGION` in someone's profile silently overriding a region the build states on purpose — a failure that only
    * reproduces on one machine. A var the build does not mention is still forwarded untouched, which is the case the feature exists for.
    *
    * This is an overlay on the forked child, applied per request. The daemon never mutates its own environment, so concurrent test runs from different
    * workspaces cannot observe each other's values.
    */
  private def computeTestEnvironment(started: Started, project: CrossProjectName, requestEnv: Map[String, String]): Map[String, String] = {
    val projectEnv = started.build.explodedProjects.get(project).flatMap(_.platform).map(_.jvmEnvironment.toMap).getOrElse(Map.empty)
    Map("NO_COLOR" -> "1") ++ requestEnv ++ projectEnv
  }

  /** Create a TestEventHandler that offers events to the DAG queue via a Dispatcher.
    *
    * Using a Dispatcher avoids the overhead of `unsafeRunSync()` on every callback (test started, test finished, output line). The Dispatcher amortizes CE3
    * runtime setup across all calls. `unsafeRunSync` is safe here because the queue is unbounded (offer never suspends).
    */
  /** @param lastActivityAt
    *   set whenever the runner reports something, which is what [[IdleTimeout]] reads to tell a slow suite from a stuck one.
    *
    * The update rides inside the effects that already enqueue an event, and nowhere else. `onSuiteStarted` and `onSuiteFinished` stay the no-ops they were:
    * `dispatcher.unsafeRunSync` blocks the calling thread, these callbacks run on whatever thread a platform adapter happens to use, and under
    * `InProcessBspServer` — where the inner server shares a cats-effect runtime with the suite driving it — making them block starved the compute pool. Healthy
    * runs deadlocked and the timeout's own poll could not be scheduled. Progress is already reported through the three callbacks below.
    */
  private def makeTestEventHandler(
      dispatcher: Dispatcher[IO],
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      project: CrossProjectName,
      lastActivityAt: Ref[IO, FiniteDuration]
  ): TestRunnerTypes.TestEventHandler =
    new TestRunnerTypes.TestEventHandler {
      private val touch: IO[Unit] = IO.monotonic.flatMap(lastActivityAt.set)

      def onTestStarted(suite: String, test: String): Unit =
        dispatcher.unsafeRunSync(
          touch >> eventQueue.offer(Some(TaskDag.DagEvent.TestStarted(project, SuiteName(suite), TestName(test), System.currentTimeMillis())))
        )
      def onTestFinished(
          suite: String,
          test: String,
          status: bleep.bsp.protocol.TestStatus,
          durationMs: Long,
          message: Option[String],
          throwable: Option[String]
      ): Unit =
        dispatcher.unsafeRunSync(
          touch >> eventQueue.offer(
            Some(
              TaskDag.DagEvent
                .TestFinished(project, SuiteName(suite), TestName(test), status, durationMs, message, throwable, System.currentTimeMillis(), None)
            )
          )
        )
      def onSuiteStarted(suite: String): Unit = ()
      def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit = ()
      def onOutput(suite: String, line: String, channel: OutputChannel): Unit =
        dispatcher.unsafeRunSync(
          touch >> eventQueue.offer(Some(TaskDag.DagEvent.Output(project, SuiteName(suite), line, channel, System.currentTimeMillis())))
        )
    }

  /** Matches `TestRunner.Options.default.idleTimeout` deliberately: a suite is a suite, and a Scala.js one has earned no more or less rope than a JVM one. */
  private val PlatformSuiteIdleTimeout: FiniteDuration = 2.minutes

  /** How long a platform runner gets to shut its node process or native binary down once asked. Short on purpose — the run is already known to be stuck. */
  private val PlatformTeardownGrace: FiniteDuration = 10.seconds

  /** Give a platform test run the bound the JVM path has always had.
    *
    * `TaskResult.TimedOut` is what the JVM path returns and what the DAG turns into a `SuiteTimedOut` protocol event, so both paths converge on one conversion.
    * No thread dump: there is no JVM to dump — the work is a node process or a native binary.
    */
  private def boundPlatformRun(
      lastActivityAt: Ref[IO, FiniteDuration],
      killSignal: Deferred[IO, KillReason]
  )(run: IO[TaskDag.TaskResult]): IO[TaskDag.TaskResult] =
    IdleTimeout
      .bound(PlatformSuiteIdleTimeout, PlatformTeardownGrace, lastActivityAt, killSignal.complete(KillReason.Timeout).attempt.void)(run)
      .map {
        case Right(result) => result
        case Left(_)       => TaskDag.TaskResult.TimedOut(None)
      }

  /** Path to the node binary to use for a JS test/run. Uses the project's `jsNodeVersion` if set; falls back to [[bleep.constants.Node]] so users without a
    * configured version still get a working node via Coursier without needing one on `PATH`.
    */
  private def nodeBinaryFor(started: Started, project: model.Project): String =
    started.pre.fetchNode(project.platform.flatMap(_.jsNodeVersion).getOrElse(bleep.constants.Node)).toAbsolutePath.toString

  /** Run a Scala.js test suite: link → run via Node.js, emit events to DAG queue. */
  private def runScalaJsTestSuite(
      started: Started,
      testTask: TaskDag.TestSuiteTask,
      classpath: List[Path],
      testEnv: Map[String, String],
      linkResult: Option[TaskDag.LinkResult],
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      killSignal: Deferred[IO, KillReason]
  ): IO[TaskDag.TaskResult] = {
    val project = started.build.explodedProjects(testTask.project)
    val sjsVersion = project.platform.flatMap(_.jsVersion).getOrElse {
      throw new IllegalStateException(s"Scala.js version not found for ${testTask.project.value}")
    }
    // No Scala version needed here any more: it was only ever used to describe the link this function used to run itself.
    val linkConfig = bleep.analysis.ScalaJsLinkConfig.Debug

    for {
      startTs <- IO.realTime.map(_.toMillis)
      lastActivityAt <- IO.monotonic.flatMap(Ref.of[IO, FiniteDuration])
      // Note: TaskStarted is already emitted by DAG executor - don't duplicate it here
      taskResult <- linkResult match {
        // Taken from the link the DAG already ran, rather than linking again here.
        //
        // This used to run its own `LinkExecutor.execute` into a second output directory, once per suite. The work is identical every time and the DAG has
        // already done it — `TaskDag` puts a LinkTask between compile and discover for exactly this reason — so an N-suite project paid for N linear links of
        // the whole program. Worse, it was billed to the suites: linking happened inside each suite's own runtime, where it counted against the idle timeout
        // that is supposed to be measuring a hung test.
        case Some(TaskDag.LinkResult.JsSuccess(mainModule, _, _, _)) =>
          // Run the specific test suite via Node.js
          val nodeBinary = nodeBinaryFor(started, project)
          boundPlatformRun(lastActivityAt, killSignal) {
            Dispatcher.sequential[IO].use { dispatcher =>
              val eventHandler = makeTestEventHandler(dispatcher, eventQueue, testTask.project, lastActivityAt)
              val suites = List(TestRunnerTypes.TestSuite(testTask.suiteName.value, testTask.suiteName.value))
              ScalaJsTestRunner
                .runTests(
                  mainModule,
                  linkConfig.moduleKind,
                  suites,
                  eventHandler,
                  ScalaJsTestRunner.NodeEnvironment.Node,
                  nodeBinary,
                  testEnv,
                  sjsVersion.scalaJsVersion,
                  classpath,
                  killSignal
                )
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
                            SuiteOutcome.fromCounts(result.passed, result.failed, result.skipped, result.ignored),
                            durationMs,
                            endTs
                          )
                      )
                    )
                    .as(classifyTestResult(result))
                }
            }
          }
        // No link output to run. The DAG links before it discovers, and discovery is what produced this suite, so reaching here means the two got out of step
        // rather than that the user did anything wrong — say so plainly instead of quietly linking again and hiding it.
        case other =>
          val endTs = System.currentTimeMillis()
          val durationMs = endTs - startTs
          val failure = TaskDag.TaskResult.Failure(
            s"no Scala.js link output for ${testTask.project.value}: expected the DAG's link to have produced one, got ${other.getOrElse("nothing")}",
            Nil
          )
          eventQueue.offer(Some(TaskDag.DagEvent.SuiteFinished(testTask.project, testTask.suiteName, erroredOutcomeOf(failure), durationMs, endTs))).void >>
            IO.pure(failure)
      }
    } yield taskResult
  }

  /** Run a Scala Native test suite: link → run binary, emit events to DAG queue. */
  private def runScalaNativeTestSuite(
      started: Started,
      testTask: TaskDag.TestSuiteTask,
      classpath: List[Path],
      testEnv: Map[String, String],
      linkResult: Option[TaskDag.LinkResult],
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      killSignal: Deferred[IO, KillReason]
  ): IO[TaskDag.TaskResult] = {
    val project = started.build.explodedProjects(testTask.project)
    val snVersion = project.platform.flatMap(_.nativeVersion).getOrElse {
      throw new IllegalStateException(s"Scala Native version not found for ${testTask.project.value}")
    }

    val framework = ScalaNativeTestRunner.detectFramework(classpath)

    for {
      startTs <- IO.realTime.map(_.toMillis)
      lastActivityAt <- IO.monotonic.flatMap(Ref.of[IO, FiniteDuration])
      // Note: TaskStarted is already emitted by DAG executor - don't duplicate it here
      taskResult <- linkResult match {
        // The binary the DAG's link wrote, rather than one linked here.
        //
        // This used to link its own, once per suite, and every suite computed the same output path — so a three-suite project linked four times and the last
        // three raced each other for one file. The DAG's link is not merely equivalent, it is identical: `LinkExecutor` resolves an absent main class to
        // `ScalaNativeTestRunner.TestMainClass` when the task is a test, which is the same constant `getTestMainClass` returns for every framework.
        case Some(TaskDag.LinkResult.NativeSuccess(binary, _)) =>
          boundPlatformRun(lastActivityAt, killSignal) {
            Dispatcher.sequential[IO].use { dispatcher =>
              val eventHandler = makeTestEventHandler(dispatcher, eventQueue, testTask.project, lastActivityAt)
              val suites = List(TestRunnerTypes.TestSuite(testTask.suiteName.value, testTask.suiteName.value))
              ScalaNativeTestRunner
                .runTestsViaAdapter(binary, suites, framework, eventHandler, testEnv, snVersion.scalaNativeVersion, classpath, killSignal)
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
                              SuiteOutcome.fromCounts(result.passed, result.failed, result.skipped, result.ignored),
                              durationMs,
                              endTs
                            )
                        )
                      )
                      .as(classifyTestResult(result))
                }
            }
          }
        // No binary to run. The DAG links before it discovers, and discovery produced this suite, so this means the two got out of step rather than that the
        // link failed — a failed link would have stopped the suite ever being scheduled. Named accordingly instead of blaming the linker.
        case other =>
          val endTs = System.currentTimeMillis()
          val durationMs = endTs - startTs
          val message =
            s"no Scala Native binary for ${testTask.project.value}: expected the DAG's link to have produced one, got ${other.getOrElse("nothing")}"
          eventQueue
            .offer(Some(TaskDag.DagEvent.SuiteFinished(testTask.project, testTask.suiteName, SuiteOutcome.Errored(message, None), durationMs, endTs)))
            .void >>
            IO.pure(TaskDag.TaskResult.Failure(message, List.empty))
      }
    } yield taskResult
  }

  /** Run a Kotlin/JS test suite: discover + run via Node.js, emit events to DAG queue. */
  private def runKotlinJsTestSuite(
      started: Started,
      testTask: TaskDag.TestSuiteTask,
      jsOutput: Path,
      testEnv: Map[String, String],
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      killSignal: Deferred[IO, KillReason]
  ): IO[TaskDag.TaskResult] =
    for {
      startTs <- IO.realTime.map(_.toMillis)
      lastActivityAt <- IO.monotonic.flatMap(Ref.of[IO, FiniteDuration])
      // Note: TaskStarted is already emitted by DAG executor - don't duplicate it here
      taskResult <-
        if (!Files.exists(jsOutput)) {
          val endTs = System.currentTimeMillis()
          val durationMs = endTs - startTs
          eventQueue
            .offer(
              Some(
                TaskDag.DagEvent
                  .SuiteFinished(testTask.project, testTask.suiteName, SuiteOutcome.Errored(s"Kotlin/JS output not found: $jsOutput", None), durationMs, endTs)
              )
            )
            .void >>
            IO.pure(TaskDag.TaskResult.Failure(s"Kotlin/JS output not found: $jsOutput", List.empty))
        } else {
          val nodeBinary = nodeBinaryFor(started, started.build.explodedProjects(testTask.project))
          boundPlatformRun(lastActivityAt, killSignal) {
            Dispatcher.sequential[IO].use { dispatcher =>
              val eventHandler = makeTestEventHandler(dispatcher, eventQueue, testTask.project, lastActivityAt)
              val suites = List(TestRunnerTypes.TestSuite(testTask.suiteName.value, testTask.suiteName.value))
              KotlinTestRunner.Js.runTests(jsOutput, suites, eventHandler, nodeBinary, testEnv, killSignal).flatMap { result =>
                val endTs = System.currentTimeMillis()
                val durationMs = endTs - startTs
                eventQueue
                  .offer(
                    Some(
                      TaskDag.DagEvent
                        .SuiteFinished(
                          testTask.project,
                          testTask.suiteName,
                          SuiteOutcome.fromCounts(result.passed, result.failed, result.skipped, result.ignored),
                          durationMs,
                          endTs
                        )
                    )
                  )
                  .as(classifyTestResult(result))
              }
            }
          }
        }
    } yield taskResult

  /** Run a Kotlin/Native test suite: run binary, emit events to DAG queue. */
  private def runKotlinNativeTestSuite(
      started: Started,
      testTask: TaskDag.TestSuiteTask,
      binary: Path,
      testEnv: Map[String, String],
      eventQueue: Queue[IO, Option[TaskDag.DagEvent]],
      killSignal: Deferred[IO, KillReason]
  ): IO[TaskDag.TaskResult] =
    for {
      startTs <- IO.realTime.map(_.toMillis)
      lastActivityAt <- IO.monotonic.flatMap(Ref.of[IO, FiniteDuration])
      // Note: TaskStarted is already emitted by DAG executor - don't duplicate it here
      taskResult <-
        if (!Files.exists(binary)) {
          val endTs = System.currentTimeMillis()
          val durationMs = endTs - startTs
          eventQueue
            .offer(
              Some(
                TaskDag.DagEvent.SuiteFinished(
                  testTask.project,
                  testTask.suiteName,
                  SuiteOutcome.Errored(s"Kotlin/Native binary not found: $binary", None),
                  durationMs,
                  endTs
                )
              )
            )
            .void >>
            IO.pure(TaskDag.TaskResult.Failure(s"Kotlin/Native binary not found: $binary", List.empty))
        } else {
          boundPlatformRun(lastActivityAt, killSignal) {
            Dispatcher.sequential[IO].use { dispatcher =>
              val eventHandler = makeTestEventHandler(dispatcher, eventQueue, testTask.project, lastActivityAt)
              // Filter to this task's suite, unless discovery could not enumerate them. `kotlinDiscovered` spells that case `<project>:<suffix>`, and a
              // `:` cannot occur in a JVM fully-qualified name — so the synthetic case is distinguishable rather than guessed at. Passing `List.empty`
              // unconditionally, as this did, ran the whole binary for *every* suite task: a two-suite project ran all its tests twice and reported each
              // run under both suite names, which reads as a passing project with double the tests.
              val suites =
                if (testTask.suiteName.value.contains(':')) List.empty
                else List(TestRunnerTypes.TestSuite(testTask.suiteName.value.split('.').last, testTask.suiteName.value))
              KotlinTestRunner.Native.runTests(binary, suites, eventHandler, testEnv, started.buildPaths.cwd, killSignal).flatMap { result =>
                val endTs = System.currentTimeMillis()
                val durationMs = endTs - startTs
                eventQueue
                  .offer(
                    Some(
                      TaskDag.DagEvent
                        .SuiteFinished(
                          testTask.project,
                          testTask.suiteName,
                          SuiteOutcome.fromCounts(result.passed, result.failed, result.skipped, result.ignored),
                          durationMs,
                          endTs
                        )
                    )
                  )
                  .as(classifyTestResult(result))
              }
            }
          }
        }
    } yield taskResult

  /** Get trace category and name for a task. */
  private def taskCatName(task: TaskDag.Task): (TraceCategory, String) = task match {
    case ct: TaskDag.CompileTask                      => (TraceCategory.Compile, ct.project.value)
    case lt: TaskDag.LinkTask                         => (TraceCategory.Link, lt.project.value)
    case dt: TaskDag.DiscoverTask                     => (TraceCategory.Discover, dt.project.value)
    case tt: TaskDag.TestSuiteTask                    => (TraceCategory.Test, s"${tt.project.value}:${tt.suiteName.value}")
    case sgt: TaskDag.SourcegenTask                   => (TraceCategory.Sourcegen, s"${sgt.script.project.value}/${sgt.script.main}")
    case apt: TaskDag.ResolveAnnotationProcessorsTask => (TraceCategory.ResolveAnnotationProcessors, apt.project.value)
    case kspt: TaskDag.RunSymbolProcessorsTask        => (TraceCategory.RunSymbolProcessors, kspt.project.value)
  }

  /** The floor under task reporting: a task that terminated abnormally — its body threw (Error), hung (TimedOut) or was killed for exceeding a time limit —
    * never got to emit its own failure event, so the stream would carry no trace of why the run went wrong, and clients replaying the events would judge the
    * run a success. Every such result becomes a protocol Error event naming the task and cause.
    *
    * Deliberately NOT emitted for Success, Failure or Skipped: a Failure means the task body ran to completion and its own event (CompileFinished /
    * LinkFinished / SourcegenFinished / ...) already carries the failure, and a Skipped task's root cause is the failed dependency, which reported itself.
    *
    * Kills are classified by their reason. `TaskResult.Cancelled` is literally `Killed(UserRequest)`, and ServerShutdown / DeadClient / ParentDying likewise
    * mean the run was torn down around the task, not that the build is defective — those are conveyed by the response's Cancelled StatusCode and by the
    * Cancelled compiles/suites the teardown leaves behind, and an Error event here would dress a Ctrl-C up as a build failure. Only Killed(Timeout) is the
    * task's own fault.
    */
  private def abnormalTaskEvent(description: String, result: TaskDag.TaskResult, timestamp: Long): Option[BleepBspProtocol.Event] =
    result match {
      case TaskDag.TaskResult.Error(error, _) => Some(BleepBspProtocol.Event.Error(s"$description errored: $error", None, timestamp))
      case _: TaskDag.TaskResult.TimedOut     => Some(BleepBspProtocol.Event.Error(s"$description timed out", None, timestamp))
      case TaskDag.TaskResult.Killed(reason)  =>
        reason match {
          case KillReason.Timeout => Some(BleepBspProtocol.Event.Error(s"$description was killed after exceeding its time limit", None, timestamp))
          case KillReason.UserRequest | KillReason.ParentDying | KillReason.ServerShutdown | KillReason.DeadClient => None
        }
      case TaskDag.TaskResult.Success | _: TaskDag.TaskResult.Failure | _: TaskDag.TaskResult.Skipped => None
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
      case TaskDag.TaskResult.Killed(_) | TaskDag.TaskResult.Cancelled | _: TaskDag.TaskResult.TimedOut =>
        BleepBspProtocol.Event.CompileFinished(project, CompileStatus.Cancelled, durationMs, Nil, skippedBecause = None, timestamp)
    }

  /** Convert a LinkResult to a LinkFinished protocol event. */
  private def linkFinishedEvent(
      project: CrossProjectName,
      result: TaskDag.LinkResult,
      durationMs: Long,
      timestamp: Long,
      platform: LinkPlatformName
  ): BleepBspProtocol.Event.LinkFinished = {
    // The platform is the task's, not the result's: `LinkResult` says JS or Native and cannot say whose.
    val (success, outputPath, generatedFiles, error) = result match {
      case TaskDag.LinkResult.JsSuccess(mainModule, sourceMap, allFiles, _) =>
        // `allFiles` already holds the chunks; the main module and source map are named separately and are not necessarily in it.
        val all = (mainModule :: sourceMap.toList ::: allFiles.toList).distinct.map(_.toString)
        (true, Some(mainModule.toString), all, None)
      case TaskDag.LinkResult.NativeSuccess(binary, _) => (true, Some(binary.toString), List(binary.toString), None)
      case TaskDag.LinkResult.Failure(err, _)          => (false, None, Nil, Some(err))
      case TaskDag.LinkResult.Killed(reason)           => (false, None, Nil, Some(s"Killed: $reason"))
      case TaskDag.LinkResult.NotApplicable            => (true, None, Nil, None)
    }
    BleepBspProtocol.Event.LinkFinished(project, success, durationMs, outputPath, generatedFiles, timestamp, platform, error)
  }

  /** Process link-specific DagEvents shared between consumeEvents and consumeCompileEvents. */
  private def processLinkEvent(
      event: TaskDag.DagEvent,
      originId: Option[String],
      traceRecorder: TraceRecorder,
      recorder: TranscriptRecorder
  ): IO[Unit] = event match {
    case TaskDag.DagEvent.LinkStarted(project, platform, timestamp) =>
      val protocolEvent = BleepBspProtocol.Event.LinkStarted(project, platform, timestamp)
      traceRecorder.recordStart(TraceCategory.Link, project.value) >>
        IO(sendEvent(originId, s"link:${project.value}", protocolEvent, recorder))
    case TaskDag.DagEvent.LinkProgress(project, phase, _, timestamp) =>
      val protocolEvent = BleepBspProtocol.Event.LinkProgress(project, phase, timestamp)
      IO(sendEvent(originId, s"link:${project.value}", protocolEvent, recorder))
    case TaskDag.DagEvent.LinkFinished(project, result, durationMs, timestamp, platform) =>
      val protocolEvent = linkFinishedEvent(project, result, durationMs, timestamp, platform)
      traceRecorder.recordEnd(TraceCategory.Link, project.value) >>
        IO(sendEvent(originId, s"link:${project.value}", protocolEvent, recorder))
    case _ => IO.unit
  }

  /** Process sourcegen-specific DagEvents shared between consumeEvents and consumeCompileEvents. */
  private def processSourcegenEvent(
      event: TaskDag.DagEvent,
      originId: Option[String],
      recorder: TranscriptRecorder
  ): IO[Unit] = event match {
    case TaskDag.DagEvent.SourcegenStarted(_, scriptMain, forProjects, timestamp) =>
      val protocolEvent = BleepBspProtocol.Event.SourcegenStarted(scriptMain, forProjects, timestamp)
      IO(sendEvent(originId, s"sourcegen-$scriptMain", protocolEvent, recorder))
    case TaskDag.DagEvent.SourcegenFinished(_, scriptMain, success, durationMs, error, timestamp) =>
      val protocolEvent = BleepBspProtocol.Event.SourcegenFinished(scriptMain, success, durationMs, error, timestamp)
      IO(sendEvent(originId, s"sourcegen-$scriptMain", protocolEvent, recorder))
    case _ => IO.unit
  }

  /** Logs the AP resolution lifecycle and sends the BSP protocol events so `ReactiveBsp` can fold them into `BuildEvent.ResolveAnnotationProcessorsFinished`,
    * which the build-state reducer counts toward `apResolutionFailed`. The trace recorder also captures these as separate spans in the chrome-trace output so
    * flamegraphs distinguish AP resolution from compile.
    */
  private def processAnnotationProcessorEvent(
      event: TaskDag.DagEvent,
      originId: Option[String],
      traceRecorder: TraceRecorder,
      recorder: TranscriptRecorder
  ): IO[Unit] =
    event match {
      case TaskDag.DagEvent.ResolveAnnotationProcessorsStarted(project, timestamp) =>
        val protocolEvent = BleepBspProtocol.Event.ResolveAnnotationProcessorsStarted(project, timestamp)
        IO(logger.withContext("project", project.value).debug("Annotation processor resolution starting")) >>
          traceRecorder.recordStart(TraceCategory.ResolveAnnotationProcessors, project.value) >>
          IO(sendEvent(originId, s"resolve-ap:${project.value}", protocolEvent, recorder))
      case TaskDag.DagEvent.ResolveAnnotationProcessorsFinished(project, success, durationMs, error, discoveredJarCount, timestamp) =>
        val msg =
          if (success) s"Annotation processor resolution finished (${discoveredJarCount} jars, ${durationMs}ms)"
          else s"Annotation processor resolution failed: ${error.getOrElse("unknown")} (${durationMs}ms)"
        val protocolEvent =
          BleepBspProtocol.Event.ResolveAnnotationProcessorsFinished(project, success, durationMs, error, discoveredJarCount, timestamp)
        IO(logger.withContext("project", project.value).info(msg)) >>
          traceRecorder.recordEnd(TraceCategory.ResolveAnnotationProcessors, project.value) >>
          IO(sendEvent(originId, s"resolve-ap:${project.value}", protocolEvent, recorder))
      case _ => IO.unit
    }

  /** KSP-side counterpart of [[processAnnotationProcessorEvent]]: logs lifecycle, records trace spans, emits BSP events that ReactiveBsp folds into
    * `BuildEvent.RunSymbolProcessorsFinished` and the build-state reducer counts toward `kspResolutionFailed`.
    */
  private def processSymbolProcessorEvent(
      event: TaskDag.DagEvent,
      originId: Option[String],
      traceRecorder: TraceRecorder,
      recorder: TranscriptRecorder
  ): IO[Unit] =
    event match {
      case TaskDag.DagEvent.RunSymbolProcessorsStarted(project, timestamp) =>
        val protocolEvent = BleepBspProtocol.Event.RunSymbolProcessorsStarted(project, timestamp)
        IO(logger.withContext("project", project.value).debug("KSP starting")) >>
          traceRecorder.recordStart(TraceCategory.RunSymbolProcessors, project.value) >>
          IO(sendEvent(originId, s"run-ksp:${project.value}", protocolEvent, recorder))
      case TaskDag.DagEvent.RunSymbolProcessorsFinished(project, success, durationMs, error, discoveredJarCount, timestamp) =>
        val msg =
          if (success) s"KSP run finished (${discoveredJarCount} processor jars, ${durationMs}ms)"
          else s"KSP run failed: ${error.getOrElse("unknown")} (${durationMs}ms)"
        val protocolEvent =
          BleepBspProtocol.Event.RunSymbolProcessorsFinished(project, success, durationMs, error, discoveredJarCount, timestamp)
        IO(logger.withContext("project", project.value).info(msg)) >>
          traceRecorder.recordEnd(TraceCategory.RunSymbolProcessors, project.value) >>
          IO(sendEvent(originId, s"run-ksp:${project.value}", protocolEvent, recorder))
      case _ => IO.unit
    }

  /** Wrap event processing with dead-client detection and kill signal propagation.
    *
    * Only the disconnection-handling tail is uncancelable — we want the killSignal completion + log to run atomically once we observe `clientDisconnected`.
    * `processEvent` itself stays cancelable so a slow `sendNotification` to a wedged client can be interrupted by the outer build-cancel rather than pinning
    * the consumer fiber.
    */
  private def withDeadClientDetection(
      killSignal: Deferred[IO, KillReason],
      contextLabel: String
  )(processEvent: IO[Unit]): IO[Unit] =
    (processEvent >> IO.whenA(clientDisconnected.get()) {
      IO.raiseError(new java.io.IOException("Client disconnected (detected via sendNotification)"))
    }).handleErrorWith {
      case error: java.io.IOException =>
        IO.uncancelable { _ =>
          IO(logger.withContext("error", error.getMessage).error(s"$contextLabel event send failed (connection dead)")) >>
            killSignal.complete(KillReason.DeadClient).attempt.void
        } >> IO.raiseError(error)
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
      killSignal: Deferred[IO, KillReason],
      traceRecorder: TraceRecorder,
      recorder: TranscriptRecorder
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
              case _: TaskDag.SourcegenTask =>
                None // Sourcegen is reported via DagEvent.SourcegenStarted/Finished, not TaskStarted/Finished
              case _: TaskDag.ResolveAnnotationProcessorsTask =>
                None // AP resolution is reported via DagEvent.ResolveAnnotationProcessors{Started,Finished}
              case _: TaskDag.RunSymbolProcessorsTask =>
                None // KSP execution is reported via DagEvent.RunSymbolProcessors{Started,Finished}
            }
            traceRecorder.recordStart(cat, name) >>
              IO(protocolEvent.foreach(e => sendTestEvent(originId, task.id.value, e, recorder)))

          case TaskDag.DagEvent.TaskFinished(task, result, durationMs, timestamp) =>
            val (cat, name) = taskCatName(task)
            val protocolEvent: Option[BleepBspProtocol.Event] = task match {
              case ct: TaskDag.CompileTask =>
                Some(compileTaskFinishedEvent(ct.project, result, durationMs, timestamp))

              case lt: TaskDag.LinkTask =>
                // Success and Failure are conveyed by the LinkFinished event the task body emits; the
                // abnormal results below never reached that emit and would otherwise vanish.
                abnormalTaskEvent(s"Link ${lt.project.value}", result, timestamp)

              case dt: TaskDag.DiscoverTask =>
                result match {
                  case TaskDag.TaskResult.Failure(msg, _) =>
                    Some(BleepBspProtocol.Event.Error(msg, None, timestamp))
                  case TaskDag.TaskResult.Success | _: TaskDag.TaskResult.Skipped | TaskDag.TaskResult.Cancelled =>
                    None // Discovery success is handled by SuitesDiscovered; Skipped's root cause reported itself
                  case other =>
                    abnormalTaskEvent(s"Test discovery for ${dt.project.value}", other, timestamp)
                }

              case tt: TaskDag.TestSuiteTask =>
                result match {
                  case TaskDag.TaskResult.Success =>
                    None // SuiteFinished already emitted by TestRunner
                  case TaskDag.TaskResult.Failure(_, _) =>
                    // A logical suite failure (failed tests / empty / no-framework / errored-from-
                    // SuiteDone) was already conveyed by the SuiteFinished(outcome) event. Emitting a
                    // SuiteError here too would double-count the suite. Infra failures with no
                    // SuiteFinished come through as TaskResult.Error below.
                    None
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
                  case TaskDag.TaskResult.Killed(reason) =>
                    // TaskResult.Cancelled is Killed(UserRequest), so the reason — not a separate case —
                    // distinguishes a user's cancellation from other kills. (A dedicated Cancelled arm
                    // after this one would be unreachable.)
                    val why = reason match {
                      case KillReason.UserRequest => "cancelled"
                      case other                  => s"killed ($other)"
                    }
                    Some(BleepBspProtocol.Event.SuiteCancelled(tt.project, tt.suiteName, Some(why), timestamp))
                  case TaskDag.TaskResult.TimedOut(threadDump) =>
                    Some(BleepBspProtocol.Event.SuiteTimedOut(tt.project, tt.suiteName, durationMs, threadDump, timestamp))
                }

              case _: TaskDag.SourcegenTask =>
                None // Sourcegen is reported via DagEvent.SourcegenFinished, not TaskFinished
              case _: TaskDag.ResolveAnnotationProcessorsTask =>
                None // AP resolution is reported via DagEvent.ResolveAnnotationProcessors{Started,Finished}
              case _: TaskDag.RunSymbolProcessorsTask =>
                None // KSP execution is reported via DagEvent.RunSymbolProcessors{Started,Finished}
            }
            val failureRefUpdate = (task, result) match {
              case (_: TaskDag.TestSuiteTask, _: TaskDag.TaskResult.Failure) =>
                totalFailedRef.update(n => math.max(n, 1))
              case _ =>
                IO.unit
            }
            traceRecorder.recordEnd(cat, name) >> failureRefUpdate >>
              IO(protocolEvent.foreach(e => sendTestEvent(originId, task.id.value, e, recorder)))

          case TaskDag.DagEvent.TestStarted(project, suite, test, timestamp) =>
            val protocolEvent = BleepBspProtocol.Event.TestStarted(project, suite, test, timestamp)
            IO(sendTestEvent(originId, s"test:$project:$suite", protocolEvent, recorder))

          case TaskDag.DagEvent.TestFinished(project, suite, test, status, durationMs, message, throwable, timestamp, location) =>
            IO(
              sendTestEvent(
                originId,
                s"test:$project:$suite",
                BleepBspProtocol.Event.TestFinished(project, suite, test, status, durationMs, message, throwable, timestamp, location),
                recorder
              )
            )

          case TaskDag.DagEvent.SuitesDiscovered(project, suites, discoveredBeforeFilters, isTestProject, timestamp) =>
            for {
              total <- totalSuitesRef.updateAndGet(_ + suites.size)
              _ <- IO(
                sendTestEvent(
                  originId,
                  s"discover:$project",
                  BleepBspProtocol.Event.SuitesDiscovered(project, suites, total, Some(discoveredBeforeFilters), isTestProject, timestamp),
                  recorder
                )
              )
            } yield ()

          case TaskDag.DagEvent.TaskProgress(task, percent, timestamp) =>
            task match {
              case ct: TaskDag.CompileTask =>
                IO(sendTestEvent(originId, task.id.value, BleepBspProtocol.Event.CompileProgress(ct.project, percent, timestamp), recorder))
              case _ =>
                IO.unit
            }

          case TaskDag.DagEvent.Output(project, suite, line, channel, timestamp) =>
            IO(sendTestEvent(originId, s"output:$project:$suite", BleepBspProtocol.Event.Output(project, suite, line, channel, timestamp), recorder))

          case TaskDag.DagEvent.SuiteFinished(project, suite, outcome, durationMs, timestamp) =>
            val protocolEvent = BleepBspProtocol.Event.SuiteFinished(project, suite, outcome, durationMs, timestamp)
            // Reliable server-side tallies for TestRunResult. A non-Executed outcome (empty /
            // no-framework / errored) contributes one failed suite so the authoritative summary is
            // red even if per-test notifications were lost. Executed contributes its real counts.
            val failedContribution = if (outcome.isFailure && outcome.failedCount == 0) 1 else outcome.failedCount
            totalPassedRef.update(_ + outcome.passedCount) >>
              totalFailedRef.update(_ + failedContribution) >>
              totalSkippedRef.update(_ + outcome.skippedCount) >>
              totalIgnoredRef.update(_ + outcome.ignoredCount) >>
              IO(sendTestEvent(originId, s"suite:$project:$suite", protocolEvent, recorder))

          case linkEvent: TaskDag.DagEvent.LinkStarted                       => processLinkEvent(linkEvent, originId, traceRecorder, recorder)
          case linkEvent: TaskDag.DagEvent.LinkProgress                      => processLinkEvent(linkEvent, originId, traceRecorder, recorder)
          case linkEvent: TaskDag.DagEvent.LinkFinished                      => processLinkEvent(linkEvent, originId, traceRecorder, recorder)
          case sgEvent: TaskDag.DagEvent.SourcegenStarted                    => processSourcegenEvent(sgEvent, originId, recorder)
          case sgEvent: TaskDag.DagEvent.SourcegenFinished                   => processSourcegenEvent(sgEvent, originId, recorder)
          case apEvent: TaskDag.DagEvent.ResolveAnnotationProcessorsStarted  => processAnnotationProcessorEvent(apEvent, originId, traceRecorder, recorder)
          case apEvent: TaskDag.DagEvent.ResolveAnnotationProcessorsFinished => processAnnotationProcessorEvent(apEvent, originId, traceRecorder, recorder)
          case kspEvent: TaskDag.DagEvent.RunSymbolProcessorsStarted         => processSymbolProcessorEvent(kspEvent, originId, traceRecorder, recorder)
          case kspEvent: TaskDag.DagEvent.RunSymbolProcessorsFinished        => processSymbolProcessorEvent(kspEvent, originId, traceRecorder, recorder)
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
      killSignal: Deferred[IO, KillReason],
      traceRecorder: TraceRecorder,
      recorder: TranscriptRecorder
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
            IO(protocolEvent.foreach(e => sendEvent(originId, task.id.value, e, recorder)))

        case TaskDag.DagEvent.TaskFinished(task, result, durationMs, timestamp) =>
          val (cat, name) = taskCatName(task)
          val protocolEvent: Option[BleepBspProtocol.Event] = task match {
            case ct: TaskDag.CompileTask =>
              logger.withContext("project", ct.project.value).withContext("durationMs", durationMs).info("Compile finished")
              Some(compileTaskFinishedEvent(ct.project, result, durationMs, timestamp))
            case lt: TaskDag.LinkTask =>
              // Success and Failure are conveyed by the LinkFinished event the task body emits; the
              // abnormal results never reached that emit and would otherwise vanish.
              abnormalTaskEvent(s"Link ${lt.project.value}", result, timestamp)
            case _ =>
              // Sourcegen/AP/KSP tasks emit their own Finished event for every result, abnormal included
              // (recovery wraps only their handler — see TaskDag.executeTask); Discover/TestSuite tasks
              // do not run in compile mode.
              None
          }
          traceRecorder.recordEnd(cat, name) >>
            IO(protocolEvent.foreach(e => sendEvent(originId, task.id.value, e, recorder)))

        case TaskDag.DagEvent.TaskProgress(task, percent, timestamp) =>
          task match {
            case ct: TaskDag.CompileTask =>
              IO(sendEvent(originId, task.id.value, BleepBspProtocol.Event.CompileProgress(ct.project, percent, timestamp), recorder))
            case _ => IO.unit
          }

        case linkEvent: TaskDag.DagEvent.LinkStarted                       => processLinkEvent(linkEvent, originId, traceRecorder, recorder)
        case linkEvent: TaskDag.DagEvent.LinkProgress                      => processLinkEvent(linkEvent, originId, traceRecorder, recorder)
        case linkEvent: TaskDag.DagEvent.LinkFinished                      => processLinkEvent(linkEvent, originId, traceRecorder, recorder)
        case sgEvent: TaskDag.DagEvent.SourcegenStarted                    => processSourcegenEvent(sgEvent, originId, recorder)
        case sgEvent: TaskDag.DagEvent.SourcegenFinished                   => processSourcegenEvent(sgEvent, originId, recorder)
        case apEvent: TaskDag.DagEvent.ResolveAnnotationProcessorsStarted  => processAnnotationProcessorEvent(apEvent, originId, traceRecorder, recorder)
        case apEvent: TaskDag.DagEvent.ResolveAnnotationProcessorsFinished => processAnnotationProcessorEvent(apEvent, originId, traceRecorder, recorder)
        case kspEvent: TaskDag.DagEvent.RunSymbolProcessorsStarted         => processSymbolProcessorEvent(kspEvent, originId, traceRecorder, recorder)
        case kspEvent: TaskDag.DagEvent.RunSymbolProcessorsFinished        => processSymbolProcessorEvent(kspEvent, originId, traceRecorder, recorder)

        case _ => IO.unit
      }

      withDeadClientDetection(killSignal, "Compile")(processEvent)
    }

  /** Convert a compiler error to a protocol Diagnostic preserving severity */
  private def toDiagnostic(error: CompilerError): BleepBspProtocol.Diagnostic = {
    // 0 is the compiler's "no position reported" sentinel for both, so it becomes None rather than a bogus line 0.
    val line = Option(error.line).filter(_ > 0)
    val column = Option(error.column).filter(_ > 0)
    val severity = error.severity match {
      case CompilerError.Severity.Error   => bleep.bsp.protocol.DiagnosticSeverity.Error
      case CompilerError.Severity.Warning => bleep.bsp.protocol.DiagnosticSeverity.Warning
      case CompilerError.Severity.Info    => bleep.bsp.protocol.DiagnosticSeverity.Info
    }
    BleepBspProtocol.Diagnostic(severity, error.message, error.rendered, error.path.map(_.toString), line, column)
  }

  private val sendEventCounter = new java.util.concurrent.atomic.AtomicInteger(0)

  /** Send a test event via BSP notification with structured data */
  private def sendTestEvent(originId: Option[String], taskId: String, event: BleepBspProtocol.Event, recorder: TranscriptRecorder): Unit = {
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
          .debug("sendTestEvent: CompileFinished")
      case e: E.CompileStarted =>
        logger.withContext("n", n).withContext("taskId", taskId).withContext("project", e.project.value).debug("sendTestEvent: CompileStarted")
      case e: E.TestFinished =>
        logger
          .withContext("n", n)
          .withContext("taskId", taskId)
          .withContext("status", e.status.wireValue)
          .withContext("project", e.project.value)
          .withContext("suite", e.suite.value)
          .withContext("test", e.test.value)
          .debug("sendTestEvent: TestFinished")
      case e: E.SuiteFinished =>
        logger
          .withContext("n", n)
          .withContext("taskId", taskId)
          .withContext("project", e.project.value)
          .withContext("suite", e.suite.value)
          .withContext("outcome", SuiteOutcome.tagOf(e.outcome))
          .withContext("passed", e.outcome.passedCount)
          .withContext("failed", e.outcome.failedCount)
          .debug("sendTestEvent: SuiteFinished")
      case e: E.SuiteError =>
        logger
          .withContext("n", n)
          .withContext("taskId", taskId)
          .withContext("project", e.project.value)
          .withContext("suite", e.suite.value)
          .withContext("error", e.error)
          .debug("sendTestEvent: SuiteError")
      case e: E.SuiteCancelled =>
        logger
          .withContext("n", n)
          .withContext("taskId", taskId)
          .withContext("project", e.project.value)
          .withContext("suite", e.suite.value)
          .debug("sendTestEvent: SuiteCancelled")
      case e: E.SuiteTimedOut =>
        logger
          .withContext("n", n)
          .withContext("taskId", taskId)
          .withContext("project", e.project.value)
          .withContext("suite", e.suite.value)
          .withContext("timeoutMs", e.timeoutMs)
          .debug("sendTestEvent: SuiteTimedOut")
      case _: E.CompileProgress | _: E.Output => () // too noisy (Output = every line of test stdout)
      case _                                  =>
        logger.withContext("n", n).withContext("taskId", taskId).withContext("event", event.getClass.getSimpleName).debug("sendTestEvent")
    }
    sendEvent(originId, taskId, event, recorder)
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

  /** End a diagnostic cycle: send empty diagnostics with reset=true for files that had errors in a previous compilation but are now clean. */
  private def clearStaleDiagnostics(diagnosticTracker: BspDiagnosticTracker): Unit =
    diagnosticTracker.finishCycle().foreach { case (docUri, targetUri) =>
      val publishParams = PublishDiagnosticsParams(
        textDocument = TextDocumentIdentifier(Uri(java.net.URI.create(docUri))),
        buildTarget = BuildTargetIdentifier(uri = Uri(java.net.URI.create(targetUri))),
        originId = None,
        diagnostics = Nil,
        reset = true
      )
      sendNotification("build/publishDiagnostics", publishParams)
    }

  /** Send a log message without project scope (for rare error cases only) */
  /** Wraps a link with its telemetry, so the two call sites cannot drift.
    *
    * Deliberately one helper rather than the same `guaranteeCase` block copied into both link handlers: the compile path already has that shape inline, and
    * duplicated telemetry is how you end up with two events that disagree about what counts as success.
    *
    * `guaranteeCase` rather than `flatMap`, for the same reason the compile path uses it — a link that is cancelled or that throws must still emit its end
    * event, or the start event is left dangling and the duration is unknowable. That is the bug shape `fork_end` was added to close for test JVMs.
    */
  private def withLinkMetrics(linkTask: TaskDag.LinkTask, workspace: String)(
      run: IO[(TaskDag.TaskResult, TaskDag.LinkResult)]
  ): IO[(TaskDag.TaskResult, TaskDag.LinkResult)] = {
    val project = linkTask.project.value
    val platform = linkTask.platform.name.wireValue
    val startedAt = System.currentTimeMillis()

    def end(success: Boolean): IO[Unit] =
      IO(
        BspMetrics.recordLinkEnd(
          project,
          workspace,
          platform,
          linkTask.releaseMode,
          linkTask.isTest,
          System.currentTimeMillis() - startedAt,
          success
        )
      )

    IO(BspMetrics.recordLinkStart(project, workspace, platform, linkTask.releaseMode, linkTask.isTest)) >>
      run.guaranteeCase {
        case cats.effect.Outcome.Succeeded(resultIO) => resultIO.flatMap { case (taskResult, _) => end(taskResult == TaskDag.TaskResult.Success) }
        case _                                       => end(false)
      }
  }

  private def createLinkLogger(): LinkExecutor.LinkLogger = new LinkExecutor.LinkLogger {
    def trace(message: String): Unit = ()
    def debug(message: String): Unit = ()
    def info(message: String): Unit = sendLogMessage(message, MessageType.Info)
    def warn(message: String): Unit = sendLogMessage(message, MessageType.Warning)
    def error(message: String): Unit = sendLogMessage(message, MessageType.Error)
  }

  /** Classify non-JVM test result into TaskResult, distinguishing test failures from process crashes. */
  /** Outcome for a non-JVM suite that could not run (link failure, missing output) — carries the failing result's message so the client shows the reason. */
  private def erroredOutcomeOf(result: TaskDag.TaskResult): SuiteOutcome =
    SuiteOutcome.Errored(
      result match {
        case TaskDag.TaskResult.Failure(e, _) => e
        case TaskDag.TaskResult.Error(e, _)   => e
        case other                            => s"test suite could not run: $other"
      },
      None
    )

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
        val classDir = started.projectPaths(crossName).classes.toUri.toString
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
            val sdOpts = javaSemanticdbOptions(pluginPath, started.buildPaths.buildDir, started.projectPaths(crossName).classes)
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
        val classDir = started.projectPaths(crossName).classes.toUri.toString
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
        val classesDir = started.projectPaths(crossName).classes
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
        val classesDir = started.projectPaths(crossName).classes

        // Take the same exclusive lock a compile takes. Deleting `classes` and `.zinc` unlocked
        // races a compile or test on another connection that is reading them right now.
        // (The unsafeRunSync is the existing pattern for sync handlers; it goes away in the
        // handler-to-IO refactor along with all the others.)
        ProjectLock
          .acquire(
            project = crossName,
            outputDir = classesDir,
            mode = ProjectLock.LockMode.Exclusive,
            timeout = lockTimeout,
            onContention = () => logger.info(s"Waiting for lock to clean ${crossName.value}")
          )
          .use { _ =>
            IO.blocking {
              if (Files.exists(classesDir)) {
                bleep.internal.FileUtils.deleteDirectory(classesDir)
                cleaned = true
              }
              // Also clean analysis dir - same path structure as BuildPaths.targetDir
              val targetDir = started.buildPaths.variantBuildDir(crossName)
              val analysisDir = targetDir.resolve(".zinc")
              if (Files.exists(analysisDir)) {
                bleep.internal.FileUtils.deleteDirectory(analysisDir)
                cleaned = true
              }
            }
          }
          .unsafeRunSync()
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

        val suites = ClasspathTestDiscovery.discover(crossName, classesDir, classpath, resolved.testFrameworks)

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

  /** The BSP types on this transport carry jsoniter codecs; the bleep admin DTOs are circe, like the rest of bleep's own protocol (see `handleBuildChanged`).
    * These two bridge that gap at the edge rather than duplicating every DTO in a second encoding.
    */
  private def circeRaw[T: io.circe.Encoder](value: T): RawJson =
    RawJson(io.circe.syntax.EncoderOps(value).asJson.noSpaces.getBytes("UTF-8"))

  private def parseAdminRequest(method: String, params: Option[RawJson]): StatusRequest =
    params match {
      case None      => throw BspException(JsonRpcErrorCodes.InvalidParams, s"$method requires params")
      case Some(raw) =>
        circeDecode[StatusRequest](new String(raw.value, "UTF-8")) match {
          case Right(p)  => p
          case Left(err) => throw BspException(JsonRpcErrorCodes.InvalidParams, s"Could not parse $method: ${err.getMessage}")
        }
    }

  private def parseCopyStateRequest(params: Option[RawJson]): CopyStateRequest =
    params match {
      case None      => throw BspException(JsonRpcErrorCodes.InvalidParams, s"${BleepServerAdmin.CopyStateMethod} requires params")
      case Some(raw) =>
        circeDecode[CopyStateRequest](new String(raw.value, "UTF-8")) match {
          case Right(p)  => p
          case Left(err) => throw BspException(JsonRpcErrorCodes.InvalidParams, s"Could not parse ${BleepServerAdmin.CopyStateMethod}: ${err.getMessage}")
        }
    }

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

  /** Run the message loop to completion, treating an interrupt of this thread as the stop signal it is.
    *
    * Not `program.unsafeRunSync()`. That is defined as `unsafeRunTimed(Long.MaxValue.nanos).get`, and `unsafeRunTimed` answers `None` in exactly one reachable
    * case here — the calling thread was interrupted while parked waiting for the result. (The other case is the timeout, which `Long.MaxValue` nanoseconds does
    * not reach.) So `.get` turned "someone asked this connection to stop" into `NoSuchElementException: None.get` thrown out of the middle of the cats-effect
    * runtime, with a stack trace pointing at `IOPlatform.unsafeRunSync` and nothing naming the interrupt.
    *
    * Interrupting the thread that owns a connection is a legitimate way to stop it, and `BspTestHarness` does precisely that in its teardown. It closes the
    * transport first, so usually the loop has already returned by the time the interrupt lands and nothing is thrown — but when the interrupt wins the race the
    * harness prints `[BSP Test Server] Server thread crashed: java.util.NoSuchElementException: None.get` for what is a clean shutdown. It is load-dependent
    * rather than constant: six occurrences in one ubuntu-22.04-arm CI job, none in the same suite on an idle dev machine. Landing next to a genuine failure, it
    * reads as the cause of it — which is exactly what it did while #628 was being diagnosed.
    *
    * `unsafeRunTimed` has already cancelled the program by the time it answers `None`, so the `guarantee` above still runs. Note it runs *asynchronously* —
    * cancellation is `fiber.cancel.unsafeRunAndForget()` — so this returns without waiting for cleanup. That was equally true before; it is called out here
    * because "run() returned" does not mean this connection's locks are released.
    *
    * The interrupt flag is re-asserted rather than consumed: `unsafeRunTimed` cleared it by catching `InterruptedException`, and swallowing that would hide the
    * stop request from any caller that checks.
    */
  private[bsp] def runToCompletion(program: IO[Unit]): Unit =
    program.unsafeRunTimed(Long.MaxValue.nanos) match {
      case Some(()) => ()
      case None     => Thread.currentThread().interrupt()
    }

  /** Enable debug logging to stderr (for development only) */
  val DebugLogging: Boolean = sys.env.get("BLEEP_BSP_DEBUG").contains("true")

  /** Companion-level debug logging, for code outside the server instance — the test runner's protocol reader, which has no `logger` of its own.
    *
    * `System.err`, not the BSP log stream: this runs while a test is executing, and anything written to the client's log stream during a run is liable to be
    * read as the test's own output, which is the exact confusion this exists to end.
    */
  def debugLogStatic(message: String): Unit =
    if (DebugLogging) System.err.println(s"[bleep-bsp] $message")

  /** Translate a junit-platform version to the junit-jupiter/junit-vintage version it belongs with.
    *
    * junit-platform 1.x pairs with jupiter/vintage 5.x at the same minor.patch; from junit 6 the two version lines are unified.
    */
  private[bsp] def junitEngineVersionFor(junitPlatformVersion: String): String =
    if (junitPlatformVersion.startsWith("1.")) "5" + junitPlatformVersion.stripPrefix("1") else junitPlatformVersion

  /** `final` is load-bearing here, not decoration. A plain `val` referenced by another `val` earlier in the same object is still `null` when that one
    * initializes, and a rule triggering on `null` never fires — silently, with no junit reaching any fork. That was originally avoided by declaring this
    * *above* [[testRuntimeRules]], which is a property of the file that any later edit can undo without anyone noticing. `final val` on a string literal is
    * constant-folded into each use site, so declaration order cannot matter.
    */
  private final val JunitPlatformOrg = "org.junit.platform"

  /** One row of [[testRuntimeRules]]: something bleep adds to a test project's fork classpath, and the condition under which it adds it.
    *
    * A rule reads only the project's resolved dependency graph, so the whole policy is inspectable as data. Supporting a new framework bridge should be a new
    * row here and nothing else.
    */
  private[bsp] sealed trait TestRuntimeRule {
    def name: String

    /** The deps this rule contributes, empty when it does not fire. Java deps throughout: every artifact bleep injects into a test fork is a plain JVM library.
      */
    def contributes(project: CrossProjectName, modules: List[ResolvedProject.ResolvedModule]): List[model.Dep.JavaDependency]
  }

  private[bsp] object TestRuntimeRule {

    /** Fires for every test project, whatever it depends on. */
    final case class Always(name: String, deps: List[model.Dep.JavaDependency]) extends TestRuntimeRule {
      override def contributes(project: CrossProjectName, modules: List[ResolvedProject.ResolvedModule]): List[model.Dep.JavaDependency] = deps
    }

    /** Fires when the project resolved at least one module in `organization`, and hands `at` the single version they all agree on.
      *
      * Disagreement is a hard failure rather than a pick-one heuristic — see [[singleResolvedVersionOf]].
      */
    final case class WhenResolved(name: String, organization: String, at: String => List[model.Dep.JavaDependency]) extends TestRuntimeRule {
      override def contributes(project: CrossProjectName, modules: List[ResolvedProject.ResolvedModule]): List[model.Dep.JavaDependency] =
        singleResolvedVersionOf(project, organization, modules) match {
          case None          => Nil
          case Some(version) => at(version)
        }
    }

    /** Fires when the project resolved `organization` and nothing at all from `butNot`.
      *
      * The negative half is what makes a fixed version safe to inject. A rule that supplies a version the project has no opinion about supplies the *only*
      * opinion; a rule that supplies one alongside the project's own creates two, and coursier reconciles those per module to the highest — which is the exact
      * mechanism that made kotest 5 pass by luck and kotest 6 fail. Mutually exclusive triggers keep that from ever arising.
      */
    final case class WhenResolvedWithout(name: String, organization: String, butNot: String, deps: List[model.Dep.JavaDependency]) extends TestRuntimeRule {
      override def contributes(project: CrossProjectName, modules: List[ResolvedProject.ResolvedModule]): List[model.Dep.JavaDependency] =
        if (modules.exists(_.organization == organization) && !modules.exists(_.organization == butNot)) deps else Nil
    }
  }

  /** What bleep adds to a test project's fork classpath, as a table.
    *
    * The invariant every row is built around: **bleep never supplies a junit-platform version when the project has one of its own.**
    *
    * That is the precise form of the rule this table replaced. bleep used to add the junit launcher and both engines to *every* test project, at bleep's own
    * version — so a project that already resolved a junit-platform got two opinions, and coursier reconciles per module to the highest. kotest 5 (1.8.2) lost
    * to the injected line and passed by luck; kotest 6 (1.13.4) won and still ended up with a stale engine from elsewhere on the classpath. The hazard was
    * never that a default existed, it was that a default *competed*. So a version bleep chooses is only ever injected where the project expressed none, and the
    * triggers below are mutually exclusive by construction: the JUnit Platform row fires on `org.junit.platform` being present, the JUnit 4 row on it being
    * absent. Nothing bleep injects can ever be reconciled against a version the project picked. [[assertCoherentJunitClasspath]] still catches it if that
    * reasoning is ever wrong.
    *
    * The other half is that injection is conditional at all: a pure ScalaTest, munit or utest build used to carry five junit artifacts it would never load, and
    * a conflict surface conjured out of nothing. `ForkedTestRunner` dispatches on the framework name and only constructs `JUnitPlatformRunner` inside that
    * branch, and `loadFramework` probes with `Class.forName` and catches `ClassNotFoundException`, so a project with no junit needs no junit.
    *
    * `bleep-test-runner` itself is deliberately not a row: its version comes from the *server*, not from the project's graph (see [[fetchBleepTestRunnerOnly]]
    * for why, and for the `dev:` short-circuit), so it has no trigger to express here.
    *
    * Nothing here carries an exclusion, and nothing here should. Every dep the table injects is a junit artifact that has to resolve its own
    * `junit-platform-engine` and `junit-platform-commons` transitively — muzzle those and the fork loses exactly the classes the row exists to provide. The one
    * dep that ever needed muzzling was the sbt adapter, which is no longer injected. Exclusions do still apply to `bleep-test-runner`, which is not a row; see
    * [[ExcludeTestRuntime]].
    */
  private[bsp] val testRuntimeRules: List[TestRuntimeRule] = List(
    TestRuntimeRule.Always(
      name = "sbt test interface — the SPI every framework's Runner is loaded through",
      deps = List(model.Dep.Java("org.scala-sbt", "test-interface", model.Versions.TestInterface))
    ),
    TestRuntimeRule.WhenResolved(
      name = "JUnit Platform — junit-jupiter, kotest, spock and anything else with a TestEngine",
      organization = JunitPlatformOrg,
      at = { platformVersion =>
        // The junit-platform jars are version-sensitive in both directions: junit hard-fails when the launcher and the engine jars disagree
        // ("OutputDirectoryCreator not available … unaligned versions"), and an engine paired with a foreign `junit-platform-commons` blows up during
        // discovery (`NoSuchMethodError: ReflectionUtils.returnsVoid`, which 1.13 dropped in favour of `returnsPrimitiveVoid`). So everything here is
        // pinned to the version the project itself resolved.
        val engineVersion = junitEngineVersionFor(platformVersion)
        // No sbt adapter here. `jupiter-interface` used to be injected alongside these, from when junit ran through `sbt.testing.Framework` like every other
        // framework — but `ForkedTestRunner` routes every junit name to `JUnitPlatformRunner`, which drives the Launcher itself, so `loadFramework`'s junit
        // branch is unreachable and the adapter was never loaded. A project that declares it for its own reasons still has it, from its own classpath.
        List(
          model.Dep.Java("org.junit.platform", "junit-platform-launcher", platformVersion),
          model.Dep.Java("org.junit.jupiter", "junit-jupiter-engine", engineVersion),
          model.Dep.Java("org.junit.vintage", "junit-vintage-engine", engineVersion)
        )
      }
    ),
    TestRuntimeRule.WhenResolvedWithout(
      name = "JUnit 4 without the platform — run through the vintage engine, which the project cannot supply itself",
      organization = "junit",
      butNot = JunitPlatformOrg,
      // JUnit 4 predates the JUnit Platform and depends on none of it, but bleep runs its suites through the vintage engine anyway: test discovery reports
      // the framework as the display name "JUnit", and `ForkedTestRunner.isJUnitPlatformFramework` matches that, so `JUnitPlatformRunner` — and therefore
      // `junit-platform-launcher` — is on the path for every JUnit 4 suite. A project depending on `com.github.sbt:junit-interface` resolves `junit:junit`
      // and nothing from `org.junit.platform`, so without this row the fork dies before the protocol handshake with NoClassDefFoundError. This used to be
      // supplied by accident, by injecting junit into every project whether it wanted it or not.
      //
      // These are bleep's own versions rather than the project's because the project has none — that is what `butNot` guarantees. See the invariant above:
      // the only opinion, never a competing one. The pair is internally consistent by construction since both come from `model.Versions`.
      deps = List(
        model.Dep.Java("org.junit.platform", "junit-platform-launcher", model.Versions.JunitPlatformLauncher),
        model.Dep.Java("org.junit.vintage", "junit-vintage-engine", model.Versions.JunitVintageEngine)
      )
    )
  )

  /** Evaluate [[testRuntimeRules]] against a test project's resolved dependency graph, keeping each dep next to the rule that produced it. The one place rules
    * are applied.
    *
    * Attribution rather than a flat set, because "why is this jar on my test classpath?" is a question users ask and bleep is the only one who can answer it:
    * these deps appear in no build file. [[testRuntimeDeps]] flattens it for the resolver; [[fetchTestRuntimeDeps]] logs it.
    */
  private[bsp] def testRuntimeDepsByRule(project: CrossProjectName, resolved: ResolvedProject): List[(String, List[model.Dep.JavaDependency])] = {
    val modules = resolvedModulesOf(project, resolved)
    testRuntimeRules.map(rule => (rule.name, rule.contributes(project, modules))).filter { case (_, deps) => deps.nonEmpty }
  }

  /** Evaluate [[testRuntimeRules]] against a test project's resolved dependency graph. */
  private[bsp] def testRuntimeDeps(project: CrossProjectName, resolved: ResolvedProject): SortedSet[model.Dep] =
    SortedSet.empty[model.Dep] ++ testRuntimeDepsByRule(project, resolved).flatMap { case (_, deps) => deps }

  private def excludeAllOf(organizations: String*): model.JsonMap[coursier.core.Organization, model.JsonSet[coursier.core.ModuleName]] =
    model.JsonMap(organizations.map(org => (coursier.core.Organization(org), model.JsonSet(coursier.core.ModuleName("*")))).toMap)

  /** Every module of every junit organization, wildcarded so a POM that later adds a junit dependency bleep does not know about is excluded too — plus
    * `jupiter-interface`. Applied to `bleep-test-runner` so it contributes nothing but itself and [[testRuntimeRules]] stays the only source of the test
    * runtime. Redundant against the POM bleep publishes today, which declares neither — deliberately so, since this is the point where a wrong answer becomes a
    * fork running the wrong engine, and it also covers runner POMs published before that was cleaned up.
    *
    * `jupiter-interface` specifically, because letting a runner POM supply it breaks the invariant that it is on the classpath exactly when junit is.
    * `ForkedTestRunner.loadFramework` probes `net.aichler.jupiter.api.JupiterFramework` by name for any framework whose name contains "junit", and catches only
    * `ClassNotFoundException`. A jar that is present but whose junit classes are not resolves the name and then throws `NoClassDefFoundError` out of the
    * constructor, which nothing catches and which kills the fork — instead of falling through to the JUnit 4 adapter the project actually declared.
    */
  private val ExcludeTestRuntime: model.JsonMap[coursier.core.Organization, model.JsonSet[coursier.core.ModuleName]] =
    excludeAllOf("org.junit.platform", "org.junit.jupiter", "org.junit.vintage", "net.aichler")

  /** `junit-platform-<module>-<version>.jar`, `junit-jupiter-<module>-<version>.jar`, `junit-vintage-<module>-<version>.jar`. Captures the module name and the
    * version, both of which are needed to tell "same module twice at two versions" from "two different modules".
    *
    * Filenames, unlike [[detectJunitPlatformVersion]], and on purpose: this is the last check before the list of paths is handed to `java -cp`, and its whole
    * value is that it inspects what actually gets executed rather than a model of it. The classpath is assembled from the project's *runtime* resolution while
    * the version above is chosen from its *compile* resolution — a check that read the same model as the decision could not catch the two drifting apart.
    */
  private val JunitJar = "(junit-(?:platform|jupiter|vintage)(?:-[a-z]+)+)-(\\d[\\w.\\-]*)\\.jar".r

  /** Every junit module on a classpath, as module name -> (version, jar) in classpath order, one entry per distinct version. */
  private def junitModuleVersions(classpath: List[Path]): Map[String, List[(String, Path)]] =
    classpath
      .flatMap { jar =>
        jar.getFileName.toString match {
          case JunitJar(module, version) => Some((module, version, jar))
          case _                         => None
        }
      }
      .groupBy { case (module, _, _) => module }
      .map { case (module, triples) =>
        (module, triples.map { case (_, version, jar) => (version, jar) }.distinctBy { case (version, _) => version })
      }

  /** A test project's resolved dependency graph, which is what [[testRuntimeRules]] read.
    *
    * Read from [[ResolvedProject.Resolution]] rather than by pattern-matching jar filenames: which version bleep aligns to is a *decision*, and it should be
    * made from the resolution coursier already produced, not from a string that happens to appear in a path. `ResolveProjects` builds this for every project,
    * so it is always there — an absent resolution means a `ResolvedProject` was constructed by something that does not resolve, which is a bug worth hearing
    * about rather than silently guessing and shipping a mismatched engine.
    */
  private def resolvedModulesOf(project: CrossProjectName, resolved: ResolvedProject): List[ResolvedProject.ResolvedModule] =
    resolved.resolution
      .getOrElse(throw new BleepException.Text(project, "cannot decide what the test runner needs: this project was resolved without a dependency graph"))
      .modules

  /** The one version every module of `organization` resolved to, or `None` when the project resolved none of them.
    *
    * Reads every module in the organization rather than a nominated one: a project depending on `junit-jupiter-api` alone resolves `junit-platform-commons` and
    * no engine, and commons is precisely the artifact an injected engine must agree with.
    *
    * Throws when they disagree. bleep cannot align to two versions at once, and picking one silently is how you get a `NoSuchMethodError` in a test report
    * instead of an error naming the conflict.
    */
  private[bsp] def singleResolvedVersionOf(
      project: CrossProjectName,
      organization: String,
      modules: List[ResolvedProject.ResolvedModule]
  ): Option[String] = {
    val inOrg = modules.filter(_.organization == organization)
    inOrg.map(_.version).distinct.sorted match {
      case Nil            => None
      case version :: Nil => Some(version)
      case several        =>
        throw new BleepException.Text(
          project,
          s"""conflicting $organization versions in this project's dependencies: ${several.mkString(", ")}.
             |${inOrg.sortBy(_.name).map(m => s"  ${m.organization}:${m.name}:${m.version}").mkString("\n")}
             |bleep injects a test runtime matching the project's own $organization, and cannot match two of them at once.
             |Pin one $organization version in this project's dependencies.""".stripMargin
        )
    }
  }

  /** One line per (module, version) with the jar it came from — the jar path is what tells you *which* resolution contributed it, which is the first thing you
    * want to know when two of them disagree.
    */
  private def describeJunitModules(modules: Map[String, List[(String, Path)]]): String =
    modules.toList
      .sortBy { case (module, _) => module }
      .flatMap { case (module, versions) =>
        versions.map { case (version, jar) => s"  ${junitOrganizationOf(module)}:$module:$version — $jar" }
      }
      .mkString("\n")

  /** The assembled test classpath must carry at most one version of each junit module.
    *
    * The forked runner classpath is the concatenation of two independent coursier resolutions — the project's own dependencies and [[fetchTestRuntimeDeps]] —
    * so nothing in coursier guarantees they agree; only [[detectJunitPlatformVersion]] does. When they disagree, the JVM picks per class whichever jar comes
    * first, which is how a junit-jupiter-engine ends up calling a method its junit-platform-commons no longer has. Fail here, naming the coordinates, rather
    * than let that surface as a `NoSuchMethodError` attributed to the user's test.
    */
  private[bsp] def assertCoherentJunitClasspath(project: CrossProjectName, classpath: List[Path]): Unit = {
    val conflicts = junitModuleVersions(classpath).filter { case (_, versions) => versions.sizeIs > 1 }
    if (conflicts.nonEmpty)
      throw new BleepException.Text(
        project,
        s"""bleep assembled an incoherent JUnit classpath for the forked test runner (listed in classpath order — the first of each module is what the JVM loads):
           |${describeJunitModules(conflicts)}
           |This is a bug in bleep's test-runner dependency alignment, not in your build. Please report it.""".stripMargin
      )
  }

  private def junitOrganizationOf(module: String): String =
    if (module.startsWith("junit-platform-")) "org.junit.platform"
    else if (module.startsWith("junit-vintage-")) "org.junit.vintage"
    else "org.junit.jupiter"

  /** Per-(resolver, evaluated rule set) memoization of the [[testRuntimeRules]] resolution.
    *
    * Resolving once per key avoids re-running Coursier on every inner-bleep `commands.test`. Without this cache, each test workspace's [[InProcessBspServer]]
    * (a fresh [[MultiWorkspaceBspServer]] per `commands.test` call) re-fetches the same artifacts; under CI's CPU contention with two parallel test JVMs that's
    * enough to trip the 120 s suite-idle timeout in #580.
    *
    * Keyed by the evaluated deps rather than by a junit version, so any future rule participates in the cache correctly without anyone remembering to widen the
    * key. Keyed by resolver-instance identity (not process-wide) so two BSP servers configured with different resolver settings — different mirrors,
    * repositories, credentials — don't share jars resolved against the wrong config. Same resolver instance reused across calls within a server still hits the
    * cache.
    */
  private val cachedTestRuntimeJars: java.util.concurrent.ConcurrentHashMap[(CoursierResolver, SortedSet[model.Dep]), List[Path]] =
    new java.util.concurrent.ConcurrentHashMap[(CoursierResolver, SortedSet[model.Dep]), List[Path]]()

  private def fetchTestRuntimeDeps(started: Started, project: CrossProjectName, resolved: ResolvedProject): List[Path] = {
    val resolver = started.resolver
    val byRule = testRuntimeDepsByRule(project, resolved)
    // Debug rather than info: correct on every run, and noise on all but the one where someone is asking why a jar is present. `bleep -d test` prints it.
    byRule.foreach { case (ruleName, deps) =>
      started.logger
        .withContext("project", project.value)
        .withContext("rule", ruleName)
        .debug(s"test runtime: ${deps.map(_.repr).mkString(", ")}")
    }
    val deps = SortedSet.empty[model.Dep] ++ byRule.flatMap { case (_, deps) => deps }
    if (deps.isEmpty) return Nil
    val key = (resolver, deps)
    val cached = cachedTestRuntimeJars.get(key)
    if (cached != null) return cached
    val result = resolver.force(
      deps,
      model.VersionCombo.Jvm(model.VersionScala.Scala3),
      libraryVersionSchemes = SortedSet.empty[model.LibraryVersionScheme],
      context = s"resolving the test runtime for ${project.value}",
      model.IgnoreEvictionErrors.No
    )
    // putIfAbsent: identical resolver from two threads is harmless (Coursier's disk cache handles
    // concurrent downloads, and the jars resolved against the same config are identical).
    val existing = cachedTestRuntimeJars.putIfAbsent(key, result.jars)
    if (existing != null) existing else result.jars
  }
}
