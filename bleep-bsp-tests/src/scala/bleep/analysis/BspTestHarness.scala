package bleep.analysis

import bleep.bsp._
import ch.epfl.scala.bsp._
import com.github.plokhotnyuk.jsoniter_scala.core._

import java.io.{PipedInputStream, PipedOutputStream}
import java.nio.file.Path
import java.util.concurrent.{ArrayBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable

/** Test harness for BSP server integration tests.
  *
  * Starts a BspServer with piped streams, sends requests via JSON-RPC, and collects responses/notifications.
  *
  * Usage:
  * {{{
  * BspTestHarness.withServer(workspaceRoot) { client =>
  *   val result = client.initialize()
  *   val targets = client.buildTargets()
  *   val compileResult = client.compile(targets.targets.map(_.id))
  *   // assertions on compileResult and client.diagnostics
  * }
  * }}}
  */
object BspTestHarness {

  /** Events collected during BSP communication */
  sealed trait BspEvent
  object BspEvent {
    case class LogMessage(messageType: Int, message: String) extends BspEvent
    case class PublishDiagnostics(uri: String, targetUri: String, diagnostics: List[DiagnosticInfo], reset: Boolean) extends BspEvent
    case class TaskStart(taskId: String, message: Option[String]) extends BspEvent
    case class TaskProgress(taskId: String, message: Option[String]) extends BspEvent
    case class TaskFinish(taskId: String, statusCode: Int, message: Option[String]) extends BspEvent
  }

  /** Handle for an async request that can be cancelled */
  trait RequestHandle[R] {
    def requestId: Int
    def cancel(): Unit
    def await(): R
    def awaitWithTimeout(timeoutMs: Long): Option[R]
  }

  case class DiagnosticInfo(
      severity: Int,
      message: String,
      line: Option[Int],
      column: Option[Int]
  )

  /** Project configuration for tests */
  case class ProjectConfig(
      name: String,
      sources: Set[Path],
      classpath: List[Path],
      outputDir: Path,
      languageConfig: LanguageConfig,
      dependsOn: Set[String],
      isTest: Boolean,
      platform: BuildLoader.Platform = BuildLoader.Platform.Jvm
  )

  object ProjectConfig {

    /** Create a simple Scala project config */
    def scala(
        name: String,
        sources: Set[Path],
        outputDir: Path,
        scalaVersion: String,
        classpath: List[Path],
        isTest: Boolean
    ): ProjectConfig =
      ProjectConfig(
        name = name,
        sources = sources,
        classpath = classpath,
        outputDir = outputDir,
        languageConfig = ScalaConfig(scalaVersion, Nil),
        dependsOn = Set.empty,
        isTest = isTest
      )

    /** Create a Scala.js project config */
    def scalaJs(
        name: String,
        sources: Set[Path],
        outputDir: Path,
        scalaVersion: String,
        sjsVersion: String,
        classpath: List[Path],
        isTest: Boolean
    ): ProjectConfig =
      ProjectConfig(
        name = name,
        sources = sources,
        classpath = classpath,
        outputDir = outputDir,
        languageConfig = ScalaConfig(scalaVersion, Nil),
        dependsOn = Set.empty,
        isTest = isTest,
        platform = BuildLoader.Platform.ScalaJs(sjsVersion, scalaVersion)
      )

    /** Create a Scala Native project config */
    def scalaNative(
        name: String,
        sources: Set[Path],
        outputDir: Path,
        scalaVersion: String,
        snVersion: String,
        classpath: List[Path],
        isTest: Boolean
    ): ProjectConfig =
      ProjectConfig(
        name = name,
        sources = sources,
        classpath = classpath,
        outputDir = outputDir,
        languageConfig = ScalaConfig(scalaVersion, Nil),
        dependsOn = Set.empty,
        isTest = isTest,
        platform = BuildLoader.Platform.ScalaNative(snVersion, scalaVersion)
      )
  }

  /** Run a test with the BSP server (simple version - auto-detects project) */
  def withServer[A](workspaceRoot: Path)(f: BspClient => A): A = {
    val harness = new BspTestHarness(workspaceRoot, None)
    harness.use(f)
  }

  /** Run a test with BSP server and explicit project configuration */
  def withProject[A](workspaceRoot: Path, config: ProjectConfig)(f: BspClient => A): A = {
    val harness = new BspTestHarness(workspaceRoot, Some(List(config)))
    harness.use(f)
  }

  /** Run a test with BSP server and multiple project configurations */
  def withProjects[A](workspaceRoot: Path, configs: List[ProjectConfig])(f: BspClient => A): A = {
    val harness = new BspTestHarness(workspaceRoot, Some(configs))
    harness.use(f)
  }

  /** Client interface for sending BSP requests */
  trait BspClient {
    def initialize(): InitializeBuildResult
    def buildTargets(): WorkspaceBuildTargetsResult
    def compile(targets: List[BuildTargetIdentifier]): CompileResult
    def compile(targets: List[BuildTargetIdentifier], arguments: List[String]): CompileResult
    def test(targets: List[BuildTargetIdentifier]): TestResult
    def run(target: BuildTargetIdentifier, mainClass: String, args: List[String]): RunResult
    def cleanCache(targets: List[BuildTargetIdentifier]): CleanCacheResult
    def shutdown(): Unit

    /** Link targets (compile with --link flag) */
    def link(targets: List[BuildTargetIdentifier], release: Boolean): CompileResult

    /** Async versions for cancellation testing */
    def compileAsync(targets: List[BuildTargetIdentifier]): RequestHandle[CompileResult]
    def compileAsync(targets: List[BuildTargetIdentifier], arguments: List[String]): RequestHandle[CompileResult]
    def testAsync(targets: List[BuildTargetIdentifier]): RequestHandle[TestResult]
    def runAsync(target: BuildTargetIdentifier, mainClass: String, args: List[String]): RequestHandle[RunResult]

    /** Cancel a request by ID */
    def cancelRequest(requestId: Int): Unit

    /** All events collected during communication */
    def events: List[BspEvent]

    /** Log messages (subset of events) */
    def logMessages: List[String]

    /** Diagnostics grouped by file URI */
    def diagnosticsByFile: Map[String, List[DiagnosticInfo]]

    /** Clear collected events and diagnostics */
    def clear(): Unit

    /** Query scalac options for targets */
    def scalacOptions(targets: List[BuildTargetIdentifier]): ScalacOptionsResult

    /** Query javac options for targets */
    def javacOptions(targets: List[BuildTargetIdentifier]): JavacOptionsResult
  }
}

class BspTestHarness(workspaceRoot: Path, projectConfigs: Option[List[BspTestHarness.ProjectConfig]]) {
  import BspTestHarness._
  import JsonRpcCodecs.given

  def use[A](f: BspClient => A): A = {
    // Create pipes for bidirectional communication
    // Client writes to clientToServer, server reads from it
    // Server writes to serverToClient, client reads from it
    val clientToServer = new PipedOutputStream()
    val serverInput = new PipedInputStream(clientToServer, 65536)

    val serverToClient = new PipedOutputStream()
    val clientInput = new PipedInputStream(serverToClient, 65536)

    // Start server in background thread
    val server = new BspServer(serverInput, serverToClient, workspaceRoot)

    // Configure build state if project configs provided
    projectConfigs.foreach { configs =>
      val buildLoader = server.getBuildLoader
      val buildState = if (configs.size == 1) {
        val cfg = configs.head
        buildLoader.loadSimpleBuild(
          projectName = cfg.name,
          sources = cfg.sources,
          classpath = cfg.classpath,
          outputDir = cfg.outputDir,
          languageConfig = cfg.languageConfig,
          isTest = cfg.isTest,
          platform = cfg.platform
        )
      } else {
        val configTuples = configs.map { cfg =>
          (cfg.name, cfg.sources, cfg.classpath, cfg.outputDir, cfg.languageConfig, cfg.dependsOn, cfg.isTest, cfg.platform)
        }
        buildLoader.loadBuild(configTuples)
      }
      server.setBuildState(buildState)
    }

    val serverThread = new Thread(
      { () =>
        try server.run()
        catch {
          case e: Exception =>
            System.err.println(s"[BSP Test Server] Server thread crashed: ${e.getClass.getName}: ${e.getMessage}")
            e.printStackTrace(System.err)
        }
      },
      "bsp-test-server"
    )
    serverThread.setDaemon(true)
    serverThread.start()

    val client = new BspClientImpl(clientInput, clientToServer)

    try f(client)
    finally {
      try client.shutdown()
      catch { case _: Exception => () }
      try {
        clientToServer.close()
        clientInput.close()
        serverInput.close()
        serverToClient.close()
      } catch { case _: Exception => () }
      serverThread.interrupt()
    }
  }

  private class BspClientImpl(
      in: PipedInputStream,
      out: PipedOutputStream
  ) extends BspClient {

    private val transport = new JsonRpcTransport(in, out)
    private val requestId = AtomicInteger(0)
    private val collectedEvents = mutable.Buffer[BspEvent]()
    private val collectedDiagnostics = mutable.Map[String, mutable.Buffer[DiagnosticInfo]]()

    // Map of pending request IDs to their response queues
    private val pendingRequests = new java.util.concurrent.ConcurrentHashMap[Int, ArrayBlockingQueue[JsonRpcResponse]]()

    // Fallback queue for responses to unknown request IDs (shouldn't happen, but safety)
    private val defaultResponseQueue = new ArrayBlockingQueue[JsonRpcResponse](100)

    // Reader thread for handling responses and notifications
    private val readerRunnable: Runnable = () =>
      try
        while (true)
          readAndDispatch()
      catch {
        case _: Exception => () // Shutdown
      }
    private val readerThread = new Thread(readerRunnable, "bsp-test-reader")
    readerThread.setDaemon(true)
    readerThread.start()

    private def readAndDispatch(): Unit = {
      val headerLine = readHeaderLine()
      if (headerLine == null) throw new RuntimeException("Stream closed")

      val contentLengthMatch = "Content-Length: (\\d+)".r.findFirstMatchIn(headerLine)
      val contentLength = contentLengthMatch.map(_.group(1).toInt).getOrElse {
        throw new RuntimeException(s"Invalid header: $headerLine")
      }

      // Skip remaining headers until empty line
      var line = readHeaderLine()
      while (line != null) line = readHeaderLine()

      // Read content
      val content = new Array[Byte](contentLength)
      var read = 0
      while (read < contentLength) {
        val r = in.read(content, read, contentLength - read)
        if (r < 0) throw new RuntimeException("Unexpected end of stream")
        read += r
      }

      // Try to parse as response or notification
      val json = new String(content, "UTF-8")

      // Check if it has an "id" field (response) or just "method" (notification)
      if (json.contains("\"result\"") || json.contains("\"error\"")) {
        // It's a response - route to correct pending request
        val response = readFromArray[JsonRpcResponse](content)
        val responseId: Option[Int] = response.id match {
          case RpcId.IntId(i)    => Some(i)
          case RpcId.StringId(s) => scala.util.Try(s.toInt).toOption
          case null              => None
        }
        responseId match {
          case Some(reqId) =>
            val queue = pendingRequests.get(reqId)
            if (queue != null) queue.put(response)
            else defaultResponseQueue.put(response)
          case None =>
            defaultResponseQueue.put(response)
        }
      } else if (json.contains("\"method\"")) {
        // It's a notification
        handleNotification(json, content)
      }
    }

    private def readHeaderLine(): String = {
      val sb = new StringBuilder
      var prev = -1
      var curr = in.read()
      while (curr != -1) {
        if (prev == '\r' && curr == '\n') {
          val result = sb.dropRight(1).toString
          return if (result.isEmpty) null else result
        }
        sb.append(curr.toChar)
        prev = curr
        curr = in.read()
      }
      if (sb.isEmpty) null else sb.toString
    }

    private def handleNotification(json: String, content: Array[Byte]): Unit = {
      val notification = readFromArray[JsonRpcNotification](content)

      notification.method match {
        case "build/logMessage" =>
          notification.params.foreach { params =>
            try {
              val logParams = readFromArray[LogMessageParams](params.value)
              collectedEvents += BspEvent.LogMessage(logParams.`type`.value, logParams.message)
            } catch { case _: Exception => () }
          }

        case "build/publishDiagnostics" =>
          notification.params.foreach { params =>
            try {
              val diagParams = readFromArray[PublishDiagnosticsParams](params.value)
              val uri = diagParams.textDocument.uri.value
              val targetUri = diagParams.buildTarget.uri.value
              val diagnostics = diagParams.diagnostics.map { d =>
                DiagnosticInfo(
                  severity = d.severity.map(_.value).getOrElse(1),
                  message = d.message,
                  line = Some(d.range.start.line),
                  column = Some(d.range.start.character)
                )
              }
              val reset = diagParams.reset
              if (reset) {
                // reset=true means replace all diagnostics for this (doc, target)
                collectedDiagnostics(uri) = mutable.Buffer.from(diagnostics)
              } else {
                val buffer = collectedDiagnostics.getOrElseUpdate(uri, mutable.Buffer())
                buffer ++= diagnostics
              }
              collectedEvents += BspEvent.PublishDiagnostics(uri, targetUri, diagnostics.toList, reset)
            } catch { case _: Exception => () }
          }

        case "build/taskStart" =>
          notification.params.foreach { params =>
            try {
              val taskParams = readFromArray[TaskStartParams](params.value)
              collectedEvents += BspEvent.TaskStart(taskParams.taskId.id, taskParams.message)
            } catch { case _: Exception => () }
          }

        case "build/taskProgress" =>
          notification.params.foreach { params =>
            try {
              val taskParams = readFromArray[TaskProgressParams](params.value)
              collectedEvents += BspEvent.TaskProgress(taskParams.taskId.id, taskParams.message)
            } catch { case _: Exception => () }
          }

        case "build/taskFinish" =>
          notification.params.foreach { params =>
            try {
              val taskParams = readFromArray[TaskFinishParams](params.value)
              collectedEvents += BspEvent.TaskFinish(
                taskParams.taskId.id,
                taskParams.status.value,
                taskParams.message
              )
            } catch { case _: Exception => () }
          }

        case _ => ()
      }
    }

    /** Create and send a request, returning a handle for async use */
    private def sendRequestAsync[P: JsonValueCodec, R: JsonValueCodec](method: String, params: P)(using
        rCodec: JsonValueCodec[R]
    ): RequestHandle[R] = {
      val id = requestId.incrementAndGet()
      val queue = new ArrayBlockingQueue[JsonRpcResponse](1)
      pendingRequests.put(id, queue)

      val paramsRaw = RawJson(writeToArray(params))
      val request = JsonRpcRequest(
        jsonrpc = "2.0",
        id = Some(RpcId.IntId(id)),
        method = method,
        params = Some(paramsRaw)
      )

      // Send request
      val content = writeToArray(request)
      val header = s"Content-Length: ${content.length}\r\n\r\n"
      synchronized {
        out.write(header.getBytes("UTF-8"))
        out.write(content)
        out.flush()
      }

      new RequestHandle[R] {
        override def requestId: Int = id

        override def cancel(): Unit = cancelRequest(id)

        override def await(): R =
          try {
            val response = queue.poll(120, TimeUnit.SECONDS)
            if (response == null) {
              throw new RuntimeException(s"Timeout waiting for response to $method")
            }
            processResponse(response, method)
          } finally pendingRequests.remove(id)

        override def awaitWithTimeout(timeoutMs: Long): Option[R] =
          try {
            val response = queue.poll(timeoutMs, TimeUnit.MILLISECONDS)
            if (response == null) None
            else Some(processResponse(response, method))
          } finally pendingRequests.remove(id)

        private def processResponse(response: JsonRpcResponse, method: String): R = {
          response.error.foreach { err =>
            throw new RuntimeException(s"BSP error ${err.code}: ${err.message}")
          }
          response.result match {
            case Some(result) => readFromArray[R](result.value)(using rCodec)
            case None         => throw new RuntimeException(s"No result in response to $method")
          }
        }
      }
    }

    private def sendRequest[P: JsonValueCodec, R: JsonValueCodec](method: String, params: P): R =
      sendRequestAsync[P, R](method, params).await()

    private def sendRequestNoParams[R: JsonValueCodec](method: String): R = {
      val id = requestId.incrementAndGet()
      val queue = new ArrayBlockingQueue[JsonRpcResponse](1)
      pendingRequests.put(id, queue)

      val request = JsonRpcRequest(
        jsonrpc = "2.0",
        id = Some(RpcId.IntId(id)),
        method = method,
        params = None
      )

      val content = writeToArray(request)
      val header = s"Content-Length: ${content.length}\r\n\r\n"
      synchronized {
        out.write(header.getBytes("UTF-8"))
        out.write(content)
        out.flush()
      }

      try {
        val response = queue.poll(120, TimeUnit.SECONDS)
        if (response == null) {
          throw new RuntimeException(s"Timeout waiting for response to $method")
        }

        response.error.foreach { err =>
          throw new RuntimeException(s"BSP error ${err.code}: ${err.message}")
        }

        response.result match {
          case Some(result) => readFromArray[R](result.value)
          case None         => throw new RuntimeException(s"No result in response to $method")
        }
      } finally pendingRequests.remove(id)
    }

    private def sendNotificationWithParams[P: JsonValueCodec](method: String, params: P): Unit = {
      val paramsRaw = RawJson(writeToArray(params))
      val notification = JsonRpcNotification(
        jsonrpc = "2.0",
        method = method,
        params = Some(paramsRaw)
      )

      val content = writeToArray(notification)
      val header = s"Content-Length: ${content.length}\r\n\r\n"
      synchronized {
        out.write(header.getBytes("UTF-8"))
        out.write(content)
        out.flush()
      }
    }

    private def sendNotification(method: String): Unit = {
      val notification = JsonRpcNotification(
        jsonrpc = "2.0",
        method = method,
        params = None
      )

      val content = writeToArray(notification)
      val header = s"Content-Length: ${content.length}\r\n\r\n"
      synchronized {
        out.write(header.getBytes("UTF-8"))
        out.write(content)
        out.flush()
      }
    }

    override def initialize(): InitializeBuildResult = {
      val params = InitializeBuildParams(
        displayName = "BspTestClient",
        version = "1.0.0",
        bspVersion = "2.1.0",
        rootUri = Uri(workspaceRoot.toUri),
        capabilities = BuildClientCapabilities(
          languageIds = List("scala", "java", "kotlin"),
          jvmCompileClasspathReceiver = Some(true)
        ),
        dataKind = None,
        data = None
      )

      val result = sendRequest[InitializeBuildParams, InitializeBuildResult]("build/initialize", params)

      // Send initialized notification
      sendNotification("build/initialized")

      result
    }

    override def buildTargets(): WorkspaceBuildTargetsResult =
      // workspace/buildTargets has no params but expects empty object
      sendRequestNoParams[WorkspaceBuildTargetsResult]("workspace/buildTargets")

    override def compile(targets: List[BuildTargetIdentifier]): CompileResult =
      compile(targets, Nil)

    override def compile(targets: List[BuildTargetIdentifier], arguments: List[String]): CompileResult = {
      val params = CompileParams(
        targets = targets,
        originId = Some("test-compile"),
        arguments = if (arguments.isEmpty) None else Some(arguments)
      )
      sendRequest[CompileParams, CompileResult]("buildTarget/compile", params)
    }

    override def test(targets: List[BuildTargetIdentifier]): TestResult = {
      val params = TestParams(
        targets = targets,
        originId = Some("test-run"),
        arguments = None,
        environmentVariables = None,
        workingDirectory = None,
        dataKind = None,
        data = None
      )
      sendRequest[TestParams, TestResult]("buildTarget/test", params)
    }

    override def run(target: BuildTargetIdentifier, mainClass: String, args: List[String]): RunResult = {
      // Create ScalaMainClass data
      val scalaMainClass = ScalaMainClass(
        className = mainClass,
        arguments = args,
        jvmOptions = Nil,
        environmentVariables = None
      )
      val params = RunParams(
        target = target,
        originId = Some("test-run"),
        arguments = Some(args),
        environmentVariables = None,
        workingDirectory = None,
        dataKind = Some(RunParamsDataKind.ScalaMainClass),
        data = Some(RawJson(writeToArray(scalaMainClass)(using ScalaMainClass.codec)))
      )
      sendRequest[RunParams, RunResult]("buildTarget/run", params)
    }

    override def link(targets: List[BuildTargetIdentifier], release: Boolean): CompileResult = {
      val args = if (release) List("--link", "--release") else List("--link")
      compile(targets, args)
    }

    override def scalacOptions(targets: List[BuildTargetIdentifier]): ScalacOptionsResult = {
      val params = ScalacOptionsParams(targets = targets)
      sendRequest[ScalacOptionsParams, ScalacOptionsResult]("buildTarget/scalacOptions", params)
    }

    override def javacOptions(targets: List[BuildTargetIdentifier]): JavacOptionsResult = {
      val params = JavacOptionsParams(targets = targets)
      sendRequest[JavacOptionsParams, JavacOptionsResult]("buildTarget/javacOptions", params)
    }

    // Async versions for cancellation testing

    override def compileAsync(targets: List[BuildTargetIdentifier]): RequestHandle[CompileResult] =
      compileAsync(targets, Nil)

    override def compileAsync(targets: List[BuildTargetIdentifier], arguments: List[String]): RequestHandle[CompileResult] = {
      val params = CompileParams(
        targets = targets,
        originId = Some("test-compile"),
        arguments = if (arguments.isEmpty) None else Some(arguments)
      )
      sendRequestAsync[CompileParams, CompileResult]("buildTarget/compile", params)
    }

    override def testAsync(targets: List[BuildTargetIdentifier]): RequestHandle[TestResult] = {
      val params = TestParams(
        targets = targets,
        originId = Some("test-run"),
        arguments = None,
        environmentVariables = None,
        workingDirectory = None,
        dataKind = None,
        data = None
      )
      sendRequestAsync[TestParams, TestResult]("buildTarget/test", params)
    }

    override def runAsync(target: BuildTargetIdentifier, mainClass: String, args: List[String]): RequestHandle[RunResult] = {
      val scalaMainClass = ScalaMainClass(
        className = mainClass,
        arguments = args,
        jvmOptions = Nil,
        environmentVariables = None
      )
      val params = RunParams(
        target = target,
        originId = Some("test-run"),
        arguments = Some(args),
        environmentVariables = None,
        workingDirectory = None,
        dataKind = Some(RunParamsDataKind.ScalaMainClass),
        data = Some(RawJson(writeToArray(scalaMainClass)(using ScalaMainClass.codec)))
      )
      sendRequestAsync[RunParams, RunResult]("buildTarget/run", params)
    }

    override def cancelRequest(requestId: Int): Unit = {
      val params = CancelRequestParams(id = Right(requestId))
      sendNotificationWithParams("$/cancelRequest", params)
    }

    override def cleanCache(targets: List[BuildTargetIdentifier]): CleanCacheResult = {
      val params = CleanCacheParams(targets = targets)
      sendRequest[CleanCacheParams, CleanCacheResult]("buildTarget/cleanCache", params)
    }

    override def shutdown(): Unit = {
      try {
        sendNotification("build/shutdown")
        Thread.sleep(100)
        sendNotification("build/exit")
      } catch {
        case _: Exception => ()
      }
      readerThread.interrupt()
    }

    override def events: List[BspEvent] = collectedEvents.toList

    override def logMessages: List[String] =
      collectedEvents.collect { case BspEvent.LogMessage(_, msg) => msg }.toList

    override def diagnosticsByFile: Map[String, List[DiagnosticInfo]] =
      collectedDiagnostics.view.mapValues(_.toList).toMap

    override def clear(): Unit = {
      collectedEvents.clear()
      collectedDiagnostics.clear()
    }
  }
}
