package bleep.bsp

import libdaemonjvm._
import libdaemonjvm.internal.{LockProcess, SocketHandler}
import libdaemonjvm.server._
import ryddig.{LogPatterns, Logger, Loggers}
import ryddig.jul.RyddigJulBridge

import java.nio.file.{Files, Path, Paths}
import java.nio.file.attribute.PosixFilePermissions
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Properties

/** Daemon entry point for the BSP server.
  *
  * This is the main class that gets spawned by the client (BspRifle). It handles:
  *   - Lock file acquisition (to prevent multiple servers)
  *   - Unix domain socket listening
  *   - Connection acceptance (multiple clients, one at a time)
  *   - Multiple workspace support (workspaces share server via JVM key)
  *   - Graceful shutdown
  *
  * Exit codes:
  *   - 0: Normal shutdown
  *   - 222: Server already running (another instance has the lock)
  *   - 1: Fatal error
  */
object BspServerDaemon {

  private val ServerAlreadyRunningExitCode = 222

  /** Registered workspaces that this server handles */
  private val workspaces = ConcurrentHashMap[Path, WorkspaceState]()

  /** State for a registered workspace */
  case class WorkspaceState(
      root: Path,
      buildState: Option[BuildState]
  )

  case class DaemonConfig(
      socketDir: Path,
      initialWorkspaces: Seq[Path],
      singleConnection: Boolean = false,
      dieWithParent: Boolean = false
  )

  def main(args: Array[String]): Unit = {
    val config = parseArgs(args) match {
      case Right(c)  => c
      case Left(err) =>
        // Logger not yet created, use System.err for arg parsing errors
        System.err.println(s"Error: $err")
        System.err.println("Usage: bsp-server --socket <dir> [--workspace <path>]...")
        System.exit(1)
        return
    }

    run(config)
  }

  /** Register a workspace with the running server. Can be called at runtime to add workspaces dynamically.
    */
  def registerWorkspace(workspaceRoot: Path): Unit = {
    val normalized = workspaceRoot.toAbsolutePath.normalize()
    workspaces.putIfAbsent(normalized, WorkspaceState(normalized, None))
  }

  /** Unregister a workspace from the server. */
  def unregisterWorkspace(workspaceRoot: Path): Unit = {
    val normalized = workspaceRoot.toAbsolutePath.normalize()
    workspaces.remove(normalized)
  }

  /** Get all registered workspaces */
  def getWorkspaces: Seq[Path] =
    workspaces.keys().asScala.toSeq

  /** Update build state for a workspace */
  def updateBuildState(workspaceRoot: Path, state: BuildState): Unit = {
    val normalized = workspaceRoot.toAbsolutePath.normalize()
    workspaces.computeIfPresent(normalized, (_, ws) => ws.copy(buildState = Some(state)))
  }

  /** Get workspace state */
  def getWorkspaceState(workspaceRoot: Path): Option[WorkspaceState] = {
    val normalized = workspaceRoot.toAbsolutePath.normalize()
    Option(workspaces.get(normalized))
  }

  def run(config: DaemonConfig): Unit = {
    // Create daemon-level logger that writes to stderr (ProcessBuilder redirects to socketDir/output)
    val logger = Loggers.stderr(LogPatterns.logFile)

    // Install JUL bridge so library logging (e.g. lsp4j) goes through ryddig
    val rootJulLogger = java.util.logging.Logger.getLogger("")
    rootJulLogger.getHandlers.foreach(rootJulLogger.removeHandler)
    RyddigJulBridge.install(logger)

    // Register initial workspaces
    config.initialWorkspaces.foreach { ws =>
      registerWorkspace(ws)
      logger.info(s"Registered workspace: ${ws.toAbsolutePath.normalize()}")
    }

    // Start parent death monitor if configured (for ephemeral servers)
    if (config.dieWithParent) {
      startParentDeathMonitor(logger)
    }

    val retryPeriod = 3.seconds
    val maxAttempts = 10

    @tailrec
    def loop(remainingAttempts: Int): Unit = {
      ensureSafeDirectoryExists(config.socketDir)
      val lockFiles = LockFiles.under(config.socketDir)

      val res = Lock.tryAcquire(lockFiles, LockProcess.default) {
        runWithLock(config, lockFiles.socketPaths, logger)
      }

      res match {
        case Left(err: LockError) if err.isInstanceOf[LockError.ZombieFound] =>
          logger.warn(s"Found zombie process, removing files in ${config.socketDir}")
          try Files.deleteIfExists(lockFiles.pidFile)
          catch { case _: Exception => () }
          try Files.deleteIfExists(lockFiles.lockFile)
          catch { case _: Exception => () }
          try Files.deleteIfExists(lockFiles.socketPaths.path)
          catch { case _: Exception => () }
          if (remainingAttempts > 0) {
            loop(remainingAttempts - 1)
          } else {
            logger.error("Failed to acquire lock after zombie cleanup")
            System.exit(1)
          }

        case Left(err: LockError) if err.isInstanceOf[LockError.AlreadyRunning] =>
          logger.info(s"BSP server already running in ${config.socketDir}")
          System.exit(ServerAlreadyRunningExitCode)

        case Left(err: LockError.RecoverableError) =>
          if (remainingAttempts > 0) {
            logger.warn(s"Caught $err, trying again in $retryPeriod")
            Thread.sleep(retryPeriod.toMillis)
            loop(remainingAttempts - 1)
          } else {
            logger.error(s"Recoverable lock error after max attempts: $err")
            System.exit(1)
          }

        case Left(err: LockError.FatalError) =>
          logger.error(s"Fatal lock error: $err")
          System.exit(1)

        case Right(()) =>
          // Normal exit
          ()
      }
    }

    loop(maxAttempts)
  }

  private def runWithLock(config: DaemonConfig, socketPaths: SocketPaths, logger: Logger): Unit = {
    val shutdownRequested = AtomicBoolean(false)
    val connectionCounter = AtomicInteger(0)
    val activeClientThreads = ConcurrentHashMap.newKeySet[Thread]()

    // Server-wide compile semaphore: limits total concurrent compilations across all connections
    val numCores = Runtime.getRuntime.availableProcessors()
    val compileSemaphore = new java.util.concurrent.Semaphore(numCores)
    logger.info(s"Compile semaphore: $numCores permits (based on available processors)")

    // Install shutdown hook
    Runtime.getRuntime.addShutdownHook(new Thread(() => shutdownRequested.set(true)))

    // Install process reaper shutdown hook.
    // Belt-and-suspenders: even if cleanup in MultiWorkspaceBspServer fails
    // (e.g., server crashes or is killed), this kills all descendant processes
    // (test JVMs, native compilers, etc.) to prevent zombie processes.
    Runtime.getRuntime.addShutdownHook(new Thread("process-reaper") {
      override def run(): Unit =
        try
          ProcessHandle.current().descendants().forEach { ph =>
            try ph.destroyForcibly()
            catch { case _: Exception => () }
          }
        catch { case _: Exception => () }
    })

    // NOTE: Do NOT redirect stdout — Zinc writes massive amounts of data to
    // stdout which would bloat the log file to tens of GB.
    // stderr is captured by ProcessBuilder.redirectError(outputFile) so
    // System.err.println calls from any code go to the server log file.
    // The logger (created via Loggers.stderr) also writes to the original
    // System.err which goes to the same output file.

    // Initialize metrics collection
    BspMetrics.initialize(config.socketDir)
    Runtime.getRuntime.addShutdownHook(new Thread("metrics-shutdown") {
      override def run(): Unit = BspMetrics.shutdown()
    })

    logger.info(s"BSP server starting...")
    logger.info(s"Socket: ${socketPaths.path}")
    logger.info(s"Initial workspaces: ${config.initialWorkspaces.mkString(", ")}")
    logger.info(s"Metrics: ${config.socketDir.resolve("metrics.jsonl")}")

    // Create server socket using libdaemonjvm
    val serverChannel = SocketHandler.server(socketPaths)
    val serverSocket = libdaemonjvm.Util.serverSocketFromChannel(serverChannel)

    logger.info(s"BSP server listening on ${socketPaths.path}")

    try
      // Accept connections concurrently — each client gets its own thread.
      // Client threads are non-daemon so the JVM stays alive even if the
      // main thread exits the accept loop.
      while (!shutdownRequested.get()) {
        logger.info(s"Waiting for next connection (channel.isOpen=${serverChannel.isOpen()})...")
        try {
          val clientSocket = serverSocket.accept()
          if (clientSocket != null) {
            val connId = connectionCounter.incrementAndGet()
            logger.info(s"Client #$connId connected")
            BspMetrics.recordConnectionOpen(connId)

            if (config.singleConnection) {
              // Single connection mode: handle inline and shutdown after
              try handleClient(clientSocket.getInputStream, clientSocket.getOutputStream, logger, config.socketDir, compileSemaphore)
              finally BspMetrics.recordConnectionClose(connId)
              try clientSocket.close()
              catch { case _: Exception => () }
              logger.info("Single connection mode - shutting down")
              shutdownRequested.set(true)
            } else {
              val thread = new Thread(
                () =>
                  try handleClient(clientSocket.getInputStream, clientSocket.getOutputStream, logger, config.socketDir, compileSemaphore)
                  finally {
                    BspMetrics.recordConnectionClose(connId)
                    try clientSocket.close()
                    catch { case _: Exception => () }
                    activeClientThreads.remove(Thread.currentThread())
                    logger.info(s"Client #$connId thread exiting")
                  },
                s"bsp-client-$connId"
              )
              // Non-daemon: keeps JVM alive independently of main thread
              activeClientThreads.add(thread)
              thread.start()
            }
          } else {
            logger.warn("accept() returned null — channel may be non-blocking")
          }
        } catch {
          case _: java.net.SocketException if shutdownRequested.get() =>
            // Expected during shutdown
            ()
          case e: java.nio.channels.ClosedChannelException =>
            logger.warn(s"Server socket closed (${e.getClass.getName}), stopping accept loop")
            shutdownRequested.set(true)
          case e: Throwable =>
            logger.error(s"Error in accept loop: ${e.getClass.getName}: ${e.getMessage}", e)
            if (!e.isInstanceOf[Exception]) {
              // For Errors (OOM, StackOverflow, etc.), stop accepting
              shutdownRequested.set(true)
            }
        }
      }
    finally {
      logger.info(s"Accept loop exited (shutdownRequested=${shutdownRequested.get()}, activeClients=${activeClientThreads.size()})")
      try serverSocket.close()
      catch { case _: Exception => () }
      // Don't join client threads — they're non-daemon and keep the JVM alive
      // on their own. This allows the main thread to continue accepting new
      // connections (if we loop back) or exit without blocking.
      logger.info("Accept loop cleanup complete")
    }
  }

  private def handleClient(
      input: java.io.InputStream,
      output: java.io.OutputStream,
      logger: Logger,
      socketDir: Path,
      compileSemaphore: java.util.concurrent.Semaphore
  ): Unit =
    try {
      // Create multi-workspace server using the daemon-level logger
      val server = new MultiWorkspaceBspServer(
        input,
        output,
        logger,
        socketDir = Some(socketDir),
        compileSemaphore = compileSemaphore
      )

      // Run server message loop
      server.run()

      logger.info("Client disconnected normally")
    } catch {
      case e: OutOfMemoryError =>
        System.err.println(s"[FATAL] OutOfMemoryError in handleClient: ${e.getMessage}")
        throw e // Let JVM handle OOM
      case e: Throwable =>
        logger.error(s"Error handling client: ${e.getClass.getName}: ${e.getMessage}", e)
        System.err.println(s"[BSP] handleClient failed: ${e.getClass.getName}: ${e.getMessage}")
        e.printStackTrace(System.err)
    }

  private def parseArgs(args: Array[String]): Either[String, DaemonConfig] = {
    var socketDir: Option[Path] = None
    var workspaces: List[Path] = Nil
    var singleConnection: Boolean = false
    var dieWithParent: Boolean = false

    var i = 0
    while (i < args.length)
      args(i) match {
        case "--socket" =>
          if (i + 1 >= args.length) return Left("--socket requires a path")
          socketDir = Some(Paths.get(args(i + 1)))
          i += 2
        case "--workspace" =>
          if (i + 1 >= args.length) return Left("--workspace requires a path")
          workspaces = workspaces :+ Paths.get(args(i + 1))
          i += 2
        case "--single-connection" =>
          singleConnection = true
          i += 1
        case "--die-with-parent" =>
          dieWithParent = true
          i += 1
        case other =>
          return Left(s"Unknown argument: $other")
      }

    socketDir match {
      case Some(s) =>
        Right(DaemonConfig(s, workspaces, singleConnection, dieWithParent))
      case None =>
        Left("--socket is required")
    }
  }

  /** Start a daemon thread that monitors stdin for EOF.
    *
    * When the parent process dies (even via SIGKILL), the OS closes the pipe, causing stdin.read() to return -1 (EOF). This allows the child to detect parent
    * death and exit cleanly.
    *
    * This is the most portable cross-platform mechanism for subprocess death detection that works from Java without native code.
    */
  private def startParentDeathMonitor(logger: Logger): Unit = {
    val thread = new Thread("parent-death-monitor") {
      override def run(): Unit =
        try {
          // Block on stdin read - returns -1 (EOF) when parent dies
          // The parent keeps stdin open specifically for this purpose
          while (System.in.read() != -1) {
            // Drain any data (shouldn't be any, but be safe)
          }
          logger.info("Parent process died (stdin EOF detected), shutting down server")
          System.exit(0)
        } catch {
          case _: java.io.IOException =>
            logger.warn("Stdin closed unexpectedly, shutting down server")
            System.exit(0)
          case _: InterruptedException =>
            // Thread interrupted during shutdown, ignore
            ()
        }
    }
    thread.setDaemon(true)
    thread.start()
    logger.info("Parent death monitor started (will exit on stdin EOF)")
  }

  private def ensureSafeDirectoryExists(dir: Path): Unit =
    if (!Files.exists(dir)) {
      Files.createDirectories(dir)
      if (!Properties.isWin) {
        Files.setPosixFilePermissions(dir, PosixFilePermissions.fromString("rwx------"))
        ()
      }
    }
}
