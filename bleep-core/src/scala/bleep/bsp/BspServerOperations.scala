package bleep.bsp

import cats.effect.IO

import java.net.{ConnectException, InetSocketAddress, Socket, StandardProtocolFamily}
import java.nio.channels.SocketChannel
import java.nio.file.{Files, Path}
import java.nio.file.attribute.PosixFilePermissions
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** Low-level operations for BSP server lifecycle management.
  *
  * All operations are wrapped in cats-effect IO for proper error handling, timeouts, and retry logic.
  */
object BspServerOperations {

  /** Exit code indicating server is already running */
  val ServerAlreadyRunningExitCode: Int = 222

  // ==========================================================================
  // Process Management
  // ==========================================================================

  /** Spawn a new BSP server process.
    *
    * The server is started as a daemon process with stdout/stderr redirected to an output file for later inspection.
    */
  def startServer(config: BspRifleConfig): IO[Process] = IO
    .blocking {
      val socketDir = config.address.socketDir
      ensureDirectoryExists(socketDir)

      val outputFile = socketDir.resolve("output")
      // Preserve previous server's output for debugging, then start fresh
      val prevOutputFile = socketDir.resolve("output.prev")
      Files.deleteIfExists(prevOutputFile)
      if (Files.exists(outputFile)) {
        Files.move(outputFile, prevOutputFile)
      }

      val command = buildServerCommand(config)

      // Write diagnostic info to output file before starting - helps debug classpath issues
      Files.writeString(
        outputFile,
        s"BSP server classpath (${config.serverClasspath.size} entries):\n" +
          config.serverClasspath.zipWithIndex.map { case (p, i) => s"  [$i] $p (exists=${Files.exists(p)})" }.mkString("\n") +
          s"\nCommand: ${command.mkString(" ")}\n\n"
      )

      val pb = new ProcessBuilder(command.asJava)
      pb.directory(config.workingDir.toFile)
      // Redirect stderr (where our logger writes) to the output file.
      // Discard stdout — libraries (Bloop, Zinc) dump massive
      // amounts of data to stdout which would bloat the log to tens of GB.
      pb.redirectOutput(ProcessBuilder.Redirect.DISCARD)
      pb.redirectError(ProcessBuilder.Redirect.appendTo(outputFile.toFile))

      val process = pb.start()

      // For dieWithParent mode, keep stdin open so child can detect parent death via EOF.
      // For shared/daemon mode, close stdin since server runs independently.
      if (!config.dieWithParent) {
        process.getOutputStream.close()
      }

      // Write PID file
      val pidFile = socketDir.resolve("pid")
      Files.writeString(pidFile, process.pid().toString)

      process
    }
    .timeoutTo(
      30.seconds,
      IO.raiseError(new RuntimeException("BSP server process failed to start within 30 seconds (ProcessBuilder.start() hung)"))
    )

  /** Build the command to start the server process.
    *
    * The server is started without any workspace - workspaces are registered dynamically via BSP initialize or bleep/registerWorkspace.
    */
  private def buildServerCommand(config: BspRifleConfig): Seq[String] = {
    val classpath = config.serverClasspath.map(_.toString).mkString(java.io.File.pathSeparator)
    val socketArg = config.address match {
      case BspRifleConfig.Address.DomainSocket(path) =>
        Seq("--socket", path.getParent.toString)
      case BspRifleConfig.Address.Tcp(host, port, _) =>
        Seq("--host", host, "--port", port.toString)
    }
    val dieWithParentArg = if (config.dieWithParent) Seq("--die-with-parent") else Seq.empty

    Seq(config.javaPath.toString) ++
      config.javaOpts ++
      Seq("-cp", classpath, config.serverMainClass) ++
      socketArg ++
      dieWithParentArg
  }

  // ==========================================================================
  // Connection Management
  // ==========================================================================

  /** Check if a server is listening at the given address.
    *
    * Performs a non-blocking connection attempt.
    */
  def check(address: BspRifleConfig.Address): IO[Boolean] = IO.blocking {
    tryConnect(address).fold(_ => false, s => { s.close(); true })
  }

  /** A connected socket or channel */
  sealed trait Connection {
    def close(): Unit
  }
  object Connection {
    case class FromSocket(socket: Socket) extends Connection {
      def close(): Unit = socket.close()
    }
    case class FromChannel(channel: SocketChannel) extends Connection {
      def close(): Unit = channel.close()
    }
  }

  /** Open a connection to the server.
    *
    * May fail with ConnectException if server is not ready.
    */
  def openConnection(address: BspRifleConfig.Address): IO[Connection] = IO.blocking {
    tryConnect(address) match {
      case Right(conn) => conn
      case Left(ex)    => throw ex
    }
  }

  /** Try to connect to the given address */
  private def tryConnect(address: BspRifleConfig.Address): Either[Exception, Connection] =
    address match {
      case BspRifleConfig.Address.DomainSocket(path) =>
        tryConnectDomainSocket(path)
      case BspRifleConfig.Address.Tcp(host, port, _) =>
        tryConnectTcp(host, port)
    }

  /** Connect via Unix domain socket */
  private def tryConnectDomainSocket(path: Path): Either[Exception, Connection] =
    Try {
      val channel = SocketChannel.open(StandardProtocolFamily.UNIX)
      val addr = java.net.UnixDomainSocketAddress.of(path)
      channel.connect(addr)
      Connection.FromChannel(channel)
    }.toEither.left.map {
      case e: Exception => e
      case t            => new RuntimeException(t)
    }

  /** Connect via TCP socket */
  private def tryConnectTcp(host: String, port: Int): Either[Exception, Connection] =
    Try {
      val socket = new Socket()
      socket.connect(new InetSocketAddress(host, port), 1000)
      Connection.FromSocket(socket)
    }.toEither.left.map {
      case e: Exception => e
      case t            => new RuntimeException(t)
    }

  // ==========================================================================
  // Retry Logic
  // ==========================================================================

  /** Wait for server to become ready with polling.
    *
    * Polls by checking if the socket file exists and the server output contains "listening". We don't try to connect because that causes the server to wait for
    * JSON-RPC messages, which blocks if the checking client just connects and disconnects.
    */
  def waitForServer(config: BspRifleConfig): IO[Unit] = {
    val socketFile = config.address match {
      case BspRifleConfig.Address.DomainSocket(path) => path
      case BspRifleConfig.Address.Tcp(_, _, dir)     => dir.resolve("socket")
    }
    val outputFile = config.address.socketDir.resolve("output")

    def isReady: IO[Boolean] = IO.blocking {
      // Check both socket file exists AND server has logged "listening"
      Files.exists(socketFile) && {
        if (Files.exists(outputFile)) {
          Files.readString(outputFile).contains("listening")
        } else false
      }
    }

    def poll(deadline: Long): IO[Unit] =
      isReady.flatMap {
        case true => IO.unit
        case false =>
          IO.realTime.flatMap { now =>
            if (now.toMillis >= deadline) {
              IO.blocking {
                val socketExists = Files.exists(socketFile)
                val outputExists = Files.exists(outputFile)
                val outputContent = if (outputExists) {
                  val content = Files.readString(outputFile)
                  if (content.length > 2000) content.takeRight(2000) else content
                } else "<no output file>"
                val pidFile = config.address.socketDir.resolve("pid")
                val pidInfo = if (Files.exists(pidFile)) s"pid file: ${Files.readString(pidFile).trim}" else "no pid file"
                val details = List(
                  s"socket file exists: $socketExists",
                  s"output file exists: $outputExists",
                  pidInfo,
                  s"socket dir: ${config.address.socketDir}",
                  s"server output:\n$outputContent"
                ).mkString("\n  ")
                new RuntimeException(
                  s"BSP server failed to start within ${config.startCheckTimeout}\n  $details"
                )
              }.flatMap(IO.raiseError)
            } else {
              IO.sleep(config.startCheckPeriod) >> poll(deadline)
            }
          }
      }

    IO.realTime.flatMap { start =>
      val deadline = start.toMillis + config.startCheckTimeout.toMillis
      poll(deadline)
    }
  }

  /** Connect with retry on transient failures.
    *
    * Retries ConnectException until connection succeeds or timeout.
    */
  def connectWithRetry(config: BspRifleConfig): IO[Connection] = {
    def isRetryable(ex: Throwable): Boolean = ex match {
      case _: ConnectException                         => true
      case _: java.nio.file.NoSuchFileException        => true // Socket file not created yet
      case _: java.nio.channels.ClosedChannelException => true // Server restarting
      case _                                           => false
    }

    def attempt(deadline: Long): IO[Connection] =
      openConnection(config.address).handleErrorWith {
        case ex if isRetryable(ex) =>
          IO.realTime.flatMap { now =>
            if (now.toMillis >= deadline) {
              IO.raiseError(
                new RuntimeException(
                  s"Failed to connect to BSP server within ${config.connectionTimeout}: ${ex.getClass.getSimpleName}: ${ex.getMessage}"
                )
              )
            } else {
              IO.sleep(config.startCheckPeriod) >> attempt(deadline)
            }
          }
        case other => IO.raiseError(other)
      }

    IO.realTime.flatMap { start =>
      val deadline = start.toMillis + config.connectionTimeout.toMillis
      attempt(deadline)
    }
  }

  // ==========================================================================
  // Shutdown
  // ==========================================================================

  /** Gracefully shutdown the server with timeout, then force kill if needed. */
  def shutdown(config: BspRifleConfig): IO[Unit] = {
    val socketDir = config.address.socketDir
    val pidFile = socketDir.resolve("pid")

    readPid(pidFile).flatMap {
      case None => cleanup(socketDir)
      case Some(pid) =>
        val graceful = sendShutdownSignal(pid) >>
          waitForProcessExit(pid, config.shutdownGracePeriod)

        graceful.handleErrorWith { _ =>
          // Grace period expired - force kill
          forceKill(pid)
        } >> cleanup(socketDir)
    }
  }

  /** Send shutdown signal to process */
  private def sendShutdownSignal(pid: Long): IO[Unit] = IO.blocking {
    ProcessHandle.of(pid).ifPresent { handle =>
      handle.destroy() // SIGTERM on Unix
    }
  }

  /** Force kill a process */
  def forceKill(pid: Long): IO[Unit] = IO.blocking {
    ProcessHandle.of(pid).ifPresent(_.destroyForcibly())
  }

  /** Wait for a process to exit */
  def waitForProcessExit(pid: Long, timeout: FiniteDuration): IO[Unit] = {
    def poll(deadline: Long): IO[Unit] =
      IO.blocking(ProcessHandle.of(pid).isEmpty).flatMap {
        case true => IO.unit
        case false =>
          IO.realTime.flatMap { now =>
            if (now.toMillis >= deadline) {
              IO.raiseError(new RuntimeException(s"Process $pid did not exit within $timeout"))
            } else {
              IO.sleep(100.millis) >> poll(deadline)
            }
          }
      }

    IO.realTime.flatMap { start =>
      val deadline = start.toMillis + timeout.toMillis
      poll(deadline)
    }
  }

  // ==========================================================================
  // PID & File Management
  // ==========================================================================

  /** Read PID from file */
  def readPid(pidFile: Path): IO[Option[Long]] = IO.blocking {
    if (Files.exists(pidFile)) {
      Some(Files.readString(pidFile).trim.toLong)
    } else {
      None
    }
  }

  /** Check if a process with the given PID is alive */
  def isProcessAlive(pid: Long): IO[Boolean] = IO.blocking {
    ProcessHandle.of(pid).isPresent
  }

  /** Cleanup socket directory (lock, pid, socket, output files) */
  def cleanup(socketDir: Path): IO[Unit] = IO.blocking {
    val filesToDelete = Seq("lock", "pid", "socket", "output")
    filesToDelete.foreach { name =>
      try Files.deleteIfExists(socketDir.resolve(name))
      catch { case _: Exception => () }
    }
  }

  /** Ensure a directory exists with proper permissions */
  private def ensureDirectoryExists(dir: Path): Unit =
    if (!Files.exists(dir)) {
      Files.createDirectories(dir)
      // Set restrictive permissions on Unix
      try
        Files.setPosixFilePermissions(dir, PosixFilePermissions.fromString("rwx------"))
      catch {
        case _: UnsupportedOperationException => () // Windows
      }
    }

  // ==========================================================================
  // Zombie Detection
  // ==========================================================================

  /** Check if there's a zombie server (lock exists but process is dead) */
  def detectZombie(socketDir: Path): IO[Boolean] = {
    val pidFile = socketDir.resolve("pid")
    val lockFile = socketDir.resolve("lock")

    IO.blocking(Files.exists(lockFile)).flatMap {
      case false => IO.pure(false)
      case true =>
        readPid(pidFile).flatMap {
          case None      => IO.pure(true) // Lock but no PID = zombie
          case Some(pid) => isProcessAlive(pid).map(!_)
        }
    }
  }

  /** Clean up zombie server state */
  def cleanupZombie(socketDir: Path): IO[Unit] =
    cleanup(socketDir)

  /** HACK: Force kill any existing server and cleanup, ensuring fresh code is used. Used during development to avoid stale server issues.
    */
  def forceKillAndCleanup(socketDir: Path): IO[Unit] = {
    val pidFile = socketDir.resolve("pid")
    readPid(pidFile).flatMap {
      case None => cleanup(socketDir)
      case Some(pid) =>
        forceKill(pid) >>
          // Wait for the process to actually exit (up to 5 seconds) instead of a blind sleep.
          // This prevents race conditions where the old process still holds file handles.
          waitForProcessExit(pid, 5.seconds).handleErrorWith { _ =>
            // If it still hasn't exited after 5s, try killing descendants too
            IO.blocking {
              ProcessHandle.of(pid).ifPresent { handle =>
                handle.descendants().forEach(_.destroyForcibly())
                handle.destroyForcibly()
              }
            } >> IO.sleep(500.millis)
          } >>
          cleanup(socketDir)
    }
  }
}
