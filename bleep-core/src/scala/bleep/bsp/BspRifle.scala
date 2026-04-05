package bleep.bsp

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import ryddig.Logger

import java.nio.file.{Files, Path}

/** Client-side library for managing BSP server lifecycle.
  *
  * Provides methods to start, connect to, and manage BSP server processes. Uses cats-effect for async operations, retries, and resource management.
  *
  * Typical usage:
  * {{{
  * val config = BspRifleConfig.default(...)
  * BspRifle.ensureRunningAndConnect(config).use { connection =>
  *   // Use connection.input/output for JSON-RPC communication
  * }
  * }}}
  */
object BspRifle {

  /** Check if a BSP server is currently running and accepting connections. */
  def check(config: BspRifleConfig): IO[Boolean] =
    BspServerOperations.check(config.address)

  /** Connect to an already running BSP server.
    *
    * Returns a Resource that manages the connection lifecycle. The connection will be closed when the Resource is released.
    *
    * @return
    *   Resource containing the connection, fails if server not running
    */
  def connect(config: BspRifleConfig): Resource[IO, BspConnection] =
    Resource
      .eval(
        for {
          conn <- BspServerOperations.openConnection(config.address)
          pidFile = config.address.socketDir.resolve("pid")
          maybePid <- BspServerOperations.readPid(pidFile)
        } yield (conn, maybePid)
      )
      .flatMap { case (conn, maybePid) => connectionToBspConnection(conn, maybePid) }

  /** Ensure a server is running, then connect to it.
    *
    * This is the main entry point for clients. It will:
    *   1. Check if a server is already running
    *   2. Clean up any zombie servers
    *   3. Start a new server if needed
    *   4. Wait for the server to be ready
    *   5. Connect and return the connection
    *
    * The returned Resource will close the connection when released, but will NOT stop the server (it may be used by other clients).
    */
  def ensureRunningAndConnect(config: BspRifleConfig, logger: Logger): Resource[IO, BspConnection] =
    Resource.eval(ensureRunning(config, logger)) >> connectWithRetry(config, logger)

  /** Ensure a server is running, starting one if necessary.
    *
    * In shared mode, reuses an existing server if one is already running. In ephemeral mode (dieWithParent), always starts a fresh server.
    *
    * Handles zombie detection and cleanup automatically.
    */
  def ensureRunning(config: BspRifleConfig, logger: Logger): IO[Unit] = {
    val socketDir = config.address.socketDir
    val mode = if (config.dieWithParent) "ephemeral" else "shared"
    for {
      _ <- IO(logger.info(s"Ensuring BSP server (mode=$mode, socket=$socketDir)"))

      _ <-
        if (config.dieWithParent) {
          // Ephemeral mode: each invocation gets its own server (unique socket dir via UUID in JvmKey)
          BspServerOperations.forceKillAndCleanup(socketDir) >>
            startAndWait(config, logger)
        } else {
          // Shared mode: reuse existing server if alive, otherwise start fresh
          val pidFile = socketDir.resolve("pid")
          BspServerOperations.readPid(pidFile).flatMap {
            case Some(pid) =>
              BspServerOperations.isProcessAlive(pid).flatMap {
                case true =>
                  // Server is already running — but it might still be starting up
                  // (PID file is written before socket is created). Wait for readiness.
                  IO(logger.info(s"BSP server already running (pid=$pid), reusing")) >>
                    BspServerOperations.waitForServer(config)
                case false =>
                  // Zombie: PID file exists but process is dead
                  IO(logger.info(s"BSP server pid=$pid is dead, cleaning up and starting fresh")) >>
                    BspServerOperations.cleanup(socketDir) >>
                    startAndWait(config, logger)
              }
            case None =>
              // No PID file — check for zombie lock file
              BspServerOperations.detectZombie(socketDir).flatMap {
                case true =>
                  IO(logger.info("Detected zombie BSP server state, cleaning up")) >>
                    BspServerOperations.cleanupZombie(socketDir) >>
                    startAndWait(config, logger)
                case false =>
                  // No server running at all — start fresh
                  startAndWait(config, logger)
              }
          }
        }
    } yield ()
  }

  /** Start a new server and wait for it to be ready. */
  def startAndWait(config: BspRifleConfig, logger: Logger): IO[Unit] =
    for {
      _ <- IO(logger.info("Waiting for BSP server to be ready"))
      process <- startServer(config)
      // Check if process exited immediately (e.g., exit code 222 = already running)
      exitedImmediately <- IO.blocking(process.isAlive).map(!_)
      _ <-
        if (exitedImmediately) {
          IO.blocking(process.exitValue()).flatMap { code =>
            if (code == BspServerOperations.ServerAlreadyRunningExitCode) {
              // Another server started between our check and start - that's fine
              IO(logger.info("BSP server already running, reusing"))
            } else {
              val outputFile = getOutputFile(config)
              IO(logger.error(s"BSP server exited immediately with code $code (server log: $outputFile)")) >>
                IO.raiseError(
                  new RuntimeException(
                    s"BSP server exited immediately with code $code"
                  )
                )
            }
          }
        } else {
          BspServerOperations.waitForServer(config).flatMap { _ =>
            IO(logger.info(s"BSP server ready (server log: ${getOutputFile(config)})"))
          }
        }
    } yield ()

  /** Start a new BSP server process.
    *
    * The server runs as a daemon and will continue running until explicitly stopped or when the JVM exits.
    *
    * @return
    *   The server process (for monitoring, not for direct interaction)
    */
  def startServer(config: BspRifleConfig): IO[Process] =
    BspServerOperations.startServer(config)

  /** Connect to a running server with retry on transient failures.
    *
    * Useful when connecting immediately after starting a server, as the server may not be ready yet.
    */
  def connectWithRetry(config: BspRifleConfig, logger: Logger): Resource[IO, BspConnection] =
    Resource
      .eval(
        for {
          _ <- IO(logger.info("Connecting to BSP server"))
          conn <- BspServerOperations.connectWithRetry(config)
          pidFile = config.address.socketDir.resolve("pid")
          maybePid <- BspServerOperations.readPid(pidFile)
          _ <- IO(logger.info(s"Connected to BSP server${maybePid.map(p => s" (pid=$p)").getOrElse("")}"))
        } yield (conn, maybePid)
      )
      .flatMap { case (conn, maybePid) => connectionToBspConnection(conn, maybePid) }

  /** Convert a low-level Connection to a BspConnection Resource with optional PID monitoring. */
  private def connectionToBspConnection(conn: BspServerOperations.Connection, serverPid: Option[Long]): Resource[IO, BspConnection] =
    (conn, serverPid) match {
      case (BspServerOperations.Connection.FromSocket(socket), Some(pid)) =>
        BspConnection.fromSocketWithPid(socket, pid)
      case (BspServerOperations.Connection.FromSocket(socket), None) =>
        BspConnection.fromSocketOnly(socket)
      case (BspServerOperations.Connection.FromChannel(channel), Some(pid)) =>
        BspConnection.fromChannelWithPid(channel, pid)
      case (BspServerOperations.Connection.FromChannel(channel), None) =>
        BspConnection.fromChannel(channel)
    }

  /** Request the server to shut down gracefully.
    *
    * Sends a shutdown signal and waits for the server to exit. If the server doesn't exit within the grace period, it will be force killed.
    */
  def exit(config: BspRifleConfig): IO[Unit] =
    BspServerOperations.shutdown(config)

  /** Get the server's output log file path. */
  def getOutputFile(config: BspRifleConfig): Path =
    config.address.socketDir.resolve("output")

  /** Read the server's output log.
    *
    * Returns the contents of stdout/stderr captured from the server process.
    */
  def readOutput(config: BspRifleConfig): IO[String] = IO.blocking {
    val outputFile = getOutputFile(config)
    if (Files.exists(outputFile)) {
      Files.readString(outputFile)
    } else {
      ""
    }
  }

  /** Read the last N lines of the server's output log. */
  def readOutputTail(config: BspRifleConfig, lines: Int): IO[String] = IO.blocking {
    val outputFile = getOutputFile(config)
    if (Files.exists(outputFile)) {
      val allLines = Files.readAllLines(outputFile)
      val start = Math.max(0, allLines.size() - lines)
      import scala.jdk.CollectionConverters.*
      allLines.asScala.slice(start, allLines.size()).mkString("\n")
    } else {
      ""
    }
  }

  /** Get information about a running server. */
  def serverInfo(config: BspRifleConfig): IO[Option[ServerInfo]] = {
    val socketDir = config.address.socketDir
    val pidFile = socketDir.resolve("pid")
    val socketPath = config.address match {
      case BspRifleConfig.Address.DomainSocket(path) => path
      case BspRifleConfig.Address.Tcp(_, _, dir)     => dir.resolve("socket")
    }

    BspServerOperations.readPid(pidFile).flatMap {
      case None      => IO.pure(None)
      case Some(pid) =>
        BspServerOperations.isProcessAlive(pid).map {
          case false => None
          case true  => Some(ServerInfo(pid, socketPath, "unknown"))
        }
    }
  }

  /** Force stop the server without graceful shutdown. */
  def forceStop(config: BspRifleConfig): IO[Unit] = {
    val pidFile = config.address.socketDir.resolve("pid")
    BspServerOperations.readPid(pidFile).flatMap {
      case None      => IO.unit
      case Some(pid) => BspServerOperations.forceKill(pid)
    } >> BspServerOperations.cleanup(config.address.socketDir)
  }

}
