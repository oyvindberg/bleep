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

  /** Per-socket-dir JVM-wide permit, the same-process half of spawn election (see [[ensureRunning]]). POSIX file locks do not exclude threads within one
    * process, so when several fibers/threads in ONE JVM race to spawn — the stress harness, and the MCP server driving concurrent operations — this keeps them
    * to a single spawner. A Semaphore, not a ReentrantLock: cats-effect runs a Resource's acquire and release on different pool threads, and a ReentrantLock
    * may only be unlocked by the thread that locked it (unlock from another thread throws IllegalMonitorStateException, leaving it held forever). A semaphore
    * permit is not thread-owned. Keyed by absolute path; entries are never removed (one tiny permit per socket dir the process ever touches).
    */
  private val jvmSpawnLocks = new java.util.concurrent.ConcurrentHashMap[Path, java.util.concurrent.Semaphore]()

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

  /** Ensure a daemon is reachable, then open a connection to it.
    *
    * The main entry point for clients: [[ensureRunning]] guarantees a daemon is accepting on this socket (connect-or-spawn), then [[connectWithRetry]] opens
    * the real connection. The returned Resource closes the connection when released but does NOT stop the server — it is shared with other clients.
    */
  def ensureRunningAndConnect(config: BspRifleConfig, logger: Logger): Resource[IO, BspConnection] =
    Resource.eval(ensureRunning(config, logger)) >> connectWithRetry(config, logger)

  /** Ensure a daemon is reachable on this socket, spawning one if not.
    *
    * Connect-or-spawn: the socket is the single source of truth. A successful connect proves a daemon owns this socket and is accepting — reuse it. Only a
    * connect *refusal* (no socket file, or a stale one with nothing listening) means there is no daemon, and that is the sole trigger to spawn. This is
    * deliberately load-blind: connecting to a bound unix socket completes at the kernel level regardless of how busy the daemon is (its accept loop runs on a
    * thread that never touches compile work), so a busy server is never mistaken for a missing one, and we never spawn a redundant daemon against it. It also
    * replaces the old pid-file/zombie reasoning entirely: a live pid whose daemon does not answer is no longer "reuse forever", and file-level cleanup of stale
    * sockets/locks is the incoming daemon's own job (libdaemonjvm's lock protocol), not ours to second-guess.
    *
    * Spawning is race-tolerant rather than race-free: several clients may spawn at once, libdaemonjvm's lock lets exactly one win, and the losers exit
    * [[BspServerOperations.ServerAlreadyRunningExitCode]] while everyone connects to the winner. Any other immediate exit is a real startup failure and is
    * surfaced with the server log. Readiness after a spawn is a connect probe ([[BspServerOperations.waitForServer]]), immune to how the log is rotated.
    */
  def ensureRunning(config: BspRifleConfig, logger: Logger): IO[Unit] = {
    val socketDir = config.address.socketDir
    val mode = if (config.dieWithParent) "ephemeral" else "shared"

    // Launch a daemon. Tolerate the lock-race loser (it exits ServerAlreadyRunning); surface any real startup failure.
    def spawn: IO[Unit] =
      oomCrashExplanation(config).flatMap(_.fold(IO.unit)(msg => IO(logger.warn(msg)))) >>
        startServer(config, logger).flatMap { process =>
          IO.blocking(process.isAlive).flatMap {
            case true  => IO.unit // came up; readiness is checked by the caller via waitForServer
            case false =>
              IO.blocking(process.exitValue()).flatMap {
                case BspServerOperations.ServerAlreadyRunningExitCode => IO.unit // another client won the race — fine, we connect to theirs
                case code                                             =>
                  IO(logger.error(s"BSP server exited immediately with code $code (server log: ${getOutputFile(config)})")) >>
                    IO.raiseError(new RuntimeException(s"BSP server exited immediately with code $code"))
              }
          }
        }

    // Elect a single spawner so a swarm hitting a cold daemon forks ONE server JVM, not one per client — the fork storm that can swamp the machine and starve
    // the build server everyone shares. Two layers, because the two kinds of contention need different primitives:
    //   - a JVM-wide ReentrantLock per socket dir, for concurrent fibers/threads WITHIN this process (the harness, the MCP server). POSIX file locks do NOT
    //     exclude same-process threads, so a file lock alone is not enough here;
    //   - a FileChannel advisory lock on `.spawn-lock`, for separate client PROCESSES — the real case, one `bleep` invocation each (verified: of 30 concurrent
    //     processes exactly one acquires). It releases automatically on channel close or process death, so there is no stale-lock timeout and no delete race.
    // Whoever holds both is the spawner; everyone else waits for that daemon via the connect probe. libdaemonjvm's lock remains the correctness backstop.
    val jvmPermit = jvmSpawnLocks.computeIfAbsent(socketDir.toAbsolutePath, _ => new java.util.concurrent.Semaphore(1))
    val spawnLockPath = socketDir.resolve(".spawn-lock")

    val spawnRole: Resource[IO, Boolean] =
      Resource
        .make(IO.blocking {
          if (!jvmPermit.tryAcquire()) (Option.empty[java.nio.channels.FileChannel], false) // another fiber/thread here is spawning
          else {
            Files.createDirectories(socketDir)
            val ch = java.nio.channels.FileChannel.open(spawnLockPath, java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE)
            val gotFile =
              try ch.tryLock() != null // null → another process holds it
              catch { case _: java.nio.channels.OverlappingFileLockException => false }
            if (gotFile) (Some(ch), true)
            else {
              try ch.close()
              catch { case _: Throwable => () }
              jvmPermit.release() // a different process is spawning; give the permit back
              (Option.empty[java.nio.channels.FileChannel], false)
            }
          }
        }) { case (chOpt, amSpawner) =>
          IO.blocking {
            chOpt.foreach(ch =>
              try ch.close()
              catch { case _: Throwable => () }
            )
            if (amSpawner) jvmPermit.release()
          }
        }
        .map(_._2)

    val spawnAndWait: IO[Unit] =
      spawnRole.use { amSpawner =>
        val ensureUp =
          if (amSpawner)
            // We hold both locks. Re-check — a previous spawner's daemon may have bound between our check and here.
            BspServerOperations.check(config.address).flatMap {
              case true  => IO.unit
              case false => spawn
            }
          else IO.unit // someone else is spawning; just wait for their daemon
        ensureUp >> BspServerOperations.waitForServer(config)
      } >> IO(logger.info(s"BSP server ready (server log: ${getOutputFile(config)})"))

    IO(logger.info(s"Ensuring BSP server (mode=$mode, socket=$socketDir)")) >> {
      if (config.dieWithParent)
        // Ephemeral: its socket dir is unique (UUID) and must be a fresh server every time — never reuse, no swarm to elect from.
        BspServerOperations.forceKillAndCleanup(socketDir) >> spawn >> BspServerOperations.waitForServer(config)
      else
        BspServerOperations.check(config.address).flatMap {
          case true  => IO(logger.info("BSP server already running, reusing"))
          case false => spawnAndWait
        }
    }
  }

  /** If the server's `output` log records the VM's OOM termination line, it died (or is right now dying) of OutOfMemoryError. -XX:+DisplayVMOutputToStderr
    * routes that line into the log the moment the VM starts terminating, so the client connected when it happened can diagnose it — in CI there is no next run
    * to notice. The `output` file always belongs to the server generation the caller was dealing with; it must be read before cleanup/forceStop/startServer,
    * which delete or rotate it.
    */
  def oomCrashExplanation(config: BspRifleConfig): IO[Option[String]] = IO.blocking {
    val log = config.address.socketDir.resolve("output")
    if (BspServerOperations.containsOomMarker(log)) {
      val cap = config.javaOpts.filter(_.startsWith("-Xmx")).lastOption.getOrElse("<unset>")
      val suggestion = bleep.MachineResources.parseMemoryMb(cap) match {
        case Some(mb) =>
          val doubled = mb * 2
          if (doubled % 1024 == 0) s"${doubled / 1024}g" else s"${doubled}m"
        case None => "8g"
      }
      Some(
        s"BSP server died of OutOfMemoryError with heap capped at $cap (see $log)." +
          s" If this recurs, raise the cap with: bleep config compile-server max-memory $suggestion"
      )
    } else None
  }

  /** Start a new BSP server process.
    *
    * The server runs as a daemon and will continue running until explicitly stopped or when the JVM exits.
    *
    * @return
    *   The server process (for monitoring, not for direct interaction)
    */
  def startServer(config: BspRifleConfig, logger: Logger): IO[Process] =
    BspServerOperations.startServer(config, logger)

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
