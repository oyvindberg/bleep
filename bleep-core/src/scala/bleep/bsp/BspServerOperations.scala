package bleep.bsp

import cats.effect.IO
import ryddig.Logger

import java.net.{ConnectException, InetSocketAddress, Socket, StandardProtocolFamily}
import java.nio.channels.SocketChannel
import java.nio.file.{Files, Path, StandardCopyOption}
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

  /** The line HotSpot prints (via VM tty output) right before -XX:+ExitOnOutOfMemoryError kills the VM. -XX:+DisplayVMOutputToStderr routes it into the socket
    * dir's `output` log, making it the durable evidence that a server died of OOM — no heap dump required.
    */
  val OomMarker = "Terminating due to java.lang.OutOfMemoryError"

  def containsOomMarker(log: Path): Boolean =
    // lossy decode: the log interleaves subprocess output that need not be valid UTF-8, and a diagnosis helper must not throw over it
    Files.exists(log) && new String(Files.readAllBytes(log), java.nio.charset.StandardCharsets.UTF_8).contains(OomMarker)

  /** Heap dumps written by older bleep versions, which ran the server with -XX:+HeapDumpOnOutOfMemoryError. They land in the server's working directory (the
    * socket dir) as `java_pid<pid>.hprof`. Current servers don't write dumps; these are legacy litter to clean up.
    */
  def listHeapDumps(socketDir: Path): List[Path] =
    if (Files.isDirectory(socketDir)) {
      val stream = Files.list(socketDir)
      try
        stream
          .iterator()
          .asScala
          .filter { p =>
            val name = p.getFileName.toString
            name.startsWith("java_pid") && name.endsWith(".hprof")
          }
          .toList
      finally stream.close()
    } else Nil

  /** A hint to append to connect/startup failure messages when the server log shows an OutOfMemoryError death.
    *
    * By the time these failures surface, startServer has rotated the logs, so the crashed generation may be under `output` or any kept generation — check them
    * all and say which one carried the evidence.
    */
  def oomHint(socketDir: Path): String =
    ("output" :: (1 to OutputGenerationsKept).map(n => s"output.$n").toList).map(socketDir.resolve).find(containsOomMarker) match {
      case None      => ""
      case Some(log) =>
        s"""\nNote: $log contains "$OomMarker" — the server ran out of heap.""" +
          " Raise the cap with: bleep config compile-server max-memory <size>"
    }

  // ==========================================================================
  // Process Management
  // ==========================================================================

  /** Spawn a new BSP server process.
    *
    * The server is started as a daemon process with stdout/stderr redirected to an output file for later inspection.
    */
  def startServer(config: BspRifleConfig, logger: Logger): IO[Process] = IO
    .blocking {
      val socketDir = config.address.socketDir
      ensureDirectoryExists(socketDir)

      // Multi-GB heap dumps left behind by older bleep versions (whose servers ran with
      // -XX:+HeapDumpOnOutOfMemoryError) accumulate one per crash until the disk fills.
      // Current servers don't write dumps — OOM death is diagnosed from the output log
      // (OomMarker) — so anything found here is litter.
      listHeapDumps(socketDir).foreach { dump =>
        val sizeMb = Files.size(dump) / (1024L * 1024L)
        Files.delete(dump)
        logger.warn(s"Deleted legacy OutOfMemoryError heap dump $dump (${sizeMb}MB)")
      }

      val outputFile = socketDir.resolve("output")
      rotateOutput(socketDir)
      pruneStaleSocketDirs(socketDir, logger)

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
      // Default ANSI-off for the BSP daemon JVM. With NO_COLOR=1 in the daemon's own env, every subprocess it ever spawns inherits the var via ProcessBuilder's
      // default env-copy behaviour — covers the test runner JvmPool, KSP runner, Kotlin/Scala JS/Native linkers, native-image, ad-hoc `git status` calls in
      // `ProjectDigest` (via scala.sys.process), and everything else without per-site plumbing. Use `putIfAbsent` so a developer who sets `NO_COLOR=` empty in
      // their shell or wants color in some specific case still wins.
      pb.environment().putIfAbsent("NO_COLOR", "1"): Unit
      // Whatever bleep-specific variables the SPAWNING client happened to have, the daemon must not
      // silently inherit. `pb.environment()` starts as a copy of this process's environment, so a
      // `BLEEP_FOO=… bleep compile` becomes permanent state on a server that then serves every other
      // client and every fork it starts — the first invocation of the day quietly configures the
      // machine. Observed exactly that: one `BLEEP_PARALLELISM=3 bleep compile` pinned the value into
      // a shared daemon, and unrelated test forks inherited it minutes later.
      //
      // Anything the daemon should know is passed deliberately — on the command line, or in the
      // config it reads for itself. Non-bleep variables (PATH, HOME, JAVA_HOME, proxy settings) are
      // left alone: those describe the machine, which the daemon genuinely shares.
      pb.environment().keySet().removeIf(k => k.startsWith("BLEEP_") && k != "BLEEP_BSP_DEBUG")
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

  /** Wait for the server to become ready by connecting to its socket.
    *
    * Readiness is a successful connect (opened, then immediately closed), NOT a substring in the `output` log. The previous log-grep approach — wait until
    * `output` contains "listening" — bricked healthy daemons: [[startServer]] rotates `output` to `output.prev` on every start attempt, so when a second
    * client's daemon-start attempt loses the lock race and exits, it has already moved the live daemon's "listening" line out of `output`. From then on the
    * grep never matched and every client timed out with "failed to start" against a daemon that was in fact accepting connections. A connect probe reflects
    * what the client actually needs — can I reach the server — and cannot be fooled by log rotation.
    *
    * The old comment warned that connecting "causes the server to wait for JSON-RPC messages". It does not: the daemon handles a connect-then-close as an
    * immediate EOF on a short-lived per-connection thread (bounded further by its read timeout). Polling stops on the first success, so this is at most a
    * couple of throwaway connects during startup.
    */
  def waitForServer(config: BspRifleConfig): IO[Unit] = {
    val socketFile = config.address match {
      case BspRifleConfig.Address.DomainSocket(path) => path
      case BspRifleConfig.Address.Tcp(_, _, dir)     => dir.resolve("socket")
    }
    val outputFile = config.address.socketDir.resolve("output")

    def isReady: IO[Boolean] =
      openConnection(config.address).attempt.flatMap {
        case Right(conn) => IO.blocking(conn.close()).attempt.as(true)
        case Left(_)     => IO.pure(false)
      }

    def poll(deadline: Long): IO[Unit] =
      isReady.flatMap {
        case true  => IO.unit
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
                  s"BSP server failed to start within ${config.startCheckTimeout}\n  $details${oomHint(config.address.socketDir)}"
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
                  s"Failed to connect to BSP server within ${config.connectionTimeout}: ${ex.getClass.getSimpleName}: ${ex.getMessage}${oomHint(config.address.socketDir)}"
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
      case None      => cleanup(socketDir)
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
      handle.destroy(): Unit // SIGTERM on Unix
    }
  }

  /** Force kill a process */
  def forceKill(pid: Long): IO[Unit] = IO.blocking {
    ProcessHandle.of(pid).ifPresent { h => h.destroyForcibly(); () }
  }

  /** Wait for a process to exit */
  def waitForProcessExit(pid: Long, timeout: FiniteDuration): IO[Unit] = {
    def poll(deadline: Long): IO[Unit] =
      IO.blocking(ProcessHandle.of(pid).isEmpty).flatMap {
        case true  => IO.unit
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

  /** Cleanup socket directory: remove the runtime state, KEEP the log.
    *
    * `output` used to be deleted here, which meant a crash destroyed its own evidence: the client's crash handler calls `forceStop` before restarting, so by
    * the time anyone read the log it was gone. Only the OOM marker survived, having been grepped out beforehand — so a crash that was NOT an OutOfMemoryError
    * (a hard VM exit, a killed process) left nothing at all to look at. Reported from the field exactly that way: "crashed twice ... no OutOfMemoryError
    * recorded", with the log already replaced by the next boot's.
    *
    * The log is rotated at the next spawn instead, which is where the generations are bounded.
    */
  def cleanup(socketDir: Path): IO[Unit] = IO.blocking {
    val filesToDelete = Seq("lock", "pid", "socket")
    filesToDelete.foreach { name =>
      try Files.deleteIfExists(socketDir.resolve(name))
      catch { case _: Exception => () }
    }
  }

  /** How many previous server logs to keep beside the live one.
    *
    * Two, not one. The client retries a crashed server once before giving up, so a systematic crash produces `crash -> restart -> crash` and TWO rotations in
    * seconds. With a single `output.prev` the second rotation overwrites the first crash's log with the second boot's — and the second boot is the short,
    * uninformative one. Two generations mean the original crash survives the retry that was supposed to help diagnose it.
    *
    * Not more than two, because these accumulate per socket directory and every bleep version mints a new one (the version is part of the JVM key).
    */
  val OutputGenerationsKept: Int = 2

  /** Total bytes of retained logs allowed in one socket directory.
    *
    * Bounds the generations, not the live file: a running server holds `output` open through an OS-level redirect, so nothing outside it can rotate or truncate
    * that file mid-boot. What CAN be bounded is history, and that is what accumulates — a day of ordinary use is under a megabyte at the default log level, but
    * a daemon run with `BLEEP_BSP_DEBUG` traces every event and grows without limit.
    *
    * When the kept generations exceed this, the oldest are dropped until they fit. So the guarantee is "a socket directory costs at most this much history",
    * which is the number that matters when there is one directory per bleep version per JVM.
    */
  val MaxRetainedLogBytes: Long = 32L * 1024 * 1024

  /** A socket directory whose daemon is gone and which nothing has touched for this long is litter.
    *
    * They accumulate silently: the JVM key includes the bleep version, so every snapshot install mints a fresh directory and abandons the previous one, each
    * holding a log, its generations, and up to 200MB of metrics. Two weeks is well past the point where an old version's logs are of any use, and far enough
    * out that a machine used intermittently never loses a directory it was still going to use.
    */
  val StaleSocketDirMaxAgeDays: Long = 14

  /** Shift `output` -> `output.1` -> `output.2`, dropping what falls off the end.
    *
    * Called at spawn, when nothing holds the file open.
    */
  private def rotateOutput(socketDir: Path): Unit =
    try {
      val live = socketDir.resolve("output")
      // Nothing to rotate on the very first boot, and nothing gained by shifting an empty file.
      if (Files.exists(live) && Files.size(live) == 0L) Files.deleteIfExists(live): Unit
      // Retire the oldest first, then shift each generation down, so no rename overwrites a file we
      // still want. Going in reverse is what makes this safe without temporary names.
      Files.deleteIfExists(socketDir.resolve(s"output.$OutputGenerationsKept")): Unit
      (OutputGenerationsKept - 1 to 1 by -1).foreach { n =>
        val from = socketDir.resolve(s"output.$n")
        if (Files.exists(from)) Files.move(from, socketDir.resolve(s"output.${n + 1}"), StandardCopyOption.REPLACE_EXISTING): Unit
      }
      if (Files.exists(live)) Files.move(live, socketDir.resolve("output.1"), StandardCopyOption.REPLACE_EXISTING): Unit
      // Legacy name from when there was exactly one generation; drop it so the directory does not
      // keep a file nothing writes or reads any more.
      Files.deleteIfExists(socketDir.resolve("output.prev")): Unit
      enforceLogBudget(socketDir)
    } catch { case _: Exception => () }

  /** Drop the oldest generations until the retained logs fit [[MaxRetainedLogBytes]]. */
  private def enforceLogBudget(socketDir: Path): Unit = {
    val generations = (1 to OutputGenerationsKept).map(n => socketDir.resolve(s"output.$n")).filter(Files.exists(_))
    var total = generations.map(Files.size).sum
    // Oldest first — the highest generation number is the furthest back.
    generations.reverse.foreach { p =>
      if (total > MaxRetainedLogBytes) {
        total -= Files.size(p)
        Files.deleteIfExists(p): Unit
      }
    }
  }

  /** Delete sibling socket directories whose daemon is gone and which nothing has touched recently.
    *
    * Run at spawn, because that is when a new directory is being added and the previous version's is being orphaned — the moment the tail grows is the right
    * moment to trim it. A directory is only removed when its recorded pid is dead (or absent) AND every file in it is older than the cutoff, so a daemon that
    * is merely idle, or one being started concurrently, is never touched.
    */
  private def pruneStaleSocketDirs(currentSocketDir: Path, logger: Logger): Unit =
    try {
      val parent = currentSocketDir.getParent
      val cutoff = System.currentTimeMillis() - StaleSocketDirMaxAgeDays * 24L * 60L * 60L * 1000L
      val stream = Files.list(parent)
      val siblings =
        try stream.iterator().asScala.filter(Files.isDirectory(_)).filterNot(_.equals(currentSocketDir)).toList
        finally stream.close()

      siblings.foreach { dir =>
        val alive =
          try Files.exists(dir.resolve("pid")) && ProcessHandle.of(Files.readString(dir.resolve("pid")).trim.toLong).isPresent
          catch { case _: Exception => false }
        if (!alive) {
          val newest =
            try {
              val s2 = Files.list(dir)
              try s2.iterator().asScala.map(Files.getLastModifiedTime(_).toMillis).maxOption.getOrElse(0L)
              finally s2.close()
            } catch { case _: Exception => Long.MaxValue } // unreadable: leave it alone
          if (newest < cutoff) {
            val freed = directorySizeBytes(dir)
            deleteRecursively(dir)
            logger.info(s"Removed stale compile-server directory $dir (no live server, untouched for ${StaleSocketDirMaxAgeDays}+ days, ${freed / 1024}KB)")
          }
        }
      }
    } catch { case _: Exception => () }

  private def directorySizeBytes(dir: Path): Long =
    try {
      val stream = Files.walk(dir)
      try stream.iterator().asScala.filter(Files.isRegularFile(_)).map(Files.size).sum
      finally stream.close()
    } catch { case _: Exception => 0L }

  private def deleteRecursively(dir: Path): Unit =
    try {
      val stream = Files.walk(dir)
      val paths =
        try stream.iterator().asScala.toList
        finally stream.close()
      paths.reverse.foreach(p => Files.deleteIfExists(p): Unit)
    } catch { case _: Exception => () }

  /** Ensure a directory exists with proper permissions */
  private def ensureDirectoryExists(dir: Path): Unit =
    if (!Files.exists(dir)) {
      Files.createDirectories(dir)
      // Set restrictive permissions on Unix
      try
        Files.setPosixFilePermissions(dir, PosixFilePermissions.fromString("rwx------")): Unit
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
      case true  =>
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
      case None      => cleanup(socketDir)
      case Some(pid) =>
        forceKill(pid) >>
          // Wait for the process to actually exit (up to 5 seconds) instead of a blind sleep.
          // This prevents race conditions where the old process still holds file handles.
          waitForProcessExit(pid, 5.seconds).handleErrorWith { _ =>
            // If it still hasn't exited after 5s, try killing descendants too
            IO.blocking {
              ProcessHandle.of(pid).ifPresent { handle =>
                handle.descendants().forEach { h => h.destroyForcibly(); () }
                handle.destroyForcibly(): Unit
              }
            } >> IO.sleep(500.millis)
          } >>
          cleanup(socketDir)
    }
  }
}
