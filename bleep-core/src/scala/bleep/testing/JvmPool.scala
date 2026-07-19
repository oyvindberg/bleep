package bleep.testing

import bleep.MachineResources
import cats.effect._
import cats.effect.std.{Queue, Semaphore}
import fs2.Stream

import java.io._
import java.nio.file.Path
import java.security.MessageDigest
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.util.Properties
import scala.util.control.NonFatal

/** A pool of reusable JVM processes for running tests.
  *
  * JVMs are expensive to start, so we pool them by classpath hash. When a test needs a JVM with a particular classpath, we either return an existing idle JVM
  * or spawn a new one.
  *
  * Key features:
  *   - JVMs keyed by classpath + options hash for reuse
  *   - Bounded concurrency via semaphore
  *   - Explicit shutdown (no shutdown hooks)
  *   - Health checks before reuse
  */
trait JvmPool {

  /** Acquire a JVM suitable for the given classpath and options.
    *
    * Returns a Resource that will release the JVM back to the pool when done.
    */
  def acquire(
      label: String,
      classpath: List[Path],
      jvmOptions: List[String],
      runnerClass: String,
      environment: Map[String, String],
      workingDirectory: Option[Path]
  ): Resource[IO, TestJvm]

  /** Shutdown all JVMs in the pool.
    *
    * This MUST be called when done with the pool. Use guarantee to ensure it runs.
    */
  def shutdown: IO[Unit]

  /** Number of JVMs currently in the pool */
  def size: IO[Int]
}

/** A handle to a forked JVM running the test runner */
trait TestJvm {

  /** Process ID of this JVM */
  def pid: Long

  /** Run a test suite and stream back responses */
  def runSuite(
      className: String,
      framework: String,
      args: List[String]
  ): Stream[IO, TestProtocol.TestResponse]

  /** Get a thread dump from the JVM */
  def getThreadDump: IO[Option[TestProtocol.TestResponse.ThreadDump]]

  /** Get a thread dump of the child JVM as a list of lines. Spawns `jstack <pid>` from the same JDK as `jvmCommand`; jstack writes its output to its own
    * stdout, so the dump stream is clean and decoupled from the test JVM's stdio (which is otherwise busy with the JSON-RPC protocol). Returns Nil if jstack
    * isn't available, the child has already died, or the call times out. Best-effort — never throws.
    *
    * Useful right before a forced kill on suite-idle timeout: surfaces *what* the test was stuck on instead of the user just seeing "timed out, no output".
    */
  def dumpThreads: IO[List[String]]

  /** Read any available stderr lines (non-blocking) */
  def drainStderr: IO[List[String]]

  /** Check if the JVM process is still alive */
  def isAlive: IO[Boolean]

  /** Kill the JVM process immediately */
  def kill: IO[Unit]
}

object JvmPool {

  /** Create a new JVM pool with the given maximum concurrency.
    *
    * IMPORTANT: The returned pool has an explicit shutdown method that MUST be called when done. Use guarantee to ensure cleanup: {{{
    * JvmPool.create(maxConcurrency, jvmCommand, workingDirectory).use { pool => // pool.shutdown is called automatically when this scope ends runTests(pool) }
    * }}}
    *
    * No shutdown hooks are used - caller is responsible for ensuring shutdown is called.
    *
    * @param maxConcurrency
    *   Maximum number of JVMs to run concurrently
    * @param jvmCommand
    *   Path to the java binary (e.g., started.jvmCommand)
    */
  def create(
      maxConcurrency: Int,
      jvmCommand: Path,
      workingDirectory: Path,
      machine: MachineResources
  ): Resource[IO, JvmPool] =
    Resource.make(
      for {
        semaphore <- Semaphore[IO](maxConcurrency.toLong)
        pool <- IO(new TrieMap[JvmKey, Queue[IO, ManagedJvm]]())
        allJvms <- Ref.of[IO, Set[ManagedJvm]](Set.empty)
      } yield new JvmPoolImpl(semaphore, machine, pool, allJvms, new TrieMap[JvmKey, Int](), jvmCommand, workingDirectory)
    )(_.shutdown)

  /** Parse a `-Xmx` value (e.g. `-Xmx2g`, `-Xmx512m`) from JVM options into MB. Last one wins (JVM semantics). None if no `-Xmx` is present.
    */
  private[testing] def parseXmxMb(jvmOptions: List[String]): Option[Long] =
    jvmOptions.reverse.collectFirst { case o if o.startsWith("-Xmx") => o }.flatMap(MachineResources.parseMemoryMb)

  private[testing] case class ExitDescription(summary: String, detail: Option[String])

  /** Describe how a forked JVM died, for the message the user actually reads.
    *
    * On Unix a process terminated by a signal reports `128 + signal`, so 137 is SIGKILL and 139 SIGSEGV. SIGKILL matters most here: the JVM cannot log anything
    * about its own SIGKILL, so without this the death is indistinguishable from a clean EOF — and SIGKILL on a big test fork almost always means the kernel
    * reclaiming memory, which is exactly the hypothesis a user needs handed to them.
    *
    * Best-effort and non-blocking beyond a short grace period: EOF on stdout usually precedes the process table catching up by a few milliseconds.
    */
  private[testing] def describeExit(process: Process): ExitDescription = {
    val exited = process.waitFor(2, java.util.concurrent.TimeUnit.SECONDS)
    if (!exited) ExitDescription("EOF on stdout, process still alive", Some("The JVM closed stdout but has not exited — it may be wedged rather than dead."))
    else
      process.exitValue() match {
        case 0   => ExitDescription("EOF on stdout, exited 0", Some("The JVM exited cleanly without sending a suite result — it likely called System.exit()."))
        case 137 =>
          ExitDescription(
            "killed by SIGKILL (exit 137)",
            Some(
              "SIGKILL is sent by the OS, not by the JVM or by bleep — on a test fork this is almost always the kernel reclaiming memory under pressure. " +
                "Lower this project's -Xmx, or lower the machine's fork-memory budget (BLEEP_FORK_MEMORY_BUDGET_MB) so fewer forks run at once."
            )
          )
        case 139 => ExitDescription("killed by SIGSEGV (exit 139)", Some("The JVM crashed; look for an hs_err_pid*.log next to the working directory."))
        case code if code > 128 => ExitDescription(s"killed by signal ${code - 128} (exit $code)", None)
        case code               => ExitDescription(s"exited with code $code", None)
      }
  }

  /** Key for pooling JVMs */
  private case class JvmKey(classpathHash: String, optionsHash: String, envHash: String, cwdHash: String)

  private object JvmKey {
    def apply(classpath: List[Path], options: List[String], environment: Map[String, String], cwd: Option[Path]): JvmKey = {
      val cpHash = hashStrings(classpath.map(_.toString))
      val optHash = hashStrings(options)
      val envHash = hashStrings(environment.toList.sorted.map { case (k, v) => s"$k=$v" })
      val cwdHash = hashStrings(cwd.map(_.toString).toList)
      JvmKey(cpHash, optHash, envHash, cwdHash)
    }

    private def hashStrings(strings: List[String]): String = {
      val md = MessageDigest.getInstance("SHA-256")
      strings.foreach(s => md.update(s.getBytes("UTF-8")))
      md.digest().take(8).map("%02x".format(_)).mkString
    }
  }

  /** Internal managed JVM wrapper.
    *
    * A daemon thread continuously drains the child's stderr into [[stderrBuffer]] so the OS pipe never blocks the child. Without this, a chatty JVM (e.g. JDK
    * 25 emitting `sun.misc.Unsafe` deprecation warnings on a heavy classpath) fills the 64KB pipe buffer, the child blocks on its next stderr write, and the
    * parent's [[stdout]]-driven protocol loop hangs forever — no test events, no progress, idle timeout fires with zero diagnostic output.
    */
  private class ManagedJvm(
      val process: Process,
      val stdin: PrintWriter,
      val stdout: BufferedReader,
      val stderr: BufferedReader,
      val key: JvmKey,
      val jvmCommand: Path
  ) {
    @volatile private var alive = true
    @volatile private var _protocolClean = true
    @volatile private var _suiteInFlight = false

    /** Buffered stderr lines collected by the drain thread. Bounded so a runaway warning storm can't OOM the parent. Oldest lines are dropped past the cap. */
    private val stderrBuffer = new java.util.concurrent.ConcurrentLinkedDeque[String]()
    private val stderrBufferCap = 2048

    locally {
      val t = new Thread(s"jvm-stderr-drain-${process.pid}") {
        override def run(): Unit =
          try {
            var line = stderr.readLine()
            while (line != null) {
              stderrBuffer.addLast(line)
              while (stderrBuffer.size > stderrBufferCap) stderrBuffer.pollFirst(): Unit
              line = stderr.readLine()
            }
          } catch { case NonFatal(_) => () }
      }
      t.setDaemon(true)
      t.start()
    }

    def isAlive: Boolean =
      alive && process.isAlive

    def protocolClean: Boolean = _protocolClean

    def markProtocolDirty(): Unit =
      _protocolClean = false

    /** True between sending a RunSuite command and consuming that suite's terminal response. While set, the child's stdout may still hold unread
      * TestFinished/SuiteDone lines from the in-flight suite, so the JVM is NOT safe to hand to another acquirer — the next RunSuite would read this suite's
      * leftover terminator and misattribute its counts. A suite that ends by cancellation (fiber killed mid-run) leaves this set precisely so [[release]] kills
      * the JVM instead of re-pooling it.
      */
    def suiteInFlight: Boolean = _suiteInFlight

    def markSuiteStarted(): Unit =
      _suiteInFlight = true

    def markSuiteFinished(): Unit =
      _suiteInFlight = false

    def markDead(): Unit =
      alive = false

    /** Get a thread dump of the child JVM. Spawns `<jvmCommand-dir>/jstack <pid>` and captures its stdout — independent of the child's own stdio, so the dump
      * doesn't collide with the child's JSON-RPC protocol stream. Returns Nil if jstack isn't on disk, the child has died, or the call times out within 10s.
      * Best-effort everywhere — never throws.
      */
    def dumpThreads(): List[String] = {
      if (!process.isAlive) return Nil
      val jstackBin = {
        val name = if (Properties.isWin) "jstack.exe" else "jstack"
        jvmCommand.getParent.resolve(name)
      }
      if (!java.nio.file.Files.isExecutable(jstackBin)) return Nil
      try {
        val pid = process.pid()
        val pb = new ProcessBuilder(jstackBin.toString, pid.toString)
        pb.redirectErrorStream(true)
        val p = pb.start()
        // jstack prints to stdout; capture it line-by-line.
        val reader = new BufferedReader(new InputStreamReader(p.getInputStream))
        val buffer = scala.collection.mutable.ListBuffer.empty[String]
        val drainer = new Thread(s"jstack-drain-${process.pid}") {
          override def run(): Unit =
            try {
              var line = reader.readLine()
              while (line != null) {
                buffer.synchronized(buffer += line): Unit
                line = reader.readLine()
              }
            } catch { case NonFatal(_) => () }
        }
        drainer.setDaemon(true)
        drainer.start()
        val finished = p.waitFor(10, java.util.concurrent.TimeUnit.SECONDS)
        if (!finished) p.destroyForcibly(): Unit
        drainer.join(1000)
        buffer.synchronized(buffer.toList)
      } catch { case NonFatal(_) => Nil }
    }

    def kill(): Unit = {
      alive = false
      try
        stdin.close()
      catch { case NonFatal(_) => }
      // Kill the entire process tree, not just the direct child.
      // If the test runner spawned sub-processes (e.g., for some test frameworks),
      // those would otherwise be orphaned and consume system resources.
      try
        process
          .descendants()
          .forEach(ph =>
            try ph.destroyForcibly(): Unit
            catch { case _: Exception => () }
          )
      catch { case NonFatal(_) => }
      process.destroyForcibly()
      try
        process.waitFor(5, java.util.concurrent.TimeUnit.SECONDS): Unit
      catch { case NonFatal(_) => }
    }

    /** Snapshot stderr lines accumulated since last call. Drains the buffer. */
    def readStderr(): String = {
      val sb = new StringBuilder
      var line = stderrBuffer.pollFirst()
      while (line != null) {
        sb.append(line).append("\n"): Unit
        line = stderrBuffer.pollFirst()
      }
      sb.toString()
    }
  }

  /** Max consecutive spawn failures per key before refusing to spawn. Prevents infinite retry when test runner jar is incompatible. */
  private val MaxSpawnFailures = 3

  private class JvmPoolImpl(
      semaphore: Semaphore[IO],
      machine: MachineResources,
      pool: TrieMap[JvmKey, Queue[IO, ManagedJvm]],
      allJvms: Ref[IO, Set[ManagedJvm]],
      spawnFailures: TrieMap[JvmKey, Int],
      jvmCommand: Path,
      workingDirectory: Path
  ) extends JvmPool {

    override def acquire(
        label: String,
        classpath: List[Path],
        jvmOptions: List[String],
        runnerClass: String,
        environment: Map[String, String],
        workingDirectory: Option[Path]
    ): Resource[IO, TestJvm] = {
      val key = JvmKey(classpath, jvmOptions, environment, workingDirectory)

      // Weight this fork by what it actually costs the machine — its -Xmx PLUS the non-heap
      // footprint every JVM carries (metaspace, code cache, thread stacks, direct buffers, GC
      // bookkeeping) — so the sum of concurrent forks stays within the fork-memory budget. Charging
      // bare -Xmx is what let two 12GB forks be admitted against a 36GB budget while really costing
      // ~30GB resident. An uncapped fork falls back to the machine's default fork weight, which is
      // already an estimate of a whole process rather than a heap.
      val forkMemoryMb = parseXmxMb(jvmOptions).map(MachineResources.forkFootprintMb).getOrElse(machine.defaultForkMemoryMb)
      val machineReservation =
        machine.reserve(MachineResources.ResourceKind.TestFork, s"test $label", cpu = 1, memoryMb = forkMemoryMb)

      // Order matters: the per-pool semaphore (a local counter bounding THIS run's parallelism) is
      // taken FIRST, the machine reservation (shared with every other client) LAST — immediately
      // before the JVM is created and released immediately after. Reserving first would hold a core
      // and a fork's worth of the machine's memory budget while merely queueing for our own run's
      // parallelism token, i.e. capacity withheld from other clients for work not yet running.
      Resource.make(semaphore.acquire)(_ => semaphore.release).flatMap { _ =>
        machineReservation.flatMap { _ =>
          Resource
            .make(getOrCreate(key, classpath, jvmOptions, runnerClass, environment, workingDirectory).map(jvm => (jvm, new TestJvmImpl(jvm): TestJvm))) {
              // Return JVM to pool; the semaphore + machine reservation are released by their own Resources.
              case (jvm, _) => release(jvm)
            }
            .map(_._2)
        }
      }
    }

    private def getOrCreate(
        key: JvmKey,
        classpath: List[Path],
        jvmOptions: List[String],
        runnerClass: String,
        environment: Map[String, String],
        cwd: Option[Path]
    ): IO[ManagedJvm] =
      for {
        queue <- IO(
          pool.getOrElseUpdate(
            key, {
              // Create queue synchronously to avoid race
              import cats.effect.unsafe.implicits.global
              Queue.unbounded[IO, ManagedJvm].unsafeRunSync()
            }
          )
        )
        maybeJvm <- queue.tryTake
        jvm <- maybeJvm match {
          case Some(existing) if existing.isAlive =>
            IO.pure(existing)
          case Some(dead) =>
            // JVM died, remove from tracking and create new
            allJvms.update(_ - dead) >> spawnJvm(key, classpath, jvmOptions, runnerClass, environment, cwd)
          case None =>
            spawnJvm(key, classpath, jvmOptions, runnerClass, environment, cwd)
        }
      } yield jvm

    private def spawnJvm(
        key: JvmKey,
        classpath: List[Path],
        jvmOptions: List[String],
        runnerClass: String,
        environment: Map[String, String],
        cwdOverride: Option[Path]
    ): IO[ManagedJvm] = {
      val failures = spawnFailures.getOrElse(key, 0)
      if (failures >= MaxSpawnFailures) {
        return IO.raiseError(
          new IOException(
            s"Test JVM failed to start $failures consecutive times. This usually means the test runner jar " +
              s"is incompatible with the project's JVM. Check that bleep-test-runner is published for the correct Java version."
          )
        )
      }
      IO
        .blocking {
          val javaPath = jvmCommand
          val cpString = classpath.map(_.toString).mkString(File.pathSeparator)

          // On Windows, command-line length is limited to 32,767 characters.
          // When the classpath is too long, pass it via CLASSPATH environment variable instead.
          val useEnvClasspath = scala.util.Properties.isWin && cpString.length > 30000

          val cmd =
            if (useEnvClasspath)
              List(javaPath.toString) ++ jvmOptions ++ List(runnerClass)
            else
              List(javaPath.toString) ++ jvmOptions ++ List("-cp", cpString, runnerClass)

          val pb = new ProcessBuilder(cmd*)
          pb.directory(cwdOverride.getOrElse(workingDirectory).toFile)
          pb.redirectErrorStream(false)
          if (useEnvClasspath) {
            pb.environment().put("CLASSPATH", cpString): Unit
          }
          // Default ANSI-off (no-color.org standard, honored by ScalaTest / JUnit / kotlinc / native-image / most JVM tooling). Set with putIfAbsent so any
          // explicit caller override — including the parent JVM's inherited NO_COLOR — still wins.
          pb.environment().putIfAbsent("NO_COLOR", "1"): Unit
          environment.foreach { case (k, v) => pb.environment().put(k, v) }

          val process = pb.start()
          val stdin = new PrintWriter(new BufferedOutputStream(process.getOutputStream), true)
          val stdout = new BufferedReader(new InputStreamReader(process.getInputStream))
          val stderr = new BufferedReader(new InputStreamReader(process.getErrorStream))

          new ManagedJvm(process, stdin, stdout, stderr, key, jvmCommand)
        }
        .flatTap(jvm => allJvms.update(_ + jvm))
        .flatTap(jvm =>
          waitForReady(jvm).onError { case _ =>
            IO(spawnFailures.updateWith(jvm.key) { case Some(n) => Some(n + 1); case None => Some(1) }).void
          }
        )
        .flatTap(jvm => IO(spawnFailures.remove(jvm.key))) // Reset on success
    }

    private def waitForReady(jvm: ManagedJvm): IO[Unit] =
      IO.interruptible {
        val line = jvm.stdout.readLine()
        if (line == null) {
          // Process terminated before the Ready handshake. Capture the exit code and pid — with no
          // stderr (a child SIGKILLed before writing leaves it empty) these are the only signal.
          // Exit 137 = SIGKILL (OOM killer / fork storm), 1 = JVM startup failure, etc. Without them
          // the failure is just "terminated before Ready (no stderr output)" — nothing to debug.
          Thread.sleep(100) // Give stderr a moment to be available
          val stderrOutput = jvm.readStderr()
          val pid = jvm.process.pid()
          val exited = jvm.process.waitFor(2, java.util.concurrent.TimeUnit.SECONDS)
          val exitStr = if (exited) s"exit code ${jvm.process.exitValue()}" else "still alive after 2s (no exit)"
          val stderrPart = if (stderrOutput.nonEmpty) s" Stderr:\n$stderrOutput" else " (no stderr output)"
          throw new IOException(s"JVM process (pid=$pid) terminated before sending Ready — $exitStr.$stderrPart")
        }
        TestProtocol.decodeResponse(line) match {
          case Right(TestProtocol.TestResponse.Ready) => ()
          case Right(other)                           =>
            throw new IOException(s"Expected Ready, got: $other")
          case Left(err) =>
            throw new IOException(s"Failed to decode response: $err, line: $line")
        }
      }.timeout(30.seconds)
        .onError { case _ => IO(jvm.kill()) }

    override def shutdown: IO[Unit] =
      // CRITICAL: Use uncancelable to ensure cleanup completes even during cancellation
      IO.uncancelable { _ =>
        for {
          jvms <- allJvms.get
          _ <- IO.blocking {
            jvms.foreach { jvm =>
              try {
                // Send shutdown command
                jvm.stdin.println(TestProtocol.encodeCommand(TestProtocol.TestCommand.Shutdown))
                jvm.stdin.flush()
              } catch { case NonFatal(_) => }
            }
          }
          // Give them a moment to shutdown gracefully
          _ <- IO.sleep(500.millis)
          _ <- IO.blocking {
            jvms.foreach(_.kill())
          }
          _ <- allJvms.set(Set.empty)
        } yield ()
      }

    override def size: IO[Int] =
      allJvms.get.map(_.size)

    /** Return a JVM to the pool for reuse */
    private def release(jvm: ManagedJvm): IO[Unit] =
      if (jvm.isAlive && jvm.protocolClean && !jvm.suiteInFlight) {
        for {
          queue <- IO(
            pool.getOrElseUpdate(
              jvm.key, {
                import cats.effect.unsafe.implicits.global
                Queue.unbounded[IO, ManagedJvm].unsafeRunSync()
              }
            )
          )
          _ <- queue.offer(jvm)
        } yield ()
      } else {
        // Dead or protocol-dirty JVM — kill and remove from tracking
        IO(jvm.kill()).attempt >> allJvms.update(_ - jvm)
      }

    private class TestJvmImpl(jvm: ManagedJvm) extends TestJvm {

      override def pid: Long = jvm.process.pid()

      override def runSuite(
          className: String,
          framework: String,
          args: List[String]
      ): Stream[IO, TestProtocol.TestResponse] = {
        val command = TestProtocol.TestCommand.RunSuite(className, framework, args)

        val body =
          Stream.eval(IO(jvm.markSuiteStarted()) >> sendCommand(command)) >>
            readResponses.takeThrough {
              case _: TestProtocol.TestResponse.SuiteDone => false
              case _: TestProtocol.TestResponse.Error     => false
              case _                                      => true
            }

        // Clear the in-flight flag only when the stream drains to its terminator
        // (SuiteDone/Error consumed) — then the protocol is at a clean boundary and the JVM
        // is safe to re-pool. On cancellation the flag stays set, so `release` kills the JVM
        // rather than handing a mid-suite protocol stream to the next acquirer.
        body.onFinalizeCase {
          case Resource.ExitCase.Succeeded => IO(jvm.markSuiteFinished())
          case _                           => IO.unit
        }
      }

      private def sendCommand(cmd: TestProtocol.TestCommand): IO[Unit] =
        IO.blocking {
          jvm.stdin.println(TestProtocol.encodeCommand(cmd))
          jvm.stdin.flush()
        }

      private def readResponses: Stream[IO, TestProtocol.TestResponse] =
        Stream.repeatEval {
          // Daemon stderr-drain thread on ManagedJvm pulls stderr off the OS pipe continuously into a bounded buffer, so we don't need to interleave drains
          // here. Just block on stdout.
          IO.interruptible {
            val line = jvm.stdout.readLine()
            if (line == null) {
              // EOF on stdout mid-session = the forked JVM died unexpectedly. Mark it dead so the pool drops it, then emit a structured `Error` response
              // (the stream's `takeThrough` upstream treats Error as a terminator). The caller's processResponses sees the Error and routes it to
              // `SuiteError`, not the silent `SuiteFinished(0,0,0,0,...)` path. Previously this returned `None` + `unNoneTerminate` — silent zero-count finish.
              jvm.markDead()
              val pid = jvm.process.pid()
              val stderrTail = jvm.readStderr()
              // Reap it and say HOW it died. "EOF on stdout" alone is undiagnosable — it looks the
              // same whether the JVM exited, crashed, or was killed by the OS. The exit status
              // distinguishes them, and an externally-signalled death (128+signal, so 137 = SIGKILL)
              // is the fingerprint of the kernel reclaiming memory, which no in-process log can show.
              val exitDescription = JvmPool.describeExit(jvm.process)
              val details = List(exitDescription.detail, Option.when(stderrTail.nonEmpty)(s"stderr tail:\n$stderrTail")).flatten match {
                case Nil   => None
                case lines => Some(lines.mkString("\n"))
              }
              TestProtocol.TestResponse.Error(s"Forked test JVM (pid=$pid) died unexpectedly (${exitDescription.summary})", details)
            } else {
              TestProtocol.decodeResponse(line) match {
                case Right(response) => response
                case Left(err)       =>
                  jvm.markProtocolDirty()
                  TestProtocol.TestResponse.Error(s"Protocol error: ${err.getMessage}", Some(s"Line: $line"))
              }
            }
          }
        }

      override def getThreadDump: IO[Option[TestProtocol.TestResponse.ThreadDump]] =
        for {
          _ <- sendCommand(TestProtocol.TestCommand.GetThreadDump)
          response <- IO
            .interruptible {
              val line = jvm.stdout.readLine()
              if (line == null) {
                jvm.markDead()
                None
              } else {
                TestProtocol.decodeResponse(line) match {
                  case Right(td: TestProtocol.TestResponse.ThreadDump) => Some(td)
                  case _                                               => None
                }
              }
            }
            .timeout(5.seconds)
            .handleError(_ => None)
        } yield response

      override def drainStderr: IO[List[String]] =
        IO.blocking {
          val output = jvm.readStderr()
          if (output.isEmpty) Nil
          else output.split('\n').toList
        }

      override def dumpThreads: IO[List[String]] =
        IO.blocking(jvm.dumpThreads())

      override def isAlive: IO[Boolean] =
        IO(jvm.isAlive)

      override def kill: IO[Unit] =
        IO(jvm.kill())
    }
  }
}
