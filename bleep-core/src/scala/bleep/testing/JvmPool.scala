package bleep.testing

import bleep.{MachineResources, ProcessMemory}
import cats.effect._
import cats.effect.std.{Queue, Semaphore}
import cats.syntax.all._
import fs2.Stream

import java.io._
import java.net.{InetAddress, ServerSocket, Socket, SocketTimeoutException}
import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.security.MessageDigest
import java.util.concurrent.TimeUnit
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
    * `defaultHeapMb` is the heap this fork gets if `jvmOptions` states no `-Xmx` of its own — the caller's configured default, not a ceiling over what the
    * caller asked for. See [[MachineResources.withHeapBound]].
    *
    * Returns a Resource that will release the JVM back to the pool when done.
    */
  def acquire(
      label: String,
      classpath: List[Path],
      jvmOptions: List[String],
      defaultHeapMb: Long,
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

  /** Run a test suite and stream back responses. `selection` says *how* to run it, decided where the classpath is known; see [[FrameworkSelection]]. */
  def runSuite(
      className: String,
      selection: FrameworkSelection,
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

  /** How long a freshly spawned fork gets to connect back on the protocol socket.
    *
    * Generous, because it covers JVM startup on a cold, loaded CI runner, and bounded, because a fork that never connects would otherwise hang the suite
    * forever. What the timeout means depends on the fork's state when it fires, so the spawn failure reports that state rather than the bare timeout: a fork
    * that has already exited hit a startup failure it described on its own stderr, while a fork still running never intended to connect at all.
    */
  private val ProtocolConnectTimeout: FiniteDuration = 60.seconds

  /** Cap on how much of a failed fork's output is quoted back. Enough for a JVM startup error, which is the only thing a fork that never connected can have had
    * time to write, and short enough that a fork which died mid-flood does not bury the message reporting it.
    */
  private val MaxChildOutputBytes: Int = 4096

  /** How often the wait for a connect-back looks up to check whether the fork is still alive. Short enough that a fork dying on startup is reported at once,
    * long enough that the check costs nothing next to [[ProtocolConnectTimeout]].
    */
  private val ProtocolPollInterval: FiniteDuration = 250.millis

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
      machine: MachineResources,
      listener: JvmPoolListener
  ): Resource[IO, JvmPool] =
    Resource.make(
      for {
        semaphore <- Semaphore[IO](maxConcurrency.toLong)
        startLimiter <- Semaphore[IO](maxConcurrentStarts(maxConcurrency).toLong)
        pool <- IO(new TrieMap[JvmKey, Queue[IO, ManagedJvm]]())
        allJvms <- Ref.of[IO, Set[ManagedJvm]](Set.empty)
        // Learn what forks cost only where we can actually measure one; elsewhere keep charging the
        // declared bound, which is what the pool did before any of this existed.
        costs <-
          if (ProcessMemory.system eq ProcessMemory.Unavailable) IO.pure(ForkCostModel.static)
          else ForkCostModel.create
      } yield new JvmPoolImpl(
        listener,
        semaphore,
        startLimiter,
        machine,
        pool,
        allJvms,
        new TrieMap[JvmKey, Int](),
        jvmCommand,
        workingDirectory,
        costs,
        ProcessMemory.system
      )
    )(_.shutdown)

  /** How many forks may be in the middle of STARTING at any one time, as a function of how wide the run is allowed to go.
    *
    * Not a limit on how many run — that is the governor's job — but on how many may be between `spawn` and a healthy handshake. The two are different problems,
    * and this one is invisible to any budget: a JVM's memory arrives over the seconds AFTER it starts, as classes load, the classpath is paged in and the JIT
    * warms. A burst of spawns is therefore a burst of demand that no measurement has seen yet. Staggering starts buys the feedback loop time to see a fork's
    * real cost before the next admission is decided on it.
    *
    * A QUARTER of the run's parallelism, at least two. Deliberately proportional rather than a fixed number: a flat constant tuned on an 18-core machine would
    * throttle a 64-core one and over-commit a 4-core one. So a wide run staggers in wider batches, a narrow run barely staggers at all.
    */
  private[testing] def maxConcurrentStarts(maxConcurrency: Int): Int = math.max(2, maxConcurrency / 4)

  /** Parse a `-Xmx` value (e.g. `-Xmx2g`, `-Xmx512m`) from JVM options into MB. Last one wins (JVM semantics). None if no `-Xmx` is present.
    */
  private[testing] def parseXmxMb(jvmOptions: List[String]): Option[Long] =
    jvmOptions.reverse.collectFirst { case o if o.startsWith("-Xmx") => o }.flatMap(MachineResources.parseMemoryMb)

  private[testing] case class ExitDescription(summary: String, detail: Option[String])

  /** Describe how a forked JVM died, for the message the user actually reads.
    *
    * `killedByUs` is checked FIRST and it is the whole point. `destroyForcibly` sends SIGKILL, so a fork bleep terminated reports exit 137 identically to one
    * the kernel terminated — and an earlier version of this reported every 137 as "the kernel reclaiming memory under pressure". That was wrong for every kill
    * bleep issued itself (start-timeout, pool eviction, contention, cancellation, shutdown), and confidently wrong: it sent a long investigation into the
    * memory subsystem chasing failures bleep was causing. Verified afterwards against the OS's own log, which had recorded no memory kills at all during a run
    * where 35 forks "died of memory pressure".
    *
    * Only when nothing in bleep killed it is an external cause a sound conclusion, and even then it is offered as the likely explanation rather than asserted.
    */
  private[testing] def describeExit(process: Process, killedByUs: Option[String]): ExitDescription = {
    val exited = process.waitFor(2, java.util.concurrent.TimeUnit.SECONDS)
    killedByUs match {
      case Some(reason) =>
        ExitDescription(s"terminated by bleep ($reason)", Some("This was not the OS: bleep terminated this process itself, for the reason above."))
      case None =>
        if (!exited)
          ExitDescription("EOF on stdout, process still alive", Some("The JVM closed stdout but has not exited — it may be wedged rather than dead."))
        else
          process.exitValue() match {
            case 0 =>
              ExitDescription("EOF on stdout, exited 0", Some("The JVM exited cleanly without sending a suite result — it likely called System.exit()."))
            case 137 =>
              ExitDescription(
                "killed by SIGKILL (exit 137)",
                Some(
                  "Nothing that records a reason terminated this process. SIGKILL carries no attribution, so this is not proof the OS did it — bleep's own " +
                    "untracked kill paths look identical. Candidates: the OS reclaiming memory (check the system log for a memory-pressure kill), another " +
                    "process, or bleep. If this JVM was small and the machine had memory free, it was not an OOM."
                )
              )
            case 139 => ExitDescription("killed by SIGSEGV (exit 139)", Some("The JVM crashed; look for an hs_err_pid*.log next to the working directory."))
            case code if code > 128 => ExitDescription(s"killed by signal ${code - 128} (exit $code), not by bleep", None)
            case code               =>
              // The same diagnosis as the exit-0 case, which already names System.exit. A test calling `System.exit(3)` lands here rather than there, and
              // used to be reported as a bare "exited with code 3" — accurate and unhelpful. bleep cannot prevent the call: the runner installs a
              // SecurityManager to block it, and JDK 24 removed SecurityManager, so on any current JVM the exit goes through and the fork simply dies.
              ExitDescription(
                s"exited with code $code",
                Some(
                  s"The JVM exited without sending a suite result. A test calling System.exit($code) is the usual cause; bleep cannot block that on JDK 24+, " +
                    "where the SecurityManager it relied on no longer exists. Everything the suite had reported before the exit is kept, and the suite is " +
                    "marked as not finished."
                )
              )
          }
    }
  }

  /** Key for pooling JVMs */
  private case class JvmKey(classpathHash: String, optionsHash: String, envHash: String, cwdHash: String) {

    /** Identity under which this kind of fork's observed cost is remembered. Same key means same classpath, same options, same environment — so what one of
      * them cost is genuinely evidence about the next.
      */
    def costKey: String = s"$classpathHash-$optionsHash-$envHash-$cwdHash"
  }

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
      /** Protocol channel to the fork — a loopback socket, deliberately not the process's stdin. */
      val stdin: PrintWriter,
      /** Protocol channel from the fork. See [[stdin]]. */
      val stdout: BufferedReader,
      val stderr: BufferedReader,
      /** The fork's actual stdout. Carries only output now that the protocol has its own socket, including whatever a subprocess started with inherited IO
        * writes straight to the descriptor — which is the only way that output can reach the user at all.
        */
      val processStdout: BufferedReader,
      val protocolSocket: java.net.Socket,
      val key: JvmKey,
      val jvmCommand: Path,
      /** Returns this process's memory reservation to the machine governor. Held for the lifetime of the PROCESS, not of the suite that happened to spawn it: a
        * JVM sitting idle in the pool is still resident and still costing the machine its whole footprint, so the reservation is only released when the process
        * is actually destroyed. Must be run exactly where the process is killed — see `JvmPoolImpl.destroy`.
        */
      val releaseMemory: IO[Unit],
      /** When this fork was created. Taken at construction, not from `process.info().startInstant()` when it dies: by then the process has been killed and the
        * OS no longer reports a start instant for it, which is why every fork_end carried a lifetime of -1.
        *
        * Genuinely last, and defaulted: anywhere else in this list a default silently rebinds the positional arguments after it, which is exactly what the
        * first attempt at this did — `stdin` became the timestamp and the build stopped compiling.
        */
      val startedAtMs: Long = System.currentTimeMillis()
  ) {
    @volatile private var alive = true
    @volatile private var _protocolClean = true
    @volatile private var _suiteInFlight = false

    /** Buffered stderr lines collected by the drain thread. Bounded so a runaway warning storm can't OOM the parent. Oldest lines are dropped past the cap. */
    private val stderrBuffer = new java.util.concurrent.ConcurrentLinkedDeque[String]()
    private val stderrBufferCap = 2048

    locally {
      def drainInto(name: String, reader: BufferedReader): Unit = {
        val t = new Thread(s"jvm-$name-drain-${process.pid}") {
          override def run(): Unit =
            try {
              var line = reader.readLine()
              while (line != null) {
                stderrBuffer.addLast(line)
                while (stderrBuffer.size > stderrBufferCap) stderrBuffer.pollFirst(): Unit
                line = reader.readLine()
              }
            } catch { case NonFatal(_) => () }
        }
        t.setDaemon(true)
        t.start()
      }
      drainInto("stderr", stderr)
      // Draining the fork's stdout is not optional. Nothing reads it otherwise, so a subprocess writing steadily to the inherited descriptor fills the pipe
      // buffer and blocks — the suite then hangs with no output and no explanation.
      drainInto("stdout", processStdout)
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

    /** Set when WE terminate this process, with why. `None` means nothing in bleep killed it, which is the only case where an external cause — the OS — is a
      * sound conclusion.
      *
      * Without this the two are indistinguishable after the fact: `destroyForcibly` sends SIGKILL, so a fork we killed reports exit 137 exactly like one the
      * kernel killed. Reporting all of them as OS memory pressure sent a long investigation into the memory subsystem for failures bleep was causing itself.
      */
    @volatile private var _killedByUs: Option[String] = None
    def killedByUs: Option[String] = _killedByUs

    def kill(reason: String): Unit = {
      if (_killedByUs.isEmpty) _killedByUs = Some(reason)
      alive = false
      try
        stdin.close()
      catch { case NonFatal(_) => }
      // Closing the socket is what the child reads as end-of-commands, the role closing its stdin used to play.
      try
        protocolSocket.close()
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
      listener: JvmPoolListener,
      semaphore: Semaphore[IO],
      startLimiter: Semaphore[IO],
      machine: MachineResources,
      pool: TrieMap[JvmKey, Queue[IO, ManagedJvm]],
      allJvms: Ref[IO, Set[ManagedJvm]],
      spawnFailures: TrieMap[JvmKey, Int],
      jvmCommand: Path,
      workingDirectory: Path,
      costs: ForkCostModel,
      processMemory: ProcessMemory
  ) extends JvmPool {

    override def acquire(
        label: String,
        classpath: List[Path],
        jvmOptions: List[String],
        defaultHeapMb: Long,
        runnerClass: String,
        environment: Map[String, String],
        workingDirectory: Option[Path]
    ): Resource[IO, TestJvm] = {
      // Bound the fork before anything else looks at these options. Everything downstream — the pool
      // key, the spawn, and what the governor is told this costs — must agree on the heap the JVM
      // will actually run with, and that is only true if the bound is applied once, here.
      val boundedOptions = MachineResources.withHeapBound(jvmOptions, defaultHeapMb)
      val key = JvmKey(classpath, boundedOptions, environment, workingDirectory)

      // NO machine CPU reservation here. The caller already holds one.
      //
      // A TestSuiteTask is charged `Cost(TestFork, cpu = 1)` by the DAG interpreter at admission (see
      // TaskDag.costOf), and this method runs INSIDE that admitted task. Reserving again here asked the
      // same finite pool for a second permit while holding the first, so once admission had handed out
      // every permit to test tasks, all of them queued for a permit that could not exist: `cpu 18/18,
      // running 18, waiting 18`, no forks spawned, no thread doing anything, forever. Compiles in other
      // workspaces starved behind it, because machine CPU is daemon-wide. The two entries were
      // distinguishable in the queue dump only by their labels — `test:proj:Suite` from the interpreter
      // and `test Suite` from here.
      //
      // The interpreter is the single authority on machine-wide capacity. What stays here is the local
      // counter bounding THIS run's parallelism, which is not machine-wide and cannot deadlock against
      // admission.
      //
      // Memory is different and is still taken below, not here: it belongs to the PROCESS, which outlives
      // this scope. An idle pooled JVM is still resident and still costs its whole footprint, so its
      // reservation is taken at spawn and returned at destroy (see `spawnJvm` / `destroy`). Tying memory
      // to the suite is what let the governor believe memory was free while live JVMs still held it.
      Resource.make(semaphore.acquire)(_ => semaphore.release).flatMap { _ =>
        Resource
          .make(
            getOrCreate(label, key, classpath, boundedOptions, runnerClass, environment, workingDirectory).map(jvm => (jvm, new TestJvmImpl(jvm): TestJvm))
          ) {
            // Return JVM to pool (or destroy it); the semaphore is released by its own Resource.
            case (jvm, _) => release(jvm)
          }
          .map(_._2)
      }
    }

    /** What to charge this fork: what forks of its kind have been measured to cost, falling back to the footprint implied by its heap bound until one has run.
      *
      * The bound is a ceiling, not a prediction. Charging it made the budget fill up at roughly a quarter of the machine's real capacity — measured median cost
      * 610MB against 2560MB charged. `withHeapBound` guarantees an `-Xmx` is present by the time we get here, so the fallback is at least a bound the process
      * is genuinely held to rather than a guess about an unbounded one.
      */
    private def costOf(key: JvmKey, jvmOptions: List[String]): IO[Long] =
      costs.estimateMb(
        key.costKey,
        parseXmxMb(jvmOptions).getOrElse(
          throw new IllegalStateException(s"fork options reached the governor without a heap bound: ${jvmOptions.mkString(" ")}")
        )
      )

    /** Destroy a JVM: learn what it cost, kill the process, stop tracking it, and only then return its memory to the governor. Ordering matters twice over —
      * the measurement has to happen while the process still exists, and releasing before the kill would let a waiter be granted memory this process has not
      * actually surrendered yet.
      */
    private def destroy(jvm: ManagedJvm, destroyReason: String): IO[Unit] =
      observeCost(jvm).attempt >> IO(jvm.kill(destroyReason)).attempt >> announceEnd(jvm).attempt >> allJvms.update(_ - jvm) >> jvm.releaseMemory

    /** Announced after `kill`, so the exit description is final and `killedByUs` is set — that flag is the only thing separating a fork bleep terminated from
      * one the OS killed, since both report exit 137.
      *
      * Lifetime comes from the JVM's own record of when the process started rather than a field we would have to keep in step; where the platform does not
      * supply it, the age is simply omitted rather than guessed.
      */
    private def announceEnd(jvm: ManagedJvm): IO[Unit] =
      IO {
        val exit = describeExit(jvm.process, jvm.killedByUs)
        listener.onForkEnd(jvm.process.pid(), System.currentTimeMillis() - jvm.startedAtMs, exit.summary, jvm.killedByUs)
      }

    /** Record what this fork actually cost the machine, for the benefit of the next one of its kind.
      *
      * Prefers the platform's own high-water mark where it keeps one (macOS `phys_footprint_peak`), because a suite is as expensive as its worst moment and
      * sampling would have to be lucky to catch it. Where there is no peak, the reading at destroy time is a floor on the truth — better than the
      * `-Xmx`-derived guess it replaces, and it only ever revises the estimate upward.
      */
    private def observeCost(jvm: ManagedJvm): IO[Unit] =
      IO.blocking(processMemory.peakFootprintMb(jvm.process.pid()).orElse(processMemory.footprintMb(jvm.process.pid())))
        .flatMap {
          case Some(mb) => costs.observe(jvm.key.costKey, mb)
          case None     => IO.unit
        }

    /** Kill one idle pooled JVM (any key) so its memory returns to the governor. `false` when the pool holds nothing idle.
      *
      * This is what stops the pool deadlocking against itself. Now that a JVM's memory reservation lasts as long as the process, a pool full of idle cached
      * JVMs can hold the entire budget, and a spawn needing memory would otherwise wait on processes that nothing will destroy until shutdown. Faced with that,
      * the pool gives up a cached JVM rather than the build.
      */
    private def evictOneIdle: IO[Boolean] =
      pool.values.toList
        .foldLeft(IO.pure(Option.empty[ManagedJvm])) { (acc, queue) =>
          acc.flatMap {
            case found @ Some(_) => IO.pure(found)
            case None            => queue.tryTake
          }
        }
        .flatMap {
          case Some(idle) => destroy(idle, "bleep: evicted from pool to free memory for a new fork").as(true)
          case None       => IO.pure(false)
        }

    /** Reserve a new process's memory, trading cached JVMs for it before agreeing to wait.
      *
      * If it doesn't fit, evict an idle pooled JVM and retry — that memory is already ours, and a warm classloader is worth less than making progress. Only
      * when nothing is left to evict do we park, and that wait terminates: at that point the budget is held by JVMs actively running suites, and when those
      * finish `release` destroys rather than pools them, because the governor reports contention.
      */
    private def reserveMemoryForSpawn(label: String, footprintMb: Long): IO[IO[Unit]] =
      machine.tryReserve(MachineResources.ResourceKind.TestFork, label, cpu = 0, memoryMb = footprintMb).flatMap {
        case Some(release) => IO.pure(release)
        case None          =>
          evictOneIdle.flatMap {
            case true  => reserveMemoryForSpawn(label, footprintMb)
            case false => machine.reserveUntilReleased(MachineResources.ResourceKind.TestFork, label, cpu = 0, memoryMb = footprintMb)
          }
      }

    private def getOrCreate(
        label: String,
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
            IO(listener.onForkReused(existing.process.pid(), label)).attempt >> IO.pure(existing)
          case Some(dead) =>
            // JVM died while idle in the pool. `destroy` (not just untracking) so its memory
            // reservation goes back to the governor — otherwise a dead process's footprint would be
            // charged for the rest of the server's life.
            destroy(dead, "bleep: pooled JVM found dead") >> spawnJvm(label, key, classpath, jvmOptions, runnerClass, environment, cwd)
          case None =>
            spawnJvm(label, key, classpath, jvmOptions, runnerClass, environment, cwd)
        }
      } yield jvm

    /** Whatever the fork wrote before it stopped, read without ever blocking.
      *
      * Only bytes already sitting in the pipe are taken, and only up to [[MaxChildOutputBytes]]. Nothing is draining these streams at this point — the reader
      * threads belong to `ManagedJvm`, which does not exist yet on this path — so a blocking read here would hang the very code whose job is to report a hang.
      */
    private def describeChildOutput(process: Process): String = {
      val quoted =
        List("stderr" -> drainAvailable(process.getErrorStream), "stdout" -> drainAvailable(process.getInputStream))
          .collect { case (name, text) if text.trim.nonEmpty => s"\n  $name: ${text.trim}" }
      if (quoted.isEmpty) " The fork wrote no output." else quoted.mkString
    }

    private def drainAvailable(stream: InputStream): String = {
      val collected = new ByteArrayOutputStream
      val buf = new Array[Byte](8192)
      var more = true
      while (more && collected.size < MaxChildOutputBytes) {
        val ready = stream.available()
        if (ready <= 0) more = false
        else {
          val n = stream.read(buf, 0, math.min(buf.length, math.min(ready, MaxChildOutputBytes - collected.size)))
          if (n <= 0) more = false else collected.write(buf, 0, n)
        }
      }
      new String(collected.toByteArray, StandardCharsets.UTF_8)
    }

    /** Wait for a freshly spawned fork to connect back, giving up the moment that becomes impossible rather than always serving the full sentence.
      *
      * Polled instead of one long `accept`, because the answer is usually available long before the deadline: a fork that died during JVM startup is never
      * going to connect, and blocking on a process that no longer exists turned a fast, fully explained failure into a minutes-long stall — 32 suites of it, in
      * the report that prompted this.
      *
      * What the give-up means depends entirely on the fork's state, which is why both branches say so. A fork that exited hit a startup failure and described
      * it on its own stderr. A fork still running never intended to connect: that is what a protocol mismatch looks like, and the case that actually happened
      * was a `bleep-test-runner` from the project's own dependencies shadowing the server's and waiting for orders on stdin. The two need opposite fixes and
      * the bare "Accept timed out" they used to share told them apart not at all — it read as a slow machine, which neither of them is.
      */
    private def awaitProtocolConnection(listener: ServerSocket, process: Process, port: Int): Socket = {
      val deadlineNanos = System.nanoTime() + ProtocolConnectTimeout.toNanos
      listener.setSoTimeout(ProtocolPollInterval.toMillis.toInt)

      def giveUp(reason: String): Nothing = {
        // Read what the fork wrote before killing it. `destroyForcibly` closes these pipes as the process is reaped, and a read landing on the far side of
        // that comes back "Stream closed", replacing the diagnosis this exists to produce.
        val childOutput = describeChildOutput(process)
        if (process.isAlive) {
          process.destroyForcibly(): Unit
          process.waitFor(5, TimeUnit.SECONDS): Unit
        }
        throw new IOException(s"Test JVM did not connect back on port $port: $reason.$childOutput")
      }

      var connected: Socket = null
      while (connected == null)
        try connected = listener.accept()
        catch {
          case _: SocketTimeoutException =>
            if (!process.isAlive) giveUp(s"the fork exited with code ${process.exitValue()} without ever connecting")
            else if (System.nanoTime() >= deadlineNanos)
              giveUp(
                s"the fork was still running $ProtocolConnectTimeout later and had not connected, so it is not speaking this server's protocol — check " +
                  "whether another bleep-test-runner is shadowing the one bleep puts on the test classpath"
              )
        }
      connected
    }

    private def spawnJvm(
        label: String,
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
      // Reserve this process's memory BEFORE starting it, and hand the release action to the
      // ManagedJvm so it lives exactly as long as the process does. If anything between here and a
      // healthy handshake fails, the reservation must be handed back — hence the bracketCase.
      //
      // The whole spawn-through-handshake window is additionally held under `startLimiter`, so only
      // MaxConcurrentStarts JVMs are ever climbing to their working set at the same time. See its
      // docs: this is the demand no budget can see, because it does not exist yet at the moment the
      // admission decision is made.
      startLimiter.permit.use { _ =>
        costOf(key, jvmOptions)
          .flatMap(reserveMemoryForSpawn(s"jvm ${key.classpathHash}", _))
          .bracketCase { releaseMemory =>
            IO
              .blocking {
                val javaPath = jvmCommand
                val cpString = classpath.map(_.toString).mkString(File.pathSeparator)

                // On Windows, command-line length is limited to 32,767 characters.
                // When the classpath is too long, pass it via CLASSPATH environment variable instead.
                val useEnvClasspath = scala.util.Properties.isWin && cpString.length > 30000

                // Quiet the JVM's own deprecation notice about `sun.misc.Unsafe`, which scala-library's `LazyVals` triggers on JDK 24+. Four lines of
                // warning on stderr of every forked test run, about code the user does not own and cannot change, landing in their test output and in
                // `<system-err>` of every report. `-XX:+IgnoreUnrecognizedVMOptions` first so older JVMs that lack the flag start rather than refuse to.
                val quietUnsafe = List("-XX:+IgnoreUnrecognizedVMOptions", "--sun-misc-unsafe-memory-access=allow")
                val cmd =
                  if (useEnvClasspath)
                    List(javaPath.toString) ++ quietUnsafe ++ jvmOptions ++ List(runnerClass)
                  else
                    List(javaPath.toString) ++ quietUnsafe ++ jvmOptions ++ List("-cp", cpString, runnerClass)

                // The fork talks protocol over a loopback socket, not over its stdout. Anything a test (or a subprocess a test starts with inherited IO —
                // Scala Native's test binaries, Testcontainers, a plain ProcessBuilder) writes to file descriptor 1 would otherwise land inside the JSON
                // stream, and the suite dies with "Protocol error: expected json value". Bound before the process starts so the child never races the listener.
                val protocolListener = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)
                val protocolPort = protocolListener.getLocalPort

                val cmdWithProtocol = cmd.head :: s"-D${ForkedTestRunnerProtocol.PortProperty}=$protocolPort" :: cmd.tail
                val pb = new ProcessBuilder(cmdWithProtocol*)
                pb.directory(cwdOverride.getOrElse(workingDirectory).toFile)
                pb.redirectErrorStream(false)
                if (useEnvClasspath) {
                  pb.environment().put("CLASSPATH", cpString): Unit
                }
                // Default ANSI-off (no-color.org standard, honored by ScalaTest / JUnit / kotlinc / native-image / most JVM tooling). Set with putIfAbsent so any
                // explicit caller override — including the parent JVM's inherited NO_COLOR — still wins.
                pb.environment().putIfAbsent("NO_COLOR", "1"): Unit
                environment.foreach { case (k, v) => pb.environment().put(k, v) }

                val process =
                  try pb.start()
                  catch {
                    case e: Throwable =>
                      protocolListener.close()
                      throw e
                  }

                val protocolSocket =
                  try awaitProtocolConnection(protocolListener, process, protocolPort)
                  catch {
                    case e: Throwable =>
                      if (process.isAlive) process.destroyForcibly(): Unit
                      throw e
                  } finally protocolListener.close()
                protocolSocket.setTcpNoDelay(true)

                val stdin = new PrintWriter(new OutputStreamWriter(protocolSocket.getOutputStream, StandardCharsets.UTF_8), true)
                val stdout = new BufferedReader(new InputStreamReader(protocolSocket.getInputStream, StandardCharsets.UTF_8))
                val stderr = new BufferedReader(new InputStreamReader(process.getErrorStream))
                val processStdout = new BufferedReader(new InputStreamReader(process.getInputStream))

                new ManagedJvm(process, stdin, stdout, stderr, processStdout, protocolSocket, key, jvmCommand, releaseMemory)
              }
              .flatTap(jvm => allJvms.update(_ + jvm))
              .flatTap(jvm =>
                waitForReady(jvm).onError { case _ =>
                  IO(spawnFailures.updateWith(jvm.key) { case Some(n) => Some(n + 1); case None => Some(1) }).void
                }
              )
              .flatTap(jvm => IO(listener.onForkStart(jvm.process.pid(), label, parseXmxMb(jvmOptions))).attempt)
              .flatTap(jvm => IO(spawnFailures.remove(jvm.key))) // Reset on success
          } {
            // On success the reservation now belongs to the ManagedJvm, which releases it when destroyed.
            // On any failure — process never started, handshake failed, cancellation — nothing owns it,
            // so hand it straight back rather than leaking the footprint of a JVM that isn't running.
            case (_, Outcome.Succeeded(_))           => IO.unit
            case (releaseMemory, Outcome.Errored(_)) => releaseMemory
            case (releaseMemory, Outcome.Canceled()) => releaseMemory
          }
      }
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
          // Same reporting as a death mid-session (`describeExit`), so the two paths don't disagree
          // about what 137 means. This one matters at least as much: a fork killed BEFORE it could
          // print Ready never ran a line of test code, and with no stderr the exit status is the
          // only evidence there is. Saying "exit code 137" without naming SIGKILL left the most
          // common startup failure — the OS refusing to back a new JVM — looking like a bleep bug.
          val exit = JvmPool.describeExit(jvm.process, jvm.killedByUs)
          val stderrPart = if (stderrOutput.nonEmpty) s" Stderr:\n$stderrOutput" else " (no stderr output)"
          val detailPart = exit.detail.fold("")(d => s" $d")
          throw new IOException(s"JVM process (pid=$pid) terminated before sending Ready — ${exit.summary}.$detailPart$stderrPart")
        }
        TestProtocol.decodeResponse(line) match {
          case Right(TestProtocol.TestResponse.Ready) => ()
          case Right(other)                           =>
            throw new IOException(s"Expected Ready, got: $other")
          case Left(err) =>
            throw new IOException(s"Failed to decode response: $err, line: $line")
        }
      }.timeout(30.seconds)
        // A slow start is not a dead JVM. Under load — 18 cores saturated, dozens of JVMs paging in a
        // large classpath — reaching Ready can legitimately take a while, and killing at 30s turned
        // "this machine is busy" into a SIGKILL we then blamed on the OS.
        .onError { case _ => IO(jvm.kill("bleep: no Ready handshake within the startup timeout")) }

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
            jvms.foreach(_.kill("bleep: pool shutdown"))
          }
          // Shutdown kills directly rather than going through `destroy`, so without this the JVMs that survived to the end of a run — usually most of them —
          // would have a fork_start and never a fork_end, and their lifetimes would be unknowable.
          _ <- jvms.toList.traverse_(jvm => announceEnd(jvm).attempt)
          _ <- allJvms.set(Set.empty)
          _ <- IO(pool.clear())
          // Backstop for the whole scheme: every process-lifetime reservation is returned here, so a
          // pool that is torn down can never leave the machine's memory budget permanently consumed —
          // which matters because these reservations are held outside any Resource scope.
          _ <- jvms.toList.traverse_(_.releaseMemory.attempt)
        } yield ()
      }

    override def size: IO[Int] =
      allJvms.get.map(_.size)

    /** Return a JVM to the pool for reuse — or destroy it.
      *
      * Caching a JVM keeps its whole memory reservation held for a warm classloader we merely HOPE to reuse. That is a good trade on an idle machine and a bad
      * one when something is queued for memory right now, so under contention we destroy instead of pooling. This is also half of the pool's liveness argument:
      * a spawn that has run out of idle JVMs to evict parks on the governor, and the running suites it is waiting for hand their memory back here rather than
      * squirreling it away in the pool.
      */
    private def release(jvm: ManagedJvm): IO[Unit] =
      if (jvm.isAlive && jvm.protocolClean && !jvm.suiteInFlight) {
        machine.isContended.flatMap {
          case true  => destroy(jvm, "bleep: not pooled because the machine is contended")
          case false =>
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
        }
      } else {
        // Dead or protocol-dirty JVM — kill it and return its memory.
        destroy(jvm, "bleep: JVM unhealthy or protocol-dirty after its suite")
      }

    private class TestJvmImpl(jvm: ManagedJvm) extends TestJvm {

      override def pid: Long = jvm.process.pid()

      override def runSuite(
          className: String,
          selection: FrameworkSelection,
          args: List[String]
      ): Stream[IO, TestProtocol.TestResponse] = {
        val command = TestProtocol.TestCommand.RunSuite(className, selection, args)

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
              val exitDescription = JvmPool.describeExit(jvm.process, jvm.killedByUs)
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
        IO(jvm.kill("bleep: explicit kill (suite timeout or cancellation)"))
    }
  }
}
