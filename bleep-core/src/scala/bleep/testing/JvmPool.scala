package bleep.testing

import bleep.machine.{ForkAcquirer, ForkDemand, ForkGrant, ForkId, ForkKey, ForkKind, ForkLifecycle, ForkRegistry, TaskId}
import cats.effect._
import cats.effect.std.Queue
import cats.syntax.all._
import fs2.Stream

import java.io._
import java.net.{InetAddress, ServerSocket, Socket, SocketTimeoutException}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.security.MessageDigest
import java.util.concurrent.TimeUnit
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.util.Properties
import scala.util.control.NonFatal

/** The daemon's pool of forked test JVMs.
  *
  * One per daemon, not per request: a fork outlives the request that started it when another request's suite reuses it, and the machine scheduler decides which
  * fork runs what (design §3.2, §10 step 11). This class keeps only the process mechanics — spawn, handshake, protocol, kill — and reports every fork's life to
  * the scheduler through [[bleep.machine.ForkLifecycle]]; it decides nothing. A request obtains its view with [[forRequest]] and asks for a fork through that
  * view, which asks the scheduler, which answers `Reuse` or `Spawn`.
  */
trait JvmPool {

  /** This pool, acting for one request: every fork it hands out was granted to that request by the scheduler. Its `shutdown` is a no-op — the pool shuts down
    * with the daemon, and the scheduler evicts idle forks nothing wants.
    */
  def forRequest(acquirer: ForkAcquirer): TestExecutor

  /** Shutdown all JVMs in the pool.
    *
    * This MUST be called when done with the pool. Use guarantee to ensure it runs.
    */
  def shutdown: IO[Unit]

  /** Number of JVMs currently in the pool */
  def size: IO[Int]
}

/** A handle to a forked JVM running the test runner */
trait TestJvm extends TestSession {

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

  /** Tells a JDK 24+ JVM not to print its `sun.misc.Unsafe` deprecation notice. Older JVMs reject it outright, so it is only ever passed to one that took it.
    */
  private val UnsafeMemoryAccessFlag = "--sun-misc-unsafe-memory-access=allow"

  private val unsafeFlagSupport = new java.util.concurrent.ConcurrentHashMap[String, java.lang.Boolean]()

  /** Does this JVM accept [[UnsafeMemoryAccessFlag]]?
    *
    * Asked once per `java` binary and cached, by starting it with the flag and nothing else. A probe rather than a version comparison because the question is
    * exactly "does this accept the flag" — parsing `java -version` to infer it adds a format to get wrong for no gain.
    *
    * A probe that cannot be run at all answers "no": the flag is a nicety, and a fork that starts with a warning beats one that does not start.
    */
  private def acceptsUnsafeMemoryAccessFlag(javaPath: java.nio.file.Path): Boolean =
    unsafeFlagSupport
      .computeIfAbsent(
        javaPath.toString,
        _ =>
          java.lang.Boolean.valueOf {
            try {
              val pb = new ProcessBuilder(javaPath.toString, UnsafeMemoryAccessFlag, "-version")
              pb.redirectErrorStream(true)
              pb.redirectOutput(ProcessBuilder.Redirect.DISCARD)
              val p = pb.start()
              p.waitFor(30, java.util.concurrent.TimeUnit.SECONDS) && p.exitValue() == 0
            } catch { case _: Exception => false }
          }
      )
      .booleanValue()

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

  /** The daemon's pool. One per daemon; shut down with it.
    *
    * @param lifecycle
    *   where every fork's start, idle and exit is reported — the scheduler
    * @param forks
    *   the daemon's register of live forks, where every fork this pool starts is registered for as long as it lives, with the means to kill it
    */
  def create(listener: JvmPoolListener, lifecycle: ForkLifecycle, forks: ForkRegistry): Resource[IO, JvmPool] =
    Resource.make(
      for {
        allJvms <- Ref.of[IO, Map[ForkId, ManagedJvm]](Map.empty)
      } yield new JvmPoolImpl(listener, allJvms, new TrieMap[ForkId, ManagedJvm](), new TrieMap[JvmKey, Int](), lifecycle, forks)
    )(_.shutdown)

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
              ExitDescription(
                "EOF on stdout, exited 0",
                Some(
                  "The JVM exited cleanly (0) without sending a suite result. Something ended the process out from under the run: a System.exit(0), a " +
                    "Runtime.halt(0), or its last non-daemon thread finishing. The fork's exit log distinguishes them — if one was written, a shutdown hook " +
                    "ran (System.exit or a normal exit) and it names the caller; its ABSENCE means no hook ran at all, i.e. Runtime.halt(0) or a hard kill. A " +
                    "daemon-thread watchdog that calls halt() to bound a subprocess is a classic source when it is armed inside a shared, long-lived fork."
                )
              )
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

  /** Whatever a fork wrote before it stopped, for a spawn-failure diagnostic — the message the user reads when a test JVM never connects back.
    *
    * `exited` decides HOW to read, and it is the whole point of this existing. An exited fork has flushed and closed its streams: drain to EOF, because that is
    * the only way to get the tail — the JVM's "Unrecognized VM option ...", "Could not create the Java Virtual Machine", an `hs_err` pointer — lands on stderr
    * a beat AFTER the process is seen dead, and `available()` reports 0 at that instant. Reading only what was `available()` is exactly how that message got
    * lost, turning a fully-explained failure into a bare "N suites never reported a result". A still-running fork has open streams, so take only what is
    * already buffered, without blocking the very code whose job is to report a hang. Bounded to `maxBytes` per stream.
    *
    * Takes a bare [[Process]] so it is unit-testable without a pool or a BSP server: spawn `java <bad-option> -version`, which exits non-zero with the reason
    * on stderr, and assert it comes back here.
    */
  private[testing] def describeChildOutput(process: Process, exited: Boolean, maxBytes: Int = MaxChildOutputBytes): String = {
    def read(stream: InputStream): String = if (exited) drainToEof(stream, maxBytes) else drainAvailable(stream, maxBytes)
    val quoted =
      List("stderr" -> read(process.getErrorStream), "stdout" -> read(process.getInputStream))
        .collect { case (name, text) if text.trim.nonEmpty => s"\n  $name: ${text.trim}" }
    if (quoted.isEmpty) " The fork wrote no output." else quoted.mkString
  }

  /** Bytes already sitting in the pipe, never blocking — safe on a process that may still be running. Stops as soon as nothing more is buffered, so a live fork
    * that has more to say later is not waited on.
    */
  private[testing] def drainAvailable(stream: InputStream, maxBytes: Int): String = {
    val collected = new ByteArrayOutputStream
    val buf = new Array[Byte](8192)
    var more = true
    while (more && collected.size < maxBytes) {
      val ready = stream.available()
      if (ready <= 0) more = false
      else {
        val n = stream.read(buf, 0, math.min(buf.length, math.min(ready, maxBytes - collected.size)))
        if (n <= 0) more = false else collected.write(buf, 0, n)
      }
    }
    new String(collected.toByteArray, StandardCharsets.UTF_8)
  }

  /** Read to EOF. Safe ONLY on a process that has already exited — its writer is closed, so `read` returns -1 rather than blocking. This is what actually
    * captures a startup failure's full stderr, which `drainAvailable` races and misses. Bounded to `maxBytes`.
    */
  private[testing] def drainToEof(stream: InputStream, maxBytes: Int): String = {
    val collected = new ByteArrayOutputStream
    val buf = new Array[Byte](8192)
    var more = true
    while (more && collected.size < maxBytes) {
      val n = stream.read(buf, 0, math.min(buf.length, maxBytes - collected.size))
      if (n < 0) more = false else collected.write(buf, 0, n)
    }
    new String(collected.toByteArray, StandardCharsets.UTF_8)
  }

  /** Key for pooling JVMs */
  private case class JvmKey(jvmHash: String, classpathHash: String, optionsHash: String, envHash: String, cwdHash: String) {

    /** What makes one fork reusable for another request's suite: same `java`, same classpath, same options, same environment, same working directory. The
      * scheduler's `ForkKey` is this plus the sharing flavour — see `JvmPoolImpl.forkKey`.
      */
    def costKey: String = s"$jvmHash-$classpathHash-$optionsHash-$envHash-$cwdHash"
  }

  private object JvmKey {
    def apply(jvmCommand: Path, classpath: List[Path], options: List[String], environment: Map[String, String], cwd: Option[Path]): JvmKey = {
      val jvmHash = hashStrings(List(jvmCommand.toString))
      val cpHash = hashStrings(classpath.map(_.toString))
      val optHash = hashStrings(options)
      val envHash = hashStrings(environment.toList.sorted.map { case (k, v) => s"$k=$v" })
      val cwdHash = hashStrings(cwd.map(_.toString).toList)
      JvmKey(jvmHash, cpHash, optHash, envHash, cwdHash)
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
      /** The scheduler's name for this fork, under which its life is reported and under which it is reused or evicted. */
      val forkId: ForkId,
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
      /** File the fork writes its exit diagnostic to (see [[ForkedTestRunnerProtocol.ExitLogProperty]]). Read by [[readExitLog]] after the fork dies, when it
        * is the only surviving account of an exit the parent otherwise sees as a bare "exited 0".
        */
      val exitLogPath: Path,
      /** Where [[kill]] announces itself. Not the pool's `listener` field reached directly, because this class is not nested in `JvmPoolImpl`; the pool passes
        * it at construction so the single kill chokepoint can report every termination on the same channel as the other fork events.
        */
      val listener: JvmPoolListener,
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

    /** Terminate the fork, escalating instead of going straight to SIGKILL.
      *
      * `graceMillis` is how long the child gets to die on its own terms: half after the socket close (which it reads as end-of-commands and exits on), half
      * after SIGTERM. Both routes run the JVM's shutdown hooks — and for a fork that started an application those hooks are what stop it and the testcontainers
      * it started. With ryuk disabled (required for testcontainers reuse, and common) those hooks are the ONLY container cleanup there is; SIGKILLing first
      * thing is how a machine ends up with dozens of orphaned databases.
      *
      * Pass 0 when the fork has forfeited its grace — it never completed the startup handshake, or a collective shutdown deadline already gave it time.
      */
    def kill(reason: String, graceMillis: Long): Unit = {
      // Every bleep-initiated socket close funnels through here (stdin/protocolSocket close below),
      // so this one announcement accounts for every fork bleep tears down. If a fork's socket goes
      // to EOF and no onForkKill named its pid first, bleep did not close it — the fork exited on
      // its own (a test's System.exit, a natural end, or an OS kill). That distinction is exactly
      // what was ambiguous when "N suites never reported a result" had no cause; recording every
      // kill on the fork-event channel (joined to fork_end by pid) settles it after the fact.
      val wasAlive = process.isAlive
      listener.onForkKill(process.pid(), reason, wasAlive, graceMillis)
      // Only claim the kill if there is something left to kill: a fork that already exited on its
      // own (e.g. gracefully during shutdown's deadline) must not be attributed to bleep — this
      // flag is the only thing separating our kills from natural exits and OS kills.
      if (wasAlive && _killedByUs.isEmpty) _killedByUs = Some(reason)
      alive = false
      try
        stdin.close()
      catch { case NonFatal(_) => }
      // Closing the socket is what the child reads as end-of-commands, the role closing its stdin used to play.
      try
        protocolSocket.close()
      catch { case NonFatal(_) => }
      def waitForExit(millis: Long): Boolean =
        millis > 0 && (try process.waitFor(millis, java.util.concurrent.TimeUnit.MILLISECONDS)
        catch { case NonFatal(_) => false })
      if (!waitForExit(graceMillis / 2)) {
        process.destroy(): Unit // SIGTERM: shutdown hooks still run if the JVM is responsive
        if (!waitForExit(graceMillis / 2)) {
          process.destroyForcibly(): Unit
        }
      }
      // Sweep whatever the child left behind, however it died. A gracefully-exited JVM reaps its
      // own children; this catches the rest so orphaned sub-processes don't consume the machine.
      try
        process
          .descendants()
          .forEach(ph =>
            try ph.destroyForcibly(): Unit
            catch { case _: Exception => () }
          )
      catch { case NonFatal(_) => }
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

    /** The fork's exit diagnostic, if it wrote one, then deleted. Empty when the file is absent — which is itself informative: a `Runtime.halt` or a hard OS
      * kill runs no shutdown hooks, so the fork never got to write it, distinguishing those from a `System.exit` (hooks run, file present).
      */
    def readExitLog(): String =
      try
        if (java.nio.file.Files.exists(exitLogPath)) {
          val content = new String(java.nio.file.Files.readAllBytes(exitLogPath), StandardCharsets.UTF_8)
          try java.nio.file.Files.deleteIfExists(exitLogPath): Unit
          catch { case NonFatal(_) => }
          content
        } else ""
      catch { case NonFatal(_) => "" }
  }

  /** Max consecutive spawn failures per key before refusing to spawn. Prevents infinite retry when test runner jar is incompatible. */
  private val MaxSpawnFailures = 3

  private class JvmPoolImpl(
      listener: JvmPoolListener,
      allJvms: Ref[IO, Map[ForkId, ManagedJvm]],
      /** Forks holding no work, by id — what a `Reuse` grant for an exclusive demand takes. A shared fork at refcount 0 is here too, its session kept. */
      idle: TrieMap[ForkId, ManagedJvm],
      spawnFailures: TrieMap[JvmKey, Int],
      lifecycle: ForkLifecycle,
      forks: ForkRegistry
  ) extends JvmPool {
    private val demandCounter = new java.util.concurrent.atomic.AtomicLong(0L)

    /** One project's shared session on one fork, and how many suites hold it. Behind a Deferred so the suites that ask while the fork is starting wait on the
      * creator rather than each starting their own.
      */
    private case class SharedSlot(session: Deferred[IO, Either[Throwable, SharedProjectSession]], refCount: Int)

    /** The per-project shared sessions, by the fork they run on, alive for as long as the fork is. Allocated here rather than threaded through the constructor
      * so `SharedProjectSession` can stay an inner class with direct access to `destroy`, `ManagedJvm` and the rest of the pool.
      */
    private val sharedSlots: Ref[IO, Map[ForkId, SharedSlot]] = Ref.unsafe(Map.empty)

    override def forRequest(acquirer: ForkAcquirer): TestExecutor = new TestExecutor {
      override def acquire(request: TestSessionRequest): Resource[IO, TestSession] =
        request.sharing match {
          case SessionSharing.Exclusive => acquireExclusive(acquirer, request)
          case SessionSharing.Shared(_) => acquireShared(acquirer, request)
        }

      /** The pool is the daemon's; a request's view has nothing of its own to tear down. Forks the request leaves idle are the scheduler's to evict. */
      override def shutdown: IO[Unit] = IO.unit
      override def size: IO[Int] = allJvms.get.map(_.size)
    }

    /** What the scheduler is asked for. The key is the pool's own, plus the sharing flavour: a fork running a per-project shared session has a reader fiber on
      * its socket that an exclusive suite's protocol would collide with, so the two kinds of fork are never substituted for one another.
      */
    private def demandFor(acquirer: ForkAcquirer, request: TestSessionRequest, key: JvmKey, boundedOptions: List[String], shared: Boolean): ForkDemand =
      ForkDemand(
        request = acquirer.requestId,
        taskId = TaskId(s"${request.label}#${demandCounter.incrementAndGet()}"),
        kind = ForkKind.TestSuite,
        key = forkKey(key, shared),
        boundMb = bleep.MemorySizes.forkFootprintMb(
          bleep.MemorySizes
            .xmxMb(boundedOptions)
            .getOrElse(throw new IllegalStateException(s"fork options reached the scheduler without a heap bound: ${boundedOptions.mkString(" ")}"))
        ),
        cpu = request.cpu,
        shared = shared
      )

    private def forkKey(key: JvmKey, shared: Boolean): ForkKey = ForkKey(s"${key.costKey}:${if (shared) "shared" else "exclusive"}")

    private def acquireExclusive(acquirer: ForkAcquirer, request: TestSessionRequest): Resource[IO, TestSession] = {
      val boundedOptions = bleep.MemorySizes.withHeapBound(request.jvmOptions, request.defaultHeapMb)
      val key = JvmKey(request.jvmCommand, request.classpath, boundedOptions, request.environment, Some(request.effectiveWorkingDirectory))
      Resource
        .make(obtain(acquirer, request, key, boundedOptions))(jvm => release(jvm, request.cpu))
        .map(jvm => new TestJvmImpl(jvm): TestSession)
    }

    /** Ask the scheduler, and act on its answer: run on the idle fork it names, or start the one it allots. A named fork found dead is destroyed — the
      * scheduler hears of its exit — and the question is asked again.
      */
    private def obtain(acquirer: ForkAcquirer, request: TestSessionRequest, key: JvmKey, boundedOptions: List[String]): IO[ManagedJvm] =
      acquirer.acquire(demandFor(acquirer, request, key, boundedOptions, shared = false), request.group).flatMap {
        case ForkGrant.Reuse(id) =>
          idle.remove(id) match {
            case Some(jvm) if jvm.isAlive => IO(listener.onForkReused(jvm.process.pid(), request.label)).attempt.as(jvm)
            case Some(dead)               => destroy(dead, "bleep: pooled JVM found dead") >> obtain(acquirer, request, key, boundedOptions)
            case None                     =>
              IO.raiseError(new IllegalStateException(s"the scheduler granted fork ${id.value} for reuse, but this pool holds no idle fork by that id"))
          }
        case ForkGrant.Spawn(id) =>
          spawnJvm(
            id,
            request.label,
            key,
            request.jvmCommand,
            request.classpath,
            boundedOptions,
            request.runnerClass,
            request.environment,
            request.effectiveWorkingDirectory
          )
      }

    /** Destroy a JVM: kill the process, stop tracking it, and tell the scheduler it is gone. The kill comes first so that `exited` is never reported for a
      * process that still exists. A shared session's reader, blocked in a socket read that no interrupt reaches, ends with the socket and is reaped after.
      */
    private def destroy(jvm: ManagedJvm, destroyReason: String): IO[Unit] =
      IO.blocking(jvm.kill(destroyReason, graceMillis = 10000)).attempt >> announceEnd(jvm).attempt >>
        allJvms.update(_ - jvm.forkId) >> IO(idle.remove(jvm.forkId): Unit) >>
        sharedSlots.modify(slots => (slots - jvm.forkId, slots.get(jvm.forkId))).flatMap {
          case Some(slot) => slot.session.tryGet.flatMap { case Some(Right(session)) => session.reap; case _ => IO.unit }
          case None       => IO.unit
        } >>
        IO(forks.unregister(jvm.forkId): Unit) >> IO(lifecycle.exited(jvm.forkId))

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

      def giveUp(reason: String, exited: Boolean): Nothing = {
        // Read what the fork wrote before killing it. `destroyForcibly` closes these pipes as the process is reaped, and a read landing on the far side of
        // that comes back "Stream closed", replacing the diagnosis this exists to produce.
        //
        // `exited` decides HOW we read. A fork that already exited (a bad JVM option, a startup crash) has written its whole story to stderr — "Unrecognized VM
        // option", "Could not create the Java Virtual Machine" — and closed it; we must drain to EOF to get it, because `available()` races the flush and
        // usually reports 0 the instant the process is detected dead, which is exactly how that message got lost. A fork still running has an open stderr, so we
        // can only take what is already buffered without blocking the very code meant to report a hang.
        val childOutput = describeChildOutput(process, exited)
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
            if (!process.isAlive) giveUp(s"the fork exited with code ${process.exitValue()} without ever connecting", exited = true)
            else if (System.nanoTime() >= deadlineNanos)
              giveUp(
                s"the fork was still running $ProtocolConnectTimeout later and had not connected, so it is not speaking this server's protocol — check " +
                  "whether another bleep-test-runner is shadowing the one bleep puts on the test classpath",
                exited = false
              )
        }
      connected
    }

    private def spawnJvm(
        forkId: ForkId,
        label: String,
        key: JvmKey,
        jvmCommand: Path,
        classpath: List[Path],
        jvmOptions: List[String],
        runnerClass: String,
        environment: Map[String, String],
        cwd: Path
    ): IO[ManagedJvm] = {
      val failures = spawnFailures.getOrElse(key, 0)
      if (failures >= MaxSpawnFailures) {
        return IO(lifecycle.exited(forkId)) >> IO.raiseError(
          new IOException(
            s"Test JVM failed to start $failures consecutive times. This usually means the test runner jar " +
              s"is incompatible with the project's JVM. Check that bleep-test-runner is published for the correct Java version."
          )
        )
      }
      // The scheduler already counts this fork as Starting under `forkId`. Whatever happens between here and a healthy handshake, it hears either that the
      // process exists (so it can be measured) or that it is gone — hence the bracketCase.
      IO.unit.bracketCase { _ =>
        IO
          .blocking {
            val javaPath = jvmCommand
            val cpString = classpath.map(_.toString).mkString(File.pathSeparator)

            // On Windows, command-line length is limited to 32,767 characters.
            // When the classpath is too long, pass it via CLASSPATH environment variable instead.
            val useEnvClasspath = scala.util.Properties.isWin && cpString.length > 30000

            // Quiet the JVM's own deprecation notice about `sun.misc.Unsafe`, which scala-library's `LazyVals` triggers on JDK 24+. Four lines of
            // warning on stderr of every forked test run, about code the user does not own and cannot change, landing in their test output and in
            // `<system-err>` of every report.
            //
            // Asked of the JVM rather than assumed, and never with `-XX:+IgnoreUnrecognizedVMOptions`. That flag does make an older JVM tolerate the
            // option — and it makes it tolerate the *user's* mistakes too, silently, wherever they appear on the line: it is not positional. A project
            // stating `-XX:+TypoedFlag` in `jvmOptions` would have its fork start anyway and its typo never mentioned. That is precisely the failure
            // `SpawnFailureDiagnosticsIT` exists to prevent, and it caught this.
            val quietUnsafe = if (JvmPool.acceptsUnsafeMemoryAccessFlag(javaPath)) List(UnsafeMemoryAccessFlag) else Nil
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

            val exitLogPath = Files.createTempFile("bleep-test-fork-exit-", ".log")
            Files.delete(exitLogPath) // the fork (re)creates it only if it actually reaches its shutdown; its absence is a signal (see ExitLogProperty)
            val cmdWithProtocol =
              cmd.head ::
                s"-D${ForkedTestRunnerProtocol.PortProperty}=$protocolPort" ::
                s"-D${ForkedTestRunnerProtocol.ExitLogProperty}=$exitLogPath" ::
                cmd.tail
            val pb = new ProcessBuilder(cmdWithProtocol*)
            pb.directory(cwd.toFile)
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

            new ManagedJvm(forkId, process, stdin, stdout, stderr, processStdout, protocolSocket, key, jvmCommand, exitLogPath, listener)
          }
          .flatTap(jvm => allJvms.update(_ + (forkId -> jvm)) >> IO(forks.register(liveFork(jvm, label))) >> IO(lifecycle.spawned(forkId, jvm.process.pid())))
          .flatTap(jvm =>
            waitForReady(jvm).onError { case _ =>
              IO(spawnFailures.updateWith(jvm.key) { case Some(n) => Some(n + 1); case None => Some(1) }).void >>
                // A fork that never said Ready is no use to anyone: kill it and let the scheduler hear it is gone.
                destroy(jvm, "bleep: fork failed its handshake")
            }
          )
          .flatTap(jvm => IO(listener.onForkStart(jvm.process.pid(), label, bleep.MemorySizes.xmxMb(jvmOptions))).attempt)
          .flatTap(jvm => IO(spawnFailures.remove(jvm.key))) // Reset on success
      } {
        // A process that never started, or a cancellation before the handshake: the scheduler counts a fork that does not exist until told otherwise.
        // (A handshake failure is reported by `destroy` above, after which the pool no longer knows the id and `exited` is not repeated.)
        case (_, Outcome.Succeeded(_)) => IO.unit
        case (_, Outcome.Errored(_))   => allJvms.get.map(_.contains(forkId)).flatMap(known => if (known) IO.unit else IO(lifecycle.exited(forkId)))
        case (_, Outcome.Canceled())   => allJvms.get.map(_.contains(forkId)).flatMap(known => if (known) IO.unit else IO(lifecycle.exited(forkId)))
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
        // No grace: a fork that never completed the handshake has not run any suite, so it has no
        // application or containers to wind down.
        .onError { case _ => IO.blocking(jvm.kill("bleep: no Ready handshake within the startup timeout", graceMillis = 0)) }

    override def shutdown: IO[Unit] =
      // CRITICAL: Use uncancelable to ensure cleanup completes even during cancellation
      IO.uncancelable { _ =>
        for {
          jvms <- allJvms.get.map(_.values.toList)
          _ <- IO.blocking {
            jvms.foreach { jvm =>
              try {
                // Send shutdown command
                jvm.stdin.println(TestProtocol.encodeCommand(TestProtocol.TestCommand.Shutdown))
                jvm.stdin.flush()
              } catch { case NonFatal(_) => }
            }
          }
          // Wait for graceful exits under one shared deadline. The Shutdown command makes a healthy
          // runner exit on its own, running its shutdown hooks — for a fork running an application that is where
          // it stops and its testcontainers get removed. The
          // old fixed 500ms then SIGKILL truncated exactly those hooks, so every run leaked its
          // containers when ryuk was disabled. Well-behaved forks exit as fast as ever; the
          // deadline only costs time on forks that are actually winding something down.
          _ <- IO.blocking {
            val deadlineNanos = System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(10)
            jvms.foreach { jvm =>
              val remaining = deadlineNanos - System.nanoTime()
              if (remaining > 0) {
                try jvm.process.waitFor(remaining, java.util.concurrent.TimeUnit.NANOSECONDS): Unit
                catch { case NonFatal(_) => }
              }
            }
          }
          // Stragglers forfeited their grace; kill() with none escalates straight to SIGKILL.
          _ <- IO.blocking {
            jvms.foreach(_.kill("bleep: pool shutdown", graceMillis = 0))
          }
          // Shutdown kills directly rather than going through `destroy`, so without this the JVMs that survived to the end of a run — usually most of them —
          // would have a fork_start and never a fork_end, and their lifetimes would be unknowable.
          _ <- jvms.toList.traverse_(jvm => announceEnd(jvm).attempt)
          _ <- allJvms.set(Map.empty)
          _ <- IO(idle.clear())
          slots <- sharedSlots.getAndSet(Map.empty)
          _ <- slots.values.toList.traverse_(slot => slot.session.tryGet.flatMap { case Some(Right(session)) => session.reap.attempt.void; case _ => IO.unit })
          _ <- IO(jvms.foreach(jvm => forks.unregister(jvm.forkId): Unit))
          _ <- IO(jvms.foreach(jvm => lifecycle.exited(jvm.forkId)))
        } yield ()
      }

    override def size: IO[Int] =
      allJvms.get.map(_.size)

    /** The daemon's handle on a fork this pool started: added when the process exists, removed where it is destroyed or shut down. Its `kill` runs the same
      * `destroy` the pool uses, so an eviction ordered by the scheduler and a death the pool notices itself are reported the same way.
      */
    private def liveFork(jvm: ManagedJvm, label: String): ForkRegistry.LiveFork =
      ForkRegistry.LiveFork(
        id = jvm.forkId,
        pids = () => Set(jvm.process.pid()),
        label = label,
        key = forkKey(jvm.key, shared = false),
        startedAtEpochMs = jvm.startedAtMs,
        kill = reason => {
          import cats.effect.unsafe.implicits.global
          destroy(jvm, reason).unsafeRunSync()
        }
      )

    /** The suite is done with its fork: back to idle, and the scheduler hears the cpu is free. Whether the fork then stays warm or goes is the scheduler's call
      * (design §5 rule 3), carried out through this pool's `destroy` when it decides to evict. A fork that died, or is protocol-dirty after a cancelled suite,
      * is destroyed here: nothing could run on it.
      */
    private def release(jvm: ManagedJvm, cpu: Int): IO[Unit] =
      if (jvm.isAlive && jvm.protocolClean && !jvm.suiteInFlight)
        IO(idle.put(jvm.forkId, jvm): Unit) >> IO(lifecycle.workFinished(jvm.forkId, cpu))
      else
        // Dead or protocol-dirty JVM — kill it; `destroy` reports the exit, which is also how the scheduler learns its cpu is free.
        destroy(jvm, "bleep: JVM unhealthy or protocol-dirty after its suite")

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

      override def runSuites(
          classNames: List[String],
          parallelism: Int,
          selection: FrameworkSelection,
          args: List[String]
      ): Stream[IO, TestProtocol.TestResponse] = {
        val command = TestProtocol.TestCommand.RunSuites(classNames, parallelism, selection, args)
        val body =
          Stream.eval(IO(jvm.markSuiteStarted()) >> sendCommand(command)) >>
            readResponses.takeThrough {
              // One execute for the whole set; every class's own SuiteDone already went by, so the batch ends at BatchComplete (or a fork-level Error).
              case TestProtocol.TestResponse.BatchComplete => false
              case _: TestProtocol.TestResponse.Error      => false
              case _                                       => true
            }
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
              // Give a just-closed process a beat to finish dying so its exit code is final and its shutdown-hook exit log is fully written before we read them.
              try jvm.process.waitFor(2, java.util.concurrent.TimeUnit.SECONDS): Unit
              catch { case NonFatal(_) => }
              val stderrTail = jvm.readStderr()
              val exitLog = jvm.readExitLog()
              // Reap it and say HOW it died. "EOF on stdout" alone is undiagnosable — it looks the
              // same whether the JVM exited, crashed, or was killed by the OS. The exit status
              // distinguishes them, and an externally-signalled death (128+signal, so 137 = SIGKILL)
              // is the fingerprint of the kernel reclaiming memory, which no in-process log can show.
              val exitDescription = JvmPool.describeExit(jvm.process, jvm.killedByUs)
              // The exit log is the fork's own account, written to a FILE that survives the pipe teardown that loses stderr. Its ABSENCE is a signal too: on a
              // clean exit-0 death with no log, no shutdown hook ran — a `Runtime.halt` or a hard kill, not a `System.exit`.
              val exitLogPart =
                if (exitLog.nonEmpty) Some(s"fork exit log:\n$exitLog")
                else if (exitDescription.summary.contains("exited 0"))
                  Some("fork wrote no exit log — no shutdown hook ran (Runtime.halt, or a hard external kill), not a System.exit.")
                else None
              val details = List(exitDescription.detail, exitLogPart, Option.when(stderrTail.nonEmpty)(s"stderr tail:\n$stderrTail")).flatten match {
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
        // On cancellation the fork is healthy: the socket close makes it exit on its own, running
        // the shutdown hooks that stop an app's containers. On a suite timeout the JVM may
        // be wedged, in which case the grace period merely delays the SIGKILL it was always
        // getting — after the suite already burned its idle timeout, that delay is noise.
        IO.blocking(jvm.kill("bleep: explicit kill (suite timeout or cancellation)", graceMillis = 10000))

      override def killSuite(className: String): IO[Unit] =
        // Exclusive: the fork runs only this suite, so stopping the suite is stopping the fork.
        kill
    }

    // ============================ Shared per-project sessions ============================

    private def acquireShared(acquirer: ForkAcquirer, request: TestSessionRequest): Resource[IO, TestSession] = {
      val boundedOptions = bleep.MemorySizes.withHeapBound(request.jvmOptions, request.defaultHeapMb)
      val key = JvmKey(request.jvmCommand, request.classpath, boundedOptions, request.environment, Some(request.effectiveWorkingDirectory))
      Resource
        .make(obtainShared(acquirer, request, key, boundedOptions))(session => releaseShared(session, request.cpu))
        .map(s => s: TestSession)
    }

    /** Ask the scheduler for a place on the project's shared fork. It answers `Reuse` for a fork that already runs the project's suites — busy or idle — and
      * `Spawn` when there is none; several suites asking at once get one `Spawn` and the rest `Reuse` on later ticks, so exactly one of them creates the
      * session and the others join it. The slot is created before the spawn so a `Reuse` that lands while the fork is still starting waits on the same Deferred
      * as the creator.
      */
    private def obtainShared(acquirer: ForkAcquirer, request: TestSessionRequest, key: JvmKey, boundedOptions: List[String]): IO[SharedProjectSession] =
      acquirer.acquire(demandFor(acquirer, request, key, boundedOptions, shared = true), request.group).flatMap {
        case ForkGrant.Spawn(id) =>
          Deferred[IO, Either[Throwable, SharedProjectSession]].flatMap { fresh =>
            sharedSlots.update(_ + (id -> SharedSlot(fresh, refCount = 1))) >>
              spawnJvm(
                id,
                request.label,
                key,
                request.jvmCommand,
                request.classpath,
                boundedOptions,
                request.runnerClass,
                request.environment,
                request.effectiveWorkingDirectory
              )
                .flatMap(SharedProjectSession.start)
                .attempt
                .flatMap { outcome =>
                  // Drop the slot on failure so a later suite can try again; the waiters parked on this Deferred get the same failure and fail their own acquire,
                  // so none of them will release (Resource.make only releases what it acquired).
                  val cleanup = outcome match {
                    case Left(_)  => sharedSlots.update(_ - id)
                    case Right(_) => IO.unit
                  }
                  cleanup >> fresh.complete(outcome) >> IO.fromEither(outcome)
                }
          }
        case ForkGrant.Reuse(id) =>
          awaitSlot(id).flatMap { slot =>
            sharedSlots.update(_.updatedWith(id)(_.map(s => s.copy(refCount = s.refCount + 1)))) >>
              IO(idle.remove(id): Unit) >>
              slot.session.get.flatMap(IO.fromEither).flatMap { session =>
                if (session.jvm.isAlive) IO(listener.onForkReused(session.jvm.process.pid(), request.label)).attempt.as(session)
                else destroy(session.jvm, "bleep: pooled shared JVM found dead") >> obtainShared(acquirer, request, key, boundedOptions)
              }
          }
      }

    /** The slot the creator registers before spawning. A `Reuse` is granted only for a fork the scheduler already counts, so the slot exists or is a few
      * microseconds from existing; waiting out that window is bounded so a missing slot is a reported bug, not a hang.
      */
    private def awaitSlot(id: ForkId): IO[SharedSlot] = {
      def loop(remaining: Int): IO[SharedSlot] =
        sharedSlots.get.map(_.get(id)).flatMap {
          case Some(slot)             => IO.pure(slot)
          case None if remaining == 0 =>
            IO.raiseError(new IllegalStateException(s"the scheduler granted shared fork ${id.value} for reuse, but this pool has no session on it"))
          case None => IO.sleep(10.millis) >> loop(remaining - 1)
        }
      loop(1000)
    }

    /** A suite is done with the shared fork. The session stays alive — the next suite of the project joins it — and when the last suite leaves, the fork is
      * idle: the scheduler keeps it warm while the project has suites still to start, and evicts it otherwise.
      */
    private def releaseShared(session: SharedProjectSession, cpu: Int): IO[Unit] =
      sharedSlots
        .modify { slots =>
          slots.get(session.jvm.forkId) match {
            case Some(slot) => (slots.updated(session.jvm.forkId, slot.copy(refCount = slot.refCount - 1)), slot.refCount - 1)
            case None       => (slots, 0) // destroyed under us
          }
        }
        .flatMap { remaining =>
          if (!session.jvm.isAlive) destroy(session.jvm, "bleep: shared JVM died under its suites")
          else
            (if (remaining == 0) IO(idle.put(session.jvm.forkId, session.jvm): Unit) else IO.unit) >> IO(lifecycle.workFinished(session.jvm.forkId, cpu))
        }

    /** A [[TestSession]] over one fork that runs several suites at once.
      *
      * A single reader fiber pulls the fork's response lines off the socket and routes each to the queue of the suite it names — every response carries its
      * suite, so no protocol change is needed to tell concurrent suites apart. `runSuite` registers a queue, sends the RunSuite, and streams from that queue
      * until the suite's terminal; different suites call it concurrently. A fork-level Error (its death) is broadcast to every in-flight suite. Cancelling one
      * suite's stream sends CancelSuite for it, interrupting just that suite's thread in the fork and leaving its siblings running.
      */
    private class SharedProjectSession private (
        val jvm: ManagedJvm,
        reader: FiberIO[Unit],
        queues: TrieMap[String, Queue[IO, TestProtocol.TestResponse]],
        threadDumps: Queue[IO, TestProtocol.TestResponse.ThreadDump]
    ) extends TestSession {

      override def pid: Long = jvm.process.pid()

      override def runSuites(
          classNames: List[String],
          parallelism: Int,
          selection: FrameworkSelection,
          args: List[String]
      ): Stream[IO, TestProtocol.TestResponse] =
        // A shared session multiplexes many independent suites; a batched one-execution run is the other model (an exclusive fork), and mixing them on the same
        // fork would have two owners of the response stream. The batch path never acquires a shared session, so reaching here is a routing bug.
        Stream.raiseError[IO](new IllegalStateException("runSuites (one-execution batch) must run on an exclusive fork, not a shared per-project session"))

      override def runSuite(className: String, selection: FrameworkSelection, args: List[String]): Stream[IO, TestProtocol.TestResponse] =
        Stream
          .eval {
            for {
              q <- Queue.unbounded[IO, TestProtocol.TestResponse]
              // Register the queue BEFORE the command, so a response cannot arrive before there is somewhere to route it.
              _ <- IO(queues.put(className, q))
              _ <- sendCommand(TestProtocol.TestCommand.RunSuite(className, selection, args))
            } yield q
          }
          .flatMap { q =>
            Stream
              .fromQueueUnterminated(q)
              .takeThrough {
                case _: TestProtocol.TestResponse.SuiteDone => false
                case _: TestProtocol.TestResponse.Error     => false
                case _                                      => true
              }
              .onFinalizeCase {
                // Clean exit: the terminal was consumed, just unregister. Cancelled/errored mid-suite: tell the fork to interrupt THIS suite (its siblings
                // keep running), then unregister. Unlike an exclusive fork, we never kill the process here — it belongs to the whole project.
                case Resource.ExitCase.Succeeded => IO(queues.remove(className)).void
                case _                           => sendCommand(TestProtocol.TestCommand.CancelSuite(className)).attempt >> IO(queues.remove(className)).void
              }
          }

      private def sendCommand(cmd: TestProtocol.TestCommand): IO[Unit] =
        // Concurrent suites share this one writer; synchronize so two commands cannot interleave mid-line on the socket.
        IO.blocking {
          jvm.stdin.synchronized {
            jvm.stdin.println(TestProtocol.encodeCommand(cmd))
            jvm.stdin.flush()
          }
        }

      override def getThreadDump: IO[Option[TestProtocol.TestResponse.ThreadDump]] =
        (sendCommand(TestProtocol.TestCommand.GetThreadDump) >> threadDumps.take.map(Some(_))).timeout(5.seconds).handleError(_ => None)

      override def dumpThreads: IO[List[String]] =
        IO.blocking(jvm.dumpThreads())

      override def drainStderr: IO[List[String]] =
        IO.blocking {
          val output = jvm.readStderr()
          if (output.isEmpty) Nil else output.split('\n').toList
        }

      override def isAlive: IO[Boolean] =
        IO(jvm.isAlive)

      override def kill: IO[Unit] =
        // Kills the whole fork — every suite on it. Used only when the session as a whole is being killed (a shared fork does not idle-timeout on one suite;
        // that cancels the suite via CancelSuite instead, below).
        IO.blocking(jvm.kill("bleep: explicit kill of shared project fork", graceMillis = 10000))

      override def killSuite(className: String): IO[Unit] =
        // The point of a shared fork: stop one suite without touching its siblings. CancelSuite interrupts just that suite's thread in the fork; the process
        // and the other suites on it keep running. If the interrupt does not take (a genuinely wedged thread) the suite stays stuck, but the fork is still
        // reclaimed when the project's last suite releases the session.
        sendCommand(TestProtocol.TestCommand.CancelSuite(className)).attempt.void

      /** Reap the reader once the fork is dead. Called from the pool's `destroy`, after the kill: the reader is blocked in a socket `readLine`, which no
        * `Thread.interrupt` reaches, so cancelling first would hang on a still-open socket. Destroying closes the socket, the `readLine` returns end-of-stream,
        * the reader loop ends on its own, and the `cancel` that follows just reaps an already-finished fiber.
        */
      def reap: IO[Unit] = reader.cancel
    }

    private object SharedProjectSession {
      def start(jvm: ManagedJvm): IO[SharedProjectSession] =
        for {
          queues <- IO(new TrieMap[String, Queue[IO, TestProtocol.TestResponse]]())
          threadDumps <- Queue.unbounded[IO, TestProtocol.TestResponse.ThreadDump]
          fiber <- readerLoop(jvm, queues, threadDumps).start
        } yield new SharedProjectSession(jvm, fiber, queues, threadDumps)

      /** Read the fork's response lines forever, routing each to the suite it names. Ends when the socket does (fork death), which it reports to every
        * in-flight suite so none hangs waiting for a terminal that will never come.
        */
      private def readerLoop(
          jvm: ManagedJvm,
          queues: TrieMap[String, Queue[IO, TestProtocol.TestResponse]],
          threadDumps: Queue[IO, TestProtocol.TestResponse.ThreadDump]
      ): IO[Unit] = {
        def broadcast(err: TestProtocol.TestResponse.Error): IO[Unit] =
          IO(queues.values.toList).flatMap(_.traverse_(_.offer(err)))

        def route(resp: TestProtocol.TestResponse): IO[Unit] =
          resp match {
            case TestProtocol.TestResponse.Ready          => IO.unit // consumed at spawn; never seen mid-session
            case td: TestProtocol.TestResponse.ThreadDump => threadDumps.offer(td)
            case e: TestProtocol.TestResponse.Error       => broadcast(e) // fork-level, names no suite: every suite on this JVM is affected
            case other                                    =>
              suiteOf(other) match {
                case Some(suite) => queues.get(suite).fold(IO.unit)(_.offer(other)) // no subscriber => drop (a late or unattributed line)
                case None        => IO.unit // a null-suite Log: unattributable console noise, dropped
              }
          }

        // A read returning null (clean EOF) and a read THROWING (the socket closed under it — exactly what `teardown` does to unblock this reader) mean the same
        // thing: no more lines. Fold them together with `.attempt` so a socket-closed exception is an ordinary end of stream, not an escaped failure that leaves
        // in-flight suites hanging.
        def endOfStream: IO[Unit] =
          IO(jvm.markDead()) >> IO
            .blocking {
              val pid = jvm.process.pid()
              val exit = JvmPool.describeExit(jvm.process, jvm.killedByUs)
              TestProtocol.TestResponse.Error(s"Shared test JVM (pid=$pid) died unexpectedly (${exit.summary})", exit.detail)
            }
            .flatMap(broadcast)

        def loop: IO[Unit] =
          IO.interruptible(jvm.stdout.readLine()).attempt.flatMap {
            case Left(_) | Right(null) => endOfStream
            case Right(line)           =>
              TestProtocol.decodeResponse(line) match {
                case Right(resp) => route(resp) >> loop
                case Left(err)   =>
                  // A garbled line means the one shared stream is corrupt; there is no per-suite recovery, so fail every in-flight suite and stop.
                  IO(jvm.markProtocolDirty()) >> broadcast(TestProtocol.TestResponse.Error(s"Protocol error: ${err.getMessage}", Some(s"Line: $line")))
              }
          }

        loop
      }

      private def suiteOf(resp: TestProtocol.TestResponse): Option[String] =
        resp match {
          case ts: TestProtocol.TestResponse.TestStarted  => Some(ts.suite)
          case tf: TestProtocol.TestResponse.TestFinished => Some(tf.suite)
          case sd: TestProtocol.TestResponse.SuiteDone    => Some(sd.suite)
          case l: TestProtocol.TestResponse.Log           => l.suite
          case _                                          => None
        }
    }
  }
}
