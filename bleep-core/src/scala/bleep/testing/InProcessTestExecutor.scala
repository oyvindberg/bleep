package bleep.testing

import cats.effect._
import fs2.Stream

import java.net.{URL, URLClassLoader}
import java.nio.file.Path
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import java.util.concurrent.{ConcurrentHashMap, Executors, LinkedBlockingQueue, ThreadFactory}
import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters._

/** Runs test suites inside the bleep-bsp server, with no fork at all.
  *
  * A suite costs a classloader and a thread here, against a JVM start and a socket handshake in [[JvmPool]] — the difference a run notices is seconds per
  * distinct classpath, and it is why this exists. What it costs instead is the process boundary, and the losses are specific rather than theoretical:
  *
  *   - **`System.exit` takes the server with it.** The forked runner installs a `SecurityManager` to turn a test's exit into a suite error; JEP 486 made that
  *     permanently unavailable from JDK 24, which is what bleep runs on. There is no supported way to stop it in process. A test that exits kills the daemon
  *     and every other workspace's build with it.
  *   - **A wedged suite cannot be killed.** [[kill]] interrupts the suite's thread, and a thread that ignores interruption keeps running. The forked path
  *     destroys a process and is done; here the idle timeout fires, the suite is reported, and the thread stays.
  *   - **Global state is shared.** System properties, the default locale and time zone, `System.out`, singletons, whatever a framework caches statically: one
  *     copy, shared by every suite running concurrently and by the server itself.
  *   - **Output is not attributed per suite.** `System.out` is one stream for the whole JVM, so with suites running concurrently there is no way to say which
  *     one printed a line without hijacking it globally and dispatching by thread. Not done here: what a test prints reaches the daemon's own stdout, and
  *     [[InProcessSession.drainStderr]] has nothing to hand back.
  *
  * None of that is dressed up as a degraded mode: a request naming `jvmOptions`, an environment or a working directory is refused, because those are properties
  * of a process and there is no process to set them on. Silently running the tests without settings the user asked for is the failure mode this refuses to
  * have.
  *
  * Every suite still runs its own [[SuiteRunner]] instance loaded from the project's own classpath, so framework loading, fingerprint ordering and event
  * translation are the same code the fork runs, reached the same way, producing the same [[TestProtocol]] lines.
  *
  * @param maxConcurrentSuites
  *   threads kept for running suites. Not a limit on how many the DAG will admit — that is the machine governor's job, and it admits test tasks by CPU. This
  *   only sizes the pool they land on.
  */
class InProcessTestExecutor(maxConcurrentSuites: Int) extends TestExecutor {

  /** The only runner entry point this executor knows how to be. A request naming another one wants something this cannot provide. */
  private val KnownRunnerClass = "bleep.testing.runner.ForkedTestRunner"

  /** One loader per distinct classpath, shared by every suite that runs against it — the analogue of [[JvmPool]] keying its forks by classpath, and the reason
    * a second suite on the same project starts instantly.
    */
  private val loaders = new ConcurrentHashMap[String, URLClassLoader]()

  private val threadCounter = new AtomicInteger(0)

  private val suitePool = Executors.newFixedThreadPool(
    math.max(1, maxConcurrentSuites),
    new ThreadFactory {
      def newThread(r: Runnable): Thread = {
        // Named so a thread dump of a stuck in-process run says which threads are tests. Without a process to point at, the thread name is the only handle a
        // person has on a suite that will not finish.
        val t = new Thread(r, s"bleep-test-inprocess-${threadCounter.incrementAndGet()}")
        t.setDaemon(true)
        t
      }
    }
  )

  private val suiteEc: ExecutionContext = ExecutionContext.fromExecutorService(suitePool)

  override def acquire(request: TestSessionRequest): Resource[IO, TestSession] =
    Resource.eval {
      IO {
        if (request.jvmOptions.nonEmpty)
          sys.error(
            s"${request.label}: in-process test execution cannot honour jvmOptions ${request.jvmOptions.mkString(", ")} — they configure a JVM, and this runs in the one already started. Fork this project's tests, or drop the options."
          )
        if (request.environment.nonEmpty)
          sys.error(
            s"${request.label}: in-process test execution cannot set environment variables (${request.environment.keys.toList.sorted.mkString(", ")}) — a process inherits its environment at start and cannot change its own. Fork this project's tests."
          )
        request.workingDirectory.foreach { wd =>
          sys.error(
            s"${request.label}: in-process test execution cannot set the working directory to $wd — a JVM has exactly one, shared with the server and every other suite. Fork this project's tests."
          )
        }
        if (request.runnerClass != KnownRunnerClass)
          sys.error(s"${request.label}: unknown test runner class ${request.runnerClass}; in-process execution only knows $KnownRunnerClass")
        request.sharing match {
          case SessionSharing.Exclusive   => ()
          case SessionSharing.Shared(key) =>
            sys.error(
              s"${request.label}: in-process test execution cannot honour a per-project shared fork (project '$key') — sharing one fork across a project's suites is a property of a forked JVM, and this runs in the server. Fork this project's tests."
            )
        }

        new InProcessSession(loaderFor(request.classpath), suiteEc)
      }
    }

  /** The classpath, loaded flat, under the platform loader.
    *
    * Flat and platform-parented on purpose. Nothing of bleep's is visible to the tests: not its Scala library (the project's may be 2.12, 2.13 or 3), not its
    * cats-effect, not its copy of the test frameworks. The only types that cross between the server and a suite are `java.*` ones — a `Consumer<String>` going
    * in and encoded protocol lines coming back — so there is no shared bleep class to conflict, and no equivalent of sbt's layering question to get wrong.
    *
    * sbt reaches the same place from the other direction: its `Flat` strategy is documented as the one to use when layering causes trouble, and layering exists
    * there to reuse loaded classes across runs. Here the loader is already reused across suites by being cached per classpath, so layering would buy the one
    * thing it is for and cost the isolation.
    */
  private def loaderFor(classpath: List[Path]): URLClassLoader =
    loaders.computeIfAbsent(
      classpath.map(_.toString).mkString(java.io.File.pathSeparator),
      _ => {
        val urls: Array[URL] = classpath.map(_.toUri.toURL).toArray
        new URLClassLoader(urls, ClassLoader.getPlatformClassLoader)
      }
    )

  override def shutdown: IO[Unit] =
    IO {
      loaders.values().asScala.foreach { loader =>
        // A loader that will not close keeps its jars mapped; say so rather than leaving the run to wonder why the files are locked on Windows.
        loader.close()
      }
      loaders.clear()
      suitePool.shutdownNow()
      ()
    }

  override def size: IO[Int] = IO(loaders.size())
}

object InProcessTestExecutor {

  /** Marks the end of a suite's response stream. Identity-compared, so it can never collide with a response. */
  private[testing] object EndOfSuite
}

/** One classpath's worth of in-process test execution.
  *
  * Not exclusive, unlike a forked [[TestJvm]]: several sessions share a loader and run concurrently, which is the point — the DAG admits N test tasks and all N
  * of them run here at once.
  */
private class InProcessSession(loader: URLClassLoader, suiteEc: ExecutionContext) extends TestSession {

  /** The thread currently running a suite, so [[kill]] has something to interrupt and [[dumpThreads]] something to point at. */
  private val runningThread = new AtomicReference[Thread](null)

  /** The server's own pid. The tests really do run in this process, so this is the truth rather than a stand-in. */
  override val pid: Long = ProcessHandle.current().pid()

  override def runSuite(
      className: String,
      selection: FrameworkSelection,
      args: List[String]
  ): Stream[IO, TestProtocol.TestResponse] = {
    // Encoded, not passed as objects. The command crosses into a classloader that shares none of bleep's types with this one, and the same encoding is what
    // goes down the socket to a fork — so the two paths are handed byte-identical instructions and decoded by the same decoder on the way back.
    val commandLine = TestProtocol.encodeCommand(TestProtocol.TestCommand.RunSuite(className, selection, args))

    Stream.eval(IO(new LinkedBlockingQueue[AnyRef]())).flatMap { queue =>
      val sink: java.util.function.Consumer[String] = (line: String) =>
        queue.put(TestProtocol.decodeResponse(line) match {
          case Right(response) => response
          case Left(err)       => TestProtocol.TestResponse.Error(s"Protocol error: ${err.getMessage}", Some(s"Line: $line"))
        })

      val runSuiteOnThread = IO
        .interruptible {
          runningThread.set(Thread.currentThread())
          try {
            val runnerClass = loader.loadClass("bleep.testing.runner.SuiteRunner")
            val ctor = runnerClass.getConstructor(classOf[java.util.function.Consumer[?]], classOf[ClassLoader], classOf[java.util.List[?]])
            val runner = ctor.newInstance(sink, loader, java.util.Collections.emptyList[java.io.Flushable]())
            runnerClass.getMethod("runSerialized", classOf[String]).invoke(runner, commandLine)
            ()
          } catch {
            case t: Throwable =>
              // The suite never got to report itself, so nothing else will. Emitted as the same terminal Error a dead fork produces, which the caller already
              // knows how to turn into a failed suite rather than a green one.
              val cause = t match {
                case e: java.lang.reflect.InvocationTargetException if e.getCause != null => e.getCause
                case other                                                                => other
              }
              queue.put(
                TestProtocol.TestResponse.Error(
                  s"in-process runner failed for $className: ${cause.getClass.getName}: ${cause.getMessage}",
                  Some(stackTraceOf(cause))
                )
              )
          } finally {
            runningThread.set(null)
            queue.put(InProcessTestExecutor.EndOfSuite)
          }
        }
        .evalOn(suiteEc)

      Stream
        .eval(runSuiteOnThread.start)
        .flatMap { fiber =>
          Stream
            .repeatEval(IO.interruptible(queue.take()))
            .takeWhile(_ ne InProcessTestExecutor.EndOfSuite)
            .map(_.asInstanceOf[TestProtocol.TestResponse])
            .onFinalize(fiber.cancel)
        }
    }
  }

  private def stackTraceOf(t: Throwable): String = {
    val sw = new java.io.StringWriter()
    t.printStackTrace(new java.io.PrintWriter(sw))
    sw.toString
  }

  /** Read straight off this JVM's own threads. No `jstack` and no attach: the threads in question are ours. */
  override def getThreadDump: IO[Option[TestProtocol.TestResponse.ThreadDump]] =
    dumpThreads.map(lines => if (lines.isEmpty) None else Some(TestProtocol.TestResponse.ThreadDump(threadDumpEntries)))

  override def dumpThreads: IO[List[String]] =
    IO(threadDumpEntries.flatMap(entry => s"\"${entry.name}\" ${entry.state}" :: entry.stackTrace.map("\tat " + _)))

  private def threadDumpEntries: List[TestProtocol.TestResponse.ThreadInfo] =
    Thread.getAllStackTraces.asScala.toList.map { case (thread, frames) =>
      TestProtocol.TestResponse.ThreadInfo(thread.getName, thread.getState.toString, frames.toList.map(_.toString))
    }

  /** Nothing to drain: in process there is no second stream to read, because the suite writes to the server's own stdout. See the class comment on
    * [[InProcessTestExecutor]] — this is a stated loss, not an empty result standing in for one.
    */
  override def drainStderr: IO[List[String]] = IO.pure(Nil)

  /** Always: this session is the server, and if the server were gone nobody would be asking. */
  override def isAlive: IO[Boolean] = IO.pure(true)

  /** Interrupt the suite's thread. Best-effort by construction — a thread that does not check for interruption keeps running, and there is no process to
    * destroy instead. The caller's idle timeout still reports the suite; what it cannot do is reclaim the thread.
    */
  override def kill: IO[Unit] =
    IO {
      val thread = runningThread.get()
      if (thread != null) thread.interrupt()
    }

  override def killSuite(className: String): IO[Unit] =
    // Each in-process session runs a single suite on its own thread, so stopping that suite is the same interrupt as `kill`.
    kill

  override def runSuites(classNames: List[String], parallelism: Int, selection: FrameworkSelection): Stream[IO, TestProtocol.TestResponse] =
    // In-process runs one suite per session on its own thread; a batched one-execution run belongs to a forked JVM, where reuse across classes is worth having.
    Stream.raiseError[IO](new IllegalStateException("runSuites (one-execution batch) is a forked-JVM path; in-process runs suites individually"))
}
