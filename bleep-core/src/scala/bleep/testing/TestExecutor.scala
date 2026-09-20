package bleep.testing

import cats.effect._
import fs2.Stream

import java.nio.file.Path

/** Whether the session this request asks for is the caller's alone, or shared with the rest of its project's suites.
  *
  * This is what makes [[bleep.model.TestForkMode]] real at the pool: `per-suite` asks for [[Exclusive]] and gets a fork to itself (one suite in flight, the
  * pool reuses forks serially); `per-project` asks for [[Shared]] and every one of the project's suites lands on the SAME fork, which runs them concurrently.
  * The key is what "same" means — all of a project's suites pass the same key. How many run at once is not this type's business: admission decides it (the DAG
  * serialises a project's suites into `maxConcurrentSuites` chains), so a suite that is not allowed to start yet never reaches the pool holding a machine
  * permit.
  */
sealed trait SessionSharing
object SessionSharing {
  case object Exclusive extends SessionSharing
  case class Shared(key: String) extends SessionSharing
}

/** Everything the caller knows about the suite it is about to run, before anything decides *where* it runs.
  *
  * A request, not a JVM description, because half of these fields only mean something to a fork. `jvmOptions`, `environment` and `workingDirectory` are
  * properties of a process, and an executor that is not starting one cannot honour them — see [[InProcessTestExecutor]], which refuses the request rather than
  * running the tests under settings the user asked for and did not get.
  */
case class TestSessionRequest(
    label: String,
    classpath: List[Path],
    jvmOptions: List[String],
    /** Heap for a fork whose `jvmOptions` state no `-Xmx`. Meaningless without a fork: in process the tests share the server's heap. */
    defaultHeapMb: Long,
    runnerClass: String,
    environment: Map[String, String],
    workingDirectory: Option[Path],
    /** Exclusive (a fork to this suite alone) or Shared (this suite's project runs all its suites in one fork). See [[SessionSharing]]. */
    sharing: SessionSharing
)

/** Hands out somewhere to run a test suite.
  *
  * Two implementations, and the difference between them is the process boundary:
  *
  *   - [[JvmPool]] forks JVMs and pools them by classpath, talking to each over a socket. Isolation is the operating system's: a suite that calls
  *     `System.exit`, wedges a thread or corrupts a static kills a process bleep can replace, and `jvmOptions`, environment and working directory are all
  *     honestly settable because there is a process to set them on.
  *   - [[InProcessTestExecutor]] runs suites in the server itself, in a classloader per classpath. No fork to pay for, so a suite starts in milliseconds rather
  *     than seconds — and no process boundary either, which is the whole trade.
  *
  * Both hand back a [[TestSession]] that speaks the same protocol, so `TestRunner` cannot tell them apart. That is deliberate and load-bearing: the forked path
  * encodes [[TestProtocol]] lines onto a socket, and the in-process path hands the identical lines to a queue. One wire format, one decoder, one set of answers
  * to "did this suite run" — a second implementation of framework loading and fingerprint matching would be a second set, and they would drift.
  */
trait TestExecutor {

  /** Somewhere to run suites with this classpath. Released back when the caller is done with it. */
  def acquire(request: TestSessionRequest): Resource[IO, TestSession]

  /** Tear down everything this executor is holding — processes, threads, classloaders.
    *
    * MUST be called. Use `guarantee`.
    */
  def shutdown: IO[Unit]

  /** How many sessions are currently held open. Forked JVMs, or live classloaders. */
  def size: IO[Int]
}

/** Somewhere a suite can be run, obtained from a [[TestExecutor]]. */
trait TestSession {

  /** The OS process the tests actually execute in: a fork's pid, or the server's own when they run in it.
    *
    * Not an identifier for the session — two in-process sessions share this number, because they really are the same process. It exists so a metric or a thread
    * dump names something a person can find with `ps`.
    */
  def pid: Long

  /** Run a test suite and stream back responses. `selection` says *how* to run it, decided where the classpath is known; see [[FrameworkSelection]]. */
  def runSuite(
      className: String,
      selection: FrameworkSelection,
      args: List[String]
  ): Stream[IO, TestProtocol.TestResponse]

  /** Run a whole set of classes of ONE framework through a single execution — for JUnit Platform, one `launcher.execute()`; for sbt test-interface, one
    * `Framework`/`Runner` with all their tasks and one `done()` (maven's `forkCount=1 reuseForks=true`). Streams every class's responses, each tagged with its
    * suite, until the batch terminator. This is what keeps an execution-scoped fixture — an application booted for the run — built once and reused across the
    * classes rather than rebuilt per class, and (for sbt) what stateful frameworks require. `parallelism` bounds how many classes run at once (1 = sequential).
    * Only the forked JVM session honours it; other sessions run suite-by-suite.
    */
  def runSuites(
      classNames: List[String],
      parallelism: Int,
      selection: FrameworkSelection,
      args: List[String]
  ): Stream[IO, TestProtocol.TestResponse]

  /** A thread dump, as the protocol carries it. */
  def getThreadDump: IO[Option[TestProtocol.TestResponse.ThreadDump]]

  /** A thread dump as plain lines, for the summary a timed-out suite prints. Best-effort — never throws. */
  def dumpThreads: IO[List[String]]

  /** Any stderr the session has buffered that did not come through the protocol. Empty where there is no separate stderr to drain. */
  def drainStderr: IO[List[String]]

  /** Is this session still usable? */
  def isAlive: IO[Boolean]

  /** Stop whatever is running here, now. A process to destroy, or a thread to interrupt. */
  def kill: IO[Unit]

  /** Stop ONE suite, by whatever means fits this session, without harming any other suite that shares it.
    *
    * The difference between this and [[kill]] is the whole reason a shared session exists. On an exclusive session there is only the one suite, so stopping it
    * is stopping the session — this is `kill`. On a per-project shared session the fork is running several suites at once, and stopping one must leave the rest
    * alone: it interrupts just that suite's thread in the fork (a `CancelSuite`), never the process. Called when a single suite times out, is cancelled, or is
    * killed — where [[kill]] would take its siblings down with it.
    */
  def killSuite(className: String): IO[Unit]
}
