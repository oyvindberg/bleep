package bleep.bsp

import bleep.bsp.TestRunnerTypes.{TestEventHandler, TestSuite}
import bleep.bsp.protocol.{OutputChannel, SuiteOutcome, TestStatus}
import bleep.testing.TestProtocol

import java.io.PrintWriter
import java.net.{InetAddress, Socket}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.Properties
import scala.jdk.CollectionConverters.*

/** Runs Scala Native test suites in a forked JVM, so the test binary's own output can be captured.
  *
  * Scala Native's `ProcessRunner` spawns the test binary with `ProcessBuilder.inheritIO()`, hardcoded — its `TestAdapter.Config` offers only `binaryFile`,
  * `envVars` and `logger`, and none of them is a hook for the process's streams. `inheritIO` hands the binary the *JVM's own* file descriptors, and running the
  * adapter inside bleep-bsp meant those were the descriptors of a detached daemon, which go nowhere. A `println` in a Scala Native test was not merely missing
  * from the report: it was written to a file descriptor no one holds the other end of.
  *
  * mill runs the same adapter and does get the output, because mill forks: `inheritIO` there inherits a subprocess whose streams mill owns. This is that same
  * arrangement. The parent spawns this main with piped stdout/stderr, so the binary inherits *this* JVM's descriptors — which are pipes the parent drains into
  * `onOutput`. (bloop sidesteps the question entirely: its `DiscoveredTestFrameworks` has only `Jvm` and `Js`, and it never runs Scala Native tests at all.)
  *
  * Test events travel over a loopback socket rather than stdout, for the same reason [[bleep.testing.runner.ForkedTestRunner]] does: stdout has to stay free
  * for the program under test, or the protocol and the tests' own printing interleave into each other.
  */
object ScalaNativeTestFork {

  /** Keys in the properties file the parent writes. A file rather than argv: a classpath is easily longer than a command line is allowed to be. */
  object Keys {
    val Port = "port"
    val ScalaNativeVersion = "scalaNativeVersion"
    val Binary = "binary"
    val Classpath = "classpath"
    val Suites = "suites"
    val EnvPrefix = "env."
  }

  /** Suite names are separated by commas — a fully qualified name cannot contain one — and classpath entries by the platform's path separator, which is what
    * that separator is for.
    */
  val SuiteSeparator: Char = ','

  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      System.err.println(s"expected exactly one argument, the path to the request properties file, got ${args.length}")
      System.exit(2)
    }

    val props = new Properties()
    val requestFile = Path.of(args(0))
    val in = Files.newInputStream(requestFile)
    try props.load(in)
    finally in.close()

    def required(key: String): String =
      Option(props.getProperty(key)).getOrElse(throw new IllegalArgumentException(s"$requestFile is missing required key '$key'"))

    def list(key: String, separator: Char): List[String] =
      Option(props.getProperty(key)).filter(_.nonEmpty).map(_.split(separator).toList.filter(_.nonEmpty)).getOrElse(Nil)

    val socket = new Socket(InetAddress.getLoopbackAddress, required(Keys.Port).toInt)
    val protocol = new PrintWriter(new java.io.OutputStreamWriter(socket.getOutputStream, StandardCharsets.UTF_8), true)

    def send(response: TestProtocol.TestResponse): Unit = protocol.println(TestProtocol.encodeResponse(response))

    val handler = new TestEventHandler {
      def onTestStarted(suite: String, test: String): Unit =
        send(TestProtocol.TestResponse.TestStarted(suite, test))

      def onTestFinished(suite: String, test: String, status: TestStatus, durationMs: Long, message: Option[String], throwable: Option[String]): Unit =
        // Both fields forwarded. The last is the source location, which this path does not recover.
        send(TestProtocol.TestResponse.TestFinished(suite, test, statusName(status), durationMs, message, throwable, None))

      def onSuiteStarted(suite: String): Unit = ()

      def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit =
        send(TestProtocol.TestResponse.SuiteDone(suite, SuiteOutcome.fromCounts(passed, failed, skipped, 0), 0L))

      /** Straight to this JVM's own streams, which the parent drains — the same pipes the test binary inherits. Sending it over the socket instead would put
        * bleep's relayed output on a different channel from the program's own, and the two would arrive out of order relative to each other.
        */
      def onOutput(suite: String, line: String, channel: OutputChannel): Unit =
        channel match {
          case OutputChannel.Stderr => System.err.println(line)
          case _                    => System.out.println(line)
        }
    }

    val exitCode =
      try {
        val env = props
          .stringPropertyNames()
          .asScala
          .filter(_.startsWith(Keys.EnvPrefix))
          .map(k => k.drop(Keys.EnvPrefix.length) -> props.getProperty(k))
          .toMap

        // The returned counts are deliberately unused: the parent reconstructs them from the events above, which is the same stream the report is built from.
        // Two independent tallies of one run is exactly the sort of disagreement that has bitten this code before.
        val _ = ScalaNativeTestRunner.runTestsViaAdapterBlocking(
          binary = Path.of(required(Keys.Binary)),
          suites = list(Keys.Suites, SuiteSeparator).map(fqn => TestSuite(fqn.split('.').lastOption.getOrElse(fqn), fqn)),
          framework = ScalaNativeTestRunner.TestFramework.Unknown,
          eventHandler = handler,
          env = env,
          scalaNativeVersion = required(Keys.ScalaNativeVersion),
          classpath = list(Keys.Classpath, java.io.File.pathSeparatorChar).map(Path.of(_))
        )
        0
      } catch {
        case t: Throwable =>
          // Reported over the protocol, not just by exiting non-zero: the parent can then attribute the failure instead of inferring "the fork died".
          send(TestProtocol.TestResponse.Error(String.valueOf(t.getMessage), Some(stackTraceOf(t))))
          1
      } finally {
        protocol.flush()
        protocol.close()
        socket.close()
      }

    System.out.flush()
    System.err.flush()
    // Explicit: the Scala Native adapter leaves non-daemon threads behind, and without this the fork lingers after its work is done.
    System.exit(exitCode)
  }

  private def statusName(status: TestStatus): String =
    status match {
      case TestStatus.Passed    => "passed"
      case TestStatus.Failed    => "failed"
      case TestStatus.Ignored   => "ignored"
      case TestStatus.Skipped   => "skipped"
      case TestStatus.Cancelled => "cancelled"
      case other                => other.toString.toLowerCase
    }

  private def stackTraceOf(t: Throwable): String = {
    val sw = new java.io.StringWriter()
    t.printStackTrace(new java.io.PrintWriter(sw))
    sw.toString
  }
}
