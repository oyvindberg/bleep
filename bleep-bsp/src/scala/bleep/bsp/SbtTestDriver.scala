package bleep.bsp

import bleep.bsp.TestRunnerTypes.{RunnerEvent, TerminationReason, TestEventHandler, TestResult, TestSuite}
import bleep.bsp.protocol.{OutputChannel, TestStatus}

/** Runs an `sbt.testing.Framework` and reports what it does to a bleep `TestEventHandler`.
  *
  * Every platform reaches its `Framework` differently. The JVM loads one off a classpath. Scala Native and Scala.js each go through a `TestAdapter` that talks
  * to a separate process. Once a platform holds a `Framework`, the work of running it is the same everywhere.
  */
object SbtTestDriver {

  /** Every suite bleep asks for is named outright, and the fingerprint only satisfies the `TaskDef` constructor. A framework matches on the name. */
  private val suiteFingerprint: sbt.testing.Fingerprint = new sbt.testing.SubclassFingerprint {
    def superclassName(): String = "java.lang.Object"
    def isModule: Boolean = false
    def requireNoArgConstructor(): Boolean = true
  }

  /** Run every named suite and report each event to the handler.
    *
    * @param framework
    *   the framework, already loaded by whatever the platform uses
    * @param suites
    *   the suites to run. An empty list asks the framework to discover its own.
    * @param eventHandler
    *   the handler bleep reports test progress through
    * @param testClassLoader
    *   the classloader the framework builds its runner against
    * @return
    *   the counts the framework reported, summed across every suite
    */
  def runFramework(
      framework: sbt.testing.Framework,
      suites: List[TestSuite],
      eventHandler: TestEventHandler,
      testClassLoader: ClassLoader
  ): TestResult = {
    val runner = framework.runner(Array.empty[String], Array.empty[String], testClassLoader)

    val taskDefs = suites.map { suite =>
      new sbt.testing.TaskDef(suite.fullyQualifiedName, suiteFingerprint, false, Array(new sbt.testing.SuiteSelector))
    }.toArray

    val tasks = runner.tasks(taskDefs)

    val suiteCounts = new scala.collection.mutable.HashMap[String, SuiteCounts]()

    val sbtEventHandler = new sbt.testing.EventHandler {
      def handle(event: sbt.testing.Event): Unit = {
        val suiteName = event.fullyQualifiedName()
        val testName = event.selector() match {
          case ts: sbt.testing.TestSelector       => ts.testName()
          case ns: sbt.testing.NestedTestSelector => ns.testName()
          case _                                  => event.fullyQualifiedName()
        }

        eventHandler.onTestStarted(suiteName, testName)

        val counts = suiteCounts.getOrElse(suiteName, SuiteCounts.empty)
        val status = event.status() match {
          case sbt.testing.Status.Success =>
            suiteCounts(suiteName) = counts.copy(passed = counts.passed + 1)
            TestStatus.Passed
          case sbt.testing.Status.Failure =>
            suiteCounts(suiteName) = counts.copy(failed = counts.failed + 1)
            TestStatus.Failed
          case sbt.testing.Status.Error =>
            suiteCounts(suiteName) = counts.copy(failed = counts.failed + 1)
            TestStatus.Error
          case sbt.testing.Status.Skipped =>
            suiteCounts(suiteName) = counts.copy(skipped = counts.skipped + 1)
            TestStatus.Skipped
          case sbt.testing.Status.Ignored =>
            suiteCounts(suiteName) = counts.copy(ignored = counts.ignored + 1)
            TestStatus.Ignored
          case sbt.testing.Status.Canceled =>
            suiteCounts(suiteName) = counts.copy(skipped = counts.skipped + 1)
            TestStatus.Cancelled
          case sbt.testing.Status.Pending =>
            suiteCounts(suiteName) = counts.copy(skipped = counts.skipped + 1)
            TestStatus.Pending
        }

        val message = Option(event.throwable()).flatMap { thrown =>
          if (thrown.isDefined) Option(thrown.get().getMessage)
          else None
        }

        eventHandler.onTestFinished(suiteName, testName, status, event.duration(), message)
      }
    }

    val sbtLoggers = Array[sbt.testing.Logger](new sbt.testing.Logger {
      def ansiCodesSupported(): Boolean = false
      def error(msg: String): Unit = eventHandler.onOutput("", msg, OutputChannel.Stderr)
      def warn(msg: String): Unit = eventHandler.onOutput("", msg, OutputChannel.Stdout)
      def info(msg: String): Unit = eventHandler.onOutput("", msg, OutputChannel.Stdout)
      def debug(msg: String): Unit = ()
      def trace(t: Throwable): Unit = eventHandler.onOutput("", t.toString, OutputChannel.Stderr)
    })

    val startedSuites = scala.collection.mutable.Set[String]()

    def executeTasks(toRun: Array[sbt.testing.Task]): Unit =
      toRun.foreach { task =>
        val suiteName = task.taskDef().fullyQualifiedName()
        if (startedSuites.add(suiteName)) {
          eventHandler.onSuiteStarted(suiteName)
        }
        executeTasks(task.execute(sbtEventHandler, sbtLoggers))
      }

    executeTasks(tasks)

    startedSuites.foreach { name =>
      val counts = suiteCounts.getOrElse(name, SuiteCounts.empty)
      eventHandler.onSuiteFinished(name, counts.passed, counts.failed, counts.skipped)
    }

    // A Runner.done() can throw after every test has already run and every count is already recorded. Scala Native's RPC handler is one that does, when the
    // linked binary was built without the done opcode. Discarding the throw keeps the results that are already in hand.
    try runner.done()
    catch { case _: Exception => () }

    eventHandler.onRunnerEvent(RunnerEvent.ProcessExited(0))

    val totals = suiteCounts.values.foldLeft(SuiteCounts.empty)((running, counts) => running.plus(counts))
    TestResult(totals.passed, totals.failed, totals.skipped, totals.ignored, TerminationReason.Completed)
  }

  private case class SuiteCounts(passed: Int, failed: Int, skipped: Int, ignored: Int) {
    def plus(other: SuiteCounts): SuiteCounts =
      SuiteCounts(passed + other.passed, failed + other.failed, skipped + other.skipped, ignored + other.ignored)
  }

  private object SuiteCounts {
    val empty: SuiteCounts = SuiteCounts(0, 0, 0, 0)
  }
}
