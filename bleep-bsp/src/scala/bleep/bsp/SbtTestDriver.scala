package bleep.bsp

import bleep.bsp.TestRunnerTypes.{RunnerEvent, TerminationReason, TestEventHandler, TestResult, TestSuite}
import bleep.bsp.protocol.{OutputChannel, TestStatus}
import scala.jdk.CollectionConverters._

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

    // A TestAdapter delivers its events on its own thread, and the counts are read back on this one. Plain mutable collections would leave the reader seeing
    // an empty map even after every event had been handled.
    val suiteCounts = new java.util.concurrent.ConcurrentHashMap[String, SuiteCounts]()

    /** Report every event from one task against that task's own suite.
      *
      * A framework is free to put whatever it likes in `Event.fullyQualifiedName`. munit puts the suite name and the test name joined by a dot, which is not a
      * suite name at all. The `TaskDef` the framework handed back does name the suite, and it is the same for every event the task fires.
      */
    def eventHandlerFor(suiteName: String) = new sbt.testing.EventHandler {
      def handle(event: sbt.testing.Event): Unit = {
        val reportedName = event.selector() match {
          case ts: sbt.testing.TestSelector       => ts.testName()
          case ns: sbt.testing.NestedTestSelector => ns.testName()
          case _                                  => event.fullyQualifiedName()
        }
        val testName = stripSuitePrefix(suiteName, reportedName)

        eventHandler.onTestStarted(suiteName, testName)

        def count(add: SuiteCounts => SuiteCounts): Unit =
          suiteCounts.compute(suiteName, (_, existing) => add(Option(existing).getOrElse(SuiteCounts.empty))): Unit

        val status = event.status() match {
          case sbt.testing.Status.Success =>
            count(counts => counts.copy(passed = counts.passed + 1))
            TestStatus.Passed
          case sbt.testing.Status.Failure =>
            count(counts => counts.copy(failed = counts.failed + 1))
            TestStatus.Failed
          case sbt.testing.Status.Error =>
            count(counts => counts.copy(failed = counts.failed + 1))
            TestStatus.Error
          case sbt.testing.Status.Skipped =>
            count(counts => counts.copy(skipped = counts.skipped + 1))
            TestStatus.Skipped
          case sbt.testing.Status.Ignored =>
            count(counts => counts.copy(ignored = counts.ignored + 1))
            TestStatus.Ignored
          case sbt.testing.Status.Canceled =>
            count(counts => counts.copy(skipped = counts.skipped + 1))
            TestStatus.Cancelled
          case sbt.testing.Status.Pending =>
            count(counts => counts.copy(skipped = counts.skipped + 1))
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

    val startedSuites = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()

    def executeTasks(toRun: Array[sbt.testing.Task]): Unit =
      toRun.foreach { task =>
        val suiteName = task.taskDef().fullyQualifiedName()
        if (startedSuites.add(suiteName)) {
          eventHandler.onSuiteStarted(suiteName)
        }
        executeTasks(task.execute(eventHandlerFor(suiteName), sbtLoggers))
      }

    executeTasks(tasks)

    // done() is the barrier that flushes events. A Scala.js TestAdapter reports its events over a socket, and Task.execute can return before the last one
    // arrives. Counting suites before this call loses those events.
    // A Runner.done() can also throw once every test has already run. Scala Native's RPC handler does when the linked binary was built without the done
    // opcode. Discarding the throw keeps the results that are already in hand.
    try runner.done()
    catch { case _: Exception => () }

    startedSuites.forEach { name =>
      val counts = Option(suiteCounts.get(name)).getOrElse(SuiteCounts.empty)
      eventHandler.onSuiteFinished(name, counts.passed, counts.failed, counts.skipped)
    }

    eventHandler.onRunnerEvent(RunnerEvent.ProcessExited(0))

    val totals = suiteCounts.values.asScala.foldLeft(SuiteCounts.empty)((running, counts) => running.plus(counts))
    TestResult(totals.passed, totals.failed, totals.skipped, totals.ignored, TerminationReason.Completed)
  }

  /** Drop a leading suite name from a reported test name.
    *
    * munit reports `example.ArithmeticSuite.addition adds` where ScalaTest and utest report `addition adds`. Returns the name unchanged when it does not start
    * with the suite, which is the case for every framework that already reports a bare test name.
    */
  private def stripSuitePrefix(suiteName: String, fullyQualifiedName: String): String =
    if (fullyQualifiedName.startsWith(suiteName + ".")) fullyQualifiedName.substring(suiteName.length + 1)
    else fullyQualifiedName

  private case class SuiteCounts(passed: Int, failed: Int, skipped: Int, ignored: Int) {
    def plus(other: SuiteCounts): SuiteCounts =
      SuiteCounts(passed + other.passed, failed + other.failed, skipped + other.skipped, ignored + other.ignored)
  }

  private object SuiteCounts {
    val empty: SuiteCounts = SuiteCounts(0, 0, 0, 0)
  }
}
