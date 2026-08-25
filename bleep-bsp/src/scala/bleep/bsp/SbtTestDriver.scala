package bleep.bsp

import bleep.bsp.TestRunnerTypes.{RunnerEvent, TerminationReason, TestEventHandler, TestResult, TestSuite}
import bleep.bsp.protocol.{OutputChannel, TestStatus}
import scala.collection.concurrent.TrieMap

/** Runs an `sbt.testing.Framework` and reports every event the framework fires to a bleep `TestEventHandler`.
  *
  * Every platform reaches its `Framework` differently. The JVM loads a `Framework` from a classpath. Scala Native and Scala.js each go through a `TestAdapter`
  * that talks to a separate process. The work of running a `Framework` is the same on all platforms.
  */
object SbtTestDriver {

  /** The fingerprint only satisfies the `TaskDef` constructor. A framework matches on the suite name instead. */
  private val suiteFingerprint: sbt.testing.Fingerprint = new sbt.testing.SubclassFingerprint {
    def superclassName(): String = "java.lang.Object"
    def isModule: Boolean = false
    def requireNoArgConstructor(): Boolean = true
  }

  /** Run each suite the caller asked for and report each event to the handler.
    *
    * @param framework
    *   the framework the platform already loaded
    * @param suites
    *   the suites to run. An empty list asks the framework to discover its own suites.
    * @param eventHandler
    *   bleep reports test progress through this handler
    * @param testClassLoader
    *   the framework builds its runner against this classloader
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

    /** A `TestAdapter` handles its events on its own thread. This `TrieMap` lets the calling thread observe the counts safely. */
    val suiteCounts = TrieMap.empty[String, SuiteCounts]

    /** Report every event from one task against that task's own suite.
      *
      * A framework is free to put whatever it likes in `Event.fullyQualifiedName`. munit puts the suite name and the test name joined by a dot. That string is
      * not a suite name. The `TaskDef` the framework handed back declares the suite. That declaration covers every event the task fires.
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
          suiteCounts.updateWith(suiteName)(existing => Some(add(existing.getOrElse(SuiteCounts.empty)))): Unit

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

    /** The driver has already reported every suite in this map as started. */
    val startedSuites = TrieMap.empty[String, Unit]

    def executeTasks(toRun: Array[sbt.testing.Task]): Unit =
      toRun.foreach { task =>
        val suiteName = task.taskDef().fullyQualifiedName()
        if (startedSuites.putIfAbsent(suiteName, ()).isEmpty) {
          eventHandler.onSuiteStarted(suiteName)
        }
        executeTasks(task.execute(eventHandlerFor(suiteName), sbtLoggers))
      }

    executeTasks(tasks)

    // `done()` is the barrier that flushes events. A Scala.js `TestAdapter` reports its events over a socket, and `Task.execute` can return before the last
    // event arrives.
    // Only a Scala Native `TestAdapter` throws here, when the native process's RPC handler declares no `done` opcode. A Scala.js `TestAdapter` runs this
    // call to completion.
    try runner.done()
    catch { case _: Exception => () }

    startedSuites.keys.foreach { name =>
      val counts = suiteCounts.getOrElse(name, SuiteCounts.empty)
      eventHandler.onSuiteFinished(name, counts.passed, counts.failed, counts.skipped)
    }

    eventHandler.onRunnerEvent(RunnerEvent.ProcessExited(0))

    val totals = suiteCounts.values.foldLeft(SuiteCounts.empty)((running, counts) => running.plus(counts))
    TestResult(totals.passed, totals.failed, totals.skipped, totals.ignored, TerminationReason.Completed)
  }

  /** Drops a leading suite name from a reported test name.
    *
    * munit reports `example.ArithmeticSuite.addition adds` where ScalaTest and utest report `addition adds`. Returns the name unchanged when the name does not
    * start with the suite.
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
