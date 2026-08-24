package bleep.bsp

import bleep.bsp.TestRunnerTypes.{TerminationReason, TestEventHandler, TestSuite}
import bleep.bsp.protocol.{OutputChannel, TestStatus}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable

/** A task that reports one event per name it is given, then hands back the nested tasks it was built with. */
class StubTask(
    suiteName: String,
    events: List[(String, sbt.testing.Status)],
    nested: Array[sbt.testing.Task],
    reportNamesLikeMunit: Boolean
) extends sbt.testing.Task {

  /** munit welds the suite name onto the front of both the event name and the selector's test name. Every other framework reports a bare test name. */
  private def reportedName(testName: String): String =
    if (reportNamesLikeMunit) s"$suiteName.$testName" else testName

  def tags(): Array[String] = Array.empty

  def taskDef(): sbt.testing.TaskDef =
    new sbt.testing.TaskDef(suiteName, StubFramework.fingerprint, false, Array(new sbt.testing.SuiteSelector))

  def execute(handler: sbt.testing.EventHandler, loggers: Array[sbt.testing.Logger]): Array[sbt.testing.Task] = {
    loggers.foreach(_.info(s"running $suiteName"))
    events.foreach { case (testName, eventStatus) =>
      handler.handle(new sbt.testing.Event {
        def fullyQualifiedName(): String = reportedName(testName)
        def fingerprint(): sbt.testing.Fingerprint = StubFramework.fingerprint
        def selector(): sbt.testing.Selector = new sbt.testing.TestSelector(reportedName(testName))
        def status(): sbt.testing.Status = eventStatus
        def throwable(): sbt.testing.OptionalThrowable =
          if (eventStatus == sbt.testing.Status.Failure) new sbt.testing.OptionalThrowable(new AssertionError(s"$testName went wrong"))
          else new sbt.testing.OptionalThrowable()
        def duration(): Long = 7L
      })
    }
    nested
  }
}

class StubRunner(tasksByDef: Array[sbt.testing.TaskDef] => Array[sbt.testing.Task]) extends sbt.testing.Runner {
  var doneWasCalled: Boolean = false

  def args(): Array[String] = Array.empty
  def remoteArgs(): Array[String] = Array.empty
  def tasks(taskDefs: Array[sbt.testing.TaskDef]): Array[sbt.testing.Task] = tasksByDef(taskDefs)

  def done(): String = {
    doneWasCalled = true
    ""
  }
}

/** The driver's contract is the sequence of calls it makes on a `TestEventHandler`. A stub framework pins that sequence with no linker, no binary, and no node
  * process.
  */
class StubFramework(runnerToReturn: StubRunner) extends sbt.testing.Framework {
  def name(): String = "stub"
  def fingerprints(): Array[sbt.testing.Fingerprint] = Array(StubFramework.fingerprint)
  def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader): sbt.testing.Runner = runnerToReturn
}

object StubFramework {
  val fingerprint: sbt.testing.Fingerprint = new sbt.testing.SubclassFingerprint {
    def superclassName(): String = "java.lang.Object"
    def isModule: Boolean = false
    def requireNoArgConstructor(): Boolean = true
  }
}

/** Records every call the driver makes. */
class RecordingHandler extends TestEventHandler {
  val testStarts = mutable.Buffer[(String, String)]()
  val testFinishes = mutable.Buffer[(String, String, TestStatus, Long, Option[String])]()
  val suiteStarts = mutable.Buffer[String]()
  val suiteFinishes = mutable.Buffer[(String, Int, Int, Int)]()
  val outputs = mutable.Buffer[(String, String, OutputChannel)]()

  def onTestStarted(suite: String, test: String): Unit =
    testStarts += ((suite, test))

  def onTestFinished(suite: String, test: String, status: TestStatus, durationMs: Long, message: Option[String]): Unit =
    testFinishes += ((suite, test, status, durationMs, message))

  def onSuiteStarted(suite: String): Unit =
    suiteStarts += suite

  def onSuiteFinished(suite: String, passed: Int, failed: Int, skipped: Int): Unit =
    suiteFinishes += ((suite, passed, failed, skipped))

  def onOutput(suite: String, line: String, channel: OutputChannel): Unit =
    outputs += ((suite, line, channel))
}

class SbtTestDriverTest extends AnyFunSuite with Matchers {

  private def driveOneSuite(events: List[(String, sbt.testing.Status)]): (RecordingHandler, TestRunnerTypes.TestResult) = {
    val task = new StubTask("example.AlphaSuite", events, Array.empty, reportNamesLikeMunit = false)
    val runner = new StubRunner(_ => Array(task))
    val handler = new RecordingHandler()
    val result = SbtTestDriver.runFramework(
      new StubFramework(runner),
      List(TestSuite("AlphaSuite", "example.AlphaSuite")),
      handler,
      getClass.getClassLoader
    )
    (handler, result)
  }

  test("SbtTestDriver: reports suite started, each test finished, and suite finished") {
    val (handler, result) = driveOneSuite(List("addition" -> sbt.testing.Status.Success, "subtraction" -> sbt.testing.Status.Failure))

    handler.suiteStarts shouldBe Seq("example.AlphaSuite")
    handler.testStarts shouldBe Seq(("example.AlphaSuite", "addition"), ("example.AlphaSuite", "subtraction"))
    handler.testFinishes.map(f => (f._2, f._3)) shouldBe Seq(("addition", TestStatus.Passed), ("subtraction", TestStatus.Failed))
    handler.suiteFinishes shouldBe Seq(("example.AlphaSuite", 1, 1, 0))

    result.passed shouldBe 1
    result.failed shouldBe 1
    result.terminationReason shouldBe TerminationReason.Completed
  }

  test("SbtTestDriver: sends the duration and the failure message to the handler") {
    val (handler, _) = driveOneSuite(List("subtraction" -> sbt.testing.Status.Failure))

    val (_, _, _, durationMs, message) = handler.testFinishes.head
    durationMs shouldBe 7L
    message shouldBe Some("subtraction went wrong")
  }

  test("SbtTestDriver: counts every sbt.testing.Status into the right column") {
    val (handler, result) = driveOneSuite(
      List(
        "ok" -> sbt.testing.Status.Success,
        "broken" -> sbt.testing.Status.Failure,
        "blew up" -> sbt.testing.Status.Error,
        "passed over" -> sbt.testing.Status.Skipped,
        "ignored" -> sbt.testing.Status.Ignored,
        "cancelled" -> sbt.testing.Status.Canceled,
        "pending" -> sbt.testing.Status.Pending
      )
    )

    result.passed shouldBe 1
    result.failed shouldBe 2
    result.skipped shouldBe 3
    result.ignored shouldBe 1
    handler.suiteFinishes shouldBe Seq(("example.AlphaSuite", 1, 2, 3))
  }

  test("SbtTestDriver: walks nested tasks and starts each suite once") {
    val grandchild = new StubTask("example.GammaSuite", List("deep" -> sbt.testing.Status.Success), Array.empty, reportNamesLikeMunit = false)
    val child = new StubTask("example.BetaSuite", List("nested" -> sbt.testing.Status.Success), Array(grandchild), reportNamesLikeMunit = false)
    val root = new StubTask("example.AlphaSuite", List("top" -> sbt.testing.Status.Success), Array(child), reportNamesLikeMunit = false)
    val handler = new RecordingHandler()

    val result = SbtTestDriver.runFramework(
      new StubFramework(new StubRunner(_ => Array(root))),
      List(TestSuite("AlphaSuite", "example.AlphaSuite")),
      handler,
      getClass.getClassLoader
    )

    handler.suiteStarts shouldBe Seq("example.AlphaSuite", "example.BetaSuite", "example.GammaSuite")
    result.passed shouldBe 3
    handler.suiteFinishes should contain theSameElementsAs Seq(
      ("example.AlphaSuite", 1, 0, 0),
      ("example.BetaSuite", 1, 0, 0),
      ("example.GammaSuite", 1, 0, 0)
    )
  }

  test("SbtTestDriver: builds one TaskDef per requested suite") {
    var seenDefs = Array.empty[sbt.testing.TaskDef]
    val runner = new StubRunner(defs => {
      seenDefs = defs
      Array.empty
    })

    SbtTestDriver.runFramework(
      new StubFramework(runner),
      List(TestSuite("AlphaSuite", "example.AlphaSuite"), TestSuite("BetaSuite", "example.BetaSuite")),
      new RecordingHandler(),
      getClass.getClassLoader
    )

    seenDefs.map(_.fullyQualifiedName()) shouldBe Array("example.AlphaSuite", "example.BetaSuite")
  }

  test("SbtTestDriver: asks the framework to discover when no suite is named") {
    var seenDefs = Array.empty[sbt.testing.TaskDef]
    val runner = new StubRunner(defs => {
      seenDefs = defs
      Array.empty
    })

    SbtTestDriver.runFramework(new StubFramework(runner), Nil, new RecordingHandler(), getClass.getClassLoader)

    seenDefs shouldBe empty
  }

  test("SbtTestDriver: sends framework logger output to the handler") {
    val (handler, _) = driveOneSuite(List("addition" -> sbt.testing.Status.Success))

    handler.outputs.map(_._2) should contain("running example.AlphaSuite")
  }

  test("SbtTestDriver: calls done on the runner") {
    val task = new StubTask("example.AlphaSuite", List("addition" -> sbt.testing.Status.Success), Array.empty, reportNamesLikeMunit = false)
    val runner = new StubRunner(_ => Array(task))

    SbtTestDriver.runFramework(
      new StubFramework(runner),
      List(TestSuite("AlphaSuite", "example.AlphaSuite")),
      new RecordingHandler(),
      getClass.getClassLoader
    )

    runner.doneWasCalled shouldBe true
  }

  /** munit reports an event as `example.AlphaSuite.addition`, which is not a suite name. */
  test("SbtTestDriver: attributes a munit-shaped event to the task's suite and trims the test name") {
    val task = new StubTask(
      "example.AlphaSuite",
      List("addition" -> sbt.testing.Status.Success, "subtraction" -> sbt.testing.Status.Failure),
      Array.empty,
      reportNamesLikeMunit = true
    )
    val handler = new RecordingHandler()

    val result = SbtTestDriver.runFramework(
      new StubFramework(new StubRunner(_ => Array(task))),
      List(TestSuite("AlphaSuite", "example.AlphaSuite")),
      handler,
      getClass.getClassLoader
    )

    handler.suiteStarts shouldBe Seq("example.AlphaSuite")
    handler.testFinishes.map(_._1) shouldBe Seq("example.AlphaSuite", "example.AlphaSuite")
    handler.testFinishes.map(_._2) shouldBe Seq("addition", "subtraction")
    handler.suiteFinishes shouldBe Seq(("example.AlphaSuite", 1, 1, 0))
    result.passed shouldBe 1
    result.failed shouldBe 1
  }

  /** The counts are read on the calling thread and written on whichever thread the framework calls back on. A `TestAdapter` uses its own. */
  test("SbtTestDriver: counts events handled on another thread") {
    val task = new StubTask("example.AlphaSuite", List("addition" -> sbt.testing.Status.Success), Array.empty, reportNamesLikeMunit = false) {
      override def execute(handler: sbt.testing.EventHandler, loggers: Array[sbt.testing.Logger]): Array[sbt.testing.Task] = {
        val thread = new Thread(() => super.execute(handler, loggers): Unit)
        thread.start()
        thread.join()
        Array.empty
      }
    }
    val handler = new RecordingHandler()

    val result = SbtTestDriver.runFramework(
      new StubFramework(new StubRunner(_ => Array(task))),
      List(TestSuite("AlphaSuite", "example.AlphaSuite")),
      handler,
      getClass.getClassLoader
    )

    result.passed shouldBe 1
    handler.suiteFinishes shouldBe Seq(("example.AlphaSuite", 1, 0, 0))
  }
}
