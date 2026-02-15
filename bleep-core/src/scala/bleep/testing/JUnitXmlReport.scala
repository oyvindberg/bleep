package bleep.testing

import java.nio.file.{Files, Path}
import java.time.{Instant, ZoneId}
import java.time.format.DateTimeFormatter

/** JUnit XML report generation from build events.
  *
  * Generates standard JUnit XML reports compatible with CI systems (Jenkins, GitHub Actions, GitLab CI, etc.).
  *
  * Output: one file per project, named `TEST-<project>.xml`, each containing a `<testsuites>` root with one `<testsuite>` per test class.
  */
@annotation.nowarn("msg=unused value")
object JUnitXmlReport {

  /** Collected result for a single test */
  case class TestCaseResult(
      name: String,
      className: String,
      timeSeconds: Double,
      status: TestStatus,
      message: Option[String],
      throwable: Option[String]
  )

  /** Collected result for a test suite (one test class) */
  case class TestSuiteResult(
      name: String,
      project: String,
      timeSeconds: Double,
      timestamp: Instant,
      testCases: List[TestCaseResult],
      systemOut: List[String],
      systemErr: List[String]
  ) {
    def tests: Int = testCases.size
    def failures: Int = testCases.count(tc => tc.status == TestStatus.Failed)
    def errors: Int = testCases.count(tc => tc.status == TestStatus.Error || tc.status == TestStatus.Timeout)
    def skipped: Int = testCases.count(tc =>
      tc.status == TestStatus.Skipped || tc.status == TestStatus.Ignored ||
        tc.status == TestStatus.Pending || tc.status == TestStatus.AssumptionFailed ||
        tc.status == TestStatus.Cancelled
    )
  }

  /** Write JUnit XML reports to the specified directory. Creates one XML file per project. */
  def writeReports(dir: Path, suites: List[TestSuiteResult]): List[Path] = {
    Files.createDirectories(dir)
    val byProject = suites.groupBy(_.project)
    byProject.map { case (project, projectSuites) =>
      val fileName = s"TEST-${sanitizeFileName(project)}.xml"
      val filePath = dir.resolve(fileName)
      val xml = renderTestSuites(project, projectSuites)
      Files.writeString(filePath, xml)
      filePath
    }.toList
  }

  private def renderTestSuites(name: String, suites: List[TestSuiteResult]): String = {
    val sb = new StringBuilder
    sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")

    val totalTests = suites.map(_.tests).sum
    val totalFailures = suites.map(_.failures).sum
    val totalErrors = suites.map(_.errors).sum
    val totalSkipped = suites.map(_.skipped).sum
    val totalTime = suites.map(_.timeSeconds).sum

    sb.append("<testsuites name=\"").append(escapeAttr(name))
    sb.append("\" tests=\"").append(totalTests)
    sb.append("\" failures=\"").append(totalFailures)
    sb.append("\" errors=\"").append(totalErrors)
    sb.append("\" skipped=\"").append(totalSkipped)
    sb.append("\" time=\"").append(formatTime(totalTime))
    sb.append("\">\n")

    suites.sortBy(_.name).foreach(renderTestSuite(sb, _))

    sb.append("</testsuites>\n")
    sb.toString
  }

  private val timestampFormat: DateTimeFormatter =
    DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").withZone(ZoneId.systemDefault())

  private def renderTestSuite(sb: StringBuilder, suite: TestSuiteResult): Unit = {
    sb.append("  <testsuite name=\"").append(escapeAttr(suite.name))
    sb.append("\" tests=\"").append(suite.tests)
    sb.append("\" failures=\"").append(suite.failures)
    sb.append("\" errors=\"").append(suite.errors)
    sb.append("\" skipped=\"").append(suite.skipped)
    sb.append("\" time=\"").append(formatTime(suite.timeSeconds))
    sb.append("\" timestamp=\"").append(timestampFormat.format(suite.timestamp))
    sb.append("\">\n")

    suite.testCases.foreach(renderTestCase(sb, _))

    if (suite.systemOut.nonEmpty) {
      sb.append("    <system-out>")
      appendCdata(sb, suite.systemOut.mkString("\n"))
      sb.append("</system-out>\n")
    }
    if (suite.systemErr.nonEmpty) {
      sb.append("    <system-err>")
      appendCdata(sb, suite.systemErr.mkString("\n"))
      sb.append("</system-err>\n")
    }

    sb.append("  </testsuite>\n")
  }

  private def renderTestCase(sb: StringBuilder, tc: TestCaseResult): Unit = {
    sb.append("    <testcase name=\"").append(escapeAttr(tc.name))
    sb.append("\" classname=\"").append(escapeAttr(tc.className))
    sb.append("\" time=\"").append(formatTime(tc.timeSeconds))
    sb.append("\"")

    tc.status match {
      case TestStatus.Passed =>
        sb.append("/>\n")

      case TestStatus.Failed =>
        sb.append(">\n")
        sb.append("      <failure message=\"").append(escapeAttr(tc.message.getOrElse(""))).append("\"")
        sb.append(" type=\"assertion\">")
        tc.throwable.foreach(t => appendCdata(sb, t))
        sb.append("</failure>\n")
        sb.append("    </testcase>\n")

      case TestStatus.Error | TestStatus.Timeout =>
        sb.append(">\n")
        val errorType = if (tc.status == TestStatus.Timeout) "timeout" else "error"
        sb.append("      <error message=\"").append(escapeAttr(tc.message.getOrElse(""))).append("\"")
        sb.append(" type=\"").append(errorType).append("\">")
        tc.throwable.foreach(t => appendCdata(sb, t))
        sb.append("</error>\n")
        sb.append("    </testcase>\n")

      case TestStatus.Skipped | TestStatus.Ignored | TestStatus.Pending | TestStatus.AssumptionFailed =>
        sb.append(">\n")
        val msg = tc.message.getOrElse(tc.status.toString.toLowerCase)
        sb.append("      <skipped message=\"").append(escapeAttr(msg)).append("\"/>\n")
        sb.append("    </testcase>\n")

      case TestStatus.Cancelled =>
        sb.append(">\n")
        sb.append("      <skipped message=\"cancelled\"/>\n")
        sb.append("    </testcase>\n")
    }
  }

  private def formatTime(seconds: Double): String = f"$seconds%.3f"

  /** Escape text for XML attribute values */
  private def escapeAttr(s: String): String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")

  /** Append text as CDATA, handling the `]]>` edge case */
  private def appendCdata(sb: StringBuilder, text: String): Unit = {
    // CDATA cannot contain ]]> — split it if present
    sb.append("<![CDATA[")
    sb.append(text.replace("]]>", "]]]]><![CDATA[>"))
    sb.append("]]>")
  }

  private def sanitizeFileName(name: String): String =
    name.replaceAll("[^a-zA-Z0-9._-]", "_")
}

/** Collects BuildEvents and produces JUnit XML report data.
  *
  * Thread safety: designed for sequential access from a single event consumer fiber.
  */
class JUnitXmlCollector {
  import JUnitXmlReport._

  private case class SuiteState(
      project: String,
      name: String,
      startTime: Instant,
      testCases: List[TestCaseResult],
      systemOut: List[String],
      systemErr: List[String]
  )

  private val activeSuites = scala.collection.mutable.Map.empty[String, SuiteState]
  private val completedSuites = scala.collection.mutable.ListBuffer.empty[TestSuiteResult]

  /** Process a build event. Call sequentially from the event consumer. */
  def handle(event: BuildEvent): Unit = event match {
    case BuildEvent.SuiteStarted(project, suite, timestamp) =>
      val key = s"$project:$suite"
      activeSuites(key) = SuiteState(project, suite, Instant.ofEpochMilli(timestamp), Nil, Nil, Nil)

    case BuildEvent.TestFinished(project, suite, test, status, durationMs, message, throwable, _) =>
      val key = s"$project:$suite"
      val tc = TestCaseResult(
        name = test,
        className = suite,
        timeSeconds = durationMs / 1000.0,
        status = status,
        message = message,
        throwable = throwable
      )
      activeSuites.get(key).foreach(state => activeSuites(key) = state.copy(testCases = tc :: state.testCases))

    case BuildEvent.SuiteFinished(project, suite, _, _, _, _, durationMs, _) =>
      val key = s"$project:$suite"
      activeSuites.remove(key).foreach { state =>
        completedSuites += TestSuiteResult(
          name = suite,
          project = project,
          timeSeconds = durationMs / 1000.0,
          timestamp = state.startTime,
          testCases = state.testCases.reverse,
          systemOut = state.systemOut,
          systemErr = state.systemErr
        )
      }

    case BuildEvent.Output(project, suite, line, isError, _) =>
      val key = s"$project:$suite"
      activeSuites.get(key).foreach { state =>
        if (isError) activeSuites(key) = state.copy(systemErr = state.systemErr :+ line)
        else activeSuites(key) = state.copy(systemOut = state.systemOut :+ line)
      }

    case BuildEvent.SuiteTimedOut(project, suite, timeoutMs, _, timestamp) =>
      val key = s"$project:$suite"
      val state = activeSuites.remove(key)
      val startTime = state.map(_.startTime).getOrElse(Instant.ofEpochMilli(timestamp))
      val existingTests = state.map(_.testCases.reverse).getOrElse(Nil)
      val timeoutCase = TestCaseResult(
        name = "(timeout)",
        className = suite,
        timeSeconds = timeoutMs / 1000.0,
        status = TestStatus.Timeout,
        message = Some(s"Suite idle timeout after ${timeoutMs / 1000}s"),
        throwable = None
      )
      completedSuites += TestSuiteResult(
        name = suite,
        project = project,
        timeSeconds = timeoutMs / 1000.0,
        timestamp = startTime,
        testCases = existingTests :+ timeoutCase,
        systemOut = state.map(_.systemOut).getOrElse(Nil),
        systemErr = state.map(_.systemErr).getOrElse(Nil)
      )

    case BuildEvent.SuiteError(project, suite, error, exitCode, signal, durationMs, timestamp) =>
      val key = s"$project:$suite"
      val state = activeSuites.remove(key)
      val startTime = state.map(_.startTime).getOrElse(Instant.ofEpochMilli(timestamp))
      val existingTests = state.map(_.testCases.reverse).getOrElse(Nil)
      val desc = signal match {
        case Some(sig) => s"Process crashed (signal $sig)"
        case None =>
          exitCode match {
            case Some(code) => s"Process exited with code $code"
            case None       => error
          }
      }
      val errorCase = TestCaseResult(
        name = "(process error)",
        className = suite,
        timeSeconds = durationMs / 1000.0,
        status = TestStatus.Error,
        message = Some(desc),
        throwable = None
      )
      completedSuites += TestSuiteResult(
        name = suite,
        project = project,
        timeSeconds = durationMs / 1000.0,
        timestamp = startTime,
        testCases = existingTests :+ errorCase,
        systemOut = state.map(_.systemOut).getOrElse(Nil),
        systemErr = state.map(_.systemErr).getOrElse(Nil)
      )

    case BuildEvent.SuiteCancelled(project, suite, reason, timestamp) =>
      val key = s"$project:$suite"
      val state = activeSuites.remove(key)
      val startTime = state.map(_.startTime).getOrElse(Instant.ofEpochMilli(timestamp))
      val existingTests = state.map(_.testCases.reverse).getOrElse(Nil)
      val cancelledCase = TestCaseResult(
        name = "(cancelled)",
        className = suite,
        timeSeconds = 0.0,
        status = TestStatus.Cancelled,
        message = reason,
        throwable = None
      )
      completedSuites += TestSuiteResult(
        name = suite,
        project = project,
        timeSeconds = 0.0,
        timestamp = startTime,
        testCases = existingTests :+ cancelledCase,
        systemOut = state.map(_.systemOut).getOrElse(Nil),
        systemErr = state.map(_.systemErr).getOrElse(Nil)
      )

    case _ => () // Ignore compile events, discovery events, etc.
  }

  /** Finalize and write all reports. Call after all events have been processed. */
  def writeReports(dir: Path): List[Path] = {
    // Finalize any suites that never got a SuiteFinished event
    activeSuites.values.foreach { state =>
      val duration = state.testCases.map(_.timeSeconds).sum
      completedSuites += TestSuiteResult(
        name = state.name,
        project = state.project,
        timeSeconds = duration,
        timestamp = state.startTime,
        testCases = state.testCases.reverse,
        systemOut = state.systemOut,
        systemErr = state.systemErr
      )
    }
    activeSuites.clear()

    JUnitXmlReport.writeReports(dir, completedSuites.toList)
  }
}
