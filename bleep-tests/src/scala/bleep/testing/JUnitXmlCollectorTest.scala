package bleep.testing

import bleep.bsp.protocol.{ProcessExit, SuiteOutcome}
import bleep.model.{CrossProjectName, ProjectName, SuiteName}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Files

/** What reaches the JUnit XML, which is the only thing CI ever sees. */
class JUnitXmlCollectorTest extends AnyFunSuite {

  private val project = CrossProjectName(ProjectName("mytest"), None)
  private val suite = SuiteName("example.Broken")

  private def report(events: List[BuildEvent]): String = {
    val collector = new JUnitXmlCollector
    events.foreach(collector.handle)
    val dir = Files.createTempDirectory("junit-xml-test")
    val files = collector.writeReports(dir)
    files.map(f => Files.readString(f)).mkString("\n")
  }

  test("a suite that both reports failures with no results and exits non-zero appears once") {
    // Both events arrive for one dying suite. `SuiteFinished` writes its record and takes the suite out of the active map, so appending on `SuiteError` put
    // the same name in the report twice — two `<testsuite>` elements each holding half the story, which a CI dashboard renders as two suites.
    val xml = report(
      List(
        BuildEvent.SuiteStarted(project, suite, 0L),
        BuildEvent.SuiteFinished(project, suite, SuiteOutcome.Executed(passed = 0, failed = 1, skipped = 0, ignored = 0), 10L, 10L),
        BuildEvent.SuiteError(project, suite, "boom", ProcessExit.ExitCode(1), 10L, 11L)
      )
    )

    assert(xml.sliding("<testsuite ".length).count(_ == "<testsuite ") == 1, xml)
    // and it still carries what each half knew
    assert(xml.contains("(process error)"), xml)
    assert(xml.contains("Process exited with code 1"), xml)
  }

  test("a suite that only exits non-zero still gets its own entry") {
    val xml = report(
      List(
        BuildEvent.SuiteStarted(project, suite, 0L),
        BuildEvent.SuiteError(project, suite, "boom", ProcessExit.ExitCode(1), 10L, 11L)
      )
    )

    assert(xml.sliding("<testsuite ".length).count(_ == "<testsuite ") == 1, xml)
    assert(xml.contains("(process error)"), xml)
  }
}
