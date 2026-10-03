package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path
import java.util.Optional

class BleepReporterTest extends AnyFunSuite with Matchers {

  private val noPosition: xsbti.Position = new xsbti.Position {
    def line(): Optional[Integer] = Optional.empty()
    def lineContent(): String = ""
    def offset(): Optional[Integer] = Optional.empty()
    def pointer(): Optional[Integer] = Optional.empty()
    def pointerSpace(): Optional[String] = Optional.empty()
    def sourcePath(): Optional[String] = Optional.empty()
    def sourceFile(): Optional[java.io.File] = Optional.empty()
  }

  private def problem(msg: String, sev: xsbti.Severity): xsbti.Problem = new xsbti.Problem {
    def category(): String = "test"
    def severity(): xsbti.Severity = sev
    def message(): String = msg
    def position(): xsbti.Position = noPosition
  }

  private def reporter(): ZincBridge.BleepReporter =
    new ZincBridge.BleepReporter(DiagnosticListener.noop, Path.of("/build"))

  test("keeps problems in the order they were logged, and knows whether any is an error or a warning") {
    val r = reporter()
    r.hasErrors shouldBe false
    r.hasWarnings shouldBe false

    r.log(problem("first", xsbti.Severity.Info))
    r.hasErrors shouldBe false
    r.hasWarnings shouldBe false

    r.log(problem("second", xsbti.Severity.Warn))
    r.hasWarnings shouldBe true
    r.hasErrors shouldBe false

    r.log(problem("third", xsbti.Severity.Error))
    r.hasErrors shouldBe true

    r.problems().map(_.message()).toList shouldBe List("first", "second", "third")
  }

  test("reset forgets the problems and the error and warning flags") {
    val r = reporter()
    r.log(problem("w", xsbti.Severity.Warn))
    r.log(problem("e", xsbti.Severity.Error))
    r.reset()
    r.problems() shouldBe empty
    r.hasErrors shouldBe false
    r.hasWarnings shouldBe false
  }

  /** A generated project reported ~18k warnings, and appending each to a `List` made logging them quadratic: ~4 GB per compile. 200k problems take milliseconds
    * when logging is linear, and minutes when it is not, so the bound is far from both.
    */
  test("logging is linear in the number of problems") {
    val r = reporter()
    val n = 200000
    val start = System.nanoTime()
    (0 until n).foreach(i => r.log(problem(s"warning $i", xsbti.Severity.Warn)))
    val seconds = (System.nanoTime() - start) / 1e9
    r.problems().length shouldBe n
    seconds should be < 10.0
  }
}
