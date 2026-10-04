package bleep.bsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** How a failed sourcegen is reported.
  *
  * Regression for bug-report-sourcegen-crash-masked-by-jvm-warnings: a script that prints the routine `sun.misc.Unsafe` preamble on every run — including
  * successful ones — had that preamble reported as its crash reason, while the actual cause was discarded. The failure was intermittent, so the only diagnostic
  * anyone ever saw was five unrelated warnings.
  */
class SourceGenFailureMessageTest extends AnyFunSuite with Matchers {

  private val jvmPreamble =
    """WARNING: package sun.misc not in java.base
      |WARNING: A terminally deprecated method in sun.misc.Unsafe has been called
      |WARNING: sun.misc.Unsafe::objectFieldOffset has been called by scala.runtime.LazyVals$
      |WARNING: Please consider reporting this to the maintainers of class scala.runtime.LazyVals$
      |WARNING: sun.misc.Unsafe::objectFieldOffset will be removed in a future release""".stripMargin

  test("routine JVM warnings are not mistaken for a failure reason") {
    SourceGenRunner.meaningfulStderr(jvmPreamble) shouldBe None
  }

  test("a real exception in stderr survives the filtering") {
    val withCause = jvmPreamble + "\njava.lang.IllegalStateException: vendor repo missing\n\tat ai.datoria.Extract.run(Extract.scala:42)"
    val kept = SourceGenRunner.meaningfulStderr(withCause).getOrElse(fail("expected the exception to be kept"))
    kept should include("IllegalStateException: vendor repo missing")
    kept should include("Extract.scala:42")
    kept should not include "sun.misc"
  }

  test("the cause leads, and a SIGKILL is named rather than replaced by warnings") {
    // The reported bug in one line: stderr was preferred whenever it was non-empty, so `signal 9` —
    // the only fact that explained anything — never reached the user.
    val msg = SourceGenRunner.failureMessage("ai.datoria.ExtractDbtMacros", SourceGenRunner.describeSignal(9), "", jvmPreamble)
    msg should include("SIGKILL")
    msg should include("reclaiming memory")
    msg should not include "sun.misc"
    msg should include("no output beyond routine JVM warnings")
  }

  test("stderr is context under the cause, never a substitute for it") {
    val msg = SourceGenRunner.failureMessage("s", "exit code 1", "", jvmPreamble + "\nBoom: it broke")
    msg.indexOf("exit code 1") should be < msg.indexOf("Boom: it broke")
    msg should include("stderr:")
  }

  // Reported building scala3: a script logged why it failed — bleep scripts log to stdout — and exited 1. Only stderr was kept, which held nothing but the
  // JVM preamble, so the summary, the history entry and the server log all said "no stderr beyond routine JVM warnings" and the reason was gone.
  test("a reason the script wrote to stdout reaches the failure message, labelled as stdout") {
    val stdout = "[info] reading test properties\n[error] scala-js test suite checkout not found at /x/y"
    val msg = SourceGenRunner.failureMessage("bleepscripts.SjsTestProperties", "exit code 1", stdout, jvmPreamble)
    msg should startWith("Sourcegen bleepscripts.SjsTestProperties failed: exit code 1")
    msg should include("stdout:\n[info] reading test properties\n[error] scala-js test suite checkout not found at /x/y")
    msg should not include "stderr:"
    msg should not include "sun.misc"
  }

  test("both streams are kept, each under its own label, stdout first") {
    val msg = SourceGenRunner.failureMessage("s", "exit code 1", "logged a reason", jvmPreamble + "\njava.lang.RuntimeException: thrown")
    msg.indexOf("stdout:") should be < msg.indexOf("logged a reason")
    msg.indexOf("logged a reason") should be < msg.indexOf("stderr:")
    msg.indexOf("stderr:") should be < msg.indexOf("RuntimeException: thrown")
  }

  test("only the tail of a long stream is kept, and the message says how much was dropped") {
    val stdout = (1 to 500).map(i => s"line $i").mkString("\n")
    val msg = SourceGenRunner.failureMessage("s", "exit code 1", stdout, "")
    msg should include(s"stdout (last ${SourceGenRunner.TailLines} of 500 lines):")
    msg should include("line 500")
    msg should include(s"line ${500 - SourceGenRunner.TailLines + 1}")
    msg should not include s"line ${500 - SourceGenRunner.TailLines}\n"
  }
}
