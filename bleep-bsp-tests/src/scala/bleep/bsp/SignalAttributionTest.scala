package bleep.bsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** A SIGKILL description must not name a culprit it cannot know.
  *
  * Regression for a message that asserted "sent by the OS, not by the script or by bleep — almost always the kernel reclaiming memory under pressure". The
  * actual killer was bleep's own daemon-wide child reaping, and a user measured the victim at a flat 160MB RSS with 8GB of RAM free while the message insisted
  * it was memory pressure. `destroyForcibly` and the kernel's OOM killer both deliver signal 9; the signal alone attributes nothing.
  */
class SignalAttributionTest extends AnyFunSuite with Matchers {

  test("SIGKILL is not blamed on the OS or on memory pressure") {
    val msg = SourceGenRunner.describeSignal(9)
    msg should include("SIGKILL")
    // The specific false claims that cost a day of debugging.
    msg should not include "not by bleep"
    msg should not include "not by the script"
    msg.toLowerCase should not include "almost always"
    // It must offer candidates to check rather than a verdict.
    msg.toLowerCase should include("candidates")
    msg.toLowerCase should include("bleep itself")
  }

  test("a small fork on a machine with free memory is explicitly told it was not an OOM") {
    SourceGenRunner.describeSignal(9).toLowerCase should include("it was not an oom")
  }

  test("other signals still say what they know") {
    SourceGenRunner.describeSignal(11) should include("SIGSEGV")
    SourceGenRunner.describeSignal(15) should include("signal 15")
  }
}
