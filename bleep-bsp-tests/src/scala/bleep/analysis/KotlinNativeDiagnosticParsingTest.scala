package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** Parsing of what the Kotlin/Native compiler writes to its message stream.
  *
  * These messages had no reader at all until recently: K2Native was invoked through its static `main`, which ends in `System.exit`, so the compiler killed the
  * BSP daemon before anything could be collected and the user saw "BSP server crashed twice" for an ordinary type error. With `exec` the messages arrive on a
  * stream we own, and this is what turns them back into diagnostics.
  */
class KotlinNativeDiagnosticParsingTest extends AnyFunSuite with Matchers {

  private def collect(output: String): List[CompilerError] = {
    val seen = mutable.ListBuffer.empty[CompilerError]
    val listener: DiagnosticListener = (d: CompilerError) => seen += d
    val returned = KotlinNativeCompiler.reportCompilerOutput(output, listener)
    // The listener and the return value must agree — the caller builds its failure from the latter while the former drives live reporting, and a difference
    // between them would show the user two different sets of errors for one compile.
    returned shouldBe seen.toList
    returned
  }

  test("the format Kotlin/Native actually emits is parsed") {
    // Verbatim from a real failing compile through `CLICompiler.exec`: the location leads and the severity follows it. The `e: `-prefixed shapes below are
    // the JVM compiler's; assuming only those is what made every Kotlin/Native failure reach the user as a bare "exit code 1".
    val d = collect("/tmp/knprobe/T.kt:2:26: error: return type mismatch: expected 'Int', actual 'String'.")
    d should have size 1
    d.head.line shouldBe 2
    d.head.column shouldBe 26
    d.head.path.map(_.toString) shouldBe Some("/tmp/knprobe/T.kt")
    d.head.severity shouldBe CompilerError.Severity.Error
    d.head.message should include("return type mismatch")
  }

  test("a path-first warning keeps its severity") {
    collect("/tmp/T.kt:1:1: warning: unused").head.severity shouldBe CompilerError.Severity.Warning
  }

  test("a 2.x diagnostic keeps its file, line and column") {
    val d = collect("e: file:///p/T.kt:2:24 Initializer type mismatch: expected 'Int', actual 'String'.")
    d should have size 1
    d.head.line shouldBe 2
    d.head.column shouldBe 24
    d.head.path.map(_.toString) shouldBe Some("/p/T.kt")
    d.head.severity shouldBe CompilerError.Severity.Error
    d.head.message should include("type mismatch")
  }

  test("the older parenthesised location is understood too") {
    val d = collect("e: /p/T.kt: (2, 24): Type mismatch")
    d.head.line shouldBe 2
    d.head.column shouldBe 24
    d.head.path.map(_.toString) shouldBe Some("/p/T.kt")
  }

  test("a link failure carries no location and is still reported") {
    // Verbatim from linking a test project as if it were a library — no `e:` prefix, no file, and the message that used to reach the user only as a crash.
    val d = collect("error: could not find '/main' function.")
    d should have size 1
    d.head.severity shouldBe CompilerError.Severity.Error
    d.head.message shouldBe "could not find '/main' function."
    d.head.path shouldBe None
  }

  test("warnings stay warnings") {
    collect("w: file:///p/T.kt:1:1 unused variable").head.severity shouldBe CompilerError.Severity.Warning
  }

  test("progress chatter is not turned into diagnostics") {
    collect("Compiling 3 files\n\n  linking...\n") shouldBe empty
  }

  test("an unparseable diagnostic is kept rather than dropped") {
    // Kotlin has changed this format before. Losing the message entirely is worse than reporting it without a location.
    val d = collect("e: something we have never seen")
    d should have size 1
    d.head.message shouldBe "something we have never seen"
    d.head.line shouldBe 0
  }
}
