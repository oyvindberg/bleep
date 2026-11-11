package bleep.logs

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The shape of a program's output: what `bleep.run` returns in place of the output itself. */
class LogShapeTest extends AnyFunSuite with Matchers {

  private def shapeOf(stream: String, lines: Seq[String]): LogShape.StreamShape = {
    val b = new LogShape.Builder(stream)
    lines.foreach(b.add)
    b.result()
  }

  test("lines that differ only in numbers, paths, quoted strings and hashes share a pattern") {
    val k = LogShape.key("WARNING: PNCompiler: 3 unresolved in /home/me/src/a/B.sql")
    LogShape.key("WARNING: PNCompiler: 17 unresolved in /home/me/src/x/Y.sql") shouldBe k
    LogShape.key("wrote 215 lines to src/generated/Foo.java") shouldBe LogShape.key("wrote 9 lines to src/generated/Bar.java")
    LogShape.key("""loaded "users" in 12ms""") shouldBe LogShape.key("""loaded "orders" in 340ms""")
    LogShape.key("commit 3fa9c1d2e4b5 ok") shouldBe LogShape.key("commit 0b1c2d3e4f5a ok")
    LogShape.key("500/709 eta 3s") shouldBe LogShape.key("501/709 eta 2s")
  }

  test("lines that say different things do not") {
    LogShape.key("[error] value foo is not a member") should not be LogShape.key("[error] not found: type Bar")
    LogShape.key("Compiling 3 files") should not be LogShape.key("Linking 3 files")
  }

  test("a pattern is keyed by its first tokens only, so a long tail does not split it") {
    LogShape.key("at com.example.A.b(A.java:12) extra words that differ one") shouldBe
      LogShape.key("at com.example.A.b(A.java:99) extra words that differ two")
  }

  test("a short stream is returned verbatim") {
    val shape = shapeOf("stdout", List("hello", "done"))
    shape.inline shouldBe Some(List("hello", "done"))
    LogShape.streamJson(shape, droppedLines = 0L).hcursor.get[List[String]]("text") shouldBe Right(List("hello", "done"))
  }

  test("a flood is summarized as patterns with counts and line ranges, and the odd lines come back as rare lines") {
    val lines = (1 to 4000).map(i => s"WARNING: PNCompiler: $i unresolved in /src/F$i.sql") ++
      (1 to 200).map(i => s"wrote $i lines to /out/G$i.java") ++
      List("generated 12 files in 340ms")
    val stdout = shapeOf("stdout", lines)
    val stderr = shapeOf("stderr", List.fill(12)("WARNING: sun.misc.Unsafe::allocateMemory has been called") :+ "Exception in thread \"main\" boom")

    stdout.lines shouldBe 4201
    stdout.patterns.map(_.count) shouldBe List(4000, 200)
    stdout.patterns.head.firstLine shouldBe 1
    stdout.patterns.head.lastLine shouldBe 4000

    val rare = LogShape.pickRare(List(stdout, stderr))
    withClue("the error-like line ranks first, then the most recent: ") {
      rare.map(q => (q.stream, q.line)) shouldBe List(("stderr", 13), ("stdout", 4201))
    }

    val json = LogShape.streamJson(stdout, droppedLines = 0L)
    val patterns = json.hcursor.get[List[String]]("patterns").toOption.get
    patterns.head should startWith("p1 ×4000 L1–4000: WARNING: PNCompiler:")
    json.noSpaces.length should be < 400
  }

  test("only the most frequent patterns are listed; the rest are counted") {
    val lines = ('a' to 'i').flatMap(p => List.fill(4)(s"kind $p event"))
    val shape = shapeOf("stdout", lines)
    shape.patterns should have size LogShape.ShownPatterns
    shape.patternsNotShown shouldBe 9 - LogShape.ShownPatterns
  }

  test("past MaxPatterns, further patterns are counted as other and get pattern id 0") {
    val b = new LogShape.Builder("stdout")
    (1 to LogShape.MaxPatterns).foreach(i => b.add(s"word${"x" * (i % 7)} ${"y" * i}"))
    b.add("something entirely new") shouldBe 0
    b.result().linesInOtherPattern shouldBe 1
  }

  test("timing reports the longest silence, the line that ended it, and how long the program was quiet at the end") {
    val silence = new LogShape.Silence(startedAtMs = 0L)
    silence.onLine("stdout", 1, 100L)
    silence.onLine("stdout", 2, 200L)
    silence.onLine("stderr", 1, 38200L)
    val t = silence.result(endedAtMs = 40200L)
    t.longestSilenceMs shouldBe 38000L
    t.longestSilenceBefore shouldBe Some(("stderr", 1))
    t.silentAtEndMs shouldBe 2000L
    LogShape.timingJson(t).hcursor.get[String]("longestSilenceBefore") shouldBe Right("stderr L1")
  }

  test("a program that printed nothing says so instead of reporting a silence before a line that never came") {
    val t = new LogShape.Silence(startedAtMs = 0L).result(endedAtMs = 5000L)
    t.anyOutput shouldBe false
    LogShape.timingJson(t).hcursor.get[Long]("silentThroughoutMs") shouldBe Right(5000L)
  }
}
