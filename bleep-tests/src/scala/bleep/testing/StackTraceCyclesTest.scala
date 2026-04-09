package bleep.testing

import org.scalactic.TripleEqualsSupport
import org.scalatest.funsuite.AnyFunSuite

class StackTraceCyclesTest extends AnyFunSuite with TripleEqualsSupport {

  test("no cycle — short stack trace passes through unchanged") {
    val input =
      """java.lang.RuntimeException: boom
        |	at com.foo.A.method(A.scala:10)
        |	at com.foo.B.method(B.scala:20)
        |	at com.foo.C.method(C.scala:30)""".stripMargin

    val result = StackTraceCycles.collapse(input)
    assert(result === input.split("\n").toList)
  }

  test("simple 2-frame cycle repeated 5 times") {
    val cycle = List(
      "\tat com.foo.A.methodA(A.scala:10)",
      "\tat com.foo.B.methodB(B.scala:20)"
    )
    val frames = List.fill(5)(cycle).flatten
    val input = ("java.lang.StackOverflowError" :: frames).mkString("\n")

    val result = StackTraceCycles.collapse(input)
    assert(result.head === "java.lang.StackOverflowError")
    assert(result(1) === "\tat com.foo.A.methodA(A.scala:10)")
    assert(result(2) === "\tat com.foo.B.methodB(B.scala:20)")
    assert(result(3).contains("2 frames repeated 5 times"))
    assert(result(3).contains("10 frames total"))
    assert(result.size === 4)
  }

  test("3-frame cycle repeated 4 times") {
    val cycle = List(
      "\tat com.foo.A.a(A.scala:1)",
      "\tat com.foo.B.b(B.scala:2)",
      "\tat com.foo.C.c(C.scala:3)"
    )
    val frames = List.fill(4)(cycle).flatten
    val input = ("java.lang.StackOverflowError" :: frames).mkString("\n")

    val result = StackTraceCycles.collapse(input)
    assert(result.head === "java.lang.StackOverflowError")
    assert(result(1) === "\tat com.foo.A.a(A.scala:1)")
    assert(result(2) === "\tat com.foo.B.b(B.scala:2)")
    assert(result(3) === "\tat com.foo.C.c(C.scala:3)")
    assert(result(4).contains("3 frames repeated 4 times"))
    assert(result(4).contains("12 frames total"))
    assert(result.size === 5)
  }

  test("cycle with trailing non-repeating frames") {
    val cycle = List(
      "\tat com.foo.A.a(A.scala:1)",
      "\tat com.foo.B.b(B.scala:2)"
    )
    val tail = List(
      "\tat com.foo.Main.main(Main.scala:100)",
      "\tat sun.reflect.NativeMethodAccessorImpl.invoke(Native Method)"
    )
    val frames = List.fill(4)(cycle).flatten ++ tail
    val input = ("java.lang.StackOverflowError" :: frames).mkString("\n")

    val result = StackTraceCycles.collapse(input)
    assert(result.head === "java.lang.StackOverflowError")
    assert(result(1) === "\tat com.foo.A.a(A.scala:1)")
    assert(result(2) === "\tat com.foo.B.b(B.scala:2)")
    assert(result(3).contains("2 frames repeated 4 times"))
    // Trailing frames preserved
    assert(result.contains("\tat com.foo.Main.main(Main.scala:100)"))
    assert(result.contains("\tat sun.reflect.NativeMethodAccessorImpl.invoke(Native Method)"))
  }

  test("cycle starting after a few non-repeating frames") {
    val prefix = List(
      "\tat com.foo.Entry.start(Entry.scala:5)"
    )
    val cycle = List(
      "\tat com.foo.A.a(A.scala:1)",
      "\tat com.foo.B.b(B.scala:2)"
    )
    val frames = prefix ++ List.fill(5)(cycle).flatten
    val input = ("java.lang.StackOverflowError" :: frames).mkString("\n")

    val result = StackTraceCycles.collapse(input)
    assert(result.head === "java.lang.StackOverflowError")
    // Prefix frame preserved
    assert(result(1) === "\tat com.foo.Entry.start(Entry.scala:5)")
    // One cycle instance
    assert(result(2) === "\tat com.foo.A.a(A.scala:1)")
    assert(result(3) === "\tat com.foo.B.b(B.scala:2)")
    assert(result(4).contains("2 frames repeated 5 times"))
  }

  test("caused-by sections processed independently") {
    val outerFrames = List(
      "\tat com.foo.Outer.run(Outer.scala:10)",
      "\tat com.foo.Outer.main(Outer.scala:5)"
    )
    val innerCycle = List(
      "\tat com.foo.A.a(A.scala:1)",
      "\tat com.foo.B.b(B.scala:2)"
    )
    val innerFrames = List.fill(4)(innerCycle).flatten
    val lines = List("java.lang.RuntimeException: wrapper") ++
      outerFrames ++
      List("Caused by: java.lang.StackOverflowError") ++
      innerFrames

    val input = lines.mkString("\n")
    val result = StackTraceCycles.collapse(input)

    // Outer exception and frames unchanged
    assert(result(0) === "java.lang.RuntimeException: wrapper")
    assert(result(1) === "\tat com.foo.Outer.run(Outer.scala:10)")
    assert(result(2) === "\tat com.foo.Outer.main(Outer.scala:5)")
    // Caused by header
    assert(result(3) === "Caused by: java.lang.StackOverflowError")
    // Inner cycle collapsed
    assert(result(4) === "\tat com.foo.A.a(A.scala:1)")
    assert(result(5) === "\tat com.foo.B.b(B.scala:2)")
    assert(result(6).contains("2 frames repeated 4 times"))
    assert(result.size === 7)
  }

  test("JVM '... N more' lines are preserved") {
    val input =
      """java.lang.RuntimeException: outer
        |	at com.foo.A.method(A.scala:10)
        |	at com.foo.B.method(B.scala:20)
        |Caused by: java.lang.IllegalStateException: inner
        |	at com.foo.C.method(C.scala:30)
        |	... 15 more""".stripMargin

    val result = StackTraceCycles.collapse(input)
    assert(result === input.split("\n").toList)
  }

  test("fewer than 3 repetitions — no collapsing") {
    val cycle = List(
      "\tat com.foo.A.a(A.scala:1)",
      "\tat com.foo.B.b(B.scala:2)"
    )
    // Only 2 repetitions — not enough to collapse
    val frames = List.fill(2)(cycle).flatten
    val input = ("java.lang.StackOverflowError" :: frames).mkString("\n")

    val result = StackTraceCycles.collapse(input)
    assert(result === input.split("\n").toList)
  }

  test("empty input") {
    val result = StackTraceCycles.collapse("")
    assert(result === List(""))
  }

  test("single-frame cycle repeated many times") {
    val frames = List.fill(100)("\tat com.foo.A.recurse(A.scala:10)")
    val input = ("java.lang.StackOverflowError" :: frames).mkString("\n")

    val result = StackTraceCycles.collapse(input)
    assert(result.head === "java.lang.StackOverflowError")
    assert(result(1) === "\tat com.foo.A.recurse(A.scala:10)")
    assert(result(2).contains("1 frames repeated 100 times"))
    assert(result(2).contains("100 frames total"))
    assert(result.size === 3)
  }

  test("large realistic StackOverflowError") {
    val cycle = List(
      "\tat com.example.Parser.parseExpr(Parser.scala:42)",
      "\tat com.example.Parser.parseTerm(Parser.scala:78)",
      "\tat com.example.Parser.parseFactor(Parser.scala:103)",
      "\tat com.example.Parser.parseExpr(Parser.scala:55)"
    )
    val frames = List.fill(250)(cycle).flatten
    val tail = List(
      "\tat com.example.Main.main(Main.scala:10)",
      "\tat java.base/java.lang.Thread.run(Thread.java:829)"
    )
    val input = ("java.lang.StackOverflowError" :: (frames ++ tail)).mkString("\n")

    val result = StackTraceCycles.collapse(input)
    // Should be: header + 4 cycle frames + indicator + 2 tail frames = 8 lines
    assert(result.size === 8)
    assert(result(5).contains("4 frames repeated 250 times"))
    assert(result(5).contains("1000 frames total"))
    assert(result(6) === "\tat com.example.Main.main(Main.scala:10)")
    assert(result(7) === "\tat java.base/java.lang.Thread.run(Thread.java:829)")
  }
}
