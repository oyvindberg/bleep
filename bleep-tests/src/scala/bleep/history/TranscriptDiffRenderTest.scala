package bleep.history

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** [[TranscriptDiffRender.text]] is a pure function over the diff document [[TranscriptDiff]] produces — these tests feed it documents in exactly that shape
  * and assert on the ANSI-stripped rendering. Colors depend on the JVM-global no-color flag, so assertions strip ANSI rather than assuming either mode.
  */
class TranscriptDiffRenderTest extends AnyFunSuite with Matchers {

  private val AnsiPattern = java.util.regex.Pattern.compile("\\[[0-9;]*[a-zA-Z]")
  private def render(doc: String): String = {
    val json = io.circe.parser.parse(doc) match {
      case Right(json) => json
      case Left(err)   => fail(s"test document does not parse: $err")
    }
    AnsiPattern.matcher(TranscriptDiffRender.text(json)).replaceAll("")
  }

  test("mechanical compile: new diagnostic, status transition, skip chain, projectsAdded") {
    val doc =
      """{
        |  "base": {"historyId": 3, "workspace": "/ws/main"},
        |  "target": {"historyId": 4, "workspace": "/ws/main"},
        |  "mode": "compile",
        |  "identical": false,
        |  "summary": "2 projects changed, 1 only in target",
        |  "changed": [
        |    {
        |      "project": "core",
        |      "reason": {"from": "up-to-date", "to": "incremental"},
        |      "status": {"from": "success", "to": "failed"},
        |      "newDiagnostics": [
        |        {
        |          "severity": "error",
        |          "message": "value withTag is not a member of com.example.Record7",
        |          "path": "/ws/main/core/src/scala/com/example/Record7.scala",
        |          "lines": [10]
        |        }
        |      ]
        |    },
        |    {
        |      "project": "core-test",
        |      "status": {"from": "success", "to": "skipped"},
        |      "skippedBecause": {"from": null, "to": "core"}
        |    }
        |  ],
        |  "projectsAdded": [{"project": "app", "reason": "clean-build"}]
        |}""".stripMargin

    val text = render(doc)
    println(text)

    text should include("compile diff")
    text should include("#3 → #4")
    text should include("2 projects changed, 1 only in target")
    text should include("core  success → failed")
    text should include("reason: up-to-date → incremental")
    text should include("+ error value withTag is not a member of com.example.Record7")
    // paths render relative to the workspace that produced them
    text should include("core/src/scala/com/example/Record7.scala:10")
    text should not include "/ws/main/core/src"
    text should include("core-test  success → skipped (waiting on core)")
    text should include("only in #4:")
    text should include("+ app  (clean-build)")
  }

  test("mechanical test: identical with still-failing tests is not a green all-clear") {
    val doc =
      """{
        |  "base": {"historyId": 4, "workspace": "/ws/main"},
        |  "target": {"historyId": 5, "workspace": "/ws/main"},
        |  "mode": "test",
        |  "identical": true,
        |  "summary": "No logical differences.",
        |  "stillFailing": [
        |    {
        |      "project": "mathy-test",
        |      "suite": "com.example.PricingTest",
        |      "test": "com.example.PricingTest.10 percent off at 100 and above",
        |      "from": "failed",
        |      "to": "failed",
        |      "messageChanged": false,
        |      "message": "expected 216, got 204"
        |    }
        |  ]
        |}""".stripMargin

    val text = render(doc)
    println(text)

    text should include("identical — no logical differences, but 1 test is still failing:")
    text should include("x com.example.PricingTest.10 percent off at 100 and above  (mathy-test)")
  }

  test("mechanical compile: identical renders a single calm line") {
    val doc =
      """{
        |  "base": {"historyId": 5, "workspace": "/ws/main"},
        |  "target": {"historyId": 6, "workspace": "/ws/main"},
        |  "mode": "compile",
        |  "identical": true,
        |  "summary": "No logical differences."
        |}""".stripMargin

    val text = render(doc)
    println(text)

    text should include("identical — no logical differences")
    text should not include "No logical differences." // the summary line is redundant with the identical line
  }

  test("mechanical test: newly failing with multiline message, fixed, added; cross-worktree header") {
    val doc =
      """{
        |  "base": {"historyId": 2, "workspace": "/ws/main"},
        |  "target": {"historyId": 1, "workspace": "/ws/agent"},
        |  "mode": "test",
        |  "crossWorkspace": true,
        |  "identical": false,
        |  "summary": "1 newlyFailing, 1 fixed, 1 added",
        |  "newlyFailing": [
        |    {
        |      "project": "mathy-test",
        |      "suite": "com.example.PricingTest",
        |      "test": "com.example.PricingTest.10 percent off at 100 and above",
        |      "from": "passed",
        |      "to": "failed",
        |      "message": "expected 216\ngot 204"
        |    }
        |  ],
        |  "fixed": [
        |    {
        |      "project": "mathy-test",
        |      "suite": "com.example.PricingTest",
        |      "test": "com.example.PricingTest.no discount below 100",
        |      "from": "failed",
        |      "to": "passed"
        |    }
        |  ],
        |  "added": [
        |    {
        |      "project": "core-test",
        |      "suite": "com.example.AuthTest",
        |      "test": "com.example.AuthTest.sample ids are positive",
        |      "status": "passed"
        |    }
        |  ]
        |}""".stripMargin

    val text = render(doc)
    println(text)

    text should include("test diff")
    text should include("#2 → #1")
    text should include("across worktrees: /ws/main → /ws/agent")
    text should include("newly failing")
    text should include("x com.example.PricingTest.10 percent off at 100 and above  (mathy-test)")
    text should include("      expected 216")
    text should include("      got 204")
    text should include("fixed")
    text should include("+ com.example.PricingTest.no discount below 100  (mathy-test)")
    text should include("added")
    text should include("+ com.example.AuthTest.sample ids are positive  (core-test)")
    text should include("      passed")
  }

  test("timing: totals, sections, alignment, suppression count") {
    val doc =
      """{
        |  "base": {"historyId": 5, "workspace": "/ws/main"},
        |  "target": {"historyId": 6, "workspace": "/ws/main"},
        |  "mode": "test",
        |  "totalBaseMs": 5142,
        |  "totalTargetMs": 1320,
        |  "totalDeltaMs": -3822,
        |  "threshold": "max(50ms, 20% of base)",
        |  "insignificantDeltasSuppressed": 12,
        |  "summary": "total 5142ms -> 1320ms, 1 slower, 1 faster",
        |  "slower": [
        |    {"project": "core-test", "suite": "com.example.S", "test": "com.example.S.slow one", "baseMs": 120, "targetMs": 480, "deltaMs": 360}
        |  ],
        |  "faster": [
        |    {"project": "core-test", "suite": "com.example.S", "test": "com.example.S.was slow", "baseMs": 480, "targetMs": 120, "deltaMs": -360}
        |  ],
        |  "slowestInTarget": [
        |    {"project": "core-test", "suite": "com.example.S", "test": "com.example.S.slow one", "durationMs": 480}
        |  ]
        |}""".stripMargin

    val text = render(doc)
    println(text)

    text should include("timing diff (test)")
    text should include("total 5142ms → 1320ms (-3822ms)")
    text should include("12 insignificant deltas suppressed")
    text should include("slower")
    text should include("120ms →     480ms  (+360ms)  com.example.S.slow one  (core-test)")
    text should include("faster")
    text should include("slowest in #6")
    text should include("480ms  com.example.S.slow one  (core-test)")
  }

  test("rejects documents that are not diffs") {
    val err = intercept[RuntimeException](render("""{"unrelated": true}"""))
    err.getMessage should include("not a diff document")
  }
}
