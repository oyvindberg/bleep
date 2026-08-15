package bleep

import bleep.commands.BuildCreateNew
import bleep.history.{DiffBase, TranscriptStore}
import io.circe.Json

/** `bleep compile --diff` / `bleep test --diff` end-to-end over the (in-process, but production) BSP server: the run happens as normal, the one-line summary
  * still names the history entry, and then ONLY the mechanical diff against the base is printed — followed by the `--timing` hint. Validation happens BEFORE
  * the run: a missing id or a mode mismatch must never cost a compile, which these tests pin by asserting no new history entry was written.
  */
class DiffFlagCliIT extends IntegrationTestHarness {

  private val myapp = model.CrossProjectName(model.ProjectName("myapp"), None)
  private val myappTest = model.CrossProjectName(model.ProjectName("myapp-test"), None)

  private def compileCmd(watch: Boolean, diffBase: Option[DiffBase]): commands.ReactiveBsp =
    commands.ReactiveBsp.compile(
      watch = watch,
      projects = Array(myapp),
      displayMode = commands.DisplayMode.NoTui,
      flamegraph = false,
      cancel = false,
      diffBase = diffBase,
      diffOutput = bleep.OutputMode.Json
    )

  private def testCmd(diffBase: Option[DiffBase]): commands.ReactiveBsp =
    commands.ReactiveBsp.test(
      watch = false,
      projects = Array(myappTest),
      displayMode = commands.DisplayMode.NoTui,
      jvmOptions = Nil,
      testArgs = Nil,
      only = Nil,
      exclude = Nil,
      includeTags = Nil,
      excludeTags = Nil,
      flamegraph = false,
      cancel = false,
      junitReportDir = None,
      diffBase = diffBase,
      diffOutput = bleep.OutputMode.Json,
      clientEnv = bleep.bsp.protocol.BleepBspProtocol.ClientEnv.current()
    )

  /** Capture what `body` printed to stdout via println — the channel the diff JSON and the timing hint use (log lines go through the logger, not here). */
  private def captureOut[A](body: => A): (A, String) = {
    val baos = new java.io.ByteArrayOutputStream()
    val ret = Console.withOut(new java.io.PrintStream(baos, true, "UTF-8"))(body)
    (ret, baos.toString("UTF-8"))
  }

  /** The mechanical diff JSON out of captured stdout — it is the only JSON printed there. */
  private def diffJson(out: String): Json = {
    val start = out.indexOf('{')
    val end = out.lastIndexOf('}')
    assert(start >= 0 && end > start, s"expected diff JSON on stdout, got: $out")
    io.circe.parser.parse(out.substring(start, end + 1)) match {
      case Right(json) => json
      case Left(err)   => fail(s"unparsable diff JSON: $err\n$out")
    }
  }

  private def baseAndTarget(json: Json): (Long, Long) = {
    val base = json.hcursor.downField("base").get[Long]("historyId")
    val target = json.hcursor.downField("target").get[Long]("historyId")
    (base.fold(e => fail(s"no base.historyId: $e"), identity), target.fold(e => fail(s"no target.historyId: $e"), identity))
  }

  integrationTest("compile --diff: bare resolves the previous compile, explicit id pins, missing id fails before running") { ws =>
    ws.bleepNew(BuildCreateNew.Language.Java, "myapp")
    val (started, _, storingLogger) = ws.start()

    // No history yet: bare --diff fails fast, and nothing ran.
    val noHistory = compileCmd(watch = false, diffBase = Some(DiffBase.Previous)).run(started)
    assert(noHistory.left.exists(_.getMessage.contains("No previous compile run recorded")))
    assert(TranscriptStore.list(started.buildPaths).isEmpty)

    compileCmd(watch = false, diffBase = None).run(started).orThrow // #1, clean build

    // Bare --diff resolves the previous compile: clean build vs noop is exactly one reason transition.
    val (res2, out2) = captureOut(compileCmd(watch = false, diffBase = Some(DiffBase.Previous)).run(started))
    res2.orThrow
    val j2 = diffJson(out2)
    assert(baseAndTarget(j2) == ((1L, 2L)))
    assert(j2.hcursor.get[Boolean]("identical") == Right(false))
    assert(out2.contains("timing: bleep history diff 1 2 --timing"))
    // The normal one-line summary still points at the entry.
    assert(storingLogger.underlying.exists(_.message.plainText.contains("History:  #2 (bleep history show 2)")))

    // Identical case: noop vs noop renders identical: true — that IS the payoff line.
    val (res3, out3) = captureOut(compileCmd(watch = false, diffBase = Some(DiffBase.Previous)).run(started))
    res3.orThrow
    val j3 = diffJson(out3)
    assert(baseAndTarget(j3) == ((2L, 3L)))
    assert(j3.hcursor.get[Boolean]("identical") == Right(true))

    // Explicit id pins the base.
    val (res4, out4) = captureOut(compileCmd(watch = false, diffBase = Some(DiffBase.Id(1L))).run(started))
    res4.orThrow
    assert(baseAndTarget(diffJson(out4)) == ((1L, 4L)))

    // Missing/evicted id fails BEFORE running: the store's text, and no new history entry.
    val before = TranscriptStore.list(started.buildPaths)
    val bad = compileCmd(watch = false, diffBase = Some(DiffBase.Id(99L))).run(started)
    assert(bad.left.exists(_.getMessage.contains("No history entry #99")))
    assert(TranscriptStore.list(started.buildPaths) == before)
    succeed
  }

  integrationTest("test --diff: bare skips compile entries to the previous test run; mode mismatch fails before running") { ws =>
    ws.bleepNew(BuildCreateNew.Language.Java, "myapp")
    val (started, _, _) = ws.start()

    testCmd(diffBase = None).run(started).orThrow // #1, mode=test
    compileCmd(watch = false, diffBase = None).run(started).orThrow // #2, mode=compile

    // Bare test --diff right after someone compiled must not trip on mode mismatch: it diffs against #1, not #2.
    val (res, out) = captureOut(testCmd(diffBase = Some(DiffBase.Previous)).run(started))
    res.orThrow
    val j = diffJson(out)
    assert(baseAndTarget(j) == ((1L, 3L)))
    assert(j.hcursor.get[String]("mode") == Right("test"))
    assert(j.hcursor.get[Boolean]("identical") == Right(true))
    assert(out.contains("timing: bleep history diff 1 3 --timing"))

    // test --diff <compile-id> fails BEFORE running, and no new history entry was written.
    val before = TranscriptStore.list(started.buildPaths)
    val mismatch = testCmd(diffBase = Some(DiffBase.Id(2L))).run(started)
    assert(mismatch.left.exists(_.getMessage.contains("#2 is a compile run, not a test run")))
    assert(TranscriptStore.list(started.buildPaths) == before)
    succeed
  }

  integrationTest("compile --diff on a failing run: the diff carries the diagnostics, the verbose failure section is suppressed") { ws =>
    ws.bleepNew(BuildCreateNew.Language.Java, "myapp")
    val (started, _, storingLogger) = ws.start()

    compileCmd(watch = false, diffBase = None).run(started).orThrow // #1, green

    ws.file("myapp/src/java/com/example/Main.java", "package com.example;\npublic class Main { this does not compile }\n")
    val (res, out) = captureOut(compileCmd(watch = false, diffBase = Some(DiffBase.Previous)).run(started))
    assert(res.isLeft, "the command still fails when the build fails")

    val j = diffJson(out)
    assert(baseAndTarget(j) == ((1L, 2L)))
    assert(j.hcursor.get[Boolean]("identical") == Right(false))
    assert(out.contains("newDiagnostics"), s"the diff's newDiagnostics carry the compile errors:\n$out")

    // The one-line summary stays; the verbose failure section is replaced by the diff.
    assert(storingLogger.underlying.exists(_.message.plainText.contains("History:  #2 (bleep history show 2)")))
    assert(!storingLogger.underlying.exists(_.message.plainText.contains("Compilation Failures")))
    succeed
  }

  integrationTest("--diff with --watch: rolling advances per cycle, an explicit id stays a fixed baseline, no prior history renders the first cycle plain") {
    ws =>
      ws.bleepNew(BuildCreateNew.Language.Java, "myapp")
      val (started, _, _) = ws.start()

      // Watch cycles are driven the way WatchMode drives them: prepareDiffRun once up front, then runOnce per cycle.
      val rolling = compileCmd(watch = true, diffBase = Some(DiffBase.Previous))
      val rollingRun = rolling.prepareDiffRun(started).orThrow

      // Cycle 1: no history existed when the watch started — plain output, but the cycle still records its entry.
      val (r1, out1) = captureOut(rolling.runOnce(started, rollingRun))
      r1.orThrow
      assert(!out1.contains("identical"), s"first cycle without prior history must render plain output, got: $out1")
      assert(TranscriptStore.list(started.buildPaths) == List(1L))

      // Cycle 2: diffs against cycle 1's entry.
      val (r2, out2) = captureOut(rolling.runOnce(started, rollingRun))
      r2.orThrow
      assert(baseAndTarget(diffJson(out2)) == ((1L, 2L)))

      // Cycle 3: rolling advanced — against cycle 2's entry, not cycle 1's.
      val (r3, out3) = captureOut(rolling.runOnce(started, rollingRun))
      r3.orThrow
      assert(baseAndTarget(diffJson(out3)) == ((2L, 3L)))

      // Explicit id + watch: a fixed baseline for every cycle.
      val fixed = compileCmd(watch = true, diffBase = Some(DiffBase.Id(1L)))
      val fixedRun = fixed.prepareDiffRun(started).orThrow
      val (f1, fOut1) = captureOut(fixed.runOnce(started, fixedRun))
      f1.orThrow
      assert(baseAndTarget(diffJson(fOut1)) == ((1L, 4L)))
      val (f2, fOut2) = captureOut(fixed.runOnce(started, fixedRun))
      f2.orThrow
      assert(baseAndTarget(diffJson(fOut2)) == ((1L, 5L)))
      succeed
  }
}
