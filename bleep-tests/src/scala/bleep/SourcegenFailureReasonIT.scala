package bleep

import bleep.bsp.protocol.BleepBspProtocol
import bleep.history.{TranscriptFormat, TranscriptStore}

/** A sourcegen script that says why it failed and exits 1: the reason must reach the build summary and the run's history entry.
  *
  * Reported building scala3: the summary said "1 sourcegen script(s) failed. Check output above for details." with no output above (TUI), and `bleep history
  * show` and the server log held only "exit code 1 (no stderr beyond routine JVM warnings)". The script had logged its reason — to stdout, where bleep scripts
  * log — and nothing kept stdout.
  */
class SourcegenFailureReasonIT extends IntegrationTestHarness {

  private val Reason = "test properties checkout not found at /nowhere/sjs-tests"

  private val Yaml = """projects:
                       |  a:
                       |    extends: common
                       |    sourcegen: scripts/testscripts.FailingGen
                       |  scripts:
                       |    extends: common
                       |    dependencies: build.bleep::bleep-core:${BLEEP_VERSION}
                       |    platform:
                       |      jvmRuntimeOptions: -Xmx256m -Xms32m
                       |templates:
                       |  common:
                       |    platform:
                       |      name: jvm
                       |    scala:
                       |      version: 3.9.0
                       |""".stripMargin

  private val FailingGen = s"""package testscripts
                              |
                              |import bleep.*
                              |
                              |object FailingGen extends BleepCodegenScript("FailingGen") {
                              |  def run(started: Started, commands: Commands, targets: List[Target], args: List[String]): Unit = {
                              |    println("$Reason")
                              |    System.out.flush()
                              |    sys.exit(1)
                              |  }
                              |}
                              |""".stripMargin

  private val a = model.CrossProjectName(model.ProjectName("a"), None)

  integrationTest("a failed sourcegen script's stdout reaches the summary and the history entry") { ws =>
    ws.yaml(Yaml)
    ws.file("scripts/src/scala/testscripts/FailingGen.scala", FailingGen)
    ws.file("a/src/scala/a/A.scala", "package a\nobject A\n")

    val (started, commands, storingLogger) = ws.start()
    val thrown = intercept[BleepException](commands.compile(List(a)))
    assert(thrown.getMessage.contains("Source generation failed"), thrown.getMessage)

    // The summary's own section: the script by name, its reason as a quoted line under it.
    val logged = storingLogger.underlying.map(_.message.plainText).toList
    assert(logged.contains("  testscripts.FailingGen"), logged.mkString("\n"))
    assert(logged.contains(s"    | $Reason"), logged.mkString("\n"))

    // The history entry — what `bleep history show` prints — carries the same reason.
    val transcript = TranscriptStore.readLatest(started.buildPaths)
    val errors = transcript.events.collect { case e: BleepBspProtocol.Event.SourcegenFinished if !e.success => e.error }
    errors match {
      case List(Some(error)) =>
        assert(error.contains("exit code 1"), error)
        assert(error.contains("stdout:"), error)
        assert(error.indexOf("stdout:") < error.indexOf(Reason), error)
      case other => fail(s"expected one failed sourcegen with an error, got $other")
    }
    val json = TranscriptFormat.formatCompileResult(transcript.events, verbose = false, query = None, limit = None, offset = None)
    val historyError = json.hcursor.downField("sourcegenFailures").downArray.get[String]("error")
    assert(historyError.exists(_.contains(Reason)), historyError.toString)
  }
}
