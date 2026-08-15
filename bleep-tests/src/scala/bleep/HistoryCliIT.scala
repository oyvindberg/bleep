package bleep

import bleep.commands.BuildCreateNew
import bleep.history.{TranscriptDiff, TranscriptStore}

/** End-to-end over the CLI surface: compiles through the (in-process, but production) BSP server leave transcripts in the workspace, the compile summary names
  * the history id, and `bleep history` / `bleep history show` / `bleep history diff` read the same files back — no daemon connection involved in the reads.
  */
class HistoryCliIT extends IntegrationTestHarness {
  integrationTest("bleep history / history show / history diff over daemon-written transcripts") { ws =>
    ws.bleepNew(BuildCreateNew.Language.Java, "myapp")

    val (started, commands, storingLogger) = ws.start()
    val myapp = model.CrossProjectName(model.ProjectName("myapp"), None)

    commands.compile(List(myapp))
    commands.compile(List(myapp))

    // The daemon wrote one transcript per run, into this workspace.
    assert(TranscriptStore.list(started.buildPaths) == List(1L, 2L))
    val t1 = TranscriptStore.read(started.buildPaths, 1L)
    assert(t1.mode == "compile")
    assert(t1.client == "bleep") // the CLI's build/initialize clientName
    assert(t1.targets == List("myapp"))

    // The compile summary points at the transcript.
    assert(storingLogger.underlying.exists(_.message.plainText.contains("History:  #1 (bleep history show 1)")))
    assert(storingLogger.underlying.exists(_.message.plainText.contains("History:  #2 (bleep history show 2)")))

    // The CLI commands run over those files.
    bleep.commands.History.ListEntries.run(started).orThrow
    bleep.commands.History.Show(id = Some(2L), project = None, query = None, limit = None, offset = None).run(started).orThrow
    bleep.commands.History.Show(id = None, project = Some("myapp"), query = None, limit = None, offset = None).run(started).orThrow
    bleep.commands.History.Diff(base = 1L, target = 2L, timing = false, limit = None, baseDir = None, output = bleep.OutputMode.Json).run(started).orThrow
    bleep.commands.History.Diff(base = 1L, target = 2L, timing = true, limit = Some(5), baseDir = None, output = bleep.OutputMode.Json).run(started).orThrow

    // `--base-dir` resolves the base id in an explicitly named workspace — here the same one, proving the path-based resolution.
    bleep.commands.History
      .Diff(base = 1L, target = 2L, timing = false, limit = None, baseDir = Some(started.buildPaths.buildDir), output = bleep.OutputMode.Json)
      .run(started)
      .orThrow

    // And the underlying diff distinguishes the runs honestly: clean build vs noop is a reason transition, nothing more.
    val diff = TranscriptDiff.mechanical(t1, TranscriptStore.read(started.buildPaths, 2L))
    assert(diff.hcursor.get[Boolean]("identical") == Right(false))
    succeed
  }
}
