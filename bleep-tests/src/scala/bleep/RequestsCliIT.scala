package bleep

import bleep.commands.BuildCreateNew
import bleep.requests.{RequestDiff, TranscriptStore}

/** End-to-end over the CLI surface: compiles through the (in-process, but production) BSP server leave transcripts in the workspace, the compile summary names
  * the request id, and `bleep requests` / `bleep details` / `bleep diff` read the same files back — no daemon connection involved in the reads.
  */
class RequestsCliIT extends IntegrationTestHarness {
  integrationTest("bleep requests / details / diff over daemon-written transcripts") { ws =>
    ws.bleepNew(BuildCreateNew.Language.Java, "myapp")

    val (started, commands, storingLogger) = ws.start()
    val myapp = model.CrossProjectName(model.ProjectName("myapp"), None)

    commands.compile(List(myapp))
    commands.compile(List(myapp))

    // The daemon wrote one transcript per request, into this workspace.
    assert(TranscriptStore.list(started.buildPaths) == List(1L, 2L))
    val t1 = TranscriptStore.read(started.buildPaths, 1L)
    assert(t1.mode == "compile")
    assert(t1.client == "bleep") // the CLI's build/initialize clientName
    assert(t1.targets == List("myapp"))

    // The compile summary points at the transcript.
    assert(storingLogger.underlying.exists(_.message.plainText.contains("Request:  #1 (bleep details 1)")))
    assert(storingLogger.underlying.exists(_.message.plainText.contains("Request:  #2 (bleep details 2)")))

    // The CLI commands run over those files.
    bleep.commands.Requests.ListRequests.run(started).orThrow
    bleep.commands.Requests.Details(id = Some(2L), project = None, query = None, limit = None, offset = None).run(started).orThrow
    bleep.commands.Requests.Details(id = None, project = Some("myapp"), query = None, limit = None, offset = None).run(started).orThrow
    bleep.commands.Requests.Diff(base = 1L, target = 2L, timing = false, limit = None, baseDir = None).run(started).orThrow
    bleep.commands.Requests.Diff(base = 1L, target = 2L, timing = true, limit = Some(5), baseDir = None).run(started).orThrow

    // `--base-dir` resolves the base id in an explicitly named workspace — here the same one, proving the path-based resolution.
    bleep.commands.Requests.Diff(base = 1L, target = 2L, timing = false, limit = None, baseDir = Some(started.buildPaths.buildDir)).run(started).orThrow

    // And the underlying diff distinguishes the runs honestly: clean build vs noop is a reason transition, nothing more.
    val diff = RequestDiff.mechanical(t1, TranscriptStore.read(started.buildPaths, 2L))
    assert(diff.hcursor.get[Boolean]("identical") == Right(false))
    succeed
  }
}
