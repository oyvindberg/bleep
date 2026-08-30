package bleep.analysis

import bleep.bsp.protocol.{BleepBspProtocol, CompileStatus, DiagnosticSeverity, LinkPlatformName, ProcessExit, SuiteOutcome, TestStatus}
import bleep.model.{CrossProjectName, ProjectName, SuiteName, TestName}
import bleep.testing.{BuildEvent, BuildState, BuildStateReducer}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** Pins the verdict on a whole run: protocol events through [[BuildEvent.fromProtocol]], folded by [[BuildStateReducer]], judged by
  * [[bleep.testing.BuildSummary.toEither]]. This chain is what `bleep test`'s exit code, the MCP tools and transcript rendering all share, so every way a run
  * can go wrong before or beside the tests — a dependency that fails to compile, a link/sourcegen/processor failure, a suite that errors, hangs or never runs —
  * must come out Left here. The founding incident: a removed constructor broke a dependency's compile, every downstream task was skipped, and the run reported
  * success with "0 tests passed".
  */
class BuildSummaryVerdictTest extends AnyFunSuite with Matchers {

  import BleepBspProtocol.{Event => E}

  private def proj(name: String): CrossProjectName = CrossProjectName(ProjectName(name), crossId = None)

  private def verdict(events: List[E]): Either[bleep.BleepException, Unit] = {
    val state = events.flatMap(BuildEvent.fromProtocol).foldLeft(BuildState.empty)(BuildStateReducer.reduce)
    state.toSummary(durationMs = 0L, wasCancelled = false).toEither
  }

  private def leftMessage(events: List[E]): String =
    verdict(events).left.map(_.getMessage) match {
      case Left(msg) => msg
      case Right(()) => fail("expected the run to be judged a failure, but it was Right")
    }

  private def compileFinished(p: String, status: CompileStatus, skippedBecause: Option[String]): E =
    E.CompileFinished(
      proj(p),
      status,
      durationMs = 1L,
      diagnostics =
        if (status == CompileStatus.Failed)
          List(BleepBspProtocol.Diagnostic(DiagnosticSeverity.Error, "boom", rendered = None, path = None, line = None, column = None))
        else Nil,
      skippedBecause = skippedBecause.map(proj),
      timestamp = 1L
    )

  private def passedTest(p: String): List[E] = List(
    E.TestFinished(
      proj(p),
      SuiteName("OkSuite"),
      TestName("ok"),
      TestStatus.Passed,
      durationMs = 1L,
      message = None,
      throwable = None,
      timestamp = 1L,
      location = None
    ),
    E.SuiteFinished(proj(p), SuiteName("OkSuite"), SuiteOutcome.Executed(1, 0, 0, 0), durationMs = 1L, timestamp = 2L)
  )

  test("a dependency failing to compile fails the run, even with zero suites and zero test events") {
    leftMessage(
      List(
        compileFinished("core", CompileStatus.Failed, skippedBecause = None),
        compileFinished("tests", CompileStatus.Skipped, skippedBecause = Some("core"))
      )
    ) should include("failed to compile")
  }

  test("a compile task that errored (threw, rather than reporting diagnostics) fails the run") {
    leftMessage(List(compileFinished("core", CompileStatus.Error, skippedBecause = None))) should include("failed to compile")
  }

  test("a link failure fails the run even when every executed test passed") {
    leftMessage(
      passedTest("app") :+
        E.LinkFinished(
          proj("app"),
          success = false,
          durationMs = 1L,
          outputPath = None,
          generatedFiles = Nil,
          timestamp = 3L,
          platform = LinkPlatformName.ScalaJs,
          error = Some("nope")
        )
    ) should include("failed to link")
  }

  test("a sourcegen failure fails the run") {
    leftMessage(
      List(E.SourcegenFinished(scriptMain = "scripts.Gen", success = false, durationMs = 1L, error = Some("script exploded"), timestamp = 1L))
    ) should include("Source generation failed")
  }

  test("an annotation-processor resolution failure fails the run") {
    leftMessage(
      List(
        E.ResolveAnnotationProcessorsFinished(
          proj("app"),
          success = false,
          durationMs = 1L,
          error = Some("no artifact"),
          discoveredJarCount = 0,
          timestamp = 1L
        )
      )
    ) should include("Annotation processor")
  }

  test("a symbol-processor (KSP) resolution failure fails the run") {
    leftMessage(
      List(E.RunSymbolProcessorsFinished(proj("app"), success = false, durationMs = 1L, error = Some("ksp broke"), discoveredJarCount = 0, timestamp = 1L))
    ) should include("KSP")
  }

  test("a crashed suite process fails the run") {
    verdict(
      passedTest("app") :+
        E.SuiteError(proj("app"), SuiteName("OomSuite"), error = "boom", processExit = ProcessExit.Signal(9), durationMs = 1L, timestamp = 3L)
    ).isLeft shouldBe true
  }

  test("a timed-out suite fails the run") {
    verdict(List(E.SuiteTimedOut(proj("app"), SuiteName("HungSuite"), timeoutMs = 60000L, threadDump = None, timestamp = 1L))).isLeft shouldBe true
  }

  test("a suite cancelled because its dependency failed fails the run") {
    leftMessage(
      passedTest("app") :+
        E.SuiteCancelled(proj("app"), SuiteName("NeverRan"), reason = Some("dependency core failed"), timestamp = 3L)
    ) should include("cancelled")
  }

  test("a build-level Error event fails the run") {
    verdict(List(E.Error(message = "discovery blew up", details = None, timestamp = 1L))).isLeft shouldBe true
  }

  test("a cancelled compile fails the run") {
    leftMessage(List(compileFinished("app", CompileStatus.Cancelled, skippedBecause = None))) should include("cancelled")
  }

  test("suites that completed without executing a single test are the silent-zero signature, not a pass") {
    verdict(List(E.SuiteFinished(proj("app"), SuiteName("EmptySuite"), SuiteOutcome.Empty, durationMs = 1L, timestamp = 1L))).isLeft shouldBe true
  }

  private def discovered(p: String, suites: List[String], beforeFilters: Int, isTestProject: Boolean): E.SuitesDiscovered =
    E.SuitesDiscovered(
      proj(p),
      suites.map(SuiteName.apply),
      totalSuitesDiscovered = suites.size,
      discoveredBeforeFilters = Some(beforeFilters),
      isTestProject = isTestProject,
      timestamp = 1L
    )

  test("a test project whose classpath scan found nothing fails the run") {
    leftMessage(
      List(
        compileFinished("tests", CompileStatus.Success, skippedBecause = None),
        discovered("tests", suites = Nil, beforeFilters = 0, isTestProject = true)
      )
    ) should include("No test suites found")
  }

  test("the failure names the projects, so the user knows where to look") {
    val msg = leftMessage(
      List(
        discovered("b-tests", suites = Nil, beforeFilters = 0, isTestProject = true),
        discovered("a-tests", suites = Nil, beforeFilters = 0, isTestProject = true)
      )
    )
    msg should include("a-tests")
    msg should include("b-tests")
    msg should include("2 test project(s)")
  }

  test("a project filtered down to nothing is the user's choice, not a broken build") {
    // `--exclude` emptied the selection: the scan DID find suites, so there is nothing wrong with the project.
    verdict(
      List(
        compileFinished("tests", CompileStatus.Success, skippedBecause = None),
        discovered("tests", suites = Nil, beforeFilters = 7, isTestProject = true)
      )
    ) shouldBe Right(())
  }

  test("an older peer's event, which carries no pre-filter count, never invents a failure") {
    // The wire form predating `discoveredBeforeFilters` decodes it as None, not 0. An empty `suites` from such a peer is genuinely ambiguous — scan found
    // nothing, or a filter emptied it — and replaying an old transcript must not turn that silence into a failure the run never had.
    import io.circe.syntax.*
    // Built by encoding a current event and deleting the field, so this stays a test about the missing field rather than about how the rest happens to encode.
    val legacyWire = discovered("tests", suites = Nil, beforeFilters = 0, isTestProject = true).asJson
      .mapObject(_.remove("discoveredBeforeFilters").remove("isTestProject"))
    legacyWire.hcursor.keys.map(_.toList) shouldBe Some(List("project", "suites", "totalSuitesDiscovered", "timestamp"))
    val decoded = legacyWire.as[E.SuitesDiscovered].getOrElse(fail("legacy SuitesDiscovered must still decode"))
    decoded.discoveredBeforeFilters shouldBe None
    decoded.isTestProject shouldBe false
    verdict(List(decoded)) shouldBe Right(())
  }

  test("a plain library named as a test target finds no suites, and that is not a fault") {
    // `bleep test` and `bleep ci` pass every named target through discovery, libraries included — `testProjects` is what the client asked for, not the set that
    // declared `isTestProject: true`. CiCommandIT caught this: its `myapp` is a library with no tests, and failing it made `bleep ci` unusable.
    verdict(
      List(
        compileFinished("myapp", CompileStatus.Success, skippedBecause = None),
        discovered("myapp", suites = Nil, beforeFilters = 0, isTestProject = false)
      )
    ) shouldBe Right(())
  }

  test("a clean run is Right") {
    verdict(compileFinished("app", CompileStatus.Success, skippedBecause = None) +: passedTest("app")) shouldBe Right(())
  }

  test("an empty event stream is Right — no suites were even attempted") {
    verdict(Nil) shouldBe Right(())
  }
}
