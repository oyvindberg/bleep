package bleep

import bleep.commands.{DisplayMode, ReactiveBsp}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** Which platform a fixture runs on, and the `platform:` block that selects it. */
sealed abstract class FixturePlatform(val id: String) {
  def platformYaml: String
  def scalaVersion: String

  /** Used in test names, so a failure says which combination broke without opening the file. */
  def describe: String

  /** Does this platform report individual test cases, or only a suite-level result?
    *
    * False on the Kotlin platforms, and that is a defect rather than a design choice — see [[PlatformFrameworkHarness.assertFixtureRan]]. Stated here so the
    * matrix asserts what is true today instead of quietly passing on a weaker check everywhere.
    */
  def reportsIndividualCases: Boolean = true

  /** `2.12`, `2.13` or `3` — what a Maven artifact suffix is built from, and the granularity at which frameworks decide what they publish.
    *
    * `None` on a Kotlin platform, where no Scala is involved at all: nothing on the classpath carries a Scala suffix, so the axis does not constrain which
    * fixtures apply. Not a stand-in value, because every stand-in would have to be a string some fixture then has to remember to list.
    */
  def scalaBinaryVersion: Option[String]
}

object FixturePlatform {

  /** 3.x collapses to `3`; 2.x keeps its minor. Anything else is a version this matrix has never been told about, and guessing at its suffix would produce a
    * resolution error blaming the framework for the harness's mistake.
    */
  def binaryVersionOf(scalaVersion: String): String =
    scalaVersion.split('.').toList match {
      case "3" :: _          => "3"
      case "2" :: minor :: _ => s"2.$minor"
      case _                 => sys.error(s"cannot derive a Scala binary version from '$scalaVersion'")
    }

  case class Jvm(scalaVersion: String) extends FixturePlatform("jvm") {
    def scalaBinaryVersion: Option[String] = Some(binaryVersionOf(scalaVersion))
    def describe: String = s"jvm / scala $scalaVersion"
    def platformYaml: String =
      """    platform:
        |      name: jvm
        |""".stripMargin
  }

  case class Js(scalaVersion: String, jsVersion: String, nodeVersion: String) extends FixturePlatform("js") {
    def scalaBinaryVersion: Option[String] = Some(binaryVersionOf(scalaVersion))
    def describe: String = s"js / scala $scalaVersion / scalajs $jsVersion"
    def platformYaml: String =
      s"""    platform:
         |      name: js
         |      jsVersion: $jsVersion
         |      jsNodeVersion: $nodeVersion
         |      jsKind: none
         |""".stripMargin
  }

  /** Kotlin/JS. A `platform: js` project with a `kotlin:` block and no Scala — which is exactly how bleep tells the two apart: `MultiWorkspaceBspServer`
    * branches on `(PlatformId.Js, project.kotlin.version.isDefined)`.
    *
    * No `jsVersion` here, because that field is Scala.js's linker version and there is no Scala.js in a Kotlin/JS build. The Kotlin compiler emits the
    * JavaScript itself.
    */
  case class KotlinJs(kotlinVersion: String, nodeVersion: String) extends FixturePlatform("kotlin-js") {
    override def reportsIndividualCases: Boolean = false
    def scalaVersion: String = sys.error("Kotlin/JS has no Scala version")
    def scalaBinaryVersion: Option[String] = None
    def describe: String = s"kotlin-js / kotlin $kotlinVersion"
    def platformYaml: String =
      s"""    platform:
         |      name: js
         |      jsNodeVersion: $nodeVersion
         |      jsKind: none
         |""".stripMargin
  }

  /** Kotlin/Native — a `platform: native` project with a `kotlin:` block. No `nativeGc`, which is Scala Native's collector setting; the Kotlin/Native toolchain
    * brings its own runtime.
    */
  case class KotlinNative(kotlinVersion: String) extends FixturePlatform("kotlin-native") {
    override def reportsIndividualCases: Boolean = false
    def scalaVersion: String = sys.error("Kotlin/Native has no Scala version")
    def scalaBinaryVersion: Option[String] = None
    def describe: String = s"kotlin-native / kotlin $kotlinVersion"
    def platformYaml: String =
      """    platform:
        |      name: native
        |""".stripMargin
  }

  case class Native(scalaVersion: String, nativeVersion: String) extends FixturePlatform("native") {
    def scalaBinaryVersion: Option[String] = Some(binaryVersionOf(scalaVersion))
    def describe: String = s"native / scala $scalaVersion / scala-native $nativeVersion"
    // `nativeGc` is spelled out because bleep requires it: `ResolveProjects` treats it as mandatory for a Scala Native project and fails the build with
    // "missing platform field `nativeGc`" when it is absent — unlike `jsKind`, which defaults. `immix` is Scala Native's own default collector.
    def platformYaml: String =
      s"""    platform:
         |      name: native
         |      nativeVersion: $nativeVersion
         |      nativeGc: immix
         |""".stripMargin
  }
}

/** Runs a [[TestFrameworkFixture]] on a [[FixturePlatform]] through the real `bleep test` entry point and asserts on what actually executed.
  *
  * The path under test is the deployed one: bootstrap a build, resolve the framework from Maven Central with the platform suffix applied, compile, link where
  * the platform needs linking, discover suites, run them, and report. Nothing is stubbed, so a runner that discovers a suite and executes none of its tests
  * fails here — which no existing suite does, and which is what #655 reports.
  */
object PlatformFrameworkHarness {

  /** Does this failure detail carry stack frames?
    *
    * Two shapes, because frameworks disagree: most render a frame per line, tab-indented (`\tat example.Fixture.method(File.scala:12)`), while hedgehog runs
    * them inline after the message. Matching only the first shape reports "no stack frames" for hedgehog; matching only ` at ` misses every tab-indented frame
    * there is, which is most of them.
    */
  def hasStackFrames(detail: String): Boolean =
    detail.linesIterator.exists(_.trim.startsWith("at ")) || detail.contains(" at ")
}

trait PlatformFrameworkHarness { self: IntegrationTestHarness =>

  protected val projectName: String = "mytest"
  protected val project: model.CrossProjectName = model.CrossProjectName(model.ProjectName(projectName), None)

  protected def yamlFor(fixture: TestFrameworkFixture, frameworkVersion: String, platform: FixturePlatform): String = {
    val deps = fixture.deps(frameworkVersion).map(d => s"    - $d").mkString("\n")
    // A Java fixture gets no `scala:` block at all. Declaring a Scala version for a project with no Scala sources would pull the compiler and the standard
    // library in for nothing, and would stop the fixture from proving that a plain Java test project works.
    val scalaBlock = fixture.language match {
      case FixtureLanguage.Scala => s"    scala:\n      version: ${platform.scalaVersion}\n"
      case FixtureLanguage.Java  => ""
      // Kotlin needs its own toolchain block for the same reason Scala does, and no `scala:` block for the same reason Java gets none.
      case FixtureLanguage.Kotlin =>
        // The Kotlin platforms carry their own toolchain version; a Kotlin fixture on the JVM takes the default.
        val kotlinVersion = platform match {
          case FixturePlatform.KotlinJs(v, _)  => v
          case FixturePlatform.KotlinNative(v) => v
          case _                               => model.Versions.Kotlin24
        }
        s"    kotlin:\n      version: $kotlinVersion\n"
    }
    s"""projects:
       |  $projectName:
       |    dependencies:
       |$deps
       |    isTestProject: true
       |${platform.platformYaml}$scalaBlock""".stripMargin
  }

  /** Run the fixture and hand back both the run's own verdict and the per-case results it recorded.
    *
    * The verdict is expected to be a failure for every fixture — each declares one failing test on purpose — so it is returned rather than thrown, and the
    * caller asserts on the counts.
    */
  protected def runFixture(
      ws: Workspace,
      fixture: TestFrameworkFixture,
      frameworkVersion: String,
      platform: FixturePlatform
  ): (Either[BleepException, Unit], List[JUnitReports.Suite]) = {
    ws.yaml(yamlFor(fixture, frameworkVersion, platform))
    ws.file(s"$projectName/src/${fixture.language.sourceDir}/${fixture.relPath}", fixture.source)
    ws.file(s"$projectName/src/${fixture.language.sourceDir}/${fixture.decoyRelPath}", fixture.decoySource)
    // The constructor-failure suite is written and run everywhere except where it is already recorded as hanging. munit on Scala.js takes the full two-minute
    // idle timeout to report that suite, which would make this test take two minutes and then time out — testing bleep's timeout rather than munit's
    // behaviour. The behaviour itself is recorded in `ctorFailureReport` and documented; running it again on every build buys nothing.
    val runCtorSuite = fixture.hasCtorErrorVariant && fixture.ctorFailureReport(platform.id) != CtorFailureReport.Hangs
    if (runCtorSuite)
      ws.file(s"$projectName/src/${fixture.language.sourceDir}/${fixture.ctorErrorRelPath}", fixture.ctorErrorSource)
    if (fixture.hasGreenVariant)
      ws.file(s"$projectName/src/${fixture.language.sourceDir}/${fixture.greenRelPath}", fixture.greenSource)
    fixture.extraFiles.foreach { case (relPath, content) => ws.file(s"$projectName/$relPath", content) }
    val (started, _, storingLogger) = ws.start()
    val reportDir: Path = ws.root.resolve("junit-reports")
    val verdict = ReactiveBsp
      .test(
        watch = false,
        projects = Array(project),
        displayMode = DisplayMode.NoTui,
        jvmOptions = Nil,
        testArgs = Nil,
        // Three suites in one run: the real one, the one whose construction throws, and one that simply passes. Selecting them together is what proves a
        // suite that cannot be built does not take its neighbours down with it — the assertions below check the real suite reported in full, that the broken
        // one was reported, and that the green one came back green. Without that third suite a run has no suite-level success in it at all, and "reported as
        // passing" is a claim nothing was checking.
        only = List(fixture.suiteFqn) ::: (if (runCtorSuite) List(fixture.ctorErrorSuiteFqn)
                                           else Nil) ::: (if (fixture.hasGreenVariant) List(fixture.greenSuiteFqn) else Nil),
        exclude = Nil,
        includeTags = Nil,
        excludeTags = Nil,
        flamegraph = false,
        cancel = false,
        junitReportDir = Some(reportDir),
        diffBase = None,
        diffOutput = OutputMode.Text,
        clientEnv = Map.empty
      )
      .run(started)
    // The run's own user-visible output, kept so the generated documentation can show what a reader would actually see rather than describing it. Written
    // from the same run that asserts on the result, so the two cannot disagree.
    // Normalised before writing: durations, temp paths, ports and pids differ on every run, and an unnormalised transcript would show up as a diff in every
    // build. What is left is the shape of the output, which is the thing worth checking in.
    val transcript = storingLogger.underlying.iterator
      .map(_.message.plainText)
      .mkString("\n")
      .replaceAll("""\(\d+ ?ms\)""", "(<duration>)")
      .replaceAll("""\d+\.\d+s""", "<duration>")
      .replaceAll("""(?m)^(.*?)\d+ms""", "$1<duration>")
      .replace(ws.root.toString, "<project>")
      .replaceAll("""port '\d+'""", "port '<port>'")
      .replaceAll("""pid=\d+""", "pid=<pid>")
      .replaceAll("""history show \d+""", "history show <n>")
      .replaceAll("""#\d+ \(bleep""", "#<n> (bleep")
    val transcriptFile = snippetsRoot / "test-framework-output" / s"${fixture.name}-${platform.id}.txt"
    Files.createDirectories(transcriptFile.getParent)
    Files.write(transcriptFile, transcript.getBytes(StandardCharsets.UTF_8)): Unit

    (verdict, JUnitReports.read(reportDir))
  }

  /** Assert the fixture ran exactly as declared: the suite was found, every passing test passed under its own name, and the one failing test was reported as a
    * failure.
    *
    * Checking the names, not just the counts, is deliberate. A runner that reports a single synthetic "suite failed" case would otherwise satisfy a count-only
    * assertion; #655's munit output ("Suite reported 1 failure(s) but no individual test results were captured") is exactly that shape.
    */
  protected def assertFixtureRan(
      fixture: TestFrameworkFixture,
      frameworkVersion: String,
      platform: FixturePlatform,
      suites: List[JUnitReports.Suite],
      verdict: Either[BleepException, Unit]
  ): org.scalatest.Assertion = {
    val context = s"${fixture.name} $frameworkVersion on ${platform.describe}"
    // When nothing ran at all the useful information is in the run's own failure, not in the (empty) report — a build that failed to resolve or compile looks
    // exactly like a runner that executed nothing unless the exception is shown.
    val why = verdict match {
      // The whole cause chain, because `BleepException.Cause` renders as a bare "Build failed" and the sentence that says what actually went wrong — an
      // unresolvable dependency, a missing toolchain — is one or more levels down.
      case Left(e) =>
        val chain = Iterator.iterate[Throwable](e)(_.getCause).takeWhile(_ != null).map(t => s"${t.getClass.getName}: ${t.getMessage}").toList
        chain.mkString("\n  run failed with: ", "\n    caused by: ", "")
      case Right(_) => ""
    }
    val rendered = (if (suites.isEmpty) "  (no suites reported at all)" else suites.map("  " + _.describe).mkString("\n")) + why

    // Kotlin/JS and Kotlin/Native no longer report a synthetic per-project suite: `discoverTestSuites` asks the linked artifact to enumerate itself and
    // hands the DAG the real fully-qualified names, so the JUnit XML carries a proper `<testsuite>` with a `<testcase>` per test. What still differs from the
    // JVM is only the *spelling* of a case name — kotlin.test on the JVM goes through junit and the Launcher reports `adds()`, while the linked artifact's
    // own `##kotlin-test##` protocol reports the bare `adds` — and a fixture carries one `reportedName` for all platforms. So the name comparisons stay on
    // the JVM path and everything that does not depend on spelling is asserted here: the totals, and the verdict the run itself produced.
    if (!platform.reportsIndividualCases) {
      val message = verdict.left.toOption.map(_.getMessage).getOrElse("")
      assert(
        message.startsWith("Tests failed:"),
        s"$context: expected the run to fail because tests failed, but it failed with: $message\n$rendered"
      )
      // Counts, both of them. Asserting only the passing count is what let a Kotlin/Native run report the two failing tests three times over — twice under the
      // suite's real name and once under a name with "(1 ms total)" appended, parsed out of a human-readable summary line — and still go green here.
      // The whole run, not one suite: the passing tests of the real suite plus those of the all-passing one selected alongside it.
      val expectedPassedInRun = fixture.expectedPassed * (if (fixture.hasGreenVariant) 2 else 1)
      assert(
        message.contains(s"$expectedPassedInRun passed"),
        s"$context: expected $expectedPassedInRun passing tests in the verdict, got: $message\n$rendered"
      )
      // The verdict's failure count is deliberately not asserted: the run also contains the suite whose constructor throws, so that number is the sum of two
      // suites. The per-suite numbers below pin this fixture, and they come from the report rather than from a summary string.
      // The verdict counts passes, failures, timeouts and cancellations — never skips (`BuildDisplay`). So a dropped skip is invisible there by
      // construction, and the reported suite is the only place it can be caught.
      val kotlinSuite = suites
        .find(_.name == fixture.suiteFqn)
        .getOrElse(fail(s"$context: no suite named ${fixture.suiteFqn} in the JUnit report. Got:\n$rendered"))
      assert(
        kotlinSuite.tests == fixture.expectedTotal,
        s"$context: expected ${fixture.expectedTotal} test cases, got ${kotlinSuite.tests}.\n$rendered"
      )
      assert(
        kotlinSuite.failures + kotlinSuite.errors == fixture.expectedNotPassing,
        s"$context: expected ${fixture.expectedNotPassing} not-passing cases in the suite attributes, got failures=${kotlinSuite.failures} " +
          s"errors=${kotlinSuite.errors}.\n$rendered"
      )
      assert(
        kotlinSuite.skipped == fixture.expectedSkipped,
        s"$context: expected skipped=${fixture.expectedSkipped}, got skipped=${kotlinSuite.skipped}.\n$rendered"
      )
      return succeed
    }

    val suite = suites
      .find(_.name == fixture.suiteFqn)
      .getOrElse(fail(s"$context: no suite named ${fixture.suiteFqn} in the JUnit report. Got:\n$rendered"))

    val passedNames = suite.cases.filter(_.status == "passed").map(_.name).sorted
    val failedNames = suite.cases.filter(c => c.status == "failure" || c.status == "error").map(_.name).sorted

    assert(
      passedNames == fixture.passingTestNames.sorted,
      s"$context: expected passing tests ${fixture.passingTestNames.sorted.mkString(", ")} but got ${passedNames.mkString(", ")}.\n$rendered"
    )
    assert(
      failedNames == fixture.reportedNotPassingNames,
      s"$context: expected the assertion failure '${fixture.reportedFailingName}' and the uncaught exception '${fixture.reportedThrowingName}' " +
        s"to be reported as not passing, but got ${failedNames.mkString(", ")}.\n$rendered"
    )
    assert(
      suite.tests == fixture.expectedTotal,
      s"$context: expected ${fixture.expectedTotal} test cases, got ${suite.tests}.\n$rendered"
    )
    // The `<testsuite>` attributes as well as the cases beneath it. They are written by different code and a reader believes the attributes — a dashboard shows
    // "6 failures" from the header without ever counting the children — so a disagreement between the two is a real defect even when the case list is right.
    assert(
      suite.failures + suite.errors == fixture.expectedNotPassing,
      s"$context: expected ${fixture.expectedNotPassing} not-passing cases in the suite attributes, got failures=${suite.failures} errors=${suite.errors}.\n$rendered"
    )
    // Every fixture's throwing test throws `RuntimeException("boom")`, so the message is the one thing that is the same across all eighteen frameworks and
    // can be asserted uniformly. Without this, a runner that reported failures with an empty message passed every other check here: the case was present,
    // its status was right, and the counts added up.
    val notPassing = suite.cases.filter(c => c.status == "failure" || c.status == "error")
    // Matched by content rather than by case name: minitest and TestNG report every case under one name, so picking "the throwing one" by name lands on
    // whichever not-passing case came first. `RuntimeException("boom")` is thrown by all eighteen fixtures, which makes its message the one string that can
    // be asserted uniformly — and a runner that reported failures with no message at all satisfied every other check here.
    // Checked as a multiset rather than by looking each case up by name: minitest and TestNG report every case under the suite's own name, so "the throwing
    // one" cannot be found by name at all. Comparing the sorted kinds sidesteps that and still pins both.
    val reporting = fixture.failureReporting(platform.id, platform.scalaBinaryVersion)
    val actualKinds = notPassing.map(c => fixture.classifyThrowable(c.message, c.detail)).map(_.toString).sorted
    val expectedKinds = List(reporting.assertionFailure, reporting.uncaughtException).map(_.toString).sorted
    assert(
      actualKinds == expectedKinds,
      s"$context: expected the failing cases to carry ${expectedKinds.mkString(" and ")} throwables, got ${actualKinds.mkString(" and ")}. " +
        "`Real` means the stack names the suite, so it is the exception that failed the test; `Fabricated` means something was attached but its stack runs " +
        "only through the framework, so it says nothing; `Absent` means no throwable at all. Details were: " +
        s"${notPassing.map(c => s"${c.name} -> ${if (c.detail.isEmpty) "(nothing)" else c.detail.take(90).replace("\n", " / ")}").mkString("; ")}\n$rendered"
    )

    // Where the human-readable reason lives when it is not on the case. Checked by looking for the failing test's own name in that stream, not merely that
    // the stream is non-empty — every fixture prints a marker, so "non-empty" would have passed for a stream that never mentioned the failure.
    reporting.explanation match {
      case ExplanationAt.OnCase         => ()
      case ExplanationAt.CapturedStdout =>
        assert(
          suite.systemOut.contains(fixture.failingTestName),
          s"$context: the reason is documented as being in the captured output, but it never mentions '${fixture.failingTestName}'.\n" +
            s"  <system-out> was: ${if (suite.systemOut.isEmpty) "(empty)" else suite.systemOut.take(300)}\n$rendered"
        )
      case ExplanationAt.CapturedStderr =>
        assert(
          suite.systemErr.contains(fixture.failingTestName),
          s"$context: the reason is documented as being in the captured error output, but it never mentions '${fixture.failingTestName}'.\n" +
            s"  <system-err> was: ${if (suite.systemErr.isEmpty) "(empty)" else suite.systemErr.take(300)}\n$rendered"
        )
      case ExplanationAt.Nowhere =>
        assert(
          !suite.systemOut.contains(fixture.failingTestName) && !suite.systemErr.contains(fixture.failingTestName),
          s"$context: recorded as reporting the reason nowhere, but the captured output does mention '${fixture.failingTestName}'. " +
            s"Point `explanation` at whichever stream it is in.\n$rendered"
        )
    }

    // A suite that cannot even be constructed must be *reported*, not swallowed. The dangerous outcome is not a bad message — it is a green run: the
    // framework fails to build the class, reports nothing, and the suite silently disappears from the results. Checked by count rather than by name because
    // frameworks name it differently (JUnit synthesises `initializationError`, others attribute it to the suite itself).
    if (fixture.hasCtorErrorVariant && fixture.ctorFailureReport(platform.id) != CtorFailureReport.Hangs) {
      val ctorSuites = suites.filter(_.name.contains(fixture.ctorErrorSuiteFqn.split('.').last))
      assert(
        ctorSuites.nonEmpty,
        s"$context: the suite whose constructor throws is missing from the report entirely — a failure that disappears is worse than one reported badly.\n" +
          rendered
      )
      // Except where the framework swallows it. Then there is nothing to mark: bleep is told the suite ran no tests, which is what an empty suite looks
      // like too, and empty suites are legal. Asserted in the negative so the day the framework is fixed, this fails and the record gets updated.
      if (fixture.ctorFailureReport(platform.id) == CtorFailureReport.NothingButSilence)
        assert(
          ctorSuites.forall(cs => cs.failures + cs.errors == 0),
          s"$context: ${fixture.name} is recorded as swallowing construction failures entirely, but the report now marks something failed. Promote it.\n" +
            rendered
        )
      else
        assert(
          ctorSuites.exists(cs => cs.failures + cs.errors > 0),
          s"$context: the suite whose constructor throws is in the report but nothing about it is marked failed, so a broken suite would read as green.\n" +
            rendered
        )
      // And *what* it says, which is the part that differs between frameworks and the part a reader actually needs. A construction failure is not a failing
      // test and not an empty suite; where the thrown exception survives, the report must carry it.
      val ctorDetail = ctorSuites.flatMap(_.cases).map(_.detail).mkString("\n")
      // Same for the constructor failure, where frames matter most: without them you are told a class could not be built and nothing about which line threw.
      if (fixture.reportsCtorStackFrames(platform.id) && fixture.ctorFailureReport(platform.id) == CtorFailureReport.NamesTheCause)
        assert(
          PlatformFrameworkHarness.hasStackFrames(ctorDetail),
          s"$context: the constructor failure reached the report but with no stack frames.\nIt said: ${ctorDetail.take(220)}\n$rendered"
        )

      fixture.ctorFailureReport(platform.id) match {
        case CtorFailureReport.NamesTheCause =>
          assert(
            ctorDetail.contains("ctor boom"),
            s"$context: ${fixture.name} is recorded as carrying the thrown exception through to the report, but the report does not contain it. " +
              s"It said: ${ctorDetail.take(220)}\n$rendered"
          )
        case CtorFailureReport.FailureWithoutCause =>
          assert(
            !ctorDetail.contains("ctor boom"),
            s"$context: ${fixture.name} is recorded as reporting the failure without the cause, but the cause now arrives — promote it to NamesTheCause.\n" +
              rendered
          )
          assert(
            ctorDetail.trim.nonEmpty,
            s"$context: ${fixture.name} is recorded as reporting the failure without the cause, but the report says nothing at all — that is " +
              s"NothingButSilence.\n$rendered"
          )
        case CtorFailureReport.Hangs =>
          // Nothing to assert about the message: the run never got one. The suite is killed by the idle timeout, which the run reports separately.
          ()
        case CtorFailureReport.NothingButSilence =>
          assert(
            !ctorDetail.contains("ctor boom"),
            s"$context: ${fixture.name} is recorded as swallowing construction failures, but the exception now reaches the report. Promote it.\n$rendered"
          )
          assert(
            ctorSuites.flatMap(_.cases).isEmpty,
            s"$context: ${fixture.name} is recorded as reporting nothing at all for a construction failure, but the report has cases for it.\n$rendered"
          )
      }
    }

    // The green suite: a suite where nothing failed must come back as a suite that passed. This is bleep's claim, not the framework's, and it is the one
    // claim the rest of this file cannot make — every other suite here is red, so "reports success" was going entirely unchecked.
    if (fixture.hasGreenVariant) {
      val greenSimple = fixture.greenSuiteFqn.split('.').last
      val green = suites.filter(_.name.contains(greenSimple))
      assert(
        green.nonEmpty,
        s"$context: the all-passing suite ${fixture.greenSuiteFqn} is missing from the report. It was selected alongside the others, so its absence means " +
          s"a suite that passes can be dropped from a run that had failures in it.\n$rendered"
      )
      green.foreach { g =>
        assert(
          g.failures + g.errors == 0,
          s"$context: ${fixture.greenSuiteFqn} contains only passing tests but the report marks ${g.failures} failure(s) and ${g.errors} error(s) on it. " +
            s"A failure from a neighbouring suite has been attributed to this one.\n$rendered"
        )
        assert(
          g.tests == fixture.passingTestNames.size,
          s"$context: expected ${fixture.passingTestNames.size} test(s) in ${fixture.greenSuiteFqn}, the report has ${g.tests}.\n$rendered"
        )
      }
    }

    // Frames, for the exception a test threw. Asserted separately from the message because they answer different questions and are lost by different bugs:
    // the message survived a reducer that dropped the throwable, and only the frames revealed it.
    if (fixture.reportsStackFrames && reporting.uncaughtException == ThrowableKind.Real) {
      // By name, not by searching the text for "boom". munit renders an assertion failure with an excerpt of the source around it, and the fixture's next
      // line is `test("throws on purpose") { throw new RuntimeException("boom") }` — so the *assertion* failure's detail contains the word too, and a text
      // search picked whichever of the two the report happened to list first.
      val boomDetail = notPassing.find(_.name == fixture.reportedThrowingName).map(_.detail).getOrElse("")
      assert(
        PlatformFrameworkHarness.hasStackFrames(boomDetail),
        s"$context: the uncaught exception reached the report but with no stack frames, so it says what was thrown and not where. " +
          s"It said: ${boomDetail.take(220)}\n$rendered"
      )
    }

    assert(
      suite.systemOut.contains(fixture.stdoutMarker),
      s"$context: the suite's first test printed '${fixture.stdoutMarker}' to stdout, but it is not in the report's <system-out>. Test output is captured " +
        s"per platform and two platforms used to drop it entirely, which looks exactly like a test that printed nothing.\n" +
        s"  <system-out> was: ${if (suite.systemOut.isEmpty) "(empty)" else suite.systemOut.take(400)}\n$rendered"
    )
    assert(
      suite.skipped == fixture.expectedSkipped,
      s"$context: expected skipped=${fixture.expectedSkipped}, got skipped=${suite.skipped}. A skip is its own reported status: a runner that " +
        s"turns it into a pass, or drops the case entirely, keeps the pass/fail counts right while losing the distinction.\n$rendered"
    )
    // No separate duplicate check: the name comparisons above are between sorted *lists*, so they already compare multiplicities. A case reported twice makes
    // the list longer and fails there. A set comparison would not, which is why these are lists — and why minitest, which genuinely reports all four of its
    // cases under the suite's own name because it echoes back the `SuiteSelector` it was handed, still passes.
    succeed
  }

  /** The whole check for one combination: run it, then assert it ran. */
  protected def checkFixture(
      ws: Workspace,
      fixture: TestFrameworkFixture,
      frameworkVersion: String,
      platform: FixturePlatform
  ): org.scalatest.Assertion = {
    val (verdict, suites) = runFixture(ws, fixture, frameworkVersion, platform)
    // The run must report failure, because every fixture contains a deliberately failing test and a deliberately throwing one. A green verdict here means
    // neither ran, or their outcomes were swallowed — both defects this suite exists to catch.
    assert(
      verdict.isLeft,
      s"${fixture.name} $frameworkVersion on ${platform.describe}: the run reported success, but the fixture contains a test that must fail.\n" +
        suites.map("  " + _.describe).mkString("\n")
    )
    assertFixtureRan(fixture, frameworkVersion, platform, suites, verdict)
  }
}
