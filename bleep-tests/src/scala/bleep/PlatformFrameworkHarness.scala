package bleep

import bleep.commands.{DisplayMode, ReactiveBsp}
import java.nio.file.Path

/** Which platform a fixture runs on, and the `platform:` block that selects it. */
sealed abstract class FixturePlatform(val id: String) {
  def platformYaml: String
  def scalaVersion: String

  /** Used in test names, so a failure says which combination broke without opening the file. */
  def describe: String
}

object FixturePlatform {
  case class Jvm(scalaVersion: String) extends FixturePlatform("jvm") {
    def describe: String = s"jvm / scala $scalaVersion"
    def platformYaml: String =
      """    platform:
        |      name: jvm
        |""".stripMargin
  }

  case class Js(scalaVersion: String, jsVersion: String, nodeVersion: String) extends FixturePlatform("js") {
    def describe: String = s"js / scala $scalaVersion / scalajs $jsVersion"
    def platformYaml: String =
      s"""    platform:
         |      name: js
         |      jsVersion: $jsVersion
         |      jsNodeVersion: $nodeVersion
         |      jsKind: none
         |""".stripMargin
  }

  case class Native(scalaVersion: String, nativeVersion: String) extends FixturePlatform("native") {
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
trait PlatformFrameworkHarness { self: IntegrationTestHarness =>

  protected val projectName: String = "mytest"
  protected val project: model.CrossProjectName = model.CrossProjectName(model.ProjectName(projectName), None)

  protected def yamlFor(fixture: TestFrameworkFixture, platform: FixturePlatform): String = {
    val deps = fixture.deps.map(d => s"    - $d").mkString("\n")
    // A Java fixture gets no `scala:` block at all. Declaring a Scala version for a project with no Scala sources would pull the compiler and the standard
    // library in for nothing, and would stop the fixture from proving that a plain Java test project works.
    val scalaBlock = fixture.language match {
      case FixtureLanguage.Scala => s"    scala:\n      version: ${platform.scalaVersion}\n"
      case FixtureLanguage.Java  => ""
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
      platform: FixturePlatform
  ): (Either[BleepException, Unit], List[JUnitReports.Suite]) = {
    ws.yaml(yamlFor(fixture, platform))
    ws.file(s"$projectName/src/${fixture.language.sourceDir}/${fixture.relPath}", fixture.source)
    val (started, _, _) = ws.start()
    val reportDir: Path = ws.root.resolve("junit-reports")
    val verdict = ReactiveBsp
      .test(
        watch = false,
        projects = Array(project),
        displayMode = DisplayMode.NoTui,
        jvmOptions = Nil,
        testArgs = Nil,
        only = Nil,
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
      platform: FixturePlatform,
      suites: List[JUnitReports.Suite],
      verdict: Either[BleepException, Unit]
  ): org.scalatest.Assertion = {
    val context = s"${fixture.name} on ${platform.id}"
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
      failedNames == List(fixture.reportedFailingName),
      s"$context: expected exactly one failing test '${fixture.reportedFailingName}' but got ${failedNames.mkString(", ")}.\n$rendered"
    )
    assert(
      suite.tests == fixture.expectedTotal,
      s"$context: expected ${fixture.expectedTotal} test cases, got ${suite.tests}.\n$rendered"
    )
    succeed
  }

  /** The whole check for one combination: run it, then assert it ran. */
  protected def checkFixture(ws: Workspace, fixture: TestFrameworkFixture, platform: FixturePlatform): org.scalatest.Assertion = {
    val (verdict, suites) = runFixture(ws, fixture, platform)
    // The run must report failure, because every fixture contains a deliberately failing test. A green verdict here means the failing test did not run or its
    // failure was swallowed — both defects this suite exists to catch.
    assert(
      verdict.isLeft,
      s"${fixture.name} on ${platform.id}: the run reported success, but the fixture contains a test that must fail.\n" +
        suites.map("  " + _.describe).mkString("\n")
    )
    assertFixtureRan(fixture, platform, suites, verdict)
  }
}
