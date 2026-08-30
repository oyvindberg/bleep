package bleep

/** Which language a fixture's source is written in, and therefore where it lives and how its project is configured. */
sealed abstract class FixtureLanguage(val sourceDir: String)
object FixtureLanguage {
  case object Scala extends FixtureLanguage("scala")
  case object Java extends FixtureLanguage("java")
  case object Kotlin extends FixtureLanguage("kotlin")
}

/** One test framework, as build dependencies plus a suite source.
  *
  * The same fixture runs on every platform that claims to support the framework, and the source is byte-identical across them — that identity is the assertion.
  * `bleep test` on a JVM project and on a Scala.js project compile the same file, so any difference in the reported result is bleep's, not the test's.
  *
  * Every fixture declares two passing tests and one failing one. A suite that only passes cannot distinguish "ran and passed" from "did not run": issue #655 is
  * precisely a runner that reported no individual results, and a green-only fixture would have been satisfied by a runner that executed nothing. The failing
  * test also proves the failure path reports a count rather than swallowing the assertion.
  */
/** What kind of throwable, if any, reaches a failing test case.
  *
  * The distinction that matters is not "is there one" but "is it the one that failed the test". Several frameworks construct an exception at *report* time
  * purely to satisfy `sbt.testing.Event`, carrying no message and a stack that runs through their own reporter — hedgehog's `MessageOnlyException` is built in
  * `hedgehog.sbt.Event$.fromReport`, and ScalaTest's Scala Native reporter hands over a bare `java.lang.Throwable` from `SbtReporter.getOptionalThrowable`.
  * Those look like information and are not, which is why a plain "is the field non-empty" check reported them as fine.
  */
/** What the report says when a suite cannot be *constructed* — a different event from a suite that ran zero tests, and from a test that failed.
  *
  * Measured per framework rather than assumed. The target is the shape bleep already achieves when a forked JVM dies: say plainly what went wrong, name the
  * cause, and keep whatever else the run produced.
  */
sealed trait CtorFailureReport
object CtorFailureReport {

  /** The thrown exception reaches the report — its message, its type, or both. Either the framework passed it on, or it escaped `task.execute` and bleep
    * wrapped it. This is the outcome to want: the reader is told what was thrown and usually where.
    */
  case object NamesTheCause extends CtorFailureReport

  /** The failure is reported and correctly attributed to the suite, but what was thrown is not in it. Three different shapes land here: the framework
    * substitutes its own wording (jqwik's "Cannot create instance of class", specs2's "cannot create an instance for"), or it reports only the wrapper it
    * caught (`ExceptionInInitializerError`, `InvocationTargetException`) with the cause dropped. From a reader's seat these are the same: the test suite is
    * red, and the reason is not here.
    */
  case object FailureWithoutCause extends CtorFailureReport

  /** Neither tests nor an error: the framework catches the exception thrown while constructing the suite and reports an empty suite instead.
    *
    * This is a bug in the framework, and it is the serious one. bleep is told the suite ran no tests, which is indistinguishable from a suite that has no tests
    * in it — an ordinary thing to have — so the failure cannot be surfaced without also failing every empty scaffold class. bleep reports what it was told; the
    * test that blew up on construction is invisible, and the build is green.
    *
    * Recorded here so it is a known, named defect in one framework rather than a tax on everyone: the alternative, failing every suite that runs no tests, was
    * tried and broke ordinary use.
    */
  case object NothingButSilence extends CtorFailureReport

  /** Worse than silence: the run hangs until the suite idle timeout fires, and is then reported as a timeout rather than as the failure it is. Costs two
    * minutes per occurrence and points the reader at the wrong problem.
    */
  case object Hangs extends CtorFailureReport
}

sealed trait ThrowableKind
object ThrowableKind {

  /** The exception that actually failed the test: its frames run through your own test class. */
  case object Real extends ThrowableKind

  /** One built by the framework's adapter when it reported the result. There is no message, and the stack is the construction site inside the framework, so it
    * says nothing about your test. Not a transport failure — the real exception usually never existed.
    */
  case object Fabricated extends ThrowableKind

  /** No throwable on the event at all. */
  case object Absent extends ThrowableKind
}

/** Where a human can read why the test failed, when the case itself does not say. */
sealed trait ExplanationAt
object ExplanationAt {

  /** On the failing test, which is where you would look first. */
  case object OnCase extends ExplanationAt

  /** In the suite's captured standard output. */
  case object CapturedStdout extends ExplanationAt

  /** In the suite's captured error output — where several frameworks log their failure summaries. */
  case object CapturedStderr extends ExplanationAt

  /** Nowhere bleep can find it. The test is reported as failed and no explanation reaches the report at all — the worst case, and worth naming so the
    * documentation can say so plainly rather than implying the reason is somewhere the reader has not looked hard enough.
    */
  case object Nowhere extends ExplanationAt
}

/** How a failure is reported for one framework on one platform.
  *
  * Split by *how* the test failed, because the two travel different paths: an assertion failure is something the framework decides, while an uncaught exception
  * is a real throwable it merely passes along. Frameworks that model assertions as values (weaver, hedgehog) still forward genuine exceptions.
  */
case class FailureReporting(
    assertionFailure: ThrowableKind,
    uncaughtException: ThrowableKind,
    explanation: ExplanationAt
)

object FailureReporting {

  /** The real exception in both cases, attached to the test. What every framework should do, and most do. */
  val Full: FailureReporting = FailureReporting(ThrowableKind.Real, ThrowableKind.Real, ExplanationAt.OnCase)
}

case class TestFrameworkFixture(
    /** Short name used in test names. */
    name: String,
    /** Dependency lines for bleep.yaml, as a function of the framework version under test. `::` for Scala artifacts so the platform suffix (`_sjs1`,
      * `_native0.5`) is applied per project; `:` for Java ones.
      */
    deps: String => List[String],
    /** Framework versions to exercise, newest first.
      *
      * The head is the version [[TestFrameworkMatrixIT]] pins, and is what CI runs. The tail exists for [[TestFrameworkVersionMatrixIT]], which is not run
      * automatically: a framework's older releases fingerprint their suites differently, name their tests differently, and in several cases predate the
      * platform artifact bleep injects alongside them. Those are the combinations where bleep's discovery and runner selection are most likely to be wrong, and
      * the least likely to be noticed, because nobody upgrades a build tool and an old test framework on the same day.
      */
    versions: List[String],
    language: FixtureLanguage,
    /** Path under the source directory, e.g. `example/MunitFixture.scala`. */
    relPath: String,
    /** Fully qualified suite name, as `bleep test` reports it. */
    suiteFqn: String,
    source: String,
    /** Extra files the fixture needs beside its suite, as (path relative to the project directory, content).
      *
      * Only Cucumber uses this today, and it is the reason the field exists: a Cucumber run is driven by `.feature` resources and step definitions, not by one
      * annotated class, so a fixture model that can express only a single source file cannot describe it at all.
      */
    extraFiles: List[(String, String)],
    /** Test names as written in the source. */
    testNames: List[String],
    /** The one test that must fail an assertion, as written in the source. */
    failingTestName: String,
    /** The one test that must throw an uncaught exception, as written in the source.
      *
      * Separate from [[failingTestName]] because the two travel different paths. sbt-testing has distinct `Status.Failure` and `Status.Error`, JUnit XML has
      * `<failure>` and `<error>`, and a runner can get one right while swallowing the other — an exception thrown outside an assertion is the one most likely
      * to escape a framework's own reporting and reach bleep as a dead suite rather than a failed test.
      */
    throwingTestName: String,
    /** The one test that must be reported as skipped, when the framework has such a concept.
      *
      * `None` where it does not: utest, ScalaCheck, hedgehog, minitest and JUnit 3 offer no way to mark a test ignored, so there is nothing to assert and
      * nothing missing. Stated per framework rather than assumed, because "skipped" is a real reported status — it is neither a pass nor a failure — and a
      * runner that quietly turns skips into passes, or drops them, is wrong in a way no pass/fail count reveals.
      */
    skippedTestName: Option[String],
    /** How a failure is reported, per platform.
      *
      * Declared per framework rather than derived, because it is a property of the framework's own reporting and not something bleep can infer: weaver and
      * hedgehog hand back an event with no throwable at all, and ScalaTest loses detail on the non-JVM adapters while munit, utest and ScalaCheck keep theirs
      * on the very same platforms. Stating it here is what lets both the matrix assert it and the docs render it from one source.
      */
    /** Keyed by platform *and* Scala binary version, because both matter. ScalaTest on Scala.js keeps a frame naming your test class under Scala 3 but not
      * under 2.13, where the test body compiles to an anonymous JS function — same framework, same platform, different amount of help.
      */
    failureReporting: (String, Option[String]) => FailureReporting,
    /** How this framework renders a test name to sbt-testing.
      *
      * Frameworks disagree, and the difference is visible to users in `bleep test` output and in JUnit XML: munit prefixes the suite's fully qualified name,
      * ScalaTest and utest report the bare name, JUnit appends parentheses. Pinning it per framework keeps the assertion exact instead of loosening it to a
      * suffix match that a runner reporting one synthetic case could satisfy.
      */
    reportedName: String => String,
    /** Platform ids this framework runs on, at a given framework version.
      *
      * A function of the version for the same reason [[scalaBinaryVersions]] is: support is not a property of a framework, it is a property of a release. A
      * framework absent from a platform is not a bleep defect, so it is simply not run there.
      */
    platforms: String => Set[String],
    /** Scala binary versions this framework publishes for, *at a given framework version*.
      *
      * A function of the version rather than a flat set, because support moves over a framework's life: scalatest 3.1.4 predates Scala 3 entirely and there is
      * no `scalatest_3:3.1.4` to resolve, while 3.2.15 beside it is fine. A combination that was never published has to be skipped rather than attempted — the
      * failure is a coursier `CantDownloadModule`, which reads exactly like a bleep defect and would train people to ignore red in this matrix.
      *
      * Java and Kotlin fixtures declare all of them: their artifacts carry no Scala suffix, so the project's Scala version cannot affect whether they resolve.
      */
    scalaBinaryVersions: String => Set[String]
) {

  /** The version CI pins. Every other version in [[versions]] is reached only by the manual matrix. */
  def currentVersion: String = versions.head

  /** `scalaBinaryVersion` is `None` on a Kotlin platform, where nothing carries a Scala suffix and the axis therefore cannot rule anything out. */
  def supports(platformId: String, scalaBinaryVersion: Option[String], frameworkVersion: String): Boolean =
    platforms(frameworkVersion).contains(platformId) && scalaBinaryVersion.forall(scalaBinaryVersions(frameworkVersion).contains)

  def passingTestNames: List[String] =
    testNames.filterNot(n => n == failingTestName || n == throwingTestName || skippedTestName.contains(n)).map(reportedName)

  def expectedSkipped: Int = skippedTestName.size
  def reportedFailingName: String = reportedName(failingTestName)
  def reportedThrowingName: String = reportedName(throwingTestName)

  /** Both non-passing cases, as the report will name them. Not split by `failure` vs `error`: sbt-testing lets a framework choose, and several report an
    * uncaught exception as a Failure carrying the throwable. What must hold is that neither case is reported as passing and neither goes missing.
    */
  def reportedNotPassingNames: List[String] = List(reportedFailingName, reportedThrowingName).sorted
  def expectedPassed: Int = passingTestNames.size

  /** The deliberately failing test plus the deliberately throwing one. */
  def expectedNotPassing: Int = 2

  private def simpleName: String = suiteFqn.split('.').last

  /** A second suite in the same project, identical but for its name.
    *
    * It exists to prove that asking for one suite runs one suite. Every framework here embeds its own name in its source — as the class name, and sometimes
    * again as a string it reports itself by — so renaming that one token yields a valid second suite without hand-writing thirteen more fixtures.
    *
    * If selection works the decoy is never run and the expected counts are unchanged. If selection is ignored, its tests run too and every count doubles, which
    * every assertion in [[PlatformFrameworkHarness.assertFixtureRan]] then catches. Kotlin/JS ignored its suite list outright — the parameter was marked
    * `@annotation.unused` — and nothing noticed, because until now no fixture ever had a second suite to leave out.
    */
  /** What every fixture's first test prints, so the matrix can assert that test output survives the round trip.
    *
    * Asserted per framework and per platform because output capture is a per-platform mechanism, and two of the five were silently dropping it: Scala.js
    * emitted no `<system-out>` element at all, and Scala Native's binary printed to a detached daemon's file descriptors. Both are indistinguishable from a
    * test that printed nothing, which is why a count-based assertion would never have caught either.
    */
  def stdoutMarker: String = TestFrameworkFixture.StdoutMarker

  /** Does a failure report from this framework carry stack frames, not just a message?
    *
    * True everywhere it was measured except minitest's constructor failure, which reports nothing at all. A message tells you *what* went wrong; the frames
    * tell you *where*, and for an exception thrown from a constructor the message alone ("ctor boom") is close to useless on its own.
    */
  def reportsStackFrames: Boolean = name != "minitest"

  /** And for a *constructor* failure specifically, which travels a different path.
    *
    * hedgehog and zio-test surface it through bleep's own "Error running suite …" wrapper: the message names the exception, but the frames are not carried onto
    * the case. minitest reports nothing at all.
    */
  def reportsCtorStackFrames(platformId: String): Boolean =
    (name, platformId) match {
      // ScalaTest's Scala Native reporter hands over an exception it built itself, so there are no frames from the failure on it.
      case ("scalatest", "native") | ("scalacheck", "native") => false
      case _                                                  => !Set("minitest", "hedgehog", "zio-test").contains(name)
    }

  /** Classify what actually arrived on a failing case, so a declaration can be checked against reality rather than trusted.
    *
    * "Real" is not "the field is populated" — that is the check this replaced, and it reported hedgehog's empty `MessageOnlyException` as a proper report. It
    * is "does this tell you why the test failed", which is true when either of two things holds:
    *
    *   - it carries a message. ScalaCheck's `Falsified after 0 passed tests. > ARG_0: 0` names no user code and is still exactly what you need.
    *   - or its stack reaches the fixture's own code. Cucumber throws a plain `AssertionError` whose message is just the step name, but the frames point at
    *     `example.Steps`, which is what makes it useful.
    *
    * Everything the fixtures generate lives in package `example`, so a frame naming it is a frame in the code under test.
    */
  def classifyThrowable(message: Option[String], detail: String): ThrowableKind =
    if (detail.trim.isEmpty) ThrowableKind.Absent
    else if (message.exists(_.trim.nonEmpty) || detail.contains("example.")) ThrowableKind.Real
    else ThrowableKind.Fabricated

  /** Why this combination reports less than the whole story, and where the reader can still find the answer. Empty when nothing is missing. */
  def fidelityCause(platformId: String, scalaBinaryVersion: Option[String]): String = {
    val reporting = failureReporting(platformId, scalaBinaryVersion)
    val where = reporting.explanation match {
      case ExplanationAt.CapturedStderr =>
        "the suite's captured error output — its `<system-err>` section, which CI viewers usually show as a *Standard error* tab on the suite rather than " +
          "on the failing test"
      case ExplanationAt.CapturedStdout => "the suite's captured output — its `<system-out>` section, shown on the suite rather than on the failing test"
      case ExplanationAt.OnCase         => "the failing test itself"
      case ExplanationAt.Nowhere        => "— nowhere bleep can reach: no explanation arrives at all"
    }
    val cause = (name, platformId) match {
      case ("weaver", _) | ("hedgehog", "jvm") =>
        s"A failed assertion here is an ordinary value rather than a thrown exception, so there is no exception in existence for bleep to attach — nothing " +
          s"is lost in transit, there was never anything to carry. The test is still correctly marked failed. The reason is in $where."
      case ("hedgehog", _) =>
        s"hedgehog's Scala.js adapter builds a `MessageOnlyException` in `Event$$.fromReport` to satisfy the interface. It carries no message and its stack " +
          s"is that construction, so it looks like a report and tells you nothing. The reason is in $where."
      case ("scalatest", "js") =>
        s"The exception is genuinely ScalaTest's `TestFailedException` — it is not a stand-in — but under Scala 2.13 it reaches you carrying nothing you " +
          s"can act on: ScalaTest's assertion failures have no message, and the frame for your own test compiles to an anonymous JavaScript function " +
          s"(`<jscode>.{anonymous}()`), so the stack names only ScalaTest's internals. Under Scala 3 the same test keeps a frame naming your test class, " +
          s"which is why the two are separate columns above. The reason is in $where."
      case ("scalatest", "native") =>
        s"ScalaTest's own Scala Native reporter is the cause, not the platform: `SbtReporter.getOptionalThrowable` hands over a freshly built, empty " +
          s"`java.lang.Throwable` instead of the exception that failed the test — the stack on it is that construction, identical whether the test failed an " +
          s"assertion or threw. The real exception never leaves ScalaTest, so there is nothing for bleep to recover; fixing it means fixing ScalaTest. " +
          s"The reason is in $where, which still names the exception type and the source line. For its full text, run the same test on the JVM or Scala.js."
      case _ => ""
    }
    cause
  }

  private def decoySimpleName: String = s"Decoy$simpleName"

  def decoySource: String = source.replace(simpleName, decoySimpleName)

  def decoyRelPath: String = relPath.replace(simpleName, decoySimpleName)

  /** Prefixed, not suffixed, and that is the whole point.
    *
    * `--only` matches by substring (`MultiWorkspaceBspServer.filterSuites`), so a decoy named `example.MunitFixtureDecoy` is matched by `--only
    * example.MunitFixture` — correctly, per those semantics. Such a decoy can never be excluded and therefore proves nothing: it ran alongside the real suite
    * on every platform and the doubled counts read as normal. `example.DecoyMunitFixture` shares no prefix, so selection has something it can actually get
    * wrong.
    */
  def decoySuiteFqn: String = suiteFqn.replace(simpleName, decoySimpleName)

  /** Does a "suite whose constructor throws" variant mean anything for this framework?
    *
    * Cucumber is the one that says no. Its discovered class is not a test class at all — it is a `@Suite` declaration whose annotations point the engine at
    * `.feature` resources, and the tests come from those. A second copy of that declaration selects the same features again, and a constructor failure in it
    * stops the engine finding anything, so the variant would be testing JUnit's suite-aggregation rather than the failure mode this is about.
    */
  def hasCtorErrorVariant: Boolean = name != "cucumber"

  /** How this framework reports a suite whose construction throws, on this platform. Measured, one combination at a time, by running exactly that.
    *
    * Platform-aware because it genuinely varies: munit reports the failure in under a second on the JVM and on Scala Native, and hangs for the full idle
    * timeout on Scala.js.
    */
  def ctorFailureReport(platformId: String): CtorFailureReport =
    (name, platformId) match {
      // Measured per combination. The behaviour is the framework's, and several frameworks behave differently once they are running on a linked artifact
      // rather than on the JVM — so this cannot be keyed on the framework alone.
      case ("munit", "js")         => CtorFailureReport.Hangs
      case ("scalatest", "native") => CtorFailureReport.FailureWithoutCause
      case ("scalacheck", "jvm")   => CtorFailureReport.FailureWithoutCause
      case ("minitest", _)         => CtorFailureReport.NothingButSilence
      case ("testng", _)           => CtorFailureReport.FailureWithoutCause
      case _                       => CtorFailureReport.NamesTheCause
    }

  private def greenSimpleName: String = s"Green$simpleName"

  /** A third suite that simply passes, so a run has something green in it to compare the failures against.
    *
    * Without one, every suite in every recording is red, and "2 passed" buried in a suite marked FAILED is not the same as seeing a suite complete. The
    * distinction it pins is bleep's, not the framework's: a suite where nothing failed must report as passing, next to one where something did.
    *
    * Derived from the fixture rather than written out per framework, for the same reason the constructor-failure variant is: eighteen hand-maintained copies of
    * the same suite drift, and the copy that drifts is the one nobody looks at because it is green. The cut is textual but not naive — it removes the failing
    * test's declaration together with the annotation lines above it, which is what stops a dangling `@Test` from re-annotating whatever came next.
    */
  def greenSource: String = {
    require(hasGreenVariant, s"$name: has no green variant")
    // Cucumber has no tests in its source to cut: the suite class is an empty declaration and the scenarios live in a feature file it selects by directory.
    // So its green suite is the same declaration pointed at a directory holding only the passing scenarios. Still derived, not a second copy to maintain.
    if (name == "cucumber")
      source
        .replace(simpleName, greenSimpleName)
        .replace("""@SelectClasspathResource("example")""", """@SelectClasspathResource("greenexample")""")
    else greenSourceByCutting
  }

  private def greenSourceByCutting: String = {
    val renamed = source.replace(simpleName, greenSimpleName)
    val lines = renamed.split('\n').toList
    val failIdx = lines.indexWhere(_.contains(failingTestName))
    require(failIdx >= 0, s"$name: could not find '$failingTestName' to cut the green suite at")
    // Back over the annotations and blank line that belong to the test being removed. In Java and Kotlin the `@Test` sits on its own line above the method,
    // and leaving it behind would annotate the closing brace's neighbour instead.
    //
    // "Annotation line" has to mean a line that is *only* annotations, not merely one that starts with `@`. kotlin.test writes the whole test as
    // `@Test fun adds() { … }`, and a looser rule walks back over every test in the suite and hands back an empty class.
    def annotationOnly(line: String): Boolean = {
      val withoutArgs = line.replaceAll("""\([^()]*\)""", "")
      val tokens = withoutArgs.trim.split("\\s+").filter(_.nonEmpty)
      tokens.nonEmpty && tokens.forall(_.startsWith("@"))
    }
    val cutFrom = {
      var i = failIdx
      while (i > 0 && { val t = lines(i - 1).trim; t.isEmpty || annotationOnly(t) }) i -= 1
      i
    }
    // Everything after the last test is the type's closing punctuation — `}` in Scala and Java, `})` where the body is a lambda — and it has to come back.
    val lastTestIdx = lines.lastIndexWhere(l => testNames.exists(l.contains))
    require(lastTestIdx >= failIdx, s"$name: the failing test is not among the last of the suite's tests")
    (lines.take(cutFrom) ::: lines.drop(lastTestIdx + 1)).mkString("\n")
  }

  def greenRelPath: String = relPath.replace(simpleName, greenSimpleName)

  def greenSuiteFqn: String = suiteFqn.replace(simpleName, greenSimpleName)

  /** Every framework has one. Cucumber gets there differently — see [[greenSource]] — but a run with no green suite in it is a run that cannot show the
    * difference between a suite that failed and a suite that passed, which is most of what these recordings are for.
    */
  def hasGreenVariant: Boolean = true

  private def ctorErrorSimpleName: String = s"CtorBoom$simpleName"

  /** A third suite whose *construction* throws, to pin what happens on a failure mode every framework meets and none documents the same way.
    *
    * A test body throwing is ordinary; a suite that cannot be built at all is different in kind — there is no test to attach the failure to, and a runner can
    * plausibly report it as an error, as a synthetic failed test, or (worst, and the thing this guards) swallow it and report an empty green suite. It also has
    * to not take the rest of the run down with it: the real suite in the same project must still run and report normally.
    *
    * Derived from the fixture rather than hand-written per framework, so it cannot drift from the suite it mirrors. The throw goes at the top of the type's
    * body, which is construction time in every language here; where the body is a lambda passed to a constructor — kotest's `FunSpec({ … })`, Spek's
    * `Spek({ … })` — the same position is inside that lambda, which is still construction.
    */
  def ctorErrorSource: String = {
    val renamed = source.replace(simpleName, ctorErrorSimpleName)
    val lines = renamed.split('\n').toList
    val declIdx = lines.indexWhere(l => l.contains(ctorErrorSimpleName) && l.contains("{"))
    require(declIdx >= 0, s"$name: could not find the type declaration to inject a constructor failure into")
    val isLambdaBody = lines(declIdx).contains("({")
    val stmt =
      if (isLambdaBody) """  throw new RuntimeException("ctor boom")"""
      else
        language match {
          case FixtureLanguage.Scala => """  throw new RuntimeException("ctor boom")"""
          // An instance initializer. `if (true)` because javac rejects statements it can prove unreachable, and a bare `throw` here makes the rest of the
          // body exactly that.
          case FixtureLanguage.Java   => """  { if (true) throw new RuntimeException("ctor boom"); }"""
          case FixtureLanguage.Kotlin => """  init { throw RuntimeException("ctor boom") }"""
        }
    val fixed = if (isLambdaBody && language == FixtureLanguage.Kotlin) """  throw RuntimeException("ctor boom")""" else stmt
    (lines.take(declIdx + 1) ::: fixed :: lines.drop(declIdx + 1)).mkString("\n")
  }

  def ctorErrorRelPath: String = relPath.replace(simpleName, ctorErrorSimpleName)

  def ctorErrorSuiteFqn: String = suiteFqn.replace(simpleName, ctorErrorSimpleName)
  def expectedTotal: Int = testNames.size
}

object TestFrameworkFixture {
  private val AllPlatforms = Set("jvm", "js", "native")
  private val JvmOnly = Set("jvm")
  private val AllScalaBinaryVersions = Set("2.12", "2.13", "3")

  val munit: TestFrameworkFixture = TestFrameworkFixture(
    name = "munit",
    deps = v => List(s"org.scalameta::munit:$v"),
    versions = List(model.Versions.Munit, "1.0.0", "0.7.29"),
    language = FixtureLanguage.Scala,
    relPath = "example/MunitFixture.scala",
    suiteFqn = "example.MunitFixture",
    // munit's Framework declares SubclassFingerprint(superclassName = "munit.Suite", isModule = false), so a munit suite is a *class*. A runner that only ever
    // looks up module accessors cannot load it — that is the munit half of #655, and the reason this fixture must stay a class.
    source = """package example
               |
               |class MunitFixture extends munit.FunSuite {
               |  test("adds") { println("hello from the test"); assertEquals(1 + 1, 2) }
               |  test("measures") { assertEquals("hello".length, 5) }
               |  test("fails on purpose") { assertEquals(1, 2) }
               |  test("throws on purpose") { throw new RuntimeException("boom") }
               |  test("skipped on purpose".ignore) { assertEquals(1, 1) }
               |}
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose", "skipped on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    skippedTestName = Some("skipped on purpose"),
    failureReporting = (_, _) => FailureReporting.Full,
    reportedName = name => s"example.MunitFixture.$name",
    // Scala Native 0.5 is newer than these releases, which only ever published for 0.4. Not a bleep limitation, and confirmed against Maven Central
    // rather than inferred: the sweep resolves them fine on the JVM and on Scala.js.
    platforms = {
      case "0.7.29" => Set("jvm", "js")
      case _        => AllPlatforms
    },
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  val scalatest: TestFrameworkFixture = TestFrameworkFixture(
    name = "scalatest",
    deps = v => List(s"org.scalatest::scalatest:$v"),
    versions = List("3.2.19", "3.2.15", "3.1.4"),
    language = FixtureLanguage.Scala,
    relPath = "example/ScalatestFixture.scala",
    suiteFqn = "example.ScalatestFixture",
    source = """package example
               |
               |import org.scalatest.funsuite.AnyFunSuite
               |
               |class ScalatestFixture extends AnyFunSuite {
               |  test("adds") { println("hello from the test"); assert(1 + 1 == 2) }
               |  test("measures") { assert("hello".length == 5) }
               |  test("fails on purpose") { assert(1 == 2) }
               |  test("throws on purpose") { throw new RuntimeException("boom") }
               |  ignore("skipped on purpose") { assert(1 == 1) }
               |}
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose", "skipped on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    skippedTestName = Some("skipped on purpose"),
    // Measured, one platform at a time. munit, utest and ScalaCheck keep their detail on these same platforms, so this is ScalaTest's reporting meeting
    // the non-JVM adapters rather than a limit of the platforms.
    failureReporting = {
      // ScalaTest's Scala Native reporter replaces the real exception with an empty `java.lang.Throwable` before bleep sees it, for both failure kinds.
      case ("native", _) => FailureReporting(ThrowableKind.Fabricated, ThrowableKind.Fabricated, ExplanationAt.CapturedStdout)
      // Under Scala 3 the assertion failure still carries a frame naming the test class; under 2.13 the body compiles to an anonymous JS function and the
      // exception arrives with no message and nothing but ScalaTest's own frames.
      case ("js", Some(v)) if v != "3" => FailureReporting(ThrowableKind.Fabricated, ThrowableKind.Real, ExplanationAt.CapturedStdout)
      case _                           => FailureReporting.Full
    },
    reportedName = identity,
    // Scala Native 0.5 is newer than these releases, which only ever published for 0.4. Not a bleep limitation, and confirmed against Maven Central
    // rather than inferred: the sweep resolves them fine on the JVM and on Scala.js.
    platforms = {
      case "3.2.15" | "3.1.4" => Set("jvm", "js")
      case _                  => AllPlatforms
    },
    // 3.1.4 predates Scala 3: there is no `scalatest_3:3.1.4` on Maven Central, so that row is skipped rather than left to fail resolution.
    scalaBinaryVersions = {
      case "3.1.4" => Set("2.12", "2.13")
      case _       => AllScalaBinaryVersions
    }
  )

  val utest: TestFrameworkFixture = TestFrameworkFixture(
    name = "utest",
    deps = v => List(s"com.lihaoyi::utest:$v"),
    versions = List("0.9.1", "0.8.5", "0.7.11"),
    language = FixtureLanguage.Scala,
    relPath = "example/UtestFixture.scala",
    suiteFqn = "example.UtestFixture",
    // A utest suite is an object, the opposite of munit's class. Both shapes have to work, and #655 reports the object shape failing too — it loaded and then
    // died inside `TestRunner.runAsync` — so this is not redundant with the munit fixture.
    source = """package example
               |
               |import utest._
               |
               |object UtestFixture extends TestSuite {
               |  val tests = Tests {
               |    test("adds") { println("hello from the test"); assert(1 + 1 == 2) }
               |    test("measures") { assert("hello".length == 5) }
               |    test("fails on purpose") { assert(1 == 2) }
               |    test("throws on purpose") { throw new RuntimeException("boom") }
               |  }
               |}
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    // utest has no ignore/skip: a case is either in the `Tests` block or it is not.
    skippedTestName = None,
    failureReporting = (_, _) => FailureReporting.Full,
    reportedName = identity,
    // Scala Native 0.5 is newer than these releases, which only ever published for 0.4. Not a bleep limitation, and confirmed against Maven Central
    // rather than inferred: the sweep resolves them fine on the JVM and on Scala.js.
    platforms = {
      case "0.7.11" => Set("jvm", "js")
      case _        => AllPlatforms
    },
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  val scalacheck: TestFrameworkFixture = TestFrameworkFixture(
    name = "scalacheck",
    // The marker prints from the object body, not from inside a property. `property(name) = prop` takes its argument by name and ScalaCheck evaluates it per
    // generated case, so a `println` anywhere inside ran 100 times and buried the rest of the output.
    deps = v => List(s"org.scalacheck::scalacheck:$v"),
    versions = List("1.18.1", "1.17.1", "1.15.4"),
    language = FixtureLanguage.Scala,
    relPath = "example/ScalacheckFixture.scala",
    suiteFqn = "example.ScalacheckFixture",
    // ScalaCheck names its own suite: the string given to `Properties` is what reaches sbt-testing, not the class name. Spelling it as the fully qualified name
    // keeps the reported suite and the discovered class the same string, which every other framework gets for free.
    source = """package example
               |
               |import org.scalacheck.Properties
               |import org.scalacheck.Prop.forAll
               |
               |object ScalacheckFixture extends Properties("example.ScalacheckFixture") {
               |  println("hello from the test")
               |  property("adds") = forAll { (n: Int) => n + 0 == n }
               |  property("measures") = forAll { (s: String) => s.length >= 0 }
               |  property("fails on purpose") = forAll { (n: Int) => n != n }
               |  property("throws on purpose") = forAll { (n: Int) => throw new RuntimeException("boom") }
               |}
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    // ScalaCheck properties have no skipped status — a property holds, fails, or is undecided.
    skippedTestName = None,
    failureReporting = (_, _) => FailureReporting.Full,
    reportedName = name => s"example.ScalacheckFixture.$name",
    // Scala Native 0.5 is newer than these releases, which only ever published for 0.4. Not a bleep limitation, and confirmed against Maven Central
    // rather than inferred: the sweep resolves them fine on the JVM and on Scala.js.
    platforms = {
      case "1.15.4" | "1.17.1" => Set("jvm", "js")
      case _                   => AllPlatforms
    },
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  val specs2: TestFrameworkFixture = TestFrameworkFixture(
    name = "specs2",
    deps = v => List(s"org.specs2::specs2-core:$v"),
    versions = List("4.20.9", "4.19.2"),
    language = FixtureLanguage.Scala,
    relPath = "example/Specs2Fixture.scala",
    suiteFqn = "example.Specs2Fixture",
    // The throwing case is spelled `if (...) throw ... else ok` rather than a bare `throw`: specs2's `>>` is overloaded, a bare `throw` types as `Nothing`,
    // and the compiler cannot pick an overload from it. The condition is always true, so the `ok` branch exists only to give the block a `Result` type.
    source = """package example
               |
               |import org.specs2.mutable.Specification
               |
               |class Specs2Fixture extends Specification {
               |  "adds" >> { println("hello from the test"); (1 + 1) must beEqualTo(2) }
               |  "measures" >> { "hello".length must beEqualTo(5) }
               |  "fails on purpose" >> { 1 must beEqualTo(2) }
               |  "throws on purpose" >> { if (1 + 1 == 2) throw new RuntimeException("boom") else ok }
               |  "skipped on purpose" >> skipped("on purpose")
               |}
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose", "skipped on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    skippedTestName = Some("skipped on purpose"),
    failureReporting = (_, _) => FailureReporting.Full,
    reportedName = identity,
    platforms = _ => Set("jvm", "js"),
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  val minitest: TestFrameworkFixture = TestFrameworkFixture(
    name = "minitest",
    deps = v => List(s"io.monix::minitest:$v"),
    versions = List("2.9.6", "2.8.2"),
    language = FixtureLanguage.Scala,
    relPath = "example/MinitestFixture.scala",
    suiteFqn = "example.MinitestFixture",
    source = """package example
               |
               |import minitest.SimpleTestSuite
               |
               |object MinitestFixture extends SimpleTestSuite {
               |  test("adds") { println("hello from the test"); assertEquals(1 + 1, 2) }
               |  test("measures") { assertEquals("hello".length, 5) }
               |  test("fails on purpose") { assertEquals(1, 2) }
               |  test("throws on purpose") { throw new RuntimeException("boom") }
               |}
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    // minitest offers no ignore marker.
    skippedTestName = None,
    failureReporting = (_, _) => FailureReporting.Full,
    // minitest reports no per-test name: its events echo back whatever selector they were handed (`minitest.runner.Task`'s event returns
    // `taskDef.selectors`), and bleep passes a `SuiteSelector`, exactly as sbt does. So every case arrives under the suite's own name, and the individual
    // names exist only in the framework's logger output. A framework limitation rather than a bleep defect — the counts are still exact, which is what this
    // fixture is really asserting.
    reportedName = _ => "example.MinitestFixture",
    // No `minitest_native0.5_3` is published, so there is nothing for bleep to run there.
    platforms = _ => Set("jvm", "js"),
    // No `minitest_3:2.8.2` was ever published; 2.9.6 is the first of its line to carry a Scala 3 artifact.
    scalaBinaryVersions = {
      case "2.8.2" => Set("2.12", "2.13")
      case _       => AllScalaBinaryVersions
    }
  )

  val junit5: TestFrameworkFixture = TestFrameworkFixture(
    name = "junit5",
    // Only the API. bleep injects the sbt-testing bridge and the junit-platform launcher/engines itself, aligned to whatever junit-platform this classpath
    // carries — see `MultiWorkspaceBspServer.externalTestRunnerDeps`.
    deps = v => List(s"org.junit.jupiter:junit-jupiter:$v"),
    versions = List(model.Versions.JunitJupiter, "5.10.1", "5.7.2"),
    language = FixtureLanguage.Java,
    relPath = "example/Junit5Fixture.java",
    suiteFqn = "example.Junit5Fixture",
    source = """package example;
               |
               |import org.junit.jupiter.api.Test;
               |import org.junit.jupiter.api.Disabled;
               |import static org.junit.jupiter.api.Assertions.assertEquals;
               |
               |public class Junit5Fixture {
               |  @Test
               |  public void adds() { System.out.println("hello from the test"); assertEquals(2, 1 + 1); }
               |
               |  @Test
               |  public void measures() { assertEquals(5, "hello".length()); }
               |
               |  @Test
               |  public void failsOnPurpose() { assertEquals(2, 1); }
               |
               |  @Test
               |  public void throwsOnPurpose() { throw new RuntimeException("boom"); }
               |
               |  @Test
               |  @Disabled("skipped on purpose")
               |  public void skippedOnPurpose() { assertEquals(1, 1); }
               |}
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("adds", "measures", "failsOnPurpose", "throwsOnPurpose", "skippedOnPurpose"),
    failingTestName = "failsOnPurpose",
    throwingTestName = "throwsOnPurpose",
    skippedTestName = Some("skippedOnPurpose"),
    failureReporting = (_, _) => FailureReporting.Full,
    reportedName = name => s"$name()",
    platforms = _ => JvmOnly,
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  val junit4: TestFrameworkFixture = TestFrameworkFixture(
    name = "junit4",
    // The vintage engine that runs these is injected by bleep, same as for junit5.
    deps = v => List(s"junit:junit:$v"),
    versions = List("4.13.2", "4.12"),
    language = FixtureLanguage.Java,
    relPath = "example/Junit4Fixture.java",
    suiteFqn = "example.Junit4Fixture",
    source = """package example;
               |
               |import org.junit.Test;
               |import org.junit.Ignore;
               |import static org.junit.Assert.assertEquals;
               |
               |public class Junit4Fixture {
               |  @Test
               |  public void adds() { System.out.println("hello from the test"); assertEquals(2, 1 + 1); }
               |
               |  @Test
               |  public void measures() { assertEquals(5, "hello".length()); }
               |
               |  @Test
               |  public void failsOnPurpose() { assertEquals(2, 1); }
               |
               |  @Test
               |  public void throwsOnPurpose() { throw new RuntimeException("boom"); }
               |
               |  @Test
               |  @Ignore("skipped on purpose")
               |  public void skippedOnPurpose() { assertEquals(1, 1); }
               |}
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("adds", "measures", "failsOnPurpose", "throwsOnPurpose", "skippedOnPurpose"),
    failingTestName = "failsOnPurpose",
    throwingTestName = "throwsOnPurpose",
    skippedTestName = Some("skippedOnPurpose"),
    failureReporting = (_, _) => FailureReporting.Full,
    // Unlike junit5, whose platform launcher reports "adds()", the vintage path reports the bare method name.
    reportedName = identity,
    platforms = _ => JvmOnly,
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  val zioTest: TestFrameworkFixture = TestFrameworkFixture(
    name = "zio-test",
    deps = v => List(s"dev.zio::zio-test:$v", s"dev.zio::zio-test-sbt:$v"),
    versions = List("2.1.14", "2.0.22"),
    language = FixtureLanguage.Scala,
    relPath = "example/ZioTestFixture.scala",
    suiteFqn = "example.ZioTestFixture",
    source = """package example
               |
               |import zio.test._
               |
               |object ZioTestFixture extends ZIOSpecDefault {
               |  def spec = suite("example.ZioTestFixture")(
               |    test("adds") { println("hello from the test"); assertTrue(1 + 1 == 2) },
               |    test("measures") { assertTrue("hello".length == 5) },
               |    test("fails on purpose") { assertTrue(1 == 2) },
               |    test("throws on purpose") { throw new RuntimeException("boom") },
               |    test("skipped on purpose") { assertTrue(1 == 1) } @@ TestAspect.ignore
               |  )
               |}
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose", "skipped on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    skippedTestName = Some("skipped on purpose"),
    failureReporting = (_, _) => FailureReporting.Full,
    // zio-test prefixes each case with the enclosing suite's label.
    reportedName = name => s"example.ZioTestFixture - $name",
    // JVM only, and not because zio-test lacks a Scala.js build — it has one, it links, its framework loads and a task runs. What comes back is a single
    // suite-level failure carrying no message and no output, which is not enough to say whose defect it is. Left off the JS and Native rows deliberately rather
    // than left failing: an unexplained red in this matrix would train people to ignore it. See the note in the PR.
    platforms = _ => JvmOnly,
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  val weaver: TestFrameworkFixture = TestFrameworkFixture(
    name = "weaver",
    deps = v => List(s"com.disneystreaming::weaver-cats:$v"),
    versions = List("0.8.4", "0.8.3"),
    language = FixtureLanguage.Scala,
    relPath = "example/WeaverFixture.scala",
    suiteFqn = "example.WeaverFixture",
    source = """package example
               |
               |import weaver._
               |
               |object WeaverFixture extends SimpleIOSuite {
               |  pureTest("adds") { println("hello from the test"); expect(1 + 1 == 2) }
               |  pureTest("measures") { expect("hello".length == 5) }
               |  pureTest("fails on purpose") { expect(1 == 2) }
               |  pureTest("throws on purpose") { throw new RuntimeException("boom") }
               |}
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    // `pureTest("name".ignore)` compiles, but the case then disappears from the report entirely rather than arriving as skipped — measured, not assumed.
    // weaver's ignore is a filter, not a status, so there is no skipped case here to assert on.
    skippedTestName = None,
    // Failures are values here, not exceptions: the event carries no throwable, so there is nothing for bleep to put on the case. The framework
    // still prints its reasoning, which arrives in `<system-out>`.
    failureReporting = {
      // No throwable on any platform. Where the summary lands differs: on the JVM weaver logs it at error level, on Scala.js it does not.
      case ("jvm", _) => FailureReporting(ThrowableKind.Absent, ThrowableKind.Real, ExplanationAt.CapturedStderr)
      case _          => FailureReporting(ThrowableKind.Absent, ThrowableKind.Real, ExplanationAt.CapturedStdout)
    },
    reportedName = identity,
    // 0.8.3 is JVM-only here, and this one is bleep's problem as much as weaver's. On Scala.js a test that throws an uncaught exception reports its result and
    // then the suite never finishes: `adds`, `measures`, `fails on purpose` and `throws on purpose` all arrive, and nothing follows. 0.8.4 on the same fixture
    // is fine, and 0.8.3 on the JVM is fine, so it is that release's JS runner failing to complete its task after an error.
    //
    // What makes it worth writing down rather than just skipping: bleep waits forever. `SbtTestingBridge` has no bound on a platform test run, so a user who
    // hits this sees `bleep test` hang with no output and no way to tell why. The matrix only noticed because its own idle timeout fired. Bounding that wait is
    // the real fix and is not attempted here.
    platforms = {
      case "0.8.3" => JvmOnly
      case _       => Set("jvm", "js")
    },
    scalaBinaryVersions = _ => Set("2.13", "3")
  )

  val hedgehog: TestFrameworkFixture = TestFrameworkFixture(
    name = "hedgehog",
    deps = v => List(s"qa.hedgehog::hedgehog-sbt:$v"),
    versions = List("0.10.1", "0.9.0"),
    language = FixtureLanguage.Scala,
    relPath = "example/HedgehogFixture.scala",
    suiteFqn = "example.HedgehogFixture",
    source = """package example
               |
               |import hedgehog._
               |import hedgehog.runner._
               |
               |object HedgehogFixture extends Properties {
               |  def tests: List[Test] = List(
               |    example("adds", { println("hello from the test"); Result.assert(1 + 1 == 2) }),
               |    example("measures", Result.assert("hello".length == 5)),
               |    example("fails on purpose", Result.assert(1 == 2)),
               |    example("throws on purpose", Result.assert(throw new RuntimeException("boom")))
               |  )
               |}
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    // hedgehog has no skip concept.
    skippedTestName = None,
    // Failures are values here, not exceptions: the event carries no throwable, so there is nothing for bleep to put on the case. The framework
    // still prints its reasoning, which arrives in `<system-out>`.
    failureReporting = {
      // On the JVM the event carries no throwable for an assertion failure; on Scala.js hedgehog's adapter builds a `MessageOnlyException` in
      // `Event$.fromReport` instead, which is not the same thing as having one.
      case ("jvm", _) => FailureReporting(ThrowableKind.Absent, ThrowableKind.Real, ExplanationAt.CapturedStdout)
      case _          => FailureReporting(ThrowableKind.Fabricated, ThrowableKind.Real, ExplanationAt.CapturedStdout)
    },
    reportedName = identity,
    // No `hedgehog-sbt_native0.5_3` is published.
    platforms = _ => Set("jvm", "js"),
    scalaBinaryVersions = _ => Set("2.13", "3")
  )

  /** Kotlin, and driven by the JUnit Platform rather than by an sbt adapter.
    *
    * That combination is the reason it is here. bleep opens a `LauncherSession` and lets the platform find its engines through the `ServiceLoader`, so Kotest
    * exercises the path no Scala framework touches — and kotest 6 brings its own junit-platform line, which is exactly the alignment case the test runtime's
    * rule table exists to get right (see `MultiWorkspaceBspServer.testRuntimeRules`, and the `NoSuchMethodError: ReflectionUtils.returnsVoid` it was written
    * for).
    */
  val kotest: TestFrameworkFixture = TestFrameworkFixture(
    name = "kotest",
    deps = v => List(s"io.kotest:kotest-runner-junit5-jvm:$v"),
    // 6.x and 5.x sit on different junit-platform lines, which is the whole point of covering both.
    versions = List(model.Versions.Kotest, "5.9.1"),
    language = FixtureLanguage.Kotlin,
    relPath = "example/KotestFixture.kt",
    suiteFqn = "example.KotestFixture",
    source = """package example
               |
               |import io.kotest.core.spec.style.FunSpec
               |import io.kotest.matchers.shouldBe
               |
               |class KotestFixture : FunSpec({
               |  test("adds") { println("hello from the test"); (1 + 1) shouldBe 2 }
               |  test("measures") { "hello".length shouldBe 5 }
               |  test("fails on purpose") { 1 shouldBe 2 }
               |  test("throws on purpose") { throw RuntimeException("boom") }
               |  xtest("skipped on purpose") { (1 + 1) shouldBe 2 }
               |})
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose", "skipped on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    skippedTestName = Some("skipped on purpose"),
    failureReporting = (_, _) => FailureReporting.Full,
    reportedName = identity,
    // `kotest-runner-junit5-jvm` is the JVM artifact by name. Kotest does publish for JS and Native, under different artifact ids and a different runner; that
    // is a separate fixture rather than a platform row on this one.
    platforms = _ => JvmOnly,
    // A Kotlin fixture carries no Scala at all, so the project's Scala version never enters into whether it resolves.
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  /** TestNG ships no sbt test-interface implementation, so it only runs through Mill's bridge — which makes this fixture a test of the bridge as much as of
    * TestNG, and the only coverage of a framework bleep can discover but not run unaided.
    */
  val testng: TestFrameworkFixture = TestFrameworkFixture(
    name = "testng",
    // The bridge is spelled with a single colon and an explicit `_2.13` suffix, not `::`. This is a Java fixture with no `scala:` block at all, and `::` asks
    // bleep to append the project's Scala suffix — which fails outright with "You need to configure a scala version". The bridge is a fixed 2.13 artifact
    // whatever the consuming project is written in, so naming it in full is both what works and what is true.
    deps = v => List(s"org.testng:testng:$v", "com.lihaoyi:mill-contrib-testng_2.13:0.9.6"),
    versions = List("7.10.2", "7.5"),
    language = FixtureLanguage.Java,
    relPath = "example/TestNGFixture.java",
    suiteFqn = "example.TestNGFixture",
    source = """package example;
               |
               |import org.testng.SkipException;
               |import org.testng.annotations.Test;
               |import static org.testng.Assert.assertEquals;
               |
               |public class TestNGFixture {
               |  @Test
               |  public void adds() { System.out.println("hello from the test"); assertEquals(1 + 1, 2); }
               |
               |  @Test
               |  public void measures() { assertEquals("hello".length(), 5); }
               |
               |  @Test
               |  public void failsOnPurpose() { assertEquals(1, 2); }
               |
               |  @Test
               |  public void throwsOnPurpose() { throw new RuntimeException("boom"); }
               |
               |  @Test
               |  public void skippedOnPurpose() { throw new SkipException("skipped on purpose"); }
               |}
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("adds", "measures", "failsOnPurpose", "throwsOnPurpose", "skippedOnPurpose"),
    failingTestName = "failsOnPurpose",
    throwingTestName = "throwsOnPurpose",
    skippedTestName = Some("skippedOnPurpose"),
    failureReporting = (_, _) => FailureReporting.Full,
    // Every case arrives under the suite's own name, the same way minitest's does and for the same reason: Mill's bridge echoes back the selector it was handed
    // — bleep passes a `SuiteSelector`, exactly as sbt does — and never names the method. The per-method names exist only in TestNG's own output. The counts
    // stay exact, which is what this fixture actually asserts: four cases, two of them not passing.
    reportedName = _ => "example.TestNGFixture",
    platforms = _ => JvmOnly,
    // Unconstrained, because nothing here is compiled against Scala: the fixture is Java and the bridge is named at a fixed suffix.
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  /** `kotlin.test` — the standard-library test API, and what a Kotlin project that never heard of Kotest will use.
    *
    * Worth its own row rather than being treated as a JUnit alias. `kotlin.test.Test` is a distinct annotation that discovery scans for by name, and on the JVM
    * the API delegates to junit — so a suite written this way must reach the Launcher rather than the sbt path, where the fork once tried
    * `Class.forName("kotlin.test")` and died.
    */
  val kotlinTest: TestFrameworkFixture = TestFrameworkFixture(
    name = "kotlin.test",
    deps = v => List(s"org.jetbrains.kotlin:kotlin-test-junit5:$v"),
    versions = List(model.Versions.Kotlin24, model.Versions.Kotlin22),
    language = FixtureLanguage.Kotlin,
    relPath = "example/KotlinTestFixture.kt",
    suiteFqn = "example.KotlinTestFixture",
    source = """package example
               |
               |import kotlin.test.Ignore
               |import kotlin.test.Test
               |import kotlin.test.assertEquals
               |
               |class KotlinTestFixture {
               |  @Test fun adds() { println("hello from the test"); assertEquals(2, 1 + 1) }
               |  @Test fun measures() { assertEquals(5, "hello".length) }
               |  @Test fun failsOnPurpose() { assertEquals(2, 1) }
               |  @Test fun throwsOnPurpose() { throw RuntimeException("boom") }
               |  @Ignore @Test fun skippedOnPurpose() { assertEquals(1, 1) }
               |}
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("adds", "measures", "failsOnPurpose", "throwsOnPurpose", "skippedOnPurpose"),
    failingTestName = "failsOnPurpose",
    throwingTestName = "throwsOnPurpose",
    skippedTestName = Some("skippedOnPurpose"),
    failureReporting = (_, _) => FailureReporting.Full,
    // On the JVM kotlin.test delegates to junit and the Launcher reports `name()`. On Kotlin/JS and Kotlin/Native there is no junit at all: bleep runs the
    // linked artifact and reads back its own `##kotlin-test##` protocol, which names the bare method.
    reportedName = name => s"$name()",
    platforms = _ => Set("jvm", "kotlin-js", "kotlin-native"),
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  /** JUnit 3, which the docs claim and nothing exercised.
    *
    * Not a version of the junit4 fixture: a JUnit 3 suite carries no annotations at all, so it is invisible to annotation scanning and reaches discovery only
    * through the `junit.framework.TestCase` base class. Different code path, different way to be wrong. The artifact is junit 4's, which still ships the JUnit
    * 3 classes, and the vintage engine is what runs it.
    */
  val junit3: TestFrameworkFixture = TestFrameworkFixture(
    name = "junit3",
    deps = v => List(s"junit:junit:$v"),
    versions = List("4.13.2"),
    language = FixtureLanguage.Java,
    relPath = "example/Junit3Fixture.java",
    suiteFqn = "example.Junit3Fixture",
    source = """package example;
               |
               |import junit.framework.TestCase;
               |
               |public class Junit3Fixture extends TestCase {
               |  public void testAdds() { System.out.println("hello from the test"); assertEquals(2, 1 + 1); }
               |
               |  public void testMeasures() { assertEquals(5, "hello".length()); }
               |
               |  public void testFailsOnPurpose() { assertEquals(2, 1); }
               |
               |  public void testThrowsOnPurpose() { throw new RuntimeException("boom"); }
               |}
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("testAdds", "testMeasures", "testFailsOnPurpose", "testThrowsOnPurpose"),
    failingTestName = "testFailsOnPurpose",
    throwingTestName = "testThrowsOnPurpose",
    // JUnit 3 predates @Ignore; a method is a test iff it is named test*, so "skipped" cannot be expressed.
    skippedTestName = None,
    failureReporting = (_, _) => FailureReporting.Full,
    reportedName = identity,
    platforms = _ => JvmOnly,
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  /** jqwik, a JUnit Platform engine the docs claim.
    *
    * The interesting row in the whole Java set: jqwik marks its tests `@Property` and `@Example`, neither of which is `@Test` under any name. It is a real
    * `TestEngine`, so the Launcher runs it perfectly well once a suite reaches the Launcher — the question this fixture asks is whether bleep's own classpath
    * scan finds the class in the first place.
    */
  val jqwik: TestFrameworkFixture = TestFrameworkFixture(
    name = "jqwik",
    deps = v => List(s"net.jqwik:jqwik:$v"),
    versions = List("1.9.2"),
    language = FixtureLanguage.Java,
    relPath = "example/JqwikFixture.java",
    suiteFqn = "example.JqwikFixture",
    source = """package example;
               |
               |import net.jqwik.api.Disabled;
               |import net.jqwik.api.Example;
               |
               |public class JqwikFixture {
               |  @Example
               |  void adds() { System.out.println("hello from the test"); if (1 + 1 != 2) throw new AssertionError("adds"); }
               |
               |  @Example
               |  void measures() { if ("hello".length() != 5) throw new AssertionError("measures"); }
               |
               |  @Example
               |  void failsOnPurpose() { throw new AssertionError("failsOnPurpose"); }
               |
               |  @Example
               |  void throwsOnPurpose() { throw new RuntimeException("boom"); }
               |
               |  @Example
               |  @Disabled("skipped on purpose")
               |  void skippedOnPurpose() {}
               |}
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("adds", "measures", "failsOnPurpose", "throwsOnPurpose", "skippedOnPurpose"),
    failingTestName = "failsOnPurpose",
    throwingTestName = "throwsOnPurpose",
    skippedTestName = Some("skippedOnPurpose"),
    failureReporting = (_, _) => FailureReporting.Full,
    reportedName = identity,
    platforms = _ => JvmOnly,
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  /** Cucumber, entered the way the JUnit Platform expects: a `@Suite` aggregator that points the Launcher at a `.feature` resource.
    *
    * Structurally unlike every other fixture here, which is the point of including it. The class bleep discovers contains no tests at all — it is a pointer.
    * The scenarios live in a Gherkin resource, the steps live in a third file, and what comes back as test cases is named after the scenarios rather than after
    * any method. If bleep's scan can find the aggregator and the Launcher expands it, the whole shape works.
    */
  val cucumber: TestFrameworkFixture = TestFrameworkFixture(
    name = "cucumber",
    deps = v =>
      List(
        s"io.cucumber:cucumber-java:$v",
        s"io.cucumber:cucumber-junit-platform-engine:$v",
        "org.junit.platform:junit-platform-suite:1.11.4"
      ),
    versions = List("7.20.1"),
    language = FixtureLanguage.Java,
    relPath = "example/CucumberFixture.java",
    suiteFqn = "example.CucumberFixture",
    source = """package example;
               |
               |import org.junit.platform.suite.api.ConfigurationParameter;
               |import org.junit.platform.suite.api.IncludeEngines;
               |import org.junit.platform.suite.api.SelectClasspathResource;
               |import org.junit.platform.suite.api.Suite;
               |
               |@Suite
               |@IncludeEngines("cucumber")
               |@SelectClasspathResource("example")
               |@ConfigurationParameter(key = "cucumber.glue", value = "example")
               |public class CucumberFixture {}
               |""".stripMargin,
    extraFiles = List(
      "src/resources/example/fixture.feature" ->
        """Feature: fixture
          |
          |  Scenario: adds
          |    Given a calculator
          |    Then adding one and one gives two
          |
          |  Scenario: measures
          |    Given a calculator
          |    Then hello has five letters
          |
          |  Scenario: failsOnPurpose
          |    Given a calculator
          |    Then the assertion fails
          |
          |  Scenario: throwsOnPurpose
          |    Given a calculator
          |    Then the step throws
          |""".stripMargin,
      "src/resources/greenexample/green.feature" ->
        """Feature: green
          |
          |  Scenario: adds
          |    Given a calculator
          |    Then adding one and one gives two
          |
          |  Scenario: measures
          |    Given a calculator
          |    Then hello has five letters
          |""".stripMargin,
      "src/java/example/Steps.java" ->
        """package example;
          |
          |import io.cucumber.java.en.Given;
          |import io.cucumber.java.en.Then;
          |
          |public class Steps {
          |  @Given("a calculator")
          |  public void aCalculator() {}
          |
          |  @Then("adding one and one gives two")
          |  public void addsUp() { System.out.println("hello from the test"); if (1 + 1 != 2) throw new AssertionError("adds"); }
          |
          |  @Then("hello has five letters")
          |  public void measures() { if ("hello".length() != 5) throw new AssertionError("measures"); }
          |
          |  @Then("the assertion fails")
          |  public void failsOnPurpose() { throw new AssertionError("failsOnPurpose"); }
          |
          |  @Then("the step throws")
          |  public void throwsOnPurpose() { throw new RuntimeException("boom"); }
          |}
          |""".stripMargin
    ),
    testNames = List("adds", "measures", "failsOnPurpose", "throwsOnPurpose"),
    failingTestName = "failsOnPurpose",
    throwingTestName = "throwsOnPurpose",
    // Cucumber can tag a scenario @ignore, but only a tag *filter* passed to the runner skips it, which is a runner-configuration question rather than
    // a report-fidelity one. Left out deliberately.
    skippedTestName = None,
    failureReporting = (_, _) => FailureReporting.Full,
    reportedName = identity,
    platforms = _ => JvmOnly,
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  /** Every framework the matrix knows about. Each runs on the platforms and Scala versions it declares. */
  val spek: TestFrameworkFixture = TestFrameworkFixture(
    name = "spek",
    // The DSL plus its own junit-platform engine. bleep supplies the launcher, same as for junit5 and kotest.
    deps = v => List(s"org.spekframework.spek2:spek-dsl-jvm:$v", s"org.spekframework.spek2:spek-runner-junit5:$v", "org.jetbrains.kotlin:kotlin-test:2.4.10"),
    versions = List("2.0.19"),
    language = FixtureLanguage.Kotlin,
    relPath = "example/SpekFixture.kt",
    suiteFqn = "example.SpekFixture",
    source = """package example
               |
               |import org.spekframework.spek2.Spek
               |import org.spekframework.spek2.style.specification.describe
               |import kotlin.test.assertEquals
               |
               |object SpekFixture : Spek({
               |  describe("arithmetic") {
               |    it("adds") { println("hello from the test"); assertEquals(2, 1 + 1) }
               |    it("measures") { assertEquals(5, "hello".length) }
               |    it("fails on purpose") { assertEquals(2, 1) }
               |    it("throws on purpose") { throw RuntimeException("boom") }
               |    xit("skipped on purpose") { assertEquals(1, 1) }
               |  }
               |})
               |""".stripMargin,
    extraFiles = Nil,
    testNames = List("adds", "measures", "fails on purpose", "throws on purpose", "skipped on purpose"),
    failingTestName = "fails on purpose",
    throwingTestName = "throws on purpose",
    skippedTestName = Some("skipped on purpose"),
    failureReporting = (_, _) => FailureReporting.Full,
    reportedName = identity,
    platforms = _ => JvmOnly,
    scalaBinaryVersions = _ => AllScalaBinaryVersions
  )

  /** Printed by the first test of every fixture. */
  val StdoutMarker: String = "hello from the test"

  val all: List[TestFrameworkFixture] =
    List(
      munit,
      scalatest,
      utest,
      scalacheck,
      specs2,
      minitest,
      zioTest,
      weaver,
      hedgehog,
      junit5,
      junit4,
      junit3,
      kotest,
      kotlinTest,
      testng,
      jqwik,
      cucumber,
      spek
    )

  /** Fixtures that publish for this platform and Scala binary version, at the one version CI pins. */
  def pinnedFor(platformId: String, scalaBinaryVersion: Option[String]): List[(TestFrameworkFixture, String)] =
    all.filter(f => f.supports(platformId, scalaBinaryVersion, f.currentVersion)).map(f => (f, f.currentVersion))

  /** Every (fixture, version) pair that publishes for this platform and Scala binary version — the whole sweep, pinned version included.
    *
    * Deliberately not `pinnedFor`'s complement: a version that CI already covers on one platform is still worth covering on another, and excluding it would
    * make the sweep's results depend on which combinations CI happens to run today.
    */
  def sweepFor(platformId: String, scalaBinaryVersion: Option[String]): List[(TestFrameworkFixture, String)] =
    all.flatMap(f => f.versions.filter(v => f.supports(platformId, scalaBinaryVersion, v)).map(v => (f, v)))
}
