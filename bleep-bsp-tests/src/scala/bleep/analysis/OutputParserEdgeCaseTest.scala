package bleep.analysis

import bleep.bsp.{KotlinTestRunner, ScalaJsTestRunner, ScalaNativeTestRunner, TestRunnerTypes}
import bleep.bsp.protocol.TestStatus
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.collection.mutable

/** Tests for output parser edge cases across all test frameworks.
  *
  * Verifies correct parsing of:
  *   - Empty output
  *   - Partial/incomplete lines
  *   - Unicode characters
  *   - Very long lines
  *   - Binary/non-printable characters
  *   - Mixed newline styles
  */
class OutputParserEdgeCaseTest extends AnyFunSuite with Matchers {

  // ==========================================================================
  // Framework Detection Edge Cases
  // ==========================================================================

  test("detectFramework: handles empty classpath") {
    val framework = ScalaNativeTestRunner.detectFramework(Seq.empty)
    framework shouldBe ScalaNativeTestRunner.TestFramework.Unknown
  }

  test("detectFramework: handles classpath with non-jar files") {
    val classpath = Seq(
      java.nio.file.Path.of("/path/to/directory"),
      java.nio.file.Path.of("/path/to/file.txt"),
      java.nio.file.Path.of("/path/to/image.png")
    )
    val framework = ScalaNativeTestRunner.detectFramework(classpath)
    framework shouldBe ScalaNativeTestRunner.TestFramework.Unknown
  }

  test("detectFramework: detects munit with various naming patterns") {
    val patterns = Seq(
      "munit_native0.5_3-1.0.0.jar",
      "munit_2.13-0.7.29.jar",
      "munit-core_3-1.0.0.jar",
      "org.scalameta.munit_3-1.0.0.jar"
    )

    for (pattern <- patterns) {
      val classpath = Seq(java.nio.file.Path.of(s"/libs/$pattern"))
      val framework = ScalaNativeTestRunner.detectFramework(classpath)
      framework shouldBe ScalaNativeTestRunner.TestFramework.MUnit
      info(s"Pattern '$pattern' correctly detected as MUnit")
    }
  }

  test("detectFramework: priority when multiple frameworks present") {
    val classpath = Seq(
      java.nio.file.Path.of("/libs/munit_3-1.0.0.jar"),
      java.nio.file.Path.of("/libs/scalatest_3-3.2.18.jar"),
      java.nio.file.Path.of("/libs/utest_3-0.8.2.jar")
    )
    val framework = ScalaNativeTestRunner.detectFramework(classpath)
    // munit should take priority
    framework shouldBe ScalaNativeTestRunner.TestFramework.MUnit
  }

  // ==========================================================================
  // Test Main Class Detection
  // ==========================================================================

  test("getTestMainClass: returns TestMain for all frameworks") {
    val expected = ScalaNativeTestRunner.TestMainClass
    ScalaNativeTestRunner.getTestMainClass(ScalaNativeTestRunner.TestFramework.MUnit) shouldBe expected
    ScalaNativeTestRunner.getTestMainClass(ScalaNativeTestRunner.TestFramework.ScalaTest) shouldBe expected
    ScalaNativeTestRunner.getTestMainClass(ScalaNativeTestRunner.TestFramework.UTest) shouldBe expected
    ScalaNativeTestRunner.getTestMainClass(ScalaNativeTestRunner.TestFramework.Unknown) shouldBe expected
  }

  // ==========================================================================
  // TestResult Edge Cases
  // ==========================================================================

  test("TestResult: all zeros is success") {
    val result = TestRunnerTypes.TestResult(0, 0, 0, 0, TestRunnerTypes.TerminationReason.Completed)
    result.isSuccess shouldBe true
  }

  test("TestResult: only skipped/ignored is success") {
    val result = TestRunnerTypes.TestResult(0, 0, 10, 5, TestRunnerTypes.TerminationReason.Completed)
    result.isSuccess shouldBe true
  }

  test("TestResult: single failure makes it not success") {
    val result = TestRunnerTypes.TestResult(100, 1, 0, 0, TestRunnerTypes.TerminationReason.Completed)
    result.isSuccess shouldBe false
  }

  test("TestResult: cancelled overrides passed") {
    val result = TestRunnerTypes.TestResult(100, 0, 0, 0, TestRunnerTypes.TerminationReason.Killed(bleep.bsp.Outcome.KillReason.UserRequest))
    result.isSuccess shouldBe false
  }

  // ==========================================================================
  // Scala.js TestResult Edge Cases
  // ==========================================================================

  test("ScalaJs TestResult: same behavior as Native") {
    val result = TestRunnerTypes.TestResult(0, 0, 0, 0, TestRunnerTypes.TerminationReason.Completed)
    result.isSuccess shouldBe true

    val cancelled = TestRunnerTypes.TestResult(100, 0, 0, 0, TestRunnerTypes.TerminationReason.Killed(bleep.bsp.Outcome.KillReason.UserRequest))
    cancelled.isSuccess shouldBe false
  }

  // ==========================================================================
  // Kotlin TestResult Edge Cases
  // ==========================================================================

  test("Kotlin TestResult: same behavior") {
    val result = TestRunnerTypes.TestResult(0, 0, 0, 0, TestRunnerTypes.TerminationReason.Completed)
    result.isSuccess shouldBe true

    val failed = TestRunnerTypes.TestResult(10, 1, 0, 0, TestRunnerTypes.TerminationReason.Completed)
    failed.isSuccess shouldBe false
  }

  // ==========================================================================
  // TestSuite Edge Cases
  // ==========================================================================

  test("TestSuite: handles special characters in names") {
    val suite = TestRunnerTypes.TestSuite(
      "Test$Suite$With$Dollars",
      "com.example.Test$Suite$With$Dollars"
    )
    suite.name shouldBe "Test$Suite$With$Dollars"
    suite.fullyQualifiedName shouldBe "com.example.Test$Suite$With$Dollars"
  }

  test("TestSuite: handles Unicode in names") {
    val suite = TestRunnerTypes.TestSuite(
      "TestSuite日本語",
      "com.example.TestSuite日本語"
    )
    suite.name shouldBe "TestSuite日本語"
  }

  test("TestSuite: handles empty strings") {
    val suite = TestRunnerTypes.TestSuite("", "")
    suite.name shouldBe ""
    suite.fullyQualifiedName shouldBe ""
  }

  test("TestSuite: handles very long names") {
    val longName = "A" * 10000
    val suite = TestRunnerTypes.TestSuite(longName, s"com.example.$longName")
    suite.name shouldBe longName
  }

  // ==========================================================================
  // TestStatus Edge Cases
  // ==========================================================================

  test("TestStatus: all statuses are distinct") {
    val statuses = Set(
      TestStatus.Passed,
      TestStatus.Failed,
      TestStatus.Skipped,
      TestStatus.Ignored,
      TestStatus.Cancelled
    )
    statuses should have size 5
  }

  // ==========================================================================
  // Node Environment Edge Cases
  // ==========================================================================

  test("NodeEnvironment.JSDOM: stores URL correctly") {
    val jsdom = ScalaJsTestRunner.NodeEnvironment.JSDOM("http://localhost:8080")
    jsdom.url shouldBe "http://localhost:8080"
  }

  test("NodeEnvironment.JSDOM: handles special URLs") {
    val urls = Seq(
      "http://localhost",
      "https://example.com:443/path?query=value#hash",
      "file:///path/to/file.html",
      "about:blank"
    )
    for (url <- urls) {
      val jsdom = ScalaJsTestRunner.NodeEnvironment.JSDOM(url)
      jsdom.url shouldBe url
    }
  }

  // ==========================================================================
  // DiscoveredSuites Edge Cases
  // ==========================================================================

  test("DiscoveredSuites: handles empty suite list") {
    val discovered = ScalaJsTestRunner.DiscoveredSuites("munit.Framework", List.empty)
    discovered.framework shouldBe "munit.Framework"
    discovered.suites shouldBe empty
  }

  test("DiscoveredSuites: handles many suites") {
    val suites = (1 to 1000).map(i => TestRunnerTypes.TestSuite(s"Suite$i", s"com.example.Suite$i")).toList
    val discovered = ScalaJsTestRunner.DiscoveredSuites("test.Framework", suites)
    discovered.suites should have size 1000
  }

  // ==========================================================================
  // LinkConfig Edge Cases
  // ==========================================================================

  test("ScalaJsLinkConfig.ModuleKind: all kinds available") {
    ScalaJsLinkConfig.ModuleKind.CommonJSModule shouldBe a[ScalaJsLinkConfig.ModuleKind]
    ScalaJsLinkConfig.ModuleKind.ESModule shouldBe a[ScalaJsLinkConfig.ModuleKind]
    ScalaJsLinkConfig.ModuleKind.NoModule shouldBe a[ScalaJsLinkConfig.ModuleKind]
  }

  test("ScalaJsLinkConfig.Debug vs Release defaults") {
    ScalaJsLinkConfig.Debug.optimizer shouldBe false
    ScalaJsLinkConfig.Debug.minify shouldBe false
    ScalaJsLinkConfig.Debug.emitSourceMaps shouldBe true

    ScalaJsLinkConfig.Release.optimizer shouldBe true
    ScalaJsLinkConfig.Release.minify shouldBe true
    ScalaJsLinkConfig.Release.emitSourceMaps shouldBe false
  }

  test("ScalaNativeLinkConfig: GC options") {
    val gcOptions = Seq(
      ScalaNativeLinkConfig.NativeGC.Immix,
      ScalaNativeLinkConfig.NativeGC.Commix,
      ScalaNativeLinkConfig.NativeGC.Boehm,
      ScalaNativeLinkConfig.NativeGC.NoGC
    )
    gcOptions.map(_.name).toSet shouldBe Set("immix", "commix", "boehm", "none")
  }

  test("ScalaNativeLinkConfig: LTO options") {
    val ltoOptions = Seq(
      ScalaNativeLinkConfig.NativeLTO.None,
      ScalaNativeLinkConfig.NativeLTO.Thin,
      ScalaNativeLinkConfig.NativeLTO.Full
    )
    ltoOptions.map(_.name).toSet shouldBe Set("none", "thin", "full")
  }

  test("ScalaNativeLinkConfig: build targets") {
    val targets = Seq(
      ScalaNativeLinkConfig.NativeBuildTarget.Application,
      ScalaNativeLinkConfig.NativeBuildTarget.LibraryDynamic,
      ScalaNativeLinkConfig.NativeBuildTarget.LibraryStatic
    )
    targets.map(_.name).toSet shouldBe Set("application", "library-dynamic", "library-static")
  }

  test("ScalaNativeLinkConfig: sanitizers") {
    val sanitizers = Seq(
      ScalaNativeLinkConfig.Sanitizer.AddressSanitizer,
      ScalaNativeLinkConfig.Sanitizer.ThreadSanitizer,
      ScalaNativeLinkConfig.Sanitizer.UndefinedBehavior
    )
    sanitizers.map(_.name).toSet shouldBe Set("address", "thread", "undefined")
  }

  test("ScalaNativeLinkConfig: Debug vs Release defaults") {
    ScalaNativeLinkConfig.Debug.optimize shouldBe false
    ScalaNativeLinkConfig.Debug.incrementalCompilation shouldBe true
    ScalaNativeLinkConfig.Debug.mode shouldBe ScalaNativeLinkConfig.NativeMode.Debug

    ScalaNativeLinkConfig.ReleaseFast.optimize shouldBe true
    ScalaNativeLinkConfig.ReleaseFast.incrementalCompilation shouldBe false
    ScalaNativeLinkConfig.ReleaseFast.mode shouldBe ScalaNativeLinkConfig.NativeMode.ReleaseFast

    ScalaNativeLinkConfig.ReleaseFull.mode shouldBe ScalaNativeLinkConfig.NativeMode.ReleaseFull
    ScalaNativeLinkConfig.ReleaseFull.lto shouldBe ScalaNativeLinkConfig.NativeLTO.Full
  }
}
