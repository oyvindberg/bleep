package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

class ZincSourceLookupTest extends AnyFunSuite with Matchers {

  test("topLevelClassName reduces a frame's class to what zinc actually records") {
    // zinc stores top-level names only, so these all have to collapse or the lookup silently misses
    ZincSourceLookup.topLevelClassName("bleep.GithubActionsTest$$anon$1") shouldBe "bleep.GithubActionsTest"
    ZincSourceLookup.topLevelClassName("bleep.Outer$Inner") shouldBe "bleep.Outer"
    ZincSourceLookup.topLevelClassName("bleep.Foo$") shouldBe "bleep.Foo" // scala object
    ZincSourceLookup.topLevelClassName("bleep.Plain") shouldBe "bleep.Plain"
  }

  /** Reads bleep's own analysis rather than a fixture: the two things worth guarding — that the marker is what we think and that anonymous classes need
    * truncating — are properties of what zinc really writes, and a hand-built Relations would just re-encode my assumptions.
    */
  test("resolves a class to its build-relative source, straight out of a real analysis") {
    val analysisFile = Path.of(".bleep/projects/bleep-tests/builds/normal/.zinc/analysis.zip").toAbsolutePath
    assume(Files.exists(analysisFile), s"no analysis at $analysisFile — run `bleep compile bleep-tests` first")

    val store = sbt.internal.inc.consistent.ConsistentFileAnalysisStore.binary(
      analysisFile.toFile,
      ZincBridge.analysisMappers(analysisFile),
      reproducible = true,
      parallelism = 4
    )
    val analysis = store.get().get().getAnalysis

    // build-relative, no machine-specific prefix, and no ${BASE} left in it
    ZincSourceLookup.relativeSourceFor(analysis, "bleep.GithubActionsTest") shouldBe
      Some("bleep-tests/src/scala/bleep/GithubActionsTest.scala")

    // the frame a ScalaTest assertion failure actually produces
    ZincSourceLookup.relativeSourceFor(analysis, "bleep.GithubActionsTest$$anon$1") shouldBe
      Some("bleep-tests/src/scala/bleep/GithubActionsTest.scala")

    // a class zinc never compiled resolves to nothing rather than to a plausible-looking wrong file
    ZincSourceLookup.relativeSourceFor(analysis, "com.example.NotInThisProject") shouldBe None
  }
}
