package bleep.testing

import org.scalatest.funsuite.AnyFunSuite

class TestTagFilterTest extends AnyFunSuite {

  test("glob: exact FQDN matches itself") {
    val r = TestTagFilter.compileGlob("bleep.FooTest")
    assert(r.matches("bleep.FooTest"))
    assert(!r.matches("bleep.FooTests"))
    assert(!r.matches("bleep.foo.FooTest"))
  }

  test("glob: single * stays within a segment") {
    val r = TestTagFilter.compileGlob("bleep.foo.*Test")
    assert(r.matches("bleep.foo.FooTest"))
    assert(r.matches("bleep.foo.Test"))
    assert(!r.matches("bleep.foo.bar.Test")) // dot blocks single-*
    assert(!r.matches("bleep.bar.FooTest"))
  }

  test("glob: ** spans dots") {
    val r = TestTagFilter.compileGlob("**IT")
    assert(r.matches("bleep.FooIT"))
    assert(r.matches("bleep.foo.bar.HeavyIT"))
    assert(r.matches("IT"))
    assert(!r.matches("bleep.FooITX"))
  }

  test("glob: ** in middle") {
    val r = TestTagFilter.compileGlob("bleep.**.HeavyTest")
    assert(r.matches("bleep.foo.HeavyTest"))
    assert(r.matches("bleep.foo.bar.baz.HeavyTest"))
    assert(!r.matches("bleep.HeavyTest")) // ** requires at least the dots around it to match (between bleep. and .HeavyTest needs content)
    assert(!r.matches("other.foo.HeavyTest"))
  }

  test("glob: regex metacharacters in plain segments are escaped") {
    val r = TestTagFilter.compileGlob("bleep.Foo$Bar")
    assert(r.matches("bleep.Foo$Bar"))
    assert(!r.matches("bleep.FooXBar"))
  }

  test("tagsFor: union across patterns") {
    val manifest = Map(
      "slow" -> Set("**IT"),
      "flaky" -> Set("bleep.FooIT")
    )
    assert(TestTagFilter.tagsFor("bleep.FooIT", manifest) == Set("slow", "flaky"))
    assert(TestTagFilter.tagsFor("bleep.BarIT", manifest) == Set("slow"))
    assert(TestTagFilter.tagsFor("bleep.JustATest", manifest) == Set.empty)
  }

  test("filter: empty includes + empty excludes = no-op") {
    val suites = List("a.A", "a.B", "a.C")
    val (kept, dropped) = TestTagFilter.filter(suites, Map.empty, Set.empty, Set.empty)
    assert(kept == suites && dropped.isEmpty)
  }

  test("filter: include narrows to tagged set; untagged dropped") {
    val suites = List("bleep.A", "bleep.BIT", "bleep.CIT", "bleep.DTest")
    val manifest = Map("slow" -> Set("**IT"))
    val (kept, _) = TestTagFilter.filter(suites, manifest, includeTags = Set("slow"), excludeTags = Set.empty)
    assert(kept == List("bleep.BIT", "bleep.CIT"))
  }

  test("filter: multiple includes = OR (union)") {
    val suites = List("bleep.AIT", "bleep.BSmoke", "bleep.COther")
    val manifest = Map(
      "slow" -> Set("**IT"),
      "smoke" -> Set("**Smoke")
    )
    val (kept, _) = TestTagFilter.filter(suites, manifest, includeTags = Set("slow", "smoke"), excludeTags = Set.empty)
    assert(kept == List("bleep.AIT", "bleep.BSmoke"))
  }

  test("filter: exclude removes from include set") {
    val suites = List("bleep.AIT", "bleep.BIT")
    val manifest = Map(
      "slow" -> Set("**IT"),
      "flaky" -> Set("bleep.BIT")
    )
    val (kept, dropped) = TestTagFilter.filter(suites, manifest, includeTags = Set("slow"), excludeTags = Set("flaky"))
    assert(kept == List("bleep.AIT"))
    assert(dropped == List("bleep.BIT"))
  }

  test("filter: exclude alone keeps untagged tests") {
    val suites = List("bleep.A", "bleep.BIT", "bleep.CTest")
    val manifest = Map("slow" -> Set("**IT"))
    val (kept, dropped) = TestTagFilter.filter(suites, manifest, includeTags = Set.empty, excludeTags = Set("slow"))
    assert(kept == List("bleep.A", "bleep.CTest"))
    assert(dropped == List("bleep.BIT"))
  }

  test("projectsRelevantForIncludes: narrows to projects declaring at least one of the included tags") {
    val tagKeys = Map(
      "bleep-tests" -> Set("slow", "flaky"),
      "bleep-bsp-tests" -> Set("smoke"),
      "bleep-pure" -> Set.empty[String] // no tags declared
    )
    val candidates = tagKeys.keySet
    val kept = TestTagFilter.projectsRelevantForIncludes[String](candidates, tagKeys.apply, Set("slow"))
    assert(kept == Set("bleep-tests"))

    // Union over multiple includes
    val keptMulti = TestTagFilter.projectsRelevantForIncludes[String](candidates, tagKeys.apply, Set("slow", "smoke"))
    assert(keptMulti == Set("bleep-tests", "bleep-bsp-tests"))

    // Empty includes = no narrowing
    val all = TestTagFilter.projectsRelevantForIncludes[String](candidates, tagKeys.apply, Set.empty)
    assert(all == candidates)
  }

  test("staleManifestEntries: warns about patterns matching nothing") {
    val manifest = Map(
      "slow" -> Set("**IT", "bleep.NoLongerExistsIT"),
      "neverused" -> Set("bleep.GhostTest")
    )
    val discovered = Set("bleep.RealIT", "bleep.OtherTest")
    val warnings = TestTagFilter.staleManifestEntries(manifest, discovered)
    // Two stale: NoLongerExistsIT, neverused/GhostTest. **IT does match RealIT so it's fine.
    assert(warnings.size == 2, warnings)
    assert(warnings.exists(_.contains("NoLongerExistsIT")))
    assert(warnings.exists(_.contains("GhostTest")))
  }
}
