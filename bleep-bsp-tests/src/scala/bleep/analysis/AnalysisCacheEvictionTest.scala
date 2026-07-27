package bleep.analysis

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Path, Paths}

/** The policy bounding the Zinc analysis cache — the largest single retainer in the compile server's heap.
  *
  * Measured on a live daemon at 7.2GB live set: ~4.5GB of it was `xsbti.api.*` held by this cache, and a forced full GC reclaimed 68MB. Soft references did not
  * bound it, so these rules do.
  */
class AnalysisCacheEvictionTest extends AnyFunSuite with Matchers {

  private val MB = 1024L * 1024
  private def p(s: String): Path = Paths.get(s)

  private def evict(entries: Vector[(String, Long, Long)], nowMs: Long, maxIdleMs: Long, budgetMb: Long): Vector[String] =
    AnalysisCache
      .selectEvictions(entries.map { case (k, used, mb) => (p(k), used, mb * MB) }, nowMs, maxIdleMs, budgetMb * MB)
      .map(_.toString)

  test("a cache within budget and recently used keeps everything") {
    val e = Vector(("a", 9_000L, 40L), ("b", 9_500L, 40L))
    evict(e, nowMs = 10_000L, maxIdleMs = 120_000L, budgetMb = 256L) shouldBe empty
  }

  test("entries idle past the cutoff are dropped regardless of budget") {
    // The cache exists to share analyses within one build. Across builds minutes apart, re-reading
    // costs a fraction of a second and saves gigabytes.
    val e = Vector(("stale", 1_000L, 10L), ("fresh", 199_000L, 10L))
    evict(e, nowMs = 200_000L, maxIdleMs = 120_000L, budgetMb = 256L) shouldBe Vector("stale")
  }

  test("over budget, least recently used go first and only until it fits") {
    // 4 x 100MB against a 250MB budget: the two oldest go, the newest two stay.
    val e = Vector(("oldest", 1_000L, 100L), ("older", 2_000L, 100L), ("newer", 3_000L, 100L), ("newest", 4_000L, 100L))
    evict(e, nowMs = 5_000L, maxIdleMs = 120_000L, budgetMb = 250L) shouldBe Vector("oldest", "older")
  }

  test("expired entries count towards relieving the budget, so fresh ones survive") {
    // Dropping the two expired entries already brings the total under budget; nothing fresh is touched.
    val e = Vector(("expired1", 1_000L, 100L), ("expired2", 1_500L, 100L), ("fresh", 999_000L, 100L))
    evict(e, nowMs = 1_000_000L, maxIdleMs = 120_000L, budgetMb = 150L) shouldBe Vector("expired1", "expired2")
  }

  test("a single entry larger than the whole budget is still evicted rather than pinning the cache") {
    // The 20MB analyses in a real build are the ones worth caching, but a budget that cannot be
    // honoured is not a budget. Re-reading is always available.
    val e = Vector(("huge", 1_000L, 400L))
    evict(e, nowMs = 2_000L, maxIdleMs = 120_000L, budgetMb = 256L) shouldBe Vector("huge")
  }

  test("an empty cache is a no-op") {
    evict(Vector.empty, nowMs = 1_000L, maxIdleMs = 120_000L, budgetMb = 256L) shouldBe empty
  }

  test("the shipped defaults hold roughly one busy workspace's analyses") {
    // Calibration, recorded so a future change to either constant is a deliberate one: a real
    // workspace measured 166 analysis files totalling 113MB on disk, which inflated to ~4.5GB of
    // live objects — so the 256MB budget is worth something like 1.5GB of heap.
    AnalysisCache.DefaultBudgetBytesPerWorkspace shouldBe 256L * MB
    AnalysisCache.DefaultMaxIdleMs shouldBe 120000L
  }
}
