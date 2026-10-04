package bleep.bsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

/** The eviction policy that bounds how much resolved-build state one daemon retains.
  *
  * Written against the pure decision function rather than the cache, because what is worth pinning down is the policy — LRU order, the two exemptions, and what
  * happens when they conflict with the bound — and exercising it through the cache would mean resolving real builds to observe it.
  */
class BuildCacheEvictionTest extends AnyFunSuite with Matchers {

  private def select(present: Vector[(String, Long)], keep: String, bound: Int, busy: Set[String] = Set.empty): Vector[String] =
    BuildCache.selectEvictions(present, keep, bound, busy.contains).map(_._1)

  test("under the bound nothing is evicted") {
    select(Vector("a" -> 1L, "b" -> 2L), keep = "b", bound = 4) shouldBe empty
  }

  test("at the bound nothing is evicted — the bound is a ceiling, not a target") {
    select(Vector("a" -> 1L, "b" -> 2L, "c" -> 3L), keep = "c", bound = 3) shouldBe empty
  }

  test("over the bound the least recently used go first, and only as many as the overage") {
    // Five entries, bound of three: the two oldest idle ones go, the rest stay.
    val present = Vector("oldest" -> 10L, "older" -> 20L, "middle" -> 30L, "recent" -> 40L, "newest" -> 50L)
    select(present, keep = "newest", bound = 3) shouldBe Vector("oldest", "older")
  }

  test("the entry just loaded is never evicted, even when it is the least recently used of them all") {
    // A pathological clock reading for the new entry must not make it the victim: it was loaded
    // because someone is about to use it, and evicting it would mean reloading it immediately.
    val present = Vector("fresh" -> 0L, "a" -> 10L, "b" -> 20L, "c" -> 30L)
    select(present, keep = "fresh", bound = 2) shouldBe Vector("a", "b")
  }

  test("a workspace with operations in flight is never evicted, even when it is the oldest") {
    val present = Vector("busy" -> 1L, "idle1" -> 2L, "idle2" -> 3L, "keep" -> 4L)
    select(present, keep = "keep", bound = 2, busy = Set("busy")) shouldBe Vector("idle1", "idle2")
  }

  test("when too many entries are busy the cache exceeds its bound rather than evicting live work") {
    // Four entries, bound of one, and everything but the kept entry is compiling. Nothing is
    // evictable, so the bound yields. A daemon can serve more workspaces at once than it caches.
    val present = Vector("busy1" -> 1L, "busy2" -> 2L, "busy3" -> 3L, "keep" -> 4L)
    select(present, keep = "keep", bound = 1, busy = Set("busy1", "busy2", "busy3")) shouldBe empty
  }

  test("busy entries still count towards the bound, so idle ones are evicted to make room for them") {
    // Three busy + two idle, bound of three. The overage is two, and both idle entries pay it.
    val present = Vector("busy1" -> 1L, "busy2" -> 2L, "busy3" -> 3L, "idle1" -> 4L, "keep" -> 5L)
    select(present, keep = "keep", bound = 3, busy = Set("busy1", "busy2", "busy3")) shouldBe Vector("idle1")
  }

  // ── memory needed elsewhere ────────────────────────────────────

  test("a shed drops every idle entry and keeps every busy one, whatever the bound") {
    BuildCache.selectIdle[String](Vector("idle1", "busy", "idle2"), isBusy = _ == "busy") shouldBe Vector("idle1", "idle2")
    BuildCache.selectIdle[String](Vector.empty, isBusy = _ => false) shouldBe empty
  }

  // ── workspaces that no longer exist ────────────────────────────

  test("a cached build whose workspace directory is gone is evicted, whatever the bound") {
    BuildCache.selectMissing[String](Vector("deleted", "alive"), exists = _ == "alive", isBusy = _ => false) shouldBe Vector("deleted")
  }

  test("a deleted workspace with work in flight is left for a later sweep") {
    BuildCache.selectMissing[String](Vector("deleted-busy", "deleted-idle"), exists = _ => false, isBusy = _ == "deleted-busy") shouldBe Vector("deleted-idle")
  }

  /** The whole path, with a real resolved build: a worktree is loaded, its directory is deleted, and the sweep the daemon runs drops it while leaving a live
    * one.
    */
  test("a real cached build is evicted once its workspace directory is deleted, and a live one is kept") {
    val root = java.nio.file.Files.createTempDirectory("bleep-build-cache-missing")
    val alive = java.nio.file.Files.createDirectories(root.resolve("alive"))
    val deleted = java.nio.file.Files.createDirectories(root.resolve("deleted"))
    val classpath = bleep.analysis.CompilerResolver.resolveScalaLibrary("3.7.4").toList
    val (analysisCache, buildCache, scheduling) = bleep.analysis.BspTestHarness.freshCaches()

    List(alive, deleted).foreach { workspace =>
      java.nio.file.Files.createDirectories(workspace.resolve("src"))
      val config = bleep.analysis.BspTestHarness.ProjectConfig.scala("p", Set(workspace.resolve("src")), "3.7.4", classpath, isTest = false)
      bleep.analysis.BspTestHarness.withProjectAndCaches(workspace, config, analysisCache, buildCache, scheduling) { client =>
        client.initialize(): Unit
        client.buildTargets(): Unit
      }
    }
    buildCache.cachedWorkspaces.toSet shouldBe Set(alive.toString, deleted.toString)

    bleep.internal.FileUtils.deleteDirectory(deleted)
    buildCache.evictMissing(ryddig.Loggers.storing())

    buildCache.cachedWorkspaces shouldBe List(alive.toString)
  }
}
