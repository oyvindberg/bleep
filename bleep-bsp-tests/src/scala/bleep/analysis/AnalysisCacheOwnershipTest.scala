package bleep.analysis

import bleep.model
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import xsbti.compile.CompileAnalysis
import xsbti.compile.analysis.{ReadCompilations, ReadSourceInfos, ReadStamps}

import java.nio.file.{Files, Path}

/** Analyses belong to a workspace, and the consequences of that: they are partitioned by it and dropped with it.
  *
  * The point of the ownership is not tidiness. An unowned global pool could neither say which build was holding the heap nor free it when that build was
  * evicted — so a `BuildCache` eviction freed the resolved build (hundreds of MB) and left its analyses (gigabytes) behind.
  */
class AnalysisCacheOwnershipTest extends AnyFunSuite with Matchers {

  /** The cache stores analyses and reports their file size; it never looks inside one. */
  private object StubAnalysis extends CompileAnalysis {
    def readStamps(): ReadStamps = ???
    def readSourceInfos(): ReadSourceInfos = ???
    def readCompilations(): ReadCompilations = ???
  }

  private def key(name: String): model.WorkspaceKey =
    model.WorkspaceKey(Path.of(s"/tmp/$name"), model.BuildVariant.Normal)

  /** A real file, because `put` sizes the entry from disk. */
  private def analysisFile(bytes: Int): Path = {
    val f = Files.createTempFile("analysis", ".zip")
    Files.write(f, Array.fill(bytes)(0: Byte))
    f.toFile.deleteOnExit()
    f
  }

  private def cache(): AnalysisCache = new AnalysisCache

  test("analyses are partitioned by workspace: one workspace cannot see another's") {
    val c = cache()
    val f = analysisFile(64)
    val mtime = Files.getLastModifiedTime(f).toMillis
    c.put(key("alpha"), f, mtime, StubAnalysis): Unit

    c.get(key("alpha"), f, mtime) shouldBe defined
    // Same analysis FILE, different workspace. Nothing is shared across workspaces on disk, and
    // nothing is shared here either — otherwise one workspace's entries would be charged to another.
    c.get(key("beta"), f, mtime) shouldBe empty
  }

  test("a changed file on disk invalidates the entry rather than serving a stale analysis") {
    val c = cache()
    val f = analysisFile(64)
    val mtime = Files.getLastModifiedTime(f).toMillis
    c.put(key("alpha"), f, mtime, StubAnalysis): Unit

    c.get(key("alpha"), f, mtime) shouldBe defined
    c.get(key("alpha"), f, mtime + 1) shouldBe empty // e.g. after `remote-cache pull`
  }

  test("evicting a workspace frees only its own analyses, and reports what it freed") {
    val c = cache()
    val a1 = analysisFile(100)
    val a2 = analysisFile(200)
    val b1 = analysisFile(400)
    c.put(key("alpha"), a1, Files.getLastModifiedTime(a1).toMillis, StubAnalysis): Unit
    c.put(key("alpha"), a2, Files.getLastModifiedTime(a2).toMillis, StubAnalysis): Unit
    c.put(key("beta"), b1, Files.getLastModifiedTime(b1).toMillis, StubAnalysis): Unit

    val freed = c.evictWorkspace(key("alpha"))
    freed.entries shouldBe 2
    freed.fileBytes shouldBe 300L

    c.get(key("alpha"), a1, Files.getLastModifiedTime(a1).toMillis) shouldBe empty
    c.get(key("beta"), b1, Files.getLastModifiedTime(b1).toMillis) shouldBe defined
  }

  test("evicting a workspace that holds nothing is a no-op, not an error") {
    val c = cache()
    c.evictWorkspace(key("never-seen")) shouldBe AnalysisCache.Freed(0, 0L)
  }

  test("stats total across workspaces and are ordered by what they hold") {
    val c = cache()
    val small = analysisFile(10)
    val large = analysisFile(900)
    c.put(key("small-ws"), small, Files.getLastModifiedTime(small).toMillis, StubAnalysis): Unit
    c.put(key("large-ws"), large, Files.getLastModifiedTime(large).toMillis, StubAnalysis): Unit

    val stats = c.stats
    stats.entries shouldBe 2
    stats.fileBytes shouldBe 910L
    // Biggest first: the whole point of per-workspace stats is answering "who is holding the heap".
    stats.perWorkspace.head.key shouldBe key("large-ws")
  }
}
