package bleep.analysis

import xsbti.compile.CompileAnalysis

import java.nio.file.{Files, Path}

/** Resolves a JVM class name to the source file that defines it, out of zinc's analysis.
  *
  * Test failures arrive as a stack frame: a class name, a bare file name (`MyTest.scala`) and a line. The bare name is all the JVM has — it comes from the
  * class file's `SourceFile` attribute, which by specification holds a file name and never a path — so recovering a usable path means asking something that
  * recorded where the source actually was. Zinc's `Relations` did exactly that at compile time.
  *
  * The alternative was reconstructing a path from the class's package plus the file name and searching the project's source roots. That guesses, and it guesses
  * wrong for any file whose name differs from its class, for several roots holding the same relative path, and for generated sources.
  *
  * Only Scala and Java-through-zinc are covered. Kotlin compiles through kotlinc, which produces no zinc analysis, so those failures keep the bare file name.
  */
object ZincSourceLookup {

  /** Zinc records only top-level class names, so a frame inside a nested, inner or anonymous class has to be reduced to its enclosing top-level class before it
    * will match. Verified against a real analysis: `definesClass("bleep.GithubActionsTest$$anon$1")` is empty while `definesClass("bleep.GithubActionsTest")`
    * resolves — and anonymous classes are where most assertion failures are raised, so skipping this step would leave nearly every ScalaTest failure
    * unresolved.
    *
    * Truncating at the first `$` also handles a Scala object's binary name (`bleep.Foo$` -> `bleep.Foo`). A class with a literal `$` in its own name resolves
    * to nothing rather than to the wrong file.
    */
  def topLevelClassName(binaryClassName: String): String =
    binaryClassName.indexOf('$') match {
      case -1  => binaryClassName
      case idx => binaryClassName.substring(0, idx)
    }

  /** The defining source as a build-relative path, or None when analysis does not know the class.
    *
    * Build-relative rather than absolute because that is how zinc stores it: bleep writes analysis through portable mappers, so source ids read back as
    * `${BASE}/bleep-tests/src/scala/bleep/Foo.scala` (see `PortableAnalysisMappers.BaseMarker`). Stripping the marker leaves precisely the repo-relative form a
    * GitHub annotation needs, with no machine-specific prefix ever entering the protocol.
    *
    * A source recorded outside the build directory keeps no `${BASE}` prefix and yields None: it cannot be expressed relative to the repository, and pointing
    * an annotation at an absolute path on a runner would attach it to nothing.
    */
  def relativeSourceFor(analysis: CompileAnalysis, binaryClassName: String): Option[String] = {
    val relations = analysis.asInstanceOf[sbt.internal.inc.Analysis].relations
    // A class name maps to one source in every layout zinc supports; `headOption` over a sorted set keeps the answer stable if that ever stops holding.
    relations
      .definesClass(topLevelClassName(binaryClassName))
      .toList
      .map(_.id())
      .sorted
      .headOption
      .flatMap(stripBaseMarker)
  }

  /** As [[relativeSourceFor]], but reading the project's analysis from disk through the shared cache.
    *
    * Goes through [[AnalysisCache]] because a failing suite reports one location per failing test: a run with fifty failures in one project would otherwise
    * deserialise the same multi-megabyte analysis fifty times, and the cache is keyed by file and mtime so a stale entry cannot outlive a recompile.
    *
    * Returns None for anything that is merely unknown — no analysis yet, an unreadable or half-written file, a class zinc never saw. This decorates output the
    * user already has, so a failure to resolve costs a bare file name, and letting it throw would take the surrounding test report down with it.
    */
  def relativeSourceForProject(analyses: AnalysisCache.Ref, analysisFile: Path, binaryClassName: String): Option[String] =
    if (!Files.exists(analysisFile)) None
    else
      try {
        val mtime = Files.getLastModifiedTime(analysisFile).toMillis
        val analysis = analyses.get(analysisFile, mtime).orElse {
          val store = sbt.internal.inc.consistent.ConsistentFileAnalysisStore.binary(
            analysisFile.toFile,
            ZincBridge.analysisMappers(analysisFile),
            reproducible = true,
            parallelism = AnalysisReadParallelism
          )
          Option(store.get().orElse(null)).map(contents => analyses.put(analysisFile, mtime, contents.getAnalysis))
        }
        analysis.flatMap(a => relativeSourceFor(a, binaryClassName))
      } catch {
        case _: Exception => None
      }

  /** Reading here is off the hot path — one file, after the tests have already run — so it does not need the compile path's wider fan-out. */
  private val AnalysisReadParallelism = 4

  private val BasePrefix: String = PortableAnalysisMappers.BaseMarker + "/"

  private def stripBaseMarker(id: String): Option[String] =
    if (id.startsWith(BasePrefix)) Some(id.substring(BasePrefix.length)) else None
}
