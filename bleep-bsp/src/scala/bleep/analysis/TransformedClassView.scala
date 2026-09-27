package bleep.analysis

import sbt.internal.inc.{APIs, Analysis}
import xsbti.UseScope
import xsbti.api.{AnalyzedClass, NameHash}
import xsbti.compile.CompileAnalysis

/** How a consumer's zinc sees a class that a dependency's post-compile transform changed, added or removed.
  *
  * Zinc decides which consumer sources to recompile by comparing, for every upstream class a consumer used, the [[AnalyzedClass]] it recorded last time with
  * the one its lookup returns now: API hash first, then a hash per name, recompiling the sources that use a changed name. It asks bleep's external lookup
  * first, at every point — recording, change detection, refreshing — so what this returns is what zinc records and compares against, consistently.
  *
  * The dependency's analysis describes its compiler output, not the transformed classes the consumer compiled against. So for a class in
  * [[OutputDeterminants.transformedClasses]] the answer is the analysis' view with the transform's effect folded in: the class's API hash and every one of its
  * name hashes are perturbed by the hash of what the transform did to it. When that changes, zinc sees this class change, and recompiles exactly the consumer
  * sources that use it — and nothing else.
  *
  * Every name rather than the ones the transform touched, because the names zinc records are the source language's, decoded (`+`, `x_=`, `Outer.Inner`), while
  * the transform is seen in bytecode (`$plus`, `x_$eq`, `Outer$Inner`), and what a consumer reads differs by language (TASTy, a Scala 2 pickle, the class
  * file). Per class is the granularity that is exact for all of them.
  */
object TransformedClassView {

  /** Marks an answer zinc must take as given. An answer without provenance makes zinc do its own lookup instead, which would describe the compiler output. */
  val Provenance = "bleep:post-compile"

  /** @param compilerView
    *   the class as the dependency's analysis describes it, if it does
    * @return
    *   None for a class the transform removed: it is not on the consumer's classpath
    */
  def apply(transformed: TransformedClass, compilerView: Option[AnalyzedClass]): Option[AnalyzedClass] =
    transformed.kind match {
      case TransformedClass.Kind.Removed                               => None
      case TransformedClass.Kind.Changed | TransformedClass.Kind.Added =>
        // A class zinc never saw (added by the transform) is described from nothing but its names.
        val base = compilerView.getOrElse(APIs.emptyAnalyzedClass.withName(transformed.binaryName))
        val perturbation = java.lang.Long.parseUnsignedLong(transformed.hash.take(8), 16).toInt
        val known = base.nameHashes.map(_.name).toSet
        val nameHashes =
          base.nameHashes.map(n => NameHash.of(n.name, n.scope, n.hash ^ perturbation)) ++
            transformed.names.filterNot(known).map(name => NameHash.of(name, UseScope.Default, perturbation))
        Some(base.withApiHash(base.apiHash ^ perturbation).withNameHashes(nameHashes).withProvenance(Provenance))
    }

  /** The class as zinc's own lookup would find it: in the analysis that has it as a product. */
  def compilerView(analyses: Iterable[CompileAnalysis], binaryClassName: String): Option[AnalyzedClass] =
    analyses.iterator
      .flatMap { a =>
        val analysis = a.asInstanceOf[Analysis]
        analysis.relations.productClassName.reverse(binaryClassName).headOption.flatMap(analysis.apis.internal.get)
      }
      .nextOption()
}
