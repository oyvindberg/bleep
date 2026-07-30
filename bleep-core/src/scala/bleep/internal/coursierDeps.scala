package bleep
package internal

import coursier.Fetch
import coursier.core.{Classifier, Configuration, Dependency, Publication, Resolution, VariantSelector}
import coursier.util.Artifact

import java.io.File

/** coursier 2.1.25 generalised two of its types to also model Gradle Module Metadata variants: `Dependency.configuration` became `variantSelector`, and the
  * artifact tuples grew an `Either[VariantPublication, Publication]`.
  *
  * Bleep only builds configuration-based dependencies (see `Dep.JavaDependency.dependency`) and only resolves from Maven/Ivy repositories, so the variant side
  * is unreachable — and bleep's own `CoursierResolver.Result` is deliberately the narrower shape, which also keeps the on-disk resolution cache format stable.
  * These convert at the boundary and throw rather than substituting a default if a variant ever does show up.
  */
object coursierDeps {
  extension (dep: Dependency) {
    def configurationOrThrow: Configuration =
      dep.variantSelector match {
        case c: VariantSelector.ConfigurationBased => c.configuration
        case other                                 =>
          sys.error(s"${dep.module}:${dep.versionConstraint.asString} selects an attribute-based variant ($other), which bleep cannot express")
      }
  }

  extension (result: Fetch.Result) {
    def fullDetailedArtifactsOrThrow: Seq[(Dependency, Publication, Artifact, Option[File])] =
      result.fullDetailedArtifacts0.map {
        case (dep, Right(pub), art, fileOpt)       => (dep, pub, art, fileOpt)
        case (dep, Left(variantPublication), _, _) =>
          sys.error(s"${dep.module}:${dep.versionConstraint.asString} resolved to a Gradle Module variant ($variantPublication), which bleep cannot express")
      }
  }

  extension (resolution: Resolution) {

    /** Note coursier's own deprecated `dependencyArtifacts` `collect`s the variant case away silently. We throw instead. */
    def dependencyArtifactsOrThrow(classifiers: Option[Seq[Classifier]], classpathOrder: Boolean): Seq[(Dependency, Publication, Artifact)] =
      resolution.dependencyArtifacts0(classifiers, attributes = None, classpathOrder = classpathOrder).map {
        case (dep, Right(pub), art)             => (dep, pub, art)
        case (dep, Left(variantPublication), _) =>
          sys.error(s"${dep.module}:${dep.versionConstraint.asString} resolved to a Gradle Module variant ($variantPublication), which bleep cannot express")
      }
  }
}
