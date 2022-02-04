package bleep.internal

import bleep.{Defaults, ExplodedBuild}

/** Throw away information which is superfluous. may be needed after import or after user edits build
  */
object normalizeBuild {
  val Pipeline: List[ExplodedBuild => ExplodedBuild] =
    List(
      unifyDeps.apply,
      Defaults.remove,
      deduplicateDependencies.apply
    )

  def apply(build: ExplodedBuild): ExplodedBuild =
    Pipeline.foldLeft(build)((acc, f) => f(acc))
}
