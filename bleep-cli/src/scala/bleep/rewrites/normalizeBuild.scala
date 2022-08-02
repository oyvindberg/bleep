package bleep
package rewrites

/** Throw away information which is superfluous. may be needed after import or after user edits build
  */
object normalizeBuild extends Rewrite {
  override val name = "normalize-build"

  val Pipeline: List[Rewrite] =
    List(
      unifyDeps,
      Defaults.remove,
      deduplicateDependencies
    )

  override def apply(build: model.ExplodedBuild): model.ExplodedBuild =
    Pipeline.foldLeft(build)((acc, f) => f(acc))
}
