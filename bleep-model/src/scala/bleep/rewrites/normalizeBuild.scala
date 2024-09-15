package bleep
package rewrites

/** Throw away information which is superfluous. may be needed after import or after user edits build
  */
object normalizeBuild extends BuildRewrite {
  override val name = model.BuildRewriteName("normalize-build")

  val Pipeline: List[BuildRewrite] =
    List(
      unifyDeps,
      Defaults.remove,
      deduplicateDependencies
    )

  protected def newExplodedProjects(oldBuild: model.Build, buildPaths: BuildPaths): Map[model.CrossProjectName, model.Project] =
    Pipeline.foldLeft(oldBuild.dropBuildFile)((acc, f) => f(acc, buildPaths)).explodedProjects
}
