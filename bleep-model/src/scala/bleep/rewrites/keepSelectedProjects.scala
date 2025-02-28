package bleep
package rewrites

import bleep.internal.TransitiveProjects

case class keepSelectedProjects(selectedProjectGlobs: List[String]) extends BuildRewrite {
  override val name = model.BuildRewriteName("keep-selected-projects")

  protected def newExplodedProjects(oldBuild: model.Build, buildPaths: BuildPaths): Map[model.CrossProjectName, model.Project] = {
    val globs = new model.ProjectGlobs(None, oldBuild.explodedProjects)
    val selectedProjectNames = selectedProjectGlobs.toArray.flatMap(str => globs.projectNameMap.getOrElse(str, Array.empty[model.CrossProjectName]))
    val withTransitive = TransitiveProjects(oldBuild, selectedProjectNames).all.toSet

    val chosen = oldBuild.explodedProjects.filter { case (name, _) => withTransitive(name) }
    chosen
  }
}
