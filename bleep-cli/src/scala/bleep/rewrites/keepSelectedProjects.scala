package bleep
package rewrites

import bleep.internal.ProjectGlobs
import bleep.model.ExplodedBuild

case class keepSelectedProjects(selectedProjectGlobs: List[String]) extends Rewrite {
  override val name = "keep-selected-projects"

  def selectedPlusTransitiveDeps(selectedProjectNames: List[model.CrossProjectName], explodedBuild: ExplodedBuild): Set[model.CrossProjectName] = {
    val b = Set.newBuilder[model.CrossProjectName]
    selectedProjectNames.foreach { name =>
      b += name
      explodedBuild.transitiveDependenciesFor(name).keys.foreach(b += _)
    }
    b.result()
  }

  override def apply(explodedBuild: ExplodedBuild): ExplodedBuild = {
    val globs = new ProjectGlobs(Nil, explodedBuild)
    val selectedProjectNames = selectedProjectGlobs.flatMap(globs.projectNameMap)
    val withTransitive = selectedPlusTransitiveDeps(selectedProjectNames, explodedBuild)

    val chosen = explodedBuild.projects.filter { case (name, _) => withTransitive(name) }
    explodedBuild.copy(projects = chosen)
  }

}
