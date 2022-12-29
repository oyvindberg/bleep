package bleep
package rewrites

case class keepSelectedProjects(selectedProjectGlobs: Array[model.ProjectGlob]) extends BuildRewrite {
  override val name = model.BuildRewriteName("keep-selected-projects")

  def selectedPlusTransitiveDeps(selectedProjectNames: Array[model.CrossProjectName], build: model.Build): Set[model.CrossProjectName] = {
    val b = Set.newBuilder[model.CrossProjectName]
    selectedProjectNames.foreach { name =>
      b += name
      build.transitiveDependenciesFor(name).keys.foreach(b += _)
    }
    b.result()
  }
  protected def newExplodedProjects(oldBuild: model.Build): Map[model.CrossProjectName, model.Project] = {
    val globs = new model.ProjectGlobs(None, oldBuild.explodedProjects)
    val selectedProjectNames = selectedProjectGlobs.flatMap(globs.projectNameMap.get).selection
    val withTransitive = selectedPlusTransitiveDeps(selectedProjectNames, oldBuild)

    val chosen = oldBuild.explodedProjects.filter { case (name, _) => withTransitive(name) }
    chosen
  }
}
