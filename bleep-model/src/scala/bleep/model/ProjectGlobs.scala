package bleep.model

/** Represents named groups of projects based on scala version and platforms
  */
class ProjectGlobs(activeProjectsFromPath: List[CrossProjectName], explodedProjects: Map[CrossProjectName, Project]) {
  def projectCompletions(projects: Iterable[CrossProjectName]): Map[String, Iterable[CrossProjectName]] = {
    val crossNames: Map[String, Iterable[CrossProjectName]] =
      projects.map(projectName => projectName.value -> List(projectName)).toMap
    val projectNames: Map[String, Iterable[CrossProjectName]] =
      projects.groupBy { case CrossProjectName(name, _) => name.value }
    val crossIds: Map[String, Iterable[CrossProjectName]] =
      projects
        .groupBy { case name @ CrossProjectName(_, crossId) =>
          crossId.orElse {
            val p = explodedProjects(name)
            CrossId.defaultFrom(
              p.scala.flatMap(_.version),
              p.platform.flatMap(_.name),
              isFull = false // todo: represent in project
            )
          }
        }
        .collect { case (Some(crossId), names) => (crossId.value, names) }

    crossIds ++ projectNames ++ crossNames
  }

  def exactProjectMap: Map[String, CrossProjectName] =
    explodedProjects.map { case (crossName, _) => crossName.value -> crossName }

  def projectNameMap: Map[String, Iterable[CrossProjectName]] = {
    val projects: Iterable[CrossProjectName] =
      activeProjectsFromPath match {
        case Nil      => explodedProjects.keys
        case nonEmpty => nonEmpty
      }
    projectCompletions(projects)
  }

  def testProjectNameMap: Map[String, Iterable[CrossProjectName]] = {
    val projects: Iterable[CrossProjectName] =
      activeProjectsFromPath match {
        case Nil      => explodedProjects.keys
        case nonEmpty => nonEmpty
      }

    val testProjects = projects.filter(projectName => explodedProjects(projectName).isTestProject.getOrElse(false))

    projectCompletions(testProjects)
  }

  def projectNamesNoCrossMap: Map[String, ProjectName] = {
    val projects: Iterable[CrossProjectName] =
      activeProjectsFromPath match {
        case Nil      => explodedProjects.keys
        case nonEmpty => nonEmpty
      }
    projects.map(p => (p.name.value, p.name)).toMap
  }
}
