package bleep.internal

import bleep.{model, ExplodedBuild, Started}

object ProjectGlobs {
  def apply(started: Started) = new ProjectGlobs(started.activeProjectsFromPath, started.build)
}

class ProjectGlobs(activeProjectsFromPath: List[model.CrossProjectName], explodedBuild: ExplodedBuild) {
  def projectCompletions(projects: Iterable[model.CrossProjectName]): Map[String, Iterable[model.CrossProjectName]] = {
    val crossNames: Map[String, Iterable[model.CrossProjectName]] =
      projects.map(projectName => projectName.value -> List(projectName)).toMap
    val projectNames: Map[String, Iterable[model.CrossProjectName]] =
      projects.groupBy { case model.CrossProjectName(name, _) => name.value }
    val crossIds: Map[String, Iterable[model.CrossProjectName]] =
      projects
        .groupBy { case name @ model.CrossProjectName(_, crossId) =>
          crossId.orElse {
            val p = explodedBuild.projects(name)
            model.CrossId.defaultFrom(p.scala.flatMap(_.version), p.platform.flatMap(_.name))
          }
        }
        .collect { case (Some(crossId), names) => (crossId.value, names) }

    crossIds ++ projectNames ++ crossNames
  }

  def projectNameMap: Map[String, Iterable[model.CrossProjectName]] = {
    val projects: Iterable[model.CrossProjectName] =
      activeProjectsFromPath match {
        case Nil      => explodedBuild.projects.keys
        case nonEmpty => nonEmpty
      }
    projectCompletions(projects)
  }

  def testProjectNameMap: Map[String, Iterable[model.CrossProjectName]] = {
    val projects: Iterable[model.CrossProjectName] =
      activeProjectsFromPath match {
        case Nil      => explodedBuild.projects.keys
        case nonEmpty => nonEmpty
      }

    val testProjects = projects.filter(projectName => !explodedBuild.projects(projectName).testFrameworks.isEmpty)

    projectCompletions(testProjects)
  }
}
