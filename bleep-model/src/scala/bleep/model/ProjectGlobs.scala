package bleep.model

import scala.collection.immutable.SortedMap
import scala.collection.mutable

/** Represents named groups of projects based on scala version and platforms
  */
class ProjectGlobs(activeProjectsFromPath: Option[Array[CrossProjectName]], explodedProjects: Map[CrossProjectName, Project]) {
  lazy val exactProjectMap: SortedMap[ProjectGlob, CrossProjectName] =
    SortedMap.empty[ProjectGlob, CrossProjectName] ++ explodedProjects.map { case (crossName, _) => ProjectGlob(crossName.value) -> crossName }

  lazy val projectNameMap: SortedMap[ProjectGlob, ProjectSelection] = {
    val projects: Array[CrossProjectName] =
      activeProjectsFromPath match {
        case None           => explodedProjects.keys.toArray
        case Some(nonEmpty) => nonEmpty
      }
    ProjectGlobs.make(explodedProjects, projects)
  }

  lazy val testProjectNameMap: SortedMap[ProjectGlob, ProjectSelection] = {
    val projects: Array[CrossProjectName] =
      activeProjectsFromPath match {
        case None           => explodedProjects.keys.toArray
        case Some(nonEmpty) => nonEmpty
      }

    val testProjects = projects.filter(projectName => explodedProjects(projectName).isTestProject.getOrElse(false))

    ProjectGlobs.make(explodedProjects, testProjects)
  }

  lazy val projectNamesNoCrossMap: SortedMap[ProjectGlob, ProjectName] = {
    val projects: Iterable[CrossProjectName] =
      activeProjectsFromPath match {
        case None           => explodedProjects.keys
        case Some(nonEmpty) => nonEmpty
      }
    SortedMap.empty[ProjectGlob, ProjectName] ++ projects.map(p => (ProjectGlob(p.name.value), p.name)).toMap
  }
}
object ProjectGlobs {
  def make(explodedProjects: Map[CrossProjectName, Project], projects: Array[CrossProjectName]): SortedMap[ProjectGlob, ProjectSelection] = {
    val includes = mutable.Map.empty[ProjectGlob, mutable.ArrayBuilder[CrossProjectName]]
    val excludes = mutable.Map.empty[ProjectGlob, mutable.ArrayBuilder[CrossProjectName]]

    def addInclude(group: String, name: CrossProjectName): Unit =
      includes.getOrElseUpdate(ProjectGlob(group), mutable.ArrayBuilder.make[CrossProjectName]) += name

    def addExclude(group: String, name: CrossProjectName): Unit =
      excludes.getOrElseUpdate(ProjectGlob(s"~$group"), mutable.ArrayBuilder.make[CrossProjectName]) += name

    def addBoth(group: String, name: CrossProjectName): Unit = {
      addInclude(group, name)
      addExclude(group, name)
    }

    projects.foreach { name =>
      val p = explodedProjects(name)
      val isTest = p.isTestProject.getOrElse(false)

      addInclude(name.value, name)
      addBoth(if (isTest) "@test" else "@main", name)
      if (name.crossId.isDefined) addInclude(name.name.value + "@*", name)

      val crossId = name.crossId.orElse {
        val p = explodedProjects(name)
        CrossId.defaultFrom(
          p.scala.flatMap(_.version),
          p.platform.flatMap(_.name),
          isFull = false // todo: represent in project
        )
      }

      crossId.foreach { crossId =>
        addBoth(s"@${crossId.value}", name)
      }
      p.platform.flatMap(_.name).foreach { platformId =>
        addBoth(s"@${platformId.value}", name)
      }
      p.scala.flatMap(_.version).foreach { scalaVersion =>
        addBoth(s"@${scalaVersion.binVersion.filter(_.isDigit)}", name)
        addBoth(s"@${scalaVersion.epoch.toString}", name)
      }
    }

    SortedMap.empty[ProjectGlob, ProjectSelection] ++ (
      includes.iterator.map { case (group, b) => (group, ProjectSelection.Include(b.result())) } ++
        excludes.iterator.map { case (group, b) => (group, ProjectSelection.Exclude(b.result())) }
    )
  }
}
