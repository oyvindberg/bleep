package bleep.model

/** Represents named groups of projects based on scala version and platforms
  */
class ProjectGlobs(activeProjectsFromPath: Option[Array[CrossProjectName]], explodedProjects: Map[CrossProjectName, Project]) {
  def projectCompletions(projects: Array[CrossProjectName]): Map[String, Array[CrossProjectName]] = {
    val crossNames: Map[String, Array[CrossProjectName]] =
      projects.map(projectName => projectName.value -> Array(projectName)).toMap
    val projectNames: Map[String, Array[CrossProjectName]] =
      projects.groupBy { case CrossProjectName(name, _) => name.value }
    val crossIds: Map[String, Array[CrossProjectName]] =
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
    val javaProjects: Map[String, Array[CrossProjectName]] = {
      val javaOnly = projects.filter { name =>
        val p = explodedProjects(name)
        p.scala.flatMap(_.version).isEmpty && p.kotlin.flatMap(_.version).isEmpty
      }
      if (javaOnly.nonEmpty) Map("java" -> javaOnly) else Map.empty
    }
    val kotlinProjects: Map[String, Array[CrossProjectName]] = {
      val kotlinOnly = projects.filter { name =>
        val p = explodedProjects(name)
        p.kotlin.flatMap(_.version).isDefined
      }
      if (kotlinOnly.nonEmpty) Map("kotlin" -> kotlinOnly) else Map.empty
    }
    val prefixGroups: Map[String, Array[CrossProjectName]] = {
      val byPrefix = scala.collection.mutable.Map.empty[String, scala.collection.mutable.ArrayBuffer[CrossProjectName]]
      projects.foreach { p =>
        val segments = p.name.value.split('/')
        var i = 1
        while (i < segments.length) {
          val prefix = segments.take(i).mkString("/")
          byPrefix.getOrElseUpdate(prefix, scala.collection.mutable.ArrayBuffer.empty) += p
          i += 1
        }
      }
      byPrefix.iterator.map { case (k, v) => (k, v.toArray) }.toMap
    }

    javaProjects ++ kotlinProjects ++ prefixGroups ++ crossIds ++ projectNames ++ crossNames
  }

  def exactProjectMap: Map[String, CrossProjectName] =
    explodedProjects.map { case (crossName, _) => crossName.value -> crossName }

  def projectNameMap: Map[String, Array[CrossProjectName]] = {
    val projects: Array[CrossProjectName] =
      activeProjectsFromPath match {
        case None           => explodedProjects.keys.toArray
        case Some(nonEmpty) => nonEmpty
      }
    projectCompletions(projects)
  }

  def testProjectNameMap: Map[String, Array[CrossProjectName]] = {
    val projects: Array[CrossProjectName] =
      activeProjectsFromPath match {
        case None           => explodedProjects.keys.toArray
        case Some(nonEmpty) => nonEmpty
      }

    val testProjects = projects.filter(projectName => explodedProjects(projectName).isTestProject.getOrElse(false))

    projectCompletions(testProjects)
  }

  def hasSourceGenProjectNameMap: Map[String, Array[CrossProjectName]] = {
    val projects: Array[CrossProjectName] =
      activeProjectsFromPath match {
        case None           => explodedProjects.keys.toArray
        case Some(nonEmpty) => nonEmpty
      }

    val ps = projects.filterNot(projectName => explodedProjects(projectName).sourcegen.isEmpty)

    projectCompletions(ps)
  }

  def projectNamesNoCrossMap: Map[String, ProjectName] = {
    val projects: Iterable[CrossProjectName] =
      activeProjectsFromPath match {
        case None           => explodedProjects.keys
        case Some(nonEmpty) => nonEmpty
      }
    projects.map(p => (p.name.value, p.name)).toMap
  }
}
