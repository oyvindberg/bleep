package bleep.analysis

import scala.collection.mutable

/** Project dependency graph for parallel compilation.
  *
  * Projects are nodes, dependencies are edges. A project can only compile after all its dependencies have compiled.
  *
  * This is the project-level replacement for the old file-level DagBuilder. Instead of tracking individual file dependencies, we track project dependencies and
  * let Zinc/Kotlin handle incremental compilation within each project.
  */
case class ProjectDag(
    projects: Map[String, ProjectConfig],
    edges: Map[String, Set[String]] // project -> dependencies
) {

  /** Get all projects that the given project depends on */
  def dependenciesOf(project: String): Set[String] =
    edges.getOrElse(project, Set.empty)

  /** Get all projects that depend on the given project */
  def dependentsOf(project: String): Set[String] =
    edges.collect { case (p, deps) if deps.contains(project) => p }.toSet

  /** Get projects with no dependencies (can compile first) */
  def roots: Set[String] =
    projects.keySet.filter(p => dependenciesOf(p).isEmpty)

  /** Get projects with no dependents (leaves) */
  def leaves: Set[String] =
    projects.keySet.filter(p => dependentsOf(p).isEmpty)

  /** Get projects that are ready to compile given already completed projects.
    *
    * A project is ready when all its dependencies have completed.
    *
    * @param completed
    *   set of already compiled project names
    * @return
    *   set of project names ready to compile
    */
  def ready(completed: Set[String]): Set[String] = {
    val remaining = projects.keySet -- completed
    remaining.filter { p =>
      val deps = dependenciesOf(p)
      deps.subsetOf(completed)
    }
  }

  /** Get topological ordering of projects for compilation.
    *
    * Projects appear after all their dependencies.
    *
    * @return
    *   ordered list of project names, or Left with cycle info if cyclic
    */
  def topologicalOrder: Either[CycleDetected, List[String]] = {
    val inDegree = mutable.Map[String, Int]()
    projects.keys.foreach { p =>
      inDegree(p) = dependenciesOf(p).size
    }

    val queue = mutable.Queue[String]()
    inDegree.foreach { case (p, deg) =>
      if (deg == 0) queue.enqueue(p)
    }

    val result = mutable.ListBuffer[String]()
    while (queue.nonEmpty) {
      val p = queue.dequeue()
      result += p
      dependentsOf(p).foreach { dep =>
        inDegree(dep) -= 1
        if (inDegree(dep) == 0) queue.enqueue(dep)
      }
    }

    if (result.size == projects.size) {
      Right(result.toList)
    } else {
      // Find a cycle
      val remaining = projects.keySet -- result.toSet
      val cycle = findCycle(remaining)
      Left(CycleDetected(cycle))
    }
  }

  /** Validate the DAG - check for cycles and missing dependencies */
  def validate: Either[DagValidationError, Unit] = {
    // Check for missing dependencies
    val missing = edges.flatMap { case (project, deps) =>
      deps.filterNot(projects.contains).map(d => (project, d))
    }
    if (missing.nonEmpty) {
      return Left(MissingDependencies(missing.toMap))
    }

    // Check for cycles
    topologicalOrder match {
      case Left(cycle) => Left(cycle)
      case Right(_)    => Right(())
    }
  }

  private def findCycle(remaining: Set[String]): List[String] = {
    if (remaining.isEmpty) return Nil

    val visited = mutable.Set[String]()
    val path = mutable.ListBuffer[String]()

    def dfs(node: String): Option[List[String]] = {
      if (path.contains(node)) {
        // Found cycle
        val cycleStart = path.indexOf(node)
        return Some((path.drop(cycleStart) :+ node).toList)
      }
      if (visited.contains(node)) return None

      visited += node
      path += node

      for (dep <- dependenciesOf(node) if remaining.contains(dep))
        dfs(dep) match {
          case Some(cycle) => return Some(cycle)
          case None        => ()
        }

      path.dropRightInPlace(1)
      None
    }

    remaining
      .collectFirst {
        case p if !visited.contains(p) => dfs(p)
      }
      .flatten
      .getOrElse(remaining.take(2).toList)
  }

  /** Get the maximum parallelism possible (width of widest layer) */
  def maxParallelism: Int =
    topologicalOrder match {
      case Left(_)      => 0
      case Right(order) =>
        var completed = Set.empty[String]
        var maxWidth = 0
        while (completed.size < projects.size) {
          val readyNow = ready(completed)
          maxWidth = math.max(maxWidth, readyNow.size)
          completed = completed ++ readyNow
        }
        maxWidth
    }

  /** Add a project to the DAG */
  def addProject(config: ProjectConfig, dependencies: Set[String]): ProjectDag =
    ProjectDag(
      projects + (config.name -> config),
      edges + (config.name -> dependencies)
    )

  /** Remove a project from the DAG */
  def removeProject(name: String): ProjectDag =
    ProjectDag(
      projects - name,
      (edges - name).view.mapValues(_ - name).toMap
    )
}

object ProjectDag {

  /** Create an empty DAG */
  val empty: ProjectDag = ProjectDag(Map.empty, Map.empty)

  /** Create a DAG from a list of project configs with dependencies.
    *
    * @param projects
    *   list of (config, dependencies) pairs
    * @return
    *   the constructed DAG
    */
  def fromProjects(projects: Seq[(ProjectConfig, Set[String])]): ProjectDag = {
    val projectMap = projects.map { case (config, _) => config.name -> config }.toMap
    val edges = projects.map { case (config, deps) => config.name -> deps }.toMap
    ProjectDag(projectMap, edges)
  }

  /** Build a DAG from project configs that have dependsOn relationships.
    *
    * @param projects
    *   list of configs
    * @param dependsOn
    *   function to get dependencies for a project name
    * @return
    *   the constructed DAG
    */
  def build(projects: Seq[ProjectConfig], dependsOn: String => Set[String]): ProjectDag = {
    val projectMap = projects.map(p => p.name -> p).toMap
    val edges = projects.map(p => p.name -> dependsOn(p.name)).toMap
    ProjectDag(projectMap, edges)
  }
}

/** DAG validation errors */
sealed trait DagValidationError

case class CycleDetected(cycle: List[String]) extends DagValidationError {
  override def toString: String = s"Cycle detected: ${cycle.mkString(" -> ")}"
}

case class MissingDependencies(missing: Map[String, String]) extends DagValidationError {
  override def toString: String = {
    val details = missing.map { case (project, dep) => s"$project depends on missing $dep" }
    s"Missing dependencies: ${details.mkString(", ")}"
  }
}
