package bleep.testing

import bleep.model.CrossProjectName

/** Represents the dependency graph of projects for scheduling compilation and test execution.
  *
  * @param allProjects
  *   All projects involved in the build
  * @param testProjects
  *   Subset of projects that contain tests
  * @param dependencies
  *   Map from project to its direct dependencies (projects it depends on)
  * @param dependents
  *   Map from project to projects that directly depend on it
  */
case class ProjectGraph(
    allProjects: Set[CrossProjectName],
    testProjects: Set[CrossProjectName],
    dependencies: Map[CrossProjectName, Set[CrossProjectName]],
    dependents: Map[CrossProjectName, Set[CrossProjectName]]
) {

  /** Projects that have no dependencies (can be compiled first) */
  def roots: Set[CrossProjectName] =
    allProjects.filter(p => dependencies.getOrElse(p, Set.empty).isEmpty)

  /** Projects that have no dependents (leaves of the graph) */
  def leaves: Set[CrossProjectName] =
    allProjects.filter(p => dependents.getOrElse(p, Set.empty).isEmpty)

  /** Check if all dependencies of a project are in the given set */
  def dependenciesSatisfied(project: CrossProjectName, completed: Set[CrossProjectName]): Boolean =
    dependencies.getOrElse(project, Set.empty).forall(completed.contains)

  /** Get all projects that are ready to compile given the set of completed projects */
  def readyToCompile(completed: Set[CrossProjectName], inProgress: Set[CrossProjectName]): Set[CrossProjectName] =
    allProjects
      .diff(completed)
      .diff(inProgress)
      .filter(dependenciesSatisfied(_, completed))

  /** Compute the transitive closure of dependents (all downstream projects) */
  def transitiveDownstream(project: CrossProjectName): Set[CrossProjectName] = {
    def go(current: Set[CrossProjectName], visited: Set[CrossProjectName]): Set[CrossProjectName] = {
      val next = current.flatMap(p => dependents.getOrElse(p, Set.empty)).diff(visited)
      if (next.isEmpty) visited
      else go(next, visited ++ next)
    }
    go(Set(project), Set.empty)
  }

  /** Compute the transitive closure of dependencies (all upstream projects) */
  def transitiveUpstream(project: CrossProjectName): Set[CrossProjectName] = {
    def go(current: Set[CrossProjectName], visited: Set[CrossProjectName]): Set[CrossProjectName] = {
      val next = current.flatMap(p => dependencies.getOrElse(p, Set.empty)).diff(visited)
      if (next.isEmpty) visited
      else go(next, visited ++ next)
    }
    go(Set(project), Set.empty)
  }
}

object ProjectGraph {

  /** Build a ProjectGraph from an exploded build configuration
    *
    * @param projectDependencies
    *   Map from each project to its direct dependencies
    * @param testProjects
    *   Set of projects that are test projects
    */
  def apply(
      projectDependencies: Map[CrossProjectName, Set[CrossProjectName]],
      testProjects: Set[CrossProjectName]
  ): ProjectGraph = {
    val allProjects = projectDependencies.keySet

    // Build reverse mapping (dependents)
    val dependents: Map[CrossProjectName, Set[CrossProjectName]] =
      projectDependencies.toList
        .flatMap { case (project, deps) =>
          deps.map(dep => dep -> project)
        }
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.toSet)
        .toMap

    ProjectGraph(
      allProjects = allProjects,
      testProjects = testProjects,
      dependencies = projectDependencies,
      dependents = dependents
    )
  }
}

/** Priority information for a project used in scheduling decisions */
case class ProjectPriority(
    project: CrossProjectName,
    isTestProject: Boolean,
    distanceToNearestTest: Int,
    reachableTestCount: Int,
    estimatedCompileWeight: Long
) {

  /** Higher score = higher priority for compilation */
  def score: Double =
    if (isTestProject) {
      // Test projects get highest priority - we want to start running tests ASAP
      10000.0
    } else if (distanceToNearestTest == 1) {
      // Projects that directly enable test projects get high priority
      1000.0 + reachableTestCount * 10.0
    } else if (distanceToNearestTest > 0) {
      // Projects closer to tests get more priority
      100.0 / distanceToNearestTest + reachableTestCount * 5.0
    } else {
      // Projects that don't lead to any tests (shouldn't happen in test runs)
      reachableTestCount * 1.0
    }
}

object ProjectPriority {

  /** Compute priorities for all projects in the graph */
  def computeAll(
      graph: ProjectGraph,
      sourceFileCounts: Map[CrossProjectName, Long]
  ): Map[CrossProjectName, ProjectPriority] = {
    // BFS backwards from test projects to compute distance to nearest test
    val distanceToTest = computeDistanceToTest(graph)

    // Count reachable tests for each project
    val reachableTests = computeReachableTests(graph)

    graph.allProjects.map { project =>
      val priority = ProjectPriority(
        project = project,
        isTestProject = graph.testProjects.contains(project),
        distanceToNearestTest = distanceToTest.getOrElse(project, Int.MaxValue),
        reachableTestCount = reachableTests.getOrElse(project, 0),
        estimatedCompileWeight = sourceFileCounts.getOrElse(project, 10L)
      )
      project -> priority
    }.toMap
  }

  /** BFS from test projects backwards to compute minimum distance to any test */
  private def computeDistanceToTest(graph: ProjectGraph): Map[CrossProjectName, Int] = {
    import scala.collection.mutable

    val distances = mutable.Map[CrossProjectName, Int]()

    // Initialize test projects with distance 0
    graph.testProjects.foreach(p => distances(p) = 0)

    // BFS backwards through dependencies
    val queue = mutable.Queue[(CrossProjectName, Int)]()
    graph.testProjects.foreach(p => queue.enqueue((p, 0)))

    while (queue.nonEmpty) {
      val (current, dist) = queue.dequeue()
      // Look at projects that this project depends on (going upstream)
      graph.dependencies.getOrElse(current, Set.empty).foreach { upstream =>
        if (!distances.contains(upstream) || distances(upstream) > dist + 1) {
          distances(upstream) = dist + 1
          queue.enqueue((upstream, dist + 1))
        }
      }
    }

    distances.toMap
  }

  /** Count how many test projects are reachable downstream from each project */
  private def computeReachableTests(graph: ProjectGraph): Map[CrossProjectName, Int] =
    graph.allProjects.map { project =>
      val downstream = graph.transitiveDownstream(project)
      val testCount = downstream.count(graph.testProjects.contains) +
        (if (graph.testProjects.contains(project)) 1 else 0)
      project -> testCount
    }.toMap
}
