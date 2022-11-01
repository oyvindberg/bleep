package bleep
package packaging

import bleep.internal.rewriteDependentData
import coursier.core.Dependency

import scala.collection.immutable.SortedMap

object packageLibraries {
  def apply(
      started: Started,
      coordinatesFor: CoordinatesFor,
      shouldInclude: model.CrossProjectName => Boolean,
      publishLayout: PublishLayout
  ): SortedMap[model.CrossProjectName, PackagedLibrary] =
    rewriteDependentData(started.build.explodedProjects).startFrom[PackagedLibrary](shouldInclude) { case (projectName, project, eval) =>
      val versionCombo = model.VersionCombo.fromExplodedProject(project).orThrowTextWithContext(projectName)

      val deps: List[Dependency] =
        List(
          started.build.resolvedDependsOn(projectName).iterator.map(eval(_).forceGet.asDependency),
          project.dependencies.values.iterator.map(_.asDependency(versionCombo).orThrowTextWithContext(projectName)),
          versionCombo.libraries(project.isTestProject.getOrElse(false)).map(_.asDependency(versionCombo).orThrowTextWithContext(projectName))
        ).flatten

      val self: Dependency = coordinatesFor(projectName, project).asDependency(versionCombo).orThrowTextWithContext(projectName)

      val files =
        publishLayout match {
          case PublishLayout.Maven(info) => GenLayout.maven(self, started.projectPaths(projectName), deps, info)
          case PublishLayout.Ivy         => GenLayout.ivy(self, started.projectPaths(projectName), deps)
        }

      PackagedLibrary(self, files)
    }
}
