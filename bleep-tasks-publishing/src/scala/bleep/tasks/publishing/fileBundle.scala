package bleep.tasks.publishing

import bleep.internal.rewriteDependentData
import bleep.{model, BleepException, RelPath, Started}
import coursier.core.{Dependency, Info}

import scala.collection.immutable.SortedMap

case class Deployable(asDependency: Dependency, files: Layout[RelPath, Array[Byte]])

object fileBundle {

  sealed trait BundleLayout
  object BundleLayout {
    case class Maven(info: Info = Info.empty) extends BundleLayout
    case object Ivy extends BundleLayout
  }

  def apply(
      started: Started,
      asDep: (model.CrossProjectName, model.Project) => model.Dep,
      shouldInclude: model.CrossProjectName => Boolean,
      bundleLayout: BundleLayout
  ): SortedMap[model.CrossProjectName, Deployable] =
    rewriteDependentData(started.build.explodedProjects).startFrom[Deployable](shouldInclude) { case (projectName, project, eval) =>
      val versionCombo = model.VersionCombo.unsafeFromExplodedProject(project, projectName)

      val deps: List[Dependency] =
        List(
          started.build.resolvedDependsOn(projectName).toList.map(eval(_).forceGet.asDependency),
          project.dependencies.values.toList.map(_.unsafeAsDependency(Some(projectName), versionCombo)),
          versionCombo.libraries(project.isTestProject.getOrElse(false)).map(_.unsafeAsDependency(Some(projectName), versionCombo)).toList
        ).flatten

      val self: Dependency = asDep(projectName, project).unsafeAsDependency(Some(projectName), versionCombo)

      val files =
        bundleLayout match {
          case BundleLayout.Maven(info) => GenLayout.maven(self, started.projectPaths(projectName), deps, info)
          case BundleLayout.Ivy         => GenLayout.ivy(self, started.projectPaths(projectName), deps)
        }

      Deployable(self, files)
    }
}
