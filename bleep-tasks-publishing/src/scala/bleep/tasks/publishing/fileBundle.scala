package bleep.tasks.publishing

import bleep.internal.rewriteDependentData
import bleep.model.{Dep, VersionScalaPlatform}
import bleep.{BleepException, RelPath, Started, model}
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
      asDep: (model.CrossProjectName, model.Project) => Dep,
      shouldInclude: model.CrossProjectName => Boolean,
      bundleLayout: BundleLayout
  ): SortedMap[model.CrossProjectName, Deployable] =
    rewriteDependentData(started.build.projects).startFrom[Deployable](shouldInclude) { case (projectName, project, recurse) =>
      val scalaPlatform = VersionScalaPlatform.fromExplodedProject(project) match {
        case Left(err)    => throw new BleepException.Text(projectName, err)
        case Right(value) => value
      }

      val deps: List[Dependency] =
        List(
          started.build.resolvedDependsOn(projectName).toList.map(recurse(_).forceGet.asDependency),
          project.dependencies.values.toList.map(_.dependencyForce(projectName, scalaPlatform)),
          scalaPlatform.libraries(project.isTestProject.getOrElse(false)).map(_.dependencyForce(projectName, scalaPlatform)).toList
        ).flatten

      val self: Dependency = asDep(projectName, project).dependencyForce(projectName, scalaPlatform)

      val files =
        bundleLayout match {
          case BundleLayout.Maven(info) => GenLayout.maven(self, started.projectPaths(projectName), deps, info)
          case BundleLayout.Ivy         => GenLayout.ivy(self, started.projectPaths(projectName), deps)
        }

      Deployable(self, files)
    }
}
