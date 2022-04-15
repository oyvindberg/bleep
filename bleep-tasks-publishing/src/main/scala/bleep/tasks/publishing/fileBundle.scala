package bleep.tasks.publishing

import bleep.internal.{rewriteDependentData, ScalaVersions}
import bleep.{model, BuildException, Dep, RelPath, Started}
import coursier.core.Dependency

import java.time.ZonedDateTime
import scala.collection.immutable.SortedMap

case class Deployable(asDependency: Dependency, files: Layout[RelPath, Array[Byte]])

object fileBundle {

  sealed trait BundleLayout
  object BundleLayout {
    case object Maven extends BundleLayout
    case class Ivy(published: ZonedDateTime) extends BundleLayout
  }

  def apply(
      started: Started,
      asDep: (model.CrossProjectName, model.Project) => Dep,
      shouldInclude: model.CrossProjectName => Boolean,
      bundleLayout: BundleLayout = BundleLayout.Maven
  ): SortedMap[model.CrossProjectName, Deployable] =
    rewriteDependentData(started.build.projects).startFrom[Deployable](shouldInclude) { case (projectName, project, recurse) =>
      val scalaVersion = ScalaVersions.fromExplodedProject(project) match {
        case Left(err)    => throw new BuildException.Text(projectName, err)
        case Right(value) => value
      }

      val deps: List[Dependency] =
        List(
          started.build.resolvedDependsOn(projectName).toList.map(recurse(_).forceGet.asDependency),
          project.dependencies.values.toList.map(_.dependencyForce(projectName, scalaVersion)),
          scalaVersion.libraries(project.isTestProject.getOrElse(false)).map(_.dependencyForce(projectName, scalaVersion)).toList
        ).flatten

      val self: Dependency = asDep(projectName, project).dependencyForce(projectName, scalaVersion)

      val files =
        bundleLayout match {
          case BundleLayout.Maven          => GenLayout.maven(self, started.projectPaths(projectName), deps)
          case BundleLayout.Ivy(published) => GenLayout.ivy(self, started.projectPaths(projectName), deps, published)
        }

      Deployable(self, files)
    }
}
