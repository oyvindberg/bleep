package bleep
package rewrites

import bleep.templates.templatesInfer

import scala.collection.immutable

/** A facade to do build rewrites in terms of exploded projects. This code will (if necessary, depending on the build type) take your changes and rewrite
  * templates and cross projects accordingly.
  *
  * Note that results are not optimal or even good, but it's a start which hides most of the complexity in the model
  */
trait BuildRewrite {
  val name: model.BuildRewriteName

  protected def newExplodedProjects(oldBuild: model.Build, buildPaths: BuildPaths): Map[model.CrossProjectName, model.Project]

  final def apply(oldBuild: model.Build, buildPaths: BuildPaths): model.Build =
    oldBuild match {
      case oldBuild: model.Build.Exploded   => apply(oldBuild, buildPaths)
      case oldBuild: model.Build.FileBacked => apply(oldBuild, buildPaths)
    }

  final def apply(oldBuild: model.Build.Exploded, buildPaths: BuildPaths): model.Build.Exploded = {
    val newProjects = newExplodedProjects(oldBuild, buildPaths)
    oldBuild.copy(explodedProjects = newProjects)
  }

  final def apply(oldBuild: model.Build.FileBacked, buildPaths: BuildPaths): model.Build.FileBacked = {
    val newProjects = newExplodedProjects(oldBuild, buildPaths)
    BuildRewrite.withProjects(oldBuild, newProjects)
  }
}

object BuildRewrite {
  def withProjects(oldBuild: model.Build.FileBacked, newProjects: Map[model.CrossProjectName, model.Project]): model.Build.FileBacked = {
    val allCrossNames = newProjects.keySet.toList ++ oldBuild.explodedProjects.keySet

    val additions = List.newBuilder[BuildPatch.AddValues]
    val removals = List.newBuilder[BuildPatch.RemoveValues]
    val removeProjects = Set.newBuilder[model.CrossProjectName]
    val rest = List.newBuilder[BuildPatch]

    allCrossNames.foreach { crossName =>
      (oldBuild.explodedProjects.get(crossName), newProjects.get(crossName)) match {
        case (Some(oldP), Some(newP)) =>
          if (oldP == newP) ()
          else {
            removals += BuildPatch.RemoveValues(oldP.removeAll(newP), Set(crossName))
            additions += BuildPatch.AddValues(newP.removeAll(oldP), Set(crossName), overwrite = true)
          }
        case (None, Some(newP)) =>
          rest += BuildPatch.AddProject(newP, crossName)
        case (Some(_), None) =>
          removeProjects += crossName
        case (None, None) =>
          sys.error("unexpected")
      }
    }

    // group changes because the implementation of patches has an easier time with templates

    val groupedAdditions: immutable.Iterable[BuildPatch.AddValues] =
      additions
        .result()
        .filterNot(_.values.isEmpty)
        .groupBy(_.values)
        .map { case (addition, sameAdditions) => BuildPatch.AddValues(addition, sameAdditions.flatMap(_.crossProjectNames).toSet, overwrite = true) }

    val groupedRemovals: immutable.Iterable[BuildPatch.RemoveValues] =
      removals
        .result()
        .filterNot(_.values.isEmpty)
        .groupBy(_.values)
        .map { case (addition, sameAdditions) => BuildPatch.RemoveValues(addition, sameAdditions.flatMap(_.crossProjectNames).toSet) }

    val groupedProjectRemovals = BuildPatch.RemoveProjects(removeProjects.result())
    val patches: Iterable[BuildPatch] =
      List(groupedProjectRemovals) ++ groupedAdditions ++ groupedRemovals ++ rest.result()

    val patched = patches.foldLeft(oldBuild)(BuildPatch.apply)

    // mostly aesthetic, lift values existing in all cross projects to the shared project
    val rebalancedCross = patched.mapBuildFile { bf =>
      val newProjects = bf.projects.map {
        // non-empty cross, as well as all represented in current project (as opposed to being all inherited)
        case (name, p) if p.cross.value.size >= 2 && patched.explodedProjectsByName(name).size == p.cross.value.size =>
          val shared = p.cross.value.values.reduce(_ intersect _)
          val newCross = p.cross.map { case (crossId, p) => (crossId, p.removeAll(shared)) }
          (name, p.union(shared).copy(cross = newCross))
        case unchanged => unchanged
      }
      bf.copy(projects = newProjects)
    }

    val withDroppedEmptyTemplates = rebalancedCross.mapBuildFile { bf =>
      val keptTemplates = bf.templates.value.collect { case (name, project) if !project.isEmpty => name }.toSet
      val newProjects = bf.projects.map { case (name, p) =>
        (name, templatesInfer.keepOnly(keptTemplates, p))
      }
      bf.copy(projects = newProjects)
    }

    withDroppedEmptyTemplates
  }
}
