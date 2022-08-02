package bleep.model

import bleep.internal.Functions.stripExtends
import bleep.internal.rewriteDependentData
import bleep.rewrites.Defaults
import bleep.yaml

import scala.collection.SortedSet
import scala.collection.immutable.SortedMap

case class ExplodedBuild(
    build: Build,
    templates: Map[TemplateId, Project],
    projects: Map[CrossProjectName, Project]
) {
  def scripts: Map[ScriptName, JsonList[ScriptDef]] =
    build.scripts.value

  def dropTemplates: ExplodedBuild =
    copy(
      templates = Map.empty,
      projects = projects.map { case (crossName, p) => (crossName, stripExtends(p)) }
    )

  val retainCrossTemplates: Map[ProjectName, JsonSet[TemplateId]] =
    build.projects.value.flatMap { case (projectName, p) =>
      Some((projectName, p.`extends`))
    }

  // in json we just specify projectName, but in bleep we need to know which cross version to pick
  val resolvedDependsOn: Map[CrossProjectName, SortedSet[CrossProjectName]] = {
    val byName: Map[ProjectName, Iterable[CrossProjectName]] =
      projects.groupBy(_._1.name).map { case (k, v) => (k, v.keys) }

    projects.map { case (crossProjectName, p) =>
      val resolvedDependsOn: SortedSet[CrossProjectName] =
        p.dependsOn.values.map { depName =>
          byName(depName) match {
            case unambiguous if unambiguous.size == 1 => unambiguous.head
            case depCrossVersions =>
              val sameCrossId = depCrossVersions.find(_.crossId == crossProjectName.crossId)

              def sameScalaAndPlatform: Option[CrossProjectName] =
                depCrossVersions.find { crossName =>
                  val depCross = projects(crossName)
                  depCross.scala.flatMap(_.version) == p.scala.flatMap(_.version) &&
                  depCross.platform.flatMap(_.name) == p.platform.flatMap(_.name)
                }

              def sameScalaBinVersionAndPlatform: Option[CrossProjectName] =
                depCrossVersions.find { crossName =>
                  val depCross = projects(crossName)
                  depCross.scala.flatMap(_.version).map(_.binVersion) == p.scala.flatMap(_.version).map(_.binVersion) &&
                  depCross.platform.flatMap(_.name) == p.platform.flatMap(_.name)
                }

              sameCrossId
                .orElse(sameScalaAndPlatform)
                .orElse(sameScalaBinVersionAndPlatform)
                .getOrElse(
                  sys.error(s"Couldn't figure out which of ${depCrossVersions.map(_.value).mkString(", ")} to use for project ${crossProjectName.value}")
                )
          }
        }

      (crossProjectName, resolvedDependsOn)
    }
  }

  def transitiveDependenciesFor(name: CrossProjectName): Map[CrossProjectName, Project] = {
    val builder = Map.newBuilder[CrossProjectName, Project]

    def go(depName: CrossProjectName): Unit =
      projects.get(depName) match {
        case Some(p) =>
          builder += ((depName, p))
          resolvedDependsOn(depName).foreach(go)

        case None =>
          throw new bleep.BleepException.Text(name, s"depends on non-existing project ${depName.value}")
      }

    resolvedDependsOn(name).foreach(go)

    builder.result()
  }
}

object ExplodedBuild {
  def diffProjects(before: ExplodedBuild, after: ExplodedBuild): SortedMap[CrossProjectName, String] = {
    val allProjects = before.projects.keySet ++ after.projects.keySet
    val diffs = SortedMap.newBuilder[CrossProjectName, String]
    allProjects.foreach { projectName =>
      (before.projects.get(projectName), after.projects.get(projectName)) match {
        case (Some(before), Some(after)) if after == before => ()
        case (Some(before), Some(after)) =>
          val onlyInBefore = yaml.encodeShortened(before.removeAll(after))
          val onlyInAfter = yaml.encodeShortened(Option(after.removeAll(before)))
          diffs += ((projectName, s"before: $onlyInBefore, after: $onlyInAfter"))
        case (Some(_), None) =>
          diffs += ((projectName, "was dropped"))
        case (None, Some(_)) =>
          diffs += ((projectName, "was added"))
        case (None, None) =>
          ()
      }
    }
    diffs.result()
  }

  def of(build: Build): ExplodedBuild = {
    val explodedTemplates: SortedMap[TemplateId, Project] =
      rewriteDependentData(build.templates.value).eager[Project] { (_, p, eval) =>
        p.`extends`.values.foldLeft(p)((acc, templateId) => acc.union(eval(templateId).forceGet))
      }

    def explode(p: Project): Project =
      p.`extends`.values.foldLeft(p)((acc, templateId) => acc.union(explodedTemplates(templateId)))

    val explodedProjects: Map[CrossProjectName, Project] =
      build.projects.value.flatMap { case (projectName, p) =>
        val explodedP = explode(p)

        val explodeCross: Map[CrossProjectName, Project] =
          if (explodedP.cross.isEmpty) {
            val withDefaults = Defaults.add.project(explodedP)
            Map(CrossProjectName(projectName, None) -> withDefaults)
          } else {
            explodedP.cross.value.map { case (crossId, crossP) =>
              val combinedWithCrossProject = explode(crossP).union(explodedP.copy(cross = JsonMap.empty))
              val withDefaults = Defaults.add.project(combinedWithCrossProject)
              (CrossProjectName(projectName, Some(crossId)), withDefaults)
            }
          }

        explodeCross
      }

    val ret = ExplodedBuild(build, explodedTemplates, explodedProjects)

    verify(ret)

    ret
  }

  def verify(build: ExplodedBuild): Unit =
    build.scripts.foreach { case (scriptName, scriptDefs) =>
      scriptDefs.values.foreach { scriptDef =>
        if (build.projects.contains(scriptDef.project)) ()
        else sys.error(s"script ${scriptName.value} references non-existing project ${scriptDef.project.value}")
      }
    }
}
