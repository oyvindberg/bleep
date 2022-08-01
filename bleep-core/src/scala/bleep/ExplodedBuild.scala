package bleep

import bleep.internal.Functions.stripExtends
import bleep.internal.{asYamlString, rewriteDependentData}

import scala.collection.SortedSet
import scala.collection.immutable.SortedMap

case class ExplodedBuild(
    build: model.Build,
    templates: Map[model.TemplateId, model.Project],
    projects: Map[model.CrossProjectName, model.Project]
) {
  def scripts: Map[model.ScriptName, JsonList[model.ScriptDef]] =
    build.scripts.value

  def dropTemplates: ExplodedBuild =
    copy(
      templates = Map.empty,
      projects = projects.map { case (crossName, p) => (crossName, stripExtends(p)) }
    )

  val retainCrossTemplates: Map[model.ProjectName, JsonSet[model.TemplateId]] =
    build.projects.value.flatMap { case (projectName, p) =>
      Some((projectName, p.`extends`))
    }

  // in json we just specify projectName, but in bleep we need to know which cross version to pick
  val resolvedDependsOn: Map[model.CrossProjectName, SortedSet[model.CrossProjectName]] = {
    val byName: Map[model.ProjectName, Iterable[model.CrossProjectName]] =
      projects.groupBy(_._1.name).map { case (k, v) => (k, v.keys) }

    projects.map { case (crossProjectName, p) =>
      val resolvedDependsOn: SortedSet[model.CrossProjectName] =
        p.dependsOn.values.map { depName =>
          byName(depName) match {
            case unambiguous if unambiguous.size == 1 => unambiguous.head
            case depCrossVersions =>
              val sameCrossId = depCrossVersions.find(_.crossId == crossProjectName.crossId)

              def sameScalaAndPlatform: Option[model.CrossProjectName] =
                depCrossVersions.find { crossName =>
                  val depCross = projects(crossName)
                  depCross.scala.flatMap(_.version) == p.scala.flatMap(_.version) &&
                  depCross.platform.flatMap(_.name) == p.platform.flatMap(_.name)
                }

              def sameScalaBinVersionAndPlatform: Option[model.CrossProjectName] =
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

  def transitiveDependenciesFor(name: model.CrossProjectName): Map[model.CrossProjectName, model.Project] = {
    val builder = Map.newBuilder[model.CrossProjectName, model.Project]

    def go(depName: model.CrossProjectName): Unit =
      projects.get(depName) match {
        case Some(p) =>
          builder += ((depName, p))
          resolvedDependsOn(depName).foreach(go)

        case None =>
          sys.error(s"Project ${name.value} depends on non-existing project ${depName.value}")
      }

    resolvedDependsOn(name).foreach(go)

    builder.result()
  }
}

object ExplodedBuild {
  def diffProjects(before: ExplodedBuild, after: ExplodedBuild): SortedMap[model.CrossProjectName, String] = {
    val allProjects = before.projects.keySet ++ after.projects.keySet
    val diffs = SortedMap.newBuilder[model.CrossProjectName, String]
    allProjects.foreach { projectName =>
      (before.projects.get(projectName), after.projects.get(projectName)) match {
        case (Some(before), Some(after)) if after == before => ()
        case (Some(before), Some(after)) =>
          val onlyInBefore = asYamlString(before.removeAll(after))
          val onlyInAfter = asYamlString(Option(after.removeAll(before)))
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

  def of(build: model.Build): ExplodedBuild = {
    val explodedTemplates: SortedMap[model.TemplateId, model.Project] =
      rewriteDependentData(build.templates.value).eager[model.Project] { (_, p, eval) =>
        p.`extends`.values.foldLeft(p)((acc, templateId) => acc.union(eval(templateId).forceGet))
      }

    def explode(p: model.Project): model.Project =
      p.`extends`.values.foldLeft(p)((acc, templateId) => acc.union(explodedTemplates(templateId)))

    val explodedProjects: Map[model.CrossProjectName, model.Project] =
      build.projects.value.flatMap { case (projectName, p) =>
        val explodedP = explode(p)

        val explodeCross: Map[model.CrossProjectName, model.Project] =
          if (explodedP.cross.isEmpty) {
            val withDefaults = Defaults.add.project(explodedP)
            Map(model.CrossProjectName(projectName, None) -> withDefaults)
          } else {
            explodedP.cross.value.map { case (crossId, crossP) =>
              val combinedWithCrossProject = explode(crossP).union(explodedP.copy(cross = JsonMap.empty))
              val withDefaults = Defaults.add.project(combinedWithCrossProject)
              (model.CrossProjectName(projectName, Some(crossId)), withDefaults)
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
