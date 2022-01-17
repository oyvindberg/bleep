package bleep

import bleep.model.CrossProjectName

import java.net.URI
import scala.collection.{immutable, SortedSet}

case class ExplodedBuild(
    version: String,
    templates: Option[Map[model.TemplateId, model.Project]],
    scripts: Option[Map[model.ScriptName, JsonList[model.ScriptDef]]],
    resolvers: JsonSet[URI],
    projects: Map[model.CrossProjectName, model.Project]
) {

  // in json we just specify projectName, but in bleep we need to know which cross version to pick
  val resolvedDependsOn: Map[CrossProjectName, SortedSet[CrossProjectName]] = {
    val byName: Map[model.ProjectName, immutable.Iterable[model.CrossProjectName]] =
      projects.groupMap(_._1.name)(_._1)

    projects.map { case (crossProjectName, p) =>
      val resolvedDependsOn: SortedSet[CrossProjectName] =
        p.dependsOn.values.map { depName =>
          byName(depName) match {
            case Seq(unambiguous: CrossProjectName) => unambiguous
            case depCrossVersions =>
              val sameCrossId = depCrossVersions.find(_.crossId == crossProjectName.crossId)

              def sameScalaAndPlatform: Option[CrossProjectName] =
                depCrossVersions.find { crossName =>
                  val depCross = projects(crossName)
                  depCross.scala.flatMap(_.version) == p.scala.flatMap(_.version) &&
                  depCross.platform.flatMap(_.name) == p.platform.flatMap(_.name)
                }

              sameCrossId
                .orElse(sameScalaAndPlatform)
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
  def of(build: model.Build): ExplodedBuild = {
    val explodedProjects: Map[model.CrossProjectName, model.Project] =
      build.projects.flatMap { case (projectName, p) =>
        def explodeTemplates(project: model.Project): model.Project =
          project.`extends`.values.foldLeft(project) { case (p, parentId) =>
            p.union(explodeTemplates(build.templates.get(parentId)))
          }

        val explodedTemplates = explodeTemplates(p).copy(`extends` = JsonList.empty)

        val explodeCross: Map[model.CrossProjectName, model.Project] =
          if (explodedTemplates.cross.isEmpty) Map(model.CrossProjectName(projectName, None) -> explodedTemplates)
          else {
            explodedTemplates.cross.value.map { case (crossId, crossP) =>
              (model.CrossProjectName(projectName, Some(crossId)), explodeTemplates(crossP).union(explodedTemplates))
            }
          }

        explodeCross
      }

    val ret = ExplodedBuild(
      version = build.version,
      templates = build.templates,
      scripts = build.scripts,
      resolvers = build.resolvers,
      projects = explodedProjects
    )

    verify(ret)

    ret
  }

  def verify(build: ExplodedBuild): Unit =
    build.scripts match {
      case None => ()
      case Some(scripts) =>
        scripts.foreach { case (scriptName, scriptDefs) =>
          scriptDefs.values.foreach { scriptDef =>
            if (build.projects.contains(scriptDef.project)) ()
            else sys.error(s"script ${scriptName.value} references non-existing project ${scriptDef.project.value}")
          }
        }
    }
}
