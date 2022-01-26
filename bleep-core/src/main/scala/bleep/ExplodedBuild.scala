package bleep

import bleep.internal.Functions.stripExtends
import bleep.internal.rewriteDependentData
import bleep.model.CrossProjectName

import java.net.URI
import scala.collection.immutable.SortedMap
import scala.collection.{immutable, SortedSet}

case class ExplodedBuild(
    version: String,
    templates: Map[model.TemplateId, model.Project],
    scripts: Map[model.ScriptName, JsonList[model.ScriptDef]],
    resolvers: JsonSet[URI],
    projects: Map[model.CrossProjectName, model.Project]
) {

  def dropTemplates: ExplodedBuild =
    copy(
      templates = Map.empty,
      projects = projects.map { case (crossName, p) => (crossName, stripExtends(p)) }
    )

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
    val explodedTemplates: SortedMap[model.TemplateId, model.Project] =
      rewriteDependentData.eager(build.templates.value) { (_, p, eval) =>
        p.`extends`.values.foldLeft(p)((acc, templateId) => acc.union(eval(templateId).forceGet))
      }

    def explode(p: model.Project): model.Project =
      p.`extends`.values.foldLeft(p)((acc, templateId) => acc.union(explodedTemplates(templateId)))

    val explodedProjects: Map[model.CrossProjectName, model.Project] =
      build.projects.value.flatMap { case (projectName, p) =>
        val explodedP = explode(p)

        val explodeCross: Map[model.CrossProjectName, model.Project] =
          if (explodedP.cross.isEmpty) Map(model.CrossProjectName(projectName, None) -> explodedP)
          else {
            explodedP.cross.value.map { case (crossId, crossP) =>
              (model.CrossProjectName(projectName, Some(crossId)), explode(crossP).union(explodedP))
            }
          }

        explodeCross
      }

    val ret = ExplodedBuild(
      version = build.version,
      templates = explodedTemplates,
      scripts = build.scripts.value,
      resolvers = build.resolvers,
      projects = explodedProjects
    )

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
