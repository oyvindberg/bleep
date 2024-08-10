package bleep
package model

import bleep.internal.rewriteDependentData
import bleep.rewrites.Defaults

import scala.collection.SortedSet
import scala.collection.immutable.SortedMap

sealed trait Build {
  def $version: BleepVersion
  def explodedProjects: Map[CrossProjectName, Project]
  def resolvers: JsonList[Repository]
  def scripts: Map[ScriptName, JsonList[ScriptDef]]
  def jvm: Option[Jvm]

  def dropBuildFile: Build.Exploded = this match {
    case build: Build.Exploded   => build
    case build: Build.FileBacked => Build.Exploded(build.file.$version, explodedProjects, build.resolvers, build.file.jvm, build.scripts)
  }

  def requireFileBacked(ctx: String): Build.FileBacked =
    this match {
      case _: Build.Exploded =>
        throw new BleepException.Text(
          s"$ctx: needs a build backed by a build file (this information may have been lost in a build rewrite)"
        )
      case build: Build.FileBacked => build
    }

  // in BuildFile we just specify projectName, but in Build we need to know which cross version to pick
  lazy val resolvedDependsOn: Map[CrossProjectName, SortedSet[CrossProjectName]] = {
    val byName: Map[ProjectName, Iterable[CrossProjectName]] =
      explodedProjectsByName.map { case (k, v) => (k, v.keys) }

    explodedProjects.map { case (crossProjectName, p) =>
      val resolvedDependsOn: SortedSet[CrossProjectName] =
        p.dependsOn.values.map { depName =>
          byName(depName) match {
            case unambiguous if unambiguous.size == 1 => unambiguous.head
            case depCrossVersions =>
              val sameCrossId = depCrossVersions.find(_.crossId == crossProjectName.crossId)

              val thisScalaVersion = p.scala.flatMap(_.version)
              val thisPlatformName = p.platform.flatMap(_.name)

              def sameScalaAndPlatform: Option[CrossProjectName] =
                depCrossVersions.find { crossName =>
                  val depCross = explodedProjects(crossName)
                  val thatScalaVersion = depCross.scala.flatMap(_.version)
                  val thatPlatformName = depCross.platform.flatMap(_.name)
                  thatScalaVersion == thisScalaVersion &&
                  thatPlatformName == thisPlatformName
                }

              def sameScalaBinVersionAndPlatform: Option[CrossProjectName] =
                depCrossVersions.find { crossName =>
                  val depCross = explodedProjects(crossName)
                  val thatBinVersion = depCross.scala.flatMap(_.version).map(_.binVersion)
                  val thatPlatformName = depCross.platform.flatMap(_.name)

                  thatBinVersion == thisScalaVersion.map(_.binVersion) &&
                  thatPlatformName == thisPlatformName
                }

              sameCrossId
                .orElse(sameScalaAndPlatform)
                .orElse(sameScalaBinVersionAndPlatform)
                .toRight {
                  s"$crossProjectName: Couldn't figure out which of ${depCrossVersions.map(_.value).mkString(", ")}"
                }
                .orThrowText
          }
        }

      (crossProjectName, resolvedDependsOn)
    }
  }

  def transitiveDependenciesFor(name: CrossProjectName): Map[CrossProjectName, Project] = {
    val builder = Map.newBuilder[CrossProjectName, Project]

    def go(depName: CrossProjectName): Unit = {
      val p = explodedProjects
        .get(depName)
        .toRight(s"depends on non-existing project ${depName.value}")
        .orThrowTextWithContext(name)
      builder += ((depName, p))
      resolvedDependsOn(depName).foreach(go)
    }

    resolvedDependsOn(name).foreach(go)

    builder.result()
  }

  lazy val explodedProjectsByName: Map[ProjectName, Map[CrossProjectName, Project]] =
    explodedProjects.groupBy { case (crossName, _) => crossName.name }
}

object Build {

  // this data structure is typically imported from sbt or otherwise. it is typically not stored in this very verbose shape
  // it's verbose because there are no templates so all projects are spelled out in full. that's what "exploded" means in this codebase
  final case class Exploded(
      $version: BleepVersion,
      explodedProjects: Map[CrossProjectName, Project],
      resolvers: JsonList[Repository],
      jvm: Option[Jvm],
      scripts: Map[ScriptName, JsonList[ScriptDef]]
  ) extends Build {
    def dropTemplates: Exploded = {
      def stripExtends(p: Project): Project =
        p.copy(
          `extends` = JsonSet.empty,
          cross = JsonMap(p.cross.value.map { case (n, p) => (n, stripExtends(p)) }.filterNot { case (_, p) => p.isEmpty })
        )

      val newProjects = explodedProjects.map { case (crossName, p) => (crossName, stripExtends(p)) }
      copy(explodedProjects = newProjects)
    }
  }

  case class FileBacked(file: BuildFile) extends Build {
    def $version: BleepVersion = file.$version
    def resolvers: JsonList[Repository] = file.resolvers
    def scripts: Map[ScriptName, JsonList[ScriptDef]] = file.scripts.value
    def jvm: Option[Jvm] = file.jvm

    def mapBuildFile(f: BuildFile => BuildFile): Build.FileBacked =
      Build.FileBacked(f(file))

    lazy val explodedTemplates: Map[TemplateId, Project] =
      rewriteDependentData(file.templates.value).eager[Project] { (_, p, eval) =>
        p.`extends`.values.foldLeft(p)((acc, templateId) => acc.union(eval(templateId).forceGet))
      }

    lazy val explodedProjects: Map[CrossProjectName, Project] = {
      def explode(p: Project): Project =
        p.`extends`.values.foldLeft(p)((acc, templateId) => acc.union(explodedTemplates(templateId)))

      file.projects.value.flatMap { case (projectName, p) =>
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
    }
  }

  def diffProjects(before: Build, after: Build): SortedMap[CrossProjectName, String] = {
    val allProjects = before.explodedProjects.keySet ++ after.explodedProjects.keySet
    val diffs = SortedMap.newBuilder[CrossProjectName, String]
    allProjects.foreach { projectName =>
      (before.explodedProjects.get(projectName), after.explodedProjects.get(projectName)) match {
        case (Some(before), Some(after)) if after == before => ()
        case (Some(before), Some(after)) =>
          val onlyInBefore = yaml.encodeShortened(before.removeAll(after))
          val onlyInAfter = yaml.encodeShortened(after.removeAll(before))
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
}
