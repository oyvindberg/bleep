package bleep
package model

import bleep.internal.rewriteDependentData
import bleep.rewrites.Defaults
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

import scala.collection.SortedSet
import scala.collection.immutable.SortedMap

sealed trait Build {
  def $version: BleepVersion
  def explodedProjects: Map[CrossProjectName, Project]
  def resolvers: JsonList[Repository]
  def scripts: Map[ScriptName, JsonList[ScriptDef]]
  def jvm: Option[Jvm]

  /** Remote-cache config from `bleep.yaml`. Survives the FileBacked → Exploded transformation that some `ResolveProjects` implementations perform — without
    * this, anything that pattern-matches on `Build.FileBacked` (and only `FileBacked`) would silently lose the field after a rewrite.
    */
  def remoteCache: Option[RemoteCacheConfig]

  def dropBuildFile: Build.Exploded = this match {
    case build: Build.Exploded   => build
    case build: Build.FileBacked =>
      Build.Exploded(build.file.$version, explodedProjects, build.resolvers, build.file.jvm, build.scripts, build.file.`remote-cache`)
  }

  def requireFileBacked(ctx: String): Build.FileBacked =
    this match {
      case _: Build.Exploded =>
        throw new BleepException.Text(
          s"$ctx: needs a build backed by a build file (this information may have been lost in a build rewrite)"
        )
      case build: Build.FileBacked => build
    }

  // A `dependsOn` entry names a project, and optionally which of its cross versions (`name@crossId`). Stated, that cross version is the one; left out, it is
  // inferred here: the one with the same cross id, else the same Scala version and platform, and so on.
  lazy val resolvedDependsOn: Map[CrossProjectName, SortedSet[CrossProjectName]] = {
    val byName: Map[ProjectName, Iterable[CrossProjectName]] =
      explodedProjectsByName.map { case (k, v) => (k, v.keys) }

    explodedProjects.map { case (crossProjectName, p) =>
      val resolvedDependsOn: SortedSet[CrossProjectName] =
        p.dependsOn.values.map {
          case ProjectRef(depName, Some(crossId)) =>
            val explicit = CrossProjectName(depName, Some(crossId))
            if (explodedProjects.contains(explicit)) explicit
            else throw new BleepException.Text(s"$crossProjectName: depends on non-existing project $explicit")
          case ProjectRef(depName, None) =>
            byName.get(depName) match {
              case None =>
                throw new BleepException.Text(s"$crossProjectName: depends on non-existing project $depName")
              case Some(unambiguous) if unambiguous.size == 1 => unambiguous.head
              case Some(depCrossVersions)                     =>
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

                def compatibleAndSamePlatform: Option[CrossProjectName] =
                  depCrossVersions.find { crossName =>
                    val depCross = explodedProjects(crossName)
                    val thatScala3Or213 = depCross.scala.flatMap(_.version).map(_.is3Or213)
                    val thatPlatformName = depCross.platform.flatMap(_.name)

                    thatScala3Or213 == thisScalaVersion.map(_.is3Or213) &&
                    thatPlatformName == thisPlatformName
                  }

                sameCrossId
                  .orElse(sameScalaAndPlatform)
                  .orElse(sameScalaBinVersionAndPlatform)
                  .orElse(compatibleAndSamePlatform)
                  .toRight {
                    s"$crossProjectName: Couldn't figure out which of ${depCrossVersions.map(_.value).mkString(", ")}"
                  }
                  .orThrowText
            }
        }

      (crossProjectName, resolvedDependsOn)
    }
  }

  /** Every [[IndirectDependency]] of every project, checked to exist. Written as exact cross names (`name` or `name@crossId`), like a script's `project`. */
  lazy val resolvedIndirectDependencies: Map[CrossProjectName, List[IndirectDependency]] =
    explodedProjects.map { case (crossProjectName, p) =>
      val indirect = p.indirectReferences
      indirect.foreach { dep =>
        if (!explodedProjects.contains(dep.project))
          throw new BleepException.Text(s"$crossProjectName: ${dep.reason} names non-existing project ${dep.project.value}")
      }
      (crossProjectName, indirect)
    }

  /** What must be built before each project: its `dependsOn` and its [[resolvedIndirectDependencies]]. Scheduling uses this; classpaths use
    * [[resolvedDependsOn]]. Checked for cycles — a cycle through an indirect edge would otherwise wait forever instead of failing.
    */
  lazy val resolvedBuildOrderDeps: Map[CrossProjectName, SortedSet[CrossProjectName]] = {
    val deps = resolvedDependsOn.map { case (crossProjectName, direct) =>
      (crossProjectName, direct ++ resolvedIndirectDependencies(crossProjectName).map(_.project))
    }
    // Depth-first, three states: absent = unvisited, false = on the current path, true = finished.
    val state = scala.collection.mutable.Map.empty[CrossProjectName, Boolean]
    def visit(p: CrossProjectName, path: List[CrossProjectName]): Unit =
      state.get(p) match {
        case Some(true)  => ()
        case Some(false) =>
          // `path` is nearest-first, so the cycle is p, then the path back down to p, reversed
          val cycle = (p :: path.takeWhile(_ != p).reverse) :+ p
          throw new BleepException.Text(s"build order cycle: ${cycle.map(_.value).mkString(" -> ")}")
        case None =>
          state(p) = false
          deps(p).foreach(visit(_, p :: path))
          state(p) = true
      }
    deps.keys.foreach(visit(_, Nil))
    deps
  }

  /** Everything that must be built before `name`, transitively through both `dependsOn` and indirect dependencies. Excludes `name` itself. */
  def transitiveBuildOrderDepsFor(name: CrossProjectName): Set[CrossProjectName] = {
    val seen = scala.collection.mutable.Set.empty[CrossProjectName]
    def go(p: CrossProjectName): Unit = resolvedBuildOrderDeps(p).foreach(dep => if (seen.add(dep)) go(dep))
    go(name)
    seen.toSet
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

  /** The platforms a project is built for, across its cross projects. The `cross-full` source layout shares sources between them, see [[SourceLayout]] */
  lazy val crossPlatforms: Map[ProjectName, Set[PlatformId]] =
    explodedProjectsByName.map { case (name, crossProjects) => (name, crossProjects.values.flatMap(_.platform.flatMap(_.name)).toSet) }
}

object Build {

  // this data structure is typically imported from sbt or otherwise. it is typically not stored in this very verbose shape
  // it's verbose because there are no templates so all projects are spelled out in full. that's what "exploded" means in this codebase
  final case class Exploded(
      $version: BleepVersion,
      explodedProjects: Map[CrossProjectName, Project],
      resolvers: JsonList[Repository],
      jvm: Option[Jvm],
      scripts: Map[ScriptName, JsonList[ScriptDef]],
      remoteCache: Option[RemoteCacheConfig]
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

  object Exploded {
    // For Build.Exploded, we need simple decoders that don't validate against available templates/projects
    // These are used when deserializing a build that has already been exploded
    private implicit val templateIdDecoder: Decoder[TemplateId] = Decoder[String].map(TemplateId.apply)
    private implicit val projectNameDecoder: Decoder[ProjectName] = Decoder[String].map(ProjectName.apply)

    // Explicit encoder/decoder for Map[CrossProjectName, Project]
    private implicit val mapCPNProjectEncoder: Encoder[Map[CrossProjectName, Project]] =
      Encoder.encodeMap[CrossProjectName, Project](using CrossProjectName.keyEncodes, Project.encodes)
    private implicit val mapCPNProjectDecoder: Decoder[Map[CrossProjectName, Project]] =
      Decoder.decodeMap[CrossProjectName, Project](using CrossProjectName.keyDecodes, Project.decodes)

    // Explicit encoder/decoder for Map[ScriptName, JsonList[ScriptDef]]
    private implicit val mapScriptEncoder: Encoder[Map[ScriptName, JsonList[ScriptDef]]] =
      Encoder.encodeMap[ScriptName, JsonList[ScriptDef]](using ScriptName.keyEncodes, JsonList.encodes[ScriptDef])
    private implicit val mapScriptDecoder: Decoder[Map[ScriptName, JsonList[ScriptDef]]] =
      Decoder.decodeMap[ScriptName, JsonList[ScriptDef]](using ScriptName.keyDecodes, JsonList.decodes[ScriptDef])

    implicit val encodes: Encoder[Exploded] = deriveEncoder
    implicit val decodes: Decoder[Exploded] = deriveDecoder
  }

  case class FileBacked(file: BuildFile) extends Build {
    def $version: BleepVersion = file.$version
    def resolvers: JsonList[Repository] = file.resolvers
    def scripts: Map[ScriptName, JsonList[ScriptDef]] = file.scripts.value
    def jvm: Option[Jvm] = file.jvm
    def remoteCache: Option[RemoteCacheConfig] = file.`remote-cache`

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

  def diffProjects(before: Build, after: Build): SortedMap[CrossProjectName, String] =
    diffProjects(before.explodedProjects, after.explodedProjects)

  def diffProjects(before: Map[CrossProjectName, Project], after: Map[CrossProjectName, Project]): SortedMap[CrossProjectName, String] = {
    val allProjects = before.keySet ++ after.keySet
    val diffs = SortedMap.newBuilder[CrossProjectName, String]
    allProjects.foreach { projectName =>
      (before.get(projectName), after.get(projectName)) match {
        case (Some(before), Some(after)) if after == before => ()
        case (Some(before), Some(after))                    =>
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
