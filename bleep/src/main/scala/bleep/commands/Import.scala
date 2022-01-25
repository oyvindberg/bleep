package bleep
package commands

import bleep.Templates.{CompressingProject, CompressingTemplate, TemplateDef}
import bleep.internal.{Os, ShortenAndSortJson}
import bleep.logging.Logger
import io.circe.syntax._

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import scala.collection.immutable.SortedMap

case class Import(logger: Logger) extends BleepCommand {
  val ignoreWhenInferringTemplates = Set.empty[model.ProjectName]

  /** The `Dep.ScalaDependency` structure has three fields we can only correctly determine in context of a given scala version. We need to propagate those three
    * flags up to all projects with same scala version or platform. After that, the "combine by cross" functionality will work better
    */
  def unifyDeps(allDeps: Iterable[Dep]): Map[Dep, Dep] = {
    val javaDeps = allDeps.collect { case x: Dep.JavaDependency => x }
    val scalaDeps = allDeps.collect { case x: Dep.ScalaDependency => x }
    val rewrittenScalaDeps: Map[Dep, Dep] =
      scalaDeps
        .groupBy(x => x.copy(forceJvm = false, for3Use213 = false, for213Use3 = false))
        .flatMap { case (base, providedByBase) =>
          val combined = base.copy(
            forceJvm = providedByBase.exists(_.forceJvm),
            for3Use213 = providedByBase.exists(_.for3Use213),
            for213Use3 = providedByBase.exists(_.for213Use3)
          )
          providedByBase.map(provided => (provided, combined))
        }
    rewrittenScalaDeps ++ javaDeps.map(x => (x, x))
  }

  case class CompressingProjects[Key](projects: Map[Key, CompressingProject]) {
    def map(f: (Key, CompressingProject) => model.Project): CompressingProjects[Key] =
      copy(projects = projects.map { case (n, cp) => (n, cp.copy(current = f(n, cp))) })
  }

  override def run(): Unit = {
    val buildPaths = BuildPaths(Os.cwd / Defaults.BuildFileName)

//    cli("sbt 'set Global / bloopConfigDir := baseDirectory.value / s\".bleep/import/bloop-${scalaBinaryVersion.value}\"' +bloopInstall")(buildPaths.buildDir)

    val build0 = importBloopFilesFromSbt(logger, buildPaths)

    val cp = CompressingProjects(build0.projects.map { case (n, p) => (n, CompressingProject(p, p)) })

    val cp1: CompressingProjects[model.CrossProjectName] = {
      val unifiedDeps: Map[Dep, Dep] =
        unifyDeps(build0.projects.flatMap(_._2.dependencies.values))

      cp.map { case (_, CompressingProject(_, current)) =>
        current.copy(
          dependencies = current.dependencies.map(unifiedDeps.apply),
          scala = current.scala.map(removeScalaDefaults),
          platform = current.platform.map(removePlatformDefaults),
          `source-layout` = current.`source-layout`.filterNot(_ == SourceLayout.Normal)
        )
      }
    }

    val cp2: CompressingProjects[model.CrossProjectName] = {
      val build = build0.copy(projects = cp1.projects.map { case (n, cp) => (n, cp.current) })
      val deduplicatedProjects = deduplicateDependencies(build).projects
      cp1.map((n, _) => deduplicatedProjects(n))
    }

    val templates: SortedMap[TemplateDef, Templates.CompressingTemplate] = {
      val projects: List[CompressingProject] = cp2.projects.filterNot { case (crossName, _) => ignoreWhenInferringTemplates(crossName.name) }.values.toList
      Templates.inferFromExistingProjects(
        TemplateDef.applicableForProjects(projects.map(_.exploded)),
        projects
      )
    }

    val cp3: CompressingProjects[model.CrossProjectName] = cp2.map { (name, cp) =>
      Templates.applyTemplates(templates, cp).current
    }

    val groupedCrossProjects: CompressingProjects[model.ProjectName] =
      CompressingProjects(
        cp3.projects.toSeq.groupBy(_._1.name).map {
          case (name, Seq((model.CrossProjectName(_, None), one))) => (name, one)
          case (name, crossProjects) =>
            val compressingProjectByCrossId: Seq[(model.CrossId, CompressingProject)] =
              crossProjects.map { case (crossProjectName, p) => (crossProjectName.crossId.get, p) }

            val currentCross: model.Project = {
              val common = compressingProjectByCrossId.map(_._2.current).reduce(_.intersect(_))
              val cross = compressingProjectByCrossId.map { case (projectName, p) => (projectName, p.current.removeAll(common)) }.toMap
              common.copy(cross = JsonMap(cross))
            }
            val explodedCross = {
              val common = compressingProjectByCrossId.map(_._2.exploded).reduce(_.intersect(_))
              val cross = compressingProjectByCrossId.map { case (projectName, p) => (projectName, p.exploded) }.toMap
              common.copy(cross = JsonMap(cross))
            }

            (name, CompressingProject(explodedCross, currentCross))
        }
      )

    val crossTemplates: SortedMap[TemplateDef, Templates.CompressingTemplate] = {
      val projects = groupedCrossProjects.projects.filterNot { case (name, _) => ignoreWhenInferringTemplates(name) }.values
      Templates.inferFromExistingProjects(
        TemplateDef.crossTemplates(projects.map(_.exploded)),
        projects.toList
      )
    }

    val groupedTemplatedCrossProjects: CompressingProjects[model.ProjectName] =
      groupedCrossProjects.map { case (name, project) =>
        Templates.applyTemplates(crossTemplates, project).current
      }

    val build = model.Build(
      build0.version,
      massageTemplates(templates ++ crossTemplates),
      build0.scripts,
      build0.resolvers,
      groupedTemplatedCrossProjects.projects.map { case (n, cp) => (n, cp.current) }
    )

    val gcBuild = garbageCollectTemplates(build)

    Files.writeString(
      buildPaths.bleepJsonFile,
      gcBuild.asJson.foldWith(ShortenAndSortJson).spaces2,
      UTF_8
    )
    logger.info(s"Imported ${build.projects.size} projects")
  }

  def garbageCollectTemplates(b: model.Build): model.Build = {
    val seen = collection.mutable.Set.empty[model.TemplateId]
    def go(p: model.Project): Unit = {
      p.`extends`.values.foreach { templateId =>
        seen += templateId
        go(b.templates.get(templateId))
      }

      p.cross.value.values.foreach(go)
    }

    b.projects.values.foreach(go)

    b.copy(templates = b.templates.map(_.filter { case (templateId, _) => seen(templateId) }))
  }

  def massageTemplates(templates: SortedMap[Templates.TemplateDef, CompressingTemplate]): Option[SortedMap[model.TemplateId, model.Project]] =
    Some(templates.map { case (templateDef, p) => (templateDef.templateName, p.current) }.filterNot(_._2.isEmpty))

  def removeScalaDefaults(ret: model.Scala): model.Scala =
    ret.copy(setup = ret.setup.map(setup => setup.removeAll(Defaults.DefaultCompileSetup)))

  def removePlatformDefaults(x: model.Platform): model.Platform =
    x.removeAll(Defaults.Jvm)
}
