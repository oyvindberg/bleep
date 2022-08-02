package bleep
package internal

import bleep.logging.{Formatter, Logger}
import bleep.model.{ExplodedBuild, JsonMap, JsonSet}
import bleep.rewrites.deduplicateDependencies

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.collection.mutable

object Templates {
  @FunctionalInterface
  trait Nameish[P] {
    def projectName(p: P): model.ProjectName
  }

  object Nameish {
    implicit val platformName: Nameish[model.ProjectName] = new Nameish[model.ProjectName] { // do not shorten to SAM, causes ClassCastException
      override def projectName(p: model.ProjectName): model.ProjectName = p
    }
    implicit val crossPlatformName: Nameish[model.CrossProjectName] = (cp: model.CrossProjectName) => cp.name
  }

  implicit class NameIshSyntax[P](private val p: P) extends AnyVal {
    def extractProjectName(implicit N: Nameish[P]): model.ProjectName = N.projectName(p)
  }

  case class ProjectKeepExploded(exploded: model.Project, current: model.Project) {
    def map(f: model.Project => model.Project) = copy(current = f(current))

    def toTemplate = TemplateKeepExploded(exploded, current)
  }
  case class TemplateKeepExploded(exploded: model.Project, current: model.Project) {
    def toProject = ProjectKeepExploded(exploded, current)
  }

  implicit class ProjectOps(p: model.Project) {
    def withTemplate(t: model.TemplateId): model.Project =
      p.copy(`extends` = p.`extends` + t)
    def dropTemplates: model.Project =
      p.copy(`extends` = JsonSet.empty)
  }

  sealed trait TemplateDef {
    final lazy val templateId: model.TemplateId =
      model.TemplateId(s"template-$name")

    final lazy val allParents: List[TemplateDef] = {
      val isTest = this match {
        case TemplateDef.Test(_) => true
        case _                   => false
      }
      val isMain = this match {
        case TemplateDef.Main(_) => true
        case _                   => false
      }

      val b = List.newBuilder[TemplateDef]
      def go(p: TemplateDef): Unit = {
        p.parents.foreach(go)
        if (isTest) {
          val test = TemplateDef.Test(p)
          if (test != this) b += test
        } else if (isMain) {
          val main = TemplateDef.Main(p)
          if (main != this) b += main
        }
        b += p
      }
      parents foreach go
      b.result().distinct
    }

    protected def name: String
    protected def parents: List[TemplateDef]
    def include(p: model.Project): Boolean
  }

  object TemplateDef {
    implicit def ordering: Ordering[TemplateDef] = Ordering.by(_.name)

    def templateGroupsForProjects(
        projects0: Map[model.CrossProjectName, ProjectKeepExploded],
        ignoreWhenInferringTemplates: model.CrossProjectName => Boolean
    ): List[TemplateDef] = {
      val projects = projects0.toList.collect { case (name, p) if !ignoreWhenInferringTemplates(name) => p.exploded }

      val scalas: List[ScalaTemplate] =
        projects
          .flatMap(p => p.scala.flatMap(_.version))
          .flatMap { v =>
            val scala2 = if (v.epoch == '2') Some(Scala2) else None
            val binVersion = ScalaBinVersion(v.binVersion, scala2)
            scala2.toList ++ List(binVersion)
          }
          .distinct

      val platforms: List[Platform] =
        projects.flatMap(p => p.platform.flatMap(_.name)).distinct.map(Platform.apply)

      val combined: List[PlatformScalaVersion] =
        scalas.flatMap(s => platforms.map(p => PlatformScalaVersion(p, s)))

      val base = List(
        List(Common),
        List(Main(Common)),
        scalas,
//        scalas.map(Main.apply),
        platforms,
//        platforms.map(Main.apply),
        combined,
//        combined.map(Main.apply),
        // test
        List(Test(Common))
//        scalas.map(Test.apply),
//        platforms.map(Test.apply),
//        combined.map(Test.apply)
      )
      base.flatten
    }

    def crossTemplates(
        crossProjects0: Map[model.ProjectName, ProjectKeepExploded],
        ignoreWhenInferringTemplates: model.ProjectName => Boolean
    ): List[TemplateDef] = {
      val crossGroups: List[SortedSet[model.CrossId]] =
        crossProjects0
          .collect {
            case (crossName, p) if !ignoreWhenInferringTemplates(crossName) && p.exploded.cross.value.size > 1 =>
              SortedSet.empty[model.CrossId] ++ p.exploded.cross.value.keys
          }
          .toList
          .distinct

      val crossGroupsWithParentGroups: Map[SortedSet[model.CrossId], List[SortedSet[model.CrossId]]] =
        crossGroups.map { crossGroup =>
          val parentGroups = crossGroups.collect {
            case maybeParent if maybeParent.size < crossGroup.size && maybeParent.forall(crossGroup) => maybeParent
          }
          (crossGroup, parentGroups)
        }.toMap

      val crossSetups: Map[SortedSet[model.CrossId], TemplateDef] = {
        implicit val x: Ordering[SortedSet[model.CrossId]] = Ordering.by(_.mkString(""))
        val allCross = SortedSet.empty[model.CrossId] ++ crossGroups.flatten

        rewriteDependentData(crossGroupsWithParentGroups).eager[TemplateDef] { case (crossGroup, parentGroups, eval) =>
          val parents = parentGroups.map(eval.apply)
          CrossSetup(crossGroup, parents.map(_.forceGet), all = allCross)
        }
      }

      val sorted: List[TemplateDef] =
        crossSetups.values.toList.sortWith { case (one, two) =>
          def inherits(current: TemplateDef, maybeParent: model.TemplateId): Boolean =
            current.templateId == maybeParent || current.parents.exists(parent => inherits(parent, maybeParent))

          val oneInherits = inherits(one, two.templateId)
          val twoInherits = inherits(two, one.templateId)
          if (oneInherits) false
          else if (twoInherits) true
          else if (one.parents.length < two.parents.length) true
          else if (one.parents.length > two.parents.length) false
          else one.templateId.value.compareTo(two.templateId.value) < 0
        }

      sorted
    }

    case object Common extends TemplateDef {
      override def name = "common"
      override def parents = Nil
      override def include(p: model.Project): Boolean = true
    }

    case class Test(parent: TemplateDef) extends TemplateDef {
      override def parents = List(parent)
      override def name = s"${parent.name}-test"
      override def include(p: model.Project): Boolean =
        parent.include(p) && p.isTestProject.getOrElse(false)
    }

    case class Main(parent: TemplateDef) extends TemplateDef {
      override def parents = List(parent)
      override def name = s"${parent.name}-main"
      override def include(p: model.Project): Boolean =
        parent.include(p) && !p.isTestProject.contains(true)
    }

    case class Platform(platformName: model.PlatformId) extends TemplateDef {
      override def parents: List[TemplateDef] = List.empty
      override def name = platformName.value
      override def include(p: model.Project): Boolean =
        parents.forall(_.include(p)) && p.cross.isEmpty && p.platform.exists(_.name.contains(platformName))
    }

    sealed trait ScalaTemplate extends TemplateDef

    case object Scala2 extends ScalaTemplate {
      override def name = s"scala-2"
      override def parents: List[TemplateDef] = List.empty
      override def include(p: model.Project): Boolean =
        parents.forall(_.include(p)) && p.cross.isEmpty && p.scala.flatMap(_.version).exists(_.epoch == '2')
    }

    case class ScalaBinVersion(binVersion: String, scala2: Option[Scala2.type]) extends ScalaTemplate {
      override def name = s"scala-$binVersion"
      override def parents: List[TemplateDef] = scala2.toList
      override def include(p: model.Project): Boolean =
        parents.forall(_.include(p)) && p.cross.isEmpty && p.scala.flatMap(_.version).exists(_.binVersion == binVersion)
    }

    case class CrossSetup(crossIds: SortedSet[model.CrossId], parents: List[TemplateDef], all: SortedSet[model.CrossId]) extends TemplateDef {
      override def toString: String = templateId.value
      override def name: String = {
        val baseName =
          if (crossIds == all) "all"
          else {
            val allByPlatform = all.groupBy(_.value.takeWhile(_.isLetter))
            crossIds
              .groupBy(_.value.takeWhile(_.isLetter))
              .map { case (platform, ids) =>
                allByPlatform.get(platform) match {
                  case Some(`ids`) => s"$platform-all"
                  case _           => s"$platform-${ids.map(_.value.dropWhile(_.isLetter)).mkString("-")}"
                }
              }
              .mkString("-")
          }

        s"cross-$baseName"
      }
      override def include(p: model.Project): Boolean =
        crossIds.forall(p.cross.value.keySet)
    }

    case class PlatformScalaVersion(platform: Platform, scalaVersion: ScalaTemplate) extends TemplateDef {
      override def name = s"${scalaVersion.name}-${platform.name}"
      override def parents: List[TemplateDef] = {
        val scala2Platform = scalaVersion match {
          case ScalaBinVersion(_, Some(_)) => Some(PlatformScalaVersion(platform, scalaVersion = Scala2))
          case _                           => None
        }
        List(platform, scalaVersion) ++ scala2Platform
      }
      override def include(p: model.Project): Boolean = parents.forall(_.include(p))
    }
  }

  /** Takes an exploded build, infers templates and applies them. also groups cross projects
    */
  def apply(logger: Logger, build0: ExplodedBuild, ignoreWhenInferringTemplates: model.ProjectName => Boolean): model.Build = {

    val s1 = step[model.CrossProjectName](
      logger,
      initialProjects = build0.projects.map { case (crossName, p) => (crossName, ProjectKeepExploded(p, p)) },
      ignoreWhenInferringTemplates = cn => ignoreWhenInferringTemplates(cn.name),
      templatingContent = RelevantProjectContent.RelevantParents,
      mkTemplates = TemplateDef.templateGroupsForProjects
    )

    val groupedCrossProjects: Map[model.ProjectName, ProjectKeepExploded] =
      groupCrossProjects(s1.templatedProjects)

    val s2 = step[model.ProjectName](
      logger,
      initialProjects = groupedCrossProjects,
      ignoreWhenInferringTemplates,
//      templatingContent = RelevantProjectContent.IncludeAllParentsInCross(allParentsFor),
      templatingContent = RelevantProjectContent.Noop,
      mkTemplates = TemplateDef.crossTemplates
    )

    val build = {
      val templates = (s1.inferredTemplates ++ s2.inferredTemplates).collect {
        case (templateDef, Some(p)) if !p.current.isEmpty => (templateDef.templateId, p.current)
      }
      val projects = s2.templatedProjects.map { case (n, cp) => (n, cp.current) }
      build0.build.copy(templates = JsonMap(templates), projects = JsonMap(projects))
    }

    garbageCollectTemplates(inlineTrivialTemplates(build))
  }

  case class StepResult[ProjectName](
      templatedProjects: Map[ProjectName, ProjectKeepExploded],
      inferredTemplates: Map[TemplateDef, Option[Templates.TemplateKeepExploded]]
  )

  @FunctionalInterface
  trait RelevantProjectContent {
    def apply(p: model.Project, templateDef: TemplateDef): model.Project
  }

  object RelevantProjectContent {
    object Noop extends RelevantProjectContent {
      override def apply(p: model.Project, templateDef: TemplateDef): model.Project = p
    }

    object RelevantParents extends RelevantProjectContent {
      override def apply(p: model.Project, templateDef: TemplateDef): model.Project = {
        val keepParent: Set[model.TemplateId] = templateDef.allParents.map(_.templateId).toSet

        p.copy(`extends` = JsonSet(p.`extends`.values.filter(keepParent)))
      }
    }
  }

  def step[ProjectName: Formatter: Nameish](
      logger: Logger,
      initialProjects: Map[ProjectName, ProjectKeepExploded],
      ignoreWhenInferringTemplates: ProjectName => Boolean,
      templatingContent: RelevantProjectContent,
      mkTemplates: (Map[ProjectName, ProjectKeepExploded], ProjectName => Boolean) => List[TemplateDef]
  ): StepResult[ProjectName] = {
    // we'll apply these groups of templates in turn
    val templates: List[TemplateDef] = mkTemplates(initialProjects, ignoreWhenInferringTemplates)

    val templateById = templates.map(x => (x.templateId, x)).toMap

    // keep state here as we go about applying
    var templatedProjects: Map[ProjectName, ProjectKeepExploded] =
      initialProjects

    val inferredTemplates: mutable.Map[TemplateDef, Option[Templates.TemplateKeepExploded]] =
      mutable.Map.empty

    templates.foreach { templateDef =>
      // filter which projects we use to infer the contents of the template
      val projects = templatedProjects.toVector.filter { case (name, pe) =>
        !ignoreWhenInferringTemplates(name) &&
        templateDef.include(pe.exploded)
      }

      // only infer contents for a template if it is used by more than one project (not counting cross)
      val initial: Either[String, Vector[(ProjectName, ProjectKeepExploded)]] = {
        val projectNames = projects.map { case (pn, _) => pn.extractProjectName }.distinct
        if (projectNames.sizeIs <= 1) Left(s"Too few qualifying project names: $projectNames")
        else Right(projects)
      }

      val fullTemplateContents: Either[String, model.Project] =
        initial.flatMap { projects =>
          projects
            .map { case (_, p) => templatingContent(p.current, templateDef) }
            .optReduce((project, project1) => project.intersectDropEmpty(project1))
            .toRight("No common settings")
        }

      val templateContentsAfterParents: Either[String, TemplateKeepExploded] =
        fullTemplateContents.map { current =>
          val exploded = current.`extends`.values.foldLeft(current) { case (acc, templateId) =>
            templateById.get(templateId).flatMap(inferredTemplates) match {
              case Some(parent) => acc.union(parent.current)
              case None         => acc
            }
          }
          val template = TemplateKeepExploded(exploded, current)
          applyInheritance(templateDef, inferredTemplates.apply, template)
        }

      inferredTemplates(templateDef) = templateContentsAfterParents.toOption

      templateContentsAfterParents match {
        case Right(template) =>
          val appliedTo = List.newBuilder[ProjectName]

          templatedProjects = templatedProjects.flatMap {
            case (name, project0) if templateDef.include(project0.exploded) =>
              tryApplyTemplate(templateDef, template, project0) match {
                case Some(project) =>
                  appliedTo += name
                  Map(name -> project)
                case None => Map(name -> project0)
              }
            case other => Some(other)
          }
          val ps = appliedTo.result()
          logger.withContext("templateId", templateDef.templateId).withContext(ps).debug(s"applied to")

        case Left(err) => logger.withContext("templateId", templateDef.templateId).debug(err)
      }
    }
    StepResult(templatedProjects, inferredTemplates.toMap)
  }

  /** Takes an exploded build, reapplies existing templates
    */
  def reapply(build0: ExplodedBuild, unexplodedTemplates: JsonMap[model.TemplateId, model.Project]): model.Build = {

    val deduplicated: Map[model.CrossProjectName, ProjectKeepExploded] = {
      val deduplicatedProjects = deduplicateDependencies(build0).projects
      build0.projects.map { case (name, exploded) => (name, ProjectKeepExploded(exploded, deduplicatedProjects(name))) }
    }

    val allTemplates: Map[model.TemplateId, TemplateKeepExploded] =
      build0.templates.map { case (name, t) => (name, TemplateKeepExploded(t, t)) }

    val templated: Map[model.CrossProjectName, ProjectKeepExploded] =
      deduplicated.map { case (name, proj) =>
        val pp = proj.map(current => reapplyTemplates(allTemplates, current))
        (name, pp)
      }

    val groupedCrossProjects: Map[model.ProjectName, ProjectKeepExploded] =
      groupCrossProjects(templated).map { case (name, p) =>
        val retainedTemplates = build0.retainCrossTemplates.getOrElse(name, JsonSet.empty[model.TemplateId])
        val pp = p.copy(current = p.current.copy(`extends` = retainedTemplates))
        (name, pp)
      }

    val groupedTemplatedCrossProjects: Map[model.ProjectName, ProjectKeepExploded] =
      groupedCrossProjects.map { case (name, project) =>
        val pp = project.map(p => reapplyTemplates(allTemplates, p))
        (name, pp)
      }

    build0.build.copy(
      templates = unexplodedTemplates,
      projects = JsonMap(groupedTemplatedCrossProjects.map { case (n, cp) => (n, cp.current) })
    )
  }

  def reapplyTemplates(templates: Map[model.TemplateId, TemplateKeepExploded], project: model.Project): model.Project = {
    // applying a template may remove other templateIds in list
    @tailrec
    def go(project: model.Project, `extends`: List[model.TemplateId]): model.Project =
      `extends` match {
        case templateId :: restTemplateIds =>
          val pp =
            if (project.`extends`.values.contains(templateId))
              project.removeAll(templates(templateId).exploded)
            else project

          go(pp, restTemplateIds)
        case Nil => project
      }
    go(project, project.`extends`.values.toList)
  }

  def groupCrossProjects(projects: Map[model.CrossProjectName, ProjectKeepExploded]): Map[model.ProjectName, ProjectKeepExploded] =
    projects.toSeq.groupBy(_._1.name).map {
      case (name, Seq((model.CrossProjectName(_, None), one))) => (name, one)
      case (name, crossProjects) =>
        val compressingProjectByCrossId: Seq[(model.CrossId, ProjectKeepExploded)] =
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

        (name, ProjectKeepExploded(explodedCross, currentCross))
    }

  def garbageCollectTemplates(b: model.Build): model.Build = {
    val seen = collection.mutable.Set.empty[model.TemplateId]
    def go(p: model.Project): Unit = {
      p.`extends`.values.foreach { templateId =>
        seen += templateId
        go(b.templates.value(templateId))
      }

      p.cross.value.values.foreach(go)
    }

    b.projects.value.values.foreach(go)

    b.copy(templates = JsonMap(b.templates.value.filter { case (templateId, _) => seen(templateId) }))
  }

  def inlineTrivialTemplates(b: model.Build): model.Build = {
    val toInline: Map[model.TemplateId, List[model.TemplateId]] =
      b.templates.value.collect {
        case (name, p) if p.`extends`.values.size <= 1 && p.copy(`extends` = JsonSet.empty).isEmpty => (name, p.`extends`.values.toList)
      }

    def expand(templateId: model.TemplateId): List[model.TemplateId] =
      toInline.get(templateId) match {
        case Some(replacements) => replacements.flatMap(expand)
        case None               => List(templateId)
      }

    def go(p: model.Project): model.Project =
      p.copy(
        `extends` = JsonSet(p.`extends`.values.flatMap(expand)),
        cross = JsonMap(p.cross.value.map { case (crossId, p) => (crossId, go(p)) })
      )

    b.copy(
      templates = JsonMap(b.templates.value.collect { case (templateId, p) if !toInline.contains(templateId) => (templateId, go(p)) }),
      projects = JsonMap(b.projects.value.map { case (name, p) => (name, go(p)) })
    )
  }

  /* templates can extend templates*/
  def applyInheritance(
      templateDef: TemplateDef,
      getTemplate: TemplateDef => Option[TemplateKeepExploded],
      project: TemplateKeepExploded
  ): TemplateKeepExploded = {
    def go(acc: TemplateKeepExploded, parent: TemplateDef): TemplateKeepExploded =
      if (acc.current.`extends`.values.contains(parent.templateId)) acc
      else {
        val rec = parent.allParents.foldLeft(acc)(go)

        getTemplate(parent)
          .flatMap(templateProject => tryApplyTemplate(parent, templateProject, rec.toProject))
          .fold(rec)(_.toTemplate)
      }

    templateDef.allParents.foldLeft(project)(go)
  }

  def tryApplyTemplate(template: TemplateDef, templateProject: TemplateKeepExploded, project: ProjectKeepExploded): Option[ProjectKeepExploded] = {
    val shortened = project.current.removeAll(templateProject.exploded)
    val isShortened = shortened != project.current

    def doesntAddNew: Boolean =
      templateProject.exploded.removeAll(project.exploded).isEmpty

    // primitive values will be overridden, so including these templates would be sound. however, it is confusing.
    def doesntHaveIncompatiblePrimitives: Boolean =
      project.exploded.union(templateProject.exploded) == templateProject.exploded.union(project.exploded)

    // this is needed since we disregard `extends` within cross projects, and also trim empty projects in `doesntAddNew`
    def sameCrossVersions: Boolean =
      templateProject.exploded.cross.value.keys.forall(project.exploded.cross.value.keySet)

    if (isShortened && doesntAddNew && doesntHaveIncompatiblePrimitives && sameCrossVersions) {
      Some(
        project.copy(
          current = shortened.withTemplate(template.templateId),
          exploded = project.exploded.withTemplate(template.templateId)
        )
      )
    } else None
  }
}
