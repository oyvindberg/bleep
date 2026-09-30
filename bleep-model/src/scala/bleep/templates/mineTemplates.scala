package bleep
package templates

import bleep.templates.ProjectNameLike.Syntax
import io.circe.Json
import io.circe.syntax.EncoderOps

import scala.collection.immutable.BitSet
import scala.collection.mutable

/** Finds templates by looking at which projects share which settings, instead of deciding up front which groups of projects a template may cover.
  *
  * Every project is split into single settings: one dependency, one compiler flag, `scala.strict=true`, one `extends` reference, and so on. The projects a
  * setting occurs in is its support. Settings with the same support are a candidate template, and so is every support: the template for a support holds every
  * setting all its projects have. A project which does not have a setting is simply outside that setting's support, so one odd project no longer stops the rest
  * from sharing a template.
  *
  * Candidates are picked greedily by how much they shrink the build file, and a template has a cost of its own so tiny ones are not worth it. The content of a
  * picked template is the real intersection of its projects - `extends` included, so parents common to all of them become the parents of the new template.
  * Splitting into settings is only used to find and score candidates, so it can be approximate without affecting correctness.
  */
object mineTemplates {

  /** One setting. Options are split into single flags, list elements are separate settings, and an element of a set of objects (a dependency with attributes,
    * say) is one setting.
    */
  case class Setting(path: String, value: String)

  case class Result[Name](projects: Map[Name, model.Project], templates: Map[model.TemplateId, model.Project])

  /** @param templateCost
    *   what a template costs in the build file, beyond its content. Keeps inference from producing many tiny templates
    * @param referenceCost
    *   what each `extends` reference to a template costs
    * @param minUsers
    *   how many projects (or templates) must share a template. Two is enough: a new build is an app and its tests, and its scala version should still be set in
    *   one place
    */
  case class Costs(templateCost: Int, referenceCost: Int, minUsers: Int)

  object Costs {

    /** For imports and re-inference: a template has to make the build file smaller */
    val default: Costs = Costs(templateCost = 4, referenceCost = 1, minUsers = 2)

    /** For a new build, which is an app and its tests. Anything they share goes into a template, so there is one place to change the scala version and the
      * build shows how templates are used from the start
      */
    val newBuild: Costs = Costs(templateCost = 0, referenceCost = 0, minUsers = 2)
  }

  /** Fields which hold sets of objects. An element is one setting, and must not be split into its attributes: a set with one element is encoded as that
    * element, not as a list, so two different dependencies would otherwise share their `configuration: provided`.
    */
  private val setOfObjects: Set[String] =
    Set(
      "dependencies",
      "boms",
      "dependencyManagement",
      "compilerPlugins",
      "annotationProcessors",
      "symbolProcessors",
      "jvmAgents",
      "sourcegen",
      "libraryVersionSchemes"
    )

  /** What a template may hold. Where a project lives and which projects it depends on stay on the project, even when several projects share them: a reader
    * looks for those on the project, and the project graph should be readable without following templates.
    */
  def templatable(p: model.Project): model.Project =
    p.copy(folder = None, dependsOn = model.JsonSet.empty, `extends` = model.JsonSet.empty, cross = model.JsonMap.empty)

  /** The settings of `p` a template may hold */
  def settings(p: model.Project): Set[Setting] = allSettings(templatable(p))

  def allSettings(p: model.Project): Set[Setting] = {
    val b = Set.newBuilder[Setting]
    def add(path: String, value: String): Unit = {
      b += Setting(path, value)
      ()
    }
    def go(path: String, json: Json): Unit =
      json.fold[Unit](
        (),
        bool => add(path, bool.toString),
        num => add(path, num.toString),
        str =>
          if (path.endsWith("ptions")) model.Options.parse(List(str), None).values.foreach(opt => add(path, opt.render.mkString(" ")))
          else add(path, str),
        arr => arr.foreach(elem => go(path, elem)),
        obj =>
          if (setOfObjects(path.split('.').last)) add(path, json.noSpaces)
          else obj.toIterable.foreach { case (k, v) => go(if (path.isEmpty) k else s"$path.$k", v) }
      )
    go("", p.asJson)
    b.result()
  }

  def apply[Name: ProjectNameLike: Ordering](
      logger: TemplateLogger,
      projects: Map[Name, model.Project],
      existingTemplates: Map[model.TemplateId, model.Project],
      /** groups of projects the build has names for, see [[Groups]]. Each is offered as a template */
      groups: List[Set[Name]],
      /** whether a template for these projects is about them, and may hold settings about anything. Other templates hold settings about one [[Facet]] */
      isAboutProjects: Set[Name] => Boolean,
      ignoreWhenInferringTemplates: Name => Boolean,
      idPrefix: String,
      costs: Costs
  ): Result[Name] = {
    // what inference works on: projects on the right, and templates on the left once they exist. a setting shared by a template and some projects moves into
    // a new template which both extend, so an early pick never keeps a setting from being shared later
    val rows = mutable.ArrayBuffer.empty[Either[model.TemplateId, Name]]
    val current = mutable.ArrayBuffer.empty[model.Project]
    val currentSettings = mutable.ArrayBuffer.empty[Set[Setting]]
    val inferFrom = mutable.BitSet.empty

    def addRow(row: Either[model.TemplateId, Name], p: model.Project, infer: Boolean): Int = {
      rows += row
      current += p
      currentSettings += settings(p)
      if (infer) inferFrom += (rows.size - 1)
      rows.size - 1
    }

    projects.toVector.sortBy(_._1).foreach { case (name, p) => addRow(Right(name), p, infer = !ignoreWhenInferringTemplates(name)) }
    val indexOf: Map[Name, Int] = rows.indices.flatMap { i =>
      rows(i) match {
        case Right(name) => Some((name, i))
        case Left(_)     => None
      }
    }.toMap
    existingTemplates.toVector.sortBy(_._1.value).foreach { case (id, p) => addRow(Left(id), p, infer = true) }

    // cross projects of one project collapse into one project later, so they are counted once
    def unitOf(i: Int): String = rows(i) match {
      case Right(name) => s"project ${name.extractProjectName.value}"
      case Left(id)    => s"template ${id.value}"
    }

    // how many cross projects a project has. a template is a unit of its own, and a single row
    val crossProjectsOf: Map[String, Int] = rows.indices.map(unitOf).groupMapReduce(identity)(_ => 1)(_ + _)
    def rowsOfUnit(unit: String): Int = if (unit.startsWith("template ")) 1 else crossProjectsOf(unit)

    // which row a template is, and which project rows reach it
    var templateIndex = Map.empty[model.TemplateId, Int]
    var reachers = Map.empty[Int, BitSet]

    // every accepted template shrinks the build by at least one setting, so this is only reached if the estimates are wrong
    val maxIterations = currentSettings.iterator.map(_.size).sum
    var iterations = 0
    var counter = 0
    var continue = true

    while (continue) {
      iterations += 1
      if (iterations > maxIterations) throw new BleepException.Text(s"Template inference did not converge after $maxIterations iterations")

      val supportBySetting = mutable.Map.empty[Setting, BitSet]
      inferFrom.foreach { i =>
        currentSettings(i).foreach { s =>
          supportBySetting.update(s, supportBySetting.getOrElse(s, BitSet.empty) + i)
        }
      }

      // how many settings of each facet have exactly this support
      val facetsBySupport: Map[BitSet, Map[Facet, Int]] =
        supportBySetting.toList.groupMap(_._2)(entry => Facet.of(entry._1.path)).map { case (support, facets) =>
          (support, facets.groupMapReduce(identity)(_ => 1)(_ + _))
        }

      val groupSupports: Set[BitSet] =
        groups.map(members => BitSet.fromSpecific(members.iterator.flatMap(indexOf.get).filter(inferFrom))).filter(_.nonEmpty).toSet

      // a candidate is a support, and a facet unless the support is a group the build has a name for
      val scored: Iterable[(Int, Int, BitSet, Option[Facet])] =
        (facetsBySupport.keySet ++ groupSupports).flatMap { support =>
          val count = support.iterator.map(unitOf).distinct.size
          if (count < costs.minUsers) Nil
          else {
            val sizes: Map[Facet, Int] =
              facetsBySupport.iterator.collect { case (other, facets) if support.subsetOf(other) => facets }.flatten.toList.groupMapReduce(_._1)(_._2)(_ + _)
            // every member but one sheds the content. a project using the template in all its cross projects gains an `extends` reference. those using it in
            // only some of them are cross built alike, and reference it from their cross setup, once. and a member which already extends a template all of
            // the new template's users use gains nothing: the new template extends that one, and the member extends the new template in its place
            val uncovered = support.filterNot { i =>
              current(i).`extends`.values.exists(t => templateIndex.get(t).exists(ti => support.subsetOf(reachers(ti))))
            }
            val (whole, partial) =
              uncovered.iterator.map(unitOf).toList.groupMapReduce(identity)(_ => 1)(_ + _).partition { case (unit, n) => n == rowsOfUnit(unit) }
            val references = whole.size + (if (partial.nonEmpty) 1 else 0)
            def savings(size: Int): Int = (count - 1) * size - references * costs.referenceCost - costs.templateCost
            val projectsOnly: Option[Set[Name]] =
              support.iterator.map(rows).foldLeft(Option(Set.empty[Name])) {
                case (Some(acc), Right(name)) => Some(acc + name)
                case _                        => None
              }
            if (projectsOnly.exists(isAboutProjects)) List((count, savings(sizes.values.sum), support, None))
            // a template which is not about a group of projects needs two settings at least: one setting behind an `extends` is indirection, not abstraction
            else sizes.toList.collect { case (facet, size) if size >= 2 => (count, savings(size), support, Some(facet)) }
          }
        }

      // of the templates which pay for themselves, the one with the most users goes first: a setting lives at the most general level it is shared at, so
      // what all projects share ends up in `template-common` and not repeated in every template for a scala version. counted in cross projects: every
      // project may have a 2.13 variant, but only some of the cross projects are
      val best: Option[(Int, BitSet, Option[Facet])] =
        scored
          .filter(_._2 > 0)
          .maxByOption { case (count, savings, support, facet) => (support.size, count, savings, -support.head, -facet.fold(-1)(Facet.All.indexOf)) }
          .map { case (_, savings, support, facet) => (savings, support, facet) }

      best match {
        case None                      => continue = false
        case Some((_, support, facet)) =>
          val shared = templatable(support.iterator.map(i => current(i)).reduce((a, b) => a.intersect(b)))
          val content = facet.fold(shared)(Facet.restrict(shared, _))
          if (content.isEmpty) {
            val shared = support.iterator.map(i => currentSettings(i)).reduce((a, b) => a.intersect(b))
            throw new BleepException.Text(
              s"Template inference: empty intersection for ${support.toList.map(rows).mkString(", ")}, which share settings ${shared.mkString(", ")}"
            )
          }

          counter += 1
          val templateId = model.TemplateId(s"$idPrefix$counter")

          // projects we don't infer from still get the template if they have all of it
          val appliesTo: Vector[Int] =
            rows.indices.filter(i => support(i) || (!inferFrom(i) && content.removeAll(current(i)).isEmpty)).toVector

          appliesTo.foreach { i =>
            val shortened = current(i).removeAll(content)
            current(i) = shortened.copy(`extends` = shortened.`extends` + templateId)
            currentSettings(i) = settings(current(i))
          }

          logger.appliedTemplateTo(templateId, appliesTo.map(rows).collect { case Right(name) => name })
          val templateRow = addRow(Left(templateId), content, infer = true)
          templateIndex = templateIndex.updated(templateId, templateRow)
          // the projects which reach the new template: those it was applied to, and those reaching a template it was applied to
          reachers = reachers.updated(
            templateRow,
            appliesTo.foldLeft(BitSet.empty) { (acc, i) =>
              rows(i) match {
                case Right(_) => acc + i
                case Left(_)  => acc ++ reachers(i)
              }
            }
          )
      }
    }

    Result(
      projects = rows.indices.flatMap { i =>
        rows(i) match {
          case Right(name) => Some((name, current(i)))
          case Left(_)     => None
        }
      }.toMap,
      templates = rows.indices.flatMap { i =>
        rows(i) match {
          case Right(_) => None
          case Left(id) => Some((id, current(i)))
        }
      }.toMap
    )
  }
}
