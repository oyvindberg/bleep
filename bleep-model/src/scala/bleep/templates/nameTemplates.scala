package bleep
package templates

import scala.collection.immutable.{SortedMap, SortedSet}
import scala.collection.mutable

/** Gives inferred templates names a reader can make sense of, by who uses them and what they hold.
  *
  *   - a template used by exactly a group of projects the build has a name for is named after the group: `common`, `common-test`, `scala-2.13`, `scala-3-js`,
  *     or a module and its tests, `doobie-hikari`
  *   - any other template is about one thing (see [[Facet]]), and named after the smallest group which has all its users and what it holds:
  *     `common-main-kind-projector`, `common-test-cats-effect-testkit`, `common-main-publish`
  *   - a cross setup is named after its cross ids: `cross-all`, `cross-jvm-212-213`
  */
object nameTemplates {
  private type Row = (model.ProjectName, Option[model.CrossId])

  def apply(buildFile: model.BuildFile, groups: Groups, explodedProjects: Map[model.CrossProjectName, model.Project]): model.BuildFile = {
    val templates = buildFile.templates.value
    val allCrossIds: SortedSet[model.CrossId] = SortedSet.empty[model.CrossId] ++ explodedProjects.keys.flatMap(_.crossId)

    def templatesReachedFrom(row: Row): Set[model.TemplateId] = {
      val (name, crossId) = row
      def extendsIn(p: model.Project): Iterable[model.TemplateId] =
        p.`extends`.values ++ crossId.flatMap(p.cross.value.get).toList.flatMap(_.`extends`.values)
      val seen = mutable.Set.empty[model.TemplateId]
      def go(id: model.TemplateId): Unit =
        if (seen.add(id)) extendsIn(templates(id)).foreach(go)
      extendsIn(buildFile.projects.value(name)).foreach(go)
      seen.toSet
    }

    val usersOf: Map[model.TemplateId, Set[model.CrossProjectName]] =
      explodedProjects.keys.toList
        .flatMap(cn => templatesReachedFrom((cn.name, cn.crossId)).map(id => (id, cn)))
        .groupMap(_._1)(_._2)
        .map { case (id, users) => (id, users.toSet) }

    // a setup holds cross entries, and may extend other setups
    def isCrossSetup(p: model.Project): Boolean =
      p.cross.value.nonEmpty && p.copy(cross = model.JsonMap.empty, `extends` = model.JsonSet.empty).isEmpty

    def setupCrossIds(p: model.Project): Set[model.CrossId] =
      p.cross.value.keySet ++ p.`extends`.values.flatMap(id => setupCrossIds(templates(id)))

    val taken = mutable.Set.empty[String]
    def pick(candidates: Iterator[String]): String = {
      val name = candidates.find(n => !taken(n)).get
      taken += name
      name
    }
    def numbered(base: String): Iterator[String] = Iterator.from(2).map(n => s"$base-$n")

    val (setupIds, contentIds) = templates.keys.toList.partition(id => isCrossSetup(templates(id)))
    // the most used templates pick names first, so they get the short ones
    def byUse(ids: List[model.TemplateId]): List[model.TemplateId] = ids.sortBy(id => (-usersOf.getOrElse(id, Set.empty).size, id.value))

    val contentNames: Map[model.TemplateId, String] =
      byUse(contentIds).map { id =>
        val users = usersOf.getOrElse(id, Set.empty)
        val name = groups.exactly(users) match {
          case Some(group)                            => pick(Iterator(group.name) ++ numbered(group.name))
          case None if groups.nearly(users).isDefined =>
            val group = groups.nearly(users).get
            pick(Iterator(group.name) ++ hints(templates(id)).iterator.map(h => s"${group.name}-$h") ++ numbered(group.name))
          case None =>
            groups.moduleWithTrait(users) match {
              case Some(name) => pick(Iterator(name) ++ numbered(name))
              case None       =>
                val context = groups.smallestContaining(users).name
                val hs = hints(templates(id))
                // two hints together say more than a number
                val pairs = hs.iterator.flatMap(a => hs.iterator.filter(_ != a).map(b => s"$a-$b"))
                pick(hs.iterator.map(h => s"$context-$h") ++ pairs.map(h => s"$context-$h") ++ numbered(context))
            }
        }
        (id, name)
      }.toMap

    val setupNames: Map[model.TemplateId, String] =
      byUse(setupIds).map { id =>
        val setup = templates(id)
        val base = crossName(setupCrossIds(setup), allCrossIds)
        // two setups for the same cross ids differ in their templates, so the one which is not the most common says which
        val distinguishing = setup.cross.value.values.flatMap(_.`extends`.values).map(contentNames).toList.distinct.sorted.map(n => s"$base-$n")
        (id, pick(Iterator(base) ++ distinguishing.iterator ++ numbered(base)))
      }.toMap

    val newNames: Map[model.TemplateId, model.TemplateId] =
      (contentNames ++ setupNames).map { case (id, name) => (id, model.TemplateId(s"template-$name")) }

    def rename(p: model.Project): model.Project =
      p.copy(
        `extends` = model.JsonSet(p.`extends`.values.map(newNames)),
        cross = model.JsonMap(p.cross.value.map { case (crossId, cp) => (crossId, rename(cp)) })
      )

    buildFile.copy(
      templates = model.JsonMap(templates.map { case (id, p) => (newNames(id), rename(p)) }),
      projects = model.JsonMap(buildFile.projects.value.map { case (name, p) => (name, rename(p)) })
    )
  }

  /** What a template is for, best first, from what it holds. A template for a group the build has no name for holds settings about one [[Facet]] */
  private def hints(p: model.Project): List[String] = {
    val plugins = p.scala.toList.flatMap(_.compilerPlugins.values).map(_.baseModuleName.value).sorted
    // one or two libraries are named, more are a set of dependencies no single one of them speaks for. if that name is taken, one of them will have to
    val (deps, depsLast) = {
      val names = (p.dependencies.values ++ p.java.toList.flatMap(_.annotationProcessors.values)).map(_.baseModuleName.value).toList.sorted
      if (names.size > 2) (List("dependencies"), names) else (names, Nil)
    }
    // a template of only BOMs is about them all, not the first
    val boms = if (p.boms.values.nonEmpty && p.dependencies.values.isEmpty) List("boms") else Nil

    val options: List[String] =
      (p.scala.toList.flatMap(_.options.values) ++ p.java.toList.flatMap(_.options.values) ++ p.platform.toList.flatMap(_.jvmOptions.values))
        .map(_.render.head)
        .sorted
    // a compiler plugin given as an option says most: `-Xplugin:ErrorProne`
    val pluginOptions = options.collect { case opt if opt.startsWith("-Xplugin:") && !opt.contains("/") => opt.stripPrefix("-Xplugin:") }
    val optionHints = pluginOptions ++ options.map { opt =>
      opt.dropWhile(_ == '-') match {
        // system properties: `-Duser.language=en`
        case property if property.startsWith("D") && property.contains("=") => property.drop(1).takeWhile(_ != '=')
        // compiler plugin options: `-P:scalanative:...`
        case pluginOpt if pluginOpt.startsWith("P:") => pluginOpt.drop(2).takeWhile(_ != ':')
        case other                                   => other.takeWhile(c => c != ':' && c != '=')
      }
    }

    val byFacet: List[String] = Facet.of(p).toList.sortBy(Facet.All.indexOf).map {
      case Facet.Compiler     => "compiler"
      case Facet.Platform     => "platform"
      case Facet.Dependencies => "dependencies"
      case Facet.Layout       => "sources"
      case Facet.Testing      => "testing"
      case Facet.Publishing   => "publish"
    }

    // the values which tell layouts apart: `cross-pure`, and the directory in a source path which is not one every project has: `scala-2.13-plus`, or
    // `shared-with-js` for `js-${PLATFORM}`, the sources a jvm project shares with js. resources are often single files, `LICENSE.md`, which say little
    val layoutHints: List[String] = {
      val common = Set(".", "..", "src", "${SCOPE}", "main", "test", "scala", "java", "kotlin", "resources")
      p.`source-layout`.map(_.id).toList ++
        p.sources.values.toList
          .flatMap(_.segments.toList.filterNot(common))
          .map {
            case "${PLATFORM}"                            => "platform"
            case shared if shared.contains("${PLATFORM}") => s"shared-with-${shared.replace("${PLATFORM}", "").stripPrefix("-").stripSuffix("-")}"
            case segment                                  => segment.replace("+", "-plus")
          }
    }

    // what a setting is called, for what it says: `strict`, `jvmOptions`, `sources`. not the ones every template has
    val generic = Set("name", "version", "options", "order", "module", "configuration")
    // publishing coordinates say nothing one by one (`url`, `developers`, a license's `distribution`): a template of them is `publish`
    val settingHints = mineTemplates
      .settings(p)
      .toList
      .filterNot(setting => Facet.of(setting.path) == Facet.Publishing)
      .map(_.path.split('.').last)
      .filterNot(generic)
      .distinct
      .sorted

    (plugins ++ deps ++ boms ++ optionHints ++ layoutHints ++ settingHints ++ byFacet ++ depsLast).map(sanitize).filter(_.length >= 2).distinct
  }

  private def sanitize(s: String): String =
    s.toLowerCase.map(c => if (c.isLetterOrDigit || c == '.' || c == '-') c else '-').replaceAll("-+", "-").stripPrefix("-").stripSuffix("-")

  /** `cross-all` when a template sets up every cross id in the build, otherwise the cross ids grouped by platform: `cross-jvm-212-213`, `cross-js-all`. Cross
    * ids for full scala versions (`jvm2.13.16`, for a compiler plugin) are a family of their own: `cross-jvm-full-all`
    */
  def crossName(crossIds: Set[model.CrossId], all: SortedSet[model.CrossId]): String = {
    val sorted = SortedSet.empty[model.CrossId] ++ crossIds
    val baseName =
      if (sorted == all) "all"
      else {
        def platformOf(id: model.CrossId) = {
          val platform = id.value.takeWhile(_.isLetter)
          if (id.value.drop(platform.length).contains('.')) s"$platform-full" else platform
        }
        val allByPlatform: Map[String, SortedSet[model.CrossId]] = all.groupBy(platformOf)
        SortedMap
          .from(sorted.groupBy(platformOf))
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
}
