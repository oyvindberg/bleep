package bleep
package templates

import scala.collection.immutable.SortedSet

/** Groups of cross projects a build already has names for, which a reader recognises without being told who is in them:
  *   - by traits: tests or main code, a scala version, a platform, and their combinations (`common-test`, `scala-2.13`, `scala-3-js`). A trait all projects
  *     share tells nothing apart, so it is left out of names
  *   - by module: a project and its tests (`doobie-hikari` for `doobie-hikari` and `doobie-hikari-test`)
  *   - a few groups of one kind together: `scala-2.13-and-3`, `js-and-native`
  *
  * Template inference offers every group as a template, and a template covering exactly the projects of a group is named after it.
  */
final class Groups(rows: Map[model.CrossProjectName, model.Project]) {
  import Groups.Group

  private def scope(p: model.Project): String = if (p.isTestProject.contains(true)) "test" else "main"

  /** The ways a row can be described by scala version: `scala-2.13.13` where the build uses several versions of one binary version, `scala-2.13`, and `scala-2`
    * where the build has several binary versions of scala 2
    */
  private val scalaLabels: model.Project => List[String] = {
    val versions = rows.values.flatMap(_.scala.flatMap(_.version)).toSet
    p =>
      p.scala.flatMap(_.version).toList.flatMap { v =>
        val full = if (versions.count(_.binVersion == v.binVersion) > 1) List(s"scala-${v.scalaVersion}") else Nil
        val epoch = if (versions.map(_.binVersion).count(_.startsWith(s"${v.epoch}.")) > 1) List(s"scala-${v.epoch}") else Nil
        full ++ List(s"scala-${v.binVersion}") ++ epoch
      }
  }

  private def platform(p: model.Project): Option[String] = p.platform.flatMap(_.name).map(_.value)

  /** `java-17`, from `--release`, `-target` or `-source`. `1.8` is java 8 */
  private def java(p: model.Project): Option[String] = {
    val rendered = p.java.toList.flatMap(_.options.values.toList.map(_.render))
    List("--release", "-target", "-source").iterator
      .flatMap(flag => rendered.collectFirst { case List(`flag`, version) => version })
      .nextOption()
      .map(version => s"java-${version.stripPrefix("1.")}")
  }

  private val byTraits: List[Group] = {
    // a dimension on which all rows agree tells nothing apart
    def varies[T](f: model.Project => T): Boolean = rows.values.map(f).toSet.size > 1

    val scopes: List[Option[String]] = None :: (if (varies(scope)) List(Some("main"), Some("test")) else Nil)
    val scalas: List[Option[String]] =
      None :: (if (varies(p => p.scala.flatMap(_.version))) rows.values.flatMap(scalaLabels).toList.distinct.sorted.map(Some.apply) else Nil)
    val platforms: List[Option[String]] = None :: (if (varies(platform)) rows.values.flatMap(platform).toList.distinct.sorted.map(Some.apply) else Nil)
    val javas: List[Option[String]] = None :: (if (varies(java)) rows.values.flatMap(java).toList.distinct.sorted.map(Some.apply) else Nil)

    for {
      sc <- scalas
      pl <- platforms
      jv <- javas
      sp <- scopes
      members = rows.collect {
        case (name, p) if sc.forall(scalaLabels(p).contains) && pl.forall(platform(p).contains) && jv.forall(java(p).contains) && sp.forall(_ == scope(p)) =>
          name
      }.toSet
      if members.nonEmpty
    } yield {
      val base = sc.toList ++ pl.toList ++ jv.toList
      Group((if (base.isEmpty) List("common") else base) ++ sp.toList mkString "-", members, isModule = false)
    }
  }

  private val byModule: List[Group] = {
    def moduleOf(name: model.ProjectName): String = name.value.stripSuffix("-test").stripSuffix("-it")
    val traitNames = byTraits.map(_.name).toSet
    rows.keys
      .groupBy(cn => moduleOf(cn.name))
      .toList
      .collect {
        // a module called like a group of traits (scalameta's `common`) is told apart
        case (module, members) if members.map(_.name).toSet.size > 1 =>
          Group(if (traitNames(module)) s"module-$module" else module, members.toSet, isModule = true)
      }
  }

  /** A module restricted to a scala version or platform: `doobie-hikari-scala-2.13`. Only used to name templates, not offered as templates */
  def moduleWithTrait(members: Set[model.CrossProjectName]): Option[String] =
    byModule.iterator
      .flatMap(module => byTraits.iterator.filterNot(_.name.startsWith("common")).map(t => (module, t)))
      .collectFirst { case (module, t) if (module.members intersect t.members) == members => s"${module.name}-${t.name}" }

  /** A few groups of one kind together, which read as one: `scala-2.13-and-3`, `js-and-native`. Code for scala 2.13 and 3 is as much a thing a build has as
    * code for scala 3, so these are offered as templates like any other group. Where several make up the same projects the shortest name is kept, `scala-2`
    * rather than `scala-2.12-and-2.13`
    */
  private val byUnion: List[Group] = {
    val platformNames: Set[String] = rows.values.flatMap(platform).toSet
    // the groups of one dimension
    val dimensions: List[List[Group]] = List(
      byTraits.filter(g =>
        g.name.startsWith("scala-") && !g.name.endsWith("-main") && !g.name.endsWith("-test") && platformNames.forall(p => !g.name.endsWith(s"-$p"))
      ),
      byTraits.filter(g => platformNames.contains(g.name))
    )
    dimensions.flatMap { candidates =>
      (2 to 3).iterator
        .flatMap(candidates.combinations)
        // groups which do not overlap
        .filter(chosen => chosen.map(_.members.size).sum == chosen.flatMap(_.members).toSet.size)
        .map { chosen =>
          val names = chosen.map(_.name).sorted
          val prefix = if (names.forall(_.startsWith("scala-"))) "scala-" else ""
          Group(prefix + names.map(_.stripPrefix(prefix)).mkString("-and-"), chosen.flatMap(_.members).toSet, isModule = false)
        }
    }
  }

  /** Every group, fewest members first. Where two groups have the same members, a group by traits is kept over a module (`common` rather than `myapp` in a
    * build of an app and its tests), and otherwise the shorter name
    */
  val all: List[Group] =
    (byTraits ++ byUnion ++ byModule)
      .groupBy(_.members)
      .values
      .map(_.minBy(g => (g.isModule, g.name.length, g.name)))
      .toList
      .sortBy(g => (g.members.size, g.name))

  private val byMembers: Map[Set[model.CrossProjectName], Group] = all.map(g => (g.members, g)).toMap

  /** The group with exactly these members */
  def exactly(members: Set[model.CrossProjectName]): Option[Group] = byMembers.get(members)

  /** A sizeable group of which these are all members but a few outliers (a tenth at most). A template for them is really the template for the group, which the
    * outliers do not use: `publish.groupId` on all main projects but one
    */
  def nearly(members: Set[model.CrossProjectName]): Option[Group] =
    all.find(g => g.members.size >= 10 && members.subsetOf(g.members) && g.members.size - members.size <= g.members.size / 10)

  /** The smallest group which has all of these members. There always is one, `common` has every row */
  def smallestContaining(members: Set[model.CrossProjectName]): Group =
    all.find(g => members.subsetOf(g.members)).getOrElse(Group("common", rows.keySet, isModule = false))

  def names: SortedSet[String] = SortedSet.from(all.map(_.name))
}

object Groups {

  /** @param isModule
    *   a module and its tests, rather than a group by traits
    */
  case class Group(name: String, members: Set[model.CrossProjectName], isModule: Boolean)
}
