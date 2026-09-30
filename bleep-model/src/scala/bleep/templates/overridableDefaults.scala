package bleep
package templates

/** Settings with one value, which a project may give a value of its own instead of the template's, since the project's own value always wins. Only settings
  * which set a default, not ones which say what a project is (a scala version, a platform): overriding those would make a template misleading.
  *
  * A default nearly all projects of a group share belongs in the group's template, and the few which differ keep their own value: `publish.groupId` of all main
  * modules but the experimental one. [[assume]] lets inference see the group as sharing the default, and [[restore]] gives the projects which differ their own
  * value back, afterwards.
  */
object overridableDefaults {

  /** Each picks one default out of a project, as a project holding only that */
  private val fragments: List[model.Project => model.Project] = {
    val e = model.Project.empty
    val nothing = model.PublishConfig(None, None, None, None, None, model.JsonSet.empty, model.JsonSet.empty, None, None)
    def publish(f: model.PublishConfig => model.PublishConfig): model.Project => model.Project =
      p => e.copy(publish = p.publish.map(f).filterNot(_.isEmpty))
    List(
      publish(pc => nothing.copy(groupId = pc.groupId)),
      publish(pc => nothing.copy(organization = pc.organization)),
      publish(pc => nothing.copy(url = pc.url)),
      publish(pc => nothing.copy(description = pc.description)),
      p => e.copy(`source-layout` = p.`source-layout`),
      p => e.copy(ignoreEvictionErrors = p.ignoreEvictionErrors),
      p =>
        e.copy(scala =
          p.scala.flatMap(sc => sc.strict.map(strict => model.Scala(None, model.Options.empty, None, model.JsonSet.empty, Some(strict), None, None, None)))
        )
    )
  }

  /** For every group, largest first: where at least four in five share a default and every other has a value of its own, they are all given the shared one. The
    * others are exceptions then, and read as such
    */
  def assume(rows: Map[model.CrossProjectName, model.Project], groups: Groups): Map[model.CrossProjectName, model.Project] = {
    var result = rows
    groups.all.sortBy(g => (-g.members.size, g.name)).foreach { group =>
      fragments.foreach { fragment =>
        val values = group.members.toList.map(row => (row, fragment(result(row))))
        // a project without a value of its own would get the template's
        if (values.nonEmpty && values.forall { case (_, v) => !v.isEmpty }) {
          val (majority, count) = values.groupMapReduce(_._2)(_ => 1)(_ + _).maxBy { case (v, n) => (n, v.toString) }
          if (count < values.size && count * 5 >= values.size * 4)
            values.foreach { case (row, v) => if (v != majority) result = result.updated(row, majority.union(result(row).removeAll(v))) }
        }
      }
    }
    result
  }

  /** Gives every cross project whose default differs from the one it now gets from its templates its own value back. A project which is cross built gets it on
    * the cross project, since a template the cross project extends would win over the project's own value
    */
  def restore(buildFile: model.BuildFile, original: Map[model.CrossProjectName, model.Project]): model.BuildFile = {
    val exploded = model.Build.FileBacked(buildFile).explodedProjects
    val projects = buildFile.projects.value.map { case (name, project) =>
      val restored = original.keys.filter(_.name == name).foldLeft(project) { (p, row) =>
        fragments.foldLeft(p) { (acc, fragment) =>
          val own = fragment(original(row))
          // [[assume]] only changed rows which had a value of their own
          if (own.isEmpty || fragment(exploded(row)) == own) acc
          else
            row.crossId match {
              case None          => own.union(acc)
              case Some(crossId) =>
                acc.copy(cross = model.JsonMap(acc.cross.value.updated(crossId, own.union(acc.cross.value.getOrElse(crossId, model.Project.empty)))))
            }
        }
      }
      (name, restored)
    }
    buildFile.copy(projects = model.JsonMap(projects))
  }
}
