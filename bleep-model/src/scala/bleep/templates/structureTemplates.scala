package bleep
package templates

/** Gives the templates mining found a structure a reader can follow.
  *
  * Mining only decides which settings live in which template. What a project ends up with depends on nothing but the set of templates each of its cross
  * projects reaches, so the structure can be chosen freely as long as every cross project reaches the same set. It is chosen to follow generality:
  *   - templates with the same users are one template, as long as that stays about one thing (see [[Facet]])
  *   - a template extends another exactly when its users are some of the other's users, and nothing lies in between. `template-common-test` extends
  *     `template-common`, `template-scala-2.13-strict` extends `template-scala-2.13`
  *   - a project extends only the most specific templates it uses. What all its cross projects use is extended by the project, the rest by the cross project
  *     which uses it
  *   - how projects are cross built becomes a cross setup template, which says for which cross ids a project is built and with which templates: `cross-all` is
  *     `jvm212: template-scala-2.12, jvm213: template-scala-2.13, jvm3: template-scala-3`. A project whose cross projects need more extends the setup and adds
  *     what it needs
  */
object structureTemplates {
  case class Result(projects: Map[model.ProjectName, model.Project], templates: Map[model.TemplateId, model.Project])

  def apply(
      exploded: Map[model.CrossProjectName, model.Project],
      mined: mineTemplates.Result[model.CrossProjectName],
      groups: Groups,
      costs: mineTemplates.Costs
  ): Result = {
    def closure(start: Set[model.TemplateId]): Set[model.TemplateId] = {
      val seen = scala.collection.mutable.Set.empty[model.TemplateId]
      def go(id: model.TemplateId): Unit = if (seen.add(id)) mined.templates(id).`extends`.values.foreach(go)
      start.foreach(go)
      seen.toSet
    }

    val reach: Map[model.CrossProjectName, Set[model.TemplateId]] =
      mined.projects.map { case (cn, p) => (cn, closure(p.`extends`.values.toSet)) }

    val minedUsers: Map[model.TemplateId, Set[model.CrossProjectName]] =
      reach.toList.flatMap { case (cn, ts) => ts.map(t => (t, cn)) }.groupMap(_._1)(_._2).map { case (t, users) => (t, users.toSet) }

    // one template per group of users, unless that bundles settings about different things for a group the build has no name for
    val merged: List[(model.TemplateId, model.Project, Set[model.CrossProjectName], List[model.TemplateId])] =
      minedUsers.toList.groupBy(_._2).toList.flatMap { case (users, templates) =>
        val ids = templates.map(_._1).sortBy(_.value)
        val clusters: List[List[model.TemplateId]] =
          if (groups.exactly(users).orElse(groups.nearly(users)).isDefined) List(ids)
          else ids.groupBy(id => Facet.of(mined.templates(id))).values.toList.map(_.sortBy(_.value))
        clusters.map { cluster =>
          val content = cluster.map(id => mined.templates(id).copy(`extends` = model.JsonSet.empty)).reduce((a, b) => a.union(b))
          (cluster.head, content, users, cluster)
        }
      }

    val renamed: Map[model.TemplateId, model.TemplateId] = merged.flatMap { case (id, _, _, olds) => olds.map(old => (old, id)) }.toMap
    val users: Map[model.TemplateId, Set[model.CrossProjectName]] = merged.map { case (id, _, us, _) => (id, us) }.toMap
    val content: Map[model.TemplateId, model.Project] = merged.map { case (id, c, _, _) => (id, c) }.toMap

    def strictlyWithin(a: model.TemplateId, b: model.TemplateId): Boolean = users(a).subsetOf(users(b)) && users(a) != users(b)

    // a template extends the templates right above it: they have all its users and more, and nothing lies in between
    val parents: Map[model.TemplateId, Set[model.TemplateId]] =
      users.keys.map { t =>
        val above = users.keySet.filter(u => strictlyWithin(t, u))
        (t, above.filterNot(u => above.exists(v => v != u && strictlyWithin(v, u))))
      }.toMap

    def ancestors(ts: Set[model.TemplateId]): Set[model.TemplateId] = {
      val seen = scala.collection.mutable.Set.empty[model.TemplateId]
      def go(t: model.TemplateId): Unit = parents(t).foreach(p => if (seen.add(p)) go(p))
      ts.foreach(go)
      seen.toSet
    }

    def mostSpecific(ts: Set[model.TemplateId]): Set[model.TemplateId] =
      ts.filterNot(t => ts.exists(u => u != t && strictlyWithin(u, t)))

    val mostSpecificOf: Map[model.CrossProjectName, Set[model.TemplateId]] =
      reach.map { case (cn, ts) => (cn, mostSpecific(ts.map(renamed))) }

    // what all cross projects of a project use moves up to the project, also when some reach it through a more specific template: a test project whose js3
    // cross project extends `scala-3-js-test` extends `common-test` itself, and its other cross projects need not. Only where that saves extending it on
    // several cross projects: doobie's `common-strict` is extended by its 2.13 cross projects alone, the others get it from their scala version's template, and
    // it is simpler left there than extended by every project
    val usedByAllCrossProjects: Map[model.ProjectName, Set[model.TemplateId]] =
      reach.keys
        .groupBy(_.name)
        .map { case (name, crossNames) =>
          val reachedByAll = crossNames.map(cn => reach(cn).map(renamed)).reduce(_ intersect _)
          val lifted = reachedByAll.filter(t => crossNames.count(cn => mostSpecificOf(cn).contains(t)) > 1)
          (name, mostSpecific(lifted))
        }

    val direct: Map[model.CrossProjectName, Set[model.TemplateId]] =
      reach.keys.map(cn => (cn, mostSpecificOf(cn) ++ usedByAllCrossProjects(cn.name))).toMap

    val grouped: Map[model.ProjectName, model.Project] =
      groupCrossProjects(exploded.map { case (cn, p) =>
        (cn, model.ProjectWithExploded(p, mined.projects(cn).copy(`extends` = model.JsonSet.fromIterable(direct(cn)))))
      }).map { case (name, p) =>
        val project0 = p.current
        val project = project0.copy(`extends` = model.JsonSet.fromIterable(mostSpecific(project0.`extends`.values.toSet)))
        // what the project extends already reaches, its cross projects need not extend again
        val fromTop = project.`extends`.values ++ ancestors(project.`extends`.values)
        val cross = project.cross.map { case (crossId, cp) => (crossId, cp.copy(`extends` = model.JsonSet.fromIterable(cp.`extends`.values -- fromTop))) }
        (name, project.copy(cross = cross))
      }

    val templates: Map[model.TemplateId, model.Project] =
      content.map { case (id, c) => (id, c.copy(`extends` = model.JsonSet.fromIterable(parents(id)))) }

    val (projects, crossSetups) = extractCrossSetups(grouped, costs)
    val allTemplates = templates ++ crossSetups
    Result(dropRedundantExtends(projects, allTemplates), allTemplates)
  }

  /** A project need not extend what each of its cross projects reaches anyway: doobie's projects get `common-strict` from their cross setup, so they do not
    * extend it themselves. What each cross project reaches stays the same
    */
  private def dropRedundantExtends(
      projects: Map[model.ProjectName, model.Project],
      templates: Map[model.TemplateId, model.Project]
  ): Map[model.ProjectName, model.Project] = {
    def reached(start: Iterable[model.TemplateId], crossId: Option[model.CrossId]): Set[model.TemplateId] = {
      val seen = scala.collection.mutable.Set.empty[model.TemplateId]
      def go(id: model.TemplateId): Unit =
        if (seen.add(id)) {
          val t = templates(id)
          (t.`extends`.values ++ crossId.flatMap(t.cross.value.get).toList.flatMap(_.`extends`.values)).foreach(go)
        }
      start.foreach(go)
      seen.toSet
    }

    projects.map { case (name, p) =>
      // its own cross ids, and those a cross setup gives it without an entry of its own
      val allCrossIds: Set[model.CrossId] =
        p.cross.value.keySet ++ reached(p.`extends`.values, None).flatMap(id => templates(id).cross.value.keySet)
      val crossIds: List[Option[model.CrossId]] = if (allCrossIds.isEmpty) List(None) else allCrossIds.toList.map(Some.apply)
      val kept = p.`extends`.values.toList.sortBy(_.value).foldLeft(p.`extends`.values.toSet) { (keep, t) =>
        val without = keep - t
        val reachedAnyway = crossIds.forall { crossId =>
          val own = crossId.flatMap(p.cross.value.get).toList.flatMap(_.`extends`.values)
          reached(without ++ own, crossId).contains(t)
        }
        if (reachedAnyway) without else keep
      }
      (name, p.copy(`extends` = model.JsonSet.fromIterable(kept)))
    }
  }

  /** A way of cross building, the templates for each cross id, shared by several projects becomes a cross setup template. A project may extend a setup which
    * gives it some of what it needs, and add the rest itself
    */
  private def extractCrossSetups(
      projects0: Map[model.ProjectName, model.Project],
      costs: mineTemplates.Costs
  ): (Map[model.ProjectName, model.Project], Map[model.TemplateId, model.Project]) = {
    type Setup = Map[model.CrossId, Set[model.TemplateId]]

    var projects = projects0
    var setups = Map.empty[model.TemplateId, model.Project]
    var counter = 0
    var continue = true

    def setupOf(p: model.Project): Setup =
      p.cross.value.collect { case (crossId, cp) if cp.`extends`.values.nonEmpty => (crossId, cp.`extends`.values.toSet) }

    def fits(setup: Setup, p: model.Project): Boolean =
      setup.forall { case (crossId, ts) => p.cross.value.get(crossId).exists(cp => ts.subsetOf(cp.`extends`.values.toSet)) }

    while (continue) {
      val candidates: List[Setup] = projects.values.map(setupOf).filter(_.nonEmpty).toList.distinct

      val scored = candidates.flatMap { setup =>
        val using = projects.collect { case (name, p) if fits(setup, p) => name }.toList
        val size = setup.values.map(_.size).sum
        val savings = (using.size - 1) * size - using.size * costs.referenceCost - costs.templateCost
        if (using.size < costs.minUsers || savings <= 0) None
        else Some((savings, setup, using))
      }

      scored.maxByOption { case (savings, setup, using) =>
        (savings, using.size, setup.toList.map { case (c, ts) => c.value + ts.map(_.value).mkString }.sorted.mkString)
      } match {
        case None                    => continue = false
        case Some((_, setup, using)) =>
          counter += 1
          val id = model.TemplateId(s"cross-setup-$counter")
          setups = setups.updated(
            id,
            model.Project.empty.copy(cross = model.JsonMap(setup.map { case (crossId, ts) =>
              (crossId, model.Project.empty.copy(`extends` = model.JsonSet.fromIterable(ts)))
            }))
          )
          // what all its users have for a cross id belongs to the setup too, `strict: true` for 2.13, say
          val crossIds: Set[model.CrossId] = using.map(name => projects(name).cross.value.keySet).reduce(_ intersect _)
          val shared: Map[model.CrossId, model.Project] =
            crossIds.toList.flatMap { crossId =>
              val common = using.map(name => projects(name).cross.value(crossId).copy(`extends` = model.JsonSet.empty)).reduce((a, b) => a.intersect(b))
              if (common.isEmpty) None else Some((crossId, common))
            }.toMap

          val entries: Map[model.CrossId, model.Project] =
            (setup.keySet ++ shared.keySet).toList.map { crossId =>
              val withExtends = model.Project.empty.copy(`extends` = model.JsonSet.fromIterable(setup.getOrElse(crossId, Set.empty)))
              (crossId, shared.get(crossId).fold(withExtends)(_.union(withExtends)))
            }.toMap
          setups = setups.updated(id, model.Project.empty.copy(cross = model.JsonMap(entries)))

          using.foreach { name =>
            val p = projects(name)
            val cross = p.cross.map { case (crossId, cp) =>
              val withoutExtends = cp.copy(`extends` = model.JsonSet.fromIterable(cp.`extends`.values -- setup.getOrElse(crossId, Set.empty)))
              (crossId, shared.get(crossId).fold(withoutExtends)(withoutExtends.removeAll))
            }
            projects = projects.updated(name, p.copy(`extends` = p.`extends` + id, cross = cross))
          }
      }
    }

    // a setup which has all of another's entries extends it, and holds only what it adds: `cross-js-all-jvm-all` is `cross-jvm-all` and the js cross ids. The
    // largest ones first, and none which overlap
    setups = setups.map { case (id, setup) =>
      val entries = setup.cross.value
      val within = setups.toList
        .collect {
          case (otherId, other) if otherId != id && other.cross.value.size < entries.size && other.cross.value.forall { case (c, e) =>
                entries.get(c).contains(e)
              } =>
            (otherId, other.cross.value.keySet)
        }
        .sortBy { case (otherId, crossIds) => (-crossIds.size, otherId.value) }
      val chosen = within.foldLeft(List.empty[(model.TemplateId, Set[model.CrossId])]) { case (acc, (otherId, crossIds)) =>
        if (acc.exists { case (_, taken) => (taken intersect crossIds).nonEmpty }) acc else (otherId, crossIds) :: acc
      }
      val provided = chosen.flatMap(_._2).toSet
      (id, setup.copy(`extends` = model.JsonSet.fromIterable(chosen.map(_._1)), cross = model.JsonMap(entries.filterNot { case (c, _) => provided(c) })))
    }

    // a cross id whose entry is left empty is still built: the setup the project extends has it
    val withoutEmptyEntries = projects.map { case (name, p) =>
      // the cross ids of the setups it extends, and of those they extend
      def crossIdsOf(t: model.TemplateId): Set[model.CrossId] =
        setups.get(t).toSet.flatMap((s: model.Project) => s.cross.value.keySet ++ s.`extends`.values.flatMap(crossIdsOf))
      val provided: Set[model.CrossId] = p.`extends`.values.toSet.flatMap(crossIdsOf)
      (name, p.copy(cross = model.JsonMap(p.cross.value.filterNot { case (crossId, cp) => cp.isEmpty && provided(crossId) })))
    }
    (withoutEmptyEntries, setups)
  }
}
