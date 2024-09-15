package bleep
package rewrites

/** The `Dep.ScalaDependency` structure has three fields we can only correctly determine in context of a given scala version. We need to propagate those three
  * flags up to all projects with same scala version or platform. After that, the "combine by cross" functionality will work better
  */
object unifyDeps extends BuildRewrite {
  override val name = model.BuildRewriteName("unify-deps")

  def findReplacements(allDeps: Iterable[model.Dep]): Map[model.Dep, model.Dep] = {
    val javaDeps = allDeps.collect { case x: model.Dep.JavaDependency => x }
    val scalaDeps = allDeps.collect { case x: model.Dep.ScalaDependency => x }
    val rewrittenScalaDeps: Map[model.Dep, model.Dep] =
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

  val OnlyJvm = Set(model.PlatformId.Jvm)

  protected def newExplodedProjects(oldBuild: model.Build, buildPaths: BuildPaths): Map[model.CrossProjectName, model.Project] = {
    val replacements: Map[model.Dep, model.Dep] =
      findReplacements(oldBuild.explodedProjects.flatMap(_._2.dependencies.values))

    val projectPlatforms: Map[model.ProjectName, Set[model.PlatformId]] =
      oldBuild.explodedProjectsByName.map { case (name, crossProjects) => (name, crossProjects.values.flatMap(_.platform.flatMap(_.name)).toSet) }

    val newProjects = oldBuild.explodedProjects.map { case (crossName, p) =>
      val newDependencies = p.dependencies.map { dep0 =>
        val dep0ForceJvm = dep0.mapScala(_.copy(forceJvm = true))

        val dep1 = replacements(dep0)

        // don't litter forceJvm = true needlessly
        if (dep1 == dep0ForceJvm && projectPlatforms(crossName.name) == OnlyJvm) dep0
        else dep1
      }
      (crossName, p.copy(dependencies = newDependencies))
    }

    newProjects
  }
}
