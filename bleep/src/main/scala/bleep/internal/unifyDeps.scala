package bleep
package internal

/** The `Dep.ScalaDependency` structure has three fields we can only correctly determine in context of a given scala version. We need to propagate those three
  * flags up to all projects with same scala version or platform. After that, the "combine by cross" functionality will work better
  */
object unifyDeps {
  def findReplacements(allDeps: Iterable[Dep]): Map[Dep, Dep] = {
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

  def apply(build: ExplodedBuild): ExplodedBuild = {
    val replacements: Map[Dep, Dep] =
      findReplacements(build.projects.flatMap(_._2.dependencies.values))

    build.copy(projects = build.projects.map { case (crossName, p) =>
      val newP = p.copy(dependencies = p.dependencies.map(replacements.apply))
      (crossName, newP)
    })
  }
}
