package bleep
package rewrites

/** The `Dep.ScalaDependency` structure has three flags whose effect depends on the project: `forceJvm` only does something on Scala.js and Scala Native
  * projects, `for3Use213` only on Scala 3 projects and `for213Use3` only on Scala 2.13 projects. Where a flag does nothing, it may as well be set the same way
  * everywhere in the build, so cross projects agree on their dependencies and the "combine by cross" functionality can lift them.
  *
  * A flag is only ever copied onto a project where it does nothing. Copying `forceJvm` onto a Scala.js project would replace the Scala.js artifact with the JVM
  * one, just because some other project in the build asked for the JVM artifact.
  */
object unifyDeps extends BuildRewrite {
  override val name = model.BuildRewriteName("unify-deps")

  /** For every scala dependency in the build, the flags set on it anywhere */
  case class Flags(forceJvm: Boolean, for3Use213: Boolean, for213Use3: Boolean)

  def findFlags(allDeps: Iterable[model.Dep]): Map[model.Dep.ScalaDependency, Flags] =
    allDeps
      .collect { case x: model.Dep.ScalaDependency => x }
      .groupBy(withoutFlags)
      .map { case (base, deps) =>
        (base, Flags(forceJvm = deps.exists(_.forceJvm), for3Use213 = deps.exists(_.for3Use213), for213Use3 = deps.exists(_.for213Use3)))
      }

  private def withoutFlags(dep: model.Dep.ScalaDependency): model.Dep.ScalaDependency =
    dep.copy(forceJvm = false, for3Use213 = false, for213Use3 = false)

  val OnlyJvm = Set(model.PlatformId.Jvm)

  protected def newExplodedProjects(oldBuild: model.Build, buildPaths: BuildPaths): Map[model.CrossProjectName, model.Project] = {
    val flags: Map[model.Dep.ScalaDependency, Flags] =
      findFlags(oldBuild.explodedProjects.flatMap(_._2.dependencies.values))

    val projectPlatforms: Map[model.ProjectName, Set[model.PlatformId]] =
      oldBuild.explodedProjectsByName.map { case (name, crossProjects) => (name, crossProjects.values.flatMap(_.platform.flatMap(_.name)).toSet) }

    oldBuild.explodedProjects.map { case (crossName, p) =>
      val isJvm = p.platform.flatMap(_.name).forall(_ == model.PlatformId.Jvm)
      val scalaVersion = p.scala.flatMap(_.version)
      val is3 = scalaVersion.exists(_.is3)
      val is213 = scalaVersion.exists(_.is213)
      // cross projects of a jvm-only project would all get the flag, and it does nothing there. don't litter the build file with it
      val onlyJvm = projectPlatforms(crossName.name) == OnlyJvm

      val newDependencies = p.dependencies.map {
        case dep: model.Dep.ScalaDependency =>
          val everywhere = flags(withoutFlags(dep))
          dep.copy(
            forceJvm = if (isJvm) everywhere.forceJvm && !onlyJvm else dep.forceJvm,
            for3Use213 = if (is3) dep.for3Use213 else everywhere.for3Use213,
            for213Use3 = if (is213) dep.for213Use3 else everywhere.for213Use3
          )
        case dep: model.Dep.JavaDependency => dep
      }
      (crossName, p.copy(dependencies = newDependencies))
    }
  }
}
