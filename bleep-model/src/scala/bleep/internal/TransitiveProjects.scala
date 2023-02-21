package bleep
package internal

// precompute all transitive deps for a set of projects
// note that this transitive set includes source generator projects
case class TransitiveProjects(deps: Array[TransitiveProjects.Deps]) {
  def direct: Array[model.CrossProjectName] =
    deps.map(_.projectName)

  def all: Array[model.CrossProjectName] = {
    val builder = Array.newBuilder[model.CrossProjectName]
    deps.foreach { case TransitiveProjects.Deps(projectName, transitiveDeps) =>
      builder += projectName
      builder ++= transitiveDeps
    }
    builder.result().distinct
  }

  def transitiveFilter(pred: model.CrossProjectName => Boolean): TransitiveProjects =
    new TransitiveProjects(
      deps.filter { case TransitiveProjects.Deps(p, transitiveDeps) => pred(p) || transitiveDeps.exists(pred) }
    )
}

object TransitiveProjects {
  case class Deps(projectName: model.CrossProjectName, deps: Array[model.CrossProjectName])

  def all(build: model.Build): TransitiveProjects =
    apply(build, build.explodedProjects.keys.toArray)

  def apply(build: model.Build, direct: Array[model.CrossProjectName]): TransitiveProjects = {
    val withTransitive = direct.distinct.map { p =>
      val deps = Array.newBuilder[model.CrossProjectName]

      def recurse(depName: model.CrossProjectName): Unit = {
        deps += depName
        build.resolvedDependsOn(depName).foreach(recurse)
        build.explodedProjects(depName).sourcegen.values.foreach { case model.ScriptDef.Main(project, _, _) => recurse(project) }
      }

      recurse(p)
      Deps(p, deps.result().distinct)
    }

    new TransitiveProjects(withTransitive)
  }
}
