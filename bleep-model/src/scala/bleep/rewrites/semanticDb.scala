package bleep
package rewrites

/** Adds SemanticDB support to a build
  *
  * @param semanticDbVersion
  *   the version for `org.scalameta:::semanticdb-scalac`
  */
class semanticDb(semanticDbVersion: String = "4.9.9") extends BuildRewrite {
  override val name = model.BuildRewriteName("semanticdb")

  protected def newExplodedProjects(oldBuild: model.Build, buildPaths: BuildPaths): Map[model.CrossProjectName, model.Project] =
    oldBuild.explodedProjects.map { case (name, p) => (name, apply(name, p, buildPaths)) }

  def apply(name: model.CrossProjectName, explodedProject: model.Project, buildPaths: BuildPaths): model.Project =
    explodedProject.scala match {
      case Some(s @ model.Scala(Some(version), _, _, _, _)) =>
        val projectPaths = buildPaths.project(name, explodedProject)
        val addedScalacOptions = List(
          Some(compilerOption(version)),
          targetRootOptions(version, projectPaths),
          Some(sourceRootOptions(version, buildPaths))
        ).flatten
        val compilerPlugins =
          compilerPlugin(version) match {
            case Some(compilerPlugin) => model.JsonSet(s.compilerPlugins.values + compilerPlugin)
            case None                 => s.compilerPlugins
          }

        explodedProject.copy(scala = Some(s.copy(compilerPlugins = compilerPlugins, options = model.Options(s.options.values ++ addedScalacOptions))))

      case _ => explodedProject
    }

  def compilerPlugin(version: model.VersionScala): Option[model.Dep.ScalaDependency] =
    if (version.is3) None
    else Some(model.Dep.ScalaFullVersion("org.scalameta", "semanticdb-scalac", semanticDbVersion))

  def compilerOption(version: model.VersionScala): model.Options.Opt = {
    val sv = version.scalaVersion
    model.Options.Opt.Flag {
      if (sv.startsWith("0.") || sv.startsWith("3.0.0-M1") || sv.startsWith("3.0.0-M2")) "-Ysemanticdb"
      else if (sv.startsWith("3.")) "-Xsemanticdb"
      else "-Yrangepos"
    }
  }

  def targetRootOptions(version: model.VersionScala, projectPaths: ProjectPaths): Option[model.Options.Opt] =
    if (version.is3) {
      Some(model.Options.Opt.WithArgs("-semanticdb-target", List(projectPaths.classes.toString)))
    } else
      Some(model.Options.Opt.Flag(s"-P:semanticdb:targetroot:${projectPaths.classes}"))

  def sourceRootOptions(version: model.VersionScala, buildPaths: BuildPaths): model.Options.Opt =
    if (version.is3)
      model.Options.Opt.WithArgs(s"-sourceroot", List(buildPaths.buildDir.toString))
    else
      model.Options.Opt.Flag(s"-P:semanticdb:sourceroot:${buildPaths.buildDir}")
}
