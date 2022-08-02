package bleep
package rewrites

case class semanticDb(buildPaths: BuildPaths) extends Rewrite {
  override val name = "semanticdb"

  override def apply(explodedBuild: model.ExplodedBuild): model.ExplodedBuild =
    explodedBuild.copy(projects = explodedBuild.projects.map { case (name, p) => (name, apply(name, p)) })

  def apply(name: model.CrossProjectName, explodedProject: model.Project): model.Project =
    explodedProject.scala match {
      case Some(s @ model.Scala(Some(version), _, _, _, _)) =>
        val projectPaths = buildPaths.project(name, explodedProject)
        val addedScalacOptions = List(Some(compilerOption(version)), targetRootOptions(version, projectPaths), sourceRootOptions(version)).flatten
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
    else Some(model.Dep.ScalaFullVersion("org.scalameta", "semanticdb-scalac", "4.5.0"))

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
      // this is copied from sbt, but wrong. semanticdbTarget ends up as empty string,
      // and the .semanticdb files ends up in the source folders.
      // the right thing seems to happen without the flag set at all, so that's where we'll leave it.
      // Some(Options.Opt.WithArgs("-semanticdb-target", List(projectPaths.classes.toString)))
      None
    } else
      Some(model.Options.Opt.Flag(s"-P:semanticdb:targetroot:${projectPaths.classes}"))

  def sourceRootOptions(version: model.VersionScala): Option[model.Options.Opt] =
    if (version.is3) { None }
    else Some(model.Options.Opt.Flag(s"-P:semanticdb:sourceroot:${buildPaths.buildDir}"))
}
