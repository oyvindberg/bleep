package bleep
package rewrites

object Defaults {
  // values copied from bloops `Config.CompileSetup.empty`
  val DefaultCompileSetup = model.CompileSetup(
    order = Some(model.CompileOrder.Mixed),
    addLibraryToBootClasspath = Some(true),
    addCompilerToClasspath = Some(false),
    addExtraJarsToClasspath = Some(false),
    manageBootClasspath = Some(true),
    filterLibraryFromClasspath = Some(true)
  )

  val Jvm: model.Platform =
    model.Platform(
      name = None,
      mainClass = None,
      jsVersion = None,
      jsKind = None,
      jsSplitStyle = None,
      jsEmitSourceMaps = None,
      jsJsdom = None,
      jsNodeVersion = None,
      jvmOptions = model.Options(Set(model.Options.Opt.Flag(s"-Duser.dir=${model.Replacements.known.BuildDir}"))),
      jvmRuntimeOptions = model.Options.empty,
      nativeVersion = None,
      nativeGc = None,
      nativeBuildTarget = None,
      nativeLinkerReleaseMode = None,
      nativeLTO = None,
      nativeMultithreading = None,
      nativeOptimize = None,
      nativeEmbedResources = None,
      nativeUseIncrementalCompilation = None
    )

  object remove extends BuildRewrite {
    override val name = model.BuildRewriteName("defaults-remove")

    protected def newExplodedProjects(oldBuild: model.Build, buildPaths: BuildPaths): Map[model.CrossProjectName, model.Project] =
      oldBuild.explodedProjects.map { case (name, p) => (name, project(p)) }

    def project(proj: model.Project): model.Project =
      proj.copy(
        scala = proj.scala.map(ret => ret.copy(setup = ret.setup.map(setup => setup.removeAll(Defaults.DefaultCompileSetup)))),
        platform = proj.platform.map(x => x.removeAll(Defaults.Jvm)),
        `source-layout` = proj.`source-layout`.filterNot { sourceLayout =>
          val default = if (proj.scala.isDefined) model.SourceLayout.Normal else model.SourceLayout.Java
          sourceLayout == default
        }
      )
  }

  object add extends BuildRewrite {
    override val name = model.BuildRewriteName("defaults-add")

    protected def newExplodedProjects(oldBuild: model.Build, buildPaths: BuildPaths): Map[model.CrossProjectName, model.Project] =
      oldBuild.explodedProjects.map { case (name, p) => (name, project(p)) }

    def project(proj: model.Project): model.Project =
      proj.copy(
        scala = proj.scala.map(x => x.copy(setup = Some(x.setup.fold(DefaultCompileSetup)(_.union(DefaultCompileSetup))))),
        platform = proj.platform.map(x => if (x.name.contains(model.PlatformId.Jvm)) x.union(Defaults.Jvm) else x),
        `source-layout` = proj.`source-layout`.orElse {
          val default = if (proj.scala.isDefined) model.SourceLayout.Normal else model.SourceLayout.Java
          Some(default)
        }
      )
  }
}
