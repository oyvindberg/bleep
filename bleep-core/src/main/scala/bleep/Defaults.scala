package bleep

import bloop.config.Config

object Defaults {
  val DefaultCompileSetup = model.CompileSetup(
    order = Some(Config.CompileSetup.empty.order),
    addLibraryToBootClasspath = Some(Config.CompileSetup.empty.addLibraryToBootClasspath),
    addCompilerToClasspath = Some(Config.CompileSetup.empty.addCompilerToClasspath),
    addExtraJarsToClasspath = Some(Config.CompileSetup.empty.addExtraJarsToClasspath),
    manageBootClasspath = Some(Config.CompileSetup.empty.manageBootClasspath),
    filterLibraryFromClasspath = Some(Config.CompileSetup.empty.filterLibraryFromClasspath)
  )

  val Jvm: model.Platform =
    model.Platform(
      name = None,
      mainClass = None,
      jsVersion = None,
      jsMode = None,
      jsKind = None,
      jsEmitSourceMaps = None,
      jsJsdom = None,
      jvmOptions = Options(Set(Options.Opt.Flag("-Duser.dir=${BUILD_DIR}"))),
      jvmRuntimeOptions = Options.empty,
      nativeVersion = None,
      nativeMode = None,
      nativeGc = None
    )

  object remove extends Rewrite {
    override val name: String = "defaults-remove"

    override def apply(build: ExplodedBuild): ExplodedBuild = {
      val newProjects = build.projects.map { case (crossName, p) => (crossName, project(p)) }
      build.copy(projects = newProjects)
    }

    def project(proj: model.Project): model.Project =
      proj.copy(
        scala = proj.scala.map(ret => ret.copy(setup = ret.setup.map(setup => setup.removeAll(Defaults.DefaultCompileSetup)))),
        platform = proj.platform.map(x => x.removeAll(Defaults.Jvm)),
        `source-layout` = proj.`source-layout`.filterNot(_ == SourceLayout.Normal)
      )
  }

  object add extends Rewrite {
    override val name: String = "defaults-add"

    override def apply(build: ExplodedBuild): ExplodedBuild = {
      val newProjects = build.projects.map { case (crossName, p) => (crossName, project(p)) }
      build.copy(projects = newProjects)
    }

    def project(proj: model.Project): model.Project =
      proj.copy(
        scala = proj.scala.map(x => x.copy(setup = Some(x.setup.fold(DefaultCompileSetup)(_.union(DefaultCompileSetup))))),
        platform = proj.platform.map(x => if (x.name.contains(model.PlatformId.Jvm)) x.union(Defaults.Jvm) else x),
        `source-layout` = proj.`source-layout`.orElse(Some(SourceLayout.Normal))
      )
  }
}
