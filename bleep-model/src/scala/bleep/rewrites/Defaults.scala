package bleep.rewrites

import bleep.model.{CompileOrder, ExplodedBuild, Options, SourceLayout}
import bleep.{model, Rewrite}

object Defaults {
  // values copied from bloops `Config.CompileSetup.empty`
  val DefaultCompileSetup = model.CompileSetup(
    order = Some(CompileOrder.Mixed),
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
      jsMode = None,
      jsKind = None,
      jsEmitSourceMaps = None,
      jsJsdom = None,
      jsNodeVersion = None,
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
        `source-layout` = proj.`source-layout`.filterNot { sourceLayout =>
          val default = if (proj.scala.isDefined) SourceLayout.Normal else SourceLayout.Java
          sourceLayout == default
        }
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
        `source-layout` = proj.`source-layout`.orElse {
          val default = if (proj.scala.isDefined) SourceLayout.Normal else SourceLayout.Java
          Some(default)
        }
      )
  }
}
