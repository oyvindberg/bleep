package bleep

import bloop.config.Config

import java.net.URI

object Defaults {
  val version = "0.0.1"
  val $schema = "https://raw.githubusercontent.com/oyvindberg/bleep/add-json-schema/schema.json"
  val MavenCentral = URI.create("https://repo1.maven.org/maven2")
  val BuildFileName = "bleep.json"

  val BleepBloopFolder = ".bleep/.bloop"
  val ScalaPluginPrefix = "-Xplugin:"

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
      jsVersion = None,
      jsMode = None,
      jsKind = None,
      jsEmitSourceMaps = None,
      jsJsdom = None,
      jsMainClass = None,
      jvmOptions = Options(Set(Options.Opt.Flag("-Duser.dir=${BUILD_DIR}"))),
      jvmMainClass = None,
      jvmRuntimeOptions = Options.empty,
      nativeVersion = None,
      nativeMode = None,
      nativeGc = None,
      nativeMainClass = None
    )

  def remove(build: ExplodedBuild): ExplodedBuild = {
    val newProjects = build.projects.map { case (crossName, p) => (crossName, remove(p)) }
    build.copy(projects = newProjects)
  }

  def remove(proj: model.Project): model.Project =
    proj.copy(
      scala = proj.scala.map(ret => ret.copy(setup = ret.setup.map(setup => setup.removeAll(Defaults.DefaultCompileSetup)))),
      platform = proj.platform.map(x => x.removeAll(Defaults.Jvm)),
      `source-layout` = proj.`source-layout`.filterNot(_ == SourceLayout.Normal)
    )

  def add(build: ExplodedBuild): ExplodedBuild = {
    val newProjects = build.projects.map { case (crossName, p) => (crossName, add(p)) }
    build.copy(projects = newProjects)
  }

  def add(proj: model.Project): model.Project =
    proj.copy(
      scala = proj.scala.map(x => x.copy(setup = Some(x.setup.fold(DefaultCompileSetup)(_.union(DefaultCompileSetup))))),
      platform = proj.platform.map(x => if (x.name.contains(model.PlatformId.Jvm)) x.union(Defaults.Jvm) else x),
      `source-layout` = proj.`source-layout`.orElse(Some(SourceLayout.Normal))
    )
}
