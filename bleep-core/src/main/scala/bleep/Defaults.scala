package bleep

import bloop.config.Config

import java.net.URI

object Defaults {
  val version = "0.0.1"

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

  val Jvm: model.Platform.Jvm =
    model.Platform.Jvm(
      options = Options(Set(Options.Opt.Flag("-Duser.dir=${BUILD_DIR}"))),
      mainClass = None,
      runtimeOptions = Options.empty
    )
}
